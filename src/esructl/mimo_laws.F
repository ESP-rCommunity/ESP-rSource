C This file is part of the ESP-r system.
C Copyright Natural Resources Canada, Government
C of Canada 2004--2006. Please Contact Ian
C Beausoliel-Morrison for details concerning licensing.

C ESP-r is free software.  You can redistribute it and/or
C modify it under the terms of the GNU General Public
C License as published by the Free Software Foundation
C (version 2 orlater).

C ESP-r is distributed in the hope that it will be useful
C but WITHOUT ANY WARRANTY; without even the implied
C warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
C PURPOSE. See the GNU General Public License for more
C details.

C You should have received a copy of the GNU General Public
C License along with ESP-r. If not, write to the Free
C Software Foundation, Inc., 59 Temple Place, Suite 330,
C Boston, MA 02111-1307 USA.
C
C================== mimo_laws.F ========================================
C
C Date:      June 28, 2007
C Author:    Alex Ferguson
C Copyright: Natural Resources Canada, (2007)
C
C This file contains control laws associated with the MIMO control 
C facility. New control loops should be appended to this file.
C
C=======================================================================

C=======================================================================
C
C  Functions:
C
C    - MIMO_invoke_law: Simple function to invoke the specified 
C         control law.
C
C=======================================================================

C------------ MIMO_invoke_law ------------------------------------------
C
C     This function invokes a control loop's corresponding law
C
C     inputs: iLoop - index of control loop;
C             fSensor_vals - List of values measured by this loop's 
C                  sensors
C
C     outputs: fActuator_vals - List of values to be applied by 
C                  this loop's actuators.
C
C-----------------------------------------------------------------------
      subroutine MIMO_invoke_law ( iLoop, fSensor_vals, fActuator_vals )
      implicit none
#include "mimo_ctl.h"
C-----------------------------------------------------------------------
C     Passed arguements 
C-----------------------------------------------------------------------
      
      integer iLoop                   ! index of control loop 
      real fSensor_vals(mMIMOList)    ! List of values for loop's sensors
      real fActuator_vals(mMIMOList)  ! List of values for loop's actuators 
                                      
      
C-----------------------------------------------------------------------
C     Local variables
C-----------------------------------------------------------------------

C-----------------------------------------------------------------------
C     Invoke loop's corresponding law
C-----------------------------------------------------------------------
      if ( iLoopLaw( iLoop ) == 1 ) then 
      
        call MIMO_OnOff_Ctl_Law ( iLoop, fSensor_vals, fActuator_vals )
        
      else
        
        write ( cContext, '(A,I2,A)') 
     &       ' Invoking law for control loop # ', iLoop, '.'
        write ( cMsg, '(A)') 
     &       ' Fatal error: Unknown control type! '
        call UsrMsg ( cContext, cMsg, 'F' )
        
      endif 


      return 
      end 

C------------------- MIMO_OnOff_Ctl_Law ---------------------------------
C
C     This function applies MIMO on-off control law
C
C     Inputs: iLoop - index of current loop;
C             fSensor_vals - List of values measured by this loop's 
C                  sensors
C
C     Outputs: fActuator_vals - List of values to be applied by 
C                  this loop's actuators.
C
C-----------------------------------------------------------------------
      subroutine MIMO_OnOff_Ctl_Law( iLoop, 
     &                               fSensor_vals, 
     &                               fActuator_vals )
      implicit none 
#include "mimo_ctl.h"
C-----------------------------------------------------------------------
C     Passed arguements 
C-----------------------------------------------------------------------
      
      integer iLoop                   ! index of control loop 
      
C-----------------------------------------------------------------------
C     Local variables
C-----------------------------------------------------------------------

      integer iSensor                ! Sensor loop
      integer iActuator              ! Actuator loop 

      logical bStateOn               ! Flag for on-off 

      real fSensor_vals(mMIMOList)   ! List of values for loop's sensors
      real fActuator_vals(mMIMOList) ! List of values for loop's actuators 
            
      logical bFatalError            ! Error flag
      
      integer iIndex 
      
      logical bAll_Below            ! Flag indicating all sensors are
                                    !   below their respective setpoints.
      
      logical bOn(mMIMOLoop)        ! Flags indicating if each loop is 
                                    !   on, or off.
                                     
      integer iLowSetPoint          ! Lower setpoint index
      integer iHighSetPoint         ! Upper setpoint index 
      
      parameter ( iLowSetPoint = 1, iHighSetPoint = 2 )
                                     
      integer iLowVal               ! Output at lower setpoint (index)
      integer iHighVal              ! Output at higher setpoint (index)
      parameter ( iLowVal = 1, iHighVal = 2 )
      
      integer iOutputIndex          ! Index of value to be output. 
                                    
C.....Initialize all controllers to off                                    
      data bOn / mMIMOLoop * .false. /                                     

      ! call MIMO_debug (" Invoked MIMO OnOff control ")
      
C-----------------------------------------------------------------------
C     Initialize 
C-----------------------------------------------------------------------
      write (cContext, '(A,I2,A)')
     &  'MIMO On-Off Control: Loop #', iLoop, '.'

C-----------------------------------------------------------------------
C     Check that the correct number of data are defined:
C
C         - Each sensor should have 2 items
C
C         - Each actuator should have 2 items 
C
C         - Loop should have one additonal data item.
C 
C-----------------------------------------------------------------------      

      do iSensor = 1, iLoopSensorCount ( iLoop ) 
      
        if ( iLoopSensorDataCount( iLoop, iSensor ) /=  2 ) then 
     
              
          write (cMsg, '(A,I2,A,I2,A,I2,A)')
     &     'Incorrect number of data items specified for sensor',
     &     iSensor, ' in loop ', iLoop,' (2 expected, but', 
     &     iLoopSensorDataCount( iLoop, iSensor ), ' were provided).'
     
          call UsrMsg ( cContext, cMsg, 'F' )
        
        endif 
      
      enddo 
      
      do  iActuator = 1, iLoopActuatorCount ( iLoop ) 
      
        if ( iLoopActuatorDataCount( iLoop, iActuator ) /=  2 ) then 
     
              
          write (cMsg, '(A,I2,A,I2,A,I2,A)')
     &     'Incorrect number of data items specified for actuator',
     &     iActuator, ' in loop ', iLoop,' (2 expected, but', 
     &     iLoopActuatorDataCount( iLoop, iActuator ), 
     &     ' were provided).'
     
          call UsrMsg ( cContext, cMsg, 'F' )
          
        endif 
            
      
      enddo 
      

      if ( iLoopDataCount ( iLoop ) /= 1 ) then
     
          write (cMsg, '(A,I2,A,I2,A,I2,A)')
     &     'Incorrect number of data items specified for loop',
     &      iLoop,' (1 expected, but', 
     &      iLoopActuatorCount(iLoop) , ' were provided).'
     
        call UsrMsg ( cContext, cMsg, 'F' )
     
      endif 



C-----------------------------------------------------------------------
C     Loop through sensors, and detetmine if any exceed the upper 
C     setpoint, or if all sensors are below lower set point. 
C-----------------------------------------------------------------------     
      bAll_Below = .true.  
      do iSensor = 1, iLoopSensorCount ( iLoop ) 
      
        
C-----------------------------------------------------------------------  
C       If any sensor values exceed their corresponding upper setpoint, 
C       set control state (bOn) to 'ON'
C-----------------------------------------------------------------------  
      
        if ( fSensor_vals ( iSensor ) >= 
     &      fLoopSensorData ( iLoop, iSensor, iHighSetPoint ) ) then
     
          bOn( iLoop ) = .true. 
               
        endif 
      
C-----------------------------------------------------------------------  
C       If this sensor value is above its corresponding lower 
C       setpoint, set bAll_Below flag to .false. 
C-----------------------------------------------------------------------  
        if ( fSensor_vals ( iSensor ) >
     &      fLoopSensorData ( iLoop, iSensor, iLowSetPoint ) ) then
     
          bAll_Below = .false.
     
        endif 
        
      enddo 
      
C-----------------------------------------------------------------------  
C       If all sensors fall below lower setpoint, set system state 
C       to off. 
C-----------------------------------------------------------------------      
      
      if ( bAll_Below ) bOn( iLoop ) = .false. 
      
      
      
      

      
C-----------------------------------------------------------------------     
C     Assign control values to actuator array
C-----------------------------------------------------------------------     
      
      if ( bOn( iLoop ) ) then 
        
        iOutputIndex = iHighVal
        
      else
      
        iOutputIndex = iLowVal
        
      endif       
      
      do iActuator = 1 , iLoopActuatorCount ( iLoop ) 
      
        fActuator_vals ( iActuator ) 
     &         = fLoopActuatorData ( iLoop, iActuator, iOutputIndex ) 
     
      enddo 
      
      
      return 
      end 
