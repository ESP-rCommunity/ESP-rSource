C This file is part of the ESP-r system.
C Copyright CANMET Energy Technology Centre 
C Natural Resources Canada, Government of Canada
C 2004. Please Contact Ian Beausoliel-Morrison for details 
C concerning licensing.

C ESP-r is free software.  You can redistribute it and/or
C modify it under the terms of the GNU General Public
C License as published by the Free Software Foundation 
C (version 2 or later).

C ESP-r is distributed in the hope that it will be useful
C but WITHOUT ANY WARRANTY; without even the implied
C warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
C PURPOSE. See the GNU General Public License for more
C details.

C You should have received a copy of the GNU General Public
C License along with ESP-r. If not, write to the Free
C Software Foundation, Inc., 59 Temple Place, Suite 330,
C Boston, MA 02111-1307 USA.

C *********** EPCNTRL
C Edit plant control function II.
      SUBROUTINE DG_controller_menu(fDG_configuration)
      IMPLICIT NONE
#include "building.h"
#include "plant.h" 
#include "DG_controller_include.h"

      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/H(60)

C.....ESP-r common block variables
      integer iuout, iuin
      character H*72            !<- array for help prompts.

C.....Array containing user-defined DG controller configuration
      real fDG_configuration(misc)     

C.....Temporary variables for holding miscellanous user
C.....responsed
      logical bDG_misc_boolean
      integer iDG_misc_integer
      real    fDG_misc_float

C.....#number of data in controller configuration
      integer iDG_Number_of_data

C.....Flags for hot/cold tank controls
      logical bDG_HOT_control, bDG_COLD_control
C.....Hot/cold tank control priority
      integer iDG_tank_priority
    
C.....Electrical output for steady-state operation
      logical bDG_constant_set
      real    fDG_control_constant
C.....Hot tank control data
      integer iDG_HWT_index, iDG_HWT_node
      integer iDG_HS_start, iDG_HS_end
      integer iDG_HS_strategy
      real    fDG_HWT_T_su,  fDG_HWT_T_sl, fDG_HWT_Target
      real    fDG_HWT_Kp,  fDG_HWT_Ki,  fDG_HWT_Kd 
      real    fDG_HWT_per_int, fDG_HWT_per_der
C.....Colt tank control data
      integer iDG_CWT_index, iDG_CWT_node
      integer iDG_CS_start, iDG_CS_end
      real    fDG_CS_constant
      integer iDG_CS_strategy
      real    fDG_CWT_T_su,  fDG_CWT_T_sl, fDG_CWT_Target
      real    fDG_CWT_Kp,  fDG_CWT_Ki,  fDG_CWT_Kd 
      real    fDG_CWT_per_int, fDG_CWT_per_der
C.....Flag for timestep output
      integer iDG_TS_out 

C.....Counter
      integer iCount

C.....Error flags from ESP-r prompts
      integer ier,ieri

C.....Intrinsic functions
C      integer int

      
C.....Initialize flag indicating if constant elec. output 
C.....has already been collected.
      bDG_constant_set = .false.

C......Dunno what this does
C      if(itrc.gt.10)then
C         call edisp(iuout,' ')
C         call edisp(iuout,
C     &        ' Current cogeneration controller data is:')
C                      
C      endif



C.....Get current values for all inputs and point to jump
C     back to if user cancels a dialog.
 77   fDG_control_constant  = fDG_configuration(2)
      iDG_tank_priority     = int(fDG_configuration(3))
      iDG_HWT_index         = int(fDG_configuration(4))
      iDG_HWT_node          = int(fDG_configuration(5))
      iDG_CWT_index         = int(fDG_configuration(6))
      iDG_CWT_node          = int(fDG_configuration(7))

      iCount = 6                ! number of data assigned so far

C.....Parameters for hot water tank control
      if ( iDG_HWT_index .gt. 0 ) then
C........Heating season duration
         iDG_HS_start    = int(fDG_configuration(iCount+2))
         iDG_HS_end      = int(fDG_configuration(iCount+3))
C........Control stragegy
         iDG_HS_strategy = int(fDG_configuration(iCount+4))

C........Tank setpoints
         fDG_HWT_T_sl    = fDG_configuration(iCount+5)
         fDG_HWT_T_su    = fDG_configuration(iCount+6)

C........Target tempreature
         fDG_HWT_Target  = fDG_HWT_T_sl + fDG_configuration(iCount+7)
     &    * ( fDG_HWT_T_su - fDG_HWT_T_sl )

C........PID data
         fDG_HWT_Kp      = fDG_configuration(iCount+8)
         fDG_HWT_Ki      = fDG_configuration(iCount+9) 
         fDG_HWT_Kd      = fDG_configuration(iCount+10)

C........Integral/derivative periods
         fDG_HWT_per_int = fDG_configuration(iCount+11)
         fDG_HWT_per_der = fDG_configuration(iCount+12)

         iCount = iCount + 11

      endif


C.....Parameters for cold water tank control 
      if ( iDG_CWT_index .gt. 0 ) then
C........Heating season duration
         iDG_CS_start   = int(fDG_configuration(iCount+2))
         iDG_CS_end     = int(fDG_configuration(iCount+3))
C........Control stragegy
         iDG_CS_strategy = int(fDG_configuration(iCount+4))

C........Tank setpoints
         fDG_CWT_T_sl   = fDG_configuration(iCount+5)
         fDG_CWT_T_su   = fDG_configuration(iCount+6)

C........Target coefficient (0->1)
         fDG_CWT_Target = fDG_CWT_T_sl + fDG_configuration(iCount+7) 
     &         * ( fDG_CWT_T_su - fDG_CWT_T_sl )

C........PID data
         fDG_CWT_Kp     = fDG_configuration(iCount+8)
         fDG_CWT_Ki     = fDG_configuration(iCount+9) 
         fDG_CWT_Kd     = fDG_configuration(iCount+10) 

C........Integral/derivative periods
         fDG_CWT_per_int= fDG_configuration(iCount+11)
         fDG_CWT_per_der= fDG_configuration(iCount+12) 

         iCount = iCount + 11
      endif     

C.....Timestep data flag
      iDG_TS_out = int(fDG_configuration(iCount+2))
      iCount = iCount + 1

C.....Determine if hot and/or cold water storage are in use
      H(1)='The cogeneration system controller may be used '
      H(2)='to effect control of a CHP system in response ' 
      H(3)='to:'
      H(4)='   - electrical loads in the building'
      H(5)='   - heating loads (requires cogeneration system '
      H(6)='     and coupled hot water storage tank)'
      H(7)='   - cooling loads (requires cogeneration system, '
      H(8)='     thermally-activated cooling equipment and '
      H(9)='     coupled cold water storage tank)'
      H(10)= ' '

      call ASKOK('Is the CHP system coupled to a hot water',
     &     'tank that you wish to control?', bDG_HOT_control,
     &     .true.,9)

      call ASKOK('Is the CHP system coupled to a cold water',
     &     'tank that you wish to control?', bDG_COLD_control,
     &     .false.,9)
      
C.........Determine which tank has priority
      if ( bDG_HOT_control .and. bDG_cold_control ) then
         H(1) = ' During periods of simulaneous heating and cooling'
         H(2) = ' loads, the cogeneration plant may not be able to '
         H(3) = 
     &        ' both loads simultaneously. You may assign priority'
         H(4) = ' to the heating load or the cooling load under  '
         H(5) = ' these circumstances'
         H(6) = ' ' 
         call EASKAB(
     &        'Which load do you wish to assign priority to?',
     &        ' ', 
     &        'Cooling loads', 
     &        'Heating loads',
     &        iDG_tank_priority,
     &        6 )

      elseif ( bDG_HOT_control ) then
         iDG_tank_priority = HOT_storage  ! <- named constant in DG_controller_include.h
      elseif ( bDG_COLD_control ) then
         iDG_tank_priority = COLD_storage ! <- named constant in DG_controller_include.h
      else
         iDG_tank_priority = 0
      endif

C.....Get data for hot water tank control
      if ( bDG_HOT_control) then

C........Before indexes are determined, check to see if they
C........are less than 1. if so, set hot water tank upper setpoint 
C........to default.
         if(iDG_HWT_index .lt. 0. ) then
            ! setting to < 0 will cause defaults to be used
            fDG_HWT_T_su = -10.0
            fDG_HWT_T_sl = -10.0 
            fDG_HWT_Kp = -10.0
            fDG_HWT_Ki = -10.0   
            fDG_HWT_Kd = -10.0
            fDG_HWT_per_int = -10.0
            fDG_HWT_per_der = -10.0
         endif


C........HWT index
         H(1) = ' The cogeneration controller needs to know '
         H(2) = ' the index of the hot water tank to be controlled '
         H(3) = ' in the plant network. The index is determined by '
         H(4) = ' the sequence in which plant components are'
         H(5) = ' defined in the plant network file (ie. the '
         H(6) = ' third component will have and index of 3).'
         H(7) = ' ' 
         H(8) = ' If you cancel at the index dialog you will be' 
         H(9) = ' returned to the start of the controller input' 
         H(10)= ' process. You may need to check your data because' 
         H(11)= ' prior data for other aspects of the controller'
         H(12)= ' may may be inconsistent.' 
         CALL EASKI(iDG_HWT_index,
     &            ' ',
     &            'Index of hot water tank in plant network',
     &            1,'F',
     &            MPCOM,'W',
     &            1,
     &            'HWT index',IERI,
     &            12)
         if(ieri.eq.-3) goto 77

C........HWT node             
         H(1) = ' The cogeneration controller needs to know '
         H(2) = ' the index of the node representing the water '
         H(3) = ' in the hot water tank that is to be controlled.'
         H(4) = ' If unsure, review the documenation of the water'
         H(5) = ' tank model.'
         H(6) = ' '
         H(7) = ' If you cancel at the index dialog you will be' 
         H(8) = ' returned to the start of the controller input' 
         H(9)= ' process. You may need to check your data because' 
         H(10)= ' prior data for other aspects of the controller'
         H(11)= ' may may be inconsistent.' 
         CALL EASKI(iDG_HWT_node,
     &        ' ',
     &        'Index of hot water tank Water node ',
     &        1,'F',
     &        MNODEC,'W',
     &        1,
     &        'HWT index',IERI,
     &        11)  
         if(ieri.eq.-3) goto 77

C........Heating season        
         H(1) = ' The controller may be configured to regulate'
         H(2) = ' the temperature of the hot water tank year-round'
         H(3) = ' or during a certian period in the year:'
         H(4) = '    - Choose "year round" if year-round space '
         H(5) = '      heating and/or domestic hot water loads '
         H(6) = '      are placed on the tank'
         H(7) = '    - Choose "seasonal" if only seasonal '
         H(8) = '      space heating loads are placed on the tank'
         H(9)=' '
         call EASKAB('Are the loads on the hot water tank ',
     &        'seasonal, or year-round?', 
     &        'Seasonal', 
     &        'Year round',
     &        iDG_misc_integer,
     &        9 )

         if (iDG_misc_integer .eq. 1) then
C...........Get seasonal start                 
            H(1)='You may define start of the heating season as ' 
            H(2)='any day between 1 (Jan 1st) and 365 (Dec 31st).'
            H(3)=' '
            H(4)=' If you cancel at the index dialog you will be' 
            H(5)=' returned to the start of the controller input' 
            H(6)=' process. You may need to check your data because' 
            H(7)=' prior data for other aspects of the controller'
            H(8)=' may may be inconsistent.' 
            CALL EASKI(iDG_HS_start,
     &           'Heating season start day',
     &           '(1-365)',
     &           1,'F',
     &           365,'F',
     &           1,
     &           'Heating season start day',IERI,
     &           8)
            if(ieri.eq.-3) goto 77

C...........Get seasonal end                
            H(1)='You may define end of the heating season end ' 
            H(2)='any day between 1 (Jan 1st) and 365 (Dec 31st).'
            H(3)=' '
            H(4)=' If you cancel at the index dialog you will be' 
            H(5)=' returned to the start of the controller input' 
            H(6)=' process. You may need to check your data because' 
            H(7)=' prior data for other aspects of the controller'
            H(8)=' may may be inconsistent.' 
            CALL EASKI(iDG_HS_end,
     &           'Heating season end day',
     &           '(1-365)',
     &           1,'F',
     &           365,'F',
     &           365,
     &           'Heating season end day',IER,
     &           8)
            if(ieri.eq.-3) goto 77
  
         else
C...........Use year start & end                 
            iDG_HS_start = 1
            iDG_HS_end   = 365
         endif

C........GET heating season control strategy
         H(1)= 'You may specify the control strategy that will '
         H(2)= 'be applied during the heating season:'
         H(3)= '   - Constant output: The controller maintains'
         H(4)= '     the output of the cogeneration system at'
         H(5)= '     a constant electrical output throughout '
         H(6)= '     the heating season.'
         H(7)= '   - Electrical load following: The controller'
         H(8)= '     matches the cogeneration system`s output'
         H(9)= '     to the building`s electrical loads.'
         H(10)='   - Thermal load following: The controller '
         H(11)='     regulates the cogeneration systems`s '
         H(12)='     output to maintain the temperature of the'
         H(13)='     hot water tank between two setpoints/'
         H(14)='   - "Hybrid" control: The controller regulates'
         H(15)='     the cogeneration system`s operation to meet'
         H(16)='     either the electrical or thermal loads, '
         H(17)='     whichever produces the greatest amount of '
         H(18)='     electrical and thermal output.'
         H(19)=' '

         call EASKATOG(
     &        'Heating season control strategy',
     &        ' ',
     &        'Constant output',
     &        'Electrical load following',
     &        'Thermal load following',
     &        '"Hybrid" control',
     &        ' ',
     &        ' ',
     &        ' ',
     &        iDG_HS_strategy,
     &        19)

         if ( iDG_HS_strategy .eq. ALG_constant ) then
C...........Get constant electrical output from user
            H(1)= 
     &           'You may specify the constant electrical output that'
            H(2)= 
     &           'the controller will use. If the specified value'
            H(3)= 
     &           'exceeds the cogeneration system`s maximum output,'
            H(4)= 
     &           'that value will be used instead. Similarly, if the'
            H(5)= 'system`s minimum output exceeds the specified '
            H(6)= 'value, the minimum output will be used instead.'
            H(7)= ' '
            call EASKR(
     &           fDG_control_constant,
     &           'Cogeneration system constant output:',
     &           '(W)',
     &           0.0, 'F',
     &           10000.0, '-',
     &           1000.0,
     &           'constant output',IER,
     &           7)
            bDG_constant_set = .true.  
         else 
            fDG_control_constant = -1.0 ! <- dummy data
         endif
         
C........Get tank set point temperatures
         H(1)= 'The upper tank temperature defines the maximum '
         H(2)= 'temperature that the hot water tank may be safely'
         H(3)= 'heated to. If the tank is heated above this '
         H(4)= 'temperature, the controller will activate the '
         H(5)= 'the plant`s heat rejection equipment. '
         H(6) = ' ' 
         
         call EASKR(
     &        fDG_HWT_T_su,
     &        'Hot water tank upper setpoint:',
     &        '(deg C)',
     &        0.0, 'F',
     &        100.0, 'F',
     &        75.0,
     &        'HWT upper setpoint',IER,
     &        6)

         if ( ( iDG_HS_strategy .eq. ALG_thermal_L )   .OR.
     &        ( iDG_HS_strategy .eq. ALG_thermal_P ) ) then

C...........HWT lower setpoint only required for themal load following
C...........algorithms                

C...........Calculate default value 
            if ( fDG_HWT_T_su - 10. .lt. 0.) then
               fDG_misc_float = 0.
            else
               fDG_misc_float = fDG_HWT_T_su - 10.0
            endif

            if ( fDG_HWT_T_sl .gt. fDG_HWT_T_su ) then
                fDG_HWT_T_sl =  fDG_misc_float
            endif



            H(1)= 'The lower tank temperature defines the lower '
            H(2)= 'bound of the operating range on the controller`s '
            H(3)= 'thermal load following algorithms. '
            H(4) = ' '            
            call EASKR(
     &           fDG_HWT_T_sl,
     &           'Hot water tank lower setpoint:',
     &           '(deg C)',
     &           0.0, 'F',
     &           fDG_HWT_T_su, 'F',
     &           (fDG_misc_float),
     &           'HWT lower setpoint',IER,
     &           4)


C...........HWT target

            if ( ( fDG_HWT_Target .gt. fDG_HWT_T_su ) .or.
     &           ( fDG_HWT_Target .lt. fDG_HWT_T_sl ) ) then
               fDG_HWT_Target = (fDG_HWT_T_su + fDG_HWT_T_sl)/2
            endif

            H(1)='You may specify the temperature at which the '
            H(2)='controller will attempt to maintain the hot  ' 
            H(3)='water tank. It must be set to a value between'
            H(4)='the tank`s lower and upper set point ' 
            write(H(5), '(A,F4.1,A,F4.1,A)') 
     &           'temperatures (',fDG_HWT_T_sl,'-',fDG_HWT_T_su,').'
                     
            call EASKR(
     &           fDG_HWT_Target,
     &           'Hot water tank target temperature ',
     &           '(deg C)',
     &           fDG_HWT_T_sl, 'F',
     &           fDG_HWT_T_su, 'F',
     &           (fDG_HWT_T_su + fDG_HWT_T_sl)/2.,
     &           'HWT Target temperature',IER,
     &           6)

C...........HWT proportional gain
            H(1)='The proportional gain describes the controller`s' 
            H(2)='sensitivity to diffeneces between the target '
            H(2)='temperature and the hot water tank`s actual  ' 
            H(3)='temperature. Higher values will produce a faster '
            H(5)='response, but may lead to controller instability.'
            H(6)='A value of 0.8 - 1.0 is recommended.'
            H(7)=' '           
            call EASKR(
     &           fDG_HWT_Kp,
     &           'Hot water tank proportional gain ',
     &           '(default = 0.8)',
     &           0.0, 'F',
     &           3.0, 'W',
     &           0.8,
     &           'HWT proportional gain',IER,
     &           7)



C...........HWT integral gain
            H(1)='The proportional gain describes the controller`s' 
            H(2)='sensitivity to historical diffeneces between the '
            H(2)='target temperature and the hot water tank`s actual' 
            H(3)='temperature. Higher values will ensure that the '
            H(5)='target temperature is eventually met, but may slow '
            H(6)='the controller`s response. A value of 0.00005 is '
            H(7)='recommended.'
            H(8)=' ' 

            call EASKR(
     &           fDG_HWT_Ki,
     &           'Hot water tank integral gain ',
     &           '(default= 0.00005)',
     &           0.0, 'F',
     &           0.1, 'W',
     &           0.00005,
     &           'HWT integral gain',IER,
     &           8)

 
C...........HWT derivative gain
            H(1)='The derivative gain describes the controller`s' 
            H(2)='sensitivity to instantanous changes in the hot water' 
            H(3)='tank`s actual temperature. Higher values will '
            H(4)='produce a faster controller, but may result in '
            H(5)='controller instability. A value of 360.0 is '
            H(6)='recommended.'
            H(7)=' ' 

            call EASKR(
     &           fDG_HWT_Kd,
     &           'Hot water tank derivative gain ',
     &           '(default=360.0)',
     &           0.0, 'F',
     &           10000.0, 'W',
     &           360.0,
     &           'HWT derivative gain',IER,
     &           7)



C...........HWT integral period
            H(1)='The integral period defines the time-frame over'
            H(2)='which the controller will be sensitive to historical'
            H(3)='differences between the actual and target '
            H(4)='temperatures. A value of 2 hours is recommended'
            H(5)=' ' 

            call EASKR(
     &           fDG_HWT_per_int,
     &           'Hot water tank integral period ',
     &           '(hours)',
     &           0.0, 'F',
     &           24.0, 'W',
     &           2.0,
     &           'HWT integral period',IER,
     &           5)


C...........HWT derivative period
            H(1)='The derivative period defines the time-frame over'
            H(2)='which the controller will be sensitive to rapid'
            H(3)='differences between the actual and target '
            H(4)='temperatures. A value of 0.5 hours is recommended'
            H(5)=' ' 

            call EASKR(
     &           fDG_HWT_per_der,
     &           'Hot water tank derivative period ',
     &           '(hours)',
     &           0.0, 'F',
     &           1.0, 'W',
     &           2.0,
     &           'HWT derivative period',IER,
     &           5)


         else
C...........Set dummy data as req'd                 
            fDG_HWT_T_sl = fDG_HWT_T_su - 10.
            fDG_HWT_Target = (fDG_HWT_T_su+ fDG_HWT_T_sl)/2.
            fDG_HWT_Kp = -10.0
            fDG_HWT_Ki = -10.0   
            fDG_HWT_Kd = -10.0
            fDG_HWT_per_int = -10.0
            fDG_HWT_per_der = -10.0
         endif
      else
C........Nix pointers for hot water tank
         iDG_HWT_index = -1
         iDG_HWT_node  = -1
      endif



C.....Get data for cold water tank control
      if ( bDG_COLD_control) then


C........Before indexes are determined, check to see if they
C........are less than 1. if so, set hot water tank upper setpoint 
C........to default.
         if(iDG_CWT_index .lt. 0. ) then
            ! setting to < 0 will cause defaults to be used
            fDG_CWT_T_su = -10.0
            fDG_CWT_T_sl = -10.0 
            fDG_CWT_Kp = -10.0
            fDG_CWT_Ki = -10.0   
            fDG_CWT_Kd = -10.0
            fDG_CWT_per_int = -10.0
            fDG_CWT_per_der = -10.0
         endif

C........CWT index
         H(1) = ' The cogeneration controller needs to know '
         H(2) = ' the index of the cold water tank to be controlled '
         H(3) = ' in the plant network. The index is determined by '
         H(4) = ' the sequence in which plant components are'
         H(5) = ' defined in the plant network file (ie. the '
         H(6) = ' third component will have and index of 3).'
         H(7) = ' ' 
         H(8) =' If you cancel at the index dialog you will be' 
         H(9) =' returned to the start of the controller input' 
         H(10)=' process. You may need to check your data because' 
         H(11)=' prior data for other aspects of the controller'
         H(12)=' may may be inconsistent.' 
         CALL EASKI(iDG_CWT_index,
     &            ' ',
     &            'Index of cold water tank in plant network',
     &            1,'F',
     &            MPCOM,'W',
     &            1,
     &            'CWT index',IERI,
     &            12)
         if(ieri.eq.-3) goto 77

C........CWT node             
         H(1) = ' The cogeneration controller needs to know '
         H(2) = ' the index of the node representing the water '
         H(3) = ' in the cold water tank that is to be controlled.'
         H(4) = ' If unsure, review the documenation of the water'
         H(5) = ' tank model.'
         H(6) = ' '
         H(7) =' If you cancel at the index dialog you will be' 
         H(8) =' returned to the start of the controller input' 
         H(9) =' process. You may need to check your data because' 
         H(10)=' prior data for other aspects of the controller'
         H(11)=' may may be inconsistent.' 
         CALL EASKI(iDG_CWT_node,
     &        ' ',
     &        'Index of cold water tank water node ',
     &        1,'F',
     &        MNODEC,'W',
     &        1,
     &        'CWT index',IERI,
     &        11)  
         if(ieri.eq.-3) goto 77

C........Cooling season        
         H(1) = ' The controller may be configured to regulate'
         H(2) = ' the temperature of the cold water tank year-round'
         H(3) = ' or during a certian period in the year:'
         H(4) = '    - Choose "year round" if year-round space '
         H(5) = '      cooling loads are placed on the tank'
         H(6) = '    - Choose "seasonal" if only seasonal '
         H(7) = '      space cooling loads are placed on the tank'
         H(8)=' '
         call EASKAB('Are the loads on the cold water tank ',
     &        'seasonal, or year-round?', 
     &        'Seasonal', 
     &        'Year round',
     &        iDG_misc_integer,
     &        8 )

         if (iDG_misc_integer .eq. 1) then
C...........Get seasonal start                 
            H(1)='You may define start of the cooling season as ' 
            H(2)='any day between 1 (Jan 1st) and 365 (Dec 31st).'
            H(3)= ' '
            H(4)=' If you cancel at the index dialog you will be' 
            H(5)=' returned to the start of the controller input' 
            H(6)=' process. You may need to check your data because' 
            H(7)=' prior data for other aspects of the controller'
            H(8)=' may may be inconsistent.' 
            CALL EASKI(iDG_CS_start,
     &           'Cooling season start day',
     &           '(1-365)',
     &           1,'F',
     &           365,'F',
     &           1,
     &           'Cooling season start day',IERI,
     &           8)
            if(ieri.eq.-3) goto 77

C...........Get seasonal end                
            H(1)='You may define end of the cooling season end ' 
            H(2)='any day between 1 (Jan 1st) and 365 (Dec 31st).'
            H(3)= ' '
            H(4)=' If you cancel at the index dialog you will be' 
            H(5)=' returned to the start of the controller input' 
            H(6)=' process. You may need to check your data because' 
            H(7)=' prior data for other aspects of the controller'
            H(8)=' may may be inconsistent.' 
            CALL EASKI(iDG_CS_end,
     &           'Cooling season end day',
     &           '(1-365)',
     &           1,'F',
     &           365,'F',
     &           365,
     &           'Cooling season end day',IERI,
     &           8)
            if(ieri.eq.-3) goto 77
  
         else
C...........Use year start & end                 
            iDG_CS_start = 1
            iDG_CS_end   = 365
         endif

C........GET cooling season control strategy
         H(1)= 'You may specify the control strategy that will '
         H(2)= 'be applied during the cooling season:'
         H(3)= '   - Constant output: The controller maintains'
         H(4)= '     the output of the cogeneration system at'
         H(5)= '     a constant electrical output throughout '
         H(6)= '     the cooling season.'
         H(7)= '   - Electrical load following: The controller'
         H(8)= '     matches the cogeneration system`s output'
         H(9)= '     to the building`s electrical loads.'
         H(10)='   - Thermal load following: The controller '
         H(11)='     regulates the cogeneration systems`s '
         H(12)='     output to maintain the temperature of the'
         H(13)='     cold water tank between two setpoints/'
         H(14)='   - "Hybrid" control: The controller regulates'
         H(15)='     the cogeneration system`s operation to meet'
         H(16)='     either the electrical or thermal loads, '
         H(17)='     whichever produces the greatest amount of '
         H(18)='     electrical and thermal output.'
         H(19)=' '

         call EASKATOG(
     &        'Cooling season control strategy',
     &        ' ',
     &        'Constant output',
     &        'Electrical load following',
     &        'Thermal load following',
     &        '"Hybrid" control',
     &        ' ',
     &        ' ',
     &        ' ',
     &        iDG_CS_strategy,
     &        19)

         if ( iDG_CS_strategy .eq. ALG_constant .and. 
     &        .not. bDG_constant_set) then
C...........Get constant electrical output from user
            H(1)= 
     &           'You may specify the constant electrical output that'
            H(2)= 
     &           'the controller will use. If the specified value'
            H(3)= 
     &           'exceeds the cogeneration system`s maximum output,'
            H(4)= 
     &           'that value will be used instead. Similarly, if the'
            H(5)= 'system`s minimum output exceeds the specified '
            H(6)= 'value, the minimum output will be used instead.'
            H(7)= ' '
            call EASKR(
     &           fDG_control_constant,
     &           'Cogeneration system constant output:',
     &           '(W)',
     &           0.0, 'F',
     &           10000.0, '-',
     &           1000.0,
     &           'constant output',IER,
     &           7)
         elseif( .not. bDG_constant_set) then
            fDG_control_constant = -1.0 ! <- dummy data
         endif
         
C........Get tank set point temperatures
         H(1)= 'The upper tank temperature defines the minimum'
         H(2)= 'temperature that the cold water tank may be safely'
         H(3)= 'cooled to. If the tank is cooled below this '
         H(4)= 'temperature, the controller will deactivate the '
         H(5)= 'the plant`s thermally-activated cooling '
         H(6)= 'equipment.'
         H(7) = ' ' 
            call EASKR(
     &           fDG_CWT_T_sl,
     &           'Cold water tank lower setpoint:',
     &           '(deg C)',
     &           0.0, 'F',
     &           90.0, 'F',
     &           5.00,
     &           'CWT lower setpoint',IER,
     &           7)

         if( (  iDG_CS_strategy .eq. ALG_thermal_L ) .OR.
     &           (  iDG_CS_strategy .eq. ALG_thermal_P ) ) then
C...........Calculate default value
            if (  fDG_CWT_T_sl + 10.0 .gt. 100. ) then 
               fDG_misc_float = 100.
            else
               fDG_misc_float =  fDG_CWT_T_sl + 10.0
            endif
            if ( fDG_CWT_T_su .lt. fDG_CWT_T_sl ) then
                fDG_CWT_T_su =  fDG_misc_float
            endif
C...........CWT lower setpoint only required for themal load following
C...........algorithms                
            H(1)= 'The upper tank temperature defines the upper '
            H(2)= 
     &           'bound of the operating range on the controller`s '
            H(3)= 
     &           'thermal load following algorithms. '
            H(4) = ' '            


         
            call EASKR(
     &           fDG_CWT_T_su,
     &           'Cold water tank upper setpoint:',
     &           '(deg C)',
     &           fDG_CWT_T_sl, 'F',
     &           100.0, 'F',
     &           fDG_misc_float,
     &           'CWT upper setpoint',IER,
     &           6)


C...........CWT target


            if ( ( fDG_CWT_Target .gt. fDG_CWT_T_su ) .or.
     &           ( fDG_CWT_Target .lt. fDG_CWT_T_sl ) ) then
               fDG_CWT_Target = (fDG_CWT_T_su + fDG_CWT_T_sl)/2
            endif

            H(1)='You may specify the temperature at which the '
            H(2)='controller will attempt to maintain the cold  ' 
            H(3)='water tank. It must be set to a value between'
            H(4)='the tank`s lower and upper set point ' 
            write(H(5), '(A,F4.1,A,F4.1,A)') 
     &           'temperatures (',fDG_CWT_T_sl,'-',fDG_CWT_T_su,').'
                     
            call EASKR(
     &           fDG_CWT_Target,
     &           'Cold water tank target temperature ',
     &           '(deg C)',
     &           fDG_CWT_T_sl, 'F',
     &           fDG_CWT_T_su, 'F',
     &           (fDG_CWT_T_su + fDG_CWT_T_sl)/2.,
     &           'CWT Target temperature',IER,
     &           6)

C...........CWT proportional gain
            H(1)='The proportional gain describes the controller`s' 
            H(2)='sensitivity to diffeneces between the target '
            H(2)='temperature and the cold water tank`s actual  ' 
            H(3)='temperature. Higher values will produce a faster '
            H(5)='response, but may lead to controller instability.'
            H(6)='A value of 0.8 - 1.0 is recommended.'
            H(7)=' '           
            call EASKR(
     &           fDG_CWT_Kp,
     &           'Cold water tank proportional gain ',
     &           '(default = 0.8)',
     &           0.0, 'F',
     &           3.0, 'W',
     &           0.8,
     &           'CWT proportional gain',IER,
     &           7)



C...........CWT integral gain
            H(1)='The proportional gain describes the controller`s' 
            H(2)='sensitivity to historical diffeneces between the '
            H(2)='target temperature and the cold water tank`s actual' 
            H(3)='temperature. Higher values will ensure that the '
            H(5)='target temperature is eventually met, but may slow '
            H(6)='the controller`s response. A value of 0.00005 is '
            H(7)='recommended.'
            H(8)=' ' 

            call EASKR(
     &           fDG_CWT_Ki,
     &           'Cold water tank integral gain ',
     &           '(default= 0.00005)',
     &           0.0, 'F',
     &           0.1, 'W',
     &           0.00005,
     &           'CWT integral gain',IER,
     &           8)

 
C...........CWT derivative gain
            H(1)='The derivative gain describes the controller`s' 
            H(2)='sensitivity to instantanous changes in the cold water' 
            H(3)='tank`s actual temperature. Higher values will '
            H(4)='produce a faster controller, but may result in '
            H(5)='controller instability. A value of 360.0 is '
            H(6)='recommended.'
            H(7)=' ' 

            call EASKR(
     &           fDG_CWT_Kd,
     &           'Cold water tank derivative gain ',
     &           '(default=360.0)',
     &           0.0, 'F',
     &           10000.0, 'W',
     &           360.0,
     &           'CWT derivative gain',IER,
     &           7)



C...........CWT integral period
            H(1)='The integral period defines the time-frame over'
            H(2)='which the controller will be sensitive to historical'
            H(3)='differences between the actual and target '
            H(4)='temperatures. A value of 2 hours is recommended'
            H(5)=' ' 

            call EASKR(
     &           fDG_CWT_per_int,
     &           'Cold water tank integral period ',
     &           '(hours)',
     &           0.0, 'F',
     &           24.0, 'W',
     &           2.0,
     &           'CWT integral period',IER,
     &           5)


C...........CWT derivative period
            H(1)='The derivative period defines the time-frame over'
            H(2)='which the controller will be sensitive to rapid'
            H(3)='differences between the actual and target '
            H(4)='temperatures. A value of 0.5 hours is recommended'
            H(5)=' ' 

            call EASKR(
     &           fDG_CWT_per_der,
     &           'Cold water tank derivative period ',
     &           '(hours)',
     &           0.0, 'F',
     &           1.0, 'W',
     &           0.5,
     &           'CWT derivative period',IER,
     &           5)


         else
C...........Set dummy data as req'd                 
            fDG_CWT_T_su = fDG_CWT_T_sl + 10.
            fDG_CWT_Target = (fDG_CWT_T_su+ fDG_CWT_T_sl)/2.
            fDG_CWT_Kp = 0.0
            fDG_CWT_Ki = 0.0   
            fDG_CWT_Kd = 0.0
            fDG_CWT_per_int = 0.0
            fDG_CWT_per_der = 0.0
         endif
      else
C........Nix pointers for cold water tank
         iDG_CWT_index = -1
         iDG_CWT_node  = -1
      endif
      


C.....Determine if timestep data should be dumped to DG_controller.res 
C.....ascii file
      H(1)='The controller can optionally export time-step data '
      H(2)='into an ascii file (DG_controller.res) at run time.'
      H(3)=' ' 
      call EASKAB(
     &     'Should the controller output time step data?',
     &     ' ', 
     &     'Yes', 
     &     'No',
     &     iDG_misc_integer,
     &     3
     &     )
      
      if ( iDG_misc_integer .eq. 1 ) then
         iDG_TS_out = 1
      else
         iDG_TS_out = 0
      endif


C.....Now populate working array 'DG_configuration' with
C.....user responses.

C.....1. Nuke existing values in working array: user
C.....   may reconfigure controller, requiring less
C.....   data than previous configuration, and we don't
C.....   want the old data to hang around
      do iCount = 1, MISC
         fDG_configuration(iCount) = 0.0
      enddo

C.....2. Assign data that exists for all configurations
      fDG_configuration(2) = fDG_control_constant
      fDG_configuration(3) = real( iDG_tank_priority )
      fDG_configuration(4) = real( iDG_HWT_index     )
      fDG_configuration(5) = real( iDG_HWT_node      )
      fDG_configuration(6) = real( iDG_CWT_index     )
      fDG_configuration(7) = real( iDG_CWT_node      )

      iCount = 6                ! number of data assigned so far

C.....Parameters for hot water tank control 
      if ( bDG_HOT_control ) then
C........Heating season duration
         fDG_configuration(iCount+2)  = real( iDG_HS_start    )
         fDG_configuration(iCount+3)  = real( iDG_HS_end      )       
C........Control stragegy
         fDG_configuration(iCount+4)  = real( iDG_HS_strategy )

C........Tank setpoints
         fDG_configuration(iCount+5)  = fDG_HWT_T_sl 
         fDG_configuration(iCount+6)  = fDG_HWT_T_su

C........Target coefficient (0->1)
         fDG_configuration(iCount+7) = ( fDG_HWT_Target - fDG_HWT_T_sl ) 
     &        /  ( fDG_HWT_T_su - fDG_HWT_T_sl )

C........PID data
         fDG_configuration(iCount+8)  = fDG_HWT_Kp
         fDG_configuration(iCount+9)  = fDG_HWT_Ki
         fDG_configuration(iCount+10) = fDG_HWT_Kd

C........Integral/derivative periods
         fDG_configuration(iCount+11) = fDG_HWT_per_int
         fDG_configuration(iCount+12) = fDG_HWT_per_der

         iCount = iCount + 11

      endif

 
C.....Parameters for cold water tank control 
      if ( bDG_COLD_control ) then
C........Heating season duration
         fDG_configuration(iCount+2)  = real( iDG_CS_start    )
         fDG_configuration(iCount+3)  = real( iDG_CS_end      )       
C........Control stragegy
         fDG_configuration(iCount+4) = real( iDG_CS_strategy )

C........Tank setpoints
         fDG_configuration(iCount+5) = fDG_CWT_T_sl 
         fDG_configuration(iCount+6) = fDG_CWT_T_su

C........Target coefficient (0->1)
         fDG_configuration(iCount+7) = ( fDG_CWT_Target - fDG_CWT_T_sl ) 
     &        /  ( fDG_CWT_T_su - fDG_CWT_T_sl )

C........PID data
         fDG_configuration(iCount+8) = fDG_CWT_Kp
         fDG_configuration(iCount+9) = fDG_CWT_Ki
         fDG_configuration(iCount+10) = fDG_CWT_Kd

C........Integral/derivative periods
         fDG_configuration(iCount+11) = fDG_CWT_per_int
         fDG_configuration(iCount+12) = fDG_CWT_per_der

         iCount = iCount + 11
      endif     

C.....Timestep data flag
      fDG_configuration(iCount+2) = real( iDG_TS_out )
      iCount = iCount + 1

C.....Number of misc data items

      fDG_configuration(1) = real( iCount )

      END
