C-----------------------------------------------------------------------
C     mimo_ctl.h: Header file for MIMO controls.
C-----------------------------------------------------------------------

      common/MIMO_sensors/iSenCount, cSenName, iSenLoc, 
     &                    cSenProp
      
      common/MIMO_actuators/iActCount, cActName, iActLoc, fActMIN,
     &                      fActMAX   
     
      
!       common/MIMO_daytypes/
!       common/MIMO_periods/
!       common/MIMO_loops/
      
C.....MAX numbers of sensors, actuators, periods, day-types, loops...      
      integer mMIMOSen 
      parameter ( mMIMOSen = 24 ) 
      
      integer mMIMOAct 
      parameter ( mMIMOAct = 24 )       
      
      integer mMIMOLoc
      parameter ( mMIMOLoc = 4 )
      
      
C.....SENSOR parameters      
      integer       iSenCount                       ! # of sensors 
      character*24  cSenName(mMIMOSen)              ! Sensor names
      integer       iSenLoc(mMIMOSen,mMIMOLoc)      ! Sensor locatation data
      character*12  cSenProp(mMIMOSen)              ! Sensed peoperty
      
C.....Actuator parameters      
      integer       iActCount                       ! # of Actuators 
      character*24  cActName(mMIMOAct)              ! Actuator names
      integer       iActLoc(mMIMOAct,mMIMOLoc)      ! Actuator locatation data
      real          fActMAX(mMIMOAct)               ! Actuator max 
      real          fActMIN(mMIMOAct)               ! Actuator min