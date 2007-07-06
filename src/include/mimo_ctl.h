C-----------------------------------------------------------------------
C     mimo_ctl.h: Header file for MIMO controls.
C-----------------------------------------------------------------------

      common/MIMO_ctl/bMIMOActive,fMIMOVersion

      common/MIMO_sensors/iSenCount, cSenName, iSenNameLen, iSenLoc, 
     &                    cSenProp, cSenDomain, cSenObject
      
      common/MIMO_actuators/iActCount, cActName, iActLoc, cActVariable,
     &                      fActMIN, fActMAX, cActDomain, cActObject,
     &                      iActNameLen
     
      
      common/MIMO_daytypes/iDayCount, cDayName, iDayNameLen,
     &                     iDayStartDD, iDayStartMM, iDayStartYYYY, 
     &                     iDayEndDD,  iDayEnd MM,  iDayEndYYYY,
     &                     iDayStartDOY, iDayEndDOY

     
      
      common/MIMO_periods/iPerCount, cPerName, iPerStartHr, iPerEndHr,
     &                    iPerNameLen
      
      common/MIMO_loops/iLoopCount, cLoopName, cLoopLaw, iLoopLaw,
     &                  iLoopPriority, iLoopNameLen,
     &                  iLoopDayTypeCount, cLoopDayTypes, iLoopDayTypes,
     &                  iLoopSensorCount, cLoopSensors, iLoopSensors,
     &                      iLoopSensorDataCount, fLoopSensorData,
     &                  iLoopActuatorCount,cLoopActuators,
     &                      iLoopActuators, fLoopActuatorData,
     &                      iLoopActuatorDataCount,
     &                  iLoopDataCount, fLoopData


C.....MAX numbers of sensors, actuators, periods, day-types, loops...      
           
      integer mMIMOList
      parameter ( mMIMOList = 24 ) 
      
      integer mMIMOLoc
      parameter ( mMIMOLoc = 3 )
      
      integer mMIMOLoop
      parameter ( mMIMOLoop = 48 )
      
      integer mMIMOData 
      parameter ( mMIMOData = 64 )
      
      integer mMIMOControlLaws
      parameter ( mMIMOControlLaws = 1 )
      
      integer mMIMOSenData 
      parameter ( mMIMOSenData = 8 )
      
      integer mMIMOActData 
      parameter ( mMIMOActData = 8 )
      
C.....Version info 
      real fMIMOVersion                             ! Version #      
      logical bMIMOActive                           ! Flag for MIMO
      
C.....SENSOR parameters
      integer       iSenCount                       ! # of sensors 
      character*24  cSenName(mMIMOList)             ! Sensor names
      integer       iSenNameLen(mMIMOList)          ! Length of sensor names
      character*3   cSenDomain(mMIMOList)           ! Sensor domain
      character*12  cSenObject(mMIMOList)           ! Sensor object
      integer       iSenLoc(mMIMOList,mMIMOLoc)     ! Sensor locataion data
      character*12  cSenProp(mMIMOList)             ! Sensed peoperty
      
C.....Actuator parameters
      integer       iActCount                       ! # of Actuators 
      character*24  cActName(mMIMOList)             ! Actuator names
      integer       iActNameLen(mMIMOList)          ! Actuator name length
      character*3   cActDomain(mMIMOList)           ! Actuator domain 
      character*12  cActObject(mMIMOList)           ! Actuator object
      integer       iActLoc(mMIMOList,mMIMOLoc)     ! Actuator locataion data
      character*24  cActVariable(mMIMOList)         ! Actuated variable 
      real          fActMAX(mMIMOList)              ! Actuator max 
      real          fActMIN(mMIMOList)              ! Actuator min
      
C.....DAY Types
      integer iDayCount                             ! # days.
      character*24 cDayName(mMIMOList)              ! Daytype names
      integer iDayNameLen(mMIMOList)                ! Daytype name length
      integer iDayStartDD(mMIMOList)                ! Start day-of-month
      integer iDayStartMM(mMIMOList)                ! Start month
      integer iDayStartYYYY(mMIMOList)              ! Start year
      integer iDayStartDOY(mMIMOList)               ! Start day-of-year
      integer iDayEndDD(mMIMOList)                  ! End day-of-month
      integer iDayEndMM(mMIMOList)                  ! End month
      integer iDayEndYYYY(mMIMOList)                ! End year
      integer iDayEndDOY(mMIMOList)                 ! END day of year. 
      
C.....PERIODS
      integer iPerCount                             ! # periods
      character*24 cPerName(mMIMOList)              ! Period names
      integer iPerNameLen(mMIMOList)                ! Period name length
      integer iPerStartHr(mMIMOList)                ! Period start hour
      integer iPerEndHr(mMIMOList)                  ! Period end hour 
      
C.....LOOPS
      integer iLoopCount                            ! # Loops 
      character*24 cLoopName(mMIMOLoop)             ! Loop names
      integer iLoopNameLen(mMIMOLoop)               ! Loop name length
      character*24 cLoopLaw(mMIMOLoop)              ! Loop type (keyword)
      integer      iLoopLaw(mMIMOLoop)              ! Loop law index
      integer iLoopPriority(mMIMOLoop)              ! Priority of loop (1-10)
      
      integer iLoopDayTypeCount(mMIMOLoop)
      character*24 cLoopDayTypes(mMIMOLoop,mMIMOList) ! Daytype (keyword)       
      integer      iLoopDayTypes(mMIMOLoop,mMIMOList) ! Daytype (index)
      
      integer iLoopPeriodCount(mMIMOLoop)
      character*24 cLoopPeriods(mMIMOLoop,mMIMOList)   ! Periods (keyword)
      integer      iLoopPeriods(mMIMOLoop,mMIMOList)   ! Periods (index)
      
      integer iLoopSensorCount(mMIMOLoop)
      character*24 cLoopSensors(mMIMOLoop,mMIMOList)   ! Loop sensors (keyword)
      integer      iLoopSensors(mMIMOLoop,mMIMOList)   ! Loop sensors (index)
      integer iLoopSensorDataCount(mMIMOLoop,mMIMOList)! # of sensor-specific data 
      real fLoopSensorData(mMIMOLoop, mMIMOList, mMIMOSenData)    ! Sensor-specific set-points
      
      integer iLoopActuatorCount(mMIMOLoop)
      character*24 cLoopActuators(mMIMOLoop,mMIMOList) ! Loop Actuators (keyword)
      integer      iLoopActuators(mMIMOLoop,mMIMOList) ! Loop Actuators (index)
      integer iLoopActuatorDataCount(mMIMOLoop,mMIMOList) ! # of actuator-specific data 
      real fLoopActuatorData(mMIMOLoop,mMIMOList,mMIMOActData)  ! Actuator-specific set-points
      
      integer iLoopDataCount(mMIMOLoop)               ! # of data in loop
      real    fLoopData(mMIMOLoop,mMIMOData)          ! Data associated with loop
      
C.....CONTROLS
      character*32 cControlLawNames(mMIMOControlLaws)   ! Names of supported controls
      integer iMIMOLawCount                     ! number of supported controls
      parameter ( iMIMOLawCount = 1 )
      
      data cControlLawNames / 'MIMO_OnOff' /
      
      
C.....Misc varaibles

      character*124 cContext          ! Message buffers                                
      character*124 cMsg              !
