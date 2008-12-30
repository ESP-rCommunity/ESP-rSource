
C RTC_data.h
C
C This file contains variable definitions and shared memory associated 
C witht the run-time coupling facility

C......Flag indicating simple demo coupling defined
      logical bRTCDemoDefined
      character*256 cRTCDemoInfo
      
      common /RTCDemo/ bRTCDemoDefined, cRTCDemoInfo