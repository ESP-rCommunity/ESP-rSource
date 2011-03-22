C-----------------------------------------------------------------------
C This file contains data structures used in the blc25 simple window 
C controller. Requires building.h.
C-----------------------------------------------------------------------

      logical bWindowsOpen(mcom)    ! Flag indicating if windows are
                                    ! open in each zone
      integer iAirFlowModel(mcom)   ! Flag indicating which flow method 
                                    ! should be used to characterize air
                                    ! exchange
                                    
      common /blc25/ bWindowsOpen, iAirFlowModel
      
C Named constants 
      integer iNoFlow                ! Flag for no model
      integer iAIM2model             ! Flag for AIM-2 model 
      integer iOPRschedule           ! Flag for air flow defined in .opr file 
      integer iFlowNetwork           ! Flag for air flow defined in 
                                     !   specific flow network.
                                     
      parameter ( iNoFlow      = 0, 
     &            iAIM2model   = 1, 
     &            iOPRschedule = 2,      
     &            iFlowNetwork = 3 )