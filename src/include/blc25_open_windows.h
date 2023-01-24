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

C-----------------------------------------------------------------------
C This common is only relevant to blc01_extended, a simple free cooling
C controller. 
C-----------------------------------------------------------------------
C Parameters for free cooling component of BCL01
      common /blc01/ bFreeCoolCtl, fFreeCoolDelivered
      logical bFreeCoolCtl(mcom)
      real fFreeCoolDelivered(mcom)
                                    
C Named constants
      real fMinDelTFreeCool         ! Minimum allowed temperature difference between indoor and outdoor temp
                                    ! to prevent excessive infiltration conductance values for free cooling. 
                                    
      parameter ( fMinDelTFreeCool = 2.0)
      
      real fCondFreeCool(mcom)      ! Conductivity assoc. with 
                                    ! free-cooling

