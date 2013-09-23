C-----------------------------------------------------------------------
C This file contains data structures used in the blc25 simple window 
C controller. Requires building.h.
C-----------------------------------------------------------------------

      logical bWindowsOpen(mcom)    ! Flag indicating if windows are
                                    ! open in each zone
      integer iAirFlowModel(mcom)   ! Flag indicating which flow method 
                                    ! should be used to characterize air
                                    ! exchange
                                    
      logical bBCL25FlowControlsActive(mcnn) ! Flag indicating blc25 
                                             ! or similar control is 
                                             ! taking over flow mfs 
                                             ! controls for this zone 
                              
      real fBCL25FlowControlSignal(mcnn)     ! Control position determined
                                             ! by blc25 (0->1)      

      integer iBCL25Mode(MCOM) 
                                             
      common /bcl25/ bWindowsOpen, 
     &               iAirFlowModel, 
     &               bBCL25FlowControlsActive,
     &               fBCL25FlowControlSignal,
     &               iBCL25Mode 
      
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

C Parameters for free cooling component of BCL01
      common /blc01/ fCondFreeCool, fFreeCoolDelivered
      
      real fFreeCoolDelivered(mcom)
      
      real fCondFreeCool(mcom)      ! Conductivity assoc. with 
                                    ! free-cooling
                                    

 
      integer iHeating
      integer iCooling 
      integer iFreeFloatWindowsClosed
      integer iFreeFloatWindowsOpen
      parameter ( iHeating                = 0, 
     &            iCooling                = 1, 
     &            iFreeFloatWindowsClosed = 2, 
     &            iFreeFloatWindowsOpen   = 3 )
    
