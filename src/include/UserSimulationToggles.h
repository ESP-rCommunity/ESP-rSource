
C.....Flags indicating which toggles are active.
      
      logical bUserGAM   ! User-specified building solution implicitness
      logical bUserSolar ! User-specified solar parameter                    

      logical bUserPltSolvConfig ! Flag for custom plant 
                                 ! solution variables
                     
C.....Values of user-specified variables
      real fUserGAM  
      integer iUserSolar       
      
C.....Plant solution variables 
      
      integer iUserMAXITP    ! Max number of iterations
      integer iUserITRCLP    ! Max number of control iterations 
      real    fUserPERREL    ! Relative error (-)
      real    fUserPERTMP    ! Absolute temp error (oC)
      real    fUserPERFLX    ! Absolute heat flux error (W) 
      real    fUserPERMFL    ! Absolute mass flow error (kg/s)
      real    fUserH2iterTol ! Absolute errror in H2 mass flow (kg/s)
      
                        
C.....Common containing user flags, values 
      COMMON/UserSpecSimToggles/
     &           bUserGAM, fUserGAM,
     &           bUserSolar, iUserSolar,
     &           bUserPltSolvConfig
     
C.....Common containing plant solution data
      COMMON/UserPltSolnParams/
     &           iUserMAXITP    ,
     &           iUserITRCLP    ,
     &           fUserPERREL    ,
     &           fUserPERTMP    ,
     &           fUserPERFLX    ,
     &           fUserPERMFL    ,
     &           fUserH2iterTol
