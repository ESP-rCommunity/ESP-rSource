
C.....Flags indicating which toggles are active.
      
      logical bUserGAM   ! User-specified building solution implicitness
      logical bUserSolar ! User-specified solar parameter                    
                        
C.....Values of user-specified variables
      real fUserGAM  
      integer iUserSolar       
      
                        
C.....Common containing user values
      COMMON/UserSpecSimToggles/
     &           bUserGAM, fUserGAM,
     &           bUserSolar, iUserSolar