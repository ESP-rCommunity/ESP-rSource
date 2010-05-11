
C.....Flags indicating which toggles are active.
      
      logical bUserGAM  ! User-specified building solution implicitness
                        
C.....Values of user-specified implicitness 
      real fUserGAM                        
                        
C.....Common containing user values
      COMMON/UserSpecSimToggles/
     &           bUserGAM, fUserGAM