C Dummy routine disabling h3k extentions in esrustats, which does not 
C use startup.f      
      logical function bH3KExtentionsActive()
      implicit none 

      bH3KExtentionsActive = .false. 

      return 
      end