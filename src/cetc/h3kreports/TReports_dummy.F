C       TReports_dummy.F
C
C       Created by Alex Ferguson
C       Created on March 19, 2004
      
C       This file contains stubs required to make bldh3k and plt without
C       the C++ reports manager.

C Logical function indicating that H3KREPORTS is not available  
      LOGICAL FUNCTION REP_XMLSTATUS()
      REP_XMLSTATUS = .FALSE.
      RETURN
      END
  
      SUBROUTINE REP_SET_META(DUMMY1, DUMMY2, DUMMY3)
      CHARACTER*(*) DUMMY1
      CHARACTER DUMMY2, DUMMY3

        ! Do nothing
        
      RETURN  
      END 

      SUBROUTINE REP_REPORT(DUMMY1, DUMMY2)
      CHARACTER*(*) DUMMY2
      REAL DUMMY1

        ! Do nothing
        
      RETURN  
      END 
      
      SUBROUTINE ADD_TO_XML_REPORTING(DUMMY1, DUMMY2, DUMMY3, DUMMY4,
     &  DUMMY5)
      CHARACTER*(*) DUMMY2, DUMMY3, DUMMY4
      CHARACTER*(*) DUMMY5
      REAL DUMMY1

        ! Do nothing
        
      RETURN  
      END 


      SUBROUTINE REP_UPDATE(DUMMY1, DUMMY2, DUMMY3)
      REAL DUMMY2
      INTEGER DUMMY1, DUMMY3

        ! Do nothing
        
      RETURN  
      END 

 
      SUBROUTINE REP_SUMMARY()
      
        ! Do nothing
        
      RETURN  
      END 
      
      SUBROUTINE REP_XML_SUMMARY()
      
        ! Do nothing
        
      RETURN  
      END 
     
      SUBROUTINE REP_CLEANUP_FILES()
      
        ! Do nothing
        
      RETURN  
      END 

      SUBROUTINE REP_SET_PARAMETER(DUMMY1, DUMMY2)
      CHARACTER*(*) DUMMY2, DUMMY1
           
      Return
      end


      SUBROUTINE config_h3k_reports_menu()
      
      return
      end

      logical function bH3K_rep_enabled()
      bH3K_rep_enabled = .false.
      return 
      end

