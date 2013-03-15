C This file contains a dummy routines that warns the user when 
C CETC code has been called by a ESP-r binary that is not 
C supproted.
      
      SUBROUTINE CETC_error_notice(cMSG)
      IMPLICIT NONE

      COMMON/OUTIN/IUOUT,IUIN
      INTEGER  IUOUT,IUIN
      CHARACTER*124 cMSG
      
      integer lnblnk
      
      CALL EDISP(IUOUT,cMSG(1:lnblnk(cMSG)))
      RETURN
      END
