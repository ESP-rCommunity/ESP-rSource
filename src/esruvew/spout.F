C ROUTINES FOR TERMINAL COMMUNICATION VIA SCRATCH PAD

      SUBROUTINE PUT(X,Y,ICH)
C PUT CHARACTER ICH AT (X, Y) IN SCALED UNITS IF ON SCREEN

#include "viewer.h"
      COMMON/GFONT/IFS,ITFS,IMFS
        character a*1

      IF(IPLN.EQ.0)RETURN
          IF(ABS(X*SCAL-XORG).GT.390.0.OR.
     &       ABS(Y*SCAL-YORG).GT.390.)RETURN
      CALL etplot(X,Y,0,0)
      a=char(ICH)
      call etlabel(a,X,Y,0,ifs)
      RETURN
      END

C--------------------------------------------------------------------
          SUBROUTINE SPD(A,N,X)

#include "viewer.h"
      COMMON/OUTIN/IUOUT,IUIN

      character*(*) A
      character outs*80

      IF(IPLN.EQ.1)CALL PLAN
      write(outs,'(A,2x,F8.2)')A(:lnblnk(A)),X
      call edisp(iuout,outs)

      RETURN
      END

C--------------------------------------------------------------

      SUBROUTINE SPS(A,N)
C   TO OUTPUT STRING ON SPAD2 WITH CONTROL

#include "viewer.h"
      COMMON/OUTIN/IUOUT,IUIN

      character*(*) A

      IF(IPLN.EQ.1)CALL PLAN
      call edisp(iuout,A)
      RETURN
      END
