C ROUTINES FOR TERMINAL COMMUNICATION VIA SCRATCH PAD

      SUBROUTINE PUT(X,Y,ICH)
C PUT CHARACTER ICH AT (X, Y) IN SCALED UNITS IF ON SCREEN

#include "viewer.h"
      integer ifs,itfs,imfs
      COMMON/GFONT/IFS,ITFS,IMFS
      character a*1
#ifdef OSI
      integer ipos,isize      ! for use with etlabel
      integer iupdown,isym    ! passed to etplot
#else
      integer*8 ipos,isize      ! for use with etlabel
      integer*8 iupdown,isym    ! passed to etplot
#endif

      IF(IPLN.EQ.0)RETURN
          IF(ABS(X*SCAL-XORG).GT.390.0.OR.
     &       ABS(Y*SCAL-YORG).GT.390.)RETURN
      iupdown=0
      isym=0
      CALL etplot(X,Y,iupdown,isym)
      a=char(ICH)
      ipos=0
      isize=ifs
      call etlabel(a,X,Y,ipos,isize)
      RETURN
      END

C--------------------------------------------------------------------
          SUBROUTINE SPD(A,N,X)
#include "viewer.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN,IEOUT

      character*(*) A
      character outs*80

      IF(IPLN.EQ.1)CALL PLAN
      write(outs,'(A,2x,F8.2)')A(1:lnblnk(A)),X
      call edisp(iuout,outs)

      RETURN
      END

C--------------------------------------------------------------

      SUBROUTINE SPS(A,N)
C   TO OUTPUT STRING ON SPAD2 WITH CONTROL

#include "viewer.h"
      COMMON/OUTIN/IUOUT,IUIN,IEOUT

      character*(*) A

      IF(IPLN.EQ.1)CALL PLAN
      call edisp(iuout,A)
      RETURN
      END
