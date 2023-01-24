C This file is part of the ESP-r system.
C Copyright Energy Systems Research Unit, University of
C Strathclyde, Glasgow Scotland, 2001.

C ESP-r is free software.  You can redistribute it and/or
C modify it under the terms of the GNU General Public
C License as published by the Free Software Foundation 
C (version 2 orlater).

C ESP-r is distributed in the hope that it will be useful
C but WITHOUT ANY WARRANTY; without even the implied
C warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
C PURPOSE. See the GNU General Public License for more
C details.

C You should have received a copy of the GNU General Public
C License along with ESP-r. If not, write to the Free
C Software Foundation, Inc., 59 Temple Place, Suite 330,
C Boston, MA 02111-1307 USA.

C *********************************************************************
C Egrid.F contains the following 1-D node distribution subroutines:
C  READ1D reads the 1D building gridding configuration file.
C  SAVE1D saves the 1D building gridding configuration file.
C  NDCONS automatically distributes nodes for a given construction
C         based on the thermophysical properties of layers.
C  NODTWO assignes two (default) nodes for each layer in the building.
C *********************************************************************

C **************************    READ1D    *****************************
C READ1D reads the 1D building gridding configuration file.
C *********************************************************************
      SUBROUTINE READ1D(FLGRID,IIN,IOUT,ITRC,IER)
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &             NPGAP(MCOM,MS,MGP)
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      COMMON/GR1D01/NNDS,NNDZ(MCOM),NNDC(MCOM,MS),NNDL(MCOM,MS,ME)
      COMMON/GR1D03/BCNV,ILUMP(MCOM,MS)

      CHARACTER FLGRID*72,OUTSTR*124

      IER=0

C Open existing ASCII construction data file as unit IIN.
      CALL EFOPSEQ(IIN,FLGRID,1,IER)
      IF(IER.LT.0)RETURN
      NNDS=0
      DO 10 ICOMP=1,NCOMP
        NNDZ(ICOMP)=0
        DO 20 I=1,NCONST(ICOMP)
          NNDC(ICOMP,I)=1
          NE=NELTS(ICOMP,I)
          CALL STRIPC(IIN,OUTSTR,99,ND,1,' construction grid ',IER)
          IF(IER.NE.0)GOTO 777
          K=0
          CALL EGETWI(OUTSTR,K,NDNM,-3,MN-1,'F','node/layer',IER)
          IF(IER.NE.0)THEN
            GOTO 777
          ELSEIF(NDNM.EQ.-3)THEN
            IF(NNDC(ICOMP,I).LE.0)NNDC(ICOMP,I)=2*NELTS(ICOMP,I)+1
            ILUMP(ICOMP,I)=NNDC(ICOMP,I)
            NNDC(ICOMP,I)=3
          ELSE
            IF(ND.NE.NE)THEN
              CALL EDISP(IOUT,'Error in the number of entities.')
              GOTO 777
            ENDIF
            ILUMP(ICOMP,I)=0
            K=0
            DO 30 J=1,NE
              CALL EGETWI(OUTSTR,K,NDNM,2,MN-1,'F','nodes',IER)
              IF(IER.NE.0)GOTO 777
              NNDL(ICOMP,I,J)=NDNM
              NNDC(ICOMP,I)=NNDC(ICOMP,I)+NNDL(ICOMP,I,J)

C If the number of nodes per construction exceeded the maximum allowable
C (i.e. MN) give warning message.
              IF(NNDC(ICOMP,I).GT.MN)THEN
                CALL EDISP(IOUT,'Large nodes/construction detected.')
                GOTO 777
              ENDIF
   30       CONTINUE
          ENDIF
          NNDZ(ICOMP)=NNDZ(ICOMP)+NNDC(ICOMP,I)
   20   CONTINUE
        NNDS=NNDS+NNDZ(ICOMP)
   10 CONTINUE
      IF(ITRC.NE.0)CALL EDISP(IOUT,
     & ' 1D node distribution configuration file read succesfuly !')
      CALL ERPFREE(IIN,ISTAT)
      RETURN
  777 NNDS=0
      DO 40 ICOMP=1,NCOMP
        CALL NODTWO(ICOMP)
        NNDS=NNDS+NNDZ(ICOMP)
   40 CONTINUE
      CALL ERPFREE(IIN,ISTAT)
      CALL USRMSG(' ',' Default 1D gridding is assumed.','W')
      RETURN
      END

C **************************    SAVE1D    *****************************
C SAVE1D saves the 1D building gridding configuration file.
C *********************************************************************
      SUBROUTINE SAVE1D(FLGRID,IIN,IOUT,ITRC,IER)
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &             NPGAP(MCOM,MS,MGP)
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      COMMON/GR1D01/NNDS,NNDZ(MCOM),NNDC(MCOM,MS),NNDL(MCOM,MS,ME)
      COMMON/GR1D03/BCNV,ILUMP(MCOM,MS)
      COMMON/OVRWT/AUTOVR
      logical AUTOVR

      CHARACTER FLGRID*72

      IER=0

C Open existing ASCII construction data file as unit IIN.
      if(AUTOVR)then
        CALL EFOPSEQ(IIN,FLGRID,3,IER)
      else
        CALL EFOPSEQ(IIN,FLGRID,4,IER)
      endif
      IF(IER.LT.0)RETURN
      DO 10 ICOMP=1,NCOMP
      DO 10 I=1,NCONST(ICOMP)
        IF(ILUMP(ICOMP,I).GT.0)THEN
          WRITE(IIN,'(A)')'    -3    # Lumped construction'
        ELSE
          WRITE(IIN,'(8I6)')(NNDL(ICOMP,I,J),J=1,NELTS(ICOMP,I))
        ENDIF
   10 CONTINUE
      IF(ITRC.NE.0)CALL EDISP(IOUT,
     &    '1D building gridding configuration file saved succesfuly.')
      CALL ERPFREE(IIN,ISTAT)
      RETURN
      END

C *****************************  NDCONS  ******************************
C NDCONS automatically distributes nodes for a given construction
C based on the thermophysical properties of layers.
C *********************************************************************
      SUBROUTINE NDCONS(ICOMP,I)
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &             NPGAP(MCOM,MS,MGP)
      COMMON/PREC15/RGAPS(MCOM,MS,MGP)
      COMMON/VTHP14/THRMLI(MCOM,MS,ME,7)
      COMMON/GR1D01/NNDS,NNDZ(MCOM),NNDC(MCOM,MS),NNDL(MCOM,MS,ME)
      COMMON/GR1D02/IADP,IVAR,FMIN,FREF,FMAX,NSTP
      COMMON/GR1D03/BCNV,ILUMP(MCOM,MS)

      CHARACTER MSG1*72,CONSTNM*25

C If this construction is lumped then return.
      IF(ILUMP(ICOMP,I).GE.1)RETURN
      TMR=3600./NSTP
      NGP=NGAPS(ICOMP,I)
      M=1

C Initialise the node distribution variables for this construction.
      NUMBR=1

C Calculate the Fourier number for each layer after assigning the number
C of nodes representing it.
      DO 10 J=1,NELTS(ICOMP,I)

C If this layer is an air gap.
        IF(M.LE.NGP)THEN
          IGP=NPGAP(ICOMP,I,M)
        ELSE
          IGP=-1
        ENDIF
        IF(IGP.EQ.J)THEN
          TERM=1.3*1005.5*RGAPS(ICOMP,I,M)/(TMR*THRMLI(ICOMP,I,J,4))
          M=M+1

C If this layer is a solid.
        ELSE
          TERM=THRMLI(ICOMP,I,J,2)*THRMLI(ICOMP,I,J,3)/
     &        (THRMLI(ICOMP,I,J,1)*TMR)
        ENDIF
        RNODE=THRMLI(ICOMP,I,J,4)*(FREF*TERM)**.5
        NNDL(ICOMP,I,J)=NINT(RNODE)
        IF(NNDL(ICOMP,I,J).LT.2)NNDL(ICOMP,I,J)=2
        FRRN=FLOAT(NNDL(ICOMP,I,J))**2./(THRMLI(ICOMP,I,J,4)**2.*TERM)

C If the Fourier number of the current layer is not within the acceptable
C range defined by the user, then try to adjust it by re-assigning the
C number of nodes for this layer.
C If the Fourier number for this layer is higher than the maximum allowable.
        IF(FRRN.GT.FMAX.AND.NNDL(ICOMP,I,J).GT.2)THEN
          NODEL=NNDL(ICOMP,I,J)-1
          TFRRN=NODEL**2./(THRMLI(ICOMP,I,J,4)**2.*TERM)
          IF(TFRRN.GE.FMIN.AND.TFRRN.LE.FMAX)THEN
            NNDL(ICOMP,I,J)=NODEL
          ELSE
            WRITE(MSG1,'(2A,3(I3,A),F14.4)')' WARNING: The Fourier ',
     &      'number for layer (',ICOMP,',',I,',',J,') is: ',FRRN
            CALL EDISP(IUOUT,MSG1)
          ENDIF
        ELSEIF(FRRN.GT.FMAX.AND.NNDL(ICOMP,I,J).EQ.2)THEN
          WRITE(MSG1,'(2A,3(I3,A),F14.4)')' WARNING: The Fourier ',
     &    'number for layer (',ICOMP,',',I,',',J,') is: ',FRRN
          CALL EDISP(IUOUT,MSG1)

C If the Fourier number for this layer is lower than the minimum allowable.
        ELSEIF(FRRN.LT.FMIN)THEN
          NODEL=NNDL(ICOMP,I,J)+1
          TFRRN=NODEL**2./(THRMLI(ICOMP,I,J,4)**2.*TERM)
          IF(TFRRN.GE.FMIN.AND.TFRRN.LE.FMAX)THEN
            NNDL(ICOMP,I,J)=NODEL
          ELSE
            WRITE(MSG1,'(2A,3(I3,A),F14.4)')' WARNING: The Fourier ',
     &      'number for layer (',ICOMP,',',I,',',J,') is: ',FRRN
            CALL EDISP(IUOUT,MSG1)
          ENDIF
        ENDIF
        NUMBR=NUMBR+NNDL(ICOMP,I,J)
   10 CONTINUE

C If the number of nodes per a construction exceeded the maximum allowable.
      IF(NUMBR.GT.MN)THEN
        CALL EDISP(IUOUT,' ')
        WRITE(CONSTNM,'(A,2(I3,A))')' construction (',ICOMP,',',I,').'
        WRITE(MSG1,'(2A)')' ERROR: The number of nodes in',CONSTNM
        CALL EDISP(IUOUT,MSG1) 
        WRITE(MSG1,'(A,I2,A)')'is more than the maximum (i.e.,',
     &        MN,') allowed.'
        CALL EDISP(IUOUT,MSG1)
        NUMBR=2*NELTS(ICOMP,I)+1
        DO 20 J=1,NELTS(ICOMP,I)
          NNDL(ICOMP,I,J)=2
   20   CONTINUE
        WRITE(MSG1,'(2A)')' Default 1D gridding assumed for',CONSTNM
        CALL USRMSG(' ',MSG1,'W')
      ENDIF

C Update the node distribution variables.
      NUMBRD=NUMBR-NNDC(ICOMP,I)
      NNDS=NNDS+NUMBRD
      NNDZ(ICOMP)=NNDZ(ICOMP)+NUMBRD
      NNDC(ICOMP,I)=NNDC(ICOMP,I)+NUMBRD
      RETURN
      END

C ******************************  NODTWO  ******************************
C NODTWO assignes two (default) nodes for each layer in the building.
C *********************************************************************
      SUBROUTINE NODTWO(ICOMP)
#include "building.h"

      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &             NPGAP(MCOM,MS,MGP)
      COMMON/GR1D01/NNDS,NNDZ(MCOM),NNDC(MCOM,MS),NNDL(MCOM,MS,ME)
      COMMON/GR1D03/BCNV,ILUMP(MCOM,MS)

      NNDZ(ICOMP)=0
      DO 10 IS=1,NCONST(ICOMP)
        ILUMP(ICOMP,IS)=0

C NNDC is the number of nodes per construction.
        NNDC(ICOMP,IS)=2*NELTS(ICOMP,IS)+1
        NNDZ(ICOMP)=NNDZ(ICOMP)+NNDC(ICOMP,IS)
        DO 20 IL=1,NELTS(ICOMP,IS)

C NNDL is the number of nodes per layer.
          NNDL(ICOMP,IS,IL)=2
   20   CONTINUE
   10 CONTINUE
      RETURN
      END
