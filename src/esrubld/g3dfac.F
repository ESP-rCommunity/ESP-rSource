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
C This file contains the following routines:

C INIT3D creates the files required in 3D simulation.
C FLORDR re-orders the 3D files for a zone.
C UPNODA updates internal and external surfaces nodes area.
C CFFVLU gets the value of a coefficient from its row and column numbers.
C UPDZMX updates the zone matrix (i.e. dynamic sparse storage).
C CHKGRD checks if the 3D zone models are assign the default 1D gridding.
C MZ3CFG is similar to MZCFGG but for 3D zone models.
C GETF12 returns the view factor for 3D surface node from 1D view factor.
C *********************************************************************

C ***************************    INIT3D    ****************************
C INIT3D creates the files required in 3D simulation.
C *********************************************************************
      SUBROUTINE INIT3D
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN

      COMMON/PREC1/SNA(MCOM,MS)
      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &             NPGAP(MCOM,MS,MGP)
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      common/rpath/path

      COMMON/GR3D100/BLDG3D,ZONE3D(MCOM)
      COMMON/GR3D101/NNODT(MCOM)
      COMMON/GR3D102/ASURFO(MCOM,MS),ASURFI(MCOM,MS)
      COMMON/GR3D108/L3DCVS(MCOM),L3DCNC(MCOM),L3DNDC(MCOM),L3DTAQ(MCOM)
      COMMON/GR3D109/L3DNDD(MCOM),L3DTLW(MCOM),L3DLWV(MCOM)
      COMMON/GR3D110/ICVS,ICNC,INDC,INDD,ITAQ,ITLW,ILWV,ITF3
      COMMON/GR3D111/NTND1(MCOM),NTND2(MCOM),N1ND1(MCOM,MS)
      COMMON/GRND100/GRND3D
      COMMON/GRND101/NNODTG,ITCNSTG,TEMP3G(MCOM),AREAGD(MCOM)
      COMMON/GRND108/LGDCVS,LGDCNC,LGDNDC,LGDTAQ,LGDNDD

      LOGICAL OK,dok,BLDG3D,ZONE3D,GRND3D

      CHARACTER*72 L3DCVS,L3DCNC,L3DNDC,L3DTAQ,L3DNDD,L3DTLW,L3DLWV
      CHARACTER*72 LGDCVS,LGDCNC,LGDNDC,LGDTAQ,LGDNDD
      CHARACTER MSG*82,L3DTMP*72,FLTYP*25,LTMP*72,path*72

      integer ier

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='INIT3D'  ! set for subroutine

      ZNP1=0.
      ZNF1=0.
      L3DTMP=' '
      DO 10 ICOMP=1,NCOMP
        NTND1(ICOMP)=0

C Skip the 1D zones.
        IF(.NOT.ZONE3D(ICOMP))GOTO 10

        L3DNDD(ICOMP)=' '
        L3DTLW(ICOMP)=' '
        L3DLWV(ICOMP)=' '
        ier=0
        CALL EFOPRAN(ICVS,L3DCVS(ICOMP),5,1,IER)
        ier=0
        CALL EFOPRAN(ICNC,L3DCNC(ICOMP),7,1,IER)
        IF(PATH.EQ.'./'.OR.PATH.EQ.' ')THEN
          WRITE(MSG,'(2A,I2,A)')'Do you want the associated 3D files ',
     &                         'for zone (',ICOMP,') to be re-ordered?'
          dok=.true.
          helptopic='re-order_3d_files'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL ASKOK(' ',MSG,OK,dok,nbhelp)
          IF(OK)CALL FLORDR(ICOMP)
        ENDIF
        CALL USRMSG(' ',
     &          ' Scaning associated 3D files... Please wait!','-')

C Create the nodes distribution file.
        FLTYP='nodes distribution'
        LTMP=L3DNDD(ICOMP)
        CALL FLNAME(ICOMP,LTMP,FLTYP,'.NDD',0,IER)
        L3DNDD(ICOMP)=LTMP
        ier=0
        CALL EFOPRAN(INDD,L3DNDD(ICOMP),4,4,IER)
        IF(IER.LT.0)then
          call edisp(iuout,'Error openning nodes distribution file.')
          call epwait
          call epagend
          STOP
        endif
        ICND=0
        MAXNOD=0
        IC=0
   20   IC=IC+1
        READ(ICVS,REC=IC,IOSTAT=ISTAT,ERR=6)ND1,VOLM1,DMM,IS1,IL1

C End of file reached.
        IF(ISTAT.LT.0)THEN
          WRITE(MSG,'(A,I2,A)')' Control volumes file for zone (',
     &                         ICOMP,') read successfully.'
          CALL EDISP(IUOUT,MSG)
          CALL ERPFREE(ICVS,ISTAT)
          CALL ERPFREE(INDD,ISTAT)
          NNODT(ICOMP)=MAXNOD+1
          IF(MAXNOD.GT.MNOD3)then
            call edisp(iuout,'Maximum number of nodes exceeded.')
            call epwait
            call epagend
            STOP
          endif
          GOTO 30
        ELSE
          MAXNOD=MAX0(MAXNOD,ND1)
        ENDIF

C Outside surface node.
        IF(IS1.GT.0.AND.IL1.EQ.1)THEN
          ICND=ICND+1
          WRITE(INDD,REC=ICND,ERR=8)ND1,IS1,IL1,VOLM1

C Inside surface node.
        ELSEIF(IS1.GT.0.AND.IL1.LT.0)THEN
          ICND=ICND+1
          WRITE(INDD,REC=ICND,ERR=8)ND1,IS1,IL1,VOLM1
          N1ND1(ICOMP,IS1)=N1ND1(ICOMP,IS1)+1

C Intra-construction node.
        ELSEIF(IS1.GT.0)THEN
          ICND=ICND+1
          WRITE(INDD,REC=ICND,ERR=8)ND1,IS1,IL1,VOLM1
        ENDIF

C Read next record in the control volumes file.
        GOTO 20

   30   FLTYP='total longwave'
        LTMP=L3DTLW(ICOMP)
        CALL FLNAME(ICOMP,LTMP,FLTYP,'.TLW',0,IER)
        L3DTLW(ICOMP)=LTMP
        ier=0
        CALL EFOPRAN(ITLW,L3DTLW(ICOMP),5,4,IER)
        IF(IER.LT.0)then
          call edisp(iuout,'Error while openning total longwave file.')
          call epwait
          call epagend
          STOP 
        endif

C Open the 3D temporary file.
        helptopic='3d_temp_file_name'
        call gethelptext(helpinsub,helptopic,nbhelp)
        FLTYP='3D temporary'
        CALL FLNAME(ICOMP,L3DTMP,FLTYP,'_TMP',nbhelp,IER)
        ier=0
        CALL EFOPRAN(ITF3,L3DTMP,3,4,IER)
        DO 40 IS=1,NCONST(ICOMP)
          ASURFO(ICOMP,IS)=0.
          ASURFI(ICOMP,IS)=0.
   40   CONTINUE
        INS=0
        IC=0
   50   IC=IC+1
        READ(ICNC,REC=IC,IOSTAT=ISTAT,ERR=7)ND1,DMM,ARA,DMM,ITP,IP1,IP2

C End of file reached.
        IF(ISTAT.LT.0)THEN
          WRITE(MSG,'(A,I2,A)')' Connections file for zone (',ICOMP,
     &                         ') read successfully.'
          CALL EDISP(IUOUT,MSG)
          DO 60 IS=1,NCONST(ICOMP)
            IF(ASURFO(ICOMP,IS).LT.1.E-8)ASURFO(ICOMP,IS)=1.E-8
            ASURFO(ICOMP,IS)=SNA(ICOMP,IS)/ASURFO(ICOMP,IS)
            IF(ASURFI(ICOMP,IS).LT.1.E-8)ASURFI(ICOMP,IS)=1.E-8
            ASURFI(ICOMP,IS)=SNA(ICOMP,IS)/ASURFI(ICOMP,IS)
   60     CONTINUE
          GOTO 110

C Connection with outside ambient (convective and radiative).
        ELSEIF(ITP.EQ.21.AND.IP1.EQ.ICOMP)THEN
          ASURFO(ICOMP,IP2)=ASURFO(ICOMP,IP2)+ARA

C Connection with inside air node (convective and radiative).
        ELSEIF(ITP.EQ.31.AND.IP1.EQ.ICOMP)THEN
          ASURFI(ICOMP,IP2)=ASURFI(ICOMP,IP2)+ARA
          INS=INS+1
          WRITE(ITF3,REC=INS,ERR=9)IP2,ND1,ARA

C Connection with outside or inside air node (convective and radiative)
C another zone.
        ELSEIF(ITP.EQ.21.OR.ITP.EQ.31)THEN

C Convective only connection with outside or inside ambient.
        ELSEIF(ITP.EQ.22.OR.ITP.EQ.32)THEN

C Connection with another interconstruction node within the 
C building (+ve IP1) or the ground (IP1 = -1).
        ELSEIF(ITP.EQ.11.AND.(IP1.EQ.-1.OR.
     &        (IP1.GT.0.AND.IP1.LE.NCOMP)))THEN

C Connection with a 1D node in zone (ITP-100) surface (IP1) node (IP2).
        ELSEIF(ITP.GT.100.AND.ITP.LE.(100+NCOMP))THEN
        ELSE
          call edisp(iuout,'Un-acceptable connection detected.')
          call epwait
          call epagend
          STOP 
        ENDIF
        GOTO 50

C Create the total longwave file from the 3D temporary file.
  110   IPSRF=100000000
        DO 70 I1=1,INS
          MNSRF=10000000
          MNODE=10000000
          DO 80 I2=1,INS
            READ(ITF3,REC=I2,ERR=9)IS2,ND2,AR2
            IF(IS2.EQ.MNSRF)THEN
              IF(ND2.LT.MNODE)THEN
                MNRC=I2
                MNODE=ND2
                ARAMN=AR2
              ELSEIF(ND2.EQ.MNODE)THEN
                call edisp(iuout,'Two interier nodes with same number.')
                call epwait
                call epagend
                STOP 
              ENDIF
            ELSEIF(IS2.LT.MNSRF)THEN
              MNRC=I2
              MNSRF=IS2
              MNODE=ND2
              ARAMN=AR2
            ENDIF
   80     CONTINUE
          NTND1(ICOMP)=NTND1(ICOMP)+1
          IF(MNSRF.NE.IPSRF)THEN
            IPSRF=MNSRF
            N1ND1(ICOMP,MNSRF)=NTND1(ICOMP)
          ENDIF
          WRITE(ITLW,REC=I1,ERR=11)MNODE,ARAMN,ZNP1,ZNF1,0

C Next step is to assure that the same node will not be selected again.
          WRITE(ITF3,REC=MNRC,ERR=9)100000000,100000000,ARAMN
   70   CONTINUE
        CALL EFDELET(ITF3,ISTAT)

C Create the longwave file from the total longwave file.
        FLTYP='longwave'
        LTMP=L3DLWV(ICOMP)
        helptopic='longwave_file_name'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL FLNAME(ICOMP,LTMP,FLTYP,'.LWV',nbhelp,IER)
        L3DLWV(ICOMP)=LTMP
        ier=0
        CALL EFOPRAN(ILWV,L3DLWV(ICOMP),4,4,IER)
        IF(IER.LT.0)STOP ' error while openning longwave file.'
        ITND2=0
        HRP21=0.
        HRF21=0.
        NC=NCONST(ICOMP)
        DO 210 ISF1=1,NCONST(ICOMP)
          IFND1=N1ND1(ICOMP,ISF1)
          IF(ISF1.EQ.NC)THEN
            ILND1=NTND1(ICOMP)
          ELSE
            ILND1=N1ND1(ICOMP,ISF1+1)-1
          ENDIF
          DO 220 IRC1=IFND1,ILND1
            READ(ITLW,REC=IRC1,ERR=11)ND1,AA1,ZNP1,ZNF1
            WRITE(ITLW,REC=IRC1,ERR=11)ND1,AA1,ZNP1,ZNF1,ITND2+1
            DO 230 ISF2=1,NCONST(ICOMP)
              IF(ISF2.EQ.ISF1)GOTO 230
              IFND2=N1ND1(ICOMP,ISF2)
              IF(ISF2.EQ.NC)THEN
                ILND2=NTND1(ICOMP)
              ELSE
                ILND2=N1ND1(ICOMP,ISF2+1)-1
              ENDIF
              DO 240 IRC2=IFND2,ILND2
                READ(ITLW,REC=IRC2,ERR=11)ND2
                ITND2=ITND2+1
                WRITE(ILWV,REC=ITND2,ERR=12)ND1,ND2,HRP21,HRF21
  240         CONTINUE
  230       CONTINUE
  220     CONTINUE
  210   CONTINUE
        NTND2(ICOMP)=ITND2
        CALL ERPFREE(ILWV,ISTAT)
        CALL ERPFREE(ITLW,ISTAT)
        CALL ERPFREE(ICNC,ISTAT)

C Open the temperature and energy file.
        ier=0
        CALL EFOPRAN(ITAQ,L3DTAQ(ICOMP),2,1,IER)

C Check that the number of records is at least more than the number of 
C nodes in the zone by one (for heat term).
        READ(ITAQ,REC=NNODT(ICOMP)+1,ERR=15)DMM,DMM
        CALL ERPFREE(ITAQ,ISTAT)
   10 CONTINUE

C Determine the number of nodes in the 3D ground model.
      IF(GRND3D)THEN
        NNODTG=0
        LGDNDD=' '
        ier=0
        CALL EFOPRAN(ICVS,LGDCVS,3,1,IER)
        IC=0
  500   IC=IC+1
        READ(ICVS,REC=IC,IOSTAT=ISTAT,ERR=6)NOD
        IF(ISTAT.EQ.0)THEN
          NNODTG=MAX0(NNODTG,NOD)
          GOTO 500
        ENDIF
        CALL ERPFREE(ICVS,ISTAT)
        ier=0
        CALL EFOPRAN(ICNC,LGDCNC,7,1,IER)
        FLTYP='nodes distribution'
        LTMP=LGDNDD
        helptopic='nodes_distr_file_name'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL FLNAME(-1,LTMP,FLTYP,'.NDD',nbhelp,IER)
        LGDNDD=LTMP
        ier=0
        CALL EFOPRAN(INDD,LGDNDD,3,4,IER)
        IR=0
        IC=1
        DO 510 ICOMP=1,NCOMP
          AREAGD(ICOMP)=0.
  510   CONTINUE

C Create the node distribution file for the ground.
  520   IC=IC+1
        READ(ICNC,REC=IC,IOSTAT=ISTAT,ERR=7)ND1,DMM,ARA,DMM,ITP,IP1,IP2
        IF(ISTAT.EQ.0)THEN
          IF(ITP.EQ.11.AND.IP1.GT.0.AND.IP1.LE.NCOMP)THEN
            IR=IR+1
            IZ3D=IP1
            WRITE(INDD,REC=IR,ERR=8)ND1,ARA,IZ3D
            AREAGD(IZ3D)=AREAGD(IZ3D)+ARA
          ELSEIF(ITP.GT.100.AND.ITP.LE.(100+NCOMP))THEN
            IR=IR+1
            IZ1D=(ITP-100)
            WRITE(INDD,REC=IR,ERR=8)ND1,ARA,IZ1D
            AREAGD(IZ1D)=AREAGD(IZ1D)+ARA
          ENDIF
          GOTO 520
        ENDIF
        CALL ERPFREE(ICNC,ISTAT)
        CALL ERPFREE(INDD,ISTAT)

C Check if the number of nodes in 3D ground model is acceptable.
        IF(NNODTG.GE.MNOD3)STOP ' number of nodes in ground is large.'

C Open the temperature and energy file and check the number of records.
        ier=0
        CALL EFOPRAN(ITAQ,LGDTAQ,2,1,IER)
        READ(ITAQ,REC=NNODTG+1,ERR=15)DMM,DMM
        CALL ERPFREE(ITAQ,ISTAT)
      ENDIF
      CALL USRMSG(' ',' ','-')
      RETURN
    6 WRITE(MSG,'(A,I6)')' Error occured at record number ',IC
      CALL USRMSG(MSG,' in the control volumes file.','W')
      STOP
    7 WRITE(MSG,'(A,I6)')' Error occured at record number ',IC
      CALL USRMSG(MSG,' in the connections file.','W')
      STOP
    8 CALL USRMSG(MSG,
     &           ' Error occured in the nodes distribution file.','W')
      STOP
    9 CALL USRMSG(MSG,' Error occured in the NODES file.','W')
      STOP
   11 CALL USRMSG(' ',
     &            ' Error occured  in the longwave file (1).','W')
      STOP
   12 WRITE(MSG,'(A,I6)')' Error occured at record number ',NTND2(ICOMP)
      CALL USRMSG(MSG,' in the longwave file (2).','W')
      STOP
   15 STOP ' Error occured in the temperature and heat file.'
      END

C **************************    FLORDR    *****************************
C FLORDR re-orders the 3D files for a zone.
C *********************************************************************
      SUBROUTINE FLORDR(IZ)
#include "building.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/GR3D108/L3DCVS(MCOM),L3DCNC(MCOM),L3DNDC(MCOM),L3DTAQ(MCOM)
      COMMON/GR3D110/ICVS,ICNC,INDC,INDD,ITAQ,ITLW,ILWV,ITF3
      COMMON/GR3D130/IFST(MNOD3),W3(MCFF3),NCLMN(MCFF3),NXT(MCFF3),SML3

      CHARACTER COPYFL*124,NTERM*1,FLTYP*25
      CHARACTER*72 L3DCVS,L3DCNC,L3DNDC,L3DTAQ,L3DTMP
      logical unixok
      integer ier

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='FLORDR'  ! set for subroutine

      CALL USRMSG(' ','Re-ordering in process. Please wait !','-')
      L3DTMP=' '
      NTERM=CHAR(0)
      INDC=INDD
      ier=0
      CALL EFOPRAN(INDC,L3DNDC(IZ),3,1,IER)

C Re-order the nodes (reduce the fill ins).
      DO 10 NODE=1,MNOD3
        NXT(NODE)=0
   10 CONTINUE
      LNODE=0
      IC=0
   20 IC=IC+1
      READ(ICNC,REC=IC,IOSTAT=ISTAT,ERR=1)ND1,DST,ARC,CND,ITP,IP1,IP2
      IF(ISTAT.LT.0)THEN
        GOTO 30

C Connection with another node within the same zone.
      ELSEIF(ITP.EQ.11.AND.IP1.EQ.IZ)THEN
        NXT(ND1)=NXT(ND1)+1
        NXT(IP2)=NXT(IP2)+1
        LNODE=MAX0(LNODE,ND1,IP2)

C Interier node with convective and radiative boundary.
      ELSEIF(ITP.EQ.31.AND.IP1.EQ.IZ)THEN
        NXT(ND1)=1000
        LNODE=MAX0(LNODE,ND1)

C Interier node with convective only boundary.
      ELSEIF(ITP.EQ.32.AND.IP1.EQ.IZ)THEN
        NXT(ND1)=NXT(ND1)+1
        LNODE=MAX0(LNODE,ND1)
      ENDIF
      GOTO 20
   30 L1000=MAX0(LNODE,1000)
      DO 40 I=1,L1000
        IFST(I)=0
        NCLMN(I)=0
   40 CONTINUE
      DO 50 I=1,LNODE
        NUM=NXT(I)
        IFST(NUM)=IFST(NUM)+1
   50 CONTINUE
      ICOUNT=1
      DO 60 I=1,L1000
        NCOUNT=IFST(I)
        IFST(I)=ICOUNT
        ICOUNT=ICOUNT+NCOUNT
   60 CONTINUE
      IFST(L1000+1)=ICOUNT
      DO 70 I=1,LNODE
        NUM=NXT(I)
        IND=IFST(NUM)
        IF(IND.EQ.IFST(NUM+1))THEN
          STOP ' Error detected while re-ordering 3D files.'
        ELSE
          NCLMN(IND)=I
          IFST(NUM)=IFST(NUM)+1
        ENDIF
   70 CONTINUE
      DO 80 I=1,LNODE
        NXT(NCLMN(I))=I
   80 CONTINUE

C Re-order the connections file.
      helptopic='3D_temporary_file'
      call gethelptext(helpinsub,helptopic,nbhelp)
      FLTYP='3D temporary'
      CALL FLNAME(IZ,L3DTMP,FLTYP,'_TMP',nbhelp,IER)
      call isunix(unixok)
      if(unixok)then
        WRITE(COPYFL,'(5a)')'cp ',L3DCNC(IZ)(1:lnblnk(L3DCNC(IZ))),
     &    ' ',L3DTMP(1:lnblnk(L3DTMP)),NTERM
      else
        WRITE(COPYFL,'(5a)')'copy /y ',
     &    L3DCNC(IZ)(1:lnblnk(L3DCNC(IZ))),
     &    ' ',L3DTMP(1:lnblnk(L3DTMP)),NTERM
      endif
      call runit(COPYFL,'-')
      ier=0
      CALL EFOPRAN(ITF3,L3DTMP,7,1,IER)
      IC=0
  100 IC=IC+1
      READ(ITF3,REC=IC,IOSTAT=ISTAT,ERR=1)ND1,DST,ARC,CND,ITP,IP1,IP2
      IF(ISTAT.LT.0)THEN
        CALL EFDELET(ITF3,ISTAT)
        call isunix(unixok)
        if(unixok)then
          WRITE(COPYFL,'(4A,A1)')'cp ',
     &      L3DCVS(IZ)(1:lnblnk(L3DCVS(IZ))),
     &      ' ',L3DTMP(1:lnblnk(L3DTMP)),NTERM
        else
          WRITE(COPYFL,'(4A,A1)')'copy /y ',
     &      L3DCVS(IZ)(1:lnblnk(L3DCVS(IZ))),
     &      ' ',L3DTMP(1:lnblnk(L3DTMP)),NTERM
        endif
        call runit(COPYFL,'-')
        ier=0
        CALL EFOPRAN(ITF3,L3DTMP,5,1,IER)
        IC=0
        GOTO 110
      ELSEIF(ITP.EQ.11.AND.IP1.EQ.IZ)THEN
        WRITE(ICNC,REC=IC,ERR=1)NXT(ND1),DST,ARC,CND,ITP,IP1,NXT(IP2)
      ELSE
        WRITE(ICNC,REC=IC,ERR=1)NXT(ND1),DST,ARC,CND,ITP,IP1,IP2
      ENDIF
      GOTO 100

C Re-order the control volumes file.
  110 IC=IC+1
      READ(ITF3,REC=IC,IOSTAT=ISTAT,ERR=1)ND,VOLM,DNHC,ISF,IL
      IF(ISTAT.LT.0)THEN
        CALL EFDELET(ITF3,ISTAT)
        call isunix(unixok)
        if(unixok)then
          WRITE(COPYFL,'(4A,A1)')'cp ',
     &      L3DNDC(IZ)(1:lnblnk(L3DNDC(IZ))),
     &      ' ',L3DTMP(1:lnblnk(L3DTMP)),NTERM
        else
          WRITE(COPYFL,'(4A,A1)')'copy /y ',
     &      L3DNDC(IZ)(1:lnblnk(L3DNDC(IZ))),
     &      ' ',L3DTMP(1:lnblnk(L3DTMP)),NTERM
        endif
        call runit(COPYFL,'-')
        ier=0
        CALL EFOPRAN(ITF3,L3DTMP,3,1,IER)
        IC=0
        GOTO 130
      ELSE
        WRITE(ICVS,REC=IC,ERR=1)NXT(ND),VOLM,DNHC,ISF,IL
      ENDIF
      GOTO 110

C Re-order the nodes file.
  130 IC=IC+1
      READ(ITF3,REC=IC,IOSTAT=ISTAT,ERR=1)XND,YND,ZND
      IF(ISTAT.LT.0)THEN
        NODTOT=IC-1
        CALL EFDELET(ITF3,ISTAT)
        CALL ERPFREE(INDC,ISTAT)
        WRITE(COPYFL,'(4A,A1)')'cp ',L3DTAQ(IZ)(1:lnblnk(L3DTAQ(IZ))),
     &                   ' ',L3DTMP(1:lnblnk(L3DTMP)),NTERM
        call runit(COPYFL,'-')
        ier=0
        CALL EFOPRAN(ITF3,L3DTMP,2,1,IER)
        CALL EFOPRAN(ITAQ,L3DTAQ(IZ),2,1,IER)
        IC=0
        GOTO 150
      ELSE
        IND=NXT(IC)
        WRITE(INDC,REC=IND,ERR=1)XND,YND,ZND
      ENDIF
      GOTO 130

C Re-order the nodes file.
  150 IC=IC+1
      READ(ITF3,REC=IC,IOSTAT=ISTAT,ERR=1)T3P,T3F
      IF(ISTAT.LT.0)THEN
        CALL EFDELET(ITF3,ISTAT)
        CALL ERPFREE(ITAQ,ISTAT)
        RETURN
      ELSE
        IF(IC.LE.NODTOT)THEN
          IND=NXT(IC)
        ELSE
          IND=IC
        ENDIF
        WRITE(ITAQ,REC=IND,ERR=1)T3P,T3F
      ENDIF
      GOTO 150
    1 CALL USRMSG(' ',
     &          ' Error detected while reading/writing to a file.','W')
      END

C *************************     UPNODA     ****************************
C UPNODA updates internal and external surfaces nodes area.
C *********************************************************************
      SUBROUTINE UPNODA(INODE,ARAEND)

      COMMON/GR3D110/ICVS,ICNC,INDC,INDD,ITAQ,ITLW,ILWV,ITF3

      IRCRD=0
   10 IRCRD=IRCRD+1
      READ(INDD,REC=IRCRD,IOSTAT=ISTAT,ERR=1)NODE,ISURFC,ILYR
      IF(ISTAT.LT.0)THEN
        RETURN
      ELSEIF(NODE.EQ.INODE)THEN
        WRITE(INDD,REC=IRCRD,ERR=2)NODE,ISURFC,ILYR,ARAEND
        RETURN
      ENDIF
      GOTO 10
    1 STOP 'error while reading nodes distribution file.'
    2 STOP 'error while writing to nodes distribution file.'
      END

C ************************    CFFVLU    *******************************
C CFFVLU gets the value of a coefficient from its row and column numbers.
C *********************************************************************
      SUBROUTINE CFFVLU(IROW,ICLMN,CVALU,IPRV,ICFF)
#include "building.h"

      COMMON/GR3D130/IFST(MNOD3),W3(MCFF3),NCLMN(MCFF3),NXT(MCFF3),SML3

      ICFF=IFST(IROW)
      IPRV=0
      IF(ICFF.EQ.0)THEN
        CVALU=0.
      ELSE
   10   ICOL=NCLMN(ICFF)
        IF(ICOL.EQ.ICLMN)THEN
          CVALU=W3(ICFF)
        ELSEIF(NXT(ICFF).GT.0)THEN
          IPRV=ICFF
          ICFF=NXT(ICFF)
          GOTO 10
        ELSE
          IPRV=ICFF
          ICFF=0
          CVALU=0.
        ENDIF
      ENDIF
      RETURN
      END

C **************************    UPDZMX    *****************************
C UPDZMX updates the zone matrix (i.e. dynamic sparse storage).
C *********************************************************************
      SUBROUTINE UPDZMX(IROW,ICLMN,CVALU,IPREV,ICOFF)
#include "building.h"

      COMMON/GR3D130/IFST(MNOD3),W3(MCFF3),NCLMN(MCFF3),NXT(MCFF3),SML3
      COMMON/GR3D131/IAIR,IPLT,IPRT,IAVL,NZMCF

C The coefficient location is known and existing.
      IF(ICOFF.GT.0)THEN

C Update the coefficient value.
        IF(ABS(CVALU).GT.SML3)THEN
          W3(ICOFF)=CVALU
          IPREV=ICOFF

C Delete existing coeffcient.
        ELSEIF(ICOFF.EQ.IFST(IROW))THEN
          IFST(IROW)=NXT(ICOFF)
          NXT(ICOFF)=IAVL
          IAVL=ICOFF
        ELSE
          NXT(IPREV)=NXT(ICOFF)
          NXT(ICOFF)=IAVL
          IAVL=ICOFF
        ENDIF

C New coefficient.
      ELSEIF(ABS(CVALU).GT.SML3)THEN
        ICOFF=IAVL
        IAVL=NXT(IAVL)
        IF(NXT(IAVL).EQ.0)THEN
          NXT(IAVL)=IAVL+1
          NZMCF=NXT(IAVL)
          IF(NZMCF.GE.MCFF3)THEN
            STOP ' maximum number of coefficients exceeded.'
          ENDIF
        ENDIF
        IF(IPREV.GT.0)THEN
          NXT(IPREV)=ICOFF
        ELSEIF(IPREV.EQ.0)THEN
          IFST(IROW)=ICOFF
        ENDIF
        NXT(ICOFF)=0
        NCLMN(ICOFF)=ICLMN
        W3(ICOFF)=CVALU
        IPREV=ICOFF
      ENDIF
      RETURN
      END

C ***************************    CHKGRD    ****************************
C CHKGRD checks if the 3D zone models are assign the default 1D gridding.
C *********************************************************************
      SUBROUTINE CHKGRD(GRDOK)
#include "building.h"

      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &             NPGAP(MCOM,MS,MGP)
      COMMON/GR1D01/NNDS,NNDZ(MCOM),NNDC(MCOM,MS),NNDL(MCOM,MS,ME)
      COMMON/GR1D03/BCNV,ILUMP(MCOM,MS)
      COMMON/GR3D100/BLDG3D,ZONE3D(MCOM)

      LOGICAL GRDOK,BLDG3D,ZONE3D

      DO 10 ICOMP=1,NCOMP
        IF(.NOT.ZONE3D(ICOMP))GOTO 10
        DO 20 IS=1,NCONST(ICOMP)
          IF(ILUMP(ICOMP,IS).GT.0)THEN
            GRDOK=.FALSE.
            RETURN
          ENDIF
        DO 20 IL=1,NELTS(ICOMP,IS)
          IF(NNDL(ICOMP,IS,IL).NE.2)THEN
            GRDOK=.FALSE.
            RETURN
          ENDIF
   20   CONTINUE
   10 CONTINUE
      GRDOK=.TRUE.
      RETURN
      END

C **************************    MZ3CFG    *****************************
C MZ3CFG is similar to MZCFGG but for 3D zone models.
C *********************************************************************
      SUBROUTINE MZ3CFG(ICOMP)
#include "building.h"

      COMMON/PREC1/SNA(MCOM,MS)
      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &              NPGAP(MCOM,MS,MGP)
      COMMON/PREC12/EI(MCOM,MS),EE(MCOM,MS),AI(MCOM,MS),
     &              AE(MCOM,MS)
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/V2/CFB(MCOM,MST,MST)
      COMMON/GR3D110/ICVS,ICNC,INDC,INDD,ITAQ,ITLW,ILWV,ITF3
      COMMON/GR3D111/NTND1(MCOM),NTND2(MCOM),N1ND1(MCOM,MS)
      COMMON/GR3D132/T3F(MNOD3)

C Set Stefan-Boltzmann constant.
      SBC=56.7E-9
      IDUM1=NTND1(ICOMP)+1
      IDUM2=NTND2(ICOMP)+1
      IRC=0
      NC=NCONST(ICOMP)

C Set future time values.
      DO 10 I1S=1,NC

C Define the nodes belonging to this surface.
        IFN1=N1ND1(ICOMP,I1S)
        IF(I1S.EQ.NC)THEN
          ILN1=NTND1(ICOMP)
        ELSE
          ILN1=N1ND1(ICOMP,I1S+1)-1
        ENDIF
        DO 20 IN1=IFN1,ILN1
          READ(ITLW,REC=IN1,ERR=1)ND1,A1,DMM,ZNF1,NNN
          T3F1=T3F(ND1)
          SUM=0.
          DO 30 I2S=1,NC
            IF(I2S.EQ.I1S)GOTO 30

C Define the nodes belonging to this surface.
            IFN2=N1ND1(ICOMP,I2S)
            IF(I2S.EQ.NC)THEN
              ILN2=NTND1(ICOMP)
            ELSE
              ILN2=N1ND1(ICOMP,I2S+1)-1
            ENDIF
            DO 40 IN2=IFN2,ILN2
              READ(ITLW,REC=IN2,ERR=1)ND2,A2
              T3F2=T3F(ND2)

C From IN2 to IN1 (/m2 of K).
              T1=T3F1+273.16
              T2=T3F2+273.16
              DTMP=ABS(T1-T2)
              IF(NSINC.EQ.1)THEN
                HRF12=5.7*CFB(ICOMP,I1S,I2S)
                HRF21=5.7*CFB(ICOMP,I2S,I1S)
                SUM=SUM+HRF21
              ELSEIF(DTMP.LT.1.E-5)THEN
                HRF12=0.
                HRF21=0.
              ELSE
                E1=EI(ICOMP,I1S)
                E2=EI(ICOMP,I2S)
                F12=GETF12(ICOMP,I1S,I2S,A2)
                F21=GETF12(ICOMP,I2S,I1S,A1)
                R1=1.-E1
                R2=1.-E2
                X1=E1*E2*(T2**4-T1**4)
                X5=T2-T1
                X6=1.-(R1*R2*(F12**2)*A1/A2)
                X7=(X1*SBC*F12*A1)/(X5*X6*A2)
                X8=0.
                DO 50 I3S=1,NC
                  IF(I3S.EQ.I1S.OR.I3S.EQ.I2S)GOTO 50
                  F13=CFB(ICOMP,I1S,I3S)
                  F23=CFB(ICOMP,I2S,I3S)
                  F32=CFB(ICOMP,I3S,I2S)*A2/SNA(ICOMP,I2S)
                  X9=(1.0-EI(ICOMP,I3S))*F13*F23
                  X10=1.-(R1*R2*(1.-EI(ICOMP,I3S))*F13*F32*F21)
                  X8=X8+X9/(SNA(ICOMP,I3S)*X10*X5)
   50           CONTINUE
                X8=X8*SBC*A1*X1
                HRF12=ABS(X7+X8)
                HRF21=HRF12*A2/A1
                SUM=SUM+HRF21
              ENDIF
              IRC=IRC+1
              READ(ILWV,REC=IRC,ERR=2)ND1,ND2,DMM,HRF
              WRITE(ILWV,REC=IRC,ERR=2)ND1,ND2,HRF,HRF21
              WRITE(ILWV,REC=IDUM2,ERR=2)0,0,0.0,0.0
   40       CONTINUE
   30     CONTINUE
          WRITE(ITLW,REC=IN1,ERR=1)ND1,A1,ZNF1,SUM,NNN
          WRITE(ITLW,REC=IDUM1,ERR=1)0,0.0,0.0,0.0,0
   20   CONTINUE
   10 CONTINUE
      RETURN
    1 STOP 'error in M3CFGG while reading/writing total longewave file.'
    2 STOP 'error in M3CFGG while reading/writing longewave file.'
      END

C *************************    GETF12    ******************************
C GETF12 returns the view factor for 3D surface node from 1D view factor.
C *********************************************************************
      FUNCTION GETF12(ICOMP,IS1,IS2,AREA2)
#include "building.h"

      COMMON/PREC1/SNA(MCOM,MS)
      COMMON/V2/CFB(MCOM,MST,MST)

      GETF12=CFB(ICOMP,IS1,IS2)*AREA2/SNA(ICOMP,IS2)
      RETURN
      END
