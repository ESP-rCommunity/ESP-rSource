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

C `CTLWRT` Takes configuration control common block data and saves to
C an ascii file.
      SUBROUTINE CTLWRT(IUNIT,IER)
#include "building.h"
#include "plant.h"
#include "net_flow.h"
#include "control.h"

      COMMON/OUTIN/IUOUT,IUIN
      common/c1/ncomp,ncon
      common/cctlnm/ctldoc,lctlf
      common/cctl/icascf(mcom)
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      common/ctlnm/znctldoc,plctldoc,flctldoc,elctldoc,glctldoc,
     &             opticdoc
      common/bctl/ncf,ibsn(mcf,4),iban(mcf,3),nbcdt(mcf),
     &       ibcdv(mcf,mbcdt,2),nbcdp(mcf,mbcdt),tbcps(mcf,mbcdt,mbcdp),
     &       ibctyp(mcf,mbcdt,mbcdp),ibclaw(mcf,mbcdt,mbcdp),
     &       bmiscd(mcf,mbcdt,mbcdp,misc)
      common/pctl/ncl,ipsn(mcl,5),ipan(mcl,4),npcdt(mcl),
     &       ipcdv(mcl,mpcdt,2),npcdp(mcl,mpcdt),tpcps(mcl,mpcdt,mpcdp),
     &       ipctyp(mcl,mpcdt,mpcdp),ipclaw(mcl,mpcdt,mpcdp),
     &       pmiscd(mcl,mpcdt,mpcdp,misc)
      common/fctl/ncc,ifsn(mcc,4),ifan(mcc,3),nfcdt(mcc),
     &       ifcdv(mcc,mfcdt,2),nfcdp(mcc,mfcdt),tfcps(mcc,mfcdt,mfcdp),
     &       ifctyp(mcc,mfcdt,mfcdp),ifclaw(mcc,mfcdt,mfcdp),
     &       fmiscd(mcc,mfcdt,mfcdp,misc)
      common/fctl4/iasocc(mcc,mcmp),nfsup(mcc)

      common/gctl/ngf,igsn(mgl,4),igan(mgl,3),ngcdt(mgl),
     &       igcdv(mgl,mgcdt,2),ngcdp(mgl,mgcdt),tgcps(mgl,mgcdt,mgcdp),
     &       igctyp(mgl,mgcdt,mgcdp),igclaw(mgl,mgcdt,mgcdp),
     &       gmiscd(mgl,mgcdt,mgcdp,misc)
      common/octl/nof,iosn(mol,4),ioan(mol,3),nocdt(mol),
     &       iocdv(mol,mocdt,2),nocdp(mol,mocdt),tocps(mol,mocdt,mocdp),
     &       ioctyp(mol,mocdt,mocdp),ioclaw(mol,mocdt,mocdp),
     &       omiscd(mol,mocdt,mocdp,misco)
      COMMON/MFLOW2/NDNAM(0:MNOD)
      COMMON/MFLOW8/CMNAM(MCMP),LTPCMP(MCMP)
      COMMON/MFLW10/NODPS(MCNN),HGTPS(MCNN),NODNE(MCNN),HGTNE(MCNN),
     &              ITPCON(MCNN),NDSCNN(MCNN,MCNS)
      COMMON/Vld20/Vldtng

      CHARACTER lctlf*72,ctldoc*248
      character outs*248,outsd*248
      CHARACTER LTPCMP*60,CMNAM*12,NDNAM*12
      character DS*7,DS1*10,DE*7,DE1*10,SSTR*72,SSTR1*72,SSTR2*248
      character znctldoc*248,plctldoc*248,flctldoc*248,elctldoc*248
      character glctldoc*248,opticdoc*248
      logical Vldtng

      IER=0

C Initialise control file.
      if(Vldtng)then
        CALL EFOPSEQ(IUNIT,LCTLF,3,IER)
      else
        CALL EFOPSEQ(IUNIT,LCTLF,4,IER)
      endif
      IF(IER.LT.0)THEN
        IER=1
        RETURN
      ENDIF

C Write data to file.
      LN=max(1,LNBLNK(ctldoc))
      WRITE(IUNIT,'(A)',IOSTAT=ios,ERR=1)CTLDOC(1:ln)
      IF(ctldoc(1:4).EQ.'NONE')GOTO 12
      IF(NCF.EQ.0)goto 2

C Building control information.
      WRITE(IUNIT,'(A)',IOSTAT=ios,ERR=1) '* Building'
      LN=max(1,LNBLNK(znctldoc))
      WRITE(IUNIT,'(A)',IOSTAT=ios,ERR=1)znctldoc(1:ln)

      WRITE(IUNIT,'(I4,A)',IOSTAT=ios,ERR=1)NCF,'  # No. of functions'

      DO 10 I=1,NCF
        II=I
        WRITE(IUNIT,'(A)',IOSTAT=ios,ERR=1)'* Control function'
        CALL EVCNTRL(0,II,1,1,'S',SSTR)
        LN=max(1,LNBLNK(SSTR))
        WRITE(IUNIT,'(A,A)',IOSTAT=ios,ERR=1)'# ',SSTR(1:ln)
        WRITE(IUNIT,'(4I5,A)',IOSTAT=ios,ERR=1)(IBSN(II,J),J=1,4),
     &  '  # sensor data'
        CALL EVCNTRL(0,II,1,1,'A',SSTR)
        LN=max(1,LNBLNK(SSTR))
        WRITE(IUNIT,'(A,A)',IOSTAT=ios,ERR=1)'# actuates ',
     &     SSTR(1:ln)
        WRITE(IUNIT,'(3I5,A)',IOSTAT=ios,ERR=1)(IBAN(II,J),J=1,3),
     &  '  # actuator data'
        WRITE(IUNIT,'(I5,A)',IOSTAT=ios,ERR=1)NBCDT(II),
     &  ' # No. day types'
        NN=NBCDT(II)
        IF(NN.EQ.0)NN=3

        DO 20 J=1,NN
          JJ=J
          CALL STDATE(IYEAR,IBCDV(II,JJ,1),DS,DS1)
          CALL STDATE(IYEAR,IBCDV(II,JJ,2),DE,DE1)
          write(outs,'(4a)') '  # valid ',DS1,' - ',DE1
          WRITE(IUNIT,'(2I5,a)',IOSTAT=ios,ERR=1)(IBCDV(II,JJ,K),K=1,2),
     &      outs(1:lnblnk(outs))
          WRITE(IUNIT,'(I6,A)',IOSTAT=ios,ERR=1)NBCDP(II,JJ),
     &      '  # No. of periods in day'
          LL=NBCDP(II,JJ)

          DO 30 K=1,LL
            KK=K
            CALL EVCNTRLAW(0,II,JJ,KK,SSTR,SSTR2)
            LN=max(1,LNBLNK(SSTR))
            WRITE(IUNIT,'(2I5,F8.3,3A)',IOSTAT=ios,ERR=1)
     &        IBCTYP(II,JJ,KK),IBCLAW(II,JJ,KK),TBCPS(II,JJ,KK),
     &        '  # ctl type, law (',SSTR(1:ln),'), start @'
    9       FORMAT(2I5,F8.3)
            WRITE(IUNIT,'(F8.0,A)',IOSTAT=ios,ERR=1)BMISCD(II,JJ,KK,1),
     &        '  # No. of data items'
   11       FORMAT(F8.1)
            NITEMS=INT(BMISCD(II,JJ,KK,1))+1
            IF(NITEMS.EQ.1)GOTO 30

C Write control data and compact, if very long then use two lines.
            if(NITEMS.lt.20)then
              WRITE(outs,'(20f11.3)',IOSTAT=ios,ERR=1)
     &          (BMISCD(II,JJ,KK,L),L=2,NITEMS)
              call SDELIM(outs,outsd,'S',IW)
              write(IUNIT,'(2x,a)')outsd(1:lnblnk(outsd))
            else
              WRITE(outs,'(20f11.3)',IOSTAT=ios,ERR=1)
     &          (BMISCD(II,JJ,KK,L),L=2,19)
              call SDELIM(outs,outsd,'S',IW)
              write(IUNIT,'(2x,a)')outsd(1:lnblnk(outsd))
              WRITE(outs,'(20f11.3)',IOSTAT=ios,ERR=1)
     &          (BMISCD(II,JJ,KK,L),L=20,nitems)
              call SDELIM(outs,outsd,'S',IW)
              write(IUNIT,'(2x,a)')outsd(1:lnblnk(outsd))
            endif
   30     CONTINUE
   20   CONTINUE
   10 CONTINUE

C Write function to zone links as a packed string.
      WRITE(IUNIT,'(A)',IOSTAT=ios,ERR=1) '# Function:Zone links'
      ipos=1
      call ailist(ipos,ncomp,ICASCF,MCOM,'C',outs,loutlen,itrunc)
      write(IUNIT,'(1x,a)',IOSTAT=ios,ERR=1) outs(1:loutlen)

C Plant.
    2 IF(NCL.EQ.0)GOTO 3
      WRITE(IUNIT,'(A)',IOSTAT=ios,ERR=1) '* Plant'

C Plant control information.
      WRITE(IUNIT,'(A)',IOSTAT=ios,ERR=1)plctldoc(1:lnblnk(plctldoc))
      WRITE(IUNIT,'(I4,A)',IOSTAT=ios,ERR=1)NCL,'  # No. of loops'
     
      DO 40 I=1,NCL
        II=I
        WRITE(IUNIT,'(A)',IOSTAT=ios,ERR=1) '* Control loops'
        CALL EVCNTRL(1,II,1,1,'S',SSTR)
        LN=max(1,LNBLNK(SSTR))
        WRITE(IUNIT,'(2A)',IOSTAT=ios,ERR=1)'# ',SSTR(1:ln)
        WRITE(IUNIT,'(5I5,A)',IOSTAT=ios,ERR=1)(IPSN(II,J),J=1,5),
     &  '  # sensor '
        CALL EVCNTRL(1,II,1,1,'A',SSTR)
        LN=max(1,LNBLNK(SSTR))
        WRITE(IUNIT,'(2A)',IOSTAT=ios,ERR=1)'# ',SSTR(1:ln)
        WRITE(IUNIT,'(4I5,A)',IOSTAT=ios,ERR=1)(IPAN(II,J),J=1,4),
     &  '  # actuator '
        WRITE(IUNIT,'(I5,A)',IOSTAT=ios,ERR=1)NPCDT(II),
     &  ' # No. day types'
        NN=NPCDT(II)
        IF(NN.EQ.0)NN=3

        DO 50 J=1,NN
          JJ=J
          CALL STDATE(IYEAR,IPCDV(II,JJ,1),DS,DS1)
          CALL STDATE(IYEAR,IPCDV(II,JJ,2),DE,DE1)
          write(outs,'(4a)') '  # valid ',DS1,' - ',DE1
          WRITE(IUNIT,'(2I5,a)',IOSTAT=ios,ERR=1)(IPCDV(II,JJ,K),K=1,2),
     &      outs(1:lnblnk(outs))
          WRITE(IUNIT,'(I6,A)',IOSTAT=ios,ERR=1)NPCDP(II,JJ),
     &      '  # No. of periods in day'
          LL=NPCDP(II,JJ)

          DO 60 K=1,LL
            KK=K
            CALL EVCNTRLAW(1,II,JJ,KK,SSTR,SSTR2)
            WRITE(IUNIT,'(2I5,F8.3,3a)',IOSTAT=ios,ERR=1)
     &        IPCTYP(II,JJ,KK),IPCLAW(II,JJ,KK),TPCPS(II,JJ,KK),
     &        '  # ctl type, law (',SSTR(1:lnblnk(SSTR)),'), start @'
            WRITE(IUNIT,'(F8.0,A)',IOSTAT=ios,ERR=1)PMISCD(II,JJ,KK,1),
     &        '  # No. of data items'
            NITEMS=INT(PMISCD(II,JJ,KK,1))+1
            IF(NITEMS.EQ.1)goto 60

C Write control data and compact, if very long then use two lines.
            if(NITEMS.lt.19)then
              WRITE(outs,'(19f13.5)',IOSTAT=ios,ERR=1)
     &          (PMISCD(II,JJ,KK,L),L=2,NITEMS)
              call SDELIM(outs,outsd,'S',IW)
              write(IUNIT,'(2x,a)')outsd(1:lnblnk(outsd))
            else
              WRITE(outs,'(19f13.5)',IOSTAT=ios,ERR=1)
     &          (PMISCD(II,JJ,KK,L),L=2,18)
              call SDELIM(outs,outsd,'S',IW)
              write(IUNIT,'(2x,a)')outsd(1:lnblnk(outsd))
              WRITE(outs,'(19f13.5)',IOSTAT=ios,ERR=1)
     &          (PMISCD(II,JJ,KK,L),L=19,nitems)
              call SDELIM(outs,outsd,'S',IW)
              write(IUNIT,'(2x,a)')outsd(1:lnblnk(outsd))
            endif
   60     CONTINUE
   50   CONTINUE
   40 CONTINUE

C Flow.
    3 IF(NCC.EQ.0)GOTO 4
      WRITE(IUNIT,'(A)',IOSTAT=ios,ERR=1) '* Mass Flow'

C Flow control information.
      LN=max(1,LNBLNK(flctldoc))
      WRITE(IUNIT,'(A)',IOSTAT=ios,ERR=1)flctldoc(1:ln)
      WRITE(IUNIT,'(I4,A)',IOSTAT=ios,ERR=1)NCC,'  # No. of controls'
     
      DO 401 I=1,NCC
        II=I
        WRITE(IUNIT,'(A)',IOSTAT=ios,ERR=1)'* Control mass'
        CALL EVCNTRL(2,II,1,1,'S',SSTR)
        LN=max(1,LNBLNK(SSTR))
        WRITE(IUNIT,'(A,A)',IOSTAT=ios,ERR=1)'# ',SSTR(1:ln)
        WRITE(IUNIT,'(4I5,A)',IOSTAT=ios,ERR=1)(IFSN(II,J),J=1,4),
     &  '  # sensor data'
        CALL EVCNTRL(2,II,1,1,'A',SSTR)
        LN=max(1,LNBLNK(SSTR))
        WRITE(IUNIT,'(A,A)',IOSTAT=ios,ERR=1)'# actuates ',
     &     SSTR(1:ln)
        WRITE(IUNIT,'(3I5,A)',IOSTAT=ios,ERR=1)(IFAN(II,J),J=1,3),
     &  '  # actuator data'
        WRITE(IUNIT,'(I5,A)',IOSTAT=ios,ERR=1)NFCDT(II),
     &  ' # No. day types'
        NN=NFCDT(II)
        IF(NN.EQ.0)NN=3

        DO 501 J=1,NN
          JJ=J
          CALL STDATE(IYEAR,IFCDV(II,JJ,1),DS,DS1)
          CALL STDATE(IYEAR,IFCDV(II,JJ,2),DE,DE1)
          write(outs,'(4a)') '  # valid ',DS1,' - ',DE1
          WRITE(IUNIT,'(2I5,a)',IOSTAT=ios,ERR=1)(IFCDV(II,JJ,K),K=1,2),
     &      outs(1:lnblnk(outs))
          WRITE(IUNIT,'(I6,A)',IOSTAT=ios,ERR=1)NFCDP(II,JJ),
     &      '  # No. of periods in day'
          LL=NFCDP(II,JJ)

          DO 601 K=1,LL
            KK=K

C In the case of mass flow the controller type is the same for
C each period. Include the verbose decoding of data in the comment line.
            CALL EVCNTRL(2,II,JJ,KK,'T',SSTR)
            LN=max(1,LNBLNK(SSTR))
            CALL EVCNTRLAW(2,II,JJ,KK,SSTR1,SSTR2)
            LN1=max(1,LNBLNK(SSTR1))
            LN2=max(1,LNBLNK(SSTR2))
            WRITE(IUNIT,'(2I5,F8.3,5A)',IOSTAT=ios,ERR=1)
     &        IFCTYP(II,1,1),IFCLAW(II,JJ,KK),TFCPS(II,JJ,KK),
     &        '  # ctl type (',SSTR(1:ln),'), law (',
     &        SSTR2(1:ln2),'), starting @'
            WRITE(IUNIT,'(F8.0,A)',IOSTAT=ios,ERR=1)FMISCD(II,JJ,KK,1),
     &        '  # No. of data items'
            NITEMS=INT(FMISCD(II,JJ,KK,1))+1
            IF(NITEMS.EQ.1)goto 601

C Write control data and compact, if very long then use two lines.
            if(NITEMS.lt.20)then
              WRITE(outs,'(20f11.3)',IOSTAT=ios,ERR=1)
     &          (FMISCD(II,JJ,KK,L),L=2,NITEMS)
              call SDELIM(outs,outsd,'S',IW)
              write(IUNIT,'(2x,a)')outsd(1:lnblnk(outsd))
            else
              WRITE(outs,'(20f11.3)',IOSTAT=ios,ERR=1)
     &          (FMISCD(II,JJ,KK,L),L=2,19)
              call SDELIM(outs,outsd,'S',IW)
              write(IUNIT,'(2x,a)')outsd(1:lnblnk(outsd))
              WRITE(outs,'(20f11.3)',IOSTAT=ios,ERR=1)
     &          (FMISCD(II,JJ,KK,L),L=20,nitems)
              call SDELIM(outs,outsd,'S',IW)
              write(IUNIT,'(2x,a)')outsd(1:lnblnk(outsd))
            endif
  601     CONTINUE
  501   CONTINUE

C If control component active write connections.
        if(ifan(ii,1).eq.-4)then
          DO 701 ICCN=1,ifan(ii,3)

C Find the index of the connection, its component and the
C number of supplimental items.  There are no supplimental items
C in the case of sensing outside boundary contiditions (ie. -3 sensor)
            icc=iasocc(ii,ICCN)
            nsup=nfsup(ii)
            if(nsup.eq.0)then
              WRITE(IUNIT,'(a12,2x,a12,2x,a12)') NDNAM(NODPS(icc)),
     &          NDNAM(NODNE(icc)),CMNAM(ITPCON(icc))
            elseif(nsup.eq.1)then
              WRITE(IUNIT,'(a12,2x,a12,2x,a12,2x,a12)') 
     &          NDNAM(NODPS(icc)),NDNAM(NODNE(icc)),CMNAM(ITPCON(icc)),
     &          NDNAM(NDSCNN(icc,1))
            elseif(nsup.eq.2)then
              WRITE(IUNIT,'(a12,2x,a12,2x,a12,2x,a12,2x,a12)')
     &          NDNAM(NODPS(icc)),NDNAM(NODNE(icc)),CMNAM(ITPCON(icc)),
     &          NDNAM(NDSCNN(icc,1)),NDNAM(NDSCNN(icc,2))
            endif
  701     continue
        endif
  401 CONTINUE

C Global.
    4 IF(NGF.EQ.0)GOTO 12
      WRITE(IUNIT,'(A)',IOSTAT=ios,ERR=1) '* System'

C Global control information.
      WRITE(IUNIT,'(A)',IOSTAT=ios,ERR=1)glctldoc(1:lnblnk(glctldoc))
      WRITE(IUNIT,'(I4)',IOSTAT=ios,ERR=1)NGF
     
      DO 480 I=1,NGF
        II=I
        WRITE(IUNIT,'(A)',IOSTAT=ios,ERR=1) '* Global control '
        WRITE(IUNIT,'(3I5)',IOSTAT=ios,ERR=1)(IGSN(II,J),J=1,3)
        WRITE(IUNIT,'(3I5)',IOSTAT=ios,ERR=1)(IGAN(II,J),J=1,3)
        WRITE(IUNIT,'(I6)',IOSTAT=ios,ERR=1)NGCDT(II)
        NN=NGCDT(II)
        IF(NN.EQ.0)NN=3

        DO 450 J=1,NN
          JJ=J
          WRITE(IUNIT,'(2i5)',IOSTAT=ios,ERR=1)(IGCDV(II,JJ,K),K=1,2)
          WRITE(IUNIT,'(I6)',IOSTAT=ios,ERR=1)NGCDP(II,JJ)
          LL=NGCDP(II,JJ)

          DO 460 K=1,LL
            KK=K
            WRITE(IUNIT,9,IOSTAT=ios,ERR=1)IGCTYP(II,JJ,KK),
     &            IGCLAW(II,JJ,KK),TGCPS(II,JJ,KK)
            WRITE(IUNIT,11,IOSTAT=ios,ERR=1)GMISCD(II,JJ,KK,1)
            NITEMS=INT(GMISCD(II,JJ,KK,1))+1
            IF(NITEMS.EQ.1)goto 460

C Write control data and compact, if very long then use two lines.
            if(NITEMS.lt.20)then
              WRITE(outs,'(20f11.3)',IOSTAT=ios,ERR=1)
     &          (GMISCD(II,JJ,KK,L),L=2,NITEMS)
              call SDELIM(outs,outsd,'S',IW)
              write(IUNIT,'(2x,a)')outsd(1:lnblnk(outsd))
            else
              WRITE(outs,'(20f11.3)',IOSTAT=ios,ERR=1)
     &          (GMISCD(II,JJ,KK,L),L=2,19)
              call SDELIM(outs,outsd,'S',IW)
              write(IUNIT,'(2x,a)')outsd(1:lnblnk(outsd))
              WRITE(outs,'(20f11.3)',IOSTAT=ios,ERR=1)
     &          (GMISCD(II,JJ,KK,L),L=20,nitems)
              call SDELIM(outs,outsd,'S',IW)
              write(IUNIT,'(2x,a)')outsd(1:lnblnk(outsd))
            endif
  460     CONTINUE
  450   CONTINUE
  480 CONTINUE

   12 CALL ERPFREE(IUNIT,ios)
      RETURN

    1 if(IOS.eq.2)then
        call edisp(iuout,'Write permission error in routine CTLWRT')
      else
        call edisp(iuout,'Write error in routine CTLWRT')
      endif
      IER=1
      goto 12

      END
