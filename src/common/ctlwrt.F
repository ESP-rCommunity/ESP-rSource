C This file is part of the ESP-r system.
C Copyright Energy Systems Research Unit, University of
C Strathclyde, Glasgow, Scotland, 2001-.

C ESP-r is free software.  You can redistribute it and/or
C modify it under the terms of the GNU General Public
C License as published by the Free Software Foundation 
C (version 2 orlater).

C ESP-r is distributed in the hope that it will be useful
C but WITHOUT ANY WARRANTY; without even the implied
C warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
C PURPOSE. See the GNU General Public License for more
C details.


C ******************** CTLWRT ********************
C Takes configuration control common block data and saves to
C an ascii file.

      SUBROUTINE CTLWRT(IUNIT,IER)
#include "building.h"
#include "net_flow.h"
#include "net_flow_data.h"
#include "control.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      
      integer ncomp,ncon
      common/c1/ncomp,ncon
      common/cctlnm/ctldoc,lctlf
      integer icascf
      common/cctl/icascf(mcom)
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      common/fctl4/iasocc(MCF,mcmp),nfsup(MCF)
      COMMON/OVRWT/AUTOVR
      logical AUTOVR
      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER NBDAYTYPE,NBCALDAYS,ICALENDER
      common/calena/calename,calentag(MDTY),calendayname(MDTY)
      character calename*32,calentag*12,calendayname*32

C High level control scope key words.
      character hcffpattern*12    ! heat, cool, or heat+cool plus detail
      common/hlcontrol/hcffpattern(2)     

      CHARACTER lctlf*72,ctldoc*248,daytype*12,lfile*72
      character outs*296,outsd*296
      character DS*7,DS1*10,DE*7,DE1*10,SSTR*96,SSTR1*96,SSTR2*248
      character DS2*8,DE2*8
      character tokens*124,comment*124,aligned_str*200

      IER=0

C Initialise control file.
      lfile=lctlf
      if(AUTOVR)then
        CALL EFOPSEQ(IUNIT,lfile,3,IER)
      else
        CALL EFOPSEQ(IUNIT,lfile,4,IER)
      endif
      IF(IER.LT.0)THEN
        IER=1
        RETURN
      ENDIF

C Write data to file beginning with header.
      WRITE(IUNIT,'(A)',IOSTAT=ios,ERR=1)'*CONTROL'
      LN=max(1,LNBLNK(ctldoc))
      WRITE(IUNIT,'(2A)',IOSTAT=ios,ERR=1) '*cdoc ',CTLDOC(1:ln)
      IF(ctldoc(1:4).EQ.'NONE')GOTO 12
      IF(NCF.EQ.0)goto 2

C Building control information.
      WRITE(IUNIT,'(A)',IOSTAT=ios,ERR=1) '*building'
      LN=max(1,LNBLNK(znctldoc))
      WRITE(IUNIT,'(2A)',IOSTAT=ios,ERR=1) '*zdoc ',znctldoc(1:ln)

C Control scope, if set, include before the number of loops.
      if(hcffpattern(1)(1:7).eq.'UNKNOWN')then
        continue
      else
        WRITE(IUNIT,'(2A)',IOSTAT=ios,ERR=1) '*scope ',hcffpattern(1)
      endif
      write(comment,'(A)') 'number of loops'
      WRITE(tokens,'(i4)') NCF
      call align_comment(28,tokens,comment,aligned_str)
      write(IUNIT,'(a)') aligned_str(1:lnblnk(aligned_str))

      DO 10 I=1,NCF
        II=I
        WRITE(IUNIT,'(A,i4,2A)',IOSTAT=ios,ERR=1)'*loop',I,' ',
     &    BCTLNAME(ii)(1:lnblnk(BCTLNAME(ii)))
        CALL EVCNTRL(0,II,1,1,'S',SSTR)
        LN=max(1,LNBLNK(SSTR))
        write(comment,'(A)') SSTR(1:ln)
        WRITE(tokens,'(4I5)') (IBSN(II,J),J=1,4)
        call align_comment(28,tokens,comment,aligned_str)
        write(IUNIT,'(a)') aligned_str(1:lnblnk(aligned_str))
        CALL EVCNTRL(0,II,1,1,'A',SSTR)
        LN=max(1,LNBLNK(SSTR))
        write(comment,'(2A)') 'actuates ',SSTR(1:ln)
        WRITE(tokens,'(3I5)') (IBAN(II,J),J=1,3)
        call align_comment(28,tokens,comment,aligned_str)
        write(IUNIT,'(a)') aligned_str(1:lnblnk(aligned_str))
        NN=NBCDT(II)
        IF(NN.EQ.0)THEN ! All day types have individual controls
          NN=nbdaytype
          daytype='individual'
          write(comment,'(A,I2)')'day types follow calendar ',nbdaytype
          WRITE(tokens,'(I5)') NBCDT(II)
          call align_comment(28,tokens,comment,aligned_str)
          write(IUNIT,'(a)') aligned_str(1:lnblnk(aligned_str))
        ELSEIF(NN.EQ.1)THEN ! All day types have same controls
          daytype='all daytypes'
          write(comment,'(A)') 'all day types have same control'
          WRITE(tokens,'(I5)') NBCDT(II)
          call align_comment(28,tokens,comment,aligned_str)
          write(IUNIT,'(a)') aligned_str(1:lnblnk(aligned_str))
        ELSE ! Day types are not used but periods of validity are active
          daytype=' '
          write(comment,'(A)') 'uses dates of validity'
          WRITE(tokens,'(I5)') NBCDT(II)
          call align_comment(28,tokens,comment,aligned_str)
          write(IUNIT,'(a)') aligned_str(1:lnblnk(aligned_str))
        ENDIF
        DO 20 J=1,NN
          JJ=J
          CALL STDATE(IYEAR,IBCDV(II,JJ,1),DS,DS1,DS2)
          CALL STDATE(IYEAR,IBCDV(II,JJ,2),DE,DE1,DE2)
          IF(DAYTYPE(1:1).NE.' '.OR.DAYTYPE(1:12).NE.'all daytypes')
     &    DAYTYPE=CALENTAG(J)
          write(comment,'(6A)') 'valid ',DS1,' - ',DE1,
     &     ', periods in ',daytype(1:lnblnk(daytype))
          WRITE(tokens,'(3I5)') (IBCDV(II,JJ,K),K=1,2),NBCDP(II,JJ)
          call align_comment(28,tokens,comment,aligned_str)
          write(IUNIT,'(a)') aligned_str(1:lnblnk(aligned_str))
          IF(DAYTYPE(1:1).NE.' '.OR.DAYTYPE(1:12).NE.'all daytypes')
     &    DAYTYPE=CALENTAG(J)

          LL=NBCDP(II,JJ)

          DO 30 K=1,LL
            KK=K
            CALL EVCNTRLAW(0,II,JJ,KK,SSTR,SSTR2)
            LN=max(1,LNBLNK(SSTR))
            LN2=max(1,LNBLNK(SSTR2))
            write(comment,'(3A)') 'ctl type, law (',SSTR(1:ln),
     &        '), start @, data items'
            WRITE(tokens,'(2I5,F8.3,F5.0)')
     &        IBCTYP(II,JJ,KK),IBCLAW(II,JJ,KK),TBCPS(II,JJ,KK),
     &        BMISCD(II,JJ,KK,1)
            call align_comment(28,tokens,comment,aligned_str)
            write(IUNIT,'(a)') aligned_str(1:lnblnk(aligned_str))
C   11       FORMAT(F8.1)
            NITEMS=INT(BMISCD(II,JJ,KK,1))+1
            IF(NITEMS.EQ.1)GOTO 30

C Write control data and compact, if very long then use two lines.
            if(NITEMS.lt.20)then
              WRITE(outs,'(20f11.3)',IOSTAT=ios,ERR=1)
     &          (BMISCD(II,JJ,KK,L),L=2,NITEMS)
              call SDELIM(outs,outsd,'S',IW)
              lnod=lnblnk(outsd)
              if(lnod.lt.28)then
                if(ln2.lt.124)then
                  write(comment,'(a)')SSTR2(1:ln2)
                else
                  write(comment,'(a)')SSTR2(1:123)
                endif
                call align_comment(28,outsd,comment,aligned_str)
                write(IUNIT,'(a)') aligned_str(1:lnblnk(aligned_str))
              else
                write(IUNIT,'(2x,a,2a)') outsd(1:lnblnk(outsd)),'  # ',
     &            SSTR2(1:ln2)
              endif
            else
              write(IUNIT,'(2a)') '# ',SSTR2(1:ln2)  
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
      WRITE(IUNIT,'(A)',IOSTAT=ios,ERR=1) '*plant'

C Plant control information.
      WRITE(IUNIT,'(2A)',IOSTAT=ios,ERR=1) '*pdoc ',
     &  plctldoc(1:lnblnk(plctldoc))
      write(comment,'(A)') 'number of loops'
      WRITE(tokens,'(i4)') NCL
      call align_comment(28,tokens,comment,aligned_str)
      write(IUNIT,'(a)') aligned_str(1:lnblnk(aligned_str))
     
      DO 40 I=1,NCL
        II=I
        WRITE(IUNIT,'(A,I4,2A)',IOSTAT=ios,ERR=1) '*loop',I,' ',
     &    PCTLNAME(ii)(1:lnblnk(PCTLNAME(ii)))
        CALL EVCNTRL(1,II,1,1,'S',SSTR)
        LN=max(1,LNBLNK(SSTR))
        write(comment,'(A)') SSTR(1:ln)
        WRITE(tokens,'(5I5)') (IPSN(II,J),J=1,5)
        call align_comment(28,tokens,comment,aligned_str)
        write(IUNIT,'(a)') aligned_str(1:lnblnk(aligned_str))
        CALL EVCNTRL(1,II,1,1,'A',SSTR)
        LN=max(1,LNBLNK(SSTR))
        write(comment,'(2A)') 'actuates ',SSTR(1:ln)
        WRITE(tokens,'(4I5)') (IPAN(II,J),J=1,4)
        call align_comment(28,tokens,comment,aligned_str)
        write(IUNIT,'(a)') aligned_str(1:lnblnk(aligned_str))
        NN=NPCDT(II)
        IF(NN.EQ.0)THEN ! All day types have individual controls
          NN=nbdaytype
          daytype='individual'
          write(comment,'(A,I2)')'day types follow calendar ',nbdaytype
          WRITE(tokens,'(I5)') NPCDT(II)
          call align_comment(28,tokens,comment,aligned_str)
          write(IUNIT,'(a)') aligned_str(1:lnblnk(aligned_str))
        ELSEIF(NN.EQ.1)THEN ! All day types have same controls
          daytype='all daytypes'
          write(comment,'(A)') 'all day types have same control'
          WRITE(tokens,'(I5)') NPCDT(II)
          call align_comment(28,tokens,comment,aligned_str)
          write(IUNIT,'(a)') aligned_str(1:lnblnk(aligned_str))
        ELSE ! Day types are not used but periods of validity are active
          daytype=' '
          write(comment,'(A)') 'uses dates of validity'
          WRITE(tokens,'(I5)') NPCDT(II)
          call align_comment(28,tokens,comment,aligned_str)
          write(IUNIT,'(a)') aligned_str(1:lnblnk(aligned_str))
        ENDIF

        DO 50 J=1,NN
          JJ=J
          CALL STDATE(IYEAR,IPCDV(II,JJ,1),DS,DS1,DS2)
          CALL STDATE(IYEAR,IPCDV(II,JJ,2),DE,DE1,DE2)
          IF(DAYTYPE(1:1).NE.' '.OR.DAYTYPE(1:12).NE.'all daytypes')
     &    DAYTYPE=CALENTAG(J)
          write(comment,'(6A)') 'valid ',DS1,' - ',DE1,
     &     ', periods in ',daytype(1:lnblnk(daytype))
          WRITE(tokens,'(3I5)') (IPCDV(II,JJ,K),K=1,2),NPCDP(II,JJ)
          call align_comment(28,tokens,comment,aligned_str)
          write(IUNIT,'(a)') aligned_str(1:lnblnk(aligned_str))
          IF(DAYTYPE(1:1).NE.' '.OR.DAYTYPE(1:12).NE.'all daytypes')
     &    DAYTYPE=CALENTAG(J)
          LL=NPCDP(II,JJ)

          DO 60 K=1,LL
            KK=K
            CALL EVCNTRLAW(1,II,JJ,KK,SSTR,SSTR2)
            LN=max(1,LNBLNK(SSTR))
            LN2=max(1,LNBLNK(SSTR2))
            write(comment,'(3A)') 'ctl type, law (',SSTR(1:ln),
     &        '), start @, data items'
            WRITE(tokens,'(2I5,F8.3,F5.0)')
     &        IPCTYP(II,JJ,KK),IPCLAW(II,JJ,KK),TPCPS(II,JJ,KK),
     &        PMISCD(II,JJ,KK,1)
            call align_comment(28,tokens,comment,aligned_str)
            write(IUNIT,'(a)') aligned_str(1:lnblnk(aligned_str))
            NITEMS=INT(PMISCD(II,JJ,KK,1))+1
            IF(NITEMS.EQ.1)goto 60

C Write control data and compact, if very long then use two lines.
            if(NITEMS.lt.20)then
              WRITE(outs,'(20f12.5)',IOSTAT=ios,ERR=1)
     &          (PMISCD(II,JJ,KK,L),L=2,NITEMS)
              call SDELIM(outs,outsd,'S',IW)
              lnod=lnblnk(outsd)
              if(lnod.lt.28)then
                if(ln2.lt.124)then
                  write(comment,'(a)')SSTR2(1:ln2)
                else
                  write(comment,'(a)')SSTR2(1:123)
                endif
                call align_comment(28,outsd,comment,aligned_str)
                write(IUNIT,'(a)') aligned_str(1:lnblnk(aligned_str))
              else
                write(IUNIT,'(2x,a,2a)') outsd(1:lnblnk(outsd)),'  # ',
     &            SSTR2(1:ln2)
              endif
            else
              write(IUNIT,'(2a)') '# ',SSTR2(1:ln2)  
              WRITE(outs,'(20f12.5)',IOSTAT=ios,ERR=1)
     &          (PMISCD(II,JJ,KK,L),L=2,19)
              call SDELIM(outs,outsd,'S',IW)
              write(IUNIT,'(2x,a)')outsd(1:lnblnk(outsd))
              WRITE(outs,'(20f12.5)',IOSTAT=ios,ERR=1)
     &          (PMISCD(II,JJ,KK,L),L=20,nitems)
              call SDELIM(outs,outsd,'S',IW)
              write(IUNIT,'(2x,a)')outsd(1:lnblnk(outsd))
            endif
   60     CONTINUE
   50   CONTINUE
   40 CONTINUE

C Flow.
    3 IF(NCC.EQ.0)GOTO 4
      WRITE(IUNIT,'(A)',IOSTAT=ios,ERR=1) '*mass flow'

C Flow control information.
      LN=max(1,LNBLNK(flctldoc))
      WRITE(IUNIT,'(2A)',IOSTAT=ios,ERR=1) '*fdoc ',flctldoc(1:ln)
      write(comment,'(A)') 'number of loops'
      WRITE(tokens,'(i4)') NCC
      call align_comment(28,tokens,comment,aligned_str)
      write(IUNIT,'(a)') aligned_str(1:lnblnk(aligned_str))
     
      DO 401 I=1,NCC
        II=I
        WRITE(IUNIT,'(A,I4,2a)',IOSTAT=ios,ERR=1)'*loop',I,' ',
     &    FCTLNAME(ii)(1:lnblnk(FCTLNAME(ii)))
        CALL EVCNTRL(2,II,1,1,'S',SSTR)
        LN=max(1,LNBLNK(SSTR))
        write(comment,'(A)') SSTR(1:ln)
        WRITE(tokens,'(4I5)') (IFSN(II,J),J=1,4)
        call align_comment(28,tokens,comment,aligned_str)
        write(IUNIT,'(a)') aligned_str(1:lnblnk(aligned_str))
        CALL EVCNTRL(2,II,1,1,'A',SSTR)
        LN=max(1,LNBLNK(SSTR))
        write(comment,'(2A)') 'actuates ',SSTR(1:ln)
        WRITE(tokens,'(3I5)') (IFAN(II,J),J=1,3)
        call align_comment(28,tokens,comment,aligned_str)
        write(IUNIT,'(a)') aligned_str(1:lnblnk(aligned_str))
        NN=NFCDT(II)
        IF(NN.EQ.0)THEN ! All day types have individual controls
          NN=nbdaytype
          daytype='individual'
          write(comment,'(A,I2)')'day types follow calendar ',nbdaytype
          WRITE(tokens,'(I5)') NFCDT(II)
          call align_comment(28,tokens,comment,aligned_str)
          write(IUNIT,'(a)') aligned_str(1:lnblnk(aligned_str))
        ELSEIF(NN.EQ.1)THEN ! All day types have same controls
          daytype='all daytypes'
          write(comment,'(A)') 'all day types have same control'
          WRITE(tokens,'(I5)') NFCDT(II)
          call align_comment(28,tokens,comment,aligned_str)
          write(IUNIT,'(a)') aligned_str(1:lnblnk(aligned_str))
        ELSE ! Day types are not used but periods of validity are active
          daytype=' '
          write(comment,'(A)') 'uses dates of validity'
          WRITE(tokens,'(I5)') NFCDT(II)
          call align_comment(28,tokens,comment,aligned_str)
          write(IUNIT,'(a)') aligned_str(1:lnblnk(aligned_str))
        ENDIF

        DO 501 J=1,NN
          JJ=J
          CALL STDATE(IYEAR,IFCDV(II,JJ,1),DS,DS1,DS2)
          CALL STDATE(IYEAR,IFCDV(II,JJ,2),DE,DE1,DE2)
          IF(DAYTYPE(1:1).NE.' '.OR.DAYTYPE(1:12).NE.'all daytypes')
     &    DAYTYPE=CALENTAG(J)
          write(comment,'(6A)') 'valid ',DS1,' - ',DE1,
     &     ', periods in ',daytype(1:lnblnk(daytype))
          WRITE(tokens,'(3I5)') (IFCDV(II,JJ,K),K=1,2),NFCDP(II,JJ)
          call align_comment(28,tokens,comment,aligned_str)
          write(IUNIT,'(a)') aligned_str(1:lnblnk(aligned_str))
          IF(DAYTYPE(1:1).NE.' '.OR.DAYTYPE(1:12).NE.'all daytypes')
     &    DAYTYPE=CALENTAG(J)
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
            write(comment,'(5A)') 'type (',SSTR(1:ln),
     &        '), law (',SSTR1(1:ln1),'), start@'
            WRITE(tokens,'(2I5,F8.3,F5.0)')
     &        IFCTYP(II,JJ,KK),IFCLAW(II,JJ,KK),TFCPS(II,JJ,KK),
     &        FMISCD(II,JJ,KK,1)
            call align_comment(28,tokens,comment,aligned_str)
            write(IUNIT,'(a)') aligned_str(1:lnblnk(aligned_str))
            NITEMS=INT(FMISCD(II,JJ,KK,1))+1
            IF(NITEMS.EQ.1)goto 601

C Write control data and compact, if very long then use two lines.
            if(NITEMS.lt.19)then
              WRITE(outs,'(19f12.5)',IOSTAT=ios,ERR=1)
     &          (FMISCD(II,JJ,KK,L),L=2,NITEMS)
              call SDELIM(outs,outsd,'S',IW)
              lnod=lnblnk(outsd)
              if(lnod.lt.28)then
                if(ln2.lt.124)then
                  write(comment,'(a)')SSTR2(1:ln2)
                else
                  write(comment,'(a)')SSTR2(1:123)
                endif
                call align_comment(28,outsd,comment,aligned_str)
                write(IUNIT,'(a)') aligned_str(1:lnblnk(aligned_str))
              else
                write(IUNIT,'(2x,a,2a)') outsd(1:lnblnk(outsd)),'  # ',
     &            SSTR2(1:ln2)
              endif
            else
              write(IUNIT,'(2a)') '# ',SSTR2(1:ln2)  
              WRITE(outs,'(19f12.5)',IOSTAT=ios,ERR=1)
     &          (FMISCD(II,JJ,KK,L),L=2,18)
              call SDELIM(outs,outsd,'S',IW)
              write(IUNIT,'(2x,a)')outsd(1:lnblnk(outsd))
              WRITE(outs,'(19f12.5)',IOSTAT=ios,ERR=1)
     &          (FMISCD(II,JJ,KK,L),L=19,nitems)
              call SDELIM(outs,outsd,'S',IW)
              write(IUNIT,'(2x,a)')outsd(1:lnblnk(outsd))
            endif
  601     CONTINUE
  501   CONTINUE

C If control component active write connections.
        if(ifan(ii,1).eq.-4)then
          write(IUNIT,'(a)') 
     &      '# node   to   node    via   component     supplemental'
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
    4 IF(NGF.EQ.0)GOTO 13
      WRITE(IUNIT,'(A)',IOSTAT=ios,ERR=1) '*global'

C Global control information.
      WRITE(IUNIT,'(2A)',IOSTAT=ios,ERR=1) '*gdoc ',
     &  glctldoc(1:lnblnk(glctldoc))
      write(comment,'(A)') 'number of loops'
      WRITE(tokens,'(i4)') NGF
      call align_comment(28,tokens,comment,aligned_str)
      write(IUNIT,'(a)') aligned_str(1:lnblnk(aligned_str))
     
      DO 480 I=1,NGF
        II=I
        WRITE(IUNIT,'(A,I4,2A)',IOSTAT=ios,ERR=1) '*loop',I,' ',
     &    GCTLNAME(ii)(1:lnblnk(GCTLNAME(ii)))
        write(comment,'(A)') 'sensor data'
        WRITE(tokens,'(3I5)') (IGSN(II,J),J=1,3)
        call align_comment(28,tokens,comment,aligned_str)
        write(IUNIT,'(a)') aligned_str(1:lnblnk(aligned_str))
        write(comment,'(A)') 'actuator data'
        WRITE(tokens,'(3I5)') (IGAN(II,J),J=1,3)
        call align_comment(28,tokens,comment,aligned_str)
        write(IUNIT,'(a)') aligned_str(1:lnblnk(aligned_str))
        NN=NGCDT(II)
        IF(NN.EQ.0)THEN ! All day types have individual controls
          NN=nbdaytype
          daytype='individual'
          write(comment,'(A,I2)')'day types follow calendar ',nbdaytype
          WRITE(tokens,'(I5)') NGCDT(II)
          call align_comment(28,tokens,comment,aligned_str)
          write(IUNIT,'(a)') aligned_str(1:lnblnk(aligned_str))
        ELSEIF(NN.EQ.1)THEN ! All day types have same controls
          daytype='all daytypes'
          write(comment,'(A)') 'all day types have same control'
          WRITE(tokens,'(I5)') NGCDT(II)
          call align_comment(28,tokens,comment,aligned_str)
          write(IUNIT,'(a)') aligned_str(1:lnblnk(aligned_str))
        ELSE ! Day types are not used but periods of validity are active
          daytype=' '
          write(comment,'(A)') 'uses dates of validity'
          WRITE(tokens,'(I5)') NGCDT(II)
          call align_comment(28,tokens,comment,aligned_str)
          write(IUNIT,'(a)') aligned_str(1:lnblnk(aligned_str))
        ENDIF

        DO 450 J=1,NN
          JJ=J
          CALL STDATE(IYEAR,IFCDV(II,JJ,1),DS,DS1,DS2)
          CALL STDATE(IYEAR,IFCDV(II,JJ,2),DE,DE1,DE2)
          IF(DAYTYPE(1:1).NE.' '.OR.DAYTYPE(1:12).NE.'all daytypes')
     &    DAYTYPE=CALENTAG(J)
          write(comment,'(6A)') 'valid ',DS1,' - ',DE1,
     &     ', periods in ',daytype(1:lnblnk(daytype))
          WRITE(tokens,'(3I5)') (IGCDV(II,JJ,K),K=1,2),NGCDP(II,JJ)
          call align_comment(28,tokens,comment,aligned_str)
          write(IUNIT,'(a)') aligned_str(1:lnblnk(aligned_str))
          DO 460 K=1,LL
            KK=K
            CALL EVCNTRL(3,II,JJ,KK,'T',SSTR)
            LN=max(1,LNBLNK(SSTR))
            CALL EVCNTRLAW(3,II,JJ,KK,SSTR1,SSTR2)
            LN1=max(1,LNBLNK(SSTR1))
            LN2=max(1,LNBLNK(SSTR2))
            write(comment,'(3A)') 'ctl type, law (',SSTR(1:ln),
     &        '), start @, data items'
            WRITE(tokens,'(2I5,F8.3,F5.0)')
     &        IGCTYP(II,JJ,KK),IGCLAW(II,JJ,KK),TGCPS(II,JJ,KK),
     &        GMISCD(II,JJ,KK,1)
            call align_comment(28,tokens,comment,aligned_str)
            write(IUNIT,'(a)') aligned_str(1:lnblnk(aligned_str))
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

C Optical control information.
  13  if(NOF.eq.0)goto 101
      WRITE(IUNIT,'(A)',IOSTAT=ios,ERR=1) '*optics'
      ln=max(1,LNBLNK(opticdoc))      
      WRITE(IUNIT,'(2A)',IOSTAT=ios,ERR=1)'*odoc ',opticdoc(1:ln)

      write(comment,'(A)') 'number of loops'
      WRITE(tokens,'(i4)') NOF
      call align_comment(28,tokens,comment,aligned_str)
      write(IUNIT,'(a)') aligned_str(1:lnblnk(aligned_str))

      DO 100 I=1,NOF
        II=I
        WRITE(IUNIT,'(A,i4,2A)',IOSTAT=ios,ERR=1)'*loop',I,' ',
     &    OCTLNAME(ii)(1:lnblnk(OCTLNAME(ii)))
        CALL EVCNTRL(5,II,1,1,'S',SSTR)
        LN=max(1,LNBLNK(SSTR))
        write(comment,'(A)') SSTR(1:ln)
        WRITE(tokens,'(3I5)') (IOSN(II,J),J=1,3)
        call align_comment(28,tokens,comment,aligned_str)
        write(IUNIT,'(a)') aligned_str(1:lnblnk(aligned_str))
        CALL EVCNTRL(5,II,1,1,'A',SSTR)
        LN=max(1,LNBLNK(SSTR))        
        write(comment,'(2A)') 'actuates ',SSTR(1:ln)
        WRITE(tokens,'(3I5)') (IOAN(II,J),J=1,3)
        call align_comment(28,tokens,comment,aligned_str)
        write(IUNIT,'(a)') aligned_str(1:lnblnk(aligned_str))
        NN=NOCDT(II)
        IF(NN.EQ.0)THEN ! All day types have individual controls
          NN=nbdaytype
          daytype='individual'
          write(comment,'(A,I2)')'day types follow calendar ',nbdaytype
          WRITE(tokens,'(I5)') NOCDT(II)
          call align_comment(28,tokens,comment,aligned_str)
          write(IUNIT,'(a)') aligned_str(1:lnblnk(aligned_str))
         ELSEIF(NN.EQ.1)THEN ! All day types have same controls
          daytype='all daytypes'
          write(comment,'(A)') 'all day types have same control'
          WRITE(tokens,'(I5)') NOCDT(II)
          call align_comment(28,tokens,comment,aligned_str)
          write(IUNIT,'(a)') aligned_str(1:lnblnk(aligned_str))
        ELSE ! Day types are not used but periods of validity are active
          daytype=' '
          write(comment,'(A)') 'uses dates of validity'
          WRITE(tokens,'(I5)') NOCDT(II)
          call align_comment(28,tokens,comment,aligned_str)
          write(IUNIT,'(a)') aligned_str(1:lnblnk(aligned_str))
        ENDIF

        DO 200 J=1,NN
          JJ=J
          CALL STDATE(IYEAR,IOCDV(II,JJ,1),DS,DS1,DS2)
          CALL STDATE(IYEAR,IOCDV(II,JJ,2),DE,DE1,DE2)
          IF(DAYTYPE(1:1).NE.' '.OR.DAYTYPE(1:12).NE.'all daytypes')
     &    DAYTYPE=CALENTAG(J)
          write(comment,'(6A)') 'valid ',DS1,' - ',DE1,
     &     ', periods in ',daytype(1:lnblnk(daytype))
          WRITE(tokens,'(3I5)') (IOCDV(II,JJ,K),K=1,2),NOCDP(II,JJ)
          call align_comment(28,tokens,comment,aligned_str)
          write(IUNIT,'(a)') aligned_str(1:lnblnk(aligned_str))
          IF(DAYTYPE(1:1).NE.' '.OR.DAYTYPE(1:12).NE.'all daytypes')
     &    DAYTYPE=CALENTAG(J)
          LL=NOCDP(II,JJ)

          DO 300 K=1,LL
            KK=K
            CALL EVCNTRLAW(5,II,JJ,KK,SSTR,SSTR2)
            LN=max(1,LNBLNK(SSTR))            
            LN2=max(1,LNBLNK(SSTR2))
            write(comment,'(3A)') 'ctl type, law (',SSTR(1:ln),
     &        '), start @, data items'
            WRITE(tokens,'(2I5,F8.3,F5.0)')
     &        IOCTYP(II,JJ,KK),IOCLAW(II,JJ,KK),TOCPS(II,JJ,KK),
     &        OMISCD(II,JJ,KK,1)
            call align_comment(28,tokens,comment,aligned_str)
            write(IUNIT,'(a)') aligned_str(1:lnblnk(aligned_str))
            NITEMS=INT(OMISCD(II,JJ,KK,1))+1
            IF(NITEMS.EQ.1)GOTO 300

C Write control data and compact, if very long then use two lines.
            if(NITEMS.lt.20)then
              WRITE(outs,'(20f11.3)',IOSTAT=ios,ERR=1)
     &          (OMISCD(II,JJ,KK,L),L=2,NITEMS)
              call SDELIM(outs,outsd,'S',IW)
              lnod=lnblnk(outsd)
              if(lnod.lt.28)then
                if(ln2.lt.124)then
                  write(comment,'(a)')SSTR2(1:ln2)
                else
                  write(comment,'(a)')SSTR2(1:123)
                endif
                call align_comment(28,outsd,comment,aligned_str)
                write(IUNIT,'(a)') aligned_str(1:lnblnk(aligned_str))
              else
                write(IUNIT,'(2x,a,2a)') outsd(1:lnblnk(outsd)),'  # ',
     &            SSTR2(1:ln2)
              endif
            else
              WRITE(outs,'(20f11.3)',IOSTAT=ios,ERR=1)
     &          (OMISCD(II,JJ,KK,L),L=2,19)
              call SDELIM(outs,outsd,'S',IW)
              write(IUNIT,'(2x,a)')outsd(1:lnblnk(outsd))
              WRITE(outs,'(20f11.3)',IOSTAT=ios,ERR=1)
     &          (OMISCD(II,JJ,KK,L),L=20,nitems)
              call SDELIM(outs,outsd,'S',IW)
              write(IUNIT,'(2x,a)')outsd(1:lnblnk(outsd))
            endif
  300     CONTINUE
  200   CONTINUE
  100 CONTINUE

C Complex fenestration control information.
  101 IF(nCFCctlloops.EQ.0)goto 12
      WRITE(IUNIT,'(A)',IOSTAT=ios,ERR=1) '*cfc'
      LN=max(1,LNBLNK(CFCctldoc))
      WRITE(IUNIT,'(2A)',IOSTAT=ios,ERR=1) '*cfcdoc ',CFCctldoc(1:ln)

      write(comment,'(A)') 'number of loops'
      WRITE(tokens,'(i4)') nCFCctlloops
      call align_comment(28,tokens,comment,aligned_str)
      write(IUNIT,'(a)') aligned_str(1:lnblnk(aligned_str))

      DO 109 I=1,nCFCctlloops
        II=I
        WRITE(IUNIT,'(A,I4,2A)',IOSTAT=ios,ERR=1)'*loop',I,' ',
     &    CFCCTLNAME(ii)(1:lnblnk(CFCCTLNAME(ii)))
        CALL EVCNTRL(6,II,1,1,'S',SSTR)
        LN=max(1,LNBLNK(SSTR))
        write(comment,'(A)') SSTR(1:ln)
        WRITE(tokens,'(4I5)') (iCFCsensor(II,J),J=1,4)
        call align_comment(28,tokens,comment,aligned_str)
        write(IUNIT,'(a)') aligned_str(1:lnblnk(aligned_str))
        CALL EVCNTRL(6,II,1,1,'A',SSTR)
        LN=max(1,LNBLNK(SSTR))
        write(comment,'(2A)') 'actuates ',SSTR(1:ln)
        WRITE(tokens,'(3I5)') (iCFCactuator(II,J),J=1,3)
        call align_comment(28,tokens,comment,aligned_str)
        write(IUNIT,'(a)') aligned_str(1:lnblnk(aligned_str))
        NN=nCFCctldaytypes(II)
        IF(NN.EQ.0)THEN  ! All day types have individual controls
          NN=nbdaytype
          daytype='individual'
          write(comment,'(A,I2)')'day types follow calendar ',nbdaytype
          WRITE(tokens,'(I5)') nCFCctldaytypes(II)
          call align_comment(28,tokens,comment,aligned_str)
          write(IUNIT,'(a)') aligned_str(1:lnblnk(aligned_str))
        ELSEIF(NN.EQ.1)THEN ! All day types have same controls
          daytype='all daytypes'
          write(comment,'(A)') 'all day types have same control'
          WRITE(tokens,'(I5)') nCFCctldaytypes(II)
          call align_comment(28,tokens,comment,aligned_str)
          write(IUNIT,'(a)') aligned_str(1:lnblnk(aligned_str))
        ELSE ! Day types are not used but periods of validity are active
          daytype=' '
          write(comment,'(A)') 'uses dates of validity'
          WRITE(tokens,'(I5)') nCFCctldaytypes(II)
          call align_comment(28,tokens,comment,aligned_str)
          write(IUNIT,'(a)') aligned_str(1:lnblnk(aligned_str))
        ENDIF
        DO 209 J=1,NN
          JJ=J
          CALL STDATE(IYEAR,iCFCctldatevalid(II,JJ,1),DS,DS1,DS2)
          CALL STDATE(IYEAR,iCFCctldatevalid(II,JJ,2),DE,DE1,DE2)
          IF(DAYTYPE(1:1).NE.' '.OR.DAYTYPE(1:12).NE.'all daytypes')
     &    DAYTYPE=CALENTAG(J)
          write(comment,'(6A)') 'valid ',DS1,' - ',DE1,
     &     ', periods in ',daytype(1:lnblnk(daytype))
          WRITE(tokens,'(3I5)') (iCFCctldatevalid(II,JJ,K),K=1,2),
     &      nCFCdayctlperiods(II,JJ)
          call align_comment(28,tokens,comment,aligned_str)
          write(IUNIT,'(a)') aligned_str(1:lnblnk(aligned_str))
          IF(DAYTYPE(1:1).NE.' '.OR.DAYTYPE(1:12).NE.'all daytypes')
     &    DAYTYPE=CALENTAG(J)

          LL=nCFCdayctlperiods(II,JJ)

          DO 309 K=1,LL
            KK=K
            CALL EVCNTRLAW(6,II,JJ,KK,SSTR,SSTR2)
            LN=max(1,LNBLNK(SSTR))
            LN2=max(1,LNBLNK(SSTR2))
            write(comment,'(3A)') 'ctl type, law (',SSTR(1:ln),
     &        '), start @, data items'
            WRITE(tokens,'(2I5,F8.3,F5.0)')
     &        iCFCctltype(II,JJ,KK),iCFCctllaw(II,JJ,KK),
     &        CFCctlperiodstart(II,JJ,KK),CFCmiscdata(II,JJ,KK,1)
            call align_comment(28,tokens,comment,aligned_str)
            write(IUNIT,'(a)') aligned_str(1:lnblnk(aligned_str))
            NITEMS=INT(CFCmiscdata(II,JJ,KK,1))+1
            IF(NITEMS.EQ.1)GOTO 309

C Write control data and compact, if very long then use two lines.
            if(NITEMS.lt.20)then
              WRITE(outs,'(20f11.3)',IOSTAT=ios,ERR=1)
     &          (CFCmiscdata(II,JJ,KK,L),L=2,NITEMS)
              call SDELIM(outs,outsd,'S',IW)
              lnod=lnblnk(outsd)
              if(lnod.lt.28)then
                if(ln2.lt.124)then
                  write(comment,'(a)')SSTR2(1:ln2)
                else
                  write(comment,'(a)')SSTR2(1:123)
                endif
                call align_comment(28,outsd,comment,aligned_str)
                write(IUNIT,'(a)') aligned_str(1:lnblnk(aligned_str))
              else
                write(IUNIT,'(2x,a,2a)') outsd(1:lnblnk(outsd)),'  # ',
     &            SSTR2(1:ln2)
              endif
            else
              write(IUNIT,'(2a)') '# ',SSTR2(1:ln2)  
              WRITE(outs,'(20f11.3)',IOSTAT=ios,ERR=1)
     &          (CFCmiscdata(II,JJ,KK,L),L=2,19)
              call SDELIM(outs,outsd,'S',IW)
              write(IUNIT,'(2x,a)')outsd(1:lnblnk(outsd))
              WRITE(outs,'(20f11.3)',IOSTAT=ios,ERR=1)
     &          (CFCmiscdata(II,JJ,KK,L),L=20,nitems)
              call SDELIM(outs,outsd,'S',IW)
              write(IUNIT,'(2x,a)')outsd(1:lnblnk(outsd))
            endif
  309     CONTINUE
  209   CONTINUE
  109 CONTINUE
  
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
