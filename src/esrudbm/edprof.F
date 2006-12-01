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

C Editing and display of profiles database

C ************* EDPROF
C Event profiles item display and editing.  If ACT='T' then produce
C a tabular listing and return, otherwise present editing menu.
C Assumes that current database is open on unit IFB.
      subroutine edprof(IFB,idbh,ic,iit,ACT,IER)
#include "epara.h"
#include "espriou.h"
      parameter (MGCL=30)
      parameter (MGIT=500)
      COMMON/MOD/MODEL
      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/PMENU/MHEAD,MCTL,MIFULL,MFULL,IST,ILEN,IPM,MPM,IPFLG

C Generic db commons:
      common/gendbf/gendbfn(13)
      common/gendb1/idbwidth(13),lastr(13),nbdbdoc(13),nbunit(13),
     &              nbsrc(13),nbcat(13),nbitem(13),idbver(13)      
      common/gendb2/dbtag(13),dbmenu(13),dbdate(13),dbnote(13),dbdoc(10)
      common/gendb3/irscat(MGCL),nbcatdoc(MGCL),nbcatit(MGCL)
      common/gendb4/cattag(MGCL),cattype(MGCL),catmenu(MGCL),
     &              catdate(MGCL),catdoc(5)

      common/gendb6/ittag(MGIT),itmenu(MGIT)
      common/gendb8a/ituntstr,itsrcstr,itcnfstr,itdate(MGIT),itdoc
      common/gendbtg/showtags

      dimension DayT(72),ST(72),A(72),B(72),C(72)
      dimension PELM(72),Patr(72)

      CHARACTER OUTSTR*124,PELM*38,h*72,ACT*1,dstmp*24
      character dbtag*12,dbmenu*32,dbdate*24,dbdoc*248,catdoc*248
      character cattag*12,cattype*8,catmenu*32,catdate*24
      character ittag*12,itmenu*32,itdate*24,itdoc*248
      character dbnote*72,outs*124,outs248*248,hold*60
      character ituntstr*42,itsrcstr*1,itcnfstr*42
      character ustr*12,unote*72,DayT*8,KEY*1,Patr*8,head*38
      character gendbfn*72,snote*248
      logical primod,OK,dok,showtags

C Scan the data from the common blocks.
    9 primod=.false.
      write(currentfile,'(a)')  gendbfn(idbh)(1:lnblnk(gendbfn(idbh)))
      call scandbhd(IFB,idbh,ier)
      call rgenitem(IFB,idbh,iit,ier)
      call prsprof(idbh,iit,'g',NCG,Patr,DayT,ST,A,B,C,ier)
      call prssrc(idbh,snote)

C If user has request a report only display the relevant table.
      if(ACT(1:1).eq.'T')then
        la=lnblnk(ittag(iit))
        lb=lnblnk(itmenu(iit))
        ld=lnblnk(dbmenu(idbh))
        write(outs248,'(8a)') 'In ',dbmenu(idbh)(1:ld),', item ',
     &    itmenu(iit)(1:lb),' (tag ',ittag(iit)(1:la),
     &    ') edited on ',itdate(iit)
        call edisp248(iuout,outs248,70)
        call edisp(iuout,' Source & notes:')
        call edisp248(iuout,snote,70)
        call edisp248(iuout,itdoc,70)
        call edisp(iuout,'  ')

        call edisp(iuout,' ')
        write(outs,'(A,i2)') ' Periods : ',NCG
        call edisp(iuout,outs)
        call edisp(iuout,' day type |start   | data for period ')
        do 30 ip=1,NCG
          write(outs,'(i3,1x,a,f6.3,3F9.4)')
     &        ip,DayT(ip),ST(ip),A(ip),B(ip),C(ip)
          call edisp(iuout,outs)
  30    continue
        call edisp(iuout,' ')
        return
      endif

C Draw intial graphics.
      IF(MODEL.EQ.8)call PROGRF(IFB,idbh,ic,iit)

C If anything has changed it needs to be written out. First
C remove spaces in data lines(s).
   3  if(primod.and.ACT(1:1).eq.'M')then
        call usrmsg('Updating item data... ',' ','-')
        call prsprof(idbh,iit,'p',NCG,Patr,DayT,ST,A,B,C,ier)

C Update db data, catg date and item record(s).
        call dstamp(dstmp)
        write(dbdate(idbh),'(a)') dstmp
        write(catdate(ic),'(a)') dstmp
        write(itdate(iit),'(a)') dstmp
        IREC=2
        write(IFB,REC=IREC,IOSTAT=IOS,ERR=1)dbdate(idbh),dbnote(idbh)
        IREC=irscat(ic)
        write(IFB,REC=IREC,IOSTAT=IOS,ERR=1)cattag(ic),cattype(ic),
     &    catmenu(ic),nbcatdoc(ic),nbcatit(ic),catdate(ic)
        call wgenitem(IFB,idbh,iit,ier)
        primod=.false.
        call usrmsg('Updating item data...done.',' ','P')

C Update the graphics.
        IF(MODEL.EQ.8)call PROGRF(IFB,idbh,ic,iit)
      endif

C Setup for paging menu.
  39  MHEAD=8
      if(ACT.eq.'M'.or.ACT.eq.'m')then
        MCTL=4
      elseif(ACT.eq.'-')then
        MCTL=3
      endif
      ILEN=NCG
      IPACT=CREATE
      CALL EKPAGE(IPACT)

C Initial menu entry setup.
      IW=-4
      write(PELM(1),'(A,A)')    '  category: ',catmenu(ic)(1:26)
      if(ACT(1:1).eq.'M')then
        write(PELM(2),'(a,a)')  'a tag: ',ittag(iit)
        write(PELM(3),'(A,A)')  'b name: ',itmenu(iit)(1:30)
      else
        write(PELM(2),'(a,a)')  '  tag: ',ittag(iit)
        write(PELM(3),'(A,A)')  '  name: ',itmenu(iit)(1:30)
      endif
      write(PELM(4),'(A,A)')    '  date: ',itdate(iit)(1:24)
      if(ACT(1:1).eq.'M')then
        write(PELM(5),'(3a)')   'c source: ',snote(1:27)
        write(PELM(6),'(2a)')   'd notes : ',itdoc(1:24)
      else
        write(PELM(5),'(3a)')   '  source: ',snote(1:27)
        write(PELM(6),'(2a)')   '  notes : ',itdoc(1:24)
      endif
      write(PELM(7),'(a,i2,a)') '  _____periods (',NCG,')___________ '
      PELM(8)=                  ' day type  start  data...         '
      M=MHEAD

C Build up a display.
      do 34 i=1,ILEN
        if(i.GE.IST.AND.(i.LE.(IST+MIFULL)))then
          M=M+1
          CALL EMKEY(M-MHEAD,KEY,IER)
          if(ACT(1:1).eq.'M')then
            write(PELM(M),'(a1,1x,a,1x,f5.2,3F7.1)')
     &        KEY,DayT(i)(1:6),ST(i),A(i),B(i),C(i)
          else
            write(PELM(M),'(2x,a,1x,f5.2,3F7.1)')
     &        DayT(i)(1:6),ST(i),A(i),B(i),C(i)
          endif
        endif
  34  continue

C If a long list include page facility text.      
      IF(IPFLG.EQ.0)THEN
        PELM(M+1)='  ____________________________  '
      ELSE
        WRITE(PELM(M+1),15)IPM,MPM 
   15   FORMAT   ('0 page ---- Part: ',I2,' of ',I2,' ---')
      ENDIF
      if(ACT(1:1).eq.'M')then
        PELM(M+2)=          '* add/delete period             '
        PELM(M+3)=           '? help                          '
        PELM(M+4)=           '- exit                          '
      else
        PELM(M+2)=           '? help                          '
        PELM(M+3)=          '- exit                          '
      endif
      MPLEM=M+MCTL

C Help text for this menu.
  2   h(1)='Editing of event profile database entities... '
      h(2)=' ...'
      h(3)=' ...'

      write(head,'(2a)') '  Item in ',dbmenu(idbh)(1:27)
      CALL EMENU(head,PELM,MPLEM,IW)

      IF(IW.EQ.0.OR.IW.EQ.MPLEM)THEN
        RETURN
      ELSEIF(IW.EQ.MPLEM-1)THEN

C Help.
        CALL PHELPD('item edit help',3,'-',0,0,IER)
      ELSEIF(IW.EQ.(MPLEM-3).and.(ACT.eq.'M'.or.ACT.eq.'m'))THEN

C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        ENDIF
        goto 39
      ELSEIF(IW.EQ.(MPLEM-2).and.ACT.eq.'-')THEN

C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        ENDIF
        goto 39
      ELSEIF(IW.EQ.2.or.IW.eq.3.or.IW.eq.5.or.IW.eq.6)THEN
        iract=0
        call EITCOMMON(IFB,idbh,ic,iit,IW,ACT,iract,primod,IER)
        if(iract.eq.3)goto 3
        if(iract.eq.9)goto 9
      ELSEIF(IW.EQ.MPLEM-2.and.ACT(1:1).eq.'M')THEN

C Add or delete a period.
        h(1)='You can delete an existing period, append a new period'
        h(2)='at the end of the list or insert a period within the '
        h(3)='list (you will be asked to select a point of insertion).'
        CALL EASKABCD(' Period options: ',' ',' delete ',
     &    ' append',' insert ','contine',IWA,3)
        if(IWA.eq.4)goto 3
        if(IWA.EQ.1)then

C If no of periods is edited then ask the user which one to remove.
C Now loop through the periods and copy the contents of periods 
C IWHICH into the next lower one.
           CALL EMENU(' Period to remove',PELM,MPLEM,IWR)
           CALL KEYIND(MPLEM,IWR,IWHICH,IO)
           write(outs,'(A,i2)')' Remove period ',iwhich
           dok=.false.
           h(1)='Chance to correct any selection errors. '
           CALL ASKOK(outs,' Are you sure?',OK,dok,1)
           IF(.NOT.OK)GOTO 3
           DO 791 IVV=IWHICH,NCG-1
             DayT(IVV)=DayT(IVV+1)
             Patr(IVV)=Patr(IVV+1)
             ST(IVV)=ST(IVV+1)
             A(IVV)=A(IVV+1)
             B(IVV)=B(IVV+1)
             C(IVV)=C(IVV+1)
  791     CONTINUE
          NCG=NCG-1
          primod=.true.
        elseif(IWA.EQ.2.or.IWA.eq.3)THEN
          if(NCG.lt.60)then
            if(IWA.EQ.3)THEN
              CALL EMENU('Point of insertion',PELM,MPLEM,IWR)
              CALL KEYIND(MPLEM,IWR,IWHICH,IO)
C              ILN=IWHICH
              NCG=NCG+1
              IVV=NCG+1
  792         CONTINUE
              IVV=IVV-1
              DayT(IVV)=DayT(IVV-1)
              Patr(IVV)=Patr(IVV-1)
              ST(IVV)=ST(IVV-1)
              A(IVV)=A(IVV-1)
              B(IVV)=B(IVV-1)
              C(IVV)=C(IVV-1)
              IF(IVV.GT.IWHICH+1) GOTO 792
              il=IWHICH
            else
              NCG=NCG+1
              il=NCG
            endif

C Set default values for new perod. Assume it starts an hour after
C the previous period unless the next period is at this time.
            DayT(il)='all'
            Patr(il)='none'
            if(ST(il-1).lt.23.0)then
              if(il.eq.NCG)then
                ST(il)=ST(il-1)+1.0
              elseif(il.lt.NCG)then
                diff=ST(il+1)-ST(il-1)
                if(diff.le.1.0)ST(il)=diff/2.0
                if(diff.gt.1.0)ST(il)=real(int(diff/2.0))
              endif
            else
              ST(il)=23.5
            endif
            A(il)=0.0
            B(il)=0.0
            C(il)=0.0
            primod=.true.
          endif
        endif
      ELSEIF(IW.GT.MHEAD.AND.IW.LT.(MPLEM-MCTL+1))THEN
        CALL KEYIND(MPLEM,IW,IFOC,IO)
        h(1)='A profile period is defined by a start time and 5 data '
        h(2)='items. For casual gains the first two are used to '
        h(3)='represent sensible and latent loads. '
        write(hold,'(f6.3,3F9.4)') ST(ifoc),A(ifoc),B(ifoc),C(ifoc)
        write(outs,'(a,i2,a)')' For period',ifoc,
     &    ' the start time and 3 data (see help):'
  42    CALL EASKS(hold,outs,' ',60,' 1.0 0.0 0.0 0.0   ',
     &    'period',IER,3)
        if(ier.eq.0.and.hold(1:2).ne.'  ')then
          k=0
          CALL EGETWR(hold,K,ST(ifoc),0.00,24.0,'W','start',IER)
          CALL EGETWR(hold,K,A(ifoc),0.0,0.0,'-','a val',IER)
          CALL EGETWR(hold,K,B(ifoc),0.0,0.0,'-','b val',IER)
          CALL EGETWR(hold,K,C(ifoc),0.0,0.0,'-','c val',IER)
          primod=.true.
        else
          goto 42
        endif
      ELSE
        IW=-1
        GOTO 2
      ENDIF
      GOTO 3

    1 call edisp(iuout,'Error updating data record')
      call edisp(iuout,outstr)
      return

      END

C *********** PROGRF
C 'PROGRF' outputs a graphical display of the current event profile.
      subroutine progrf(IFB,idbh,ic,iit)
      parameter (MGIT=500)
      PARAMETER  (MC=35)
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/GFONT/IFS,ITFS,IMFS
      common/gendb6/ittag(MGIT),itmenu(MGIT)

C      DIMENSION VAL1(1440),VAL2(1440)
      DIMENSION VAL1(288),VAL2(288),VAL3(288)
      dimension DayT(72),ST(72),A(72),B(72),C(72),Patr(72)
      character DayT*8,Patr*8,ETEXT*72
      character ittag*12,itmenu*32
      logical vclose1,vclose2

      call prsprof(idbh,iit,'g',NCG,Patr,DayT,ST,A,B,C,ier)

      YMAX=100.
      YMIN=0.

C Set up data ready for graph drawing.
      call startbuffer()
      call axiscale(igw,igwh,0.0,24.0,YMIN,YMAX,xsc,ysc,sca,Xadd,Yadd)
      call linescale(igl,Xadd,xsc,igb,Yadd,ysc)
      call horaxis(0.,24.,igl,igr,igb,Xadd,xsc,1,'Time (minutes)')
      call vrtaxis(YMIN,YMAX,igl,igb,igt,Yadd,ysc,0,0,'Percent')

C Pick up the data increment for the horizontal axis.
      call dinterval(0.,24.,DT,NDEC,1)

C Print header
      WRITE(ETEXT,'(2a)')'Profile: ',itmenu(iit)
      call viewtext(ETEXT,1,0,IFS)

      DO 60 I=1,288
        VAL1(I)=0.
        VAL2(I)=0.
        VAL3(I)=0.
   60 CONTINUE

C Assign values of each event. Loop through at 5 minute intervals and
C check if time is within one of the profile periods.
      DO 30 J=1,288
        time=(real(J)/12.0)
        DO 20 I=1,NCG

c Period of each event.
          if(I.lt.NCG)then
            if(time.gt.ST(I).and.time.le.ST(I+1))then
              VAL1(J)=A(I)
              VAL2(J)=B(I)
              VAL3(J)=C(I)
            endif
          else
            if(time.gt.ST(I))then
              VAL1(J)=A(I)
              VAL2(J)=B(I)
              VAL3(J)=C(I)
            endif
          endif
   20   CONTINUE
   30 CONTINUE

C Plot VAL1 array. If both timestep values are zero then do not
C bother to plot.
      IPEN=1
      CALL ETPLOT(0.0,0.0,0,0)
      DO 40 I=1,287
        MARK=0
        time1=(real(I)/12.0)
        time2=(real(I+1)/12.0)
        call eclose(VAL1(I),0.0,0.001,vclose1)
        call eclose(VAL1(I+1),0.0,0.001,vclose2)
        if(vclose1.and.vclose2)then
          continue
        else
          CALL ETPLOT(time1,VAL1(I),0,0)
          CALL ETPLOT(time2,VAL1(I+1),1,0)
        endif
        call eclose(VAL2(I),0.0,0.001,vclose1)
        call eclose(VAL2(I+1),0.0,0.001,vclose2)
        if(vclose1.and.vclose2)then
          continue
        else
          CALL ETPLOT(time1,VAL2(I),0,0)
          CALL ETPLOT(time2,VAL2(I+1),-6,0)
        endif
        call eclose(VAL3(I),0.0,0.001,vclose1)
        call eclose(VAL3(I+1),0.0,0.001,vclose2)
        if(vclose1.and.vclose2)then
          continue
        else
          CALL ETPLOT(time1,VAL3(I),0,0)
          CALL ETPLOT(time2,VAL3(I+1),-106,0)
        endif
   40 CONTINUE

      return

      END

C ******************** ELSTRIPC 
C ELSTRIPC strips comments from a ASCII file (extremely long) string and returns the data.
C It assumes that if a string begins with a '#' then the whole line is 
C a comment an the next line is read.  If a ' #' is discovered within
C a line the rest of the line is removed. 
C IER=0 if ok. MSG is a text string used in error messages. If
C IR=0 then acts silently, otherwise notes when EOF found.
C IEXP is the number of expected items in the line: 
C   IEXP = 0 means don't care or already know no. items - don't check
C   IEXP >0  means a specific number of items expected (error if not)
C   IEXP = 99 check number of items and return in ITEMS
 
      SUBROUTINE ELSTRIPC(INPCH,OUTSTR,IEXP,ITEMS,IR,MSG,IER)
#include "espriou.h"

      CHARACTER*600 OUTSTR,STRING,TMP
      CHARACTER*124 MSG1
      CHARACTER*(*) MSG

C Read a line of the file, strip off any trailing blanks, if the first
C character is a # then read the next line from the file.
      IER=0
    8 READ(INPCH,10,IOSTAT=ISTAT,ERR=101,END=102)STRING
   10 FORMAT(A)
      tmp=STRING(1:LNBLNK(STRING))

C Take the string and check for a #, discarding any text which follows.
      iloc = INDEX(tmp,'#')
      if(iloc.eq.1)then
        goto 8
      elseif(iloc.eq.0)then
        OUTSTR=tmp
      elseif(iloc.gt.1)then
        OUTSTR=tmp(1:ILOC-1)
      endif

C Find out the number of separate words/string groupings.
      if(IEXP.eq.99)then
        CALL ELCHITMS(OUTSTR,ITEMS)
      elseif(IEXP.eq.0)then
        ITEMS=0
      elseif(IEXP.gt.0)then
        CALL ELCHITMS(OUTSTR,ITEMS)
        if(IEXP.ne.ITEMS)then
          if(currentfile(1:2).ne.'  ')then
            WRITE(tmp,'(3A,I3,3A)')' In ',
     &        currentfile(1:lnblnk(currentfile)),' expecting',IEXP,
     &        ' items (',MSG(1:lnblnk(MSG)),') in...'
            CALL LUSRMSG(tmp,OUTSTR,'F')
          else
            WRITE(tmp,'(A,I3,A,A,A)')' Expecting',IEXP,' items (',
     &         MSG(1:lnblnk(MSG)),') in...'
            CALL LUSRMSG(tmp,OUTSTR,'F')
          endif
        endif
      endif
      
    4 RETURN

  101 IER=1
      IF(IR.EQ.1)THEN
        if(currentfile(1:2).ne.'  ')then
          WRITE(MSG1,'(5A)')' In ',currentfile(1:lnblnk(currentfile)),
     &      ' error reading (',MSG(1:lnblnk(MSG)),') in...'
        else
          WRITE(MSG1,'(A,A)',IOSTAT=ISTAT,ERR=1)' Error reading: ',
     &      MSG(1:lnblnk(MSG)),') in...'
        endif
        CALL LUSRMSG(MSG1,OUTSTR,'W')
      ENDIF
      goto 4

  102 IER=2
      IF(IR.EQ.1)THEN
        if(currentfile(1:2).ne.'  ')then
          WRITE(tmp,'(5A)')' In ',currentfile(1:lnblnk(currentfile)),
     &      ' error reading (',MSG(1:lnblnk(MSG)),') EOF sensed.'
        else
          WRITE(tmp,'(3A)',IOSTAT=ISTAT,ERR=1)' Error reading ',
     &      MSG(1:lnblnk(MSG)),' EOF sensed.'
        endif
        CALL LUSRMSG(' ',tmp,'-')
      ENDIF
      goto 4
   1  write(6,*) 'STRIPC: error writing warning re: ',OUTSTR

      END


c ******************** ELUSRMSG
c Generic error reporting facility.  MSG1
C and MSG2 are text strings (upto 600 char) to be printed.  LEVEL is a
C single character 'W' or 'w' for warning (in graphic mode followed by
C a clearing of dialog), 'F' or 'f' for Fatal error,
C '-' to only print the messages, '?' is a prompt in text mode. If
C LEVEL is 'P' or 'p' then pause breifly before continuing. In the case of a 
C fatal error STOP will be called, otherwise execution will return to
C the calling point.
C In the case of a graphic menu the c function msg_box will be called
C and the messages will appear in a dialogue box at the bottom of the 
C graphic window. The user must have previously called open_msg_box(2).
 
      SUBROUTINE ELUSRMSG(MSG1,MSG2,LEVEL)
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      common/OUTIN/IUOUT,IUIN
      CHARACTER*(*) MSG1,MSG2
      CHARACTER outs*124,LEVEL*1,blnk*2,blnk2*2,cont*8

      IF(MSG2(1:1).EQ.'-')MSG2=' '
      blnk='  '
      blnk2='  '
      cont='continue'

C Print out message according to the combination of strings passed. Use
C the dialogue box if terminal type 8.
      IF(MMOD.NE.8)THEN
        IF(LEVEL.EQ.'-'.or.LEVEL.eq.'P'.or.LEVEL.eq.'p')THEN
          if(lnblnk(MSG1).gt.124)then
            WRITE(outs,'(A)',iostat=ios,err=1) MSG1(1:124)
          else
            WRITE(outs,'(A)',iostat=ios,err=1) MSG1(1:LNBLNK(MSG1))
          endif
          call edisp(iuout,outs)
        ELSEIF(LEVEL.EQ.'W'.OR.LEVEL.EQ.'w')THEN
          if(lnblnk(MSG1).gt.114)then
            WRITE(outs,201,iostat=ios,err=1)MSG1(1:114)
          else
            WRITE(outs,201,iostat=ios,err=1)MSG1(1:LNBLNK(MSG1))
          endif
 201      FORMAT(' Warning: ',a)
          call edisp(iuout,outs)
        ELSEIF(LEVEL.EQ.'F'.OR.LEVEL.EQ.'f')THEN
          if(lnblnk(MSG1).gt.110)then
            WRITE(outs,202,iostat=ios,err=1)MSG1(1:110)
          else
            WRITE(outs,202,iostat=ios,err=1)MSG1(1:LNBLNK(MSG1))
          endif
 202      FORMAT(' Fatal error: ',a)
          call edisp(iuout,outs)

C Single line prompt.
        ELSEIF(LEVEL.EQ.'?'.AND.MSG2(1:LNBLNK(MSG2)).EQ.' ')THEN
          if(lnblnk(msg1).gt.124)then
            WRITE(iuout,203,iostat=ios,err=1)MSG1(1:124)
          else
            WRITE(iuout,203,iostat=ios,err=1)MSG1(1:LNBLNK(MSG1))
          endif
 203      FORMAT(a,' ',$)
        ELSEIF(LEVEL.EQ.'?'.AND.MSG2(1:LNBLNK(MSG2)).NE.' ')THEN
          if(lnblnk(msg1).gt.124)then
            WRITE(outs,'(A)',iostat=ios,err=1)MSG1(1:124)
          else
            WRITE(outs,'(A)',iostat=ios,err=1)MSG1(1:LNBLNK(MSG1))
          endif
          call edisp(iuout,outs)
        ELSE
          call edisp(iuout,' Incorrect syntax in USRMSG ')
        ENDIF

        IF(MSG2(1:LNBLNK(MSG2)).NE.' ')THEN
          IF(LEVEL.EQ.'?')THEN
            WRITE(iuout,203,iostat=ios,err=1) MSG2(1:LNBLNK(MSG2))
          ELSE
            if(lnblnk(msg2).gt.124)then
              WRITE(outs,'(A)',iostat=ios,err=1) MSG2(1:124)
            else
              WRITE(outs,'(A)',iostat=ios,err=1) MSG2(1:LNBLNK(MSG2))
            endif
            call edisp(iuout,outs)
          ENDIF
        ENDIF
        IF(LEVEL.EQ.'F'.OR.LEVEL.EQ.'f')then
          call pauses(1)
          STOP
        endif
        RETURN
      ELSEIF(MMOD.EQ.8)THEN
        IF(LEVEL.EQ.'-'.OR.LEVEL.EQ.'?')THEN
          CALL msgbox(MSG1,MSG2)
        ELSEIF(LEVEL.EQ.'W'.OR.LEVEL.EQ.'w')THEN

C If warning mode then clear continuebox after use clicks continue.
          CALL continuebox(MSG1,MSG2,cont)
          CALL msgbox(blnk,blnk2)
        ELSEIF(LEVEL.EQ.'P'.OR.LEVEL.EQ.'p')THEN
          CALL msgbox(MSG1,MSG2)
          call pauses(1)
        ELSEIF(LEVEL.EQ.'F'.OR.LEVEL.EQ.'f')THEN

C Place in text window since about to exit from program.
          WRITE(iuout,202,iostat=ios,err=1)MSG1(1:LNBLNK(MSG1))
          WRITE(iuout,'(A)',iostat=ios,err=1)MSG2(1:LNBLNK(MSG2))
        ELSE
          CALL msgbox(' ',' Incorrect syntax in USRMSG.')
          call pauses(1)
        ENDIF

        IF(LEVEL.EQ.'F'.OR.LEVEL.EQ.'f')then
          call pauses(2)
          STOP
        endif
        RETURN
      ENDIF
    1 call edisp(iuout,'USRMSG: error composing prompt. ')
      return
      END


C ********************* ELCHITMS 
C ELCHITMS checks an extremely long character string (A), returning the number of data
C items (IW) separated by ' ' tab or ','.  Note to keep from 
C overwriting the string passed, deal with a copy.
      SUBROUTINE ELCHITMS(A,IW)
      CHARACTER*(*) A
      CHARACTER B*600,C*1

      lastcc = MIN0(LNBLNK(A),599)
      WRITE(B,'(A)',IOSTAT=ISTAT,ERR=1) A(1:lastcc+1)
C      WRITE(B,'(A)') A(1:LNBLNK(A)+1)

C Loop through B and convert all ',' and 'tabs' to blanks.
      DO 100 I=1,LNBLNK(B)+1
        C=B(I:I)
        IF(C.EQ.','.OR.ICHAR(C).EQ.9) B(I:I)=' '
  100 CONTINUE

C Loop through B and see how many 'words'.
      K=0
      IW=0
  102 IW=IW+1

C Start by skipping blanks before the word.
   10 K=K+1
      C=B(K:K)
      IF(C.EQ.' ') GOTO 10

C Loop character by character until separator is found.
   20 K=K+1
      C=B(K:K)
      IF(K.GE.LNBLNK(B))RETURN
      IF(C.EQ.' ') GOTO 102
      GOTO 20
   1  write(6,*) 'CHITMS: error in internal write: ',A
 
      END

