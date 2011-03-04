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

C ************* EDENVCI
C Environmental construction impacts display and editing. If ACT='T'
C then produce a tabular listing and return, otherwise present editing menu.
C Assumes that current database is open on unit IFB.
      subroutine edenvci(IFB,idbh,ic,iit,ACT,IER)
#include "espriou.h"
C espriou.h provides currentfile.
#include "esprdbfile.h"
C providing paramaters MGDB,MGCL,MGIT
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN

C Generic db commons:
      common/gendb2/dbtag(MGDB),dbmenu(MGDB),dbdate(MGDB),dbnote(MGDB),
     &              dbdoc(10)
      common/gendb3/irscat(MGCL),nbcatdoc(MGCL),nbcatit(MGCL)
      common/gendb4/cattag(MGCL),cattype(MGCL),catmenu(MGCL),
     &              catdate(MGCL),catdoc(5)

      common/gendb6/ittag(MGIT),itmenu(MGIT)
      common/gendb8a/ituntstr,itsrcstr,itcnfstr,itdate(MGIT),itdoc
      common/gendb8b/itdat(20),itrep(20)

      dimension PELM(21)
      dimension trbsn(3),dtrbs(3),rtrbs(3),assite(2),lasmat(2)

      CHARACTER OUTSTR*124,PELM*38,ACT*1,dstmp*24,kact*1
      character dbtag*12,dbmenu*32,dbdate*24,dbdoc*248,catdoc*248
      character cattag*12,cattype*8,catmenu*32,catdate*24
      character ittag*12,itmenu*32,itdate*24,itdoc*248,head*38
      character dbnote*72,ituntstr*42,itsrcstr*1,itcnfstr*42
      character hold*32,selitem*12,item*12,class*12
      character assite*12,lassurn*12,laspern*12,trbsn*12,lasmat*12
      character itdat*248,itrep*248,selmenu*32,trntag*12,trnmenu*32
      character lassurm*32,lasperm*32,asitm1*32,asitm2*32,lasm1*32
      character lasm2*32
      character snote*248
      logical primod
      integer MPLEM,IW ! max items and current menu item

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='edenvci'  ! set for subroutine
      

C Scan the data from the common blocks.
    9 primod=.false.
      write(currentfile,'(a)')  gendbfn(idbh)(1:lnblnk(gendbfn(idbh)))
      call scandbhd(IFB,idbh,ier)
      call rgenitem(IFB,idbh,iit,ier)

C Depending on the category call different encoding/decoding.
      call prsenvci(idbh,iit,'g',lassurn,laspern,ntrn,trbsn,
     &  dtrbs,rtrbs,assite,lasmat,ier)
      call prssrc(idbh,snote)

      call prsmenu(8,lassurn,lassurm,iassur,iasscat)
      call prsmenu(8,laspern,lasperm,iasper,iaspcat)
      call prsmenu(8,assite(1),asitm1,iasit1,iasitcat)
      call prsmenu(8,assite(2),asitm2,iasit2,iasitcat)
      call prsmenu(8,lasmat(1),lasm1,iasm1,iasm1cat)
      call prsmenu(8,lasmat(2),lasm2,iasm2,iasm2cat)

C After parsing menus recover the header of the current db.
      IFB=igendbf(idbh)
      call scandbhd(IFB,idbh,ier)

C If user has request a report only display the relevant table.
      if(ACT(1:1).eq.'T')then
        call edisp(iuout,'Should have called epopenv...')
        return
      endif

C If anything has changed it needs to be written out. First
C remove spaces in data lines(s).
   3  if(primod.and.ACT(1:1).eq.'M')then
        call usrmsg('Updating item data... ',' ','-')
        call prsenvci(idbh,iit,'p',lassurn,laspern,ntrn,trbsn,
     &   dtrbs,rtrbs,assite,lasmat,ier)
        call edisp(iuout,' The revised data is... ')
        call edisp(iuout,itdat(1))
        call edisp(iuout,itdat(2))
        call edisp(iuout,itdat(3))

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
      endif

C Make up the display.
      IW=-4
      write(PELM(1),'(2A)')    '  category: ',catmenu(ic)(1:26)
      write(PELM(2),'(3A)') kact(act,'a'),' tag: ',ittag(iit)
      write(PELM(3),'(3A)') kact(act,'b'),' name: ',itmenu(iit)(1:30)
      write(PELM(4),'(2A)')    '  date: ',itdate(iit)(1:24)
      write(PELM(5),'(3a)') kact(act,'c'),' source: ',snote(1:27)
      write(PELM(6),'(3a)') kact(act,'d'),' notes: ',itdoc(1:24)
      PELM(7)=                 '  _________________________________ '
      la=MIN0(lnblnk(lassurm),20)
      lb=MIN0(lnblnk(lasperm),20)
      lc=MIN0(lnblnk(asitm1),19)
      ld=MIN0(lnblnk(asitm2),19)
      le=MIN0(lnblnk(lasm1),19)
      lf=MIN0(lnblnk(lasm2),19)
      write(PELM(8),'(5a)') kact(act,'e'),' assembly (sur): ',
     &  lassurm(1:la)
      write(PELM(9),'(5a)') kact(act,'f'),' assembly (per): ',
     &  lasperm(1:lb)
      write(PELM(10),'(a)')
     &  '  _transport_distance_loss__name____'
      write(PELM(11),'(2a,f7.1,f6.2,2a)') kact(act,'g'),
     &  ' 1st >site:',dtrbs(1),rtrbs(1),' ',
     &  trbsn(1)(1:lnblnk(trbsn(1)))
      write(PELM(12),'(2a,f7.1,f6.2,2a)') kact(act,'h'),
     &  ' 2nd >site:',dtrbs(2),rtrbs(2),' ',
     &  trbsn(2)(1:lnblnk(trbsn(2)))
      write(PELM(13),'(2a,f7.1,f6.2,2a)') kact(act,'i'),
     &  ' 3rd >site:',dtrbs(3),rtrbs(3),' ',
     &  trbsn(3)(1:lnblnk(trbsn(3)))
      write(PELM(14),'(3a)') kact(act,'j'),' 1st site asmbly: ',
     &  asitm1(1:lc)
      write(PELM(15),'(3a)') kact(act,'k'),' 2nd site asmbly: ',
     &  asitm2(1:ld)
      write(PELM(16),'(3a)') kact(act,'l'),' 1st maintenance: ',
     &  lasm1(1:le)
      write(PELM(17),'(3a)') kact(act,'m'),' 2nd maintenance: ',
     &  lasm2(1:lf)
      PELM(18)=           ' ___________________________________ '
      PELM(19)=           '? Help                               '
      PELM(20)=           '- Exit                               '
      MPLEM=20

C Help text for this menu.
  2   helptopic='dbm_trn_asmb_impacts'
      call gethelptext(helpinsub,helptopic,nbhelp)

      write(head,'(2a)') '  Item in ',dbmenu(idbh)(1:27)
      CALL EMENU(head,PELM,MPLEM,IW)

      IF(IW.EQ.0.OR.IW.EQ.MPLEM)THEN
        RETURN
      ELSEIF(IW.EQ.MPLEM-1)THEN

C Help.
        helptopic='dbm_trn_asmb_impacts'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('construction effect edit help',nbhelp,'-',0,0,IER)
      ELSEIF(IW.EQ.2.or.IW.eq.3.or.IW.eq.5.or.IW.eq.6)THEN
        iract=0
        call EITCOMMON(IFB,idbh,ic,iit,IW,ACT,iract,primod,IER)
        if(iract.eq.3)goto 3
        if(iract.eq.9)goto 9
      elseif(IW.EQ.8.or.IW.eq.9)then

C Provide browse popup for environmental db (Constr assembly surface).
        if(gendbok(8))then
          class=cattag(ic)
          item=ittag(iit)
          call BKITEM(idbh,class,item,ic,iit)
          if(IW.eq.8)then
            call choseitct('envir_imp',ACT,lassurn,'envir_imp',
     &        'CNSTRASM',selitem,selmenu,itmsel,icatsel,IER)
            if(ACT(1:1).eq.'M'.and.itmsel.ne.0)then
              lassurn=selitem
              lassurm=selmenu
              primod=.true.
            endif
          elseif(IW.eq.9)then
            call choseitct('envir_imp',ACT,laspern,'envir_imp',
     &        'CNSTRASM',selitem,selmenu,itmsel,icatsel,IER)
            if(ACT(1:1).eq.'M'.and.itmsel.ne.0)then
              laspern=selitem
              lasperm=selmenu
              primod=.true.
            endif
          endif
          call RCVITEM(idbh,class,item,ic,iit)
          if(item.ne.ittag(iit))then
            call edisp(iuout,'woops: got confused recovering item.')
          endif
        else
          call usrmsg('Sorry, environ impacts not available.',' ','P')
        endif
      elseif(IW.eq.11.or.IW.eq.12.or.IW.eq.13)then

C Provide browse or popup for 1st transport attributes. After choseitem
C and prsmenu re-establish the current database header.
        if(gendbok(9))then
          if(IW.eq.11)trntag=trbsn(1)
          if(IW.eq.12)trntag=trbsn(2)
          if(IW.eq.13)trntag=trbsn(3)
          call prsmenu(9,trntag,trnmenu,itrn,itrncat)
          IFB=igendbf(idbh)
          call scandbhd(IFB,idbh,ier)
          class=cattag(ic)
          item=ittag(iit)
          call BKITEM(idbh,class,item,ic,iit)
          call choseitem('transport',ACT,trntag,'envir_imp',selitem,
     &      selmenu,itmsel,icatsel,IER)
          IFB=igendbf(idbh)
          call scandbhd(IFB,idbh,ier)
          if(ACT(1:1).eq.'M'.and.itmsel.ne.0)then
            if(IW.eq.11)trbsn(1)=selitem
            if(IW.eq.12)trbsn(2)=selitem
            if(IW.eq.13)trbsn(3)=selitem
            primod=.true.
          endif
          call RCVITEM(idbh,class,item,ic,iit)
          if(item.ne.ittag(iit))then
            call edisp(iuout,'woops: got confused recovering item.')
          endif

C Edit 1st transport distance and breakage rate.
          if(IW.eq.11)write(hold,'(f8.2,f7.3)') dtrbs(1),rtrbs(1)
          if(IW.eq.12)write(hold,'(f8.2,f7.3)') dtrbs(2),rtrbs(2)
          if(IW.eq.13)write(hold,'(f8.2,f7.3)') dtrbs(3),rtrbs(3)
          CALL EASKS(hold,'Transport distance (km) and breakage rate:',
     &      ' ',32,'1.00 0.00','trn1 dist and brk',IER,nbhelp)
          if(ier.eq.0.and.hold(1:2).ne.'  ')then
            k=0
            if(IW.eq.11)then
             CALL EGETWR(hold,K,dtrbs(1),0.00,0.0,'-','dtrbs',IER)
             CALL EGETWR(hold,K,rtrbs(1),0.00,1.0,'W','rtrbs',IER)
            elseif(IW.eq.12)then
             CALL EGETWR(hold,K,dtrbs(2),0.00,0.0,'-','dtrbs',IER)
             CALL EGETWR(hold,K,rtrbs(2),0.00,1.0,'W','rtrbs',IER)
            elseif(IW.eq.13)then
             CALL EGETWR(hold,K,dtrbs(3),0.00,0.0,'-','dtrbs',IER)
             CALL EGETWR(hold,K,rtrbs(3),0.00,1.0,'W','rtrbs',IER)
            endif
          endif
          primod=.true.
        else
          call usrmsg('Sorry, transport db not available.',' ','W')
        endif
      elseif(IW.EQ.14.or.IW.eq.15)then

C Provide browse popup for environmental db (assembly on site).
        if(gendbok(8))then
          class=cattag(ic)
          item=ittag(iit)
          call BKITEM(idbh,class,item,ic,iit)
          if(IW.eq.14)then
            call choseitct('envir_imp',ACT,assite(1),'envir_imp',
     &        'ASMPRJ  ',selitem,selmenu,itmsel,icatsel,IER)
            if(ACT(1:1).eq.'M'.and.itmsel.ne.0)then
              assite(1)=selitem
              asitm1=selmenu
              primod=.true.
            endif
          elseif(IW.eq.15)then
            call choseitct('envir_imp',ACT,assite(2),'envir_imp',
     &        'ASMPRJ  ',selitem,selmenu,itmsel,icatsel,IER)
            if(ACT(1:1).eq.'M'.and.itmsel.ne.0)then
              assite(2)=selitem
              asitm2=selmenu
              primod=.true.
            endif
          endif
          call RCVITEM(idbh,class,item,ic,iit)
          if(item.ne.ittag(iit))then
            call edisp(iuout,'woops: got confused recovering item.')
          endif
        else
          call usrmsg('Sorry, environ impacts not available.',' ','P')
        endif
      elseif(IW.EQ.16.or.IW.eq.17)then

C Provide browse popup for environmental db (maintenance).
        if(gendbok(8))then
          class=cattag(ic)
          item=ittag(iit)
          call BKITEM(idbh,class,item,ic,iit)
          if(IW.eq.16)then
            call choseitct('envir_imp',ACT,lasmat(1),'envir_imp',
     &        'CNSTRMNT',selitem,selmenu,itmsel,icatsel,IER)
            if(ACT(1:1).eq.'M'.and.itmsel.ne.0)then
              lasmat(1)=selitem
              lasm1=selmenu
              primod=.true.
            endif
          elseif(IW.eq.17)then
            call choseitct('envir_imp',ACT,lasmat(2),'envir_imp',
     &        'CNSTRMNT',selitem,selmenu,itmsel,icatsel,IER)
            if(ACT(1:1).eq.'M'.and.itmsel.ne.0)then
              lasmat(2)=selitem
              lasm2=selmenu
              primod=.true.
            endif
          endif
          call RCVITEM(idbh,class,item,ic,iit)
          if(item.ne.ittag(iit))then
            call edisp(iuout,'woops: got confused recovering item.')
          endif
        else
          call usrmsg('Sorry, environ impacts not available.',' ','P')
        endif
      else
        IW=-1
        GOTO 2
      ENDIF
      GOTO 3

    1 call edisp(iuout,'Error updating data record')
      call edisp(iuout,outstr)
      return

      end
      
C ************* EDENVI
C Environmental impacts for recycling-inceneration-dump processes
C display and editing. If ACT='T' then produce a tabular listing
C and return, otherwise present editing menu.
C Assumes that current database is open on unit IFB.
      subroutine edenvi(IFB,idbh,ic,iit,ACT,IER)
#include "espriou.h"
#include "esprdbfile.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN

C Generic db commons:
      common/gendb2/dbtag(MGDB),dbmenu(MGDB),dbdate(MGDB),dbnote(MGDB),
     &              dbdoc(10)
      common/gendb3/irscat(MGCL),nbcatdoc(MGCL),nbcatit(MGCL)
      common/gendb4/cattag(MGCL),cattype(MGCL),catmenu(MGCL),
     &              catdate(MGCL),catdoc(5)

      common/gendb6/ittag(MGIT),itmenu(MGIT)
      common/gendb8/irsit(MGIT),itcat(MGIT)
      common/gendb8a/ituntstr,itsrcstr,itcnfstr,itdate(MGIT),itdoc
      common/gendb8b/itdat(20),itrep(20)

      dimension PELM(18),trbsn(3),dtrbs(3),eimp(9)

      CHARACTER OUTSTR*124,PELM*44,ACT*1,dstmp*24
      character dbtag*12,dbmenu*32,dbdate*24,dbdoc*248,catdoc*248
      character cattag*12,cattype*8,catmenu*32,catdate*24
      character ittag*12,itmenu*32,itdate*24,itdoc*248
      character dbnote*72,ituntstr*42,itsrcstr*1,itcnfstr*42
      character trbsn*12,ct*8
      character selitem*12,item*12,class*12
      character itdat*248,itrep*248
      character selmenu*32,trntag*12,trnmenu*32,kact*1
      character snote*248
      logical primod
      integer MPLEM,IW ! max items and current menu item

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='edenvi'  ! set for subroutine

C Scan the data from the common blocks.
    9 primod=.false.
      write(currentfile,'(a)')  gendbfn(idbh)(1:lnblnk(gendbfn(idbh)))
      call scandbhd(IFB,idbh,ier)
      call rgenitem(IFB,idbh,iit,ier)

C Depending on the category call different encoding/decoding.
      ict=itcat(iit)
      IREC=irscat(ict)
        read(IFB,REC=IREC,IOSTAT=IOS,ERR=1)cattag(ict),cattype(ict),
     &    catmenu(ict),nbcatdoc(ict),nbcatit(ict),catdate(ict)
      ct=cattype(ict)
      if(ct(1:6).eq.'RECYCP'.or.
     &   ct(1:6).eq.'INCENP'.or.ct(1:5).eq.'DUMPP')then
        call prsenvi(idbh,iit,'g',ntrn,trbsn,dtrbs,eimp,ier)
        call prssrc(idbh,snote)
        GWP=eimp(1)
        AP=eimp(2)
        PS=eimp(3)
        UNRE=eimp(4)
      else
        ier= -1
        call usrmsg('Cannot edit items of this type.',' ','W')
        return
      endif

C If user has request a report only display the relevant table.
      if(ACT(1:1).eq.'T')then
        call edisp(iuout,'Should have called epopenv...')
        return
      endif

C If anything has changed it needs to be written out. First
C remove spaces in data lines(s).
   3  if(primod.and.ACT(1:1).eq.'M')then
        call usrmsg('Updating item data... ',' ','-')
        eimp(1)=GWP
        eimp(2)=AP
        eimp(3)=PS
        eimp(4)=UNRE
        call prsenvi(idbh,iit,'p',ntrn,trbsn,dtrbs,eimp,ier)

        call edisp(iuout,' The revised data is... ')
        call edisp(iuout,itdat(1))
        call edisp(iuout,itdat(2))

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
      endif

C Make up the display.
      IW=-4
      write(PELM(1),'(2A)')    '  category: ',catmenu(ic)(1:26)
      write(PELM(2),'(3A)') kact(act,'a'),' tag: ',ittag(iit)
      write(PELM(3),'(3A)') kact(act,'b'),' name: ',itmenu(iit)(1:30)
      write(PELM(4),'(2A)')    '  date: ',itdate(iit)(1:24)
      write(PELM(5),'(3a)') kact(act,'c'),' source: ',snote(1:27)
      write(PELM(6),'(3a)') kact(act,'d'),' notes: ',itdoc(1:24)
      write(PELM(7),'(a)')
     &  '  _transport_distance__name____'
      write(PELM(8),'(2a,f7.1,2a)') kact(act,'g'),
     &  ' 1st >site:',dtrbs(1),' ',trbsn(1)(1:lnblnk(trbsn(1)))
      write(PELM(9),'(2a,f7.1,2a)') kact(act,'h'),
     &  ' 2nd >site:',dtrbs(2),' ',trbsn(2)(1:lnblnk(trbsn(2)))
      write(PELM(10),'(2a,f7.1,2a)') kact(act,'i'),
     &  ' 3rd >site:',dtrbs(3),' ',trbsn(3)(1:lnblnk(trbsn(3)))
      write(PELM(11),'(2a,1Pe10.3)') kact(act,'h'),
     &  ' global warming poten:',GWP
      write(PELM(12),'(2a,1Pe10.3)') kact(act,'i'),
     &  ' acidification poten :',AP
      write(PELM(13),'(2a,1Pe10.3)') kact(act,'j'),
     &  ' photoSmog depletion :',PS
      write(PELM(14),'(2a,1Pe10.3)') kact(act,'k'),
     &  ' nonrenewable energy :',UNRE
      PELM(15)=                ' __________________________________ '
      PELM(16)=                '? help                              '
      PELM(17)=                '- exit                              '
      MPLEM=17

C Help text for this menu.
      helptopic='dbm_rec_dump_impacts'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Depending on the category use different heading.
   2  if(ct(1:6).eq.'RECYCP')then
        CALL EMENU(' Item in recycling processes',PELM,MPLEM,IW)
      elseif(ct(1:6).eq.'INCENP')then
        CALL EMENU(' Item in incineration processes',PELM,MPLEM,IW)
      elseif(ct(1:5).eq.'DUMPP')then
        CALL EMENU(' Item in dump processes',PELM,MPLEM,IW)
      endif
      IF(IW.EQ.0.OR.IW.EQ.MPLEM)THEN
        RETURN
      ELSEIF(IW.EQ.MPLEM-1)THEN

C Help.
        helptopic='dbm_rec_dump_impacts'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('dump item edit help',7,'-',0,0,IER)
      ELSEIF(IW.EQ.2.or.IW.eq.3.or.IW.eq.5.or.IW.eq.6)THEN
        iract=0
        call EITCOMMON(IFB,idbh,ic,iit,IW,ACT,iract,primod,IER)
        if(iract.eq.3)goto 3
        if(iract.eq.9)goto 9
      ELSEIF(IW.EQ.8.or.IW.EQ.9.or.IW.EQ.10)THEN

C Provide browse or popup for 1st/2nd/3rdtransport attributes.
C Re-establish current db header after calls to prsmenu and choseitem.
        if(gendbok(9))then
          if(IW.eq.8)trntag=trbsn(1)
          if(IW.eq.9)trntag=trbsn(2)
          if(IW.eq.10)trntag=trbsn(3)
          call prsmenu(9,trntag,trnmenu,itrn,itrncat)
          IFB=igendbf(idbh)
          call scandbhd(IFB,idbh,ier)
          class=cattag(ic)
          item=ittag(iit)
          call BKITEM(idbh,class,item,ic,iit)
          call choseitem('transport',ACT,trntag,'envir_imp',selitem,
     &      selmenu,itmsel,icatsel,IER)
          IFB=igendbf(idbh)
          call scandbhd(IFB,idbh,ier)
          if(ACT(1:1).eq.'M'.and.itmsel.ne.0)then
            if(IW.eq.8)trbsn(1)=selitem
            if(IW.eq.9)trbsn(2)=selitem
            if(IW.eq.10)trbsn(3)=selitem
            primod=.true.
          endif
          call RCVITEM(idbh,class,item,ic,iit)
          if(item.ne.ittag(iit))then
            call edisp(iuout,'woops: got confused recovering item.')
          endif

C Edit 1st transport distance and breakage rate.
          if(IW.eq.8)then
            CALL EASKR(dtrbs(1),' ',' 1st transport distance (km):',
     &        0.0,'W',1000.,'W',1.,'dtrbs(1)',IER,nbhelp)
          elseif(IW.eq.9)then
            CALL EASKR(dtrbs(2),' ',' 2nd transport distance (km):',
     &        0.0,'W',1000.,'W',1.,'dtrbs(2)',IER,nbhelp)
          elseif(IW.eq.10)then
            CALL EASKR(dtrbs(3),' ',' 3rd transport distance (km):',
     &        0.0,'W',1000.,'W',1.,'dtrbs(3)',IER,nbhelp)
          endif
          primod=.true.
        else
          call usrmsg('Sorry, transport db not available.',' ','W')
        endif
      ELSEIF(IW.eq.11.or.IW.eq.12.or.IW.eq.13.or.IW.eq.14)THEN

C Edit the impacts.
        if(ACT(1:1).eq.'M')then
          call edimpact(GWP,AP,PS,UNRE,ier)
          primod=.true.
        else
          call usrmsg('Currently in browse mode.',' ','P')
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
      

C ************* EDENVF
C Environmental material effects display and editing. If ACT='T'
C then produce a tabular listing and return, otherwise present editing menu.
C Assumes that current database is open on unit IFB.
      subroutine edenvf(IFB,idbh,ic,iit,ACT,IER)
#include "espriou.h"
#include "esprdbfile.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN

C Generic db commons:
      common/gendb2/dbtag(MGDB),dbmenu(MGDB),dbdate(MGDB),dbnote(MGDB),
     &              dbdoc(10)
      common/gendb3/irscat(MGCL),nbcatdoc(MGCL),nbcatit(MGCL)
      common/gendb4/cattag(MGCL),cattype(MGCL),catmenu(MGCL),
     &              catdate(MGCL),catdoc(5)

      common/gendb6/ittag(MGIT),itmenu(MGIT)
      common/gendb8a/ituntstr,itsrcstr,itcnfstr,itdate(MGIT),itdoc
      common/gendb8b/itdat(20),itrep(20)

      dimension PELM(26),eimp(9),trbsn(3),dtrbs(3),rtrbs(3)

      CHARACTER OUTSTR*124,PELM*38,ACT*1,dstmp*24
      character dbtag*12,dbmenu*32,dbdate*24,dbdoc*248,catdoc*248
      character cattag*12,cattype*8,catmenu*32,catdate*24
      character ittag*12,itmenu*32,itdate*24,itdoc*248
      character dbnote*72,ituntstr*42,itsrcstr*1,itcnfstr*42
      character ustr*12,unote*72,ct*8,trbsn*12,rn*12,in*12,dn*12
      character itdat*248,itrep*248
      character hold*32,selitem*12,item*12,class*12
      character selmenu*32,trntag*12,trnmenu*32,kact*1
      character trnmen1*32,trnmen2*32,trnmen3*32
      character rnmenu*32,inmenu*32,dnmenu*32
      character snote*248
      logical primod
      integer MPLEM,IW ! max items and current menu item

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='edenvf'  ! set for subroutine

C Scan the data from the common blocks.
    9 primod=.false.
      write(currentfile,'(a)')  gendbfn(idbh)(1:lnblnk(gendbfn(idbh)))
      call scandbhd(IFB,idbh,ier)
      call rgenitem(IFB,idbh,iit,ier)

C Depending on the category call different encoding/decoding.
      call prsenvf(idbh,iit,'g',fablif,brkias,ntrn,trbsn,dtrbs,
     &  rtrbs,rn,rr,in,ri,dn,rd,eimp,ier)
      call prssrc(idbh,snote)

      call prsmenu(8,rn,rnmenu,irn,irncat)
      call prsmenu(8,in,inmenu,iin,iincat)
      call prsmenu(8,dn,dnmenu,idn,idncat)
      call prsmenu(9,trbsn(1),trnmen1,itrn,itrncat)
      call prsmenu(9,trbsn(2),trnmen2,itrn,itrncat)
      call prsmenu(9,trbsn(3),trnmen3,itrn,itrncat)

C After parsing menus recover the header of the current db.
      IFB=igendbf(idbh)
      call scandbhd(IFB,idbh,ier)
      GWP=eimp(1)
      AP=eimp(2)
      PS=eimp(3)
      UNRE=eimp(4)

C If user has request a report only display the relevant table.
      if(ACT(1:1).eq.'T')then
        call edisp(iuout,'Should have called epopenv...')
        return
      endif

C If anything has changed it needs to be written out. First
C remove spaces in data lines(s). Recover category data.
   3  if(primod.and.ACT(1:1).eq.'M')then
        call usrmsg('Updating item data... ',' ','-')
        eimp(1)=GWP
        eimp(2)=AP
        eimp(3)=PS
        eimp(4)=UNRE
        call prsenvf(idbh,iit,'p',fablif,brkias,ntrn,trbsn,dtrbs,
     &    rtrbs,rn,rr,in,ri,dn,rd,eimp,ier)

        call edisp(iuout,' The revised data is... ')
        call edisp(iuout,itdat(1))

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
      endif

C Make up the display.
      IW=-4
      write(PELM(1),'(2A)')    '  category: ',catmenu(ic)(1:26)
      write(PELM(2),'(3A)') kact(act,'a'),' tag: ',ittag(iit)
      write(PELM(3),'(3A)') kact(act,'b'),' name: ',itmenu(iit)(1:30)
      write(PELM(4),'(2A)')    '  date: ',itdate(iit)(1:24)
      write(PELM(5),'(3a)') kact(act,'c'),' source: ',snote(1:27)
      write(PELM(6),'(3a)') kact(act,'d'),' notes: ',itdoc(1:24)
      PELM(7)=                 '  _________________________________ '
      la=MIN0(lnblnk(rnmenu),18)
      lb=MIN0(lnblnk(inmenu),18)
      lc=MIN0(lnblnk(dnmenu),18)
      write(PELM(8),'(2a,f6.1)') kact(act,'e'),' material life:',fablif
      write(PELM(9),'(2a,f6.3)') kact(act,'f'),
     &  ' breakage during asmbly:',brkias
      write(PELM(10),'(a)')
     &  '  _transport_distance_loss__name____'
      write(PELM(11),'(2a,f7.1,f6.2,2a)') kact(act,'g'),
     &  ' 1st >site:',dtrbs(1),rtrbs(1),' ',trnmen1(1:12)
      write(PELM(12),'(2a,f7.1,f6.2,2a)') kact(act,'h'),
     &  ' 2nd >site:',dtrbs(2),rtrbs(2),' ',trnmen2(1:12)
      write(PELM(13),'(2a,f7.1,f6.2,2a)') kact(act,'i'),
     &  ' 3rd >site:',dtrbs(3),rtrbs(3),' ',trnmen3(1:12)
      write(PELM(14),'(2a,F6.3,2a)') kact(act,'j'),' recycling:',
     &  rr,' ',rnmenu(1:la)
      write(PELM(15),'(2a,F6.3,2a)') kact(act,'k'),' incinerat:',
     &  ri,' ',inmenu(1:lb)
      write(PELM(16),'(2a,F6.3,2a)') kact(act,'m'),' dump     :',
     &  rd,' ',dnmenu(1:lc)
      call prsunit(idbh,18,ustr,unote)
      write(PELM(17),'(2a,1Pe10.3,1x,a)') kact(act,'n'),
     &  ' glob warmng:',GWP,ustr(1:lnblnk(ustr))
      call prsunit(idbh,19,ustr,unote)
      write(PELM(18),'(2a,1Pe10.3,1x,a)') kact(act,'o'),
     &  ' acidification:',AP,ustr(1:lnblnk(ustr))
      call prsunit(idbh,20,ustr,unote)
      write(PELM(19),'(2a,1Pe10.3,1x,a)') kact(act,'p'),
     &  ' photoSmog    :',PS,ustr(1:lnblnk(ustr))
      call prsunit(idbh,21,ustr,unote)
      write(PELM(20),'(2a,1Pe10.3,1x,a)') kact(act,'q'),
     &  ' nonren energy:',UNRE,ustr(1:lnblnk(ustr))

C After parsing units recover the header of the current db.
      IFB=igendbf(idbh)
      call scandbhd(IFB,idbh,ier)
      PELM(21)= ' ___________________________________ '
      PELM(22)= '? help                               '
      PELM(23)= '- exit                               '
      MPLEM=23

C Help text for this menu.
      helptopic='dbm_material_life'
      call gethelptext(helpinsub,helptopic,nbhelp)

   2  CALL EMENU('  Item in material effects',PELM,MPLEM,IW)

      IF(IW.EQ.0.OR.IW.EQ.MPLEM)THEN
        RETURN
      ELSEIF(IW.EQ.MPLEM-1)THEN

C Help.
        helptopic='dbm_material_life'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('material effects item edit',nbhelp,'-',0,0,IER)
      ELSEIF(IW.EQ.2.or.IW.eq.3.or.IW.eq.5.or.IW.eq.6)THEN
        iract=0
        call EITCOMMON(IFB,idbh,ic,iit,IW,ACT,iract,primod,IER)
        if(iract.eq.3)goto 3
        if(iract.eq.9)goto 9
      ELSEIF(IW.EQ.8)THEN
        CALL EASKR(fablif,' ',' Material life (years):',
     &    0.0,'W',1000.,'W',1.,'fablif',IER,nbhelp)
        primod=.true.
      ELSEIF(IW.EQ.9)THEN
        CALL EASKR(brkias,' ',' Breakage (fraction) during assembly:',
     &    0.0,'W',1.,'W',1.,'brkias',IER,nbhelp)
        primod=.true.
      ELSEIF(IW.EQ.11.or.IW.EQ.12.or.IW.EQ.13)THEN

C Provide browse or popup for 1st/2nd/3rdtransport attributes.
        if(gendbok(9))then
          if(IW.eq.11)trntag=trbsn(1)
          if(IW.eq.12)trntag=trbsn(2)
          if(IW.eq.13)trntag=trbsn(3)
          call prsmenu(9,trntag,trnmenu,itrn,itrncat)
          IFB=igendbf(idbh)
          call scandbhd(IFB,idbh,ier)
          class=cattag(ic)
          item=ittag(iit)
          call BKITEM(idbh,class,item,ic,iit)
          call choseitem('transport',ACT,trntag,'envir_imp',selitem,
     &      selmenu,itmsel,icatsel,IER)
          IFB=igendbf(idbh)
          call scandbhd(IFB,idbh,ier)
          if(ACT(1:1).eq.'M'.and.itmsel.ne.0)then
            if(IW.eq.11)trbsn(1)=selitem
            if(IW.eq.12)trbsn(2)=selitem
            if(IW.eq.13)trbsn(3)=selitem
            primod=.true.
          endif
          call RCVITEM(idbh,class,item,ic,iit)
          if(item.ne.ittag(iit))then
            call edisp(iuout,'woops: got confused recovering item.')
          endif

C Edit 1st transport distance and breakage rate.
          if(IW.eq.11)write(hold,'(f8.2,f7.3)') dtrbs(1),rtrbs(1)
          if(IW.eq.12)write(hold,'(f8.2,f7.3)') dtrbs(2),rtrbs(2)
          if(IW.eq.13)write(hold,'(f8.2,f7.3)') dtrbs(3),rtrbs(3)
          CALL EASKS(hold,'Transport distance (km) and breakage rate:',
     &      ' ',32,'1.00 0.00','trn1 dist and brk',IER,nbhelp)
          if(ier.eq.0.and.hold(1:2).ne.'  ')then
            k=0
            if(IW.eq.11)then
             CALL EGETWR(hold,K,dtrbs(1),0.00,0.0,'-','dtrbs',IER)
             CALL EGETWR(hold,K,rtrbs(1),0.00,1.0,'W','rtrbs',IER)
            elseif(IW.eq.12)then
             CALL EGETWR(hold,K,dtrbs(2),0.00,0.0,'-','dtrbs',IER)
             CALL EGETWR(hold,K,rtrbs(2),0.00,1.0,'W','rtrbs',IER)
            elseif(IW.eq.13)then
             CALL EGETWR(hold,K,dtrbs(3),0.00,0.0,'-','dtrbs',IER)
             CALL EGETWR(hold,K,rtrbs(3),0.00,1.0,'W','rtrbs',IER)
            endif
          endif
          primod=.true.
        else
          call usrmsg('Sorry, transport db not available.',' ','W')
        endif
      ELSEIF(IW.EQ.14)THEN

C Provide browse popup for environmental db (recycling).
        CALL EASKR(rr,' ',' Recycling rate:',
     &    0.0,'W',1.,'W',1.,'rr',IER,nbhelp)
        if(gendbok(8))then
          class=cattag(ic)
          item=ittag(iit)
          call BKITEM(idbh,class,item,ic,iit)
          call choseitct('envir_imp',ACT,rn,'envir_imp','RECYCP  ',
     &      selitem,selmenu,itmsel,icatsel,IER)
          if(ACT(1:1).eq.'M'.and.itmsel.ne.0)then
            rn=selitem
            rnmenu=selmenu
            primod=.true.
          endif
          call RCVITEM(idbh,class,item,ic,iit)
          if(item.ne.ittag(iit))then
            call edisp(iuout,'woops: got confused recovering item.')
          endif
        else
          call usrmsg('Sorry, environ impacts not available.',' ','P')
        endif
      ELSEIF(IW.EQ.15)THEN

C Provide browse popup for environmental db (incineration).
        CALL EASKR(ri,' ',' Incineration rate:',
     &    0.0,'W',1.,'W',1.,'ri',IER,nbhelp)
        if(gendbok(8))then
          class=cattag(ic)
          item=ittag(iit)
          call BKITEM(idbh,class,item,ic,iit)
          call choseitct('envir_imp',ACT,in,'envir_imp','INCENP  ',
     &      selitem,selmenu,itmsel,icatsel,IER)
          if(ACT(1:1).eq.'M'.and.itmsel.ne.0)then
            in=selitem
            inmenu=selmenu
            primod=.true.
          endif
          call RCVITEM(idbh,class,item,ic,iit)
          if(item.ne.ittag(iit))then
            call edisp(iuout,'woops: got confused recovering item.')
          endif
        else
          call usrmsg('Sorry, environ impacts not available.',' ','P')
        endif
      ELSEIF(IW.EQ.16)THEN

C Provide browse popup for environmental db (dump).
        CALL EASKR(rd,' ',' Dump rate:',
     &    0.0,'W',1.,'W',1.,'rd',IER,nbhelp)
        if(gendbok(8))then
          class=cattag(ic)
          item=ittag(iit)
          call BKITEM(idbh,class,item,ic,iit)
          call choseitct('envir_imp',ACT,dn,'envir_imp','DUMPP   ',
     &      selitem,selmenu,itmsel,icatsel,IER)
          if(ACT(1:1).eq.'M'.and.itmsel.ne.0)then
            dn=selitem
            dnmenu=selmenu
            primod=.true.
          endif
          call RCVITEM(idbh,class,item,ic,iit)
          if(item.ne.ittag(iit))then
            call edisp(iuout,'woops: got confused recovering item.')
          endif
        else
          call usrmsg('Sorry, environ impacts not available.',' ','P')
        endif
      ELSEIF(IW.eq.17.or.IW.eq.18.or.IW.eq.19.or.IW.eq.20)THEN

C Edit the impacts.
        if(ACT(1:1).eq.'M')then
          call edimpact(GWP,AP,PS,UNRE,ier)
          primod=.true.
        else
          call usrmsg('Currently in browse mode.',' ','P')
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
      
C ************* EDENVP
C Environmental effects for site assembly, site maintenance and
C prefabrication assembly display and editing. If ACT='T'
C then produce a tabular listing and return, otherwise present editing menu.
C Assumes that current database is open on unit IFB.
      subroutine edenvp(IFB,idbh,ic,iit,ACT,IER)
#include "espriou.h"
#include "esprdbfile.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN

C Generic db commons:
      common/gendb2/dbtag(MGDB),dbmenu(MGDB),dbdate(MGDB),dbnote(MGDB),
     &              dbdoc(10)
      common/gendb3/irscat(MGCL),nbcatdoc(MGCL),nbcatit(MGCL)
      common/gendb4/cattag(MGCL),cattype(MGCL),catmenu(MGCL),
     &              catdate(MGCL),catdoc(5)

      common/gendb6/ittag(MGIT),itmenu(MGIT)
      common/gendb8/irsit(MGIT),itcat(MGIT)
      common/gendb8a/ituntstr,itsrcstr,itcnfstr,itdate(MGIT),itdoc
      common/gendb8b/itdat(20),itrep(20)
      common/gendbtg/showtags

      dimension PELM(16),eimp(9)

      CHARACTER OUTSTR*124,PELM*39,ACT*1,dstmp*24
      character dbtag*12,dbmenu*32,dbdate*24,dbdoc*248,catdoc*248
      character cattag*12,cattype*8,catmenu*32,catdate*24
      character ittag*12,itmenu*32,itdate*24,itdoc*248
      character dbnote*72,outs*124,ituntstr*42,itsrcstr*1,itcnfstr*42
      character ustr*12,unote*724,ct*8,tag*1
      character itdat*248,itrep*248,kact*1
      character snote*248
      logical primod,showtags
      integer MPLEM,IW  ! max items and current menu item

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='edenvp'  ! set for subroutine

C Scan the data from the common blocks.
    9 primod=.false.
      write(currentfile,'(a)')  gendbfn(idbh)(1:lnblnk(gendbfn(idbh)))
      IFB=igendbf(idbh)
      call scandbhd(IFB,idbh,ier)
      call rgenitem(IFB,idbh,iit,ier)

C Depending on the category call different encoding/decoding.
C Check that the item is in an allowable category.
      ict=itcat(iit)
      ct=cattype(ict)
      if(ct(1:6).eq.'ASMPRJ'.or.ct(1:8).eq.'CNSTRMNT'.or.
     &   ct(1:8).eq.'CNSTRASM')then
        continue
      else
        call usrmsg('Cannot edit items of this type.',' ','W')
        ier= -1
        return
      endif
      call prsenvp(idbh,iit,'g',tag,brkasp,eimp,ier)
      call prssrc(idbh,snote)
      GWP=eimp(1)
      AP=eimp(2)
      PS=eimp(3)
      UNRE=eimp(4)

C If user has request a report only display the relevant table.
      if(ACT(1:1).eq.'T')then
        call edisp(iuout,'Should have called epopenv...')
        return
      endif

C If anything has changed it needs to be written out. First
C remove spaces in data lines(s). Recover category data.
   3  if(primod.and.ACT(1:1).eq.'M')then
        call usrmsg('Updating item data... ',' ','-')
        eimp(1)=GWP
        eimp(2)=AP
        eimp(3)=PS
        eimp(4)=UNRE
        call prsenvp(idbh,iit,'p',tag,brkasp,eimp,ier)

        call edisp(iuout,' The revised data is... ')
        call edisp(iuout,itdat(1))

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
      endif

C Make up the display.
      IW=-4
      write(PELM(1),'(2A)')    '  category: ',catmenu(ic)(1:26)
      write(PELM(2),'(3A)') kact(act,'a'),' tag: ',ittag(iit)
      write(PELM(3),'(3A)') kact(act,'b'),' name: ',itmenu(iit)(1:30)
      write(PELM(4),'(2A)')    '  date: ',itdate(iit)(1:24)
      write(PELM(5),'(3a)') kact(act,'c'),' source: ',snote(1:27)
      write(PELM(6),'(3a)') kact(act,'d'),' notes: ',itdoc(1:24)
      PELM(7)=                 '  ________________________________ '
      call prsunit(idbh,2,ustr,unote)
      if(ct(1:8).eq.'CNSTRMNT')then
        write(PELM(8),'(2a,f6.1)') kact(act,'e'),
     &    ' maintenence frequency:',brkasp
      else
        write(PELM(8),'(2a,f6.1)') kact(act,'e'),
     &    ' breakage rate       :',brkasp
      endif
      call prsunit(idbh,3,ustr,unote)
      write(PELM(9),'(2a,1Pe10.3,1x,a)') kact(act,'f'),
     &  ' global warmng:',GWP,ustr(1:lnblnk(ustr))
      call prsunit(idbh,4,ustr,unote)
      write(PELM(10),'(2a,1Pe10.3,1x,a)') kact(act,'g'),
     &  ' acidification:',AP,ustr(1:lnblnk(ustr))
      call prsunit(idbh,5,ustr,unote)
      write(PELM(11),'(2a,1Pe10.3,1x,a)') kact(act,'h'),
     &  ' photoSmog    :',PS,ustr(1:lnblnk(ustr))
      call prsunit(idbh,6,ustr,unote)
      write(PELM(12),'(2a,1Pe10.3,1x,a)') kact(act,'i'),
     &  ' nonren energy:',UNRE,ustr(1:lnblnk(ustr))

C After parsing units recover the header of the current db.
      IFB=igendbf(idbh)
      call scandbhd(IFB,idbh,ier)
      PELM(13)=                ' ________________________________ '
      PELM(14)=                '? help                            '
      PELM(15)=                '- exit                            '
      MPLEM=15

C Help text for this menu.
   2  if(ct(1:6).eq.'ASMPRJ')then
        helptopic='dbm_site_assembly'
        call gethelptext(helpinsub,helptopic,nbhelp)
      elseif(ct(1:8).eq.'CNSTRMNT')then
        helptopic='dbm_site_maintenance'
        call gethelptext(helpinsub,helptopic,nbhelp)
      elseif(ct(1:8).eq.'CNSTRASM')then
        helptopic='dbm_prefab_assembly'
        call gethelptext(helpinsub,helptopic,nbhelp)
      endif

C Depending on the category use different heading.
      if(ct(1:6).eq.'ASMPRJ')then
        CALL EMENU(' Item in site assembly',PELM,MPLEM,IW)
      elseif(ct(1:8).eq.'CNSTRMNT')then
        CALL EMENU(' Item in site maintenance',PELM,MPLEM,IW)
      elseif(ct(1:8).eq.'CNSTRASM')then
        CALL EMENU(' Item in prefabrication assembly',PELM,MPLEM,IW)
      endif
      IF(IW.EQ.0.OR.IW.EQ.MPLEM)THEN
        RETURN
      ELSEIF(IW.EQ.MPLEM-1)THEN

C Help.
        if(ct(1:6).eq.'ASMPRJ')then
          CALL PHELPD('assembly on prj item edit help',nbhelp,'-',
     &      0,0,IER)
        elseif(ct(1:8).eq.'CNSTRMNT')then
          CALL PHELPD('constr maint item edit help',nbhelp,'-',
     &      0,0,IER)
        elseif(ct(1:8).eq.'CNSTRASM')then
          CALL PHELPD('constr assembly item edit help',nbhelp,'-',
     &      0,0,IER)
        endif
      ELSEIF(IW.EQ.2.or.IW.eq.3.or.IW.eq.5.or.IW.eq.6)THEN
        iract=0
        call EITCOMMON(IFB,idbh,ic,iit,IW,ACT,iract,primod,IER)
        if(iract.eq.3)goto 3
        if(iract.eq.9)goto 9
      ELSEIF(IW.EQ.8)THEN
        CALL EASKR(brkias,' ',' Breakage rate: (0.0-1.0)',
     &    0.0,'W',1.,'W',1.,'brkasp',IER,nbhelp)
        primod=.true.
      ELSEIF(IW.eq.9.or.IW.eq.10.or.IW.eq.11.or.IW.eq.12)THEN

C Edit the impacts.
        if(ACT(1:1).eq.'M')then
          call edimpact(GWP,AP,PS,UNRE,ier)
          primod=.true.
        else
          call usrmsg('Currently in browse mode.',' ','P')
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


C ************* EPOPENV
C Environmental impact item popup display. 
C Assumes that current database is open on unit IFB.
C << this might need to rescan header of IFB >>
      subroutine epopenv(IFB,idbh,ic,iit,ACT,IER)
#include "esprdbfile.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN

C Generic db commons:
      common/gendb2/dbtag(MGDB),dbmenu(MGDB),dbdate(MGDB),dbnote(MGDB),
     &              dbdoc(10)
      common/gendbs/srcchr(24),srcnote(24)
      common/gendb3/irscat(MGCL),nbcatdoc(MGCL),nbcatit(MGCL)
      common/gendb4/cattag(MGCL),cattype(MGCL),catmenu(MGCL),
     &              catdate(MGCL),catdoc(5)
      common/gendb6/ittag(MGIT),itmenu(MGIT)
      common/gendb8/irsit(MGIT),itcat(MGIT)
      common/gendb8a/ituntstr,itsrcstr,itcnfstr,itdate(MGIT),itdoc
      common/gendbtg/showtags

      dimension trbsn(3),dtrbs(3),rtrbs(3),assite(2),lasmat(2)
      dimension eimp(9)
      CHARACTER dbtag*12,dbmenu*32,dbdate*24,dbdoc*248,catdoc*248
      character cattag*12,cattype*8,catmenu*32,catdate*24,ACT*1
      character ittag*12,itmenu*32,itdate*24,itdoc*248
      character dbnote*72,ituntstr*42,itsrcstr*1,itcnfstr*42
      character ustr*12,unote*72,srcchr*4,srcnote*248
      character assite*12,lassurn*12,laspern*12,trbsn*12,lasmat*12
      character rn*12,in*12,dn*12,tag*1,ct*8
      character trnmen1*32,trnmen2*32,trnmen3*32
      character rnmenu*32,inmenu*32,dnmenu*32
      character lassurm*32,lasperm*32,asitm1*32,asitm2*32,lasm1*32
      character lasm2*32,snote*248,outs*124,outs248*248
      logical showtags

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='epopenv'  ! set for subroutine

C Scan the data from the common blocks.
      call rgenitem(IFB,idbh,iit,ier)
      call prssrc(idbh,snote)

C If user has request a report only display the relevant table.
      if(ACT(1:1).eq.'T'.or.ACT(1:1).eq.'H')then
        continue
      else
        return
      endif
      la=lnblnk(ittag(iit))
      lb=lnblnk(itmenu(iit))
      ld=lnblnk(dbmenu(idbh))
      write(outs248,'(8a)') 'In ',dbmenu(idbh)(1:ld),', item ',
     &  itmenu(iit)(1:lb),' (tag ',ittag(iit)(1:la),
     &  ') edited on ',itdate(iit)
      call edisp248(iuout,outs248,70)
      call edisp(iuout,' Source & notes:')
      call edisp248(iuout,snote,70)
      call edisp248(iuout,itdoc,70)
      call edisp(iuout,'  ')

C Based on category type parse data and display.
      ict=itcat(iit)
      IREC=irscat(ict)
      read(IFB,REC=IREC,IOSTAT=IOS,ERR=1)cattag(ict),cattype(ict),
     &  catmenu(ict),nbcatdoc(ict),nbcatit(ict),catdate(ict)
      ct=cattype(ict)
      if(ct(1:6).eq.'RECYCP'.or.
     &   ct(1:6).eq.'INCENP'.or.ct(1:5).eq.'DUMPP')then
        call prsenvi(idbh,iit,'g',ntrn,trbsn,dtrbs,eimp,ier)
        call prsmenu(9,trbsn(1),trnmen1,itrn,itrncat)
        call prsmenu(9,trbsn(2),trnmen2,itrn,itrncat)
        call prsmenu(9,trbsn(3),trnmen3,itrn,itrncat)

C After parsing menus recover the header of the current db.
        IFB=igendbf(idbh)
        call scandbhd(IFB,idbh,ier)
        GWP=eimp(1)
        AP=eimp(2)
        PS=eimp(3)
        UNRE=eimp(4)
        write(outs,'(a)')
     &    '  ________________distance__loss_name_______'
        call edisp(iuout,outs)
        write(outs,'(a,f8.1,2a)')' 1st transp>site: ',dtrbs(1),
     &    ' ',trnmen1
        call edisp(iuout,outs)
        write(outs,'(a,f8.1,2a)')' 2nd transp>site: ',dtrbs(2),
     &    ' ',trnmen2
        call edisp(iuout,outs)
        write(outs,'(a,f8.1,2a)')' 3rd transp>site: ',dtrbs(3),
     &    ' ',trnmen3
        call edisp(iuout,outs)
        call prsunit(idbh,7,ustr,unote)
        write(outs,'(a,1Pe10.3,1x,a)')
     &    ' Global Warming potential:',GWP,ustr(1:lnblnk(ustr))
        call edisp(iuout,outs)
        call prsunit(idbh,8,ustr,unote)
        write(outs,'(a,1Pe10.3,1x,a)')
     &    ' Acidification potential :',AP,ustr(1:lnblnk(ustr))
        call edisp(iuout,outs)
        call prsunit(idbh,9,ustr,unote)
        write(outs,'(a,1Pe10.3,1x,a)')
     &    ' PhotoSmog depletion :',PS,ustr(1:lnblnk(ustr))
        call edisp(iuout,outs)
        call prsunit(idbh,10,ustr,unote)
        write(outs,'(a,1Pe10.3,1x,a)')
     &   ' Nonrenewable energy:',UNRE,ustr(1:lnblnk(ustr))
        call edisp(iuout,outs)

C After parsing units recover the header of the current db.
        IFB=igendbf(idbh)
        call scandbhd(IFB,idbh,ier)
        return
      elseif(ct(1:6).eq.'ASMPRJ'.or.ct(1:8).eq.'CNSTRMNT'.or.
     &   ct(1:8).eq.'CNSTRASM')then
        call prsenvp(idbh,iit,'g',tag,brkasp,eimp,ier)
        GWP=eimp(1)
        AP=eimp(2)
        PS=eimp(3)
        UNRE=eimp(4)
        call prsunit(idbh,2,ustr,unote)
        if(ct(1:8).eq.'CNSTRMNT')then
          write(outs,'(a,f6.3,1x,a)') ' Maintenence freq.:',brkasp,ustr
        else
          write(outs,'(a,f6.3,1x,a)') ' Breakage rate:',brkasp,ustr
        endif
        call edisp(iuout,outs)
        call prsunit(idbh,3,ustr,unote)
        write(outs,'(a,1Pe10.3,1x,a)')
     &    ' Global Warming potential:',GWP,ustr(1:lnblnk(ustr))
        call edisp(iuout,outs)
        call prsunit(idbh,4,ustr,unote)
        write(outs,'(a,1Pe10.3,1x,a)')
     &    ' Acidification potential :',AP,ustr(1:lnblnk(ustr))
        call edisp(iuout,outs)
        call prsunit(idbh,5,ustr,unote)
        write(outs,'(a,1Pe10.3,1x,a)')
     &    ' PhotoSmog depletion :',PS,ustr(1:lnblnk(ustr))
        call edisp(iuout,outs)
        call prsunit(idbh,6,ustr,unote)
        write(outs,'(a,1Pe10.3,1x,a)')
     &    ' Nonrenewable energy:',UNRE,ustr(1:lnblnk(ustr))
        call edisp(iuout,outs)

C After parsing units recover the header of the current db.
        IFB=igendbf(idbh)
        call scandbhd(IFB,idbh,ier)
        return
      elseif(ct(1:6).eq.'MATLCA')then
        call prsenvf(idbh,iit,'g',fablif,brkias,ntrn,trbsn,dtrbs,
     &    rtrbs,rn,rr,in,ri,dn,rd,eimp,ier)
        call prsmenu(8,rn,rnmenu,irn,irncat)
        call prsmenu(8,in,inmenu,iin,iincat)
        call prsmenu(8,dn,dnmenu,idn,idncat)
        call prsmenu(9,trbsn(1),trnmen1,itrn,itrncat)
        call prsmenu(9,trbsn(2),trnmen2,itrn,itrncat)
        call prsmenu(9,trbsn(3),trnmen3,itrn,itrncat)

C After parsing menus recover the header of the current db.
        IFB=igendbf(idbh)
        call scandbhd(IFB,idbh,ier)
        GWP=eimp(1)
        AP=eimp(2)
        PS=eimp(3)
        UNRE=eimp(4)
        call prsunit(idbh,1,ustr,unote)
        write(outs,'(a,f6.1,1x,a)') ' Material life:',fablif,
     &    ustr(1:lnblnk(ustr))
        call edisp(iuout,outs)
        call prsunit(idbh,2,ustr,unote)
        write(outs,'(a,f6.3,1x,a)') ' Breakage during asmbly:',
     &    brkias,ustr(1:lnblnk(ustr))
        call edisp(iuout,outs)
        write(outs,'(a)')
     &    '  ________________distance__loss_name_______'
        call edisp(iuout,outs)
        write(outs,'(a,f8.1,f6.3,2a)')' 1st transp>site: ',dtrbs(1),
     &    rtrbs(1),' ',trnmen1
        call edisp(iuout,outs)
        write(outs,'(a,f8.1,f6.3,2a)')' 2nd transp>site: ',dtrbs(2),
     &    rtrbs(2),' ',trnmen2
        call edisp(iuout,outs)
        write(outs,'(a,f8.1,f6.3,2a)')' 3rd transp>site: ',dtrbs(3),
     &    rtrbs(3),' ',trnmen3
        call edisp(iuout,outs)
        call prsunit(idbh,13,ustr,unote)
        if(showtags)then
         write(outs,'(5a,F6.3,1x,a)') ' Recycling: (',rn(1:lnblnk(rn)),
     &    ') ',rnmenu(1:lnblnk(rnmenu)),'  ',rr,ustr(1:lnblnk(ustr))
        else
         write(outs,'(3a,F6.3,1x,a)') ' Recycling: ',
     &    rnmenu(1:lnblnk(rnmenu)),'  ',rr,ustr(1:lnblnk(ustr))
        endif
        call edisp(iuout,outs)
        call prsunit(idbh,15,ustr,unote)
        if(showtags)then
         write(outs,'(5a,F6.3,1x,a)') ' Incinerat: (',in(1:lnblnk(in)),
     &    ') ',inmenu(1:lnblnk(inmenu)),'  ',ri,ustr(1:lnblnk(ustr))
        else
         write(outs,'(3a,F6.3,1x,a)') ' Incinerat: ',
     &    inmenu(1:lnblnk(inmenu)),'  ',ri,ustr(1:lnblnk(ustr))
        endif
        call edisp(iuout,outs)
        call prsunit(idbh,17,ustr,unote)
        write(outs,'(5a,F6.3,1x,a)') ' Dump     : (',dn(1:lnblnk(dn)),
     &    ') ',dnmenu(1:lnblnk(dnmenu)),'  ',rd,ustr(1:lnblnk(ustr))
        call edisp(iuout,outs)
        call prsunit(idbh,18,ustr,unote)
        write(outs,'(a,1Pe10.3,1x,a)') 
     &    ' Global Warming potential:',GWP,ustr(1:lnblnk(ustr))
        call edisp(iuout,outs)
        call prsunit(idbh,19,ustr,unote)
        write(outs,'(a,1Pe10.3,1x,a)') 
     &    ' Acidification potential :',AP,ustr(1:lnblnk(ustr))
        call edisp(iuout,outs)
        call prsunit(idbh,20,ustr,unote)
        write(outs,'(a,1Pe10.3,1x,a)') 
     &    ' PhotoSmog depletion :',PS,ustr(1:lnblnk(ustr))
        call edisp(iuout,outs)
        call prsunit(idbh,21,ustr,unote)
        write(outs,'(a,1Pe10.3,1x,a)') 
     &    ' Nonrenewable energy:',UNRE,ustr(1:lnblnk(ustr))
        call edisp(iuout,outs)

C After parsing units recover the header of the current db.
        IFB=igendbf(idbh)
        call scandbhd(IFB,idbh,ier)
        return
      elseif(ct(1:7).eq.'CONSIMP')then
        call prsenvci(idbh,iit,'g',lassurn,laspern,ntrn,trbsn,
     &    dtrbs,rtrbs,assite,lasmat,ier)
        call prsmenu(8,lassurn,lassurm,iassur,iasscat)
        call prsmenu(8,laspern,lasperm,iasper,iaspcat)
        call prsmenu(8,assite(1),asitm1,iasit1,iasitcat)
        call prsmenu(8,assite(2),asitm2,iasit2,iasitcat)
        call prsmenu(8,lasmat(1),lasm1,iasm1,iasm1cat)
        call prsmenu(8,lasmat(2),lasm2,iasm2,iasm2cat)
        call prsmenu(9,trbsn(1),trnmen1,itrn,itrncat)
        call prsmenu(9,trbsn(2),trnmen2,itrn,itrncat)
        call prsmenu(9,trbsn(3),trnmen3,itrn,itrncat)

C After parsing menus recover the header of the current db.
        IFB=igendbf(idbh)
        call scandbhd(IFB,idbh,ier)

        if(showtags)then
         write(outs,'(4a)') ' Prefab assembly (sur): (',
     &    lassurn(1:lnblnk(lassurn)),') ',lassurm(1:lnblnk(lassurm))
        else
         write(outs,'(2a)') ' Prefab assembly (sur): ',
     &    lassurm(1:lnblnk(lassurm))
        endif
        call edisp(iuout,outs)
        if(showtags)then
         write(outs,'(4a)') ' Prefab assembly (per): (',
     &    laspern(1:lnblnk(laspern)),') ',lasperm(1:lnblnk(lasperm))
        else
         write(outs,'(2a)') ' Prefab assembly (per): ',
     &    lasperm(1:lnblnk(lasperm))
        endif
        call edisp(iuout,outs)
        write(outs,'(a)')
     &    '  ________________distance__loss_rate_name_______'
        call edisp(iuout,outs)
        write(outs,'(a,f8.1,f6.3,2a)')' 1st transp>site: ',dtrbs(1),
     &    rtrbs(1),' ',trnmen1
        call edisp(iuout,outs)
        write(outs,'(a,f8.1,f6.3,2a)')' 2nd transp>site: ',dtrbs(2),
     &    rtrbs(2),' ',trnmen2
        call edisp(iuout,outs)
        write(outs,'(a,f8.1,f6.3,2a)')' 3rd transp>site: ',dtrbs(3),
     &    rtrbs(3),' ',trnmen3
        call edisp(iuout,outs)
        if(showtags)then
         write(outs,'(4a)')' 1st Site assembly: (',
     &    assite(1)(1:lnblnk(assite(1))),') ',asitm1(1:lnblnk(asitm1))
        else
         write(outs,'(2a)')' 1st Site assembly: ',
     &     asitm1(1:lnblnk(asitm1))
        endif
        call edisp(iuout,outs)
        if(showtags)then
         write(outs,'(4a)')' 2nd Site assembly: (',
     &    assite(2)(1:lnblnk(assite(2))),') ',asitm2(1:lnblnk(asitm2))
        else
         write(outs,'(2a)')' 2nd Site assembly: ',
     &    asitm2(1:lnblnk(asitm2))
        endif
        call edisp(iuout,outs)
        if(showtags)then
         write(outs,'(4a)') ' 1st maintenance: (',
     &   lasmat(1)(1:lnblnk(lasmat(1))),') ',lasm1(1:lnblnk(lasm1))
        else
         write(outs,'(2a)') ' 1st maintenance: ',
     &   lasm1(1:lnblnk(lasm1))
        endif
        call edisp(iuout,outs)
        if(showtags)then
         write(outs,'(4a)') ' 2nd maintenance: (',
     &   lasmat(2)(1:lnblnk(lasmat(2))),') ',lasm2(1:lnblnk(lasm2))
        else
         write(outs,'(2a)') ' 2nd maintenance: ',
     &   lasm2(1:lnblnk(lasm2))
        endif
        call edisp(iuout,outs)
        return
      endif

      return

C Error messages.
    1 call edisp(iuout,'EPOPENV: Error updating data record')
      return
      END
