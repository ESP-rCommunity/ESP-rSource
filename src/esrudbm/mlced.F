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

C In this file:
C EDMLC Construction item display and editing.

C ************* EDMLC
C EDMLC Construction item display and editing.  If ACT='T' then produce
C a tabular listing and return, if ACT='H' present tabular listing
C as a pop-up, otherwise present editing menu.
C Assumes that current database is open on unit IFB.
      subroutine edmlc(IFB,idbh,ic,iit,ACT,IER)
#include "building.h"
#include "esprdbfile.h"
C providing paramaters MGDB,MGCL,MGIT
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL
      common/pophelp/h(60)

C Generic db commons:
      common/gendb1/idbwidth(MGDB),lastr(MGDB),nbdbdoc(MGDB),
     &              nbunit(MGDB),nbsrc(MGDB),nbcat(MGDB),nbitem(MGDB),
     &              idbver(MGDB)      
      common/gendbu/unitchr(24),unitstr(24),unitnote(24)
      common/gendb2/dbtag(MGDB),dbmenu(MGDB),dbdate(MGDB),dbnote(MGDB),
     &              dbdoc(10)
      common/gendb3/irscat(MGCL),nbcatdoc(MGCL),nbcatit(MGCL)
      common/gendb4/cattag(MGCL),cattype(MGCL),catmenu(MGCL),
     &              catdate(MGCL),catdoc(5)

      common/gendb6/ittag(MGIT),itmenu(MGIT)
      common/gendb8/irsit(MGIT),itcat(MGIT)
      common/gendb8a/ituntstr,itsrcstr,itcnfstr,itdate(MGIT),itdoc
      common/gendb8c/nbitusc(MGIT),nbitdat(MGIT),
     &               nbitrep(MGIT),ibitusc(MGIT)

C Backup of documentation (for editing).
      common/tmpdoca/bkdoctyp,bkdoc(60),bk4(24),bk12(24),bk72(24),
     &  bk248(24)
      common/tmpdocb/bkdocok,nbkdoc,indexbk

      dimension PELM(30)
      dimension thrmtg(ME),gaptg(ME),mattag(ME),DT(ME)
      dimension IP(ME),DRA(ME,3),laydesc(ME)

      CHARACTER OUTSTR*124,PELM*38,h*72,ACT*1,key*1,dstmp*24
      character dbtag*12,db*12,dbmenu*32,dbdate*24,dbdoc*248,catdoc*248
      character cattag*12,cattype*8,catmenu*32,catdate*24,ctype*8
      character ittag*12,itmenu*32,itdate*24,itdoc*248
      character dbnote*72,outs*124,ituntstr*42,itsrcstr*1,itcnfstr*42
      character acoutag*12,colormtag*12,lcatag*12
      character symname*12,othcolr*12,incolr*12,othacou*12,inacou*12
      character OTF*4,OPT*12,cnstimp*12,lcatype*1
      character thrmtg*1,gaptg*1,mattag*12,laydesc*32
      character class*12,item*12,mmatmenu*32
      character mmattag*12
      character bkdoctyp*2,bkdoc*72,bk4*4,bk12*12,bk72*72,bk248*248
      character snote*248
      character kact*1,head*38
      character oacumenu*32,iacumenu*32,ocmenu*32,icmenu*32
      character selitem*12,selmenu*32,cnstm*32
      character unitchr*4,unitstr*12,unitnote*72
      logical primod,ok,dok,bkdocok,modit,dmodit
      real misc1,misc2

C Scan the data from the common blocks.
    9 primod=.false.
      IAFIL=IFIL+1
      call rgenitem(IFB,idbh,iit,ier)

      call PRSMLC(idbh,iit,'g',symname,othcolr,incolr,OTF,OPT,
     &  othacou,inacou,othir,sinir,othsol,sinsol,cnstimp,
     &  nlay,thrmtg,gaptg,mattag,DT,IP,DRA,laydesc,ier)
      call prssrc(idbh,snote)
      call prsmenu(5,othacou,oacumenu,iacou,iacoucat)
      call prsmenu(5,inacou,iacumenu,iacou,iacoucat)
      call prsmenu(6,othcolr,ocmenu,icolr,icolrcat)
      call prsmenu(6,incolr,icmenu,icolr,icolrcat)
      call prsmenu(8,cnstimp,cnstm,icnstimp,icnstcat)

C After parsing menus recover the header of the current db.
      IFB=igendbf(idbh)
      call scandbhd(IFB,idbh,ier)

C If user has request a report only the call should have been epopmlc.
      if(ACT(1:1).eq.'T')then
        db='multicon_db'
        call edmat2(db,IFB,idbh,ic,iit,ACT,ier)
        return
      elseif(ACT(1:1).eq.'H')then
        db='multicon_db'
        call edmat2(db,IFB,idbh,ic,iit,ACT,ier)
        return
      endif

   3  if(primod.and.ACT(1:1).eq.'M')then

C If anything has changed it needs to be written out. Recover category
C data first.
        call prsmlc(idbh,iit,'p',symname,othcolr,incolr,OTF,OPT,
     &   othacou,inacou,othir,sinir,othsol,sinsol,cnstimp,
     &   nlay,thrmtg,gaptg,mattag,DT,IP,DRA,laydesc,ier)
C debug...
C        call lstcat(6,ic,ier)

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
      thick=0.0
      do 142 il=1,nlay
        thick=thick+DT(il)
 142  continue

C Make up the display.
      IW=-4
      write(PELM(1),'(2A)')    '  category: ',catmenu(ic)(1:26)
      write(PELM(2),'(3A)') kact(act,'a'),' tag : ',ittag(iit)
      write(PELM(3),'(3A)') kact(act,'b'),' name: ',itmenu(iit)(1:30)
      write(PELM(4),'(2A)')    '  date: ',itdate(iit)(1:24)

C Show item source.
      write(PELM(5),'(3a)') kact(act,'c'),' source: ',snote(1:27)

C Include nb of lines of documentation.
      write(PELM(6),'(3a)') kact(act,'d'),' notes: ',itdoc(1:24)
      PELM(7)=                    ' __________________________________'

      la=MIN0(lnblnk(ocmenu),23)
      lb=MIN0(lnblnk(icmenu),23)
      lc=MIN0(lnblnk(oacumenu),23)
      ld=MIN0(lnblnk(iacumenu),23)
      write(PELM(8),'(5a)') kact(act,'h'),' type: ',OTF(1:lnblnk(OTF)),
     &  '  optics: ',OPT(1:lnblnk(OPT))
      write(PELM(9) ,'(3a)') kact(act,'e'),' non-symetric : ',
     &  symname(1:lnblnk(symname))
      write(PELM(10),'(4a)')  kact(act,'f'),' colour: out(',
     &  ocmenu(1:la),')'
      write(PELM(11),'(4a)') kact(act,'g'),' colour: in(',
     &  icmenu(1:lb),')'
      write(PELM(12),'(4a)') kact(act,'i'),' acoust: out(',
     &  oacumenu(1:lc),')'
      write(PELM(13),'(4a)') kact(act,'j'),' acoust: in(',
     &  iacumenu(1:ld),')'
      if(cnstimp(1:4).eq.'none')then
        write(PELM(14),'(2a)') kact(act,'k'),
     &    ' environ impact: none defined'
      else
        lh=MIN0(lnblnk(cnstm),22)
        write(PELM(14),'(3a)') kact(act,'k'),' envir impact: ',
     &    cnstm(1:lh)
      endif
      write(PELM(15),'(a,f6.3,a,i2,a)')'  __total thick(m)',
     &  thick,' in',nlay,' layers__'
      PELM(16)=  '    type       thick description'
      do 43 il=1,nlay
        call prsmenu(2,mattag(il),mmatmenu,imat,imatcat)
        IFB=igendbf(idbh)
        call scandbhd(IFB,idbh,ier)
        ml=MIN0(lnblnk(mmatmenu),16)
        CALL EMKEY(11+il,KEY,IER)
        if(ACT(1:1).eq.'M')then
          if(thrmtg(il).eq.'t'.and.gaptg(il).eq.'s')then
            write(PELM(16+il),'(a,i2,a,f5.3,2a)') key,il,
     &      ' thrm solid ',DT(il),' ',mmatmenu(1:ml)
          elseif(thrmtg(il).eq.'t'.and.gaptg(il).eq.'g')then
            write(PELM(16+il),'(a,i2,a,f5.3,2a)') key,il,
     &      ' thrmal gap ',DT(il),' ',mmatmenu(1:ml)
          elseif(thrmtg(il).eq.'n'.and.gaptg(il).eq.'f')then
            write(PELM(16+il),'(a,i2,a,f5.3,2a)') key,il,
     &      ' LCIA frame ',DT(il),' ',mmatmenu(1:ml)
          elseif(thrmtg(il).eq.'n'.and.gaptg(il).eq.'c')then
            write(PELM(16+il),'(a,i2,a,f5.3,2a)') key,il,
     &      ' LCIA spacr ',DT(il),' ',mmatmenu(1:ml)
          elseif(thrmtg(il).eq.'n'.and.gaptg(il).eq.'s')then
            write(PELM(16+il),'(a,i2,a,f5.3,2a)') key,il,
     &      ' LCIA fixng ',DT(il),' ',mmatmenu(1:ml)
          endif
        else
          if(thrmtg(il).eq.'t'.and.gaptg(il).eq.'s')then
            write(PELM(16+il),'(i3,a,f5.3,2a)') il,
     &      ' thrm solid ',DT(il),' ',mmatmenu(1:ml)
          elseif(thrmtg(il).eq.'t'.and.gaptg(il).eq.'g')then
            write(PELM(16+il),'(i3,a,f5.3,2a)') il,
     &      ' thrmal gap ',DT(il),' ',mmatmenu(1:ml)
          elseif(thrmtg(il).eq.'n'.and.gaptg(il).eq.'f')then
            write(PELM(16+il),'(i3,a,f5.3,2a)') il,
     &      ' LCIA frame ',DT(il),' ',mmatmenu(1:ml)
          elseif(thrmtg(il).eq.'n'.and.gaptg(il).eq.'c')then
            write(PELM(16+il),'(i3,a,f5.3,2a)') il,
     &      ' LCIA spacr ',DT(il),' ',mmatmenu(1:ml)
          elseif(thrmtg(il).eq.'n'.and.gaptg(il).eq.'s')then
            write(PELM(16+il),'(i3,a,f5.3,2a)') il,
     &      ' LCIA fixng ',DT(il),' ',mmatmenu(1:ml)
          endif
        endif
  43  continue

C After parsing menus recover the header of the current db.
      IFB=igendbf(idbh)
      call scandbhd(IFB,idbh,ier)
      PELM(17+nlay)= ' __________________________________ '
      PELM(18+nlay)= '* add/delete layer                  '
      PELM(19+nlay)= '? Help                              '
      PELM(20+nlay)= '- Exit                              '
      MPLEM=20+nlay
      call usrmsg(' ',' ','-')
      write(head,'(2a)') '  Item in ',dbmenu(idbh)(1:27)

C Help text for this menu.
   2  h(1)='Editing of constructions database entities...'
      h(2)=' ...'
      h(3)=' ...'

      CALL EMENU(head,PELM,MPLEM,IW)

      IF(IW.EQ.0.OR.IW.EQ.MPLEM)THEN
        RETURN
      ELSEIF(IW.EQ.MPLEM-1)THEN

C Help.
        CALL PHELPD('constructions item edit help',3,'-',0,0,IER)
      ELSEIF(IW.EQ.2.or.IW.eq.3.or.IW.eq.5.or.IW.eq.6)THEN
        iract=0
        call EITCOMMON(IFB,idbh,ic,iit,IW,ACT,iract,primod,IER)
        if(iract.eq.3)goto 3
        if(iract.eq.9)goto 9
      ELSEIF(IW.EQ.MPLEM-2.and.ACT(1:1).eq.'M')THEN

C Add or delete a layer.
        h(1)='You can delete an existing layer, append a new layer '
        h(2)='at the `inside face` of the surface (remember the '
        h(3)='order is other-side to zone-side), or insert a layer'
        h(4)='(you will be asked to select a point of insertion and'
        h(5)='existing inner layers will be shifted).'
        CALL EASKABCD(' Layer options: ',' ',' Delete ',
     &    ' Append @ inside face',' Insert ','contine',IWA,5)
        if(IWA.eq.4)goto 3
        if(IWA.EQ.1)then

C If no of layers is edited then ask the user which one to remove.
C Now loop through the layers and copy the contents of layers >
C IWHICH into the next lower one.
          CALL EMENU('Layer to remove',PELM,MPLEM,IWR)
          IWHICH=IWR-15
          write(6,*) ' request to remove layer ',iwhich
          write(outs,'(A,A)')' Remove ',laydesc(IWHICH)(1:18)
          dok=.false.
          h(1)='Pause in case you made a mistake... '
          CALL ASKOK(outs,' Are you sure? ',OK,dok,1)
          IF(.NOT.OK)GOTO 2
          DO 791 IVV=IWHICH,nlay-1
            thrmtg(IVV)=thrmtg(IVV+1)
            gaptg(IVV)=gaptg(IVV+1)
            mattag(IVV)=mattag(IVV+1)
            laydesc(IVV)=laydesc(IVV+1)
            IP(IVV)=IP(IVV+1)
            DT(IVV)=DT(IVV+1)
            DRA(IVV,1)=DRA(IVV+1,1)
            DRA(IVV,2)=DRA(IVV+1,2)
            DRA(IVV,3)=DRA(IVV+1,3)
  791     CONTINUE
          nlay=nlay-1
          primod=.true.
        elseif(IWA.EQ.2.or.IWA.eq.3)THEN
          if(nlay.lt.ME)then
            if(IWA.EQ.3)THEN
              CALL EMENU('Point of insertion',PELM,MPLEM,IWR)
              IWHICH=IWR-15
              ILN=IWHICH
              nlay=nlay+1
              IVV=nlay+1
  792         CONTINUE
              IVV=IVV-1
              thrmtg(IVV)=thrmtg(IVV-1)
              gaptg(IVV)=gaptg(IVV-1)
              mattag(IVV)=mattag(IVV-1)
              laydesc(IVV)=laydesc(IVV-1)
              IP(IVV)=IP(IVV-1)
              DT(IVV)=DT(IVV-1)
              DRA(IVV,1)=DRA(IVV-1,1)
              DRA(IVV,2)=DRA(IVV-1,2)
              DRA(IVV,3)=DRA(IVV-1,3)
              IF(IVV.GT.IWHICH+1) GOTO 792
              il=IWHICH
            else
              nlay=nlay+1
              il=nlay
            endif

C Set initial data for layer.
            mattag(il)='none'
            thrmtg(il)='t'
            gaptg(il)='s'
            DT(il)=0.025
            IP(il)=0
            laydesc(il)='none'
            primod=.true.
          endif
        endif

C Need to cast this into the common blocks and hold this new information
C while reconfiguring the database.
        call PRSMLC(idbh,iit,'p',symname,othcolr,incolr,OTF,OPT,
     &   othacou,inacou,othir,sinir,othsol,sinsol,cnstimp,
     &   nlay,thrmtg,gaptg,mattag,DT,IP,DRA,laydesc,ier)
        class=cattag(ic)
        item=ittag(iit)
        call BKITEM(idbh,class,item,ic,iit)
        bkdocok=.TRUE.
        bkdoctyp='IL'
        indexbk=iit
        nbkdoc=nlay
        call usrmsg('Updating database for layer change...',' ','-')
        call dstamp(dstmp)
        write(dbdate(idbh),'(a)') dstmp
        write(itdate(iit),'(a)') dstmp
        IREC=2
        WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)dbdate(idbh),dbnote(idbh)
        IREC=irsit(iit)
        if(idbver(idbh).eq.1)then
          nbitdoc=1
          WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)ittag(iit),itmenu(iit),
     &      itdate(iit),itcat(iit),nbitdoc,nbitdat(iit),nlay,
     &      nbitusc(iit),ibitusc(iit)
        else
          WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)ittag(iit),itmenu(iit),
     &      itdate(iit),itcat(iit),nbitdat(iit),nlay,
     &      nbitusc(iit),ibitusc(iit)
        endif
C Reconfigure the database by exporting to ascii file and then re-reading.
        call tagend(IFB,idbh,ier)
        call recongdb(IAFIL,IFB,gendbfn(1),idbh,IER)
        if(ier.ne.0)then
          call edisp(iuout,'Error reconfiguring db.')
        endif

C Now recover the common block data and extract local variables.
C Use rgenitem because db will have correct number of layers.
        call rgenitem(IFB,idbh,iit,ier)

        call prsmlc(idbh,iit,'g',symname,othcolr,incolr,OTF,OPT,
     &   othacou,inacou,othir,sinir,othsol,sinsol,cnstimp,
     &   nlay,thrmtg,gaptg,mattag,DT,IP,DRA,laydesc,ier)
      call usrmsg('Updating database for layer change...done.',' ','-')
        bkdocok=.FALSE.

C Proceed to edit the layer data (if one added/inserted).
        if(IWA.EQ.2.or.IWA.eq.3)then
          goto 47
        else
          goto 3
        endif
      ELSEIF(IW.EQ.8)THEN
C << type and optics >>
      ELSEIF(IW.EQ.9)THEN

C Associated mlc if non-symetrical << >>

      ELSEIF(IW.EQ.10)THEN

C Provide browse or popup for outside face colour attribute.
        if(gendbok(6))then
          class=cattag(ic)
          item=ittag(iit)
          call BKITEM(idbh,class,item,ic,iit)
          call edisp(iuout,'For outer face colour...')
          call choseitem('colourimetry',ACT,othcolr,'multicon_db',
     &      selitem,selmenu,itmsel,icatsel,IER)
          IFB=igendbf(idbh)
          call scandbhd(IFB,idbh,ier)
          if(ACT(1:1).eq.'M'.and.itmsel.ne.0)then
            othcolr=selitem
            ocmenu=selmenu
            primod=.true.
          endif

C Recover the items common blocks and re-establish the category
C common blocks.
          call RCVITEM(idbh,class,item,ic,iit)
          if(item.ne.ittag(iit))then
            call edisp(iuout,'woops: got confused recovering item.')
          endif
        else
          call usrmsg('Sorry, colour db not available.',' ','P')
        endif
      ELSEIF(IW.EQ.11)THEN

C Provide browse or popup for inside face colour attribute.
        if(gendbok(6))then
          class=cattag(ic)
          item=ittag(iit)
          call BKITEM(idbh,class,item,ic,iit)
          call edisp(iuout,'For inside face colour...')
          call choseitem('colourimetry',ACT,incolr,'multicon_db',
     &      selitem,selmenu,itmsel,icatsel,IER)
          IFB=igendbf(idbh)
          call scandbhd(IFB,idbh,ier)
          if(ACT(1:1).eq.'M'.and.itmsel.ne.0)then
            incolr=selitem
            icmenu=selmenu
            primod=.true.
          endif

C Recover the items common blocks and re-establish the category
C common blocks (nbcatdoc volitile during SELCAT << ? >>)
          call RCVITEM(idbh,class,item,ic,iit)
          if(item.ne.ittag(iit))then
            call edisp(iuout,'woops: got confused recovering item.')
          endif
        else
          call usrmsg('Sorry, colour db not available.',' ','P')
        endif
      ELSEIF(IW.EQ.12)THEN

C Provide browse or popup for outside face acoustic attribute. Advise
C user of current data and then backup.
        if(gendbok(5))then
          class=cattag(ic)
          item=ittag(iit)
          write(outs,'(8a)')'The construction acoustic attributes for ',
     &      item(1:lnblnk(item)),' in categ ',class(1:lnblnk(class)),
     &      ' are ',othacou(1:lnblnk(othacou)),' & ',
     &      inacou(1:lnblnk(inacou))
          call edisp(iuout,outs)
          call BKITEM(idbh,class,item,ic,iit)

C Access the acoustic db and allow selection or pop-updisplay.
          call edisp(iuout,
     &       ' Select an outside material acoustic attribute...')
          call choseitem('acoustic',ACT,othacou,'multicon_db',
     &      selitem,selmenu,itmsel,icatsel,IER)
          IFB=igendbf(idbh)
          call scandbhd(IFB,idbh,ier)
          if(ACT(1:1).eq.'M'.and.itmsel.ne.0)then
            othacou=selitem
            oacumenu=selmenu
            primod=.true.
          endif
          call RCVITEM(idbh,class,item,ic,iit)
          if(item.ne.ittag(iit))then
            call edisp(iuout,'woops: got confused recovering item.')
          endif
        else
          call usrmsg('Sorry, acoustic db not available.',' ','P')
        endif
      ELSEIF(IW.EQ.13)THEN

C Provide browse or popup for outside face acoustic attribute. Advise
C user of current data and then backup.
        if(gendbok(5))then
          class=cattag(ic)
          item=ittag(iit)
          write(outs,'(8a)')'The construction acoustic attributes for ',
     &      item(1:lnblnk(item)),' in categ ',class(1:lnblnk(class)),
     &      ' are ',othacou(1:lnblnk(othacou)),' & ',
     &      inacou(1:lnblnk(inacou))
          call edisp(iuout,outs)
          call BKITEM(idbh,class,item,ic,iit)

C Access the acoustic db and allow selection or pop-updisplay.
          call edisp(iuout,
     &       ' Select an inside material acoustic attribute...')
          call choseitem('acoustic',ACT,inacou,'multicon_db',
     &      selitem,selmenu,itmsel,icatsel,IER)
          IFB=igendbf(idbh)
          call scandbhd(IFB,idbh,ier)
          if(ACT(1:1).eq.'M'.and.itmsel.ne.0)then
            inacou=selitem
            iacumenu=selmenu
            primod=.true.
          endif
          call RCVITEM(idbh,class,item,ic,iit)
          if(item.ne.ittag(iit))then
            call edisp(iuout,'woops: got confused recovering item.')
          endif
        else
          call usrmsg('Sorry, acoustic db not available.',' ','P')
        endif
      ELSEIF(IW.EQ.14)THEN

C Provide browse popup for environmental db (constructions impacts).
        if(gendbok(8))then
          class=cattag(ic)
          item=ittag(iit)
          call BKITEM(idbh,class,item,ic,iit)
          call choseitct('envir_imp',ACT,cnstimp,'multicon_db',
     &      'CONSIMP ',selitem,selmenu,itmsel,icatsel,IER)
          if(ACT(1:1).eq.'M'.and.itmsel.ne.0)then
            cnstimp=selitem
            cnstm=selmenu
            primod=.true.
          endif
          call RCVITEM(idbh,class,item,ic,iit)
          if(item.ne.ittag(iit))then
            call edisp(iuout,'woops: got confused recovering item.')
          endif
        else
          call usrmsg('Sorry, environ impacts not available.',' ','P')
        endif
        continue
      ELSEIF(IW.ge.17.and.IW.lt.MPLEM-2)THEN

C Provide browse or popup for layer attributes. Advise user of current
C data and then back this up.
        if(gendbok(2))then
          il=IW-16
          goto 47
        else
          call usrmsg('Sorry, material db not available.',' ','P')
        endif
       call edisp(iuout,'one of the layers')
C << >>
      ELSE
        IW=-1
        GOTO 2
      ENDIF
      GOTO 3

C Editing of an layer (existing or new).
  47  continue
      class=cattag(ic)
      item=ittag(iit)
      call BKITEM(idbh,class,item,ic,iit)

C Present material pop-up.
      mmattag=mattag(il)
      call choseitem('material','-',mmattag,'multicon_db',selitem,
     &  selmenu,itmsel,icatsel,IER)
      IFB=igendbf(idbh)
      call scandbhd(IFB,idbh,ier)

C If editing offer user selection of materials. Based on the material
C extract new layer description attribute.
      if(ACT(1:1).eq.'M')then

C Before presenting material list ask user if this is a thermal
C layer or an LCA-only layer.
        modit=.false.
        dmodit=.false.
        if(thrmtg(il).eq.'t'.and.gaptg(il).eq.'g')then
          write(outs,'(a,i1,a)') 'Layer ',il,' is a thermal air gap'
          h(1)='You can change this to a solid thermal layer or a'
          h(2)='LCIA-only layer (spacer/fixing/frame)'
          CALL ASKOK(outs,'Change this?',modit,dmodit,2)
        elseif(thrmtg(il).eq.'t'.and.gaptg(il).eq.'s')then
          write(outs,'(a,i1,a)') 'Layer ',il,' is a thermal solid'
          h(1)='You can change this to a thermal air gap or a'
          h(2)='LCIA-only layer (spacer/fixing/frame)'
          CALL ASKOK(outs,'Change this?',modit,dmodit,2)
        elseif(thrmtg(il).eq.'n'.and.gaptg(il).eq.'f')then
          write(outs,'(a,i1,a)') 'Layer ',il,' is an LCIA frame'
          h(1)='You can change this to a thermal air gap or a'
          h(2)='LCIA-only layer (spacer/fixing)'
          CALL ASKOK(outs,'Change this?',modit,dmodit,2)
        elseif(thrmtg(il).eq.'n'.and.gaptg(il).eq.'c')then
          write(outs,'(a,i1,a)') 'Layer ',il,' is an LCIA spacer'
          h(1)='You can change this to a thermal layer or a'
          h(2)='LCIA-only layer (frame/fixing)'
          CALL ASKOK(outs,'Change this?',modit,dmodit,2)
        elseif(thrmtg(il).eq.'n'.and.gaptg(il).eq.'s')then
          write(outs,'(a,i1,a)') 'Layer ',il,' is an LCIA fixing device'
          h(1)='You can change this to a thermal layer or a'
          h(2)='LCIA-only layer (frame/spacer)'
          CALL ASKOK(outs,'Change this?',modit,dmodit,2)
        endif

C Present options.
        h(1)='Thermal layers may be solids or gasses (the latter will '
        h(2)='have a thermal resistance across the gap).'
        h(3)=' '
        h(4)='LCIA frame: layers can represent a frame around the '
        h(5)='perimeter of a surface with a constant cross section. '
        h(6)='You will be asked for a frame type. Impacts are by '
        h(7)='linear meter. '
        h(8)=' '
        h(9)='LCIA spacer: layers can represent a spacer (typically '
        h(10)='within a glazing unit). You will be asked for a spacer '
        h(11)='type and the total gap thickness for the spacer. '
        h(12)='Depending on the type of spacer the impact will be '
        h(13)='expressed per linear meter and by meter of width. '
        h(14)=' '
        h(15)='LCIA other: can represent any other surface related '
        h(16)='object. For a glue or coating or membrane use the real'
        h(17)='thickness. For nails or structural supports give the'
        h(18)='`equivalent` thickness that will have the same mass as '
        h(19)='object. Impacts are expressed per unit of mass.'
        CALL EASKATOG('Options:',' ','thermal solid','thermal gas',
     &    'LCIA frame','LCIA spacer','LICA other','continue',
     &    ' ',IWM,19)
        if(IWM.eq.1)then
          thrmtg(il)='t'
          gaptg(il)='s'
        elseif(IWM.eq.2)then
          thrmtg(il)='t'
          gaptg(il)='g'
        elseif(IWM.eq.3)then
          thrmtg(il)='n'
          gaptg(il)='f'
        elseif(IWM.eq.4)then
          thrmtg(il)='n'
          gaptg(il)='c'
        elseif(IWM.eq.5)then
          thrmtg(il)='n'
          gaptg(il)='s'
        elseif(IWM.eq.6)then
          continue
        endif
        call choseitem('material',ACT,mmattag,'multicon_db',selitem,
     &    selmenu,itmsel,icatsel,IER)
        IFB=igendbf(idbh)
        call scandbhd(IFB,idbh,ier)
        if(itmsel.ne.0)then

C Recover category information.
          isdbhash=2
          ISBFIL=igendbf(2)
          IREC=irscat(icatsel)
          read(ISBFIL,REC=IREC,IOSTAT=IOS,ERR=1)cattag(icatsel),
     &      cattype(icatsel),catmenu(icatsel),nbcatdoc(icatsel),
     &      nbcatit(icatsel),catdate(icatsel)
          ctype=cattype(icatsel)
          mattag(il)=selitem
          mmatmenu=selmenu
          itm=itmsel
          call rgenitem(isbfil,idbh,itm,ier)
          call prsmat(isdbhash,iit,'g',acoutag,colormtag,lcatag,
     &      lcatype,r1lca,r2lca,NCODE,DBCON,DBDEN,DBSHT,E,A,DRV,
     &      spec,rough,misc1,misc2,ier)
          mmatmenu='none'
          call prsmenu(isdbhash,mattag(il),mmatmenu,ihyg,ihygcat)
          IFB=igendbf(idbh)
          call scandbhd(IFB,idbh,ier)
          primod=.true.

C Check the material category type, if GAS the set IP()=0.
C Check to see that category matches earlier choice of gaptg().
          if(ctype(1:3).eq.'GAS')then
            IP(il)=0 
            gaptg(il)='g'
          else
            IP(il)=ihyg
            if(thrmtg(il).eq.'t')gaptg(il)='s'
          endif
          laydesc(il)=mmatmenu

C If other than an LCIA frame ask for thickness.
          if(thrmtg(il).eq.'n'.and.gaptg(il).eq.'f')then
            continue
          else
            H(1)='The thickness of the layer is nominally between'
            H(2)='0.0001 metres and 0.500 metres.'
            VAL=DT(il)
            CALL EASKR(VAL,' ',' Layer thickness (metres): ',
     &        0.0001,'W',0.5,'W',0.25,'layer thickness',IER,2)
            DT(il)=VAL
          endif

C If an air layer then ask for gap resistance.
          IF(IP(il).EQ.0)THEN
            H(1)='A set of default assumptions on the R value of'
            H(2)='an air gap is requested, in case the '
            H(3)='construction is used in different orientations.'
            H(4)='This information allows for the automatic'
            H(5)='creation of zone construction files.'
            WRITE(outs,'(2a)')'Default R value for air gap in ',
     &        item(1:lnblnk(item))

            VAL=0.17
            CALL EASKR(VAL,outs,
     &        ' if the construction orientation is vertical: ',
     &        0.01,'W',99.9,'W',0.17,'def air gap R',IER,5)
            DRA(il,1)=VAL
            CALL EASKR(VAL,outs,
     &        ' if the construction is a floor or ceiling  : ',
     &        0.01,'W',99.9,'W',0.17,'def air gap R',IER,6)
            DRA(il,2)=VAL
            CALL EASKR(VAL,outs,
     &        ' if the construction is sloped or UNKNOWN   : ',
     &        0.01,'W',99.9,'W',0.17,'def air gap R',IER,6)
            DRA(il,3)=VAL
          ELSE
            DRA(il,1)=0.0
            DRA(il,2)=0.0
            DRA(il,3)=0.0
          ENDIF

C If an inner or an outer layer then need to check against the
C current optical and acoustic attributes for MLC.
          if(il.eq.1)then
            if(othacou(1:lnblnk(othacou)).ne.
     &         acoutag(1:lnblnk(acoutag)))then
              write(outs,'(5a)') 'Use material acoustic ',
     &          acoutag(1:lnblnk(acoutag)),' rather than ',
     &          othacou(1:lnblnk(othacou)),'?'
              dok=.true.
              h(1)='An inconsistency was found in the database. '
              h(2)='The acoustic attributes are different. '
              call askok(outs,' ',ok,dok,2)
              if(ok)then
                othacou=acoutag
              endif
            endif
            if(othcolr(1:lnblnk(othcolr)).ne.
     &         colormtag(1:lnblnk(colormtag)))then
              write(outs,'(5a)') 'Use material colour ',
     &          colormtag(1:lnblnk(colormtag)),' rather than ',
     &          othcolr(1:lnblnk(othcolr)),'?'
              dok=.true.
              h(1)='An inconsistency was found in the database. '
              h(2)='The colour attributes are different. '
              call askok(outs,' ',ok,dok,2)
              if(ok)then
                othcolr=colormtag
              endif
            endif
          elseif(il.eq.nlay.and.thrmtg(il).eq.'t')then
            if(othacou(1:lnblnk(othacou)).ne.
     &         inacou(1:lnblnk(inacou)))then
              write(outs,'(5a)') 'Use material acoustic ',
     &          acoutag(1:lnblnk(acoutag)),' rather than ',
     &          inacou(1:lnblnk(inacou)),'?'
              dok=.true.
              h(1)='An inconsistency was found in the database. '
              h(2)='The acoustic attributes are different. '
              call askok(outs,' ',ok,dok,2)
              if(ok)then
                inacou=acoutag
              endif
            endif
            if(incolr(1:lnblnk(incolr)).ne.
     &         colormtag(1:lnblnk(colormtag)))then
              write(outs,'(5a)') 'Use material colour ',
     &          colormtag(1:lnblnk(colormtag)),' rather than ',
     &          incolr(1:lnblnk(incolr)),'?'
              dok=.true.
              h(1)='An inconsistency was found in the database. '
              h(2)='The colour attributes are different. '
              call askok(outs,' ',ok,dok,2)
              if(ok)then
                incolr=colormtag
              endif
            endif
          endif
          primod=.true.
        endif
      else
      endif
      call RCVITEM(idbh,class,item,ic,iit)
      if(item.ne.ittag(iit))then
        call edisp(iuout,'woops: got confused recovering item.')
      endif
      goto 3

    1 call edisp(iuout,'Error updating data record')
      call edisp(iuout,outstr)
      return
      end
