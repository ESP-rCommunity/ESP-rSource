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

C This includes facilities to manipulate an item from a generic
C database.
C EITCOMMON: Common (first 6 menu items) of generic database item.
C edimpact: Edit environmental impacts (Global Warming Potential (GWP), Acidification
C    Potential (AP), PhotoSmog depletion (PS)and Utilisation of Non Renewable
C    Energy (UNRE).
C EDMAT2: Material and construction item (display only to prevent recursion).
C EDMAT: Material item display and editing.
C EDACOUS: Acoustics item display and editing.
C EDCOLOUR: Colour item display and editing.
C EDTRANSP: Transport item display and editing.
C BKITEM: Put current item in alternative common block structure for later
C   recovery.
C RCVITEM: Recover item from alternative common block structure.
C CPITEM: Copy item iitorig to catg iccopy returning index of copied item as iitcopy. 
C MVITEM: Move item iitorig in icorig to catg icmove returning index of moved 
C   item as iitmove. 
C Choseitem: supports selection of an entity from a different database.
C Choseitct: supports selection of an entity from a specific category.

C ************* EITCOMMON
C EITCOMMON: Common (first 6 menu items) of generic database item.
C Assumes that current database is open on unit IFB.
C Works with the current file unit IFB and the current hash table idbh,
C and the current category ic and item iit. Ino is the menu index
C the user selected and iract informs the parent code what to do
C when EITCOMMON returns.
      subroutine eitcommon(IFB,idbh,ic,iit,ino,ACT,iract,primod,IER)
#include "espriou.h"
C espriou.h provides currentfile.
#include "esprdbfile.h"
C providing paramaters MGDB,MGCL,MGIT
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN

C Generic db commons:
      common/gendb1/idbwidth(MGDB),lastr(MGDB),nbdbdoc(MGDB),
     &              nbunit(MGDB),nbsrc(MGDB),nbcat(MGDB),nbitem(MGDB),
     &              idbver(MGDB)      
      common/gendbu/unitchr(24),unitstr(24),unitnote(24)
      common/gendbs/srcchr(24),srcnote(24)
      common/gendb6/ittag(MGIT),itmenu(MGIT)
      common/gendb8a/ituntstr,itsrcstr,itcnfstr,itdate(MGIT),itdoc
      common/gendb8c/nbitusc(MGIT),nbitdat(MGIT),
     &               nbitrep(MGIT),ibitusc(MGIT)

      character ittag*12,itmenu*32,itdate*24,itdoc*248
      character ACT*1,outs*124
      character ituntstr*42,itsrcstr*1,itcnfstr*42
      character unitchr*4,unitstr*12,unitnote*72
      character srcchr*4,srcnote*248,t12*12,t32*32,t248*248
      logical primod

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='eitcommon'  ! set for subroutine
      helptopic='dbm_edit_common_abcd'
      call gethelptext(helpinsub,helptopic,nbhelp)

      write(currentfile,'(a)')  gendbfn(idbh)(1:lnblnk(gendbfn(idbh)))
      if(ino.EQ.2)THEN
        if(ACT(1:1).eq.'M')then
          t12=ittag(iit)
          CALL EASKS(t12,' ',' Identity string:  ',
     &      12,'xxx','item identity',IER,nbhelp)
          if(ier.eq.0.and.t12(1:2).ne.'  ')then
            ittag(iit)=t12
            primod=.true.
          endif
        else
          call usrmsg('Currently in browse mode.',' ','P')
        endif
        iract=3
      elseif(ino.EQ.3)THEN
        if(ACT(1:1).eq.'M')then
          t32=itmenu(iit)
          CALL EASKS(t32,' ',
     &      ' Item descriptive phrase ? ',32,'new item',
     &      'no descriptor supplied',IER,nbhelp)
          if(ier.eq.0.and.t32(1:2).ne.'  ')then
            itmenu(iit)=t32
            primod=.true.
          endif
        else
          call usrmsg('Currently in browse mode.',' ','P')
        endif
        iract=3
      elseif(ino.EQ.5)THEN

C Display current sources and units.
C << what about uncertainty >>
        call edisp(iuout,'Units             Description ')
        do 11 iu=1,nbunit(idbh)
          write(outs,'(1x,a,2x,a,1x,a)')unitchr(iu)(1:1),
     &      unitstr(iu),unitnote(iu)(1:lnblnk(unitnote(iu)))
          call edisp(iuout,outs)
  11    continue
        call edisp(iuout,ituntstr)
        iract=3
      elseif(ino.EQ.6)THEN

C Item documentation (up to 248 characters). There is one block
C of text and no need to adjust the layout of the database if
C the block of text changes.
        call edisp248(iuout,itdoc,96)
        if(ACT(1:1).eq.'M'.or.ACT(1:1).eq.'m')then

C Non-proforma editing of category documentation. 
          t248=' '
          write(t248,'(a)')itdoc(1:lnblnk(itdoc))
          write(outs,'(a,I2)')' Documentation of item ',iit
          CALL EASKS248(t248,outs,': ',72,': ','id',IER,nbhelp)
          if(ier.eq.0)then
            write(itdoc,'(a)')t248(1:lnblnk(t248))
            primod=.true. 
            iract=3
          endif
        endif
      endif
      return
      end

C ******** edimpact ****************
C edimpact: Edit environmental impacts (Global Warming Potential (GWP), Acidification
C Potential (AP), PhotoSmog depletion (PS)and Utilisation of Non Renewable
C Energy (UNRE).
      subroutine edimpact(GWP,AP,PS,UNRE,ier)
      character hold*54

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='edimpact'  ! set for subroutine
      helptopic='dbm_GWP_AP_PS_NRE'
      call gethelptext(helpinsub,helptopic,nbhelp)
      
      write(hold,'(1P4e13.3)') GWP,AP,PS,UNRE
      CALL EASKS(hold,
     & 'GWP (kgeqCO2/kg), AP (kgeqSO2/kg), POCP (kgeqNOX/kg)',
     & 'NRE (MJ/kg)',54,'0.000 0.000 0.000 0.000','impacts',
     & IER,nbhelp)
      if(ier.eq.0.and.hold(1:2).ne.'  ')then
        k=0
        CALL EGETWR(hold,K,GWP,-0.01,999.0,'W','GWP',IER)
        CALL EGETWR(hold,K,AP,-0.01,999.0,'W','AP',IER)
        CALL EGETWR(hold,K,PS,-0.01,999.0,'W','PS',IER)
        CALL EGETWR(hold,K,UNRE,-0.01,999.0,'W','UNRE',IER)
      endif
      return
      end

C ************* EDMAT2
C EDMAT2: Material & construction item display (only). If ACT='H' present
C tabular listing as a pop-up. Assumes that current database is open on
C unit IFB.
      subroutine edmat2(db,IFB,idbh,ic,iit,ACT,IER)
#include "building.h"
#include "espriou.h"
#include "esprdbfile.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN

C Generic db commons:
      common/gendb2/dbtag(MGDB),dbmenu(MGDB),dbdate(MGDB),dbnote(MGDB),
     &              dbdoc(10)
      common/gendb6/ittag(MGIT),itmenu(MGIT)
      common/gendb8a/ituntstr,itsrcstr,itcnfstr,itdate(MGIT),itdoc
      common/gendbtg/showtags

      dimension thrmtg(ME),gaptg(ME),mattag(ME),DT(ME)
      dimension IP(ME),DRA(ME,3),laydesc(ME)

      CHARACTER ACT*1,db*12
      character dbtag*12,dbmenu*32,dbdate*24,dbdoc*248
      character ittag*12,itmenu*32,itdate*24,itdoc*248,lcatype*1
      character dbnote*72,ituntstr*42,itsrcstr*1,itcnfstr*42
      character acoutag*12,colormtag*12,lcatag*12
      character acoumenu*32,colormenu*32,lcamenu*32
      character unote*72,snote*248
      character ustr*12,ustra*12,ustrb*12,ustrc*12,ustrd*12,ustre*12
      character ustrf*12,ustrg*12,ustrh*12,ustri*12
      character oacumenu*32,iacumenu*32,ocmenu*32,icmenu*32
      character symname*12,othcolr*12,incolr*12,othacou*12,inacou*12
      character OTF*4,OPT*12,cnstimp*12,outs*124,outs248*248
      character cnstm*32,mmatmenu*32
      character thrmtg*1,gaptg*1,mattag*12,laydesc*32
      logical primod,showtags
      real misc1,misc2

C Scan the data from the common blocks.
      write(currentfile,'(a)')  gendbfn(idbh)(1:lnblnk(gendbfn(idbh)))
      call scandbhd(IFB,idbh,ier)
      call rgenitem(IFB,idbh,iit,ier)

      if(db(1:11).eq.'multicon_db')then
        call PRSMLC(idbh,iit,'g',symname,othcolr,incolr,OTF,OPT,
     &    othacou,inacou,othir,sinir,othsol,sinsol,cnstimp,
     &    nlay,thrmtg,gaptg,mattag,DT,IP,DRA,laydesc,ier)
        call prssrc(idbh,snote)

        call prsmenu(5,othacou,oacumenu,iacou,iacoucat)
        call prsmenu(5,inacou,iacumenu,iacou,iacoucat)
        call prsmenu(6,othcolr,ocmenu,icolr,icolrcat)
        call prsmenu(6,incolr,icmenu,icolr,icolrcat)
        call prsmenu(8,cnstimp,cnstm,icnstimp,icnstcat)
        IFB=igendbf(idbh)
        call scandbhd(IFB,idbh,ier)

      elseif(db(1:8).eq.'material')then

C Generate text for a material db item.
        call prsmat(idbh,iit,'g',acoutag,colormtag,lcatag,
     &    lcatype,r1lca,r2lca,NCODE,DBCON,DBDEN,DBSHT,E,A,DRV,
     &    spec,rough,misc1,misc2,ier)

C Get source for this item.
        call prssrc(idbh,snote)

C Get the units for each of the data fields.
        call prsunit(idbh,8,ustr,unote)
        call prsunit(idbh,9,ustra,unote)
        call prsunit(idbh,10,ustrb,unote)
        call prsunit(idbh,11,ustrc,unote)
        call prsunit(idbh,12,ustrd,unote)
        call prsunit(idbh,13,ustre,unote)
        call prsunit(idbh,14,ustrf,unote)
        call prsunit(idbh,15,ustrg,unote)
        call prsunit(idbh,16,ustrh,unote)
        call prsunit(idbh,17,ustri,unote)

C Debug
C        write(6,*) ustr,ustra,ustrb,ustrc,ustrd,ustre
C        write(6,*) ustrf,ustrg,ustrh,ustri

C Recover associated text for the material attributes.
        call prsmenu(5,acoutag,acoumenu,iacou,iacoucat)
        call prsmenu(6,colormtag,colormenu,icolr,icolrcat)
        call prsmenu(8,lcatag,lcamenu,ilca,ilcacat)

C After parsing menus recover the header of the current db.
        IFB=igendbf(idbh)
        call scandbhd(IFB,idbh,ier)
      endif

C If user has request a report display start of table (this is
C common to all item types).
      if(ACT(1:1).eq.'T'.or.ACT(1:1).eq.'H')then
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
      endif

C Now print bespoke portions of the table.
      if(db(1:11).eq.'multicon_db')then

C Generate text for constructions.
        thick=0.0
        do 143 il=1,nlay
          thick=thick+DT(il)
 143    continue
        if(symname(1:4).eq.'same')then
          write(outs,'(5a)') ittag(iit)(1:la),
     &      ' is symmetrical of type ',OTF(1:lnblnk(OTF)),
     &      ' & optics: ',OPT(1:lnblnk(OPT))
        else
          write(outs,'(7a)') ittag(iit)(1:la),
     &      ' matches (reversed) ',symname,' & is of type ',
     &      OTF(1:lnblnk(OTF)),' & optics: ',OPT(1:lnblnk(OPT))
        endif
        call edisp(iuout,outs)
        call edisp(iuout,'  ')
        call edisp(iuout,'           inside face    outside face')
        write(outs,'(a,2f10.3)') ' IR emmis  ',sinir,othir
        call edisp(iuout,outs)
        write(outs,'(a,2f10.3)') ' Solar abs ',sinsol,othsol
        call edisp(iuout,outs)

        if(showtags)then
          write(outs,'(8a)') ' colour (',
     &      incolr(1:lnblnk(incolr)),') ',iacumenu(1:lnblnk(iacumenu)),
     &      ' (',othcolr(1:lnblnk(othcolr)),') ',
     &      ocmenu(1:lnblnk(ocmenu))
        else
          write(outs,'(4a)') ' colour    ',
     &      iacumenu(1:lnblnk(iacumenu)),'  ',ocmenu(1:lnblnk(ocmenu))
        endif
        call edisp(iuout,outs)
        if(showtags)then
          write(outs,'(8a)') ' acoustic (',
     &      inacou(1:lnblnk(inacou)),') ',iacumenu(1:lnblnk(iacumenu)),
     &      ' (',othacou(1:lnblnk(othacou)),') ',
     &      oacumenu(1:lnblnk(oacumenu))
        else
          write(outs,'(4a)') ' acoustic  ',
     &      iacumenu(1:lnblnk(iacumenu)),'  ',
     &      oacumenu(1:lnblnk(oacumenu))
        endif
        call edisp(iuout,outs)

C If environmental impacts information, show some of this.
        if(cnstimp(1:4).eq.'none')then
          lm=lnblnk(itmenu(iit))
          write(outs,'(2a)')
     &   ' No environmental impacts defined for ',itmenu(iit)(1:lm)
        else
          write(outs,'(4a)')' Environmental impact: (',
     &      cnstimp(1:lnblnk(cnstimp)),') ',cnstm(1:lnblnk(cnstm))
        endif
        call edisp(iuout,outs)
        if(nlay.gt.0)then
          lm=lnblnk(itmenu(iit))
          write(outs,'(1x,2a,i1,a,f6.3,a)') 
     &     itmenu(iit)(1:lm),' has ',nlay,
     &     ' layers and an overall thickness of',thick,'m.'

          if(showtags)then
            write(outs,'(2a)')
     &        ' layer|type   |material   |thick|legacy|',
     &        ' description   '
            call edisp(iuout,outs)
            write(outs,'(a)')
     &        '              |name       | (m) | id   | '
            call edisp(iuout,outs)
            do 42 il=1,nlay
              ml=lnblnk(mattag(il))
              if(ml.lt.10)ml=10
              if(thrmtg(il).eq.'t'.and.gaptg(il).eq.'s')then
                write(outs,'(i2,3a,f6.3,i4,2a)') il,
     &           ' thrm solid (',mattag(il)(1:ml),') ',
     &           DT(il),IP(il),'  ',laydesc(il)
              elseif(thrmtg(il).eq.'t'.and.gaptg(il).eq.'g')then
                write(outs,'(i2,3a,f6.3,i4,2a)') il,
     &           ' thrmal gap (',mattag(il)(1:ml),') ',
     &           DT(il),IP(il),'  ',laydesc(il)
              elseif(thrmtg(il).eq.'n'.and.gaptg(il).eq.'f')then
                write(outs,'(i2,3a,f6.3,i4,2a)') il,
     &           ' LCIA frame (',mattag(il)(1:ml),') ',
     &           DT(il),IP(il),'  ',laydesc(il)
              elseif(thrmtg(il).eq.'n'.and.gaptg(il).eq.'c')then
                write(outs,'(i2,3a,f6.3,i4,2a)') il,
     &           ' LCIA spacr (',mattag(il)(1:ml),') ',
     &           DT(il),IP(il),'  ',laydesc(il)
              elseif(thrmtg(il).eq.'n'.and.gaptg(il).eq.'s')then
                write(outs,'(i2,3a,f6.3,i4,2a)') il,
     &           ' LCIA fixng (',mattag(il)(1:ml),') ',
     &           DT(il),IP(il),'  ',laydesc(il)
              endif
              call edisp(iuout,outs)
  42        continue
          else

C Report without tags.
            write(outs,'(2a)')
     &      ' layer|type   |thick|legacy| descriptions of'
            call edisp(iuout,outs)
            write(outs,'(a)')
     &      '              | (m) | id   | material'
            call edisp(iuout,outs)
            do 242 il=1,nlay

C Recover the current descriptions of the material tags.
              call prsmenu(2,mattag(il),mmatmenu,imat,imatcat)
              IFB=igendbf(idbh)
              call scandbhd(IFB,idbh,ier)
              ml=lnblnk(mattag(il))
              mm=lnblnk(mmatmenu)
              if(ml.lt.10)ml=10
              if(thrmtg(il).eq.'t'.and.gaptg(il).eq.'s')then
                write(outs,'(i2,a,f6.3,i4,2a)') il,
     &          ' thrm solid  ',DT(il),IP(il),'  ',mmatmenu(1:mm)
              elseif(thrmtg(il).eq.'t'.and.gaptg(il).eq.'g')then
                write(outs,'(i2,a,f6.3,i4,2a)') il,
     &          ' thrmal gap  ',DT(il),IP(il),'  ',mmatmenu(1:mm)
              elseif(thrmtg(il).eq.'n'.and.gaptg(il).eq.'f')then
                write(outs,'(i2,a,f6.3,i4,2a)') il,
     &          ' LCIA frame  ',DT(il),IP(il),'  ',mmatmenu(1:mm)
              elseif(thrmtg(il).eq.'n'.and.gaptg(il).eq.'c')then
                write(outs,'(i2,a,f6.3,i4,2a)') il,
     &          ' LCIA spacr  ',DT(il),IP(il),'  ',mmatmenu(1:mm)
              elseif(thrmtg(il).eq.'n'.and.gaptg(il).eq.'s')then
                write(outs,'(i2,a,f6.3,i4,2a)') il,
     &          ' LCIA fixng  ',DT(il),IP(il),'  ',mmatmenu(1:mm)
              endif
              call edisp(iuout,outs)
 242        continue

C After parsing menus recover the header of the current db.
            IFB=igendbf(idbh)
            call scandbhd(IFB,idbh,ier)
          endif
        endif
      elseif(db(1:8).eq.'material')then
        write(outs,'(A)')  ' '
        call edisp(iuout,outs)
        if(showtags)then
          write(outs,'(4a)') ' acoustic: (',
     &     acoutag(1:lnblnk(acoutag)),') ',acoumenu(1:lnblnk(acoumenu))
        else
          write(outs,'(2a)') ' acoustic: ',
     &     acoumenu(1:lnblnk(acoumenu))
        endif
        call edisp(iuout,outs)
        if(showtags)then
          write(outs,'(4a)') ' colour: (',
     &     colormtag(1:lnblnk(colormtag)),') ',
     &     colormenu(1:lnblnk(colormenu))
        else
          write(outs,'(2a)') ' colour: ',
     &      colormenu(1:lnblnk(colormenu))
        endif
        call edisp(iuout,outs)
        if(showtags)then
         write(outs,'(4a)') ' environmental impacts: (',
     &    lcatag(1:lnblnk(lcatag)),') ',lcamenu(1:lnblnk(lcamenu))
        else
         write(outs,'(2a)') ' environmental impacts: ',
     &    lcamenu(1:lnblnk(lcamenu))
        endif
        call edisp(iuout,outs)
        if(lcatype.eq.'-')then
          write(outs,'(a)') ' '
        else
          write(outs,'(3a,2f7.3)') ' non-thermal use: (',
     &      lcatype,') data ',r1lca,r2lca
        endif
        call edisp(iuout,outs)
        write(outs,'(2a)') 
     &    ' Conductivity  Density  Specific Heat Emissivity ',
     &    ' Absorptivity'
        call edisp(iuout,outs)
        write(outs,'(10A)') ' ',ustr,' ',ustra,' ',ustrb,
     &    ' ',ustrc,' ',ustrd
        call edisp(iuout,outs)
        write(outs,'(2x,5F11.3)') DBCON,DBDEN,DBSHT,E,A
        call edisp(iuout,outs)
        write(outs,'(a)') 
     &  ' Diffusion res Specularity Roughness  Miscel Miscel '
        call edisp(iuout,outs)
        write(outs,'(10A)') ' ',ustre,' ',ustrf,' ',ustrg,
     &    ' ',ustrh,' ',ustri
        call edisp(iuout,outs)
        write(outs,'(2x,5F11.3)') DRV,spec,rough,misc1,misc2
        call edisp(iuout,outs)
      endif
      return
      end


C ************* EDMAT
C  EDMAT: Material item display and editing.  If ACT='T' then produce
C a tabular listing and return, if ACT='H' present tabular listing
C as a pop-up, otherwise present editing menu.
C Assumes that current database is open on unit IFB.
      subroutine edmat(IFB,idbh,ic,iit,ACT,IER)
#include "espriou.h"
#include "esprdbfile.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN

C Generic db commons:
      common/gendb1/idbwidth(MGDB),lastr(MGDB),nbdbdoc(MGDB),
     &              nbunit(MGDB),nbsrc(MGDB),nbcat(MGDB),nbitem(MGDB),
     &              idbver(MGDB)      
      common/gendb2/dbtag(MGDB),dbmenu(MGDB),dbdate(MGDB),dbnote(MGDB),
     &              dbdoc(10)
      common/gendb3/irscat(MGCL),nbcatdoc(MGCL),nbcatit(MGCL)
      common/gendb4/cattag(MGCL),cattype(MGCL),catmenu(MGCL),
     &              catdate(MGCL),catdoc(5)

      common/gendb6/ittag(MGIT),itmenu(MGIT)
      common/gendb8a/ituntstr,itsrcstr,itcnfstr,itdate(MGIT),itdoc
      common/gendbtg/showtags

      dimension PELM(24)

      CHARACTER OUTSTR*124,PELM*38,ACT*1,dstmp*24,db*12
      character dbtag*12,dbmenu*32,dbdate*24,dbdoc*248,catdoc*248
      character cattag*12,cattype*8,catmenu*32,catdate*24
      character ittag*12,itmenu*32,itdate*24,itdoc*248,lcatype*1
      character dbnote*72,ituntstr*42,itsrcstr*1,itcnfstr*42
      character acoutag*12,colormtag*12,lcatag*12
      character acoumenu*32,colormenu*32,lcamenu*32
      character class*12,item*12,selitem*12,selmenu*32
      character kact*1,head*38,snote*248
      character ustr*12,ustra*12,ustrb*12,ustrc*12,ustrd*12,ustre*12
      character ustrf*12,ustrg*12,ustrh*12,ustri*12,unote*72
      logical primod
      real misc1,misc2
      logical showtags
      integer MPLEM,IW ! max items and current menu item

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='edmat'  ! set for subroutine

C Scan the data from the common blocks.
    9 primod=.false.
      write(currentfile,'(a)')  gendbfn(idbh)(1:lnblnk(gendbfn(idbh)))
      call scandbhd(IFB,idbh,ier)
      call rgenitem(IFB,idbh,iit,ier)
      call prsmat(idbh,iit,'g',acoutag,colormtag,lcatag,
     &  lcatype,r1lca,r2lca,NCODE,DBCON,DBDEN,DBSHT,E,A,DRV,
     &  spec,rough,misc1,misc2,ier)
      call prssrc(idbh,snote)

C Recover associated text for the material attributes.
      write(currentfile,'(a)')  gendbfn(5)(1:lnblnk(gendbfn(5)))
      call prsmenu(5,acoutag,acoumenu,iacou,iacoucat)
      write(currentfile,'(a)')  gendbfn(6)(1:lnblnk(gendbfn(6)))
      call prsmenu(6,colormtag,colormenu,icolr,icolrcat)
      write(currentfile,'(a)')  gendbfn(8)(1:lnblnk(gendbfn(8)))
      call prsmenu(8,lcatag,lcamenu,ilca,ilcacat)

C After parsing menus recover the header of the current db.
      IFB=igendbf(idbh)
      call scandbhd(IFB,idbh,ier)

C Get the units for each of the data fields.
      call prsunit(idbh,8,ustr,unote)
      call prsunit(idbh,9,ustra,unote)
      call prsunit(idbh,10,ustrb,unote)
      call prsunit(idbh,11,ustrc,unote)
      call prsunit(idbh,12,ustrd,unote)
      call prsunit(idbh,13,ustre,unote)
      call prsunit(idbh,14,ustrf,unote)
      call prsunit(idbh,15,ustrg,unote)
      call prsunit(idbh,16,ustrh,unote)
      call prsunit(idbh,17,ustri,unote)

C Debug.
C      write(6,*) ustr,ustra,ustrb,ustrc,ustrd,ustre
C      write(6,*) ustrf,ustrg,ustrh,ustri

C If user has request a report only display the relevant table.
      if(ACT(1:1).eq.'T'.or.ACT(1:1).eq.'H')then

C To prevent recursion this probably should use edmat2
        db='material'
        call edmat2(db,IFB,idbh,ic,iit,ACT,ier)
        return
      endif

C If anything has changed it needs to be written out. First
C remove spaces in data lines(s). Recover category information.
   3  if(primod.and.ACT(1:1).eq.'M')then
        call usrmsg('Updating item data... ',' ','-')
        call PRSMAT(idbh,iit,'p',acoutag,colormtag,lcatag,
     &    lcatype,r1lca,r2lca,NCODE,DBCON,DBDEN,DBSHT,E,A,DRV,
     &    spec,rough,misc1,misc2,ier)

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
      write(PELM(1),'(2A)')      '  category: ',catmenu(ic)(1:26)
      write(PELM(2),'(3A)') kact(act,'a'),' tag : ',ittag(iit)
      write(PELM(3),'(3A)') kact(act,'b'),' name: ',itmenu(iit)(1:30)
      write(PELM(4),'(2A)')      '  date: ',itdate(iit)(1:24)
      write(PELM(5),'(3a)') kact(act,'c'),' source: ',snote(1:27)
      write(PELM(6),'(3a)') kact(act,'d'),' notes: ',itdoc(1:24)
      PELM(7)=                   '  ____________description__  '

C Adjust printed width of description.
      lb=MIN0(lnblnk(acoumenu),20)
      lc=MIN0(lnblnk(colormenu),20)
      ld=MIN0(lnblnk(lcamenu),20)
      write(PELM(8),'(5a)')kact(act,'f'), ' acoustic     : ',
     &  acoumenu(1:lb)
      write(PELM(9),'(5a)')kact(act,'g'), ' colour       : ',
     &  colormenu(1:lc)
      write(PELM(10),'(5a)')kact(act,'i'),' environ impct: ',
     &  lcamenu(1:ld)
      if(lcatype.eq.'-')then
        write(PELM(11),'(3a)')kact(act,'j'),'  ',lcatype
      else
        write(PELM(11),'(4a,2f7.3)')kact(act,'j'),' use: (',lcatype,
     &    ') data ',r1lca,r2lca
      endif
      write(PELM(12),'(2A,F9.3,1x,a)')kact(act,'e'),' conductivity :',
     &  DBCON,ustr
      write(PELM(13),'(2A,F8.2,1x,a)')kact(act,'f'),' density      :',
     &  DBDEN,ustra
      write(PELM(14),'(2A,F8.2,1x,a)')kact(act,'g'),' specific heat:',
     &  DBSHT,ustrb
      write(PELM(15),'(2A,F8.2,1x,a)')kact(act,'h'),' emissivity   :',
     &  E,ustrc
      write(PELM(16),'(2A,F8.2,1x,a)')kact(act,'i'),' absorptivity :',
     &  A,ustrd
      write(PELM(17),'(2A,F6.0,1x,a)')kact(act,'j'),' diffusion res:',
     &  DRV,ustre
      write(PELM(18),'(2A,F8.2,1x,a)')kact(act,'m'),' specularity  :',
     &  spec,ustrf
      write(PELM(19),'(2A,F8.2,1x,a)')kact(act,'n'),' roughness    :',
     &  rough,ustrg
      write(PELM(20),'(2A,F6.0,1x,a)')kact(act,'o'),' miscel data 1:',
     &  misc1,ustrh
      write(PELM(21),'(2A,F6.0,1x,a)')kact(act,'p'),' miscel data 2:',
     &  misc2,ustri
      PELM(22)=           '  _________________________________'
      PELM(23)=           '? help                          '
      PELM(24)=           '- exit                          '
      MPLEM=24

C Help text for this menu.
   2  helptopic='dbm_material_edits'
      call gethelptext(helpinsub,helptopic,nbhelp)

      call usrmsg(' ',' ','-')
      write(head,'(2a)') '  Item in ',dbmenu(idbh)(1:27)

      CALL EMENU(head,PELM,MPLEM,IW)

      IF(IW.EQ.0.OR.IW.EQ.MPLEM)THEN
        RETURN
      ELSEIF(IW.EQ.MPLEM-1)THEN

C Help.
        helptopic='dbm_material_edits'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('material item edit help',nbhelp,'-',0,0,IER)
      ELSEIF(IW.EQ.2.or.IW.eq.3.or.IW.eq.5.or.IW.eq.6)THEN

C << eventually adjust to include uncertainty in the common editing? >>
        iract=0
        call EITCOMMON(IFB,idbh,ic,iit,IW,ACT,iract,primod,IER)
        if(iract.eq.3)goto 3
        if(iract.eq.9)goto 3
      ELSEIF(IW.EQ.8)THEN

C Provide browse or popup for incide face acoustic attribute.
        if(gendbok(5))then
          class=cattag(ic)
          item=ittag(iit)
          call BKITEM(idbh,class,item,ic,iit)
          call choseitem('acoustic',ACT,acoutag,'material',selitem,
     &      selmenu,itmsel,icatsel,IER)
          IFB=igendbf(idbh)
          call scandbhd(IFB,idbh,ier)
          if(ACT(1:1).eq.'M'.and.itmsel.ne.0)then
            acoutag=selitem
            acoumenu=selmenu
            primod=.true.
          endif
          call RCVITEM(idbh,class,item,ic,iit)
          if(item.ne.ittag(iit))then
            call edisp(iuout,'woops: got confused recovering item.')
          endif
        else
          call usrmsg('Sorry, acoustic db not available.',' ','P')
        endif
      ELSEIF(IW.EQ.9)THEN

C Provide browse or popup for colour attribute.
        if(gendbok(6))then
          class=cattag(ic)
          item=ittag(iit)
          call BKITEM(idbh,class,item,ic,iit)
          call choseitem('colourimetry',ACT,colormtag,'material',
     &      selitem,selmenu,itmsel,icatsel,IER)
          IFB=igendbf(idbh)
          call scandbhd(IFB,idbh,ier)
          if(ACT(1:1).eq.'M'.and.itmsel.ne.0)then
            colormtag=selitem
            colormenu=selmenu
            primod=.true.
          endif
          call RCVITEM(idbh,class,item,ic,iit)
          if(item.ne.ittag(iit))then
            call edisp(iuout,'woops: got confused recovering item.')
          endif
        else
          call usrmsg('Sorry, colour db not available.',' ','P')
        endif
      ELSEIF(IW.EQ.10)THEN

C Provide browse popup for environmental db.
        if(gendbok(8))then
          class=cattag(ic)
          item=ittag(iit)
          call BKITEM(idbh,class,item,ic,iit)
          call choseitem('envir_imp',ACT,lcatag,'material',selitem,
     &      selmenu,itmsel,icatsel,IER)
          IFB=igendbf(idbh)
          call scandbhd(IFB,idbh,ier)
          if(ACT(1:1).eq.'M'.and.itmsel.ne.0)then
            lcatag=selitem
            lcamenu=selmenu
            primod=.true.
          endif
          call RCVITEM(idbh,class,item,ic,iit)
          if(item.ne.ittag(iit))then
            call edisp(iuout,'woops: got confused recovering item.')
          endif
        else
          call usrmsg('Sorry, environ impacts not available.',' ','P')
        endif
      ELSEIF(IW.EQ.11)THEN
        if(ACT(1:1).eq.'M')then
          call usrmsg('<< select LCA name >>',' ','P')
        else
          call usrmsg('Currently in browse mode.',' ','P')
        endif
      ELSEIF(IW.EQ.12)THEN
        if(ACT(1:1).eq.'M')then
          CALL EASKR(DBCON,' ',' Conductivity : ',
     &            0.001,'W',300.,'W',1.,'conductivity',IER,nbhelp)
          primod=.true.
          call usrmsg(' ',' ','-')
        else
          call usrmsg('Currently in browse mode.',' ','P')
        endif
      ELSEIF(IW.EQ.13)THEN
        if(ACT(1:1).eq.'M')then
          CALL EASKR(DBDEN,' ',' Density  : ',
     &             1.0,'W',9000.,'W',100.,'density',IER,nbhelp)
          primod=.true.
          call usrmsg(' ',' ','-')
        else
          call usrmsg('Currently in browse mode.',' ','P')
        endif
      ELSEIF(IW.EQ.14)THEN
        if(ACT(1:1).eq.'M')then
          CALL EASKR(DBSHT,' ',' Specific heat : ',
     &             1.0,'W',2000.,'W',100.,'specific heat',IER,nbhelp)
          primod=.true.
          call usrmsg(' ',' ','-')
        else
          call usrmsg('Currently in browse mode.',' ','P')
        endif
      ELSEIF(IW.EQ.15)THEN
        if(ACT(1:1).eq.'M')then
          CALL EASKR(E,' ',' Surface emissivity : ',
     &             0.01,'W',0.99,'W',0.9,'emissivity',IER,nbhelp)
          primod=.true.
          call usrmsg(' ',' ','-')
        else
          call usrmsg('Currently in browse mode.',' ','P')
        endif
      ELSEIF(IW.EQ.16)THEN
        if(ACT(1:1).eq.'M')then
          CALL EASKR(A,' ',' Surface absorptance : ',
     &             0.01,'W',0.99,'W',0.9,'absorptance',IER,nbhelp)
          primod=.true.
          call usrmsg(' ',' ','-')
        else
          call usrmsg('Currently in browse mode.',' ','P')
        endif
      ELSEIF(IW.EQ.17)THEN
        if(ACT(1:1).eq.'M')then
          CALL EASKR(DRV,' ',' Diffusion resistance : ',
     &      1.0,'W',20000.,'W',1.,'diffusion resistance',IER,nbhelp)
          primod=.true.
          call usrmsg(' ',' ','-')
        else
          call usrmsg('Currently in browse mode.',' ','P')
        endif
      ELSEIF(IW.EQ.18)THEN
        if(ACT(1:1).eq.'M')then
          CALL EASKR(spec,' ',' Specularity : ',
     &      0.0,'W',2.,'W',0.,'specularity',IER,nbhelp)
          primod=.true.
          call usrmsg(' ',' ','-')
        else
          call usrmsg('Currently in browse mode.',' ','P')
        endif
      ELSEIF(IW.EQ.19)THEN
        if(ACT(1:1).eq.'M')then
          CALL EASKR(rough,' ',' Roughness (visible) : ',
     &      0.0,'W',2.,'W',0.,'roughness',IER,nbhelp)
          primod=.true.
          call usrmsg(' ',' ','-')
        else
          call usrmsg('Currently in browse mode.',' ','P')
        endif
      ELSEIF(IW.EQ.20)THEN
        if(ACT(1:1).eq.'M')then
          CALL EASKR(misc1,' ',' Miscel data (1st) : ',
     &      0.0,'-',0.0,'-',0.,'miscel 1',IER,nbhelp)
          primod=.true.
          call usrmsg(' ',' ','-')
        else
          call usrmsg('Currently in browse mode.',' ','P')
        endif
      ELSEIF(IW.EQ.21)THEN
        if(ACT(1:1).eq.'M')then
          CALL EASKR(misc2,' ',' Miscel data (2nd) : ',
     &      0.0,'-',0.0,'-',0.,'miscel 2',IER,nbhelp)
          primod=.true.
          call usrmsg(' ',' ','-')
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


C ************* EDACOUS
C EDACOUS: Acoustics item display and editing.  If ACT='T' then produce
C a tabular listing and return, if ACT='H' present tabular listing
C as a pop-up, otherwise present editing menu.
C Assumes that current database is open on unit IFB.
      subroutine edacous(IFB,idbh,ic,iit,ACT,IER)
#include "espriou.h"
#include "esprdbfile.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN

C Generic db commons:
      common/gendb1/idbwidth(MGDB),lastr(MGDB),nbdbdoc(MGDB),
     &              nbunit(MGDB),nbsrc(MGDB),nbcat(MGDB),nbitem(MGDB),
     &              idbver(MGDB)      
      common/gendb2/dbtag(MGDB),dbmenu(MGDB),dbdate(MGDB),dbnote(MGDB),
     &              dbdoc(10)
      common/gendb3/irscat(MGCL),nbcatdoc(MGCL),nbcatit(MGCL)
      common/gendb4/cattag(MGCL),cattype(MGCL),catmenu(MGCL),
     &              catdate(MGCL),catdoc(5)

      common/gendb6/ittag(MGIT),itmenu(MGIT)
      common/gendb8a/ituntstr,itsrcstr,itcnfstr,itdate(MGIT),itdoc
      common/gendbtg/showtags

      dimension PELM(20),FRQ(17)

      CHARACTER OUTSTR*124,PELM*38,ACT*1,dstmp*24
      character dbtag*12,dbmenu*32,dbdate*24,dbdoc*248,catdoc*248
      character cattag*12,cattype*8,catmenu*32,catdate*24
      character ittag*12,itmenu*32,itdate*24,itdoc*248
      character dbnote*72,atype*4,aunit*8
      character ituntstr*42,itsrcstr*1,itcnfstr*42
      character ustr*12,unote*72,hold*32,kact*1,head*38
      character snote*248,outs*124,outs248*248
      logical primod,showtags
      integer MPLEM,IW ! max items and current menu item

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='edacous'  ! set for subroutine

C Scan the data from the common blocks. Check category type to see if
C SYS or MAT also check first character of item name to see if it starts
C with E for material or C for composite.
    9 primod=.false.
      write(currentfile,'(a)')  gendbfn(idbh)(1:lnblnk(gendbfn(idbh)))
      call scandbhd(IFB,idbh,ier)
      call rgenitem(IFB,idbh,iit,ier)
      call PRSACOU(idbh,iit,'g',atype,aunit,FRQ,ier)
      call prssrc(idbh,snote)

C If user has request a report only display the relevant table.
      if(ACT(1:1).eq.'T'.or.ACT(1:1).eq.'H')then
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

        if(atype(1:1).eq.'C'.or.atype(1:1).eq.'c')then
          write(outs,'(2a)') ' Composite properties, Unit: ',aunit
        elseif(atype(1:1).eq.'M'.or.atype(1:1).eq.'m')then
          write(outs,'(2a)') ' Material properties, Unit: ',aunit
        endif
        call edisp(iuout,outs)
        write(outs,'(A)') 
     &' coef @ 100, 125,  160,  200,  250,  315, 400'
        call edisp(iuout,outs)
        write(outs,'(5x,7f6.3)') (FRQ(j),j=1,7)
        call edisp(iuout,outs)
        write(outs,'(A)') 
     &' coef @ 500, 630, 800, 1000, 1250, 1600, 2000, 2500, 3150, 4000'
        call edisp(iuout,outs)
        write(outs,'(5x,10f6.3)') (FRQ(j),j=8,17)
        call edisp(iuout,outs)

        return
      endif

C If anything has changed it needs to be written out. First
C remove spaces in data lines(s). Recover category data.
   3  if(primod.and.ACT(1:1).eq.'M')then
        call usrmsg('Updating item data... ',' ','-')
        call PRSACOU(idbh,iit,'p',atype,aunit,FRQ,ier)

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
      write(PELM(5),'(3A)') kact(act,'c'),' source: ',snote(1:27)
      write(PELM(6),'(3a)') kact(act,'d'),' notes: ',itdoc(1:24)
      PELM(7)=                '  _______________________________'
      if(atype(1:1).eq.'C'.or.atype(1:1).eq.'c')then
        write(PELM(8) ,'(2a)') kact(act,'e'),' type: Composite'
      elseif(atype(1:1).eq.'M'.or.atype(1:1).eq.'m')then
        write(PELM(8) ,'(2a)')  kact(act,'e'),' type: Material'
      endif
C SC I have just modified the display of the unit
C       write(PELM(10) ,'(3a)') kact(act,'f'),' unit :',aunit
      call prsunit(idbh,3,ustr,unote)
      IFB=igendbf(idbh)
      call scandbhd(IFB,idbh,ier)

       write(PELM(9) ,'(5a)') kact(act,'f'),' unit :',
     &                   '[-/', ustr(1:lnblnk(ustr)),']'
      PELM(10)=               '  __abs coef @ 17 frequencies____ '
      write(PELM(11),'(a,4f7.3)') kact(act,'g'),frq(1),frq(2),frq(3),
     &  frq(4)
      write(PELM(12),'(a,4f7.3)') kact(act,'h'),frq(5),frq(6),frq(7),
     &  frq(8)
      write(PELM(13),'(a,4f7.3)') kact(act,'i'),frq(9),frq(10),frq(11),
     &  frq(12)
      write(PELM(14),'(a,3f7.3)') kact(act,'j'),frq(13),frq(14),frq(15)
      write(PELM(15),'(a,2f7.3)') kact(act,'k'),frq(16),frq(17)
      PELM(16)=           ' _________________________________'
      PELM(17)=           '? Help                            '
      PELM(18)=           '- Exit                            '
      MPLEM=18

C Help text for this menu.
   2  helptopic='dbm_acoustic_edit'
      call gethelptext(helpinsub,helptopic,nbhelp)

      write(head,'(2a)') '  Item in ',dbmenu(idbh)(1:27)

      CALL EMENU(head,PELM,MPLEM,IW)

      IF(IW.EQ.0.OR.IW.EQ.MPLEM)THEN
        RETURN
      ELSEIF(IW.EQ.2.or.IW.eq.3.or.IW.eq.5.or.IW.eq.6)THEN
        iract=0
        call EITCOMMON(IFB,idbh,ic,iit,IW,ACT,iract,primod,IER)
        if(iract.eq.3)goto 3
        if(iract.eq.9)goto 9
      ELSEIF(IW.EQ.8)THEN

C Acoustic Type.
        if(ACT(1:1).eq.'M')then
          CALL EASKABC(' Acoustic coef related to:',' ',
     &      'composite construction','material','cancel',irpt,nbhelp)
C SC The buttons where invert
          if(irpt.eq.1)atype='C   '
          if(irpt.eq.2)atype='M   '
          primod=.true.
        endif
      ELSEIF(IW.EQ.9)THEN

C Units.
        call edisp(iuout,'<< acoustic units here... >>')
      ELSEIF(IW.EQ.11)THEN

C Coef for frequencies 100, 125, 160.
        if(ACT(1:1).eq.'M')then
          write(hold,'(4f7.3)') frq(1),frq(2),frq(3),frq(4)
          CALL EASKS(hold,'Coefficients @ frequencies 100 125 160 200:',
     &      ' ',32,'0.00 0.00 0.00 0.00','frq(1-4)',IER,nbhelp)
          if(ier.eq.0.and.hold(1:2).ne.'  ')then
            k=0
            CALL EGETWR(hold,K,FRQ(1),0.00,10.0,'W','frq co',IER)
            CALL EGETWR(hold,K,FRQ(2),0.00,10.0,'W','frq co',IER)
            CALL EGETWR(hold,K,FRQ(3),0.00,10.0,'W','frq co',IER)
            CALL EGETWR(hold,K,FRQ(4),0.00,10.0,'W','frq co',IER)
            primod=.true.
          endif
          call usrmsg(' ',' ','-')
        else
          call usrmsg('Currently in browse mode.',' ','P')
        endif
      ELSEIF(IW.EQ.12)THEN

C Coef for frequencies 250, 315, 400, 500.
        if(ACT(1:1).eq.'M')then
          write(hold,'(4f7.3)') frq(5),frq(6),frq(7),frq(8)
          CALL EASKS(hold,
     &      'Coefficients for frequencies 250 315 400 500: ',
     &      ' ',32,'0.00 0.00 0.00 0.00','frq(5-8)',IER,nbhelp)
          if(ier.eq.0.and.hold(1:2).ne.'  ')then
            k=0
            CALL EGETWR(hold,K,FRQ(5),0.00,10.0,'W','frq co',IER)
            CALL EGETWR(hold,K,FRQ(6),0.00,10.0,'W','frq co',IER)
            CALL EGETWR(hold,K,FRQ(7),0.00,10.0,'W','frq co',IER)
            CALL EGETWR(hold,K,FRQ(8),0.00,10.0,'W','frq co',IER)
            primod=.true.
          endif
          call usrmsg(' ',' ','-')
        else
          call usrmsg('Currently in browse mode.',' ','P')
        endif
      ELSEIF(IW.EQ.13)THEN

C Coef for frequencies 630, 800, 1000, 1250
        if(ACT(1:1).eq.'M')then
          write(hold,'(4f7.3)') frq(9),frq(10),frq(11),frq(12)
          CALL EASKS(hold,
     &      'Coefficients for frequencies 630 800 1000 1250: ',
     &      ' ',32,'0.00 0.00 0.00 0.00','frq(9-12)',IER,nbhelp)
          if(ier.eq.0.and.hold(1:2).ne.'  ')then
            k=0
            CALL EGETWR(hold,K,FRQ(9),0.00,10.0,'W','frq co',IER)
            CALL EGETWR(hold,K,FRQ(10),0.00,10.0,'W','frq co',IER)
            CALL EGETWR(hold,K,FRQ(11),0.00,10.0,'W','frq co',IER)
            CALL EGETWR(hold,K,FRQ(12),0.00,10.0,'W','frq co',IER)
            primod=.true.
          endif
          call usrmsg(' ',' ','-')
        else
          call usrmsg('Currently in browse mode.',' ','P')
        endif
      ELSEIF(IW.EQ.14)THEN

C Coef for frequencies 1600, 2000, 2500.
        if(ACT(1:1).eq.'M')then
          write(hold,'(3f7.3)') frq(13),frq(14),frq(15)
          CALL EASKS(hold,
     &      'Coefficients for frequencies 1600, 2000, 2500: ',
     &      ' ',32,'0.00 0.00 0.00','frq(13-15)',IER,nbhelp)
          if(ier.eq.0.and.hold(1:2).ne.'  ')then
            k=0
            CALL EGETWR(hold,K,FRQ(13),0.00,10.0,'W','frq co',IER)
            CALL EGETWR(hold,K,FRQ(14),0.00,10.0,'W','frq co',IER)
            CALL EGETWR(hold,K,FRQ(15),0.00,10.0,'W','frq co',IER)
            primod=.true.
          endif
          call usrmsg(' ',' ','-')
        else
          call usrmsg('Currently in browse mode.',' ','P')
        endif
      ELSEIF(IW.EQ.15)THEN

C Coef for frequencies 3150, 4000.
        if(ACT(1:1).eq.'M')then
          write(hold,'(2f7.3)') frq(16),frq(17)
          CALL EASKS(hold,'Coefficients for frequencies 3150, 4000: ',
     &      ' ',32,'0.00 0.00','frq(16-17)',IER,nbhelp)
          if(ier.eq.0.and.hold(1:2).ne.'  ')then
            k=0
            CALL EGETWR(hold,K,FRQ(16),0.00,10.0,'W','frq co',IER)
            CALL EGETWR(hold,K,FRQ(17),0.00,10.0,'W','frq co',IER)
            primod=.true.
          endif
          call usrmsg(' ',' ','-')
        else
          call usrmsg('Currently in browse mode.',' ','P')
        endif
      ELSEIF(IW.EQ.17)THEN

C Help.
        helptopic='dbm_acoustic_edit'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('Room acoustic item edit help',nbhelp,'-',0,0,IER)
      ELSE
        IW=-1
        GOTO 2
      ENDIF
      GOTO 3

    1 call edisp(iuout,'Error updating data record')
      call edisp(iuout,outstr)
      return

      END

C ************* EDCOLOUR
C EDCOLOUR: Colour item display and editing.  If ACT='T' then produce
C a tabular listing and return,, if ACT='H' present tabular listing
C as a pop-up, otherwise present editing menu.
C Assumes that current database is open on unit IFB.
      subroutine edcolour(IFB,idbh,ic,iit,ACT,IER)
#include "espriou.h"
#include "esprdbfile.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN

C Generic db commons:
      common/gendb1/idbwidth(MGDB),lastr(MGDB),nbdbdoc(MGDB),
     &              nbunit(MGDB),nbsrc(MGDB),nbcat(MGDB),nbitem(MGDB),
     &              idbver(MGDB)      
      common/gendb2/dbtag(MGDB),dbmenu(MGDB),dbdate(MGDB),dbnote(MGDB),
     &              dbdoc(10)
      common/gendbu/unitchr(24),unitstr(24),unitnote(24)
      common/gendb3/irscat(MGCL),nbcatdoc(MGCL),nbcatit(MGCL)
      common/gendb4/cattag(MGCL),cattype(MGCL),catmenu(MGCL),
     &              catdate(MGCL),catdoc(5)

      common/gendb6/ittag(MGIT),itmenu(MGIT)
      common/gendb8a/ituntstr,itsrcstr,itcnfstr,itdate(MGIT),itdoc
      common/gendbtg/showtags

      dimension PELM(15)

      CHARACTER OUTSTR*124,PELM*38,ACT*1,dstmp*24
      character dbtag*12,dbmenu*32,dbdate*24,dbdoc*248,catdoc*248
      character cattag*12,cattype*8,catmenu*32,catdate*24
      character ittag*12,itmenu*32,itdate*24,itdoc*248
      character dbnote*72,head*38
      character ituntstr*42,itsrcstr*1,itcnfstr*42
      character unitchr*4,unitstr*12,unitnote*72,ustr*12,unote*72
      character coltype*12,colptrn*72,hold*32,kact*1
      character snote*248,outs*124,outs248*248
      logical primod,showtags
      integer MPLEM,IW  ! max items and current menu item

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='edcolour'  ! set for subroutine

C Scan the data from the common blocks.
    9 primod=.false.
      write(currentfile,'(a)')  gendbfn(idbh)(1:lnblnk(gendbfn(idbh)))
      call scandbhd(IFB,idbh,ier)
      call rgenitem(IFB,idbh,iit,ier)
      call PRSCOLR(idbh,iit,'g',IR255,IG255,IB255,greyrefl,coltype,
     &  colptrn,ier)
      call prssrc(idbh,snote)

C If user has request a report only display the relevant table.
      if(ACT(1:1).eq.'T'.or.ACT(1:1).eq.'H')then
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

        call prsunit(idbh,4,ustr,unote)
        write(outs,'(a,3i4,a,f7.4,1x,a)') ' red green blue:',
     &    IR255,IG255,IB255,' (0-255) grey reflect: ',greyrefl,ustr
        call edisp(iuout,outs)
        write(outs,'(4a)') ' extension: ',coltype(1:lnblnk(coltype)),
     &    ' pattern: ',colptrn
        call edisp(iuout,outs)
        call edisp(iuout,' ')

C After parsing units recover the header of the current db.
        IFB=igendbf(idbh)
        call scandbhd(IFB,idbh,ier)
        return
      endif

C If anything has changed it needs to be written out. First
C remove spaces in data lines(s).
   3  if(primod.and.ACT(1:1).eq.'M')then
        call usrmsg('Updating item data... ',' ','-')
        call PRSCOLR(idbh,iit,'p',IR255,IG255,IB255,greyrefl,coltype,
     &    colptrn,ier)

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
      write(PELM(3),'(3A)') kact(act,'b'),' name: ',
     &  itmenu(iit)(1:30)
      write(PELM(4),'(2A)')    '  date: ',itdate(iit)(1:24)
      write(PELM(5),'(3a)') kact(act,'c'),' source: ',snote(1:27)
      write(PELM(6),'(3a)') kact(act,'d'),' notes: ',itdoc(1:24)
      PELM(7)=                    '  ____________________________ '
      write(PELM(8) ,'(2a,3i4,1x,a)') kact(act,'e'),' red green blue:',
     &  IR255,IG255,IB255,'(0-255)'
      call prsunit(idbh,4,ustr,unote)
      write(PELM(9),'(2a,f7.4,1x,a)')kact(act,'f'),' grey reflect  :',
     &  greyrefl,ustr
      call prsunit(idbh,5,ustr,unote)
      write(PELM(10),'(3a)') kact(act,'f'),' extension: ',coltype
      call prsunit(idbh,6,ustr,unote)
      write(PELM(11),'(3a)') kact(act,'g'),' pattern: ',colptrn(1:26)
      IFB=igendbf(idbh)
      call scandbhd(IFB,idbh,ier)
      PELM(12)=           ' ______________________________ '
      PELM(13)=           '? help                          '
      PELM(14)=           '- exit                          '
      MPLEM=14

C Help text for this menu.
   2  helptopic='dbm_edit_colour'
      call gethelptext(helpinsub,helptopic,nbhelp)

      write(head,'(2a)') '  Item in ',dbmenu(idbh)(1:27)

      CALL EMENU(head,PELM,MPLEM,IW)

      IF(IW.EQ.0.OR.IW.EQ.MPLEM)THEN
        RETURN
      ELSEIF(IW.EQ.MPLEM-1)THEN

C Help.
        helptopic='dbm_edit_colour'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('material item edit help',nbhelp,'-',0,0,IER)
      ELSEIF(IW.EQ.2.or.IW.eq.3.or.IW.eq.5.or.IW.eq.6)THEN
        iract=0
        call EITCOMMON(IFB,idbh,ic,iit,IW,ACT,iract,primod,IER)
        if(iract.eq.3)goto 3
        if(iract.eq.9)goto 9
      ELSEIF(IW.EQ.8)THEN

C Red (if edited update grey).
        if(ACT(1:1).eq.'M')then
          write(hold,'(3i6)') IR255,IG255,IB255
  42      CALL EASKS(hold,' Red green blue (0-255): ',
     &      ' ',32,' 128 128 128','rgb values',IER,nbhelp)
          if(ier.eq.0.and.hold(1:2).ne.'  ')then
            k=0
            CALL EGETWI(hold,K,IR255,0,255,'W','red',IER)
            CALL EGETWI(hold,K,IG255,0,255,'W','green',IER)
            CALL EGETWI(hold,K,IB255,0,255,'W','blue',IER)
            primod=.true.
            greyrefl=((real(IR255)/255.0)*0.256)+
     &               ((real(IG255)/255)*0.67)+
     &               ((real(IB255)/255)*0.0645)
            call usrmsg(' ',' ','-')
          else
            goto 42
          endif
        else
          call usrmsg('Currently in browse mode.',' ','P')
        endif
      ELSEIF(IW.EQ.9)THEN

C Grey reflect.
        if(ACT(1:1).eq.'M')then
          CALL EASKR(greyrefl,' Grey reflectance (usually derived from',
     &      'the RGB values): ',0.0,'W',1.,'W',0.5,'grey refl',
     &      IER,nbhelp)
          primod=.true.
          call usrmsg(' ',' ','-')
        else
          call usrmsg('Currently in browse mode.',' ','P')
        endif
      ELSEIF(IW.EQ.10)THEN

C Extension type.
        if(ACT(1:1).eq.'M')then
          call usrmsg('<< select extension type >>',' ','P')
        else
          call usrmsg('Currently in browse mode.',' ','P')
        endif
      ELSEIF(IW.EQ.11)THEN

C Extension pattern or file name.
        if(ACT(1:1).eq.'M')then
          call usrmsg('<< select pattern name or file >>',' ','P')
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

C ************* EDTRANSP
C EDTRANSP: Transport item display and editing.  If ACT='T' then produce
C a tabular listing and return, if ACT='H' present tabular listing
C as a pop-up, otherwise present editing menu.
C Assumes that current database is open on unit IFB.
      subroutine edtransp(IFB,idbh,ic,iit,ACT,IER)
#include "espriou.h"
#include "esprdbfile.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN

C Generic db commons:
      common/gendb1/idbwidth(MGDB),lastr(MGDB),nbdbdoc(MGDB),
     &              nbunit(MGDB),nbsrc(MGDB),nbcat(MGDB),nbitem(MGDB),
     &              idbver(MGDB)      
      common/gendb2/dbtag(MGDB),dbmenu(MGDB),dbdate(MGDB),dbnote(MGDB),
     &              dbdoc(10)
      common/gendb3/irscat(MGCL),nbcatdoc(MGCL),nbcatit(MGCL)
      common/gendb4/cattag(MGCL),cattype(MGCL),catmenu(MGCL),
     &              catdate(MGCL),catdoc(5)

      common/gendb6/ittag(MGIT),itmenu(MGIT)
      common/gendb8a/ituntstr,itsrcstr,itcnfstr,itdate(MGIT),itdoc
      common/gendb8b/itdat(20),itrep(20)
      common/gendbtg/showtags

      dimension PELM(21),eimp(9)

      CHARACTER OUTSTR*124,PELM*38,ACT*1,dstmp*24
      character dbtag*12,dbmenu*32,dbdate*24,dbdoc*248,catdoc*248
      character cattag*12,cattype*8,catmenu*32,catdate*24
      character ittag*12,itmenu*32,itdate*24,itdoc*248
      character dbnote*72,ituntstr*42,itsrcstr*1,itcnfstr*42
      character ustr*12,unote*72
      character itdat*248,itrep*248,kact*1,head*38
      character snote*248,outs*124,outs248*248
      logical primod,showtags

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='edtransp'  ! set for subroutine

C Scan the data from the common blocks.
    9 primod=.false.
      write(currentfile,'(a)')  gendbfn(idbh)(1:lnblnk(gendbfn(idbh)))
      call scandbhd(IFB,idbh,ier)
      call rgenitem(IFB,idbh,iit,ier)
      call prstransp(idbh,iit,'g',eimp,ier)
      call prssrc(idbh,snote)
      GWP=eimp(1)
      AP=eimp(2)
      PS=eimp(3)
      UNRE=eimp(4)

C If user has request a report only display the relevant table.
      if(ACT(1:1).eq.'T'.or.ACT(1:1).eq.'H')then
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

        call prsunit(idbh,1,ustr,unote)
        write(outs,'(a,1Pe10.3,1x,a)') 
     &    ' Global Warming potential:',GWP,ustr(1:lnblnk(ustr))
        call edisp(iuout,outs)
        call prsunit(idbh,2,ustr,unote)
        write(outs,'(a,1Pe10.3,1x,a)') 
     &    ' Acidification potential :',AP,ustr(1:lnblnk(ustr))
        call edisp(iuout,outs)
        call prsunit(idbh,3,ustr,unote)
        write(outs,'(a,1Pe10.3,1x,a)') 
     &    ' PhotoSmog depletion :',PS,ustr(1:lnblnk(ustr))
        call edisp(iuout,outs)
        call prsunit(idbh,4,ustr,unote)
        write(outs,'(a,1Pe10.3,1x,a)') 
     &    ' Nonrenewable energy:',UNRE,ustr(1:lnblnk(ustr))
        call edisp(iuout,outs)
        IFB=igendbf(idbh)
        call scandbhd(IFB,idbh,ier)
        return
      endif

C If anything has changed it needs to be written out. First
C remove spaces in data lines(s). Recover categ data.
   3  if(primod.and.ACT(1:1).eq.'M')then
        call usrmsg('Updating item data... ',' ','-')
        eimp(1)=GWP
        eimp(2)=AP
        eimp(3)=PS
        eimp(4)=UNRE
        call prstransp(idbh,iit,'p',eimp,ier)
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
     &    catmenu(ic),nbcatdoc,nbcatit(ic),catdate(ic)
        call wgenitem(IFB,idbh,iit,ier)
        primod=.false.
        call usrmsg('Updating item data...done.',' ','P')
      endif

C Make up the display.
      IW=-4
      write(PELM(1),'(2A)')    '  category: ',catmenu(ic)(1:26)
      write(PELM(2),'(3A)') kact(act,'a'),' tag: ',ittag(iit)
      write(PELM(3),'(3A)') kact(act,'b'),' name: ',
     &  itmenu(iit)(1:30)
      write(PELM(4),'(2A)')    '  date: ',itdate(iit)(1:24)
      write(PELM(5),'(3a)') kact(act,'c'),' source: ',snote(1:27)
      write(PELM(6),'(3a)') kact(act,'d'),' notes: ',itdoc(1:24)
      PELM(7)=                    '  ____________________________ '
      call prsunit(idbh,1,ustr,unote)
      write(PELM(8) ,'(2a,1Pe10.3,1x,a)') kact(act,'e'),
     &  ' global warm :',GWP,ustr(1:lnblnk(ustr))
      call prsunit(idbh,2,ustr,unote)
      write(PELM(9) ,'(2a,1Pe10.3,1x,a)') kact(act,'f'),
     &  ' acidificatin:',AP,ustr(1:lnblnk(ustr))
      call prsunit(idbh,3,ustr,unote)
      write(PELM(10),'(2a,1Pe10.3,1x,a)') kact(act,'g'),
     &  ' photoSmog   :',PS,ustr(1:lnblnk(ustr))
      call prsunit(idbh,4,ustr,unote)
      write(PELM(11),'(2a,1Pe10.3,1x,a)') kact(act,'h'),
     &  ' nonrn energy:',UNRE,ustr(1:lnblnk(ustr))
      IFB=igendbf(idbh)
      call scandbhd(IFB,idbh,ier)
      PELM(12)=           ' ______________________________ '
      PELM(13)=           '? help                          '
      PELM(14)=           '- exit                          '
      MPLEM=14

C Help text for this menu.
   2  helptopic='dbm_edit_transport'
      call gethelptext(helpinsub,helptopic,nbhelp)

      call usrmsg(' ',' ','-')
      write(head,'(2a)') '  Item in ',dbmenu(idbh)(1:27)

      CALL EMENU(head,PELM,MPLEM,IW)

      IF(IW.EQ.0.OR.IW.EQ.MPLEM)THEN
        RETURN
      ELSEIF(IW.EQ.2.or.IW.eq.3.or.IW.eq.5.or.IW.eq.6)THEN
        iract=0
        call EITCOMMON(IFB,idbh,ic,iit,IW,ACT,iract,primod,IER)
        if(iract.eq.3)goto 3
        if(iract.eq.9)goto 9
      ELSEIF(IW.EQ.8.or.IW.eq.9.or.IW.eq.10.or.IW.eq.11)THEN

C Edit the impacts. 
        if(ACT(1:1).eq.'M')then
          call edimpact(GWP,AP,PS,UNRE,ier)
          primod=.true.
        else
          call usrmsg('Currently in browse mode.',' ','P')
        endif
      ELSEIF(IW.EQ.13)THEN

C Help.
        helptopic='dbm_edit_transport'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('transport item edit help',nbhelp,'-',0,0,IER)
      ELSE
        IW=-1
        GOTO 2
      ENDIF
      GOTO 3

    1 call edisp(iuout,'Error updating data record')
      call edisp(iuout,outstr)
      return

      END
      
      
C ************* BKITEM
C BKITEM: Put current item in alternative common block structure for later
C recovery.
      SUBROUTINE BKITEM(idbh,class,item,ic,iit)
#include "esprdbfile.h" 

      common/gendb6/ittag(MGIT),itmenu(MGIT)
      common/bgendb6/bittag,bitmenu
      common/gendb8/irsit(MGIT),itcat(MGIT)
      common/bgendb8/bitcat
      common/gendb8a/ituntstr,itsrcstr,itcnfstr,itdate(MGIT),itdoc
      common/bgendb8a/bituntstr,bitsrcstr,bitcnfstr,bitdate,bitdoc
      common/gendb8b/itdat(20),itrep(20)
      common/bgendb8b/bitdat(20),bitrep(20)
      common/gendb8c/nbitusc(MGIT),nbitdat(MGIT),
     &               nbitrep(MGIT),ibitusc(MGIT)
      common/bgendb8c/bnbitusc,bnbitdat,bnbitrep,bibitusc

C Backing item common.
      common/gendbb/bic,bclass,biit,bitem

      character itdat*248,itrep*248
      character bitdat*248,bitrep*248
      character ittag*12,itmenu*32,itdate*24,itdoc*248
      character class*12,bclass*12,item*12,bitem*12
      character bittag*12,bitmenu*32,bitdate*24,bitdoc*248
      character ituntstr*42,itsrcstr*1,itcnfstr*42
      character bituntstr*42,bitsrcstr*1,bitcnfstr*42
      integer bnbitusc,bnbitdat,bnbitrep,bic,biit,bitcat
      integer bibitusc

      bic=ic
      bclass=class
      biit=iit
      bitem=item
      bittag=ittag(iit)
      bitmenu=itmenu(iit)
      bitcat=itcat(iit)
      bituntstr=ituntstr
      bitsrcstr=itsrcstr
      bitcnfstr=itcnfstr
      bitdate=itdate(iit)
      bnbitusc=nbitusc(iit)
      bibitusc=ibitusc(iit)
      bnbitdat=nbitdat(iit)
      bnbitrep=nbitrep(iit)
      bitdoc=itdoc
      do 137 m=1,nbitdat(iit)
        bitdat(m)=itdat(m)
 137   continue
      if(nbitrep(iit).gt.0)then
        do 138 n=1,nbitrep(iit)
          bitrep(n)=itrep(n)
 138    continue
      endif
      RETURN
      end
      
C ************* RCVITEM
C RCVITEM: Recover item from alternative common block structure.
      SUBROUTINE RCVITEM(idbh,class,item,ic,iit)
#include "esprdbfile.h"

      common/gendb6/ittag(MGIT),itmenu(MGIT)
      common/bgendb6/bittag,bitmenu
      common/gendb8/irsit(MGIT),itcat(MGIT)
      common/bgendb8/bitcat
      common/gendb8a/ituntstr,itsrcstr,itcnfstr,itdate(MGIT),itdoc
      common/bgendb8a/bituntstr,bitsrcstr,bitcnfstr,bitdate,bitdoc
      common/gendb8b/itdat(20),itrep(20)
      common/bgendb8b/bitdat(20),bitrep(20)
      common/gendb8c/nbitusc(MGIT),nbitdat(MGIT),
     &               nbitrep(MGIT),ibitusc(MGIT)
      common/bgendb8c/bnbitusc,bnbitdat,bnbitrep,bibitusc

C Backing item common.
      common/gendbb/bic,bclass,biit,bitem

      character itdat*248,itrep*248
      character bitdat*248,bitrep*248
      character ittag*12,itmenu*32,itdate*24,itdoc*248
      character class*12,bclass*12,item*12,bitem*12
      character bittag*12,bitmenu*32,bitdate*24,bitdoc*248
      character ituntstr*42,itsrcstr*1,itcnfstr*42
      character bituntstr*42,bitsrcstr*1,bitcnfstr*42
      integer bnbitusc,bnbitdat,bnbitrep,bic,biit,bitcat
      integer bibitusc

      ic=bic
      class=bclass
      iit=biit
      item=bitem
      ittag(iit)=bittag
      itmenu(iit)=bitmenu
      itcat(iit)=bitcat
      ituntstr=bituntstr
      itsrcstr=bitsrcstr
      itcnfstr=bitcnfstr
      itdate(iit)=bitdate
      nbitusc(iit)=bnbitusc
      ibitusc(iit)=bibitusc
      nbitdat(iit)=bnbitdat
      nbitrep(iit)=bnbitrep
      itdoc=bitdoc
      do 137 m=1,nbitdat(iit)
        itdat(m)=bitdat(m)
 137   continue
      if(nbitrep(iit).gt.0)then
        do 138 n=1,nbitrep(iit)
          itrep(n)=bitrep(n)
 138    continue
      endif
      RETURN
      end
      
C ************* CPITEM
C CPITEM: Copy item iitorig to catg iccopy returning index of copied item as iitcopy. 
      SUBROUTINE CPITEM(IFB,idbh,icorig,iitorig,iccopy,iitcopy,act,ier)
#include "esprdbfile.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      common/gendb1/idbwidth(MGDB),lastr(MGDB),nbdbdoc(MGDB),
     &              nbunit(MGDB),nbsrc(MGDB),nbcat(MGDB),nbitem(MGDB),
     &              idbver(MGDB)      
      common/gendb2/dbtag(MGDB),dbmenu(MGDB),dbdate(MGDB),dbnote(MGDB),
     &              dbdoc(10)
      common/gendb3/irscat(MGCL),nbcatdoc(MGCL),nbcatit(MGCL)
      common/gendb4/cattag(MGCL),cattype(MGCL),catmenu(MGCL),
     &              catdate(MGCL),catdoc(5)
      common/gendb6/ittag(MGIT),itmenu(MGIT)
      common/gendb8/irsit(MGIT),itcat(MGIT)
      common/gendb8a/ituntstr,itsrcstr,itcnfstr,itdate(MGIT),itdoc
      common/gendb8b/itdat(20),itrep(20)
      common/gendb8c/nbitusc(MGIT),nbitdat(MGIT),
     &               nbitrep(MGIT),ibitusc(MGIT)

      character item*12,itdat*248,itrep*248,dbnote*72
      character dbtag*12,dbmenu*32,dbdate*24,dbdoc*248,catdoc*248
      character cattag*12,cattype*8,catmenu*32,catdate*24,catcpdate*24
      character ittag*12,itmenu*32,itdate*24,itdoc*248
      character ituntstr*42,itsrcstr*1,itcnfstr*42,outs*124,outsd*124
      character c20*20,ptag*12,dstmp*24,act*1,itemtag*12
      real misc1,misc2

C Position of new item will be at lastr+1 so compute and updated lastr
C for the current type of database.
      if(nbitem(idbh)+1.gt.MGIT)then
        call usrmsg('Database is full, unable to add items.',' ','W')
        ier=1
        return
      endif

C Keep track of both category dates and documentation.
      IREC=irscat(iccopy)
      read(IFB,REC=IREC,IOSTAT=IOS,ERR=1)cattag(iccopy),
     &  cattype(iccopy),catmenu(iccopy),nbcpcatdoc,nbcatit(iccopy),
     &  catdate(iccopy)
      IREC=irscat(icorig)
      read(IFB,REC=IREC,IOSTAT=IOS,ERR=1)cattag(icorig),
     &  cattype(icorig),catmenu(icorig),nbcatdoc(icorig),
     &  nbcatit(icorig),catdate(icorig)

C Recover data related to item index iitorig.
      call rgenitem(IFB,idbh,iitorig,ier)
      if(ier.ne.0)then
        call usrmsg('Could not recover original item.',' ','W')
        ier=1
        return
      endif

      write(item,'(a)')ittag(iitorig)
      nbitem(idbh)=nbitem(idbh)+1
      iitcopy=nbitem(idbh)
      IREC=lastr(idbh)+1
      irsit(iitcopy)=IREC

C The increment in records in the file for this item is
C 2 for header, 3 for help, plus data and repeating records.
      nbrecdoc=3
      increm= 2+nbrecdoc+nbitdat(iitorig)+nbitrep(iitorig)
      lastr(idbh)=lastr(idbh)+increm
      if(act(1:1).eq.'n')then
        if(dbtag(idbh)(1:11).eq.'multicon_db')then
          ptag = itemtag('constr',iitcopy)
        elseif(dbtag(idbh)(1:8).eq.'material')then
          ptag = itemtag('mat',iitcopy)
        elseif(dbtag(idbh)(1:8).eq.'acoustic')then
          ptag = itemtag('acou',iitcopy)
        elseif(dbtag(idbh)(1:12).eq.'colourimetry')then
          ptag = itemtag('colour',iitcopy)
        elseif(dbtag(idbh)(1:9).eq.'envir_imp')then
          if(cattype(icorig)(1:7).eq.'CONSIMP')irpt=1
          if(cattype(icorig)(1:8).eq.'CNSTRASM')irpt=2
          if(cattype(icorig)(1:8).eq.'CNSTRMNT')irpt=3
          if(cattype(icorig)(1:6).eq.'ASMPRJ')irpt=4
          if(cattype(icorig)(1:6).eq.'MATLCA')irpt=5
          if(cattype(icorig)(1:6).eq.'RECYCP')irpt=6
          if(cattype(icorig)(1:6).eq.'INCENP')irpt=7
          if(cattype(icorig)(1:5).eq.'DUMPP')irpt=8
          if(irpt.eq.1)then
            ptag = itemtag('cnstrimp',iitcopy)
          elseif(irpt.eq.2)then
            ptag = itemtag('cnstrasmb',iitcopy)
          elseif(irpt.eq.3)then
            ptag = itemtag('cnstrmnt',iitcopy)
          elseif(irpt.eq.4)then
            ptag = itemtag('asmbprj',iitcopy)
          elseif(irpt.eq.5)then
            ptag = itemtag('matlca',iitcopy)
          elseif(irpt.eq.6)then
            ptag = itemtag('recyc',iitcopy)
          elseif(irpt.eq.7)then
            ptag = itemtag('incin',iitcopy)
          elseif(irpt.eq.8)then
            ptag = itemtag('dump',iitcopy)
          endif
        elseif(dbtag(idbh)(1:9).eq.'transport')then
          ptag = itemtag('transp',iitcopy)
        elseif(dbtag(idbh)(1:9).eq.'pressc_db')then
          ptag = itemtag('pressc',iitcopy)
        elseif(dbtag(idbh)(1:8).eq.'profiles')then
          ptag = itemtag('profile',iitcopy)
        else
          call usrmsg('Exiting: database type not recognised.',' ','W')
          ier=1
          return
        endif

        write(ittag(iitcopy),'(a)') ptag
        write(itmenu(iitcopy),'(3a)') ptag(1:lnblnk(ptag)),
     &    ' copy ',item(1:lnblnk(item))
      elseif(act(1:1).eq.'r')then
        ittag(iitcopy)=ittag(iitorig)
        itmenu(iitcopy)=itmenu(iitorig)
      endif
      call dstamp(dstmp)
      write(dbdate(idbh),'(a)') dstmp
      write(catcpdate,'(a)') dstmp
      write(itdate(iitcopy),'(a)') dstmp

      itcat(iitcopy)=iccopy

C Update the database pointers to items, update db and category dates
C and number of db items and catg associated items, then write out the new
C item data. 
      call updirsit(IFB,idbh,'W')
      c20='*ESP-r binary db    '
      irec=1
      write(IFB,REC=IREC,IOSTAT=IOS,ERR=1)c20,dbtag(idbh),dbmenu(idbh),
     &  idbwidth(idbh),nbunit(idbh),nbsrc(idbh),nbcat(idbh),
     &  nbitem(idbh),nbdbdoc(idbh),lastr(idbh)
      IREC=2
      write(IFB,REC=IREC,IOSTAT=IOS,ERR=1)dbdate(idbh),dbnote(idbh)
      nbcatit(iccopy)=nbcatit(iccopy)+1
      IREC=irscat(iccopy)
      write(IFB,REC=IREC,IOSTAT=IOS,ERR=1)cattag(iccopy),
     &  cattype(iccopy),catmenu(iccopy),nbcpcatdoc,
     &  nbcatit(iccopy),catcpdate
      IREC=irscat(iccopy)
      call wgenitem(IFB,idbh,iitcopy,ier)
      RETURN

C Error trap.
   1  write(outs,'(a,I6,a,i3)') 'CPITEM: Error reading record ',IREC,
     &  ' of file unit ',ifb
      call edisp(iuout,outs)
      CALL ERPFREE(IFB,IOS)
      ier=1
      return
      end
      
C ************* MVITEM
C MVITEM: Move item iitorig in icorig to catg icmove returning index of moved 
C item as iitmove. 
      SUBROUTINE MVITEM(IFB,idbh,icorig,iitorig,icmove,iitmove,ier)
#include "esprdbfile.h"

      COMMON/OUTIN/IUOUT,IUIN
      common/gendb1/idbwidth(MGDB),lastr(MGDB),nbdbdoc(MGDB),
     &              nbunit(MGDB),nbsrc(MGDB),nbcat(MGDB),nbitem(MGDB),
     &              idbver(MGDB)      
      common/gendb3/irscat(MGCL),nbcatdoc(MGCL),nbcatit(MGCL)
      common/gendb4/cattag(MGCL),cattype(MGCL),catmenu(MGCL),
     &              catdate(MGCL),catdoc(5)
      common/gendb6/ittag(MGIT),itmenu(MGIT)
      common/gendb8/irsit(MGIT),itcat(MGIT)
      common/gendb8a/ituntstr,itsrcstr,itcnfstr,itdate(MGIT),itdoc
      common/gendb8c/nbitusc(MGIT),nbitdat(MGIT),
     &               nbitrep(MGIT),ibitusc(MGIT)

      character catdoc*248,cattag*12,cattype*8,catmenu*32,catdate*24
      character ittag*12,itmenu*32,itdate*24,itdoc*248
      character ituntstr*42,itsrcstr*1,itcnfstr*42,outs*124
      character dstmp*24

C Position of new item will be at lastr+1 so compute and updated lastr
C for the current type of database (use call to CPITEM for this).
      if(nbitem(idbh)+1.gt.MGIT)then
        call usrmsg('Sorry, the database is full.',' ','W')
        ier=1
        return
      endif

C Copy the source item to the end of the database retaining its tag.
      call CPITEM(IFB,idbh,icorig,iitorig,icmove,iit,'r',ier)
      if(ier.eq.0)then
        iitmove=iit
      else
        call usrmsg('Could not copy original item.',' ','W')
        iitmove=0
        ier=1
        return
      endif

C Recover origin category document lines prior to updating.
      IREC=irscat(icorig)
      read(IFB,REC=IREC,IOSTAT=IOS,ERR=1)cattag(icorig),
     &  cattype(icorig),catmenu(icorig),nbcatdoc(icorig),
     &  nbcatit(icorig),catdate(icorig)

C Now decrement the associated items in original category and reset the
C original item category association to zero so that it will not be
C picked up in a subsequent reconfiguration of the database.
      call dstamp(dstmp)
      write(catdate(icorig),'(a)') dstmp
      nbcatit(icorig)=nbcatit(icorig)-1
      IREC=irscat(icorig)
      write(IFB,REC=IREC,IOSTAT=IOS,ERR=1)cattag(icorig),
     &  cattype(icorig),catmenu(icorig),nbcatdoc(icorig),
     &  nbcatit(icorig),catdate(icorig)

      itcat(iitorig)=0
      IREC=irsit(iitorig)
      if(idbver(idbh).eq.1)then
        nbitdoc=1
        write(IFB,REC=IREC,IOSTAT=IOS,ERR=1)ittag(iitorig),
     &    itmenu(iitorig),itdate(iitorig),itcat(iitorig),
     &    nbitdoc,nbitdat(iitorig),nbitrep(iitorig),
     &    nbitusc(iitorig),ibitusc(iitorig)
      else
        write(IFB,REC=IREC,IOSTAT=IOS,ERR=1)ittag(iitorig),
     &    itmenu(iitorig),itdate(iitorig),itcat(iitorig),
     &    nbitdat(iitorig),nbitrep(iitorig),
     &    nbitusc(iitorig),ibitusc(iitorig)
      endif
      call tagend(IFB,idbh,ier)

      RETURN

C Error trap.
   1  write(outs,'(a,I6,a,i3)') 'MVITEM: Error reading record ',IREC,
     &  ' of file unit ',ifb
      call edisp(iuout,outs)
      CALL ERPFREE(IFB,IOS)
      ier=1
      return
      end

C ***** choseitem
C Choseitem supports selection of an entity from a different database.
C It is passed the requested database tag, ACT='M' to list-select and
C ACT='-' to popup an item to see details. selitm is the name (12 char)
C of the item selected, itmsel is its index if found and icatsel is the
C category of the selected item. Curitem is the existing item name and
C curdb are the calling codes current database (to help filter the
C categories the user is offered to select from).
C Because it makes use of prsmenu() the calling code may need to issue
C a call scandbhd for the main database.
      subroutine choseitem(db,ACT,curitem,curdb,selitem,selmenu,itmsel,
     &  icatsel,IER)
#include "esprdbfile.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN

C Generic db commons:
      common/gendb1/idbwidth(MGDB),lastr(MGDB),nbdbdoc(MGDB),
     &              nbunit(MGDB),nbsrc(MGDB),nbcat(MGDB),nbitem(MGDB),
     &              idbver(MGDB)      
      common/gendb4/cattag(MGCL),cattype(MGCL),catmenu(MGCL),
     &              catdate(MGCL),catdoc(5)
      common/gendb6/ittag(MGIT),itmenu(MGIT)
      common/gendb8/irsit(MGIT),itcat(MGIT)

      character*(*) db,curdb
      CHARACTER ACT*1,ittag*12,itmenu*32,selitem*12,selmenu*32
      character curitem*12,menu*32,outs*124,headr*60,headc*60,ctype*8
      character catdoc*248,cattag*12,cattype*8,catmenu*32,catdate*24

      if(db(1:8).eq.'material')then
        isdbhash=2
        ISBFIL=igendbf(2)
        call prsmenu(isdbhash,curitem,menu,index,icurcat)
        headr='Please select a replacement material for '
        headc='The current material attribute is '
        ctype='ALL     '
      elseif(db(1:8).eq.'acoustic')then
        isdbhash=5
        ISBFIL=igendbf(5)
        call prsmenu(isdbhash,curitem,menu,index,icurcat)
        headr='Please select a replacement acoustic for '
        headc='The current acoustic attribute is '
        ctype='ALL     '
      elseif(db(1:12).eq.'colourimetry')then
        isdbhash=6
        ISBFIL=igendbf(6)
        call prsmenu(isdbhash,curitem,menu,index,icurcat)
        headr='Please select a replacement colour for '
        headc='The current colour attribute is '
        ctype='ALL     '
      elseif(db(1:9).eq.'envir_imp')then

C In the case im envir_imp, if being accessed from aother category
C within the envir_imp db then only certain categories should be
C displayed.
        isdbhash=8
        ISBFIL=igendbf(8)
        call prsmenu(isdbhash,curitem,menu,index,icurcat)

C Debug.
C        if(icurcat.ne.0)write(6,*) 'cur item cat type is ',
C     &    cattype(icurcat)
C        write(6,*) 'cur db is ',curdb

        if(curdb(1:8).eq.'material')then
          headr='Please select a replacement material effect for '
          headc='The current material effect attribute is '
          ctype='MATLCA  '
        elseif(curdb(1:11).eq.'multicon_db')then
          headr='Please select a replacement construction effect for '
          headc='The current construction effect attribute is '
          ctype='CONSIMP '
        else
          headr='Please select a replacement effect for '
          headc='The current effect attribute is '
          ctype='ALL     '
        endif
      elseif(db(1:9).eq.'transport')then
        isdbhash=9
        ISBFIL=igendbf(9)
        call prsmenu(isdbhash,curitem,menu,index,icurcat)
        headr='Please select a replacement transport for '
        headc='The current transport attribute is '
        ctype='ALL     '
      endif
      if(ACT(1:1).eq.'M')then
        write(outs,'(a,1x,2a)') headr(1:lnblnk(headr)),
     &    menu(1:lnblnk(menu)),'.'
        call usrmsg(outs,' ','-')
        call SELCAT(isbfil,isdbhash,ctype,itm,icl,IER)
        if(itm.ne.0)then
          selitem=ittag(itm)
          call prsmenu(isdbhash,selitem,selmenu,index,icx)
          itmsel=index
          icatsel=icl
          return
        endif
      else
        if(curitem(1:4).eq.'none')then
          call usrmsg('There is no attribute of this type ',
     &       'defined at this time.','P')
          selitem='-'
          selmenu='-'
          itmsel=0
          icatsel=0
          return
        endif
        do 38 itother=1,nbitem(isdbhash)
          if(curitem(1:12).eq.ittag(itother)(1:12))then
            write(outs,'(a,1x,2a)') headc(1:lnblnk(headc)),
     &        menu(1:lnblnk(menu)),'.'
            call usrmsg(outs,' ','-')
            icother=itcat(itother)
            if(db(1:8).eq.'material')then
              call edmat2(db,isbfil,isdbhash,icother,itother,'H',IER)
            elseif(db(1:8).eq.'acoustic')then
              call edacous(isbfil,isdbhash,icother,itother,'H',IER)
            elseif(db(1:12).eq.'colourimetry')then
              call edcolour(isbfil,isdbhash,icother,itother,'H',IER)
            elseif(db(1:9).eq.'envir_imp')then
              call epopenv(isbfil,isdbhash,icother,itother,'H',IER)
            elseif(db(1:9).eq.'transport')then
              call edtransp(isbfil,isdbhash,icother,itother,'H',IER)
            else

            endif
            selitem='-'
            selmenu='-'
            itmsel=0
            icatsel=0
            return
          endif
  38    continue
      endif
      return
      end

C ***** choseitct
C Choseitct supports selection of an entity from a specific category.
C It is passed the requested database tag, ACT='M' to list-select and
C ACT='-' to popup an item to see details. selitm is the name (12 char)
C of the item selected, itmsel is its index if found and icatsel is the
C category of the selected item. Curdb is the calling db type, ctype
C is the requested category to select from. 
      subroutine choseitct(db,ACT,curitem,curdb,ctype,selitem,selmenu,
     &  itmsel,icatsel,IER)
#include "esprdbfile.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN

C Generic db commons:
      common/gendb1/idbwidth(MGDB),lastr(MGDB),nbdbdoc(MGDB),
     &              nbunit(MGDB),nbsrc(MGDB),nbcat(MGDB),nbitem(MGDB),
     &              idbver(MGDB)      
      common/gendb4/cattag(MGCL),cattype(MGCL),catmenu(MGCL),
     &              catdate(MGCL),catdoc(5)
      common/gendb6/ittag(MGIT),itmenu(MGIT)
      common/gendb8/irsit(MGIT),itcat(MGIT)

      character*(*) db,curdb
      CHARACTER ACT*1,ittag*12,itmenu*32,selitem*12,selmenu*32
      character curitem*12,menu*32,outs*124,headr*60,headc*60,ctype*8
      character catdoc*248,cattag*12,cattype*8,catmenu*32,catdate*24

C Debug.
C      write(6,*) 'cur db is ',curdb
C      write(6,*) 'intended category is ',ctype

      if(db(1:8).eq.'material')then
        isdbhash=2
        ISBFIL=igendbf(2)
        headr='Please select a replacement material for '
        headc='The current material attribute is '
      elseif(db(1:8).eq.'acoustic')then
        isdbhash=5
        ISBFIL=igendbf(5)
        headr='Please select a replacement acoustic for '
        headc='The current acoustic attribute is '
      elseif(db(1:12).eq.'colourimetry')then
        isdbhash=6
        ISBFIL=igendbf(6)
        headr='Please select a replacement colour for '
        headc='The current colour attribute is '
      elseif(db(1:9).eq.'envir_imp')then

C In the case im envir_imp, if being accessed from aother category
C within the envir_imp db then only certain categories should be
C displayed.
        isdbhash=8
        ISBFIL=igendbf(8)
        call prsmenu(isdbhash,curitem,menu,index,icurcat)
        if(ctype.eq.'MATLCA  ')then
          headr='Please select a replacement material effect for '
          headc='The current material effect attribute is '
        elseif(ctype.eq.'CONSIMP ')then
          headr='Please select a replacement constructions effect for '
          headc='The current constructions effect attribute is '
        elseif(ctype.eq.'CNSTRASM')then
          headr='Please select a replacement assembly effect for '
          headc='The current assembly effect attribute is '
        elseif(ctype.eq.'CNSTRMNT')then
          headr='Please select a replacement maintenance effect for '
          headc='The current maintenance effect attribute is '
        elseif(ctype.eq.'ASMPRJ  ')then
          headr='Please select a replacement assembly effect for '
          headc='The current assembly effect attribute is '
        elseif(ctype.eq.'RECYCP  ')then
          headr='Please select a replacement recycling effect for '
          headc='The current recycling effect attribute is '
        elseif(ctype.eq.'INCENP  ')then
          headr='Please select a replacement incineration effect for '
          headc='The current incineration effect attribute is '
        elseif(ctype.eq.'DUMPP   ')then
          headr='Please select a replacement dump effect for '
          headc='The current dump effect attribute is '
        else
          headr='Please select a replacement effect for '
          headc='The current effect attribute is '
        endif
      elseif(db(1:9).eq.'transport')then
        isdbhash=9
        ISBFIL=igendbf(9)
        headr='Please select a replacement transport for '
        headc='The current transport attribute is '
      endif
      call prsmenu(isdbhash,curitem,menu,index,icurcat)
      if(ACT(1:1).eq.'M')then
        write(outs,'(a,1x,2a)') headr(1:lnblnk(headr)),
     &    menu(1:lnblnk(menu)),'.'
        call usrmsg(outs,' ','-')
        call SELCAT(isbfil,isdbhash,ctype,itm,icl,IER)
        if(itm.ne.0)then
          selitem=ittag(itm)
          call prsmenu(isdbhash,selitem,selmenu,index,icx)
          itmsel=index
          icatsel=icl
          return
        endif
      else
        if(curitem(1:4).eq.'none')then
          call usrmsg('There is no attribute of this type ',
     &       'defined at this time.','P')
          selitem='-'
          selmenu='-'
          itmsel=0
          icatsel=0
          return
        endif
        do 38 itother=1,nbitem(isdbhash)
          if(curitem(1:12).eq.ittag(itother)(1:12))then
            write(outs,'(a,1x,2a)') headc(1:lnblnk(headc)),
     &        menu(1:lnblnk(menu)),'.'
            call usrmsg(outs,' ','-')
            icother=itcat(itother)
            if(db(1:8).eq.'material')then
              call edmat2(db,isbfil,isdbhash,icother,itother,'H',IER)
            elseif(db(1:8).eq.'acoustic')then
              call edacous(isbfil,isdbhash,icother,itother,'H',IER)
            elseif(db(1:12).eq.'colourimetry')then
              call edcolour(isbfil,isdbhash,icother,itother,'H',IER)
            elseif(db(1:9).eq.'envir_imp')then
              call epopenv(isbfil,isdbhash,icother,itother,'H',IER)
            elseif(db(1:9).eq.'transport')then
              call edtransp(isbfil,isdbhash,icother,itother,'H',IER)
            else

            endif
            selitem='-'
            selmenu='-'
            itmsel=0
            icatsel=0
            return
          endif
  38    continue
      endif
      return
      end
