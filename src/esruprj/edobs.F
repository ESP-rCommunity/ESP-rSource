C This file is part of the ESP-r system.
C Copyright Energy Systems Research Unit, University of
C Strathclyde, Glasgow Scotland, 2001-2011.

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

C edobs.f contains code to allow the editing of site obstructions files.

C EDOBS:  High level controller for editing obstruction blocks
C ECROBS: Read in existing obstruction file or create a fresh one.
C EDOBSB: Edit details of an obstruction block.
C EREVEAL: Composes a window reveal out of obstruction blocks.

C << NOTE: transition state in terms of obstruction blocks within v1.1
C << zone geometry files - several routines are using readgeo calls
C << to keep the data refreshed so geowrite calls can be made.

C ************* EDOBS 
C EDOBS: Edit obstruction blocks if there is an obstruction file
C associated with this zone or if there are obstructions held
C in the version 1.1 zone geometry file. If older geometry file
C then use separate obstructions file and allow user to create one.
C Use file unit IPRODB temporarily.
      SUBROUTINE EDOBS(ITRC,ITRU,ICOMP,IER)
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "epara.h"
#include "esprdbfile.h"
C esprdbfile.h supplies the following:
C LPRFDB,IPRODB (for event profile database)
#include "material.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN

      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)

      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)
      COMMON/RAY2/ITDSP,ITBND,ITEPT,ITZNM,ITSNM,ITVNO,ITORG,ITSNR,
     &            ITOBS,ITHLS,ITHLZ,ITGRD,GRDIS,ITPPSW
      COMMON/RAY3/MODIFY,MODLEN,MODBND

      dimension IVALB(MB)
      CHARACTER*33 VERT(35)
      character*32 PICK(MB)
      CHARACTER KEY*1,HOLD*24
      character blkroot*8,mat*12,tbn*12
      character holds*36
      character outs*124
      LOGICAL MODIFY,MODLEN,MODBND,modcfg,unixok
      logical newgeo  ! to use for testing if new/old geometry file.
      logical silent  ! to signal quiet dependency resolution
      logical anothercopy  ! logical true if user has copied a block
      integer llbm,llbn  ! for length of block material and name
      logical havesaved  ! to signal zone file written
      integer IW  ! for radio button
      integer MVERT,IVERT ! max items and current menu item
      real angr   ! rotation angle

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='EDOBS'  ! set for subroutine

C Check if Unix-based or DOS based.
      call isunix(unixok)

      modcfg=.false.
      havesaved=.false.
      ITOBS = 0
      anothercopy=.false.  ! nothing copied yet

C Obstructions for whole model are in common blocks there should
C be no need to re-scan this information because it is only called
C from within subroutine X of edgeo.F.

C If newer geometry file it may include obstructions. If the value
C of iobs for this zone is 2 then obstructions were included so
C re-read the geometry file.
      IUF=IPRODB
      newgeo=.false.  ! assume older format geometry.
      call eclose(gversion(icomp),1.1,0.01,newgeo)
      if(newgeo)then
        if(IOBS(ICOMP).EQ.2)then
          continue
        elseif(IOBS(ICOMP).EQ.1)then

C There was an existing zone obstructions file and the obstructions
C should be transferred into the zone geometry file. Note: ecrobs
C takes care of updating the model cfg file.
          silent=.false.
          CALL ECROBS(ITRC,ITRU,IUF,ICOMP,silent,IER)
        elseif(IOBS(ICOMP).EQ.0)then
          IOBS(ICOMP)=2
          MODIFY=.TRUE.
        endif
      else

C Check for obstructions. If file exists ask user to confirm
C it or dereference it. If it does not exist ask user for
C a name and or whether it should be ignored.
        IUF=IPRODB
        IF(IOBS(ICOMP).EQ.1)THEN
          silent=.false.
          CALL ECROBS(ITRC,ITRU,IUF,ICOMP,silent,IER)
          if(IOBS(ICOMP).eq.0)then
            modcfg=.true.
            call usrmsg('updating model to remove obstructions...',
     &        ' ','-')
            CALL EMKCFG('-',IER)
            call usrmsg('updating model to remove obstructions...done.',
     &        ' ','-')
            return
          endif
        ELSEIF(IOBS(ICOMP).eq.0)THEN

C There is no mention of obstructions so get name and create a dummy
C block if it doesn't exist. If user dereferenced the iobs will be zero and
C the model will not have been changed so just return.
          silent=.false.
          CALL ECROBS(ITRC,ITRU,IUF,ICOMP,silent,IER)
          if(IOBS(ICOMP).eq.0)then
            return
          else
            modcfg=.true.
          endif
        ENDIF
        if(modcfg)then
          call usrmsg(' updating model to add obstructions...',
     &      ' ','-')
          CALL EMKCFG('-',IER)
          call usrmsg(' updating model to add obstructions...done.',
     &      ' ','-')
          MODIFY=.TRUE.
        endif
      endif


C Initial rotation values.
      ANGR=0.
      x1=0.
      y1=0.

C Setup for multi-page menu.
   91 MHEAD=4
      MCTL=8
      ILEN=nbobs(icomp)
      IPACT=CREATE
      CALL EKPAGE(IPACT)

C Initial menu entry setup.
   92 IER=0
      ILEN=nbobs(icomp)
      IVERT=-3

C Loop through the items until the page to be displayed. M is the 
C current menu line index. Build up text strings for the menu as well
C as a list for copy and delete use. 
      M=MHEAD
      DO 10 L=1,ILEN
        llbm=LNBLOCKMAT(ICOMP,L)
        llbn=LNBLOCKNAME(ICOMP,L)
        WRITE(PICK(L),304)L,BLOCKNAME(ICOMP,L)(1:llbn),
     &    BLOCKMAT(ICOMP,L)(1:llbm)
  304   FORMAT(I3,2X,A,1x,A)
        IF(L.GE.IST.AND.(L.LE.(IST+MIFULL)))THEN
          M=M+1
          CALL EMKEY(M,KEY,IER)
          WRITE(VERT(M),303)KEY,L,BLOCKNAME(ICOMP,L)(1:llbn),
     &      BLOCKMAT(ICOMP,L)(1:llbm)
  303     FORMAT(A1,I3,2X,A,1x,A)
        ENDIF
   10 CONTINUE

C Present a list of the existing blocks and options. If we reached
C this point with zero gridding then assign a default value.
      if(NOX(icomp).eq.0) NOX(icomp)=20
      if(NOZ(icomp).eq.0) NOZ(icomp)=20
      WRITE(VERT(1),'(A,2I4)')  'a Surface X&Z grid:',NOX(icomp),
     &  NOZ(icomp)
      WRITE(VERT(2),'(A,I3)')   '  No. obstr blocks:',nbobs(icomp)
      VERT(3)    =              '  __________________________  '
      VERT(4)    =              ' Blk| description & compos    '

C Number of actual items displayed.
      MVERT=M+MCTL

C If a long list include page facility text.      
      IF(IPFLG.EQ.0)THEN
        VERT(M+1)='  __________________________  '
      ELSE
        WRITE(VERT(M+1),15)IPM,MPM 
   15   FORMAT   ('0 Page --- Part: ',I2,' of ',I2,' ---')
      ENDIF
      VERT(M+2)  ='* add/delete/copy obstruction '
      VERT(M+3)  ='~ rotate/transfrm obstructions'
      VERT(M+4)  ='@ create window reveal        '
      VERT(M+5)  ='> shading & insol directives  '
      VERT(M+6)  ='! list obstruction details    '
      VERT(M+7)  ='? help                        '
      VERT(M+8)  ='- exit this menu              '

C If a modification has been done then update the obstruction
C file and/or the zone geometry file so that changes can be drawn.
C This is necessary because obstructions are not true zones.
      if(MODIFY)then
        if(newgeo)then
          call geowrite(IUF,LGEOM(ICOMP),ICOMP,iuout,3,IER)
        else
          CALL MKGOMST(IUF,ZOBS(ICOMP),ICOMP,IER)
        endif
        havesaved=.true.
      endif

C If editing the obstructions and user has asked for an update of the
C image then use common block info for the zone as well as for the
C obstructions (ie. set a flag to use obstruction commons rather than
C read of file).
      nzg=1
      nznog(1)=ICOMP
      izgfoc=ICOMP
      CALL ADJVIEW(IER)

C Having updated the view (which uses MODIFY), if havesaved is true
C then we can unset MODIFY.
      if(havesaved.and.MODIFY) MODIFY=.false.

C Help text for this menu.
      helptopic='solar_obs_overview'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Now display the menu.
      CALL EMENU('Obstructions',VERT,MVERT,IVERT)

      IF(IVERT.EQ.MVERT)THEN
        silent = .false.
        call sumrchg(ICOMP,'r',silent)
        RETURN

      ELSEIF(IVERT.EQ.(MVERT-1))THEN

        helptopic='solar_obs_overview'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('obstr menu',nbhelp,'-',0,0,IER)

      ELSEIF(IVERT.EQ.(MVERT-2))THEN

C List details of obstructions.
        if(nbobs(icomp).gt.0)then
          call edisp(iuout,'Details of obstruction blocks:')
          write(outs,'(a,i2,a,i2,a)')'Shading based on grids of ',
     &      NOX(icomp),' by ',NOZ(icomp),' for surfaces.'
          call edisp(iuout,outs)
          write(outs,'(2a)')
     &      'Block X- Y- Z- coords DX- DY- DZ- values Orient Opacity ',
     &      'Name Material'
	  call edisp(iuout,outs)
          DO 9995 I=1,nbobs(icomp)
            lnbn=LNBLOCKNAME(icomp,I)
            lnbm=LNBLOCKMAT(icomp,I)
            if(BLOCKTYP(icomp,I)(1:4).eq.'obs ')then
              WRITE(outs,9994)I,XOB(icomp,I),YOB(icomp,I),
     &          ZOB(icomp,I),DXOB(icomp,I),DYOB(icomp,I),
     &          DZOB(icomp,I),BANGOB(icomp,I,1),OPOB(icomp,I),
     &          BLOCKNAME(icomp,I)(1:lnbn),BLOCKMAT(icomp,I)(1:lnbm)
 9994         FORMAT(I3,6F8.2,2F7.2,' ',a,' ',a)
              call edisp(iuout,outs)
            elseif(BLOCKTYP(icomp,I)(1:4).eq.'obs3')then
              WRITE(outs,9993)I,XOB(icomp,I),YOB(icomp,I),
     &          ZOB(icomp,I),DXOB(icomp,I),DYOB(icomp,I),
     &          DZOB(icomp,I),BANGOB(icomp,I,1),BANGOB(icomp,I,2),
     &          BANGOB(icomp,I,3),OPOB(icomp,I),
     &          BLOCKNAME(icomp,I)(1:lnbn),BLOCKMAT(icomp,I)(1:lnbm)
 9993         FORMAT(I3,6F8.2,4F7.2,' ',a,' ',a)
              call edisp(iuout,outs)
            elseif(BLOCKTYP(icomp,I)(1:4).eq.'obsp')then
              WRITE(outs,'(i3,5a)')I,' ',BLOCKNAME(icomp,I)(1:lnbn),
     &          ' ',BLOCKMAT(icomp,I)(1:lnbm),
     &          ' is a 6 sided polygon obstruction' 
              call edisp(iuout,outs)
            endif
 9995     CONTINUE
          call edisp(iuout,' ')
        endif

      ELSEIF(IVERT.EQ.(MVERT-3))THEN

C Take user to menu of setup directives and option to start shading analysis.
         CALL EPMENSV
         call EDINSUL(ICOMP,IER)
         CALL EPMENRC

      ELSEIF(IVERT.EQ.(MVERT-4))THEN

C Compose a window reveal from 4 obstructions. Ask for which surface,
C then get a root name for obstructions, the get thickness of the
C wall.
        helptopic='solar_obs_overview'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('obstr reveal',nbhelp,'-',0,0,IER)

        CALL EPMENSV
        CALL EASKSUR(ICOMP,IRS,'-','Which surface should the reveal',
     &    'be build around (a vertical transparent surface).',IER)
        CALL EPMENRC
        if(IRS.ne.0)then
          icc=izstocn(ICOMP,IRS)
          write(blkroot,'(a)')SSNAME(icc)(1:8)
          CALL EASKS(blkroot,' Root name for related obstructions?',
     &      ' ',8,'revl  ','reveal root name',IER,nbhelp)

          CALL EASKR(RTK,' ',' Thickness of the wall (m)?',
     &      0.0,'W',1.0,'W',0.2,'thickness',IER,nbhelp)

C Offset is thickness + 5mm, block other dimension is 10mm.
          OWID=0.01

C Material to associate with the reveal.
          CALL EPMENSV
          CALL EPKMLC(ISEL,
     &      'Select an OPAQUE construction from the list to',
     &      'associate with the reveal for visualisation purposes.',IER)
          CALL EPMENRC
          IF(ISEL.GT.0)then
            WRITE(mat,'(A12)')DESC(ISEL)(1:12)
          else
            mat='NONE'
          endif
          AZI=SSPAZI(icc)
          ELV=SSPELV(icc)
          call EREVEAL(ICOMP,IRS,AZI,ELV,RTK,OWID,blkroot,mat,IER)
          MODIFY=.TRUE.
          call warnmod(ICOMP,'ob+')
          CALL ADJVIEW(IER)
        endif

      ELSEIF(IVERT.EQ.(MVERT-5))THEN

C Rotate an obstruction.
        CALL EASKABC(' Choices:',' ','rotate','transform',
     &     'continue',IBOPT,nbhelp)
        if(IBOPT.eq.1)then
          CALL EPMENSV
          INPIC=nbobs(icomp)
          CALL EPICKS(INPIC,IVALB,' ',' Which obstructions to rotate:',
     &      30,nbobs(icomp),PICK,' block list',IER,nbhelp)
          CALL EPMENRC
          CALL EASKR(ANGR,' ','Rotation (degrees, anticlockwise +ve)?',
     &      -359.0,'W',359.0,'W',0.0,'rotation',IER,nbhelp)
          if(ANGR.LT.-.01.OR.ANGR.GT..01)then

C Rotation choices.
            CALL EASKAB(' Rotate about the:',' ',
     &        'site origin','user specified point',IW,nbhelp)
            if(IW.eq.1)then
              X1 = 0.
              Y1 = 0.
            elseif(IW.eq.2)then
              CALL EASKR(x1,' ',' X coordinate (m)?',
     &            0.0,'-',0.0,'-',0.0,'x point',IER,nbhelp)
              CALL EASKR(y1,' ',' Y coordinate (m)?',
     &            0.0,'-',0.0,'-',0.0,'y point',IER,nbhelp)
            endif

            PI = 4.0 * ATAN(1.0)
            A=-ANGR*PI/180.
            CA=COS(A)
            SA=SIN(A)
            do 86 ij=1,INPIC
              IFOC=IVALB(ij)
              XXX=XOB(ICOMP,IFOC)-X1
              YYY=YOB(ICOMP,IFOC)-Y1
              XR=XXX*CA+YYY*SA
              YR=YYY*CA-XXX*SA
              XOB(ICOMP,IFOC)=XR+X1
              YOB(ICOMP,IFOC)=YR+Y1
              BANGOB(ICOMP,IFOC,1)=BANGOB(ICOMP,IFOC,1)+ANGR
              do 89 ibe=1,8
                XXX=XBP(icomp,IFOC,ibe)-X1
                YYY=YBP(icomp,IFOC,ibe)-Y1
                XR=XXX*CA+YYY*SA
                YR=YYY*CA-XXX*SA
                XBP(icomp,IFOC,ibe)=XR+X1
                YBP(icomp,IFOC,ibe)=YR+Y1
  89          continue
  86        continue
            MODIFY=.TRUE.
            call warnmod(ICOMP,'ob+')
          endif
        elseif(IBOPT.eq.2)then
          CALL EPMENSV
          INPIC=nbobs(icomp)
          CALL EPICKS(INPIC,IVALB,' ','Transform which obstructions:',
     &        30,nbobs(icomp),PICK,' block list',IER,nbhelp)
          CALL EPMENRC

C Ask for transform distance for obstruction and then apply. If first
C time for copy use zeros, otherwise re-present the previous values.
          if(anothercopy)then
            write(holds,'(3f10.4)') VALX,VALY,VALZ
          else
            holds = ' 0.00  0.00  0.00 '
          endif
 152      CALL EASKS(HOLDS,' X Y & Z offsets: ',' ',
     &      36,' 0. 0. 0. ','offsets',IER,nbhelp)
          K=0
          CALL EGETWR(HOLDS,K,VALX,-50.0,50.0,'W','X off',IER)
          CALL EGETWR(HOLDS,K,VALY,-50.0,50.0,'W','Y off',IER)
          CALL EGETWR(HOLDS,K,VALZ,-50.0,50.0,'W','Z off',IER)
          if(ier.ne.0)goto 152
          do 87 ij=1,INPIC
            IFOC=IVALB(ij)
            XOB(icomp,IFOC)=XOB(icomp,IFOC)+VALX
            YOB(icomp,IFOC)=YOB(icomp,IFOC)+VALY
            ZOB(icomp,IFOC)=ZOB(icomp,IFOC)+VALZ
            do 88 ibe=1,8
              XBP(icomp,IFOC,ibe)=XBP(icomp,IFOC,ibe)+VALX
              YBP(icomp,IFOC,ibe)=YBP(icomp,IFOC,ibe)+VALY
              ZBP(icomp,IFOC,ibe)=ZBP(icomp,IFOC,ibe)+VALZ
  88        continue
  87      continue
          MODIFY=.TRUE.
          call warnmod(ICOMP,'ob+')
          if(.NOT.anothercopy) anothercopy=.true.  ! something transformed
        else
          GOTO 92
        endif

      ELSEIF(IVERT.EQ.(MVERT-6))THEN

C +- Obstruction.
        IW=1
        CALL EASKATOG('Modify obstruction list:',' ',
     &    'add','delete','copy','continue',' ',' ',' ',IW,nbhelp)
        IF(IW.EQ.2)THEN

C Build up text strings for the delete menu. 
          CALL EPMENSV
          INPIC=MAX0(nbobs(icomp)-2,4)
          CALL EPICKS(INPIC,IVALB,' ','Delete which obstruction(s):',
     &        30,nbobs(icomp),PICK,' delete options',IER,nbhelp)
          CALL EPMENRC
          if(inpic.gt.0)then
            do 143 ijb=1,INPIC
              ID=IVALB(ijb)
              IF(nbobs(icomp).LT.2)then

C To delete the last obstruction, dereference the file and exit.
                if(newgeo)then
                  IOBS(ICOMP)=0
                  ZOBS(ICOMP)=' '
                else
                  ZOBS(ICOMP)=' '
                  IOBS(ICOMP)=0
                  call usrmsg(
     &              ' updating model to remove final obstruction...',
     &              ' ','-')
                  CALL EMKCFG('-',IER)
                  call usrmsg(
     &            ' updating model to remove final obstruction...done.',
     &            ' ','P')
                  return
                endif
              endif
              if(ID.eq.0)GOTO 92
              DO 791 IDV=ID,nbobs(icomp)-1
                XOB(icomp,IDV)=XOB(icomp,IDV+1)
                YOB(icomp,IDV)=YOB(icomp,IDV+1)
                ZOB(icomp,IDV)=ZOB(icomp,IDV+1)
                DXOB(icomp,IDV)=DXOB(icomp,IDV+1)
                DYOB(icomp,IDV)=DYOB(icomp,IDV+1)
                DZOB(icomp,IDV)=DZOB(icomp,IDV+1)
                BANGOB(icomp,IDV,1)=BANGOB(icomp,IDV+1,1)
                BANGOB(icomp,IDV,2)=BANGOB(icomp,IDV+1,2)
                BANGOB(icomp,IDV,3)=BANGOB(icomp,IDV+1,3)
                OPOB(icomp,IDV)=OPOB(icomp,IDV+1)
                BLOCKNAME(icomp,IDV)=BLOCKNAME(icomp,IDV+1)
                LNBLOCKNAME(icomp,IDV)=LNBLOCKNAME(icomp,IDV+1)
                BLOCKMAT(icomp,IDV)=BLOCKMAT(icomp,IDV+1)
                LNBLOCKMAT(icomp,IDV)=LNBLOCKMAT(icomp,IDV+1)
                BLOCKTYP(icomp,IDV)=BLOCKTYP(icomp,IDV+1)
                do 792 ibe=1,8
                  XBP(icomp,IDV,ibe)=XBP(icomp,IDV+1,ibe)
                  YBP(icomp,IDV,ibe)=YBP(icomp,IDV+1,ibe)
                  ZBP(icomp,IDV,ibe)=ZBP(icomp,IDV+1,ibe)
  792           continue
  791         CONTINUE
              nbobs(icomp)=nbobs(icomp)-1
  143       continue
          endif
          MODIFY=.TRUE.
          call warnmod(ICOMP,'ob-')
        ELSEIF(IW.EQ.1)THEN
          IF(nbobs(icomp)+1.LE.MB)THEN
            nbobs(icomp)=nbobs(icomp)+1
	    nbo=nbobs(icomp)
            XOB(icomp,nbo)=1.0
            YOB(icomp,nbo)=1.0
            ZOB(icomp,nbo)=0.0
            DXOB(icomp,nbo)=1.0
            DYOB(icomp,nbo)=1.0
            DZOB(icomp,nbo)=1.0
            BANGOB(icomp,nbo,1)=0.0
            BANGOB(icomp,nbo,2)=0.0
            BANGOB(icomp,nbo,3)=0.0
            OPOB(icomp,nbo)=1.0
            BLOCKNAME(icomp,nbo)='new_blk'
            LNBLOCKNAME(icomp,nbo)=7
            BLOCKMAT(icomp,nbo)='NONE'
            LNBLOCKMAT(icomp,nbo)=4
            BLOCKTYP(icomp,nbo)='obs '
            MODIFY=.TRUE.
            call warnmod(ICOMP,'ob+')
            if(newgeo)then
              call geowrite(IUF,LGEOM(ICOMP),ICOMP,iuout,3,IER)
            else
              CALL MKGOMST(IUF,ZOBS(ICOMP),ICOMP,IER)
            endif
            CALL ADJVIEW(IER)
          ELSE
            CALL USRMSG(' ',' Too many obstructions defined!','W')
            GOTO 92
          ENDIF

C Edit the new block, first hilight the obstruction.
	  nbo=nbobs(icomp)
          ITOBS = nbo
          CALL EPMENSV      ! save state of the calling menue
   77     CALL ADJVIEW(IER) ! redraw the model
          CALL EDOBSB(ITRU,IUF,ICOMP,nbo,ianother,IER)
          MODIFY=.TRUE.
          call warnmod(ICOMP,'ob+')

C If user asked for next or prior obstruction reset nbo and call again.
          if(ianother.eq.0)then
            continue
          elseif(ianother.lt.0.and.nbo.gt.1)then
            nbo=nbo-1; ITOBS = nbo; MODIFY=.TRUE.
            goto 77
          elseif(ianother.gt.0.and.nbo.lt.nbobs(icomp))then
            nbo=nbo+1; ITOBS = nbo; MODIFY=.TRUE.
            goto 77
          endif
          CALL EPMENRC  ! recover state of calling menue
          GOTO 92       ! re-display the menu
        elseif(IW.EQ.3)THEN

C Copy obstruction.
	  nbo=nbobs(icomp)
          CALL EPMENSV
          INPIC=1
          CALL EPICKS(INPIC,IVALB,' ','Copy which obstruction:',
     &      30,nbo,PICK,' copy options',IER,nbhelp)
          CALL EPMENRC
          IFOC=IVALB(1)
          if(nbo+1.LE.MB.and.IFOC.ne.0)then
            nbobs(icomp)=nbobs(icomp)+1  ! increment count nbobs and nbo
            nbo=nbo+1
 244        write(tbn,'(a)') BLOCKNAME(ICOMP,IFOC)
            CALL EASKS(tbn,' Re-name the obstruction copy:',
     &        '(original block name shown)',
     &        12,'block','obs block name',IER,nbhelp)
            if(tbn(1:12).ne.BLOCKNAME(ICOMP,IFOC)(1:12))then
              write(BLOCKNAME(ICOMP,nbo),'(a)') tbn
              LNBLOCKNAME(ICOMP,nbo)=lnblnk(tbn)
            else
              call usrmsg('The block name must be unique',
     &                    'please supply a different name','W')
              goto 244
            endif

C Ask for transform distance for obstruction and then apply. If first
C time for copy use zeros, otherwise re-present the previous values.
            if(anothercopy)then
              write(holds,'(3f10.4)') VALX,VALY,VALZ
            else
              holds = ' 0.00  0.00  0.00 '
            endif
 243        CALL EASKS(HOLDS,'Transform obstruction (X Y Z metres):',
     &        ' ',36,' 0.0 0.0 0.0','transforms XYZ',IER,nbhelp)
            K=0
            CALL EGETWR(HOLDS,K,VALX,-99.,99.,'W','X tr',IER)
            CALL EGETWR(HOLDS,K,VALY,-99.,99.,'W','Y tr',IER)
            CALL EGETWR(HOLDS,K,VALZ,-99.,99.,'W','Z tr',IER)
            if(ier.ne.0)goto 243
	    nbo=nbobs(icomp)
            XOB(icomp,nbo)=XOB(icomp,IFOC)+VALX
            YOB(icomp,nbo)=YOB(icomp,IFOC)+VALY
            ZOB(icomp,nbo)=ZOB(icomp,IFOC)+VALZ
            DXOB(icomp,nbo)=DXOB(icomp,IFOC)
            DYOB(icomp,nbo)=DYOB(icomp,IFOC)
            DZOB(icomp,nbo)=DZOB(icomp,IFOC)
            BANGOB(icomp,nbo,1)=BANGOB(icomp,IFOC,1)
            BANGOB(icomp,nbo,2)=BANGOB(icomp,IFOC,2)
            BANGOB(icomp,nbo,3)=BANGOB(icomp,IFOC,3)
            OPOB(icomp,nbo)=OPOB(icomp,IFOC)
            BLOCKMAT(icomp,nbo)=BLOCKMAT(icomp,IFOC)
            LNBLOCKMAT(icomp,nbo)=LNBLOCKMAT(icomp,IFOC)
            BLOCKTYP(icomp,nbo)=BLOCKTYP(icomp,IFOC)
            do 245 ibe=1,8
              XBP(icomp,nbo,ibe)=XBP(icomp,IFOC,ibe)+VALX
              YBP(icomp,nbo,ibe)=YBP(icomp,IFOC,ibe)+VALY
              ZBP(icomp,nbo,ibe)=ZBP(icomp,IFOC,ibe)+VALZ
  245       continue
            MODIFY=.TRUE.
            call warnmod(ICOMP,'ob+')
            if(.NOT.anothercopy) anothercopy=.true.  ! something copied

            if(newgeo)then
              call geowrite(IUF,LGEOM(ICOMP),ICOMP,iuout,3,IER)
            else
              CALL MKGOMST(IUF,ZOBS(ICOMP),ICOMP,IER)
            endif
	    nbo=nbobs(icomp)
            ITOBS = nbo
            CALL EPMENSV       ! save the calling menu layout
  78        CALL ADJVIEW(IER)  ! redraw wireframe
            call cfgtogg(icfg_type,icfgz,icfgn,icfgc,icfgdfn,
     &                             iicfgz,iicfgn,iicfgc,iicfgdfn)

C Edit the copied block.
            CALL EDOBSB(ITRU,IUF,ICOMP,nbo,ianother,IER)
            MODIFY=.TRUE.

C If user asked for next or prior obstruction reset nbo and call again.
            if(ianother.eq.0)then
              continue
            elseif(ianother.lt.0.and.nbo.gt.1)then
              nbo=nbo-1; ITOBS = nbo; MODIFY=.TRUE.
              goto 78
            elseif(ianother.gt.0.and.nbo.lt.nbobs(icomp))then
              nbo=nbo+1; ITOBS = nbo; MODIFY=.TRUE.
              goto 78
            endif
            CALL EPMENRC   ! recover the calling menu layout
            GOTO 91
          endif
        elseif(IW.EQ.4)THEN
          GOTO 92
        ENDIF

      ELSEIF(IVERT.EQ.(MVERT-7))THEN

C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        ENDIF

      ELSEIF(IVERT.EQ.1)THEN

C Surface X & Z grid density. 
  42    HOLD=' '
        WRITE(HOLD,'(1x,2i5)')NOX(icomp),NOZ(icomp)
        CALL EASKS(HOLD,' Surface X & Z grid density:',' ',
     &     24,' 20 20  ','surf grid',IER,nbhelp)
        K=0
        CALL EGETWI(HOLD,K,NOX(icomp),4,MOX,'W','surf x grid',IER)
        CALL EGETWI(HOLD,K,NOZ(icomp),4,MOZ,'W','surf z grid',IER)
        if(ier.ne.0)goto 42
        MODIFY=.TRUE.
        call warnmod(ICOMP,'ob+')

      ELSEIF(IVERT.GT.MHEAD.AND.IVERT.LT.(MVERT-MCTL+1))THEN

C Edit block identified by KEYIND.
        CALL KEYIND(MVERT,IVERT,IFOC,IO)
        IB=IFOC
        if(IB.ne.0)then
          CALL EPMENSV     ! record calling menu status
          ITOBS = ib       ! mark which obs to highlight
          MODIFY=.TRUE.    ! tell it to pay attention to the highlight
  79      CALL ADJVIEW(IER)  ! redraw wireframe
          CALL EDOBSB(ITRU,IUF,ICOMP,IB,ianother,IER)

C If user asked for next or prior obstruction reset nbo and call again.
          if(ianother.eq.0)then
            continue
          elseif(ianother.lt.0.and.ib.gt.1)then
            ib=ib-1; ITOBS = ib; MODIFY=.TRUE.
            goto 79
          elseif(ianother.gt.0.and.ib.lt.nbobs(icomp))then
            ib=ib+1; ITOBS = ib; MODIFY=.TRUE.
            goto 79
          endif
          CALL EPMENRC
        endif
      ENDIF
      IVERT=-4
      GOTO 92

      END

C ************* ECROBS 
C ECROBS: Read in existing obstruction file or create a fresh one.
C If silent is true then either create or deference silently. If
C the geometry file is version 1.1 or greater then it can hold
C the block information.  Because this is called from several
C code blocks it is still necessary to re-scan the zone files.
      SUBROUTINE ECROBS(ITRC,ITRU,IUF,ICOMP,silent,IER)
#include "building.h"
#include "model.h"

C geometry.h provides commons G0/G2/precz/GS5/GS6.
#include "geometry.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN

      COMMON/RAY3/MODIFY,MODLEN,MODBND

      CHARACTER LTMP*72,OFILE*72
      character sfile*72,snpfile*72,fs*1
      LOGICAL XST,MODIFY,MODLEN,MODBND,unixok,silent,makedummy
      logical newgeo  ! to use for testing if new/old geometry file.
      logical clkok   ! to register user actions in dialog

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='ECROBS'  ! set for subroutine

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

C If newer geometry file it may include obstructions. Update the value
C iobs() and set makedummy. If the value of iobs for this zone
C is 1 then scan obstructions file and write to geometry file. If
C the value of iobs is 2 then there are already obstructions in the
C geometry file so just return. If the value of iobs for this zone
C is 0 then set makedummy to true.
      newgeo=.false.  ! assume older format geometry.
      call eclose(gversion(icomp),1.1,0.01,newgeo)
      if(newgeo)then
        CALL ERPFREE(IUF,ios)
        call georead(IUF,LGEOM(ICOMP),ICOMP,0,ITRU,IER)
        if(IOBS(ICOMP).EQ.2)then
          if(nbobs(icomp).eq.0)then
            ZOBS(ICOMP)=' '
            makedummy=.true.
            goto 42
          else
            return
          endif
        elseif(IOBS(ICOMP).EQ.1)then

C New format geometry with old obstructions. Scan and write with geometry.
          call FINDFIL(ZOBS(ICOMP),XST)
          if(XST)then
            CALL EGOMST(IUF,ICOMP,ZOBS(ICOMP),0,ITRC,ITRU,IER)
            if(IER.eq.0)then
              ZOBS(ICOMP)=' '
              IOBS(ICOMP)=2
              call geowrite(IUF,LGEOM(ICOMP),ICOMP,iuout,3,IER)

C Save the model configuration file before returning.
              CALL EMKCFG('-',IER)
              return
            else
              ZOBS(ICOMP)=' '
              IOBS(ICOMP)=2
              makedummy=.true.
              goto 42
            endif
          endif
        elseif(IOBS(ICOMP).EQ.0)then
          ZOBS(ICOMP)=' '
          IOBS(ICOMP)=2
          makedummy=.true.
          goto 42
        endif
      endif

C If older geometry file ask for the obstruction file, if it exists
C then read it in, if not found create one with default information.
C In either case offer the choice to dereference obstructions.
      if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
        WRITE(OFILE,'(A,A4)')zname(ICOMP)(1:lnzname(ICOMP)),'.obs'
      else
        WRITE(OFILE,'(3A,A4)') zonepth(1:lnblnk(zonepth)),fs,
     &    zname(ICOMP)(1:lnzname(ICOMP)),'.obs'
      endif

C If silent is true create standard name. If this matches an
C existing file and scanning the file via egomst is error-free
C then use it, otherwise add a dummy obstruction file silently.
      if(silent)then
        makedummy=.false.
        if(ZOBS(ICOMP)(1:2).eq.'  '.or.ZOBS(ICOMP)(1:4).eq.'UNKN')then
          ZOBS(ICOMP)=OFILE
          call FINDFIL(OFILE,XST)
          if(XST)then
            CALL EGOMST(IUF,ICOMP,ZOBS(ICOMP),0,ITRC,ITRU,IER)
            IF(IER.eq.0)then
              IOBS(ICOMP)=1
              return
            else
              makedummy=.true.
            endif 
          else
            makedummy=.true.
          endif
        else

C There was a possible file name so see if it exists and if
C a scan is error free use it otherwise setup dummy.
          call FINDFIL(ZOBS(ICOMP),XST)
          if(XST)then
            CALL EGOMST(IUF,ICOMP,ZOBS(ICOMP),0,ITRC,ITRU,IER)
            IF(IER.eq.0)then
              IOBS(ICOMP)=1
              return
            else
              makedummy=.true.
            endif 
          else
            makedummy=.true.
          endif
        endif
      endif

 42   if(makedummy)then

C If we need to make a dummy obstruction file do this or include
C the dummy obstruction in the zone geometry file.
        nbobs(icomp)=1
        XOB(icomp,1)=1.0
        YOB(icomp,1)=1.0
        ZOB(icomp,1)=0.0
        DXOB(icomp,1)=1.0
        DYOB(icomp,1)=1.0
        DZOB(icomp,1)=1.0
        BANGOB(icomp,1,1)=0.0
        BANGOB(icomp,1,2)=0.0
        BANGOB(icomp,1,3)=0.0
        OPOB(icomp,1)=1.0
        BLOCKNAME(ICOMP,1)='First'
        LNBLOCKNAME(ICOMP,1)=5
        BLOCKMAT(ICOMP,1)='NONE'
        LNBLOCKMAT(ICOMP,1)=4
        BLOCKTYP(ICOMP,1)='obs '
        NOX(icomp)=20
        NOZ(icomp)=20
        if(newgeo)then
          call geowrite(IUF,LGEOM(ICOMP),ICOMP,iuout,3,IER)
          if(IER.eq.0)then
            CALL EMKCFG('-',IER)
            return
          else
            call usrmsg('Unable to add obstructions to geometry',
     &        'file. Skipping task.','W')
            IOBS(ICOMP)=0
            ier=1
          endif
        else
          CALL MKGOMST(IUF,ZOBS(ICOMP),ICOMP,IER)
          if(IER.eq.0)then
            IOBS(ICOMP)=1
            CALL EMKCFG('-',IER)
            return
          else
            call usrmsg('Unable to create placeholder obstructions',
     &        'file. Skipping task.','W')
            IOBS(ICOMP)=0
            ier=1
          endif
        endif
        return
      endif
 
C If we get to this point browse for existing obstructions files. If
C newgeo is true adapt the help message and the dialog text.
      helptopic='obstruction_creation'
      call gethelptext(helpinsub,helptopic,nbhelp)
      if(ZOBS(ICOMP)(1:2).eq.'  '.or.ZOBS(ICOMP)(1:4).eq.'UNKN')then
        LTMP=OFILE
        iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
        if(iglib.eq.2)then
          if(newgeo)then
            CALL EASKSCMD(LTMP,'Legacy obstructions import (see help):',
     &        ' ','browse',clkok,72,OFILE,'obstruction file',
     &        ISER,nbhelp)
          else
            CALL EASKSCMD(LTMP,'Zone obstructions file (see help):',
     &        ' ','browse',clkok,72,OFILE,'obstruction file',
     &        ISER,nbhelp)
          endif
          if(clkok)iclkok=2   ! notice browse button
          if(iser.eq.-3)then  ! notice GTK cancel button
            IOBS(ICOMP)=0
            ZOBS(ICOMP)=' '
            return
          endif
        else
          if(newgeo)then
           CALL EASKS2CMD(LTMP,'Legacy obstructions import (see help):',
     &       ' ','cancel','browse',iclkok,72,OFILE,'obstruction file',
     &       ISER,nbhelp)
          else
           CALL EASKS2CMD(LTMP,'Zone obstructions file (see help):',
     &       ' ','cancel','browse',iclkok,72,OFILE,'obstruction file',
     &       ISER,nbhelp)
          endif
          if(iclkok.eq.1)then    ! notice X11 cancel option
            return
          endif
        endif
      else

C Obstruction file name is non-blank.
        if(newgeo)then
          LTMP=ZOBS(ICOMP)
          CALL EASKS2CMD(LTMP,
     &      'Legacy obstructions file import (see help):',' ',
     &      'dereference','browse',iclkok,72,OFILE,'obstruction file',
     &      ISER,nbhelp)
        else
          LTMP=ZOBS(ICOMP)
          CALL EASKS2CMD(LTMP,'Obstructions file (see help):',
     &      ' ','dereference','browse',iclkok,72,OFILE,'obstr file',
     &      ISER,nbhelp)
        endif
        if(iser.eq.-3) return  ! notice cancel button
        if(iclkok.eq.1)then    ! notice dereference
          ZOBS(ICOMP)=' '
          IOBS(ICOMP)=0
          nbobs(icomp)=0       ! reinforce dereference
          return
        endif
      endif

C User asked to browse within the model for other obstruction files.
      if(iclkok.eq.2)then
        sfile=' '
        snpfile=' '
        call edisp(iuout,' ')
        call browsefilelist('?','zon','obs',sfile,snpfile,nfile,iier)
        if(nfile.gt.0)then
          sfile=' '
          snpfile=' '
          call browsefilelist('b','zon','obs',sfile,snpfile,nfile,
     &      iier)
          if(snpfile(1:2).ne.'  ')then
            write(LTMP,'(3a)')zonepth(1:lnblnk(zonepth)),fs,
     &        snpfile(1:lnblnk(snpfile))
          else
            LTMP=OFILE
            CALL EASKS(LTMP,
     &        ' Obstructions file name: (because browse was blank) ',
     &        ' ',72,OFILE,'obstructions file',ISER,nbhelp)
            if(iser.eq.-3)then  ! notice cancel button
              ZOBS(ICOMP)=' '
              IOBS(ICOMP)=0
              return
            endif
          endif
        else
          LTMP=OFILE
          CALL EASKS(LTMP,
     &      ' Obstructions file name: (because browse was blank)',
     &      ' ',72,OFILE,'obstructions file',ISER,nbhelp)
          if(iser.eq.-3) then  ! notice cancel button
            ZOBS(ICOMP)=' '
            IOBS(ICOMP)=0
            return
          endif
        endif
        ZOBS(ICOMP)=LTMP
      else
        IF(LTMP(1:2).NE.'  ')ZOBS(ICOMP)=LTMP
      endif

C Because this is a potentially new file it must be scanned rather
C than rely on information already in common blocks.
      call FINDFIL(LTMP,XST)
      IF(XST)THEN
        CALL EGOMST(IUF,ICOMP,ZOBS(ICOMP),0,ITRC,ITRU,IER)
        IF(IER.NE.0)RETURN
        IOBS(ICOMP)=1 
      ELSE
          
C Provide default values for the new obstructions file.
        nbobs(icomp)=1
        XOB(icomp,1)=1.0
        YOB(icomp,1)=1.0
        ZOB(icomp,1)=0.0
        DXOB(icomp,1)=1.0
        DYOB(icomp,1)=1.0
        DZOB(icomp,1)=1.0
        BANGOB(icomp,1,1)=0.0
        BANGOB(icomp,1,2)=0.0
        BANGOB(icomp,1,3)=0.0
        OPOB(icomp,1)=1.0
        BLOCKNAME(ICOMP,1)='First'
        LNBLOCKNAME(ICOMP,1)=5
        BLOCKMAT(ICOMP,1)='NONE'
        LNBLOCKMAT(ICOMP,1)=4
        BLOCKTYP(ICOMP,1)='obs '
        NOX(icomp)=20
        NOZ(icomp)=20

C Write to geometry file or the obstructions file.
        if(newgeo)then
          call geowrite(IUF,LGEOM(ICOMP),ICOMP,iuout,3,IER)
          IF(IER.NE.0)RETURN
          IOBS(ICOMP)=2 
          MODIFY=.TRUE.
        else
          CALL MKGOMST(IUF,ZOBS(ICOMP),ICOMP,IER)
          IF(IER.NE.0)RETURN
          IOBS(ICOMP)=1 
          MODIFY=.TRUE.
         endif
      ENDIF
      RETURN
      END

C ************* EDOBSB 
C EDOBSB: Edit details of an obstruction block. Adapt interface
C if user requests conversion to a general 6 sided polygon. This
C subroutine assues that the current zone geometry common blocks
C have already been filled.
      SUBROUTINE EDOBSB(ITRU,IUF,ICOMP,IB,ianother,IER)
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "esprdbfile.h"
#include "material.h"
#include "prj3dv.h"

C Parameters
      integer ITRU   ! unit for writing errors
      integer IUF    ! file unit
      integer ICOMP  ! zone number
      integer IB     ! block number
      integer ianother ! +1 jump next -1 jump previous otherwise zero
      integer IER    ! zero is ok
      
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/GB1/XB(12),YB(12),ZB(12),JVNB(6,4)
      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)
      COMMON/RAY2/ITDSP,ITBND,ITEPT,ITZNM,ITSNM,ITVNO,ITORG,ITSNR,
     &            ITOBS,ITHLS,ITHLZ,ITGRD,GRDIS,ITPPSW
      COMMON/RAY3/MODIFY,MODLEN,MODBND

      LOGICAL MODIFY,MODLEN,MODBND
      DIMENSION VERT(25),IVLST(MTV)
      CHARACTER VERT*33,MSG*72,S12*12
      character HOLD*32,outs*124
      logical newgeo  ! to use for testing if new/old geometry file.
      logical close   ! to see if non-zero block elevation.
      logical havesaved  ! to signal zone file written
      integer llbm    ! length for material
      real top        ! for the highest point of the block
      integer NITMS,IVERT ! max items and current menu item

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='EDOBSB'  ! set for subroutine

C Initial values for point at angle facility.
      V1=1.0
      AZ=0.0
      EL=0.0
      top=0.0

      havesaved=.false.
      newgeo=.false.  ! assume older format geometry.
      call eclose(gversion(icomp),1.1,0.01,newgeo)

C Initial menu entry setup.
      iz=icomp
      MODIFY=.FALSE.
   92 IER=0
      IVERT=-3

C If a modification has been done then update the obstruction
C file or geometry file.
      if(MODIFY)then
        if(newgeo)then
          call geowrite(IUF,LGEOM(ICOMP),ICOMP,iuout,3,IER)
        else
          CALL MKGOMST(IUF,ZOBS(ICOMP),ICOMP,IER)
        endif
        havesaved=.true.
      endif

C Extract coordinates for corners for the current block to 
C display in the command menu (depending on obstruction type).
      if(BLOCKTYP(ICOMP,IB)(1:4).eq.'obs ')then
        CALL CNVBLK(XOB(ICOMP,IB),YOB(ICOMP,IB),ZOB(ICOMP,IB),
     &    DXOB(ICOMP,IB),DYOB(ICOMP,IB),DZOB(ICOMP,IB),
     &    BANGOB(ICOMP,IB,1))
        top=ZB(5)
      elseif(BLOCKTYP(ICOMP,IB)(1:4).eq.'obs3')then
        CALL CNVBLK3A(XOB(ICOMP,IB),YOB(ICOMP,IB),ZOB(ICOMP,IB),
     &    DXOB(ICOMP,IB),DYOB(ICOMP,IB),DZOB(ICOMP,IB),
     &    BANGOB(ICOMP,IB,1),BANGOB(ICOMP,IB,2),BANGOB(ICOMP,IB,3))
        if(ZB(5).gt.top) top=ZB(5)
        if(ZB(6).gt.top) top=ZB(6)
        if(ZB(7).gt.top) top=ZB(7)
        if(ZB(8).gt.top) top=ZB(8)
      elseif(BLOCKTYP(ICOMP,IB)(1:4).eq.'obsp')then
        call CNVBLKP(ICOMP,IB) ! convert obsp type.
        if(ZB(5).gt.top) top=ZB(5)
        if(ZB(6).gt.top) top=ZB(6)
        if(ZB(7).gt.top) top=ZB(7)
        if(ZB(8).gt.top) top=ZB(8)
      endif
      
      ITOBS = IB

C Set menu header text. No support for angled obstructions in legacy
C geometry files.
      if(BLOCKTYP(ICOMP,IB)(1:4).eq.'obs '.or.
     &   BLOCKTYP(ICOMP,IB)(1:4).eq.'obs3')then
        WRITE(VERT(1),'(A,3F6.2)')  'a origin X Y Z:',XOB(ICOMP,IB),
     &    YOB(ICOMP,IB),ZOB(ICOMP,IB)
        WRITE(VERT(2),'(A,3F6.2)')  'b block  W D H:',DXOB(ICOMP,IB),
     &    DYOB(ICOMP,IB),DZOB(ICOMP,IB)
        if(newgeo)then
          WRITE(VERT(3),'(A,F7.2)') 'c rotation (Z): ',
     &      BANGOB(ICOMP,IB,1)
          WRITE(VERT(4),'(A,F7.2)') 'd rotation (Y): ',
     &      BANGOB(ICOMP,IB,2)
          WRITE(VERT(5),'(A,F7.2)') 'e tilt (NA)   : ',
     &      BANGOB(ICOMP,IB,3)
        else
          WRITE(VERT(3),'(A,F7.2)') 'c rotation (Z): ',
     &      BANGOB(ICOMP,IB,1)
          WRITE(VERT(4),'(A,F7.2)') 'd elev (NA)   : ',
     &      BANGOB(ICOMP,IB,2)
          WRITE(VERT(5),'(A,F7.2)') 'e tilt (NA)   : ',
     &      BANGOB(ICOMP,IB,3)
        endif
        WRITE(VERT(6),'(A,A)')      'f name        : ',
     &    BLOCKNAME(ICOMP,IB)
        llbm=LNBLOCKMAT(ICOMP,IB)
        WRITE(VERT(7),'(A,A)')      'g construction: ',
     &    BLOCKMAT(ICOMP,IB)(1:llbm)
        WRITE(VERT(8),'(A,F6.2)')   'h opacity   : ',OPOB(ICOMP,IB)
        VERT(9)                  =  '  ____________________________ '
        VERT(10)                  =  '  block coords    X       Y   '
        WRITE(VERT(11),'(A,2F8.3)') '  front left  :',XB(1),YB(1)
        WRITE(VERT(12),'(A,2F8.3)') '  front right :',XB(2),YB(2)
        WRITE(VERT(13),'(A,2F8.3)') '  back right  :',XB(3),YB(3)
        WRITE(VERT(14),'(A,2F8.3)') '  back left   :',XB(4),YB(4)
        WRITE(VERT(15),'(A,F8.3,A)')'  top @       :',top,' (Z)'
        VERT(16)                  = '  ____________________________  '
        VERT(17)                  = '  zone bounds  X    Y     Z     '
        WRITE(VERT(18),'(A,3F8.3)') '  max: ',ZXMX(iz),ZYMX(iz),
     &    ZZMX(iz)
        WRITE(VERT(19),'(A,3F8.3)') '  min: ',ZXMN(iz),ZYMN(iz),
     &    ZZMN(iz)
        VERT(20)                  = '  ____________________________  '
        if(ib.gt.1.and.ib.lt.nbobs(iz))then
          VERT(21)=                 '< jump to previous obstruction  '
          VERT(22)=                 '> jump to next obstruction      '
        elseif(ib.eq.1)then
          VERT(21)=                 '                                '
          VERT(22)=                 '> jump to next obstruction      '        
        elseif(ib.eq.nbobs(iz))then
          VERT(21)=                 '< jump to previous obstruction  '
          VERT(22)=                 '                                '
        endif
        VERT(23)                  = '* convert to general polygons   '
        VERT(24)                  = '? help                          '
        VERT(25)                  = '- exit                          '
      else
        VERT(1)                   = ' coords   X     Y      Z      '
        WRITE(VERT(2),'(A,3F8.3)')  'a 1:',XBP(iz,ib,1),YBP(iz,ib,1),
     &    ZBP(iz,ib,1)
        WRITE(VERT(3),'(A,3F8.3)')  'b 2:',XBP(iz,ib,2),YBP(iz,ib,2),
     &    ZBP(iz,ib,2)
        WRITE(VERT(4),'(A,3F8.3)')  'c 3:',XBP(iz,ib,3),YBP(iz,ib,3),
     &    ZBP(iz,ib,3)
        WRITE(VERT(5),'(A,3F8.3)')  'd 4:',XBP(iz,ib,4),YBP(iz,ib,4),
     &    ZBP(iz,ib,4)
        WRITE(VERT(6),'(A,3F8.3)')  'e 5:',XBP(iz,ib,5),YBP(iz,ib,5),
     &    ZBP(iz,ib,5)
        WRITE(VERT(7),'(A,3F8.3)')  'f 6:',XBP(iz,ib,6),YBP(iz,ib,6),
     &    ZBP(iz,ib,6)
        WRITE(VERT(8),'(A,3F8.3)')  'g 7:',XBP(iz,ib,7),YBP(iz,ib,7),
     &    ZBP(iz,ib,7)
        WRITE(VERT(9),'(A,3F8.3)')  'h 8:',XBP(iz,ib,8),YBP(iz,ib,8),
     &    ZBP(iz,ib,8)
        VERT(10)                  = '  ____________________________ '
        WRITE(VERT(11),'(A,A)')     'i name        : ',
     &    BLOCKNAME(ICOMP,IB)
        llbm=LNBLOCKMAT(ICOMP,IB)
        WRITE(VERT(12),'(A,A)')     'j construction: ',
     &    BLOCKMAT(ICOMP,IB)(1:llbm)
        WRITE(VERT(13),'(A,F6.2)')   'k opacity   : ',OPOB(ICOMP,IB)
        VERT(14)                  = '  ____________________________ '
        VERT(15)                  = '  zone bounds  X    Y     Z     '
        WRITE(VERT(16),'(A,3F8.3)') '  max: ',ZXMX(iz),ZYMX(iz),
     &    ZZMX(iz)
        WRITE(VERT(17),'(A,3F8.3)') '  min: ',ZXMN(iz),ZYMN(iz),
     &    ZZMN(iz)
        VERT(18)                  = '  ____________________________  '
        if(ib.gt.1.and.ib.lt.nbobs(iz))then
          VERT(19)=                 '< jump to previous obstruction  '
          VERT(20)=                 '> jump to next obstruction      '
        elseif(ib.eq.1)then
          VERT(19)=                 '                                '
          VERT(20)=                 '> jump to next obstruction      '        
        elseif(ib.eq.nbobs(iz))then
          VERT(19)=                 '< jump to previous obstruction  '
          VERT(20)=                 '                                '
        endif
        VERT(21)                  = '? help                          '
        VERT(22)                  = '- exit                          '

      endif
      
C Display the zone.
      nzg=1
      nznog(1)=ICOMP
      izgfoc=ICOMP
      CALL ADJVIEW(IER)

C Having updated the view (which uses MODIFY), if havesaved is true
C then we can unset MODIFY.
      if(havesaved.and.MODIFY) MODIFY=.false.

C Help text for this menu.
      helptopic='obstruction_prism'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Now display the menu.
      if(BLOCKTYP(ICOMP,IB)(1:4).eq.'obs '.or.
     &   BLOCKTYP(ICOMP,IB)(1:4).eq.'obs3')then
        NITMS=25
        CALL EMENU('Block Details',VERT,NITMS,IVERT)
      else
        NITMS=22
        CALL EMENU('Obstruction Details',VERT,NITMS,IVERT)
      endif

C One set of interface actions if type 'obs' or 'obs3'.
      if(BLOCKTYP(ICOMP,IB)(1:4).eq.'obs '.or.
     &   BLOCKTYP(ICOMP,IB)(1:4).eq.'obs3')then
        if(IVERT.EQ.25)then
          ITOBS = 0
          CALL ADJVIEW(IER)
          ianother=0  ! signal no jump to previous or next
          RETURN
        elseif(IVERT.EQ.1)then
  42      CALL EASKABCD('Choices:',' ','edit origin','use zone vertex',
     &      '@ angle&dist from vertex','continue',IBOPT,nbhelp)
          if(IBOPT.eq.1)then
            HOLD=' '
            WRITE(HOLD,'(1x,3f9.4)')XOB(ICOMP,IB),YOB(ICOMP,IB),
     &        ZOB(ICOMP,IB)
            MODIFY=.TRUE.
          elseif(IBOPT.eq.2)then

C Use current zone G1 common blocks to present list of points
C via epkvert.
            inpick=1
            CALL EPMENSV
            call EPKVERT(INPICK,IVLST,'Vertices for origin',
     &        'Select a vertex to define the origin...',' ',nbhelp,ier)
            CALL EPMENRC
            if(inpick.eq.1)then
              iwhich1=IVLST(1)
              HOLD=' '
              WRITE(HOLD,'(1x,3f9.4)')X(iwhich1),Y(iwhich1),Z(iwhich1)
              MODIFY=.TRUE.
            else
              goto 92
            endif
          elseif(IBOPT.eq.3)then

C Use current zone G1 common blocks to present list of points
C via epkvert.
            inpick=1
            CALL EPMENSV
            call EPKVERT(INPICK,IVLST,'Vertice in zone',
     &        'Select a vertex to begin from...',' ',nbhelp,ier)
            CALL EPMENRC
            if(inpick.eq.1)then
              iwhich1=IVLST(1)
              write(hold,'(f10.4,f9.3,f8.3)') V1,AZ,EL
 342          CALL EASKS(HOLD,
     &    'Distance (m), azimuth (north=0, east=90), elev (vert=90):',
     &    ' ',32,' 1. 0. 0. ','dist azim elev',IER,nbhelp)
              K=0
              CALL EGETWR(HOLD,K,V1,-999.9,999.9,'W','dist',IER)
              CALL EGETWR(HOLD,K,AZ,-359.9,359.9,'W','azim',IER)
              CALL EGETWR(HOLD,K,EL,-90.0,90.0,'W','elev',IER)
              if(ier.ne.0)goto 342
              PI = 4.0 * ATAN(1.0)
              RAD = PI/180.
              RYAZI = AZ*RAD
              RSALT = EL*RAD
              z3 = V1*SIN(RSALT)
              XYDIS = V1*COS(RSALT)
              IF (XYDIS .LT. 1E-6)THEN
                x3 = 0.
                y3 = 0.
              ELSE
                x3 = XYDIS*SIN(RYAZI)
                y3 = XYDIS*COS(RYAZI)
              ENDIF
              write(outs,'(a,3f10.4)') ' Point @ X,Y,Z:',x3+X(iwhich1),
     &          y3+Y(iwhich1),z3+Z(iwhich1)
              call edisp(itru,outs)
              x3=x3+X(iwhich1)
              y3=y3+Y(iwhich1)
              z3=z3+Z(iwhich1)
              HOLD=' '
              WRITE(HOLD,'(1x,3f9.4)')x3,y3,z3
              MODIFY=.TRUE.
            else
              goto 92
            endif
          elseif(IBOPT.eq.4)then
            goto 92
          endif
          WRITE(MSG,'(a,I3,a)')'For block ',IB,' origin: '
          CALL EASKS(HOLD,MSG,
     &     ' X Y Z coords ',32,' 1.0 1.0 0.0  ','blk org xyz',
     &     IER,nbhelp)
          K=0
          CALL EGETWR(HOLD,K,VX,-999.0,999.9,'W','blk orgx',IER)
          CALL EGETWR(HOLD,K,VY,-999.0,999.9,'W','blk orgy',IER)
          CALL EGETWR(HOLD,K,VZ,-99.0,999.9,'W','blk orgz',IER)
          XOB(ICOMP,IB)=VX
          YOB(ICOMP,IB)=VY
          ZOB(ICOMP,IB)=VZ
          if(ier.ne.0)goto 42
          MODIFY=.TRUE.
          MODBND=.TRUE.
          MODLEN=.TRUE.
          call warnmod(ICOMP,'ob+')
        ELSEIF(IVERT.EQ.2)THEN
  43      HOLD=' '
          WRITE(HOLD,'(1x,3f8.3)')DXOB(ICOMP,IB),DYOB(ICOMP,IB),
     &      DZOB(ICOMP,IB)
          WRITE(MSG,'(a,I3,a)')'For block ',IB,' dimensions: '
          CALL EASKS(HOLD,MSG,' width (X), depth (Y), height (Z) ',
     &      32,' 1.0 1.0 1.0  ','blk WDH',IER,nbhelp)
          K=0
          CALL EGETWR(HOLD,K,VX,0.001,99.9,'W','obs blk wid',IER)
          CALL EGETWR(HOLD,K,VY,0.001,99.9,'W','obs blk dep',IER)
          CALL EGETWR(HOLD,K,VZ,0.001,99.9,'W','obs blk hgt',IER)
          if(ier.ne.0)goto 43
          DXOB(ICOMP,IB)=VX
          DYOB(ICOMP,IB)=VY
          DZOB(ICOMP,IB)=VZ
          MODIFY=.TRUE.
          MODBND=.TRUE.
          MODLEN=.TRUE.
          call warnmod(ICOMP,'ob+')
        ELSEIF(IVERT.EQ.3)THEN
 44       VX=BANGOB(ICOMP,IB,1)
          CALL EASKR(VX,MSG,' Block Z rotation? ',
     &       -359.0,'W',359.0,'W',0.0,'block Z rotation',IER,nbhelp)
          if(ier.ne.0)goto 44
          BANGOB(ICOMP,IB,1)=VX
          MODIFY=.TRUE.
          MODBND=.TRUE.
          MODLEN=.TRUE.
          call warnmod(ICOMP,'ob+')
        ELSEIF(IVERT.EQ.4)THEN

C If newer geometry file offer a 2nd axis of rotation.
          if(newgeo)then
            continue
          else
            call usrmsg('Older format file does not support the',
     &        'second rotation.','W')
            goto 92
          endif
 45       VX=BANGOB(ICOMP,IB,2)
          CALL EASKR(VX,MSG,' Block Y rotation (see help)? ',
     &       -180.0,'W',180.0,'W',0.0,'block Y rotation',IER,nbhelp)
          if(ier.ne.0)goto 45
	  call eclose(VX,0.0,0.1,close)
          if(.NOT.close)then
            BANGOB(ICOMP,IB,2)=VX
            BLOCKTYP(ICOMP,IB)='obs3'   ! alter the type if non-zero
            MODIFY=.TRUE.
            MODBND=.TRUE.
            MODLEN=.TRUE.
            call warnmod(ICOMP,'ob+')
          endif
        ELSEIF(IVERT.EQ.5)THEN
          call edisp(iuout,'Tilting obstruction is not yet supported.')
          goto 92
        ELSEIF(IVERT.EQ.6)THEN
          S12=BLOCKNAME(ICOMP,IB)
          CALL EASKS(S12,' ',' Name of obstruction? ',
     &      12,'obstruction','Block name',IER,nbhelp)
          IF(S12(1:2).NE.'  ')then
            BLOCKNAME(ICOMP,IB)=S12
            LNBLOCKNAME(ICOMP,IB)=lnblnk(S12)
          endif
          MODIFY=.TRUE.
        ELSEIF(IVERT.EQ.7)THEN

C Note: this logic only picks up the first 12 char of construction.
          CALL EPMENSV
          CALL EPKMLC(ISEL,
     &      'Select an OPAQUE construction from the list to',
     &      'associate with the block for visualisation purposes.',IER)
          CALL EPMENRC
          IF(ISEL.GT.0)then
            WRITE(BLOCKMAT(icomp,IB),'(A)')DESC(ISEL)(1:12)
            LNBLOCKMAT(icomp,IB)=lnblnk(BLOCKMAT(icomp,IB))
            MODIFY=.TRUE.
          endif
          IF(IER.EQ.1)THEN
            CALL USRMSG(' ',
     &     'A problem was encountered with the block construction','W')
          ENDIF
        ELSEIF(IVERT.EQ.8)THEN
 46       VX=OPOB(ICOMP,IB)
          CALL EASKR(VX,MSG,' Block opacity (see help)? ',
     &       0.0,'W',1.0,'W',1.0,'block opacity',IER,nbhelp)
          if(ier.ne.0)goto 46
          OPOB(icomp,IB)=VX
          MODIFY=.TRUE.
        ELSEIF(IVERT.EQ.21)THEN

C Jump to previous obstruction.
          ianother= -1
          return
        ELSEIF(IVERT.EQ.22)THEN

C Jump to next obstruction.
          ianother=1
          return
        ELSEIF(IVERT.EQ.23)THEN

C Convert current obstruction block into general polygon obstruction.
          if(BLOCKTYP(ICOMP,IB)(1:4).eq.'obs ')then
            CALL CNVBLK(XOB(ICOMP,IB),YOB(ICOMP,IB),ZOB(ICOMP,IB),
     &        DXOB(ICOMP,IB),DYOB(ICOMP,IB),DZOB(ICOMP,IB),
     &        BANGOB(ICOMP,IB,1))
          elseif(BLOCKTYP(ICOMP,IB)(1:4).eq.'obs3')then
            CALL CNVBLK3A(XOB(ICOMP,IB),YOB(ICOMP,IB),ZOB(ICOMP,IB),
     &        DXOB(ICOMP,IB),DYOB(ICOMP,IB),DZOB(ICOMP,IB),
     &        BANGOB(ICOMP,IB,1),BANGOB(ICOMP,IB,2),BANGOB(ICOMP,IB,3))
          endif
          do 56 ibe=1,8
            XBP(ICOMP,IB,ibe)=XB(ibe)
            YBP(ICOMP,IB,ibe)=YB(ibe)
            ZBP(ICOMP,IB,ibe)=ZB(ibe)
  56      continue
	  
          BLOCKTYP(ICOMP,IB)='obsp'
          DXOB(ICOMP,IB)=0.0  ! reset unused data
          DYOB(ICOMP,IB)=0.0
          DZOB(ICOMP,IB)=0.0
          XOB(ICOMP,IB)=0.0
          YOB(ICOMP,IB)=0.0
          ZOB(ICOMP,IB)=0.0
          BANGOB(ICOMP,IB,1)=0.0
          BANGOB(ICOMP,IB,2)=0.0
          BANGOB(ICOMP,IB,3)=0.0
          OPOB(ICOMP,IB)=0.0
          MODIFY=.TRUE.
          call warnmod(ICOMP,'ob+')
          GOTO 92
	  
        ELSEIF(IVERT.EQ.24)THEN
          helptopic='obstruction_prism'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL PHELPD('obstr menu',nbhelp,'-',0,0,IER)
        ELSE
          GOTO 92
        ENDIF
        GOTO 92
      else

C Interface options if a general polygon obstruction.
        if(IVERT.EQ.20)then
          ITOBS = 0
          CALL ADJVIEW(IER)
          ianother=0  ! signal no jump to previous or next
          RETURN
        elseif(IVERT.EQ.2)then
  431     HOLD=' '
          WRITE(HOLD,'(1x,3f8.3)')XBP(ICOMP,IB,1),YBP(ICOMP,IB,1),
     &      ZBP(ICOMP,IB,1)
          WRITE(MSG,'(a,I3,a)')'For block ',IB,' lower front left: '
          CALL EASKS(HOLD,MSG,' X(m) Y(m) Z(m) (see help)',
     &      32,' 1.0 1.0 1.0  ','lower front left',IER,nbhelp)
          K=0
          CALL EGETWR(HOLD,K,VX,0.001,99.9,'W','obs v1 X',IER)
          CALL EGETWR(HOLD,K,VY,0.001,99.9,'W','obs v1 Y',IER)
          CALL EGETWR(HOLD,K,VZ,0.001,99.9,'W','obs v1 Z',IER)
          if(ier.ne.0)goto 431
          XBP(ICOMP,IB,1)=VX
          YBP(ICOMP,IB,1)=VY
          ZBP(ICOMP,IB,1)=VZ
          MODIFY=.TRUE.
          MODBND=.TRUE.
          MODLEN=.TRUE.
          call warnmod(ICOMP,'ob+')
        elseif(IVERT.EQ.3)then
  432     HOLD=' '
          WRITE(HOLD,'(1x,3f8.3)')XBP(ICOMP,IB,2),YBP(ICOMP,IB,2),
     &      ZBP(ICOMP,IB,2)
          WRITE(MSG,'(a,I3,a)')'For block ',IB,' lower front right: '
          CALL EASKS(HOLD,MSG,' X(m) Y(m) Z(m) (see help)',
     &      32,' 1.0 1.0 1.0  ','lower front right',IER,nbhelp)
          K=0
          CALL EGETWR(HOLD,K,VX,-99.9,99.9,'W','obs v2 X',IER)
          CALL EGETWR(HOLD,K,VY,-99.9,99.9,'W','obs v2 Y',IER)
          CALL EGETWR(HOLD,K,VZ,-99.9,99.9,'W','obs v2 Z',IER)
          if(ier.ne.0)goto 432
          XBP(ICOMP,IB,2)=VX
          YBP(ICOMP,IB,2)=VY
          ZBP(ICOMP,IB,2)=VZ
          MODIFY=.TRUE.
          MODBND=.TRUE.
          MODLEN=.TRUE.
          call warnmod(ICOMP,'ob+')
        elseif(IVERT.EQ.4)then
  433     HOLD=' '
          WRITE(HOLD,'(1x,3f8.3)')XBP(ICOMP,IB,3),YBP(ICOMP,IB,3),
     &      ZBP(ICOMP,IB,3)
          WRITE(MSG,'(a,I3,a)')'For block ',IB,' lower back right: '
          CALL EASKS(HOLD,MSG,' X(m) Y(m) Z(m) (see help)',
     &      32,' 1.0 1.0 1.0  ','lower back right',IER,nbhelp)
          K=0
          CALL EGETWR(HOLD,K,VX,-99.9,99.9,'W','obs v3 X',IER)
          CALL EGETWR(HOLD,K,VY,-99.9,99.9,'W','obs v3 Y',IER)
          CALL EGETWR(HOLD,K,VZ,-99.9,99.9,'W','obs v3 Z',IER)
          if(ier.ne.0)goto 433
          XBP(ICOMP,IB,3)=VX
          YBP(ICOMP,IB,3)=VY
          ZBP(ICOMP,IB,3)=VZ
          MODIFY=.TRUE.
          MODBND=.TRUE.
          MODLEN=.TRUE.
          call warnmod(ICOMP,'ob+')
        elseif(IVERT.EQ.5)then
  434     HOLD=' '
          WRITE(HOLD,'(1x,3f8.3)')XBP(ICOMP,IB,4),YBP(ICOMP,IB,4),
     &      ZBP(ICOMP,IB,4)
          WRITE(MSG,'(a,I3,a)')'For block ',IB,' lower back left: '
          CALL EASKS(HOLD,MSG,' X(m) Y(m) Z(m) (see help)',
     &      32,' 1.0 1.0 1.0  ','lower back left',IER,nbhelp)
          K=0
          CALL EGETWR(HOLD,K,VX,-99.9,99.9,'W','obs v4 X',IER)
          CALL EGETWR(HOLD,K,VY,-99.9,99.9,'W','obs v4 Y',IER)
          CALL EGETWR(HOLD,K,VZ,-99.9,99.9,'W','obs v4 Z',IER)
          if(ier.ne.0)goto 434
          XBP(ICOMP,IB,4)=VX
          YBP(ICOMP,IB,4)=VY
          ZBP(ICOMP,IB,4)=VZ
          MODIFY=.TRUE.
          MODBND=.TRUE.
          MODLEN=.TRUE.
          call warnmod(ICOMP,'ob+')
        elseif(IVERT.EQ.6)then
  435     HOLD=' '
          WRITE(HOLD,'(1x,3f8.3)')XBP(ICOMP,IB,5),YBP(ICOMP,IB,5),
     &      ZBP(ICOMP,IB,5)
          WRITE(MSG,'(a,I3,a)')'For block ',IB,' upper front left: '
          CALL EASKS(HOLD,MSG,' X(m) Y(m) Z(m) (see help)',
     &      32,' 1.0 1.0 1.0  ','upper front left',IER,nbhelp)
          K=0
          CALL EGETWR(HOLD,K,VX,-99.9,99.9,'W','obs v5 X',IER)
          CALL EGETWR(HOLD,K,VY,-99.9,99.9,'W','obs v5 Y',IER)
          CALL EGETWR(HOLD,K,VZ,-99.9,99.9,'W','obs v5 Z',IER)
          if(ier.ne.0)goto 435
          XBP(ICOMP,IB,5)=VX
          YBP(ICOMP,IB,5)=VY
          ZBP(ICOMP,IB,5)=VZ
          MODIFY=.TRUE.
          MODBND=.TRUE.
          MODLEN=.TRUE.
          call warnmod(ICOMP,'ob+')
        elseif(IVERT.EQ.7)then
  436     HOLD=' '
          WRITE(HOLD,'(1x,3f8.3)')XBP(ICOMP,IB,6),YBP(ICOMP,IB,6),
     &      ZBP(ICOMP,IB,6)
          WRITE(MSG,'(a,I3,a)')'For block ',IB,' upper front right: '
          CALL EASKS(HOLD,MSG,' X(m) Y(m) Z(m) (see help)',
     &      32,' 1.0 1.0 1.0  ','upper front right',IER,nbhelp)
          K=0
          CALL EGETWR(HOLD,K,VX,-99.9,99.9,'W','obs v6 X',IER)
          CALL EGETWR(HOLD,K,VY,-99.9,99.9,'W','obs v6 Y',IER)
          CALL EGETWR(HOLD,K,VZ,-99.9,99.9,'W','obs v6 Z',IER)
          if(ier.ne.0)goto 436
          XBP(ICOMP,IB,6)=VX
          YBP(ICOMP,IB,6)=VY
          ZBP(ICOMP,IB,6)=VZ
          MODIFY=.TRUE.
          MODBND=.TRUE.
          MODLEN=.TRUE.
          call warnmod(ICOMP,'ob+')
        elseif(IVERT.EQ.8)then
  437     HOLD=' '
          WRITE(HOLD,'(1x,3f8.3)')XBP(ICOMP,IB,7),YBP(ICOMP,IB,7),
     &      ZBP(ICOMP,IB,7)
          WRITE(MSG,'(a,I3,a)')'For block ',IB,' upper back right: '
          CALL EASKS(HOLD,MSG,' X(m) Y(m) Z(m) (see help)',
     &      32,' 1.0 1.0 1.0  ','upper back right',IER,nbhelp)
          K=0
          CALL EGETWR(HOLD,K,VX,-99.9,99.9,'W','obs v7 X',IER)
          CALL EGETWR(HOLD,K,VY,-99.9,99.9,'W','obs v7 Y',IER)
          CALL EGETWR(HOLD,K,VZ,-99.9,99.9,'W','obs v7 Z',IER)
          if(ier.ne.0)goto 437
          XBP(ICOMP,IB,7)=VX
          YBP(ICOMP,IB,7)=VY
          ZBP(ICOMP,IB,7)=VZ
          MODIFY=.TRUE.
          MODBND=.TRUE.
          MODLEN=.TRUE.
          call warnmod(ICOMP,'ob+')
        elseif(IVERT.EQ.9)then
  438     HOLD=' '
          WRITE(HOLD,'(1x,3f8.3)')XBP(ICOMP,IB,8),YBP(ICOMP,IB,8),
     &      ZBP(ICOMP,IB,8)
          WRITE(MSG,'(a,I3,a)')'For block ',IB,' upper back left: '
          CALL EASKS(HOLD,MSG,' X(m) Y(m) Z(m) (see help)',
     &      32,' 1.0 1.0 1.0  ','upper back left',IER,nbhelp)
          K=0
          CALL EGETWR(HOLD,K,VX,-99.9,99.9,'W','obs v8 X',IER)
          CALL EGETWR(HOLD,K,VY,-99.9,99.9,'W','obs v8 Y',IER)
          CALL EGETWR(HOLD,K,VZ,-99.9,99.9,'W','obs v8 Z',IER)
          if(ier.ne.0)goto 438
          XBP(ICOMP,IB,8)=VX
          YBP(ICOMP,IB,8)=VY
          ZBP(ICOMP,IB,8)=VZ
          MODIFY=.TRUE.
          MODBND=.TRUE.
          MODLEN=.TRUE.
          call warnmod(ICOMP,'ob+')
        elseif(IVERT.EQ.11)then
          S12=BLOCKNAME(ICOMP,IB)
          CALL EASKS(S12,' ',' Name of obstruction? ',
     &      12,'obstruction','Block name',IER,nbhelp)
          IF(S12(1:2).NE.'  ')then
            BLOCKNAME(ICOMP,IB)=S12
            LNBLOCKNAME(ICOMP,IB)=lnblnk(S12)
          endif
          MODIFY=.TRUE.
        ELSEIF(IVERT.EQ.12)THEN

C Note: this logic only picks up the first 12 char of construction.
          CALL EPMENSV
          CALL EPKMLC(ISEL,
     &      'Select an OPAQUE construction from the list to',
     &      'associate with the block for visualisation purposes.',IER)
          CALL EPMENRC
          IF(ISEL.GT.0)then
            WRITE(BLOCKMAT(icomp,IB),'(A)')DESC(ISEL)(1:12)
            LNBLOCKMAT(icomp,IB)=lnblnk(BLOCKMAT(icomp,IB))
            MODIFY=.TRUE.
          endif
          IF(IER.EQ.1)THEN
            CALL USRMSG(' ',
     &     'A problem was encountered with the block construction','W')
          ENDIF
        ELSEIF(IVERT.EQ.13)THEN
 446      VX=OPOB(ICOMP,IB)
          CALL EASKR(VX,MSG,' Block opacity (see help)? ',
     &       0.0,'W',1.0,'W',1.0,'block opacity',IER,nbhelp)
          if(ier.ne.0)goto 446
          OPOB(icomp,IB)=VX
          MODIFY=.TRUE.
        ELSEIF(IVERT.EQ.19)THEN

C Jump to previous obstruction.
          ianother= -1
          return
        ELSEIF(IVERT.EQ.20)THEN

C Jump to next obstruction.
          ianother=1
          return
        ELSEIF(IVERT.EQ.21)THEN
          CALL PHELPD('obstr menu',48,'-',0,0,IER)
        ELSE
          GOTO 92
        ENDIF
        GOTO 92
      endif

      END

C ********************* EREVEAL 
C EREVEAL Composes a window reveal out of obstruction blocks. Assumes 
C that common block G1,GS5,GS6 are current.

C STEP 1 - Find equation of surface IRS (EQN(4)) via EPLNEQN and get centre
C          Centre of Gravity (VP(3))
C STEP 2 - Set up Eye Point normal to plane at C. of G. (EP(3))
C STEP 3 - Find matrix and reverse matrix via EYEMAT to transform window
C          points to normal view.
C STEP 4 - Find co-ordinates of 'origin' as transformed via ORTTRN to
C          to normal view and then the corners, width and height.
C STEP 5 - Displace corners for obstruction blocks.
C STEP 6 - Apply reverse transformation to obstruction corners via
C          ORTTRN to give vertices in global co-ords.
C STEP 7 - Derive new obstructions.
C IRS is the selected surface, AZI is its asimuth, RTK is the thickness
C of the reveal (adj wall), OWID is the width of the obstruction block.
      SUBROUTINE EREVEAL(ICOMP,IRS,azim,elev,RTK,OWID,blkroot,mat,IER)
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "esprdbfile.h"
      
      integer lnblnk  ! function definition

C Parameters
      integer icomp   ! zone index
      integer irs     ! selected surface
      real azim       ! azimuth of parent surface
      real elev       ! elevation of parent surface
      real RTK        ! projection (m) of the reveal
      real OWID       ! width of the reveal
      character blkroot*8  ! root name of the set of obstructions
      character mat*12  ! first 12 characters of obstruction construction
      integer IER     ! if zero ok

      COMMON/OUTIN/IUOUT,IUIN

      DIMENSION  XX(MV),YY(MV),ZZ(MV),XXW(4),YYW(4),ZZW(4)
      DIMENSION  TMAT(4,4),RMAT(4,4),VP(3),EP(3),EQN(4)

      logical clx0,cly0,clx1,cly1,clxm1,clym1

C Note: mat is only passed as the first 12 char of construction.
      CHARACTER HOLDS*36
      logical newgeo  ! to use for testing if new/old geometry file.
      integer nbo     ! local counter

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='EREVEAL'  ! set for subroutine

C Check if ok to add 4 obstructions.
      IUF=IPRODB
      if(nbobs(icomp)+4.GT.MB)then
        call usrmsg('Reveal not added, run out of obstructions',
     &    'in this zone. ','W')
        return
      endif

      newgeo=.false.  ! assume older format geometry.
      call eclose(gversion(icomp),1.1,0.01,newgeo)

C Create working vertices of the selected surface.
C Cases for 4 sided surfaces and user defined corners.
      helptopic='window_reveal'
      call gethelptext(helpinsub,helptopic,nbhelp)
      if(NVER(IRS).ne.4)then
        CALL EASKAB('The selected surface doesn`t have 4 sides.',
     &    'Choices: ','specify 4 corners','cancel',IW,nbhelp)
        if(IW.eq.1)then
          write(HOLDS,'(4I5)') JVN(IRS,1),JVN(IRS,2),JVN(IRS,3),
     &      JVN(IRS,4)
 153      CALL EASKS(HOLDS,' 4 vertices defining corners: ',' ',
     &      36,' 1 2 3 4 ','corners',IER,nbhelp)
          K=0
          CALL EGETWI(HOLDS,K,ICOR1,1,NTV,'W','cor 1',IER)
          CALL EGETWI(HOLDS,K,ICOR2,1,NTV,'W','cor 2',IER)
          CALL EGETWI(HOLDS,K,ICOR3,1,NTV,'W','cor 3',IER)
          CALL EGETWI(HOLDS,K,ICOR4,1,NTV,'W','cor 4',IER)
          if(ier.ne.0)goto 153
          N = 4
          XX(1) = X(icor1)
          YY(1) = Y(icor1)
          ZZ(1) = Z(icor1)
          XX(2) = X(icor2)
          YY(2) = Y(icor2)
          ZZ(2) = Z(icor2)
          XX(3) = X(icor3)
          YY(3) = Y(icor3)
          ZZ(3) = Z(icor3)
          XX(4) = X(icor4)
          YY(4) = Y(icor4)
          ZZ(4) = Z(icor4)
        else
          return
        endif
      else
        N = NVER(IRS)
        DO 150 J = 1,N
          XX(J) = X(JVN(IRS,J))
          YY(J) = Y(JVN(IRS,J))
          ZZ(J) = Z(JVN(IRS,J))
  150   CONTINUE
      endif

C Offset is thickness + 5mm
      vdis= 0.005+RTK

C Find transformation matrices that normalise face.
      call PLEQN(XX,YY,ZZ,N,VP,EQN,IERR)
      IF (IERR .LT. 0)  GOTO  100
      DO 250 J = 1,3
        EP(J) = VP(J) + EQN(J)
  250 CONTINUE
      CALL  EYEMAT(EP,VP,1.0,TMAT,RMAT)

C Transform all points in surface and find lower left corner and upper
C right.  DDX is width, DDZ is height
      XMIN=0.0
      YMIN=0.0
      XMAX=0.0
      YMAX=0.0
      DO 300 I=1,N
        CALL ORTTRN(XX(I),YY(I),ZZ(I),TMAT,X1,Y1,ZZZ,IERR)
        IF(X1.LT.XMIN)XMIN=X1
        IF(Y1.LT.YMIN)YMIN=Y1
        IF(X1.GT.XMAX)XMAX=X1
        IF(Y1.GT.YMAX)YMAX=Y1
  300 CONTINUE
      DDX=XMAX-XMIN
      DDZ=YMAX-YMIN

      XXW(1)=XMIN-OWID
      YYW(1)=YMIN-OWID
      XXW(2)=XMIN-OWID
      YYW(2)=YMAX
      XXW(3)=XMIN-OWID
      YYW(3)=YMIN-OWID
      XXW(4)=XMAX
      YYW(4)=YMIN-OWID

C Take each window or door and apply transformation first shifting
C the Z point by vdis.
      ZZZ=ZZZ-vdis
      DO 350 K = 1,4
        CALL  ORTTRN(XXW(K),YYW(K),ZZZ,RMAT,XX1,YY1,ZZ1,IERR)
        XXW(K) = XX1
        YYW(K) = YY1
        ZZW(K) = ZZ1
  350 CONTINUE

C Find orientation of surface and then the orientation of the blocks.
C Find which quadrant.
      call AZ2UV(azim,elev,vdx,vdy,vdz)

C Check if tollerably close to an axis.
      CALL ECLOSE(vdx,0.0,0.001,clx0)
      CALL ECLOSE(vdy,0.0,0.001,cly0)
      CALL ECLOSE(vdx,1.0,0.001,clx1)
      CALL ECLOSE(vdy,1.0,0.001,cly1)
      CALL ECLOSE(vdx,-1.0,0.001,clxm1)
      CALL ECLOSE(vdy,-1.0,0.001,clym1)
      if(clx0.and.cly1)then
        RO=180.0
      elseif(clx1.and.cly0)then
        RO=90.0
      elseif(clx0.and.clym1)then
        RO= 0.0
      elseif(clxm1.and.cly0)then
        RO= (-90.0)
      elseif(vdx.gt.0.0.and.vdy.gt.0.0)then
        RO= 180.0 - azim
      elseif(vdx.gt.0.0.and.vdy.lt.0.0)then
        RO= 180.0 - azim
      elseif(vdx.lt.0.0.and.vdy.lt.0.0)then
        RO= 180.0 - azim
      elseif(vdx.lt.0.0.and.vdy.gt.0.0)then
        RO = (azim - 180.0) * (-1.)
      endif

C Create the sill obstruction.
      nbobs(icomp)=nbobs(icomp)+1
      nbo=nbobs(icomp)
      XOB(icomp,nbo)=XXW(1)
      YOB(icomp,nbo)=YYW(1)
      ZOB(icomp,nbo)=ZZW(1)
      DXOB(icomp,nbo)=DDX+OWID+OWID
      DYOB(icomp,nbo)=RTK
      DZOB(icomp,nbo)=OWID
      BANGOB(icomp,nbo,1)=RO
      BANGOB(icomp,nbo,2)=0.0
      BANGOB(icomp,nbo,3)=0.0
      write(BLOCKNAME(icomp,nbo),'(a,a1)')
     &  blkroot(1:lnblnk(blkroot)),'s'
      LNBLOCKNAME(icomp,nbo)=lnblnk(BLOCKNAME(icomp,nbo))
      write(BLOCKMAT(icomp,nbo),'(a)') mat(1:lnblnk(mat))
      LNBLOCKMAT(icomp,nbo)=lnblnk(mat)
      BLOCKTYP(icomp,nbo)='obs '

C If newer geometry file it may include obstructions.
      if(newgeo)then
        call geowrite(IUF,LGEOM(ICOMP),ICOMP,iuout,3,IER)
      else
        CALL MKGOMST(IUF,ZOBS(ICOMP),ICOMP,IER)
      endif

C Create the head obstruction.
      nbobs(icomp)=nbobs(icomp)+1
      nbo=nbobs(icomp)
      XOB(icomp,nbo)=XXW(2)
      YOB(icomp,nbo)=YYW(2)
      ZOB(icomp,nbo)=ZZW(2)
      DXOB(icomp,nbo)=DDX+OWID+OWID
      DYOB(icomp,nbo)=RTK
      DZOB(icomp,nbo)=OWID
      BANGOB(icomp,nbo,1)=RO
      BANGOB(icomp,nbo,2)=0.0
      BANGOB(icomp,nbo,3)=0.0
      write(BLOCKNAME(icomp,nbo),'(a,a1)')
     &  blkroot(1:lnblnk(blkroot)),'h'
      LNBLOCKNAME(icomp,nbo)=lnblnk(BLOCKNAME(icomp,nbo))
      write(BLOCKMAT(icomp,nbo),'(a)') mat(1:lnblnk(mat))
      LNBLOCKMAT(icomp,nbo)=lnblnk(mat)
      BLOCKTYP(icomp,nbo)='obs '
      if(newgeo)then
        call geowrite(IUF,LGEOM(ICOMP),ICOMP,iuout,3,IER)
      else
        CALL MKGOMST(IUF,ZOBS(ICOMP),ICOMP,IER)
      endif

C Create the left side fin.
      nbobs(icomp)=nbobs(icomp)+1
      nbo=nbobs(icomp)
      XOB(icomp,nbo)=XXW(3)
      YOB(icomp,nbo)=YYW(3)
      ZOB(icomp,nbo)=ZZW(3)
      DXOB(icomp,nbo)=OWID
      DYOB(icomp,nbo)=RTK
      DZOB(icomp,nbo)=DDZ+OWID+OWID
      BANGOB(icomp,nbo,1)=RO
      BANGOB(icomp,nbo,2)=0.0
      BANGOB(icomp,nbo,3)=0.0
      write(BLOCKNAME(icomp,nbo),'(a,a1)')
     &  blkroot(1:lnblnk(blkroot)),'l'
      LNBLOCKNAME(icomp,nbo)=lnblnk(BLOCKNAME(icomp,nbo))
      write(BLOCKMAT(icomp,nbo),'(a)') mat(1:lnblnk(mat))
      LNBLOCKMAT(icomp,nbo)=lnblnk(mat)
      BLOCKTYP(icomp,nbo)='obs '
      if(newgeo)then
        call geowrite(IUF,LGEOM(ICOMP),ICOMP,iuout,3,IER)
      else
        CALL MKGOMST(IUF,ZOBS(ICOMP),ICOMP,IER)
      endif

C Create the right side fin.
      nbobs(icomp)=nbobs(icomp)+1
      nbo=nbobs(icomp)
      XOB(icomp,nbo)=XXW(4)
      YOB(icomp,nbo)=YYW(4)
      ZOB(icomp,nbo)=ZZW(4)
      DXOB(icomp,nbo)=OWID
      DYOB(icomp,nbo)=RTK
      DZOB(icomp,nbo)=DDZ+OWID+OWID
      BANGOB(icomp,nbo,1)=RO
      BANGOB(icomp,nbo,2)=0.0
      BANGOB(icomp,nbo,3)=0.0
      write(BLOCKNAME(icomp,nbo),'(a,a1)')
     &  blkroot(1:lnblnk(blkroot)),'r'
      LNBLOCKNAME(icomp,nbo)=lnblnk(BLOCKNAME(icomp,nbo))
      write(BLOCKMAT(icomp,nbo),'(a)') mat(1:lnblnk(mat))
      LNBLOCKMAT(icomp,nbo)=lnblnk(mat)
      BLOCKTYP(icomp,nbo)='obs '
      if(newgeo)then
        call geowrite(IUF,LGEOM(ICOMP),ICOMP,iuout,3,IER)
      else
        CALL MKGOMST(IUF,ZOBS(ICOMP),ICOMP,IER)
      endif

  100 CONTINUE

      RETURN
      END

