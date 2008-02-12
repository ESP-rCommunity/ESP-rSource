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

C edobs.f contains code to allow the editing of site obstructions files.

C ************* EDOBS 
C EDOBS: Edit obstruction blocks if there is an obstruction file
C associated with this zone. If one does not exist then allow user
C to create one.
C Use file unit IPRODB temporarily.
      SUBROUTINE EDOBS(ITRC,ITRU,ICOMP,IER)
#include "building.h"
#include "epara.h"
#include "esprdbfile.h"
C esprdbfile.h supplies the following:
C LPRFDB,IPRODB (for event profile database)

      common/pophelp/h(60)
      COMMON/G6/SSNAME(MCON),SSOTF(MCON),SSMLCN(MCON),SSVFC(MCON),
     &          SSOTHER(MCON,3),SSPARENT(MCON),SSUSE(MCON,2)
      COMMON/GS5/NB,XO(MB),YO(MB),ZO(MB),DX(MB),DY(MB),DZ(MB),BANG(MB)
      COMMON/GS6/NOX,NOZ,NGX,NGZ,BLKNAME(MB),BLKMAT(MB)
      COMMON/G7/SSNA(MCON),SSPAZI(MCON),SSPELV(MCON),SSPERIM(MCON),
     &          SSUREQN(MCON,4),SSURCOG(MCON,3),SSURVN(MCON,3)
      COMMON/precz/zname(MCOM),zdesc(MCOM)
      COMMON/INDICS/IVF(MCOM),ISI(MCOM),IHC(MCOM),
     &              ITW(MCOM),ICGC(MCOM),IOBS(MCOM)
      common/UDESC/LVIEW(MCOM),LHCCO(MCOM),
     &             LTWIN(MCOM),LCGCIN(MCOM),ZOBS(MCOM)
      COMMON/PMENU/MHEAD,MCTL,MIFULL,MFULL,IST,ILEN,IPM,MPM,IPFLG
      COMMON/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)
      COMMON/C24/IZSTOCN(MCOM,MS)
      COMMON/MLC/NMLC,DESC(MMLC),DTHK(MMLC,ME),IPR(MMLC,ME),
     &           LAYERS(MMLC),DRAIR(MMLC,ME,3)
      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)
      COMMON/RAY2/ITDSP,ITBND,ITEPT,ITZNM,ITSNM,ITVNO,ITORG,ITSNR,
     &            ITOBS,ITHLS,ITHLZ,ITGRD,GRDIS,ITPPSW
      COMMON/RAY3/MODIFY,MODLEN,MODBND
      common/paths/zonepth,netpth,ctlpth,imgpth,radpth,docpth,tmppth,
     &             dbspth
      common/pmchange/comold,tmcold,vwfold,ishold

      dimension IVALB(MB)
      CHARACTER*72 LSNAM,LPROJ,LGEOM,LSHAD,LTHRM,LUTIL
      CHARACTER*72 LVIEW,LHCCO,LTWIN,LCGCIN,ZOBS
      CHARACTER*33 VERT(35)
      character*30 PICK(MB)
      CHARACTER SSMLCN*32,SSVFC*4,SSOTF*32,SSOTHER*24,SSNAME*12,SSUSE*8
      character SSPARENT*12
      CHARACTER KEY*1,H*72,BLKNAME*8,BLKMAT*12,zname*12,HOLD*24
      character blkroot*6,mat*12,DESC*48,tbn*8
      character holds*36,SFIL*72,DSFIL*72,ZN*12,zdesc*64
      character zonepth*24,netpth*24,ctlpth*24,imgpth*24,radpth*24
      character docpth*24,tmppth*24,dbspth*24
      LOGICAL MODIFY,MODLEN,MODBND,modcfg
      LOGICAL comold,tmcold,vwfold,ishold,silent,unixok

C Check if Unix-based or DOS based.
      call isunix(unixok)

C Check for obstructions. If file exists ask user to confirm
C it or dereference it. If it does not exist ask user for
C a name and or whether it should be ignored.
      modcfg=.false.
      ITOBS = 0
      IUF=IPRODB
      write(zn,'(A)') zname(ICOMP)
      IF(IOBS(ICOMP).EQ.1)THEN
        silent=.false.
        CALL ECROBS(ITRC,ITRU,IUF,ICOMP,silent,IER)
        if(IOBS(ICOMP).eq.0)then
          modcfg=.true.
          call usrmsg(' updating model to remove obstructions...',
     &      ' ','-')
          CALL EMKCFG('-',IER)
          call usrmsg(' updating model to remove obstructions...done.',
     &      ' ','-')
          return
        endif
      ELSEIF(IOBS(ICOMP).NE.1)THEN

C There is no mention of obstructions so get name and create a dummy
C if it doesn't exist. If user dereferenced the iobs will be zero and
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
      ENDIF

C Initial rotation values.
      ANG=0.
      x1=0.
      y1=0.

C Setup for multi-page menu.
   91 MHEAD=4
      MCTL=7
      ILEN=NB
      IPACT=CREATE
      CALL EKPAGE(IPACT)

C Initial menu entry setup.
   92 IER=0
      ILEN=NB
      IVERT=-3

C Loop through the items until the page to be displayed. M is the 
C current menu line index. Build up text strings for the menu as well
C as a list for copy and delete use. 
      M=MHEAD
      DO 10 L=1,ILEN
        WRITE(PICK(L),304)L,BLKNAME(L),BLKMAT(L)
  304   FORMAT(I3,2X,A,1x,A)
        IF(L.GE.IST.AND.(L.LE.(IST+MIFULL)))THEN
          M=M+1
          CALL EMKEY(M,KEY,IER)
          WRITE(VERT(M),303)KEY,L,BLKNAME(L),BLKMAT(L)
  303     FORMAT(A1,I3,2X,A,1x,A)
        ENDIF
   10 CONTINUE

C Present a list of the existing blocks and options.
      WRITE(VERT(1),'(A,2I3)')  'a Surface X&Z grid:',NOX,NOZ
      WRITE(VERT(2),'(A,I3)')   '  No. obstr blocks:',NB
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
      VERT(M+5)  ='> shading & insolation        '
      VERT(M+6)  ='? help                        '
      VERT(M+7)  ='- exit this menu              '

C If a modification has been done then update the obstruction
C file so that changes can be drawn.  This is necessary because
C obstructions are not true zones.
      IF(MODIFY)CALL MKGOMST(IUF,ZOBS(ICOMP),ICOMP,IER)

C If editing the obstructions and user has asked for an update of the
C image then use common block info for the zone as well as for the
C obstructions (ie. set a flag to use obstruction commons rather than
C read of file).
      nzg=1
      nznog(1)=ICOMP
      izgfoc=ICOMP
      CALL ADJVIEW(ITRC,IER)

C Help text for this menu.
      H(1)='In order to account for external shading devices, '
      H(2)='window reveals, trees, outside walls, etc. in a '
      H(3)='detailed shading analysis the user should approx-'
      H(4)='imate such objects as one or more prisms.'
      H(5)=' '
      H(6)='The obstruction composition defaults to NONE, but '
      H(7)='can be set to a multi-layer construction for use in '
      H(8)='visualisation studies. '

C Now display the menu.
      CALL EMENU('Obstructions',VERT,MVERT,IVERT)
      IF(IVERT.EQ.MVERT)THEN
        call sumrchg(ICOMP,'r')
        RETURN
      ELSEIF(IVERT.EQ.(MVERT-1))THEN
        CALL PHELPD('obstr menu',8,'-',0,0,IER)
       ELSEIF(IVERT.EQ.(MVERT-2))THEN

C Allow user to start shading analysis or at least mark preferences.
         H(1)='Having defined obstructions, you must run a separate'
         H(2)='analysis to predice temporal distirbution patterns.'
         CALL EASKABC('Shading & insolation analysis options: ',' ',
     &    'do now','do later','cancel',IW,2)
         if(IW.eq.1.or.IW.eq.2)then
           if(ISI(ICOMP).eq.0)then
             if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
               WRITE(SFIL,'(2a)')ZN(1:lnblnk(ZN)),'.shd'
             else
               WRITE(SFIL,'(4a)') zonepth(1:lnblnk(zonepth)),'/',
     &           ZN(1:lnblnk(ZN)),'.shd'
             endif
             DSFIL = 'new.shd'
             H(1)='The shading/ insulation database holds time '
             H(2)='varying external surface shading and internal'
             H(3)='surface insolation.'
             CALL EASKS(SFIL,' New zone shading/ insulation database?',
     &        ' ',72,DSFIL,'shd/ins db',IER,3)
             if(SFIL(1:2).ne.'  ')then
               LSHAD(ICOMP)=SFIL
               ISI(ICOMP)=1
               call tstamp('>','PRJ: adding shading to configuration')
               CALL EMKCFG('-',IER)
             endif
           endif
         endif
         if(IW.eq.1)then

C Get logical name of terminal type, expand problem name
C to include the path and create a string to drive ish.
           call tstamp('>','PRJ: start shading')

C If existing db out of date the perform recalculation otherwise
C start interactive session.
           if(ishold)then
             call comissionish(icomp,'sra',ier)
           else
             call comissionish(icomp,'in ',ier)
           endif
           ishold=.false.
         endif
       ELSEIF(IVERT.EQ.(MVERT-3))THEN

C Compose a window reveal from 4 obstructions. Ask for which surface,
C then get a root name for obstructions, the get thickness of the
C wall.
        H(1)='For shading and daylighting analysis it is useful to '
        H(2)='use obstruction blocks to represent the thickness  '
        H(3)='of an opening (ie. window reveal). This is automated '
        H(4)='by adding 4 blocks 5mm away from the surface with a '
        H(5)='thickness of the adjacent wall.  '
        H(6)=' '
        H(7)='Each of the blocks will share an root name.  '
        CALL PHELPD('obstr reveal',7,'-',0,0,IER)

        CALL EPMENSV
        CALL EASKSUR(ICOMP,IRS,'-','Which surface should the reveal',
     &    'be build around (a vertical transparent surface).',IER)
        CALL EPMENRC
        if(IRS.ne.0)then
          icc=izstocn(ICOMP,IRS)
          write(blkroot,'(a)')SSNAME(icc)(1:6)
          H(1)='This root name will be added to for each of the 4 '
          H(2)='obstructions: nglz -> nglzs, nglzh, nglzl, nglzr '
          H(3)='where nglzs is the sill, nglzh is the head, nglzl and'
          H(4)='nglzr are the left and right fins (seen from outside)'
          CALL EASKS(blkroot,' Root name for related obstructions?',
     &      ' ',6,'revl  ','reveal root name',IER,4)

          H(1)='Thickness associated with surrounding wall (m). Check'
          H(2)='the multi-layer construction for this. '
          CALL EASKR(RTK,' ',' Thickness of the wall (m)?',
     &      0.0,'W',1.0,'W',0.2,'thickness',IER,2)

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
          CALL ADJVIEW(ITRC,IER)
        endif
      ELSEIF(IVERT.EQ.(MVERT-4))THEN

C Rotate an obstruction.
        H(1)='Choose one or more blocks to rotate or transform'
        H(2)='within the site coordinate system.'
        CALL EASKABC(' Choices:',' ','rotate','transform',
     &     'continue',IBOPT,2)
        if(IBOPT.eq.1)then
          CALL EPMENSV
          INPIC=NB
          H(1)='Choose one or more blocks to rotate.'
          CALL EPICKS(INPIC,IVALB,' ',' Which obstructions to rotate:',
     &      30,NB,PICK,' block list',IER,1)
          CALL EPMENRC
          H(1)='The rotation is applied at the obstruction origin,'
          H(2)='with the anticlockwise direction positive. '
          CALL EASKR(ANG,' ',' Rotation (degrees, anticlockwise +ve)?',
     &      -359.0,'W',359.0,'W',0.0,'rotation',IER,2)
          if(ANG.LT.-.01.OR.ANG.GT..01)then

C Rotation choices.
            H(1)='Rotate obstruction about the site origin or about'
            H(2)='a specified point.  To rotate about the '
            H(3)='obstruction origin use the edit option.'
            CALL EASKAB(' Rotate about the:',' ',
     &        'site origin','user specified point',IW,3)
            if(IW.eq.1)then
              X1 = 0.
              Y1 = 0.
            elseif(IW.eq.2)then
              H(1)='Point is in the site coordinate system.'
              CALL EASKR(x1,' ',' X coordinate (m)?',
     &            0.0,'-',0.0,'-',0.0,'x point',IER,1)
              CALL EASKR(y1,' ',' Y coordinate (m)?',
     &            0.0,'-',0.0,'-',0.0,'y point',IER,1)
            endif
            PI = 4.0 * ATAN(1.0)
            A=-ANG*PI/180.
            CA=COS(A)
            SA=SIN(A)
            do 86 ij=1,INPIC
              IFOC=IVALB(ij)
              XXX=XO(IFOC)-X1
              YYY=YO(IFOC)-Y1
              XR=XXX*CA+YYY*SA
              YR=YYY*CA-XXX*SA
              XO(IFOC)=XR+X1
              YO(IFOC)=YR+Y1
              BANG(IFOC)=BANG(IFOC)+ANG
  86        continue
            MODIFY=.TRUE.
            call warnmod(ICOMP,'ob+')
          endif
        elseif(IBOPT.eq.2)then
          CALL EPMENSV
          INPIC=NB
          H(1)='Choose one or more blocks to transform.'
          CALL EPICKS(INPIC,IVALB,' ','Transform which obstructions:',
     &        30,NB,PICK,' block list',IER,1)
          CALL EPMENRC
          HOLDS= ' 0.  0.  0. '
          H(1)='Offsets are from block origin in metres. '
 152      CALL EASKS(HOLDS,' X Y & Z offsets: ',' ',
     &      36,' 0. 0. 0. ','offsets',IER,1)
          K=0
          CALL EGETWR(HOLDS,K,VALX,-50.0,50.0,'W','X off',IER)
          CALL EGETWR(HOLDS,K,VALY,-50.0,50.0,'W','Y off',IER)
          CALL EGETWR(HOLDS,K,VALZ,-50.0,50.0,'W','Z off',IER)
          if(ier.ne.0)goto 152
          do 87 ij=1,INPIC
            IFOC=IVALB(ij)
            XO(IFOC)=XO(IFOC)+VALX
            YO(IFOC)=YO(IFOC)+VALY
            ZO(IFOC)=ZO(IFOC)+VALZ
  87      continue
          MODIFY=.TRUE.
          call warnmod(ICOMP,'ob+')
        else
          GOTO 92
        endif
      ELSEIF(IVERT.EQ.(MVERT-5))THEN

C +- Obstruction.
        h(1)='Use this to add / delete / copy obstructions. '
        CALL EASKATOG('Modify obstruction list:',' ',
     &    'add','delete','copy','continue',' ',' ',' ',IW,1)
        IF(IW.EQ.2)THEN

C Build up text strings for the delete menu. 
          CALL EPMENSV
          INPIC=MAX0(NB-2,4)
          H(1)='Choose block(s) to delete.'
          CALL EPICKS(INPIC,IVALB,' ','Delete which obstruction(s):',
     &        30,NB,PICK,' delete options',IER,1)
          CALL EPMENRC
          if(inpic.gt.0)then
            do 143 ijb=1,INPIC
              ID=IVALB(ijb)
              IF(NB.LT.2)then

C To delete the last obstruction, dereference the file and exit.
                ZOBS(ICOMP)=' '
                IOBS(ICOMP)=0
                call usrmsg(
     &            ' updating model to remove final obstruction...',
     &            ' ','-')
                CALL EMKCFG('-',IER)
                call usrmsg(
     &            ' updating model to remove final obstruction...done.',
     &            ' ','P')
                return
              endif
              if(ID.eq.0)GOTO 92
              DO 791 IDV=ID,NB-1
                XO(IDV)=XO(IDV+1)
                YO(IDV)=YO(IDV+1)
                ZO(IDV)=ZO(IDV+1)
                DX(IDV)=DX(IDV+1)
                DY(IDV)=DY(IDV+1)
                DZ(IDV)=DZ(IDV+1)
                BANG(IDV)=BANG(IDV+1)
                BLKNAME(IDV)=BLKNAME(IDV+1)
                BLKMAT(IDV)=BLKMAT(IDV+1)
  791         CONTINUE
              NB=NB-1
  143       continue
          endif
          MODIFY=.TRUE.
          call warnmod(ICOMP,'ob-')
        ELSEIF(IW.EQ.1)THEN
          IF(NB+1.LE.MB)THEN
            NB=NB+1
            XO(NB)=1.0
            YO(NB)=1.0
            ZO(NB)=0.0
            DX(NB)=1.0
            DY(NB)=1.0
            DZ(NB)=1.0
            BANG(NB)=0
            BLKNAME(NB)='new_blk'
            BLKMAT(NB)='NONE'
            MODIFY=.TRUE.
            call warnmod(ICOMP,'ob+')
            CALL MKGOMST(IUF,ZOBS(ICOMP),ICOMP,IER)
            CALL ADJVIEW(ITRC,IER)
          ELSE
            CALL USRMSG(' ',' Too many obstructions defined!','W')
            GOTO 92
          ENDIF

C Edit the new block, first hilight the obstruction.
          IB=NB
          ITOBS = IB
          CALL ADJVIEW(ITRC,IER)
          CALL EPMENSV
          CALL EDOBSB(ITRC,ITRU,IUF,ICOMP,IB,IER)
          CALL EPMENRC
          MODIFY=.TRUE.
          call warnmod(ICOMP,'ob+')
          GOTO 91
        elseif(IW.EQ.3)THEN

C Copy obstruction.
          CALL EPMENSV
          INPIC=1
          H(1)='Choose a block to copy.'
          CALL EPICKS(INPIC,IVALB,' ','Copy which obstruction:',
     &      30,NB,PICK,' copy options',IER,1)
          CALL EPMENRC
          IFOC=IVALB(1)
          if(NB+1.LE.MB.and.IFOC.ne.0)then
            NB=NB+1
 244        write(tbn,'(a)') BLKNAME(IFOC)
            H(1)='Supply a unique block name for this copy (perhaps'
            H(2)='modifying thenname of the original block) '
            CALL EASKS(tbn,' Re-name the obstruction copy:',
     &        '(original block name shown)',
     &        8,'block','block name',IER,2)
            if(tbn(1:8).ne.BLKNAME(IFOC)(1:8))then
              write(BLKNAME(NB),'(a)') tbn
            else
              call usrmsg('The block name must be unique',
     &                    'please supply a different name','W')
              goto 244
            endif

C Ask for transform distance for obstruction and then apply.
            H(1)='Transform shifts a solar obstruction origin'
            H(2)='by a given amount in X Y Z axis. Leave these'
            H(3)='at zero if copy should overlay the original. '
            holds = ' 0.00  0.00  0.00 '
 243        CALL EASKS(HOLDS,'Transform obstruction (X Y Z metres):',
     &        ' ',36,' 0.0 0.0 0.0','transforms XYZ',IER,3)
            K=0
            CALL EGETWR(HOLDS,K,VALX,-99.,99.,'W','X tr',IER)
            CALL EGETWR(HOLDS,K,VALY,-99.,99.,'W','Y tr',IER)
            CALL EGETWR(HOLDS,K,VALZ,-99.,99.,'W','Z tr',IER)
            if(ier.ne.0)goto 243
            XO(NB)=XO(IFOC)+VALX
            YO(NB)=YO(IFOC)+VALY
            ZO(NB)=ZO(IFOC)+VALZ
            DX(NB)=DX(IFOC)
            DY(NB)=DY(IFOC)
            DZ(NB)=DZ(IFOC)
            BANG(NB)=BANG(IFOC)
            BLKMAT(NB)=BLKMAT(IFOC)
            MODIFY=.TRUE.
            call warnmod(ICOMP,'ob+')
            CALL MKGOMST(IUF,ZOBS(ICOMP),ICOMP,IER)
            CALL ADJVIEW(ITRC,IER)
            call cfgtogg(icfg_type,icfgz,icfgn,icfgc,icfgdfn,
     &                             iicfgz,iicfgn,iicfgc,iicfgdfn)

C Edit the copied block.
            IB=NB
            CALL EPMENSV
            CALL EDOBSB(ITRC,ITRU,IUF,ICOMP,IB,IER)
            CALL EPMENRC
            MODIFY=.TRUE.
            GOTO 91
          endif
        elseif(IW.EQ.4)THEN
          GOTO 92
        ENDIF
      ELSEIF(IVERT.EQ.(MVERT-6))THEN

C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        ENDIF
      ELSEIF(IVERT.EQ.1)THEN

C Surface X & Z grid density. 
        H(1)='In order to determine the shading distribution '
        H(2)='a grid is superimposed on each surface.  This '
        H(3)='may be between 4 and 20 points in each direction.'
        H(4)='Detailed grids require more processing time but'
        H(5)='will allow greater resolution, especially for'
        H(6)='complex surfaces or those with thin edges.'
  42    HOLD=' '
        WRITE(HOLD,'(1x,2i5)')NOX,NOZ
        CALL EASKS(HOLD,' Surface X & Z grid density:',' ',
     &     24,' 20 20  ','surf grid',IER,6)
        K=0
        CALL EGETWI(HOLD,K,NOX,4,MOX,'W','surf x grid',IER)
        CALL EGETWI(HOLD,K,NOZ,4,MOZ,'W','surf z grid',IER)
        if(ier.ne.0)goto 42
        MODIFY=.TRUE.
        call warnmod(ICOMP,'ob+')
      ELSEIF(IVERT.GT.MHEAD.AND.IVERT.LT.(MVERT-MCTL+1))THEN

C Edit block identified by KEYIND.
        CALL KEYIND(MVERT,IVERT,IFOC,IO)
        IB=IFOC
        if(IB.ne.0)then
          CALL EPMENSV
          CALL EDOBSB(ITRC,ITRU,IUF,ICOMP,IB,IER)
          CALL EPMENRC
        endif
      ENDIF
      IVERT=-4
      GOTO 92

      END

C ************* ECROBS 
C ECROBS: Read in existing obstruction file or create a fresh one.
C If silent is true then either create or deference silently.
      SUBROUTINE ECROBS(ITRC,ITRU,IUF,ICOMP,silent,IER)
#include "building.h"
      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/GS5/NB,XO(MB),YO(MB),ZO(MB),DX(MB),DY(MB),DZ(MB),BANG(MB)
      COMMON/GS6/NOX,NOZ,NGX,NGZ,BLKNAME(MB),BLKMAT(MB)
      common/UDESC/LVIEW(MCOM),LHCCO(MCOM),
     &             LTWIN(MCOM),LCGCIN(MCOM),ZOBS(MCOM)
      COMMON/INDICS/IVF(MCOM),ISI(MCOM),IHC(MCOM),
     &              ITW(MCOM),ICGC(MCOM),IOBS(MCOM)
      COMMON/precz/zname(MCOM),zdesc(MCOM)
      COMMON/RAY3/MODIFY,MODLEN,MODBND
      common/paths/zonepth,netpth,ctlpth,imgpth,radpth,docpth,tmppth,
     &             dbspth

      CHARACTER H*72,BLKNAME*8,BLKMAT*12,LTMP*72,OFILE*72
      character zname*12,zdesc*64
      CHARACTER*72 LVIEW,LHCCO,LTWIN,LCGCIN,ZOBS
      character zonepth*24,netpth*24,ctlpth*24,imgpth*24,radpth*24
      character docpth*24,tmppth*24,dbspth*24
      character sfile*72,snpfile*72,fs*1
      LOGICAL XST,MODIFY,MODLEN,MODBND,unixok,silent,makedummy

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

C Ask for the obstruction file, if it exists then read it in, if not
C found create one with default information. In either case offer
C the choice to dereference or ignore obstructions.
      if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
        WRITE(OFILE,'(A,A4)')zname(ICOMP)(1:lnblnk(zname(ICOMP))),'.obs'
      else
        WRITE(OFILE,'(3A,A4)') zonepth(1:lnblnk(zonepth)),fs,
     &    zname(ICOMP)(1:lnblnk(zname(ICOMP))),'.obs'
      endif

C If silent is true create standard name. If this matches an
C existing file use it, otherwise add a dummy obstruction file silently.
      if(silent)then
        makedummy=.false.
        if(ZOBS(ICOMP)(1:2).eq.'  '.or.ZOBS(ICOMP)(1:4).eq.'UNKN')then
          ZOBS(ICOMP)=OFILE
          call FINDFIL(OFILE,XST)
          if(XST)then
            CALL EGOMST(IUF,ZOBS(ICOMP),0,ITRC,ITRU,IER)
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
          call FINDFIL(ZOBS(ICOMP),XST)
          if(XST)then
            CALL EGOMST(IUF,ZOBS(ICOMP),0,ITRC,ITRU,IER)
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

        if(makedummy)then

C If we need to make a dummy obstruction file do this.
          NB=1
          XO(1)=1.0
          YO(1)=1.0
          ZO(1)=0.0
          DX(1)=1.0
          DY(1)=1.0
          DZ(1)=1.0
          BANG(1)=0
          BLKNAME(1)='First'
          BLKMAT(1)='NONE'
          NOX=20
          NOZ=20
          NGX=10
          NGZ=10
          CALL MKGOMST(IUF,ZOBS(ICOMP),ICOMP,IER)
          IF(IER.eq.0)then
            IOBS(ICOMP)=1
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
 
      H(1)='In order to account for external shading devices,'
      H(2)='window reveals, trees, outside walls, etc. in a '
      H(3)='detailed shading analysis the user should'
      H(4)='approximate such objects as one or more prisms' 
      H(5)='which are described in a site obstruction file.'
      H(6)=' '
      H(7)='The browse option presents a list of files in the '
      H(8)='project `zones` folder.'
      H(9)=' '
      H(10)='The ignore/dereference option will return you to the zone'
      H(11)='geometry menu without changing the model.'
      if(ZOBS(ICOMP)(1:2).eq.'  '.or.ZOBS(ICOMP)(1:4).eq.'UNKN')then
        LTMP=OFILE
        CALL EASKS2CMD(LTMP,'Zone obstructions file (see help):',' ',
     &    'ignore','browse',iclkok,72,OFILE,'obstruction file',IER,11)
        if(iclkok.eq.1)then
          IOBS(ICOMP)=0
          ZOBS(ICOMP)=' '
          return
        endif
      else
        LTMP=ZOBS(ICOMP)
        CALL EASKS2CMD(LTMP,'Zone obstructions file (see help):',' ',
     &    'dereference','browse',iclkok,72,OFILE,'obstruction file',
     &    IER,7)
        if(iclkok.eq.1)then
          ZOBS(ICOMP)=' '
          IOBS(ICOMP)=0
          return
        endif
      endif
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
     &        ' ',72,OFILE,'obstructions file',IER,5)
          endif
        else
          LTMP=OFILE
          CALL EASKS(LTMP,
     &      ' Obstructions file name: (because browse was blank)',
     &      ' ',72,OFILE,'obstructions file',IER,5)
        endif
        ZOBS(ICOMP)=LTMP
      else
        IF(LTMP(1:2).NE.'  ')ZOBS(ICOMP)=LTMP
      endif
      call FINDFIL(LTMP,XST)
      IF(XST)THEN
        CALL EGOMST(IUF,ZOBS(ICOMP),0,ITRC,ITRU,IER)
        IF(IER.NE.0)RETURN
        IOBS(ICOMP)=1 
      ELSE
          
C Provide default values for the new obstructions file.
        NB=1
        XO(1)=1.0
        YO(1)=1.0
        ZO(1)=0.0
        DX(1)=1.0
        DY(1)=1.0
        DZ(1)=1.0
        BANG(1)=0
        BLKNAME(1)='First'
        BLKMAT(1)='NONE'
        NOX=20
        NOZ=20
        NGX=10
        NGZ=10
        CALL MKGOMST(IUF,ZOBS(ICOMP),ICOMP,IER)
        IF(IER.NE.0)RETURN
        IOBS(ICOMP)=1 
        MODIFY=.TRUE.
      ENDIF
      RETURN
      END

C ************* EDOBSB 
C EDOBSB: Edit details of an obstruction block.
      SUBROUTINE EDOBSB(ITRC,ITRU,IUF,ICOMP,IB,IER)
#include "building.h"
      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      COMMON/GB1/XB(12),YB(12),ZB(12),JVNB(6,4)
      COMMON/GS5/NB,XO(MB),YO(MB),ZO(MB),DX(MB),DY(MB),DZ(MB),BANG(MB)
      COMMON/GS6/NOX,NOZ,NGX,NGZ,BLKNAME(MB),BLKMAT(MB)
      COMMON/MLC/NMLC,DESC(MMLC),DTHK(MMLC,ME),IPR(MMLC,ME),
     &           LAYERS(MMLC),DRAIR(MMLC,ME,3)
      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)
      COMMON/RAY2/ITDSP,ITBND,ITEPT,ITZNM,ITSNM,ITVNO,ITORG,ITSNR,
     &            ITOBS,ITHLS,ITHLZ,ITGRD,GRDIS,ITPPSW
      COMMON/RAY3/MODIFY,MODLEN,MODBND
      common/UDESC/LVIEW(MCOM),LHCCO(MCOM),
     &             LTWIN(MCOM),LCGCIN(MCOM),ZOBS(MCOM)

C ZXMN(),ZYMN(),ZZMN() are the zone minimum bounds.
C ZXMX(),ZYMX(),ZZMX() are the zone maximum bounds.
      COMMON/RAY7/ZXMN(MCOM),ZYMN(MCOM),ZZMN(MCOM),ZXMX(MCOM),
     &            ZYMX(MCOM),ZZMX(MCOM),ZBFLG(MCOM)
      LOGICAL MODIFY,MODLEN,MODBND
      DIMENSION VERT(19),IVLST(MTV)
      CHARACTER VERT*33,MSG*72,H*72,S8*8
      character BLKNAME*8,BLKMAT*12,HOLD*32,DESC*48,outs*124
      CHARACTER*72 LVIEW,LHCCO,LTWIN,LCGCIN,ZOBS

C Initial values for point at angle facility.
      V1=1.0
      AZ=0.0
      EL=0.0

C Initial menu entry setup.
      iz=icomp
      MODIFY=.FALSE.
   92 IER=0
      IVERT=-3

C If a modification has been done then update the obstruction
C file so that changes can be drawn.  This is necessary because
C obstructions are not true zones.
      if(MODIFY)CALL MKGOMST(IUF,ZOBS(ICOMP),ICOMP,IER)

C Extract coordinates for corners for the current block to 
C display in the command menu.
      CALL CNVBLK(XO(IB),YO(IB),ZO(IB),DX(IB),DY(IB),DZ(IB),BANG(IB))
      ITOBS = IB

C Set menu header text.
      WRITE(VERT(1),'(A,3F6.2)')  'a origin X Y Z:',XO(IB),YO(IB),ZO(IB)
      WRITE(VERT(2),'(A,3F6.2)')  'b block  W D H:',DX(IB),DY(IB),DZ(IB)
      WRITE(VERT(3),'(A,F7.2)')   'c rotation    : ',BANG(IB)
      WRITE(VERT(4),'(A,A)')      'd name        : ',BLKNAME(IB)
      WRITE(VERT(5),'(A,A)')      'e construction: ',BLKMAT(IB)
      VERT(6)                  =  '  ____________________________ '
      VERT(7)                  =  '  block coords    X       Y   '
      WRITE(VERT(8),'(A,2F8.3)')  '  front left  :',XB(1),YB(1)
      WRITE(VERT(9),'(A,2F8.3)')  '  front right :',XB(2),YB(2)
      WRITE(VERT(10),'(A,2F8.3)') '  back right  :',XB(3),YB(3)
      WRITE(VERT(11),'(A,2F8.3)') '  back left   :',XB(4),YB(4)
      WRITE(VERT(12),'(A,F8.3,A)')'  top @       :',ZB(5),' (Z)'
      VERT(13)                  = '  ____________________________  '
      VERT(14)                  = '  zone bounds  X    Y     Z     '
      WRITE(VERT(15),'(A,3F8.3)') '  max: ',ZXMX(iz),ZYMX(iz),ZZMX(iz)
      WRITE(VERT(16),'(A,3F8.3)') '  min: ',ZXMN(iz),ZYMN(iz),ZZMN(iz)
      VERT(17)                  = '  ____________________________  '
      VERT(18)                  = '? help                          '
      VERT(19)                  = '- exit                          '

C Display the zone.
      nzg=1
      nznog(1)=ICOMP
      izgfoc=ICOMP
      CALL ADJVIEW(ITRC,IER)

C Help text for this menu.
      H(1)='The specification of obstructions is currently based'
      H(2)='on an obstruction being a prism.  Assuming there is'
      H(3)='no rotation the width of the block is its distance'
      H(4)='along the X axis, the depth is along the Y axis, and'
      H(5)='the height is along the Z axis.  The block may then'
      H(6)='be rotated with the angle being measured anticlockwise'
      H(7)='from the X axis.  Note this is currently a different'
      H(8)='syntax from the rotation of zones.'
      H(9)=' '
      H(10)='To assist in determining the placement of obstruc- '
      H(11)='tion blocks the site coordinates of the 4 corners:'
      H(12)=' Front Left  = Block origin, '
      H(13)=' Front Right = Right side (along initial X axis), '
      H(14)=' Back Right  = Side diagonal from block origin '
      H(15)=' '
      H(16)='The associated zone bounds are also show. '
      H(17)='The construction gives surfaces properties for use'
      H(18)='in visualisation applications. '

C Now display the menu.
      CALL EMENU('Block Details',VERT,19,IVERT)
      IF(IVERT.EQ.19)THEN
        ITOBS = 0
        CALL ADJVIEW(ITRC,IER)
        RETURN
      ELSEIF(IVERT.EQ.1)THEN
  42    H(1)='You can edit the origin of the block, make it the same'
        H(2)='as a vertex in the zone, or the same as a vertex in the'
        H(3)='zone after editing.'
        CALL EASKABCD(' Choices:',' ','edit origin','use zone vertex',
     &     '@ angle&dist from vertex','continue',IBOPT,3)
        if(IBOPT.eq.1)then
          HOLD=' '
          WRITE(HOLD,'(1x,3f9.4)')XO(IB),YO(IB),ZO(IB)
        elseif(IBOPT.eq.2)then
          inpick=1
          CALL EPMENSV
          call EPKVERT(INPICK,IVLST,'Vertices for origin',
     &      'Select a vertex to define the origin...',' ',3,ier)
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
          inpick=1
          CALL EPMENSV
          call EPKVERT(INPICK,IVLST,'Vertice in zone',
     &      'Select a vertex to begin from...',' ',3,ier)
          CALL EPMENRC
          if(inpick.eq.1)then
            iwhich1=IVLST(1)
            write(hold,'(f10.4,f9.3,f8.3)') V1,AZ,EL
            H(1)='Distance is in metres, azimuth assumes north=0 '
            H(2)='+ degrees clockwise, elevation 90 is vertical. '
 342        CALL EASKS(HOLD,
     &      'Distance (m), azimuth (north=0, east=90), elev (vert=90):',
     &      ' ',32,' 1. 0. 0. ','dist azim elev',IER,2)
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
     &        y3+Y(iwhich1),z3+Z(iwhich1)
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
        H(1)='The origin of the block is in site coords. '
        WRITE(MSG,'(a,I3,a)')'For block ',IB,' origin: '
        CALL EASKS(HOLD,MSG,
     &     ' X Y Z coords ',32,' 1.0 1.0 0.0  ','blk org xyz',IER,1)
        K=0
        CALL EGETWR(HOLD,K,XO(IB),-999.0,999.9,'W','blk orgx',IER)
        CALL EGETWR(HOLD,K,YO(IB),-999.0,999.9,'W','blk orgy',IER)
        CALL EGETWR(HOLD,K,ZO(IB),-99.0,999.9,'W','blk orgz',IER)
        if(ier.ne.0)goto 42
        MODIFY=.TRUE.
        MODBND=.TRUE.
        MODLEN=.TRUE.
        call warnmod(ICOMP,'ob+')
      ELSEIF(IVERT.EQ.2)THEN
  43    HOLD=' '
        WRITE(HOLD,'(1x,3f8.3)')DX(IB),DY(IB),DZ(IB)
        H(1)='The width (with no rotation) is along the X axis.'
        WRITE(MSG,'(a,I3,a)')'For block ',IB,' dimensions: '
        CALL EASKS(HOLD,MSG,' width (X), depth (Y), height (Z) ',
     &     32,' 1.0 1.0 1.0  ','blk WDH',IER,1)
        K=0
        CALL EGETWR(HOLD,K,DX(IB),0.001,99.9,'W','blk wid',IER)
        CALL EGETWR(HOLD,K,DY(IB),0.001,99.9,'W','blk dep',IER)
        CALL EGETWR(HOLD,K,DZ(IB),0.001,99.9,'W','blk hgt',IER)
        if(ier.ne.0)goto 43
        MODIFY=.TRUE.
        MODBND=.TRUE.
        MODLEN=.TRUE.
        call warnmod(ICOMP,'ob+')
      ELSEIF(IVERT.EQ.3)THEN
        H(1)='Rotation is anticlockwise degrees between the '
        H(2)='length side of the obstruction and the X axis.'
        CALL EASKR(BANG(IB),MSG,' Block rotation? ',
     &       -359.0,'W',359.0,'W',0.0,'block rotation',IER,2)
        MODIFY=.TRUE.
        MODBND=.TRUE.
        MODLEN=.TRUE.
        call warnmod(ICOMP,'ob+')
      ELSEIF(IVERT.EQ.4)THEN
        H(1)='A descriptive name (up to 8 char) is recommended.'
        S8=BLKNAME(IB)
        CALL EASKS(S8,' ',' Name of obstruction? ',
     &    8,'obstruction','Block name',IER,1)
        IF(S8.NE.' ')BLKNAME(IB)=S8
        MODIFY=.TRUE.
      ELSEIF(IVERT.EQ.5)THEN
        CALL EPMENSV
        CALL EPKMLC(ISEL,
     &    'Select an OPAQUE construction from the list to',
     &    'associate with the block for visualisation purposes.',IER)
        CALL EPMENRC
        IF(ISEL.GT.0)then
          WRITE(BLKMAT(IB),'(A12)')DESC(ISEL)(1:12)
          MODIFY=.TRUE.
        endif
        IF(IER.EQ.1)THEN
          CALL USRMSG(' ',' A problem was encountered..','W')
        ENDIF
      ELSEIF(IVERT.EQ.18)THEN
        CALL PHELPD('obstr menu',18,'-',0,0,IER)
      ELSE
        GOTO 92
      ENDIF
      GOTO 92

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
#include "esprdbfile.h"
      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      COMMON/GS5/NB,XO(MB),YO(MB),ZO(MB),DX(MB),DY(MB),DZ(MB),BANG(MB)
      COMMON/GS6/NOX,NOZ,NGX,NGZ,BLKNAME(MB),BLKMAT(MB)
      common/UDESC/LVIEW(MCOM),LHCCO(MCOM),
     &             LTWIN(MCOM),LCGCIN(MCOM),ZOBS(MCOM)

      DIMENSION  XX(MV),YY(MV),ZZ(MV),XXW(4),YYW(4),ZZW(4)
      DIMENSION  TMAT(4,4),RMAT(4,4),VP(3),EP(3),EQN(4)

      logical clx0,cly0,clx1,cly1,clxm1,clym1
      CHARACTER BLKNAME*8,BLKMAT*12,blkroot*6,mat*12,HOLDS*36,H*72
      CHARACTER*72 LVIEW,LHCCO,LTWIN,LCGCIN,ZOBS

C Check if ok to add 4 obstructions.
      IUF=IPRODB
      if(NB+4.GT.MB)then
        call usrmsg('Reveal not added, run out of obstructions',
     &    'in this zone. ','W')
        return
      endif

C Create working vertices of the selected surface.
C Cases for 4 sided surfaces and user defined corners.
      if(NVER(IRS).ne.4)then
        h(1)='Reveals only created around a rectangular opening. '
        h(2)='You can specify 4 corners (i.e. 4 vertices, '
        h(3)='beginning lower left and going anti-clockwise). '
        CALL EASKAB('The selected surface doesn`t have 4 sides.',
     &    'Choices: ','specify 4 corners','cancel',IW,3)
        if(IW.eq.1)then
          write(HOLDS,'(4I5)') JVN(IRS,1),JVN(IRS,2),JVN(IRS,3),
     &      JVN(IRS,4)
          h(1)='You can specify 4 corners (i.e. 4 vertices, '
          h(2)='beginning lower left and going anti-clockwise). '
 153      CALL EASKS(HOLDS,' 4 vertices defining corners: ',' ',
     &      36,' 1 2 3 4 ','corners',IER,1)
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
      NB=NB+1
      XO(NB)=XXW(1)
      YO(NB)=YYW(1)
      ZO(NB)=ZZW(1)
      DX(NB)=DDX+OWID+OWID
      DY(NB)=RTK
      DZ(NB)=OWID
      BANG(NB)=RO
      write(BLKNAME(NB),'(a,a1)')blkroot(1:lnblnk(blkroot)),'s'
      BLKMAT(NB)=mat
      CALL MKGOMST(IUF,ZOBS(ICOMP),ICOMP,IER)

C Create the head obstruction.
      NB=NB+1
      XO(NB)=XXW(2)
      YO(NB)=YYW(2)
      ZO(NB)=ZZW(2)
      DX(NB)=DDX+OWID+OWID
      DY(NB)=RTK
      DZ(NB)=OWID
      BANG(NB)=RO
      write(BLKNAME(NB),'(a,a1)')blkroot(1:lnblnk(blkroot)),'h'
      BLKMAT(NB)=mat
      CALL MKGOMST(IUF,ZOBS(ICOMP),ICOMP,IER)

C Create the left side fin.
      NB=NB+1
      XO(NB)=XXW(3)
      YO(NB)=YYW(3)
      ZO(NB)=ZZW(3)
      DX(NB)=OWID
      DY(NB)=RTK
      DZ(NB)=DDZ+OWID+OWID
      BANG(NB)=RO
      write(BLKNAME(NB),'(a,a1)')blkroot(1:lnblnk(blkroot)),'l'
      BLKMAT(NB)=mat
      CALL MKGOMST(IUF,ZOBS(ICOMP),ICOMP,IER)

C Create the right side fin.
      NB=NB+1
      XO(NB)=XXW(4)
      YO(NB)=YYW(4)
      ZO(NB)=ZZW(4)
      DX(NB)=OWID
      DY(NB)=RTK
      DZ(NB)=DDZ+OWID+OWID
      BANG(NB)=RO
      write(BLKNAME(NB),'(a,a1)')blkroot(1:lnblnk(blkroot)),'r'
      BLKMAT(NB)=mat
      CALL MKGOMST(IUF,ZOBS(ICOMP),ICOMP,IER)

  100 CONTINUE

      RETURN
      END

