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


C RADCFG.F contains routines to control the exectution of RADIANCE from E2R.
C This is mainl done via the RADCFG file which contains data on the RADIANCE 
C rif file to be used and its intended purpose.

C RADCFGIN:  reads the contents of the radiance configuration file.
C RADCFGOUT: writes the contents of the radiance configuration file.
C whichrif:  presents a list of existing rif files.
C newscene:  asks default questions for new rif file.
C ADDSCENE:  allows the creation of a default translation from ESP-r to radiance.
C setdfgrid: allows the creation of the daylight factor grid points.
C DFGRIDLBL: writes out an index of daylight grid point (act = g|G) or
C            the daylight factor (act = d|D).
C SENLOC:    creates a file containing the sensor locations and orientations
C checkrif:  checks for existing scenes or creates a new scene with the given 
C            parameters if none found.
C RDCFTMP:   Read in daylight coefficients to common and rewrites to casual 
C            gain control file.


C ********************* RADCFGIN *********************
C RADCFGIN reads the contents of the radiance configuration file.
C << perhaps pass in the name of the file so as go get around
C << the issue of which folder it is in.
      SUBROUTINE RADCFGIN(tfile)
#include "building.h"
#include "model.h"
#include "e2r_common.h"
      
      integer lnblnk  ! function definition
      
      character tfile*96  ! the radiance configuration file

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      common/FILEP/IFIL
      common/rpath/path

      common/user/browse

      character WORD*124, OUTS*124, OUTSTR*124
      character pfile*72,path*72,fs*1

      logical unixok
      logical browse
      
      IRCFG = IFIL+17

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

C Initialise esp-r radiance cfg (*.rcf) file, assumed to be in model
C cfg folder.

C << but it most likely is in the rad folder. Sort out logic
C << to be similar to e2r.F near line 487.

      call ERPFREE(IRCFG,ISTAT)
      call EFOPSEQ(IRCFG,tfile,1,IER)
C      write(6,*) ' rcf status ',ier
C      write(6,*) ' rcf file ',LRADCF(1:lnblnk(LRADCF))
C      write(6,*) ' tfile file ',tfile(1:lnblnk(tfile))

C Read lines from file, header first.
      CALL STRIPC(IRCFG,OUTSTR,99,ND,1,'line 1',IER)
      IF(IER.NE.0)RETURN
      if (OUTSTR(1:13).ne.'*ESP-r visual') then
        call usrmsg('The specified file is not an ESP-r - ',
     &              'RADIANCE configuration file','F')
      endif

      CALL STRIPC(IRCFG,OUTSTR,99,ND,1,'date',IER)
      write (outs,'(a,a)') 'Scenes last modified on ',
     &                                         OUTSTR(1:lnblnk(OUTSTR))
      call edisp (iuout,outs)

C Now read number of scenes contained.
      CALL STRIPC(IRCFG,OUTSTR,99,ND,1,'scene nb line',IER)
      K=0
      CALL EGETWI(OUTSTR,K,NSCENE,1,MCOM+1,'W','no of scenes',IER)

      
C For each scene read at least 5 items of data.
      IER=0
      do 10 I=1,NSCENE

C Reset the number of modelled blind states to zero.
        NBSRIF(I)=-1
 30     CALL STRIPC(IRCFG,OUTSTR,99,ND,1,'rif data',IER)
        if (OUTSTR(1:9).eq.'*sceneEND') goto 20
        if (IER.eq.0) then
          K=0
          CALL EGETW(OUTSTR,K,WORD,'W','data id',IFLAG)
          if (WORD(1:4).eq.'*snm') then

C SCENE is depreciated if found just continue
            continue
          elseif (WORD(1:4).eq.'*rnm') then

C First rif name corresponds to default blind state, subsquent rifs within 
C this scene represent alternate blind states.
            NBSRIF(I)=NBSRIF(I)+1
            if (NBSRIF(I).gt.NABS) then
              call usrmsg('Scene has too many rif files.','  ','W')
            endif
            if (NBSRIF(I).eq.0) then
              CALL EGETW(OUTSTR,K,RIFNAME(I),'W','rif name',IFLAG)
            else
              CALL EGETW(OUTSTR,K,LBSRIF(I),'W','bl st',IFLAG)
            endif
          elseif (WORD(1:4).eq.'*srt') then
            CALL EGETW(OUTSTR,K,SCENERT(I),'W','scene root',IFLAG)
          elseif (WORD(1:4).eq.'*rde') then
            CALL EGETRM(OUTSTR,K,SCENEDESC(I),'W','scene desc',IFLAG)
          elseif (WORD(1:4).eq.'*rpu') then
            CALL EGETW(OUTSTR,K,SCENEPURP(I),'W','scene purp',IFLAG)
          elseif (WORD(1:4).eq.'*rgd') then

C Note: there is one grid or sensor data file name per scene.
            CALL EGETW(OUTSTR,K,LDFGRID(I),'W','grid data',IFLAG)
          elseif (WORD(1:4).eq.'*slc') then
            CALL EGETW(OUTSTR,K,LDFGRID(I),'W','sensor data',IFLAG)
          else
            call usrmsg(' Unrecognised data prefix.',' ','W')
          endif
        endif
        goto 30
 20     continue
 10   continue

      CALL ERPFREE(IRCFG,ISTAT)
      RETURN
      END

C ********************* RADCFGOUT *********************
C RADCFGOUT writes the contents of the ESP-r radiance configuration file.
      SUBROUTINE RADCFGOUT(ICHK)
#include "building.h"
#include "model.h"
#include "e2r_common.h"
      
      integer lnblnk  ! function definition

      common/FILEP/IFIL

      character OUTS*72
      character dstmp*24,pfile*72

C Check that ICHK is a sensible value.
      IRCFG = IFIL+17
      if (ICHK.ne.3.and.ICHK.ne.4) ICHK=4

C Get current time.
      call dstamp(dstmp)

C Set file name including path and open.
      write (pfile,'(a,a)') runpath(1:lnrp),LRADCF(1:lnblnk(LRADCF))
      call ERPFREE(IRCFG,ISTAT)
      call FPOPEN(IRCFG,ISTAT,1,ICHK,pfile)

C Write header first.
      write(IRCFG,'(a)')'*ESP-r visualisation scene configuration file.'
      write(IRCFG,'(2a)') dstmp,' # latest file modification.'

C Now write number of scenes contained.
      write(IRCFG,'(i3,a)') NSCENE,' # No. of scenes'

C For each scene write 1) root identifier, 2) short description 
C and 3) intended use/ purpose.

      do 10 I=1,NSCENE

C Find the longest string in this scene.
        IML=10
        ISLENG=LNBLNK(RIFNAME(I))
        IF(ISLENG.GT.IML)IML=ISLENG
        ISLENG=LNBLNK(LBSRIF(I))
        IF(ISLENG.GT.IML)IML=ISLENG
        ISLENG=LNBLNK(SCENERT(I))
        IF(ISLENG.GT.IML)IML=ISLENG
        ISLENG=LNBLNK(SCENEDESC(I))
        IF(ISLENG.GT.IML)IML=ISLENG
        ISLENG=LNBLNK(SCENEPURP(I))
        IF(ISLENG.GT.IML)IML=ISLENG

        write(IRCFG,'(a,i3)') '# Scene number',I
        OUTS=RIFNAME(I)
        write(IRCFG,'(3a)')'*rnm ',OUTS(1:IML),' # Rif name'
        if (NBSRIF(I).gt.0) then
          OUTS=LBSRIF(I)
          write(IRCFG,'(3a)')'*rnm ',OUTS(1:IML),' # Alternate rif'
        endif
        OUTS=SCENERT(I)
        write(IRCFG,'(3a)')'*srt ',OUTS(1:IML),' # Scene root'
        OUTS=SCENEDESC(I)
        write(IRCFG,'(3a)')'*rde ',OUTS(1:IML),' # Description'
        OUTS=SCENEPURP(I)
        write(IRCFG,'(3a)')'*rpu ',OUTS(1:IML),' # Intended use'

        if (SCENEPURP(I)(1:8).eq.'Day_fact') then
          OUTS=LDFGRID(I)
          write(IRCFG,'(3a)')'*rgd ',OUTS(1:IML),' # DF grid data'
        elseif (SCENEPURP(I)(1:7).eq.'Day_lux'.or.
     &          SCENEPURP(I)(1:5).eq.'Illum') then
          OUTS=LDFGRID(I)
          write(IRCFG,'(3a)')'*rgd ',OUTS(1:IML),' # lux grid data'
        elseif (SCENEPURP(I)(1:8).eq.'Coupling') then
          OUTS=LDFGRID(I)
          write(IRCFG,'(3a)')'*slc ',OUTS(1:IML),' # Sensor locations'
        elseif (SCENEPURP(I)(1:8).eq.'Day_coef') then
          OUTS=LDFGRID(I)
          write(IRCFG,'(3a)')'*slc ',OUTS(1:IML),' # Sensor locations'
        endif
        write(IRCFG,'(a)')'*sceneEND'
 10   continue

      write(IRCFG,'(a)') '*end'
      CALL ERPFREE(IRCFG,ISTAT)

      RETURN
      END

C ********************* whichrif *********************
C whichrif presents a list of existing rif files.
C If MODE is 's' then create a static list, 'm' then allow modification.
      SUBROUTINE whichrif(MODE)
#include "building.h"
#include "epara.h"
#include "e2r_common.h"
#include "help.h"

      COMMON/FILEP/IFIL

      character MODE*1, VERT*30, KEY*1

      dimension VERT(35)

      LOGICAL SELECT,delzone,cpyzone,modscene
      integer IW  ! for radio button
      integer MVERT,IVERT  ! max items and current menu item

      helpinsub='radcfg'  ! set for subroutine

 1    IER=0
      SELECT=.false.
      delzone=.false.
      cpyzone=.false.
      modscene=.false.

C Initialise scene menu variables based on window size. 
C IVERT is the menu position, MVERT the current number of menu lines.
C Use shorter menu depending on what MOD is.
      SELECT=.FALSE.
      MHEAD=0
      IF(MODE.EQ.'M'.OR.MODE.EQ.'m')THEN
        MCTL=4
      ELSE
        MCTL=3
      endif
      ILEN=NSCENE
      IPACT=CREATE
      CALL EKPAGE(IPACT)

C Initial menu entry setup.
   92 IER=0
      IVERT=-3

C Loop through the items until the page to be displayed. M is the 
C current menu line index. Build up text strings for the menu. 
    3 M=MHEAD
      DO 10 L=1,ILEN
        IF(L.GE.IST.AND.(L.LE.(IST+MIFULL)))THEN
          M=M+1
          CALL EMKEY(L,KEY,IER)
          WRITE(VERT(M),14)KEY,SCENERT(L)
   14     FORMAT(A1,1X,A)
        ENDIF
   10 CONTINUE

C Number of actual items displayed.
      MVERT=M+MCTL

C If a long list include page facility text and info on portion seen.      
      IF(IPFLG.EQ.0)THEN
        VERT(M+1)=  '  ----------------------'
      ELSE
        WRITE(VERT(M+1),15)IPM,MPM 
   15   FORMAT ('0 page part: ',I1,' -- of:',I1)
      ENDIF

C If MODE has been passed as a M then add an item at the end allowing
C list to be modified otherwise blank choice.  If user is browsing
C no need to show additional options.
      IF(MODE.EQ.'M'.OR.MODE.EQ.'m')THEN
        VERT(M+2)='* add/delete/copy scene '
        VERT(M+3)  ='? help                '
        VERT(M+4)  ='- exit menu           '
      ELSE
        VERT(M+2)  ='? help                '
        VERT(M+3)  ='- exit menu           '
      ENDIF

C Display the menu.
      CALL EMENU('Scenes',VERT,MVERT,IVERT)
      IF(IVERT.LE.MHEAD)THEN
        IVERT=-1
        goto 3
      ELSEIF(IVERT.EQ.MVERT)THEN

C If no selection has been made before exit then return.
        IF(.NOT.SELECT)then
          ICHK=4
          if(modscene)call RADCFGOUT(ICHK)
          RETURN
        endif
      ELSEIF(IVERT.EQ.(MVERT-1))THEN

C Produce help text.
        helptopic='scene_selection_list'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('scene section',nbhelp,'-',0,0,IER)
      ELSEIF(IVERT.GT.MHEAD.AND.IVERT.LT.(MVERT-MCTL+1))THEN

C Decode from the potentially long list to the zone number via KEYIND.
C If delete or copy scene previously selected then new selection should 
C be deleted or copied, respectively.
        CALL KEYIND(MVERT,IVERT,IFOC,IO)
        SELECT=.TRUE.
        ISCENE=IFOC
        if(delzone)then   ! act on prior request to delete
          do 1110 I=IFOC,NSCENE
            if (I.eq.NSCENE) then
              RIFNAME(I)='UNKNOWN'
              SCENERT(I)='UNKNOWN'
              SCENEDESC(I)='UNKNOWN'
              SCENEPURP(I)='UNKNOWN'
              LBSRIF(I)='NONE'
            else
              RIFNAME(I)=RIFNAME(I+1)
              SCENERT(I)=SCENERT(I+1)
              SCENEDESC(I)=SCENEDESC(I+1)
              SCENEPURP(I)=SCENEPURP(I+1)
              LBSRIF(I)=LBSRIF(I+1)
            endif
 1110     continue
          NSCENE=NSCENE-1
          ISCENE=1
          modscene=.true.
        elseif(cpyzone)then  ! act on previous request to copy scene
          NSCENE=NSCENE+1
          RIFNAME(NSCENE)=RIFNAME(IFOC)
          SCENERT(NSCENE)=SCENERT(IFOC)
          SCENEDESC(NSCENE)=SCENEDESC(IFOC)
          SCENEPURP(NSCENE)=SCENEPURP(IFOC)
          LBSRIF(I)=LBSRIF(IFOC)
          ISCENE=NSCENE

C Check new scenes data by calling newscene.  As newscene adds one to NSCENE 
C subtract one just now, no other data will be effected. It assumes that
C the user will provide appropriate alternative names.
          NSCENE=NSCENE-1
          call usrmsg('Edit copied scene (alter name & data).',
     &      ' ','W')
          call newscene('-')
          modscene=.true.
        endif

C If a zone has been deleted then redisplay menu, 
C foremost to reset ISCENE.
        if (delzone) then
          goto 1
        else

C Re-scan rif file if not a new zone.
          ITA2 = IFIL+7
          ITA3 = IFIL+10
          call RRIF(ita2,ita3,RIFNAME(ISCENE),'r',ier)
          RETURN
        endif
      endif

C Scene list manipulation.
      IF(IVERT.EQ.(MVERT-2))THEN

C Allow a scene to be added or deleted.
        IF(MODE.EQ.'M'.OR.MODE.EQ.'m')THEN
          helptopic='scene_manage_list'
          call gethelptext(helpinsub,helptopic,nbhelp)
          IW=1
          CALL EASKATOG(' ','Action: ','add scene','delete scene',
     &      'copy scene','cancel',' ',' ',' ',IW,nbhelp)
          if(IW.EQ.1)then

C Add a new scene (new rif file etc).
            delzone = .false.
            CALL EASKABC(' ','Create scene:','silently',
     &        'interactively','cancel',IWDV,nbhelp)
            if (IWDV.eq.1) then
              call newscene('c')   ! silent creation
              cmdact='Create'
              call autorad
              ICHK=4
              call radcfgout(ICHK)
            elseif (IWDV.eq.2) then
              call newscene('-')   ! interactive creation
              cmdact='Create'
              call autorad
              ICHK=4
              call radcfgout(ICHK)
            else
              goto 92
            endif
            RETURN
          elseif(IW.EQ.2)then

C Delete a scene.
            delzone = .true.
            call usrmsg(' ','Select scene to delete from list.','-')
            IVERT=-2
            goto 3
          elseif(IW.EQ.3)then

C Copy a scene
            cpyzone = .true.
            call usrmsg(' ','Select scene to copy from list.','-')
            IVERT=-2
            goto 3
          endif

        ELSE

C If there are enough items allow paging control via EKPAGE.
          IF(IPFLG.EQ.1)THEN
            IPACT=EDIT
            CALL EKPAGE(IPACT)
          ENDIF
        ENDIF
      
      else

C Not one of the legal menu choices.
        IVERT=-1
        goto 92
      ENDIF
      IVERT=-2
      goto 3

      END


C ********************* newscene *********************
C newscene asks default questions for new rif file or 
C sets values if MODE='s' (silent) from command line or
C if MODE='a' (aim) will ask for aim and name files automatically or
C if MODE='c' (create) then allow all purposes and auto name files or
C if MODE='-' is similar to 'a' but does not use word default in dialog.
      SUBROUTINE newscene(MODE)
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "e2r_common.h"
#include "help.h"

      integer lnblnk  ! function definition

      integer ncomp,ncon
      common/C1/NCOMP,NCON

      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)

      dimension IVALS(MCOM)

      character MODE*1,outs*124
      character dname*28,tname*28
      character dname72*72,tname72*72

      helpinsub='radcfg'  ! set for subroutine

C Ask for purpose of scene if new scene.
      if (MODE.eq.'s') then
        IZONE=0
        IRP=0
        if (aim(1:8).eq.'External') then
          IRP=1
          izone=-1
          zone='FRED'
        elseif (aim(1:8).eq.'Internal') then
          IRP=2
          call zindex(zone,izone)
        elseif (aim(1:5).eq.'Glare') then
          IRP=3
          call zindex(zone,izone)
        elseif (aim(1:8).eq.'Day_fact') then
          IRP=4
          call zindex(zone,izone)
        elseif (aim(1:8).eq.'Coupling') then
          IRP=5
          call zindex(zone,izone)
        elseif (aim(1:8).eq.'Day_coef') then
          IRP=6
          call zindex(zone,izone)
        elseif (aim(1:7).eq.'Day_lux') then
          IRP=7
          call zindex(zone,izone)
        elseif (aim(1:5).eq.'Illum') then
          IRP=8
          call zindex(zone,izone)
        elseif (aim(1:9).eq.'Night_ext') then
          IRP=9
          izone=-1
          zone='FRED'
        endif
      elseif (MODE.eq.'a') then
        helptopic='purpose_of_rif_scene'
        call gethelptext(helpinsub,helptopic,nbhelp)
        ino=0
        idno=1
        call MENUATOL('Select a default scene.',' Default scene',
     &    'a External images','b Internal images','c Glare sources',
     &    'd Daylight factors','e not applicable','f not applicable',
     &    'g Daylight lux @points','h Illuminance @points',
     &    'i Night external',' ',' ',' ',ino,idno,nbhelp)
        IRP=INO
      else

C Mode is not c or a.
        helptopic='purpose_of_rif_scene'
        call gethelptext(helpinsub,helptopic,nbhelp)
        ino=0
        idno=1
        call MENUATOL('Select a scene purpose.',' Scene purpose',
     &    'a External images','b Internal images','c Glare sources',
     &    'd Daylight factors','e Coupling','f Daylight coefficients',
     &    'g Daylight lux @points','h Illuminance @points',
     &    'i Night external',' ',' ',' ',ino,idno,nbhelp)
        IRP=INO
      endif

C If no purpose then exit.
      if(IRP.eq.0)return

C Check for tmc file
      call inqu_tmc

C Reset glazing treatment to default.
      iglzty=1

C Increment scene counter.
      NSCENE=NSCENE+1

C Set new scene to focus scene.
      ISCENE=NSCENE

C Reset the number of chosen zones (nzg) to NCOMP.
      nzg=NCOMP
      izgfoc=0
      do 3 I=1,NCOMP
        nznog(I)=I
 3    continue

C Now ask for zone for internal and glare purposes.
      if (IRP.eq.2.or.IRP.eq.3.or.IRP.eq.5.or.IRP.eq.6.or.
     &    IRP.eq.7.or.IRP.eq.8) then
        if (IZONE.le.0) then
          helptopic='pick_zone_for_int_view'
          call gethelptext(helpinsub,helptopic,nbhelp)
 30       INPIC=1
          CALL EPICKS(INPIC,IVALS,' ',' Pick a focus zone: ',
     &      12,NCOMP,zname,' zone list',IER,nbhelp)
          if (INPIC.eq.0) goto 30
          ifocz=IVALS(1)
        else
          ifocz=IZONE
        endif
      endif
      
C Set assumptions for each scene purpose and gather any additional 
C information.
      if (IRP.eq.1) then 
        SCENEPURP(ISCENE)='External'
        intext = 0   ! external view
        iadobe = 2   ! medium adobe texture
        imgqua = 'Medium'
        detlvl = 'Medium'
        llvar  = 'High'
        indrcb = 1
        ipicx  = 500
        penumb = 'False'
        crenrp = '-av 10 10 10 '
        coconv = '  '
        ifocz = -1
      elseif (IRP.eq.2) then 

C Internal image selected.
        SCENEPURP(ISCENE)='Internal'
        intext = 1  ! internal view
        iadobe = 3  ! fine adobe texture
        imgqua = 'Medium'
        detlvl = 'Medium'
        llvar  = 'High'
        indrcb = 1
        ipicx  = 500
        penumb = 'False'
        crenrp = '-av 1 1 1 '
        coconv = '  '
      elseif (IRP.eq.3) then 

C Glare image selected.
        SCENEPURP(ISCENE)='Glare'
        intext = 1
        iadobe = 2  ! medium adobe texture
        imgqua = 'Medium'
        detlvl = 'Medium'
        llvar  = 'High'
        indrcb = 2
        ipicx  = 500
        penumb = 'False'
        crenrp = '-av 0 0 0 '
        coconv = '  '
      elseif (IRP.eq.4) then 

C Daylight factor calculation selected.
        SCENEPURP(ISCENE)='Day_fact'
        intext = 1
        iadobe = 2  ! medium adobe texture
        imgqua = 'Medium'
        detlvl = 'Medium'
        llvar  = 'High'
        indrcb = 2
        ipicx  = 500
        penumb = 'True'
        crenrp = '-av 0 0 0 -ab 1'
        coconv = '  '
      elseif (IRP.eq.5) then 

C Coupling calculation selected. (thermal/lighting simulation)
        SCENEPURP(ISCENE)='Coupling'
        intext = 1
        iadobe = 2  ! medium adobe texture
        imgqua = 'Medium'
        detlvl = 'Medium'
        llvar  = 'High'
        indrcb = 2
        ipicx  = 500
        penumb = 'False'
        crenrp = '-av 0 0 0  '
        coconv = '  '
        vewcmds(ISCENE)='  '
        indxvew = 0
      elseif (IRP.eq.6) then 

C Daylight coefficient calculation selected. (thermal/lighting simulation)
        SCENEPURP(ISCENE)='Day_coef'
        intext = 1
        imgqua = 'Medium'
        detlvl = 'Medium'
        llvar  = 'High'
        indrcb = 2
        ipicx  = 500
        penumb = 'False'
        crenrp = '-av 0 0 0 '
        coconv = '  '
      elseif (IRP.eq.7) then 

C Daylight lux (illuminance at grid points with CIE sky) calculation selected.
        SCENEPURP(ISCENE)='Day_lux'
        intext = 1
        iadobe = 2  ! medium adobe texture
        imgqua = 'Medium'
        detlvl = 'Medium'
        llvar  = 'High'
        indrcb = 2
        ipicx  = 500
        penumb = 'True'
        crenrp = '-av 0 0 0 -ab 1'
        coconv = '  '
      elseif (IRP.eq.8) then 

C Illuminance at grid points with std sky calculation selected.
        SCENEPURP(ISCENE)='Illum'
        intext = 1
        iadobe = 2  ! medium adobe texture
        imgqua = 'Medium'
        detlvl = 'Medium'
        llvar  = 'High'
        indrcb = 2
        ipicx  = 500
        penumb = 'True'
        crenrp = '-av 0 0 0 -ab 1'
        coconv = '  '
      elseif (IRP.eq.9) then 

C Sun positions.
        SCENEPURP(ISCENE)='Night_ext'
        intext = 0   ! external view
        iadobe = 2  ! medium adobe texture
        imgqua = 'Medium'
        detlvl = 'Medium'
        llvar  = 'High'
        indrcb = 2
        ipicx  = 500
        penumb = 'True'
        crenrp = '-av 0.0005 0.0005 0.0005 '
        coconv = '  '
        ifocz = -1
      endif

C Set default names based on above.
      if (IRP.eq.2.or.IRP.eq.3.or.IRP.eq.5.or.IRP.eq.6.or.
     &    IRP.eq.7.or.IRP.eq.8) then
        write (outs,'(3a)') zname(ifocz)(1:lnzname(ifocz)),
     &     '_',SCENEPURP(ISCENE)(1:6)
        write (SCENEDESC(ISCENE),'(5a)') 'Scene of ',
     &     zname(ifocz)(1:lnzname(ifocz)),' for ',
     &     SCENEPURP(ISCENE)(1:lnblnk(SCENEPURP(ISCENE))),' analysis.'
      else
        IL=min(lnblnk(cfgroot),21)
        write (outs,'(3a)') cfgroot(1:IL),'_',SCENEPURP(ISCENE)(1:6)
        write (SCENEDESC(ISCENE),'(5a)') 'Scene of ',
     &     cfgroot(1:lnblnk(cfgroot)),' for ',
     &     SCENEPURP(ISCENE)(1:lnblnk(SCENEPURP(ISCENE))),' analysis.'
      endif
      write (SCENERT(ISCENE),'(a)') outs(1:28)
      write (RIFNAME(ISCENE),'(a,a)') 
     &              SCENERT(ISCENE)(1:lnblnk(SCENERT(ISCENE))),'.rif'
      write (LBSRIF(ISCENE),'(a,a)') 
     &            SCENERT(ISCENE)(1:lnblnk(SCENERT(ISCENE))),'_a.rif'

C Additional details for specific purposes.
      if (IRP.eq.4.or.IRP.eq.7.or.IRP.eq.8) then

C Set up daylight factor or illuminance calc points.
        call setdfgrid(MODE,izone,ier)

      elseif (IRP.eq.5.or.IRP.eq.6) then 

C Get sensor information from cgc file and create a slc file.
        call SENLOC
      endif

C Clear command line variables. << why do we do this? >>
      zone='NONE'
C      aim='NONE'
      cmdact='NONE'

C If not in silent mode then confirm names with user.
      if (MODE.eq.'-') then

C Ask for root name and use this for rif name.
 15     write(tname,'(a)') SCENERT(ISCENE)
        write(dname,'(a)') SCENERT(ISCENE)
        CALL EASKS(tname,'Scene root name?',' ',28,dname,
     &    'root name',IER,nbhelp)
        IF(tname(1:2).EQ.'  ') GOTO 15
        write(SCENERT(ISCENE),'(a)') tname(1:lnblnk(tname))
        write (RIFNAME(ISCENE),'(a,a)') 
     &    SCENERT(ISCENE)(1:lnblnk(SCENERT(ISCENE))),'.rif'
        write (LBSRIF(ISCENE),'(a,a)') 
     &    SCENERT(ISCENE)(1:lnblnk(SCENERT(ISCENE))),'_a.rif'

C Ask for additional description.
 25     write(tname72,'(a)') SCENEDESC(ISCENE)
        write(dname72,'(a)') SCENEDESC(ISCENE)
        CALL EASKS(tname72,'Description of the scene?',' ',72,
     &    dname72,'scene desc',IER,nbhelp)
        IF(tname72(1:2).EQ.'  ') GOTO 25
        write(SCENEDESC(ISCENE),'(a)') tname72(1:lnblnk(tname72))

C Debug
        write(6,*) 'scene root ',SCENERT(ISCENE)
        write(6,*) 'scene desc ',SCENEDESC(ISCENE)
      endif

C Call rifset to set up all remaining data.
      call rifset(ifocz,'f')

      RETURN
      END

C ********************** ADDSCENE **********************
C ADDSCENE allows the creation of a default translation from ESP-r to radiance.
      subroutine ADDSCENE(IRCFG)
#include "building.h"
#include "model.h"
#include "e2r_common.h"
#include "help.h"

      integer lnblnk  ! function definition

      CHARACTER pfile*72

      helpinsub='radcfg'  ! set for subroutine

C Ask if a default image should be created.
      helptopic='create_scene_interactive'
      call gethelptext(helpinsub,helptopic,nbhelp)
      CALL EASKABC('This is a new visualisation.',
     &  'Create scene:','silently','interactively',
     &  'cancel',IWDV,nbhelp)
      if(IWDV.eq.1)then

C Check for command line information.
        call tstamp('>','E2R: creating default scene')
        if (zone(1:4).ne.'NONE'.and.aim(1:4).ne.'NONE') then
          call checkrif('n')
        else
          call newscene('a')
          cmdact='Create'
          call autorad
          ICHK=4
          call radcfgout(ICHK)
        endif
      elseif(IWDV.eq.2)then

C Create new scene file asking the user what the purpose is.
        call tstamp('>','E2R: controlling default scene')
        write (pfile,'(a,a)') runpath(1:lnrp),LRADCF(1:lnblnk(LRADCF))
        call ERPFREE(IRCFG,ISTAT)
        call FPOPEN(IRCFG,ISTAT,1,3,pfile)
        call newscene('-')
        cmdact='Create'
        call autorad
        ICHK=4
        call radcfgout(ICHK)
      endif

      RETURN
      END

C ********************** setdfgrid **********************
C setdfgrid allows the creation of the daylight factor grid points or
C illuminance grid points.
C if MODE is not - then sensor points file name confirmation required.
      subroutine setdfgrid(MODE,izone,ier)
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "prj3dv.h"
#include "e2r_common.h"
#include "help.h"

      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/FILEP/IFIL
      
      integer ncomp,ncon
      common/C1/NCOMP,NCON

      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)

      character DESCRC*25
      character outs*124,tfile*72
      character ITEM(MV)*15, t12*12, MODE*1
      character ldfg*72   ! local file name
      logical keep

      dimension IVALS(MCOM), PEQN(4), ITEMV(MV)

      helpinsub='radcfg'  ! set for subroutine

C New grid so set number of grid points to zero.
      NDFP=0

C Remember current graphics display options.
      call SVDSOPT

C Ask for zone (if not defined), surface and working plane height.
      if (IZONE.eq.0) then
        helptopic='select_zone_for_df'
        call gethelptext(helpinsub,helptopic,nbhelp)
 32     INPIC=1
        CALL EPICKS(INPIC,IVALS,
     &    'Generate DF or illuminance for which zone. ',
     &    '(You will then be asked to grid a surface.) ',
     &    12,NCOMP,zname,'Select zone',IER,nbhelp)
        if (INPIC.eq.0) goto 32
        ifocz=IVALS(1)
        nzg=1
        nznog(1)=ifocz
        izgfoc=0
      else
        ifocz=izone
        nzg=1
        nznog(1)=izone
        izgfoc=0
      endif
      MODIFYVIEW=.TRUE.
      MODLEN=.TRUE.
      MODBND=.TRUE.
      ITSNM=0
      CALL redraw(IER)

C Ask for surface and highlight.
      call EASKSUR(ifocz,IS,'-',
     &    'Select a surface to grid. (You can then transform ',
     &    'the grid perpendicular to this surface.)',IER)
      CALL INLNST(1)
      CALL SURADJ(ifocz,IS,IE,TMP,IZC,ISC,IC,DESCRC)
      LINSTY(IC)=2
      MODIFYVIEW=.TRUE.
      ITVNO=0
      CALL redraw(IER)
      CALL INLNST(1)

C Setup help for the next few dialogs.
      helptopic='distance_to_df_grid'
      call gethelptext(helpinsub,helptopic,nbhelp)
      WPDIST=0.9
      CALL EASKR(WPDIST,' Distance of grid from surface ? ',' ',0.001,
     &  'F',2.5,'W',0.9,'grid transform',IER,nbhelp)
      call EASKAB('Transform grid to: ',' ','Inside','Outside',
     &  IW,nbhelp)
      if (IW.eq.1) then
        dirf=-1.
      else
        dirf=1.
      endif

C Now read zone geometry file to enable edge selection.
      ITA1 = IFIL+6
      call georead(ITA1,LGEOM(ifocz),ifocz,0,IUOUT,IER)

C Generate a list of edges. JVN(MS,MV)
      NI=0
      do 10 I=1,NVER(IS)-1
        write(ITEM(I),'(2(a,i3))')'edge v',JVN(IS,I),'->v',JVN(IS,I+1)
        NI=NI+1
 10   continue
      NI=NI+1
      write(ITEM(NI),'(2(a,i3))')'edge v',JVN(IS,NI),'->v',JVN(IS,1)
 12   INPIC=1
      CALL EPICKS(INPIC,ITEMV,
     &  'Select an edge (defined by vertex numbers). ',
     &  'The grid will be parallel to this edge. ',
     &  15,NI,ITEM,'Select edge',IER,nbhelp)
      if (INPIC.eq.0) goto 12

C Translate edge selection into vertex numbers.
      if (ITEMV(1).eq.NI) then
        IVS=JVN(IS,NI)
        IVE=JVN(IS,1)
      else
        IVS=JVN(IS,ITEMV(1))
        IVE=JVN(IS,ITEMV(1)+1)
      endif

C Grid density.
      helptopic='df_grid_density'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Major grid lines are parallel with the selected edge.
      igmajor=4
      igminor=6
      write (t12,'(2i4)') igmajor,igminor
 25   CALL EASKS(t12,'Grid density? (First item is number of grid',
     &   'points in the rows parallel to the chosen edge.) ',
     &   12,' 2  2','grid density',IER,nbhelp)
      K=0
      CALL EGETWI(t12,K,igmajor,1,25,'W','grid lines',IER)
      CALL EGETWI(t12,K,igminor,1,25,'W','grid lines',IER)
      if (igmajor.lt.1.or.igminor.lt.1) then
        call usrmsg('There must be at least 1 grid',
     &              'line in each direction.','W')
        goto 25
      endif

C Calculate grid points.
      WPDIST=WPDIST*dirf
      call GRID(IS,igmajor,igminor,IVS,IVE,WPDIST,DFPTS,NDFP,PEQN,IER)
      call ERPFREE(ITA1,ISTAT)

C Get file name for grid points.
      write (ldfg,'(a,a4)') 
     &  SCENERT(NSCENE)(1:lnblnk(SCENERT(NSCENE))),'.grd'
      if (MODE.eq.'-') then
 55     CALL EASKS(LDFG,'DF or illuminance grid points file name?',
     &    ' ',72,'points.grd','df points file name',IER,nbhelp)
        IF(LDFG.EQ.' ') GOTO 55
        LDFGRID(NSCENE)=LDFG
      endif

C Open df points file in working directory.
      write (tfile,'(a,a)') runpath(1:lnrp),
     &  LDFGRID(NSCENE)(1:lnblnk(LDFGRID(NSCENE)))
      call FPOPEN(ITA1,ISTAT,1,3,tfile)
      do 552 IC=1,3
        PEQN(IC)=PEQN(IC)*dirf
 552  continue

C Write points and sensor direction (plane normal).
      do 555 IV=1,NDFP
       write(ITA1,'(6(f8.4,1x))')(DFPTS(IV,IX),IX=1,3),(PEQN(IY),IY=1,3)
 555  continue
      DFDIR(1)=PEQN(1)
      DFDIR(2)=PEQN(2)
      DFDIR(3)=PEQN(3)
      call ERPFREE(ITA1,ISTAT)

C Display grid on the zone... (if in graphic mode)
      if (MMOD.EQ.8) then
        call edisp(iuout,'  ')
        call edisp(iuout,'The proposed calculation points are shown')
        call edisp(iuout,'as dots in the graphic feedback.')
        MODIFYVIEW=.TRUE.
        ITZNM=1
        ITSNM=1
        ITVNO=1
        CALL redraw(IER)
        CALL DFGRIDLBL('g')
      endif

C Ask user if focus should remain on this zone or whole model
      helptopic='df_stay_focused'
      call gethelptext(helpinsub,helptopic,nbhelp)
      CALL EASKOK(' ','Keep graphic feedback focused on this zone?',
     &  KEEP,nbhelp)
      if(KEEP)then
        continue
      else

C Reset the number of chosen zones (nzg) to NCOMP.
        nzg=NCOMP
        izgfoc=0
        do 300 I=1,NCOMP
          nznog(I)=I
 300    continue
      endif

C Reset display options.
      call RCDSOPT

      return
      end

C ********************** DFGRIDLBL **********************
C DFGRIDLBL writes out an index of daylight grid point (act = g|G) or
C the daylight factor (act = d|D) or lux (act = l|L).
      SUBROUTINE DFGRIDLBL(act)
#include "building.h"
#include "prj3dv.h"
#include "e2r_common.h"
      
      integer lnblnk  ! function definition

C Parameters
      character act*1  ! action

C      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/FILEP/IFIL
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      integer ifs,itfs,imfs
      COMMON/GFONT/IFS,ITFS,IMFS

      DIMENSION  COG1(3),COG2(3),COG3(3)

      CHARACTER VT*6
      character OUTSTR*124,tfile*72

#ifdef OSI
      integer iix,iiy,iid1,iid2,iicol
#else
      integer*8 iix,iiy,iid1,iid2,iicol
#endif
      
C If not in graphical mode return.
      if (MMOD.ne.8) return

C If there are no grid points warn the user and check if
C there is a *.df file or a *.df- file that can be scanned again.
      if(NDFP.eq.0)then
        ITA1 = IFIL+6

        call ERPFREE(ITA1,ISTAT)
        write(tfile,'(a,a)')runpath(1:lnrp),
     &    LDFGRID(ISCENE)(1:lnblnk(LDFGRID(ISCENE)))
        call FPOPEN(ITA1,ISTAT,1,1,tfile)
        if(ISTAT.eq.0)then
          continue
        else
          call ERPFREE(ITA1,ISTAT)
          write(tfile,'(3a)')runpath(1:lnrp),
     &      LDFGRID(ISCENE)(1:lnblnk(LDFGRID(ISCENE))),'-'
          call FPOPEN(ITA1,ISTAT,1,1,tfile)
          if(ISTAT.eq.0)then
            continue
          else
            CALL USRMSG('Could not read grid points file.',
     &       'Cannot report without this information. Returning.','W')
            return
          endif
        endif
  42    CALL STRIPC(ITA1,OUTSTR,0,ND,0,'grid points',IFER)
        if (IFER.eq.0) then
          NDFP=NDFP+1
          K=0
          CALL EGETWR(OUTSTR,K,XTMP,0.,0.,'-','grid x',IER)
          DFPTS(NDFP,1)=XTMP
          CALL EGETWR(OUTSTR,K,XTMP,0.,0.,'-','grid y',IER)
          DFPTS(NDFP,2)=XTMP
          CALL EGETWR(OUTSTR,K,XTMP,0.,0.,'-','grid z',IER)
          DFPTS(NDFP,3)=XTMP
          CALL EGETWR(OUTSTR,K,DFDIR(1),0.,0.,'-','vec x',IER)
          CALL EGETWR(OUTSTR,K,DFDIR(2),0.,0.,'-','vec y',IER)
          CALL EGETWR(OUTSTR,K,DFDIR(3),0.,0.,'-','vec z',IER)
          goto 42
        endif
        call ERPFREE(ITA1,ISTAT)
        if (NDFP.eq.0) then
          CALL USRMSG('Could not read grid points file.',
     &      'Cannot report without this information. Returning.','W')
          return
        endif
      endif

C If vertices are to be labeled do it in the smallest font.
      call SVDSOPT
      IF(IFS.GT.0)CALL winfnt(0)
      MODIFYVIEW=.TRUE.
      MODLEN=.TRUE.
      MODBND=.TRUE.
      ITZNM=1
      ITSNM=1
      ITVNO=1
      CALL redraw(IER)

C Loop through grid points.
      do 10 I=1,NDFP
        COG1(1)=DFPTS(I,1)
        COG1(2)=DFPTS(I,2)
        COG1(3)=DFPTS(I,3)
        CALL VECTRN(COG1,TEMAT,COG2,IER)
        CALL VECTRN(COG2,ETSMAT,COG3,IER)
        call u2pixel(COG3(1),COG3(2),iix,iiy)
        CALL ecirc(iix,iiy,3,1)
        if(act.eq.'g'.or.act.eq.'G')then
          IF(I.le.9)THEN
            WRITE(VT,'(i1)') I
          ELSEIF(I.gt.9.and.I.le.99)THEN
            WRITE(VT,'(i2)') I
          ELSEIF(I.gt.99)THEN
            WRITE(VT,'(i3)') I
          ENDIF
        elseif(act.eq.'d'.or.act.eq.'D')then
          IF(DFVALS(I).lt.10.0)THEN
            WRITE(VT,'(f4.2)') DFVALS(I)
          ELSEIF(DFVALS(I).ge.10.0)THEN
            WRITE(VT,'(f4.1)') DFVALS(I)
          ENDIF
        elseif(act.eq.'l'.or.act.eq.'L')then
          IF(DFVALS(I).lt.1000.0)THEN
            WRITE(VT,'(f5.0)') DFVALS(I)
          ELSEIF(DFVALS(I).ge.1000.0)THEN
            WRITE(VT,'(f6.0)') DFVALS(I)
          ENDIF
        endif
        iid1=iix+1; iid2=iiy-1; iicol=0
        call textatxy(iid1,iid2,VT,'-',iicol)
 10   continue

C Reset display options.
      call RCDSOPT
      IF(IFS.GT.0)CALL winfnt(IFS)

      RETURN
      END


C ********************** SENLOC **********************
C SENLOC creates a file containing the sensor locations and orientations
C for use with a 'Coupling' scene purpose.

      SUBROUTINE SENLOC
#include "building.h"
#include "model.h"
#include "e2r_common.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/FILEP/IFIL

      COMMON/CGCIN2N/NCGTC(MCOM,MDTY),NLITZ(MCOM),IDFST(MCOM,MLCOM),
     & CGX(MCOM,MLCOM,MDF),CGY(MCOM,MLCOM,MDF),CGH(MCOM,MLCOM,MDF),
     & UX(MCOM,MLCOM,MDF),UY(MCOM,MLCOM,MDF),UH(MCOM,MLCOM,MDF),
     & SETPT(MCOM,MLCOM),SYSPER(MCOM,MLCOM),SOFFLL(MCOM,MLCOM),
     & IOFFDT(MCOM,MLCOM),SMLOUT(MCOM,MLCOM),SMEOUT(MCOM,MLCOM)

      COMMON/DAYF/NDF(MCOM,MLCOM),DFDAT(MCOM,MLCOM,MGT,MDF),
     & AZIJL(MCOM,MGT),SUMDF(MCOM,MLCOM,MDF),NUDWIN(MCOM)

      CHARACTER LCGCINT*72
      character outs*124

      logical XST

      write(outs,'(a)')'Reading casual gain control file.'
      call edisp (iuout,outs)
      write(outs,'(a,a)')'Searching for sensor locations ',
     &                                        'and orientations.'
      call edisp (iuout,outs)

C Define temporary unit numbers.
      ITA1 = IFIL+6
      call ERPFREE(ITA1,ISTAT)

C Set local file name.
      LCGCINT=LCGCIN(ifocz)
      ICOMP=ifocz
      ier=0
      if (LCGCINT(1:2).eq.'  ') then
        write(outs,'(a)')'Error: casual gain control file undefined!'
        call edisp (iuout,outs)
        return
      endif
      itrc=0
      call ercgcf(itrc,itru,LCGCINT,ICOMP,ier)

C Open sensor info file (do not add path as radiance is run locally).
      write(LDFGRID(ISCENE),'(a,a)')
     &  SCENERT(ISCENE)(1:lnblnk(SCENERT(ISCENE))),'.slc'

C Check for file (add working dir path)
      write (outs,'(a,a)') runpath(1:lnblnk(runpath)),
     &  LDFGRID(ISCENE)(1:lnblnk(LDFGRID(ISCENE)))
      call FINDFIL(outs,XST)
      if(XST)then
        call FPOPEN(ITA1,ISTAT,1,0,outs)
        call EFDELET(ITA1,ISTAT)
      endif
      call FPOPEN(ITA1,ISTAT,1,2,outs)

C Write sensor information to radiance grid file.
      do 10 I=1,NLITZ(ICOMP)
        if ((IDFST(ICOMP,I).EQ.4).or.(IDFST(ICOMP,I).EQ.5)) then
          do 20 J=1,NDF(ICOMP,I)
            write (ITA1,'(6f10.3)') CGX(ICOMP,I,J),CGY(ICOMP,I,J),
     &         CGH(ICOMP,I,J),UX(ICOMP,I,J),UY(ICOMP,I,J),UH(ICOMP,I,J)
 20       continue
        endif
 10   continue
      call ERPFREE(ITA1,ISTAT)

      RETURN
      END

C ********************* checkrif *********************
C checkrif checks for existing scenes or creates a new scene with the given 
C parameters if none found.
C If filexst is 'o' then old file, 'n' then create a new file.
      SUBROUTINE checkrif(filexst)
#include "building.h"
#include "model.h"
#include "e2r_common.h"
      
      integer lnblnk  ! function definition

      COMMON/FILEP/IFIL

      character filexst*1
      character pfile*72

      LOGICAL modscene

      IRCFG=IFIL+17
      ITA2=IFIL+7
      ITA3=IFIL+10
      IER=0
      ISCENE=0
      modscene=.false.

C Clear rcf file unit no/ channel.
      CALL ERPFREE(IRCFG,ISTAT)

C Recover zone index from command line variable zone.
      if (zone(1:4).eq.'Def_') then
        izone=-1
      else
        if (aim(1:8).eq.'External') then
          izone=-1
          zone='  '
        else
          call zindex(zone,izone)
        endif
      endif

C If rcf file does exist then search for existing rif file.
      if (filexst.eq.'o') then
        do 10 i=1,NSCENE
          if (SCENEPURP(I)(1:5).eq.aim(1:5)) then

C Possible match, further check - look at rif file.
            ifocz=-1
            call RRIF(ITA2,ITA3,RIFNAME(I),'s',IER)
            if (ifocz.eq.IZONE) then

C Got match.
              ISCENE=I

C Update sensor information from cgc file.
              if(aim(1:8).eq.'Coupling'.or.aim(1:8).eq.'Day_coef')then
                call SENLOC
              endif

C If daylight coefficients and cmdact=create and model exists then issue warning.
              if(aim(1:8).eq.'Day_coef'.and.
     &           cmdact(1:6).eq.'Create')then
                call usrmsg('Model already exists!',
     &                      'Using existing model.','W')
                cmdact='Calculate'
              endif
              modscene=.true.
            endif
          endif
 10     continue
      else

C Create a new rcf file.
        write (LRADCF,'(a,a)') cfgroot(1:lnblnk(cfgroot)),'.rcf'

C Set file name including path and open.
        write(pfile,'(a,a)') runpath(1:lnblnk(runpath)),
     &    LRADCF(1:lnblnk(LRADCF))
        call ERPFREE(IRCFG,ISTAT)
        call FPOPEN(IRCFG,ISTAT,1,2,pfile)
      endif

C If not an existing scene then use newscene to configure
C it and then autorad to build the matching descriptive
C files and save to rif file and lastly the esp-r rcf file.
      if (.not.modscene) then
        call newscene('s')
        cmdact='Create'
        call autorad
        IFC=3
        call MKRIF(ITA2,RIFNAME(ISCENE),IFC,0,IER)
        if (NBSRIF(ISCENE).eq.1) then
          call mkrif(ita2,LBSRIF(ISCENE),IFC,1,ier)
        endif

C Update rcf file.
        ICHK=3
        call RADCFGOUT(ICHK)
        cmdact='NONE'   ! reset because we have done it
      endif

      return
      end


C ********************** RDCFTMP **********************
C RDCFTMP Read in daylight coefficients to common and rewrites to casual 
C gain control file.

      SUBROUTINE RDCFTMP(LFTMP,ISET)
#include "building.h"
#include "model.h"
#include "e2r_common.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/FILEP/IFIL

      COMMON/CGCIN2N/NCGTC(MCOM,MDTY),NLITZ(MCOM),IDFST(MCOM,MLCOM),
     & CGX(MCOM,MLCOM,MDF),CGY(MCOM,MLCOM,MDF),CGH(MCOM,MLCOM,MDF),
     & UX(MCOM,MLCOM,MDF),UY(MCOM,MLCOM,MDF),UH(MCOM,MLCOM,MDF),
     & SETPT(MCOM,MLCOM),SYSPER(MCOM,MLCOM),SOFFLL(MCOM,MLCOM),
     & IOFFDT(MCOM,MLCOM),SMLOUT(MCOM,MLCOM),SMEOUT(MCOM,MLCOM)

      COMMON/DAYF/NDF(MCOM,MLCOM),DFDAT(MCOM,MLCOM,MGT,MDF),
     & AZIJL(MCOM,MGT),SUMDF(MCOM,MLCOM,MDF),NUDWIN(MCOM)

C Daylight coefficient common block data definition:
C MCOM  - max no of thermal zones.
C MLCOM - max no of lighting zones per thermal zone.
C MDF   - max no of sensors per lighting zone.
C MDCFP - max no of daylight coefficient data sets.
C MDCFS - max no of alternative daylight coefficient data sets.
C MDCFV - max no of sky patches.
      COMMON/DCOEF/NDCFP,NDCFS(MDCFP),DCOEF(MDCFP,MDCFS,MDCFV),
     & IDCFID(MCOM,MLCOM,MDF)

      CHARACTER*72 LCGCINT,LFTMP
      character outs*124

C Define temporary unit numbers.
      ITA1 = IFIL+6
      call ERPFREE(ITA1,ISTAT)

C Set local file name.
      LCGCINT=LCGCIN(ifocz)
      ICOMP=ifocz
      ier=0
      if (LCGCINT(1:2).eq.'  ') then
        write(outs,'(a)')'Error: casual gain control file undefined!'
        call edisp (iuout,outs)
        return
      endif
      itrc=0
      call ercgcf(itrc,itru,LCGCINT,ICOMP,ier)

C Read illuminance information from temp file.
      call FPOPEN(ITA1,ISTAT,1,0,LFTMP)
      do 5 IP=1,145
       do 10 ILZ=1,NLITZ(ICOMP)
        if (IDFST(ICOMP,ILZ).EQ.5) then
         do 20 ISE=1,NDF(ICOMP,ILZ)
          CALL STRIPC(ITA1,OUTS,0,ND,0,'transfer file',IER)
          if (ier.ne.0) then 
           call usrmsg('Cannot read transfer file:',LFTMP,'F')
           return
          endif
          K=0
          call EGETWR(OUTS,K,SIL,0.,10000.,'W','illuminance',IER)
          JSEN=IDCFID(ICOMP,ILZ,ISE)
          DCOEF(JSEN,ISET,IP)=SIL
 20      continue
        endif
 10    continue
 5    continue
      call ERPFREE(ITA1,ISTAT)

C Write data to casual gain control file.
      call CASCTMK(LCGCINT,ICOMP,'s',IER)

      RETURN
      END
