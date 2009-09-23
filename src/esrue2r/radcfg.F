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

C RADCFG.F contains routines to control the exectution of RADIANCE from E2R.
C This is mainl done via the RADCFG file which contains data on the RADIANCE 
C rif file to be used and its intended purpose.

C RADCFGIN:  reads the contents of the radiance configuration file.
C RADCFGOUT: writes the contents of the radiance configuration file.
C whichrif:  presents a list of existing rif files.
C newscene:  asks default questions for new rif file or 
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
      SUBROUTINE RADCFGIN
#include "building.h"
#include "e2r_common.h"
#include "control.h"

      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      common/FILEP/IFIL
      common/rpath/path
      common/expath/runpath,pathtype
      common/paths/zonepth,netpth,ctlpth,imgpth,radpth,docpth,tmppth,
     &             dbspth
      common/user/browse

      character zonepth*24,netpth*24,ctlpth*24,imgpth*24,radpth*24
      character docpth*24,tmppth*24,dbspth*24
      character WORD*124, OUTS*124, OUTSTR*124
      character pfile*72,path*72,runpath*72,pathtype*24,fs*1
      
      character tmp_rifname*72
      
      integer lnrp  ! length of run path

      integer ifocz_tmp(MBP+1)

      logical unixok
      logical browse
      
      IRCFG = IFIL+17

C from checkrif ... file pointers for .rif??
      ITA2=IFIL+7
      ITA3=IFIL+10
C --

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif
      lnrp=lnblnk(runpath)

C Copy existing model to runpath (unix only).
      if(unixok)then
        if(browse)then
          write (outs,'(6a)') 'cp -r ',path(1:lnblnk(path)),
     &      radpth(1:lnblnk(radpth)),fs,'* ',runpath(1:lnrp)
          call runit(outs,'-')
        endif
      else
      
C For native windows no need to copy.
C        if(browse)then
C          write (outs,'(6a)') 'xcopy /e /i /y /c ',path(1:lnblnk(path)),
C     &      radpth(1:lnblnk(radpth)),fs,'* ',runpath(1:lnrp)
C        endif
C        call runit(outs,'-')
      endif
      if(unixok)then
        if(browse)then
          write (outs,'(5a)') 'cp -r ',path(1:lnblnk(path)),
     &          LRADCF(1:lnblnk(LRADCF)),' ',runpath(1:lnrp)
          call runit(outs,'-')
        endif
      else
      
C For native windows no need to copy.
C        write (outs,'(5a)') 'xcopy /e /i /y /c ',path(1:lnblnk(path)),
C     &          LRADCF(1:lnblnk(LRADCF)),' ',runpath(1:lnrp)
C        call runit(outs,'-')
      endif

C Set file name including path.
      write (pfile,'(a,a)') runpath(1:lnrp),LRADCF(1:lnblnk(LRADCF))

C Initialise radiance cfg file.
      call ERPFREE(IRCFG,ISTAT)
      call FPOPEN(IRCFG,ISTAT,1,1,pfile)

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
      CALL STRIPC(IRCFG,OUTSTR,99,ND,1,'scene number line',IER)
      K=0
      CALL EGETWI(OUTSTR,K,NSCENE,1,MCOM+1,'W','no of scenes',IER)

C For each scene read (at least) 5 items of data
      IER=0
      do 10 I=1,NSCENE
        irif=0 ! initialize rif counter for current scene
 30     CALL STRIPC(IRCFG,OUTSTR,99,ND,1,'rif data',IER)
        if (OUTSTR(1:9).eq.'*sceneEND') goto 20
        if (IER.eq.0) then
          K=0
          CALL EGETW(OUTSTR,K,WORD,'W','data id',IFLAG)
          if (WORD(1:4).eq.'*snm') then
            CALL EGETW(OUTSTR,K,SCENE(I),'W','scene name',IFLAG)
          elseif (WORD(1:4).eq.'*rnm') then
C Entry for .rif file found:
            irif=irif+1 ! increment the number of rif files in this scene
            CALL EGETW(OUTSTR,K,tmp_rifname,'W','rif name',IFLAG)
            ! evaluate for which zone this rif file (and thus the scene) is used !! ((18.09.09 TODO !!))
            !! ***
			      ! ifocz is set in RRIF:
            call RRIF(ITA2,ITA3,tmp_rifname,'s',IER)
			      ifocz_tmp(irif)=ifocz ! store current value of ifocz
            if (irif .gt. 1) then
			        if (ifocz_tmp(irif-1) .ne. ifocz) then
                ! if subsequent rifs in this Scene have different zone => fatal error!
                call usrmsg('Scene rif files use different zones',
     &			                                          '  ','F')
			        endif
			      endif
			      write(RIFNAME(ifocz,I,irif),'(a)')tmp_rifname
			      ! if subsequent scenes have same zone and same purpose => fatal error! ((how???))
            !! ***
C First rif name corresponds to default blind state, subsquent rifs within 
C this scene represent alternate blind states (when no control is defined in .tmc file, NABS=0).
            if (irif.gt.(NABS+1)) then
             call usrmsg('Scene has too many rif files.','  ','W')
            endif
c End of .rif file logic
          elseif (WORD(1:4).eq.'*srt') then
            CALL EGETW(OUTSTR,K,SCENERT(I),'W','scene root',IFLAG)
          elseif (WORD(1:4).eq.'*rde') then
            CALL EGETRM(OUTSTR,K,SCENEDESC(I),'W','scene desc',IFLAG)
          elseif (WORD(1:4).eq.'*rpu') then
            CALL EGETW(OUTSTR,K,SCENEPURP(I),'W','scene purp',IFLAG)
          elseif (WORD(1:4).eq.'*rgd') then
            CALL EGETW(OUTSTR,K,LDFGRID,'W','grid data',IFLAG)
          elseif (WORD(1:4).eq.'*slc') then
            CALL EGETW(OUTSTR,K,LDFGRID,'W','sensor data',IFLAG)
          else
            call usrmsg(' Unrecognised data prefix.',' ','W')
          endif
        endif
        goto 30
 20     continue
        NBSRIF(ifocz,I)=irif ! store number of rif files for this scene
 10   continue ! loop through all scenes in .rcf file

      CALL ERPFREE(IRCFG,ISTAT)
      RETURN
      END

C ********************* RADCFGOUT *********************
C RADCFGOUT writes the contents of the radiance configuration file.
      SUBROUTINE RADCFGOUT(ICHK)
#include "building.h"
#include "e2r_common.h"

      integer lnblnk  ! function definition

      common/FILEP/IFIL
      common/expath/runpath,pathtype

      character OUTS*72
      character dstmp*24,pfile*72,runpath*72,pathtype*24
      integer lnrp  ! length of run path
      integer irif ! loop variable for more than one rif per scene

C Check that ICHK is a sensible value.
      lnrp=lnblnk(runpath)
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
      write(IRCFG,'(a,a)') dstmp,' # latest file modification.'

C Now write number of rif files contained.
      write(IRCFG,'(i3,a)') NSCENE,' # No. of scenes'
      
C For each scene write 1) name, 2) rif name(s), 3) root identifier. 4) short description 
C and 5) intended use/ purpose as well as optional data


      do 10 I=1,NSCENE

C first, we must make sure that we have the correct zone value for the current scene ...
        do 55 IZ=1,NCOMP
          if (RIFNAME(IZ,I,1)(1:4) .ne. 'UNKN') then
            ! set focus zone to correct value for first .rif in current scene
            ifocz=IZ
          endif
55      continue

C Find the longest string in this scene.
        IML=10
        ISLENG=LNBLNK(SCENE(I))
        IF(ISLENG.GT.IML)IML=ISLENG
        do 66 irif= 1,NBSRIF(ifocz,I)
          ISLENG=LNBLNK(RIFNAME(ifocz,I,irif))
          IF(ISLENG.GT.IML)IML=ISLENG
 66     continue

        ISLENG=LNBLNK(SCENERT(I))
        IF(ISLENG.GT.IML)IML=ISLENG
        ISLENG=LNBLNK(SCENEDESC(I))
        IF(ISLENG.GT.IML)IML=ISLENG
        ISLENG=LNBLNK(SCENEPURP(I))
        IF(ISLENG.GT.IML)IML=ISLENG
C 1)
        write(IRCFG,'(a,i3)') '# Scene number',I
        OUTS=SCENE(I)
        write(IRCFG,'(a,a,a)')'*snm ',OUTS(1:IML),' # Scene name'

C 2)
        do 67 irif= 1,NBSRIF(ifocz,I)
          if (scenepurp(I)(1:6) .eq. 'Extern') then
            OUTS=RIFNAME(1,I,irif)
          else
            OUTS=RIFNAME(ifocz,I,irif)
          endif
          write(IRCFG,'(a,a,a)')'*rnm ',OUTS(1:IML),' # Rif name'
          if (scenepurp(I)(1:6) .eq. 'Extern') goto 68 !! 21/09/09 experimental
 67     continue
 68     continue ! jump tag for 'Extern' scene; 21/09/09 experimental

C 3), 4) and 5)
        OUTS=SCENERT(I)
        write(IRCFG,'(a,a,a)')'*srt ',OUTS(1:IML),' # Scene prefix'
        OUTS=SCENEDESC(I)
        write(IRCFG,'(a,a,a)')'*rde ',OUTS(1:IML),' # Description'
        OUTS=SCENEPURP(I)
        write(IRCFG,'(a,a,a)')'*rpu ',OUTS(1:IML),' # Intended use'

        if (SCENEPURP(I)(1:8).eq.'Day_fact') then
          OUTS=LDFGRID
          write(IRCFG,'(a,a,a)')'*rgd ',OUTS(1:IML),' # DF grid data'

        elseif (SCENEPURP(I)(1:8).eq.'Coupling') then
          OUTS=LDFGRID
          write(IRCFG,'(a,a,a)')'*slc ',OUTS(1:IML),
     &                                          ' # Sensor locations'
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

      COMMON/FILEP/IFIL
      common/pophelp/h(60)

      character MODE*1, H*72, VERT*24, KEY*1

      dimension VERT(35)

      LOGICAL SELECT,delzone,cpyzone,modscene

      integer irif ! loop variable for more than one rif per scene
      
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
          WRITE(VERT(M),14)KEY,SCENE(L)
   14     FORMAT(A1,1X,A12)
        ENDIF
   10 CONTINUE

C Number of actual items displayed.
      MVERT=M+MCTL

C If a long list include page facility text and info on portion seen.      
      IF(IPFLG.EQ.0)THEN
        VERT(M+1)=  '  ------------------'
      ELSE
        WRITE(VERT(M+1),15)IPM,MPM 
   15   FORMAT ('0 page part: ',I1,' -- of:',I1)
      ENDIF

C If MODE has been passed as a M then add an item at the end allowing
C list to be modified otherwise blank choice.  If user is browsing
C no need to show additional options.
      IF(MODE.EQ.'M'.OR.MODE.EQ.'m')THEN
        VERT(M+2)='* add/delete/copy  '
        VERT(M+3)  ='? help           '
        VERT(M+4)  ='- exit this menu '
      ELSE
        VERT(M+2)  ='? help           '
        VERT(M+3)  ='- exit this menu '
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
        H(1)='A scene will be selected from those available'
        if (MODE.EQ.'M'.OR.MODE.EQ.'m') then
          H(2)='The scene list can be modified by selecting *'
        else
          H(2)='  '
        endif
        H(3)='The < command selects by index rather than a-z.'
        CALL PHELPD('scene section',3,'-',0,0,IER)
      ELSEIF(IVERT.GT.MHEAD.AND.IVERT.LT.(MVERT-MCTL+1))THEN

C Decode from the potentially long list to the zone number via KEYIND.
C If delete or copy scene previously selected then new selection should 
C be deleted or copied, respectively.
        CALL KEYIND(MVERT,IVERT,IFOC,IO)
        SELECT=.TRUE.
        ISCENE=IFOC
        if(delzone)then
          do 1110 I=IFOC,NSCENE
            if (I.eq.NSCENE) then
              SCENE(I)='UNDEFINED'
              do 66 irif= 1,NBSRIF(ifocz,I)
                RIFNAME(ifocz,I,irif)='UNKNOWN'
 66           continue
              SCENERT(I)='UNKNOWN'
              SCENEDESC(I)='UNKNOWN'
              SCENEPURP(I)='UNKNOWN'
            else
              SCENE(I)=SCENE(I+1)
              do 67 irif= 1,NBSRIF(ifocz,I+1)
                RIFNAME(ifocz,I,irif)=RIFNAME(ifocz,I+1,1) ! ?? logic sound ??
 67           continue
              SCENERT(I)=SCENERT(I+1)
              SCENEDESC(I)=SCENEDESC(I+1)
              SCENEPURP(I)=SCENEPURP(I+1)
            endif
 1110     continue
          NSCENE=NSCENE-1
          ISCENE=1
          modscene=.true.
        elseif(cpyzone)then
          NSCENE=NSCENE+1
          SCENE(NSCENE)=SCENE(IFOC)
          do 68 irif= 1,NBSRIF(ifocz,IFOC)
            RIFNAME(ifocz,NSCENE,irif)=RIFNAME(ifocz,IFOC,irif)
 68       continue
          SCENERT(NSCENE)=SCENERT(IFOC)
          SCENEDESC(NSCENE)=SCENEDESC(IFOC)
          SCENEPURP(NSCENE)=SCENEPURP(IFOC)
          ISCENE=NSCENE

C Check new scenes data by calling newscene.  As newscene adds one to NSCENE 
C subtract one just now, no other data will be effected.
          NSCENE=NSCENE-1
          call usrmsg(' Edit copied scene.',' ','W')
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
          do 69 irif= 1,NBSRIF(ifocz,iscene)
            call RRIF(ita2,ita3,rifname(ifocz,iscene,irif),'r',ier)
 69       continue
          RETURN
        endif
      endif

C Scene list manipulation.
      IF(IVERT.EQ.(MVERT-2))THEN

C Allow a scene to be added or deleted.
        IF(MODE.EQ.'M'.OR.MODE.EQ.'m')THEN
          H(1)='As with other entities a standard set of add/delete'
          H(2)='and copy functions is provided. '
          CALL EASKATOG(' ','Action: ','add scene','delete scene',
     &      'copy scene','cancel',' ',' ',' ',IW,2)
          if(IW.EQ.1)then
            delzone = .false.
      H(1)='Radiance desktop creates allows the creation of separate'
      H(2)='scenes each with a specific purpose. A modest scene can be'
      H(3)='created silently with assumed defaults. '
      H(4)=' '
      H(5)='Once you have created a scene you can return to the'
      H(6)='definition section to update or add detail to the model.'
            CALL EASKABC(' ','Create scene:','silently',
     &        'interactively','cancel',IWDV,6)
            if (IWDV.eq.1) then
              call newscene('c')
              act='Create'
              call autorad
              ICHK=4
              call radcfgout(ICHK)
            elseif (IWDV.eq.1) then
              call newscene('-')
              act='Create'
              call autorad
              ICHK=4
              call radcfgout(ICHK)
            else
              goto 92
            endif
            ICHK=4
            call RADCFGOUT(ICHK)
            RETURN
          elseif(IW.EQ.2)then
            delzone = .true.
            call usrmsg(' ','Select scene to delete. ','-')
            IVERT=-2
            goto 3
          elseif(IW.EQ.3)then
            cpyzone = .true.
            call usrmsg(' ','Select scene to copy. ','-')
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
C if MODE='c' (create) then allow all purposes and auto name files.
      SUBROUTINE newscene(MODE)
#include "building.h"
#include "geometry.h"
#include "e2r_common.h"

      integer lnblnk  ! function definition

      common/pophelp/h(60)

      integer ncomp,ncon
      common/C1/NCOMP,NCON
      common/C21/IFCFG,cfgroot,LCFGF

      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)

      dimension IVALS(MCOM)

      character cfgroot*24,LCFGF*72
      character h*72,MODE*1,outs*124
      character dname*28,tname*28
      character dname72*72,tname72*72

C Ask for purpose of scene if new scene.
      if (MODE.eq.'s') then
        IZONE=0
        IRP=0
        if (aim(1:8).eq.'External') then
          IRP=1
c          izone=-1 ! does not seem necessary
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
        elseif (aim(1:7).eq.'Sun_pos') then
          IRP=7
c          izone=-1 ! does not seem necessary
        endif
      elseif (MODE.eq.'a') then
        H(1)='The purpose of the scene is used when creating the '
        H(2)='scene description files. It is also used to control'
        H(3)='the application of Radiance.'
        ino=0
        idno=1
        call MENUATOL('Select a default scene.',' Default scene',
     &    'a External images','b Internal images','c Glare sources',
     &    'd Daylight factors',' ',' ',
     &    ' ',' ',' ',' ',' ',' ',ino,idno,3)
        IRP=INO
      else
        H(1)='The purpose of the scene is used when creating the '
        H(2)='scene description files. It is also used to control'
        H(3)='the application of Radiance.'
        ino=0
        idno=1
        call MENUATOL('Select a scene purpose.',' Scene purpose',
     &    'a External images','b Internal images','c Glare sources',
     &    'd Daylight factors','e Coupling','f Daylight coefficients',
     &    ' ',' ',' ',' ',' ',' ',ino,idno,3)
        IRP=INO
      endif

C If no purpose then exit.
      if(IRP.eq.0)return

C Check for tmc file
      call inqu_tmc

C Reset glazing treatment to default.
      iglzty=1

C Increment scene counter
      NSCENE=NSCENE+1

C Set new scene to focus scene.
      ISCENE=NSCENE

C Reset the number of chosen zones (nzg) to NCOMP and re-initialize nznog() .. for what???
      nzg=NCOMP
      izgfoc=0
      do 3 I=1,NCOMP
        nznog(I)=I
 3    continue

C Now ask for zone for internal and glare purposes.
      if (IRP.eq.2 .or.IRP.eq.3 .or.IRP.eq.4 .or.
     &                  IRP.eq.5 .or.IRP.eq.6) then
        if (IZONE.le.0) then
          H(1)=' Pick a zone as the focus zone for the Radiance'
          H(2)=' internal image calculations.'
 30       INPIC=1
          CALL EPICKS(INPIC,IVALS,' ',' Pick a focus zone: ',
     &        12,NCOMP,zname,' zone list',IER,2)
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
        intext = 0
        imgqua = 'Medium'
        detlvl = 'Medium'
        llvar  = 'High'
        indrcb = 1
        ipicx  = 258
        penumb = 'False'
        crenrp = '-av 10 10 10 '
        coconv = '  '
        ifocz = 1 ! use zone 1 as focus for external view (RIFNAME !)
      elseif (IRP.eq.2) then 

C Internal image selected.
        SCENEPURP(ISCENE)='Internal'
        intext = 1
        imgqua = 'Medium'
        detlvl = 'Medium'
        llvar  = 'High'
        indrcb = 1
        ipicx  = 258
        penumb = 'False'
        crenrp = '-av 1 1 1 '
        coconv = '  '
      elseif (IRP.eq.3) then 

C Glare image selected.
        SCENEPURP(ISCENE)='Glare'
        intext = 1
        imgqua = 'Medium'
        detlvl = 'Medium'
        llvar  = 'High'
        indrcb = 2
        ipicx  = 258
        penumb = 'False'
        crenrp = '-av 0 0 0 '
        coconv = '  '
      elseif (IRP.eq.4) then 

C Daylight factor calculation selected.
        SCENEPURP(ISCENE)='Day_fact'
        intext = 1
        imgqua = 'High'
        detlvl = 'Medium'
        llvar  = 'High'
        indrcb = 2
        ipicx  = 258
        penumb = 'True'
        crenrp = '-av 0 0 0 -ab 1'
        coconv = '  '
      elseif (IRP.eq.5) then 

C Coupling calculation selected. (thermal/lighting simulation)
        SCENEPURP(ISCENE)='Coupling'
        intext = 1
        imgqua = 'High'
        detlvl = 'Medium'
        llvar  = 'High'
        indrcb = 2
        ipicx  = 258
        penumb = 'False'
        crenrp = '-av 0 0 0  '
        coconv = '  '
      elseif (IRP.eq.6) then 

C Daylight coefficient calculation selected. (thermal/lighting simulation)
        SCENEPURP(ISCENE)='Day_coef'
        intext = 1
        imgqua = 'High'
        detlvl = 'Medium'
        llvar  = 'High'
        indrcb = 2
        ipicx  = 258
        penumb = 'False'
        crenrp = '-av 0 0 0 '
        coconv = '  '
      elseif (IRP.eq.7) then 

C Sun positions.
        SCENEPURP(ISCENE)='Sun_pos'
        intext = 0
        imgqua = 'Medium'
        detlvl = 'Medium'
        llvar  = 'High'
        indrcb = 1
        ipicx  = 258
        penumb = 'False'
        crenrp = '-av 10 10 10 '
        coconv = ' '
c        ifocz = -1
      endif

C Set default names based on above.
      if (IRP.eq.2 .or.IRP.eq.3 .or.IRP.eq.4 .or.
     &                  IRP.eq.5 .or.IRP.eq.6) then
        write (outs,'(3a)') zname(ifocz)(1:lnzname(ifocz)),
     &     '_',SCENEPURP(ISCENE)(1:6)
        write (SCENEDESC(ISCENE),'(5a)') 'Scene of ',
     &     zname(ifocz)(1:lnzname(ifocz)),' for ',
     &     SCENEPURP(ISCENE)(1:lnblnk(SCENEPURP(ISCENE))),
     &           ' analysis.'
      else
        IL=min(lnblnk(cfgroot),21)
        write (outs,'(3a)') cfgroot(1:IL),'_',SCENEPURP(ISCENE)(1:6)
        write (SCENEDESC(ISCENE),'(a,a,a,a,a)') 'Scene of ',
     &     cfgroot(1:lnblnk(cfgroot)),' for ',
     &     SCENEPURP(ISCENE)(1:lnblnk(SCENEPURP(ISCENE))),
     &            ' analysis.'
      endif
      write (SCENE(ISCENE),'(a)') outs(1:28)
      write (SCENERT(ISCENE),'(a)') outs(1:28)
      do 66 irif= 1,NBSRIF(ifocz,iscene)
        write (rifname(ifocz,iscene,irif),'(a,a)') 
     &      SCENERT(ISCENE)(1:lnblnk(SCENERT(ISCENE))),'.rif'
 66   continue

C Additional details for specific purposes.
      if (IRP.eq.4) then

C Set up daylight factor calc points.
        call setdfgrid(MODE,izone,ier)
      elseif (IRP.eq.5.or.IRP.eq.6) then 

C Get sensor information from cgc file.
        call SENLOC
      endif

C Clear command line variables.
      zone='NONE'
      aim='NONE'
      act='NONE'

C If not in silent mode then confirm names with user.
      if (MODE.eq.'-') then

C Ask for scene name.
 5      H(1)='Many Radiance models may be created for a given ESP-r '
        H(2)='thermal model. Each of these Radiance models is called'
        H(3)='a scene, e.g. one scene may be of the buildings '
        H(4)='exterior and another could be an internal image of a'
        H(5)='zone. Each scene can have multiple viewpoints.'
        tname=SCENE(ISCENE)
        dname=SCENE(ISCENE)
        CALL EASKS(tname,'Scene name? ',' ',28,dname,'scene name',IER,5)
        IF(tname(1:2).EQ.'  ') GOTO 5
        SCENE(ISCENE)=tname

C Ask for root name and use this for rif name.
 15     H(1)='The scene prefix (one word) is used in the creation of'
        H(2)='the rif file name and as a root name for the description,'
        H(3)='view, octree and image files. '
        tname=SCENERT(ISCENE)
        dname=SCENERT(ISCENE)
        CALL EASKS(tname,'Scene root name?',' ',28,dname,'root name',
     &    IER,3)
        IF(tname(1:2).EQ.'  ') GOTO 15
        SCENERT(ISCENE)=tname
        do 67 irif= 1,NBSRIF(ifocz,iscene)
          write (rifname(ifocz,iscene,irif),'(a,a)') 
     &      SCENERT(ISCENE)(1:lnblnk(SCENERT(ISCENE))),'.rif'
 67     continue

C Ask for additional description.
 25     H(1)='The description is used as an additional aide to'
        H(2)='identify a scenes contents.'
        tname72=SCENEDESC(ISCENE)
        dname72=SCENEDESC(ISCENE)
        CALL EASKS(tname72,' Description ? ',' ',72,
     &                                    dname72,'scene desc',IER,2)
        IF(tname72(1:2).EQ.'  ') GOTO 25
        SCENEDESC(ISCENE)=tname72
      endif

C Call rifset to set up all remaining data.
      call rifset(ifocz,'f')

      RETURN
      END

C ********************** ADDSCENE **********************
C ADDSCENE allows the creation of a default translation from ESP-r to radiance.
      subroutine ADDSCENE(IRCFG)
#include "building.h"
#include "e2r_common.h"

      integer lnblnk  ! function definition

      common/pophelp/h(60)

      common/expath/runpath,pathtype

      CHARACTER H*72,runpath*72,pathtype*24,pfile*72
      integer lnrp  ! length of run path

C Ask if a default image should be created.
      lnrp=lnblnk(runpath)
      H(1)='Radiance desktop creates allows the creation of separate'
      H(2)='scenes each with a specific purpose. A modest scene can be'
      H(3)='created silently with assumed defaults. '
      H(4)=' '
      H(5)='Once you have created a scene you can return to the'
      H(6)='definition section to update or add detail to the model.'
      CALL EASKABC('This is a new visualisation.',
     &        'Create scene:','silently','interactively',
     &        'cancel',IWDV,6)
      if(IWDV.eq.1)then

C Check for command line information.
        call tstamp('>','E2R: creating default scene')
        if (zone(1:4).ne.'NONE'.and.aim(1:4).ne.'NONE') then
          call checkrif('n')
        else
          call newscene('a')
          act='Create'
          call autorad
          ICHK=4
          call radcfgout(ICHK)
        endif
      elseif(IWDV.eq.2)then

C Create new scene file.
        call tstamp('>','E2R: controlling default scene')
        write (pfile,'(a,a)') runpath(1:lnrp),LRADCF(1:lnblnk(LRADCF))
        call ERPFREE(IRCFG,ISTAT)
        call FPOPEN(IRCFG,ISTAT,1,3,pfile)
        call newscene('-')
        act='Create'
        call autorad
        ICHK=4
        call radcfgout(ICHK)
      endif

      RETURN
      END

C ********************** setdfgrid **********************
C setdfgrid allows the creation of the daylight factor grid points.
C if MODE is not - then sensor points file name confirmation required.
      subroutine setdfgrid(MODE,izone,ier)
#include "building.h"
#include "geometry.h"
#include "e2r_common.h"

      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/FILEP/IFIL
      common/pophelp/h(60)

      common/expath/runpath,pathtype

      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV

      COMMON/RAY2/ITDSP,ITBND,ITEPT,ITZNM,ITSNM,ITVNO,ITORG,ITSNR,
     &            ITOBS,ITHLS,ITHLZ,ITGRD,GRDIS,ITPPSW

      COMMON/RAY3/MODIFY,MODLEN,MODBND
      LOGICAL MODIFY,MODLEN,MODBND

      COMMON/RAY6/LINSTY(MCON)

      integer ncomp,ncon
      common/C1/NCOMP,NCON
      integer nccode,indutl
      character LSNAM*72,LPROJ*72,LGEOM*72,LSHAD*72,LTHRM*72,LUTIL*72
      common/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)

      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)

      character h*72,DESCRC*25
      character outs*124,runpath*72,pathtype*24,tfile*72
      character ITEM(MV)*15, t12*12, MODE*1

      dimension IVALS(MCOM), PEQN(4), ITEMV(MV)

      logical newgeo  ! to use for testing if new/old geometry file.
      integer lnrp  ! length of run path

C New grid so set number of grid points to zero.
      lnrp=lnblnk(runpath)
      NDFP=0
      newgeo=.false.  ! assume older format geometry.

C Remember current graphics display options.
      call SVDSOPT

C Ask for zone (if not defined), surface and working plane height.
      if (IZONE.le.0) then
        H(1)=' Pick a zone as the focus zone for the Radiance'
        H(2)=' daylight factor calculations.'
 32     INPIC=1
        CALL EPICKS(INPIC,IVALS,
     &        'Generate daylight factors for which zone. ',
     &        '(You will then be asked to grid a surface.) ',
     &        12,NCOMP,zname,'Select zone',IER,2)
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
      MODIFY=.TRUE.
      MODLEN=.TRUE.
      MODBND=.TRUE.
      ITSNM=0
      CALL ADJVIEW(IER)

C Ask for surface and highlight.
      call EASKSUR(ifocz,IS,'-',
     &    'Select a surface to grid. (You can then transform ',
     &    'the grid perpendicular to this surface.)',IER)
      CALL INLNST(1)
      CALL SURADJ(ifocz,IS,IE,TMP,IZC,ISC,IC,DESCRC)
      LINSTY(IC)=2
      MODIFY=.TRUE.
      ITVNO=0
      CALL ADJVIEW(IER)
      CALL INLNST(1)

      H(1)='The grid points on the chosen surface can be '
      H(2)='transformed along the surface normal to give points in'
      H(3)='a parallel plane.  This is useful for calculating'
      H(4)='daylight factors on the working plane.'
      H(5)='The grid can be transformed to the inside or outside'
      H(6)='of the surface, and must be at least 1mm from the'
      H(7)='surface.'
      WPDIST=0.9
      CALL EASKR(WPDIST,' Distance of grid from surface ? ',' ',0.001,
     &                          'F',2.5,'W',0.9,'grid transform',IER,7)
      H(1)='The grid can be transformed to the inside or outside'
      H(2)='of the surface.  The default is inside.'
      call EASKAB('Transform grid to: ',' ','Inside','Outside',IW,2)
      if (IW.eq.1) then
        dirf=-1.
      else
        dirf=1.
      endif

C Now read zone geometry file to enable edge selection.
      ITA1 = IFIL+6
      ITRC=0
      call eclose(gversion(ifocz),1.1,0.01,newgeo)
      if(newgeo)then
        call georead(ITA1,LGEOM(ifocz),ifocz,0,ITRC,IUOUT,IER)
      else
        call EGOMIN(ITA1,LGEOM(ifocz),ifocz,0,ITRC,IUOUT,IER)
      endif

C Generate a list of edges. JVN(MS,MV)
      NI=0
      do 10 I=1,NVER(IS)-1
        write(ITEM(I),'(2(a,i3))')'edge v',JVN(IS,I),'->v',JVN(IS,I+1)
        NI=NI+1
 10   continue
      NI=NI+1
      write(ITEM(NI),'(2(a,i3))')'edge v',JVN(IS,NI),'->v',JVN(IS,1)
 12   INPIC=1
      H(1)='The grid will be formed in rows parallel '
      H(2)='to the selected edge.'
      CALL EPICKS(INPIC,ITEMV,
     &        'Select an edge (defined by vertex numbers). ',
     &        'The grid will be parallel to this edge. ',
     &        15,NI,ITEM,'Select edge',IER,2)
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
      H(1)=' A bounding box is created for the selected surface.'
      H(2)=' The grid is specified by intersection points of lines'
      H(3)=' parallel and perpendicular to the selected edge.'
      H(4)=' The grid lines are equally spaced, starting half the'
      H(5)=' width of a space from the edge, e.g. '
      H(6)= '      ||     |          |'
      H(7)= '      ||     |          |'
      H(8)= '      ||<--->|<-------->|'
      H(9)= '      || x/2 |     x    |'
      H(10)='      ||     |          |'
      H(11)='     edge    |          grid line 2'
      H(12)='           grid line 1'

C Major grid lines are parallel with the selected edge.
      igmajor=4
      igminor=6
      write (t12,'(2i4)') igmajor,igminor
 25   CALL EASKS(t12,'Grid density? (First item is number of grid',
     &      'points in the rows parallel to the chosen edge.) ',
     &      12,' 2  2','grid density',IER,12)
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
      write (LDFGRID,'(a,a4)')
     &          SCENERT(NSCENE)(1:lnblnk(SCENERT(NSCENE))),'.grd'
      H(1)=' Supply a local file name (i.e. without a path).'
      if (MODE.eq.'-') then
 55     CALL EASKS(LDFGRID,'Daylight factor grid points file name?',
     &    ' ',72,'points.grd','df points file name',IER,1)
        IF(LDFGRID.EQ.' ') GOTO 55
      endif

C Open df points file in working directory.
      write (tfile,'(a,a)') runpath(1:lnrp),LDFGRID(1:lnblnk(LDFGRID))
      call FPOPEN(ITA1,ISTAT,1,3,tfile)
      do 552 IC=1,3
        PEQN(IC)=PEQN(IC)*dirf
 552  continue

C Write points and sensor direction (plane normal).
      do 555 IV=1,NDFP
       write(ITA1,'(6(f8.4,1x))')(DFPTS(IV,IX),IX=1,3),
     &         (PEQN(IY),IY=1,3)
 555  continue
      DFDIR(1)=PEQN(1)
      DFDIR(2)=PEQN(2)
      DFDIR(3)=PEQN(3)
      call ERPFREE(ITA1,ISTAT)

C Display grid on the zone... (if in graphic mode)
      if (MMOD.EQ.8) then
        call edisp(iuout,'  ')
        write (outs,'(a,a)') 'The proposed daylight factor calculation',
     &                  ' points are shown as'
        call edisp(iuout,outs)
        call edisp(iuout,'dots in the graphic feedback.')
        MODIFY=.TRUE.
        ITZNM=1
        ITSNM=1
        ITVNO=1
        CALL ADJVIEW(IER)
        CALL DFGRIDLBL('g')
      endif

C Reset the number of chosen zones (nzg) to NCOMP.
      nzg=NCOMP
      izgfoc=0
      do 300 I=1,NCOMP
        nznog(I)=I
 300  continue

C Reset display options.
      call RCDSOPT

      return
      end

C ********************** DFGRIDLBL **********************
C DFGRIDLBL writes out an index of daylight grid point (act = g|G) or
C the daylight factor (act = d|D).
      SUBROUTINE DFGRIDLBL(act_1)
#include "building.h"
#include "prj3dv.h"
#include "e2r_common.h"
      
      integer lnblnk  ! function definition

C      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL
      common/expath/runpath,pathtype
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      integer ifs,itfs,imfs
      COMMON/GFONT/IFS,ITFS,IMFS

      COMMON/RAY2/ITDSP,ITBND,ITEPT,ITZNM,ITSNM,ITVNO,ITORG,ITSNR,
     &            ITOBS,ITHLS,ITHLZ,ITGRD,GRDIS,ITPPSW

      COMMON/RAY3/MODIFY,MODLEN,MODBND
      LOGICAL MODIFY,MODLEN,MODBND

      DIMENSION COG1(3),COG2(3),COG3(3)

      CHARACTER VT*5,act_1*1
      character OUTSTR*124,tfile*72
      character runpath*72,pathtype*24
      integer lnrp  ! length of run path

#ifdef OSX
      integer iix,iiy,iid1,iid2
#else
      integer*8 iix,iiy,iid1,iid2
#endif
      
C If not in graphical mode return.
      if (MMOD.ne.8) return

C If there are no grid points warn the user and check if
C there is a *.df file or a *.df- file that can be scanned again.
      if(NDFP.eq.0)then
        ITA1 = IFIL+6
        lnrp=lnblnk(runpath)

        call ERPFREE(ITA1,ISTAT)
        write(tfile,'(a,a)')runpath(1:lnrp),
     &                      LDFGRID(1:lnblnk(LDFGRID))
        call FPOPEN(ITA1,ISTAT,1,1,tfile)
        if(ISTAT.eq.0)then
          continue
        else
          call ERPFREE(ITA1,ISTAT)
          write(tfile,'(3a)')runpath(1:lnrp),
     &                      LDFGRID(1:lnblnk(LDFGRID)),'-'
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
      MODIFY=.TRUE.
      MODLEN=.TRUE.
      MODBND=.TRUE.
      ITZNM=1
      ITSNM=1
      ITVNO=1
      CALL ADJVIEW(IER)

C Loop through grid points.
      do 10 I=1,NDFP
        COG1(1)=DFPTS(I,1)
        COG1(2)=DFPTS(I,2)
        COG1(3)=DFPTS(I,3)
        CALL VECTRN(COG1,TEMAT,COG2,IER)
        CALL VECTRN(COG2,ETSMAT,COG3,IER)
        call u2pixel(COG3(1),COG3(2),iix,iiy)
        CALL ecirc(iix,iiy,3,1)
        if(act_1.eq.'g'.or.act_1.eq.'G')then
          IF(I.le.9)THEN
            WRITE(VT,'(i1)') I
          ELSEIF(I.gt.9.and.I.le.99)THEN
            WRITE(VT,'(i2)') I
          ELSEIF(I.gt.99)THEN
            WRITE(VT,'(i3)') I
          ENDIF
        else
          IF(DFVALS(I).lt.10.0)THEN
            WRITE(VT,'(f4.2)') DFVALS(I)
          ELSEIF(DFVALS(I).ge.10.0)THEN
            WRITE(VT,'(f4.1)') DFVALS(I)
          ENDIF
        endif
        iid1=iix+1; iid2=iiy-1;
        CALL wstxpt(iid1,iid2,VT)
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
#include "e2r_common.h"

      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL
      common/expath/runpath,pathtype

      COMMON/CGCIN2N/NCGTC(MCOM,MDTY),NLITZ(MCOM),IDFST(MCOM,MLCOM),
     & CGX(MCOM,MLCOM,MDF),CGY(MCOM,MLCOM,MDF),CGH(MCOM,MLCOM,MDF),
     & UX(MCOM,MLCOM,MDF),UY(MCOM,MLCOM,MDF),UH(MCOM,MLCOM,MDF),
     & SETPT(MCOM,MLCOM),SYSPER(MCOM,MLCOM),SOFFLL(MCOM,MLCOM),
     & IOFFDT(MCOM,MLCOM),SMLOUT(MCOM,MLCOM),SMEOUT(MCOM,MLCOM)

      COMMON/DAYF/NDF(MCOM,MLCOM),DFDAT(MCOM,MLCOM,MGT,MDF),
     & AZIJL(MCOM,MGT),SUMDF(MCOM,MLCOM,MDF),NUDWIN(MCOM)

      common/UDESC/LVIEW(MCOM),LHCCO(MCOM),
     &             LTWIN(MCOM),LCGCIN(MCOM),ZOBS(MCOM)

      CHARACTER*72 LVIEW,LHCCO,LTWIN,LCGCIN,ZOBS
      CHARACTER*72 LCGCINT
      character outs*124,runpath*72,pathtype*24

      logical XST

Cag@02072009 comment out following verbosity (5 lines):
C      write(outs,'(a)')'Reading casual gain control file.'
C      call edisp (iuout,outs)
c      write(outs,'(a,a)')'Searching for sensor locations ',
C     &                                        'and orientations.'
C      call edisp (iuout,outs)

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
      write(LDFGRID,'(a,a)')
     &    SCENERT(ISCENE)(1:lnblnk(SCENERT(ISCENE))),'.slc'

C Check for file (add working dir path)
      write (outs,'(a,a)') runpath(1:lnblnk(runpath)),
     &                                     LDFGRID(1:lnblnk(LDFGRID))
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
#include "e2r_common.h"
      
      integer lnblnk  ! function definition

      COMMON/FILEP/IFIL

      common/C21/IFCFG,cfgroot,LCFGF
      common/expath/runpath,pathtype

      character filexst*1
      character cfgroot*24,LCFGF*72
      character pfile*72,runpath*72,pathtype*24

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
c        izone=-1 ! does not seem to be used by the code except in "newscene()"
      else
        if (aim(1:8).eq.'External') then
          izone=1 ! use zone 1 as "default zone"
          ifocz=1 ! also focus on this default zone
          zone='FRED'
        else
          call zindex(zone,izone)
        endif
      endif

C If rcf file does exist then search for existing rif file.
      if (filexst.eq.'o') then
        do 10 I=1,NSCENE
          if (SCENEPURP(I)(1:5).eq.aim(1:5)) then
C Possible match, further check - look at rif file if not UNKNOWN (not set) for zone,scene ...
            if (RIFNAME(izone,I,1)(1:4) .ne. 'UNKN') then
              call RRIF(ITA2,ITA3,RIFNAME(izone,I,1),'s',IER)
              if (ifocz.eq.izone) then
C Got match.
                ISCENE=I
C Update sensor information from cgc file.
                if(aim(1:8).eq.'Coupling'.or.aim(1:8).eq.'Day_coef')then
                  call SENLOC
                endif

C If daylight coefficients and act=create and model exists then issue warning.
                if(aim(1:8).eq.'Day_coef'.and.act(1:6).eq.'Create')then
                  call usrmsg('Model already exists!',
     &                         'Using existing model.','W')
                  act='Calculate'
                endif
                modscene=.true.
                return ! one match should be enough ...
              endif
            endif
          endif
 10     continue
      else

C Create a new rcf file.
        write (LRADCF,'(a,a)') cfgroot(1:lnblnk(cfgroot)),'.rcf'

C Set file name including path and open.
        write(pfile,'(a,a)') runpath(1:lnblnk(runpath)),
     &                                         LRADCF(1:lnblnk(LRADCF))
        call ERPFREE(IRCFG,ISTAT)
        call FPOPEN(IRCFG,ISTAT,1,2,pfile)
      endif

C If not an existing scene then create it.
      if (.not.modscene) then
        call newscene('s')
        IFC=3
        do 66 irif= 1,NBSRIF(ifocz,iscene)
          call MKRIF(ITA2,rifname(ifocz,iscene,irif),IFC,0,IER)
c        if (NBSRIF(ISCENE).eq.1) then
c          call mkrif(ita2,LBSRIF(ISCENE),IFC,1,ier)
c        endif
 66     continue

C Update rcf file and configuration file.
        ICHK=3
        call RADCFGOUT(ICHK)
      endif

      return
      end


C ********************** RDCFTMP **********************
C RDCFTMP Read in daylight coefficients to common and rewrites to casual 
C gain control file.

      SUBROUTINE RDCFTMP(LFTMP,ISET)
#include "building.h"
#include "e2r_common.h"

      COMMON/OUTIN/IUOUT,IUIN
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

      common/UDESC/LVIEW(MCOM),LHCCO(MCOM),
     &             LTWIN(MCOM),LCGCIN(MCOM),ZOBS(MCOM)

      CHARACTER*72 LVIEW,LHCCO,LTWIN,LCGCIN,ZOBS
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
