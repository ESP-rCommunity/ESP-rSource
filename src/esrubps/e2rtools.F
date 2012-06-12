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

C e2rtools.F contains routines to fill the e2r commons from
C external programs (in this case, bps).

C RADCFGIN:  reads the contents of the radiance configuration file.
C ReadRIF: Read a Radiance rif file.



C ********************* ReadRCF *********************
C !!!!   C O P Y   o f   R A D C F G I N   f r o m   f i l e  radcfg.F !!!!
C RADRCFIN reads the contents of the radiance configuration file.
      SUBROUTINE ReadRCF(icomp)
#include "building.h"
#include "model.h"
#include "e2r_common.h"
      
#include "control.h"

      integer lnblnk  ! function definition

c      COMMON/OUTIN/IUOUT,IUIN
      common/FILEP/IFIL
      common/rpath/path

      common/user/browse

      character tmp_rifname*72

      character WORD*124, OUTS*124, OUTSTR*124
      character pfile*72,path*72

      integer ifocz_tmp(MBP+1)

      logical unixok
      logical browse
      
      IRCFG = IFIL+17
      icfcfil = IFIL+26 ! ? see list in bps.F ...

C from checkrif ... file pointers for .rif??
      ITA2=IFIL+7
      ITA3=IFIL+10
C --

C Set file name including path.
C !!! this assumes runpath is '../rad/'!
      write(runpath,'(a)') LRADCF(1:7)
      lnrp=lnblnk(runpath)
      write(LRADCF,'(a)') LRADCF(8:lnblnk(LRADCF))

      write (pfile,'(2a)')runpath(1:lnrp),LRADCF(1:lnblnk(LRADCF))
C DEBUG:
c      write(*,*)'** ReadRCF, pfile=',pfile(1:lnblnk(pfile)),
c     &               ' (runpath=',runpath(1:lnrp),
c     &               '; lradcf=',LRADCF(1:lnblnk(LRADCF)),')'

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
cx      write (outs,'(a,a)') 'Scenes last modified on ',
cx     &                                       OUTSTR(1:lnblnk(OUTSTR))
cx      call edisp (iuout,outs)

C Now read number of scenes contained.
      CALL STRIPC(IRCFG,OUTSTR,99,ND,1,'scene number line',IER)
      K=0
      CALL EGETWI(OUTSTR,K,NSCENE,1,MCOM+1,'W','no of scenes',IER)

      
C For each scene read at least 5 items of data.
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
            ! ifocz is set in ReadRIF:
            call ReadRIF(ITA2,ITA3,tmp_rifname,'s',IER)
            ifocz_tmp(irif)=ifocz ! store current value of ifocz
            if (icomp.eq.ifocz) ISCENE=I ! this is the one ...

            if (irif .gt. 1) then
              if (ifocz_tmp(irif-1) .ne. ifocz) then
              ! if subsequent rifs in this Scene have different zone => fatal error!
              call usrmsg('Scene rif files use different zones',
     &                                                    '  ','F')
			        endif
			      endif
            write(RIFNAME(ifocz,I,irif),'(a)')tmp_rifname
            ! if subsequent scenes have same zone and same purpose => fatal error! ((how???))
            !! ***
C First rif name corresponds to default blind state, subsquent rifs within 
C this scene represent alternate blind states (when no control is defined in .tmc file, NABS=0).
            if (irif.gt.(NABS+1)) then
             call usrmsg('Scene has too many rif files.','  ','W')
            elseif (irif.lt.(NABS+1)) then
             call usrmsg('Scene does not have enough files.','  ','W')
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


C ********************* ReadRIF *********************
C !!!!   C O P Y   o f   R R I F  f r o m   f i l e  e2rfile.F !!!!
C ReadRIF: Read a Radiance rif file.
C rscedes(xx) = Scene descriptor - order important.
C rscedes(1) = Scene descriptor for sky.
C rscedes(2) = Scene descriptor for glazings.
C rscedes(xx) = Scene descriptor for everything else.
C rzoncmd = ZONE command string e.g.  ZONE= Interior 4 10 1 7 0 2.3
C rupaxis  = Up axis: single character 'Z'   Default is Z
      
C imgqua = Image Quality [Low|Medium|High] Default is Low
C        ie. QUALITY= Medium
C detlvl = Detail level  [Low|Medium|High] Default is Medium but with
C        non-complex image then set to low.  High if for surface patterns.
C llvar = Light level Variability  [Low|Medium|High] Default is Low
C        but with daylighting should be high.  If outside then ??
C indrcb= Indirect calculation importance: integer 0-2  
C       0=direct light, 1=typically one reflection, 
C       2=more reflections (e.g.lightshelfs)
C ipicx  = Resolution or image width [X pixels]  Default is 512.
C penumb = Penumbras [False|True]  Default is False.  << ?? >>
C        ie. PENUMBRAS= True
C itrack = Progress reporting in whole minutes: 'REPORT= 10'

C crenrp = Extra rendering command lines for rpict. 
C coconv = Extra commands for oconv.

C NOTE: Octree path/file.  Relitive to rif folder and root as in 'rif' file.
C        'OCTREE= octree/lights.oct'
C NOTE: Pict path/file root name.  Relative to rif file folder.
C        'PICTURE= images/lights' get interpreted to images/lights_lit.pic

C vewcmds(20) Structure giving view information. First column is interpreted as
C          a short name for the view - examples below:
C   view= abs -vp 5 2 1.5 -vd 1 1 0 # -vp -vd syntax as in a file.
C rvewsh(10) Array of short view names for selection.
C RVB - reporting verbosity, 's'= silent.
C IERR=1 if a scene file is missing.

      SUBROUTINE ReadRIF(ITA2,ITA3,filepar,RVB,IERR)
#include "building.h"
#include "model.h"
#include "espriou.h"
#include "e2r_common.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      common/FILEP/IFIL
c      common/rpath/path
     
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON

      character tvewsh*10,tvewcmds*124
      character OUTS*124,OUTSTR*124,WORD*20,tfile*72
      character tsc*72,tword*10,filepar*72,RVB*1,fs*1

      logical unixok

      IERR=0

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

C Reset render and octree special options.
      crenrp='   '
      coconv=' -w '

C Reset once we know if outside or inside.
      indxscn=0
      indxvew=0
      surexpos= -1.00
      do 40 i=1,10
        rscedes(i)=' '
  40  continue

C Clear non-default views.
      do 41 i=1,20
        vewcmds(i)=' '
        rvewsh(i)=' '
  41  continue

C Attempt to read in rif file.
      write (tfile,'(3a)')radpth(1:lnblnk(radpth)),fs,
     &                      filepar(1:lnblnk(filepar))
      CALL ERPFREE(ITA2,ISTAT)
      call FPOPEN(ITA2,ISTAT,1,0,tfile)
      if (ISTAT.LT.0) then
        call usrmsg(' Problem detected while opening...',tfile,'W')
        return
      endif
      write(currentfile,'(a)') filepar(1:lnblnk(filepar))

C Test if e2r information structure can be recovered.
      if (RVB.ne.'s') then
        write (outs,'(a,a)') 'Scanning rif file: ',
     &                          filepar(1:lnblnk(filepar))
        call edisp(iuout,outs)
      endif
  39  READ(ITA2,'(a)',IOSTAT=ISTAT,END=102)OUTSTR
      if(OUTSTR(1:13).eq.'#e2r_gen_view')then
        K=14
        CALL EGETWI(OUTSTR,K,intext,0,1,'W','inside',IER)
        CALL EGETWI(OUTSTR,K,ifocz,-1,NCOMP,'W','zone',IER) ! set ifocz
        if (RVB.ne.'s') then
          call edisp(iuout,' Recovered interior/exterior.')
        endif
      elseif(OUTSTR(1:14).eq.'#e2r_glaz_type')then
        K=15
        CALL EGETWI(OUTSTR,K,iglzty,1,3,'W','glaz type',IER)
        if (RVB.ne.'s') then
          call edisp(iuout,' Recovered glazing treatment.')
        endif
      else
        goto 39
      endif
      goto 39

C Close and then re-open the file.
 102  continue
      ! call usrmsg('  ','  ','-')
      call ERPFREE(ITA2,ISTAT)
      call FPOPEN(ITA2,ISTAT,1,1,tfile)

      
C Now loop looking for key words and data.
 42   CALL STRIPC(ITA2,OUTSTR,99,ND,0,'rif line',IFLAG)
      IF(IFLAG.ne.0)goto 900
      K=0
      CALL EGETW(OUTSTR,K,WORD,'-','key word',IFLAG)
      IF(IFLAG.ne.0) then
        write(*,*)'** Blank lines in .rif file? Delete!!'
        goto 900
      endif
      if (WORD(1:2).EQ.'  ') then
        GOTO 42
      endif
C `rad` related contents.
      if(word(1:5).eq.'ZONE=')then
        CALL EGETRM(OUTSTR,K,rzoncmd,'W','ZONE commands',IER)
      elseif(word(1:8).eq.'QUALITY=')then
        CALL EGETW(OUTSTR,K,imgqua,'-','imgqua',IFLAG)
      elseif(word(1:11).eq.'RESOLUTION=')then
        CALL EGETWI(OUTSTR,K,ipicx,50,1024,'W','X res',IER)
      elseif(word(1:9).eq.'INDIRECT=')then
        CALL EGETWI(OUTSTR,K,indrcb,0,2,'W','INDIRECT bnc',IER)
      elseif(word(1:7).eq.'DETAIL=')then
        CALL EGETW(OUTSTR,K,detlvl,'-','DETAIL level',IFLAG)
      elseif(word(1:10).eq.'PENUMBRAS=')then
        CALL EGETW(OUTSTR,K,penumb,'-','PENUMBRAS T/F',IFLAG)
      elseif(word(1:12).eq.'VARIABILITY=')then
        CALL EGETW(OUTSTR,K,llvar,'-','VARIABILITY level',IFLAG)
      elseif(word(1:3).eq.'UP=')then
        CALL EGETW(OUTSTR,K,tword,'-','Up axis',IFLAG)
        write(rupaxis,'(a1)') tword(1:1)
      elseif(word(1:7).eq.'REPORT=')then
        CALL EGETWI(OUTSTR,K,itrack,0,10,'W','REPORT freq',IER)
      elseif(word(1:9).eq.'EXPOSURE=')then
        CALL EGETWR(OUTSTR,K,surexpos,-5.,5.,'W','EXPOSURE ',IER)
      elseif(word(1:6).eq.'oconv=')then
        CALL EGETRM(OUTSTR,K,coconv,'W','oconv commands',IER)

C Run time files.
      elseif(word(1:8).eq.'AMBFILE=')then
        CALL EGETRM(OUTSTR,K,rambfil,'W','AMBIENT file',IER)
      elseif(word(1:8).eq.'OPTFILE=')then
        CALL EGETRM(OUTSTR,K,optnfil,'W','OPTFILE file',IER)
      elseif(word(1:7).eq.'OCTREE=')then
        CALL EGETRM(OUTSTR,K,octfil,'W','octree file',IER)
      elseif(word(1:8).eq.'PICTURE=')then
        CALL EGETRM(OUTSTR,K,picfil,'W','picture file',IER)
      elseif(word(1:6).eq.'scene='.or.word(1:6).eq.'illum=')then

C Scene files.
C << Still require to parse files in the rscedes based hopefully on
C << the ending characters of the file.
        indxscn=indxscn+1
        CALL EGETRM(OUTSTR,K,rscedes(indxscn),'-','scene(s)',IER)
        tsc = rscedes(indxscn)
        if (tsc(1:2).eq.'  ') then
          if (RVB.ne.'s') then
            if (indxscn.eq.1) then
              call edisp(iuout,'Sky file undefined! ')
            elseif (indxscn.eq.2) then
              call edisp(iuout,'Glazing file undefined! ')
            endif
          endif
        else
          call figscene(ITA3,tsc,ier)
          if (ier.gt.1) IERR=1
        endif
      elseif(word(1:5).eq.'view=')then
        CALL EGETW(OUTSTR,K,tvewsh,'-','shrt vew',IFLAG)
        CALL EGETRM(OUTSTR,K,tvewcmds,'W','view cmds',IER)
        indxvew=indxvew+1
        rvewsh(indxvew)=tvewsh
        vewcmds(indxvew)=tvewcmds

C Echo and save render settings.
      elseif(word(1:7).eq.'render=')then
        CALL EGETRM(OUTSTR,K,crenrp,'-','render cmds',IER)
        if(crenrp(1:2).ne.'  '.and.RVB.ne.'s')then
          call edisp(iuout,'Render options set by expert user! ')
          call edisp(iuout,crenrp)
        endif
      else
        call edisp(iuout,' Unknown command line in rif file...')
        call edisp(iuout,OUTSTR)
      endif
      goto 42

900   continue
      ! call usrmsg('  ','  ','-')
      CALL ERPFREE(ITA2,ISTAT)

      return
      end


C ******* FIGSCEN
C !!!!   C O P Y   o f   F I G S C E N  f r o m   f i l e  e2rfile.F !!!!
C Attempt to judge the type of file in a scene description from the
C terminating character (or later the contents).
      subroutine figscene(ITA3,cmdname,ier)
#include "building.h"
#include "model.h"
#include "e2r_common.h"

      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN

      character cmdname*72,OUTSTR*124,tfile*72,fs*1

      logical XST,unixok

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif


      ier=0

C Attempt to read in file, note it is assumed to be one line.
      write(tfile,'(3a)')radpth(1:lnblnk(radpth)),fs,
     &                      cmdname(1:lnblnk(cmdname))
      INQUIRE (FILE=tfile,EXIST=XST)
      if (XST) then
        CALL ERPFREE(ITA3,ISTAT)
        call FPOPEN(ITA3,ISTAT,1,1,tfile)
        IF(IER.LT.0)THEN
          call usrmsg(' Problem while reading:',tfile,'W')
          return
        ENDIF
        READ(ITA3,'(a)',IOSTAT=ISTAT,END=102)OUTSTR
        if(OUTSTR(1:31).eq.    '# Radiance exterior composition')then
          write(rofil,'(a)') cmdname(1:lnblnk(cmdname))
          outdone=.true.
        elseif(OUTSTR(1:31).eq.'# Radiance interior composition')then
          write(rzfil,'(a)') cmdname(1:lnblnk(cmdname))
          indone=.true.
        elseif(OUTSTR(1:26).eq.'# Radiance opaque material')then
          write(matfil,'(a)') cmdname(1:lnblnk(cmdname))
        elseif(OUTSTR(1:18).eq.'# Radiance glazing')then
          write(glzfil,'(a)') cmdname(1:lnblnk(cmdname))
          glzdone=.true.
        elseif(OUTSTR(1:8).eq. '# gensky')then
          write(rskyfil,'(a)') cmdname(1:lnblnk(cmdname))
          skydone=.true.
        elseif(OUTSTR(1:11).eq. '# gendaylit')then
          write(rskyfil,'(a)') cmdname(1:lnblnk(cmdname))
          skydone=.true.
        elseif(OUTSTR(1:11).eq. '# sky patch')then
          write(rskyfil,'(a)') cmdname(1:lnblnk(cmdname))
          skydone=.true.
        elseif(OUTSTR(1:31).eq.'# Radiance CFC glazing definiti')then
          write(cfcfil,'(a)') cmdname(1:lnblnk(cmdname))
c          outdone=.true.
        else

C << needs to be fixed to hold additional (say 10) scene files >>
          call edisp(iuout,'The initial line of the file is: ')
          call edisp(iuout,OUTSTR)
          call edisp(iuout,'this has not be recognised and will be')
          call edisp(iuout,'REMOVED from the rif file.')
        endif
      else
        ier=1
        call usrmsg(' Could not find: ',tfile,'W')
      endif

      CALL ERPFREE(ITA3,ISTAT)
      return

 102  CALL ERPFREE(ITA3,ISTAT)
      call edisp(iuout,'Problem scanning the scene. ')
      ier=-1
      return

      end

