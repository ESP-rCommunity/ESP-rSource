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

C Routines to read in various radiance generated files.

C ******* Default views formation.
      SUBROUTINE RRVEW(ITA2,rviewf,IER)

#include "building.h"
#include "e2r_common.h"

C espriou.h provides current file.
#include "espriou.h"

      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN

      character OUTSTR*124,WORD*20,rviewf*72,outs*124
      character tfile*72

      IER=0

C Attempt to read in rview file, note it is assumed to be one line.
      write (tfile,'(2a)') runpath(1:lnrp),rviewf(1:lnblnk(rviewf))
      CALL ERPFREE(ITA2,ISTAT)
      call FPOPEN(ITA2,ISTAT,1,0,tfile)
      if (ISTAT.LT.0) then
        call usrmsg(' Problem detected while opening...',rviewf,'W')
        return
      endif
      write(currentfile,'(a)') tfile(1:lnblnk(tfile))

      CALL STRIPC(ITA2,OUTSTR,99,ND,1,'view line',IER)
      IF(IER.eq.2)ier=0
      K=0
      CALL EGETW(OUTSTR,K,WORD,'W','rview',IFLAG)
      IF(IFLAG.ne.0)goto 43
      if(word(1:5).eq.'rview')then
  42    CALL EGETW(OUTSTR,K,WORD,'-','command',IFLAG)
        IF(IFLAG.ne.0)goto 43
        if(word(1:3).eq.'-vp')then
          CALL EGETWR(OUTSTR,K,rvpx,0.,0.,'-','X view pt',IER)
          CALL EGETWR(OUTSTR,K,rvpy,0.,0.,'-','Y view pt',IER)
          CALL EGETWR(OUTSTR,K,rvpz,0.,0.,'-','Z view pt',IER)
          IF(IER.ne.0)goto 43
        elseif(word(1:3).eq.'-vd')then
          CALL EGETWR(OUTSTR,K,vdx,0.,0.,'-','X view vec',IER)
          CALL EGETWR(OUTSTR,K,vdy,0.,0.,'-','Y view vec',IER)
          CALL EGETWR(OUTSTR,K,vdz,0.,0.,'-','Z view vec',IER)
          IF(IER.ne.0)goto 43
        elseif(word(1:3).eq.'-vu')then
          CALL EGETWR(OUTSTR,K,vu,0.,0.,'-','skip vu',IER)
          CALL EGETWR(OUTSTR,K,vu,0.,0.,'-','skip vu',IER)
          CALL EGETWR(OUTSTR,K,vu,0.,0.,'-','skip vu',IER)
          IF(IFLAG.ne.0)goto 43
        elseif(word(1:3).eq.'-vh')then
          CALL EGETWR(OUTSTR,K,angh,0.,0.,'-','horiz ang',IER)
          IF(IER.ne.0)goto 43
        elseif(word(1:3).eq.'-vv')then
          CALL EGETWR(OUTSTR,K,angv,0.,0.,'-','vert ang',IER)
          IF(IER.ne.0)goto 43
        elseif(word(1:3).eq.'-vs')then
          CALL EGETWR(OUTSTR,K,vs,0.,0.,'-','skip vl',IER)
          IF(IER.ne.0)goto 43
        elseif(word(1:4).eq.'-vtv')then
          goto 42
        elseif(word(1:3).eq.'-vl')then
          CALL EGETWR(OUTSTR,K,vl,0.,0.,'-','skip vl',IER)
          IF(IER.ne.0)goto 43
        else
          goto 43
        endif
        goto 42
      else
        call edisp(iuout,' Not an rview file...')
      endif
  43  CALL ERPFREE(ITA2,ISTAT)
      ier=0

      call edisp(iuout,' ')
      write(outs,'(a,3F8.2,a,3F7.3,a,F6.1,a,F6.1,a)') 
     &   'rview -vtv -vp ',rvpx,rvpy,rvpz,' -vd ',vdx,vdy,vdz,
     &   ' -vu 0 0 1 -vh ',angh,' -vv ',angv,' -vs 0 -vl 0 '
      call edisp(iuout,outs)

      return
      end

C ************** RRSKY **************
C RRSKY Read radiance sky file.

      SUBROUTINE RRSKY(ITA2,ga,IER)
#include "building.h"
#include "site.h"
#include "e2r_common.h"

C espriou.h provides current file.
#include "espriou.h"

      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN

      character OUTSTR*124,WORD*20,outs*124,CMDL*124,skopt*3
      character tfile*72

      ier=0

C Attempt to read in sky file.
      write (tfile,'(2a)') runpath(1:lnrp),
     &                     rskyfil(1:lnblnk(rskyfil))
      CALL ERPFREE(ITA2,ISTAT)
      call FPOPEN(ITA2,ISTAT,1,0,tfile)
      if (ISTAT.LT.0) then
        call usrmsg(' Problem detected while opening...',rskyfil,'W')
        return
      endif
      write(currentfile,'(a)') tfile(1:lnblnk(tfile))

C Read the first line and get past '# '.
      READ(ITA2,'(a)',IOSTAT=ISTAT,END=102)OUTSTR
      write(CMDL,'(a)') outstr(3:lnblnk(outstr))
      K=0
      CALL EGETW(CMDL,K,WORD,'W','gensky',IFLAG)
      IF(IFLAG.ne.0)goto 43
      if(word(1:6).ne.'gensky')then
        call edisp(iuout,' Not a radiance sky file...')
        ier=1
        return
      endif
      KT=K   ! there might or might not be a key word -ang
      CALL EGETW(CMDL,KT,WORD,'W','gensky option',IFLAG)
      if(word(1:4).eq.'-ang')then
C BUG?? --- possibly, "azis" should read "azimuth" (the common variable) and "alts" should be "elevtn"??
        CALL EGETWR(CMDL,KT,alts,0.,90.,'W','alt',IER)
        CALL EGETWR(CMDL,KT,azis,-90.,90.,'W','azi',IER)
        write(outs,'(a,2f7.2)') 'Radiance sun alt & azim=',alts,azis
        call edisp(iuout,outs)
        K=KT  ! reset K so options can be read after the -ang values
      else
        CALL EGETWI(CMDL,K,IMO,1,12,'W','Month',IER)
        CALL EGETWI(CMDL,K,IDO,1,31,'W','Day',IER)
        CALL EGETWR(CMDL,K,rtime,0.,24.,'W','Time',IER)
      endif

  42  CALL EGETW(CMDL,K,WORD,'-','option',IER)
      IF(IER.ne.0)goto 43

      if(word(1:2).eq.'+c')then
        isky=4
        skopt= ' +c'  
      elseif(word(1:2).eq.'+s')then
        isky=3
        skopt= ' +s' 
      elseif(word(1:2).eq.'-s')then
        isky=2
        skopt= ' -s'  
      elseif(word(1:2).eq.'-c')then
        skopt= ' -c'  
        isky=1
      elseif(word(1:2).eq.'-g')then
        CALL EGETWR(CMDL,K,rgrfl,0.,0.99,'-','Ground refl',IER)
      elseif(word(1:2).eq.'-a')then
        CALL EGETWR(CMDL,K,sitelat,-89.,89.,'-','latitude',IER)
      elseif(word(1:2).eq.'-o')then
        CALL EGETWR(CMDL,K,sitelongdif,-15.,15.,'-','longitude',IER)
        sitelongdif= -sitelongdif
      elseif(word(1:2).eq.'-m')then
        CALL EGETWR(CMDL,K,XM,-15.,15.,'-','meridian',IER)
      else
        goto 43
      endif
      goto 42

C ESP-r sets XM to 0 so that sitelongdif can be relative (Radiance assumes 
C absolute values) need to update sitelongdif if this is the case.
 43   if (abs(XM).gt.0.1) then
        sitelongdif=sitelongdif+XM
      endif

C Read the second line to get solar altitude and azimuth.
      IGA=0
      do 430 I=1,3
        READ(ITA2,'(a)',IOSTAT=ISTAT,END=102)OUTSTR

C New version of radiance has extra line in sky file therfore sun
C location may be on next line.
        if (OUTSTR(3:7).eq.'Local') then
          call edisp(iuout,OUTSTR)
        elseif (OUTSTR(3:7).eq.'Solar') then
          write(CMDL,'(a)') outstr(30:lnblnk(outstr))
          K=0
          CALL EGETWR(CMDL,K,alts,0.,40.,'-','sun altitude',IER)
          CALL EGETWR(CMDL,K,azis,0.,40.,'-','sun azimuth',IER)
          write(outs,'(a,2f7.2)') 'Radiance sun alt & azim=',alts,azis
          call edisp(iuout,outs)
        elseif (OUTSTR(3:8).eq.'Ground') then
          write(CMDL,'(a)') outstr(24:lnblnk(outstr))
          K=0
          CALL EGETWR(CMDL,K,ga,0.,40.,'-','gr abmient',IER)
          IGA=1
        else
          call edisp(iuout,'Could not recognise line:')
          call edisp(iuout,OUTSTR)
        endif
 430  continue

 102  CALL ERPFREE(ITA2,ISTAT)
      ier=0

C Check that ground ambient read.
      if (IGA.eq.0) then
        call edisp(iuout,'Could not recover ground ambient value !')
        call edisp(iuout,'Assuming value of 0.20')
        ga=0.2
      endif

C Convert month&day to julian day....
      call eday(IDO,IMO,irdoy)

C Confirm.
      call edisp(iuout,' ')
      call edisp(iuout,' interpretation of sky file header...')
      write(outs,'(a,2i3,f6.2,a,a,f5.2,a,f7.1,a,f6.1,a)') 
     &     ' gensky ',IMO,IDO,rtime,skopt,' -g ',rgrfl,
     &     ' -a ',sitelat,' -o ',-sitelongdif,' -m 0.0'
      call edisp(iuout,outs)
      write(outs,'(a,f6.2)') ' ground ambient value = ',ga
      call edisp(iuout,outs)

      return
      end

C ********************* RRIF *********************
C RRIF: Read a Radiance rif file.
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

      SUBROUTINE RRIF(ITA2,ITA3,filepar,RVB,IERR)
#include "building.h"
#include "espriou.h"
#include "e2r_common.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON

      character tvewsh*10,tvewcmds*124
      character OUTS*124,OUTSTR*124,WORD*20,tfile*72
      character tsc*72,tword*10,filepar*72,RVB*1

      IERR=0

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
      write (tfile,'(2a)') runpath(1:lnrp),filepar(1:lnblnk(filepar))
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
        CALL EGETWI(OUTSTR,K,ifocz,-1,NCOMP,'W','zone',IER)
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
          call figscen(ITA3,tsc,ier)
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


C ********************* MKRIF *********************
C MKRIF - Write radiance RIF file.
C IFC=4 ask before overwriting, IFC=3 overwrite.
      SUBROUTINE MKRIF(ITA2,file,IFC,IGL,IER)
#include "building.h"
#include "e2r_common.h"

      integer lnblnk  ! function definition

      character file*72,tfile*72

      logical XST

C Open rif file.
      IER=0
      write(tfile,'(2a)')runpath(1:lnrp),file(1:lnblnk(file))
      CALL ERPFREE(ITA2,ISTAT)
      call FPOPEN(ITA2,ISTAT,1,IFC,tfile)
      if (ISTAT.LT.0) then
        call usrmsg(' Problem detected while opening:',tfile,'W')
        IER=1
        return
      endif

C Start to write file.
C This root and folder are scanned when read in.
      WRITE(ITA2,'(a,a)')'# Radiance RIF file: ',file(1:lnblnk(file))
      WRITE(ITA2,'(a,3i3)')'#e2r_gen_view ',intext,ifocz
      WRITE(ITA2,'(a,i3)') '#e2r_glaz_type ',iglzty

C Update the view descriptors.
      if(vewcmds(1)(1:2).ne.'  ')then
        do 142 i=1,indxvew
          write(ITA2,'(a,a,1x,a)')'view= ',
     &      rvewsh(i)(1:lnblnk(rvewsh(i))),
     &      vewcmds(i)(1:lnblnk(vewcmds(i)))
 142    continue
      endif

C Update the scene descriptors. The sky file must be first, followed 
C by the glazing file (ESRU definition).  Print blanks if they are not 
C defined.
      indxscn = 2

C Sky file.
      write (tfile,'(2a)') runpath(1:lnrp),rskyfil(1:lnblnk(rskyfil))
      INQUIRE(FILE=tfile,EXIST=XST)
      if (.not.XST) then
        rscedes(1)='  '
        write(ITA2,'(a)')'scene= '
      else
        write(rscedes(1),'(a)') rskyfil(1:lnblnk(rskyfil))
        write(ITA2,'(a,a)')'scene= ',rskyfil(1:lnblnk(rskyfil))
      endif

C Glazing file.
C First check if alternative glazing specified (in case of blind 
C shutter control).
      if (IGL.gt.0) then
        write (tfile,'(2a)') runpath(1:lnrp),aglzfil(1:lnblnk(aglzfil))
        INQUIRE(FILE=tfile,EXIST=XST)
        if (.not.XST) then
          rscedes(2)='  '
          write(ITA2,'(a)')'scene= '
        else
          if (iglzty.eq.1) then
            write(rscedes(2),'(a)') aglzfil(1:lnblnk(aglzfil))
            write(ITA2,'(a,a)')'scene= ',aglzfil(1:lnblnk(aglzfil))
          else
            write(rscedes(2),'(a)') aglzfil(1:lnblnk(aglzfil))
            write(ITA2,'(a,a)')'illum= ',aglzfil(1:lnblnk(aglzfil))
          endif
        endif
      else
        write (tfile,'(2a)') runpath(1:lnrp),glzfil(1:lnblnk(glzfil))
        INQUIRE(FILE=tfile,EXIST=XST)
        if (.not.XST) then
          rscedes(2)='  '
          write(ITA2,'(a)')'scene= '
        else
          if (iglzty.eq.1) then
            write(rscedes(2),'(a)') glzfil(1:lnblnk(glzfil))
            write(ITA2,'(a,a)')'scene= ',glzfil(1:lnblnk(glzfil))
          else
            write(rscedes(2),'(a)') glzfil(1:lnblnk(glzfil))
            write(ITA2,'(a,a)')'illum= ',glzfil(1:lnblnk(glzfil))
          endif
        endif
      endif

C Remaining scene files - only if found is anything written to the rif file(s).
C Materials file.
      write (tfile,'(2a)') runpath(1:lnrp),matfil(1:lnblnk(matfil))
      INQUIRE(FILE=tfile,EXIST=XST)
      if (XST) then
        indxscn = indxscn + 1
        write(rscedes(indxscn),'(a)') matfil(1:lnblnk(matfil))
        write(ITA2,'(a,a)')'scene= ',matfil(1:lnblnk(matfil))
      endif

C Outside description file.
      write (tfile,'(2a)') runpath(1:lnrp),rofil(1:lnblnk(rofil))
      INQUIRE(FILE=tfile,EXIST=XST)
      if (XST) then
        indxscn = indxscn + 1
        write(rscedes(indxscn),'(a)') rofil(1:lnblnk(rofil))
        write(ITA2,'(a,a)')'scene= ',rofil(1:lnblnk(rofil))
      endif

C Inside description file.
      write (tfile,'(2a)') runpath(1:lnrp),rzfil(1:lnblnk(rzfil))
      INQUIRE(FILE=tfile,EXIST=XST)
      if (XST) then
        indxscn = indxscn + 1
        write(rscedes(indxscn),'(a)') rzfil(1:lnblnk(rzfil))
        write(ITA2,'(a,a)')'scene= ',rzfil(1:lnblnk(rzfil))
      endif

C Misc materials description file.
      write (tfile,'(2a)') runpath(1:lnrp),rmmfil(1:lnblnk(rmmfil))
      INQUIRE(FILE=tfile,EXIST=XST)
      if (XST) then
        indxscn = indxscn + 1
        write(rscedes(indxscn),'(a)') rmmfil(1:lnblnk(rmmfil))
        write(ITA2,'(a,a)')'scene= ',rmmfil(1:lnblnk(rmmfil))
      endif

C Misc scene description file.
      write (tfile,'(2a)') runpath(1:lnrp),rmfil(1:lnblnk(rmfil))
      INQUIRE(FILE=tfile,EXIST=XST)
      if (XST) then
        indxscn = indxscn + 1
        write(rscedes(indxscn),'(a)') rmfil(1:lnblnk(rmfil))
        write(ITA2,'(a,a)')'scene= ',rmfil(1:lnblnk(rmfil))
      endif

C Remainder of rif file.
      write(ITA2,'(a)')    'UP= Z'
      write(ITA2,'(a,a6)') 'QUALITY= ',imgqua
      write(ITA2,'(a,a6)') 'DETAIL= ',detlvl
      write(ITA2,'(a,a6)') 'VARIABILITY= ',llvar
      write(ITA2,'(a,2I5,F5.2)') 'RESOLUTION= ',ipicx
      write(ITA2,'(a,i2)') 'INDIRECT= ',indrcb
      write(ITA2,'(a,a5)') 'PENUMBRAS= ',penumb
      write(ITA2,'(a,i4)') 'REPORT= ',itrack
      if(crenrp(1:2).ne.'  ')then
        write(ITA2,'(a,a)') 'render= ',crenrp(1:lnblnk(crenrp))
      endif
      if(coconv(1:2).ne.'  ')then
        write(ITA2,'(a,a)') 'oconv= ',coconv(1:lnblnk(coconv))
      endif
      write(ITA2,'(a,a)') 'ZONE= ',rzoncmd(1:lnblnk(rzoncmd))

C Radiance run time files.
C If any were read in then the path is stripped from them so as to 
C create files in writable directories.
C Ambient file.
      if(rambfil(1:4).ne.'NONE')then
        if (IGL.gt.0) then
          write(ITA2,'(3a)')'AMBFILE= ',rambfil(1:lnblnk(rambfil)),'_a'
        else
          write(ITA2,'(a,a)')'AMBFILE= ',rambfil(1:lnblnk(rambfil))
        endif
      endif

C Rendering options file.
      if(optnfil(1:7).ne.'UNKNOWN')then
        write(ITA2,'(a,a)')'OPTFILE= ',optnfil(1:lnblnk(optnfil))
      endif

C Octree.
      if(octfil(1:7).ne.'UNKNOWN')then
        if (IGL.gt.0) then
          write(ITA2,'(a,a,a)')'OCTREE= ',octfil(1:lnblnk(octfil)),'_a'
        else
          write(ITA2,'(a,a)')'OCTREE= ',octfil(1:lnblnk(octfil))
        endif
      endif

C Picture file.
      if(picfil(1:7).ne.'UNKNOWN')then
        if (IGL.gt.0) then
          write(ITA2,'(a,a,a)')'PICTURE= ',picfil(1:lnblnk(picfil)),'_a'
        else
          write(ITA2,'(a,a)')'PICTURE= ',picfil(1:lnblnk(picfil))
        endif
      endif

      CALL ERPFREE(ITA2,ISTAT)

      return
      end

C ******* FIGSCEN
C Attempt to judge the type of file in a scene description from the
C terminating character (or later the contents).
      subroutine figscen(ITA3,cmdname,ier)
#include "building.h"
#include "e2r_common.h"

      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN

      character cmdname*72,OUTSTR*124,tfile*72

      logical XST

      ier=0

C Attempt to read in file, note it is assumed to be one line.
      write(tfile,'(a,a)')runpath(1:lnrp),cmdname(1:lnblnk(cmdname))
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
