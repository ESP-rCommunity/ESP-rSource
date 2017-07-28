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

C ESRU: radiance translator - sky, room and outside definition facilities.
C  SKYFORM: Sky calculations & file generation.
C  E2RFORM: Inside and outside materials and composition generation.
C  mkriofil: Write inside/outside materials and composition files.
C  mkxform: creates a file to hold xform commands for IES.rad entries.

C ******* Sky calculations. *******
      SUBROUTINE SKYFORM(IER)
#include "building.h"
#include "site.h"
#include "e2r_common.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL

      character item*30,DESCRH*5,DESCRD*5,DESCR*7,DESCR1*10
      character skopt*3,outs*124,tfile*72
      character ltmp72*72,dtmp72*72
      integer NITEMS,INO ! max items and current menu item

      DIMENSION ITEM(14)

      logical OK,DOK,XST

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='SKYFORM'  ! set for subroutine

      ITA1 = IFIL+6
      ITA2 = IFIL+7
      ier=0

C Generate a default name for the sky file.
      write (rskyfil,'(a,a4)') 
     &  SCENERT(ISCENE)(1:lnblnk(SCENERT(ISCENE))),'.sky'

C Ask for name of the sky file.  If existing, attempt to
C read it otherwise remember name.
 142  helptopic='sky_def_local_file'
      call gethelptext(helpinsub,helptopic,nbhelp)
      ltmp72=rskyfil
      dtmp72=rskyfil
      CALL EASKS(ltmp72,' Sky distribution file name ?',' ',72,
     &  dtmp72,'sky file name',IER,nbhelp)
      if(ltmp72(1:2).eq.'  ')goto 142
      rskyfil=ltmp72
      write (tfile,'(a,a)') runpath(1:lnrp),rskyfil(1:lnblnk(rskyfil))
      CALL ERPFREE(ITA2,ISTAT)
      INQUIRE (FILE=tfile,EXIST=XST)
      IF(XST)THEN

C Read a radiance sky file, taking info into commons.
        call RRSKY(ITA2,ga,IER)
        if (ier.eq.0) skydone=.true.
      endif
      call usrmsg(' ',' ','-')

C If daylight factors then set sky to CIE overcast.
      if (SCENEPURP(ISCENE)(1:8).eq.'Day_fact') isky=1

10    INO = -4
      write(ITEM(1),'(A,F6.1)') 'a Site Latitude: ',sitelat
      write(ITEM(2),'(A,F6.1)') 'b Longitude diff: ',sitelongdif

      write(ITEM(3),'(A,I6)')   'c Year: ',iryear
      call STDATE(IRYEAR,irdoy,DESCR,DESCR1)
      call EDTIME(rtime,DESCRH,DESCRD,TIMER)
      write(ITEM(4),'(A,A,A,A)') 'd Date: ',DESCR1,' @ ',DESCRH
      write(ITEM(5),'(A,F6.1)')  'e Ground refl: ',rgrfl
      if(isky.eq.1)then
        write(ITEM(6),'(A)')    'f Sky >> CIE std overcast   '
        skopt= ' -c'  
      elseif(isky.eq.2)then
        write(ITEM(6),'(A)')    'f Sky >> CIE clear (no sun) '
        skopt= ' -s'  
      elseif(isky.eq.3)then
        write(ITEM(6),'(A)')    'f Sky >> CIE clear sunny    '
        skopt= ' +s' 
      elseif(isky.eq.4)then
        write(ITEM(6),'(A)')    'f Sky >> Uniform cloudy     '
        skopt= ' -u'  
      endif
      ITEM(7) =                 '  ------------------------  '
      ITEM(8) =                 'g Generate sky description  '
      ITEM(9)=                  '! Browse/edit sky info file '
      ITEM(10)=                 '  ------------------------  '
      ITEM(11)=                 '? Help                      '
      ITEM(12)=                 '- exit                      '
      NITEMS = 12

C Help text for the menu.
  20  helptopic='sky_distribution_menu'
      call gethelptext(helpinsub,helptopic,nbhelp)

      CALL EMENU('  Sky description',ITEM,NITEMS,INO)
      IF(INO.EQ.1)THEN
        CALL EASKR(sitelat,' ',' Site Latitude ? ',
     &    -89.0,'W',89.,'W',50.2,'latitude',IER,nbhelp)
        skydone=.false.
      elseif(INO.EQ.2)then
        CALL EASKR(sitelongdif,' ',
     &    'Longitude difference from reference time meridian?',
     &    -15.0,'W',15.0,'W',0.0,'site longitude diff',IER,nbhelp)
        skydone=.false.
      elseif(INO.EQ.3)then
        CALL EASKI(iryear,' ',' Year ? ',
     &    1900,'F',2051,'W',2007,'year',IER,nbhelp)
      elseif(INO.EQ.4)then
        call edisp(iuout,' ')
        call edisp(iuout,' Date and time for the view... ')
        CALL EDAYR(irdoy,IDO,IMO)
        CALL ASKTIM(2,1,IMO,IDO,irdoy,RTIME,IT,IER)
        skydone=.false.
      elseif(INO.EQ.5)then
        CALL EASKR(rgrfl,' ',' Ground reflectance? ',
     &    0.0,'W',0.99,'W',0.2,'ground refl',IER,nbhelp)
        skydone=.false.
      elseif(INO.EQ.6)then

C Sky Toggle.
        isky=isky+1
        IF(isky.GT.4)isky=1
        skydone=.false.
      elseif(INO.EQ.8)then

C If daylight factors then check if sky is CIE overcast.
      if (SCENEPURP(ISCENE)(1:8).eq.'Day_fact') then
        if (isky.ne.1) then 
          helptopic='sky_not_CIE_menu'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL EASKAB(' The sky is not a CIE standard overcast sky.',
     &      'Do you want to: ','proceed','change sky type',IW,nbhelp)
          if (IW.eq.2) goto 10
        endif
      endif

C Generate skydone information. Begin by creating a command line
C which includes redirection of output to the sky definition file.
        CALL EDAYR(irdoy,IDO,IMO)
        write(outs,'(a,2i3,f6.2,a,a,f5.2,2(a,f7.1),a)')'gensky',IMO,IDO,
     &    RTIME,skopt,' -g ',rgrfl,' -a ',sitelat,' -o ',-sitelongdif,
     &    ' -m 0.0'
        CALL wrtsky(outs)
        call RRSKY(ITA2,ga,IER)
        if (SCENEPURP(ISCENE)(1:8).eq.'External') then
          call RADPAR('av',ga)
        elseif (SCENEPURP(ISCENE)(1:8).eq.'Internal') then
          xga=ga/10.
          call RADPAR('av',xga)
        elseif (SCENEPURP(ISCENE)(1:9).eq.'Night_ext') then
          xga=0.0005
          call RADPAR('av',xga)
        endif
      elseif(INO.EQ.9)then
        helptopic='sky_description_browse'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKABC(' Sky description:',' ',
     &    'browse file','edit file','ignore',IW,nbhelp)
        if(IW.eq.1)then
          write (tfile,'(a,a)') runpath(1:lnrp),
     &                          rskyfil(1:lnblnk(rskyfil))
          call vifile(ITA1,tfile,'b',ier)
        elseif(IW.eq.2)then
          write (tfile,'(a,a)') runpath(1:lnrp),
     &                          rskyfil(1:lnblnk(rskyfil))
          call vifile(ITA1,tfile,'e',ier)
        endif
      elseif(INO.EQ.11)then

c Explains sky & time menu.
        helptopic='sky_distribution_menu'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('sky menu',nbhelp,'-',0,0,IER)
      elseif(INO.EQ.12)then
        if(skydone)then
          return
        else
          dok=.false.
          helptopic='sky_file_not_generated'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL ASKOK(' Sky file has not yet been generated...',
     &      ' Is it ok to exit ?',OK,dok,nbhelp)
          IF(OK)return
          goto 10
        endif
      else
        goto 20
      endif
      goto 10

      end

C ********************** wrtsky **********************
C wrtsky calls radiance and writes sky info to file.

      SUBROUTINE wrtsky(skycmd)
#include "building.h"
#include "e2r_common.h"

      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL

      character skycmd*124,doit*124,tfile*72

      LOGICAL XST

      ITA3=IFIL+10
      call edisp(iuout,' creating sky description file.')
      write(tfile,'(a,a)')runpath(1:lnrp),rskyfil(1:lnblnk(rskyfil))
      INQUIRE (FILE=tfile,EXIST=XST)
      if(XST)then
        call FPOPEN(ITA3,ISTAT,1,1,tfile)
        call EFDELET(ITA3,ISTAT)
      endif
      write(doit,'(a,a,a)') skycmd(1:lnblnk(skycmd)),' > ', 
     &                                           tfile(1:lnblnk(tfile))

C Create sky description.
      call runit(doit,'-')
      skydone=.true.

      return
      end
      

C ************** e2rform **************
C Translate esp data to radiance geom and materials.
C ACT = 's' silent, ACT = 'i' interactive.
      SUBROUTINE e2rform(ACT,IER)
#include "building.h"
#include "model.h"
#include "e2r_common.h"
#include "prj3dv.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL
      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      COMMON/GTFIL/GTGEOM

      LOGICAL OK,DOK,XST,fict

      DIMENSION ITEM(17)
      character*72 tfile
      character item*32,ACT*1,GTGEOM*72
      integer NITEMS,INO ! max items and current menu item

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='e2rform'  ! set for subroutine
      ITA1 = IFIL+6
      outdone=.false.
      fict=.false.
      iscope=2
      ier=0
C      GTGEOM = 'UNKNOWN'

C Open the materials, mlc & optics db.
      call module_opendb(ier)

      icomp = nznog(1)
      izgfoc = nznog(1)

C Fictitious transmission threshhold.
      ftr=0.92

10    INO = -4
      ITEM(1) =       ' Toggles ...                   '
      if(iscope.eq.1)then
        ITEM(2) =     'a  Filter >> all surfaces      '
      elseif(iscope.eq.2)then
        ITEM(2) =     'a  Filter >> all surfs & obstr '
      endif
      if(fict)then
        ITEM(3) =     'b  Fictitious surf >> included '
      else
        ITEM(3) =     'b  Fictitious surf >> omitted  '
      endif
      ITEM(4) =       'c  Ground topology             '

      ITEM(5) =       '  ________________________     '
      ITEM(6) =       'd Generate description         '
      ITEM(7) =       '  Browse/edit:                 '
      ITEM(8) =       'e  Glazing properties& compos. '
      ITEM(9) =       'f  Opaque surface properites   '
      ITEM(10) =      'g  Outside composition         '
      ITEM(11) =      'h  Inside composition          '
      ITEM(12) =      'i  IES entity xforms           '
      ITEM(13) =      '  ______________________       '
      ITEM(14)=       '? Help                         '
      ITEM(15)=       '- Exit                         '
      NITEMS = 15

C Help text for this menu.
  20  helptopic='e2r_outside_comp'
      call gethelptext(helpinsub,helptopic,nbhelp)

      CALL EMENU('Outside & Zone composition',ITEM,NITEMS,INO)
      if(INO.EQ.2)then

C Toggle of included topology.
        iscope=iscope+1
        IF(iscope.GT.2)iscope=1
        outdone=.false.
      elseif(INO.EQ.3)then

C If fictitious to be omitted then ask what is the transmission
C value above which a fictitious surface is indicated.
        if(fict)then
          fict=.false.
          CALL EASKR(ftr,
     &      ' Above what (normal) visible transmission value ',
     &      ' should a surface be considered fictitious ? ',
     &      0.9,'W',0.99,'W',0.9,'fict threashold',IER,nbhelp)
        else
          fict=.true.
        endif
      elseif(INO.EQ.4)then

C Read ground file.
        CALL EASKS(gtgeom,' Ground topology/geometry file name ?',
     &        ' ',72,'grnd.geo','ground geom file',IER,nbhelp)
        INQUIRE (FILE=gtgeom,EXIST=XST)
        if(XST)then
          call EGRNIN(ITA1,GTGEOM,ITRC,ITRU,IER)
        else
          call usrmsg('File not found, ignored...',' ','W')
        endif
      elseif(INO.EQ.6)then

C Write out materials, composition & glazing files.
        nzg=NCOMP
        DO 46 I=1,nzg
          nznog(I)=I
 46     CONTINUE
        call mkriofil(ftr,fict,ACT,IER)

C Re-Read in the initial zone geometry file into common & restore
C the image.
        call georead(ITA1,LGEOM(ICOMP),ICOMP,1,IUOUT,IER)
        nzg=1
        nznog(1)=ICOMP
        MODIFYVIEW=.TRUE.
        MODBND=.TRUE.
        izgfoc=ICOMP
        CALL ADJVIEW(IER)
        outdone=.true.
      elseif(INO.EQ.8)then
        CALL EASKABC(' Glazing description:',' ',
     &   'browse file','edit file','ignore',IW,nbhelp)
        if(IW.eq.1)then
          write (tfile,'(a,a)') runpath(1:lnrp),
     &                          glzfil(1:lnblnk(glzfil))
          call vifile(iglzfil,tfile,'b',ier)
        elseif(IW.eq.2)then
          write (tfile,'(a,a)') runpath(1:lnrp),
     &                          glzfil(1:lnblnk(glzfil))
          call vifile(iglzfil,tfile,'e',ier)
        endif
      elseif(INO.EQ.9)then
        CALL EASKABC(' Opaque materials:',' ',
     &   'browse file','edit file',',ignore',IW,nbhelp)
        if(IW.eq.1)then
          write (tfile,'(a,a)') runpath(1:lnrp),
     &                          matfil(1:lnblnk(matfil))
          call vifile(imatfil,tfile,'b',ier)
        elseif(IW.eq.2)then
          write (tfile,'(a,a)') runpath(1:lnrp),
     &                          matfil(1:lnblnk(matfil))
          call vifile(imatfil,tfile,'e',ier)
        endif
      elseif(INO.EQ.10)then
        CALL EASKABC(' Outside opaque description:',' ',
     &   'browse file','edit file','ignore',IW,nbhelp)
        if(IW.eq.1)then
          write (tfile,'(a,a)') runpath(1:lnblnk(runpath)),
     &                          rofil(1:lnblnk(rofil))
          call vifile(irofil,tfile,'b',ier)
        elseif(IW.eq.2)then
          write (tfile,'(a,a)') runpath(1:lnrp),
     &                          rofil(1:lnblnk(rofil))
          call vifile(irofil,tfile,'e',ier)
        endif
      elseif(INO.EQ.11)then
        CALL EASKABC(' Inside opaque description:',' ',
     &   'browse file','edit file','ignore',IW,nbhelp)
        if(IW.eq.1)then
          write (tfile,'(a,a)') runpath(1:lnrp),
     &                          rzfil(1:lnblnk(rzfil))
          call vifile(irzfil,tfile,'b',ier)
        elseif(IW.eq.2)then
          write (tfile,'(a,a)') runpath(1:lnrp),
     &                          rzfil(1:lnblnk(rzfil))
          call vifile(irzfil,tfile,'e',ier)
        endif
      elseif(INO.EQ.12)then
        CALL EASKABC(' IES entity xforms:',' ',
     &   'browse file','edit file','ignore',IW,nbhelp)
        if(IW.eq.1)then
          write (tfile,'(a,a)') runpath(1:lnrp),
     &                          iesfil(1:lnblnk(iesfil))
          call vifile(iiesfil,tfile,'b',ier)
        elseif(IW.eq.2)then
          write (tfile,'(a,a)') runpath(1:lnrp),
     &                          iesfil(1:lnblnk(iesfil))
          call vifile(iiesfil,tfile,'e',ier)
        endif
      elseif(INO.EQ.(NITEMS-1))then

C Explains composition/geometry menu.
        helptopic='e2r_outside_comp'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('outside menu',nbhelp,'e2r_files   ',0,0,IER)
      elseif(INO.EQ.NITEMS)then
        if(outdone)then
          return
        else
          dok=.false.
          helptopic='e2r_outside_not_created'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL ASKOK(' Descriptions not yet been generated...',
     &      ' Is it ok to exit ?',OK,dok,nbhelp)
          IF(OK)return
          goto 10
        endif
      else
        goto 20
      endif
      goto 10

      end

C ********************* mkriofil *********************
C mkriofil: Write inside/outside materials and composition files.
C If ACT='i' then interactive, ACT='s' for silent.
      SUBROUTINE mkriofil(ftr,fict,ACT,IER)
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "esprdbfile.h"
#include "material.h"
#include "e2r_common.h"
#include "prj3dv.h"
    
      integer lnblnk  ! function definition

      COMMON/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)

C TMC data
      COMMON/PRECTC/ITMCFL(MCOM,MS),TMCT(MCOM,MTMC,5),
     &       TMCA(MCOM,MTMC,ME,5),TMCREF(MCOM,MTMC),TVTR(MCOM,MTMC)
      COMMON/PRECT2/TMCT2(MCOM,MTMC,5,MBP),TMCA2(MCOM,MTMC,ME,5,MBP),
     &              TVTR2(MCOM,MTMC,MBP)
      COMMON/TMCB1/IBCMT(MCOM,MTMC)

      COMMON/GOPT/DG(5),HG(5),UVAL,VTRN,NTL,AB(ME,5),RF(ME),SRF,SAB

C For obstruction blocks of type "obsp" as well as converted obs3.
C << and probably also for obs3 ? >>
      COMMON/GB1/XB(12),YB(12),ZB(12),JVNB(6,4)

      common/grndpl/NGT,NGTV,XGT(MGTV),YGT(MGTV),ZGT(MGTV),JGVN(MGRT,8),
     &  NGVER(MGRT),IVEDGE(MGRT)

      CHARACTER GSNAME*6,GMLCN*32
      COMMON/GT5/GSNAME(MGRT),GMLCN(MGRT)

      integer matarrayindex ! the indes within matdatarray
      logical closemat1,closemat2

      DIMENSION ZSN(MS),iskip(MMLC),VP(3),EQN(4)
      DIMENSION SVALS(3),TVALS(3),TFORM(3)
      DIMENSION XX(MV),YY(MV),ZZ(MV),XT(MV),YT(MV),ZT(MV),TRNS(3)

      character ZSN*28
      character ZN*12,outs*124,material*32,QT*1
      character GDESCR*36,DESCRC*25
      CHARACTER OPT*12  ! for optical name of the MLC
      CHARACTER ACT*1,doit*144
      character outsn*124,outsd*124, tfile*72
      character pfile*72,atfile*72,ltmp72*72,dtmp72*72,ObsMatName*32
      integer lnbm  ! length of block material
      integer nobs  ! number of obstructions
      integer nvis  ! number of visual entities

      LOGICAL OK,DOK,fict,extern,XST,similar

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='mkriofil'  ! set for subroutine

      ITA1 = IFIL+6
      ITA2 = IFIL+7
      ITA3 = IFIL+10
      nobs=0
      nvis=0
      ISTAT=0
      IER=0

C Generate help for the various dialogs.
      helptopic='writing_transpar_stuff'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Find version of materials database.
      call eclose(matver,1.1,0.01,closemat1)
      call eclose(matver,1.2,0.01,closemat2)
      if(closemat1.or.closemat2)then
        continue
      else
        call usrmsg('The materials arrays are incomplete so creation',
     &    'of surface attributes not possible.','W')
        ier=1
        return
      endif

C Create a glazing file, default type is 'glass' with user choice 
C to treat as 'mkillum'.
      call edisp(iuout,' creating glazing file. ')
      CALL ERPFREE(iglzfil,ISTAT)
      CALL ERPFREE(ita2,ISTAT)
      if(ACT.eq.'i')then
 10     ltmp72=glzfil
        dtmp72=glzfil
        CALL EASKS(ltmp72,' Glazing file name ?',' ',
     &    72,dtmp72,'glazing file name',IER,nbhelp)
        if(ltmp72(1:2).eq.'  ') goto 10
        glzfil=ltmp72
        write(tfile,'(a,a)')runpath(1:lnrp),glzfil(1:lnblnk(glzfil))
        call FPOPEN(iglzfil,ISTAT,1,4,tfile)

C Ask for alternative glazing file.
        if (NABS.gt.0) then
          dok=.true.
          call ASKOK('Blind shutter control is active.',
     &      'Create model with blinds closed as well?',OK,dok,nbhelp)
          if (OK) then
            NBSRIF(ISCENE)=NABS
 12         ltmp72=aglzfil
            dtmp72=aglzfil
            CALL EASKS(ltmp72,' Alternative glazing file name ?',' ',
     &        72,dtmp72,'glazing file name',IER,nbhelp)
            if(ltmp72(1:2).eq.'  ') goto 12
            aglzfil=ltmp72
            write(atfile,'(a,a)')runpath(1:lnrp),
     &                           aglzfil(1:lnblnk(aglzfil))
            call FPOPEN(ita2,ISTAT,1,4,atfile)
          endif
        endif
      else
        write(tfile,'(a,a)')runpath(1:lnrp),glzfil(1:lnblnk(glzfil))
        call FPOPEN(iglzfil,ISTAT,1,3,tfile)
        if (NABS.gt.0) then
          NBSRIF(ISCENE)=NABS
          write(atfile,'(a,a)')runpath(1:lnrp),
     &                         aglzfil(1:lnblnk(aglzfil))
          call FPOPEN(ita2,ISTAT,1,3,atfile)
        endif
      endif
      IF(ISTAT.LT.0)THEN
        call usrmsg(' Problem detected while opening:',glzfil,'W')
        IER=1
        return
      ENDIF

      WRITE(iglzfil,'(a)') '# Radiance glazing definitions'
      WRITE(iglzfil,'(a)') '# (first line of file must not be edited)'
      WRITE(iglzfil,'(a)') '  '

      WRITE(iglzfil,'(a)') 'void  glass  unknown_glz'
      WRITE(iglzfil,'(a)') '0 '
      WRITE(iglzfil,'(a)') '0 '
      WRITE(iglzfil,'(a)') '3    0.87    0.87    0.87'
      WRITE(iglzfil,'(a)') '  '

C Aternative glazing descriptions file.
      if (NABS.gt.0) then
        WRITE(ita2,'(a)') '# Radiance glazing (alt) definitions'
        WRITE(ita2,'(a)') '# (first line of file must not be edited)'
        WRITE(ita2,'(a)') '  '

        WRITE(ita2,'(a)') 'void  glass  unknown_glz'
        WRITE(ita2,'(a)') '0 '
        WRITE(ita2,'(a)') '0 '
        WRITE(ita2,'(a)') '3    0.87    0.87    0.87'
        WRITE(ita2,'(a)') '  '
      endif

C Create an opaque materials file. Take all of the materials in
C the existing mlc db and create a set of properties.
      call edisp(iuout,' creating materials for opaque surfaces. ')
      CALL ERPFREE(imatfil,ISTAT)
      if(ACT.eq.'i')then
 20     ltmp72=matfil
        dtmp72=matfil
        CALL EASKS(ltmp72,' Opaque materials file name ?',' ',
     &    72,dtmp72,'opaque mat file name',IER,nbhelp)
        if(ltmp72(1:2).eq.'  ') goto 20
        matfil=ltmp72
        write(tfile,'(a,a)')runpath(1:lnrp),matfil(1:lnblnk(matfil))
        call FPOPEN(imatfil,ISTAT,1,4,tfile)
      else
        write(tfile,'(a,a)')runpath(1:lnrp),matfil(1:lnblnk(matfil))
        call FPOPEN(imatfil,ISTAT,1,3,tfile)
      endif
      IF(ISTAT.LT.0)THEN
        call usrmsg(' Problem detected while opening...',matfil,'W')
        return
      ENDIF

      WRITE(imatfil,'(a)') '# Radiance opaque material definitions.'
      WRITE(imatfil,'(a)') '# (first line of file must not be edited)'
      WRITE(imatfil,'(a)') '  '

C Define rough medium and fine finish effects for surfaces.
      WRITE(imatfil,'(a)') '# use this for fine surface finish '
      WRITE(imatfil,'(a)') 'void brightfunc fin_in'
      WRITE(imatfil,'(a)') '4 dirt dirt.cal -s 0.005'
      WRITE(imatfil,'(a)') '0 '
      WRITE(imatfil,'(a)') '1 0.05'
      WRITE(imatfil,'(a)') '  '
      WRITE(imatfil,'(a)') 'void texfunc fin_ex'
      WRITE(imatfil,'(a)') '6 cdx cdy cdz adobe.cal -s 0.02'
      WRITE(imatfil,'(a)') '0 '
      WRITE(imatfil,'(a)') '1 0.01'
      WRITE(imatfil,'(a)') '  '
      WRITE(imatfil,'(a)') '# use this for medium surface finish '
      WRITE(imatfil,'(a)') 'void brightfunc min_in'
      WRITE(imatfil,'(a)') '4 dirt dirt.cal -s 0.01'
      WRITE(imatfil,'(a)') '0 '
      WRITE(imatfil,'(a)') '1 0.05'
      WRITE(imatfil,'(a)') '  '
      WRITE(imatfil,'(a)') 'void texfunc min_ex'
      WRITE(imatfil,'(a)') '6 cdx cdy cdz adobe.cal -s 0.03'
      WRITE(imatfil,'(a)') '0 '
      WRITE(imatfil,'(a)') '1 0.015'
      WRITE(imatfil,'(a)') '  '
      WRITE(imatfil,'(a)') '# use this for rough surface finish '
      WRITE(imatfil,'(a)') 'void brightfunc rin_in'
      WRITE(imatfil,'(a)') '4 dirt dirt.cal -s 0.05'
      WRITE(imatfil,'(a)') '0 '
      WRITE(imatfil,'(a)') '1 0.05'
      WRITE(imatfil,'(a)') '  '
      WRITE(imatfil,'(a)') 'void texfunc rin_ex'
      WRITE(imatfil,'(a)') '6 cdx cdy cdz adobe.cal -s 0.05'
      WRITE(imatfil,'(a)') '0 '
      WRITE(imatfil,'(a)') '1 0.05'
      WRITE(imatfil,'(a)') '  '

C Always include a colour for external UNKNOWN mlc.
      WRITE(imatfil,'(a)') 'void plastic rc_ex_unknown'
      WRITE(imatfil,'(a)') '0 '
      WRITE(imatfil,'(a)') '0 '
      srefl=0.5
      WRITE(imatfil,'(a,3F5.2,a)') '5 ',srefl,srefl,srefl,' 0 0 '
      WRITE(imatfil,'(a)') '  '

C Always include a colour for internal UNKNOWN mlc.
      WRITE(imatfil,'(a)') 'void plastic rc_in_unknown'
      WRITE(imatfil,'(a)') '0 '
      WRITE(imatfil,'(a)') '0 '
      srefl=0.5
      WRITE(imatfil,'(a,3F5.2,a)') '5 ',srefl,srefl,srefl,' 0 0 '
      WRITE(imatfil,'(a)') '  '

C Open the inside geometry/composition file.
      call edisp(iuout,' creating inside composition. ')
      CALL ERPFREE(irzfil,ISTAT)
      if(ACT.eq.'i')then
 22     ltmp72=rzfil
        dtmp72=rzfil
        CALL EASKS(ltmp72,' Inside compositon file name ?','  ',
     &    72,dtmp72,'room file name',IER,nbhelp)
        if(ltmp72(1:2).eq.'  ') goto 22
        rzfil=ltmp72
        write(tfile,'(a,a)')runpath(1:lnrp),rzfil(1:lnblnk(rzfil))
        call FPOPEN(irzfil,ISTAT,1,4,tfile)
      else
        write(tfile,'(a,a)')runpath(1:lnrp),rzfil(1:lnblnk(rzfil))
        call FPOPEN(irzfil,ISTAT,1,3,tfile)
      endif
      IF(IER.LT.0)THEN
        call usrmsg(' Problem detected while opening...',rzfil,'W')
        return
      ENDIF

C Write inside composition header.
      WRITE(irzfil,'(a)') '# Radiance interior composition file '
      WRITE(irzfil,'(a)') '# (first line of file must not be edited)'
      WRITE(irzfil,'(a)') '  '

C List database.
      do 45 IM=1,NMLC

C Initial assumption is that material is not ficticious and should
C not be skipped.
        iskip(im)=0

C Pick up general description of the composite.
        WRITE(OPT,'(A)') mlcoptical(IM)(1:12)

C For the inside and outside layer get surface properties.
        matarrayindex=IPRMAT(IM,1)   ! which materials array index
        if(matarrayindex.ge.0)then
          AE=matdbina(matarrayindex)
        endif

        matarrayindex=IPRMAT(IM,LAYERS(IM))   ! which materials array index
        if(matarrayindex.ge.0)then
          AI=matdbina(matarrayindex)
        endif

C If a tmc then find tranmission functions.  If more transparent than
C the ficticious limit then mark to be skipped. Convert visible
C transmission to transmissivity (as per Ward 1/12/94 Radiance 2.4 paper).
C If TMC has an opaque layer then set gvt to a small number.
        if(mlctype(IM)(1:4).NE.'OPAQ')then
          CALL EROPTDB(ITRC,iuout,OPT,GDESCR,IER)
          if(VTRN.gt.0.02)then
          gvt1=(sqrt(0.8402528435+(0.007252224*VTRN*VTRN))-0.9166530661)
          gvt=gvt1/0.0036261119/VTRN
          else
           gvt1=0.02
           gvt=0.02
          endif
          if(.NOT.(fict))then
            if(VTRN.gt.ftr)iskip(im)=1
          endif

C Setup equivalent for default use. 
          WRITE(iglzfil,'(a,a)') 'void glass ',
     &      mlcname(IM)(1:lnmlcname(IM))
          WRITE(iglzfil,'(a)') '0 '
          WRITE(iglzfil,'(a)') '0 '
          WRITE(iglzfil,'(a,3F6.2)') '3  ',gvt,gvt,gvt
          WRITE(iglzfil,'(a)') '  '

C Aternative glazing descriptions file.
          if (NABS.gt.0) then

C Check for blind shutter control.
            do 100 newfoc=1,ncomp
              call georead(ita3,LGEOM(newfoc),newfoc,1,ITRU,IER)
              do 111 isf=1,nsur
               ICN=IZSTOCN(newfoc,isf)
               if(ITMCFL(newfoc,isf).gt.0.AND.
     &            SSMLCN(icn)(1:lnblnk(SSMLCN(icn))).eq.
     &            mlcname(IM)(1:lnmlcname(IM)))then
                if (IBCMT(newfoc,ITMCFL(newfoc,isf)).gt.0) then
                 VTRAN=TVTR2(newfoc,ITMCFL(newfoc,isf),NABS)
                 if(VTRN.gt.0.02)then
                  gvt1=sqrt(0.8402528435+(0.007252224*VTRAN*VTRAN))
                  gvt=(gvt1-0.9166530661)/0.0036261119/VTRAN
                 else
                  gvt1=0.02
                  gvt=0.02
                 endif
               endif
              endif
 111         continue
 100        continue
            WRITE(ita2,'(3a)')'void glass ',
     &        mlcname(IM)(1:lnmlcname(IM)),'_sw'
            WRITE(ita2,'(a)') '0 '
            WRITE(ita2,'(a)') '0 '
            WRITE(ita2,'(a,3F6.2)') '3  ',gvt,gvt,gvt
            WRITE(ita2,'(a)') '  '
          endif

        else
          WRITE(imatfil,'(a)') '# External MLC Colours... '
          if (SCENEPURP(ISCENE)(1:8).eq.'External'.or.
     &        SCENEPURP(ISCENE)(1:9).eq.'Night_ext'.or.
     &        SCENEPURP(ISCENE)(1:8).eq.'Internal') then
            if(iadobe.eq.3)then
              WRITE(imatfil,'(2a)') 'fin_ex plastic rc_ex_',
     &          mlcname(IM)(1:lnmlcname(IM))
            elseif(iadobe.eq.2)then
              WRITE(imatfil,'(2a)') 'min_ex plastic rc_ex_',
     &          mlcname(IM)(1:lnmlcname(IM))
            elseif(iadobe.eq.1)then
              WRITE(imatfil,'(2a)') 'rin_ex plastic rc_ex_',
     &          mlcname(IM)(1:lnmlcname(IM))
            endif
          else
            WRITE(imatfil,'(a,a)') 'void plastic rc_ex_',
     &        mlcname(IM)(1:lnmlcname(IM))
          endif
          WRITE(imatfil,'(a)') '0 '
          WRITE(imatfil,'(a)') '0 '
          srefl=1.0-AE
          WRITE(imatfil,'(a,3F5.2,a)') '5 ',srefl,srefl,srefl,' 0 0 '
          WRITE(imatfil,'(a)') '  '

          WRITE(imatfil,'(a)') '# Internal MLC Colours... '
          if (SCENEPURP(ISCENE)(1:8).eq.'External'.or.
     &        SCENEPURP(ISCENE)(1:9).eq.'Night_ext'.or.
     &        SCENEPURP(ISCENE)(1:8).eq.'Internal') then
            if(iadobe.eq.3)then
              WRITE(imatfil,'(a,a)') 'fin_in plastic rc_in_',
     &          mlcname(IM)(1:lnmlcname(IM))
            elseif(iadobe.eq.2)then
              WRITE(imatfil,'(a,a)') 'min_in plastic rc_in_',
     &          mlcname(IM)(1:lnmlcname(IM))
            elseif(iadobe.eq.1)then
              WRITE(imatfil,'(a,a)') 'rin_in plastic rc_in_',
     &          mlcname(IM)(1:lnmlcname(IM))
            endif
          else
            WRITE(imatfil,'(a,a)') 'void plastic rc_in_',
     &        mlcname(IM)(1:lnmlcname(IM))
          endif
          WRITE(imatfil,'(a)') '0 '
          WRITE(imatfil,'(a)') '0 '
          srefl=1.0-AI
          WRITE(imatfil,'(a,3F5.2,a)') '5 ',srefl,srefl,srefl,' 0 0 '
          WRITE(imatfil,'(a)') '  '
        endif
   45 continue

C Open the outsite geometry/composition file.
      call edisp(iuout,' creating outside composition. ')
      CALL ERPFREE(irofil,ISTAT)
      if(ACT.eq.'i')then
 24     ltmp72=rofil
        dtmp72=rofil
        CALL EASKS(ltmp72,' Outside description file name ?','  ',
     &    72,dtmp72,'outside file name',IER,nbhelp)
        if(ltmp72(1:2).eq.'  ') goto 24
        rofil=ltmp72
        write(tfile,'(a,a)')runpath(1:lnrp),rofil(1:lnblnk(rofil))
        call FPOPEN(irofil,ISTAT,1,4,tfile)
      else
        write(tfile,'(a,a)')runpath(1:lnrp),rofil(1:lnblnk(rofil))
        call FPOPEN(irofil,ISTAT,1,3,tfile)
      endif
      IF(IER.LT.0)THEN
        call usrmsg(' Problem detected while opening...',rofil,'W')
        return
      ENDIF

      WRITE(irofil,'(a)') '# Radiance exterior composition file '
      WRITE(irofil,'(a)') '# (first line of file must not be edited)'
      WRITE(irofil,'(a)') '  '
      WRITE(irofil,'(a)') '# standard sky and ground '
      WRITE(irofil,'(a)') '# BEWARE: the RGB figures in the glow'
      WRITE(irofil,'(a)') '#   definitions must integrate to one.'

      if (SCENEPURP(ISCENE)(1:9).eq.'Night_ext') then

C Use values suggested by Radiance book.
        WRITE(irofil,'(a)') '# define night sky... '
        WRITE(irofil,'(a)') '  '
        WRITE(irofil,'(a)') 'skyfunc glow sky_glow'
        WRITE(irofil,'(a)') '0  '
        WRITE(irofil,'(a)') '0  '
        WRITE(irofil,'(a)') '4 2e-6 2e-6 1e-5 0 '
        WRITE(irofil,'(a)') '  '
        WRITE(irofil,'(a)') 'sky_glow source sky'
        WRITE(irofil,'(a)') '0  '
        WRITE(irofil,'(a)') '0  '
        WRITE(irofil,'(a)') '4 0 0 1 180  '
        WRITE(irofil,'(a)') '  '
        WRITE(irofil,'(a)') '# define ground... '
        WRITE(irofil,'(a)') 'skyfunc glow ground_glow'
        WRITE(irofil,'(a)') '0  '
        WRITE(irofil,'(a)') '0  '
        WRITE(irofil,'(a)') '4 1e-5 8e-6  5e-6 0'
        WRITE(irofil,'(a)') '  '
        WRITE(irofil,'(a)') 'ground_glow source ground'
        WRITE(irofil,'(a)') '0  '
        WRITE(irofil,'(a)') '0  '
        WRITE(irofil,'(a)') '4 0 0 -1 180  '
        WRITE(irofil,'(a)') '  '
      elseif (SCENEPURP(ISCENE)(1:8).eq.'Day_coef') then
        continue ! Do not use sky for Day_coef purpose
      else

C All other scene types use this standard approach.
        WRITE(irofil,'(a)') '# define sky... '
        WRITE(irofil,'(a)') '  '
        WRITE(irofil,'(a)') 'skyfunc glow sky_glow'
        WRITE(irofil,'(a)') '0  '
        WRITE(irofil,'(a)') '0  '
        WRITE(irofil,'(a)') '4 .986 .986 1.202  0 '
        WRITE(irofil,'(a)') '  '
        WRITE(irofil,'(a)') 'sky_glow source sky'
        WRITE(irofil,'(a)') '0  '
        WRITE(irofil,'(a)') '0  '
        WRITE(irofil,'(a)') '4 0 0 1 180  '
        WRITE(irofil,'(a)') '  '
        WRITE(irofil,'(a)') '# define ground... '
        WRITE(irofil,'(a)') 'skyfunc glow ground_glow'
        WRITE(irofil,'(a)') '0  '
        WRITE(irofil,'(a)') '0  '
        WRITE(irofil,'(a)') '4 1.276 .957 .319  0 '
        WRITE(irofil,'(a)') '  '
        WRITE(irofil,'(a)') 'ground_glow source ground'
        WRITE(irofil,'(a)') '0  '
        WRITE(irofil,'(a)') '0  '
        WRITE(irofil,'(a)') '4 0 0 -1 180  '
        WRITE(irofil,'(a)') '  '
      endif

C The ground ring material is the RGD values for the ground glow modified 
C by the ground reflectivity (thus the ground is a shade of brown).
      WRITE(irofil,'(a)') 'void brightfunc mud'
      WRITE(irofil,'(a)') '4 dirt dirt.cal -s 0.5'
      WRITE(irofil,'(a)') '0 '
      WRITE(irofil,'(a)') '1 0.3'
      WRITE(irofil,'(a)') ' '
      WRITE(irofil,'(a)') 'mud plastic ground_mat  '
      WRITE(irofil,'(a)') '0  '
      WRITE(irofil,'(a)') '0  '
      WRITE(irofil,'(a,3F5.2,a)') '5 ',
     &                      1.276*rgrfl,0.957*rgrfl,0.319*rgrfl,' 0 0 '
      WRITE(irofil,'(a)') '  '
      WRITE(irofil,'(a)') 'ground_mat ring groundplane '
      WRITE(irofil,'(a)') '0  '
      WRITE(irofil,'(a)') '0  '

C Make sure the ground ring is at least 30m radius.
      if(grdd.lt.15.)grdd=15.0
      WRITE(irofil,'(a,2F7.1,a,F7.1)') '8 ',grcx,grcy,
     &  ' -0.01  0. 0. 1. 0. ',grdd*2.
      WRITE(irofil,'(a)') ' '
      if(ngt.gt.0)then

C If there is a ground topology defined then write this out
C to the outside file.
        WRITE(irofil,'(a)') '# Outside ground topology defs... '
        WRITE(irofil,'(a)') '  '
        do 775 isf=1,ngt

C Find the matching mlc index.
          ICF=-1
          call matchmlcdesc(GMLCN(isf),icf)

C If not found then it is probably unknown.
          if(GMLCN(isf)(1:4).eq.'UNKN')then
            WRITE(irofil,'(a,a)')'rc_ex_unknown  polygon ',
     &        GSNAME(isf)(1:lnblnk(GSNAME(isf)))
            WRITE(irofil,'(a)') '0 '
            WRITE(irofil,'(a)') '0 '
            WRITE(irofil,'(I3)')NGVER(isf)*3
          else
            WRITE(irofil,'(a,a,a,a)')'rc_ex_',
     &        GMLCN(isf)(1:lnblnk(GMLCN(isf))),'  polygon ',
     &        GSNAME(isf)(1:lnblnk(GSNAME(isf)))
            WRITE(irofil,'(a)') '0 '
            WRITE(irofil,'(a)') '0 '
            WRITE(irofil,'(I3)')NGVER(isf)*3
          endif

C Write the surface.
          do 774 iv=1,NGVER(isf)
            K = JGVN(isf,iv)
            WRITE(irofil,'(3F11.5)')XGT(K),YGT(K),ZGT(K)
774       continue
          WRITE(irofil,'(a)') '  '
 775    continue
      endif
      WRITE(irofil,'(a)') '# other definitions of outside follow... '

C If there are other zones that might be seen stick that data
C in here.
      newpic=nzg
      do 54 mz=1,newpic
        newfoc=nznog(mz)

C Clear properties.
        nbobs(mz)=0

C Read in the zone geometry.
        WRITE(outs,'(a,a)')' Scanning : ',LGEOM(newfoc)
        if(ACT.eq.'i')then
          call usrmsg(' ',outs,'-')
          call georead(ITA1,LGEOM(newfoc),newfoc,1,IUOUT,IER)
        else
          call georead(ITA1,LGEOM(newfoc),newfoc,1,IUOUT,IER)
        endif

C Check if there is a legacy zone obstruction file scan it.
        if(IOBS(newfoc).EQ.1)then
          CALL EGOMST(ITA1,newfoc,ZOBS(newfoc),0,ITRC,ITRU,IER)
        endif

C Read zone constructions file.
        call FINDFIL(LTHRM(newfoc),XST)
        if(XST)then
          CALL ECONST(LTHRM(newfoc),ITA1,newfoc,0,IUOUT,IER)
          if (LTWIN(newfoc)(1:2).ne.'  '.and.
     &        LTWIN(newfoc)(1:4).ne.'UNKN') then
            CALL ERTWIN(0,IUOUT,ITA1,LTWIN(newfoc),newfoc,IER)
          endif
        endif
        ZN=zname(newfoc)

C Display the other zone, resetting all surface lines to std.
        if(ACT.eq.'i')then
          MODIFYVIEW=.TRUE.
          MODLEN=.TRUE.
          MODBND=.TRUE.
          CALL INLNST(1)
          nzg=1
          nznog(1)=newfoc
          izgfoc=newfoc
          CALL ADJVIEW(IER)
        endif

C Generate combined zone & generate surface names as identifiers.
        DO IS=1,NSUR
          ICN=IZSTOCN(newfoc,is)
          write(zsn(is),'(3a)') ZN(1:lnblnk(ZN)),':',SSNAME(icn)
        ENDDO  ! IS

        WRITE(irofil,'(3a)')'# ',ZN(1:lnblnk(ZN)),' Surface defs..'
        WRITE(irofil,'(a)') '  '
        WRITE(irzfil,'(3a)')'# ',ZN(1:lnblnk(ZN)),' Surface defs..'
        WRITE(irzfil,'(a)') '  '
        WRITE(iglzfil,'(3a)')'# ',ZN(1:lnblnk(ZN)),' Surface defs..'
        WRITE(iglzfil,'(a)') '  '

C Aternative glazing descriptions file.
        if (NABS.gt.0) then
          WRITE(ita2,'(3a)')'# ',ZN(1:lnblnk(ZN)),' Surface defs..'
          WRITE(ita2,'(a)') '  '
        endif

C Confirm inclusion of obstructions.
        if(nbobs(newfoc).ge.1)then
          if(ACT.eq.'i')then
            write(outs,'(3a)') ' Include ',ZN(1:lnblnk(ZN)),
     &                         ' obstructions ? '
            dok=.true.
            CALL ASKOK(' ',outs,OK,dok,nbhelp)
          else
            OK=.TRUE.
          endif
          if(.not.OK)goto 61


          WRITE(irofil,'(a)') '# Obstruction defs... '
          WRITE(irofil,'(a)') '  '

          nobs=nobs+nbobs(newfoc)  ! increment count of obstructions

          do 43 ib=1,nbobs(newfoc)

C Create a genrbox command to match the obstruction.
C BLOCKTYP (4 char) type of block:
C   'obs ' - standard block (origin, three dimensions, one rotation)
C   'obs3' - general block (origin, three dimensions, three rotations)
C            which must be converted as per obsp so that rotations do
C            not rely on xform
C   'obsp' - general polygon (six sides formed from 8 vertices)

C Set material name
            if (BLOCKMAT(newfoc,ib)(1:4).eq.'NONE') then
              lnbm=7
              ObsMatName='unknown'
            else
              lnbm=LNBLOCKMAT(newfoc,ib)
              ObsMatName=BLOCKMAT(newfoc,ib)
            endif

            if (BLOCKTYP(newfoc,ib).eq.'obs ') then

C Create a >genrbox< command to match the obstruction.
              WRITE(irofil,'(4a,3f9.3,a,f9.3,a,3f9.3)')
     &          '!genrbox rc_ex_',ObsMatName(1:lnbm),'  ',        
     &          BLOCKNAME(newfoc,IB)(1:LNBLOCKNAME(newfoc,IB)),
     &          DXOB(newfoc,ib),DYOB(newfoc,ib),DZOB(newfoc,ib),
     &          ' | xform -rz ',BANGOB(newfoc,ib,1),
     &          ' -t ',XOB(newfoc,ib),YOB(newfoc,ib),ZOB(newfoc,ib)
              WRITE(irofil,'(a)') ' '
            elseif (BLOCKTYP(newfoc,ib).eq.'obs3') then

C Convert obs3 to polygons here because of differences in how
C xform does rotations different from how obs3 does rotations
              CALL CNVBLK3A(XOB(newfoc,IB),YOB(newfoc,IB),
     &          ZOB(newfoc,IB),DXOB(newfoc,IB),DYOB(newfoc,IB),
     &          DZOB(newfoc,IB),BANGOB(newfoc,IB,1),
     &          BANGOB(newfoc,IB,2),BANGOB(newfoc,IB,3))

C Create a >polygon< command for each of 6 faces
              do isf=1,6 ! hard-code this?
                WRITE(irofil,'(5a,I1)')'rc_ex_',
     &            ObsMatName,'  polygon ',
     &            BLOCKNAME(newfoc,IB)(1:LNBLOCKNAME(newfoc,IB)),
     &            '_', isf  ! add face number to block name
                WRITE(irofil,'(a)') '0 '
                WRITE(irofil,'(a)') '0 '
                WRITE(irofil,'(I3)') 12 ! NGVER(???)*3 

C Write the coordinates for face isf.
                do iv=1,4 ! NGVER(???)
                  K = JVNB(isf,iv) ! variable for obstruction blocks?
                  WRITE(irofil,'(3F11.5)') XB(K),YB(K),ZB(K)
                enddo
                WRITE(irofil,'(a)') '  '
              enddo   ! isf

            elseif (BLOCKTYP(newfoc,ib).eq.'obsp') then

C Create a >polygon< command for each of 6 faces
              do isf=1,6 ! hard-code this?
                WRITE(irofil,'(5a,I1)')'rc_ex_',
     &            ObsMatName,'  polygon ',
     &            BLOCKNAME(newfoc,IB)(1:LNBLOCKNAME(newfoc,IB)),
     &            '_', isf  ! add face number to block name
                WRITE(irofil,'(a)') '0 '
                WRITE(irofil,'(a)') '0 '
                WRITE(irofil,'(I3)') 12 ! NGVER(???)*3 

C Write the coordinates for face isf.
                do iv=1,4 ! NGVER(???)
                  K = JVNB(isf,iv) ! variable for obstruction blocks?
                  WRITE(irofil,'(3F11.5)')
     &              XBP(newfoc,IB,K),YBP(newfoc,IB,K),ZBP(newfoc,IB,K)
                enddo  ! iv
                WRITE(irofil,'(a)') '  '
              enddo  ! isf
            else
               ! error, shouldn't end up here ...
            endif  ! check BLOCKTYP()
 43       continue ! loop through all obstructions of zone
        endif      ! nbobs() .ge. 1


C Confirm inclusion of visual object.
        if(nbvis(newfoc).ge.1)then
          if(ACT.eq.'i')then
            write(outs,'(3a)') ' Include ',ZN(1:lnblnk(ZN)),
     &                         ' visual entities ? '
            dok=.true.
            CALL ASKOK(' ',outs,OK,dok,nbhelp)
          else
            OK=.TRUE.
          endif
          if(.not.OK)goto 61

          WRITE(irofil,'(a)') '# visual object defs... '
          WRITE(irofil,'(a)') '  '

          nvis=nvis+nbvis(newfoc)  ! increment count of visual entities

          do 143 ib=1,nbvis(newfoc)

C Create a genrbox command to match the visual entities.
C VISTYP (4 char) type of block:
C   'vis ' - standard block (origin, three dimensions, one rotation)
C   'vis3' - general block (origin, three dimensions, three rotations)
C            which must be converted as per visp so that rotations do
C            not rely on xform
C   'visp' - general polygon (six sides formed from 8 vertices)

C Set material name
            if (VISMAT(newfoc,ib)(1:4).eq.'NONE') then
              lnbm=7
              ObsMatName='unknown'
            else
              lnbm=LNVISMAT(newfoc,ib)
              ObsMatName=VISMAT(newfoc,ib)
            endif

            if (VISTYP(newfoc,ib).eq.'vis ') then

C Create a >genrbox< command to match the obstruction.
              WRITE(irofil,'(4a,3f9.3,a,f9.3,a,3f9.3)')
     &          '!genrbox rc_ex_',ObsMatName(1:lnbm),'  ',        
     &          VISNAME(newfoc,IB)(1:LNVISNAME(newfoc,IB)),
     &          DXOV(newfoc,ib),DYOV(newfoc,ib),DZOV(newfoc,ib),
     &          ' | xform -rz ',BANGOV(newfoc,ib,1),
     &          ' -t ',XOV(newfoc,ib),YOV(newfoc,ib),ZOV(newfoc,ib)
              WRITE(irofil,'(a)') ' '
            elseif (VISTYP(newfoc,ib).eq.'vis3') then

C Convert vis3 to polygons here because of differences in how
C xform does rotations different from how obs3 does rotations
              CALL CNVBLK3A(XOV(newfoc,IB),YOV(newfoc,IB),
     &          ZOV(newfoc,IB),DXOV(newfoc,IB),DYOV(newfoc,IB),
     &          DZOV(newfoc,IB),BANGOV(newfoc,IB,1),
     &          BANGOV(newfoc,IB,2),BANGOV(newfoc,IB,3))

C Create a >polygon< command for each of 6 faces
              do isf=1,6 ! hard-code this?
                WRITE(irofil,'(5a,I1)')'rc_ex_',
     &            ObsMatName,'  polygon ',
     &            VISNAME(newfoc,IB)(1:LNVISNAME(newfoc,IB)),
     &            '_', isf  ! add face number to block name
                WRITE(irofil,'(a)') '0 '
                WRITE(irofil,'(a)') '0 '
                WRITE(irofil,'(I3)') 12 ! NGVER(???)*3 

C Write the coordinates for face isf.
                do iv=1,4 ! NGVER(???)
                  K = JVNB(isf,iv) ! variable for visual blocks?
                  WRITE(irofil,'(3F11.5)') XB(K),YB(K),ZB(K)
                enddo  ! iv
                WRITE(irofil,'(a)') '  '
              enddo  ! isf

            elseif (VISTYP(newfoc,ib).eq.'visp') then

C Create a >polygon< command for each of 6 faces
              do isf=1,6 ! hard-code this?
                WRITE(irofil,'(5a,I1)')'rc_ex_',
     &            ObsMatName,'  polygon ',
     &            VISNAME(newfoc,IB)(1:LNVISNAME(newfoc,IB)),
     &            '_', isf  ! add face number to block name
                WRITE(irofil,'(a)') '0 '
                WRITE(irofil,'(a)') '0 '
                WRITE(irofil,'(I3)') 12 ! NGVER(???)*3 

C Write the coordinates for face isf.
                do iv=1,4 ! NGVER(???)
                  K = JVNB(isf,iv) ! variable for visual blocks?
                  WRITE(irofil,'(3F11.5)')
     &              XVP(newfoc,IB,K),YVP(newfoc,IB,K),ZVP(newfoc,IB,K)
                enddo  ! iv
                WRITE(irofil,'(a)') '  '
              enddo  ! isf
            else
               ! error, shouldn't end up here ...
            endif  ! check VISTYP()
 143      continue ! if nbvis
        endif

  61    continue  ! jump point

C Now surface information.
        WRITE(irofil,'(a)') '# Outside zone Surface defs... '
        WRITE(irofil,'(a)') '  '
        do 75 isf=1,nsur

C Set surface logicals to false.
          extern=.false.
          similar=.false.

C Get the surfaces connection index;
C Opaque,
C   External connections: create two surfaces (shift 3mm inwards).
C   Other connections: create one surface (shift 3mm inwards).
C Transparent,
C   All connections: create one surface (no shift).
          CALL SURADJ(newfoc,isf,IE,TMP,IZC,ISC,icnx,DESCRC)
          ICN=IZSTOCN(newfoc,isf)
          if(IE.eq.0) then
            extern=.true.
          elseif(IE.eq.1) then
            similar=.true.
          endif

C Find the matching mlc index and set the material name.
C Radiance does not yet know what to do with CFC and CFC2.
C << use call to matchmlcdesc >>
          ICF=-1
          ifict=0
          call matchmlcdesc(SSMLCN(icn),icf)
          if(ICF.eq.-1.or.SSMLCN(icn)(1:4).eq.'UNKN')then
            if (SSOTF(icn)(1:4).ne.'OPAQ'.and.
     &          SSOTF(icn)(1:4).ne.'CFC '.and.
     &          SSOTF(icn)(1:4).ne.'CFC2') then
              material='unknown_glz'
            else
              material='unknown'
            endif
          else
            lnsm=lnblnk(SSMLCN(icn))
            write(material,'(a)') SSMLCN(icn)(1:lnsm)
            if(iskip(icf).eq.1)ifict=1
          endif

C Depending on surface type (fict/tran/opaq) write header info.  
          if(ifict.eq.1)then
C          if(iskip(icf).eq.1)then
            WRITE(outs,'(a,a)') '# skipping fict surface: ',zsn(isf)
            if(ACT.eq.'i')call edisp(iuout,outs)
            WRITE(iglzfil,'(a)') outs
            WRITE(iglzfil,'(a)') '  '

C Aternative glazing descriptions file.
            if (NABS.gt.0) then
              WRITE(ita2,'(a)') outs
              WRITE(ita2,'(a)') '  '
            endif
          elseif(SSOTF(icn)(1:4).ne.'OPAQ'.and.
     &           SSOTF(icn)(1:4).ne.'CFC '.and.
     &           SSOTF(icn)(1:4).ne.'CFC2')then

C Create surface based on the following:
C  Focus zone - create.
C  External glz - create.
C  Partition - create if not already created or focus zone.
            WRITE(iglzfil,'(a)') '  '
            if (NABS.gt.0) WRITE(ita2,'(a)') '  '
            if (extern.or.similar.or.(newfoc.eq.ifocz).or.
     &                   .not.((IZC.eq.ifocz).or.(IZC.lt.newfoc))) then
              IANS=1
C Logic - only allow precalc of illum in focus zone.
C              if ((newfoc.eq.ifocz).and. 
C     &            ((SCENEPURP(ISCENE)(1:8).eq.'Internal').or.
C     &             (SCENEPURP(ISCENE)(1:5).eq.'Glare'))) then
C Logic - ask about precalc in all internal zones.
              if (((SCENEPURP(ISCENE)(1:8).eq.'Internal').or.
     &             (SCENEPURP(ISCENE)(1:5).eq.'Glare'))) then

C Highlight surface on image.
                if(ACT.eq.'i')then
                  MODIFYVIEW=.TRUE.
                  CALL INLNST(1)
                  CALL SURADJ(ifocz,isf,IE,TMP,IZC,ISC,IC,DESCRC)
                  LINSTY(IC)=2
                  CALL ADJVIEW(IER)

                  helptopic='precalculate_indirect_i'
                  call gethelptext(helpinsub,helptopic,nbhelp)
                  write (outs,'(a,a,a)') 'Surface ',
     &                   zsn(isf)(1:lnblnk(zsn(isf))),' is transparent.'
                 call EASKAB(outs,'Pre-calculate indirect illumination',
     &             'no (default)','yes',IANS,nbhelp)
                  iglzty=2
                else
                  IANS=1
                endif
              endif

C Write mkillum header (e=exclude, i=include).
              if (IANS.eq.1) then 
                WRITE(iglzfil,'(a,a,a)') '#@mkillum e=',
     &                         material(1:lnblnk(material)),' d=48 s=32' 
                if (NABS.gt.0) WRITE(ita2,'(a,a,a)') '#@mkillum e=',
     &                      material(1:lnblnk(material)),'_sw d=48 s=32' 
              else 
                WRITE(iglzfil,'(a,a,a)') '#@mkillum i=',
     &                         material(1:lnblnk(material)),' d=48 s=32'
                if (NABS.gt.0) WRITE(ita2,'(a,a,a)') '#@mkillum i=',
     &                      material(1:lnblnk(material)),'_sw d=48 s=32' 
              endif

C If IANS=2 then ask if surface should be subdivided.
              IDIV=1
              if (IANS.eq.2) then 
                write(outs,'(a,a,a)')'Surface ',
     &            zsn(isf)(1:lnblnk(zsn(isf))),' can be subdivided.'
                call EASKAB(outs,'Create ','one surface (default)',
     &            'multiple surfaces',IDIV,nbhelp)
              endif

              if (IDIV.eq.2) then
                call CHECKREC(ISF,IRECT)
                IR=0

C IRECT: -1 not rectangular; 0 error; 1 rectangular.
                if (IRECT.eq.1) then
                  call PARAMET(ISF,SVALS,TVALS,TFORM,IR)
                  if (IR.eq.0) then
                    QT=CHAR(39)
                    WRITE(iglzfil,666)'!gensurf',
     &                material(1:lnblnk(material)),
     &                zsn(isf)(1:lnblnk(zsn(isf))),
     &                QT,SVALS(1),'*s+',TVALS(1),'*t',QT,
     &                QT,SVALS(2),'*s+',TVALS(2),'*t',QT,
     &                QT,SVALS(3),'*s+',TVALS(3),'*t',QT,' 2  2',
     &                ' | xform -t ',(TFORM(IXX),IXX=1,3)
                    if (NABS.gt.0) then
                      write(outs,'(a,a)') 
     &                  material(1:lnblnk(material)),'_sw'
                      WRITE(ita2,666)'!gensurf',outs(1:lnblnk(outs)),
     &                  zsn(isf)(1:lnblnk(zsn(isf))),
     &                  QT,SVALS(1),'*s+',TVALS(1),'*t',QT,
     &                  QT,SVALS(2),'*s+',TVALS(2),'*t',QT,
     &                  QT,SVALS(3),'*s+',TVALS(3),'*t',QT,' 2  2',
     &                  ' | xform -t ',(TFORM(IXX),IXX=1,3)
                    endif
 666                format (3(a,1x),3(a,f8.4,a,f8.4,a,a,1x),a,a,3f8.4)
                  endif
                elseif (IRECT.eq.-1) then
                  WRITE(outs,'(a,a)')
     &                   'Warning cannot subdivide surface: ', zsn(isf)
                  call edisp(iuout,outs)
                  IR=1
                else
                  WRITE(outs,'(a,a)')
     &                   'Warning cannot subdivide surface: ', zsn(isf)
                  call edisp(iuout,outs)
                  IR=1
                endif
                if (IR.ne.0) then
                  WRITE(iglzfil,'(3a)')material(1:lnblnk(material)),
     &              '  polygon  ',zsn(isf)(1:lnblnk(zsn(isf)))
                  WRITE(iglzfil,'(a)') '0 '
                  WRITE(iglzfil,'(a)') '0 '
                  WRITE(iglzfil,'(I3)')NVER(isf)*3
                  do 1274 iv=NVER(isf),1,-1
                    WRITE(iglzfil,'(3F11.5)')
     &                X(JVN(isf,iv)),Y(JVN(isf,iv)),Z(JVN(isf,iv))
 1274             continue
                  if (NABS.gt.0) then
                    WRITE(ita2,'(3a)')material(1:lnblnk(material)),
     &                '_sw  polygon  ',zsn(isf)(1:lnblnk(zsn(isf)))
                    WRITE(ita2,'(a)') '0 '
                    WRITE(ita2,'(a)') '0 '
                    WRITE(ita2,'(I3)')NVER(isf)*3
                    do 1275 iv=NVER(isf),1,-1
                      WRITE(ita2,'(3F11.5)')
     &                  X(JVN(isf,iv)),Y(JVN(isf,iv)),Z(JVN(isf,iv))
 1275               continue
                  endif
                endif
              else
                WRITE(iglzfil,'(3a)') material(1:lnblnk(material)),
     &            '  polygon  ',zsn(isf)(1:lnblnk(zsn(isf)))
                WRITE(iglzfil,'(a)') '0 '
                WRITE(iglzfil,'(a)') '0 '
                WRITE(iglzfil,'(I3)')NVER(isf)*3
                do 1174 iv=NVER(isf),1,-1
                  WRITE(iglzfil,'(3F11.5)')
     &              X(JVN(isf,iv)),Y(JVN(isf,iv)),Z(JVN(isf,iv))
 1174           continue
                if (NABS.gt.0) then
                  WRITE(ita2,'(3a)') material(1:lnblnk(material)),
     &              '_sw  polygon  ',zsn(isf)(1:lnblnk(zsn(isf)))
                  WRITE(ita2,'(a)') '0 '
                  WRITE(ita2,'(a)') '0 '
                  WRITE(ita2,'(I3)')NVER(isf)*3
                  do 1175 iv=NVER(isf),1,-1
                    WRITE(ita2,'(3F11.5)')
     &                X(JVN(isf,iv)),Y(JVN(isf,iv)),Z(JVN(isf,iv))
 1175             continue
                endif
              endif
            else
              WRITE(outs,'(a,a)')'# skipping transparent surface: ',
     &                                                         zsn(isf)
              if(ACT.eq.'i')call edisp(iuout,outs)
              WRITE(iglzfil,'(a)') outs
              WRITE(iglzfil,'(a)') '  '
              if (NABS.gt.0) then
                WRITE(ita2,'(a)') outs
                WRITE(ita2,'(a)') '  '
              endif
            endif
          else

C Opaque surfaces.
C Prep. for transforms along normal (outwards for outside, inwards
C for inside.)
            N=NVER(isf)
            DO 150 iv = 1,N
              XX(iv) = X(JVN(isf,iv))
              YY(iv) = Y(JVN(isf,iv))
              ZZ(iv) = Z(JVN(isf,iv))
  150       CONTINUE
            WRITE(irzfil,'(4a)')'rc_in_',
     &        material(1:lnblnk(material)),'  polygon  ',
     &        zsn(isf)(1:lnblnk(zsn(isf)))
            WRITE(irzfil,'(a)') '0 '
            WRITE(irzfil,'(a)') '0 '
            WRITE(irzfil,'(I3)')NVER(isf)*3

C Shift surface inwards by 3mm (only if extern=true).
            N=NVER(isf)
C            if (extern) then
              vdis = -0.003

C Find transformation matrices that normalise face.
              call PLEQN(XX,YY,ZZ,N,VP,EQN,IERR)
              IF (IERR .NE. 0)then
                write(outs,'(a,a)') ' PLEQN problem with ',zsn(isf)
                call edisp(itru,outs)
              endif
              DO 352 K = 1,3
                TRNS(k)=EQN(k)*vdis
  352         continue
C            else
C              TRNS(1)= 0.00
C              TRNS(2)= 0.00
C              TRNS(3)= 0.00
C            endif
            do 353 k=1,N
              XT(k)=XX(k)+TRNS(1)
              YT(k)=YY(k)+TRNS(2)
              ZT(k)=ZZ(k)+TRNS(3)
  353       continue
            iv=N+1
174         iv=iv-1
            WRITE(irzfil,'(3F11.5)')XT(iv),YT(iv),ZT(iv)
            if(iv.gt.1)goto 174
            WRITE(irzfil,'(a)') '  '

            if (extern) then
              WRITE(irofil,'(4a)')'rc_ex_',
     &          material(1:lnblnk(material)),'  polygon  ',
     &          zsn(isf)(1:lnblnk(zsn(isf)))
              WRITE(irofil,'(a)') '0 '
              WRITE(irofil,'(a)') '0 '
              WRITE(irofil,'(I3)')NVER(isf)*3

C Shift surface outwards by 3mm.
              vdis = 0.003

C Find transformation matrices that normalise face.
              call PLEQN(XX,YY,ZZ,N,VP,EQN,IERR)
              IF (IERR .LT. 0)then
                write(outs,'(a,a)') ' PLEQN problem with ',zsn(isf)
                call edisp(itru,outs)
              endif
              DO 354 K = 1,3
                TRNS(k)=EQN(k)*vdis
  354         continue
              do 355 k=1,N
                XT(k)=XX(k)+TRNS(1)
                YT(k)=YY(k)+TRNS(2)
                ZT(k)=ZZ(k)+TRNS(3)
  355         continue
              do 74 iv=1,N
                WRITE(irofil,'(3F11.5)')XT(iv),YT(iv),ZT(iv)
 74           continue
              WRITE(irofil,'(a)') '  '
            endif
          endif
 75     continue
 54   continue
      outdone = .TRUE.
      indone = .TRUE.
      CALL ERPFREE(irofil,ISTAT)
      CALL ERPFREE(irzfil,ISTAT)
      CALL ERPFREE(imatfil,ISTAT)
      CALL ERPFREE(iglzfil,ISTAT)
      CALL ERPFREE(ita2,ISTAT)

C If there are more than 6 obstructions or visual entities it is worth
C expanding the outside definition via an 'xform -e' command to speed
C calculation.
      if(nobs.gt.6.or.nvis.gt.6)then
        write(outs,'(a,i3,a,i3,a)') 'expanding ',nobs,
     &    ' obstructions and ',nvis,' visual entities...'
        call usrmsg(outs,' ','-')
        write(tfile,'(a,a)') runpath(1:lnrp),'exprofil'
        INQUIRE (FILE=tfile,EXIST=XST)
        if(XST)then
          call FPOPEN(ITA1,ISTAT,1,3,tfile)
          call EFDELET(ITA1,ISTAT)
        endif
        CALL ERPFREE(irofil,ISTAT)
        CALL ERPFREE(ita1,ISTAT)

C tfile is used for the backup copy of the outside description - if it 
C exists delete.
        write(tfile,'(3a)') runpath(1:lnrp),rofil(1:lnblnk(rofil)),'-'
        INQUIRE (FILE=tfile,EXIST=XST)
        if(XST)then
          call FPOPEN(ITA1,ISTAT,1,3,tfile)
          call EFDELET(ITA1,ISTAT)
        endif

C Change dir to runpath and expand outside description to tmp file.
C Make backup of unxepanded file by moving existing file to tfile.
C xform needs the file preceeded by ./   Pause briefly between
C runit calls to allow OS to detect new file creation.
        write(doit,'(5a)') 'cd ',runpath(1:lnrp),
     &    '; xform -e ./',rofil(1:lnblnk(rofil)),' >exprofil'

C Debug.
C        write(6,*) doit

        call runit(doit,'-')
        call pausems(400)
        write(doit,'(7a)') 'cd ',runpath(1:lnrp),
     &    '; mv ./',rofil(1:lnblnk(rofil)),' ',
     &    rofil(1:lnblnk(rofil)),'-'

C Debug.
C        write(6,*) doit

        call runit(doit,'-')
        call usrmsg('expanding obstructions...done.',' ','-')

        CALL ERPFREE(irofil,ISTAT)
        CALL ERPFREE(ita1,ISTAT)

C Compact file by opening temporary file and writing it out to the 
C original outside description file name.
        write(pfile,'(a,a)') runpath(1:lnrp),rofil(1:lnblnk(rofil))
        call FPOPEN(irofil,ISTAT,1,3,pfile)
        write(tfile,'(a,a)') runpath(1:lnrp),'exprofil'
        call FPOPEN(ita1,ISTAT,1,3,tfile)
        call usrmsg('compacting file...',' ','-')

C Make sure the first line of the exterior composition follows
C e2r conventions.
        write(irofil,'(a)',iostat=ios,err=103) 
     &    '# Radiance exterior composition'
  42    continue
        read(ita1,'(a124)',iostat=ios,err=101,end=102)outsn
        call SDELIM(outsn,outsd,'S',IW)
        write(irofil,'(a)',iostat=ios,err=103) outsd(1:lnblnk(outsd))
        goto 42

C Remove temporary file and close.
 102    CALL EFDELET(ita1,ISTAT)
        call erpfree(ita1,istat)
        call erpfree(irofil,istat)
        call usrmsg('compacting file...done.',' ','-')
      endif

      return

 101  call edisp(iuout,'error reading temporary outside file.')
      call erpfree(ita1,istat)
      call erpfree(irofil,istat)
      return

 103  call edisp(iuout,'error writing compact outside file.')
      call erpfree(ita1,istat)
      call erpfree(irofil,istat)
      return

      end

C mkxform creates a file to hold xform commands for IES.rad entries.
C If ACT='i' then interactive, ACT='s' for silent.
      subroutine mkxform(ACT,IER)
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "esprdbfile.h"
#include "material.h"
#include "e2r_common.h"
#include "prj3dv.h"
    
      integer lnblnk  ! function definition

C Passed parameters.
      character act*1
      integer ier

      COMMON/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)
      real BB(2,3)   ! bounding box in site coordinates
      real roties    ! assumed rotation

C Local variables
      logical found
      character ltmp72*72,dtmp72*72,tfile*72
      real trnsx,trnsy,trnsz  ! transforms IES entity to surface cog.
      character combinedname*36,outs*124
      character iescontrolledname*36  ! ies2rad will have generated several
      dimension iescontrolledname(5)  ! rad files for different control states
      integer loop,loop2,iiessteps(5)

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='mkxform'  ! set for subroutine

      ITA1 = IFIL+6
      ISTAT=0
      IER=0

      if(nbofies.eq.0)then
        return
      endif

C Generate help for the various dialogs.
      helptopic='writing_ies_xforms'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Loop through all the surfaces looking for SUSE=FIXTURE.
      found=.false.
      do icn=1,ncon
        if(SSUSE(icn,1)(1:7).eq.'FIXTURE')then
          if(SSUSE(icn,2)(1:2).eq.'- ')then
            continue
          else

C Found something - think of what to do...
            found=.true.
            write(outs,*) 'found surface with fixture',icn,
     &        SSUSE(icn,1),SSUSE(icn,2)
            call edisp(iuout,outs)
          endif
        endif
      enddo  ! if icn

C If no surfaces were associated with an IES entity then
C no need to create this file.
      if(.NOT.found) return

C Generate a file name.
      if(ACT.eq.'i')then
 22     ltmp72=iesfil
        dtmp72=iesfil
        CALL EASKS(ltmp72,' IES data xforms file name ?','  ',
     &    72,dtmp72,'ies data xforms file name',IER,nbhelp)
        if(ltmp72(1:2).eq.'  ') goto 22
        iesfil=ltmp72
        write(tfile,'(a,a)')runpath(1:lnrp),iesfil(1:lnblnk(iesfil))
        call FPOPEN(iiesfil,ISTAT,1,4,tfile)
      else
        write(tfile,'(a,a)')runpath(1:lnrp),iesfil(1:lnblnk(iesfil))
        call FPOPEN(iiesfil,ISTAT,1,3,tfile)
      endif

C Write a header into this file.
      WRITE(iiesfil,'(a)') '# Radiance IES transforms file '
      WRITE(iiesfil,'(a)') '# (do not modify first line) '

C Process each zone.
      do newfoc=1,ncomp
        do isf=1,nzsur(newfoc)
          ICN=IZSTOCN(newfoc,isf)
          if(SSUSE(icn,1)(1:7).eq.'FIXTURE')then
            if(SSUSE(icn,2)(1:2).eq.'- ')then
              continue
            else

C Loop through the known IES entities to find a match.
              do loop=1,nbofies
                if(iesname(loop).eq.SSUSE(icn,2))then

C The transform (assuming the IES data is centred at the
C site origin) is to the surface centre of gravity.
                  trnsx=ssurcog(icn,1)
                  trnsy=ssurcog(icn,2)
                  trnsz=ssurcog(icn,3)-0.01   ! bit lower in case embedded in bounding surface

C The rotation of the IES data is assumed to be a match
C of its longer dimension axis (X or Y) and the longest
C axis of the surface.
                  call SUR3DLEHI(newfoc,isf,WID,HIGH,BB)
                  if((BB(2,1)-BB(1,1)).ge.(BB(2,2)-BB(1,2)))then

C Associated surface is longer along X axis. If IES entity is
C also longer along X no rotation needed. 
                    if(iesalong(loop).eq.'X'.or.
     &                 iesalong(loop).eq.'x')then
                      roties=0.0
                    else
                      roties=90.0
                    endif
                  else

C Associated surface is wider along Y axis.
                    if(iesalong(loop).eq.'Y'.or.
     &                 iesalong(loop).eq.'y')then
                      roties=0.0
                    else
                      roties=90.0
                    endif
                  endif
                  
C Name for the beastie - probably based on zone:surface:iesname
                  lnz=lnblnk(zname(newfoc))
                  lns=lnblnk(ssname(icn))
                  lnu=lnblnk(SSUSE(icn,2))
                  write(combinedname,'(5a)') zname(newfoc)(1:lnz),
     &              ':',ssname(icn)(1:lns),':',SSUSE(icn,2)(1:lnu)
                  lncmb=lnblnk(combinedname)

C Similar logic to edoptic.F
                  if(iespercents(loop).eq.1)then
                    iiessteps(1)=100; iiessteps(2)=001
                  elseif(iespercents(loop).eq.2)then
                    iiessteps(1)=100; iiessteps(2)=001
                  elseif(iespercents(loop).eq.4)then
                    iiessteps(1)=100; iiessteps(2)=50
                    iiessteps(3)=20; iiessteps(4)=01
                  elseif(iespercents(loop).eq.5)then
                    iiessteps(1)=100; iiessteps(2)=75
                    iiessteps(3)=50; iiessteps(4)=25; iiessteps(5)=01
                  endif
              
C Report what has been decided.
C Debug
                  write(6,*) 'found surface with fixture',icn,
     &              SSUSE(icn,1),SSUSE(icn,2),combinedname,trnsx,
     &              trnsy,trnsz,roties
                  write(6,*) iescontrolledname(1)
                  write(6,*) iescontrolledname(2)
                  write(6,*) iescontrolledname(3)
                  write(6,*) iescontrolledname(4)

C For each control state.  Put a hash in front of subsequent lines (it is
C up to the user to edit the file at simulation run time).
                  do loop2=1,iespercents(loop)
                    write(iescontrolledname(loop2),'(a,i3.3,a)')
     &                SSUSE(icn,2)(1:lnu),iiessteps(loop2),'.rad'
                    lnctl=lnblnk(iescontrolledname(loop2))
                    if(loop2.eq.1)then
                      WRITE(iiesfil,'(3a,f7.2,a,3f7.3,2a)') 
     &                  ' !xform -e -n ',combinedname(1:lncmb),
     &                  ' -rz ',roties,' -rx 0 -t ',
     &                  trnsx,trnsy,trnsz,' ',
     &                  iescontrolledname(loop2)(1:lnctl)
                      WRITE(6,'(3a,f7.2,a,3f7.3,2a)') ' !xform -e -n ',
     &                  combinedname(1:lncmb),' -rz ',roties,
     &                  ' -rx 0 -t ',trnsx,trnsy,trnsz,' ',
     &                  iescontrolledname(loop2)(1:lnctl)
                    else
                      WRITE(iiesfil,'(3a,f7.2,a,3f7.3,2a)') 
     &                  '# !xform -e -n ',combinedname(1:lncmb),
     &                  ' -rz ',roties,' -rx 0 -t ',
     &                  trnsx,trnsy,trnsz,' ',
     &                  iescontrolledname(loop2)(1:lnctl)
                      WRITE(6,'(3a,f7.2,a,3f7.3,2a)') 
     &                  '# !xform -e -n ',combinedname(1:lncmb),' -rz ',
     &                  roties,' -rx 0 -t ',trnsx,trnsy,trnsz,' ',
     &                  iescontrolledname(loop2)(1:lnctl)
                    endif
                  enddo  ! of loop2

C Depending on the control need to write out multiple lines but
C initially only uncomment the ON state.

                else
                  continue
                endif
              enddo  ! of loop
            endif
          endif
        enddo  ! of isf
      enddo    ! of newfoc     
      CALL ERPFREE(iiesfil,ios)

      return
      end
