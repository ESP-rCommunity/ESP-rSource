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
C  mkriofil Write inside/outside materials and composition files.

C ******* Sky calculations. *******
      SUBROUTINE SKYFORM(IER)
#include "building.h"
#include "site.h"
#include "e2r_common.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL
      common/pophelp/h(60)
      common/expath/runpath,pathtype

      character item*30,h*72,DESCRH*5,DESCRD*5,DESCR*7,DESCR1*10
      character skopt*3,outs*124,tfile*72
      character runpath*72,pathtype*24,ltmp72*72,dtmp72*72
      integer lnrp  ! length of run path

      DIMENSION ITEM(14)

      logical OK,DOK,XST

      ITA1 = IFIL+6
      ITA2 = IFIL+7
      ier=0
      lnrp=lnblnk(runpath)

C Generate a default name for the sky file.
      write (rskyfil,'(a,a4)') 
     &       SCENERT(ISCENE)(1:lnblnk(SCENERT(ISCENE))),'.sky'

C Ask for name of the sky file.  If existing, attempt to
C read it otherwise remember name.
 142  H(1)=' Supply a local file name (i.e. without a path).'
      H(2)=' This file holds the sky definition.'
      ltmp72=rskyfil
      dtmp72=rskyfil
      CALL EASKS(ltmp72,' Sky distribution file name ?',' ',72,
     &                                  dtmp72,'sky file name',IER,2)
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
  20  H(1)='This menu defines the sky distribution function for a'
      H(2)='Radiance scene.'
      H(3)=' '
      H(4)='* Site Latitude: (imported from ESP-r configuration).'
      H(5)=' '
      H(6)='* Site Longitude: (imported from ESP-r configuration).'
      H(7)=' '
      H(8)='* Year: this should be defined automatically.'
      H(9)=' '
      H(10)='* Date: change this to the date and time of the'
      H(11)='  simulation, or leave it as the default.'
      H(12)=' '
      H(13)='* Ground refl: this should be defined automatically.'
      H(14)='  '
      H(15)='* Sky: Toggle the type of sky conditions for the'
      H(16)='  simulation. The CIE std overcast sky is the'
      H(17)='  standard lighting design sky condition.'
      H(18)=' '
      H(19)='* Generate sky description: Create the Radiance sky'
      H(20)='  description file.'
      H(21)=' '
      H(22)='* Display sky info: display the contents of the'
      H(23)='  sky description file in the text window.'

      CALL EMENU('  Sky description',ITEM,NITEMS,INO)
      IF(INO.EQ.1)THEN
        H(1)='The site latitude should be in degrees. '
        CALL EASKR(sitelat,' ',' Site Latitude ? ',
     &           -89.0,'W',89.,'W',50.2,'latitude',IER,1)
        skydone=.false.
      elseif(INO.EQ.2)then
        H(1)='Site longitude difference is degrees from some local'
        H(2)='reference meridian, east +ve, e.g. -4.1 for Glasgow'
        H(3)='relative to the Greenwich meridian.'
        H(4)=' '
        H(5)='Note this definition is different from that used by'
        H(6)='radiance.  Also, if a meridian other than the prime '
        H(7)='is defined in a sky file this will be reset to 0 and'
        H(8)='the longtitude difference calculated accordingly. '
        CALL EASKR(sitelongdif,' ',
     &    'Longitude difference from reference time meridian?',
     &     -15.0,'W',15.0,'W',0.0,'site longitude diff',IER,8)
        skydone=.false.
      elseif(INO.EQ.3)then
        H(1)='The year is requred to sort out days of the week. '
        CALL EASKI(iryear,' ',' Year ? ',
     &          1900,'F',2010,'W',1993,'year',IER,1)
      elseif(INO.EQ.4)then
        call edisp(iuout,' ')
        call edisp(iuout,' Date and time for the view... ')
        CALL EDAYR(irdoy,IDO,IMO)
        CALL ASKTIM(2,1,IMO,IDO,irdoy,RTIME,IT,IER)
        skydone=.false.
      elseif(INO.EQ.5)then
        H(1)='The average ground reflectance should be a fraction'
        H(2)='between 0.0 and 0.99.'
        CALL EASKR(rgrfl,' ',' Ground reflectance? ',
     &             0.0,'W',0.99,'W',0.2,'ground refl',IER,2)
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
          CALL EASKAB(' The sky is not a CIE standard overcast sky.',
     &            'Do you want to: ','proceed','change sky type',IW,0)
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
        endif
      elseif(INO.EQ.9)then
        H(1)='Browsing lists the file contents in the text feedback'
        H(2)='area, editing spawns an editor with the file name.'
        CALL EASKABC(' Sky description:',' ',
     &              'browse file','edit file','ignore',IW,2)
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
        CALL PHELPD('sky menu',23,'-',0,0,IER)
      elseif(INO.EQ.12)then
        if(skydone)then
          return
        else
          dok=.false.
          h(1)='Recent work has not yet been used to generated a sky'
          h(2)='file for use by radiance. If you exit without creating'
          h(3)='the sky file the model will be incomplete. '
          CALL ASKOK(' Sky file has not yet been generated...',
     &      ' Is it ok to exit ?',OK,dok,3)
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

      common/expath/runpath,pathtype

      character skycmd*124,doit*124,tfile*72,runpath*72,pathtype*24
      integer lnrp  ! length of run path

      LOGICAL XST

      lnrp=lnblnk(runpath)
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
      SUBROUTINE e2rform(ACT_1,IER)
#include "building.h"
#include "model.h"

C geometry.h provides commons G0 and G2.
#include "geometry.h"
#include "e2r_common.h"

      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL
      common/pophelp/h(60)
      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      COMMON/GTFIL/GTGEOM
      common/expath/runpath,pathtype

      COMMON/RAY3/MODIFY,MODLEN,MODBND
      LOGICAL MODIFY,MODLEN,MODBND

      LOGICAL OK,DOK,XST,fict
      logical newgeo  ! to use for testing if new/old geometry file.

      DIMENSION ITEM(17)
      character*72 tfile
      character runpath*72,pathtype*24
      character item*32,h*72,ACT_1*1,GTGEOM*72
      integer lnrp  ! length of run path

      ITA1 = IFIL+6
      outdone=.false.
      fict=.false.
      newgeo=.false.  ! assume older format geometry.
      iscope=2
      ier=0
C      GTGEOM = 'UNKNOWN'

C Open the primitive, mlc & optics db.
      lnrp=lnblnk(runpath)
      call opendb(ier)

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
      ITEM(12) =      '  ______________________       '
      ITEM(13)=       '? Help                         '
      ITEM(14)=       '- Exit                         '
      NITEMS = 14

C Help text for this menu.
  20  H(1)='This menu defines the composition of the Radiance'
      H(2)='scene in terms of materials and geometry (inside and'
      H(3)='outside. '
      H(4)=' '
      H(5)='There are 3 files created - materials (surface '
      H(6)='properties for surfaces and geometric composition'
      H(7)='of the inside and outside.  These are based on the '
      H(8)='current ESP-r materials and geometry specifications'
      H(9)='(taking into account the number of zones etc.). '
      H(10)=' '
      H(11)='After `generating` the files you may then manually'
      H(12)='edit them as required to add in textures or colours.'
      H(13)='Note that if you change one of the named material '
      H(14)='colours, then any geometric entity referencing it '
      H(15)='will be changed. '
      H(16)=' '
      H(17)='The `outer` face of internal partitions are EXCLUDED'
      H(18)='because they are duplicates of the inner face of the '
      H(19)='adjacent zone and cannot be seen in any event.  '
      H(20)=' '
      H(21)='=============| |===========     '
      H(22)='             | |                '
      H(23)='    visible->| |<- visible face '
      H(24)='    face     | |                '
      H(25)='  zone a     | |   zone b       '
      H(26)='             | |                '
      H(27)='=============| |===========     '
      H(28)='              |                 '
      H(29)='              `--- other sides of surfs '
      H(30)='                   not seen and EXCLUDED '

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
          H(1)='This value allows e2r to distinguish between normal'
          H(2)='glass and fictitious surfaces. '
          CALL EASKR(ftr,
     &      ' Above what (normal) visible transmission value  ',
     &      ' should a surface be considered fictitious ? ',
     &      0.9,'W',0.99,'W',0.9,'fict threashold',IER,2)
        else
          fict=.true.
        endif
      elseif(INO.EQ.4)then

C Read ground file.
        H(1)='This file contains a description of a ground form '
        H(2)='for use by Radiance and viewing (later by esp). '
        CALL EASKS(gtgeom,' Ground topology/geometry file name ? ',
     &        ' ',72,'grnd.geo','ground geom file',IER,2)
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
        call mkriofil(ftr,fict,ACT_1,IER)

C Re-Read in the initial zone geometry file into common & restore
C the image.
        call eclose(gversion(icomp),1.1,0.01,newgeo)
        if(newgeo)then
          call georead(ITA1,LGEOM(ICOMP),ICOMP,1,IUOUT,IER)
        else
          call egomin(ITA1,LGEOM(ICOMP),ICOMP,1,ITRC,IUOUT,IER)
        endif
        nzg=1
        nznog(1)=ICOMP
        MODIFY=.TRUE.
        MODBND=.TRUE.
        izgfoc=ICOMP
        CALL ADJVIEW(IER)
        outdone=.true.
      elseif(INO.EQ.8)then
        H(1)='Browsing lists the file contents in the text feedback'
        H(2)='area, editing spawns an editor with the file name.'
        CALL EASKABC(' Glazing description:',' ',
     &   'browse file','edit file','ignore',IW,2)
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
        H(1)='Browsing lists the file contents in the text feedback'
        H(2)='area, editing spawns an editor with the file name.'
        CALL EASKABC(' Opaque materials:',' ',
     &   'browse file','edit file',',ignore',IW,2)
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
        H(1)='Browsing lists the file contents in the text feedback'
        H(2)='area, editing spawns an editor with the file name.'
        CALL EASKABC(' Outside opaque description:',' ',
     &   'browse file','edit file','ignore',IW,2)
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
        H(1)='Browsing lists the file contents in the text feedback'
        H(2)='area, editing spawns an editor with the file name.'
        CALL EASKABC(' Inside opaque description:',' ',
     &   'browse file','edit file','ignore',IW,2)
        if(IW.eq.1)then
          write (tfile,'(a,a)') runpath(1:lnrp),
     &                          rzfil(1:lnblnk(rzfil))
          call vifile(irzfil,tfile,'b',ier)
        elseif(IW.eq.2)then
          write (tfile,'(a,a)') runpath(1:lnrp),
     &                          rzfil(1:lnblnk(rzfil))
          call vifile(irzfil,tfile,'e',ier)
        endif
      elseif(INO.EQ.(NITEMS-1))then

C Explains composition/geometry menu.
        CALL PHELPD('outside menu',30,'e2r_files   ',0,0,IER)
      elseif(INO.EQ.NITEMS)then
        if(outdone)then
          return
        else
          dok=.false.
          h(1)='The Radiance outside surfaces description file'
          h(2)='has not yet been generated. You might lose information'
          h(3)='if you exit before the file is generated. '
          CALL ASKOK(' Descriptions not yet been generated...',
     &      ' Is it ok to exit ?',OK,dok,3)
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
      SUBROUTINE mkriofil(ftr,fict,ACT_1,IER)
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "esprdbfile.h"
#include "e2r_common.h"
      
      integer lnblnk  ! function definition

      COMMON/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)

      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      COMMON/G6/SSNAME(MCON),SSOTF(MCON),SSMLCN(MCON),SSVFC(MCON),
     &          SSOTHER(MCON,3),SSPARENT(MCON),SSUSE(MCON,2)
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      COMMON/C24/IZSTOCN(MCOM,MS)

C TMC data
      COMMON/PRECTC/ITMCFL(MCOM,MS),TMCT(MCOM,MTMC,5),
     &       TMCA(MCOM,MTMC,ME,5),TMCREF(MCOM,MTMC),TVTR(MCOM,MTMC)
      COMMON/PRECT2/TMCT2(MCOM,MTMC,5,MBP),TMCA2(MCOM,MTMC,ME,5,MBP),
     &              TVTR2(MCOM,MTMC,MBP)
      COMMON/TMCB1/IBCMT(MCOM,MTMC)

      COMMON/GOPT/DG(5),HG(5),UVAL,VTRN,NTL,AB(ME,5),RF(ME),SRF,SAB

      character DESC*48
      integer NMLC,IPR,LAYERS
      real DTHK,DRAIR
      COMMON/MLC/NMLC,DESC(MMLC),DTHK(MMLC,ME),IPR(MMLC,ME),
     &           LAYERS(MMLC),DRAIR(MMLC,ME,3)
     
      common/grndpl/NGT,NGTV,XGT(MGTV),YGT(MGTV),ZGT(MGTV),JGVN(MGRT,8),
     &  NGVER(MGRT),IVEDGE(MGRT)

      COMMON/GT5/GSNAME(MGRT),GMLCN(MGRT)

      common/expath/runpath,pathtype

C Arrays of material properties.
      real matver
      integer matcats,matcatitems,matdbitems
      character matdbdate*24,matdbdoc*248,matcatname*32,matcatdoc*248
      common/matheader/matver,matcats,matdbitems,matcatitems(MGCL),
     &  matdbdate,matdbdoc,matcatname(MGCL),matcatdoc(MGCL)

      real matdbcon,matdbden,matdbsht,matdboute,matdbine
      real matdbouta,matdbina,matdbdrv,matdbthick
      common/matdatarray/matlegindex(MGIT),matcatindex(MGIT),
     &  matdbcon(MGIT),matdbden(MGIT),matdbsht(MGIT),matdboute(MGIT),
     &  matdbine(MGIT),matdbouta(MGIT),matdbina(MGIT),matdbdrv(MGIT),
     &  matdbthick(MGIT)

      COMMON/RAY3/MODIFY,MODLEN,MODBND
      LOGICAL MODIFY,MODLEN,MODBND

      COMMON/RAY6/LINSTY(MCON)

      integer mathash
      common/mathasharray/mathash(0:600)

      character matname*32,matdoc*248,matopaq*1
      common/matnamearray/matname(MGIT),matdoc(MGIT),matopaq(MGIT)

      integer legmat  ! local variable for legacy index.
      integer matarrayindex ! the indes within matdatarray
      logical closemat
      logical newgeo  ! to use for testing if new/old geometry file.

      DIMENSION ZSN(MS),iskip(MMLC),VP(3),EQN(4)
      DIMENSION SVALS(3),TVALS(3),TFORM(3)
      DIMENSION XX(MV),YY(MV),ZZ(MV),XT(MV),YT(MV),ZT(MV),TRNS(3)

      CHARACTER SSMLCN*32,SSVFC*4,SSOTF*32,SSOTHER*24,SSNAME*12,SSUSE*8
      character SSPARENT*12
      character ZSN*28
      character h*72,ZN*12,outs*124,material*12,QT*1
      character GDESCR*36,DESCRC*25
      CHARACTER NAM*72,TITL*72,MLCN*12,OTF*4,OPT*12
      CHARACTER GMLCN*12,GSNAME*6,ACT_1*1,doit*144
      character outsn*124,outsd*124, tfile*72, runpath*72,pathtype*24
      character pfile*72,atfile*72,ltmp72*72,dtmp72*72
      integer lnrp  ! length of run path
      integer lnbm  ! length of block material

      LOGICAL OK,DOK,fict,extern,XST,similar

      ITA1 = IFIL+6
      ITA2 = IFIL+7
      ITA3 = IFIL+10
      nobs=0
      ISTAT=0
      IER=0
      newgeo=.false.  ! assume older format geometry.
      lnrp=lnblnk(runpath)

C Find version of materials database.
      call eclose(matver,1.1,0.01,closemat)

C Create a glazing file, default type is 'glass' with user choice 
C to treat as 'mkillum'.
      call edisp(iuout,' creating glazing file. ')
      CALL ERPFREE(iglzfil,ISTAT)
      CALL ERPFREE(ita2,ISTAT)
      if(ACT_1.eq.'i')then
 10     H(1)=' Supply a local file name (i.e. without a path).'
        H(2)=' This file holds glazed surface details. '
        ltmp72=glzfil
        dtmp72=glzfil
        CALL EASKS(ltmp72,' Glazing file name ?',' ',
     &                             72,dtmp72,'glazing file name',IER,2)
        if(ltmp72(1:2).eq.'  ') goto 10
        glzfil=ltmp72
        write(tfile,'(a,a)')runpath(1:lnrp),glzfil(1:lnblnk(glzfil))
        call FPOPEN(iglzfil,ISTAT,1,4,tfile)

C Ask for alternative glazing file.
        if (NABS.gt.0) then
          dok=.true.
          h(1)='The model includes controls to optical properties '
          h(2)='and you have the option to create a Radiance model'
          h(3)='which includes the alternative optical state. '
          call ASKOK('Blind shutter control is active.',
     &      'Create model with blinds closed as well?',OK,dok,3)
          if (OK) then
            NBSRIF(ifocz,ISCENE)=NABS+1 ! number of alternative sets + base case
 12         H(1)=' Supply a local file name (i.e. without a path).'
            H(2)=' This file holds alternative glazed surface details,'
            H(3)=' e.g. representing closed blinds. '
            ltmp72=aglzfil
            dtmp72=aglzfil
            CALL EASKS(ltmp72,' Alternative glazing file name ?',' ',
     &                            72,dtmp72,'glazing file name',IER,3)
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
          NBSRIF(ifocz,ISCENE)=NABS+1
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
      if(ACT_1.eq.'i')then
 20     H(1)=' Supply a local file name (i.e. without a path).'
        H(2)=' This file holds surface properties for the opaque '
        H(3)=' surfaces, obstructions and ground definition.'
        ltmp72=matfil
        dtmp72=matfil
        CALL EASKS(ltmp72,' Opaque materials file name ?',' ',
     &                          72,dtmp72,'opaque mat file name',IER,3)
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

C Define a finished effect for surfaces.
      WRITE(imatfil,'(a)') 'void brightfunc fin_in'
      WRITE(imatfil,'(a)') '4 dirt dirt.cal -s 0.05'
      WRITE(imatfil,'(a)') '0 '
      WRITE(imatfil,'(a)') '1 0.05'
      WRITE(imatfil,'(a)') '  '
      WRITE(imatfil,'(a)') 'void texfunc fin_ex'
      WRITE(imatfil,'(a)') '6 cdx cdy cdz adobe.cal -s 0.05'
      WRITE(imatfil,'(a)') '0 '
      WRITE(imatfil,'(a)') '1 0.05'

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
      if(ACT_1.eq.'i')then
 22     H(1)=' Supply a local file name (i.e. without a path).'
        H(2)=' This file holds the definition of the zone.'
        ltmp72=rzfil
        dtmp72=rzfil
        CALL EASKS(ltmp72,' Inside compositon file name ?','  ',
     &                                72,dtmp72,'room file name',IER,2)
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
        WRITE(MLCN,'(A)')DESC(IM)(1:12) ! MLC construction name
        WRITE(OTF,'(A)') DESC(IM)(15:18)
        IF(OTF.EQ.' ')OTF='OPAQ'
        IF(OTF.EQ.'TRAN'.or.OTF.EQ.'CFC ')THEN
          WRITE(OPT,'(A)') DESC(IM)(21:32) ! "OPAQUE", "<optics data set name>" or "USE GSLedit"
          IF(OPT.EQ.' ')OPT='UNKNOWN'
        ELSE
          OPT='OPAQUE'
        ENDIF

C For the inside and outside layer get surface properties.
        if(closemat)then
          legmat=IPR(IM,1)   ! which legacy index
          if(legmat.gt.0)then
            matarrayindex=mathash(legmat)  ! find matching array index
            E=matdbine(matarrayindex)
            AE=matdbina(matarrayindex)
          endif
        else
          CALL ERPCDB(IFMAT,IPR(IM,1),iuout,IER,DBCON,DBDEN,
     &      DBSHT,E,AE,DRV,TITL,NAM)
        endif
        if(closemat)then
          legmat=IPR(IM,LAYERS(IM))   ! which legacy index
          if(legmat.gt.0)then
            matarrayindex=mathash(legmat)  ! find matching array index
            E=matdbine(matarrayindex)
            AI=matdbina(matarrayindex)
          endif
        else
          CALL ERPCDB(IFMAT,IPR(IM,LAYERS(IM)),iuout,IER,DBCON,DBDEN,
     &      DBSHT,E,AI,DRV,TITL,NAM)
        endif

C If a tmc then find tranmission functions.  If more transparent than
C the ficticious limit then mark to be skipped. Convert visible
C transmission to transmissivity (as per Ward 1/12/94 Radiance 2.4 paper).
C If TMC has an opaque layer then set gvt to a small number.
        if(OTF.EQ.'TRAN'.or.OTF.EQ.'CFC ')then
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
          WRITE(iglzfil,'(a,a)') 'void glass ',MLCN(1:lnblnk(MLCN))
          WRITE(iglzfil,'(a)') '0 '
          WRITE(iglzfil,'(a)') '0 '
          WRITE(iglzfil,'(a,3F6.2)') '3  ',gvt,gvt,gvt
          WRITE(iglzfil,'(a)') '  '

C Aternative glazing descriptions file.
          if (NABS.gt.0) then

C Check for blind shutter control.
            do 100 newfoc=1,ncomp
              call eclose(gversion(newfoc),1.1,0.01,newgeo)
              if(newgeo)then
                call georead(ita3,LGEOM(newfoc),newfoc,1,ITRU,IER)
              else
                call egomin(ita3,LGEOM(newfoc),newfoc,1,ITRC,ITRU,IER)
              endif
              do 111 isf=1,nsur
               ICN=IZSTOCN(newfoc,isf)
               if(ITMCFL(newfoc,isf).gt.0.AND.
     &            SSMLCN(icn)(1:12).eq.MLCN(1:12))then
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
            WRITE(ita2,'(3a)')'void glass ',MLCN(1:lnblnk(MLCN)),'_sw'
            WRITE(ita2,'(a)') '0 '
            WRITE(ita2,'(a)') '0 '
            WRITE(ita2,'(a,3F6.2)') '3  ',gvt,gvt,gvt
            WRITE(ita2,'(a)') '  '
          endif

C create a glass entry for each glass layer of CFC (".or." in line 866 would need to be removed!)
c        elseif (OTF.EQ.'CFC ')then
c          do ilay 1 to im_ncfc_el(izone,icfc)
c          
c            if (im_cfcltp(izone,icfc,ilay).eq.2) ! layer is blind
c            
c            endif
c            
c          enddo
C Blind control?
c          if (NABS.gt.0) then
c          
c          endif

        else ! Not a TMC or CFC ...
          WRITE(imatfil,'(a)') '# External MLC Colours... '
          if (SCENEPURP(ISCENE)(1:8).eq.'External'.or.
     &        SCENEPURP(ISCENE)(1:8).eq.'Internal') then
            WRITE(imatfil,'(a,a)') 'fin_ex plastic rc_ex_',
     &        MLCN(1:lnblnk(MLCN))
          else
            WRITE(imatfil,'(a,a)') 'void plastic rc_ex_',
     &        MLCN(1:lnblnk(MLCN))
          endif
          WRITE(imatfil,'(a)') '0 '
          WRITE(imatfil,'(a)') '0 '
          srefl=1.0-AE
          WRITE(imatfil,'(a,3F5.2,a)') '5 ',srefl,srefl,srefl,' 0 0 '
          WRITE(imatfil,'(a)') '  '

          WRITE(imatfil,'(a)') '# Internal MLC Colours... '
          if (SCENEPURP(ISCENE)(1:8).eq.'External'.or.
     &        SCENEPURP(ISCENE)(1:8).eq.'Internal') then
            WRITE(imatfil,'(a,a)') 'fin_in plastic rc_in_',
     &        MLCN(1:lnblnk(MLCN))
          else
            WRITE(imatfil,'(a,a)') 'void plastic rc_in_',
     &        MLCN(1:lnblnk(MLCN))
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
      if(ACT_1.eq.'i')then
 24     H(1)='This holds the definition of outside bodies.'
        ltmp72=rofil
        dtmp72=rofil
        CALL EASKS(ltmp72,' Outside description file name ?','  ',
     &                             72,dtmp72,'outside file name',IER,1)
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

C Do not use sky for Day_coef purpose.
      if (SCENEPURP(ISCENE)(1:8).ne.'Day_coef') then
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
     &                                 ' -0.01  0. 0. 1. 0. ',grdd*2.
      WRITE(irofil,'(a)') ' '
      if(ngt.gt.0)then

C If there is a ground topology defined then write this out
C to the outside file.
        WRITE(irofil,'(a)') '# Outside ground topology defs... '
        WRITE(irofil,'(a)') '  '
        do 775 isf=1,ngt

C Find the matching mlc index.
          ICF=-1
          DO 701 IC=1,NMLC
            IF(DESC(IC)(1:12).EQ.GMLCN(isf)(1:12))ICF=IC
 701      CONTINUE

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
        call eclose(gversion(newfoc),1.1,0.01,newgeo)
        if(ACT_1.eq.'i')then
          call usrmsg(' ',outs,'-')
          if(newgeo)then
            call georead(ITA1,LGEOM(newfoc),newfoc,1,IUOUT,IER)
          else
            call egomin(ITA1,LGEOM(newfoc),newfoc,1,ITRC,IUOUT,IER)
          endif
        else
          if(newgeo)then
            call georead(ITA1,LGEOM(newfoc),newfoc,1,IUOUT,IER)
          else
            call egomin(ITA1,LGEOM(newfoc),newfoc,1,0,IUOUT,IER)
          endif
        endif
        call FINDFIL(LTHRM(newfoc),XST)
        if(XST)then
          CALL ECONST(LTHRM(newfoc),ITA1,newfoc,0,IUOUT,IER)
          if (LTWIN(newfoc)(1:2).ne.'  '.and.
     &                              LTWIN(newfoc)(1:4).ne.'UNKN') then
            CALL ERTWIN(0,IUOUT,ITA1,LTWIN(newfoc),newfoc,IER)
          endif
        endif
        ZN=zname(newfoc)

C Display the other zone, resetting all surface lines to std.
        if(ACT_1.eq.'i')then
          MODIFY=.TRUE.
          MODLEN=.TRUE.
          MODBND=.TRUE.
          CALL INLNST(1)
          nzg=1
          nznog(1)=newfoc
          izgfoc=newfoc
          CALL ADJVIEW(IER)
        endif

C Generate combined zone & generate surface names as identifiers.
        DO 60 IS=1,NSUR
          ICN=IZSTOCN(newfoc,is)
          write(zsn(is),'(3a)') ZN(1:lnblnk(ZN)),':',SSNAME(icn)
  60    continue

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
          if(ACT_1.eq.'i')then
            write(outs,'(3a)') ' Include ',ZN(1:lnblnk(ZN)),
     &                            ' obstructions ? '
            dok=.true.
            h(1)='The model includes solar obstructions in this '
            h(2)='zone and these can be included in the Radiance '
            h(3)='model if you answer yes. '
            CALL ASKOK(' ',outs,OK,dok,3)
          else
            OK=.TRUE.
          endif
          if(.not.OK)goto 61


          WRITE(irofil,'(a)') '# Obstruction defs... '
          WRITE(irofil,'(a)') '  '
          nobs=nobs+nbobs(newfoc)  ! increment count of obstructions
          do 43 ib=1,nbobs(newfoc)

C Create a genbox command to match the obstruction.

C << to be done - find out syntax for blocks with multiple rotations >>

            if(BLOCKMAT(newfoc,ib)(1:4).eq.'NONE')then
              WRITE(irofil,'(a,a,3f9.3,a,f9.3,a,3f9.3)')
     &          '!genbox rc_ex_unknown ',
     &          BLOCKNAME(newfoc,IB)(1:LNBLOCKNAME(newfoc,IB)),
     &          DXOB(newfoc,ib),DYOB(newfoc,ib),DZOB(newfoc,ib),
     &          ' | xform -rz ',BANGOB(newfoc,ib,1),
     &          ' -t ',XOB(newfoc,ib),YOB(newfoc,ib),ZOB(newfoc,ib)
              WRITE(irofil,'(a)') ' '
            else
              lnbm=LNBLOCKMAT(newfoc,ib)
              WRITE(irofil,'(a,a,a,a,3f9.3,a,f9.3,a,3f9.3)')
     &          '!genbox  rc_ex_',BLOCKMAT(newfoc,ib)(1:lnbm),
     &          '  ',        
     &          BLOCKNAME(newfoc,IB)(1:LNBLOCKNAME(newfoc,IB)),
     &          DXOB(newfoc,ib),DYOB(newfoc,ib),DZOB(newfoc,ib),
     &          ' | xform -rz ',BANGOB(newfoc,ib,1),
     &          ' -t ',XOB(newfoc,ib),YOB(newfoc,ib),ZOB(newfoc,ib)
              WRITE(irofil,'(a)') ' '
            endif
 43       continue
        endif
  61    continue

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
          ICF=-1
          ifict=0
          DO 201 IC=1,NMLC
            IF(DESC(IC)(1:12).EQ.SSMLCN(icn)(1:12))ICF=IC
 201      CONTINUE
          if(ICF.eq.-1.or.SSMLCN(icn)(1:4).eq.'UNKN')then
            if (SSOTF(icn)(1:4).ne.'OPAQ') then
C     &            .and.SSOTF(icn)(1:4).ne.'CFC ') then
              material='unknown_glz'
            else
              material='unknown'
            endif
          else
            write(material,'(a)') SSMLCN(icn)(1:12)
            if(iskip(icf).eq.1)ifict=1
          endif

C Depending on surface type (fict/tran/opaq) write header info.  
          if(ifict.eq.1)then
C          if(iskip(icf).eq.1)then
            WRITE(outs,'(a,a)') '# skipping fict surface: ',zsn(isf)
            if(ACT_1.eq.'i')call edisp(iuout,outs)
            WRITE(iglzfil,'(a)') outs
            WRITE(iglzfil,'(a)') '  '

C Aternative glazing descriptions file.
            if (NABS.gt.0) then
              WRITE(ita2,'(a)') outs
              WRITE(ita2,'(a)') '  '
            endif
          elseif(SSOTF(icn)(1:4).ne.'OPAQ') then
C     &            .and.SSOTF(icn)(1:4).ne.'CFC ')then

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
                if(ACT_1.eq.'i')then
                  MODIFY=.TRUE.
                  CALL INLNST(1)
                  CALL SURADJ(ifocz,isf,IE,TMP,IZC,ISC,IC,DESCRC)
                  LINSTY(IC)=2
                  CALL ADJVIEW(IER)

                  H(1)='Large sources of indirect illumination should'
                  H(2)='have their distributions pre-calculated as  '
                  H(3)='this optimises the rendering process.  '
                  H(4)='(Note: the pre-calculation is achieved   '
                  H(5)='  via the Radiance utility mkillum.)  '
                  write (outs,'(a,a,a)') 'Surface ',
     &                   zsn(isf)(1:lnblnk(zsn(isf))),' is transparent.'
                 call EASKAB(outs,'Pre-calculate indirect illumination',
     &                                     'no (default)','yes',IANS,5)
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
     &               zsn(isf)(1:lnblnk(zsn(isf))),' can be subdivided.'
                call EASKAB(outs,'Create ','one surface (default)',
     &                                      'multiple surfaces',IDIV,0)
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
     &                    material(1:lnblnk(material)),
     &                    zsn(isf)(1:lnblnk(zsn(isf))),
     &                    QT,SVALS(1),'*s+',TVALS(1),'*t',QT,
     &                    QT,SVALS(2),'*s+',TVALS(2),'*t',QT,
     &                    QT,SVALS(3),'*s+',TVALS(3),'*t',QT,' 2  2',
     &                    ' | xform -t ',(TFORM(IXX),IXX=1,3)
                    if (NABS.gt.0) then
                      write(outs,'(a,a)') 
     &                               material(1:lnblnk(material)),'_sw'
                      WRITE(ita2,666)'!gensurf',outs(1:lnblnk(outs)),
     &                    zsn(isf)(1:lnblnk(zsn(isf))),
     &                    QT,SVALS(1),'*s+',TVALS(1),'*t',QT,
     &                    QT,SVALS(2),'*s+',TVALS(2),'*t',QT,
     &                    QT,SVALS(3),'*s+',TVALS(3),'*t',QT,' 2  2',
     &                    ' | xform -t ',(TFORM(IXX),IXX=1,3)
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
                  WRITE(iglzfil,'(a,a,a)')material(1:lnblnk(material)),
     &                        '  polygon  ',zsn(isf)(1:lnblnk(zsn(isf)))
                  WRITE(iglzfil,'(a)') '0 '
                  WRITE(iglzfil,'(a)') '0 '
                  WRITE(iglzfil,'(I3)')NVER(isf)*3
                  do 1274 iv=NVER(isf),1,-1
                    WRITE(iglzfil,'(3F11.5)')
     &                     X(JVN(isf,iv)),Y(JVN(isf,iv)),Z(JVN(isf,iv))
 1274             continue
                  if (NABS.gt.0) then
                    WRITE(ita2,'(a,a,a)')material(1:lnblnk(material)),
     &                     '_sw  polygon  ',zsn(isf)(1:lnblnk(zsn(isf)))
                    WRITE(ita2,'(a)') '0 '
                    WRITE(ita2,'(a)') '0 '
                    WRITE(ita2,'(I3)')NVER(isf)*3
                    do 1275 iv=NVER(isf),1,-1
                      WRITE(ita2,'(3F11.5)')
     &                     X(JVN(isf,iv)),Y(JVN(isf,iv)),Z(JVN(isf,iv))
 1275               continue
                  endif
                endif
              else ! surface is/was not divided
                WRITE(iglzfil,'(a,a,a)') material(1:lnblnk(material)),
     &                        '  polygon  ',zsn(isf)(1:lnblnk(zsn(isf)))
                WRITE(iglzfil,'(a)') '0 '
                WRITE(iglzfil,'(a)') '0 '
                WRITE(iglzfil,'(I3)')NVER(isf)*3
                do 1174 iv=NVER(isf),1,-1
                  WRITE(iglzfil,'(3F11.5)')
     &                     X(JVN(isf,iv)),Y(JVN(isf,iv)),Z(JVN(isf,iv))
 1174           continue
                if (NABS.gt.0) then
                  WRITE(ita2,'(a,a,a)') material(1:lnblnk(material)),
     &                     '_sw  polygon  ',zsn(isf)(1:lnblnk(zsn(isf)))
                  WRITE(ita2,'(a)') '0 '
                  WRITE(ita2,'(a)') '0 '
                  WRITE(ita2,'(I3)')NVER(isf)*3
                  do 1175 iv=NVER(isf),1,-1
                    WRITE(ita2,'(3F11.5)')
     &                     X(JVN(isf,iv)),Y(JVN(isf,iv)),Z(JVN(isf,iv))
 1175             continue
                endif
              endif
            else
              WRITE(outs,'(a,a)')'# skipping transparent surface: ',
     &                                                         zsn(isf)
              if(ACT_1.eq.'i')call edisp(iuout,outs)
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
            WRITE(irzfil,'(a,a,a,a)')'rc_in_',
     &                      material(1:lnblnk(material)),'  polygon  ',
     &                                      zsn(isf)(1:lnblnk(zsn(isf)))
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
              WRITE(irofil,'(a,a,a,a)')'rc_ex_',
     &                      material(1:lnblnk(material)),'  polygon  ',
     &                                      zsn(isf)(1:lnblnk(zsn(isf)))
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

C If there are more than 6 obstructions it is worth expanding
C the outside definition via an 'xform -e' command to speed
C calculation.
      if(nobs.gt.6)then
        write(outs,'(a,i3,a)') 'expanding ',nobs,' obstructions...'
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
