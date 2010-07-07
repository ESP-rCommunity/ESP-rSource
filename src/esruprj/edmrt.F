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

C ************* EDMRT 
C Edit MRT attributes via a paging menu.
C ITRU = unit number for user output, IER=0 OK, IER=1 problem.
C If silent is true then called from either a global facility
C or from version facility. A file with area weighted view factors
C will be created if act = 'a' and if act = 'v' then ray tracing
C calculations will be commissioned. If silent the viewfactor file name
C will have been defined prior to calling edmrt.
      SUBROUTINE EDMRT(ITRC,ITRU,IUF,ICOMP,silent,act,IER)
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "epara.h"
      
      integer lnblnk  ! function definition

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN
      common/user/browse
      common/CFGV/icfgv

      common/pophelp/h(60)

      COMMON/G6/SSNAME(MCON),SSOTF(MCON),SSMLCN(MCON),SSVFC(MCON),
     &          SSOTHER(MCON,3),SSPARENT(MCON),SSUSE(MCON,2)

      common/C21/IFCFG,cfgroot,LCFGF
      COMMON/C24/IZSTOCN(MCOM,MS)
      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)
      COMMON/RAY3/MODIFY,MODLEN,MODBND
      COMMON/RAY5/ZCOG(MCOM,3),XMN,YMN,ZMN,XMX,YMX,ZMX
      COMMON/MRTC/NCUB,XOC(MCUB),YOC(MCUB),ZOC(MCUB),DXC(MCUB),
     &            DYC(MCUB),DZC(MCUB),CANG(MCUB),IVFOK(MCUB),CUBN(MCUB)
      COMMON/MRTF/FFL(MCUB,6,MSM),VEWF(MS,MS)
      common/paths/zonepth,netpth,ctlpth,imgpth,radpth,docpth,tmppth,
     &             dbspth
      common/appw/iappw,iappx,iappy

      DIMENSION VERT(35)
      DIMENSION SNA(MS),PAZI(MS),PELV(MS)
      LOGICAL MODIFY,MODLEN,MODBND,concat,cfgmod,browse,clkok,XST
      logical silent,unixok
      logical newgeo  ! to use for testing if new/old geometry file.
      CHARACTER VERT*46,H*72,CUBN*6,LTMP*72,cfgroot*24
      CHARACTER LCFGF*72
      character VFILE*72,head*32,outs*124,KEY*1,ETEXT*82
      CHARACTER SSMLCN*32,SSVFC*4,SSOTF*32,SSOTHER*24,SSNAME*12,SSUSE*8
      character SSPARENT*12
      character doit*248,tmode*8,tfile*72,ZN*12
      character zonepth*24,netpth*24,ctlpth*24,imgpth*24,radpth*24
      character docpth*24,tmppth*24,dbspth*24,act*1
      character longtfile*144,longtfiledos*144
      integer icni,icnj  ! connection numbers for pair of surfaces.
#ifdef OSI
      integer iside,isize,ifont     ! passed to viewtext
      integer ICPMOD                ! passed from tchild
#else
      integer*8 iside,isize,ifont   ! passed to viewtext
      integer*8 ICPMOD              ! passed from tchild
#endif

      cfgmod=.false.
      newgeo=.false.  ! assume older format geometry.

C Check if Unix-based or DOS based.
      call isunix(unixok)

C If user is browsing model then give warning and exit.
      if(browse)then
        call usrmsg(
     &  'You do not have permission to increase the resolution of this',
     &  'model. To calculate viewfactors you must own the model.','W')
        return
      endif

C Initialize the default viewfactors and MRT sensor file name and the 
C matching geometry file.
      if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
        WRITE(VFILE,'(2a)')zname(ICOMP)(1:lnzname(ICOMP)),'.vwf'
      else
        WRITE(VFILE,'(4a)') zonepth(1:lnblnk(zonepth)),'/',
     &        zname(ICOMP)(1:lnzname(ICOMP)),'.vwf'
      endif
 
      write(head,'(2A)')'  Viewfactors in ',zname(ICOMP)
      write(ZN,'(A)') zname(ICOMP)

C Set VEWF array in preparation for editing or silent creation.
      call eclose(gversion(icomp),1.1,0.01,newgeo)
      if(newgeo)then
        call georead(IUF,LGEOM(ICOMP),ICOMP,1,IUOUT,IER)
      else
        call egomin(IUF,LGEOM(ICOMP),ICOMP,1,ITRC,IUOUT,IER)
      endif

C << update logic to use alternative source for ZOA rather than call zinfo >>
      CALL ZINFO(SNA,ZOA,PAZI,PELV,ZVOL)
      NS=NZSUR(ICOMP)
      DO 111 I=1,NS
        II=I
        DO 201 J=1,NS
          JJ=J
          icc=izstocn(icomp,ii)
          jcc=izstocn(icomp,jj)
          VEWF(II,JJ)=0.0
          IF(II.EQ.JJ)goto 201
          VEWF(II,JJ)=(SSNA(jcc)/(ZOA-SSNA(icc)))
  201   CONTINUE
  111 CONTINUE
      if(silent)then
        if(IVF(ICOMP).eq.1)then
          call FINDFIL(LVIEW(ICOMP),XST)
          if(XST)then
            call edisp(iuout,' Opened existing viewfactor file.')
            CALL ERMRT(ITRC,ITRU,IUF,LVIEW(ICOMP),ICOMP,IER)
          else
            VFILE=LVIEW(ICOMP)
            CALL usrmsg(
     &        'Creating initial viewfactor file...',vfile,'-')
            CALL EMKMRT(LVIEW(ICOMP),LGEOM(ICOMP),NZSUR(ICOMP),IUF,
     &        ICOMP,'a',IER)
            if(ier.ne.0)then
              call usrmsg(
     &  'Problem detected while creating viewfactor data file.',
     &  'Check that you have adequate permissions and disk space.','W')
              IVF(ICOMP)=0
              return
            else
              IVF(ICOMP)=1
            endif
          endif
        elseif(IVF(ICOMP).eq.0)then
          VFILE=LVIEW(ICOMP)
          CALL usrmsg('Creating initial viewfactor file...',vfile,'-')
          CALL EMKMRT(LVIEW(ICOMP),LGEOM(ICOMP),NZSUR(ICOMP),IUF,
     &      ICOMP,'a',IER)
          if(ier.ne.0)then
            call usrmsg(
     &  'Problem detected while creating viewfactor data file.',
     &  'Check that you have adequate permissions and disk space.','W')
            IVF(ICOMP)=0
            return
          else
            CALL usrmsg(
     &        'Creating initial viewfactor file...done.',' ','-')
            IVF(ICOMP)=1
          endif
        endif

C If act = 'a' then return.
        if(act.eq.'a')then
          return
        else

C Otherwise commission an explicit calculation.

C Get logical name of terminal type, expand model name
C to include the path and create a string to drive espvwf module.
          doit = ' '
          call tchild(ICPMOD)
          call termode(ICPMOD,tmode)
          if(unixok)then
            call addpath(LCFGF,longtfile,concat)
          else

C If running on a non-unix machine see if there are spaces in the name
C and change any / to \.
            call addpath(LCFGF,longtfile,concat)
            call cmdfiledos(longtfile,longtfiledos,ier)
            longtfile=' '
            longtfile=longtfiledos
          endif

C If prj initial size is a % of default pass this on to child with
C an offset from prj start position.
          if(iappw.gt.0.and.iappw.le.200)then
            write(doit,'(3a,3i4,4a)') 'espvwf -mode ',tmode,
     &        ' -s ',iappw,iappx+20,iappy+30,' -file ',
     &        longtfile(:lnblnk(longtfile)),' -zone ',ZN(1:lnblnk(ZN))
          else
            write(doit,'(6a)') 'espvwf -mode ',tmode,
     &        ' -s 0 0 0 -file ',longtfile(1:lnblnk(longtfile)),
     &        ' -zone ',ZN(1:lnblnk(ZN))
          endif
          call usrmsg('starting espvwf module via',doit,'-')
          call runit(doit,tmode)
          call usrmsg(' Reading viewfactor file....',' ','-')
          call pauses(1)
          CALL ERMRT(ITRC,ITRU,IUF,LVIEW(ICOMP),ICOMP,IER)
          return
        endif
      endif

C Check and see if a viewfactor file exists.
C Viewfactor file exists, offer option to dereference it. Otherwise
C read it or create one with default (area weighted) viewfactors
C and no MRT sensors.
      if(IVF(ICOMP).eq.1)then
        H(1)='The zone viewfactor file contains data from the'
        H(2)='ESRU espvwf program which allows a more robust treatment'
        H(3)='of longwave radiation within a zone.'
        H(4)=' '
        H(5)='If you want to ignore the existing viewfactors for this'
        H(6)='zone click on dereference. '
        LTMP=LVIEW(ICOMP)
        clkok=.false.
   46   CALL EASKSCMD(LTMP,'Zone viewfactor file? ',' ',
     &    'dereference',clkok,72,VFILE,'viewfactor file',IER,6)
        if(clkok)then
          LVIEW(ICOMP)='UNKNOWN'
          IVF(ICOMP)=0
          call usrmsg(' updating model to remove mrt...',' ','-')
          CALL EMKCFG('-',IER)
          cfgmod=.false.
          call usrmsg(' updating model to remove mrt...done',' ','-')
          return
        else
          IF(IER.NE.0)GOTO 46
          IF(LTMP(1:2).NE.'  ')THEN
            LVIEW(ICOMP)=LTMP
            IVF(ICOMP)=1
          ELSE
            goto 46
          ENDIF
        endif
        call edisp(iuout,' Opened existing viewfactor file.')
        CALL ERMRT(ITRC,ITRU,IUF,LVIEW(ICOMP),ICOMP,IER)
      else

C No mrt file referenced so offer default name as well as an option
C to back out of the facility.
        H(1)='The zone viewfactor file contains data from the'
        H(2)='ESRU vwf program which allows a more robust treatment'
        H(3)='of longwave radiation within a zone.'
        H(4)=' '
        H(5)='If you want to ignore the existing viewfactors for this'
        H(6)='zone click on dereference. '
        LTMP=VFILE
        clkok=.false.
   47   CALL EASKSCMD(LTMP,'Zone viewfactor file? ',' ',
     &    'dereference',clkok,72,VFILE,'viewfactor file',IER,6)
        if(clkok)then
          return
        else
          IF(LTMP(1:2).NE.'  ')THEN
            LVIEW(ICOMP)=LTMP
            IVF(ICOMP)=1
          ELSE
            goto 47
          ENDIF

C Check just in case the file exists (or was previously dereferenced).
          call FINDFIL(LVIEW(ICOMP),XST)
          IF(XST)THEN
            call edisp(iuout,' Opened existing viewfactor file.')
            CALL ERMRT(ITRC,ITRU,IUF,LVIEW(ICOMP),ICOMP,IER)
            cfgmod=.true.
          else
            NCUB=0
            CALL usrmsg('Creating area weighted viewfactor file...',
     &        ' ','-')
            CALL EMKMRT(LVIEW(ICOMP),LGEOM(ICOMP),NZSUR(ICOMP),IUF,
     &        ICOMP,'a',IER)
            if(ier.ne.0)then
              call usrmsg(
     &  'Problem detected while creating viewfactor data file.',
     &  'Check that you have adequate permissions and disk space.','W')
              IVF(ICOMP)=0
              return
            endif
            CALL usrmsg(
     &        'Creating area weighted viewfactor file...done.',
     &        ' ','-')
            cfgmod=.true.
          endif
        endif
      endif
      if(cfgmod)then
        call usrmsg(' updating model to include mrt...',' ','-')
        CALL EMKCFG('-',IER)
        cfgmod=.false.
        call usrmsg(' updating model to include mrt...done',' ','-')
      endif

C On entry refresh geometry, determine its bounds, force it
C drawn and remember number of surfaces to pass to sensor file.
      NZS=NZSUR(icomp)
      nzg=1
      nznog(1)=ICOMP
      izgfoc=ICOMP
      MODIFY=.TRUE.
      MODLEN=.TRUE.
      MODBND=.TRUE.
      CALL ESCZONE(ICOMP)
      CALL BNDOBJ(0,IER)
      CALL ERCZONE(ICOMP)

C Initialise menu size variables based on window size. 
C IVERT is the menu position, MVERT the current no of lines.
      MHEAD=9
      MCTL=6
      ILEN=NCUB
      IPACT=CREATE
      CALL EKPAGE(IPACT)

C Initial menu entry setup.
   92 IER=0
      ILEN=NCUB
      IVERT=-3

C Loop through the items until the page to be displayed. M is the 
C current menu line index. Build up text strings for the menu. 
    3 M=MHEAD
      WRITE(VERT(1),'(A,A)')  '  Viewfactor file: ',LVIEW(ICOMP)(1:26)
      VERT(2)='  ___________________________________________'
      VERT(3)='a Edit zone viewfactors                      '
      VERT(4)='b Calculate zone or MRT sensor viewfactors   '
      VERT(5)='  ___________________________________________'
      WRITE(VERT(6),'(A,I2)') '  No of MRT sensors: ',NCUB
      VERT(7)='  ___________________________________________'
      VERT(8)='  Sensor|   Origin (m)    |    Size (m)  |Rot'
      VERT(9)='  name  |Xcord|Ycord|Zcord|len |wid |ht  |deg'

C Loop through the items until the page to be displayed. M is the 
C current menu line index. Build up text strings for the menu. 
      M=MHEAD
      DO 10 L=1,ILEN
        IF(L.GE.IST.AND.(L.LE.(IST+MIFULL)))THEN
          M=M+1
          CALL EMKEY(M,KEY,IER)
          WRITE(VERT(M),303)KEY,CUBN(L),XOC(L),YOC(L),ZOC(L),
     &                      DXC(L),DYC(L),DZC(L),CANG(L)
  303     FORMAT(A1,1X,A6,3F6.1,3F5.2,F5.0)
        ENDIF
   10 CONTINUE

C Number of actual items displayed.
      MVERT=M+MCTL

C If a long list include page facility text.      
      IF(IPFLG.EQ.0)THEN
        VERT(M+1)='  ___________________________________________'
      ELSE
        WRITE(VERT(M+1),15)IPM,MPM 
   15   FORMAT   ('0 Page --- Part: ',I2,' of ',I2,' ---')
      ENDIF
      VERT(M+2)  ='+ Add a MRT sensor                 '
      VERT(M+3)  ='> Save MRT sensors & viewfactors '
      VERT(M+4)  ='! List zone viewfactors            '
      VERT(M+5)  ='? Help                             '
      VERT(M+6)  ='- Exit to model definition         '

C If editing the MRT sensors and user has asked for an 
C update of the image then use common block info for 
C the zone, obstructions and sensors.
      nzg=1
      nznog(1)=ICOMP
      izgfoc=ICOMP
      CALL ADJVIEW(IER)
      call cfgtogg(icfg_type,icfgz,icfgn,icfgc,icfgdfn,
     &                             iicfgz,iicfgn,iicfgc,iicfgdfn)
      if(MMOD.EQ.8)then
        call opencfg(icfg_type,icfgz,icfgn,icfgc,icfgdfn,
     &                             iicfgz,iicfgn,iicfgc,iicfgdfn)
        WRITE(etext,'(2A)')'Model: ',modeltitle(1:lnblnk(modeltitle))
        iside=1
        isize=1
        ifont=1
        call viewtext(etext,iside,isize,ifont)
      endif
      CALL ESCZONE(ICOMP)
      CALL DRWSEN(ier)
      CALL ERCZONE(ICOMP)

C Help text for this menu.
      H(1) ='This menu allows zone viewfactors or MRT sensors to'
      H(2) ='be viewed and edited.  These are then passed to the '
      H(3) ='program `espvwf` for further processing if a robust '
      H(4) ='evaluation of view factors or position dependent '
      H(5) ='mean radiant temperatures is required. '
      H(6) =' '
      H(7) ='View factors may also be created from scratch in'
      H(8) ='this section of prj.  Note that the default treatment'
      H(9) ='is to have area weighted viewfactors which do not'
      H(10) ='require the creation of a view factor file.'
      H(11) =' '
      H(12) ='MRT sensors are rectangular bodies similar to those'
      H(13) ='used for zone obstructions. The default size is a'
      H(14) ='100mm cube.  They are placed within a zone to find '
      H(15) ='the view-factor weighted mean radiant temperature. '
      H(16) =' '
      H(17)='If you wish you may alter the dimension of the sensor'
      H(18)='to approximate an occupant or other object. '

C Now display the menu.
      CALL EMENU(head,VERT,MVERT,IVERT)
      IF(IVERT.EQ.MVERT)THEN

C Exit to main routine.
        RETURN
      ELSEIF(IVERT.EQ.(MVERT-1))THEN

C List help text for the  menu.
        CALL PHELPD('mrt sensor',18,'-',0,0,IER)
      ELSEIF(IVERT.EQ.(MVERT-2))THEN

C List viewfactor contents.
        call edisp(itru,' ')
        WRITE(outs,'(A,I2)')' Number of surfaces = ',NZS
        call edisp(itru,outs)
        call edisp(itru,' ')
        call edisp(itru,' View Factors from Surface to Surface')
        DO 860 I=1,NZS
          DO 850 J=1,NZS
            ICNI=IZSTOCN(ICOMP,I)
            ICNJ=IZSTOCN(ICOMP,J)
            WRITE(outs,5203)SSNAME(icni),I,SSNAME(icnj),J,VEWF(I,J)
 5203       FORMAT(10X,A12,I3,'  to ',A12,I3,' = ',F6.4)
            call edisp(itru,outs)
  850     CONTINUE
  860   CONTINUE
      ELSEIF(IVERT.EQ.(MVERT-3))THEN

C Save viewfactor & sensor file...
        H(1)='The zone viewfactor file contains data from the'
        H(2)='ESRU vwf program which allows a more robust treatment'
        H(3)='of longwave radiation within a zone.'
        LTMP=LVIEW(ICOMP)
   48   CALL EASKS(LTMP,' Zone viewfactor file? ',
     &    ' ',72,VFILE,'viewfactor file',IER,3)
        IF(IER.NE.0)GOTO 48
        IF(LTMP(1:2).NE.'  ')THEN
          LVIEW(ICOMP)=LTMP
          IVF(ICOMP)=1
        ELSE
          IVF(ICOMP)=0
          goto 48
        ENDIF
        CALL usrmsg(' Updating the viewfactor file...',' ','-')
        CALL EMKMRT(LVIEW(ICOMP),LGEOM(ICOMP),NZS,IUF,ICOMP,'v',IER)
        CALL usrmsg(' Updating the viewfactor file...done.',' ','-')
      ELSEIF(IVERT.EQ.(MVERT-4))THEN

C Add a sensor.
        IF(NCUB+1.LE.MCUB)THEN
          NCUB=NCUB+1
          XOC(NCUB)=ZCOG(ICOMP,1)
          YOC(NCUB)=ZCOG(ICOMP,2)
          ZOC(NCUB)=ZCOG(ICOMP,3)
          DXC(NCUB)=0.1
          DYC(NCUB)=0.1
          DZC(NCUB)=0.1
          CANG(NCUB)=0.
          write(CUBN(NCUB),'(A4,I1)')'sen_',NCUB
          MODIFY=.TRUE.
        ELSE
          CALL USRMSG(' ',' Too many sensors defined!','W')
        ENDIF
      ELSEIF(IVERT.EQ.(MVERT-5))THEN

C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        ENDIF
      ELSEIF(IVERT.EQ.3)THEN

C Manual editing of viewfactors.
        A=0.0
        IS=1
        CALL EPMENSV
        CALL EASKSUR(ICOMP,IS,'-',
     &    'Select surface for viewfactor editing.',' ',IER)
  31    do 33 MM=1,NZS
          if(MM.NE.IS)then
            ICNI=IZSTOCN(ICOMP,IS)
            ICNJ=IZSTOCN(ICOMP,MM)
            write(outs,'(A,A15,A,A15)') ' For ',SSNAME(ICNI),' to ',
     &                                   SSNAME(ICNJ)
            V=VEWF(IS,MM)
            CALL EASKR(V,outs,' view factor: ',
     &        0.,'F',0.999,'F',0.,'viewfactors',IER,1)
            VEWF(IS,MM)=V
          else
            VEWF(IS,IS)=0.
          endif
          A=A+VEWF(IS,MM)
  33    continue
        IF(ABS(A).LT.0.9888.OR.ABS(A).GT.1.002)then
          write(outs,'(A,F8.5)')' currently the total is ',A,
     &                          ' please re-edit...'
          call usrmsg(
     &    ' The sum of all viewfactors to this surface must equal 1.0',
     &    outs,'W')
          goto 31
        endif
        CALL EPMENRC
      ELSEIF(IVERT.EQ.4)THEN

C View factor calculations - offer choice of default viewfactors or
C call to ESPVWF. Recover information on areas in the current zone for
C use in area weighted computation.
        h(1)='The ray-tracing calcs will start a calculation module.'
        call easkabc(' Zone viewfactor calculations can be either',
     &    ':','Area weighted','Based on ray-tracing calcs','continue',
     &    IW,1)
        if(IW.eq.1)then

C << Rewrite logic to use G7 common blocks rather than call zinfo. >>

          CALL ZINFO(SNA,ZOA,PAZI,PELV,ZVOL)
          NS=NZSUR(icomp)
          DO 11 I=1,NS
            II=I
            DO 20 J=1,NS
              JJ=J
              icc=izstocn(icomp,ii)
              jcc=izstocn(icomp,jj)
              VEWF(II,JJ)=0.0
              IF(II.EQ.JJ)goto 20
              VEWF(II,JJ)=(SSNA(jcc)/(ZOA-SSNA(icc)))
   20       CONTINUE
   11     CONTINUE
        elseif(IW.eq.2)then

C Get logical name of terminal type, expand model name
C to include the path and create a string to drive espvwf.
          doit = ' '
          call tchild(ICPMOD)
          call termode(ICPMOD,tmode)
          if(unixok)then
            call addpath(LCFGF,longtfile,concat)
          else

C If running on a non-unix machine see if there are spaces in the name
C and change any / to \.
            call addpath(LCFGF,longtfile,concat)
            call cmdfiledos(longtfile,longtfiledos,ier)
            longtfile=' '
            longtfile=longtfiledos
          endif

C If prj initial size is a % of default pass this on to child with
C an offset from prj start position.
          if(iappw.gt.0.and.iappw.le.200)then
            write(doit,'(3a,3i4,5a)') 'espvwf -mode ',tmode,
     &        ' -s ',iappw,iappx+20,iappy+30,' -file ',
     &        longtfile(:lnblnk(longtfile)),' -zone ',ZN(1:lnblnk(ZN)),
     &        ' &'
          else
            write(doit,'(7a)') 'espvwf -mode ',tmode,
     &        ' -s 0 0 0 -file ',longtfile(:lnblnk(longtfile)),
     &        ' -zone ',ZN(1:lnblnk(ZN)),' &'
          endif
          call usrmsg('starting espvwf module via',doit,'-')
          call runit(doit,tmode)

          H(1)='When the computation is completed you will be '
          H(2)='asked whether you want to make use of this data.'
          H(3)='The computation may take some time... do not '
          H(4)='attempt to use the data until it is finished. '
          CALL PHELPD('espvwf running',4,'-',0,0,IER)
          h(1)='The previous pop-up message applies.'
          CALL EASKABC(' Post calculation choices :',' ',
     &      'use new data','revert (no viewfactors)','continue',irt,1)
          if(irt.eq.1)then
            call edisp(iuout,' Reading viewfactor file....')
            CALL ERMRT(ITRC,ITRU,IUF,LVIEW(ICOMP),ICOMP,IER)
            goto 92
          elseif(irt.eq.2)then
            IVF(ICOMP)=0
            CALL ERPFREE(IUF,ISTAT)
            CALL EFOPSEQ(IUF,LVIEW(ICOMP),1,IER)
            if(ier.eq.0)then
              write(outs,'(A,A)') ' removing ',LVIEW(ICOMP)
              call edisp(itru,outs)
              CALL EFDELET(IUF,ISTAT)
            endif
            CALL EMKCFG('-',IER)
          endif
        endif
      ELSEIF(IVERT.GT.MHEAD.AND.IVERT.LT.(MVERT-MCTL+1))THEN

C Edit item identified by KEYIND.
        CALL KEYIND(MVERT,IVERT,IFOC,IO)
        ICUB=IFOC
        CALL EDMRTS(ICOMP,ICUB,IER)
      ELSE

C Not one of the legal menu choices.
        IVERT=-1
        GOTO 3
      ENDIF
      GOTO 92

      END 


C ************* EDMRTS
C EDMRTS: Edit details of a MRT sensor block.
      SUBROUTINE EDMRTS(ICOMP,ICUB,IER)
#include "building.h"
#include "model.h"
#include "geometry.h"
      
      integer lnblnk  ! function definition

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)

      COMMON/MRTC/NCUB,XOC(MCUB),YOC(MCUB),ZOC(MCUB),DXC(MCUB),
     &            DYC(MCUB),DZC(MCUB),CANG(MCUB),IVFOK(MCUB),CUBN(MCUB)

      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)
      COMMON/RAY3/MODIFY,MODLEN,MODBND
      DIMENSION VERT(15)
      CHARACTER VERT*28,MSG*72,S6*6,CUBN*6,head*30
      character H*72,ETEXT*82
      LOGICAL MODIFY,MODLEN,MODBND
#ifdef OSI
      integer iside,isize,ifont     ! passed to viewtext
#else
      integer*8 iside,isize,ifont     ! passed to viewtext
#endif

C Initial menu entry setup.
      write(head,'(A,A)')'  MRT details: ',zname(ICOMP)
      MODIFY=.FALSE.
   92 IER=0
      IVERT=-3

C Set menu header text.
      WRITE(VERT(1),'(A,F7.3)') 'a Sensor origin X : ',XOC(ICUB)
      WRITE(VERT(2),'(A,F7.3)') 'b Sensor origin Y : ',YOC(ICUB)
      WRITE(VERT(3),'(A,F7.3)') 'c Sensor origin Z : ',ZOC(ICUB)
      WRITE(VERT(4),'(A,F7.3)') 'd Sensor width (X): ',DXC(ICUB)
      WRITE(VERT(5),'(A,F7.3)') 'e Sensor depth (Y): ',DYC(ICUB)
      WRITE(VERT(6),'(A,F7.3)') 'f Sensor height(Z): ',DZC(ICUB)
      WRITE(VERT(7),'(A,F5.1)') 'g Sensor rotation : ',CANG(ICUB)
      WRITE(VERT(8),'(A,A6)')   'h Sensor name : ',CUBN(ICUB)
      VERT(9)    ='  _______________________ '
      VERT(10)   ='? Help                    '
      VERT(11)   ='- Exit to MRT menu        '

C If editing the sensors and user has asked for an update of the
C image then use common block info for the zone as well as for the
C sensors. Draw the zone and obstructions as standard and then
C draw the sensor cubes.  Set all surface lines to standard width.
      CALL INLNST(1)
      nzg=1
      nznog(1)=ICOMP
      izgfoc=ICOMP
      CALL ADJVIEW(IER)
      call cfgtogg(icfg_type,icfgz,icfgn,icfgc,icfgdfn,
     &                             iicfgz,iicfgn,iicfgc,iicfgdfn)
      if(MMOD.EQ.8)then
        call opencfg(icfg_type,icfgz,icfgn,icfgc,icfgdfn,
     &                             iicfgz,iicfgn,iicfgc,iicfgdfn)
        WRITE(etext,'(2A)')'Model: ',modeltitle(1:lnblnk(modeltitle))
        iside=1
        isize=1
        ifont=1
        call viewtext(etext,iside,isize,ifont)
      endif
      CALL ESCZONE(ICOMP)
      CALL DRWSEN(ier)
      CALL ERCZONE(ICOMP)

C Help text for this menu.
      H(1)='The specification of MRT sensors is currently based'
      H(2)='on a sensor being a prism.  Assuming there is no'
      H(3)='rotation the width of the sensor is its distance'
      H(4)='along the X axis, the depth is along the Y axis, and'
      H(5)='the height is along the Z axis.  The sensor may then'
      H(6)='be rotated with the angle being measured anticlockwise'
      H(7)='from the X axis.'

C Now display the menu.
      CALL EMENU(head,VERT,11,IVERT)
      IF(IVERT.EQ.1)THEN
        H(1)='The origin of the sensor is in site coords. '
        WRITE(MSG,'(a,I3)')'For sensor ',ICUB
        CALL EASKR(XOC(ICUB),MSG,'Origin X coordinate?',
     &       -999.9,'W',999.9,'W',1.0,'origin X coord',IER,1)
        IVFOK(icub)=0
        MODIFY=.TRUE.
        MODLEN=.TRUE.
      ELSEIF(IVERT.EQ.2)THEN
        H(1)='The origin of the sensor is in site coords. '
        CALL EASKR(YOC(ICUB),MSG,' Origin Y coordinate?',
     &       -999.9,'W',999.9,'W',1.0,'origin X coord',IER,1)
        IVFOK(icub)=0
        MODIFY=.TRUE.
        MODLEN=.TRUE.
      ELSEIF(IVERT.EQ.3)THEN
        H(1)='The origin of the sensor is in site coords. '
        CALL EASKR(ZOC(ICUB),MSG,'Origin Z coordinate?',
     &       -999.9,'W',999.9,'W',1.0,'origin X coord',IER,1)
        MODIFY=.TRUE.
        MODLEN=.TRUE.
      ELSEIF(IVERT.EQ.4)THEN
        H(1)='The width (with no rotation) is along the X axis.'
        CALL EASKR(DXC(ICUB),MSG,' Sensor width? ',
     &       0.001,'W',99.9,'W',1.0,'sensor X dimension',IER,1)
        IVFOK(icub)=0
        MODIFY=.TRUE.
        MODLEN=.TRUE.
      ELSEIF(IVERT.EQ.5)THEN
        H(1)='The depth (with no rotation) is along the Y axis.'
        CALL EASKR(DYC(ICUB),MSG,' Sensor depth? ',
     &       0.001,'W',99.9,'W',1.0,'sensor Y dimension',IER,1)
        IVFOK(icub)=0
        MODIFY=.TRUE.
        MODLEN=.TRUE.
      ELSEIF(IVERT.EQ.6)THEN
        H(1)='The height (with no rotation) is along the Z axis.'
        CALL EASKR(DZC(ICUB),MSG,' Sensor height? ',
     &       0.001,'W',99.9,'W',1.0,'sensor Z dimension',IER,1)
        IVFOK(icub)=0
        MODIFY=.TRUE.
        MODLEN=.TRUE.
      ELSEIF(IVERT.EQ.7)THEN
        H(1)='Rotation is anticlockwise degrees between the '
        H(2)='length side of the sensor and the X axis.'
        CALL EASKR(CANG(ICUB),MSG,' Sensor rotation? ',
     &       -359.0,'W',359.0,'W',0.0,'sensor rotation',IER,2)
        IVFOK(icub)=0
        MODIFY=.TRUE.
        MODLEN=.TRUE.
      ELSEIF(IVERT.EQ.8)THEN
        H(1)='A descriptive name (up to 6 char) is recommended.'
        S6=' '
        CALL EASKS(S6,' ',' Name of MRT sensor ? ',
     &    6,'sensor','sensor name',IER,1)
        IF(S6.NE.' ')CUBN(ICUB)=S6
        MODIFY=.TRUE.
      ELSEIF(IVERT.EQ.10)THEN
        CALL PHELPD('sensor menu',7,'-',0,0,IER)
      ELSEIF(IVERT.EQ.11)THEN
        return
      ENDIF
      GOTO 92

      END
