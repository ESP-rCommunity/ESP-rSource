C This file is part of the ESP-r system.
C Copyright Energy Systems Research Unit, University of
C Strathclyde, Glasgow, Scotland, 2001-.

C ESP-r is free software.  You can redistribute it and/or
C modify it under the terms of the GNU General Public
C License as published by the Free Software Foundation 
C (version 2 or later).

C ESP-r is distributed in the hope that it will be useful
C but WITHOUT ANY WARRANTY; without even the implied
C warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
C PURPOSE. See the GNU General Public License for more
C details.


C This file contains the following subroutines.
C CFDCOMP - controls the editing of CFD input files.
C CFDGGRD - define domain gridding.
C CFDSVAR - controls the editing of CFD solution variables.
C CFDEDAT - controls the editing of CFD equations.
C CFDEDATC- controls the editing of contaminant relaxation factors.
C CFDBVAR - controls the editing of CFD boundary conditions.
C DELCFDBC- deletes a CFD boundary condition.
C MKCFDBC - deletes a CFD boundary condition.
C GENCFDBC- controlling routine for boundary condition auto-generation.
C GENOCBC - auto-generates BCs representing occupants.
C GENEDGBC- auto-generates edge BCs from zone geometry.
C GENVOBC - auto-generates blockages from zone visual entities.
C GENOSBC - auto-generates blockages in areas that fall outside the zone.
C EDAIRO  - controls the editing of CFD air flow opening boundary variables.
C MFCONF  - defines conflation of CFD and mfs 
C EDSLDB  - controls the editing of CFD solid boundary variables.
C EDSRC   - controls the editing of CFD sources - humidity, CO2, etc.
C EDBLK   - controls the editing of CFD blockages to flow
C HANDSHK - controls the editing of the handshaking mechanism.
C PIKCELS - controls the editing of the cells defining a boundary region.
C PIKFACE - controls the editing of the faceof the boundary region.
C TOPBOT  - define upper and lower curvilinear x-section for use in
C           gridding generation.
C GRIDDEF - define gridding via regions and specify number of cells
C           and cell distribution for each region.
C ASKMFNOD- asks for one or more mass flow nodes (copy of mfoutp.F).
C ASKMFCON- Asks for one or more mass flow connections (copy of mfoutp.F).  


C ******************** CFDCOMP ********************
C Controls the editing of CFD input files.
C ITRU unit number for user output, IUF unit number for CFD input file.
C IER=0 indicates no error.

      SUBROUTINE CFDCOMP(izone,iuf,IER)
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "cfd.h"
#include "espriou.h"
#include "prj3dv.h"
#include "help.h"
      
      integer lnblnk  ! function definition

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/FILEP/IFIL
      common/OUTIN/IUOUT,IUIN,IEOUT
      common/gzonpik/izgfoc,nzg,nznog(mcom)
      COMMON/DEFLT3/DFCFD,DECMPDBFL,DICONDBFL
      common/cfdfil/LCFD(MCOM),IFCFD(MCOM)
      COMMON/ICFNOD/ICFD,ICP
      common/param2/TITLE(MNZ),CFTRFL(MNZ),LPHI(MNZ)
      common/GRIDEFN/origin,xgrid,ygrid,zgrid,zegrid,ortho
      common/ndcfd/ncfdnd,icfdnd(MNZ),NCONF
      integer ncfdnd,icfdnd,NCONF
      COMMON/CFDVIS/HAS_GEOM,ISHSB,ISHAO,IFACES,ISHBLK,ISHSRC,ISHGEO,
     &              INITD
      logical HAS_GEOM
      character INITD*6

C 3D visualisation mode.
      COMMON/MODVIS/IVISMOD

      logical origin,xgrid,ygrid,zgrid,zegrid,ortho
      logical OK,XST,CFDsave

      character OUTSTR*124,WORD*124,ETEXT*82
      CHARACTER ITEM(12)*34
      character ltmp*72,dtmp*72,LCFD*72
      CHARACTER*72 TITLE,CFTRFL,LPHI
      character*72 DFCFD,DECMPDBFL,DICONDBFL
      integer found
      integer NITMS,INO ! max items and current menu item

#ifdef OSI
      integer iside,isize,ifont
#else
      integer*8 iside,isize,ifont
#endif

      helpinsub='edcfd'  ! set for subroutine

      IVISMOD=-1

C ICFD=1 for the first CFD domain in the building, 2 for the second CFD
C domain etc. ICFD is set independently in VERMAN in prj.F, any updates
C here should be mirrored there as well.
      found=0
      do 234 iconf=1,nconf
        if ( icfdnd(iconf).eq.izone )then
          ICFD=iconf; found=1
        endif
  234 continue
      if(found.eq.0)then

C No existing domain for this zone, clear domain independent commons,
C set ICFD, and initialise simulation parameters.
C Do not increment NCONF yet, as user might not include the domain.
        CALL RESETCFD(IER)
        ICFD=nconf+1
        CALL CFDDEFLT
      endif
      ICP = izone

C Ensure operations file has been read for dynamic occupant information.
      IUO=IFIL+1
      CALL ERPFREE(IUO,ISTAT)
      CALL EROPER(0,IUOUT,IUO,ICP,IER)
      if (IER.ne.0) then
        write(outstr,'(2a)')'If you wish to use dynamic occupants,',
     &    ' please define zone operations before continuing.'
        call edisp(IUOUT,outstr) 
      endif

      TITLE(ICFD) = '...'

C Set the flag to indicate that the data has not been saved to the
C CFD input file.
      CFDsave=.false.

C Set flags to indiate the origin of CFD domain and that gridding of axes
C have not yet been defined.
      origin=.false.; xgrid=.false.; ygrid=.false.; zgrid=.false.

C= Assume orthogonal domain.
      ortho=.true.

C Ensure that G1 common blocks are correctly assigned.
      call georead(IFIL+1,LGEOM(izone),izone,1,IUOUT,IER)

C Draw the zone and display the vertex numbers.
C << changed to use CFD visualisation subroutines >>
      call tstamp('>','PRJ: edit CFD domain') 
      nzg=1; nznog(1)=izone; izgfoc=izone
      itvno=0; itsnm=0

C Setup help text for main menu.
      helptopic='cfd_opening_menu'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Get the name of the CFD input file.
      if(LCFD(izone)(1:7).eq.'UNKNOWN')then
        if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
          WRITE(LCFD(izone),'(A,A4)')
     &      zname(izone)(1:lnzname(izone)),'.dfd'
        else
          WRITE(LCFD(izone),'(A,A,A,A4)') zonepth(1:lnblnk(zonepth)),
     &      '/',zname(izone)(1:lnzname(izone)),'.dfd'
        endif
      endif
      ltmp=LCFD(izone)
      CALL EASKS(ltmp,' ','CFD domain definition file?',72,DFCFD,
     &                                    'dfd file',IER,nbhelp)
      if(ltmp(1:2).ne.'  ')LCFD(izone)=ltmp

C If the CFD input file exists, read in its contents.
C This is a two-stage process: open the file and get the conflation
C flag then read in the data based on the conflation type.
      CALL ERPFREE(IUF,ISTAT)
      call FINDFIL(LCFD(izone),XST)
      if(XST)then
        write(currentfile,'(a)')LCFD(izone)(1:LNBLNK(LCFD(izone)))
        CALL EFOPSEQ(IUF,LCFD(izone),1,IER)
        IF(IER.NE.0)goto 1
        CALL STRIPC(IUF,OUTSTR,0,ND,1,'dfd line 1',IER)
        if(OUTSTR(1:7).eq.'*DFS V2')then
          CALL ERPFREE(IUF,ISTAT)
          call DFDREAD(IZONE,0,iuout,IER)
          if (IER.ne.0) return
        elseif(OUTSTR(1:15).eq.'DFS DESCRIPTION')then
          CALL STRIPC(IUF,OUTSTR,0,ND,1,'dfd line 2',IER)
          K=0
          CALL EGETW(OUTSTR,K,WORD,'W','tag',IFLAG)
          if(WORD(1:11).eq.'*conflation')then

C Set the conflation flag.
            CALL EGETWI(OUTSTR,K,iv,0,3,'F','confla type',IER)
            IFCFD(izone)=iv
            CALL ERPFREE(IUF,ISTAT)

C Set up for reading in CFD input file.
            IBLD=0; IMFN=0
            if(IFCFD(izone).eq.1) IBLD=1
            if(IFCFD(izone).eq.2) IBLD=1
            if(IFCFD(izone).eq.3) IMFN=1
            CALL EFOPSEQ(IUF,LCFD(izone),1,IER)

C << Force reporting. >>
            CALL CFDDTA(2,iuout,izone,IBLD,IMFN,IER)

C Set flags to indicate that CFD origin and gridding has been defined.
            origin=.true.; xgrid=.true.; ygrid=.true.; zgrid=.true.
          else
            call usrmsg('DFD confl. tag missing in:',LCFD(izone),'W')
            CALL ERPFREE(IUF,ISTAT)
            return
          endif
        else
          call usrmsg('Not a recognised file in:',LCFD(izone),'W')
          CALL ERPFREE(IUF,ISTAT)
          return
        endif

C Default values for visualisation.
        if (origin.and.xgrid.and.ygrid.and.zgrid) then
          IVISMOD=2
          HAS_GEOM=.TRUE.
          ISHSB=-1; ISHAO=-1; IFACES=1; ISHBLK=-1; ISHSRC=-1; ISHGEO=1
        endif
      endif
      CALL ERPFREE(IUF,ISTAT)

      MODIFYVIEW=.TRUE.
      MODLEN=.TRUE.
      MODBND=.TRUE.

C Present menu of options for editing CFD input file.
   10 INO=-4
      ITEM(1) ='1 title: '
      write (ITEM(2),'(a,a)') '  ',TITLE(ICFD)(1:30)
      ITEM(3) ='  -------------------------- '
      if (IFCFD(izone).eq.0) then
        ITEM(4) ='a CFD coupling >> Off '
      else
        ITEM(4) ='a CFD coupling >> On  '
      endif
      ITEM(5) ='b geometry and gridding '
      ITEM(6) ='c solution variables '
      ITEM(7) ='d boundary conditions '
      ITEM(8) ='  -------------------------- '
      ITEM(9) ='! report domain details '
      ITEM(10) ='> save CFD input file '
      ITEM(11)='? help'
      ITEM(12)='- exit menu'
      NITMS=12

      IF(MMOD.EQ.8)THEN

C Only display if domain geometry has been defined.
        if (origin.and.xgrid.and.ygrid.and.zgrid) then
          call redraw(IER)
        else

C Just display title.
          WRITE(ETEXT,'(2A)')'Domain: ',
     &      TITLE(izone)(1:lnblnk(TITLE(izone)))
          iside=1; isize=1; ifont=1
          call viewtext(ETEXT,iside,isize,ifont)
        endif
      
      ENDIF

C Help text for this menu.
  12  helptopic='cfd_opening_menu'
      call gethelptext(helpinsub,helptopic,nbhelp)
 
      CALL EMENU('Zone CFD definition',ITEM,NITMS,INO)

C Wrong pick.
      IF(INO.EQ.0)THEN
        INO=-1
        GOTO 12

C Edit description.
      ELSEIF(INO.EQ.1)THEN
        dtmp='Model of room air flow'
        ltmp=TITLE(ICFD)
        call EASKS(ltmp,' ','Model description?',72,dtmp,
     &                  'CFD description',IER,42)
        TITLE(ICFD)=ltmp

C Conflation options.
      ELSEIF(INO.EQ.4)THEN
        helptopic='cfd_conflation_options'
        call gethelptext(helpinsub,helptopic,nbhelp)
        call EASKOK(' ','Couple CFD with other domains?',OK,nbhelp)
        if(OK)then
          IFCFD(izone)=4
          helptopic='cfd_conflation_active'
          call gethelptext(helpinsub,helptopic,nbhelp)
          call PHELPD('CFD coupling activated',nbhelp,'-',0,0,IER)
        else 
          IFCFD(izone)=0
        endif

C Geometry and gridding.
      ELSEIF(INO.EQ.5)THEN 
        call CFDGGRD(izone,IER)
        CALL GRID(ier)
        CALL NEW2OLD

C Solution variables and initial Ccnditions.
      ELSEIF(INO.EQ.6)THEN 
        call CFDSVAR(IER)

C Boundary conditions.
      ELSEIF(INO.EQ.7)THEN 
        call CFDBVAR(IZONE,IER)

C Report model details.
      ELSEIF(INO.EQ.9)THEN
        CALL ERPFREE(IUF,ISTAT)
        call DFDREAD(IZONE,2,iuout,IER)

C Save data to dfs input file.
      ELSEIF(INO.EQ.10)THEN 
        call DFDSV(IUF,IZONE,IER)
        if (IER.eq.0) CFDsave=.true.

C Help.
      ELSEIF(INO.EQ.(NITMS-1))THEN
        helptopic='cfd_opening_menu'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('CFD inputs',nbhelp,'-',0,0,IER)

C Exit menu.
      ELSEIF(INO.EQ.NITMS)THEN
        if(.not.CFDsave)then
          CALL EASKOK('Data not yet saved.',
     &                'Confirm exit?',OK,nbhelp)
          IF(.NOT.OK)GOTO 10
        endif
        return

      ENDIF
      GOTO 10

C Error handling
    1 CALL USRMSG('Problem with CFD input file line:',OUTSTR,'W')
      return
      end


C ******************* CFDGGRD *******************
C CFDGGRD - define domain gridding.
C IUF unit number for CFD input file.
C IER=0 indicates no error.

      SUBROUTINE CFDGGRD(izone,IER)
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "cfd.h"
#include "prj3dv.h"
#include "help.h"

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/FILEP/IFIL
      common/OUTIN/IUOUT,IUIN,IEOUT
      integer menuchw,igl,igr,igt,igb,igw,igwh
      common/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      common/gzonpik/izgfoc,nzg,nznog(mcom)

C NREG(1,*) is X, NREG(2,*) is Y, NREG(3,*) is west, NREG(4,*) is east
C (the east (4,*) is for curvilinear gridding).
      common/GRIDFN/NCELX(MNREG,MNZ),NCELY(MNREG,MNZ),NCELZ(MNREG,MNZ),
     &  NCELZE(MNREG,MNZ),XREG(MNREG,MNZ),YREG(MNREG,MNZ),
     &  ZREG(MNREG,MNZ),ZREGE(MNREG,MNZ),Xplaw(MNREG,MNZ),
     &  Yplaw(MNREG,MNZ),Zplaw(MNREG,MNZ),Zplawe(MNREG,MNZ),NREG(4,MNZ)
      common/GRDVRTS/iorg(MNZ),ixend(MNZ),iyend(MNZ),izend(MNZ),
     &  izende(MNZ)

      common/GRIDEFN/origin,xgrid,ygrid,zgrid,zegrid,ortho
      COMMON/ICFNOD/ICFD,ICP
      common/grdmax/NTCX,NTCY,NTCZ

      common/EQTION/CALLU(MNZ),CALLV(MNZ),CALLW(MNZ),CALLT(MNZ),
     &             CALLC(MNZ),KEMDL(MNZ),BUOY(MNZ),BOUSSI(MNZ),
     &             ZEROT(MNZ),ZandKE(MNZ),MITzero(MNZ)

      COMMON/CFDVIS/HAS_GEOM,ISHSB,ISHAO,IFACES,ISHBLK,ISHSRC,ISHGEO,
     &              INITD
      logical HAS_GEOM
      character INITD*6

C 3D visualisation mode.
      COMMON/MODVIS/IVISMOD

C Arrays for sorting domains from (see code for comments).
      dimension XS(MTV),XSS(MTV),XSI(MTV),YS(MTV),YSS(MTV),YSI(MTV)
      dimension ZS(MTV),ZSI(MTV),ZSS(MTV)

      LOGICAL CALLU,CALLV,CALLW,CALLT,CALLC,KEMDL,BUOY,BOUSSI
      logical close,griderr
      logical origin,xgrid,ygrid,zgrid,zegrid,ortho
      LOGICAL ZEROT,ZandKE,MITzero,vclose

      character ITEM(16)*36
      character outs*124,outs248*248

      integer iglib   ! if 1 then X11, if 2 then GTK, if 3 then text only.
      integer iorgt,ixendt,iyendt,izendt,izendet  ! for local editing
      integer NITMS,INO ! max items and current menu item

#ifdef OSI
      integer iigl,iigr,iigt,iigb,iigw,iigwh
      integer iiw1,iiw2,iiw3,iiw4,iimenu
      integer ilf,igfw,igfh,ild,igdw,igdh
#else
      integer*8 iigl,iigr,iigt,iigb,iigw,iigwh
      integer*8 iiw1,iiw2,iiw3,iiw4,iimenu
      integer*8 ilf,igfw,igfh,ild,igdw,igdh
#endif

      helpinsub='edcfd'  ! set for subroutine

C Assume axes defined and set initial values for general resolution
C of the X Y Z axis and visualisation options.
C << Todo: set these based on a higher level concept of
C << low/medium/high resolution and/or the size of the zone.
C -- Visualisation variables already defined in calling routine
      origin=.TRUE.
      ieorg=0; icellx=20; icelly=20; icellz=20
      itrc=0   ! silent feedback initially
c      ISHSB=1
c      ISHAO=1
c      ISHBLK=1
c      IFACES=1

C Help text for this menu.
      helptopic='cfd_gridding_setup'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Check if we have vertex at ends of axis data.
      if (IORG(ICFD).eq.0) then

        call usrmsg('You will have to define the CFD axis relationship',
     &              'to the zone geometry via vertex numbers.','W')

C Display zone geometry with vertex numbers to aid in selection
C Restore standard graphics window variables.
        IVISMOD=1
        if(MMOD.EQ.8) call redraw(IER)

        write(outs,*) 'Coordinates in the zone...'
        call edisp(iuout,outs)
        write(outs,*) '         1      2      3      4      5      6'
        call edisp(iuout,outs)
        indisp=MIN0(32,NZTV(izone))
        write(outs248,'(a,32F7.3)') 'x axis',(X(j),j=1,indisp)
        call edisp248(iuout,outs248,100)
        write(outs248,'(a,32F7.3)') 'y axis',(Y(j),j=1,indisp)
        call edisp248(iuout,outs248,100)
        write(outs248,'(a,32F7.3)') 'z axis',(Y(j),j=1,indisp)
        call edisp248(iuout,outs248,100)

C Ask user for zone vertex indices of the origin of cfd domain
C followed by vertex at end of X axis, vertex at end of Y axis
C and the vertex for the upper Z axis point (usually above the
C Y axis end point).
        iorgt=IORG(ICFD)
        CALL EASKI(iorgt,' Specify vertex at origin of CFD domain. ',
     &    ' Which `v`? ',1,'F',NTV,'F',1,'orig vertex',IERI,nbhelp)
        if(ieri.eq.-3) then
          origin=.false.
          return
        endif
        ixendt=ixend(ICFD)
        CALL EASKI(ixendt,' Specify vertex at end of X axis. ',
     &    ' Which `v`? ',1,'F',NTV,'F',2,'x-axis vertex',IERI,nbhelp)
        if(ieri.eq.-3) then
          origin=.false.
          return
        endif

        iyendt=iyend(ICFD)
        CALL EASKI(iyendt,' Specify vertex at end of Y axis. ',
     &    ' Which `v`? ',1,'F',NTV,'F',4,'y-axis vertex',IERI,nbhelp)
        if(ieri.eq.-3) then
          origin=.false.
          return
        endif

        izendt=izend(ICFD)
        CALL EASKI(izendt,' Specify vertex at end of Z axis. ',
     &    ' Which `v`? ',1,'F',NTV,'F',5,'z-axis vertex',IERI,nbhelp)
        if(ieri.eq.-3) then
          origin=.false.
          return
        endif

C No cancels so instantiate variables.
        IORG(ICFD)=iorgt
        ixend(ICFD)=ixendt; iyend(ICFD)=iyendt; izend(ICFD)=izendt
        IVISMOD=2
        HAS_GEOM=.TRUE.
        ISHSB=-1; ISHAO=-1; IFACES=1; ISHBLK=-1; ISHSRC=-1; ISHGEO=1
        if (MMOD.eq.8) call redraw(IER)        
      endif

C Make sure axes are orthogonal, as dfs only supports Cartesian coordinates.
C Check X and Y.
      call ang3vtx(X(ixend(ICFD)),Y(ixend(ICFD)),Z(ixend(ICFD)),
     &  X(IORG(ICFD)),Y(IORG(ICFD)),Z(IORG(ICFD)),X(iyend(ICFD)),
     &  Y(iyend(ICFD)),Z(iyend(ICFD)),ang3)
      call eclose(ang3,90.00,0.1,close)
      if(.NOT.close) then
        call usrmsg(
     &    'X & Y axes are not orthogonal. Please check and respecify.',
     &    '  ','W')
        origin=.FALSE.; xgrid=.FALSE.
      endif

C Check X and Z.
      call ang3vtx(X(ixend(ICFD)),Y(ixend(ICFD)),Z(ixend(ICFD)),
     &  X(IORG(ICFD)),Y(IORG(ICFD)),Z(IORG(ICFD)),X(izend(ICFD)),
     &  Y(izend(ICFD)),Z(izend(ICFD)),ang3)
      call eclose(ang3,90.00,0.1,close)
      if(.NOT.close) then
        call usrmsg(
     &    'X & Z axes are not orthogonal. Please check and respecify.',
     &    '  ','W')
        origin=.FALSE.; ygrid=.FALSE.
      endif

C Check Y and Z.
      call ang3vtx(X(iyend(ICFD)),Y(iyend(ICFD)),Z(iyend(ICFD)),
     &  X(IORG(ICFD)),Y(IORG(ICFD)),Z(IORG(ICFD)),X(izend(ICFD)),
     &  Y(izend(ICFD)),Z(izend(ICFD)),ang3)
      call eclose(ang3,90.00,0.1,close)
      if(.NOT.close) then
        call usrmsg(
     &    'Y & Z axes are not orthogonal. Please check and respecify.',
     &    '  ','W')
        origin=.FALSE.; zgrid=.FALSE.
      endif

C Determine length of each axis.
      xdis=crowxyz(X(IORG(ICFD)),Y(IORG(ICFD)),Z(IORG(ICFD)),
     &  X(ixend(ICFD)),Y(ixend(ICFD)),Z(ixend(ICFD)))
      ydis=crowxyz(X(IORG(ICFD)),Y(IORG(ICFD)),Z(IORG(ICFD)),
     &  X(iyend(ICFD)),Y(iyend(ICFD)),Z(iyend(ICFD)))
      zdis=crowxyz(X(IORG(ICFD)),Y(IORG(ICFD)),Z(IORG(ICFD)),
     &  X(izend(ICFD)),Y(izend(ICFD)),Z(izend(ICFD)))

C Non orthogonal.
      if (.NOT.ortho) then

C Check Y and Ze.
        call ang3vtx(X(iyend(ICFD)),Y(iyend(ICFD)),Z(iyend(ICFD)),
     &    X(IORG(ICFD)),Y(IORG(ICFD)),Z(IORG(ICFD)),X(izende(ICFD)),
     &    Y(izende(ICFD)),Z(izende(ICFD)),ang3)
        call eclose(ang3,90.00,0.1,close)
        if(.NOT.close) then
          call usrmsg('Y & Ze axes are not orthogonal. ',
     &                'Please check and respecify.','W')
          INO=1
          goto 9
        endif
      endif

C Set-up data for menu.
   14 INO=-4

C Check for gridding errors.
      if (origin.AND.xgrid.AND.ygrid.AND.zgrid) then
        griderr=.FALSE.
      else
        griderr=.TRUE.
      endif

C Need to define counter for variable length menu.
      NCP=0

C Create menu text.
      ITEM(1) ='1 define origin and axes.'
      ITEM(2) ='  Vertex ids:'
      if (ortho) then
        write (ITEM(3),'(4(a,i2))') ' O=',IORG(ICFD),': Vx=',
     &    ixend(ICFD),', Vy=',iyend(ICFD),', Vz=',izend(ICFD)
      else
        write (ITEM(3),'(5(a,i2))') ' O=',IORG(ICFD),': Vx=',
     &    ixend(ICFD),', Vy=',iyend(ICFD),', Vz=',izend(ICFD),' Vze=',
     &    izende(ICFD)
      endif
      ITEM(4) =' ------------------------------ '
      ITEM(5) ='a generate uniform grid         '
      ITEM(6) ='b estimate regions from geometry'
      ITEM(7) =' Axis Regions Total cells'
      write (ITEM(8),'(a,2i6)') 'c X  ',NREG(1,ICFD),NTCX
      write (ITEM(9),'(a,2i6)') 'd Y  ',NREG(2,ICFD),NTCY
      write (ITEM(10),'(a,2i6)') 'e Z  ',NREG(3,ICFD),NTCZ
      if (.NOT.ortho) then
        write (ITEM(11),'(a,2i6)') 'f Ze ',NREG(4,ICFD),NTCZe
        NCP=1
      endif
      if (griderr) then
        ITEM(11+NCP) ='  Gridding incomplete or errors! '
      else
        ITEM(11+NCP) ='g visualise gridding             '
      endif
      ITEM(12+NCP) =' ------------------------------ '
      if(itrc.eq.0)then
        ITEM(13+NCP) ='> feedback silent             '
      elseif(itrc.eq.1)then
        ITEM(13+NCP) ='> feedback brief              '
      elseif(itrc.eq.2)then
        ITEM(13+NCP) ='> feedback verbose            '
      endif
      ITEM(14+NCP)   ='? help                        '
      ITEM(15+NCP)   ='- exit                        '
      NITMS=15+NCP

      IF(MMOD.EQ.8)THEN

C Recalculate gridding in case this has changed.
        CALL INICNT
        CALL GRID(ier)
        call NEW2OLD

        call redraw(IER)
      ENDIF

C Help text for this menu.
      helptopic='cfd_gridding_setup'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Display menu.
      CALL EMENU('Geometry and gridding',ITEM,NITMS,INO)

C Prompt user to select axis vertices.
C Display zone geometry with vertex numbers to aid in selection.
C Restore standard graphics window variables.
 9    if(INO.EQ.1)then
        if(MMOD.EQ.8)then

C Setup and pass in parameters to win3d.
          iiw1=4; iiw2=1; iiw3=1; iiw4=3; iimenu=menuchw
          iigl=igl; iigr=igr; iigt=igt; iigb=igb; iigw=igw; iigwh=igwh
          ilf=2; ild=LIMTTY
          iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
          if(iglib.eq.1)then
            CALL feedbox(iimenu,ilf,igfw,igfh)
            CALL opengdisp(iimenu,ild,ilf,igdw,igdh)
          endif
          CALL win3d(iimenu,iiw1,iiw2,iiw3,iiw4,
     &      iigl,iigr,iigt,iigb,iigw,iigwh)
          igl=int(iigl); igr=int(iigr); igt=int(iigt); igb=int(iigb)
          igw=int(iigw); igwh=int(iigwh)
          call startbuffer()

C Redraw zone and vertex points. Note, if user has returned
C from 3D grid visualisation then G1 common blocks will need
C to be refreshed.
          nzg=1; nznog(1)=izone; izgfoc=izone
          call georead(IFIL+1,LGEOM(izone),izone,1,IUOUT,IER)
          CALL ESCZONE(izone)
          CALL BNDOBJ(0,IER)
          CALL ERCZONE(izone)
          MODIFYVIEW=.TRUE.; MODLEN=.TRUE.; MODBND=.TRUE.
          itvno=0; itsnm=0
          CALL INLNST(1)
          CALL ADJVIEW(IER)
        endif

C Specify origin and axes of CFD domain.
        write(outs,*) 'Coordinates in the zone...'
        call edisp(iuout,outs)
        write(outs,*) '         1      2      3      4      5      6'
        call edisp(iuout,outs)
        indisp=MIN0(32,NZTV(izone))
        write(outs248,'(a,32F7.3)') 'x axis',(X(j),j=1,indisp)
        call edisp248(iuout,outs248,100)
        write(outs248,'(a,32F7.3)') 'y axis',(Y(j),j=1,indisp)
        call edisp248(iuout,outs248,100)
        write(outs248,'(a,32F7.3)') 'z axis',(Y(j),j=1,indisp)
        call edisp248(iuout,outs248,100)
        iorgt=IORG(ICFD)
        CALL EASKI(iorgt,' Specify vertex at origin of CFD domain. ',
     &    ' Which `v`? ',1,'F',NTV,'F',1,'orig vertex',IERI,nbhelp)
        if(ieri.eq.-3) then
          origin=.false.
          return
        endif

        ixendt=ixend(ICFD)
        CALL EASKI(ixendt,' Specify vertex at end of X axis. ',
     &    ' Which `v`? ',1,'F',NTV,'F',1,'x-axis vertex',IERI,nbhelp)
        if(ieri.eq.-3) then
          origin=.false.
          return
        endif

        iyendt=iyend(ICFD)
        CALL EASKI(iyendt,' Specify vertex at end of Y axis. ',
     &    ' Which `v`? ',1,'F',NTV,'F',1,'y-axis vertex',IERI,nbhelp)
        if(ieri.eq.-3) then
          origin=.false.
          return
        endif

        izendt=izend(ICFD)
        CALL EASKI(izendt,' Specify vertex at end of Z axis. ',
     &    ' Which `v`? ',1,'F',NTV,'F',1,'z-axis vertex',IERI,nbhelp)
        if(ieri.eq.-3) then
          origin=.false.
          return
        endif

C No cancels so instantiate variables.
        IORG(ICFD)=iorgt
        ixend(ICFD)=ixendt; iyend(ICFD)=iyendt; izend(ICFD)=izendt

C Make sure axes are orthogonal, as dfs only supports Cartesian coordinates.
C Check X and Y.
        call ang3vtx(X(ixend(ICFD)),Y(ixend(ICFD)),Z(ixend(ICFD)),
     &    X(IORG(ICFD)),Y(IORG(ICFD)),Z(IORG(ICFD)),X(iyend(ICFD)),
     &    Y(iyend(ICFD)),Z(iyend(ICFD)),ang3)
        call eclose(ang3,90.00,0.1,close)
        if(.NOT.close) then
          call edisp(iuout,' X & Y axes are not orthogonal. Respecify.')
          goto 9
        endif

C Check X and Z.
        call ang3vtx(X(ixend(ICFD)),Y(ixend(ICFD)),Z(ixend(ICFD)),
     &    X(IORG(ICFD)),Y(IORG(ICFD)),Z(IORG(ICFD)),X(izend(ICFD)),
     &    Y(izend(ICFD)),Z(izend(ICFD)),ang3)
        call eclose(ang3,90.00,0.1,close)
        if(.NOT.close) then
          call edisp(iuout,' X & Z axes are not orthogonal. Respecify.')
          goto 9
        endif

C Check Y and Z.
        call ang3vtx(X(iyend(ICFD)),Y(iyend(ICFD)),Z(iyend(ICFD)),
     &    X(IORG(ICFD)),Y(IORG(ICFD)),Z(IORG(ICFD)),X(izend(ICFD)),
     &    Y(izend(ICFD)),Z(izend(ICFD)),ang3)
        call eclose(ang3,90.00,0.1,close)
        if(.NOT.close) then
          call edisp(iuout,' Y & Z axes are not orthogonal. Respecify.')
          goto 9
        endif

C Determine length of each axis.
        xdis=crowxyz(X(IORG(ICFD)),Y(IORG(ICFD)),Z(IORG(ICFD)),
     &    X(ixend(ICFD)),Y(ixend(ICFD)),Z(ixend(ICFD)))
        ydis=crowxyz(X(IORG(ICFD)),Y(IORG(ICFD)),Z(IORG(ICFD)),
     &    X(iyend(ICFD)),Y(iyend(ICFD)),Z(iyend(ICFD)))
        zdis=crowxyz(X(IORG(ICFD)),Y(IORG(ICFD)),Z(IORG(ICFD)),
     &    X(izend(ICFD)),Y(izend(ICFD)),Z(izend(ICFD)))

C Set flag to indicate that origin has been defined.
        origin=.true.

C Generate a uniform grid.
      elseif(INO.eq.5)then

C Ask user how big the cells should be.
        write(outs,'(a)') 
     &    'Roughly how large would you like cells to be?'
        CALL EASKR(size,outs,'(meters)',0.0,'F',0.5,'W',0.1,
     &    'cell size',IER,nbhelp)

C Set number of regions in each axis to 1.
        NREG(1,ICFD)=1; NREG(2,ICFD)=1; NREG(3,ICFD)=1

C Find zone extents in each direction.
        CALL CFDTRANS(2,SZCOORDS(izone,IORG(ICFD),1),
     &                  SZCOORDS(izone,IORG(ICFD),2),
     &                  SZCOORDS(izone,IORG(ICFD),3),
     &                  xmnx,ymny,zmnz,IER)
        CALL CFDTRANS(2,SZCOORDS(izone,IXEND(ICFD),1),
     &                  SZCOORDS(izone,IXEND(ICFD),2),
     &                  SZCOORDS(izone,IXEND(ICFD),3),
     &                  xmxx,xmxy,xmxz,IER)
        CALL CFDTRANS(2,SZCOORDS(izone,IYEND(ICFD),1),
     &                  SZCOORDS(izone,IYEND(ICFD),2),
     &                  SZCOORDS(izone,IYEND(ICFD),3),
     &                  ymxx,ymxy,ymxz,IER)
        CALL CFDTRANS(2,SZCOORDS(izone,IZEND(ICFD),1),
     &                  SZCOORDS(izone,IZEND(ICFD),2),
     &                  SZCOORDS(izone,IZEND(ICFD),3),
     &                  zmxx,zmxy,zmxz,IER)
        xd=xmxx-xmnx; yd=ymxy-ymny; zd=zmxz-zmnz

C Extent / size of cell = number of cells
        ixn=NINT(xd/size); iyn=NINT(yd/size); izn=NINT(zd/size)

        XREG(1,ICFD)=xd; NCELX(1,ICFD)=ixn; NTCX=ixn
        YREG(1,ICFD)=yd; NCELY(1,ICFD)=iyn; NTCY=iyn
        ZREG(1,ICFD)=zd; NCELZ(1,ICFD)=izn; NTCZ=izn
        
        XPLAW(1,ICFD)=1.; YPLAW(1,ICFD)=1.; ZPLAW(1,ICFD)=1.

C Set flags to indicate that CFD origin and gridding have been defined.
        origin=.true.; xgrid=.true.; ygrid=.true.; zgrid=.true.

C Set up regions by scanning geometry.
      elseif(INO.EQ.6)then

C Scan X,Y,Z coordinates and then sort in ascending order. Make use of
C XS,YS,ZS for array to sort and XSI,YSI,ZSI for their link back to
C initial position. Use XSS,YSS,ZSS for the compacted sorted list (with
C duplicates removed).

C Take into account if the zone is offset from the site origin.
C Use the zone bounds (ZXMN ZYMN ZZMN). Double check the max on each
C of the axis.
        xxmax=ZXMN(izone); yymax=ZYMN(izone); zzmax=ZZMN(izone)
        do 42 il=1,NZTV(izone)
          XS(il)=X(il); YS(il)=Y(il); ZS(il)=Z(il)
          if(X(il).gt.xxmax) xxmax=X(il)
          if(Y(il).gt.yymax) yymax=Y(il)
          if(Z(il).gt.zzmax) zzmax=Z(il)
          XSS(il)=ZXMN(izone); YSS(il)=ZYMN(izone); ZSS(il)=ZZMN(izone)
          XSI(il)=il; YSI(il)=il; ZSI(il)=il
  42    continue

C Sort each axis in ascending order.
C Debug.
C        write(6,*) 'maximums found ',xxmax,yymax,zzmax
C        write(6,*) 'max geom file ',ZXMX(izone),ZYMX(izone),ZZMX(izone)
C        write(6,*) 'array x',(X(j),j=1,NZTV(izone))
C        write(6,*) 'array xs',(XS(j),j=1,NZTV(izone))
        KFLAG = 2
        call SORTR(XS,XSI,NZTV(izone),KFLAG)
C        write(6,*) 'sorted array xs',(XS(j),j=1,NZTV(izone))
C        write(6,*) 'array xsi ',(xsi(j),j=1,NZTV(izone))
        call SORTR(YS,YSI,NZTV(izone),KFLAG)
        call SORTR(ZS,ZSI,NZTV(izone),KFLAG)

C Start at smallest value for each axis. tolx,toly,tolz is how small
C a cell can be at max resolution.
        vclose=.false.
        ilx=1
        tolx=(xdis/real(NTCELX)) * 0.2
        XSS(1)=XS(1)
        ily=1
        toly=(ydis/real(NTCELY)) * 0.2
        YSS(1)=YS(1)
        ilz=1
        ZSS(1)=ZS(1)
        tolz=(zdis/real(NTCELZ)) * 0.2

C Debug.
        if(itrc.gt.0)then
          write(outs,'(a,i3,F6.3,i3,F6.3,i3,F6.3)') 
     &     'Max cells & smallest cell dimension in each axis: ',
     &      NTCELX,tolx,NTCELY,toly,NTCELZ,tolz
          call edisp(iuout,outs)
        endif
        do 43 il=2,NZTV(izone)

C For X/Y/Z axis in turn see if next sorted point is sufficiently bigger
C than the last one to qualify as a new region boundary. Note that in
C less complex zones only the latter part of XS,YS,ZS array will have
C anything other than zero.
          vclose=.false.
          CALL ECLOSE(XS(il),XSS(ilx),tolx,vclose)
          if(vclose)then
            continue
          else
            if(XS(il).gt.XSS(ilx))then
              ilx=ilx+1
              XSS(ilx)=XS(il)
            endif
          endif
          vclose=.false.
          CALL ECLOSE(YS(il),YSS(ily),toly,vclose)
          if(vclose)then
            continue
          else
            if(YS(il).gt.YSS(ily))then
              ily=ily+1
              YSS(ily)=YS(il)
            endif
          endif
          vclose=.false.
          CALL ECLOSE(ZS(il),ZSS(ilz),tolz,vclose)
          if(vclose)then
            continue
          else
            if(ZS(il).gt.ZSS(ilz))then
              ilz=ilz+1
              ZSS(ilz)=ZS(il)
            endif
          endif
  43    continue

C Notify user of the number of regions in each axis and the width of each.
        if(itrc.gt.0)then
          call edisp(iuout,'  ')
          call edisp(iuout,'  regions   points at region extents...')
          write(outs,'(a,i2,10F7.3)') 'x axis ',ilx-1,(XSS(j),j=1,ilx)
          call edisp(iuout,outs)
          write(outs,'(a,10F7.3)')    '  reg-width ',
     &      ((XSS(j+1)-XSS(j)),j=1,ilx-1)
          call edisp(iuout,outs)
          write(outs,'(a,i2,10F7.3)') 'y axis ',ily-1,(YSS(j),j=1,ily)
          call edisp(iuout,outs)
          write(outs,'(a,10F7.3)')    '  reg-width ',
     &      ((YSS(j+1)-YSS(j)),j=1,ily-1)
          call edisp(iuout,outs)
          write(outs,'(a,i2,10F7.3)') 'z axis ',ilz-1,(ZSS(j),j=1,ilz)
          call edisp(iuout,outs)
          write(outs,'(a,10F7.3)')    '  reg-width ',
     &      ((ZSS(j+1)-ZSS(j)),j=1,ilz-1)
          call edisp(iuout,outs)
        endif

C Record number of regions in each axis (one less than scan above).
        if(NREG(1,ICFD).eq.0)NREG(1,ICFD)=ilx-1
        if(NREG(2,ICFD).eq.0)NREG(2,ICFD)=ily-1
        if(NREG(3,ICFD).eq.0)NREG(3,ICFD)=ilz-1

C Initial request for total cells in X axis.
C << could do better by asking region by region >>
        write(outs,'(a,i3,a,f6.2,a)') 
     &    'How many total cells along X axis (max is ',NTCELX,
     &    ' over ',xdis,'m) would you like to have?'
        CALL EASKI(icellx,outs,' ',1,'F',NTCELX,'F',10,
     &    'requested total X cells',IERI,nbhelp)
        if(ieri.eq.-3) then
          xgrid=.FALSE.
          return
        endif
        tolx=(xdis/real(icellx))
        ntcx=0
        if(ilx.ge.2)then
          do 44 ij=1,ilx-1
            xd = XSS(ij+1)-XSS(ij)
            XREG(ij,ICFD) = xd
            NCELX(ij,ICFD) = int(xd/tolx)
            if(NCELX(ij,ICFD).eq.0)NCELX(ij,ICFD) = 1
            Xplaw(ij,ICFD) = 1.0
            ntcx = ntcx + NCELX(ij,ICFD)
  44      continue
        else
          xd = XSS(2)-XSS(1)  ! in case of one region
          XREG(1,ICFD) = xd
          NCELX(1,ICFD) = int(xd/tolx)
          if(NCELX(1,ICFD).eq.0)NCELX(1,ICFD) = 1
          Xplaw(1,ICFD) = 1.0
          ntcx = ntcx + NCELX(1,ICFD)
        endif

C Debug.
        if(itrc.gt.1)then
          write(outs,*) 'xd tolx ',xd,tolx,int(xd/tolx)
          call edisp(iuout,outs)
          write(outs,*) 'ncell in x ',ntcx,(NCELX(j,icfd),j=1,ilx-1)
          call edisp(iuout,outs)
        endif

C Initial request for total cells in Y axis.
C << alternative asking region by region >>
        write(outs,'(a,i3,a,f6.2,a)') 
     &    'How many total cells along Y axis (max is ',NTCELY,
     &    ' over ',ydis,'m) would you like to have?'
        CALL EASKI(icelly,outs,' ',1,'F',NTCELY,'F',10,
     &    'requested total Y cells',IERI,nbhelp)
        if(ieri.eq.-3) then
          xgrid=.FALSE.
          return
        endif
        toly=(ydis/real(icelly))
        ntcy=0
        if(ily.ge.2)then
          do 45 ij=1,ily-1
            yd = YSS(ij+1)-YSS(ij)
            YREG(ij,ICFD) = yd
            NCELY(ij,ICFD) = int(yd/toly)
            if(NCELY(ij,ICFD).eq.0)NCELY(ij,ICFD) = 1
            Yplaw(ij,ICFD) = 1.0
            ntcy = ntcy + NCELY(ij,ICFD)
  45      continue
        else
          yd = YSS(2)-YSS(1)  ! in case of one region
          YREG(1,ICFD) = yd
          NCELY(1,ICFD) = int(yd/toly)
          if(NCELY(1,ICFD).eq.0)NCELY(1,ICFD) = 1
          Yplaw(1,ICFD) = 1.0
          ntcy = ntcy + NCELY(1,ICFD)
        endif

C Debug.
        if(itrc.gt.1)then
          write(outs,*) 'yd toly ',yd,toly,int(yd/toly)
          call edisp(iuout,outs)
          write(outs,*) 'ncell in y ',(NCELY(j,icfd),j=1,ily-1)
          call edisp(iuout,outs)
        endif

C Initial request for total cells in Z axis.
C << alternative asking region by region ? >>
        write(outs,'(a,i3,a,f6.2,a)') 
     &    'How many total cells along Z axis (max is ',NTCELZ,
     &    ' over ',zdis,'m) would you like to have?'
        CALL EASKI(icellz,outs,' ',1,'F',NTCELZ,'F',10,
     &    'requested total Z cells',IERI,nbhelp)
        if(ieri.eq.-3) then
          xgrid=.FALSE.
          return
        endif
        tolz=(zdis/real(icellz))
        ntcz=0
        if(ilz.ge.2)then
          do 46 ij=1,ilz-1
            zd = ZSS(ij+1)-ZSS(ij)
            ZREG(ij,ICFD) = zd
            NCELZ(ij,ICFD) = int(zd/tolz)
            if(NCELZ(ij,ICFD).eq.0)NCELZ(ij,ICFD) = 1
            Zplaw(ij,ICFD) = 1.0
            ntcz = ntcz + NCELZ(ij,ICFD)
  46      continue
        else
          zd = ZSS(2)-ZSS(1)  ! in case of one region
          ZREG(1,ICFD) = zd
          NCELZ(1,ICFD) = int(zd/tolz)
          if(NCELZ(1,ICFD).eq.0)NCELZ(1,ICFD) = 1
          Zplaw(1,ICFD) = 1.0
          ntcz = ntcz + NCELZ(1,ICFD)
        endif

C Debug.
        if(itrc.gt.1)then
          write(outs,*)'Z distance ',zd,' tollerance ',tolz,int(zd/tolz)
          call edisp(iuout,outs)
          write(outs,*) 'ncell in z ',(NCELZ(j,icfd),j=1,ilz-1)
          call edisp(iuout,outs)
        endif

C Set flags to indicate that CFD origin and gridding have been defined.
        origin=.true.; xgrid=.true.; ygrid=.true.; zgrid=.true.

C Advise the user what to do next.
        helptopic='cfd_gridding_auto'
        call gethelptext(helpinsub,helptopic,nbhelp)
        call PHELPD('CFD what to do next',nbhelp,'-',0,0,IER)

C Grid X-axis.
      elseif(INO.EQ.8)then
        call GRIDDEF(1,NREG,NTCX,xdis,NCELX,XREG,Xplaw,ier)
        if (ier.ne.0) then
          griderr=.TRUE.; xgrid=.FALSE.
        else
          xgrid=.TRUE.
        endif

C Grid Y-axis.
      elseif(INO.EQ.9)then
        call GRIDDEF(2,NREG,NTCY,ydis,NCELY,YREG,Yplaw,ier)
        if (ier.ne.0) then
          griderr=.TRUE.; ygrid=.FALSE.
        else
          ygrid=.TRUE.
        endif

C Have user subdivide Z-axis into regions and specify number of cells
C and cell distribution for each region. Additional questions asked
C if curvilinear.
      elseif(INO.EQ.10)then

C Ask whether Z is Orthogonal or curvilinear.
        CALL EASKABC('Options for Z gridding:',' ','orthogonal',
     &               'curvilinear','cancel',IWO,nbhelp)
        if(iwo.ne.3)then
          ortho=.TRUE.

C Grid Zwest-axis for both cases.
          call GRIDDEF(3,NREG,NTCZ,zdis,NCELZ,ZREG,Zplaw,ier)
          if (ier.ne.0) then
            griderr=.TRUE.; zgrid=.FALSE.
          else
            zgrid=.TRUE.
          endif
          if(iwo.eq.2)then

C Additional Z axis must be gridded.
            ortho=.FALSE.
            call usrmsg(
     &        'A non-orthogonal grid requires a Z distribution',
     &        'of points on the East face of the domain. ','W')
            if(ieorg.eq.0) ieorg=1
            CALL EASKI(ieorg,'Specify vertex at start of east Z axis.',
     &        'Which `v`? ',1,'F',NTV,'F',1,'ez axis vertex',
     &        IERI,nbhelp)
            if(ieri.eq.-3) then
              zegrid=.FALSE.
              return
            endif
            izendet=izende(ICFD)
            CALL EASKI(izendet,'Specify vertex at end of east Z axis.',
     &        'Which `v`? ',1,'F',NTV,'F',1,'ez axis vertex',
     &        IERI,nbhelp)
            if(ieri.eq.-3) then
              zegrid=.FALSE.
              return
            endif

C User did not cancel so instantiate values.
            izende(ICFD)=izendet

            zedis=crowxyz(X(ieorg),Y(ieorg),Z(ieorg),X(izende(ICFD)),
     &              Y(izende(ICFD)),Z(izende(ICFD)))

C Need to ask for vertices which represent the
C top and bottom boundaries of the curvilinear x-section.
            call TOPBOT(izone,IER)

C Grid Zeast-axis.
            call GRIDDEF(4,NREG,NTCZe,zedis,NCELZe,ZREGe,Zplawe,ier)
            if (ier.ne.0) then
              griderr=.TRUE.; zegrid=.FALSE.
            else
              zegrid=.TRUE.
            endif
          endif
        endif

C Visualize gridding.
      elseif(INO.EQ.(11+NCP).and.(.NOT.griderr))then
        if(origin.and.xgrid.and.ygrid.and.zgrid)then
          call cgd(izone,ierr)
        else
          CALL USRMSG(' Define origin of CFD domain ',
     &         ' and grid axes first ','W')
        endif

      elseif(INO.EQ.NITMS-1)then
        helptopic='cfd_gridding_setup'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('gridding',nbhelp,'-',0,0,IER)

      elseif(INO.EQ.NITMS-2)then
        itrc=itrc+1           ! toggle feedback level
        if(itrc.gt.2) itrc=0

C Exit menu.
      elseif(INO.EQ.NITMS)then
        if (griderr) CALL USRMSG(' ',
     &    'The current gridding scheme is incomplete!','W')

C Check grid definition and current velocity solution requirements are 
C consistant.
        if (CALLU(ICFD).OR.CALLV(ICFD).OR.CALLW(ICFD)) then
          if (NTCX.eq.1) then
            CALLU(ICFD)=.FALSE.
          else
            CALLU(ICFD)=.TRUE.
          endif
          if (NTCY.eq.1) then
            CALLV(ICFD)=.FALSE.
          else
            CALLV(ICFD)=.TRUE.
          endif
          if (NTCZ.eq.1) then
            CALLW(ICFD)=.FALSE.
          else
            CALLW(ICFD)=.TRUE.
          endif
        endif
        return
      endif
      goto 14

      end
    

C ******************** CFDSVAR ********************
C Controls the editing of CFD solution variables.

      SUBROUTINE CFDSVAR(IER)
#include "building.h"
#include "cfd.h"
#include "help.h"

      COMMON/ICFNOD/ICFD,ICP
      common/grdmax/NTCX,NTCY,NTCZ

C Equations solved.
      common/EQTION/CALLU(MNZ),CALLV(MNZ),CALLW(MNZ),CALLT(MNZ),
     &             CALLC(MNZ),KEMDL(MNZ),BUOY(MNZ),BOUSSI(MNZ),
     &             ZEROT(MNZ),ZandKE(MNZ),MITzero(MNZ)
      common/EQTION3/CALLMA(MNZ),CALPOL(MCTM,MNZ),POLNAM(MCTM,MNZ),
     &               NCTM(MNZ),JHUMINDX(MNZ),URFC(MCTM)

C Solution methods.
      common/METHDS/ITURB(MNZ),IBUOY(MNZ)

      character ITEM(11+MCTM)*40, POLNAM*12
      LOGICAL CALLU,CALLV,CALLW,CALLT,CALLC,KEMDL,BUOY,BOUSSI
      LOGICAL ZEROT,ZandKE,MITzero
      LOGICAL CALPOL,CALLMA,OK
      integer NITMS,INO ! max items and current menu item

      helpinsub='edcfd'  ! set for subroutine

C Check gridding to set velocity solution.
      if (NTCX.gt.1) then
        CALLU(ICFD)=.TRUE.
      else
        CALLU(ICFD)=.FALSE.
      endif
      if (NTCY.gt.1) then
        CALLV(ICFD)=.TRUE.
      else
        CALLV(ICFD)=.FALSE.
      endif
      if (NTCZ.gt.1) then
        CALLW(ICFD)=.TRUE.
      else
        CALLW(ICFD)=.FALSE.
      endif

C Create menu showing 4 main solution categories and allow toggling 
C to change solution parameters. Once a solution category 
C has been activated then display associated state variables and allow 
C definition of initial values and relaxation factors.
 5    do 10 I=1,14
        ITEM(I)='  '
 10   continue

C Menu content.
      ITEM(1)= '  velocity: solved'
      if (.NOT.CALLT(ICFD)) then
        ITEM(2)= 'a temperature >> not solved'
      else
        ITEM(2)= 'a temperature >> solved'
      endif
      if (ITURB(ICFD).eq.0) then
        ITEM(3)= 'b turbulence >> none (laminar)'
      elseif (ITURB(ICFD).eq.1) then
        ITEM(3)= 'b turbulence >> k-e turbulence'
      elseif (ITURB(ICFD).eq.2) then
        ITEM(3)= 'b turbulence >> fixed eddy viscosity'
      elseif (ITURB(ICFD).eq.3) then
        ITEM(3)= 'b turbulence >> MIT zero-equation'
      elseif (ITURB(ICFD).eq.4) then
        ITEM(3)= 'b turbulence >> fixed visc -> k-e'
      endif
      if (IBUOY(ICFD).eq.0) then
        ITEM(4)= 'c buoyancy >> not solved'
      elseif (IBUOY(ICFD).eq.1) then
        ITEM(4)= 'c buoyancy >> ideal gas'
      elseif (IBUOY(ICFD).eq.2) then
        ITEM(4)= 'c buoyancy >> Boussinesq approx'
      endif
      DO 121 ICTM=1,NCTM(ICFD)
        IF(CALPOL(ICTM,ICFD))THEN
          WRITE(ITEM(4+ICTM),'(A,1X,A,1X,A,1X)')CHAR(99+ICTM),
     &         POLNAM(ICTM,ICFD),'>> solved'
        ELSE
          WRITE(ITEM(4+ICTM),'(A,1X,A,1X,A,1X)')CHAR(99+ICTM),
     &         POLNAM(ICTM,ICFD),'>> not solved'
        ENDIF
 121  CONTINUE
      if (.NOT.CALLMA(ICFD)) then
        WRITE(ITEM(5+NCTM(ICFD)),'(2A)')CHAR(100+NCTM(ICFD)),
     &       ' mean age of air >> not solved'
      else
        WRITE(ITEM(5+NCTM(ICFD)),'(2A)')CHAR(100+NCTM(ICFD)),
     &       ' mean age of air >> solved'
      endif
      ITEM(6+NCTM(ICFD))= ' ------------------------------------- '
      ITEM(7+NCTM(ICFD))= '# edit solution parameters'
      ITEM(8+NCTM(ICFD))= '* reset to defaults'
      ITEM(9+NCTM(ICFD))= ' ------------------------------------- '
      ITEM(10+NCTM(ICFD))='? help'
      ITEM(11+NCTM(ICFD))='- exit menu'
      NITMS=11+NCTM(ICFD)

C Help text for this menu.
      helptopic='cfd_equations_to_solve'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Display menu.
      CALL EMENU('Solution parameters',ITEM,NITMS,INO)

C Check for pick of empty box.
      if(INO.EQ.2)then

C Toggle temperature solution.
        if (.NOT.CALLT(ICFD)) then
          CALLT(ICFD)=.TRUE.
        else
          CALLT(ICFD)=.FALSE.
        endif
      elseif(INO.EQ.3)then

C Toggle through turbulence methods.
        ITURB(ICFD)=ITURB(ICFD)+1
        if (ITURB(ICFD).gt.4) ITURB(ICFD)=0
      elseif(INO.EQ.4)then

C Toggle buoyancy.
        IBUOY(ICFD)=IBUOY(ICFD)+1
        if (IBUOY(ICFD).gt.2) IBUOY(ICFD)=0
      elseif(INO.GT.4.AND.INO.LE.(4+NCTM(ICFD)))then

C Contaminants.
        IF(CALPOL(INO-4,ICFD))THEN
          CALPOL(INO-4,ICFD)=.FALSE.
        ELSE
          CALPOL(INO-4,ICFD)=.TRUE.
        ENDIF
      elseif(INO.EQ.5+NCTM(ICFD))then

C Mean age of air.
        if (CALLMA(ICFD)) then
          CALLMA(ICFD)=.false.
        else
          CALLMA(ICFD)=.true.
        endif
      elseif(INO.EQ.8+NCTM(ICFD))then

C Reset defaults.
        call easkok(' ',
     &    'Reset solution parameters to default values?',
     &    OK,nbhelp)
        if (OK) call CFDDEFLT
      elseif(INO.eq.7+NCTM(ICFD))then

C Edit solution parameters.
        call CFDEDAT        
      elseif(INO.eq.(NITMS-1)) then

C Help.
        helptopic='cfd_equations_to_solve'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('solution variables',nbhelp,'-',0,0,IER)
      elseif(INO.eq.NITMS) then
        return
      endif
      goto 5

      end

C ************************ CFDEDAT ************************
C Controls the editing of CFD equations.

      SUBROUTINE CFDEDAT
#include "building.h"
#include "cfd.h"
#include "help.h"

      COMMON/ICFNOD/ICFD,ICP
      common/grdmax/NTCX,NTCY,NTCZ

C Equations solved.
      common/EQTION/CALLU(MNZ),CALLV(MNZ),CALLW(MNZ),CALLT(MNZ),
     &             CALLC(MNZ),KEMDL(MNZ),BUOY(MNZ),BOUSSI(MNZ),
     &             ZEROT(MNZ),ZandKE(MNZ),MITzero(MNZ)
      common/EQTION3/CALLMA(MNZ),CALPOL(MCTM,MNZ),POLNAM(MCTM,MNZ),
     &               NCTM(MNZ),JHUMINDX(MNZ),URFC(MCTM)

C Solution methods.
      common/METHDS/ITURB(MNZ),IBUOY(MNZ)

C Initial values.
      COMMON/INITIA/UINIT(MNZ),VINIT(MNZ),WINIT(MNZ),PINIT(MNZ),
     &              TINIT(MNZ),TEINIT(MNZ),EDINIT(MNZ),POLINIT(MNZ,MCTM)

C Under-relaxation factors.
      COMMON/LINRFC/URFCU(MNZ),URFCV(MNZ),URFCW(MNZ),URFCP(MNZ),
     &              URFCT(MNZ),URFCK(MNZ),URFCE(MNZ),URFCVS(MNZ),
     &              URFCC(MNZ,MCTM)
      COMMON/LINRFC2/URFCU2(MNZ),URFCV2(MNZ),URFCW2(MNZ),URFCP2(MNZ),
     &              URFCT2(MNZ),URFCK2(MNZ),URFCE2(MNZ),URFCVS2(MNZ),
     &              URFCC2(MNZ,MCTM)

C Convergence criteria.
      common/param1/MAXITR(MNZ),IMONT(MNZ),JMONT(MNZ),KMONT(MNZ),
     &             IPPHI(MNZ),SRMAX(MNZ)

C Additional data needed for some models.
      COMMON/ZTURB/rMOOT(MNZ),nZtoKE(MNZ)
      COMMON/BUOYAN/BUOYA,BOUSSA,TBAR(MNZ)
      LOGICAL BUOYA,BOUSSA
 
      character ITEM(33)*40,ltmp*16,dtmp*16, POLNAM*12

      LOGICAL CALLU,CALLV,CALLW,CALLT,CALLC,KEMDL,BUOY,BOUSSI
      LOGICAL ZEROT,ZandKE,MITzero
      LOGICAL CALPOL,CALLMA
      integer NITMS,INO ! max items and current menu item

      helpinsub='edcfd'  ! set for subroutine

C Check gridding to set velocity solution.
      if (NTCX.gt.1) then
        CALLU(ICFD)=.TRUE.
      else
        CALLU(ICFD)=.FALSE.
      endif
      if (NTCY.gt.1) then
        CALLV(ICFD)=.TRUE.
      else
        CALLV(ICFD)=.FALSE.
      endif
      if (NTCZ.gt.1) then
        CALLW(ICFD)=.TRUE.
      else
        CALLW(ICFD)=.FALSE.
      endif

C Create menu showing 4 main solution categories and allow toglling 
C to activate various categories.  Once a solution category 
C has been activated then display associated state variables and allow 
C definition of initial values and relaxation factors.
 5    INO=-4
      do 10 I=1,31
        ITEM(I)='  '
 10   continue

C Clear display area.
      call usrmsg('  ','  ','-')

C Now create menu.
      ITEM(1)= ' ----- Velocity in X direction ----- '
      if (CALLU(ICFD)) then
        write(ITEM(2),'(a,f6.3)')'a initial value: ',UINIT(ICFD)
        write(ITEM(3),'(a,f5.2)')'b high relaxation factor: ',
     &                                                 URFCU(ICFD)
        write(ITEM(4),'(a,f5.2)')'c low relaxation factor: ',
     &                                                 URFCU2(ICFD)
      else
        ITEM(2)='  N/A (not solved)'
        ITEM(3)='  N/A (not solved)'
        ITEM(4)='  N/A (not solved)'
      endif
      ITEM(5)= ' ----- Velocity in Y direction ----- '
      if (CALLV(ICFD)) then
        write(ITEM(6),'(a,f6.3)')'d initial value: ',VINIT(ICFD)
        write(ITEM(7),'(a,f5.2)')'e high relaxation factor: ',
     &                                                 URFCV(ICFD)
        write(ITEM(8),'(a,f5.2)')'f low relaxation factor: ',
     &                                                 URFCV2(ICFD)
      else
        ITEM(6)='  N/A (not solved)'
        ITEM(7)='  N/A (not solved)'
        ITEM(8)='  N/A (not solved)'
      endif
      ITEM(9)= ' ----- Velocity in Z direction ----- '
      if (CALLW(ICFD)) then
        write(ITEM(10),'(a,f6.3)')'g initial value: ',WINIT(ICFD)
        write(ITEM(11),'(a,f5.2)')'h high relaxation factor: ',
     &                                                  URFCW(ICFD)
        write(ITEM(12),'(a,f5.2)')'i low relaxation factor: ',
     &                                                  URFCW2(ICFD)
      else
        ITEM(10)='  N/A (not solved)'
        ITEM(11)='  N/A (not solved)'
        ITEM(12)='  N/A (not solved)'
      endif
      ITEM(13)=' ----------- Temperature ----------- '
      if (CALLT(ICFD)) then
        ITEM(14)='j temperature >> solved'
      else
        ITEM(14)='j temperature >> not solved'
      endif
      write(ITEM(15),'(a,f6.3)')'k initial value: ',TINIT(ICFD)
      write(ITEM(16),'(a,f5.2)')'l high relaxation factor: ',URFCT(ICFD)
      write(ITEM(17),'(a,f5.2)')'m low relaxation factor: ',URFCT2(ICFD)
      ITEM(18)=' ----------- Turbulence ------------ '
      if (ITURB(ICFD).eq.0) then
        ITEM(19)= 'n turbulence >> none'
      elseif (ITURB(ICFD).eq.1) then
        ITEM(19)= 'n turbulence >> k-e'
        write(ITEM(20),'(a,E7.2E1,a,E7.2E1)')'o initial value - k: ',
     &    TEINIT(ICFD),'; e: ',EDINIT(ICFD)
        write(ITEM(21),'(a,f5.2,a,f5.2)')'p high relx. factor - k: ',
     &    URFCK(ICFD),'; e: ',URFCE(ICFD)
        write(ITEM(22),'(a,f5.2,a,f5.2)')'q low relax. factor - k:',
     &    URFCK2(ICFD),'; e: ',URFCE2(ICFD)
      elseif (ITURB(ICFD).eq.2) then
        ITEM(19)= 'n turbulence >> fixed eddy viscosity'
        write (ITEM(20),'(a,f6.2)') 'o eddy viscosity: ',rMOOT(ICFD)
      elseif (ITURB(ICFD).eq.3) then
        ITEM(19)= 'n turbulence >> MIT zero-equation'
      elseif (ITURB(ICFD).eq.4) then
        ITEM(19)= 'n turbulence >> fixed viscosity -> k-e'
        write(ITEM(20),'(a,f6.2)')'o eddy viscosity: ',rMOOT(ICFD)
        write(ITEM(21),'(a,i5)')'p transition on iteration: ',
     &                                                     nZtoKE(ICFD)
        write(ITEM(22),'(a)')'q toggle to k-e to set parameters'
      endif
      ITEM(23)=' ------------- Buoyancy ------------ '
      if (IBUOY(ICFD).eq.0) then
        ITEM(24)= 'r buoyancy >> not solved'
      elseif (IBUOY(ICFD).eq.1) then
        ITEM(24)= 'r buoyancy >> ideal gas'
      elseif (IBUOY(ICFD).eq.2) then
        ITEM(24)= 'r buoyancy >> Boussinesq approx.'
        write(ITEM(25),'(a,f6.2)')'s Reference temperature: ',
     &                                                       TBAR(ICFD)
      endif
      ITEM(26)=' ------- Convergence criteria ------ '
      write(ITEM(27),'(a,i5)')  't maximum iterations: ',MAXITR(ICFD)
      write(ITEM(28),'(a,f7.5)')'u maximum residual: ',SRMAX(ICFD)
      write(ITEM(29),'(a,3i4)') 'v monitored cell (i,j,k): ',
     &                        IMONT(ICFD)-1,JMONT(ICFD)-1,KMONT(ICFD)-1
      ITEM(30)=' ----------------------------------- '
      write(ITEM(31),'(a)')'+ contaminants'
      ITEM(32)='? help'
      ITEM(33)='- exit menu'

      NITMS=33

C Help text for this menu.
      helptopic='cfd_equations_to_edit'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Display menu.
      CALL EMENU('Solution parameters',ITEM,NITMS,INO)

C Ask for initial value of velocity in X direction.
      if(INO.EQ.2)then
        VAL=UINIT(ICFD)
        CALL EASKR(VAL,' ','Initial value for X velocity?',-1.0,
     &             'W',1.0,'W',0.001,'U init',IER,nbhelp)
        if (IER.eq.0) UINIT(ICFD)=VAL

C Ask for high relaxation factor for velocity in X direction.
      elseif(INO.EQ.3)then
        VAL=URFCU(ICFD)
        CALL EASKR(VAL,'High relaxation factor for',
     &   'X velocity?',0.0,'F',1.5,'W',0.5,'U relax 1',IER,nbhelp)
        if (IER.eq.0) URFCU(ICFD)=VAL

C Ask for Low relaxation factor for velocity in X direction.
      elseif(INO.EQ.4)then
        VAL=URFCU2(ICFD)
        CALL EASKR(VAL,'Low relaxation factor for',
     &   'X velocity?',0.0,'F',1.5,'W',0.05,'U relax 2',IER,nbhelp)
        if (IER.eq.0) URFCU2(ICFD)=VAL

C Ask for initial value of velocity in Y direction.
      elseif(INO.EQ.6)then
        VAL=VINIT(ICFD)
        CALL EASKR(VAL,' ','Initial value for Y velocity?',-1.0,
     &             'W',1.0,'W',0.001,'V init',IER,nbhelp)
        if (IER.eq.0) VINIT(ICFD)=VAL

C Ask for high relaxation factor for velocity in Y direction.
      elseif(INO.EQ.7)then
        VAL=URFCV(ICFD)
        CALL EASKR(VAL,'High relaxation factor for',
     &   'Y velocity?',0.0,'F',1.5,'W',0.5,'V relax 1',IER,nbhelp)
        if (IER.eq.0) URFCV(ICFD)=VAL

C Ask for low relaxation factor for velocity in Y direction.
      elseif(INO.EQ.8)then
        VAL=URFCV2(ICFD)
        CALL EASKR(VAL,'Low relaxation factor for',
     &   'Y velocity?',0.0,'F',1.5,'W',0.05,'V relax 2',IER,nbhelp)
        if (IER.eq.0) URFCV2(ICFD)=VAL

C Ask for initial value of velocity in Z direction.
      elseif(INO.eq.10)then
        VAL=WINIT(ICFD)
        CALL EASKR(VAL,' ','Initial value for Z velocity?',-1.0,
     &            'W',1.0,'W',0.001,'W init',IER,nbhelp)
        if (IER.eq.0) WINIT(ICFD)=VAL

C Ask for high relaxation factor for velocity in Z direction.
      elseif(INO.EQ.11)then
        VAL=URFCW(ICFD)
        CALL EASKR(VAL,'High relaxation factor for',
     &   'Z velocity?',0.0,'F',1.5,'W',0.5,'W relax 1',IER,nbhelp)
        if (IER.eq.0) URFCW(ICFD)=VAL

C Ask for low relaxation factor for velocity in Z direction.
      elseif(INO.EQ.12)then
        VAL=URFCW2(ICFD)
        CALL EASKR(VAL,'Low relaxation factor for',
     &   'Z velocity?',0.0,'F',1.5,'W',0.05,'W relax 2',IER,nbhelp)
        if (IER.eq.0) URFCW2(ICFD)=VAL

C (De)activate temperature solution.
      elseif(INO.eq.14)then      
        if (.NOT.CALLT(ICFD)) then
          CALLT(ICFD)=.TRUE.
        else
          CALLT(ICFD)=.FALSE.
        endif

C Ask for initial value of temperature.
      elseif(INO.eq.15)then
        VAL=TINIT(ICFD)
        CALL EASKR(VAL,' ','Initial value for temperature?',
     &                         10.,'W',30.,'W',20.,'T init',IER,nbhelp)
        if (IER.eq.0) TINIT(ICFD)=VAL

C Ask for high relaxation factor for temperature.
      elseif(INO.EQ.16)then
        VAL=URFCT(ICFD)
        CALL EASKR(VAL,'High relaxation factor for',
     &   'temperature?',0.0,'F',1.5,'W',1.0,'T relax 1',IER,nbhelp)
        if (IER.eq.0) URFCT(ICFD)=VAL

C Ask for low relaxation factor for temperature.
      elseif(INO.EQ.17)then
        VAL=URFCT2(ICFD)
        CALL EASKR(VAL,'Low relaxation factor for',
     &   'temperature?',0.0,'F',1.5,'W',0.25,'T relax 2',IER,nbhelp)
        if (IER.eq.0) URFCT2(ICFD)=VAL

C Toggle through turbulence methods.
      elseif(INO.eq.19)then   
        ITURB(ICFD)=ITURB(ICFD)+1
        if (ITURB(ICFD).gt.4) ITURB(ICFD)=0

C Edit turbulence data according to current method.
      elseif(INO.EQ.20)then
        if (ITURB(ICFD).eq.1) then

C Ask for initial values for k and epsilon.
          VAL=TEINIT(ICFD)
          CALL EASKE(VAL,' ','Initial value for turbulent energy?',
     &                      0.0,'W',0.1,'W',0.005,'TE init',IER,nbhelp)
          if (IER.eq.0) TEINIT(ICFD)=VAL
          VAL=EDINIT(ICFD)
          CALL EASKE(VAL,' ','Initial value for energy dissipation?',
     &                      0.0,'W',0.1,'W',0.005,'ED init',IER,nbhelp)
          if (IER.eq.0) EDINIT(ICFD)=VAL
        elseif ((ITURB(ICFD).eq.2).or.(ITURB(ICFD).eq.4)) then

C Ask for fixed eddy viscosity.
          VAL=rMOOT(ICFD)
          CALL EASKR(VAL,' ','Fixed value for eddy viscosity?',0.0,'F',
     &               500.0,'F',90.0,'eddy viscos',IER,nbhelp)
          if (IER.eq.0) rMOOT(ICFD)=VAL
        endif

C Edit turbulence data according to current method.
      elseif(INO.EQ.21)then
        if (ITURB(ICFD).eq.1) then

C Ask for high relaxation factors for k and epsilon.
          VAL=URFCK(ICFD)
          CALL EASKR(VAL,'High relaxation factor for',
     & 'turbulent energy?',0.0,'F',1.5,'W',1.0,'TE relax 1',IER,nbhelp)
          if (IER.eq.0) URFCK(ICFD)=VAL
          VAL=URFCE(ICFD)
          CALL EASKR(VAL,'High relaxation factor for',
     &      'energy dissipation?',0.0,'F',1.5,'W',1.0,'ED relax 1',IER,
     &                                                          nbhelp)
          if (IER.eq.0) URFCE(ICFD)=VAL

C Ask for iteration to transition from fixed eddy viscosity to k-e.  
        elseif (ITURB(ICFD).eq.4) then
          IVAL=nZtoKE(ICFD)
          CALL EASKI(IVAL,'Iteration number to transition to',
     &      'k-e model?',1,'F',1000,'W',10,'fev->ke iter',IER,nbhelp)
          if (IER.eq.0) nZtoKE(ICFD)=IVAL
        endif

C Edit turbulence data according to current method.
      elseif(INO.EQ.22)then

C Ask for low relaxation factors for k and epsilon.
        if (ITURB(ICFD).eq.1) then
          VAL=URFCK2(ICFD)
          CALL EASKR(VAL,'Low relaxation factor for',
     &       'turbulent energy?',0.0,'F',1.5,'W',0.05,'TE relax 2',IER,
     &                                                          nbhelp)
          if (IER.eq.0) URFCK2(ICFD)=VAL
          VAL=URFCE2(ICFD)
          CALL EASKR(VAL,'Low relaxation factor for',
     &     'energy dissipation?',0.0,'F',1.5,'W',0.05,'ED relax 2',IER,
     &                                                          nbhelp)
          if (IER.eq.0) URFCE2(ICFD)=VAL

C Toggle to k-e.
        elseif (ITURB(ICFD).eq.4) then
          ITURB(ICFD)=1
        endif

C Toggle buoyancy.
      elseif(INO.EQ.24)then
        IBUOY(ICFD)=IBUOY(ICFD)+1
        if (IBUOY(ICFD).gt.2) IBUOY(ICFD)=0

      elseif(INO.EQ.25)then

C Ask for reference temperature.
        if (IBUOY(ICFD).eq.2) then
          VAL=TBAR(ICFD)
          CALL EASKR(VAL,' ','Reference temperature?',10.,
     &                               'W',30.,'W',20.,'Tbar',IER,nbhelp)
          if (IER.eq.0) TBAR(ICFD)=VAL
        endif

C Ask for maximum number of iterations.
      elseif(INO.EQ.27)then
        helptopic='cfd_convergence_crit'
        call gethelptext(helpinsub,helptopic,nbhelp)
        IVAL=MAXITR(ICFD)
        CALL EASKI(IVAL,' ','Maximum number of iterations?',
     &             1,'F',10000,'W',3750,'CFD iters',IER,nbhelp)
        if (IER.eq.0) MAXITR(ICFD)=IVAL

C Ask for maximum residual.
      elseif (INO.eq.28) then
        helptopic='cfd_convergence_crit'
        call gethelptext(helpinsub,helptopic,nbhelp)
        VAL=SRMAX(ICFD)
        CALL EASKR(VAL,' ','Maximum residual?',0.,'F',1.,'F',
     &                                   0.01,'max resid',IER,nbhelp)
        if (IER.eq.0) SRMAX(ICFD)=VAL
C Ask for monitored cell.
      elseif (INO.eq.29) then
        write (ltmp,'(3i4)') IMONT(ICFD)-1,JMONT(ICFD)-1,KMONT(ICFD)-1
        write (dtmp,'(a)') ' 1  1  1'
        call EASKS(ltmp,' ','Monitored cell (i,j,k)?',16,
     &                                    dtmp,'monit cell',IER,nbhelp)
        K=0
        call EGETWI(ltmp,K,IVAL,0,NTCELX,'W','I mon cell',IER)
        if (IER.eq.0) IMONT(ICFD)=IVAL+1
        call EGETWI(ltmp,K,IVAL,0,NTCELY,'W','J mon cell',IER)
        if (IER.eq.0) JMONT(ICFD)=IVAL+1
        call EGETWI(ltmp,K,IVAL,0,NTCELZ,'W','K mon cell',IER)
        if (IER.eq.0) KMONT(ICFD)=IVAL+1

C Check that contaminants have been defined.
      elseif (INO.eq.NITMS-2) then

C Go to contaminant equation setup menu.
        if (NCTM(ICFD).gt.0) then
          CALL CFDEDATC

C Display popup.
        else
          helptopic='cfd_no_contam_defined'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL PHELPD('No contaminants defined',nbhelp,'-',0,0,IER)
        endif 

C Menu help.       
      elseif(INO.eq.(NITMS-1)) then
        helptopic='cfd_equations_to_edit'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('Solution parameters',nbhelp,'-',0,0,IER)

C Exit.
      elseif(INO.eq.NITMS) then
        return
      endif
      goto 5

      end

C ******************** CFDEDATC ********************
C A continuation of CFDEDAT, controlling editing of
C CFD contaminant equations.

      SUBROUTINE CFDEDATC
#include "building.h"
#include "cfd.h"
#include "help.h"

      COMMON/ICFNOD/ICFD,ICP

C Equations solved.
      common/EQTION3/CALLMA(MNZ),CALPOL(MCTM,MNZ),POLNAM(MCTM,MNZ),
     &               NCTM(MNZ),JHUMINDX(MNZ),URFC(MCTM)

C Under-relaxation factors.
      COMMON/LINRFC/URFCU(MNZ),URFCV(MNZ),URFCW(MNZ),URFCP(MNZ),
     &              URFCT(MNZ),URFCK(MNZ),URFCE(MNZ),URFCVS(MNZ),
     &              URFCC(MNZ,MCTM)
      COMMON/LINRFC2/URFCU2(MNZ),URFCV2(MNZ),URFCW2(MNZ),URFCP2(MNZ),
     &              URFCT2(MNZ),URFCK2(MNZ),URFCE2(MNZ),URFCVS2(MNZ),
     &              URFCC2(MNZ,MCTM)

      character ITEM(3+3*NCTM(ICFD))*40,POLNAM*12,tstr*13,tstr1*30,
     &  tstr2*30

      LOGICAL CALPOL,CALLMA
      integer NITMS,INO ! max items and current menu item

      helpinsub='edcfd'  ! set for subroutine

      NITMS=3+3*NCTM(ICFD)

C First clear menu.
 5    INO=-4
      do I=1,NITMS
        ITEM(I)='  '
      enddo

C Create menu.
      do ICTM=1,NCTM(ICFD)
        IND=(ICTM-1)*3+1
        IF(CALPOL(ICTM,ICFD))THEN
          WRITE(ITEM(IND),'(A,1X,A,1X,A,1X)')CHAR(96+IND),
     &       POLNAM(ICTM,ICFD)(1:lnblnk(POLNAM(ICTM,ICFD))),'>> solved'
        ELSE
          WRITE(ITEM(IND),'(A,1X,A,1X,A,1X)')CHAR(96+IND),
     &   POLNAM(ICTM,ICFD)(1:lnblnk(POLNAM(ICTM,ICFD))),'>> not solved'
        ENDIF
        IND=(ICTM-1)*3+2
        write(ITEM(IND),'(a,1x,a,f5.2)')CHAR(96+IND),
     &                         'Relaxation factor 1: ',URFCC(ICFD,ICTM)
        IND=(ICTM-1)*3+3
        write(ITEM(IND),'(a,1x,a,f5.2)')CHAR(96+IND),
     &                        'Relaxation factor 2: ',URFCC2(ICFD,ICTM)
      enddo
      ITEM(NITMS-2)=' ------------------------------ '
      ITEM(NITMS-1)='? help'
      ITEM(NITMS)=  '- exit menu'

C Help text for this menu.
      helptopic='cfd_contam_equ_to_edit'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Display menu.
      CALL EMENU('Contaminant relaxation factors',ITEM,NITMS,INO)

C Process INO to get ICTM and option 1, 2 or 3.
      ICTM=(INO+2)/3
      IND=INO-(ICTM-1)*3

C Create temporary strings.
      write(tstr,'(2a)')POLNAM(ICTM,ICFD)(1:lnblnk(POLNAM(ICTM,ICFD))),
     &                                                              '?'
      write(tstr1,'(a,i1,a)')'contam ',ICTM,' relax 1'
      write(tstr2,'(a,i1,a)')'contam ',ICTM,' relax 2'
      
C Check for Help or Exit.
      IF(INO.eq.(NITMS-1))THEN

C Help text for this menu.
        helptopic='cfd_contam_equ_to_edit'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('Solution parameters (contam.)',nbhelp,'-',0,0,IER)
      ELSEIF(INO.eq.NITMS)THEN

C Exit.
        return
      ELSEIF(IND.eq.1)THEN

C Toggle contaminant equation.
        if (CALPOL(ICTM,ICFD)) then
          CALPOL(ICTM,ICFD)=.false.
        else
          CALPOL(ICTM,ICFD)=.true.
        endif
      ELSEIF(IND.eq.2)THEN

C Ask for relaxation factor 1.
        VAL=URFCC(ICFD,ICTM)
        CALL EASKR(VAL,'Relaxation factor 1 for',
     &                       tstr,0.0,'F',1.5,'W',1.0,tstr1,IER,nbhelp)
        if (IER.eq.0) URFCC(ICFD,ICTM)=VAL
      ELSEIF(IND.eq.3)THEN

C Ask for relaxation factor 2.
        VAL=URFCC2(ICFD,ICTM)
        CALL EASKR(VAL,'Relaxation factor 2 for',
     &                      tstr,0.0,'F',1.5,'W',0.25,tstr2,IER,nbhelp)
        if (IER.eq.0) URFCC2(ICFD,ICTM)=VAL
      ENDIF

      goto 5
      END      

C ******************** CFDBVAR ********************
C Controls the editing of CFD boundary parameters.

      SUBROUTINE CFDBVAR(IZONE,IER)
#include "building.h"
#include "cfd.h"
#include "epara.h"
#include "help.h"

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/ICFNOD/ICFD,ICP
      COMMON/OUTIN/IUOUT,IUIN,IEOUT   
      common/cfdconf/ICFBLD(MNZ),ICFMFS(MNZ)
      common/KEYVOLS/NVOL(MNZ),IVOLF(MNVLS,MNZ),IVCELLS(MNVLS,MNZ,2),
     &               JVCELLS(MNVLS,MNZ,2),KVCELLS(MNVLS,MNZ,2)
      common/KEYVOLN/VOLNAME(MNVLS,MNZ),VCsurf(MNVLS,MNZ),
     &               BLKSURF(MNVLS,MNZ,6)
      common/KEYVDAT/IVTYPE(MNVLS,MNZ),VOLTemp(MNVLS,MNZ),
     &          VOLHeat(MNVLS,MNZ),IVConfl(MNVLS,MNZ),VOLHum(MNVLS,MNZ),
     &          VOLCO2(MNVLS,MNZ),VOLVel(MNVLS,MNZ),VOLDir(MNVLS,MNZ,2),
     &          VOLArea(MNVLS,MNZ),VOLPres(MNVLS,MNZ),
     &          VOLPol(MCTM,MNVLS,MNZ)
      common/GRIDEFN/origin,xgrid,ygrid,zgrid,zegrid,ortho
      COMMON/GFONT/IFS,ITFS,IMFS

      dimension IDEL(MNVLS)

      character ITEM(MNVLS+8)*40
      character KEY*1,t24*24
      character VOLNAME*12,VCsurf*12,BLKSURF*12,t12*12,d12*12
      character NAMES(MNVLS)*12
      CHARACTER*72 TITLE,CFTRFL,LPHI
      logical origin,xgrid,ygrid,zgrid,zegrid,ortho,show
      integer INB     ! for radio button
      integer NITMS,INO,IAIN ! max items and current menu item
      real dum(1),dums(MCTM)

      helpinsub='edcfd'  ! set for subroutine

C Set up multi-page menu.
 4    MHEAD=3
      MCTL=5
      ILEN=NVOL(ICFD)
      IPACT=CREATE
      CALL EKPAGE(IPACT)

C Menu entry setup.
 5    IER=0

C Create menu text.
      M=MHEAD
      ITEM(1)='1 infer BCs from geometry'
      ITEM(2)='2 place occupant BCs'
      ITEM(3)=' --------------------'
      do 10 I=1,ILEN
        IF(I.GE.IST.AND.(I.LE.(IST+MIFULL)))THEN
          M=M+1
          call EMKEY(I,KEY,IER)
          t24='  '
          if (IVTYPE(I,ICFD).eq.1) then
            t24='Solid   | Temp'
          elseif (IVTYPE(I,ICFD).eq.2) then
            t24='Solid   | Heat'
          elseif (IVTYPE(I,ICFD).eq.3) then
            t24='Solid   | Symmetrical'
          elseif (IVTYPE(I,ICFD).eq.4) then
            t24='Solid   | Conflated'
          elseif (IVTYPE(I,ICFD).eq.5) then
            t24='Solid   | Conflated'
          elseif (IVTYPE(I,ICFD).eq.6) then
            t24='Solid   | Conflated'
          elseif (IVTYPE(I,ICFD).eq.10) then
            t24='Opening | Pressure'
            IF (ICFMFS(ICFD).EQ.1)T24='Opening | Mass flow nwk'
          elseif (IVTYPE(I,ICFD).eq.11) then
            t24='Opening | Velocity'
            IF (ICFMFS(ICFD).EQ.1)T24='Opening | Mass flow nwk'
          elseif (IVTYPE(I,ICFD).eq.12) then
            t24='Opening | Zero gradient'
            IF (ICFMFS(ICFD).EQ.1)T24='Opening | Mass flow nwk'
          elseif (IVTYPE(I,ICFD).eq.13) then
            t24='Opening | Mass flow nwk'
          elseif (IVTYPE(I,ICFD).eq.20) then
            t24='Source  | Contaminant'
          elseif (IVTYPE(I,ICFD).eq.30) then
            t24='Blockage| Heat flux'
          elseif (IVTYPE(I,ICFD).eq.31) then
            t24='Blockage| Temperature'
          elseif (IVTYPE(I,ICFD).eq.32) then
            t24='Blockage| Conflated'
          elseif (IVTYPE(I,ICFD).eq.33) then
            t24='Blockage| Conflated'
          elseif (IVTYPE(I,ICFD).eq.34) then
            t24='Blockage| Casual gain'
          elseif (IVTYPE(I,ICFD).eq.35) then
            t24='Blockage| Occupant'
          else
            t24='UNKNOWN'
          endif
          write (ITEM(M),'(a,1x,3a)') KEY,VOLNAME(I,ICFD),':',t24
        endif
 10   continue

C Number of actual items displayed.
      NITMS=M+MCTL

C If a long list include page facility text.      
      IF(IPFLG.EQ.0)THEN  
        ITEM(M+1)=' --------------------'
      ELSE
        WRITE(ITEM(M+1),'(a,i2,a,i2,a)')'0 Page: ',IPM,' of ',MPM,
     &    ' --------'
      ENDIF
      ITEM(M+2)='+ add/ delete boundary definition'
      ITEM(M+3)='! grid parameters'
      ITEM(M+4)='? help'
      ITEM(M+5)='- exit menu'

      IF(MMOD.EQ.8)THEN

C Recalculate gridding in case this has changed.
        CALL INICNT
        CALL GRID(ier)
        call NEW2OLD

        call redraw(ier)
      ENDIF

C Help text for this menu.
      helptopic='cfd_boundary_variables'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Set fixed width font.
      itmp=IMFS
      if(IMFS.eq.4) IMFS=0
      if(IMFS.eq.5) IMFS=1
      if(IMFS.eq.6) IMFS=2
      if(IMFS.eq.7) IMFS=3
      call userfonts(IFS,ITFS,IMFS)

C Display menu.
      INO=-1
      CALL EMENU('Boundary conditions',ITEM,NITMS,INO)
      
C Return to previous font.
      IMFS=itmp
      call userfonts(IFS,ITFS,IMFS)

C Do things in reverse order.
      if (INO.eq.NITMS) then

        return
      elseif (INO.eq.(NITMS-1)) then

C Help.
        helptopic='cfd_boundary_variables'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('boundary conditions',nbhelp,'-',0,0,IER)
      elseif (INO.eq.(NITMS-2)) then

C View the gridding.
        if (origin.and.xgrid.AND.ygrid.AND.zgrid) then
          call cgd(izone,ierr)
        else
          CALL USRMSG('Define origin of CFD domain',
     &         'and grid axes first.','W')
        endif

      elseif (INO.eq.(NITMS-3)) then

C Add/delete a boundary definition.
        call easkabc(' ','Boundary condition:','add',
     &               'delete','cancel',IAD,nbhelp)
        if (IAD.eq.1) then
          INB=1
          call EASKATOG(' ','Type of boundary?',
     &       'air flow opening','solid surface','source',
     &       'blockage','cancel',' ',' ',INB,nbhelp)

C Create boundary condition and call relevant editing routine.
          dum=(/0/)
          show=.false.
          t12='            '
          if (INB.eq.1) then

C Air flow opening.
            write (t12,'(a,i3.3)') 'Open',NVOL(ICFD)+1
            d12='  '
            call EASKS(t12,' ','Boundary condition name?',12,d12,
     &                     'BC name',IER,nbhelp)
            if (IER.eq.0) then
              call st2name(t12,d12)
            endif
            call MKCFDBC(show,11,0,0,0,0,0,0,0,d12,t12,dum,1,
     &        20.0,0.0,0.0,0.0,0.0,IER)
            call EDAIRO(NVOL(ICFD),IER)
            call NEW2OLD            
          elseif (INB.eq.2) then

C A solid boundary has been added.
            write (t12,'(a,i3.3)') 'Wall',NVOL(ICFD)+1
            d12='  '
            call EASKS(t12,' ','Boundary condition name?',12,d12,
     &                     'BC name',IER,nbhelp)
            if (IER.eq.0) then
              call st2name(t12,d12)
            endif
            call MKCFDBC(show,1,0,0,0,0,0,0,0,d12,t12,dum,1,
     &        20.0,0.0,0.0,0.0,0.0,IER)
            call EDSLDB(IZONE,NVOL(ICFD),IER)
            
          elseif (INB.eq.3) then

C A source has been added.
            write (t12,'(a,i3.3)') 'Source',NVOL(ICFD)+1
            d12='  '
            call EASKS(t12,' ','Boundary condition name?',12,d12,
     &                        'BC name',IER,nbhelp)
            if (IER.eq.0) then
              call st2name(t12,d12)
            endif
            do ictm=1,MCTM
              dums(ictm)=0.0
            enddo
            call MKCFDBC(show,20,9,0,0,0,0,0,0,d12,t12,dums,MCTM,
     &        0.0,0.0,0.0,0.0,0.0,IER)
            call EDSRC(NVOL(ICFD),IER)
            call NEW2OLD
            
          elseif (INB.eq.4) then

C A blockage has been added.
            write (t12,'(a,i3.3)') 'Block',NVOL(ICFD)+1
            d12='  '
            call EASKS(t12,' ','Boundary condition name?',12,d12,
     &                        'BC name',IER,nbhelp)
            if (IER.eq.0) then
              call st2name(t12,d12)
            endif
            call MKCFDBC(show,30,9,0,0,0,0,0,0,d12,t12,dum,1,
     &        0.0,0.0,0.0,0.0,0.0,IER)
            call EDBLK(NVOL(ICFD),IER)
            call NEW2OLD
          endif
        elseif (IAD.eq.2) then

C Delete a boundary condition.
          if (NVOL(ICFD).gt.0) then
            INDEL=1
            do 233 I=1,NVOL(ICFD)
              NAMES(I)=VOLNAME(I,ICFD)
 233        continue
            INVOL=NVOL(ICFD)
            call EPICKS(INDEL,IDEL,' ',' ',12,INVOL,NAMES,
     &        'Key volume name',IER,nbhelp)
            CALL DELCFDBC(IDEL(1))
          endif
        endif
        goto 4

      elseif (INO.eq.(NITMS-4)) then
 
C If there are enough items, allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        ENDIF
          
      elseif (INO.GT.MHEAD.AND.INO.LT.(NITMS-MCTL+1)) then

C Edit volume identified by KEYIND.
        CALL KEYIND(NITMS,INO,IAIN,IO)

C Existing volume, edit according to type.
        if (IVTYPE(IAIN,ICFD).ge.1.AND.IVTYPE(IAIN,ICFD).lt.10) then
          call EDSLDB(IZONE,IAIN,IER)
        elseif(IVTYPE(IAIN,ICFD).ge.10.and.IVTYPE(IAIN,ICFD).lt.20)then
          call EDAIRO(IAIN,IER)
        elseif(IVTYPE(IAIN,ICFD).ge.20.and.IVTYPE(IAIN,ICFD).lt.30)then
          call EDSRC(IAIN,IER)
          goto 4
        elseif(IVTYPE(IAIN,ICFD).ge.30.and.IVTYPE(IAIN,ICFD).lt.40)then
          call EDBLK(IAIN,IER)
        endif

C Infer boundary conditions from geometry.
      elseif (INO.eq.1) then
        CALL GENCFDBC(3,IER)
        if (IER.ne.0) then
          CALL EDISP(IUOUT,'Error during BC generation.')
        endif
        goto 4
        
C Place occupant BCs.
      elseif (INO.eq.2) then
        CALL GENCFDBC(4,IER)
        if (IER.ne.0) then
          CALL EDISP(IUOUT,'Error during BC generation.')
        endif
        goto 4

      endif
      goto 5

      end

C ******************** DELCFDBC ********************
C Deletes a CFD boundary condition (with index ibc).

      SUBROUTINE DELCFDBC(ibc)
#include "building.h"
#include "cfd.h"

      COMMON/ICFNOD/ICFD,ICP
      common/KEYVOLS/NVOL(MNZ),IVOLF(MNVLS,MNZ),IVCELLS(MNVLS,MNZ,2),
     &               JVCELLS(MNVLS,MNZ,2),KVCELLS(MNVLS,MNZ,2)
      common/KEYVOLN/VOLNAME(MNVLS,MNZ),VCsurf(MNVLS,MNZ),
     &               BLKSURF(MNVLS,MNZ,6)
      character VOLNAME*12,VCsurf*12,BLKSURF*12
      common/KEYVDAT/IVTYPE(MNVLS,MNZ),VOLTemp(MNVLS,MNZ),
     &          VOLHeat(MNVLS,MNZ),IVConfl(MNVLS,MNZ),VOLHum(MNVLS,MNZ),
     &          VOLCO2(MNVLS,MNZ),VOLVel(MNVLS,MNZ),VOLDir(MNVLS,MNZ,2),
     &          VOLArea(MNVLS,MNZ),VOLPres(MNVLS,MNZ),
     &          VOLPol(MCTM,MNVLS,MNZ)
      COMMON/CTDFAF/ICTDFAF(MNZ),SRCE(MNVLS,MCTM,MNZ),ICCSRC(MNZ),
     &              ICC2NC(MCTM,MNZ),SRCFRC(MNVLS,MCTM,MNZ)
      character SRCE*12

C Move all definitions greater than selected volume up one slot.
      do I=ibc,NVOL(ICFD)
        if (I.lt.NVOL(ICFD)) then
          IVOLF(I,ICFD)=IVOLF(I+1,ICFD)
          IVCELLS(I,ICFD,1)=IVCELLS(I+1,ICFD,1)
          IVCELLS(I,ICFD,2)=IVCELLS(I+1,ICFD,2)
          JVCELLS(I,ICFD,1)=JVCELLS(I+1,ICFD,1)
          JVCELLS(I,ICFD,2)=JVCELLS(I+1,ICFD,2)
          KVCELLS(I,ICFD,1)=KVCELLS(I+1,ICFD,1)
          KVCELLS(I,ICFD,2)=KVCELLS(I+1,ICFD,2)
          VOLNAME(I,ICFD)=VOLNAME(I+1,ICFD)
          VCsurf(I,ICFD)=VCsurf(I+1,ICFD)
          IVTYPE(I,ICFD)=IVTYPE(I+1,ICFD)
          VOLTemp(I,ICFD)=VOLTemp(I+1,ICFD)
          VOLHeat(I,ICFD)=VOLHeat(I+1,ICFD)
          IVConfl(I,ICFD)=IVConfl(I+1,ICFD)
          VOLHum(I,ICFD)=VOLHum(I+1,ICFD)
          VOLCO2(I,ICFD)=VOLCO2(I+1,ICFD)
          VOLVel(I,ICFD)=VOLVel(I+1,ICFD)
          VOLDir(I,ICFD,1)=VOLDir(I+1,ICFD,1)
          VOLDir(I,ICFD,2)=VOLDir(I+1,ICFD,2)
          VOLArea(I,ICFD)=VOLArea(I+1,ICFD)
          VOLPres(I,ICFD)=VOLPres(I+1,ICFD)
          do J=1,MCTM
            VOLPol(J,I,ICFD)=VOLPol(J,I+1,ICFD)
            SRCE(I,J,ICFD)=SRCE(I+1,J,ICFD)
            SRCFRC(I,J,ICFD)=SRCFRC(I+1,J,ICFD)
          enddo
          BLKSURF(I,ICFD,1)=BLKSURF(I+1,ICFD,1)
          BLKSURF(I,ICFD,2)=BLKSURF(I+1,ICFD,2)
          BLKSURF(I,ICFD,3)=BLKSURF(I+1,ICFD,3)
          BLKSURF(I,ICFD,4)=BLKSURF(I+1,ICFD,4)
          BLKSURF(I,ICFD,5)=BLKSURF(I+1,ICFD,5)
          BLKSURF(I,ICFD,6)=BLKSURF(I+1,ICFD,6)
        else
          IVOLF(I,ICFD)=0
          IVCELLS(I,ICFD,1)=0; IVCELLS(I,ICFD,2)=0
          JVCELLS(I,ICFD,1)=0; JVCELLS(I,ICFD,2)=0
          KVCELLS(I,ICFD,1)=0; KVCELLS(I,ICFD,2)=0
          VOLNAME(I,ICFD)=' '; VCsurf(I,ICFD)=' '
          IVTYPE(I,ICFD)=0
          VOLTemp(I,ICFD)=0.; VOLHeat(I,ICFD)=0.
          IVConfl(I,ICFD)=0
          VOLHum(I,ICFD)=0.; VOLCO2(I,ICFD)=0.
          VOLVel(I,ICFD)=0.
          VOLDir(I,ICFD,1)=0.; VOLDir(I,ICFD,2)=0.
          VOLArea(I,ICFD)=0.; VOLPres(I,ICFD)=0.
          do J=1,MCTM
            VOLPol(J,I,ICFD)=0.
            SRCE(I,J,ICFD)=' '
            SRCFRC(I,J,ICFD)=0.0
          enddo
          BLKSURF(I,ICFD,1)='NONE'; BLKSURF(I,ICFD,2)='NONE'
          BLKSURF(I,ICFD,3)='NONE'; BLKSURF(I,ICFD,4)='NONE'
          BLKSURF(I,ICFD,5)='NONE'; BLKSURF(I,ICFD,6)='NONE'
        endif
      enddo
      NVOL(ICFD)=NVOL(ICFD)-1
      CALL NEW2OLD

      RETURN
      END

C ******************** MKCFDBC ********************
C Create a single CFD boundary condition. Input parameters:

C itype (IVTYPE): BC type
C Solid types:
C  1 = temperature
C  2 = heat flux
C  3 = symmetrical
C  4 = temperature, conflated
C  5 = heat flux, conflated
C  6 = symmetrical, conflated
C Air flow opening types:
C 10 = pressure
C 11 = velocity
C 12 = zero gradient
C 13 = mass
C Source types:
C 20 = source
C Blockage types:
C 30 = heat flux
C 31 = temperature
C 32 = conflated to single surface
C 33 = conflated to multiple surfaces
C 34 = casual gain
C 35 = dynamic person

C ifac (IVOLF): face
C  1 = west
C  2 = east
C  3 = south
C  4 = north
C  5 = low
C  6 = high
C  7 = whole
C  8 = block
C  9 = source

C i1,i2,j1,j2,k1,k2 (IVCELLS,JVCELLS,KVCELLS): start and end cell indices
C for i,j and k directions.

C nam (VOLNAME): volume name
C confsurf (VCsurf): conflated surface name
C pol(npol) (VOLPol): contaminant concentrations (source only)

C dat1,dat2,dat3,dat4,dat5: depending on itype, data for the BC
C itype=1:  VOLTemp,-      ,-      ,-      ,-
C itype=2:  VOLHeat,-      ,-      ,-      ,-
C itype=3:  -      ,-      ,-      ,-      ,-
C itype=4:  VOLTemp,IVConfl,-      ,-      ,-
C itype=5:  VOLHeat,IVConfl,-      ,-      ,-
C itype=6:  -      ,IVConfl,-      ,-      ,-
C itype=10: VOLTemp,VOLArea,VOLPres,-      ,-
C itype=11: VOLTemp,VOLArea,VOLVel ,VOLDir1,VOLDir2
C itype=12: VOLTemp,VOLArea,-      ,-      ,-
C itype=13: VOLTemp,VOLArea,VOLVel ,-      ,-
C itype=20: VOLHeat,IDcasgn,Fcasgn ,occtype,-
C itype=30: VOLHeat,-      ,-      ,-      ,-
C itype=31: VOLTemp,-      ,-      ,-      ,-
C itype=32: VOLTemp,-      ,-      ,-      ,-
C itype=33: surface numbers for west, north, south, high, and low faces
C           respectively (confsurf is name for east surface)
C itype=34: IDcasgn,Fcasgn ,-      ,-      ,-      
C itype=35: occtype,occpart,-      ,-      ,-

C occtype:
C 1.0 = man
C 2.0 = woman
C 3.0 = child

C occpart:
C 1.0 = head
C 2.0 = trunk
C 3.0 = left arm
C 4.0 = right arm
C 5.0 = left leg
C 6.0 = right leg
C 7.0 = whole bady

C If show is true, will highlight the boundary condition in the 3D model
C display and print text feedback, then pause for half a second.

C Error codes:
C IER=1: unrecognised itype
C IER=2: cells already contain a blockage or source (silent) < currently unused >

      SUBROUTINE MKCFDBC(show,itype,ifac,i1,i2,j1,j2,k1,k2,nam,confsurf,
     &  pol,npol,dat1,dat2,dat3,dat4,dat5,IER)
#include "building.h"
#include "cfd.h"
#include "geometry.h"

      logical show
      integer itype,ifac,i1,i2,j1,j2,k1,k2,npol
      character nam*12,confsurf*12
      real pol(npol),dat1,dat2,dat3,dat4,dat5

      COMMON/OUTIN/IUOUT,IUIN,IEOUT    
      COMMON/ICFNOD/ICFD,ICP
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      common/KEYVOLS/NVOL(MNZ),IVOLF(MNVLS,MNZ),IVCELLS(MNVLS,MNZ,2),
     &               JVCELLS(MNVLS,MNZ,2),KVCELLS(MNVLS,MNZ,2)
      common/KEYVOLN/VOLNAME(MNVLS,MNZ),VCsurf(MNVLS,MNZ),
     &               BLKSURF(MNVLS,MNZ,6)
      character VOLNAME*12,VCsurf*12,BLKSURF*12
      common/KEYVDAT/IVTYPE(MNVLS,MNZ),VOLTemp(MNVLS,MNZ),
     &          VOLHeat(MNVLS,MNZ),IVConfl(MNVLS,MNZ),VOLHum(MNVLS,MNZ),
     &          VOLCO2(MNVLS,MNZ),VOLVel(MNVLS,MNZ),VOLDir(MNVLS,MNZ,2),
     &          VOLArea(MNVLS,MNZ),VOLPres(MNVLS,MNZ),
     &          VOLPol(MCTM,MNVLS,MNZ)
      COMMON/NDMAP/NOPEN(MNZ),MFNODE(MCFND,MNZ),IOPENi(MCFND,MNZ),
     &             IOPENf(MCFND,MNZ),JOPENi(MCFND,MNZ),
     &             JOPENf(MCFND,MNZ),KOPENi(MCFND,MNZ),
     &             KOPENf(MCFND,MNZ),FIXM(MCFND,MNZ),
     &             FIXT(MCFND,MNZ),FIXC(MCFND,MNZ),
     &             FIXK(MCFND,MNZ),FIXE(MCFND,MNZ),
     &             IWOPEN(MCFND,MNZ),ICFDCN(MCFND,MNZ),
     &             ICNACT(MCFND,MNZ),IVOLNOP(MCFND,MNZ)
      COMMON/Sbdary/NSB(MNZ),ISBi(MNSBZ,MNZ),ISBf(MNSBZ,MNZ),
     &              JSBi(MNSBZ,MNZ),JSBf(MNSBZ,MNZ),
     &              KSBi(MNSBZ,MNZ),KSBf(MNSBZ,MNZ),
     &              ISUFLC(MNSBZ,MNZ),IWSB(MNSBZ,MNZ),SSB(MNSBZ,MNZ),
     &              SSBHC(MNSBZ,MNZ),IVOLNSB(MNSBZ,MNZ),
     &              ITCtype(MNSBZ,MNZ),icTREF(MNSBZ,MNZ)
      common/blksso/NBLK(MNZ),INBLK(MNVLS,MNZ),NSSO(MNZ),
     &          INSSO(MNVLS,MNZ),BLKTEMP(MNVLS,6)
      COMMON/CFDVIS/HAS_GEOM,ISHSB,ISHAO,IFACES,ISHBLK,ISHSRC,ISHGEO,
     &              INITD
      logical HAS_GEOM
      character INITD*6
      common/KEYCASGN/IDcasgn(MNVLS,MNZ),Fcasgn(MNVLS,MNZ)
      COMMON/CTDFAF/ICTDFAF(MNZ),SRCE(MNVLS,MCTM,MNZ),ICCSRC(MNZ),
     &              ICC2NC(MCTM,MNZ),SRCFRC(MNVLS,MCTM,MNZ)
      character SRCE*12

      character outs*124,stype*8,sfac*15

      if (itype.eq.1) then
        NVOL(ICFD)=NVOL(ICFD)+1
        n=NVOL(ICFD)
        IVTYPE(n,ICFD)=itype
        IVOLF(n,ICFD)=ifac
        IVCELLS(n,ICFD,1)=i1; IVCELLS(n,ICFD,2)=i2
        JVCELLS(n,ICFD,1)=j1; JVCELLS(n,ICFD,2)=j2
        KVCELLS(n,ICFD,1)=k1; KVCELLS(n,ICFD,2)=k2
        VOLNAME(n,ICFD)=nam
        VCsurf(n,ICFD)=confsurf
        VOLTemp(n,ICFD)=dat1
      elseif (itype.eq.2) then
        NVOL(ICFD)=NVOL(ICFD)+1
        n=NVOL(ICFD)
        IVTYPE(n,ICFD)=itype
        IVOLF(n,ICFD)=ifac
        IVCELLS(n,ICFD,1)=i1; IVCELLS(n,ICFD,2)=i2
        JVCELLS(n,ICFD,1)=j1; JVCELLS(n,ICFD,2)=j2
        KVCELLS(n,ICFD,1)=k1; KVCELLS(n,ICFD,2)=k2
        VOLNAME(n,ICFD)=nam
        VCsurf(n,ICFD)=confsurf
        VOLHeat(n,ICFD)=dat1
      elseif (itype.eq.3) then
        NVOL(ICFD)=NVOL(ICFD)+1
        n=NVOL(ICFD)
        IVTYPE(n,ICFD)=itype
        IVOLF(n,ICFD)=ifac
        IVCELLS(n,ICFD,1)=i1; IVCELLS(n,ICFD,2)=i2
        JVCELLS(n,ICFD,1)=j1; JVCELLS(n,ICFD,2)=j2
        KVCELLS(n,ICFD,1)=k1; KVCELLS(n,ICFD,2)=k2
        VOLNAME(n,ICFD)=nam
        VCsurf(n,ICFD)=confsurf
      elseif (itype.eq.4) then
        NVOL(ICFD)=NVOL(ICFD)+1
        n=NVOL(ICFD)
        IVTYPE(n,ICFD)=itype
        IVOLF(n,ICFD)=ifac
        IVCELLS(n,ICFD,1)=i1; IVCELLS(n,ICFD,2)=i2
        JVCELLS(n,ICFD,1)=j1; JVCELLS(n,ICFD,2)=j2
        KVCELLS(n,ICFD,1)=k1; KVCELLS(n,ICFD,2)=k2
        VOLNAME(n,ICFD)=nam
        VCsurf(n,ICFD)=confsurf
        VOLTemp(n,ICFD)=dat1
        IVconfl(n,ICFD)=int(dat2)
      elseif (itype.eq.5) then
        NVOL(ICFD)=NVOL(ICFD)+1
        n=NVOL(ICFD)
        IVTYPE(n,ICFD)=itype
        IVOLF(n,ICFD)=ifac
        IVCELLS(n,ICFD,1)=i1; IVCELLS(n,ICFD,2)=i2
        JVCELLS(n,ICFD,1)=j1; JVCELLS(n,ICFD,2)=j2
        KVCELLS(n,ICFD,1)=k1; KVCELLS(n,ICFD,2)=k2
        VOLNAME(n,ICFD)=nam
        VCsurf(n,ICFD)=confsurf
        VOLHeat(n,ICFD)=dat1
        IVconfl(n,ICFD)=int(dat2)
      elseif (itype.eq.6) then
        NVOL(ICFD)=NVOL(ICFD)+1
        n=NVOL(ICFD)
        IVTYPE(n,ICFD)=itype
        IVOLF(n,ICFD)=ifac
        IVCELLS(n,ICFD,1)=i1; IVCELLS(n,ICFD,2)=i2
        JVCELLS(n,ICFD,1)=j1; JVCELLS(n,ICFD,2)=j2
        KVCELLS(n,ICFD,1)=k1; KVCELLS(n,ICFD,2)=k2
        VOLNAME(n,ICFD)=nam
        VCsurf(n,ICFD)=confsurf
        IVconfl(n,ICFD)=int(dat2)
      elseif (itype.eq.10) then
        NVOL(ICFD)=NVOL(ICFD)+1
        n=NVOL(ICFD)
        IVTYPE(n,ICFD)=itype
        IVOLF(n,ICFD)=ifac
        IVCELLS(n,ICFD,1)=i1; IVCELLS(n,ICFD,2)=i2
        JVCELLS(n,ICFD,1)=j1; JVCELLS(n,ICFD,2)=j2
        KVCELLS(n,ICFD,1)=k1; KVCELLS(n,ICFD,2)=k2
        VOLNAME(n,ICFD)=nam
        VOLTemp(n,ICFD)=dat1
        VOLArea(n,ICFD)=dat2
        VOLPres(n,ICFD)=dat3
      elseif (itype.eq.11) then
        NVOL(ICFD)=NVOL(ICFD)+1
        n=NVOL(ICFD)
        IVTYPE(n,ICFD)=itype
        IVOLF(n,ICFD)=ifac
        IVCELLS(n,ICFD,1)=i1; IVCELLS(n,ICFD,2)=i2
        JVCELLS(n,ICFD,1)=j1; JVCELLS(n,ICFD,2)=j2
        KVCELLS(n,ICFD,1)=k1; KVCELLS(n,ICFD,2)=k2
        VOLNAME(n,ICFD)=nam
        VOLTemp(n,ICFD)=dat1
        VOLArea(n,ICFD)=dat2
        VOLVel(n,ICFD)=dat3
        VOLDir(n,ICFD,1)=dat4
        VOLDir(n,ICFD,2)=dat5
      elseif (itype.eq.12) then
        NVOL(ICFD)=NVOL(ICFD)+1
        n=NVOL(ICFD)
        IVTYPE(n,ICFD)=itype
        IVOLF(n,ICFD)=ifac
        IVCELLS(n,ICFD,1)=i1; IVCELLS(n,ICFD,2)=i2
        JVCELLS(n,ICFD,1)=j1; JVCELLS(n,ICFD,2)=j2
        KVCELLS(n,ICFD,1)=k1; KVCELLS(n,ICFD,2)=k2
        VOLNAME(n,ICFD)=nam
        VOLTemp(n,ICFD)=dat1
        VOLArea(n,ICFD)=dat2
      elseif (itype.eq.13) then
        NVOL(ICFD)=NVOL(ICFD)+1
        n=NVOL(ICFD)
        IVTYPE(n,ICFD)=itype
        IVOLF(n,ICFD)=ifac
        IVCELLS(n,ICFD,1)=i1; IVCELLS(n,ICFD,2)=i2
        JVCELLS(n,ICFD,1)=j1; JVCELLS(n,ICFD,2)=j2
        KVCELLS(n,ICFD,1)=k1; KVCELLS(n,ICFD,2)=k2
        VOLNAME(n,ICFD)=nam
        VOLTemp(n,ICFD)=dat1
        VOLArea(n,ICFD)=dat2
        VOLVel(n,ICFD)=dat3
      elseif (itype.eq.20) then
        NVOL(ICFD)=NVOL(ICFD)+1
        n=NVOL(ICFD)
        IVTYPE(n,ICFD)=itype
        IVOLF(n,ICFD)=ifac
        IVCELLS(n,ICFD,1)=i1; IVCELLS(n,ICFD,2)=i2
        JVCELLS(n,ICFD,1)=j1; JVCELLS(n,ICFD,2)=j2
        KVCELLS(n,ICFD,1)=k1; KVCELLS(n,ICFD,2)=k2
        VOLNAME(n,ICFD)=nam
        VOLPol(1:npol,n,ICFD)=pol
        do i=1,npol
          SRCE(n,i,ICFD)=' '
          SRCFRC(n,i,ICFD)=0.0
        enddo
        VOLHeat(n,ICFD)=dat1
        IDcasgn(n,ICFD)=NINT(dat2)
        Fcasgn(n,ICFD)=dat3
        itmp=NINT(dat4)
        if (itmp.eq.1) then
          VCsurf(n,ICFD)='man'
        elseif (itmp.eq.2) then
          VCsurf(n,ICFD)='woman'
        elseif (itmp.eq.3) then
          VCsurf(n,ICFD)='child'
        else
          VCsurf(n,ICFD)='none'
        endif
      elseif (itype.eq.30) then
        NVOL(ICFD)=NVOL(ICFD)+1
        n=NVOL(ICFD)
        IVTYPE(n,ICFD)=itype
        IVOLF(n,ICFD)=ifac
        IVCELLS(n,ICFD,1)=i1; IVCELLS(n,ICFD,2)=i2
        JVCELLS(n,ICFD,1)=j1; JVCELLS(n,ICFD,2)=j2
        KVCELLS(n,ICFD,1)=k1; KVCELLS(n,ICFD,2)=k2
        VOLNAME(n,ICFD)=nam
        VOLHeat(n,ICFD)=dat1
        VCsurf(n,ICFD)=confsurf
        BLKSURF(n,ICFD,1)='NONE'
        BLKSURF(n,ICFD,2)='NONE'
        BLKSURF(n,ICFD,3)='NONE'
        BLKSURF(n,ICFD,4)='NONE'
        BLKSURF(n,ICFD,5)='NONE'
        BLKSURF(n,ICFD,6)='NONE'
      elseif (itype.eq.31) then
        NVOL(ICFD)=NVOL(ICFD)+1
        n=NVOL(ICFD)
        IVTYPE(n,ICFD)=itype
        IVOLF(n,ICFD)=ifac
        IVCELLS(n,ICFD,1)=i1; IVCELLS(n,ICFD,2)=i2
        JVCELLS(n,ICFD,1)=j1; JVCELLS(n,ICFD,2)=j2
        KVCELLS(n,ICFD,1)=k1; KVCELLS(n,ICFD,2)=k2
        VOLNAME(n,ICFD)=nam
        VOLTemp(n,ICFD)=dat1
        VCsurf(n,ICFD)=confsurf
        BLKSURF(n,ICFD,1)='NONE'
        BLKSURF(n,ICFD,2)='NONE'
        BLKSURF(n,ICFD,3)='NONE'
        BLKSURF(n,ICFD,4)='NONE'
        BLKSURF(n,ICFD,5)='NONE'
        BLKSURF(n,ICFD,6)='NONE'
      elseif (itype.eq.32) then
        NVOL(ICFD)=NVOL(ICFD)+1
        n=NVOL(ICFD)
        IVTYPE(n,ICFD)=itype
        IVOLF(n,ICFD)=ifac
        IVCELLS(n,ICFD,1)=i1; IVCELLS(n,ICFD,2)=i2
        JVCELLS(n,ICFD,1)=j1; JVCELLS(n,ICFD,2)=j2
        KVCELLS(n,ICFD,1)=k1; KVCELLS(n,ICFD,2)=k2
        VOLNAME(n,ICFD)=nam
        VOLTemp(n,ICFD)=dat1
        VCsurf(n,ICFD)=confsurf
        BLKSURF(n,ICFD,1)='NONE'
        BLKSURF(n,ICFD,2)='NONE'
        BLKSURF(n,ICFD,3)='NONE'
        BLKSURF(n,ICFD,4)='NONE'
        BLKSURF(n,ICFD,5)='NONE'
        BLKSURF(n,ICFD,6)='NONE'
      elseif (itype.eq.33) then
        NVOL(ICFD)=NVOL(ICFD)+1
        n=NVOL(ICFD)
        IVTYPE(n,ICFD)=itype
        IVOLF(n,ICFD)=ifac
        IVCELLS(n,ICFD,1)=i1; IVCELLS(n,ICFD,2)=i2
        JVCELLS(n,ICFD,1)=j1; JVCELLS(n,ICFD,2)=j2
        KVCELLS(n,ICFD,1)=k1; KVCELLS(n,ICFD,2)=k2
        VOLNAME(n,ICFD)=nam
        VOLTemp(n,ICFD)=0.
        VCsurf(n,ICFD)=' '
        BLKSURF(n,ICFD,1)=confsurf
        itmp=NINT(dat1)
        if (itmp.eq.0) then
          BLKSURF(n,ICFD,2)='NONE'
        else
          BLKSURF(n,ICFD,2)=SNAME(ICP,itmp)
        endif
        itmp=INT(dat2)
        if (itmp.eq.0) then
          BLKSURF(n,ICFD,3)='NONE'
        else
          BLKSURF(n,ICFD,3)=SNAME(ICP,itmp)
        endif
        itmp=INT(dat3)
        if (itmp.eq.0) then
          BLKSURF(n,ICFD,4)='NONE'
        else
          BLKSURF(n,ICFD,4)=SNAME(ICP,itmp)
        endif
        itmp=INT(dat4)
        if (itmp.eq.0) then
          BLKSURF(n,ICFD,5)='NONE'
        else
          BLKSURF(n,ICFD,5)=SNAME(ICP,itmp)
        endif
        itmp=INT(dat5)
        if (itmp.eq.0) then
          BLKSURF(n,ICFD,6)='NONE'
        else
          BLKSURF(n,ICFD,6)=SNAME(ICP,itmp)
        endif
      elseif (itype.eq.34) then
        NVOL(ICFD)=NVOL(ICFD)+1
        n=NVOL(ICFD)
        IVTYPE(n,ICFD)=itype
        IVOLF(n,ICFD)=ifac
        IVCELLS(n,ICFD,1)=i1; IVCELLS(n,ICFD,2)=i2
        JVCELLS(n,ICFD,1)=j1; JVCELLS(n,ICFD,2)=j2
        KVCELLS(n,ICFD,1)=k1; KVCELLS(n,ICFD,2)=k2
        VOLNAME(n,ICFD)=nam
        VOLTemp(n,ICFD)=0.
        VCsurf(n,ICFD)=' '
        BLKSURF(n,ICFD,1)='NONE'
        BLKSURF(n,ICFD,2)='NONE'
        BLKSURF(n,ICFD,3)='NONE'
        BLKSURF(n,ICFD,4)='NONE'
        BLKSURF(n,ICFD,5)='NONE'
        BLKSURF(n,ICFD,6)='NONE'
        IDcasgn(n,ICFD)=NINT(dat1)
        Fcasgn(n,ICFD)=dat2
      elseif (itype.eq.35) then
        NVOL(ICFD)=NVOL(ICFD)+1
        n=NVOL(ICFD)
        IVTYPE(n,ICFD)=itype
        IVOLF(n,ICFD)=ifac
        IVCELLS(n,ICFD,1)=i1; IVCELLS(n,ICFD,2)=i2
        JVCELLS(n,ICFD,1)=j1; JVCELLS(n,ICFD,2)=j2
        KVCELLS(n,ICFD,1)=k1; KVCELLS(n,ICFD,2)=k2
        VOLNAME(n,ICFD)=nam
        VOLTemp(n,ICFD)=0.
        VCsurf(n,ICFD)=' '
        itmp=NINT(dat1)
        if (itmp.eq.1) then
          BLKSURF(n,ICFD,1)='man'
        elseif (itmp.eq.2) then
          BLKSURF(n,ICFD,1)='woman'
        elseif (itmp.eq.3) then
          BLKSURF(n,ICFD,1)='child'
        else
          BLKSURF(n,ICFD,1)='UNKNOWN'
        endif
        itmp=NINT(dat2)
        if (itmp.eq.1) then
          BLKSURF(n,ICFD,2)='head'
        elseif (itmp.eq.2) then
          BLKSURF(n,ICFD,2)='trunk'
        elseif (itmp.eq.3) then
          BLKSURF(n,ICFD,2)='left_arm'
        elseif (itmp.eq.4) then
          BLKSURF(n,ICFD,2)='right_arm'
        elseif (itmp.eq.5) then
          BLKSURF(n,ICFD,2)='left_leg'
        elseif (itmp.eq.6) then
          BLKSURF(n,ICFD,2)='right_leg'
        else
          BLKSURF(n,ICFD,2)='whole_body'
        endif
        BLKSURF(n,ICFD,3)='NONE'
        BLKSURF(n,ICFD,4)='NONE'
        BLKSURF(n,ICFD,5)='NONE'
        BLKSURF(n,ICFD,6)='NONE'
      else
        IER=1
        write(outs,'(a,i2)')'MKCFDBC: unrecognised type ',itype
        call EDISP(IUOUT,outs)
        goto 999
      endif
      call NEW2OLD

      if (show) then
        if (itype.ge.1 .and. itype.le.6) then
          stype='solid   '
          lstype=5          
          ISHSB=NSB(ICFD); ISHAO=-1; ISHBLK=-1; ISHSRC=-1;
        elseif (itype.ge.10 .and. itype.le.13) then
          stype='opening '
          lstype=7
          ISHSB=-1; ISHAO=NOPEN(ICFD); ISHBLK=-1; ISHSRC=-1;
        elseif (itype.eq.20) then
          stype='source  '
          lstype=6
          ISHSB=-1; ISHAO=-1; ISHBLK=-1; ISHSRC=-1 ! << Need to implement number of sources variable
        elseif (itype.ge.30 .and. itype.lt.40) then
          stype='blockage'
          lstype=8
          ISHSB=-1; ISHAO=-1; ISHBLK=NBLK(ICFD); ISHSRC=-1
        endif

        if (ifac.eq.1) then 
          sfac=' on west face '
          lsfac=14
        elseif (ifac.eq.2) then
          sfac=' on east face '
          lsfac=14
        elseif (ifac.eq.3) then
          sfac=' on south face '
          lsfac=15
        elseif (ifac.eq.4) then
          sfac=' on north face '
          lsfac=15
        elseif (ifac.eq.5) then
          sfac=' on low face '
          lsfac=13
        elseif (ifac.eq.6) then
          sfac=' on high face '
          lsfac=14
        else
          sfac=' '
          lsfac=1
        endif

        write(outs,'(6a)')' Created ',stype(1:lstype),' BC',
     &    sfac(1:lsfac),'called ',nam(1:lnblnk(nam))
        call EDISP(IUOUT,outs)
        if (MMOD.eq.8) then
          call redraw(IER)
          call pausems(500)
        endif
      endif

  999 RETURN
      END

C ******************** GENCFDBC ********************
C Infers CFD boundary conditions from zone geometry.
C  imode = 0 - all
C  imode = 1 - edge BCs (solid + opening).
C  imode = 2 - internal BCs (blockages + sources).
C  imode = 3 - 1 + 2
C  imode = 4 - occupant BCs

      SUBROUTINE GENCFDBC(imode,IER)
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "cfd.h"
#include "help.h"

      integer imode

      COMMON/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN,IEOUT     
      common/GRDVRTS/iorg(MNZ),ixend(MNZ),iyend(MNZ),izend(MNZ),
     &  izende(MNZ)
      COMMON/ICFNOD/ICFD,ICP
      common/KEYVOLS/NVOL(MNZ),IVOLF(MNVLS,MNZ),IVCELLS(MNVLS,MNZ,2),
     &               JVCELLS(MNVLS,MNZ,2),KVCELLS(MNVLS,MNZ,2)
      common/KEYVDAT/IVTYPE(MNVLS,MNZ),VOLTemp(MNVLS,MNZ),
     &          VOLHeat(MNVLS,MNZ),IVConfl(MNVLS,MNZ),VOLHum(MNVLS,MNZ),
     &          VOLCO2(MNVLS,MNZ),VOLVel(MNVLS,MNZ),VOLDir(MNVLS,MNZ,2),
     &          VOLArea(MNVLS,MNZ),VOLPres(MNVLS,MNZ),
     &          VOLPol(MCTM,MNVLS,MNZ)
      COMMON/CFDVIS/HAS_GEOM,ISHSB,ISHAO,IFACES,ISHBLK,ISHSRC,ISHGEO,
     &              INITD
      logical HAS_GEOM
      character INITD*6
      COMMON/C24/IZSTOCN(MCOM,MS)
      common/dynamico/isdynamicocup(MCOM)

      character outs*124,t6*6,t12*12
      logical ok
      logical doafnasc
      real vmax,wmin,wmax,r1,r2

      helpinsub='edcfd'  ! set for subroutine

C Remember visualisation settings.
      IISHSB=ISHSB; IISHAO=ISHAO; IISHBLK=ISHBLK; IISHSRC=ISHSRC
      IISHGEO=ISHGEO; IIFACES=IFACES

C Ensure parent-child relationships are defined.
      call SURREL2('u',ICP,IER)

C If there are already some defined, ask user if they want to clear
C existing boundary conditions.
      if (NVOL(ICFD).gt.0) then
        if (imode.eq.0) then
          iw=1
          helptopic='cfdbc_infer_delete_exist'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL EASKABC('Clear all boundary conditions?',' ','yes','no',
     &      'cancel',iw,nbhelp)
          if (iw.eq.1) then
            do I=1,NVOL(ICFD)              
              call DELCFDBC(I)
            enddo
          elseif (iw.eq.3) then
            return
          endif
        endif
        if (imode.eq.1 .or. imode.eq.3 .or. imode.eq.0) then
          iw=1
          helptopic='cfdbc_infer_delete_exist'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL EASKABC('Clear solid and opening boundary conditions?',
     &      ' ','yes','no','cancel',iw,nbhelp)
          if (iw.eq.1) then
            do I=1,NVOL(ICFD)
              if (IVTYPE(I,ICFD).lt.20) call DELCFDBC(I)
            enddo
          elseif (iw.eq.3) then
            return
          endif
        endif
        if (imode.eq.2 .or. imode.eq.3 .or. imode.eq.0) then
          iw=1
          helptopic='cfdbc_infer_delete_exist'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL EASKABC('Clear source and blockage boundary conditions?',
     &      ' ','yes','no','cancel',iw,nbhelp)
          if (iw.eq.1) then
            do I=1,NVOL(ICFD)
              if (IVTYPE(I,ICFD).ge.20) call DELCFDBC(I)
            enddo
          elseif (iw.eq.3) then
            return
          endif
        endif
      endif

C Make sure CFD - geometry transforms have been initialised.
      write(t6,'(2I3.3)')ICFD,ICP
      if (INITD.ne.t6) then
        if (HAS_GEOM) then
          CALL INIT_CFDTRANS(IER)
        else
          CALL INIT_CFDTRANS_NOGEOM(IER)
        endif
      endif
      if (IER.ne.0) then
        write(outs,'(2a)')
     &    'Error initialising coordinate transforms.',
     &    'Check gridding.'
        CALL EDISP(IUOUT,outs)
        goto 999
      endif

C Grab the X, Y, and Z coordinates of the domain extremes (in CFD axes)
      umin=0.0; vmin=0.0; wmin=0.0
      CALL CFDTRANS(2,szcoords(ICP,ixend(ICFD),1),
     &                szcoords(ICP,ixend(ICFD),2),
     &                szcoords(ICP,ixend(ICFD),3),umax,r1,r2,IER)
      CALL CFDTRANS(2,szcoords(ICP,iyend(ICFD),1),
     &                szcoords(ICP,iyend(ICFD),2),
     &                szcoords(ICP,iyend(ICFD),3),r1,vmax,r2,IER)
      CALL CFDTRANS(2,szcoords(ICP,izend(ICFD),1),
     &                szcoords(ICP,izend(ICFD),2),
     &                szcoords(ICP,izend(ICFD),3),r1,r2,wmax,IER)
      if (IER.ne.0) then
        write(outs,'(2a)')
     &    'Error transforming coordinates for domain axes.',
     &    'Check gridding.'
        CALL EDISP(IUOUT,outs)
        goto 999
      endif

C Loop through zone surfaces.
      if (imode.eq.1 .or. imode.eq.3 .or. imode.eq.0) then

C Create solid and opening BCs.
        if (NZSUR(ICP).gt.0) then
          write(outs,'(a)')
     &      'Scanning surfaces for solid and opening BCs.'
          CALL EDISP(IUOUT,' ')
          CALL EDISP(IUOUT,outs)

          ok=.true.
          do ISUR=1,NZSUR(ICP)
            ICNN=IZSTOCN(ICP,ISUR)

C If this surface is a child, skip it; children are processed along with
C their parent.
            if (iparent(ICNN).ne.0) CYCLE

            ifac=0
            CALL GENEDGBC(isur,umax,vmax,wmax,ifac,ibctyp,isumin,isumax,
     &        isvmin,isvmax,iswmin,iswmax,doafnasc,ISTAT)

            if (ISTAT.eq.0) then
              CALL EDISP(IUOUT,'* No problems *')
            elseif (ISTAT.eq.4 .or. ISTAT.eq.6) then
              CALL EDISP(IUOUT,'* Surface skipped, but no problems *')            
            else
              CALL EDISP(IUOUT,'! Problem encountered !')
              ok=.false.
            endif
          enddo
          if (.not.ok) then
            CALL EDISP(IUOUT,
     &        'Warning: boundaries may not be fully defined.')
            CALL EDISP(IUOUT,
     &        'Please check text feedback above.')
          endif
        else        
          CALL EDISP(IUOUT,'* No surfaces detected *')
        endif
      endif

C Create blockage BCs.
      if (imode.eq.2 .or. imode.eq.3 .or. imode.eq.0) then

C Create blockages for cells that fall outside the zone.
        write(outs,'(a)')
     &    'Scanning for cells outside zone.'
        CALL EDISP(IUOUT,' ')
        CALL EDISP(IUOUT,outs)
        CALL GENOSBC(istat)
        if (istat.eq.0) then
          CALL EDISP(IUOUT,'* No problems *')
        else
          CALL EDISP(IUOUT,'! Problem encountered !')
        endif
        CALL EDISP(IUOUT,' ')

C Because we need to check all visual entities against one another to
C check for intersections, we loop through entities inside the
C subroutine instead of at this level.
        if (NBVIS(ICP).gt.0) then
          write(outs,'(a)')
     &      'Scanning visual entities for blockage BCs.'
          CALL EDISP(IUOUT,outs)
          CALL GENVOBC(ISTAT)
          if (ISTAT.ne.0) then            
            CALL EDISP(IUOUT,
     &      'Warning: problem(s) encountered while creating blockages.')
            CALL EDISP(IUOUT,
     &        'Please check text feedback above.')
          endif
        else
          CALL EDISP(IUOUT,'* No visual entities detected *')
        endif
      endif

C Place occupant BCs.
      if (imode.eq.4 .or. imode.eq.0) then

C First, check if dynamic people are defined in the zone operations.
C If they're not, pop up a message asking the user to define them.
        if (isdynamicocup(ICP).eq.0) then
          helptopic='cfdbc_infer_occ_notdy'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL PHELPD('Function not available',nbhelp,'-',0,0,IER)
        else
          write(outs,'(a)')
     &      'Commencing auto-generation of occupant BCs.'
          CALL EDISP(IUOUT,outs)
          call GENOCBC(ISTAT)
        endif
      endif          

C Restore visualisation settings.
      ISHSB=IISHSB; ISHAO=IISHAO; ISHBLK=IISHBLK; ISHSRC=IISHSRC
      ISHGEO=IISHGEO; IFACES=IIFACES
      call redraw(IER)

  999 RETURN
      END


C ******************** GENOCBC ********************
C Used in the auto-generation of CFD BCs.
C Creates blockages and sources representing occupants.

C istat:
C 1 = error
C 2 = no occupants

      SUBROUTINE GENOCBC(istat)
#include "building.h"
#include "cfd.h"
#include "help.h"
#include "schedule.h"
#include "prj3dv.h"
      
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/ICFNOD/ICFD,ICP
      common/caleni/NBDAYTYPE,NBCALDAYS(MDTY),ICALENDER(365)
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      common/dynamico/isdynamicocup(MCOM)
      COMMON/CFDVIS/HAS_GEOM,ISHSB,ISHAO,IFACES,ISHBLK,ISHSRC,ISHGEO,
     &              INITD
      logical HAS_GEOM
      character INITD*6
      COMMON/ALL/NI,NJ,NK,NIM1,NJM1,NKM1,NIM2,NJM2,NKM2
      COMMON/GEOM/XP(ntcelx),YP(ntcely),ZP(ntcelz),
     &  DXEP(ntcelx),DXPW(ntcelx),DYNP(ntcely),DYPS(ntcely),
     &  DZHP(ntcelz),DZPL(ntcelz),SEW(ntcelx),SNS(ntcely),SHL(ntcelz),
     &  XU(ntcelx),YV(ntcely),ZW(ntcelz)
      common/KEYVOLS/NVOL(MNZ),IVOLF(MNVLS,MNZ),IVCELLS(MNVLS,MNZ,2),
     &  JVCELLS(MNVLS,MNZ,2),KVCELLS(MNVLS,MNZ,2)
      common/blksso/NBLK(MNZ),INBLK(MNVLS,MNZ),NSSO(MNZ),
     &  INSSO(MNVLS,MNZ),BLKTEMP(MNVLS,6)

      logical ok,INSIDEP,dosrc
      character outs*124,t12*12
      DIMENSION apol(1000)
      real dum(1)
#ifdef OSI
      integer ik,ix,iy,iix,iiy
#else
      integer*8 ik,ix,iy,iix,iiy
#endif     

      helpinsub='edcfd'

      istat=0

C First, scan zone operations to determine the maximum number of each
C occupant type.   
      ok=.false.
      do igty=1,MGTY
        if (caskeytype(ICP,igty)(1:14).eq.'dynamic_people') then
          ok=.true.
          exit
        endif
      enddo

      if (.not.ok) then
        istat=1
        outs='GENOCBC: error while scanning zone operations.'
        call edisp(ieout,outs)
        return
      endif

      mxmen=0; imen=0
      mxwom=0; iwom=0
      mxchi=0; ichi=0
      do idty=1,NBDAYTYPE
        if (NCAS(idty).gt.0) then
          do ip=1,NCAS(idty)
            if (ABS(ICGT(idty,ip)).eq.igty) then
              if (TNBMEN(idty,ip).gt.mxmen) mxmen=NINT(TNBMEN(idty,ip))
              if (TNBWOMEN(idty,ip).gt.mxwom)
     &          mxwom=NINT(TNBWOMEN(idty,ip))
              if (TNBCHILD(idty,ip).gt.mxchi)
     &          mxchi=NINT(TNBCHILD(idty,ip))
            endif
          enddo
        endif
      enddo

      if (mxmen.eq.0 .and. mxwom.eq.0 .and. mxchi.eq.0) then
        istat=2
        outs='No occupants detected.'
        call edisp(iuout,outs)
        return
      endif

C If we're using the segmented model, we need a source as well as the
C blockage.
      if (isdynamicocup(ICP).eq.4) then
        dosrc=.true.
      else
        dosrc=.false.
      endif

C Create BCs for each person.

C TODO: For the time being, just create a single whole body blockage. In
C future we could extend this to create individual parts for the
C segmented model.
      do ip=1,mxmen+mxwom+mxchi

C ipty: 1 = man, 2 = woman, 3 = child
        if (ip.le.mxmen) then
          ipty=1; imen=imen+1; xhgt=1.75; xwdt=0.4;  xdpt=0.3
          write(outs,'(a,i3,a)')'Processing occupant ',ip,' (man) ...'
        elseif (ip.le.mxmen+mxwom) then
          ipty=2; iwom=iwom+1; xhgt=1.6;  xwdt=0.4;  xdpt=0.3
          write(outs,'(a,i3,a)')'Processing occupant ',ip,' (woman) ...'
        else
          ipty=3; ichi=ichi+1; xhgt=1.3;  xwdt=0.35; xdpt=0.25
          write(outs,'(a,i3,a)')'Processing occupant ',ip,' (child) ...'
        endif
        call edisp(iuout,outs)

C Starting estimate is in the middle of the domain, facing north.
C idir: 1 = north/south, 2 = east/west
        i=NIM1/2; j=NJM1/2; idir=1

C If we're in graphic mode, ask user to click where they want to place
C the occupant.
    3   if (MMOD.eq.8) then
          call edisp(iuout,
     &    '  Click where on the floor you want to place this occupant.')
          call edisp(iuout,
     &      '  Press v to reposition the view, c to cancel.')
          ISHSB=-1; ISHAO=-1; ISHBLK=-1; ISHSRC=-1; ISHGEO=0; IFACES=1
          call redraw(IER)          
    1     CALL trackview(ik,ix,iy)
          if (ik.eq.118) then ! pressed v
            call tmpmenu
            goto 1
          elseif (ik.eq.99) then ! pressed c
            outs='Cancelled, skipping this occupant.'
            call edisp(IUOUT,outs)
            goto 2
          endif

C Find a cell on the low face that corresponds to the location clicked.
          ok=.false.
          k=2
          do i=2,NIM1
            do j=2,NJM1
              
C Transform lower face corners to screen coords.
              ier=0
              iapnt=1
              apol(iapnt)=4.0
              CALL CFDTRANS(1,XU(i),YV(j),ZW(k),x0,y0,z0,ier)
              CALL ORTTRN(x0,y0,z0,TSMAT,x1,y1,z1,ier)
              call u2pixel(x1,y1,iix,iiy)
              if (ier.ne.0) then
                istat=1
                outs='GENOCBC: error converting coordinates.'
                call edisp(ieout,outs)
                return
              endif
              apol(2)=real(iix); apol(3)=real(iiy); apol(4)=0.0
              CALL CFDTRANS(1,XU(i+1),YV(j),ZW(k),x0,y0,z0,ier)
              CALL ORTTRN(x0,y0,z0,TSMAT,x1,y1,z1,ier)
              call u2pixel(x1,y1,iix,iiy)
              if (ier.ne.0) then
                istat=1
                outs='GENOCBC: error converting coordinates.'
                call edisp(ieout,outs)
                return
              endif
              apol(5)=real(iix); apol(6)=real(iiy); apol(7)=0.0
              CALL CFDTRANS(1,XU(i+1),YV(j+1),ZW(k),x0,y0,z0,ier)
              CALL ORTTRN(x0,y0,z0,TSMAT,x1,y1,z1,ier)
              call u2pixel(x1,y1,iix,iiy)
              if (ier.ne.0) then
                istat=1
                outs='GENOCBC: error converting coordinates.'
                call edisp(ieout,outs)
                return
              endif
              apol(8)=real(iix); apol(9)=real(iiy); apol(10)=0.0
              CALL CFDTRANS(1,XU(i),YV(j+1),ZW(k),x0,y0,z0,ier)
              CALL ORTTRN(x0,y0,z0,TSMAT,x1,y1,z1,ier)
              call u2pixel(x1,y1,iix,iiy)
              if (ier.ne.0) then
                istat=1
                outs='GENOCBC: error converting coordinates.'
                call edisp(ieout,outs)
                return
              endif
              apol(11)=real(iix); apol(12)=real(iiy); apol(13)=0.0

C Check if clicked point is within the face.
              ok=INSIDEP(real(ix),real(iy),3,iapnt,apol)
              if (ok) exit
            enddo
            if (ok) exit
          enddo
          if (.not.ok) then
            call edisp(iuout,'Invalid point, try again.')
            goto 1
          endif

C Confirm direction.
          call EASKAB('What direction does the occupant face?',' ',
     &      'North or South (J)','East or West (I)',idir,nbhelp)

C Find cells to block considering approximate size.
          iis=i; iif=i; xis=XU(i)
          ijs=j; ijf=j; xjs=YV(j)
          iks=k; ikf=k; xks=ZW(k)
          if (idir.eq.1) then
            xi=xwdt; xj=xdpt
          elseif (idir.eq.2) then
            xi=xdpt; xj=xwdt
          endif
          xk=xhgt
          do i=iis+1,NIM1
            if (XP(i)-xis.ge.xi) exit
            iif=i
          enddo
          do j=ijs+1,NJM1
            if (YP(j)-xjs.ge.xj) exit
            ijf=j
          enddo
          do k=iks+1,NKM1
            if (ZP(k)-xks.ge.xk) exit
            ikf=k
          enddo

C If not in graphic mode, just ask user to specify extents.
        else
          iis=1; iif=NIM1; ijs=1; ijf=NJM1; iks=1; ikf=NKM1; iface=8
          call PIKCELS(iis,iif,ijs,ijf,iks,ikf,iface,IER)
          if (IER.ne.0) then
            istat=1
            outs='GENOCBC: error specifying BC extents.'
            call edisp(IEOUT,outs)
            return
          endif
        endif

Create new blockage BC.
    4   dum=(/0/)
        if (ipty.eq.1) then
          write (t12,'(a3,i3.3,a3,i3.3)')'blk',NBLK(ICFD)+1,'_om',imen
        elseif (ipty.eq.2) then
          write (t12,'(a3,i3.3,a3,i3.3)')'blk',NBLK(ICFD)+1,'_ow',iwom
        elseif (ipty.eq.3) then
          write (t12,'(a3,i3.3,a3,i3.3)')'blk',NBLK(ICFD)+1,'_oc',ichi
        else
          istat=1
          outs='GENOCBC: invalid occupant type.'
          call edisp(IEOUT,outs)
          return
        endif
        call MKCFDBC(.true.,35,8,iis,iif,ijs,ijf,iks,ikf,t12,
     &    '            ',dum,1,real(ipty),7.0,0.0,0.0,0.0,IER)
        if (IER.ne.0) then
          istat=1
          outs='GENOCBC: error creating BC.'
          call edisp(IEOUT,outs)
          return
        endif

C If in graphic mode ...
        if (MMOD.eq.8) then

C Ask user if this is correct.
          iopt=1
          helptopic='cfdbc_infer_occ_prevw'          
          call gethelptext(helpinsub,helptopic,nbhelp)
          call EASKABCD('Preview of occupant.','Options:',
     &      'confirm','click location','manual edit','cancel',iopt,
     &      nbhelp)
          if (iopt.eq.2) then ! pick on display again
            call DELCFDBC(NVOL(ICFD))
            goto 3
          elseif (iopt.eq.3) then ! manually edit extents
            call DELCFDBC(NVOL(ICFD))
            iface=8
            call PIKCELS(iis,iif,ijs,ijf,iks,ikf,iface,IER)
            if (IER.ne.0) then
              istat=1
              outs='GENOCBC: error specifying BC extents.'
              call edisp(IEOUT,outs)
              return
            endif
            goto 4
          elseif (iopt.eq.4) then ! cancel
            call DELCFDBC(NVOL(ICFD))
            outs='Cancelled, skipping this occupant.'
            call edisp(IUOUT,outs)
          endif
          ISHBLK=-1
          call redraw(IER)
        endif
    2   continue 
      enddo

      return
      end


C ******************** GENOSBC ********************
C Used in the auto-generation of CFD BCs inferred from geometry.
C Creates blockages for areas of the domain that fall outside the
C zone.

      SUBROUTINE GENOSBC(istat)
#include "building.h"
#include "geometry.h"
#include "cfd.h"

      integer istat

      COMMON/ICFNOD/ICFD,ICP
      COMMON/ALL/NI,NJ,NK,NIM1,NJM1,NKM1,NIM2,NJM2,NKM2
      common/KEYVOLS/NVOL(MNZ),IVOLF(MNVLS,MNZ),IVCELLS(MNVLS,MNZ,2),
     &               JVCELLS(MNVLS,MNZ,2),KVCELLS(MNVLS,MNZ,2)
      common/blksso/NBLK(MNZ),INBLK(MNVLS,MNZ),NSSO(MNZ),
     &          INSSO(MNVLS,MNZ),BLKTEMP(MNVLS,6)

      character outs*124,t12*12,tt12*12
      logical LINVOL,coni,conj,conk,con,lfac,ok
      logical CFDPOINTC
      dimension iapnt(100),apol(1000)
      real dum(1)

      istat=0
      
C Initialise data structure for polygon conversions.
      CALL ESMCFDZON(ICP,istat)
      if (istat.ne.0) return

C Scan through each grid point.
C Loop in axis order K, J, I.
      do ik=2,NKM1
        do ij=2,NJM1
          do ii=2,NIM1

C Check if cell is already in a boundary condition.
            ok=.true.
            do ivol=1,NVOL(ICFD)
              if (LINVOL(ii,ij,ik,
     &            IVCELLS(ivol,ICFD,1),IVCELLS(ivol,ICFD,2),
     &            JVCELLS(ivol,ICFD,1),JVCELLS(ivol,ICFD,2),
     &            KVCELLS(ivol,ICFD,1),KVCELLS(ivol,ICFD,2))) then
                ok=.false.
                exit
              endif
            enddo
            if (.not.ok) cycle

C Scan through each surface.
            coni=.false.
            conj=.false.
            conk=.false.
            con=.false.
            do is=1,NZSUR(ICP)
              
C Convert surface to polygons for point containment checks.
              CALL ENCPOL(is,ina,inap,iapnt,apol)

C Check if cell is entirely contained by this surface in each dimension.
C Do not match points on surface edges; this means that grid points
C which fall exactly on surface edges will be blocked (which should be
C consisent with edge BC autogeneration).
              if (.not.coni) then
                coni=CFDPOINTC(ii,ij,ik,1,is)
              endif
              if (.not.conj) then
                conj=CFDPOINTC(ii,ij,ik,2,is)
              endif
              if (.not.conk) then
                conk=CFDPOINTC(ii,ij,ik,3,is)
              endif
              if (coni.and.conj.and.conk) then
                con=.true.
                exit
              endif
            enddo
            
            if (.not.con) then
C Cell is outside the domain.
C In each direction in which the cell is contained, try and find
C surfaces which match the cell faces. We may not find any, but keep
C looking as we expand the blockage. Do not allow the blockage to expand
C beyond the first surface matched in each direction. Eventually we
C should end up with a blockage that can be correctly conflated to any
C surfaces on the edge of it.

C Determine if the current cell faces match any surfaces.
C Diagonal surfaces might match more than one face, so check for this.
              iise=0; iisw=0; iisn=0; iiss=0; iish=0; iisl=0
              do is=1,NZSUR(ICP)
                CALL ENCPOL(is,ina,inap,iapnt,apol)
                if (.not.coni) then
                  ise=0; isw=0
                  ifac=-12
                elseif (.not.conj) then
                  isn=0; iss=0
                  ifac=-34
                elseif (.not.conk) then
                  ish=0; isl=0
                  ifac=-56
                endif
                CALL CFDSURFC(is,ii,ij,ik,ifac,iapnt(1),apol,lfac,istat)
                if (istat.ne.0) return
                if (ifac.eq.1 .or. ifac.eq.2) then
                  if (ifac.eq.1) then
                    ise=is; isw=0
                  elseif (ifac.eq.2) then
                    ise=0; isw=is
                  endif
                  if (conj) then
                    ish=0; isl=0
                    ifac=34
                  elseif (conk) then
                    isn=0; iss=0
                    ifac=56
                  endif
                  CALL CFDSURFC(is,ii,ij,ik,ifac,iapnt(1),apol,lfac,
     &              istat)
                  if (istat.ne.0) return
                  if (ifac.eq.3) then
                    isn=is; iss=0
                  elseif (ifac.eq.4) then
                    isn=0; iss=is
                  elseif (ifac.eq.5) then
                    ish=is; isl=0                      
                  elseif (ifac.eq.6) then
                    ish=0; isl=is
                  else
                    isn=0; iss=0; ish=0; isl=0
                  endif
                elseif (ifac.eq.3 .or. ifac.eq.4) then
                  if (ifac.eq.3) then
                    isn=is; iss=0
                  elseif (ifac.eq.4) then
                    isn=0; iss=is
                  endif
                  ifac=56
                  CALL CFDSURFC(is,ii,ij,ik,ifac,iapnt(1),apol,lfac,
     &              istat)
                  if (istat.ne.0) return
                  if (ifac.eq.5) then
                    ish=is; isl=0
                  elseif (ifac.eq.6) then
                    ish=0; isl=is
                  else
                    ish=0; isl=0
                  endif
                elseif (ifac.eq.5) then
                  ish=is; isl=0                
                elseif (ifac.eq.6) then
                  ish=0; isl=is
                else
                  ise=0; isw=0; isn=0; iss=0; ish=0; isl=0
                endif
                if (ise.ne.0) iise=ise
                if (isw.ne.0) iisw=isw
                if (isn.ne.0) iisn=isn
                if (iss.ne.0) iiss=iss
                if (ish.ne.0) iish=ish
                if (isl.ne.0) iisl=isl
              enddo
              ise=iise; isw=iisw; isn=iisn; iss=iiss; ish=iish; isl=iisl

C Try to expand the blockage as far as possible.
              iist=ii; ijst=ij; ikst=ik
              iifn=ii; ijfn=ij; ikfn=ik

C If the east boundary not yet found, expand until:
C  1. the east boundary is located; or
C  2. a second new boundary in any direction is discovered.
              if (ise.eq.0) then
                ok=.true.
                do iii=ii+1,NIM1                  

C Check if cell is already in a boundary condition.
                  do ivol=1,NVOL(ICFD)
                    if (LINVOL(iii,ijst,ikst,
     &                IVCELLS(ivol,ICFD,1),IVCELLS(ivol,ICFD,2),
     &                JVCELLS(ivol,ICFD,1),JVCELLS(ivol,ICFD,2),
     &                KVCELLS(ivol,ICFD,1),KVCELLS(ivol,ICFD,2))) then
                      ok=.false.
                      exit
                    endif
                  enddo
                  if (.not.ok) exit

C Loop through surfaces.
                  do is=1,NZSUR(ICP)
                    CALL ENCPOL(is,ina,inap,iapnt,apol)

C Check for east boundary.
                    ifac=1
                    CALL CFDSURFC(
     &                is,iii,ijst,ikst,ifac,iapnt(1),apol,lfac,istat)
                    if (istat.ne.0) return
                    if (lfac) then
                      iifn=iii
                      ise=is
                      ok=.false.
                      exit
                    endif

C Check for other new boundaries. Don't check surfaces which we have
C already matched. The logic is that we only stop if we find a second
C surface that we could match on any face. This ensures the minimum
C possible number of blockages.
                    if (is.ne.isn) then
                      ifac=3
                      CALL CFDSURFC(
     &                  is,iii,ijst,ikst,ifac,iapnt(1),apol,lfac,istat)
                      if (istat.ne.0) return
                      if (lfac) then
                        if (isn.eq.0) then
                          isn=is
                        else
                          ok=.false.
                          exit
                        endif
                      endif
                    endif
                    if (is.ne.iss) then
                      ifac=4
                      CALL CFDSURFC(
     &                  is,iii,ijst,ikst,ifac,iapnt(1),apol,lfac,istat)
                      if (istat.ne.0) return
                      if (lfac) then
                        if (iss.eq.0) then
                          iss=is
                        else
                          ok=.false.
                          exit
                        endif
                      endif
                    endif
                    if (is.ne.ish) then
                      ifac=5
                      CALL CFDSURFC(
     &                  is,iii,ijst,ikst,ifac,iapnt(1),apol,lfac,istat)
                      if (istat.ne.0) return
                      if (lfac) then
                        if (ish.eq.0) then
                          ish=is
                        else
                          ok=.false.
                          exit
                        endif
                      endif
                    endif
                    if (is.ne.isl) then
                      ifac=6
                      CALL CFDSURFC(
     &                  is,iii,ijst,ikst,ifac,iapnt(1),apol,lfac,istat)
                      if (istat.ne.0) return
                      if (lfac) then
                        if (isl.eq.0) then
                          isl=is
                        else
                          ok=.false.
                          exit
                        endif
                      endif
                    endif
                  enddo
                  if (.not.ok) exit
                  iifn=iii
                enddo
              endif

C Expand the blockage north.
              if (isn.eq.0) then
                ok=.true.
                do iij=ij+1,NJM1

C Loop through I cells on this J layer.
                  do iii=iist,iifn

C Check if cell is already in a boundary condition.
                    do ivol=1,NVOL(ICFD)
                      if (LINVOL(iii,iij,ikst,
     &                  IVCELLS(ivol,ICFD,1),IVCELLS(ivol,ICFD,2),
     &                  JVCELLS(ivol,ICFD,1),JVCELLS(ivol,ICFD,2),
     &                  KVCELLS(ivol,ICFD,1),KVCELLS(ivol,ICFD,2))) then
                        ok=.false.
                        exit
                      endif
                    enddo
                    if (.not.ok) exit

C Loop through surfaces.
                    do is=1,NZSUR(ICP)
                      CALL ENCPOL(is,ina,inap,iapnt,apol)

C Check for new boundaries.
                      if (is.ne.isn) then
                        ifac=3
                        CALL CFDSURFC(
     &                    is,iii,iij,ikst,ifac,iapnt(1),apol,lfac,istat)
                        if (istat.ne.0) return
                        if (lfac) then
                          if (isn.eq.0) then
                            isn=is
                          else
                            ok=.false.
                            exit
                          endif
                        endif
                      endif
                      if (is.ne.ise) then
                        ifac=1
                        CALL CFDSURFC(
     &                    is,iii,iij,ikst,ifac,iapnt(1),apol,lfac,istat)
                        if (istat.ne.0) return
                        if (lfac) then
                          if (ise.eq.0 .and. iii.eq.iifn) then
                            ise=is
                          else
                            ok=.false.
                            exit
                          endif
                        endif
                      endif
                      if (is.ne.isw) then
                        ifac=2
                        CALL CFDSURFC(
     &                    is,iii,iij,ikst,ifac,iapnt(1),apol,lfac,istat)
                        if (istat.ne.0) return
                        if (lfac) then
                          if (isw.eq.0 .and. iii.eq.iist) then
                            isw=is
                          else
                            ok=.false.
                            exit
                          endif
                        endif
                      endif
                      if (is.ne.ish) then
                        ifac=5
                        CALL CFDSURFC(
     &                    is,iii,iij,ikst,ifac,iapnt(1),apol,lfac,istat)
                        if (istat.ne.0) return
                        if (lfac) then
                          if (ish.eq.0) then
                            ish=is
                          else
                            ok=.false.
                            exit
                          endif
                        endif
                      endif
                      if (is.ne.isl) then
                        ifac=6
                        CALL CFDSURFC(
     &                    is,iii,iij,ikst,ifac,iapnt(1),apol,lfac,istat)
                        if (istat.ne.0) return
                        if (lfac) then
                          if (isl.eq.0) then
                            isl=is
                          else
                            ok=.false.
                            exit
                          endif
                        endif
                      endif
                    enddo
                    if (.not.ok) exit
                  enddo
                  if (.not.ok) exit
                  ijfn=iij

C Found a north boundary.
                  if (isn.ne.0) exit
                enddo
              endif
              
C Expand blockage upwards (high direction).
              if (ish.eq.0) then
                ok=.true.
                do iik=ik+1,NKM1

C Loop through J rows on this K layer.
                  do iij=ijst,ijfn

C Loop through I cells in this J row.
                    do iii=iist,iifn

C Check if cell is already in a boundary condition.
                      do ivol=1,NVOL(ICFD)
                        if (LINVOL(iii,iij,iik,
     &                    IVCELLS(ivol,ICFD,1),IVCELLS(ivol,ICFD,2),
     &                    JVCELLS(ivol,ICFD,1),JVCELLS(ivol,ICFD,2),
     &                   KVCELLS(ivol,ICFD,1),KVCELLS(ivol,ICFD,2)))then
                          ok=.false.
                          exit
                        endif
                      enddo
                      if (.not.ok) exit

C Loop through surfaces.
                      do is=1,NZSUR(ICP)
                        CALL ENCPOL(is,ina,inap,iapnt,apol)

C Check for new boundaries.
                        if (is.ne.ish) then
                          ifac=5
                          CALL CFDSURFC(is,iii,iij,iik,ifac,iapnt(1),
     &                      apol,lfac,istat)
                          if (istat.ne.0) return
                          if (lfac) then
                            if (ish.eq.0) then
                              ish=is
                            else
                              ok=.false.
                              exit
                            endif
                          endif
                        endif
                        if (is.ne.ise) then
                          ifac=1
                          CALL CFDSURFC(is,iii,iij,iik,ifac,iapnt(1),
     &                      apol,lfac,istat)
                          if (istat.ne.0) return
                          if (lfac) then
                            if (ise.eq.0 .and. iii.eq.iifn) then
                              ise=is
                            else
                              ok=.false.
                              exit
                            endif
                          endif
                        endif
                        if (is.ne.isw) then
                          ifac=2
                          CALL CFDSURFC(is,iii,iij,iik,ifac,iapnt(1),
     &                      apol,lfac,istat)
                          if (istat.ne.0) return
                          if (lfac) then
                            if (isw.eq.0 .and. iii.eq.iist) then
                              isw=is
                            else
                              ok=.false.
                              exit
                            endif
                          endif
                        endif
                        if (is.ne.isn) then
                          ifac=3
                          CALL CFDSURFC(is,iii,iij,iik,ifac,iapnt(1),
     &                      apol,lfac,istat)
                          if (istat.ne.0) return
                          if (lfac) then
                            if (isn.eq.0 .and. iij.eq.ijfn) then
                              isn=is
                            else
                              ok=.false.
                              exit
                            endif
                          endif
                        endif
                        if (is.ne.iss) then
                          ifac=4
                          CALL CFDSURFC(is,iii,iij,iik,ifac,iapnt(1),
     &                      apol,lfac,istat)
                          if (istat.ne.0) return
                          if (lfac) then
                            if (iss.eq.0 .and. iij.eq.ijst) then
                              iss=is
                            else
                              ok=.false.
                              exit
                            endif
                          endif
                        endif
                      enddo
                      if (.not.ok) exit
                    enddo
                    if (.not.ok) exit
                  enddo
                  if (.not.ok) exit
                  ikfn=iik

C Found a high boundary.
                  if (ish.ne.0) exit
                enddo
              endif

C We have expanded the blockage as much as possible.
C Now create the boundary condition.
              write (t12,'(a,i3.3)')'blk',NBLK(ICFD)+1
              dum=(/0/)
              if (ise.eq.0 .and. isw.eq.0 .and. isn.eq.0 .and.
     &            iss.eq.0 .and. ish.eq.0 .and. isl.eq.0) then
                tt12='            '
                CALL MKCFDBC(.true.,30,8,iist,iifn,ijst,ijfn,ikst,ikfn,
     &            t12,tt12,dum,1,0.,0.,0.,0.,0.,istat)
              else
                if (ise.eq.0) then
                  tt12='NONE        '
                else
                  tt12=SNAME(ICP,ise)
                endif
                CALL MKCFDBC(.true.,33,8,iist,iifn,ijst,ijfn,ikst,ikfn,
     &            t12,tt12,dum,1,real(isw),real(isn),
     &            real(iss),real(ish),real(isl),istat)
              endif              
              if (istat.ne.0) return
            endif
          enddo
        enddo
      enddo

  999 return
      end  
      
C ******************** CFDPOINTC ********************
C Checks if a CFD grid point is contained within a polygon in
C a specified dimension. Assumes that CFD geometry transforms
C have been initialised by calling INIT_CFDTRANS.

C To make sure we're consistent with edge boundary conditions, copy the
C approach used in GENEDGBC.

C (ii,ij,ik) - indices of CFD grid point
C idim - dimension: 1 = X, 2 = Y, 3 = Z
C isur - surface index

! C (iapnt,apol) - polygon data structures describing the surface such as
! C                created by common/matpol.F: ENCPOL
! C edge - logical, whether to allow matches on an edge of the surface
! C imode: 1 = check cell grid point (default)
! C        2 = check entire cell

      ! LOGICAL FUNCTION CFDPOINTC(ii,ij,ik,idim,iapnt,apol,edge,imode)
      LOGICAL FUNCTION CFDPOINTC(ii,ij,ik,idim,isur)
#include "building.h"
#include "geometry.h"
#include "cfd.h"

      integer ii,ij,ik,idim
      dimension apol(1000)
      logical edge
      
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/ICFNOD/ICFD,ICP
      COMMON/GEOM/XP(ntcelx),YP(ntcely),ZP(ntcelz),
     &            DXEP(ntcelx),DXPW(ntcelx),DYNP(ntcely),DYPS(ntcely),
     &            DZHP(ntcelz),DZPL(ntcelz),
     &            SEW(ntcelx),SNS(ntcely),SHL(ntcelz),
     &            XU(ntcelx),YV(ntcely),ZW(ntcelz)
      
      logical ok
      character snam*12,outs*124

      CFDPOINTC=.false.
      imode=1

C Set coordinates.
      if (idim.eq.1) then
        if (imode.eq.2) then
          x11=YV(ij); x12=YV(ij+1); x21=ZW(ik); x22=ZW(ik+1)
        else
          x1=YP(ij); x2=ZP(ik)
        endif
      elseif (idim.eq.2) then
        if (imode.eq.2) then
          x11=XU(ii); x12=XU(ii+1); x21=ZW(ik); x22=ZW(ik+1)
        else
          x1=XP(ii); x2=ZP(ik)
        endif
      elseif (idim.eq.3) then
        if (imode.eq.2) then
          x11=XU(ii); x12=XU(ii+1); x21=YV(ij); x22=YV(ij+1)
        else
          x1=XP(ii); x2=YP(ij)
        endif
      else
        x1=0.; x2=0.
      endif

C Check point containment.
      ! if (imode.eq.2) then
      !   if (edge) then
      !     ok=INSIDEP(x11,x21,idim,iapnt,apol)
      !   else
      !     ok=INSIDEPNB(x11,x21,idim,iapnt,apol)
      !   endif
      !   if (.not.ok) then
      !     CFDPOINTC=.false.
      !     return
      !   endif
      !   if (edge) then
      !     ok=INSIDEP(x11,x22,idim,iapnt,apol)
      !   else
      !     ok=INSIDEPNB(x11,x22,idim,iapnt,apol)
      !   endif
      !   if (.not.ok) then
      !     CFDPOINTC=.false.
      !     return
      !   endif
      !   if (edge) then
      !     ok=INSIDEP(x12,x22,idim,iapnt,apol)
      !   else
      !     ok=INSIDEPNB(x12,x22,idim,iapnt,apol)
      !   endif
      !   if (.not.ok) then
      !     CFDPOINTC=.false.
      !     return
      !   endif
      !   if (edge) then
      !     ok=INSIDEP(x12,x21,idim,iapnt,apol)
      !   else
      !     ok=INSIDEPNB(x12,x21,idim,iapnt,apol)
      !   endif
      !   if (.not.ok) then
      !     CFDPOINTC=.false.
      !     return
      !   endif
      !   CFDPOINTC=.true.
      ! else
      !   if (edge) then
      !     CFDPOINTC=INSIDEP(x1,x2,idim,iapnt,apol)
      !   else
      !     CFDPOINTC=INSIDEPNB(x1,x2,idim,iapnt,apol)
      !   endif
      ! endif


C Establish surface extents.
C s?min/max  = min/max extent of the surface in the ? direction in CFD
C              domain coords (real).
      snam=SNAME(ICP,isur)
      IER=0
      do I=1,isznver(ICP,isur)
        IVRT=iszjvn(ICP,isur,I)
        CALL CFDTRANS(2,szcoords(ICP,IVRT,1),szcoords(ICP,IVRT,2),
     &    szcoords(ICP,IVRT,3),u,v,w,IER)
        if (IER.ne.0) then
          write(outs,'(2a,i3.3,1x,2a)')'Error transforming ',
     &      'coordinates for surface ',ISUR,snam(1:lnblnk(snam)),
     &      '. Check gridding.'
          CALL EDISP(IUOUT,outs)
          EXIT
        endif
        if (I.eq.1) then
          sumin=u; sumax=u
          svmin=v; svmax=v
          swmin=w; swmax=w
        else
          if (u.lt.sumin) sumin=u
          if (u.gt.sumax) sumax=u
          if (v.lt.svmin) svmin=v
          if (v.gt.svmax) svmax=v
          if (w.lt.swmin) swmin=w
          if (w.gt.swmax) swmax=w
        endif
      enddo

      if (idim.eq.1) then
        CFDPOINTC=x1.ge.svmin.and.x1.lt.svmax .and.
     &            x2.ge.swmin.and.x2.lt.swmax
      elseif (idim.eq.2) then
        CFDPOINTC=x1.ge.sumin.and.x1.lt.sumax .and.
     &            x2.ge.swmin.and.x2.lt.swmax
      elseif (idim.eq.3) then
        CFDPOINTC=x1.ge.sumin.and.x1.lt.sumax .and.
     &            x2.ge.svmin.and.x2.lt.svmax
      endif

      return
      end

C ******************** CFDSURFC ********************
C Checks if a surface matches boundaries of a CFD grid cell.
C Checks the surface normal to ensure that we do not incorrectly match
C the wrong side of the cell; we should only match if the outside of the
C surface is the outside of the face.

C Assumes CFD-geometry transforms have been initialised by calling
C INIT_CFDTRANS.

C is - surface number
C (ii,ij,ik) - indices of CFD grid cell
C ifac - face of CFD cell: 0 = unknown, check and return
C                          1 = east
C                          2 = west
C                          12 = east or west
C                          -12 = any except east or west
C                          3 = north
C                          4 = south
C                          34 = north or south
C                          -34 = any except north or south
C                          5 = high
C                          6 = low
C                          56 = high or low
C                          -56 = any except high or low
C (iapnt,apol) - polygon data structures describing the surface such as
C                created by common/matpol.F: ENCPOL
C ans - logical decision if the surface matches

C Note that if the surface matches more than 1 face of the cell, you'll
C need to call this sub multiple times with different values of ifac, as
C it will only detect the first match in the order above.

      SUBROUTINE CFDSURFC(is,ii,ij,ik,ifac,iapnt,apol,ans,ier)
#include "building.h"
#include "cfd.h"

      integer is,ii,ij,ik,ifac,ier
      logical ans
      dimension apol(1000)
      
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/GEOM/XP(ntcelx),YP(ntcely),ZP(ntcelz),
     &            DXEP(ntcelx),DXPW(ntcelx),DYNP(ntcely),DYPS(ntcely),
     &            DZHP(ntcelz),DZPL(ntcelz),
     &            SEW(ntcelx),SNS(ntcely),SHL(ntcelz),
     &            XU(ntcelx),YV(ntcely),ZW(ntcelz)
      COMMON/G1M/XM(MTV),YM(MTV),ZM(MTV),NSURM,JVNM(MSM,MV),
     &           NVERM(MSM),NTVM

      character outs*128
      logical INSIDEP,coni,conj,conk
      dimension xic(MV),xjc(MV),xkc(MV),xdum(3),xeqn(4)

      ier=0

C Get point coordinates.
      xi=XP(ii); xj=YP(ij); xk=ZP(ik)

C Assign dimension from ifac.
      if (ifac.eq.1.or.ifac.eq.2.or.ifac.eq.12) then
        idim=1
        x1=xj; x2=xk
      elseif (ifac.eq.3.or.ifac.eq.4.or.ifac.eq.34) then
        idim=2
        x1=xi; x2=xk
      elseif (ifac.eq.5.or.ifac.eq.6.or.ifac.eq.56) then
        idim=3
        x1=xi; x2=xj
      elseif(ifac.eq.0.or.ifac.eq.-12.or.ifac.eq.-34.or.ifac.eq.-56)then
        idim=0
      else
        write(outs,'(a)')'CFDSURFC: invalid ifac value'
        CALL EDISP(IUOUT,outs)
        ier=1
        return
      endif

C First, make sure the centre of the cell is contained in the surface in
C the requested dimension. If face is unknown, use this as a check for
C what face it might be on.
      if (idim.eq.0) then
        icon=0
        if (INSIDEP(xj,xk,1,iapnt,apol).and.ifac.ne.-12) then
          coni=.true.
          icon=icon+1
        else
          coni=.false.
        endif
        if (INSIDEP(xi,xk,2,iapnt,apol).and.ifac.ne.-34) then
          conj=.true.
          icon=icon+1
        else
          conj=.false.
        endif
        if (INSIDEP(xi,xj,3,iapnt,apol).and.ifac.ne.-56) then
          conk=.true.
          icon=icon+1
        else
          conk=.false.
        endif

        if (icon.eq.0) then
C Not contained in any dimension; surface does not match.
          ans=.false.
          return
        endif

C Contained in at least one dimension.
C Check for the case that a surface bisects a cell such that the centre
C of the cell is contained, but does not match a face. We do this by
C checking each corner of the cell for containment; if none are
C contained in a particular dimension then this is true for this
C dimension.
        ximn=XU(ii); xjmn=YV(ij); xkmn=ZW(ik)
        ximx=XU(ii+1); xjmx=YV(ij+1); xkmx=ZW(ik+1)

        if (coni) then
          if (.not.INSIDEP(xjmn,xkmn,1,iapnt,apol) .and. 
     &        .not.INSIDEP(xjmn,xkmx,1,iapnt,apol) .and.
     &        .not.INSIDEP(xjmx,xkmn,1,iapnt,apol) .and.
     &        .not.INSIDEP(xjmx,xkmx,1,iapnt,apol)) then
            coni=.false.
            icon=icon-1
          endif
        endif          
        if (conj) then
          if (.not.INSIDEP(ximn,xkmn,2,iapnt,apol) .and. 
     &        .not.INSIDEP(ximn,xkmx,2,iapnt,apol) .and.
     &        .not.INSIDEP(ximx,xkmn,2,iapnt,apol) .and.
     &        .not.INSIDEP(ximx,xkmx,2,iapnt,apol)) then
            conj=.false.
            icon=icon-1
          endif
        endif          
        if (conk) then
          if (.not.INSIDEP(ximn,xjmn,3,iapnt,apol) .and. 
     &        .not.INSIDEP(ximn,xjmx,3,iapnt,apol) .and.
     &        .not.INSIDEP(ximx,xjmn,3,iapnt,apol) .and.
     &        .not.INSIDEP(ximx,xjmx,3,iapnt,apol)) then
            conk=.false.
            icon=icon-1
          endif
        endif

C Now, any remainaing containments should be legitimate.
        if (icon.eq.0) then
          ans=.false.
          return

        elseif (icon.eq.1) then
C Contained in 1 dimension; surface might match one of two faces.
          if (coni) then
            ifac=12
          elseif (conj) then
            ifac=34
          elseif (conk) then
            ifac=56
          endif

        elseif (icon.eq.2) then
C Contained in 2 dimensions.
          if (.not.coni) then
            ifac=-12
          elseif (.not.conj) then
            ifac=-34
          elseif (.not.conk) then
            ifac=-56
          endif

        elseif (icon.eq.3) then
C Contained in 3 dimensions, we need to check all faces.
          ifac=0
        endif

      else        
        if (INSIDEP(x1,x2,idim,iapnt,apol)) then
          if (idim.eq.1) then
            coni=.true.
          elseif (idim.eq.2) then
            conj=.true.
          elseif (idim.eq.3) then
            conk=.true.
          endif
        else
          ans=.false.
          return
        endif
      endif

C Check to see if the surface matches any possible faces.
C For a given dimension, if the surface is within the velocity cell
C thickness, at the centre of the cell in the plane, then it matches the
C face. Only allow matches that make sense given that we know which
C side of the surface is the outside.

C First we need to find the plane equation for the surface.
C Use G1M common; assume this has been converted to CFD coordinates
C using ESMCFDZON.
      do iv=1,NVERM(is)
        xic(iv)=XM(JVNM(is,iv))
        xjc(iv)=YM(JVNM(is,iv))
        xkc(iv)=ZM(JVNM(is,iv))
      enddo
      CALL PLEQN(xic,xjc,xkc,NVERM(is),xdum,xeqn,IER)
      if (IER.ne.0) then
        write(outs,'(a,i4)')
     &    'CFDSURFC: failed to find plane equation for surface',is
        CALL EDISP(IUOUT,outs)
        ier=1
        return
      endif

C Scan each face that we are allowed.
C East face.
      if (ifac.eq.1 .or. ifac.eq.12 .or. ifac.eq.0 .or. 
     &    ifac.eq.-34 .or. ifac.eq.-56) then
        if (coni .and. xeqn(1).lt.0) then
          xis=(xeqn(4)-xeqn(2)*xj-xeqn(3)*xk)/xeqn(1)
          ximn=XP(ii); ximx=XP(ii+1)
          if (xis.gt.ximn .and. xis.le.ximx) then ! it's a match!
            ifac=1
            ans=.true.
            return
          endif
        endif
      endif
      
C West face.
      if (ifac.eq.2 .or. ifac.eq.12 .or. ifac.eq.0 .or. 
     &    ifac.eq.-34 .or. ifac.eq.-56) then
        if (coni .and. xeqn(1).gt.0) then
          xis=(xeqn(4)-xeqn(2)*xj-xeqn(3)*xk)/xeqn(1)
          ximn=XP(ii-1); ximx=XP(ii)
          if (xis.gt.ximn .and. xis.le.ximx) then ! it's a match!
            ifac=2
            ans=.true.
            return
          endif
        endif
      endif

C North face.
      if (ifac.eq.3 .or. ifac.eq.34 .or. ifac.eq.0 .or. 
     &    ifac.eq.-12 .or. ifac.eq.-56) then
        if (conj .and. xeqn(2).lt.0) then
          xjs=(xeqn(4)-xeqn(1)*xi-xeqn(3)*xk)/xeqn(2)
          xjmn=YP(ij); xjmx=YP(ij+1)
          if (xjs.gt.xjmn .and. xjs.le.xjmx) then ! it's a match!
            ifac=3
            ans=.true.
            return
          endif
        endif
      endif
      
C South face.
      if (ifac.eq.4 .or. ifac.eq.34 .or. ifac.eq.0 .or. 
     &    ifac.eq.-12 .or. ifac.eq.-56) then
        if (conj .and. xeqn(2).gt.0) then
          xjs=(xeqn(4)-xeqn(1)*xi-xeqn(3)*xk)/xeqn(2)
          xjmn=YP(ij-1); xjmx=YP(ij)
          if (xjs.gt.xjmn .and. xjs.le.xjmx) then ! it's a match!
            ifac=4
            ans=.true.
            return
          endif
        endif
      endif
      
C High face.
      if (ifac.eq.5 .or. ifac.eq.56 .or. ifac.eq.0 .or. 
     &    ifac.eq.-12 .or. ifac.eq.-34) then
        if (conk .and. xeqn(3).lt.0) then
          xks=(xeqn(4)-xeqn(1)*xi-xeqn(2)*xj)/xeqn(3)
          xkmn=ZP(ik); xkmx=ZP(ik+1)
          if (xks.gt.xkmn .and. xks.le.xkmx) then ! it's a match!
            ifac=5
            ans=.true.
            return
          endif
        endif
      endif
      
C Low face.
      if (ifac.eq.6 .or. ifac.eq.56 .or. ifac.eq.0 .or. 
     &    ifac.eq.-12 .or. ifac.eq.-34) then
        if (conk .and. xeqn(3).gt.0) then
          xks=(xeqn(4)-xeqn(1)*xi-xeqn(2)*xj)/xeqn(3)
          xkmn=ZP(ik-1); xkmx=ZP(ik)
          if (xks.gt.xkmn .and. xks.le.xkmx) then ! it's a match!
            ifac=6
            ans=.true.
            return
          endif
        endif
      endif

C If we get to this point, it hasn't matched anything.
      ans=.false.
      return

      end

C ******************** GENVOBC ********************
C Used in the auto-generation of CFD BCs inferred from geometry.

C Creates blockage BCs to represent visual entities, contracting where
C required to try and avoid intersecting blockages.

C ISTAT = 1: Error.
C ISTAT = 2: Entity(s) skipped, but no error.

      SUBROUTINE GENVOBC(istat)
#include "building.h"
#include "geometry.h"
#include "cfd.h"

      integer istat

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/ICFNOD/ICFD,ICP      
      COMMON/GB1/XB(12),YB(12),ZB(12),JVNB(6,4)
      COMMON/GEOM/XP(ntcelx),YP(ntcely),ZP(ntcelz),
     &            DXEP(ntcelx),DXPW(ntcelx),DYNP(ntcely),DYPS(ntcely),
     &            DZHP(ntcelz),DZPL(ntcelz),
     &            SEW(ntcelx),SNS(ntcely),SHL(ntcelz),
     &            XU(ntcelx),YV(ntcely),ZW(ntcelz)
      common/blksso/NBLK(MNZ),INBLK(MNVLS,MNZ),NSSO(MNZ),
     &          INSSO(MNVLS,MNZ),BLKTEMP(MNVLS,6)

      character outs*124,vetyp*4,t12*12,tt12*12
      logical ok,cureng,preveng,show,dou,dov,dow
      real eumin,eumax,evmin,evmax,ewmin,ewmax,dum(1)
      real tolumin,tolumax,tolvmin,tolvmax,tolwmin,tolwmax
      integer aicls(8,3)
      real ec(8,3) ! coordinates of entity vertices in CFD domain
      integer aieext(MB,6) ! entity extents in scalar cell indices.
      integer aiestat(MB) ! status of each entity:
C -1: skipped
C 0:  block
C 1:  plane, surface normal in the u direction
C 2:  plane, surface normal in the v direction
C 3:  plane, surface normal in the w direction
C 4:  stick, length in u direction
C 5:  stick, length in v direction
C 6:  stick, length in w direction
C 7:  blob
C A "plane" is an entity 1 cell thick in 1 dimension,
C a "stick" is an entity 1 cell thick in 2 dimensions,
C a "blob" is an entity 1 cell thick in 3 dimensions (i.e. a single cell);
C otherwise an entity is a "block".
      integer aiext(6),ajext(6) ! entity extents, instantiated depending
C on status to check for intersections.
      integer aiesplt(MB) ! entity that new ones were split from.
      integer aie2nrw(MB) ! "too-narrow-ness":
C Takes values 1 to 7, composed of the sum of:
C 1 - too narrow in the u direction
C 2 - too narrow in the v direction
C 4 - too narrow in the w direction
C Therefore:
C 3 - u and v
C 5 - u and w
C 6 - v and w
C 7 - u, v, and w

      ISTAT=0

C Loop over visual entities.
      ne=NBVIS(ICP)
      do ive=1,NBVIS(ICP)

C Hijack the loop to initialise aiesplt.
        aiesplt(ive)=0

        vetyp=VISTYP(ICP,ive)
        t12=VISNAME(ICP,ive)

        write(outs,'(a,i3.3,1x,2a)')' Processing visual entity ',ive,
     &    t12(1:lnblnk(t12)),' ...'
        CALL EDISP(IUOUT,outs)

C Convert entity description into coordinates in common GB1.
        if(vetyp.eq.'vis ')then
          CALL CNVBLK(XOV(ICP,ive),YOV(ICP,ive),ZOV(ICP,ive),
     &      DXOV(ICP,ive),DYOV(ICP,ive),DZOV(ICP,ive),
     &      BANGOV(ICP,ive,1))
        elseif(vetyp.eq.'vis3')then
          CALL CNVBLK3A(XOV(ICP,ive),YOV(ICP,ive),ZOV(ICP,ive),
     &      DXOV(ICP,ive),DYOV(ICP,ive),DZOV(ICP,ive),
     &      BANGOV(ICP,ive,1),BANGOV(ICP,ive,2),BANGOV(ICP,ive,3))
        elseif(vetyp.eq.'visp')then
          call CNVVISP(ICP,ive)
        endif

C Establish extents.
        eumin=99999.9; eumax=0.0
        evmin=99999.9; evmax=0.0
        ewmin=99999.9; ewmax=0.0
        IER=0
        ok=.true.
        do I=1,8
          CALL CFDTRANS(2,XB(I),YB(I),ZB(I),u,v,w,IER)
          if (IER.ne.0) then
            ok=.false.
            EXIT
          endif
          ec(I,1)=u; ec(I,2)=v; ec(I,3)=w
          if (u.lt.eumin) eumin=u
          if (u.gt.eumax) eumax=u
          if (v.lt.evmin) evmin=v
          if (v.gt.evmax) evmax=v
          if (w.lt.ewmin) ewmin=w
          if (w.gt.ewmax) ewmax=w
        enddo
        if (.not.ok) then          
          write(outs,'(a)')
     &      ' Error transforming coordinates. Check gridding.'
          CALL EDISP(IUOUT,outs)
          ISTAT=1
          goto 999
        endif

C Find grid cells to best match entity vertices. Note that this will
C find indices on the velocity grid. This is negatively offset from the
C scalar grid, so for example XU(1) is the negative U edge of the scalar
C cell XP(1), and the positive U edge is XU(2). So the actual (scalar)
C grid cells to use are ie?min and ie?max-1.
        CALL FDCFDPT(2,eumin,evmin,ewmin,ieumin,ievmin,iewmin,IER)
        CALL FDCFDPT(2,eumax,evmax,ewmax,ieumax,ievmax,iewmax,IER)
        if (IER.ne.0) then
          write(outs,'(a)')
     &      ' Error finding cells to match geometry. Check gridding.'
          CALL EDISP(IUOUT,outs)
          ISTAT=1
          goto 999
        endif

C Establish narrowness.
C "Too narrow" is defined as less than half the cells width.
C "Narrow" is defined as a single cell.
C Store "too-narrow-ness" for detecting surface details.
        inarrow=0
        itoonarrow=0
        aie2nrw(ive)=0
        if (ieumin.eq.ieumax .or. ieumin.eq.ieumax-1) then
          inarrow=inarrow+1
          if ((eumax-eumin).lt.(SEW(ieumin)/2.0)) then
            itoonarrow=itoonarrow+1
            aie2nrw(ive)=aie2nrw(ive)+1
          endif
        endif
        if (ievmin.eq.ievmax .or. ievmin.eq.ievmax-1) then
          inarrow=inarrow+2
          if ((evmax-evmin).lt.(SNS(ievmin)/2.0)) then
            itoonarrow=itoonarrow+1
            aie2nrw(ive)=aie2nrw(ive)+2
          endif
        endif
        if (iewmin.eq.iewmax .or. iewmin.eq.iewmax-1) then
          inarrow=inarrow+4
          if ((ewmax-ewmin).lt.(SHL(iewmin)/2.0)) then
            itoonarrow=itoonarrow+1
            aie2nrw(ive)=aie2nrw(ive)+4
          endif
        endif

C If it is too narrow in at least 2 dimensions, then it is not large
C enough to represent.
        if (itoonarrow.ge.2) then
          write(outs,'(2a)')
     &      ' This entity is too narrow, skipping.'
          CALL EDISP(IUOUT,outs)
          aiestat(ive)=-1
          ne=ne-1
          CYCLE

C If it is narrow in at least 1 dimension, check for cases where the
C extents straddle a cell boundary, and work out where it should go. In
C the unlikely case that the cell boundary is on the exact midpoint,
C default to the negative side in the axial direction.
        elseif (inarrow.gt.0) then
          if (ieumin.eq.ieumax) then
            if (abs(eumin-XP(ieumin-1)).le.abs(eumax-XP(ieumax))) then
              ieumin=ieumin-1
            else
              ieumax=ieumax+1
            endif
          endif
          if (ievmin.eq.ievmax) then
            if (abs(evmin-YP(ievmin-1)).le.abs(evmax-YP(ievmax))) then
              ievmin=ievmin-1
            else
              ievmax=ievmax+1
            endif
          endif
          if (iewmin.eq.iewmax) then
            if (abs(ewmin-ZP(iewmin-1)).le.abs(ewmax-ZP(iewmax))) then
              iewmin=iewmin-1
            else
              iewmax=iewmax+1
            endif
          endif
        endif

C Now make sure the entity is roughly oriented with the CFD domain axes.
C For directions in which the entity is not narrow, ensure that 
C coordinates for each vertex are within half the cells width of the
C nearest extent. To protect against cases of thin diagonals, in each
C plane of two non-narrow directions, ensure that more than two vertices
C are not both close to one corner.

C Set tolerances.
        if (inarrow.eq.0 .or. inarrow.eq.2 .or.
     &      inarrow.eq.4 .or. inarrow.eq.6) then
          tolumin=abs(XU(ieumin)-XP(ieumin))
          tolumax=abs(XU(ieumax)-XP(ieumax-1))
          dou=.true.
        else
          dou=.false.
        endif
        if (inarrow.eq.0 .or. inarrow.eq.1 .or. 
     &      inarrow.eq.4 .or. inarrow.eq.5) then
          tolvmin=abs(YV(ievmin)-YP(ievmin))
          tolvmax=abs(YV(ievmax)-YP(ievmax-1))
          dov=.true.
        else
          dov=.false.
        endif
        if (inarrow.eq.0 .or. inarrow.eq.1 .or. 
     &      inarrow.eq.2 .or. inarrow.eq.3) then
          tolwmin=abs(ZW(iewmin)-ZP(iewmin))
          tolwmax=abs(ZW(iewmax)-ZP(iewmax-1))
          dow=.true.
        else
          dow=.false.
        endif

        do I=1,8

C Check for closeness to extents.
          if (dou) then
            CALL ECLOSE(ec(I,1),eumin,tolumin,ok)
            if (ok) then
              aicls(I,1)=1
            else
              CALL ECLOSE(ec(I,1),eumax,tolumax,ok)
              if (ok) then
                aicls(I,1)=2
              else
                EXIT
              endif
            endif
          endif
          if (dov) then
            CALL ECLOSE(ec(I,2),evmin,tolvmin,ok)
            if (ok) then
              aicls(I,2)=1
            else
              CALL ECLOSE(ec(I,2),evmax,tolvmax,ok)
              if (ok) then
                aicls(I,2)=2
              else
                EXIT
              endif
            endif
          endif
          if (dow) then
            CALL ECLOSE(ec(I,3),ewmin,tolwmin,ok)
            if (ok) then
              aicls(I,3)=1
            else
              CALL ECLOSE(ec(I,3),ewmax,tolwmax,ok)
              if (ok) then
                aicls(I,3)=2
              else
                EXIT
              endif
            endif
          endif

C Check for thin diagonals.
          if (I.gt.1) then

C Number of vertices close to corners of the bounding box in each plane.
            iuvnn=0; iuvnx=0; iuvxx=0; iuvxn=0
            iuwnn=0; iuwnx=0; iuwxx=0; iuwxn=0
            ivwnn=0; ivwnx=0; ivwxx=0; ivwxn=0

            do J=I-1,1,-1
              if (dou.and.dov) then
                if (aicls(I,1).eq.1 .and. aicls(J,1).eq.1) then
                  if (aicls(I,2).eq.1 .and. aicls(J,2).eq.1) then
                    iuvnn=iuvnn+1
                    if (iuvnn.gt.2) then
                      ok=.false.
                      EXIT
                    endif
                  elseif (aicls(I,2).eq.2 .and. aicls(J,2).eq.2) then
                    iuvnx=iuvnx+1
                    if (iuvnx.gt.2) then
                      ok=.false.
                      EXIT
                    endif
                  endif
                elseif (aicls(I,1).eq.2 .and. aicls(J,1).eq.2) then
                  if (aicls(I,2).eq.1 .and. aicls(J,2).eq.1) then
                    iuvxn=iuvxn+1
                    if (iuvxn.gt.2) then
                      ok=.false.
                      EXIT
                    endif
                  elseif (aicls(I,2).eq.2 .and. aicls(J,2).eq.2) then
                    iuvxx=iuvxx+1
                    if (iuvxx.gt.2) then
                      ok=.false.
                      EXIT
                    endif
                  endif
                endif
              endif
              if (dou.and.dow) then
                if (aicls(I,1).eq.1 .and. aicls(J,1).eq.1) then
                  if (aicls(I,3).eq.1 .and. aicls(J,3).eq.1) then
                    iuwnn=iuwnn+1
                    if (iuwnn.gt.2) then
                      ok=.false.
                      EXIT
                    endif
                  elseif (aicls(I,3).eq.2 .and. aicls(J,3).eq.2) then
                    iuwnx=iuwnx+1
                    if (iuwnx.gt.2) then
                      ok=.false.
                      EXIT
                    endif
                  endif
                elseif (aicls(I,1).eq.2 .and. aicls(J,1).eq.2) then
                  if (aicls(I,3).eq.1 .and. aicls(J,3).eq.1) then
                    iuwxn=iuwxn+1
                    if (iuwxn.gt.2) then
                      ok=.false.
                      EXIT
                    endif
                  elseif (aicls(I,3).eq.2 .and. aicls(J,3).eq.2) then
                    iuwxx=iuwxx+1
                    if (iuwxx.gt.2) then
                      ok=.false.
                      EXIT
                    endif
                  endif
                endif
              endif
              if (dov.and.dow) then
                if (aicls(I,2).eq.1 .and. aicls(J,2).eq.1) then
                  if (aicls(I,3).eq.1 .and. aicls(J,3).eq.1) then
                    ivwnn=ivwnn+1
                    if (ivwnn.gt.2) then
                      ok=.false.
                      EXIT
                    endif
                  elseif (aicls(I,3).eq.2 .and. aicls(J,3).eq.2) then
                    ivwnx=ivwnx+1
                    if (ivwnx.gt.2) then
                      ok=.false.
                      EXIT
                    endif
                  endif
                elseif (aicls(I,2).eq.2 .and. aicls(J,2).eq.2) then
                  if (aicls(I,3).eq.1 .and. aicls(J,3).eq.1) then
                    ivwxn=ivwxn+1
                    if (ivwxn.gt.2) then
                      ok=.false.
                      EXIT
                    endif
                  elseif (aicls(I,3).eq.2 .and. aicls(J,3).eq.2) then
                    ivwxx=ivwxx+1
                    if (ivwxx.gt.2) then
                      ok=.false.
                      EXIT
                    endif
                  endif
                endif
              endif
            enddo
            if (.not.ok) EXIT
          endif
        enddo

        if (.not.ok) then
          write(outs,'(a)')
     &      ' This entity is not aligned with CFD axes, skipping.'
          CALL EDISP(IUOUT,outs)
          aiestat(ive)=-1
          ne=ne-1
          CYCLE
        endif

C Determine if entity is a block, a plane, a stick, or a blob.
C A "plane" is an entity 1 cell thick in 1 dimension,
C a "stick" is an entity 1 cell thick in 2 dimensions,
C a "blob" is an entity 1 cell thick in 3 dimensions (i.e. a single cell);
C otherwise an entity is a "block".
        if (inarrow.eq.1) then ! plane, normal in u direction
          aiestat(ive)=1
        elseif (inarrow.eq.2) then ! plane, normal in v direction
          aiestat(ive)=2
        elseif (inarrow.eq.4) then ! plane, normal in w direction
          aiestat(ive)=3
        elseif (inarrow.eq.3) then ! stick, pointing in w direction
          aiestat(ive)=6
        elseif (inarrow.eq.5) then ! stick, pointing in v direction
          aiestat(ive)=5
        elseif (inarrow.eq.6) then ! stick, pointing in u direction
          aiestat(ive)=4
        elseif (inarrow.eq.7) then ! blob
          aiestat(ive)=7
        elseif (inarrow.eq.0) then! block
          aiestat(ive)=0          
        else
          write(outs,'(a)')' Error determining entity status.'
          CALL EDISP(IUOUT,outs)
          ISTAT=1
          goto 999
        endif

C Store entity extents, in terms of scalar cells.
        aieext(ive,1)=ieumin; aieext(ive,2)=ieumax-1
        aieext(ive,3)=ievmin; aieext(ive,4)=ievmax-1
        aieext(ive,5)=iewmin; aieext(ive,6)=iewmax-1

        write(outs,'(a)')' No problems.'
        CALL EDISP(IUOUT,outs)

      enddo

C We now have the extents and status of all entities that we want to
C create. Check if there are none left.
      if (ne.le.0) goto 999

C Scan through and check for intersections. Follow these rules to
C resolve them:
C 1: First, if any entity is completely engulfed by another, skip it.
C 2: Next, if a stick intersects anything, shorten or split it.
C 3: Next, if a plane intersects anything, split it or slice stick(s)
C    off the offending edge(s).
C 4: Finally, if a block intersects anything, split the block or slice
C    plane(s) off the offending face(s).
      write(outs,'(a)')' Scanning entities for intersections ...'
      CALL EDISP(IUOUT,outs)
      isplt=0



C First, scan for engulfing. This will handle any blobs.
      do i=1,NBVIS(ICP)
        if (aiestat(i).eq.-1) CYCLE

C Is this entity engulfed by any other?
        do j=1,NBVIS(ICP)
          if (i.eq.j .or. aiestat(j).eq.-1) CYCLE          
          if (aieext(i,1).ge.aieext(j,1) .and. 
     &        aieext(i,2).le.aieext(j,2) .and.
     &        aieext(i,3).ge.aieext(j,3) .and. 
     &        aieext(i,4).le.aieext(j,4) .and.
     &        aieext(i,5).ge.aieext(j,5) .and. 
     &        aieext(i,6).le.aieext(j,6)) then

C Yes, so skip it.
            aiestat(i)=-1
            t12=VISNAME(ICP,i)
            tt12=VISNAME(ICP,j)
            write(outs,'(a,i3.3,1x,2a,i3.3,1x,2a)')' Entity ',
     &        i,t12(1:lnblnk(t12)),' is engulfed by entity ',
     &        j,tt12(1:lnblnk(tt12)),', skipping.'
            call edisp(IUOUT,outs)
            EXIT
          endif
        enddo
      enddo



C Now, scan sticks (aiestat 4-6).
C Instantiate the number of entities and use a while instead of a for,
C because the number of entities we need to scan may increase due to
C splitting of existing entities.
      ne=NBVIS(ICP)
      i=0
      do while (i.le.ne)
        i=i+1
        if (aiestat(i).lt.4 .or. aiestat(i).gt.6) CYCLE

C aiext - extents of the "i" entity depending on orientation:
C aiext(1) - negative end of the stick
C aiext(2) - positive end of the stick
C aiext(3 and 4) - coordinates of the blob seen by looking down the
C                  length of the stick.
        if (aiestat(i).eq.1) then ! plane normal is in u direction
          aiext(1)=aieext(i,1); aiext(2)=aieext(i,2)
          aiext(3)=aieext(i,3); aiext(4)=aieext(i,5)
        elseif (aiestat(i).eq.2) then ! plane normal is in v direction
          aiext(1)=aieext(i,3); aiext(2)=aieext(i,4)
          aiext(3)=aieext(i,1); aiext(4)=aieext(i,5)
        elseif (aiestat(i).eq.3) then ! plane normal is in w direction
          aiext(1)=aieext(i,5); aiext(2)=aieext(i,6)
          aiext(3)=aieext(i,1); aiext(4)=aieext(i,3)
        endif

C Scan through all other entities (comparison entities).
C We know that any entity split from the "i" entity won't intersect it,
C so don't worry about increasing the value of ne inside the loop.
        ne2=ne
        do j=1,ne2
          if (i.eq.j .or. aiestat(j).eq.-1) CYCLE

C ajext - extents of the comparison entity depending on orientation of
C the "i" entity:
C aiext(1 and 2) - extents in the direction of the length of the stick
C aiext(3, 4, 5 and 6) - extents in the plane to which the stick is
C                        normal
          if (aiestat(i).eq.4) then ! stick is in u direction
            ajext(1)=aieext(j,1); ajext(2)=aieext(j,2)
            ajext(3)=aieext(j,3); ajext(4)=aieext(j,4)
            ajext(5)=aieext(j,5); ajext(6)=aieext(j,6)
          elseif (aiestat(i).eq.5) then ! stick is in v direction
            ajext(1)=aieext(j,3); ajext(2)=aieext(j,4)
            ajext(3)=aieext(j,1); ajext(4)=aieext(j,2)
            ajext(5)=aieext(j,5); ajext(6)=aieext(j,6)
          elseif (aiestat(i).eq.6) then ! stick is in w direction
            ajext(1)=aieext(j,5); ajext(2)=aieext(j,6)
            ajext(3)=aieext(j,1); ajext(4)=aieext(j,2)
            ajext(5)=aieext(j,3); ajext(6)=aieext(j,4)
          endif

C If the negative end of the stick is beyond the positive edge of the 
C comparison entity, or if the positive end of the stick is before the
C negative edge of the comparison entity, the two do not intersect.
          if (aiext(1).gt.ajext(2) .or. aiext(2).lt.ajext(1)) CYCLE

C Check for the case of a surface detail. If the stick is adjacent to
C the comparison entity, and too narrow in that direction, and engulfed
C by the comparison entity in the plane normal to that direction, then
C it's probably a surface detail - ignore it.
          if (aie2nrw(i).gt.0) then
            ok=.true.
            if (aiestat(i).eq.4) then
              if ((aiext(3).eq.ajext(3)-1 .or. 
     &             aiext(3).eq.ajext(4)+1) .and.
     &            (aie2nrw(i).eq.2 .or. aie2nrw(i).eq.3 .or.
     &             aie2nrw(i).eq.6 .or. aie2nrw(i).eq.7) .and.
     &            (aiext(1).ge.ajext(1) .and. aiext(2).le.ajext(2) .and.
     &             aiext(4).ge.ajext(5) .and. aiext(4).le.ajext(6)))then
                ok=.false.

              elseif ((aiext(4).eq.ajext(5)-1 .or. 
     &                 aiext(4).eq.ajext(6)+1) .and.
     &                (aie2nrw(i).eq.4 .or. aie2nrw(i).eq.5 .or.
     &                 aie2nrw(i).eq.6 .or. aie2nrw(i).eq.7) .and.
     &            (aiext(1).ge.ajext(1) .and. aiext(2).le.ajext(2) .and.
     &             aiext(3).ge.ajext(3) .and. aiext(3).le.ajext(4)))then
                ok=.false.
              endif

            elseif (aiestat(i).eq.5) then
              if ((aiext(3).eq.ajext(3)-1 .or. 
     &             aiext(3).eq.ajext(4)+1) .and.
     &            (aie2nrw(i).eq.1 .or. aie2nrw(i).eq.3 .or.
     &             aie2nrw(i).eq.5 .or. aie2nrw(i).eq.7) .and.
     &            (aiext(1).ge.ajext(1) .and. aiext(2).le.ajext(2) .and.
     &             aiext(4).ge.ajext(5) .and. aiext(4).le.ajext(6)))then
                ok=.false.

              elseif ((aiext(4).eq.ajext(5)-1 .or. 
     &                 aiext(4).eq.ajext(6)+1) .and.
     &                (aie2nrw(i).eq.4 .or. aie2nrw(i).eq.5 .or.
     &                 aie2nrw(i).eq.6 .or. aie2nrw(i).eq.7) .and.
     &            (aiext(1).ge.ajext(1) .and. aiext(2).le.ajext(2) .and.
     &             aiext(3).ge.ajext(3) .and. aiext(3).le.ajext(4)))then
                ok=.false.
              endif

            elseif (aiestat(i).eq.6) then
              if ((aiext(3).eq.ajext(3)-1 .or. 
     &             aiext(3).eq.ajext(4)+1) .and.
     &            (aie2nrw(i).eq.1 .or. aie2nrw(i).eq.3 .or.
     &             aie2nrw(i).eq.5 .or. aie2nrw(i).eq.7) .and.
     &            (aiext(1).ge.ajext(1) .and. aiext(2).le.ajext(2) .and.
     &             aiext(4).ge.ajext(5) .and. aiext(4).le.ajext(6)))then
                ok=.false.

              elseif ((aiext(4).eq.ajext(5)-1 .or. 
     &                 aiext(4).eq.ajext(6)+1) .and.
     &                (aie2nrw(i).eq.2 .or. aie2nrw(i).eq.3 .or.
     &                 aie2nrw(i).eq.6 .or. aie2nrw(i).eq.7) .and.
     &            (aiext(1).ge.ajext(1) .and. aiext(2).le.ajext(2) .and.
     &             aiext(3).ge.ajext(3) .and. aiext(3).le.ajext(4)))then
                ok=.false.
              endif
            endif

            if (.not.ok) then
              aiestat(i)=-1
              t12=VISNAME(ICP,i)
              tt12=VISNAME(ICP,j)
              write(outs,'(a,i3.3,1x,2a,i3.3,1x,2a)')' Entity ',i,
     &          t12(1:lnblnk(t12)),' is a surface detail of entity ',
     &          j,tt12(1:lnblnk(tt12)),', skipping.'
              CALL edisp(IUOUT,outs)
              EXIT
            endif
          endif

C If the stick blob is not engulfed by the projection of the comparison
C entity, the two do not intersect.
          if (.not.(aiext(3).ge.ajext(3) .and.
     &              aiext(3).le.ajext(4) .and.
     &              aiext(4).ge.ajext(5) .and.
     &              aiext(4).le.ajext(6))) CYCLE

C Debug.
          write(outs,'(a,i3.3,a,i3.3)')
     &      ' Entity ',i,' intersects entity ',j
          call edisp(IUOUT,outs)

C Is the negative end of the stick engulfed?
          preveng=aiext(1).ge.ajext(1)

C Check for a stick-that-is-actually-a-blob; this can happen if a stick
C has been shortened or split.
          if (aiext(1).eq.aiext(2)) then
            if (preveng) then
              aiestat(i)=-1
              EXIT
            endif
          endif

C Scan through the stick (negative to positive) for any change in
C engulfed-ness.
          iengst=0
          ok=.false.
          do k=aiext(1)+1,aiext(2)
            cureng=k.ge.ajext(1) .and. k.le.ajext(2)
            if (cureng .and. .not.preveng) then 

C Current cell is engulfed, previous isn't. Remember this cell and carry
C on scanning, to see if the stick comes out the other end.
              iengst=k

            elseif (preveng .and. .not.cureng) then 

C Current cell is not engulfed, previous is. 
              if (iengst.gt.0) then

C Stick is bisected by the comparison entity; split the stick.
C Note that we set the status of the new entity as a stick, even if it
C is a blob; this is to ensure that the "i" entity loop picks it up and
C checks it against other entities (the code for sticks should handle
C blobs just fine).
                ne=ne+1
                if (aiestat(i).eq.4) then
                  aieext(i,2)=iengst-1
                  aieext(ne,1)=k;           aieext(ne,2)=aiext(2)
                  aieext(ne,3)=aieext(i,3); aieext(ne,4)=aieext(i,4)
                  aieext(ne,5)=aieext(i,5); aieext(ne,6)=aieext(i,6)
                elseif (aiestat(i).eq.5) then
                  aieext(i,4)=iengst-1
                  aieext(ne,1)=aieext(i,1); aieext(ne,2)=aieext(i,2)
                  aieext(ne,3)=k;           aieext(ne,4)=aiext(2)
                  aieext(ne,5)=aieext(i,5); aieext(ne,6)=aieext(i,6)
                elseif (aiestat(i).eq.6) then
                  aieext(i,6)=iengst-1
                  aieext(ne,1)=aieext(i,1); aieext(ne,2)=aieext(i,2)
                  aieext(ne,3)=aieext(i,3); aieext(ne,4)=aieext(i,4)
                  aieext(ne,5)=k;           aieext(ne,6)=aiext(2)
                endif

C Update current instantiated i entity extents.
                if (aiestat(i).eq.1) then ! plane normal is in u direction
                  aiext(1)=aieext(i,1); aiext(2)=aieext(i,2)
                  aiext(3)=aieext(i,3); aiext(4)=aieext(i,5)
                elseif (aiestat(i).eq.2) then ! plane normal is in v direction
                  aiext(1)=aieext(i,3); aiext(2)=aieext(i,4)
                  aiext(3)=aieext(i,1); aiext(4)=aieext(i,5)
                elseif (aiestat(i).eq.3) then ! plane normal is in w direction
                  aiext(1)=aieext(i,5); aiext(2)=aieext(i,6)
                  aiext(3)=aieext(i,1); aiext(4)=aieext(i,3)
                endif

                aiestat(ne)=aiestat(i)
                isplt=isplt+1
                aiesplt(isplt)=i
                ok=.true.
                EXIT

              else

C Negative end of the stick is engulfed up to this point; shorten the
C stick.
                if (aiestat(i).eq.4) then
                  aieext(i,1)=k
                elseif (aiestat(i).eq.5) then
                  aieext(i,3)=k
                elseif (aiestat(i).eq.6) then
                  aieext(i,5)=k
                endif

C Update current instantiated i entity extents.
                if (aiestat(i).eq.1) then ! plane normal is in u direction
                  aiext(1)=aieext(i,1); aiext(2)=aieext(i,2)
                  aiext(3)=aieext(i,3); aiext(4)=aieext(i,5)
                elseif (aiestat(i).eq.2) then ! plane normal is in v direction
                  aiext(1)=aieext(i,3); aiext(2)=aieext(i,4)
                  aiext(3)=aieext(i,1); aiext(4)=aieext(i,5)
                elseif (aiestat(i).eq.3) then ! plane normal is in w direction
                  aiext(1)=aieext(i,5); aiext(2)=aieext(i,6)
                  aiext(3)=aieext(i,1); aiext(4)=aieext(i,3)
                endif

                ok=.true.
                EXIT
              endif
            endif
            preveng=cureng
          enddo

C If ok is true, we've already dealt with the intersection.
          if (ok) CYCLE

C If we've reached this point then we've now been through the entire
C stick. In theory, the only way we should get to this point is if the
C positive end of the stick is engulfed, and we need to shorten it.
          if (iengst.gt.0) then
            if (aiestat(i).eq.4) then
              aieext(i,2)=iengst-1
            elseif (aiestat(i).eq.5) then
              aieext(i,4)=iengst-1
            elseif (aiestat(i).eq.6) then
              aieext(i,6)=iengst-1
            endif

C Update current instantiated i entity extents.
            if (aiestat(i).eq.1) then ! plane normal is in u direction
              aiext(1)=aieext(i,1); aiext(2)=aieext(i,2)
              aiext(3)=aieext(i,3); aiext(4)=aieext(i,5)
            elseif (aiestat(i).eq.2) then ! plane normal is in v direction
              aiext(1)=aieext(i,3); aiext(2)=aieext(i,4)
              aiext(3)=aieext(i,1); aiext(4)=aieext(i,5)
            elseif (aiestat(i).eq.3) then ! plane normal is in w direction
              aiext(1)=aieext(i,5); aiext(2)=aieext(i,6)
              aiext(3)=aieext(i,1); aiext(4)=aieext(i,3)
            endif

            CYCLE
          endif

C If we get to here, something has gone wrong.
          t12=VISNAME(ICP,i)
          tt12=VISNAME(ICP,j)
          write(outs,'(a,i3.3,1x,2a,i3.3,1x,2a)')
     &      ' Error while scanning for intersection of entities ',i,
     &      t12(1:lnblnk(t12)),' and ',j,tt12(1:lnblnk(tt12)),'.'
          CALL edisp(IUOUT,outs)
          ISTAT=1
          goto 999
        enddo ! next comparison entity
      enddo ! next i entity




C Now, scan planes (aiestat 1-3).
      i=0
      do while (i.le.ne)
        i=i+1
        if (aiestat(i).lt.1 .or. aiestat(i).gt.3) CYCLE

C aiext - extents of the "i" entity depending on orientation:
C aiext(1, 2 , 3 and 4) - extents of the plane looking from the
C                         direction of its normal
C aiext(5) - coordinate in the normal axis (plane constant)
        if (aiestat(i).eq.1) then ! plane normal is in u direction
          aiext(1)=aieext(i,3); aiext(2)=aieext(i,4)
          aiext(3)=aieext(i,5); aiext(4)=aieext(i,6)
          aiext(5)=aieext(i,1)
        elseif (aiestat(i).eq.2) then ! plane normal is in v direction
          aiext(1)=aieext(i,1); aiext(2)=aieext(i,2)
          aiext(3)=aieext(i,5); aiext(4)=aieext(i,6)
          aiext(5)=aieext(i,3)
        elseif (aiestat(i).eq.3) then ! plane normal is in w direction
          aiext(1)=aieext(i,1); aiext(2)=aieext(i,2)
          aiext(3)=aieext(i,3); aiext(4)=aieext(i,4)
          aiext(5)=aieext(i,5)
        endif

C Scan through all other entities (comparison entities).
        ne2=ne
        do j=1,ne2
          if (i.eq.j .or. aiestat(j).eq.-1) CYCLE

C ajext - extents of the comparison entity depending on orientation of
C the "i" entity:
C ajext(1, 2, 3 and 4) - extents looking from the direction normal to
C                        the "i" plane
C ajext(5 and 6) - extents in the direction of the normal.
          if (aiestat(i).eq.1) then ! plane normal is in u direction
            ajext(1)=aieext(j,3); ajext(2)=aieext(j,4)
            ajext(3)=aieext(j,5); ajext(4)=aieext(j,6)
            ajext(5)=aieext(j,1); ajext(6)=aieext(j,2)
          elseif (aiestat(i).eq.2) then ! plane normal is in v direction
            ajext(1)=aieext(j,1); ajext(2)=aieext(j,2)
            ajext(3)=aieext(j,5); ajext(4)=aieext(j,6)
            ajext(5)=aieext(j,3); ajext(6)=aieext(j,4)
          elseif (aiestat(i).eq.3) then ! plane normal is in w direction
            ajext(1)=aieext(j,1); ajext(2)=aieext(j,2)
            ajext(3)=aieext(j,3); ajext(4)=aieext(j,4)
            ajext(5)=aieext(j,5); ajext(6)=aieext(j,6)
          endif

C In each of the two directions of the plane, if the negative edge of
C the plane is beyond the positive edge of the comparison entity, or if
C the positive end of the plane is before the negative edge of the
C comparison entity, the two do not intersect.
          if (aiext(1).gt.ajext(2) .or. aiext(2).lt.ajext(1) .or.
     &        aiext(3).gt.ajext(4) .or. aiext(4).lt.ajext(3)) CYCLE

C Check for the case of a surface detail. If the plane is adjacent to
C the comparison entity, and too narrow in that direction, and engulfed
C by the comparison entity in the plane normal to that direction, then
C it's probably a surface detail - ignore it.
          if (aie2nrw(i).gt.0) then
            ok=.true.
            if (aiestat(i).eq.1) then
              if ((aiext(5).eq.ajext(5)-1 .or. 
     &             aiext(5).eq.ajext(6)+1) .and.
     &            (aie2nrw(i).eq.1 .or. aie2nrw(i).eq.3 .or.
     &             aie2nrw(i).eq.5 .or. aie2nrw(i).eq.7) .and.
     &            (aiext(1).ge.ajext(1) .and. aiext(2).le.ajext(2) .and.
     &             aiext(3).ge.ajext(3) .and. aiext(4).le.ajext(4)))then
                ok=.false.
              endif

            elseif (aiestat(i).eq.2) then
              if ((aiext(5).eq.ajext(5)-1 .or. 
     &             aiext(5).eq.ajext(6)+1) .and.
     &            (aie2nrw(i).eq.2 .or. aie2nrw(i).eq.3 .or.
     &             aie2nrw(i).eq.6 .or. aie2nrw(i).eq.7) .and.
     &            (aiext(1).ge.ajext(1) .and. aiext(2).le.ajext(2) .and.
     &             aiext(3).ge.ajext(3) .and. aiext(4).le.ajext(4)))then
                ok=.false.
              endif

            elseif (aiestat(i).eq.3) then
              if ((aiext(5).eq.ajext(5)-1 .or. 
     &             aiext(5).eq.ajext(6)+1) .and.
     &            (aie2nrw(i).eq.4 .or. aie2nrw(i).eq.5 .or.
     &             aie2nrw(i).eq.6 .or. aie2nrw(i).eq.7) .and.
     &            (aiext(1).ge.ajext(1) .and. aiext(2).le.ajext(2) .and.
     &             aiext(3).ge.ajext(3) .and. aiext(4).le.ajext(4)))then
                ok=.false.
              endif
            endif

            if (.not.ok) then
              aiestat(i)=-1
              t12=VISNAME(ICP,i)
              tt12=VISNAME(IVP,j)
              write(outs,'(a,i3.3,1x,2a,i3.3,1x,2a)')' Entity ',i,
     &          t12(1:lnblnk(t12)),' is a surface detail of entity ',j,
     &          tt12(1:lnblnk(tt12)),', skipping.'
              CALL edisp(IUOUT,outs)
              EXIT
            endif
          endif

C If the plane constant is not within the extents of the comparison
C entity in that direction, the two do not intersect.
          if (.not.(aiext(5).ge.ajext(5) .and.
     &              aiext(5).le.ajext(6))) CYCLE

C Debug.
          write(outs,'(a,i3.3,a,i3.3)')
     &      ' Entity ',i,' intersects entity ',j
          call edisp(IUOUT,outs)

          ichkdm=0

C There should be an intersection somewhere. There are 3 possible
C scenarios:
C 1 - A nice easy case where the plane is entirely bisected or sliced by
C     the comparison entity. 
C 2 - The comparison entity makes a hole in the plane. 
C 3 - The comparison entity takes a chunk out of the edge of the plane.
C     This is currently not handled.
          if (aiext(1).ge.ajext(1) .and. aiext(2).le.ajext(2)) then

C Plane is engulfed at some point in its first dimension - search
C dimension is the second.
            ichkdm=3

          elseif (aiext(3).ge.ajext(3) .and. aiext(4).le.ajext(4)) then

C Plane is engulfed at some point in its second dimension - search
C dimension is the first.
            ichkdm=1

          elseif (aiext(1).lt.ajext(1) .and. aiext(2).gt.ajext(2) .and.
     &            aiext(3).lt.ajext(3) .and. aiext(4).gt.ajext(4)) then

C Comparison entity makes a hole in the plane. In this case, do nothing
C - when the comparison entity is scanned, it will be bisected.
            CYCLE

          else

C The comparison entity takes a chunk out of the edge of the plane.
C Currently this is not handled, so skip the plane.
            aiestat(i)=-1
            t12=VISNAME(ICP,i)
            tt12=VISNAME(IVP,j)
            write(outs,'(a,i3.3,1x,2a,i3.3,1x,2a,i3.3,a)')
     &        ' Intersection of entities ',i,t12(1:lnblnk(t12)),
     &        ' and ',j,tt12(1:lnblnk(tt12)),
     &        ' is too complex, skipping entity ',i,'.'
            call edisp(IUOUT,outs)
            ISTAT=2
            EXIT
          endif  

C Is the negative edge of the plane engulfed?
          preveng=aiext(ichkdm).ge.ajext(ichkdm)

C Check for a plane-that-is-actually-a-stick in the search dimension;
C this can happen if a stick has been shortened or split.
          if (aiext(ichkdm).eq.aiext(ichkdm+1)) then
            if (preveng) then
              aiestat(i)=-1
              EXIT
            endif
          endif

C Scan through the plane in the search dimension (negative to positive)
C for any change in engulfed-ness.
          iengst=0
          ok=.false.
          do k=aiext(ichkdm)+1,aiext(ichkdm+1)
            cureng=k.ge.ajext(ichkdm) .and. k.le.ajext(ichkdm+1)
            if (cureng .and. .not.preveng) then 

C Current stick is engulfed, previous isn't. Remember this cell and
C carry on scanning, to see if the plane comes out the other end.
              iengst=k

            elseif (preveng .and. .not.cureng) then 

C Current stick is not engulfed, previous is. 
              if (iengst.gt.0) then

C Plane is bisected by the comparison entity; split the plane.
C Note that we set the status of the new entity as a plane, even if it
C is a stick; this is to ensure that the "i" entity loop picks it up and
C checks it against other entities (the code for planes should handle
C sticks just fine).
                ne=ne+1
                if (aiestat(i).eq.1) then
                  if (ichkdm.eq.1) then ! split in v direction
                    aieext(i,4)=iengst-1
                    aieext(ne,1)=aieext(i,1); aieext(ne,2)=aieext(i,2)
                    aieext(ne,3)=k;           aieext(ne,4)=aiext(2)
                    aieext(ne,5)=aieext(i,5); aieext(ne,6)=aieext(i,6)
                  elseif (ichkdm.eq.3) then ! split in z direction
                    aieext(i,6)=iengst-1
                    aieext(ne,1)=aieext(i,1); aieext(ne,2)=aieext(i,2)
                    aieext(ne,3)=aieext(i,3); aieext(ne,4)=aieext(i,4)
                    aieext(ne,5)=k;           aieext(ne,6)=aiext(4)
                  endif
                elseif (aiestat(i).eq.2) then
                  if (ichkdm.eq.1) then ! split in u direction
                    aieext(i,2)=iengst-1
                    aieext(ne,1)=k;           aieext(ne,2)=aiext(2)
                    aieext(ne,3)=aieext(i,3); aieext(ne,4)=aieext(i,4)
                    aieext(ne,5)=aieext(i,5); aieext(ne,6)=aieext(i,6)
                  elseif (ichkdm.eq.3) then ! split in z direction
                    aieext(i,6)=iengst-1
                    aieext(ne,1)=aieext(i,1); aieext(ne,2)=aieext(i,2)
                    aieext(ne,3)=aieext(i,3); aieext(ne,4)=aieext(i,4)
                    aieext(ne,5)=k;           aieext(ne,6)=aiext(4)
                  endif
                elseif (aiestat(i).eq.3) then
                  if (ichkdm.eq.1) then ! split in u direction
                    aieext(i,2)=iengst-1
                    aieext(ne,1)=k;           aieext(ne,2)=aiext(2)
                    aieext(ne,3)=aieext(i,3); aieext(ne,4)=aieext(i,4)
                    aieext(ne,5)=aieext(i,5); aieext(ne,6)=aieext(i,6)
                  elseif (ichkdm.eq.3) then ! split in v direction
                    aieext(i,4)=iengst-1
                    aieext(ne,1)=aieext(i,1); aieext(ne,2)=aieext(i,2)
                    aieext(ne,3)=k;           aieext(ne,4)=aiext(4)
                    aieext(ne,5)=aieext(i,5); aieext(ne,6)=aieext(i,6)
                  endif
                endif

C Update current instantiated i entity extents.
                if (aiestat(i).eq.1) then ! plane normal is in u direction
                  aiext(1)=aieext(i,3); aiext(2)=aieext(i,4)
                  aiext(3)=aieext(i,5); aiext(4)=aieext(i,6)
                  aiext(5)=aieext(i,1)
                elseif (aiestat(i).eq.2) then ! plane normal is in v direction
                  aiext(1)=aieext(i,1); aiext(2)=aieext(i,2)
                  aiext(3)=aieext(i,5); aiext(4)=aieext(i,6)
                  aiext(5)=aieext(i,3)
                elseif (aiestat(i).eq.3) then ! plane normal is in w direction
                  aiext(1)=aieext(i,1); aiext(2)=aieext(i,2)
                  aiext(3)=aieext(i,3); aiext(4)=aieext(i,4)
                  aiext(5)=aieext(i,5)
                endif

                aiestat(ne)=aiestat(i)
                isplt=isplt+1
                aiesplt(isplt)=i
                ok=.true.
                EXIT

              else

C Negative end of the plane is engulfed up to this point; shorten the
C plane.
                if (aiestat(i).eq.1) then
                  if (ichkdm.eq.1) then ! shorten in v direction
                    aieext(i,3)=k
                  elseif (ichkdm.eq.3) then ! shorten in z direction
                    aieext(i,5)=k
                  endif
                elseif (aiestat(i).eq.2) then
                  if (ichkdm.eq.1) then ! shorten in u direction
                    aieext(i,1)=k
                  elseif (ichkdm.eq.3) then ! shorten in z direction
                    aieext(i,5)=k
                  endif
                elseif (aiestat(i).eq.3) then
                  if (ichkdm.eq.1) then ! shorten in u direction
                    aieext(i,1)=k
                  elseif (ichkdm.eq.3) then ! shorten in v direction
                    aieext(i,3)=k
                  endif
                endif

C Update current instantiated i entity extents.
                if (aiestat(i).eq.1) then ! plane normal is in u direction
                  aiext(1)=aieext(i,3); aiext(2)=aieext(i,4)
                  aiext(3)=aieext(i,5); aiext(4)=aieext(i,6)
                  aiext(5)=aieext(i,1)
                elseif (aiestat(i).eq.2) then ! plane normal is in v direction
                  aiext(1)=aieext(i,1); aiext(2)=aieext(i,2)
                  aiext(3)=aieext(i,5); aiext(4)=aieext(i,6)
                  aiext(5)=aieext(i,3)
                elseif (aiestat(i).eq.3) then ! plane normal is in w direction
                  aiext(1)=aieext(i,1); aiext(2)=aieext(i,2)
                  aiext(3)=aieext(i,3); aiext(4)=aieext(i,4)
                  aiext(5)=aieext(i,5)
                endif

                ok=.true.
                EXIT
              endif
            endif
            preveng=cureng
          enddo

C If ok is true, we've already dealt with the intersection.
          if (ok) CYCLE

C If we've reached this point then we've now been through the entire
C search. In theory, the only way we should get to this point is if the
C positive end of the plane is engulfed, and we need to shorten it.
          if (iengst.gt.0) then
            if (aiestat(i).eq.1) then
              if (ichkdm.eq.1) then ! shorten in v direction
                aieext(i,4)=iengst-1
              elseif (ichkdm.eq.3) then ! shorten in z direction
                aieext(i,6)=iengst-1
              endif
            elseif (aiestat(i).eq.2) then
              if (ichkdm.eq.1) then ! shorten in u direction
                aieext(i,2)=iengst-1
              elseif (ichkdm.eq.3) then ! shorten in z direction
                aieext(i,6)=iengst-1
              endif
            elseif (aiestat(i).eq.3) then
              if (ichkdm.eq.1) then ! shorten in u direction
                aieext(i,2)=iengst-1
              elseif (ichkdm.eq.3) then ! shorten in v direction
                aieext(i,4)=iengst-1
              endif
            endif

C Update current instantiated i entity extents.
            if (aiestat(i).eq.1) then ! plane normal is in u direction
              aiext(1)=aieext(i,3); aiext(2)=aieext(i,4)
              aiext(3)=aieext(i,5); aiext(4)=aieext(i,6)
              aiext(5)=aieext(i,1)
            elseif (aiestat(i).eq.2) then ! plane normal is in v direction
              aiext(1)=aieext(i,1); aiext(2)=aieext(i,2)
              aiext(3)=aieext(i,5); aiext(4)=aieext(i,6)
              aiext(5)=aieext(i,3)
            elseif (aiestat(i).eq.3) then ! plane normal is in w direction
              aiext(1)=aieext(i,1); aiext(2)=aieext(i,2)
              aiext(3)=aieext(i,3); aiext(4)=aieext(i,4)
              aiext(5)=aieext(i,5)
            endif

            CYCLE
          endif

C If we get to here, something has gone wrong.
          t12=VISNAME(ICP,i)
          tt12=VISNAME(IVP,j)
          write(outs,'(a,i3.3,1x,2a,i3.3,1x,2a)')
     &      ' Error while scanning for intersection of entities ',i,
     &      t12(1:lnblnk(t12)),' and ',j,tt12(1:lnblnk(tt12)),'.'
          ISTAT=1
          goto 999
        enddo ! next comparison entity
      enddo ! next i entity

C Now, scan blocks (aiestat 0).
      i=0
      do while (i.le.ne)
        i=i+1
        if (aiestat(i).ne.0) CYCLE

C aiext - extents of the "i" entity.
        aiext(1)=aieext(i,1); aiext(2)=aieext(i,2)
        aiext(3)=aieext(i,3); aiext(4)=aieext(i,4)
        aiext(5)=aieext(i,5); aiext(6)=aieext(i,6)

C Scan through all other entities (comparison entities).
        ne2=ne
        do j=1,ne2
          if (i.eq.j .or. aiestat(j).eq.-1) CYCLE

C ajext - extents of the comparison entity.
          ajext(1)=aieext(j,1); ajext(2)=aieext(j,2)
          ajext(3)=aieext(j,3); ajext(4)=aieext(j,4)
          ajext(5)=aieext(j,5); ajext(6)=aieext(j,6)

C In each of the three dimensions, if the negative edge of the block is
C beyond the positive edge of the comparison entity, or if the positive
C end of the block is before the negative edge of the comparison entity,
C the two do not intersect.
          if (aiext(1).gt.ajext(2) .or. aiext(2).lt.ajext(1) .or.
     &        aiext(3).gt.ajext(4) .or. aiext(4).lt.ajext(3) .or.
     &        aiext(5).gt.ajext(5) .or. aiext(6).lt.ajext(6)) CYCLE

C Debug.
          write(outs,'(a,i3.3,a,i3.3)')
     &      ' Entity ',i,' intersects entity ',j
          call edisp(IUOUT,outs)

          ichkdm=0

C There should be an intersection somewhere. There are 2 possible
C scenarios:
C 1 - A nice easy case where the block is entirely engulfed by the
C     comparison entity in two dimensions.
C 2 - The reverse; the comparison entity is entirely engulfed by the
C     block in two dimensions.
C 2 - The comparison entity takes a chunk out of one or more face(s) of
C     the block. This is currently not handled.
          if (aiext(1).ge.ajext(1) .and. aiext(2).le.ajext(2) .and. 
     &        aiext(3).ge.ajext(3) .and. aiext(4).le.ajext(4)) then

C Block is engulfed is the u and v directions - search dimension is w.
            ichkdm=5
            
          elseif (ajext(1).ge.aiext(1) .and. ajext(2).le.aiext(2) .and. 
     &        ajext(3).ge.aiext(3) .and. ajext(4).le.aiext(4)) then
     
C Block engulfs comparison entity in two dimensions. In this case do
C nothing - when the comparison entity is scanned, it should be handled
C correctly.
            CYCLE

          elseif (aiext(1).ge.ajext(1) .and. aiext(2).le.ajext(2) .and. 
     &            aiext(5).ge.ajext(5) .and. aiext(6).le.ajext(6)) then

C Block is engulfed is the u and w directions - search dimension is v.
            ichkdm=3
            
          elseif (ajext(1).ge.aiext(1) .and. ajext(2).le.aiext(2) .and. 
     &            ajext(5).ge.aiext(5) .and. ajext(6).le.aiext(6)) then
            CYCLE

          elseif (aiext(3).ge.ajext(3) .and. aiext(4).le.ajext(4) .and. 
     &            aiext(5).ge.ajext(5) .and. aiext(6).le.ajext(6)) then

C Block is engulfed is the v and w directions - search dimension is u.
            ichkdm=1

          elseif (ajext(3).ge.aiext(3) .and. ajext(4).le.aiext(4) .and. 
     &            ajext(5).ge.aiext(5) .and. ajext(6).le.aiext(6)) then
            CYCLE

          else

C The comparison entity takes a chunk out of one or more face(s) of the
C block. Currently this is not handled, so skip the block.
            aiestat(i)=-1
            t12=VISNAME(ICP,i)
            tt12=VISNAME(IVP,j)
            write(outs,'(a,i3.3,1x,2a,i3.3,1x,2a,i3.3,a)')
     &        ' Intersection of entities ',i,t12(1:lnblnk(t12)),
     &        ' and ',j,tt12(1:lnblnk(tt12)),
     &        ' is too complex, skipping entity ',i,'.'
            call edisp(IUOUT,outs)
            ISTAT=2
            EXIT
          endif  

C Is the negative edge of the block engulfed?
          preveng=aiext(ichkdm).ge.ajext(ichkdm)

C Check for a block-that-is-actually-a-plane in the search dimension;
C this can happen if a block has been shortened or split.
          if (aiext(ichkdm).eq.aiext(ichkdm+1)) then
            if (preveng) then
              aiestat(i)=-1
              EXIT
            endif
          endif

C Scan through the block in the search dimension (negative to positive)
C for any change in engulfed-ness.
          iengst=0
          ok=.false.
          do k=aiext(ichkdm)+1,aiext(ichkdm+1)
            cureng=k.ge.ajext(ichkdm) .and. k.le.ajext(ichkdm+1)
            if (cureng .and. .not.preveng) then 

C Current plane is engulfed, previous isn't. Remember this cell and
C carry on scanning, to see if the block comes out the other end.
              iengst=k

            elseif (preveng .and. .not.cureng) then 

C Current plane is not engulfed, previous is. 
              if (iengst.gt.0) then

C Block is bisected by the comparison entity; split the block.
C Note that we set the status of the new entity as a block, even if it
C is a plane; this is to ensure that the "i" entity loop picks it up and
C checks it against other entities (the code for blocks should handle
C planes just fine).
                ne=ne+1
                if (ichkdm.eq.1) then ! split in u direction
                  aieext(i,2)=iengst-1
                  aieext(ne,1)=k;           aieext(ne,2)=aiext(2)
                  aieext(ne,3)=aieext(i,3); aieext(ne,4)=aieext(i,4)
                  aieext(ne,5)=aieext(i,5); aieext(ne,6)=aieext(i,6)
                elseif (ichkdm.eq.3) then ! split in v direction                  
                  aieext(i,4)=iengst-1
                  aieext(ne,1)=aieext(i,1); aieext(ne,2)=aieext(i,2)
                  aieext(ne,3)=k;           aieext(ne,4)=aiext(4)
                  aieext(ne,5)=aieext(i,5); aieext(ne,6)=aieext(i,6)
                elseif (ichkdm.eq.3) then ! split in z direction
                  aieext(i,6)=iengst-1
                  aieext(ne,1)=aieext(i,1); aieext(ne,2)=aieext(i,2)
                  aieext(ne,3)=aieext(i,3); aieext(ne,4)=aieext(i,4)
                  aieext(ne,5)=k;           aieext(ne,6)=aiext(6)
                endif

C Update current instantiated i entity extents.
                aiext(1)=aieext(i,1); aiext(2)=aieext(i,2)
                aiext(3)=aieext(i,3); aiext(4)=aieext(i,4)
                aiext(5)=aieext(i,5); aiext(6)=aieext(i,6)

                aiestat(ne)=aiestat(i)
                isplt=isplt+1
                aiesplt(isplt)=i
                ok=.true.
                EXIT

              else

C Negative end of the block is engulfed up to this point; slice planes
C off the block.
                if (ichkdm.eq.1) then ! shorten in u direction
                  aieext(i,1)=k
                elseif (ichkdm.eq.3) then ! shorten in v direction
                  aieext(i,3)=k
                elseif (ichkdm.eq.5) then ! shorten in z direction
                  aieext(i,5)=k
                endif

C Update current instantiated i entity extents.
                aiext(1)=aieext(i,1); aiext(2)=aieext(i,2)
                aiext(3)=aieext(i,3); aiext(4)=aieext(i,4)
                aiext(5)=aieext(i,5); aiext(6)=aieext(i,6)

                ok=.true.
                EXIT
              endif
            endif
            preveng=cureng
          enddo

C If ok is true, we've already dealt with the intersection.
          if (ok) CYCLE

C If we've reached this point then we've now been through the entire
C search. In theory, the only way we should get to this point is if the
C positive end of the block is engulfed, and we need to slice planes off
C it.
          if (iengst.gt.0) then
            if (ichkdm.eq.1) then ! shorten in u direction
              aieext(i,2)=iengst-1
            elseif (ichkdm.eq.3) then ! shorten in v direction
              aieext(i,4)=iengst-1
            elseif (ichkdm.eq.5) then ! shorten in z direction
              aieext(i,6)=iengst-1
            endif

C Update current instantiated i entity extents.
            aiext(1)=aieext(i,1); aiext(2)=aieext(i,2)
            aiext(3)=aieext(i,3); aiext(4)=aieext(i,4)
            aiext(5)=aieext(i,5); aiext(6)=aieext(i,6)

            CYCLE
          endif

C If we get to here, something has gone wrong.
          ive=VOBJILIST(ICP,ivo,i)
          jve=VOBJILIST(ICP,ivo,j)
          t12=VISNAME(ICP,i)
          tt12=VISNAME(IVP,j)
          write(outs,'(a,i3.3,1x,2a,i3.3,1x,2a)')
     &      ' Error while scanning for intersection of entities ',i,
     &      t12(1:lnblnk(t12)),' and ',j,tt12(1:lnblnk(tt12)),'.'
          ISTAT=1
          goto 999
        enddo ! next comparison entity
      enddo ! next i entity
      call edisp(IUOUT,' Done.')

C Make blockages.
      show=.true.
      dum=(/0/)
      do i=1,ne
        if (aiestat(i).eq.-1) CYCLE

        if (i.le.NBVIS(ICP)) then
          ive=i
        else          
          ive=aiesplt(ne-NBVIS(ICP))
        endif

        write (t12,'(a,i3.3,a,i3.3)')'blk',NBLK(ICFD)+1,'_ve',ive
        tt12='            '
        call MKCFDBC(show,30,8,aieext(i,1),aieext(i,2),aieext(i,3),
     &    aieext(i,4),aieext(i,5),aieext(i,6),t12,tt12,dum,1,0.0,0.0,
     &    0.0,0.0,0.0,IER)
      enddo

  999 RETURN
      END


C ******************** GENEDGBC ********************
C Used in the auto-generation of CFD BCs inferred from geometry.
C The subroutine is recursive because child surfaces can also
C be parents.

C Given a surface index isur, the extent of each axis ?max, and
C optionally the face index ifac, this subroutine:
C    finds the surface extents and, if ifac=0, determines what boundary
C    face the surface lies on,
C    checks the usage attributes to determine what boundary conditions
C    are needed (returned in ibctyp),
C    checks if the surface is rectangular and oriented with the CFD
C    domain axes,
C    finds the CFD grid cells that best match the extents of the surface
C    (returned in is[u/v/w][min/max]),
C    checks for child surfaces, runs this subroutine on them, and
C    confirms that we can handle the parent-child topology,
C    creates BC(s) for child surface(s) if applicable,
C    creates BC(s) for parent surface if applicable,

C ifac = 0: don't know yet, check
C ifac = 1: West
C ifac = 2: East
C ifac = 3: South
C ifac = 4: North
C ifac = 5: Low
C ifac = 6: High
C ifac = 7: not on a boundary face, surface normal in the u direction
C ifac = 8: not on a boundary face, surface normal in the v direction
C ifac = 9: not on a boundary face, surface normal in the w direction

C ibctyp = 1:  solid bc(s) for the whole surface
C ibctyp = 2:  single opening bc for the whole surface
C ibctyp = 3:  opening bc for bottom row of cells, solid bc for the rest
C ibctyp = 4:  opening bc for middle row of cells, solid bcs for the rest
C ibctyp = 5:  opening bc for 25% of height, at bottom edge, solid bc for
C              the rest
C ibctyp = 6:  opening bc for 25% of width, at vertical centre line,
C              solid bcs for rest
C ibctyp = 7:  opening bcs for 12.5% of height, at top and bottom edges,
C              solid bc for rest
C ibctyp = 8:  opening bc for 75% of height and width, at bottom edge,
C              solid bcs for rest
C ibctyp = 9:  for frames; opening bc as close to middle row of parent
C              and child as we can get
C ibctyp = 10: for frames; opening bc for 20% of child width, as close
C              to top edge as we can get

C ISTAT = 1: Fatal error, already reported.
C ISTAT = 2: Surface is not rectangular or aligned.
C ISTAT = 3: More than 1 child.
C ISTAT = 4: Surface is too narrow.
C ISTAT = 5: Problem with usage tags.
C ISTAT = 6: Surface is not on a boundary.

C doafnasc is currently unused, pending further development.

      RECURSIVE SUBROUTINE GENEDGBC(isur,umax,vmax,wmax,ifac,ibctyp,
     &  isumin,isumax,isvmin,isvmax,iswmin,iswmax,doafnasc,ISTAT)
#include "building.h"
#include "geometry.h"
#include "cfd.h"
#include "help.h"

      integer isur,ifac,ibctyp,isumin,isumax,isvmin,isvmax,iswmin,iswmax
      real umax,vmax,wmax,umin,vmin,wmin
      logical doafnasc

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/ICFNOD/ICFD,ICP
      COMMON/CFDVIS/HAS_GEOM,ISHSB,ISHAO,IFACES,ISHBLK,ISHSRC,ISHGEO,
     &              INITD
      logical HAS_GEOM
      character INITD*6
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      COMMON/C24/IZSTOCN(MCOM,MS)
      common/KEYVOLS/NVOL(MNZ),IVOLF(MNVLS,MNZ),IVCELLS(MNVLS,MNZ,2),
     &               JVCELLS(MNVLS,MNZ,2),KVCELLS(MNVLS,MNZ,2)
      COMMON/NDMAP/NOPEN(MNZ),MFNODE(MCFND,MNZ),IOPENi(MCFND,MNZ),
     &             IOPENf(MCFND,MNZ),JOPENi(MCFND,MNZ),
     &             JOPENf(MCFND,MNZ),KOPENi(MCFND,MNZ),
     &             KOPENf(MCFND,MNZ),FIXM(MCFND,MNZ),
     &             FIXT(MCFND,MNZ),FIXC(MCFND,MNZ),
     &             FIXK(MCFND,MNZ),FIXE(MCFND,MNZ),
     &             IWOPEN(MCFND,MNZ),ICFDCN(MCFND,MNZ),
     &             ICNACT(MCFND,MNZ),IVOLNOP(MCFND,MNZ)
      COMMON/Sbdary/NSB(MNZ),ISBi(MNSBZ,MNZ),ISBf(MNSBZ,MNZ),
     &              JSBi(MNSBZ,MNZ),JSBf(MNSBZ,MNZ),
     &              KSBi(MNSBZ,MNZ),KSBf(MNSBZ,MNZ),
     &              ISUFLC(MNSBZ,MNZ),IWSB(MNSBZ,MNZ),SSB(MNSBZ,MNZ),
     &              SSBHC(MNSBZ,MNZ),IVOLNSB(MNSBZ,MNZ),
     &              ITCtype(MNSBZ,MNZ),icTREF(MNSBZ,MNZ)

      logical ok,dok,dochild(MCHILD),dochildren,dothisur,lumped,show
      logical isrect
      logical bumin,bumax,bvmin,bvmax,bwmin,bwmax
      character outs*124,t12*12,facstr*1,snam*12
      real dum(1),sumin,sumax,svmin,svmax,swmin,swmax
      real u,v,w,ua,va,wa
      integer iparext(4),iparca,icldext(MCHILD,4),icldtotca,isclose(4),
     &        icldbctyp(MCHILD),icldverts(MV)

      helpinsub='edcfd'  ! set for calling subroutine

      ISTAT=0
      dum=(/0/)
      show=.true.
      icnn=IZSTOCN(ICP,isur)  ! connection for the parent surface

C Check if this surface is a child. Unless it is also a parent, it will
C be created by the invocation of this subroutine for its parent.
      if (IPARENT(icnn).gt.0) then
        if (NBCHILD(icnn).gt.0) then
          dothisur=.true.
        else          
          dothisur=.false.
        endif
      else
        dothisur=.true.
      endif

      snam=SNAME(ICP,isur) ! surface name

C Establish surface extents, and if not already known determine what
C face the surface lies on if any (south, west, low, etc).
C b?min/max  = is on the face matching the min/max extent of the domain
C              in the ? direction (logical).
C s?min/max  = min/max extent of the surface in the ? direction in CFD
C              domain coords (real).
      bumin=.TRUE.; bumax=.TRUE.
      bvmin=.TRUE.; bvmax=.TRUE.
      bwmin=.TRUE.; bwmax=.TRUE.
      iminmax=6
      umin=0.0; vmin=0.0; wmin=0.0
      sumin=umax; sumax=umin
      svmin=vmax; svmax=vmin
      swmin=wmax; swmax=wmin
      IER=0
      do I=1,isznver(ICP,isur)
        IVRT=iszjvn(ICP,isur,I)
        CALL CFDTRANS(2,szcoords(ICP,IVRT,1),szcoords(ICP,IVRT,2),
     &    szcoords(ICP,IVRT,3),u,v,w,IER)
        if (IER.ne.0) then
          write(outs,'(2a,i3.3,1x,2a)')'Error transforming ',
     &      'coordinates for surface ',ISUR,snam(1:lnblnk(snam)),
     &      '. Check gridding.'
          CALL EDISP(IUOUT,outs)
          iminmax=-1
          EXIT
        endif
        if (u.lt.sumin) sumin=u
        if (u.gt.sumax) sumax=u
        if (v.lt.svmin) svmin=v
        if (v.gt.svmax) svmax=v
        if (w.lt.swmin) swmin=w
        if (w.gt.swmax) swmax=w

        if (ifac.eq.0) then
          if (bumin) then
            call eclose(umin,u,0.01,ok)
            if (.not.ok) then
              bumin=.FALSE.
              iminmax=iminmax-1
            endif
          endif
          if (bumax) then
            call eclose(umax,u,0.01,ok)
            if (.not.ok) then
              bumax=.FALSE.
              iminmax=iminmax-1
            endif
          endif
          if (bvmin) then
            call eclose(vmin,v,0.01,ok)
            if (.not.ok) then
              bvmin=.FALSE.
              iminmax=iminmax-1
            endif
          endif
          if (bvmax) then
            call eclose(vmax,v,0.01,ok)
            if (.not.ok) then
              bvmax=.FALSE.
              iminmax=iminmax-1
            endif
          endif
          if (bwmin) then
            call eclose(wmin,w,0.01,ok)
            if (.not.ok) then
              bwmin=.FALSE.
              iminmax=iminmax-1
            endif
          endif
          if (bwmax) then
            call eclose(wmax,w,0.01,ok)
            if (.not.ok) then
              bwmax=.FALSE.
              iminmax=iminmax-1
            endif
          endif
        endif
      enddo

      if (ifac.eq.0) then
        if (iminmax.eq.0) then

C Surface is not on a boundary face. Next surface.
          write(outs,'(a,i3.3,1x,2a)')'Surface ',ISUR,
     &      snam(1:lnblnk(snam)),
     &      ' is not on a boundary face, skipping.'
          CALL EDISP(IUOUT,outs)
          ISTAT=6
          goto 999
        elseif (iminmax.eq.1) then

C Surface is on a boundary. Continue.           
          write(outs,'(a,i3.3,1x,2a)')'Surface ',ISUR,
     &     snam(1:lnblnk(snam)),' is on a boundary face, processing ...'
          CALL EDISP(IUOUT,outs)
          if (bumin) then ! west
            ifac=1
          elseif (bumax) then ! east
            ifac=2
          elseif (bvmin) then ! south
            ifac=3
          elseif (bvmax) then ! north
            ifac=4
          elseif (bwmin) then ! low
            ifac=5
          elseif (bwmax) then ! high
            ifac=6
          endif
        elseif (iminmax.gt.1) then

C More than one boundary face. Notify the user, then skip to the next
C surface.
          write(outs,'(a,i3.3,1x,3a)')'Surface ',ISUR,
     &      snam(1:lnblnk(snam)),
     &    ' seems to be on more than one boundary face. Check gridding.'
          CALL EDISP(IUOUT,outs)
          ISTAT=1
          goto 999
        else

C Something has gone wrong, presumably already reported. Skip this
C surface.
          ISTAT=1
          goto 999
        endif
      endif

C Now check if the surface is rectangular and aligned with the axial
C directions.
C First, check if this surface is a parent. If it is, we first need to
C process the child(ren) - wait until later.
      if (NBCHILD(icnn).gt.0) then
        isrect=.false.
      else

C Ensure that each vertex shares at least one coordinate in the plane of
C the boundary face with the next vertex.
        do I=1,isznver(ICP,isur)
          IVRT=iszjvn(ICP,isur,I)
          CALL CFDTRANS(2,szcoords(ICP,IVRT,1),szcoords(ICP,IVRT,2),
     &      szcoords(ICP,IVRT,3),u,v,w,IER)
          if (I.eq.isznver(ICP,isur)) then
            JVRT=iszjvn(ICP,isur,1)
          else
            JVRT=iszjvn(ICP,isur,I+1)
          endif
          CALL CFDTRANS(2,szcoords(ICP,JVRT,1),szcoords(ICP,JVRT,2),
     &      szcoords(ICP,JVRT,3),ua,va,wa,IER)
          if (ifac.eq.1 .or. ifac.eq.2) then
            call ECLOSE(v,va,0.01,ok)
            if (.not.ok) call ECLOSE(w,wa,0.01,ok)
          elseif (ifac.eq.3 .or. ifac.eq.4) then
            call ECLOSE(u,ua,0.01,ok)
            if (.not.ok) call ECLOSE(w,wa,0.01,ok)
          elseif (ifac.eq.5 .or. ifac.eq.6) then
            call ECLOSE(u,ua,0.01,ok)
            if (.not.ok) call ECLOSE(v,va,0.01,ok)
          endif
          if (.not.ok) then
            ISTAT=2
            EXIT
          endif
        enddo
        if (ISTAT.eq.2) then
          CALL EDISP(IUOUT,
     &    ' This surface is not aligned with CFD axes, skipping.')
          goto 999
        endif

C Check if the surface is rectangular.
C If the surface has less than 4 vertices, it definitely isn't
C rectanglar.
        if (isznver(ICP,isur).lt.4) then
          ISTAT=2
          CALL EDISP(IUOUT,
     &      ' This surface is not rectangular, skipping.')
          goto 999

C If it has 4 vertices, passing the check for alignment means a valid
C surface must be a rectangle.
        elseif (isznver(ICP,isur).eq.4) then          
          isrect=.true.

C Otherwise, the surface has more than 4 vertices. Check for the case of
C a rectangle with extra vertices in the sides.
        else
          icorners=0
          do I=1,isznver(ICP,isur)
            IVRT=iszjvn(ICP,isur,I)
            CALL CFDTRANS(2,szcoords(ICP,IVRT,1),szcoords(ICP,IVRT,2),
     &        szcoords(ICP,IVRT,3),u,v,w,IER)
            if (ifac.eq.1 .or. ifac.eq.2) then
              call ECLOSE(v,svmin,0.01,ok)
              if (.not.ok) call ECLOSE(v,svmax,0.01,ok)
              call ECLOSE(w,swmin,0.01,dok)
              if (.not.dok) call ECLOSE(w,swmax,0.01,dok)
              if (ok.and.dok) icorners=icorners+1
              if (ok.or.dok) CYCLE
            elseif (ifac.eq.3 .or. ifac.eq.4) then
              call ECLOSE(u,sumin,0.01,ok)
              if (.not.ok) call ECLOSE(u,sumax,0.01,ok)
              call ECLOSE(w,swmin,0.01,dok)
              if (.not.dok) call ECLOSE(w,swmax,0.01,dok)
              if (ok.and.dok) icorners=icorners+1
              if (ok.or.dok) CYCLE
            elseif (ifac.eq.5 .or. ifac.eq.6) then
              call ECLOSE(u,sumin,0.01,ok)
              if (.not.ok) call ECLOSE(u,sumax,0.01,ok)
              call ECLOSE(v,svmin,0.01,dok)
              if (.not.dok) call ECLOSE(v,svmax,0.01,dok)
              if (ok.and.dok) icorners=icorners+1
              if (ok.or.dok) CYCLE
            endif
            ISTAT=2
            EXIT
          enddo
          if (icorners.eq.4.and.ISTAT.eq.0) then
            isrect=.true.
          else

C The surface is not rectangular; ignore it for now.
            ISTAT=2
            CALL EDISP(IUOUT,
     &        ' This surface is not rectangular, skipping.')
            goto 999
          endif
          isrect=.true.
        endif
      endif

C Initialise face variables.
      if (ifac.eq.1) then
        IFACES=3
        facstr='W'
      elseif (ifac.eq.2) then
        IFACES=3
        facstr='E'
      elseif (ifac.eq.3) then
        IFACES=2
        facstr='S'
      elseif (ifac.eq.4) then
        IFACES=2
        facstr='N'
      elseif (ifac.eq.5) then
        IFACES=4
        facstr='L'
      elseif (ifac.eq.6) then
        IFACES=4
        facstr='H'
      endif

C Sniff around usage flags to determine what this surface is. This code
C is designed to be as consistent as possible with flow network
C autogeneration (scan_flow_use in mfprb3.F). If the usage flags in
C geometry files are updated, this code shouild be updated accordingly.
      ibctyp=0
      t12=SUSE(ICP,isur,1)
      if (t12(1:4).eq.'WALL') then
        ibctyp=1

      elseif (t12(1:4).eq.'DOOR' .or. t12(3:6).eq.'DOOR') then
        t12=SUSE(ICP,isur,2)
        if (t12(1:6).eq.'CLOSED') then
          ibctyp=1
        elseif (t12(1:8).eq.'UNDERCUT') then
          ibctyp=3
        elseif (t12(1:4).eq.'OPEN') then
          ibctyp=2
        elseif (t12(1:5).eq.'BIDIR') then
          ibctyp=2
        else
          goto 51
        endif

      elseif (t12(1:5).eq.'FRAME' .or. t12(3:7).eq.'FRAME') then
        t12=SUSE(ICP,isur,2)
        if (t12(1:6).eq.'CLOSED') then
          ibctyp=1
        elseif (t12(1:5).eq.'CRACK') then
          ibctyp=9
        elseif (t12(1:5).eq.'VENT') then
          ibctyp=10
        else
          goto 51
        endif        

      elseif (t12(1:6).eq.'WINDOW' .or. t12(3:8).eq.'WINDOW') then
        t12=SUSE(ICP,isur,2)
        if (t12(1:6).eq.'CLOSED') then
          ibctyp=1
        elseif (t12(1:5).eq.'CRACK') then
          ibctyp=4
        elseif (t12(1:4).eq.'OPEN') then
          CALL ZSURLEHI(ICP,ISUR,XYDIS,ZDIS,i1,i2,i3,i4,DZLLFF)
          if (XYDIS/ZDIS.gt.1.0) then
            ibctyp=6
          else
            ibctyp=5
          endif
        elseif (t12(1:4).eq.'SASH') then
          ibctyp=7
        elseif (t12(1:5).eq.'BIDIR') then
          ibctyp=2
        else
          goto 51
        endif

      elseif (t12(1:5).eq.'GRILL') then
        t12=SUSE(ICP,isur,2)
        if (t12(1:6).eq.'CLOSED') then
          ibctyp=1
        elseif (t12(1:5).eq.'CRACK') then
          ibctyp=4
        elseif (t12(1:5).eq.'INLET') then
          ibctyp=2
        elseif (t12(1:7).eq.'EXTRACT') then
          ibctyp=2
        elseif (t12(1:4).eq.'OPEN') then
          ibctyp=2
        else
          goto 51
        endif

      elseif (t12(1:4).eq.'ROOF') then
        ibctyp=1

      elseif (t12(1:4).eq.'FICT') then
        t12=SUSE(ICP,isur,2)
        if (t12(1:6).eq.'CLOSED') then
          ibctyp=1
        elseif (t12(1:5).eq.'CRACK') then
          ibctyp=3
        elseif (t12(1:4).eq.'OPEN') then
          ibctyp=2
        elseif (t12(1:5).eq.'BIDIR') then
          ibctyp=8
        else
          goto 51
        endif

      elseif (t12(1:5).eq.'FLOOR') then
        ibctyp=1

      elseif (t12(1:5).eq.'FURNI') then
        ibctyp=1

      elseif (t12(1:5).eq.'PARTN') then
        ibctyp=1

      elseif (t12(1:5).eq.'STRUC') then
C << Extend to scan for these not on boundaries, and create
C blockages/sources >>
        ibctyp=1

      elseif (t12(1:7).eq.'ITEQUIP') then
C << Extend to scan for these not on boundaries, and create
C blockages/sources >>
        ibctyp=1

      elseif (t12(1:7).eq.'FIXTURE') then
C << Extend to scan for these not on boundaries, and create
C blockages/sources >>
        ibctyp=1

      elseif (t12(1:6).eq.'PLANTS') then
C << Extend to scan for these not on boundaries, and create
C blockages/sources >>
        ibctyp=1

      elseif (t12(1:1).eq.'-') then
C Assume that a surface with no usage tag is a solid surface.
        ibctyp=1
        write(outs,'(2a)')' This surface has no usage tag, assuming a',
     &    ' solid surface.'
        CALL EDISP(IUOUT,outs)
      else
        goto 51
      endif

C If usage tag is not recognised, notify user and skip to the next
C surface.
      goto 52
   51 write(outs,'(3a)')' The usage tag ',t12(1:lnblnk(t12)),
     &  ' is not recognised, skipping this surface.'
      CALL EDISP(IUOUT,outs)
      ISTAT=5
      goto 999

C Find grid cells to best match the extents of the surface. Note that
C this will find indices on the velocity grid. This is negatively offset
C from the scalar grid, so for example XU(1) is the negative U edge of
C the scalar cell XP(1), and the positive U edge is XU(2).
C So the actual (scalar) grid cells to use are is?min and is?max-1.
   52 CALL FDCFDPT(2,sumin,svmin,swmin,isumin,isvmin,iswmin,IER)
      CALL FDCFDPT(2,sumax,svmax,swmax,isumax,isvmax,iswmax,IER)
      if (IER.ne.0) then
        write(outs,'(a)')
     &    ' Error finding cells to match geometry. Check gridding.'
        CALL EDISP(IUOUT,outs)
        ISTAT=1
        goto 999
      endif
      if (ifac.eq.1 .or. ifac.eq.2) then
        if (ifac.eq.1) then
          isumax=isumin+1
        else
          isumin=isumax-1
        endif
        if (isvmin.eq.isvmax) then
          write(outs,'(2a)')' This surface is too narrow in the Y',
     &      ' direction, no BC will be created.'
          CALL EDISP(IUOUT,outs)
          ISTAT=4
          goto 999
        elseif (iswmin.eq.iswmax) then
          write(outs,'(2a)')' This surface is too narrow in the Z',
     &      ' direction, no BC will be created.'
          CALL EDISP(IUOUT,outs)
          ISTAT=4
          goto 999
        endif
        iparext(1:4)=(/isvmin,isvmax,iswmin,iswmax/)
        iparca=(isvmax-isvmin)*(iswmax-iswmin)
      elseif (ifac.eq.3 .or. ifac.eq.4) then
        if (ifac.eq.3) then
          isvmax=isvmin+1
        else
          isvmin=isvmax-1
        endif
        if (isumin.eq.isumax) then
          write(outs,'(2a)')' This surface is too narrow in the X',
     &      ' direction, no BC will be created.'
          CALL EDISP(IUOUT,outs)
          ISTAT=4
          goto 999
        elseif (iswmin.eq.iswmax) then
          write(outs,'(2a)')' This surface is too narrow in the Z',
     &      ' direction, no BC will be created.'
          CALL EDISP(IUOUT,outs)
          ISTAT=4
          goto 999
        endif
        iparext(1:4)=(/isumin,isumax,iswmin,iswmax/)
        iparca=(isumax-isumin)*(iswmax-iswmin)
      elseif (ifac.eq.5 .or. ifac.eq.6) then
        if (ifac.eq.5) then
          iswmax=iswmin+1
        else
          iswmin=iswmax-1
        endif
        if (isumin.eq.isumax) then
          write(outs,'(2a)')' This surface is too narrow in the X',
     &      ' direction, no BC will be created.'
          CALL EDISP(IUOUT,outs)
          ISTAT=4
          goto 999
        elseif (isvmin.eq.isvmax) then
          write(outs,'(2a)')' This surface is too narrow in the Y',
     &      ' direction, no BC will be created.'
          CALL EDISP(IUOUT,outs)
          ISTAT=4
          goto 999
        endif
        iparext(1:4)=(/isumin,isumax,isvmin,isvmax/)
        iparca=(isumax-isumin)*(isvmax-isvmin)
      endif

C Now, check for children.
      if (NBCHILD(icnn).gt.0) then
        write(outs,'(a)')' Child surfaces detected.'
        CALL EDISP(IUOUT,outs)
        icldtotca=0; icldverts=0; ncldverts=0
        dochildren=.false.
        do icld=1,NBCHILD(icnn)
          icldcnn=ICHILD(icnn,icld)  ! establish child connection
          icldzn=IC1(icldcnn)        ! and zone
          icldsur=IE1(icldcnn)       ! and surface
          t12=SNAME(icldzn,icldsur)
          write(outs,'(a,i3.3,1x,2a)')' Processing child surface ',
     &      icldsur,t12(1:lnblnk(t12)),' ...'
          CALL EDISP(IUOUT,outs)

C Call this subroutine for the child.
          CALL GENEDGBC(icldsur,umax,vmax,wmax,ifac,iibctyp,iisumin,
     &      iisumax,iisvmin,iisvmax,iiswmin,iiswmax,doafnasc,ISTAT)

C Check for problems with the child.
          if (ISTAT.eq.1) then
            write(outs,'(a)')
     &        ' Error with child, parent will also be skipped.'
            CALL EDISP(IUOUT,outs)
            goto 999
          elseif (ISTAT.eq.2) then
            write(outs,'(a)')
     &        ' Child is not rectangular, parent will also be skipped.'
            CALL EDISP(IUOUT,outs)
            goto 999
          elseif (ISTAT.ge.3 .and. ISTAT.le.5) then ! can't do child, but can still do parent
            write(outs,'(a)')
     &        ' Child will be skipped, but processing will continue.'
            CALL EDISP(IUOUT,outs)
            dochild(icld)=.false.
          else
            write(outs,'(a)')
     &        ' No problems with child, processing will continue.'
            CALL EDISP(IUOUT,outs)
            dochild(icld)=.true.
            dochildren=.true.
          endif
          icldbctyp(icld)=iibctyp
          
C Assemble an array of vertices in child surfaces, for checking
C rectangularity.
          do i=1,isznver(icldzn,icldsur)
            ivrt=iszjvn(icldzn,icldsur,i)           
            ncldverts=ncldverts+1
            icldverts(ncldverts)=ivrt
          enddo

          if (dochild(icld)) then
C Store the extents of the child (in cells), and the total cells covered
C by all children.
            if (ifac.eq.1) then
              iisumax=iisumin+1
              icldext(icld,1:4)=(/iisvmin,iisvmax,iiswmin,iiswmax/)
              icldtotca=icldtotca+(iisvmax-iisvmin)*(iiswmax-iiswmin)
            elseif (ifac.eq.2) then
              iisumax=iisumin+1
              icldext(icld,1:4)=(/iisvmin,iisvmax,iiswmin,iiswmax/)
              icldtotca=icldtotca+(iisvmax-iisvmin)*(iiswmax-iiswmin)
            elseif (ifac.eq.3) then
              iisvmax=iisvmin+1
              icldext(icld,1:4)=(/iisumin,iisumax,iiswmin,iiswmax/)
              icldtotca=icldtotca+(iisumax-iisumin)*(iiswmax-iiswmin)
            elseif (ifac.eq.4) then
              iisvmax=iisvmin+1
              icldext(icld,1:4)=(/iisumin,iisumax,iiswmin,iiswmax/)
              icldtotca=icldtotca+(iisumax-iisumin)*(iiswmax-iiswmin)
            elseif (ifac.eq.5) then
              iiswmax=iiswmin+1
              icldext(icld,1:4)=(/iisumin,iisumax,iisvmin,iisvmax/)
              icldtotca=icldtotca+(iisumax-iisumin)*(iisvmax-iisvmin)
            elseif (ifac.eq.6) then
              iiswmax=iiswmin+1
              icldext(icld,1:4)=(/iisumin,iisumax,iisvmin,iisvmax/)
              icldtotca=icldtotca+(iisumax-iisumin)*(iisvmax-iisvmin)
            endif
          endif
        enddo

C We now have the extents of all children, and have confirmed that they
C are all rectangular. We now need to ensure that the parent is also
C rectangular. To do this, use a procedure similar to childless surfaces
C with more than 4 vertices, with the exception that any vertex that is
C in one of the children must be ommited from the check.
        if (.not.isrect) then
          icorners=0
          do i=1,isznver(ICP,isur)   ! vertices in parent surface
            ivrt=iszjvn(ICP,isur,i)
            ok=.false.
            do j=1,ncldverts
              if (ivrt.eq.icldverts(j)) then
                ok=.true.
                EXIT
              endif
            enddo
            if (ok) CYCLE
            CALL CFDTRANS(2,szcoords(ICP,ivrt,1),szcoords(ICP,ivrt,2),
     &        szcoords(ICP,ivrt,3),u,v,w,IER)
            if (ifac.eq.1 .or. ifac.eq.2) then
              call ECLOSE(v,svmin,0.01,ok)
              if (.not.ok) call ECLOSE(v,svmax,0.01,ok)
              call ECLOSE(w,swmin,0.01,dok)
              if (.not.dok) call ECLOSE(w,swmax,0.01,dok)
              if (ok.and.dok) icorners=icorners+1
              if (ok.or.dok) CYCLE
            elseif (ifac.eq.3 .or. ifac.eq.4) then
              call ECLOSE(u,sumin,0.01,ok)
              if (.not.ok) call ECLOSE(u,sumax,0.01,ok)
              call ECLOSE(w,swmin,0.01,dok)
              if (.not.dok) call ECLOSE(w,swmax,0.01,dok)
              if (ok.and.dok) icorners=icorners+1
              if (ok.or.dok) CYCLE
            elseif (ifac.eq.5 .or. ifac.eq.6) then
              call ECLOSE(u,sumin,0.01,ok)
              if (.not.ok) call ECLOSE(u,sumax,0.01,ok)
              call ECLOSE(v,svmin,0.01,dok)
              if (.not.dok) call ECLOSE(v,svmax,0.01,dok)
              if (ok.and.dok) icorners=icorners+1
              if (ok.or.dok) CYCLE
            endif
            ISTAT=2
            EXIT
          enddo
          if (ISTAT.eq.2) then
            CALL EDISP(IUOUT,
     &        ' Parent surface is not rectangular, skipping.')
            goto 999
          endif
          if (icorners.lt.4) then
            ISTAT=2
            CALL EDISP(IUOUT,
     &        ' Parent surface is not aligned with CFD axes, skipping.')
            goto 999
          endif
          isrect=.true.
        endif

C If all children are ignored then treat this surface like a
C non-parent; skip the following code.
        if (.not.dochildren) goto 99

C Now, we try to establish if the parent is worth representing on its
C own, or should be lumped in with the child(ren).

C Rules are (within the discretized CFD grid):
C 1: If the whole extent of the parent is covered by its child(ren),
C    then the parent will not be represented with a solid BC.
C 2: Otherwise, provided there is a single child, if any edge of the
C    child is more than 1 cell away from its neighbouring parent edge,
C    the parent will be represented with a solid BC.
C 3: Otherwise, highlight the surface and ask the user.
        if (icldtotca.ge.iparca) then ! all cells of parent taken up by children
          write(outs,'(2a)')
     &  ' Parent framing is narrow; no solid BC will be created for it.'
          CALL EDISP(IUOUT,outs)
          lumped=.true.
        else          
          if (NBCHILD(icnn).eq.1) then

C Check proximity of child edges to parent edges.
C isclose(side): 
C 0 - not close
C 1 - coincident
C 2 - within 1 cell width
            if (icldext(1,1).eq.iparext(1)) then
              isclose(1)=1
            elseif (icldext(1,1)-1.eq.iparext(1)) then
              isclose(1)=2
            else
              isclose(1)=0
            endif
            if (icldext(1,2).eq.iparext(2)) then
              isclose(2)=1
            elseif (icldext(1,2)+1.eq.iparext(2)) then
              isclose(2)=2
            else
              isclose(2)=0
            endif
            if (icldext(1,3).eq.iparext(3)) then
              isclose(3)=1
            elseif (icldext(1,3)-1.eq.iparext(3)) then
              isclose(3)=2
            else
              isclose(3)=0
            endif
            if (icldext(1,4).eq.iparext(4)) then
              isclose(4)=1
            elseif (icldext(1,4)+1.eq.iparext(4)) then
              isclose(4)=2
            else
              isclose(4)=0
            endif

C If all edges of the child are close to the edges of the present
C surface, ask user if they want to lump the present surface in with its
C child. Do not ask if ...
            if (isclose(1).gt.0 .and. isclose(2).gt.0 .and.
     &          isclose(3).gt.0 .and. isclose(4).gt.0) then

C ... the present surface is a frame and we need space for an opening
C BC, or if the child is also a parent (in which case the BCs would have
C been created in the invocation of GENEDGBC for that surface).
              if ((ibctyp.eq.9 .or. ibctyp.eq.10) .or.
     &            NBCHILD(icldcnn).gt.0) then
                lumped=.false.
                write(outs,'(3a)')' Parent framing is narrow; BCs ',
     &            'created for it may not well represent surface ',
     &            'position and area.'
                call EDISP(IUOUT,outs)
                write(outs,'(a)')
     &            ' Please check after autogeneration is complete.'
                call EDISP(IUOUT,outs)
                goto 70
              endif

              ISHSB=0; ISHAO=0; ISHBLK=0; ISHSRC=0; ISHGEO=-ISUR
              call redraw(IER)
              IR=2
              helptopic='cfdbc_parentframing'
              call gethelptext(helpinsub,helptopic,nbhelp)
   61         CALL EASKABC(' ',
     &         'Represent parent surface with solid BCs?',
     &         'yes','no','preview',IR,nbhelp)
              ok=.false.
              if (IR.eq.1) then
                lumped=.false.
              elseif (IR.eq.2) then
                lumped=.true.

C Extend the extents of the child to those of the parent.
                icldext(1,1)=iparext(1)
                icldext(1,2)=iparext(2)
                icldext(1,3)=iparext(3)
                icldext(1,4)=iparext(4)
              elseif (IR.eq.3) then ! preview

C First, show the parent surface highlighted.
                ISHSB=0; ISHAO=0; ISHBLK=0; ISHSRC=0; ISHGEO=-isur
                CALL redraw(IER)
                CALL continuebox('Parent surface.',' ','ok')

C Create the boundary conditions that would be created for the parent.
                icreated=0
                if (isclose(1).eq.2) then
                  if (ifac.eq.1 .or. ifac.eq.2) then
                    i1=isumin; i2=isumax-1
                    j1=isvmin; j2=isvmin
                    k1=iswmin; k2=iswmax-1
                  else
                    i1=isumin; i2=isumin
                    j1=isvmin; j2=isvmax-1
                    k1=iswmin; k2=iswmax-1
                  endif
                  write (t12,'(2a,i3.3,a,i3.3)')
     &              facstr,'sld',NSB(ICFD)+1,'_s',isur
                  call MKCFDBC(.false.,4,ifac,i1,i2,j1,j2,k1,k2,t12,
     &              SNAME(ICP,isur),dum,1,20.0,0.0,0.0,0.0,0.0,IER)
                  icreated=icreated+1

                endif
                if (isclose(2).eq.2) then
                  if (ifac.eq.1 .or. ifac.eq.2) then
                    i1=isumin;   i2=isumax-1
                    j1=isvmax-1; j2=isvmax-1
                    k1=iswmin;   k2=iswmax-1
                  else
                    i1=isumax-1; i2=isumax-1
                    j1=isvmin;   j2=isvmax-1
                    k1=iswmin;   k2=iswmax-1
                  endif
                  write (t12,'(2a,i3.3,a,i3.3)')
     &              facstr,'sld',NSB(ICFD)+1,'_s',isur
                  call MKCFDBC(.false.,4,ifac,i1,i2,j1,j2,k1,k2,t12,
     &              SNAME(ICP,isur),dum,1,20.0,0.0,0.0,0.0,0.0,IER)
                  icreated=icreated+1
                endif
                if (isclose(3).eq.2) then
                  if (ifac.eq.5 .or. ifac.eq.6) then
                    if (isclose(1).eq.2) then                      
                      i1=isumin+1
                    else
                      i1=isumin
                    endif
                    if (isclose(2).eq.2) then
                      i2=isumax-2
                    else
                      i2=isumax-1
                    endif
                    j1=isvmin; j2=isvmin
                    k1=iswmin; k2=iswmax-1
                  else
                    if (isclose(1).eq.2) then                      
                      i1=isumin+1
                    else
                      i1=isumin
                    endif
                    if (isclose(2).eq.2) then
                      i2=isumax-2
                    else
                      i2=isumax-1
                    endif
                    j1=isvmin; j2=isvmax-1
                    k1=iswmin; k2=iswmin
                  endif
                  write (t12,'(2a,i3.3,a,i3.3)')
     &              facstr,'sld',NSB(ICFD)+1,'_s',isur
                  call MKCFDBC(.false.,4,ifac,i1,i2,j1,j2,k1,k2,t12,
     &              SNAME(ICP,isur),dum,1,20.0,0.0,0.0,0.0,0.0,IER)
                  icreated=icreated+1
                endif
                if (isclose(4).eq.2) then
                  if (ifac.eq.5 .or. ifac.eq.6) then
                    if (isclose(1).eq.2) then                      
                      i1=isumin+1
                    else
                      i1=isumin
                    endif
                    if (isclose(2).eq.2) then
                      i2=isumax-2
                    else
                      i2=isumax-1
                    endif
                    j1=isvmax-1; j2=isvmax-1
                    k1=iswmin;   k2=iswmax-1
                  else
                    if (isclose(1).eq.2) then                      
                      i1=isumin+1
                    else
                      i1=isumin
                    endif
                    if (isclose(2).eq.2) then
                      i2=isumax-2
                    else
                      i2=isumax-1
                    endif
                    j1=isvmin;   j2=isvmax-1
                    k1=iswmax-1; k2=iswmax-1
                  endif
                  write (t12,'(2a,i3.3,a,i3.3)')
     &              facstr,'sld',NSB(ICFD)+1,'_s',ISUR
                  call MKCFDBC(.false.,4,ifac,i1,i2,j1,j2,k1,k2,t12,
     &              SNAME(ICP,isur),dum,1,20.0,0.0,0.0,0.0,0.0,IER)
                  icreated=icreated+1
                endif

                if (icreated.eq.1) then
                  ISHSB=NSB(ICFD); ISHAO=0; ISHBLK=0; ISHSRC=0
                  ISHGEO=-9999
                  call redraw(IER)
                  call continuebox(
     &              'Preview of parent surface boundary condition.',
     &              ' ','ok')
                else

C Show the new boundary conditions highlighted, one by one, with a half
C second delay.
   62             do i=1,icreated
                    ISHSB=NSB(ICFD)-i+1; ISHAO=0; ISHBLK=0; ISHSRC=0
                    ISHGEO=-9999
                    call redraw(IER)
                    if (i.lt.icreated) call pausems(500)
                  enddo
                  IR=1
                  call EASKAB(
     &              'Preview of parent surface boundary conditions.',
     &              ' ','ok','show again',IR,nbhelp)
                  if (IR.eq.2) goto 62
                endif

C Delete BC(s) created for preview.
                do i=1,icreated
                  call DELCFDBC(NVOL(ICFD))
                enddo

C Show child surface.
                ISHSB=0; ISHAO=0; ISHBLK=0; ISHSRC=0; ISHGEO=-icldsur
                CALL redraw(IER)
                CALL continuebox('Child surface.',' ','ok')

C Preview child BC with parent BC(s).
   63           if (ifac.eq.1 .or. ifac.eq.2) then
                  i1=isumin; i2=isumax-1
                  if (isclose(1).eq.2) then
                    j1=iparext(1)+1
                  else
                    j1=iparext(1)
                  endif
                  if (isclose(2).eq.2) then
                    j2=iparext(2)-2
                  else
                    j2=iparext(2)-1
                  endif
                  if (isclose(3).eq.2) then
                    k1=iparext(3)+1
                  else
                    k1=iparext(3)
                  endif
                  if (isclose(4).eq.2) then
                    k2=iparext(4)-2
                  else
                    k2=iparext(4)-1
                  endif
                elseif (ifac.eq.3 .or. ifac.eq.4) then
                  if (isclose(1).eq.2) then
                    i1=iparext(1)+1
                  else
                    i1=iparext(1)
                  endif
                  if (isclose(2).eq.2) then
                    i2=iparext(2)-2
                  else
                    i2=iparext(2)-1
                  endif
                  j1=isvmin; j2=isvmax-1
                  if (isclose(3).eq.2) then
                    k1=iparext(3)+1
                  else
                    k1=iparext(3)
                  endif
                  if (isclose(4).eq.2) then
                    k2=iparext(4)-2
                  else
                    k2=iparext(4)-1
                  endif
                elseif (ifac.eq.5 .or. ifac.eq.6) then
                  if (isclose(1).eq.2) then
                    i1=iparext(1)+1
                  else
                    i1=iparext(1)
                  endif
                  if (isclose(2).eq.2) then
                    i2=iparext(2)-2
                  else
                    i2=iparext(2)-1
                  endif
                  if (isclose(3).eq.2) then
                    j1=iparext(3)+1
                  else
                    j1=iparext(3)
                  endif
                  if (isclose(4).eq.2) then
                    j2=iparext(4)-2
                  else
                    j2=iparext(4)-1
                  endif
                  k1=iswmin; k2=iswmax-1
                endif
                write (t12,'(2a,i3.3,a,i3.3)')
     &            facstr,'sld',NSB(ICFD)+1,'_s',icldsur
                call MKCFDBC(.false.,4,ifac,i1,i2,j1,j2,k1,k2,t12,
     &            SNAME(IC1(icldcnn),IE1(icldcnn)),dum,1,
     &            20.0,0.0,0.0,0.0,0.0,IER)
                ISHSB=NSB(ICFD); ISHAO=0; ISHBLK=0; ISHGEO=-9999
                call redraw(IER)
                call continuebox(
     &            'Preview of child surface BC with parent BC(s).',
     &            ' ','ok')

C Delete preview BC.
                call DELCFDBC(NVOL(ICFD))

C Preview child BC without parent BC(s).
                if (ifac.eq.1 .or. ifac.eq.2) then
                  i1=isumin;     i2=isumax-1
                  j1=iparext(1); j2=iparext(2)-1
                  k1=iparext(3); k2=iparext(4)-1
                elseif (ifac.eq.3 .or. ifac.eq.4) then
                  i1=iparext(1); i2=iparext(2)-1
                  j1=isvmin;     j2=isvmax-1
                  k1=iparext(3); k2=iparext(4)-1
                elseif (ifac.eq.5 .or. ifac.eq.6) then
                  i1=iparext(1); i2=iparext(2)-1
                  j1=iparext(3); j2=iparext(4)-1
                  k1=iswmin;     k2=iswmax-1
                endif
                write (t12,'(2a,i3.3,a,i3.3)')
     &            facstr,'sld',NSB(ICFD)+1,'_s',icldsur
                call MKCFDBC(.false.,4,ifac,i1,i2,j1,j2,k1,k2,t12,
     &            SNAME(IC1(icldcnn),IE1(icldcnn)),dum,1,
     &            20.0,0.0,0.0,0.0,0.0,IER)
                ISHSB=NSB(ICFD); ISHAO=0; ISHBLK=0; ISHGEO=-9999
                call redraw(IER)
                call EASKAB(
     &            'Preview of child surface BC without parent BC(s).',
     &              ' ','ok','show again',IR,nbhelp)

C Delete preview BC.
                call DELCFDBC(NVOL(ICFD))

                if (IR.eq.2) goto 63

                ok=.true.
              else
                ok=.true.
              endif
              if (ok) goto 61
   70         CONTINUE
            else
C Otherwise, create solid BC(s) for parent.
              lumped=.false.
            endif

C If there is more than one child, we don't handle it.
          else
            write(outs,'(2a)')
     &        ' Parent has more than one child and is not narrow,',
     &        ' skipping parent and children.'
            call EDISP(IUOUT,outs)
            ISTAT=3
            goto 999
          endif
        endif

C ibctyp = 1:  solid BC(s) for the whole surface
C ibctyp = 2:  single opening BC for the whole surface
C ibctyp = 3:  opening BC for bottom row of cells, solid BC for the rest
C ibctyp = 4:  opening BC for middle row of cells, solid BCs for the rest
C ibctyp = 5:  opening BC for 25% of height, at bottom edge, solid BC for
C              the rest
C ibctyp = 6:  opening BC for 25% of width, at vertical centre line,
C              solid BCs for rest
C ibctyp = 7:  opening BCs for 12.5% of height, at top and bottom edges,
C              solid BC for rest
C ibctyp = 8:  opening BC for 75% of height and width, at bottom edge,
C              solid BCs for rest
C ibctyp = 9:  for frames; opening BC as close to middle row of parent
C              and child as we can get
C ibctyp = 10: for frames; opening BC for 20% of child width, as close
C              to top edge as we can get

Create child(ren) BCs.
        do icld=1,NBCHILD(icnn)
          if (.not.dochild(icld)) CYCLE
          icldcnn=ICHILD(icnn,icld)  ! establish child connection
          icldzn=IC1(icldcnn)        ! and zone
          icldsur=IE1(icldcnn)       ! and surface
          iibctyp=icldbctyp(icld)

C If this child is also a parent, it will have already been created in
C its own invocation of this function; do not create BCs for it.
          if (NBCHILD(icldcnn).gt.0) CYCLE

          write(outs,'(a,i3.3,2a)')' Creating BC(s) for surface ',
     &    icldsur,
     &    SNAME(icldzn,icldsur)(1:lnblnk(SNAME(icldzn,icldsur))),' ...'
          ISHSB=0; ISHAO=0; ISHBLK=0; IFACES=1; ISHGEO=-icldsur
          call redraw(IER)
          call pausems(500)
          ISHGEO=-9999

C Boundary condition geometry generation.
C Boundary conditions are created by calling subroutine MKCFDBC.
C Arguments 4 - 9 of this call are the cell indices of the extents of
C the new BC; min (1) and max (2) in the i, j, and k directions.
C Generally speaking, these extents are generated based on the extents
C of the surface (and possibly any parents/children), and a rule set
C denoted by ibctyp (iibctyp for a child surface). The stucture of this
C code is broadly:
C 1 - compute any distances and points required (e.g. midpoints) in
C     terms of cell indices
C 2 - assign new surface extents depending on which face the new surface
C     is on, denoted by ifac
C 3 - generate a name for the new BC, with the structure:
C     FtypXXX_sYYY
C     where F   = 1 character identifying face; W, E, S, N, L, H
C           typ = 3 characters identifying BC type; sld, opn, blk, src
C           XXX = 0 padded integer id for the new BC
C           YYY = 0 padded integer id of the associated building surface
C 4 - call MKCFDBC to generate the new BC.

C Single solid BC for the whole surface.
          if (iibctyp.eq.1) then
            if (ifac.eq.1 .or. ifac.eq.2) then
              i1=isumin;          i2=isumax-1
              j1=icldext(icld,1); j2=icldext(icld,2)-1
              k1=icldext(icld,3); k2=icldext(icld,4)-1
            elseif (ifac.eq.3 .or. ifac.eq.4) then
              i1=icldext(icld,1); i2=icldext(icld,2)-1
              j1=isvmin;          j2=isvmax-1
              k1=icldext(icld,3); k2=icldext(icld,4)-1
            elseif (ifac.eq.5 .or. ifac.eq.6) then
              i1=icldext(icld,1); i2=icldext(icld,2)-1
              j1=icldext(icld,3); j2=icldext(icld,4)-1
              k1=iswmin;          k2=iswmax-1
            endif
            write (t12,'(2a,i3.3,a,i3.3)')
     &        facstr,'sld',NSB(ICFD)+1,'_s',icldsur
            call MKCFDBC(show,4,ifac,i1,i2,j1,j2,k1,k2,t12,
     &        SNAME(icldzn,icldsur),dum,1,
     &        20.0,0.0,0.0,0.0,0.0,IER)

C Single opening BC for the whole surface.
          elseif (iibctyp.eq.2) then
            if (ifac.eq.1 .or. ifac.eq.2) then
              i1=isumin;          i2=isumax-1
              j1=icldext(icld,1); j2=icldext(icld,2)-1
              k1=icldext(icld,3); k2=icldext(icld,4)-1
            elseif (ifac.eq.3 .or. ifac.eq.4) then
              i1=icldext(icld,1); i2=icldext(icld,2)-1
              j1=isvmin;          j2=isvmax-1
              k1=icldext(icld,3); k2=icldext(icld,4)-1
            elseif (ifac.eq.5 .or. ifac.eq.6) then
              i1=icldext(icld,1); i2=icldext(icld,2)-1
              j1=icldext(icld,3); j2=icldext(icld,4)-1
              k1=iswmin;          k2=iswmax-1
            endif
            write (t12,'(2a,i3.3,a,i3.3)')
     &        facstr,'opn',NOPEN(ICFD)+1,'_s',icldsur
            call MKCFDBC(show,11,ifac,i1,i2,j1,j2,k1,k2,t12,
     &        SNAME(icldzn,icldsur),dum,1,
     &        20.0,0.0,0.01,0.0,0.0,IER)

C Opening BC for bottom row of cells, solid BC for the rest.
          elseif (iibctyp.eq.3) then
            if (ifac.eq.1 .or. ifac.eq.2) then
              i1=isumin;          i2=isumax-1
              j1=icldext(icld,1); j2=icldext(icld,2)-1
              k1=icldext(icld,3); k2=icldext(icld,3)
            elseif (ifac.eq.3 .or. ifac.eq.4) then
              i1=icldext(icld,1); i2=icldext(icld,2)-1
              j1=isvmin;          j2=isvmax-1
              k1=icldext(icld,3); k2=icldext(icld,3)
            elseif (ifac.eq.5 .or. ifac.eq.6) then
              i1=icldext(icld,1); i2=icldext(icld,2)-1
              j1=icldext(icld,3); j2=icldext(icld,3)
              k1=iswmin;          k2=iswmax-1
            endif
            write (t12,'(2a,i3.3,a,i3.3)')
     &        facstr,'opn',NOPEN(ICFD)+1,'_s',icldsur
            call MKCFDBC(show,11,ifac,i1,i2,j1,j2,k1,k2,t12,
     &        SNAME(icldzn,icldsur),dum,1,
     &        20.0,0.0,0.01,0.0,0.0,IER)

C Is there any more of the surface for a solid BC?
            if (icldext(icld,4)-1.gt.icldext(icld,3)) then
              if (ifac.eq.1 .or. ifac.eq.2) then
                i1=isumin;            i2=isumax-1
                j1=icldext(icld,1);   j2=icldext(icld,2)-1
                k1=icldext(icld,3)+1; k2=icldext(icld,4)-1
              elseif (ifac.eq.3 .or. ifac.eq.4) then
                i1=icldext(icld,1);   i2=icldext(icld,2)-1
                j1=isvmin;            j2=isvmax-1
                k1=icldext(icld,3)+1; k2=icldext(icld,4)-1
              elseif (ifac.eq.5 .or. ifac.eq.6) then
                i1=icldext(icld,1);   i2=icldext(icld,2)-1
                j1=icldext(icld,3)+1; j2=icldext(icld,4)-1
                k1=iswmin;            k2=iswmax-1
              endif
              write (t12,'(2a,i3.3,a,i3.3)')
     &          facstr,'sld',NSB(ICFD)+1,'_s',icldsur
              call MKCFDBC(show,4,ifac,i1,i2,j1,j2,k1,k2,t12,
     &          SNAME(icldzn,icldsur),dum,1,
     &          20.0,0.0,0.0,0.0,0.0,IER)

            endif

C Opening BC for middle row of cells, solid BCs for the rest.
          elseif (iibctyp.eq.4) then

C Find middle row.
            irows=icldext(icld,4)-icldext(icld,3)
            imidrow=icldext(icld,3)+irows/2

            if (ifac.eq.1 .or. ifac.eq.2) then
              i1=isumin;          i2=isumax-1
              j1=icldext(icld,1); j2=icldext(icld,2)-1
              k1=imidrow;         k2=imidrow
            elseif (ifac.eq.3 .or. ifac.eq.4) then
              i1=icldext(icld,1); i2=icldext(icld,2)-1
              j1=isvmin;          j2=isvmax-1
              k1=imidrow;         k2=imidrow
            elseif (ifac.eq.5 .or. ifac.eq.6) then
              i1=icldext(icld,1); i2=icldext(icld,2)-1
              j1=imidrow;         j2=imidrow
              k1=iswmin;          k2=iswmax-1
            endif
            write (t12,'(2a,i3.3,a,i3.3)')
     &        facstr,'opn',NOPEN(ICFD)+1,'_s',icldsur
            call MKCFDBC(show,11,ifac,i1,i2,j1,j2,k1,k2,t12,
     &        SNAME(icldzn,icldsur),dum,1,
     &        20.0,0.0,0.01,0.0,0.0,IER)

C Is there any more of the surface below for a solid BC?
            if (icldext(icld,3).lt.imidrow) then
              if (ifac.eq.1 .or. ifac.eq.2) then
                i1=isumin;          i2=isumax-1
                j1=icldext(icld,1); j2=icldext(icld,2)-1
                k1=icldext(icld,3); k2=imidrow-1
              elseif (ifac.eq.3 .or. ifac.eq.4) then
                i1=icldext(icld,1); i2=icldext(icld,2)-1
                j1=isvmin;          j2=isvmax-1
                k1=icldext(icld,3); k2=imidrow-1
              elseif (ifac.eq.5 .or. ifac.eq.6) then
                i1=icldext(icld,1); i2=icldext(icld,2)-1
                j1=icldext(icld,3); j2=imidrow-1
                k1=iswmin;          k2=iswmax-1
              endif
              write (t12,'(2a,i3.3,a,i3.3)')
     &          facstr,'sld',NSB(ICFD)+1,'_s',icldsur
              call MKCFDBC(show,4,ifac,i1,i2,j1,j2,k1,k2,t12,
     &          SNAME(icldzn,icldsur),dum,1,
     &          20.0,0.0,0.0,0.0,0.0,IER)
            endif

C Is there any more of the surface above for a solid BC?
            if ((icldext(icld,4)-1).gt.imidrow) then
              if (ifac.eq.1 .or. ifac.eq.2) then
                i1=isumin;          i2=isumax-1
                j1=icldext(icld,1); j2=icldext(icld,2)-1
                k1=imidrow+1;       k2=icldext(icld,4)-1
              elseif (ifac.eq.3 .or. ifac.eq.4) then
                i1=icldext(icld,1); i2=icldext(icld,2)-1
                j1=isvmin;          j2=isvmax-1
                k1=imidrow+1;       k2=icldext(icld,4)-1
              elseif (ifac.eq.5 .or. ifac.eq.6) then
                i1=icldext(icld,1); i2=icldext(icld,2)-1
                j1=imidrow+1;       j2=icldext(icld,4)-1
                k1=iswmin;          k2=iswmax-1
              endif
              write (t12,'(2a,i3.3,a,i3.3)')
     &          facstr,'sld',NSB(ICFD)+1,'_s',icldsur
              call MKCFDBC(show,4,ifac,i1,i2,j1,j2,k1,k2,t12,
     &          SNAME(icldzn,icldsur),dum,1,
     &          20.0,0.0,0.0,0.0,0.0,IER)
            endif

C Opening BC for 25% of height, at bottom edge, solid BC for the rest.
          elseif (iibctyp.eq.5) then

C Find 25% of height, rounded, at least 1 row.
            rrows=real(icldext(icld,4)-icldext(icld,3))
            iqtr=nint(rrows/4.0)
            if (iqtr.eq.0) iqtr=1

            if (ifac.eq.1 .or. ifac.eq.2) then
              i1=isumin;          i2=isumax-1
              j1=icldext(icld,1); j2=icldext(icld,2)-1
              k1=icldext(icld,3); k2=icldext(icld,3)+iqtr-1
            elseif (ifac.eq.3 .or. ifac.eq.4) then
              i1=icldext(icld,1); i2=icldext(icld,2)-1
              j1=isvmin;          j2=isvmax-1
              k1=icldext(icld,3); k2=icldext(icld,3)+iqtr-1
            elseif (ifac.eq.5 .or. ifac.eq.6) then
              i1=icldext(icld,1); i2=icldext(icld,2)-1
              j1=icldext(icld,3); j2=icldext(icld,3)+iqtr-1
              k1=iswmin;          k2=iswmax-1
            endif
            write (t12,'(2a,i3.3,a,i3.3)')
     &        facstr,'opn',NOPEN(ICFD)+1,'_s',icldsur
            call MKCFDBC(show,11,ifac,i1,i2,j1,j2,k1,k2,t12,
     &        SNAME(icldzn,icldsur),dum,1,
     &        20.0,0.0,0.01,0.0,0.0,IER)

C Is there any more of the surface for a solid BC?
            if ((icldext(icld,4)-1).gt.(icldext(icld,3)+iqtr)) then
              if (ifac.eq.1 .or. ifac.eq.2) then
                i1=isumin;               i2=isumax-1
                j1=icldext(icld,1);      j2=icldext(icld,2)-1
                k1=icldext(icld,3)+iqtr; k2=icldext(icld,4)-1
              elseif (ifac.eq.3 .or. ifac.eq.4) then
                i1=icldext(icld,1);      i2=icldext(icld,2)-1
                j1=isvmin;               j2=isvmax-1
                k1=icldext(icld,3)+iqtr; k2=icldext(icld,4)-1
              elseif (ifac.eq.5 .or. ifac.eq.6) then
                i1=icldext(icld,1);      i2=icldext(icld,2)-1
                j1=icldext(icld,3)+iqtr; j2=icldext(icld,4)-1
                k1=iswmin;               k2=iswmax-1
              endif
              write (t12,'(2a,i3.3,a,i3.3)')
     &          facstr,'sld',NSB(ICFD)+1,'_s',icldsur
              call MKCFDBC(show,4,ifac,i1,i2,j1,j2,k1,k2,t12,
     &          SNAME(icldzn,icldsur),dum,1,
     &          20.0,0.0,0.0,0.0,0.0,IER)
            endif

C Opening BC for 25% of width, at vertical centre line, solid BCs for
C rest.
          elseif (iibctyp.eq.6) then

C Find 25% of width, rounded, at least 1 column, on vertical centre
C line.
            icols=icldext(icld,2)-icldext(icld,1)
            rcols=real(icols)
            iqtr=nint(rcols/4.0)
            if (iqtr.eq.0) iqtr=1
            imidcol=icldext(icld,1)+icols/2
            if (mod(iqtr,2).eq.0) then
              imidqtrmin=imidcol-iqtr/2
              imidqtrmax=imidcol+iqtr/2-1
            else
              imidqtrmin=imidcol-iqtr/2
              imidqtrmax=imidcol+iqtr/2
            endif

            if (ifac.eq.1 .or. ifac.eq.2) then
              i1=isumin;          i2=isumax-1
              j1=imidqtrmin;      j2=imidqtrmax
              k1=icldext(icld,3); k2=icldext(icld,4)-1
            elseif (ifac.eq.3 .or. ifac.eq.4) then
              i1=imidqtrmin;      i2=imidqtrmax
              j1=isvmin;          j2=isvmax-1
              k1=icldext(icld,3); k2=icldext(icld,4)-1
            elseif (ifac.eq.5 .or. ifac.eq.6) then
              i1=imidqtrmin;      i2=imidqtrmax
              j1=icldext(icld,3); j2=icldext(icld,4)-1
              k1=iswmin;          k2=iswmax-1
            endif
            write (t12,'(2a,i3.3,a,i3.3)')
     &        facstr,'opn',NOPEN(ICFD)+1,'_s',icldsur
            call MKCFDBC(show,11,ifac,i1,i2,j1,j2,k1,k2,t12,
     &        SNAME(icldzn,icldsur),dum,1,
     &        20.0,0.0,0.01,0.0,0.0,IER)

C Is there any more of the surface to the left for a solid BC?
            if ((icldext(icld,1)).lt.imidqtrmin) then
              if (ifac.eq.1 .or. ifac.eq.2) then
                i1=isumin;          i2=isumax-1
                j1=icldext(icld,1); j2=imidqtrmin-1
                k1=icldext(icld,3); k2=icldext(icld,4)-1
              elseif (ifac.eq.3 .or. ifac.eq.4) then
                i1=icldext(icld,1); i2=imidqtrmin-1
                j1=isvmin;          j2=isvmax-1
                k1=icldext(icld,3); k2=icldext(icld,4)-1
              elseif (ifac.eq.5 .or. ifac.eq.6) then
                i1=icldext(icld,1); i2=imidqtrmin-1
                j1=icldext(icld,3); j2=icldext(icld,4)-1
                k1=iswmin;          k2=iswmax-1
              endif
              write (t12,'(2a,i3.3,a,i3.3)')
     &          facstr,'sld',NSB(ICFD)+1,'_s',icldsur
              call MKCFDBC(show,4,ifac,i1,i2,j1,j2,k1,k2,t12,
     &          SNAME(icldzn,icldsur),dum,1,
     &          20.0,0.0,0.0,0.0,0.0,IER)
            endif

C Is there any more of the surface to the right for a solid BC?
            if ((icldext(icld,2)-1).gt.imidqtrmax) then
              if (ifac.eq.1 .or. ifac.eq.2) then
                i1=isumin;          i2=isumax-1
                j1=imidqtrmax+1;    j2=icldext(icld,2)-1
                k1=icldext(icld,3); k2=icldext(icld,4)-1
              elseif (ifac.eq.3 .or. ifac.eq.4) then
                i1=imidqtrmax+1;    i2=icldext(icld,2)-1
                j1=isvmin;          j2=isvmax-1
                k1=icldext(icld,4); k2=icldext(icld,4)-1
              elseif (ifac.eq.5 .or. ifac.eq.6) then
                i1=imidqtrmax+1;    i2=icldext(icld,2)-1
                j1=icldext(icld,3); j2=icldext(icld,4)-1
                k1=iswmin;          k2=iswmax-1
              endif
              write (t12,'(2a,i3.3,a,i3.3)')
     &          facstr,'sld',NSB(ICFD)+1,'_s',icldsur
              call MKCFDBC(show,4,ifac,i1,i2,j1,j2,k1,k2,t12,
     &          SNAME(icldzn,icldsur),dum,1,
     &          20.0,0.0,0.0,0.0,0.0,IER)
            endif

C Opening BCs for 12.5% of height, at top and bottom edges, solid BC for
C rest.
          elseif (iibctyp.eq.7) then

C Find 12.5% of height, rounded, at least 1 row.
            irows=icldext(icld,4)-icldext(icld,3)
            rrows=real(irows)
            i8th=nint(rrows/8.0)
            if (i8th.eq.0) i8th=1

C Is there enough space for both openings?
            ok=.true.
   80       if (irows.lt.(i8th*2)) then
              if (i8th.gt.1) then
                i8th=i8th-1
              else
                write(outs,'(3a)')' Not enough space for openings at',
     &            ' top and bottom of this sash window; only one',
     &            ' opening will be created.'
                call EDISP(IUOUT,outs)
                ok=.false.
              endif
              goto 80
            endif

C Opening at bottom edge.
            if (ifac.eq.1 .or. ifac.eq.2) then
              i1=isumin;          i2=isumax-1
              j1=icldext(icld,1); j2=icldext(icld,2)-1
              k1=icldext(icld,3); k2=icldext(icld,3)+i8th-1
            elseif (ifac.eq.3 .or. ifac.eq.4) then
              i1=icldext(icld,1); i2=icldext(icld,2)-1
              j1=isvmin;          j2=isvmax-1
              k1=icldext(icld,3); k2=icldext(icld,3)+i8th-1
            elseif (ifac.eq.5 .or. ifac.eq.6) then
              i1=icldext(icld,1); i2=icldext(icld,2)-1
              j1=icldext(icld,3); j2=icldext(icld,3)+i8th-1
              k1=iswmin;          k2=iswmax-1
            endif
            write (t12,'(2a,i3.3,a,i3.3)')
     &        facstr,'opn',NOPEN(ICFD)+1,'_s',icldsur
            call MKCFDBC(show,11,ifac,i1,i2,j1,j2,k1,k2,t12,
     &        SNAME(icldzn,icldsur),dum,1,
     &        20.0,0.0,0.01,0.0,0.0,IER)

C Is there room for the top opening?
            if (ok) then
              if (ifac.eq.1 .or. ifac.eq.2) then
                i1=isumin;               i2=isumax-1
                j1=icldext(icld,1);      j2=icldext(icld,2)-1
                k1=icldext(icld,4)-i8th; k2=icldext(icld,4)-1
              elseif (ifac.eq.3 .or. ifac.eq.4) then
                i1=icldext(icld,1);      i2=icldext(icld,2)-1
                j1=isvmin;               j2=isvmax-1
                k1=icldext(icld,4)-i8th; k2=icldext(icld,4)-1
              elseif (ifac.eq.5 .or. ifac.eq.6) then
                i1=icldext(icld,1);      i2=icldext(icld,2)-1
                j1=icldext(icld,4)-i8th; j2=icldext(icld,4)-1
                k1=iswmin;               k2=iswmax-1
              endif
              itop=icldext(icld,4)-i8th-1
              write (t12,'(2a,i3.3,a,i3.3)')
     &          facstr,'opn',NOPEN(ICFD)+1,'_s',icldsur
              call MKCFDBC(show,11,ifac,i1,i2,j1,j2,k1,k2,t12,
     &          SNAME(icldzn,icldsur),dum,1,
     &          20.0,0.0,0.01,0.0,0.0,IER)
            else
              itop=icldext(icld,4)-1
            endif

C Is there any more of the surface for a solid BC?
            if ((icldext(icld,3)+i8th).le.itop) then
              if (ifac.eq.1 .or. ifac.eq.2) then
                i1=isumin;               i2=isumax-1
                j1=icldext(icld,1);      j2=icldext(icld,2)-1
                k1=icldext(icld,3)+i8th; k2=itop
              elseif (ifac.eq.3 .or. ifac.eq.4) then
                i1=icldext(icld,1);      i2=icldext(icld,2)-1
                j1=isvmin;               j2=isvmax-1
                k1=icldext(icld,3)+i8th; k2=itop
              elseif (ifac.eq.5 .or. ifac.eq.6) then
                i1=icldext(icld,1);      i2=icldext(icld,2)-1
                j1=icldext(icld,3)+i8th; j2=itop
                k1=iswmin;               k2=iswmax-1
              endif
              write (t12,'(2a,i3.3,a,i3.3)')
     &          facstr,'sld',NSB(ICFD)+1,'_s',icldsur
              call MKCFDBC(show,4,ifac,i1,i2,j1,j2,k1,k2,t12,
     &          SNAME(icldzn,icldsur),dum,1,
     &          20.0,0.0,0.0,0.0,0.0,IER)
            endif

C Opening BC for 75% of height and width, at bottom edge, solid BCs for
C rest.            
          elseif (iibctyp.eq.8) then

C Find 75% of height, rounded, at least 1 row, at bottom edge.
            irows=icldext(icld,4)-icldext(icld,3)
            rrows=real(irows)
            i3qtrhgt=nint(3.0*rrows/4.0)
            if (i3qtrhgt.eq.0) i3qtrhgt=1
            ibtm3qtrhgtmin=icldext(icld,3)
            ibtm3qtrhgtmax=icldext(icld,3)+i3qtrhgt-1

C Find 75% of width, rounded, at least 1 column, on vertical centre
C line.
            icols=icldext(icld,2)-icldext(icld,1)
            rcols=real(icols)
            i3qtrwdt=nint(3.0*rcols/4.0)
            if (i3qtrwdt.eq.0) i3qtrwdt=1
            imidcol=icldext(icld,1)+icols/2
            if (mod(i3qtrwdt,2).eq.0) then
              imid3qtrwdtmin=imidcol-i3qtrwdt/2
              imid3qtrwdtmax=imidcol+i3qtrwdt/2-1
            else
              imid3qtrwdtmin=imidcol-i3qtrwdt/2
              imid3qtrwdtmax=imidcol+i3qtrwdt/2
            endif

C Opening BC.
            if (ifac.eq.1 .or. ifac.eq.2) then
              i1=isumin;         i2=isumax-1
              j1=imid3qtrwdtmin; j2=imid3qtrwdtmax
              k1=ibtm3qtrhgtmin; k2=ibtm3qtrhgtmax
            elseif (ifac.eq.3 .or. ifac.eq.4) then
              i1=imid3qtrwdtmin; i2=imid3qtrwdtmax
              j1=isvmin;         j2=isvmax-1
              k1=ibtm3qtrhgtmin; k2=ibtm3qtrhgtmax
            elseif (ifac.eq.5 .or. ifac.eq.6) then
              i1=imid3qtrwdtmin; i2=imid3qtrwdtmax
              j1=ibtm3qtrhgtmin; j2=ibtm3qtrhgtmax
              k1=iswmin;         k2=iswmax-1
            endif
            write (t12,'(2a,i3.3,a,i3.3)')
     &        facstr,'opn',NOPEN(ICFD)+1,'_s',icldsur
            call MKCFDBC(show,11,ifac,i1,i2,j1,j2,k1,k2,t12,
     &        SNAME(icldzn,icldsur),dum,1,
     &        20.0,0.0,0.01,0.0,0.0,IER)

C Is there room left at the top?
            if (ibtm3qtrhgtmax.lt.(icldext(icld,4)-1)) then
              if (ifac.eq.1 .or. ifac.eq.2) then
                i1=isumin;           i2=isumax-1
                j1=icldext(icld,1);  j2=icldext(icld,2)-1
                k1=ibtm3qtrhgtmax+1; k2=icldext(icld,4)-1
              elseif (ifac.eq.3 .or. ifac.eq.4) then
                i1=icldext(icld,1);  i2=icldext(icld,2)-1
                j1=isvmin;           j2=isvmax-1
                k1=ibtm3qtrhgtmax+1; k2=icldext(icld,4)-1
              elseif (ifac.eq.5 .or. ifac.eq.6) then
                i1=icldext(icld,1);  i2=icldext(icld,2)-1
                j1=ibtm3qtrhgtmax+1; j2=icldext(icld,4)-1
                k1=iswmin;           k2=iswmax-1
              endif
              write (t12,'(2a,i3.3,a,i3.3)')
     &          facstr,'sld',NSB(ICFD)+1,'_s',icldsur
              call MKCFDBC(show,4,ifac,i1,i2,j1,j2,k1,k2,t12,
     &          SNAME(icldzn,icldsur),dum,1,
     &          20.0,0.0,0.0,0.0,0.0,IER)
              ok=.true.
            else
              ok=.false.
            endif

C Is there room at the left?
            if (imid3qtrwdtmin.gt.icldext(icld,1)) then
              if (ok) then
                itop=icldext(icld,4)-2
              else
                itop=icldext(icld,4)-1
              endif
              if (ifac.eq.1 .or. ifac.eq.2) then
                i1=isumin;          i2=isumax-1
                j1=icldext(icld,1); j2=imid3qtrwdtmin-1
                k1=icldext(icld,3); k2=itop
              elseif (ifac.eq.3 .or. ifac.eq.4) then
                i1=icldext(icld,1); i2=imid3qtrwdtmin-1
                j1=isvmin;          j2=isvmax-1
                k1=icldext(icld,3); k2=itop
              elseif (ifac.eq.5 .or. ifac.eq.6) then
                i1=icldext(icld,1); i2=imid3qtrwdtmin-1
                j1=icldext(icld,3); j2=itop
                k1=iswmin;          k2=iswmax-1
              endif
              write (t12,'(2a,i3.3,a,i3.3)')
     &          facstr,'sld',NSB(ICFD)+1,'_s',icldsur
              call MKCFDBC(show,4,ifac,i1,i2,j1,j2,k1,k2,t12,
     &          SNAME(icldzn,icldsur),dum,1,
     &          20.0,0.0,0.0,0.0,0.0,IER)
            endif

C Is there room at the right?
            if (imid3qtrwdtmax.lt.(icldext(icld,2)-1)) then
              if (ok) then
                itop=icldext(icld,4)-2
              else
                itop=icldext(icld,4)-1
              endif
              if (ifac.eq.1 .or. ifac.eq.2) then
                i1=isumin;           i2=isumax-1
                j1=imid3qtrwdtmax+1; j2=icldext(icld,2)-1
                k1=icldext(icld,3);  k2=itop
              elseif (ifac.eq.3 .or. ifac.eq.4) then
                i1=imid3qtrwdtmax+1; i2=icldext(icld,2)-1
                j1=isvmin;           j2=isvmax-1
                k1=icldext(icld,3);  k2=itop
              elseif (ifac.eq.5 .or. ifac.eq.6) then
                i1=imid3qtrwdtmax+1; i2=icldext(icld,2)-1
                j1=icldext(icld,3);  j2=itop
                k1=iswmin;           k2=iswmax-1
              endif
              write (t12,'(2a,i3.3,a,i3.3)')
     &          facstr,'sld',NSB(ICFD)+1,'_s',icldsur
              call MKCFDBC(show,4,ifac,i1,i2,j1,j2,k1,k2,t12,
     &          SNAME(icldzn,icldsur),dum,1,
     &          20.0,0.0,0.0,0.0,0.0,IER)
            endif

C 9 and 10 are for frames.
          elseif (iibctyp.eq.9 .or. iibctyp.eq.10) then
            write(outs,'(a)')
     &' This surface is a frame, but has no children. Check usage tags.'
            call EDISP(IUOUT,outs)
          endif
        enddo

C Create parent surface BC.
        if (dothisur .and. .not.lumped) then

          write(outs,'(a,i3.3,2a)')' Creating BC(s) for surface ',
     &      isur,SNAME(ICP,isur)(1:lnblnk(SNAME(ICP,isur))),' ...'
          ISHSB=0; ISHAO=0; ISHBLK=0; IFACES=1; ISHGEO=-isur
          call redraw(IER)
          call pausems(500)
          ISHGEO=-9999

C Solid BCs drawn around child.
          if (ibctyp.eq.1) then

C Is there space on the top?
            irows=iparext(4)-icldext(1,4)
            if (irows.gt.0) then
              if (ifac.eq.1 .or. ifac.eq.2) then
                i1=isumin;       i2=isumax-1
                j1=iparext(1);   j2=iparext(2)-1
                k1=icldext(1,4); k2=iparext(4)-1
              elseif (ifac.eq.3 .or. ifac.eq.4) then
                i1=iparext(1);   i2=iparext(2)-1
                j1=isvmin;       j2=isvmax-1
                k1=icldext(1,4); k2=iparext(4)-1
              elseif (ifac.eq.5 .or. ifac.eq.6) then
                i1=iparext(1);   i2=iparext(2)-1
                j1=icldext(1,4); j2=iparext(4)-1
                k1=iswmin;       k2=iswmax-1
              endif
              write (t12,'(2a,i3.3,a,i3.3)')
     &          facstr,'sld',NSB(ICFD)+1,'_s',isur
              call MKCFDBC(show,4,ifac,i1,i2,j1,j2,k1,k2,t12,
     &          SNAME(ICP,isur),dum,1,20.0,0.0,0.0,0.0,0.0,IER)
              itop=iparext(4)-1-irows
            else
              itop=iparext(4)-1
            endif

C Is there space on the bottom?
            irows=icldext(1,3)-iparext(3)
            if (irows.gt.0) then
              if (ifac.eq.1 .or. ifac.eq.2) then
                i1=isumin;     i2=isumax-1
                j1=iparext(1); j2=iparext(2)-1
                k1=iparext(3); k2=icldext(1,3)-1
              elseif (ifac.eq.3 .or. ifac.eq.4) then
                i1=iparext(1); i2=iparext(2)-1
                j1=isvmin;     j2=isvmax-1
                k1=iparext(3); k2=icldext(1,3)-1
              elseif (ifac.eq.5 .or. ifac.eq.6) then
                i1=iparext(1); i2=iparext(2)-1
                j1=iparext(3); j2=icldext(1,3)-1
                k1=iswmin;     k2=iswmax-1
              endif
              write (t12,'(2a,i3.3,a,i3.3)')
     &          facstr,'sld',NSB(ICFD)+1,'_s',isur
              call MKCFDBC(show,4,ifac,i1,i2,j1,j2,k1,k2,t12,
     &          SNAME(ICP,isur),dum,1,20.0,0.0,0.0,0.0,0.0,IER)
              ibot=iparext(3)+irows
            else
              ibot=iparext(3)
            endif

C Is there space on the left?
            icols=icldext(1,1)-iparext(1)
            if (icols.gt.0) then
              if (ifac.eq.1 .or. ifac.eq.2) then
                i1=isumin;     i2=isumax-1
                j1=iparext(1); j2=icldext(1,1)-1
                k1=ibot;       k2=itop
              elseif (ifac.eq.3 .or. ifac.eq.4) then
                i1=iparext(1); i2=icldext(1,1)-1
                j1=isvmin;     j2=isvmax-1
                k1=ibot;       k2=itop
              elseif (ifac.eq.5 .or. ifac.eq.6) then
                i1=iparext(1); i2=icldext(1,1)-1
                j1=ibot;       j2=itop
                k1=iswmin;     k2=iswmax-1
              endif
              write (t12,'(2a,i3.3,a,i3.3)')
     &          facstr,'sld',NSB(ICFD)+1,'_s',isur
              call MKCFDBC(show,4,ifac,i1,i2,j1,j2,k1,k2,t12,
     &          SNAME(ICP,isur),dum,1,20.0,0.0,0.0,0.0,0.0,IER)
            endif

C Is there space on the right?
            icols=iparext(2)-icldext(1,2)
            if (icols.gt.0) then
              if (ifac.eq.1 .or. ifac.eq.2) then
                i1=isumin;       i2=isumax-1
                j1=icldext(1,2); j2=iparext(2)-1
                k1=ibot;         k2=itop
              elseif (ifac.eq.3 .or. ifac.eq.4) then
                i1=icldext(1,2); i2=iparext(2)-1
                j1=isvmin;       j2=isvmax-1
                k1=ibot;         k2=itop
              elseif (ifac.eq.5 .or. ifac.eq.6) then
                i1=icldext(1,2); i2=iparext(2)-1
                j1=ibot;         j2=itop
                k1=iswmin;       k2=iswmax-1
              endif
              write (t12,'(2a,i3.3,a,i3.3)')
     &          facstr,'sld',NSB(ICFD)+1,'_s',isur
              call MKCFDBC(show,4,ifac,i1,i2,j1,j2,k1,k2,t12,
     &          SNAME(ICP,isur),dum,1,20.0,0.0,0.0,0.0,0.0,IER)
            endif

C Parent is not compatible with ibctyp=2 to 8.
          elseif (ibctyp.ge.2 .and. ibctyp.le.8) then
            write(outs,'(2a)')' Parent surface BCs could not be',
     &        ' created based on usage flags.'
            call EDISP(IUOUT,outs)
            ISTAT=5
            goto 999

C Frame with crack. Use first edge where there is space (in order of
C top, bottom, left, right) for crack, other edges where there is space
C use a solid BC.
          elseif (ibctyp.eq.9) then

            ok=.false.
C Is there space on the top?
            irows=iparext(4)-icldext(1,4)
            if (irows.gt.0) then
              if (ifac.eq.1 .or. ifac.eq.2) then
                i1=isumin;       i2=isumax-1
                j1=iparext(1);   j2=iparext(2)-1
                k1=icldext(1,4); k2=icldext(1,4)
              elseif (ifac.eq.3 .or. ifac.eq.4) then
                i1=iparext(1);   i2=iparext(2)-1
                j1=isvmin;       j2=isvmax-1
                k1=icldext(1,4); k2=icldext(1,4)
              elseif (ifac.eq.5 .or. ifac.eq.6) then
                i1=iparext(1);   i2=iparext(2)-1
                j1=icldext(1,4); j2=icldext(1,4)
                k1=iswmin;       k2=iswmax-1
              endif
              write (t12,'(2a,i3.3,a,i3.3)')
     &          facstr,'opn',NOPEN(ICFD)+1,'_s',isur
              call MKCFDBC(show,11,ifac,i1,i2,j1,j2,k1,k2,t12,
     &          SNAME(ICP,isur),dum,1,20.0,0.0,0.01,0.0,0.0,IER)
              ok=.true.

C Any space left over that needs filling with a solid bc?
              if (irows.gt.1) then
                if (ifac.eq.1 .or. ifac.eq.2) then
                  i1=isumin;         i2=isumax-1
                  j1=iparext(1);     j2=iparext(2)-1
                  k1=icldext(1,4)+1; k2=iparext(4)-1
                elseif (ifac.eq.3 .or. ifac.eq.4) then
                  i1=iparext(1);     i2=iparext(2)-1
                  j1=isvmin;         j2=isvmax-1
                  k1=icldext(1,4)+1; k2=iparext(4)-1
                elseif (ifac.eq.5 .or. ifac.eq.6) then
                  i1=iparext(1);     i2=iparext(2)-1
                  j1=icldext(1,4)+1; j2=iparext(4)-1
                  k1=iswmin;         k2=iswmax-1
                endif
                write (t12,'(2a,i3.3,a,i3.3)')
     &            facstr,'sld',NSB(ICFD)+1,'_s',isur
                call MKCFDBC(show,4,ifac,i1,i2,j1,j2,k1,k2,t12,
     &            SNAME(ICP,isur),dum,1,20.0,0.0,0.0,0.0,0.0,IER)
              endif

              itop=iparext(4)-1-irows
            else
              itop=iparext(4)-1
            endif

C Is there space on the bottom?
            irows=icldext(1,3)-iparext(3)
            if (irows.gt.0) then
              if (ok) then ! opening already created?
                if (ifac.eq.1 .or. ifac.eq.2) then
                  i1=isumin;     i2=isumax-1
                  j1=iparext(1); j2=iparext(2)-1
                  k1=iparext(3); k2=icldext(1,3)-1
                elseif (ifac.eq.3 .or. ifac.eq.4) then
                  i1=iparext(1); i2=iparext(2)-1
                  j1=isvmin;     j2=isvmax-1
                  k1=iparext(3); k2=icldext(1,3)-1
                elseif (ifac.eq.5 .or. ifac.eq.6) then
                  i1=iparext(1); i2=iparext(2)-1
                  j1=iparext(3); j2=icldext(1,3)-1
                  k1=iswmin;     k2=iswmax-1
                endif
                write (t12,'(2a,i3.3,a,i3.3)')
     &            facstr,'sld',NSB(ICFD)+1,'_s',isur
                call MKCFDBC(show,4,ifac,i1,i2,j1,j2,k1,k2,t12,
     &            SNAME(ICP,isur),dum,1,20.0,0.0,0.0,0.0,0.0,IER)
              else
                if (ifac.eq.1 .or. ifac.eq.2) then
                  i1=isumin;         i2=isumax-1
                  j1=iparext(1);     j2=iparext(2)-1
                  k1=icldext(1,3)-1; k2=icldext(1,3)-1
                elseif (ifac.eq.3 .or. ifac.eq.4) then
                  i1=iparext(1);     i2=iparext(2)-1
                  j1=isvmin;         j2=isvmax-1
                  k1=icldext(1,3)-1; k2=icldext(1,3)-1
                elseif (ifac.eq.5 .or. ifac.eq.6) then
                  i1=iparext(1);     i2=iparext(2)-1
                  j1=icldext(1,3)-1; j2=icldext(1,3)-1
                  k1=iswmin;         k2=iswmax-1
                endif
                write (t12,'(2a,i3.3,a,i3.3)')
     &            facstr,'opn',NOPEN(ICFD)+1,'_s',isur
                call MKCFDBC(show,11,ifac,i1,i2,j1,j2,k1,k2,t12,
     &            SNAME(ICP,isur),dum,1,20.0,0.0,0.01,0.0,0.0,IER)
                ok=.true.

C Any space left over that needs filling with a solid bc?
                if (irows.gt.1) then
                  if (ifac.eq.1 .or. ifac.eq.2) then
                    i1=isumin;     i2=isumax-1
                    j1=iparext(1); j2=iparext(2)-1
                    k1=iparext(3); k2=icldext(1,3)-2
                  elseif (ifac.eq.3 .or. ifac.eq.4) then
                    i1=iparext(1); i2=iparext(2)-1
                    j1=isvmin;     j2=isvmax-1
                    k1=iparext(3); k2=icldext(1,3)-2
                  elseif (ifac.eq.5 .or. ifac.eq.6) then
                    i1=iparext(1); i2=iparext(2)-1
                    j1=iparext(3); j2=icldext(1,3)-2
                    k1=iswmin;     k2=iswmax-1
                  endif
                  write (t12,'(2a,i3.3,a,i3.3)')
     &              facstr,'sld',NSB(ICFD)+1,'_s',isur
                  call MKCFDBC(show,4,ifac,i1,i2,j1,j2,k1,k2,t12,
     &              SNAME(ICP,isur),dum,1,20.0,0.0,0.0,0.0,0.0,IER)
                endif
              endif
              ibot=iparext(3)+irows
            else
              ibot=iparext(3)
            endif

C Is there space on the left?
            icols=icldext(1,1)-iparext(1)
            if (icols.gt.0) then
              if (ok) then
                if (ifac.eq.1 .or. ifac.eq.2) then
                  i1=isumin;     i2=isumax-1
                  j1=iparext(1); j2=icldext(1,1)-1
                  k1=ibot;       k2=itop
                elseif (ifac.eq.3 .or. ifac.eq.4) then
                  i1=iparext(1); i2=icldext(1,1)-1
                  j1=isvmin;     j2=isvmax-1
                  k1=ibot;       k2=itop
                elseif (ifac.eq.5 .or. ifac.eq.6) then
                  i1=iparext(1); i2=icldext(1,1)-1
                  j1=ibot;       j2=itop
                  k1=iswmin;     k2=iswmax-1
                endif
                write (t12,'(2a,i3.3,a,i3.3)')
     &            facstr,'sld',NSB(ICFD)+1,'_s',isur
                call MKCFDBC(show,4,ifac,i1,i2,j1,j2,k1,k2,t12,
     &            SNAME(ICP,isur),dum,1,20.0,0.0,0.0,0.0,0.0,IER)
              else
                if (ifac.eq.1 .or. ifac.eq.2) then
                  i1=isumin;         i2=isumax-1
                  j1=icldext(1,1)-1; j2=icldext(1,1)-1
                  k1=ibot;           k2=itop
                elseif (ifac.eq.3 .or. ifac.eq.4) then
                  i1=icldext(1,1)-1; i2=icldext(1,1)-1
                  j1=isvmin;         j2=isvmax-1
                  k1=ibot;           k2=itop
                elseif (ifac.eq.5 .or. ifac.eq.6) then
                  i1=icldext(1,1)-1; i2=icldext(1,1)-1
                  j1=ibot;           j2=itop
                  k1=iswmin;         k2=iswmax-1
                endif
                write (t12,'(2a,i3.3,a,i3.3)')
     &            facstr,'opn',NOPEN(ICFD)+1,'_s',isur
                call MKCFDBC(show,11,ifac,i1,i2,j1,j2,k1,k2,t12,
     &            SNAME(ICP,isur),dum,1,20.0,0.0,0.01,0.0,0.0,IER)

C Any space left over that needs filling with a solid bc?
                if (icols.gt.1) then
                  if (ifac.eq.1 .or. ifac.eq.2) then
                    i1=isumin;     i2=isumax-1
                    j1=iparext(1); j2=icldext(1,1)-2
                    k1=ibot;       k2=itop
                  elseif (ifac.eq.3 .or. ifac.eq.4) then
                    i1=iparext(1); i2=icldext(1,1)-2
                    j1=isvmin;     j2=isvmax-1
                    k1=ibot;       k2=itop
                  elseif (ifac.eq.5 .or. ifac.eq.6) then
                    i1=iparext(1); i2=icldext(1,1)-2
                    j1=ibot;       j2=itop
                    k1=iswmin;     k2=iswmax-1
                  endif
                  write (t12,'(2a,i3.3,a,i3.3)')
     &              facstr,'sld',NSB(ICFD)+1,'_s',isur
                  call MKCFDBC(show,4,ifac,i1,i2,j1,j2,k1,k2,t12,
     &              SNAME(ICP,isur),dum,1,20.0,0.0,0.0,0.0,0.0,IER)
                endif
              endif
            endif

C Is there space on the right?
            icols=iparext(2)-icldext(1,2)
            if (icols.gt.0) then
              if (ok) then
                if (ifac.eq.1 .or. ifac.eq.2) then
                  i1=isumin;       i2=isumax-1
                  j1=icldext(1,2); j2=iparext(2)-1
                  k1=ibot;         k2=itop
                elseif (ifac.eq.3 .or. ifac.eq.4) then
                  i1=icldext(1,2); i2=iparext(2)-1
                  j1=isvmin;       j2=isvmax-1
                  k1=ibot;         k2=itop
                elseif (ifac.eq.5 .or. ifac.eq.6) then
                  i1=icldext(1,2); i2=iparext(2)-1
                  j1=ibot;         j2=itop
                  k1=iswmin;       k2=iswmax-1
                endif
                write (t12,'(2a,i3.3,a,i3.3)')
     &            facstr,'sld',NSB(ICFD)+1,'_s',isur
                call MKCFDBC(show,4,ifac,i1,i2,j1,j2,k1,k2,t12,
     &            SNAME(ICP,isur),dum,1,20.0,0.0,0.0,0.0,0.0,IER)
              else
                if (ifac.eq.1 .or. ifac.eq.2) then
                  i1=isumin;       i2=isumax-1
                  j1=icldext(1,2); j2=icldext(1,2)
                  k1=ibot;         k2=itop
                elseif (ifac.eq.3 .or. ifac.eq.4) then
                  i1=icldext(1,2); i2=icldext(1,2)
                  j1=isvmin;       j2=isvmax-1
                  k1=ibot;         k2=itop
                elseif (ifac.eq.5 .or. ifac.eq.6) then
                  i1=icldext(1,2); i2=icldext(1,2)
                  j1=ibot;         j2=itop
                  k1=iswmin;       k2=iswmax-1
                endif
                write (t12,'(2a,i3.3,a,i3.3)')
     &            facstr,'opn',NOPEN(ICFD)+1,'_s',isur
                call MKCFDBC(show,11,ifac,i1,i2,j1,j2,k1,k2,t12,
     &            SNAME(ICP,isur),dum,1,20.0,0.0,0.01,0.0,0.0,IER)

C Any space left over that needs filling with a solid bc?
                if (icols.gt.1) then
                  if (ifac.eq.1 .or. ifac.eq.2) then
                    i1=isumin;         i2=isumax-1
                    j1=icldext(1,2)+1; j2=iparext(2)-1
                    k1=ibot;           k2=itop
                  elseif (ifac.eq.3 .or. ifac.eq.4) then
                    j1=icldext(1,2)+1; j2=iparext(2)-1
                    j1=isvmin;         j2=isvmax-1
                    k1=ibot;           k2=itop
                  elseif (ifac.eq.5 .or. ifac.eq.6) then
                    j1=icldext(1,2)+1; j2=iparext(2)-1
                    j1=ibot;           j2=itop
                    k1=iswmin;         k2=iswmax-1
                  endif
                  write (t12,'(2a,i3.3,a,i3.3)')
     &              facstr,'sld',NSB(ICFD)+1,'_s',isur
                  call MKCFDBC(show,4,ifac,i1,i2,j1,j2,k1,k2,t12,
     &              SNAME(ICP,isur),dum,1,20.0,0.0,0.0,0.0,0.0,IER)
                endif
              endif
            endif

C Frame with a trickle vent. Use first edge where there is space (in
C order of top, bottom, left, right) for vent, other edges where there
C is space use a solid BC.
          elseif (ibctyp.eq.10) then

c Find 20% of frame inside width, rounded, at least 1 cell.            
            icols=icldext(icld,2)-icldext(icld,1)
            rcols=real(icols)
            iwdt=nint(rcols/5.0)
            if (iwdt.eq.0) iwdt=1
            icols=iparext(2)-iparext(1)

            ok=.false.

C Is there space on the top?
            irows=iparext(4)-icldext(1,4)
            if (irows.gt.0) then

C Find cells to put the trickle vent in the middle.
              imidcol=icldext(icld,1)+icols/2
              if (mod(iwdt,2).eq.0) then
                iwdtmin=imidcol-iwdt/2
                iwdtmax=imidcol+iwdt/2-1
              else
                iwdtmin=imidcol-iwdt/2
                iwdtmax=imidcol+iwdt/2
              endif

              if (ifac.eq.1 .or. ifac.eq.2) then
                i1=isumin;       i2=isumax-1
                j1=iwdtmin;      j2=iwdtmax
                k1=icldext(1,4); k2=icldext(1,4)
              elseif (ifac.eq.3 .or. ifac.eq.4) then
                i1=iwdtmin;      i2=iwdtmax
                j1=isvmin;       j2=isvmax-1
                k1=icldext(1,4); k2=icldext(1,4)
              elseif (ifac.eq.5 .or. ifac.eq.6) then
                i1=iwdtmin;      i2=iwdtmax
                j1=icldext(1,4); j2=icldext(1,4)
                k1=iswmin;       k2=iswmax-1
              endif
              write (t12,'(2a,i3.3,a,i3.3)')
     &          facstr,'opn',NOPEN(ICFD)+1,'_s',isur
              call MKCFDBC(show,11,ifac,i1,i2,j1,j2,k1,k2,t12,
     &          SNAME(ICP,isur),dum,1,20.0,0.0,0.01,0.0,0.0,IER)
              ok=.true.

              if (iparext(1).lt.iwdtmin) then
                if (ifac.eq.1 .or. ifac.eq.2) then
                  i1=isumin;       i2=isumax-1
                  j1=iparext(1);   j2=iwdtmin-1
                  k1=icldext(1,4); k2=icldext(1,4)
                elseif (ifac.eq.3 .or. ifac.eq.4) then
                  i1=iparext(1);   i2=iwdtmin-1
                  j1=isvmin;       j2=isvmax-1
                  k1=icldext(1,4); k2=icldext(1,4)
                elseif (ifac.eq.5 .or. ifac.eq.6) then
                  i1=iparext(1);   i2=iwdtmin-1
                  j1=icldext(1,4); j2=icldext(1,4)
                  k1=iswmin;       k2=iswmax-1
                endif
                write (t12,'(2a,i3.3,a,i3.3)')
     &            facstr,'sld',NSB(ICFD)+1,'_s',isur
                call MKCFDBC(show,4,ifac,i1,i2,j1,j2,k1,k2,t12,
     &            SNAME(ICP,isur),dum,1,20.0,0.0,0.0,0.0,0.0,IER)
              endif

              if ((iparext(2)-1).gt.iwdtmax) then
                if (ifac.eq.1 .or. ifac.eq.2) then
                  i1=isumin;       i2=isumax-1
                  j1=iwdtmax+1;    j2=iparext(2)-1
                  k1=icldext(1,4); k2=icldext(1,4)
                elseif (ifac.eq.3 .or. ifac.eq.4) then
                  i1=iwdtmax+1;    i2=iparext(2)-1
                  j1=isvmin;       j2=isvmax-1
                  k1=icldext(1,4); k2=icldext(1,4)
                elseif (ifac.eq.5 .or. ifac.eq.6) then
                  i1=iwdtmax+1;    i2=iparext(2)-1
                  j1=icldext(1,4); j2=icldext(1,4)
                  k1=iswmin;       k2=iswmax-1
                endif
                write (t12,'(2a,i3.3,a,i3.3)')
     &            facstr,'sld',NSB(ICFD)+1,'_s',isur
                call MKCFDBC(show,4,ifac,i1,i2,j1,j2,k1,k2,t12,
     &            SNAME(ICP,isur),dum,1,20.0,0.0,0.0,0.0,0.0,IER)
              endif

C Any space left over that needs filling with a solid bc?
              if (irows.gt.1) then
                if (ifac.eq.1 .or. ifac.eq.2) then
                  i1=isumin;         i2=isumax-1
                  j1=iparext(1);     j2=iparext(2)-1
                  k1=icldext(1,4)+1; k2=iparext(4)-1
                elseif (ifac.eq.3 .or. ifac.eq.4) then
                  i1=iparext(1);     i2=iparext(2)-1
                  j1=isvmin;         j2=isvmax-1
                  k1=icldext(1,4)+1; k2=iparext(4)-1
                elseif (ifac.eq.5 .or. ifac.eq.6) then
                  i1=iparext(1);     i2=iparext(2)-1
                  j1=icldext(1,4)+1; j2=iparext(4)-1
                  k1=iswmin;         k2=iswmax-1
                endif
                write (t12,'(2a,i3.3,a,i3.3)')
     &            facstr,'sld',NSB(ICFD)+1,'_s',isur
                call MKCFDBC(show,4,ifac,i1,i2,j1,j2,k1,k2,t12,
     &            SNAME(ICP,isur),dum,1,20.0,0.0,0.0,0.0,0.0,IER)
              endif

              itop=iparext(4)-1-irows
            else
              itop=iparext(4)-1
            endif

C Is there space on the bottom?
            irows=icldext(1,3)-iparext(3)
            if (irows.gt.0) then
              if (ok) then ! opening already created?
                if (ifac.eq.1 .or. ifac.eq.2) then
                  i1=isumin;     i2=isumax-1
                  j1=iparext(1); j2=iparext(2)-1
                  k1=iparext(3); k2=icldext(1,3)-1
                elseif (ifac.eq.3 .or. ifac.eq.4) then
                  i1=iparext(1); i2=iparext(2)-1
                  j1=isvmin;     j2=isvmax-1
                  k1=iparext(3); k2=icldext(1,3)-1
                elseif (ifac.eq.5 .or. ifac.eq.6) then
                  i1=iparext(1); i2=iparext(2)-1
                  j1=iparext(3); j2=icldext(1,3)-1
                  k1=iswmin;     k2=iswmax-1
                endif
                write (t12,'(2a,i3.3,a,i3.3)')
     &            facstr,'sld',NSB(ICFD)+1,'_s',isur
                call MKCFDBC(show,4,ifac,i1,i2,j1,j2,k1,k2,t12,
     &            SNAME(ICP,isur),dum,1,20.0,0.0,0.0,0.0,0.0,IER)
              else

C Find cells to put the trickle vent in the middle.
                imidcol=icldext(icld,1)+icols/2
                if (mod(iwdt,2).eq.0) then
                  iwdtmin=imidcol-iwdt/2
                  iwdtmax=imidcol+iwdt/2-1
                else
                  iwdtmin=imidcol-iwdt/2
                  iwdtmax=imidcol+iwdt/2
                endif

                if (ifac.eq.1 .or. ifac.eq.2) then
                  i1=isumin;         i2=isumax-1
                  j1=iwdtmin;        j2=iwdtmax
                  k1=icldext(1,3)-1; k2=icldext(1,3)-1
                elseif (ifac.eq.3 .or. ifac.eq.4) then
                  i1=iwdtmin;        i2=iwdtmax
                  j1=isvmin;         j2=isvmax-1
                  k1=icldext(1,3)-1; k2=icldext(1,3)-1
                elseif (ifac.eq.5 .or. ifac.eq.6) then
                  i1=iwdtmin;        i2=iwdtmax
                  j1=icldext(1,3)-1; j2=icldext(1,3)-1
                  k1=iswmin;         k2=iswmax-1
                endif
                write (t12,'(2a,i3.3,a,i3.3)')
     &            facstr,'opn',NOPEN(ICFD)+1,'_s',isur
                call MKCFDBC(show,11,ifac,i1,i2,j1,j2,k1,k2,t12,
     &            SNAME(ICP,isur),dum,1,20.0,0.0,0.01,0.0,0.0,IER)
                ok=.true.

                if (iparext(1).lt.iwdtmin) then
                  if (ifac.eq.1 .or. ifac.eq.2) then
                    i1=isumin;         i2=isumax-1
                    j1=iparext(1);     j2=iwdtmin-1
                    k1=icldext(1,3)-1; k2=icldext(1,3)-1
                  elseif (ifac.eq.3 .or. ifac.eq.4) then
                    i1=iparext(1);     i2=iwdtmin-1
                    j1=isvmin;         j2=isvmax-1
                    k1=icldext(1,3)-1; k2=icldext(1,3)-1
                  elseif (ifac.eq.5 .or. ifac.eq.6) then
                    i1=iparext(1);     i2=iwdtmin-1
                    j1=icldext(1,3)-1; j2=icldext(1,3)-1
                    k1=iswmin;         k2=iswmax-1
                  endif
                  write (t12,'(2a,i3.3,a,i3.3)')
     &              facstr,'sld',NSB(ICFD)+1,'_s',isur
                  call MKCFDBC(show,4,ifac,i1,i2,j1,j2,k1,k2,t12,
     &              SNAME(ICP,isur),dum,1,20.0,0.0,0.0,0.0,0.0,IER)
                endif

                if ((iparext(2)-1).gt.iwdtmax) then
                  if (ifac.eq.1 .or. ifac.eq.2) then
                    i1=isumin;         i2=isumax-1
                    j1=iwdtmax+1;      j2=iparext(2)-1
                    k1=icldext(1,3)-1; k2=icldext(1,3)-1
                  elseif (ifac.eq.3 .or. ifac.eq.4) then
                    i1=iwdtmax+1;      i2=iparext(2)-1
                    j1=isvmin;         j2=isvmax-1
                    k1=icldext(1,3)-1; k2=icldext(1,3)-1
                  elseif (ifac.eq.5 .or. ifac.eq.6) then
                    i1=iwdtmax+1;      i2=iparext(2)-1
                    j1=icldext(1,3)-1; j2=icldext(1,3)-1
                    k1=iswmin;         k2=iswmax-1
                  endif
                  write (t12,'(2a,i3.3,a,i3.3)')
     &              facstr,'sld',NSB(ICFD)+1,'_s',isur
                  call MKCFDBC(show,4,ifac,i1,i2,j1,j2,k1,k2,t12,
     &              SNAME(ICP,isur),dum,1,20.0,0.0,0.0,0.0,0.0,IER)
                endif

C Any space left over that needs filling with a solid bc?
                if (irows.gt.1) then
                  if (ifac.eq.1 .or. ifac.eq.2) then
                    i1=isumin;     i2=isumax-1
                    j1=iparext(1); j2=iparext(2)-1
                    k1=iparext(3); k2=icldext(1,3)-2
                  elseif (ifac.eq.3 .or. ifac.eq.4) then
                    i1=iparext(1); i2=iparext(2)-1
                    j1=isvmin;     j2=isvmax-1
                    k1=iparext(3); k2=icldext(1,3)-2
                  elseif (ifac.eq.5 .or. ifac.eq.6) then
                    i1=iparext(1); i2=iparext(2)-1
                    j1=iparext(3); j2=icldext(1,3)-2
                    k1=iswmin;     k2=iswmax-1
                  endif
                  write (t12,'(2a,i3.3,a,i3.3)')
     &              facstr,'sld',NSB(ICFD)+1,'_s',isur
                  call MKCFDBC(show,4,ifac,i1,i2,j1,j2,k1,k2,t12,
     &              SNAME(ICP,isur),dum,1,20.0,0.0,0.0,0.0,0.0,IER)
                endif
              endif
              ibot=iparext(3)+irows
            else
              ibot=iparext(3)
            endif

            irows=iparext(4)-iparext(3)

C Is there space on the left?
            icols=icldext(1,1)-iparext(1)
            if (icols.gt.0) then
              if (ok) then
                if (ifac.eq.1 .or. ifac.eq.2) then
                  i1=isumin;     i2=isumax-1
                  j1=iparext(1); j2=icldext(1,1)-1
                  k1=ibot;       k2=itop
                elseif (ifac.eq.3 .or. ifac.eq.4) then
                  i1=iparext(1); i2=icldext(1,1)-1
                  j1=isvmin;     j2=isvmax-1
                  k1=ibot;       k2=itop
                elseif (ifac.eq.5 .or. ifac.eq.6) then
                  i1=iparext(1); i2=icldext(1,1)-1
                  j1=ibot;       j2=itop
                  k1=iswmin;     k2=iswmax-1
                endif
                write (t12,'(2a,i3.3,a,i3.3)')
     &            facstr,'sld',NSB(ICFD)+1,'_s',isur
                call MKCFDBC(show,4,ifac,i1,i2,j1,j2,k1,k2,t12,
     &            SNAME(ICP,isur),dum,1,20.0,0.0,0.0,0.0,0.0,IER)
              else

C Find cells to put the trickle vent in the middle.
                imidrow=icldext(icld,3)+irows/2
                if (mod(iwdt,2).eq.0) then
                  iwdtmin=imidrow-iwdt/2
                  iwdtmax=imidrow+iwdt/2-1
                else
                  iwdtmin=imidrow-iwdt/2
                  iwdtmax=imidrow+iwdt/2
                endif

                if (ifac.eq.1 .or. ifac.eq.2) then
                  i1=isumin;         i2=isumax-1
                  j1=icldext(1,1)-1; j2=icldext(1,1)-1
                  k1=iwdtmin;        k2=iwdtmax
                elseif (ifac.eq.3 .or. ifac.eq.4) then
                  i1=icldext(1,1)-1; i2=icldext(1,1)-1
                  j1=isvmin;         j2=isvmax-1
                  k1=iwdtmin;        k2=iwdtmax
                elseif (ifac.eq.5 .or. ifac.eq.6) then
                  i1=icldext(1,1)-1; i2=icldext(1,1)-1
                  j1=iwdtmin;        j2=iwdtmax
                  k1=iswmin;         k2=iswmax-1
                endif
                write (t12,'(2a,i3.3,a,i3.3)')
     &            facstr,'opn',NOPEN(ICFD)+1,'_s',isur
                call MKCFDBC(show,11,ifac,i1,i2,j1,j2,k1,k2,t12,
     &            SNAME(ICP,isur),dum,1,20.0,0.0,0.01,0.0,0.0,IER)
                ok=.true.

                if (ibot.lt.iwdtmin) then
                  if (ifac.eq.1 .or. ifac.eq.2) then
                    i1=isumin;         i2=isumax-1
                    j1=icldext(1,1)-1; j2=icldext(1,1)-1
                    k1=ibot;           k2=iwdtmin-1
                  elseif (ifac.eq.3 .or. ifac.eq.4) then
                    i1=icldext(1,1)-1; i2=icldext(1,1)-1
                    j1=isvmin;         j2=isvmax-1
                    k1=ibot;           k2=iwdtmin-1
                  elseif (ifac.eq.5 .or. ifac.eq.6) then
                    i1=icldext(1,1)-1; i2=icldext(1,1)-1
                    j1=ibot;           j2=iwdtmin-1
                    k1=iswmin;         k2=iswmax-1
                  endif
                  write (t12,'(2a,i3.3,a,i3.3)')
     &              facstr,'sld',NSB(ICFD)+1,'_s',isur
                  call MKCFDBC(show,4,ifac,i1,i2,j1,j2,k1,k2,t12,
     &              SNAME(ICP,isur),dum,1,20.0,0.0,0.0,0.0,0.0,IER)
                endif

                if (itop.gt.iwdtmax) then
                  if (ifac.eq.1 .or. ifac.eq.2) then
                    i1=isumin;         i2=isumax-1
                    j1=icldext(1,1)-1; j2=icldext(1,1)-1
                    k1=iwdtmax+1;      k2=itop
                  elseif (ifac.eq.3 .or. ifac.eq.4) then
                    i1=icldext(1,1)-1; i2=icldext(1,1)-1
                    j1=isvmin;         j2=isvmax-1
                    k1=iwdtmax+1;      k2=itop
                  elseif (ifac.eq.5 .or. ifac.eq.6) then
                    i1=icldext(1,1)-1; i2=icldext(1,1)-1
                    j1=iwdtmax+1;      j2=itop
                    k1=iswmin;         k2=iswmax-1
                  endif
                  write (t12,'(2a,i3.3,a,i3.3)')
     &              facstr,'sld',NSB(ICFD)+1,'_s',isur
                  call MKCFDBC(show,4,ifac,i1,i2,j1,j2,k1,k2,t12,
     &              SNAME(ICP,isur),dum,1,20.0,0.0,0.0,0.0,0.0,IER)
                endif

C Any space left over that needs filling with a solid bc?
                if (icols.gt.1) then
                  if (ifac.eq.1 .or. ifac.eq.2) then
                    i1=isumin;     i2=isumax-1
                    j1=iparext(1); j2=icldext(1,1)-2
                    k1=ibot;       k2=itop
                  elseif (ifac.eq.3 .or. ifac.eq.4) then
                    i1=iparext(1); i2=icldext(1,1)-2
                    j1=isvmin;     j2=isvmax-1
                    k1=ibot;       k2=itop
                  elseif (ifac.eq.5 .or. ifac.eq.6) then
                    i1=iparext(1); i2=icldext(1,1)-2
                    j1=ibot;       j2=itop
                    k1=iswmin;     k2=iswmax-1
                  endif
                  write (t12,'(2a,i3.3,a,i3.3)')
     &              facstr,'sld',NSB(ICFD)+1,'_s',isur
                  call MKCFDBC(show,4,ifac,i1,i2,j1,j2,k1,k2,t12,
     &              SNAME(ICP,isur),dum,1,
     &              20.0,0.0,0.0,0.0,0.0,IER)
                endif
              endif
            endif

C Is there space on the right?
            icols=iparext(2)-icldext(1,2)
            if (icols.gt.0) then
              if (ok) then
                if (ifac.eq.1 .or. ifac.eq.2) then
                  i1=isumin;       i2=isumax-1
                  j1=icldext(1,2); j2=iparext(2)-1
                  k1=ibot;         k2=itop
                elseif (ifac.eq.3 .or. ifac.eq.4) then
                  i1=icldext(1,2); i2=iparext(2)-1
                  j1=isvmin;       j2=isvmax-1
                  k1=ibot;         k2=itop
                elseif (ifac.eq.5 .or. ifac.eq.6) then
                  i1=icldext(1,2); i2=iparext(2)-1
                  j1=ibot;         j2=itop
                  k1=iswmin;       k2=iswmax-1
                endif
                write (t12,'(2a,i3.3,a,i3.3)')
     &            facstr,'sld',NSB(ICFD)+1,'_s',isur
                call MKCFDBC(show,4,ifac,i1,i2,j1,j2,k1,k2,t12,
     &            SNAME(ICP,isur),dum,1,20.0,0.0,0.0,0.0,0.0,IER)
              else

C Find cells to put the trickle vent in the middle.
                imidrow=icldext(icld,3)+irows/2
                if (mod(iwdt,2).eq.0) then
                  iwdtmin=imidrow-iwdt/2
                  iwdtmax=imidrow+iwdt/2-1
                else
                  iwdtmin=imidrow-iwdt/2
                  iwdtmax=imidrow+iwdt/2
                endif

                if (ifac.eq.1 .or. ifac.eq.2) then
                  i1=isumin;       i2=isumax-1
                  j1=icldext(1,2); j2=icldext(1,2)
                  k1=iwdtmin;      k2=iwdtmax
                elseif (ifac.eq.3 .or. ifac.eq.4) then
                  i1=icldext(1,2); i2=icldext(1,2)
                  j1=isvmin;       j2=isvmax-1
                  k1=iwdtmin;      k2=iwdtmax
                elseif (ifac.eq.5 .or. ifac.eq.6) then
                  i1=icldext(1,2); i2=icldext(1,2)
                  j1=iwdtmin;      j2=iwdtmax
                  k1=iswmin;       k2=iswmax-1
                endif
                write (t12,'(2a,i3.3,a,i3.3)')
     &            facstr,'opn',NOPEN(ICFD)+1,'_s',isur
                call MKCFDBC(show,11,ifac,i1,i2,j1,j2,k1,k2,t12,
     &            SNAME(ICP,isur),dum,1,20.0,0.0,0.01,0.0,0.0,IER)
                ok=.true.

                if (ibot.lt.iwdtmin) then
                  if (ifac.eq.1 .or. ifac.eq.2) then
                    i1=isumin;       i2=isumax-1
                    j1=icldext(1,2); j2=icldext(1,2)
                    k1=ibot;         k2=iwdtmin-1
                  elseif (ifac.eq.3 .or. ifac.eq.4) then
                    i1=icldext(1,2); i2=icldext(1,2)
                    j1=isvmin;       j2=isvmax-1
                    k1=ibot;         k2=iwdtmin-1
                  elseif (ifac.eq.5 .or. ifac.eq.6) then
                    i1=icldext(1,2); i2=icldext(1,2)
                    j1=ibot;         j2=iwdtmin-1
                    k1=iswmin;       k2=iswmax-1
                  endif
                  write (t12,'(2a,i3.3,a,i3.3)')
     &              facstr,'sld',NSB(ICFD)+1,'_s',isur
                  call MKCFDBC(show,4,ifac,i1,i2,j1,j2,k1,k2,t12,
     &              SNAME(ICP,isur),dum,1,20.0,0.0,0.0,0.0,0.0,IER)
                endif

                if (itop.gt.iwdtmax) then
                  if (ifac.eq.1 .or. ifac.eq.2) then
                    i1=isumin;       i2=isumax-1
                    j1=icldext(1,2); j2=icldext(1,2)
                    k1=iwdtmax+1;    k2=itop
                  elseif (ifac.eq.3 .or. ifac.eq.4) then
                    i1=icldext(1,2); i2=icldext(1,2)
                    j1=isvmin;       j2=isvmax-1
                    k1=iwdtmax+1;    k2=itop
                  elseif (ifac.eq.5 .or. ifac.eq.6) then
                    i1=icldext(1,2); i2=icldext(1,2)
                    j1=iwdtmax+1;    j2=itop
                    k1=iswmin;       k2=iswmax-1
                  endif
                  write (t12,'(2a,i3.3,a,i3.3)')
     &              facstr,'sld',NSB(ICFD)+1,'_s',isur
                  call MKCFDBC(show,4,ifac,i1,i2,j1,j2,k1,k2,t12,
     &              SNAME(ICP,isur),dum,1,20.0,0.0,0.0,0.0,0.0,IER)
                endif

C Any space left over that needs filling with a solid bc?
                if (icols.gt.1) then
                  if (ifac.eq.1 .or. ifac.eq.2) then
                    i1=isumin;         i2=isumax-1
                    j1=icldext(1,2)+1; j2=iparext(2)-1
                    k1=ibot;           k2=itop
                  elseif (ifac.eq.3 .or. ifac.eq.4) then
                    j1=icldext(1,2)+1; j2=iparext(2)-1
                    j1=isvmin;         j2=isvmax-1
                    k1=ibot;           k2=itop
                  elseif (ifac.eq.5 .or. ifac.eq.6) then
                    j1=icldext(1,2)+1; j2=iparext(2)-1
                    j1=ibot;           j2=itop
                    k1=iswmin;         k2=iswmax-1
                  endif
                  write (t12,'(2a,i3.3,a,i3.3)')
     &              facstr,'sld',NSB(ICFD)+1,'_s',isur
                  call MKCFDBC(show,4,ifac,i1,i2,j1,j2,k1,k2,t12,
     &              SNAME(ICP,isur),dum,1,20.0,0.0,0.0,0.0,0.0,IER)
                endif
              endif
            endif
          endif
        endif

C BC(s) have been created for child(ren) and parent; return.
        goto 999
      endif

C This surface is not a parent; create BC(s).
  99  if (dothisur) then

        write(outs,'(a,i3.3,2a)')' Creating BC(s) for surface ',
     &    isur,
     & SNAME(ICP,isur)(1:lnblnk(SNAME(ICP,isur))),
     &    ' ...'
        ISHSB=0; ISHAO=0; ISHBLK=0; ISHSRC=0; IFACES=1; ISHGEO=-isur
        call redraw(IER)
        call pausems(500)
        ISHGEO=-9999

C "frame with a crack" usage tags (ibctyp 9) might be used to put a
C crack in a solid surface - in this case, switch to a default crack
C treatment (ibctyp 4).
        if (ibctyp.eq.9) ibctyp=4

C Single solid BC for the whole surface.
        if (ibctyp.eq.1) then
          if (ifac.eq.1 .or. ifac.eq.2) then
            i1=isumin;     i2=isumax-1
            j1=iparext(1); j2=iparext(2)-1
            k1=iparext(3); k2=iparext(4)-1
          elseif (ifac.eq.3 .or. ifac.eq.4) then
            i1=iparext(1); i2=iparext(2)-1
            j1=isvmin;     j2=isvmax-1
            k1=iparext(3); k2=iparext(4)-1
          elseif (ifac.eq.5 .or. ifac.eq.6) then
            i1=iparext(1); i2=iparext(2)-1
            j1=iparext(3); j2=iparext(4)-1
            k1=iswmin;     k2=iswmax-1
          endif
          write (t12,'(2a,i3.3,a,i3.3)')
     &      facstr,'sld',NSB(ICFD)+1,'_s',isur
          call MKCFDBC(show,4,ifac,i1,i2,j1,j2,k1,k2,t12,
     &      SNAME(ICP,isur),dum,1,20.0,0.0,0.0,0.0,0.0,IER)

C Single opening BC for the whole surface.
        elseif (ibctyp.eq.2) then
          if (ifac.eq.1 .or. ifac.eq.2) then
            i1=isumin;     i2=isumax-1
            j1=iparext(1); j2=iparext(2)-1
            k1=iparext(3); k2=iparext(4)-1
          elseif (ifac.eq.3 .or. ifac.eq.4) then
            i1=iparext(1); i2=iparext(2)-1
            j1=isvmin;     j2=isvmax-1
            k1=iparext(3); k2=iparext(4)-1
          elseif (ifac.eq.5 .or. ifac.eq.6) then
            i1=iparext(1); i2=iparext(2)-1
            j1=iparext(3); j2=iparext(4)-1
            k1=iswmin;     k2=iswmax-1
          endif
          write (t12,'(2a,i3.3,a,i3.3)')
     &      facstr,'opn',NOPEN(ICFD)+1,'_s',isur
          call MKCFDBC(show,11,ifac,i1,i2,j1,j2,k1,k2,t12,
     &      SNAME(ICP,isur),dum,1,20.0,0.0,0.01,0.0,0.0,IER)

C Opening BC for bottom row of cells, solid BC for the rest.
        elseif (ibctyp.eq.3) then
          if (ifac.eq.1 .or. ifac.eq.2) then
            i1=isumin;     i2=isumax-1
            j1=iparext(1); j2=iparext(2)-1
            k1=iparext(3); k2=iparext(3)
          elseif (ifac.eq.3 .or. ifac.eq.4) then
            i1=iparext(1); i2=iparext(2)-1
            j1=isvmin;     j2=isvmax-1
            k1=iparext(3); k2=iparext(3)
          elseif (ifac.eq.5 .or. ifac.eq.6) then
            i1=iparext(1); i2=iparext(2)-1
            j1=iparext(3); j2=iparext(3)
            k1=iswmin;     k2=iswmax-1
          endif
          write (t12,'(2a,i3.3,a,i3.3)')
     &      facstr,'opn',NOPEN(ICFD)+1,'_s',isur
          call MKCFDBC(show,11,ifac,i1,i2,j1,j2,k1,k2,t12,
     &      SNAME(ICP,isur),dum,1,20.0,0.0,0.01,0.0,0.0,IER)

C Is there any more of the surface for a solid BC?
          if (iparext(4)-1.gt.iparext(3)) then
            if (ifac.eq.1 .or. ifac.eq.2) then
              i1=isumin;       i2=isumax-1
              j1=iparext(1);   j2=iparext(2)-1
              k1=iparext(3)+1; k2=iparext(4)-1
            elseif (ifac.eq.3 .or. ifac.eq.4) then
              i1=iparext(1);   i2=iparext(2)-1
              j1=isvmin;       j2=isvmax-1
              k1=iparext(3)+1; k2=iparext(4)-1
            elseif (ifac.eq.5 .or. ifac.eq.6) then
              i1=iparext(1);   i2=iparext(2)-1
              j1=iparext(3)+1; j2=iparext(4)-1
              k1=iswmin;       k2=iswmax-1
            endif
            write (t12,'(2a,i3.3,a,i3.3)')
     &        facstr,'sld',NSB(ICFD)+1,'_s',isur
            call MKCFDBC(show,4,ifac,i1,i2,j1,j2,k1,k2,t12,
     &        SNAME(ICP,isur),dum,1,20.0,0.0,0.0,0.0,0.0,IER)

          endif

C Opening BC for middle row of cells, solid BCs for the rest.
        elseif (ibctyp.eq.4) then

C Find middle row.
          irows=iparext(4)-iparext(3)
          imidrow=iparext(3)+irows/2

          if (ifac.eq.1 .or. ifac.eq.2) then
            i1=isumin;          i2=isumax-1
            j1=icldext(icld,1); j2=icldext(icld,2)-1
            k1=imidrow;         k2=imidrow
          elseif (ifac.eq.3 .or. ifac.eq.4) then
            i1=icldext(icld,1); i2=icldext(icld,2)-1
            j1=isvmin;          j2=isvmax-1
            k1=imidrow;         k2=imidrow
          elseif (ifac.eq.5 .or. ifac.eq.6) then
            i1=icldext(icld,1); i2=icldext(icld,2)-1
            j1=imidrow;         j2=imidrow
            k1=iswmin;          k2=iswmax-1
          endif
          write (t12,'(2a,i3.3,a,i3.3)')
     &      facstr,'opn',NOPEN(ICFD)+1,'_s',isur
          call MKCFDBC(show,11,ifac,i1,i2,j1,j2,k1,k2,t12,
     &      SNAME(ICP,isur),dum,1,20.0,0.0,0.01,0.0,0.0,IER)

C Is there any more of the surface below for a solid BC?
          if (iparext(3).lt.imidrow) then
            if (ifac.eq.1 .or. ifac.eq.2) then
              i1=isumin;     i2=isumax-1
              j1=iparext(1); j2=iparext(2)-1
              k1=iparext(3); k2=imidrow-1
            elseif (ifac.eq.3 .or. ifac.eq.4) then
              i1=iparext(1); i2=iparext(2)-1
              j1=isvmin;     j2=isvmax-1
              k1=iparext(3); k2=imidrow-1
            elseif (ifac.eq.5 .or. ifac.eq.6) then
              i1=iparext(1); i2=iparext(2)-1
              j1=iparext(3); j2=imidrow-1
              k1=iswmin;     k2=iswmax-1
            endif
            write (t12,'(2a,i3.3,a,i3.3)')
     &        facstr,'sld',NSB(ICFD)+1,'_s',isur
            call MKCFDBC(show,4,ifac,i1,i2,j1,j2,k1,k2,t12,
     &        SNAME(ICP,isur),dum,1,20.0,0.0,0.0,0.0,0.0,IER)
          endif

C Is there any more of the surface above for a solid BC?
          if ((iparext(4)-1).gt.imidrow) then
            if (ifac.eq.1 .or. ifac.eq.2) then
              i1=isumin;     i2=isumax-1
              j1=iparext(1); j2=iparext(2)-1
              k1=imidrow+1;  k2=iparext(4)-1
            elseif (ifac.eq.3 .or. ifac.eq.4) then
              i1=iparext(1); i2=iparext(2)-1
              j1=isvmin;     j2=isvmax-1
              k1=imidrow+1;  k2=iparext(4)-1
            elseif (ifac.eq.5 .or. ifac.eq.6) then
              i1=iparext(1); i2=iparext(2)-1
              j1=imidrow+1;  j2=iparext(4)-1
              k1=iswmin;     k2=iswmax-1
            endif
            write (t12,'(2a,i3.3,a,i3.3)')
     &        facstr,'sld',NSB(ICFD)+1,'_s',isur
            call MKCFDBC(show,4,ifac,i1,i2,j1,j2,k1,k2,t12,
     &        SNAME(ICP,isur),dum,1,20.0,0.0,0.0,0.0,0.0,IER)
          endif

C Opening BC for 25% of height, at bottom edge, solid BC for the rest.
        elseif (ibctyp.eq.5) then

C Find 25% of height, rounded, at least 1 row.
          rrows=real(iparext(4)-iparext(3))
          iqtr=nint(rrows/4.0)
          if (iqtr.eq.0) iqtr=1

          if (ifac.eq.1 .or. ifac.eq.2) then
            i1=isumin;     i2=isumax-1
            j1=iparext(1); j2=iparext(2)-1
            k1=iparext(3); k2=iparext(3)+iqtr-1
          elseif (ifac.eq.3 .or. ifac.eq.4) then
            i1=iparext(1); i2=iparext(2)-1
            j1=isvmin;     j2=isvmax-1
            k1=iparext(3); k2=iparext(3)+iqtr-1
          elseif (ifac.eq.5 .or. ifac.eq.6) then
            i1=iparext(1); i2=iparext(2)-1
            j1=iparext(3); j2=iparext(3)+iqtr-1
            k1=iswmin;     k2=iswmax-1
          endif
          write (t12,'(2a,i3.3,a,i3.3)')
     &      facstr,'opn',NOPEN(ICFD)+1,'_s',isur
          call MKCFDBC(show,11,ifac,i1,i2,j1,j2,k1,k2,t12,
     &      SNAME(ICP,isur),dum,1,20.0,0.0,0.01,0.0,0.0,IER)

C Is there any more of the surface for a solid BC?
          if ((iparext(4)-1).gt.(iparext(3)+iqtr)) then
            if (ifac.eq.1 .or. ifac.eq.2) then
              i1=isumin;          i2=isumax-1
              j1=iparext(1);      j2=iparext(2)-1
              k1=iparext(3)+iqtr; k2=iparext(4)-1
            elseif (ifac.eq.3 .or. ifac.eq.4) then
              i1=iparext(1);      i2=iparext(2)-1
              j1=isvmin;          j2=isvmax-1
              k1=iparext(3)+iqtr; k2=iparext(4)-1
            elseif (ifac.eq.5 .or. ifac.eq.6) then
              i1=iparext(1);      i2=iparext(2)-1
              j1=iparext(3)+iqtr; j2=iparext(4)-1
              k1=iswmin;          k2=iswmax-1
            endif
            write (t12,'(2a,i3.3,a,i3.3)')
     &        facstr,'sld',NSB(ICFD)+1,'_s',isur
            call MKCFDBC(show,4,ifac,i1,i2,j1,j2,k1,k2,t12,
     &        SNAME(ICP,isur),dum,1,20.0,0.0,0.0,0.0,0.0,IER)
          endif

C Opening BC for 25% of width, at vertical centre line, solid BCs for
C rest.
        elseif (ibctyp.eq.6) then

C Find 25% of width, rounded, at least 1 column, on vertical centre
C line.
          icols=iparext(2)-iparext(1)
          rcols=real(icols)
          iqtr=nint(rcols/4.0)
          if (iqtr.eq.0) iqtr=1
          imidcol=iparext(1)+icols/2
          if (mod(iqtr,2).eq.0) then
            imidqtrmin=imidcol-iqtr/2
            imidqtrmax=imidcol+iqtr/2-1
          else
            imidqtrmin=imidcol-iqtr/2
            imidqtrmax=imidcol+iqtr/2
          endif

          if (ifac.eq.1 .or. ifac.eq.2) then
            i1=isumin;     i2=isumax-1
            j1=imidqtrmin; j2=imidqtrmax
            k1=iparext(3); k2=iparext(4)-1
          elseif (ifac.eq.3 .or. ifac.eq.4) then
            i1=imidqtrmin; i2=imidqtrmax
            j1=isvmin;     j2=isvmax-1
            k1=iparext(3); k2=iparext(4)-1
          elseif (ifac.eq.5 .or. ifac.eq.6) then
            i1=imidqtrmin; i2=imidqtrmax
            j1=iparext(3); j2=iparext(4)-1
            k1=iswmin;     k2=iswmax-1
          endif
          write (t12,'(2a,i3.3,a,i3.3)')
     &      facstr,'opn',NOPEN(ICFD)+1,'_s',isur
          call MKCFDBC(show,11,ifac,i1,i2,j1,j2,k1,k2,t12,
     &      SNAME(ICP,isur),dum,1,20.0,0.0,0.01,0.0,0.0,IER)

C Is there any more of the surface to the left for a solid BC?
          if ((iparext(1)).lt.imidqtrmin) then
            if (ifac.eq.1 .or. ifac.eq.2) then
              i1=isumin;     i2=isumax-1
              j1=iparext(1); j2=imidqtrmin-1
              k1=iparext(3); k2=iparext(4)-1
            elseif (ifac.eq.3 .or. ifac.eq.4) then
              i1=iparext(1); i2=imidqtrmin-1
              j1=isvmin;     j2=isvmax-1
              k1=iparext(3); k2=iparext(4)-1
            elseif (ifac.eq.5 .or. ifac.eq.6) then
              i1=iparext(1); i2=imidqtrmin-1
              j1=iparext(3); j2=iparext(4)-1
              k1=iswmin;     k2=iswmax-1
            endif
            write (t12,'(2a,i3.3,a,i3.3)')
     &        facstr,'sld',NSB(ICFD)+1,'_s',isur
            call MKCFDBC(show,4,ifac,i1,i2,j1,j2,k1,k2,t12,
     &        SNAME(ICP,isur),dum,1,20.0,0.0,0.0,0.0,0.0,IER)
          endif

C Is there any more of the surface to the right for a solid BC?
          if ((iparext(2)-1).gt.imidqtrmax) then
            if (ifac.eq.1 .or. ifac.eq.2) then
              i1=isumin;       i2=isumax-1
              j1=imidqtrmax+1; j2=iparext(2)-1
              k1=iparext(3);   k2=iparext(4)-1
            elseif (ifac.eq.3 .or. ifac.eq.4) then
              i1=imidqtrmax+1; i2=iparext(2)-1
              j1=isvmin;       j2=isvmax-1
              k1=iparext(4);   k2=iparext(4)-1
            elseif (ifac.eq.5 .or. ifac.eq.6) then
              i1=imidqtrmax+1; i2=iparext(2)-1
              j1=iparext(3);   j2=iparext(4)-1
              k1=iswmin;       k2=iswmax-1
            endif
            write (t12,'(2a,i3.3,a,i3.3)')
     &        facstr,'sld',NSB(ICFD)+1,'_s',isur
            call MKCFDBC(show,4,ifac,i1,i2,j1,j2,k1,k2,t12,
     &        SNAME(ICP,isur),dum,1,20.0,0.0,0.0,0.0,0.0,IER)
          endif

C Opening BCs for 12.5% of height, at top and bottom edges, solid BC for
C rest.
        elseif (ibctyp.eq.7) then

C Find 12.5% of height, rounded, at least 1 row.
          irows=iparext(4)-iparext(3)
          rrows=real(irows)
          i8th=nint(rrows/8.0)
          if (i8th.eq.0) i8th=1

C Is there enough space for both openings?
          ok=.true.
  90       if (irows.lt.(i8th*2)) then
            if (i8th.gt.1) then
              i8th=i8th-1
            else
              write(outs,'(3a)')' Not enough space for openings at',
     &          ' top and bottom of this sash window; only one',
     &          ' opening will be created.'
              call EDISP(IUOUT,outs)
              ok=.false.
            endif
            goto 90
          endif

C Opening at bottom edge.
          if (ifac.eq.1 .or. ifac.eq.2) then
            i1=isumin;     i2=isumax-1
            j1=iparext(1); j2=iparext(2)-1
            k1=iparext(3); k2=iparext(3)+i8th-1
          elseif (ifac.eq.3 .or. ifac.eq.4) then
            i1=iparext(1); i2=iparext(2)-1
            j1=isvmin;     j2=isvmax-1
            k1=iparext(3); k2=iparext(3)+i8th-1
          elseif (ifac.eq.5 .or. ifac.eq.6) then
            i1=iparext(1); i2=iparext(2)-1
            j1=iparext(3); j2=iparext(3)+i8th-1
            k1=iswmin;     k2=iswmax-1
          endif
          write (t12,'(2a,i3.3,a,i3.3)')
     &      facstr,'opn',NOPEN(ICFD)+1,'_s',isur
          call MKCFDBC(show,11,ifac,i1,i2,j1,j2,k1,k2,t12,
     &      SNAME(ICP,isur),dum,1,20.0,0.0,0.01,0.0,0.0,IER)

C Is there room for the top opening?
          if (ok) then
            if (ifac.eq.1 .or. ifac.eq.2) then
              i1=isumin;          i2=isumax-1
              j1=iparext(1);      j2=iparext(2)-1
              k1=iparext(4)-i8th; k2=iparext(4)-1
            elseif (ifac.eq.3 .or. ifac.eq.4) then
              i1=iparext(1);      i2=iparext(2)-1
              j1=isvmin;          j2=isvmax-1
              k1=iparext(4)-i8th; k2=iparext(4)-1
            elseif (ifac.eq.5 .or. ifac.eq.6) then
              i1=iparext(1);      i2=iparext(2)-1
              j1=iparext(4)-i8th; j2=iparext(4)-1
              k1=iswmin;          k2=iswmax-1
            endif
            itop=iparext(4)-i8th-1
            write (t12,'(2a,i3.3,a,i3.3)')
     &        facstr,'opn',NOPEN(ICFD)+1,'_s',isur
            call MKCFDBC(show,11,ifac,i1,i2,j1,j2,k1,k2,t12,
     &        SNAME(ICP,isur),dum,1,
     &        20.0,0.0,0.01,0.0,0.0,IER)
          else
            itop=iparext(4)-1
          endif

C Is there any more of the surface for a solid BC?
          if ((iparext(3)+i8th).le.itop) then
            if (ifac.eq.1 .or. ifac.eq.2) then
              i1=isumin;          i2=isumax-1
              j1=iparext(1);      j2=iparext(2)-1
              k1=iparext(3)+i8th; k2=itop
            elseif (ifac.eq.3 .or. ifac.eq.4) then
              i1=iparext(1);      i2=iparext(2)-1
              j1=isvmin;          j2=isvmax-1
              k1=iparext(3)+i8th; k2=itop
            elseif (ifac.eq.5 .or. ifac.eq.6) then
              i1=iparext(1);      i2=iparext(2)-1
              j1=iparext(3)+i8th; j2=itop
              k1=iswmin;          k2=iswmax-1
            endif
            write (t12,'(2a,i3.3,a,i3.3)')
     &        facstr,'sld',NSB(ICFD)+1,'_s',isur
            call MKCFDBC(show,4,ifac,i1,i2,j1,j2,k1,k2,t12,
     &        SNAME(ICP,isur),dum,1,20.0,0.0,0.0,0.0,0.0,IER)
          endif

C Opening BC for 75% of height and width, at bottom edge, solid BCs for
C rest.            
        elseif (ibctyp.eq.8) then

C Find 75% of height, rounded, at least 1 row, at bottom edge.
          irows=iparext(4)-iparext(3)
          rrows=real(irows)
          i3qtrhgt=nint(3.0*rrows/4.0)
          if (i3qtrhgt.eq.0) i3qtrhgt=1
          ibtm3qtrhgtmin=iparext(3)
          ibtm3qtrhgtmax=iparext(3)+i3qtrhgt-1

C Find 75% of width, rounded, at least 1 column, on vertical centre
C line.
          icols=iparext(2)-iparext(1)
          rcols=real(icols)
          i3qtrwdt=nint(3.0*rcols/4.0)
          if (i3qtrwdt.eq.0) i3qtrwdt=1
          imidcol=iparext(1)+icols/2
          if (mod(i3qtrwdt,2).eq.0) then
            imid3qtrwdtmin=imidcol-i3qtrwdt/2
            imid3qtrwdtmax=imidcol+i3qtrwdt/2-1
          else
            imid3qtrwdtmin=imidcol-i3qtrwdt/2
            imid3qtrwdtmax=imidcol+i3qtrwdt/2
          endif

C Opening BC.
          if (ifac.eq.1 .or. ifac.eq.2) then
            i1=isumin;         i2=isumax-1
            j1=imid3qtrwdtmin; j2=imid3qtrwdtmax
            k1=ibtm3qtrhgtmin; k2=ibtm3qtrhgtmax
          elseif (ifac.eq.3 .or. ifac.eq.4) then
            i1=imid3qtrwdtmin; i2=imid3qtrwdtmax
            j1=isvmin;         j2=isvmax-1
            k1=ibtm3qtrhgtmin; k2=ibtm3qtrhgtmax
          elseif (ifac.eq.5 .or. ifac.eq.6) then
            i1=imid3qtrwdtmin; i2=imid3qtrwdtmax
            j1=ibtm3qtrhgtmin; j2=ibtm3qtrhgtmax
            k1=iswmin;         k2=iswmax-1
          endif
          write (t12,'(2a,i3.3,a,i3.3)')
     &      facstr,'opn',NOPEN(ICFD)+1,'_s',isur
          call MKCFDBC(show,11,ifac,i1,i2,j1,j2,k1,k2,t12,
     &      SNAME(ICP,isur),dum,1,20.0,0.0,0.01,0.0,0.0,IER)

C Is there room left at the top?
          if (ibtm3qtrhgtmax.lt.(iparext(4)-1)) then
            if (ifac.eq.1 .or. ifac.eq.2) then
              i1=isumin;           i2=isumax-1
              j1=iparext(1);       j2=iparext(2)-1
              k1=ibtm3qtrhgtmax+1; k2=iparext(4)-1
            elseif (ifac.eq.3 .or. ifac.eq.4) then
              i1=iparext(1);       i2=iparext(2)-1
              j1=isvmin;           j2=isvmax-1
              k1=ibtm3qtrhgtmax+1; k2=iparext(4)-1
            elseif (ifac.eq.5 .or. ifac.eq.6) then
              i1=iparext(1);       i2=iparext(2)-1
              j1=ibtm3qtrhgtmax+1; j2=iparext(4)-1
              k1=iswmin;           k2=iswmax-1
            endif
            write (t12,'(2a,i3.3,a,i3.3)')
     &        facstr,'sld',NSB(ICFD)+1,'_s',isur
            call MKCFDBC(show,4,ifac,i1,i2,j1,j2,k1,k2,t12,
     &        SNAME(ICP,isur),dum,1,20.0,0.0,0.0,0.0,0.0,IER)
            ok=.true.
            itophgt=(k2-k1+1)
          else
            ok=.false.
          endif

C Is there room at the left?
          if (imid3qtrwdtmin.gt.iparext(1)) then
            if (ok) then
              itop=iparext(4)-1-itophgt
            else
              itop=iparext(4)-1
            endif
            if (ifac.eq.1 .or. ifac.eq.2) then
              i1=isumin;     i2=isumax-1
              j1=iparext(1); j2=imid3qtrwdtmin-1
              k1=iparext(3); k2=itop
            elseif (ifac.eq.3 .or. ifac.eq.4) then
              i1=iparext(1); i2=imid3qtrwdtmin-1
              j1=isvmin;     j2=isvmax-1
              k1=iparext(3); k2=itop
            elseif (ifac.eq.5 .or. ifac.eq.6) then
              i1=iparext(1); i2=imid3qtrwdtmin-1
              j1=iparext(3); j2=itop
              k1=iswmin;     k2=iswmax-1
            endif
            write (t12,'(2a,i3.3,a,i3.3)')
     &        facstr,'sld',NSB(ICFD)+1,'_s',isur
            call MKCFDBC(show,4,ifac,i1,i2,j1,j2,k1,k2,t12,
     &        SNAME(ICP,isur),dum,1,20.0,0.0,0.0,0.0,0.0,IER)
          endif

C Is there room at the right?
          if (imid3qtrwdtmax.lt.(iparext(2)-1)) then
            if (ok) then
              itop=iparext(4)-1-itophgt
            else
              itop=iparext(4)-1
            endif
            if (ifac.eq.1 .or. ifac.eq.2) then
              i1=isumin;           i2=isumax-1
              j1=imid3qtrwdtmax+1; j2=iparext(2)-1
              k1=iparext(3);       k2=itop
            elseif (ifac.eq.3 .or. ifac.eq.4) then
              i1=imid3qtrwdtmax+1; i2=iparext(2)-1
              j1=isvmin;           j2=isvmax-1
              k1=iparext(3);       k2=itop
            elseif (ifac.eq.5 .or. ifac.eq.6) then
              i1=imid3qtrwdtmax+1; i2=iparext(2)-1
              j1=iparext(3);       j2=itop
              k1=iswmin;           k2=iswmax-1
            endif
            write (t12,'(2a,i3.3,a,i3.3)')
     &        facstr,'sld',NSB(ICFD)+1,'_s',isur
            call MKCFDBC(show,4,ifac,i1,i2,j1,j2,k1,k2,t12,
     &        SNAME(ICP,isur),dum,1,20.0,0.0,0.0,0.0,0.0,IER)
          endif

C 9 and 10 are for frames.
        elseif (ibctyp.eq.9 .or. ibctyp.eq.10) then
          write(outs,'(a)')
     &' This surface is a frame, but has no children. Check usage tags.'
          call EDISP(IUOUT,outs)
        endif
      endif

  999 if (ISTAT.gt.0 .and. ibctyp.gt.1) doafnasc=.false.

      RETURN
      END


C ******************** EDAIRO ********************
C Controls the editing of CFD air flow opening boundary variables.
C IER=0 indicates no error.

      SUBROUTINE EDAIRO(IAO,IER)
#include "building.h"
#include "cfd.h"
#include "help.h"

      COMMON/ICFNOD/ICFD,ICP
      COMMON/SPAD/MMOD,LIMIT,LIMTTY

      common/cfdconf/ICFBLD(MNZ),ICFMFS(MNZ)
      common/KEYVOLS/NVOL(MNZ),IVOLF(MNVLS,MNZ),IVCELLS(MNVLS,MNZ,2),
     &               JVCELLS(MNVLS,MNZ,2),KVCELLS(MNVLS,MNZ,2)
      common/KEYVOLN/VOLNAME(MNVLS,MNZ),VCsurf(MNVLS,MNZ),
     &               BLKSURF(MNVLS,MNZ,6)
      common/KEYVDAT/IVTYPE(MNVLS,MNZ),VOLTemp(MNVLS,MNZ),
     &          VOLHeat(MNVLS,MNZ),IVConfl(MNVLS,MNZ),VOLHum(MNVLS,MNZ),
     &          VOLCO2(MNVLS,MNZ),VOLVel(MNVLS,MNZ),VOLDir(MNVLS,MNZ,2),
     &          VOLArea(MNVLS,MNZ),VOLPres(MNVLS,MNZ),
     &          VOLPol(MCTM,MNVLS,MNZ)

      COMMON/NDMAP/NOPEN(MNZ),MFNODE(MCFND,MNZ),IOPENi(MCFND,MNZ),
     &             IOPENf(MCFND,MNZ),JOPENi(MCFND,MNZ),
     &             JOPENf(MCFND,MNZ),KOPENi(MCFND,MNZ),
     &             KOPENf(MCFND,MNZ),FIXM(MCFND,MNZ),
     &             FIXT(MCFND,MNZ),FIXC(MCFND,MNZ),
     &             FIXK(MCFND,MNZ),FIXE(MCFND,MNZ),
     &             IWOPEN(MCFND,MNZ),ICFDCN(MCFND,MNZ),
     &             ICNACT(MCFND,MNZ),IVOLNOP(MCFND,MNZ)
      COMMON/CFDVIS/HAS_GEOM,ISHSB,ISHAO,IFACES,ISHBLK,ISHSRC,ISHGEO,
     &              INITD
      logical HAS_GEOM
      character INITD*6

      character ITMSS(13)*50
      character face*6,ltmp*17,t12*12,d12*12
      character VOLNAME*12, VCsurf*12, BLKSURF*12,t20*20
      integer IWO  ! for radio button
      integer NITMS,INO ! max items and current menu item

      helpinsub='edcfd'  ! set for subroutine

C Adjust each cell by -1 as dfs adds fictitious cells around the domain.
      Ii=IVCELLS(IAO,ICFD,1)-1; Iif=IVCELLS(IAO,ICFD,2)-1
      Ji=JVCELLS(IAO,ICFD,1)-1; Jf=JVCELLS(IAO,ICFD,2)-1
      Ki=KVCELLS(IAO,ICFD,1)-1; Kf=KVCELLS(IAO,ICFD,2)-1

C Check if new opening.
      if (Ii.lt.0.or.Iif.lt.0.or.
     &    Ji.lt.0.or.Jf.lt.0.or.
     &    Ki.lt.0.or.Kf.lt.0) then 
        t20='Air opening'
        if(IVOLF(IAO,ICFD).ne.0)IFACE=IVOLF(IAO,ICFD)
        call PIKCELS(Ii,Iif,Ji,Jf,Ki,Kf,IFACE,IER)
        IVOLF(IAO,ICFD)=IFACE
        IVCELLS(IAO,ICFD,1)=Ii+1; IVCELLS(IAO,ICFD,2)=Iif+1
        JVCELLS(IAO,ICFD,1)=Ji+1; JVCELLS(IAO,ICFD,2)=Jf+1
        KVCELLS(IAO,ICFD,1)=Ki+1; KVCELLS(IAO,ICFD,2)=Kf+1
        call NEW2OLD
      endif

C Generate text for location index
 5    FACE='  '
      if (IVOLF(IAO,ICFD).eq.1) then
        FACE='West'
      elseif (IVOLF(IAO,ICFD).eq.2) then
        FACE='East'
      elseif (IVOLF(IAO,ICFD).eq.3) then
        FACE='South'
      elseif (IVOLF(IAO,ICFD).eq.4) then
        FACE='North'
      elseif (IVOLF(IAO,ICFD).eq.5) then
        FACE='Low'
      elseif (IVOLF(IAO,ICFD).eq.6) then
        FACE='High'
      elseif (IVOLF(IAO,ICFD).eq.7) then
        FACE='Whole'
      elseif (IVOLF(IAO,ICFD).eq.8) then
        FACE='Block'
      elseif (IVOLF(IAO,ICFD).eq.9) then
        FACE='Source'
      endif

C Set boundary type string.
      ltmp='  '
      if (IVTYPE(IAO,ICFD).eq.10) then
        ltmp='Pressure'
      elseif (IVTYPE(IAO,ICFD).eq.11) then
        ltmp='Velocity'
      elseif (IVTYPE(IAO,ICFD).eq.12) then
        ltmp='Zero gradient'
      elseif (IVTYPE(IAO,ICFD).eq.13) then
        ltmp='Mass flow'
      endif
      IF (ICFMFS(ICFD).EQ.1)THEN
        LTMP='from flow network'
      ENDIF

C Create a menu showing definitions for different conflation schemes.
      write (ITMSS(1),'(a,a)')   'a name: ',VOLNAME(IAO,ICFD)
      write (ITMSS(2),'(a,a)')   'b type: ',ltmp
      write (ITMSS(3),'(a)')     ' -----------------------------'
      write (ITMSS(4),'(a,a)')   '         Is  If  Js  Jf  Ks  Kf '
      write (ITMSS(5),'(a,6(i3,1x))') 'c cells:',Ii,Iif,Ji,Jf,Ki,Kf
      write (ITMSS(7),'(a)')     ' -----------------------------'
      if (IVTYPE(IAO,ICFD).eq.10) then
        write(ITMSS(6),'(a)')   '  '
        write(ITMSS(8),'(a,f6.2)')'e pressure:',VOLPres(IAO,ICFD)
      elseif (IVTYPE(IAO,ICFD).eq.11) then
        write(ITMSS(6),'(a,a)')   '  face: ',FACE
        write(ITMSS(8),'(a,f6.2,a,2f6.2)')'e mass fow rate: ',
     &      VOLVel(IAO,ICFD),', direction: ',(VOLDir(IAO,ICFD,I),I=1,2)
      elseif (IVTYPE(IAO,ICFD).eq.12) then
        write(ITMSS(6),'(a,a)')   '  face: ',FACE
        write(ITMSS(8),'(a)')   '  '
      elseif (IVTYPE(IAO,ICFD).eq.13) then
        write(ITMSS(6),'(a)')   '  '
        write(ITMSS(8),'(a,f6.2)')'e mass flow rate: ',VOLVel(IAO,ICFD)
      endif
      write(ITMSS(9),'(a,f6.2)')'f temperature: ',VOLTemp(IAO,ICFD)
      if (VOLArea(IAO,ICFD).gt.0.) then
        write(ITMSS(10),'(a,f6.2)')'g real area: ',VOLArea(IAO,ICFD)
      else
        write(ITMSS(10),'(a)')'g real area: cell area'
      endif
      ITMSS(11)=' -----------------------------'
      ITMSS(12)='? help'
      ITMSS(13)='- exit menu'
      NITMS=13

      IF(MMOD.EQ.8)THEN
C Find index in old commons of opening specified by index IAO in new commons
        do 638 I=1,NOPEN(ICFD)
          if (IAO.eq.IVOLNOP(I,ICFD)) then
            ISHAO_TMP=ISHAO
            ISHGEO_TMP=ISHGEO
            ISHAO=I
            ISHGEO=0
            CALL redraw(IER)
            ISHAO=ISHAO_TMP
            ISHGEO=ISHGEO_TMP
          endif
638     continue
      ENDIF

C Help text for this menu.
      helptopic='cfd_air_boundary'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Display menu.
      INO=-1
      CALL EMENU('Air flow opening edit',ITMSS,NITMS,INO)

      if (INO.eq.1) then
        t12=VOLNAME(IAO,ICFD)
        d12='  '
        call EASKS(t12,' ','Opening name?',12,d12,'open name',
     &    IER,nbhelp)
        if (IER.eq.0) then
          call st2name(t12,d12)
          VOLNAME(IAO,ICFD)=d12
        endif
      elseif (INO.eq.2) then

C Choose an opening type.
        IWO=1

C Zero gradient BCs do not work, disabled in the interface for the time
C being.
C        CALL EASKATOG(' ','Opening types:','pressure',
C     &    'velocity','zero gradient','from flow network',
C     &    'cancel',' ',' ',IWO,nbhelp)
        CALL EASKATOG(' ','Opening types:','pressure','velocity',
     &    'from flow network','cancel',' ',' ',' ',IWO,nbhelp)
        if (iwo.eq.3) then
          iwo=4
        elseif (iwo.eq.4) then
          iwo=5
        endif
        
        IF(IWO.EQ.4)THEN
          ICFMFS(ICFD)=1
          CALL MFCONF(ICFD)
        ENDIF

C Set BC type.
        if (IWO.ge.1.AND.IWO.le.3) THEN
          IVTYPE(IAO,ICFD)=IWO+9
          ICFMFS(ICFD)=0
        ENDIF

C Reset location to whole of volume for non-face choices.
        if (IWO.eq.1.OR.IWO.eq.3) then
          IVOLF(IAO,ICFD)=7
        else
          t20='Air opening'
          call PIKFACE(t20,VOLNAME(IAO,ICFD),Ii,Iif,Ji,Jf,Ki,Kf,
     &      IVOLF(IAO,ICFD),IER)
        endif
      elseif (INO.eq.5) then
        t20='Air opening'
        if(IVOLF(IAO,ICFD).ne.0)IFACE=IVOLF(IAO,ICFD)
        call PIKCELS(Ii,Iif,Ji,Jf,Ki,Kf,IFACE,IER)
        IVOLF(IAO,ICFD)=IFACE
        IVCELLS(IAO,ICFD,1)=Ii+1; IVCELLS(IAO,ICFD,2)=Iif+1
        JVCELLS(IAO,ICFD,1)=Ji+1; JVCELLS(IAO,ICFD,2)=Jf+1
        KVCELLS(IAO,ICFD,1)=Ki+1; KVCELLS(IAO,ICFD,2)=Kf+1
        call NEW2OLD
      elseif (INO.eq.8) then
        if (IVTYPE(IAO,ICFD).eq.10) then
          VAL=VOLPres(IAO,ICFD)
          call EASKR(VAL,' ','Cell pressure (Pa)?',
     &      0.,'W',10000.,'W',50.0,'pressure',IER,nbhelp)
          if (IER.eq.0) VOLPres(IAO,ICFD)=VAL
        elseif (IVTYPE(IAO,ICFD).eq.11) then
          VAL=VOLVel(IAO,ICFD)
          call EASKR(VAL,' ',
     &      'Mass flow rate (kg/s)?',
     &       -50.,'W',50.,'W',0.5,'flow rate',IER,nbhelp)
          if (IER.eq.0) VOLVel(IAO,ICFD)=VAL
        elseif (IVTYPE(IAO,ICFD).eq.13) then
          VAL=VOLVel(IAO,ICFD)
          call EASKR(VAL,' ','Mass flow rate entering cells (kg/s)?',
     &      -10.,'W',10.,'W',0.02,'mass flow',IER,nbhelp)
          if (IER.eq.0) VOLVel(IAO,ICFD)=VAL
        endif
      elseif (INO.eq.9) then
        VAL=VOLTemp(IAO,ICFD)
        call EASKR(VAL,' ','Temperature of air entering domain (C)?',
     &     -50.,'W',50.,'W',20.,'temperature',IER,nbhelp)
        if (IER.eq.0) VOLTemp(IAO,ICFD)=VAL
      elseif (INO.eq.9) then
        VAL=VOLArea(IAO,ICFD)
        call EASKR(VAL,' ','Real area of opening?',
     &    -0.1,'W',50.,'W',0.,'area',IER,nbhelp)
        if (IER.eq.0) VOLArea(IAO,ICFD)=VAL
      elseif (INO.eq.(NITMS-1)) then

C Help.
        helptopic='cfd_air_boundary'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('boundary conditions',nbhelp,'-',0,0,IER)
      elseif (INO.eq.NITMS) then

C Return.
        return
      endif

      goto 5
      end

C ******************** MFCONF ********************
C Defines conflation of dfs and mfs.

      SUBROUTINE MFCONF(ICFD)
#include "building.h"
#include "cfd.h"
#include "net_flow.h"
#include "help.h"

      common/KEYVOLS/NVOL(MNZ),IVOLF(MNVLS,MNZ),IVCELLS(MNVLS,MNZ,2),
     &               JVCELLS(MNVLS,MNZ,2),KVCELLS(MNVLS,MNZ,2)
      common/KEYVOLN/VOLNAME(MNVLS,MNZ),VCsurf(MNVLS,MNZ),
     &               BLKSURF(MNVLS,MNZ,6)
      common/KEYVDAT/IVTYPE(MNVLS,MNZ),VOLTemp(MNVLS,MNZ),
     &          VOLHeat(MNVLS,MNZ),IVConfl(MNVLS,MNZ),VOLHum(MNVLS,MNZ),
     &          VOLCO2(MNVLS,MNZ),VOLVel(MNVLS,MNZ),VOLDir(MNVLS,MNZ,2),
     &          VOLArea(MNVLS,MNZ),VOLPres(MNVLS,MNZ),
     &          VOLPol(MCTM,MNVLS,MNZ)
      COMMON/CFDMFS1/ICFDNOD(MNZ),ICFDCNN(MNVLS,MNZ)
      DIMENSION ICPK(MCNN),INPK(MNOD)

      character VOLNAME*12, VCsurf*12, BLKSURF*12
      CHARACTER PROMPT1*72, PROMPT2*72

      helpinsub='edcfd'  ! set for subroutine

C Select mfs node representative of CFD domain
      helptopic='cfd_mass_flow_node'
      call gethelptext(helpinsub,helptopic,nbhelp)
      INOPT=0
      NNPK=1
      PROMPT1=' '
      PROMPT2='Node representing CFD domain?'
      call ASKMFNOD(INOPT,NNPK,INPK,PROMPT1,PROMPT2,nbhelp)
      if (NNPK.gt.0.and.INPK(1).gt.0) then
        ICFDNOD(ICFD)=INPK(1)
      endif

C Select mfs connections representative of all CFD openings
      INODE=INPK(1)
      DO 100 I=1,NVOL(ICFD)
        IF(IVTYPE(I,ICFD).GE.10.AND.IVTYPE(I,ICFD).LE.13)THEN
          WRITE(PROMPT1,'(2A)')
     &    'Connection represented by opening ',VOLNAME(I,ICFD)
          CALL ASKMFCON(INODE,NNPK,ICPK,PROMPT1,PROMPT2)
          if (NNPK.gt.0.and.ICPK(1).gt.0) then
            ICFDCNN(I,ICFD)=ICPK(1)
          endif
        ENDIF
 100  CONTINUE

      RETURN
      END


C ******************** EDSLDB ********************
C Controls the editing of CFD solid boundary variables.
C IER=0 indicates no error.

      SUBROUTINE EDSLDB(IZONE,ISB,IER)
#include "building.h"
#include "geometry.h"
#include "cfd.h"
#include "help.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/ICFNOD/ICFD,ICP
      COMMON/GEOM/XP(ntcelx),YP(ntcely),ZP(ntcelz),
     1            DXEP(ntcelx),DXPW(ntcelx),DYNP(ntcely),DYPS(ntcely),
     2            DZHP(ntcelz),DZPL(ntcelz),
     3            SEW(ntcelx),SNS(ntcely),SHL(ntcelz),
     4            XU(ntcelx),YV(ntcely),ZW(ntcelz)

      common/METHDS/ITURB(MNZ),IBUOY(MNZ)
      common/KEYVOLS/NVOL(MNZ),IVOLF(MNVLS,MNZ),IVCELLS(MNVLS,MNZ,2),
     &               JVCELLS(MNVLS,MNZ,2),KVCELLS(MNVLS,MNZ,2)
      common/KEYVOLN/VOLNAME(MNVLS,MNZ),VCsurf(MNVLS,MNZ),
     &               BLKSURF(MNVLS,MNZ,6)
      common/KEYVDAT/IVTYPE(MNVLS,MNZ),VOLTemp(MNVLS,MNZ),
     &          VOLHeat(MNVLS,MNZ),IVConfl(MNVLS,MNZ),VOLHum(MNVLS,MNZ),
     &          VOLCO2(MNVLS,MNZ),VOLVel(MNVLS,MNZ),VOLDir(MNVLS,MNZ,2),
     &          VOLArea(MNVLS,MNZ),VOLPres(MNVLS,MNZ),
     &          VOLPol(MCTM,MNVLS,MNZ)
      COMMON/CFDVIS/HAS_GEOM,ISHSB,ISHAO,IFACES,ISHBLK,ISHSRC,ISHGEO,
     &              INITD
      logical HAS_GEOM
      character INITD*6

      COMMON/Sbdary/NSB(MNZ),ISBi(MNSBZ,MNZ),ISBf(MNSBZ,MNZ),
     &              JSBi(MNSBZ,MNZ),JSBf(MNSBZ,MNZ),
     &              KSBi(MNSBZ,MNZ),KSBf(MNSBZ,MNZ),
     &              ISUFLC(MNSBZ,MNZ),IWSB(MNSBZ,MNZ),SSB(MNSBZ,MNZ),
     &              SSBHC(MNSBZ,MNZ),IVOLNSB(MNSBZ,MNZ),
     &              ITCtype(MNSBZ,MNZ),icTREF(MNSBZ,MNZ)

      character ITMSS(13)*40
      character face*6,ltmp*16,t12*12,d12*12,outs*124
      character VOLNAME*12, VCsurf*12, BLKSURF*12
      CHARACTER t20*20
      integer IWO  ! for radio button
      integer NITMS,INO ! max items and current menu item

      helpinsub='edcfd'  ! set for subroutine

C Adjust each cell by -1 as dfs adds fictitious cells around the domain.
      Ii=IVCELLS(ISB,ICFD,1)-1; Iif=IVCELLS(ISB,ICFD,2)-1
      Ji=JVCELLS(ISB,ICFD,1)-1; Jf=JVCELLS(ISB,ICFD,2)-1
      Ki=KVCELLS(ISB,ICFD,1)-1; Kf=KVCELLS(ISB,ICFD,2)-1
      itrc=0   ! silent feedback initially

C Check if new solid boundary.
      if (Ii.lt.0.or.Iif.lt.0.or.
     &    Ji.lt.0.or.Jf.lt.0.or.
     &    Ki.lt.0.or.Kf.lt.0) then 
        t20='Solid boundary'
        if(IVOLF(ISB,ICFD).ne.0)IFACE=IVOLF(ISB,ICFD)
        call PIKCELS(Ii,Iif,Ji,Jf,Ki,Kf,IFACE,IER)
        IVOLF(ISB,ICFD)=IFACE
        IVCELLS(ISB,ICFD,1)=Ii+1; IVCELLS(ISB,ICFD,2)=Iif+1
        JVCELLS(ISB,ICFD,1)=Ji+1; JVCELLS(ISB,ICFD,2)=Jf+1
        KVCELLS(ISB,ICFD,1)=Ki+1; KVCELLS(ISB,ICFD,2)=Kf+1
        call NEW2OLD
      endif

C Generate text for location index
 5    FACE='  '
      if (IVOLF(ISB,ICFD).eq.1) then
        FACE='West'
      elseif (IVOLF(ISB,ICFD).eq.2) then
        FACE='East'
      elseif (IVOLF(ISB,ICFD).eq.3) then
        FACE='South'
      elseif (IVOLF(ISB,ICFD).eq.4) then
        FACE='North'
      elseif (IVOLF(ISB,ICFD).eq.5) then
        FACE='Low'
      elseif (IVOLF(ISB,ICFD).eq.6) then
        FACE='High'
      elseif (IVOLF(ISB,ICFD).eq.7) then
        FACE='Whole'
      elseif (IVOLF(ISB,ICFD).eq.8) then
        FACE='Block'
      elseif (IVOLF(ISB,ICFD).eq.9) then
        FACE='Source'
      endif

C Set boundary type string.
      ltmp='  '
      if (IVTYPE(ISB,ICFD).eq.1) then
        ltmp='Temperature'
      elseif (IVTYPE(ISB,ICFD).eq.2) then
        ltmp='Heat flux'
      elseif (IVTYPE(ISB,ICFD).eq.3) then
        ltmp='Symmetrical'
      elseif (IVTYPE(ISB,ICFD).eq.4) then
        ltmp='Conflated  '
      elseif (IVTYPE(ISB,ICFD).eq.5) then
        ltmp='Conflated  '
      elseif (IVTYPE(ISB,ICFD).eq.6) then
        ltmp='Conflated  '
      endif

C Create a menu showing definitions for different conflation schemes.
      write (ITMSS(1),'(a,a)')   'a name: ',VOLNAME(ISB,ICFD)
      write (ITMSS(2),'(a,a)')   'b type: ',ltmp
      write (ITMSS(3),'(a)')     ' -----------------------------'
      write (ITMSS(4),'(a,a)')   '         Is  If  Js  Jf  Ks  Kf '
      write (ITMSS(5),'(a,6(i3,1x))') 'c cells:',Ii,Iif,Ji,Jf,Ki,Kf

C If not silent echo some XYZ coordinates in space. Need to offset
C indicies to get correct position.
C      write(6,*) 'XU Ii Ii+1 If If+1 If+2 If+3 ',Ii,Iif,
C     &  XU(Ii),XU(Ii+1),XU(Iif),XU(Iif+1),XU(Iif+2),XU(Iif+3)
C      write(6,*) 'XP Ii Ii+1 If If+1 If+2 ',
C     &  XP(Ii),XP(Ii+1),XP(Iif),XP(Iif+1),XP(Iif+2)
C      write(6,*) 'DXEP Ii Ii+1 If If+1 If+2 If+3',Ii,Iif,
C     &  DXEP(Ii),DXEP(Ii+1),DXEP(Iif),DXEP(Iif+1),DXEP(Iif+2)
      call edisp(iuout,
     &  'Cell X start & end     Y start & end     Z start & end')
      write(outs,'(6f10.4)') XU(Ii+1),XU(Iif+2),YV(Ji+1),YV(Jf+2),
     &  ZW(Ki+1),ZW(Kf+2)
      call edisp(iuout,outs)
      write (ITMSS(6),'(a,a)')   '  face: ',FACE
      write (ITMSS(7),'(a)')     ' -----------------------------'
      if (IVTYPE(ISB,ICFD).eq.1) then
        write(ITMSS(8),'(a,f6.2)')'e temperature: ',
     &    VOLTemp(ISB,ICFD)
        write(ITMSS(9),'(a)')     '  '
      elseif (IVTYPE(ISB,ICFD).eq.2) then
        write(ITMSS(8),'(a,f7.0)')'e heat flux: ',VOLHeat(ISB,ICFD)
        write(ITMSS(9),'(a)')     '  '
      elseif (IVTYPE(ISB,ICFD).eq.3) then
        write(ITMSS(8),'(a)')   '  '
        write(ITMSS(9),'(a)')   '  '
      elseif (IVTYPE(ISB,ICFD).eq.4) then
        write(ITMSS(8),'(a,a)')'e conflated to surface: ',
     &                                              VCsurf(ISB,ICFD)
        ivcfl=IVConfl(ISB,ICFD)
        if(ivcfl.eq.0)then
          write(ITMSS(9),'(a)')'f handshaking: not yet defined'
        elseif(ivcfl.eq.1)then
          write(ITMSS(9),'(a)')'f handshaking: one-way log-law CFD'
        elseif(ivcfl.eq.3)then
          write(ITMSS(9),'(a)')'f handshaking: one-way Yuan    CFD'
        elseif(ivcfl.eq.4)then
          write(ITMSS(9),'(a)')'f handshaking: one-way Yuan   BSim'
        elseif(ivcfl.eq.5)then
          write(ITMSS(9),'(a)')'f handshaking: one-way Yuan CFD avg'
        elseif(ivcfl.eq.6)then
          write(ITMSS(9),'(a)')'f handshaking: one-way log-law BSim'
        elseif(ivcfl.eq.7)then
          write(ITMSS(9),'(a)')'f handshaking: one-way log-law CFD avg'
        elseif(ivcfl.eq.8)then
          write(ITMSS(9),'(a)')'f handshaking: one-way log-law CFD loc'
        elseif(ivcfl.eq.9)then
          write(ITMSS(9),'(a)')'f handshaking: two-way log-law CFD'
        elseif(ivcfl.eq.11)then
          write(ITMSS(9),'(a)')'f handshaking: two-way Yuan CFD'
        elseif(ivcfl.eq.12)then
          write(ITMSS(9),'(a)')'f handshaking: to-way Yuan CFD loc'
        elseif(ivcfl.eq.13)then
          write(ITMSS(9),'(a)')'f handshaking: two-way log-law CFD'
        elseif(ivcfl.eq.14)then
          write(ITMSS(9),'(a)')'f handshaking: two-way log-law local'
        else
          write(ITMSS(9),'(a,i4)')'f handshaking: UNKNOWN type',ivcfl
        endif
C        write(ITMSS(9),'(a,i4)')'f handshaking: ',IVConfl(ISB,ICFD)
      endif
      ITMSS(10) =' -----------------------------'
      if(itrc.eq.0)then
        ITMSS(11) ='> feedback silent             '
      elseif(itrc.eq.1)then
        ITMSS(11) ='> feedback brief              '
      elseif(itrc.eq.2)then
        ITMSS(11) ='> feedback verbose            '
      endif
      
      ITMSS(12)='? help'
      ITMSS(13)='- exit menu'
      NITMS=13

      IF(MMOD.EQ.8)THEN
C Find index in old commons of boundary specified by index ISB in new commons.
        do 195 I=1,NSB(ICFD)
          if (ISB.eq.IVOLNSB(I,ICFD)) then
            ISHSB_TMP=ISHSB
            ISHGEO_TMP=ISHGEO
            ISHSB=I
            ISHGEO=0
            CALL redraw(IER)
            ISHSB=ISHSB_TMP
            ISHGEO=ISHGEO_TMP
          endif
195     continue
      ENDIF

C Help text for this menu.
      helptopic='cfd_solid_boundary_edit'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Display menu.
      INO=-1
      CALL EMENU('Solid boundary edit',ITMSS,NITMS,INO)
      if (INO.eq.1) then

C Get boundary name.
        t12=VOLNAME(ISB,ICFD)
        d12='  '
        call EASKS(t12,' ','Boundary name?',12,d12,'bnd name',
     &    IER,nbhelp)
        if (IER.eq.0) then
          call st2name(t12,d12)
          VOLNAME(ISB,ICFD)=d12
        endif
      elseif (INO.eq.2) then

C Choose an opening type.
        IWO=1
        CALL EASKATOG(' ','Boundary type:','temperature','heat flux',
     &    'symmetrical','building surface','cancel','  ','  ',
     &    IWO,nbhelp)

C Remember non-conflated option if building surface chosen.
        if (IWO.eq.4) then
          if (ITURB(ICFD).eq.0) then
            call usrmsg('Building surface boundary conditions are',
     &                'applicable only in turbulent flows.','W')
            call usrmsg('Please select a method for modelling',
     &         'turbulence via the solution variables menu pick.','W')
          else
            IVTYPE(ISB,ICFD)=IVTYPE(ISB,ICFD)+3
          endif
        else
          IVTYPE(ISB,ICFD)=IWO
        endif
      elseif (INO.eq.5) then
        t20='Solid boundary'
        if(IVOLF(ISB,ICFD).ne.0)IFACE=IVOLF(ISB,ICFD)
        call PIKCELS(Ii,Iif,Ji,Jf,Ki,Kf,IFACE,IER)
        IVOLF(ISB,ICFD)=IFACE
        IVCELLS(ISB,ICFD,1)=Ii+1; IVCELLS(ISB,ICFD,2)=Iif+1
        JVCELLS(ISB,ICFD,1)=Ji+1; JVCELLS(ISB,ICFD,2)=Jf+1
        KVCELLS(ISB,ICFD,1)=Ki+1; KVCELLS(ISB,ICFD,2)=Kf+1
        call NEW2OLD
      elseif (INO.eq.8) then
        if (IVTYPE(ISB,ICFD).eq.1) then
          VAL=VOLTemp(ISB,ICFD)
          call EASKR(VAL,' ','Temperature at chosen face of cells?',
     &           -100.,'W',100.,'W',10.,'temperature',IER,nbhelp)
          if (IER.eq.0) VOLTemp(ISB,ICFD)=VAL
        elseif (IVTYPE(ISB,ICFD).eq.2) then
          VAL=VOLHeat(ISB,ICFD)
          call EASKR(VAL,' ','Heat flux at chosen face of cells?',
     &           -1000.,'W',1000.,'W',10.,'heat flux',IER,nbhelp)
          if (IER.eq.0) VOLHeat(ISB,ICFD)=VAL
        elseif (IVTYPE(ISB,ICFD).eq.4) then
          CALL EASKSUR(IZONE,IS,'-','Select a surface.',' ',IER)
          VCsurf(ISB,ICFD)=SNAME(IZONE,IS)
        endif
      elseif (INO.eq.9) then
        if (IVTYPE(ISB,ICFD).eq.4) then
          call HANDSHK(IHST,IER)
          if (IER.eq.0) IVConfl(ISB,ICFD)=IHST
        endif
      elseif (INO.eq.(NITMS-1)) then

C Help.
        helptopic='cfd_solid_boundary_edit'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('boundary conditions',nbhelp,'-',0,0,IER)
      elseif (INO.eq.NITMS) then

C Return.
        return
      endif

      goto 5
      end

C ******************** EDSRC ********************
C Controls the editing of CFD sources - humidity, CO2 etc.
C IER=0 indicates no error.

      SUBROUTINE EDSRC(ISB,IER)
#include "building.h"
#include "cfd.h"
#include "net_flow.h"
#include "net_flow_data.h"
#include "help.h"

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/ICFNOD/ICFD,ICP
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      common/KEYVOLS/NVOL(MNZ),IVOLF(MNVLS,MNZ),IVCELLS(MNVLS,MNZ,2),
     &               JVCELLS(MNVLS,MNZ,2),KVCELLS(MNVLS,MNZ,2)
      common/KEYVOLN/VOLNAME(MNVLS,MNZ),VCsurf(MNVLS,MNZ),
     &               BLKSURF(MNVLS,MNZ,6)
      common/KEYVDAT/IVTYPE(MNVLS,MNZ),VOLTemp(MNVLS,MNZ),
     &          VOLHeat(MNVLS,MNZ),IVConfl(MNVLS,MNZ),VOLHum(MNVLS,MNZ),
     &          VOLCO2(MNVLS,MNZ),VOLVel(MNVLS,MNZ),VOLDir(MNVLS,MNZ,2),
     &          VOLArea(MNVLS,MNZ),VOLPres(MNVLS,MNZ),
     &          VOLPol(MCTM,MNVLS,MNZ)
      common/KEYCASGN/IDcasgn(MNVLS,MNZ),Fcasgn(MNVLS,MNZ)
      common/EQTION3/CALLMA(MNZ),CALPOL(MCTM,MNZ),POLNAM(MCTM,MNZ),
     &               NCTM(MNZ),JHUMINDX(MNZ),URFC(MCTM)
      COMMON/SCHMTT/SCHMT(MCTM,MNZ),GFM(MCTM,MNZ),VCRIT(MCTM,MNZ),
     &       TBOIL(MCTM,MNZ),TCRIT(MCTM,MNZ),ISCHMT(MCTM,MNZ)
      COMMON/CTDFAF/ICTDFAF(MNZ),SRCE(MNVLS,MCTM,MNZ),ICCSRC(MNZ),
     &              ICC2NC(MCTM,MNZ),SRCFRC(MNVLS,MCTM,MNZ)
      COMMON/CONTM0/NCONTM,NOCNTM,CONTMNAM(MCONTM)
      COMMON/CONTM5/SPMSUP(MSPMNO,MCSD),SSLINK2(MSPMNO,MNOD),
     &      NSSNO(MNOD),SPMTYP(MSPMNO),SSNAME(MSPMNO),NSPMNO,
     &      SSLINK1(MSPMNO,MCONTM)
      common/cfdconf/ICFBLD(MNZ),ICFMFS(MNZ)
      COMMON/CFDMFS1/ICFDNOD(MNZ),ICFDCNN(MNVLS,MNZ)
      COMMON/CFDVIS/HAS_GEOM,ISHSB,ISHAO,IFACES,ISHBLK,ISHSRC,ISHGEO,
     &              INITD
      logical HAS_GEOM
      character INITD*6
      common/cfdfil/LCFD(MCOM),IFCFD(MCOM)
      character LCFD*72
      common/dynamico/isdynamicocup(MCOM)
      COMMON/LINRFC/URFCU(MNZ),URFCV(MNZ),URFCW(MNZ),URFCP(MNZ),
     &              URFCT(MNZ),URFCK(MNZ),URFCE(MNZ),URFCVS(MNZ),
     &              URFCC(MNZ,MCTM)
      COMMON/LINRFC2/URFCU2(MNZ),URFCV2(MNZ),URFCW2(MNZ),URFCP2(MNZ),
     &              URFCT2(MNZ),URFCK2(MNZ),URFCE2(MNZ),URFCVS2(MNZ),
     &              URFCC2(MNZ,MCTM)
      COMMON/GFONT/IFS,ITFS,IMFS
      COMMON/INITIA/UINIT(MNZ),VINIT(MNZ),WINIT(MNZ),PINIT(MNZ),
     &              TINIT(MNZ),TEINIT(MNZ),EDINIT(MNZ),POLINIT(MNZ,MCTM)
      COMMON/CONTM6/CNCNI(MCONTM,MNOD)
      COMMON/AFN/IAIRN,LAPROB,ICAAS(MCOM)
      CHARACTER LAPROB*72

      character ITMSS(14+MCTM)*60,ITUM(MCTM+1)*50,WORD*50
      character t12*12,d12*12,t20*20,HOLD*32,WORD1*12
      character VOLNAME*12,VCsurf*12,BLKSURF*12
      character*12 SRCNAM,POLNAM,CONTMNAM,SSNAME,SRCE
      CHARACTER ITMSS1(14+MCTM)*60,ITMSS2(14+MCTM)*60
      character outs*124
      LOGICAL CALPOL,CALLMA,OK
      INTEGER SPMTYP,SSLINK1,SSLINK2
      integer NITMS,INO,NITMSS1,IIO,NITMSS2,IXO,NITUM,INUO ! max items and current menu item

      helpinsub='edcfd'  ! set for subroutine

C Initialise SRCE.
      IF(ICTDFAF(ICFD).NE.1)THEN
        DO 420 ICTM=1,NCTM(ICFD)
          DO 520 II=1,NVOL(ICFD)
            SRCE(II,ICTM,ICFD)=' '
            SRCFRC(II,ICTM,ICFD)=0.0
 520      CONTINUE
 420    CONTINUE
      ENDIF

C Adjust each cell by -1 as dfs adds fictitious cells around the domain.
      Ii=IVCELLS(ISB,ICFD,1)-1; Iif=IVCELLS(ISB,ICFD,2)-1
      Ji=JVCELLS(ISB,ICFD,1)-1; Jf=JVCELLS(ISB,ICFD,2)-1
      Ki=KVCELLS(ISB,ICFD,1)-1; Kf=KVCELLS(ISB,ICFD,2)-1

C Check if new source.
      if (Ii.lt.0.or.Iif.lt.0.or.
     &    Ji.lt.0.or.Jf.lt.0.or.
     &    Ki.lt.0.or.Kf.lt.0) then 
        t20='Source definition'
        if(IVOLF(ISB,ICFD).ne.0)IFACE=IVOLF(ISB,ICFD)
        call PIKCELS(Ii,Iif,Ji,Jf,Ki,Kf,IFACE,IER)
        IVOLF(ISB,ICFD)=IFACE
        IVCELLS(ISB,ICFD,1)=Ii+1; IVCELLS(ISB,ICFD,2)=Iif+1
        JVCELLS(ISB,ICFD,1)=Ji+1; JVCELLS(ISB,ICFD,2)=Jf+1
        KVCELLS(ISB,ICFD,1)=Ki+1; KVCELLS(ISB,ICFD,2)=Kf+1
        call NEW2OLD
      endif

C Create a menu showing definitions for different sources.
 144  write (ITMSS(1),'(a,a)')  'a name: ',VOLNAME(ISB,ICFD)
      if (NOCNTM.eq.1 .and. ICFMFS(ICFD).eq.1) then
        IF(ICTDFAF(ICFD).EQ.1)then
          write (ITMSS(2),'(a)')'b network coupling: on'
        else
          write (ITMSS(2),'(a)')'b network coupling: off'
        endif
      else
        write (ITMSS(2),'(a)')  '  network coupling: not available'
      endif
      write (ITMSS(3),'(a)')    ' -----------------------------'
      write (ITMSS(4),'(a,a)')  '         Is  If  Js  Jf  Ks  Kf '
      write (ITMSS(5),'(a,6(i3,1x))')'c cells:',Ii,Iif,Ji,Jf,Ki,Kf
      write (ITMSS(6),'(a)')    ' -----------------------------'
      write(ITMSS(7),'(a,f8.0)')'e heat flux (W): ',VOLHeat(ISB,ICFD)
      write(ITMSS(8),'(a,i3)')  'f cas gain index: ',IDCasgn(ISB,ICFD)
      write(ITMSS(9),'(a,f4.2)')'g gain fraction: ',FCasgn(ISB,ICFD)

C This option reserved for when multi-segmented dynamic people model is
C fixed.
C      write(ITMSS(10),'(2a)')   'h occupant type: ',VCsurf(ISB,ICFD)
      write(ITMSS(10),'(a)')
     &  '  ----------------------------------------'
      write (ITMSS(11),'(a)')
     &  '  contaminant     | source strength (kg/s)'
      WRITE (ITMSS(12),'(A)')
     &  '     name         | or name and fraction  '
      DO ICTM=1,NCTM(ICFD)
        CALPOL(ICTM,ICFD)=.TRUE.
        WORD1=SRCE(ISB,ICTM,ICFD)
        if (WORD1(:1).EQ.' ') then ! constant source
          write(hold,'(F12.10)')VOLPOL(ICTM,ISB,ICFD)
        elseif (WORD1(1:6).EQ.'CFDOPN') then ! represents opening
          read(WORD1(7:9),'(i3.3)')ix
          icnn=ICFDCNN(ix,ICFD)
          if (NODPS(ICNN).eq.ICFDNOD(ICFD)) then
            hold=NDNAM(NODNE(ICNN))
          elseif (NODNE(ICNN).eq.ICFDNOD(ICFD)) then
            hold=NDNAM(NODPS(ICNN))
          else
            hold='UNKNOWN'
          endif
        elseif (JHUMINDX(ICFD).eq.ictm .and. 
     &          VCsurf(ISB,ICFD)(1:4).ne.'none') then ! represents occupant moisture
          hold='occupant'
        else ! represents network source
          write(hold,'(a12,1x,f5.3)')SRCE(ISB,ICTM,ICFD),
     &     SRCFRC(ISB,ICTM,ICFD)
        ENDIF
        IF(JHUMINDX(ICFD).NE.ICTM)THEN
          WRITE(ITMSS(12+ICTM),'(A1,1X,A16,1X,A)')CHAR(104+ICTM),
     &        POLNAM(ICTM,ICFD),hold
        ELSE
          WRITE(ITMSS(12+ICTM),'(A1,1X,A12,A4,1X,A)')CHAR(104+ICTM),
     &        POLNAM(ICTM,ICFD),' (m)',hold
        ENDIF
      ENDDO
      NITMS=16+NCTM(ICFD)
      ITMSS(NITMS-3)='  ----------------------------------------'
      ITMSS(NITMS-2)='+ add/delete contaminant'
      ITMSS(NITMS-1)='? help'
      ITMSS(NITMS)=  '- exit menu'

      IF(MMOD.EQ.8)THEN
        isrc=0
        do 639 I=1,NVOL(ICFD)
          if (IVTYPE(I,ICFD).eq.20) then
            isrc=isrc+1
            if (ISB.eq.I) then
              ISHBLK_TMP=ISHBLK
              ISHGEO_TMP=ISHGEO
              ISHBLK=isrc
              ISHGEO=0
              CALL redraw(IER)
              ISHBLK=ISHBLK_TMP
              ISHGEO=ISHGEO_TMP
            endif
          endif
639     continue   
      ENDIF

C Help text for this menu.
  7   helptopic='cfd_source_boundary_edit'
      call gethelptext(helpinsub,helptopic,nbhelp)

C If in graphic model show the current grid.
      IF(MMOD.EQ.8)THEN

C Recalculate gridding in case this has changed.
        CALL INICNT
        CALL GRID(ier)
        call NEW2OLD

        call redraw(ier)
      ENDIF

      IF(VOLNAME(ISB,ICFD)(1:4).EQ.'SRC_')THEN
        helptopic='src_ctmnetwork_warn'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('Warning!',nbhelp,'-',0,0,IER)
      ENDIF

C Set fixed width font.
      itmp=IMFS
      if(IMFS.eq.4) IMFS=0
      if(IMFS.eq.5) IMFS=1
      if(IMFS.eq.6) IMFS=2
      if(IMFS.eq.7) IMFS=3
      call userfonts(IFS,ITFS,IMFS)
      
C Display menu.
      INO=-1
      CALL EMENU('Sources edit',ITMSS,NITMS,INO)

C Return to previous font.
      IMFS=itmp
      call userfonts(IFS,ITFS,IMFS)

      if (INO.lt.1) then
        goto 7
      elseif (ITMSS(INO)(1:1).eq.' ') then
        goto 7

C Set boundary name.
      elseif (INO.eq.1) then
        t12=VOLNAME(ISB,ICFD)
        d12='  '
        call EASKS(t12,' ','Source name?',12,d12,'src name',
     &    IER,nbhelp)
        if (IER.eq.0) then
          call st2name(t12,d12)
          VOLNAME(ISB,ICFD)=d12
        endif

C Set coupling with contaminants network on/off.
      elseif (INO.eq.2) then
        CALL EASKABC('Contaminant network coupling','Options:','off',
     &    'on','cancel',IHOWL,nbhelp)
        IF(IHOWL.EQ.1)THEN
          if (ICTDFAF(ICFD).eq.1) then
            CALL EASKOK('All linkage data will be cleared!','continue?',
     &        OK,nbhelp)
            IF(.NOT.OK)GOTO 144
          endif

C Clear coupling commons.
          ICTDFAF(ICFD)=0
          do ictm=1,MCTM
            ICC2NC(ictm,ICFD)=0
            do iiv=1,NVOL(ICFD)
              if (IVTYPE(iiv,ICFD).eq.20) then
                SRCE(iiv,ictm,ICFD)=' '
                SRCFRC(iiv,ictm,ICFD)=0.0
              endif
            enddo
          enddo
        ELSEIF(IHOWL.EQ.2)THEN

          if (ICTDFAF(ICFD).eq.1) goto 144

          if (NCTM(ICFD).eq.0) then
            call edisp(iuout,'Please define contaminant(s) first.')
            goto 144
          endif

C Take contaminant definitions from contaminant flow network.
          ICTDFAF(ICFD)=1

C Initial coupling setup.
C There are existing CFD contaminants, offer the chance to associate
C these with network contaminants.
 3100     do icctm=1,NCTM(ICFD)
            do inctm=1,NCONTM
              WRITE(ITMSS1(inctm),'(2A,A12)')CHAR(96+inctm),' ',
     &          CONTMNAM(inctm)
            enddo
            ITMSS1(NCTM(ICFD)+1) =' ------------------------'
            ITMSS1(NCTM(ICFD)+2) ='? help                   '
            ITMSS1(NCTM(ICFD)+3) ='- none of the above      '
            NITMSS1=NCTM(ICFD)+3
            IIO=-1
            write(outs,'(4a)')'Select a network contaminant that',
     &        ' represents CFD contaminant ',POLNAM(icctm,ICFD),'.'
            call edisp(iuout,outs)
            write(outs,'(3a)')'Which is contaminant ',
     &        POLNAM(icctm,ICFD),'?'
            CALL EMENU(outs,ITMSS1,NITMSS1,IIO)
            IF(IIO.EQ.NITMSS1)THEN
              ICC2NC(icctm,ICFD)=0
            ELSEIF(IIO.EQ.NITMSS1-1)THEN
              helptopic='cfd_ctm_network_assoc'
              call gethelptext(helpinsub,helptopic,nbhelp)
              CALL PHELPD('CFD contaminant network association',
     &          nbhelp,'-',0,0,IER)
            ELSEIF(IIO.GE.1.AND.IIO.LE.NCTM(ICFD))THEN
              ICC2NC(icctm,ICFD)=IIO
              POLINIT(ICFD,icctm)=CNCNI(IIO,ICAAS(ICP)) ! take initial conc from network
            ELSE
              GOTO 3100
            ENDIF
          enddo
          
C Ask if any of the network contaminants are moisture.
          if (JHUMINDX(ICFD).eq.0) then
            DO ICTM=1,NCONTM
              WRITE(ITMSS1(ICTM),'(2A,A12)')CHAR(96+ICTM),' ',
     &          CONTMNAM(ICTM)
            enddo
            ITMSS1(NCTM(ICFD)+1) =' ------------------------'
            ITMSS1(NCTM(ICFD)+2) ='? help                   '
            ITMSS1(NCTM(ICFD)+3) ='- none of the above      '
            NITMSS1=NCTM(ICFD)+3
 3200       IIO=-1
            CALL EMENU('Which is moisture?',ITMSS1,NITMSS1,IIO)
            IF(IIO.EQ.NITMSS1)THEN
              CONTINUE
            ELSEIF(IIO.EQ.NITMSS1-1)THEN
              helptopic='cfd_ctm_network_moist'
              call gethelptext(helpinsub,helptopic,nbhelp)
              CALL PHELPD('CFD contaminant network moisture',
     &          nbhelp,'-',0,0,IER)
              goto 3200
            ELSEIF(IIO.GE.1.AND.IIO.LE.NCTM(ICFD))THEN
              JHUMINDX(ICFD)=NCTM(ICFD)+IIO
            ELSE
              GOTO 3200
            ENDIF
          endif

C Create new CFD contaminants for network contaminants that have not
C already been associated.
          do inctm=1,NCONTM
            ok=.true.
            do icctm=1,NCTM(ICFD)
              if (ICC2NC(icctm,ICFD).eq.inctm) then
                ok=.false.
                exit
              endif
            enddo
            if (.not.ok) cycle
            NCTM(ICFD)=NCTM(ICFD)+1; icctm=NCTM(ICFD)
            POLNAM(icctm,ICFD)=CONTMNAM(inctm)
            ICC2NC(icctm,ICFD)=inctm
            URFCC(ICFD,icctm)=0.9 ! default URFs
            URFCC2(ICFD,icctm)=0.5
            POLINIT(ICFD,icctm)=CNCNI(inctm,ICAAS(ICP)) ! take initial conc from network
            do iiv=1,NVOL(ICFD)
              if (IVTYPE(iiv,ICFD).eq.20) then
                VOLPOL(icctm,iiv,ICFD)=0.0
                SRCE(iiv,icctm,ICFD)=' '
                SRCFRC(iiv,icctm,ICFD)=0.0
              endif
            enddo

C Ask for information to calculate Schmidt number.
            if (icctm.eq.JHUMINDX(ICFD)) then
              write(outs,'(3a)')'Contaminant ',POLNAM(icctm,ICFD),
     &          ' is water vapour, turbulent Schmidt number is 0.59.'
              call edisp(iuout,outs)
              ISCHMT(ICTM,ICFD)=0; SCHMT(ICTM,ICFD)=0.59
            else
              helptopic='cfd_contam_schmidt'
              call gethelptext(helpinsub,helptopic,nbhelp)
              CALL EASKABCD('Specify turbulent Schmidt number for',
     &          POLNAM(icctm,ICFD),'dynamic','fixed','CO2',
     &          'water vapour',ISCHM,nbhelp)
              IF(ISCHM.EQ.1)THEN
                ISCHMT(icctm,ICFD)=1
                HOLD=' 44.0 94.0 217.0 304.0 '
                CALL EASKS(HOLD,
     &            'enter mol mass(g/mol), crit vol(cm^3/mol)',
     &            'boiling pt (K), and crit pt (K)',32,
     &            ' 44.0 94.0 217.0 304.0 ','schmidt param',IER,
     &            nbhelp)
                K=0
                CALL EGETWR(HOLD,K,VL1,1.,1000.,'W','molar mass',IER)
                CALL EGETWR(HOLD,K,VL2,1.,1000.,'W','crit vol',IER)
                CALL EGETWR(HOLD,K,VL3,100.,500.,'W','T_boil',IER)
                CALL EGETWR(HOLD,K,VL4,100.,1000.,'W','T_crit',IER)
                GFM(icctm,ICFD)=VL1; VCRIT(icctm,ICFD)=VL2; 
                TBOIL(icctm,ICFD)=VL3; TCRIT(icctm,ICFD)=VL4
              ELSEIF(ISCHM.EQ.2)THEN
                ISCHMT(icctm,ICFD)=0
                call EASKR(VAL,'Fixed turbulent Schmidt number for',
     &            POLNAM(icctm,ICFD),0.1,'W',2.5,'W',1.,'schmidt #',
     &            IER,nbhelp)
                SCHMT(icctm,ICFD)=VAL
              ELSEIF(ISCHM.EQ.3)THEN
                ISCHMT(icctm,ICFD)=1
                GFM(icctm,ICFD)=44.0; VCRIT(icctm,ICFD)=94.0
                TBOIL(icctm,ICFD)=217.0; TCRIT(icctm,ICFD)=304.0
              elseif (ischm.eq.4) then
                ISCHMT(icctm,ICFD)=0; SCHMT(icctm,ICFD)=0.59
              ENDIF
            endif
          enddo

C Add a source type boundary condition for each opening type boundary
C condition, and use SRCE to store strings to associate them.
          NOBC=NVOL(ICFD)
          DO IV=1,NOBC
            IF(IVTYPE(IV,ICFD).GE.10.AND.IVTYPE(IV,ICFD).LE.19)THEN
              NVOL(ICFD)=NVOL(ICFD)+1
              NUMVOL=NVOL(ICFD)
              WRITE(VOLNAME(NUMVOL,ICFD),'(2A)')'SRC_',
     &          VOLNAME(IV,ICFD)(1:8)
              IVTYPE(NUMVOL,ICFD)=20
              IVOLF(NUMVOL,ICFD)=9
              DO L=1,2
                IVCELLS(NUMVOL,ICFD,L)=IVCELLS(IV,ICFD,L)
                JVCELLS(NUMVOL,ICFD,L)=JVCELLS(IV,ICFD,L)
                KVCELLS(NUMVOL,ICFD,L)=KVCELLS(IV,ICFD,L)
              enddo
              VOLHEAT(NUMVOL,ICFD)=0.0
              IDCASGN(NUMVOL,ICFD)=0
              FCASGN(NUMVOL,ICFD)=0.0
              VCSURF(NUMVOL,ICFD)='none'
              DO ICTM=1,NCTM(ICFD)
                VOLPOL(ICTM,NUMVOL,ICFD)=0.0
                if (ICC2NC(ictm,ICFD).ne.0) then
                  write(SRCE(NUMVOL,ictm,ICFD),'(a,i3.3)')'CFDOPN',IV
                  SRCFRC(NUMVOL,ictm,ICFD)=1.0
                else
                  SRCE(NUMVOL,ictm,ICFD)=' '
                  SRCFRC(NUMVOL,ictm,ICFD)=0.0
                endif
              enddo
            ENDIF
          enddo

          if (NSPMNO.gt.0) then
            helptopic='cfd_contam_sinks'
            call gethelptext(helpinsub,helptopic,nbhelp)
            CALL EASKOK('Link contaminant network source/sinks',
     &        'to this source boundary condition?',OK,nbhelp)
          else
            OK=.false.
          endif
          IF(OK)THEN
            DO icctm=1,NCTM(ICFD)
              inctm=ICC2NC(icctm,ICFD)
              if (inctm.eq.0) cycle
              IX=1
              DO ISPMNO=1,NSPMNO
                IF(SSLINK1(ISPMNO,inctm).NE.0)THEN
                  WRITE(ITMSS2(IX),'(A,1X,A)')CHAR(96+IX),
     &            SSNAME(SSLINK1(ISPMNO,inctm))
                  IX=IX+1
                endif
              enddo
              IF (IX.EQ.1) then
                write(outs,'(3a)')
     &            'No sources/sinks defined for network contaminant ',
     &            CONTMNAM(inctm),'.'
                call edisp(iuout,outs)
                SRCE(ISB,icctm,ICFD)=' '
                SRCFRC(ISB,icctm,ICFD)=0.0
                cycle
              endif
              ITMSS2(IX)=   ' ------------------'
              ITMSS2(IX+1)= '? help'
              ITMSS2(IX+2)= '- none of the above'
              NITMSS2=IX+2
              WRITE(outs,'(3A)')
     &          'Select source/sink for network contaminant ',
     &          CONTMNAM(inctm),'.'
              call edisp(iuout,outs)
 3300         IXO=-1
              WRITE(WORD,'(3A)')'Contaminant ',CONTMNAM(inctm),
     &          ' source/sink'
              CALL EMENU(WORD,ITMSS2,NITMSS2,IXO)
              IF(IXO.EQ.NITMSS2)THEN
                SRCE(ISB,icctm,ICFD)=' '
                SRCFRC(ISB,icctm,ICFD)=0.0
              ELSEIF(IXO.EQ.NITMSS2-1)THEN
                CALL PHELPD('Choose source/sink',nbhelp,'-',0,0,IER)
                GOTO 3300
              ELSEIF(IXO.GE.1 .AND. IXO.LE.NITMSS2-3)THEN
                SRCE(ISB,icctm,ICFD)=SSNAME(SSLINK1(IXO,inctm))
                val=1.0
                CALL EASKR(val,'What fraction is injected here?',
     &            '(0.0 - 1.0)',0.0,'F',1.0,'F',1.0,'src frac',IER,
     &            nbhelp)
                SRCFRC(ISB,icctm,ICFD)=val
              ELSE
                GOTO 3300
              ENDIF
            enddo
          ENDIF
        ENDIF
        GOTO 144

C Define CFD grid cells for source type boundary condition
      elseif (INO.eq.5) then
        t20='Source definition'
        if(IVOLF(ISB,ICFD).ne.0)IFACE=IVOLF(ISB,ICFD)
        call PIKCELS(Ii,Iif,Ji,Jf,Ki,Kf,IFACE,IER)
        IVOLF(ISB,ICFD)=IFACE
        IVCELLS(ISB,ICFD,1)=Ii+1; IVCELLS(ISB,ICFD,2)=Iif+1
        JVCELLS(ISB,ICFD,1)=Ji+1; JVCELLS(ISB,ICFD,2)=Jf+1
        KVCELLS(ISB,ICFD,1)=Ki+1; KVCELLS(ISB,ICFD,2)=Kf+1
        call NEW2OLD

C Set heat flux.
      elseif (INO.eq.7) then
        VAL=VOLHeat(ISB,ICFD)
        call EASKR(VAL,'Heat flux (W) in source volume?',
     &    ' ',-1000.,'W',1000.,'W',10.,'heat flux',IER,nbhelp)
        if (IER.eq.0) VOLHeat(ISB,ICFD)=VAL

C Link casual gain type.
      elseif (INO.eq.8) then
        if (IFCFD(ICFD).eq.0) then
          call usrmsg('This domain is not coupled.',
     &      'Option not available.','w')
          goto 7
        endif
        ival=IDcasgn(ISB,ICFD)
        call PIKCGTYP(ICP,ival,nbhelp)
        IDcasgn(ISB,ICFD)=IVAL

C Set casual gain fraction.
      elseif (INO.eq.9) then
        if (IFCFD(ICFD).eq.0) then
          call usrmsg('This domain is not coupled.',
     &      'Option not available.','w')
          goto 7
        endif
        VAL=FCasgn(ISB,ICFD)
        call EASKR(VAL,'What fraction of casual gain is generated? ',
     &  ' ',0.,'W',1.,'W',1.,'casgn fraction',IER,nbhelp)
        if (IER.eq.0) FCasgn(ISB,ICFD)=VAL

C Set occupant type for link with dynamic people.
C Temporarily disabled until multi-segmented model is fixed.
      elseif (INO.eq.10) then  

        call usrmsg('Option currently not available.',' ','w')
        goto 7

        if (IFCFD(ICFD).eq.0) then
          call usrmsg('This domain is not coupled.',
     &      'Option not available.','w')
          goto 7
        endif
        if (isdynamicocup(ICP).ne.4) then
          call usrmsg('Multi-segmented person model not defined.',
     &      'Option not available.','w')
          goto 7
        endif
        i=2
        CALL EASKABCD('Occupant type?',' ','none','man','woman','child',
     &    i,nbhelp) ! store occupant type in VCsurf for sources
        if (i.eq.1) then
          VCsurf(ISB,ICFD) = 'none'
        elseif (i.eq.2) then
          VCsurf(ISB,ICFD) = 'man'
        elseif (i.eq.3) then
          VCsurf(ISB,ICFD) = 'woman'
        elseif (i.eq.4) then
          VCsurf(ISB,ICFD) = 'child'
        endif

C Manage contaminants.
      elseif (INO.eq.NITMS-2) then
        helptopic='cfd_contam_sinks'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKABC(' ','Contaminant source:','add',
     &      'delete','cancel',IOWL,nbhelp)
        IF(IOWL.EQ.1)THEN
          helptopic='cfd_contam_sinks'
          call gethelptext(helpinsub,helptopic,nbhelp)
          SRCNAM=' '
          d12='Contaminant'
          call EASKS(SRCNAM,'Contaminant name? ','  ',12,d12,
     &      'cntmnt name',IER,nbhelp)
          NCTM(ICFD)=NCTM(ICFD)+1; ICTM=NCTM(ICFD)
          POLNAM(NCTM(ICFD),ICFD)=SRCNAM
          call EASKR(VAL,'What is the source strength (kg/s) ',
     &      ' ',0.,'W',10.,'W',1.,'src strength',IER,nbhelp)
          VOLPOL(NCTM(ICFD),ISB,ICFD)=VAL
          CALPOL(NCTM(ICFD),ICFD)=.TRUE.
          CALL EASKOK(' ','Is this contaminant water vapour?',OK,0)
          if (ok) then
            JHUMINDX(ICFD)=ictm
            write(outs,'(3a)')'Contaminant ',POLNAM(ictm,ICFD),
     &        ' is water vapour, turbulent Schmidt number is 0.59.'
            call edisp(iuout,outs)
            ISCHMT(ICTM,ICFD)=0; SCHMT(ICTM,ICFD)=0.59

C Ask for information to calculate Schmidt number.
          else
            helptopic='cfd_contam_schmidt'
            call gethelptext(helpinsub,helptopic,nbhelp)
            CALL EASKABCD('Specify turbulent Schmidt number for',
     &        POLNAM(ICTM,ICFD),'dynamic','fixed','CO2',
     &        'water vapour',ISCHM,nbhelp)
            IF(ISCHM.EQ.1)THEN
              ISCHMT(ICTM,ICFD)=1
              HOLD=' 44.0 94.0 217.0 304.0 '
              CALL EASKS(HOLD,
     &          'enter mol mass(g/mol), crit vol(cm^3/mol)',
     &          'boiling pt (K), and crit pt (K)',32,
     &          ' 44.0 94.0 217.0 304.0 ','schmidt param',IER,
     &          nbhelp)
              K=0
              CALL EGETWR(HOLD,K,VL1,1.,1000.,'W','molar mass',IER)
              CALL EGETWR(HOLD,K,VL2,1.,1000.,'W','crit vol',IER)
              CALL EGETWR(HOLD,K,VL3,100.,500.,'W','T_boil',IER)
              CALL EGETWR(HOLD,K,VL4,100.,1000.,'W','T_crit',IER)
              GFM(ICTM,ICFD)=VL1; VCRIT(ICTM,ICFD)=VL2; 
              TBOIL(ICTM,ICFD)=VL3; TCRIT(ICTM,ICFD)=VL4
            ELSEIF(ISCHM.EQ.2)THEN
              ISCHMT(ICTM,ICFD)=0
              call EASKR(VAL,'Fixed turbulent Schmidt number for',
     &          POLNAM(ICTM,ICFD),0.1,'W',2.5,'W',1.,'schmidt #',
     &          IER,nbhelp)
              SCHMT(ICTM,ICFD)=VAL
            ELSEIF(ISCHM.EQ.3)THEN
              ISCHMT(ICTM,ICFD)=1
              GFM(ICTM,ICFD)=44.0; VCRIT(ICTM,ICFD)=94.0
              TBOIL(ICTM,ICFD)=217.0; TCRIT(ICTM,ICFD)=304.0
            elseif (ischm.eq.4) then
              ISCHMT(ICTM,ICFD)=0; SCHMT(ICTM,ICFD)=0.59
            ENDIF
          endif
          
          ICC2NC(ictm,ICFD)=0
          URFCC(ICFD,ictm)=0.9 ! default URFs
          URFCC2(ICFD,ictm)=0.5
          POLINIT(ICFD,ictm)=0.0004 ! default initial concentration
          do iiv=1,NVOL(ICFD)
            if (IVTYPE(iiv,ICFD).eq.20) then
              VOLPOL(ictm,iiv,ICFD)=0.0
              SRCE(iiv,ictm,ICFD)=' '
              SRCFRC(iiv,ictm,ICFD)=0.0
            endif
          enddo

C Delete contaminant.
        ELSEIF(IOWL.EQ.2)THEN
          WRITE(ITUM(1),'(a)')' Contaminant name: '
          DO ICTM=1,NCTM(ICFD)
            WRITE(ITUM(1+ICTM),'(3A)')
     &        CHAR(96+ICTM),' ',POLNAM(ICTM,ICFD)
          enddo
          NITUM=1+NCTM(ICFD)
          INUO=-1
          CALL EMENU('Choose contaminant to delete',ITUM,NITUM,INUO)
          DO ICTM=INUO-1,NCTM(ICFD)-1
            POLNAM(ICTM,ICFD)=POLNAM(ICTM+1,ICFD)
            CALPOL(ICTM,ICFD)=CALPOL(ICTM+1,ICFD)
            SCHMT(ICTM,ICFD)=SCHMT(ICTM+1,ICFD)
            ISCHMT(ICTM,ICFD)=ISCHMT(ICTM+1,ICFD)
            GFM(ICTM,ICFD)=GFM(ICTM+1,ICFD)
            VCRIT(ICTM,ICFD)=VCRIT(ICTM+1,ICFD)
            TBOIL(ICTM,ICFD)=TBOIL(ICTM+1,ICFD)
            TCRIT(ICTM,ICFD)=TCRIT(ICTM+1,ICFD)
            ICC2NC(ictm,ICFD)=ICC2NC(ictm+1,ICFD)
            URFCC(ICFD,ictm)=URFCC(ICFD,ictm+1)
            URFCC2(ICFD,ictm)=URFCC2(ICFD,ictm+1)

C Reset humidity/ water index if applicable.
            IF(JHUMINDX(ICFD).EQ.INUO)THEN
              JHUMINDX(ICFD)=0
            ELSEIF(JHUMINDX(ICFD).GT.INUO)THEN
              JHUMINDX(ICFD)=JHUMINDX(ICFD)-1
            ENDIF

C Delete this contaminant from all key volumes.
            DO I=1,NVOL(ICFD)
              if (IVTYPE(i,ICFD).eq.20) then
                VOLPOL(ICTM,I,ICFD)=VOLPOL(ICTM+1,I,ICFD)
                SRCE(i,ictm,ICFD)=SRCE(i,ictm+1,ICFD)
                SRCFRC(i,ictm,ICFD)=SRCFRC(i,ictm+1,ICFD)
              endif
            enddo
            POLNAM(NCTM(ICFD),ICFD)=' '
            CALPOL(NCTM(ICFD),ICFD)=.FALSE.
            DO I=1,NVOL(ICFD)
              IF(IVTYPE(I,ICFD).EQ.20)VOLPOL(NCTM(ICFD),I,ICFD)=0.
            enddo
            NCTM(ICFD)=NCTM(ICFD)-1
          enddo
        ENDIF
        GOTO 144

C Edit contaminants.
      elseif (INO.gt.12.and.INO.lt.13+NCTM(ICFD)) then
        ICTM=INO-12
        helptopic='cfd_src_editctm'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKOK('Edit contaminant parameters?',' ',OK,nbhelp)

        if (OK) then
          helptopic='cfd_link_contam_sink'
          call gethelptext(helpinsub,helptopic,nbhelp)
          SRCNAM=POLNAM(ICTM,ICFD)
          d12='Contaminant'
          call EASKS(SRCNAM,'Contaminant name?','  ',12,d12,
     &     'cntmnt name',IER,nbhelp)
          POLNAM(ICTM,ICFD)=SRCNAM
          
          CALL EASKOK(' ','Is this contaminant water vapour?',
     &      OK,nbhelp)
          IF(OK)THEN
            JHUMINDX(ICFD)=ICTM
          ELSE
            IF(JHUMINDX(ICFD).EQ.ICTM)JHUMINDX(ICFD)=0
          ENDIF

C Ask for information to calculate Schmidt number.
          helptopic='cfd_contam_schmidt'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL EASKABC('How to specify turbulent Schmidt No.?'
     &      ,' ','parameters','manual','default',ISCHM,nbhelp)
          IF(ISCHM.EQ.1)THEN
            ISCHMT(ICTM,ICFD)=1
            CALL EASKS(HOLD,'enter mol mass (g/mol) crit vol (cm^3/mol)'
     &        ,'boiling pt (K) and crit pt (K)',32
     &        ,' 44.0 94.0 217.0 304.0',' ',IER,nbhelp)
            K=0
            CALL EGETWR(HOLD,K,VL1,1.,1000.,'W','molar mass',IER)
            CALL EGETWR(HOLD,K,VL2,1.,1000.,'W','crit vol',IER)
            CALL EGETWR(HOLD,K,VL3,100.,500.,'W','T_boil',IER)
            CALL EGETWR(HOLD,K,VL4,100.,1000.,'W','T_crit',IER)
            GFM(ICTM,ICFD)=VL1; VCRIT(ICTM,ICFD)=VL2; 
            TBOIL(ICTM,ICFD)=VL3; TCRIT(ICTM,ICFD)=VL4
          ELSEIF(ISCHM.EQ.2)THEN
            ISCHMT(ICTM,ICFD)=0
            call EASKR(VAL,'What is Turbulent Schmidt Number ',
     &        ' ',0.5,'W',1.5,'W',1.,'schmidt #',IER,nbhelp)
            SCHMT(ICTM,ICFD)=VAL
          ELSEIF(ISCHM.EQ.3)THEN
            ISCHMT(ICTM,ICFD)=0; SCHMT(ICTM,ICFD)=1.0
            IF(JHUMINDX(ICFD).EQ.ICTM)SCHMT(ICTM,ICFD)=0.59
          ENDIF
        endif

C Get source sink information from contaminants network.
        IF(ICTDFAF(ICFD).EQ.1)THEN
          helptopic='cfd_src_editcpl'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL EASKOK('Edit network coupling?',' ',OK,nbhelp)
          if (.not.OK) goto 144
          helptopic='cfd_ctm_network_assoc'
          call gethelptext(helpinsub,helptopic,nbhelp)
          icctm=ictm
          do inctm=1,NCONTM
            WRITE(ITMSS1(ICTM),'(2A,A12)')CHAR(96+ICTM),' ',
     &        CONTMNAM(ICTM)
          enddo
          ITMSS1(NCTM(ICFD)+1) =' ------------------------'
          ITMSS1(NCTM(ICFD)+2) ='? help                   '
          ITMSS1(NCTM(ICFD)+3) ='- none of the above      '
          NITMSS1=NCTM(ICFD)+3
          IIO=-1
          write(outs,'(4a)')'Select a network contaminant that',
     &      ' represents CFD contaminant ',POLNAM(icctm,ICFD),'.'
          call edisp(iuout,outs)
          write(outs,'(3a)')'Which is contaminant ',
     &      POLNAM(icctm,ICFD),'?'
 5264     CALL EMENU(outs,ITMSS1,NITMSS1,IIO)
          IF(IIO.EQ.NITMSS1)THEN
            ICC2NC(icctm,ICFD)=0
          ELSEIF(IIO.EQ.NITMSS1-1)THEN
            CALL PHELPD('CFD contaminant network association',
     &       nbhelp,'-',0,0,IER)
            goto 5264
          ELSEIF(IIO.GE.1.AND.IIO.LE.NCTM(ICFD))THEN
            ICC2NC(icctm,ICFD)=IIO              
          ELSE
            GOTO 5264
          ENDIF

          if (ICC2NC(ictm,ICFD).gt.0) then
            IF(NSPMNO.gt.0)THEN
              inctm=ICC2NC(ictm,ICFD)
              IX=1
              DO ISPMNO=1,NSPMNO
                IF(SSLINK1(ISPMNO,inctm).NE.0)THEN
                  WRITE(ITMSS2(IX),'(A,1X,A)')CHAR(96+IX),
     &              SSNAME(SSLINK1(ISPMNO,inctm))
                  IX=IX+1
                endif
              enddo
              IF (IX.EQ.1) then
                write(outs,'(3a)')
     &            'No sources/sinks defined for network contaminant ',
     &            CONTMNAM(inctm),'.'
                call edisp(iuout,outs)
                SRCE(ISB,ictm,ICFD)=' '
                SRCFRC(ISB,ictm,ICFD)=0.0
              endif
              ITMSS2(IX)=   ' ------------------'
              ITMSS2(IX+1)= '? help'
              ITMSS2(IX+2)= '- none of the above'
              NITMSS2=IX+2
              WRITE(outs,'(3A)')
     &          'Select source/sink for contaminant ',
     &          POLNAM(icctm,ICFD),'.'
              call edisp(iuout,outs)
 5294         IXO=-1
              WRITE(WORD,'(3A)')'Contaminant ',POLNAM(icctm,ICFD),
     &          ' source/sink'
              CALL EMENU(WORD,ITMSS2,NITMSS2,IXO)
              IF(IXO.EQ.NITMSS2)THEN
                SRCE(ISB,ictm,ICFD)=' '
                SRCFRC(ISB,ictm,ICFD)=0.0
              ELSEIF(IXO.EQ.NITMSS2-1)THEN
                CALL PHELPD('Choose source/sink',nbhelp,'-',0,0,IER)
                GOTO 5294
              ELSEIF(IXO.GE.1 .AND. IXO.LE.NITMSS2-3)THEN
                SRCE(ISB,ictm,ICFD)=SSNAME(SSLINK1(IXO,inctm))
                val=1.0
                CALL EASKR(val,'What fraction is injected here?',
     &            '(0.0 - 1.0)',0.0,'F',1.0,'F',1.0,'src frac',IER,
     &            nbhelp)
                SRCFRC(ISB,ictm,ICFD)=val
              ELSE
                GOTO 5294
              ENDIF
            else
              SRCE(ISB,ictm,ICFD)=' '
              SRCFRC(ISB,ictm,ICFD)=0.0
            ENDIF
          else
            SRCE(ISB,ictm,ICFD)=' '
            SRCFRC(ISB,ictm,ICFD)=0.0
          endif
        endif

C Ask for static release rate.
        if (SRCE(ISB,ictm,ICFD).eq.' ') then
          helptopic='cfd_source_boundary_edit'
          call gethelptext(helpinsub,helptopic,nbhelp)
          VAL=VOLPOL(ICTM,ISB,ICFD)
          call EASKR(VAL,'Release rate (kg/s)?',
     &    ' ',0.,'W',10.,'W',1.,'src strength',IER,nbhelp)
          VOLPOL(ICTM,ISB,ICFD)=VAL
        endif

      elseif (INO.eq.(NITMS-1)) then

C Create help for 'sources edit' menu.
        helptopic='cfd_source_boundary_edit'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('sources edit help',nbhelp,'-',0,0,IER)
      elseif (INO.eq.NITMS) then
        return
      else
        goto 7
      endif
      goto 144

      end


C ******************** EDBLK ********************
C Controls the editing of CFD blockages to flow.
C IER=0 indicates no error.

      SUBROUTINE EDBLK(ISB,IER)
#include "building.h"
#include "geometry.h"
#include "cfd.h"
#include "schedule.h"
#include "help.h"

      COMMON/ICFNOD/ICFD,ICP
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      common/KEYVOLS/NVOL(MNZ),IVOLF(MNVLS,MNZ),IVCELLS(MNVLS,MNZ,2),
     &               JVCELLS(MNVLS,MNZ,2),KVCELLS(MNVLS,MNZ,2)
      common/KEYVOLN/VOLNAME(MNVLS,MNZ),VCsurf(MNVLS,MNZ),
     &               BLKSURF(MNVLS,MNZ,6)
      character VOLNAME*12, VCsurf*12, BLKSURF*12
      common/KEYVDAT/IVTYPE(MNVLS,MNZ),VOLTemp(MNVLS,MNZ),
     &          VOLHeat(MNVLS,MNZ),IVConfl(MNVLS,MNZ),VOLHum(MNVLS,MNZ),
     &          VOLCO2(MNVLS,MNZ),VOLVel(MNVLS,MNZ),VOLDir(MNVLS,MNZ,2),
     &          VOLArea(MNVLS,MNZ),VOLPres(MNVLS,MNZ),
     &          VOLPol(MCTM,MNVLS,MNZ)
      COMMON/GEOM/XP(ntcelx),YP(ntcely),ZP(ntcelz),
     1            DXEP(ntcelx),DXPW(ntcelx),DYNP(ntcely),DYPS(ntcely),
     2            DZHP(ntcelz),DZPL(ntcelz),
     3            SEW(ntcelx),SNS(ntcely),SHL(ntcelz),
     4            XU(ntcelx),YV(ntcely),ZW(ntcelz)
      common/blksso/NBLK(MNZ),INBLK(MNVLS,MNZ),NSSO(MNZ),
     &          INSSO(MNVLS,MNZ),BLKTEMP(MNVLS,6)
      COMMON/CFDVIS/HAS_GEOM,ISHSB,ISHAO,IFACES,ISHBLK,ISHSRC,ISHGEO,
     &              INITD
      logical HAS_GEOM
      character INITD*6
      common/KEYCASGN/IDcasgn(MNVLS,MNZ),Fcasgn(MNVLS,MNZ)
      common/dynamico/isdynamicocup(MCOM)
      common/cfdfil/LCFD(MCOM),IFCFD(MCOM)
      character LCFD*72
      
      character ITMSS(20)*50
      character ltmp*40,t12*12,d12*12,t20*20

      logical ERROR,isdynamic
      integer NITMS,INO ! max items and current menu item

      helpinsub='edcfd'  ! set for subroutine
      helptopic='cfd_blockage_menu'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Adjust each cell by -1 as dfs adds fictitious cells around the domain.
      Ii=IVCELLS(ISB,ICFD,1)-1; Iif=IVCELLS(ISB,ICFD,2)-1
      Ji=JVCELLS(ISB,ICFD,1)-1; Jf=JVCELLS(ISB,ICFD,2)-1
      Ki=KVCELLS(ISB,ICFD,1)-1; Kf=KVCELLS(ISB,ICFD,2)-1

C Check if new blockage.
      if (Ii.lt.0.or.Iif.lt.0.or.
     &    Ji.lt.0.or.Jf.lt.0.or.
     &    Ki.lt.0.or.Kf.lt.0) then 
        t20='Blockage'
        if(IVOLF(ISB,ICFD).ne.0)IFACE=IVOLF(ISB,ICFD)
        call PIKCELS(Ii,Iif,Ji,Jf,Ki,Kf,IFACE,IER)
        IVOLF(ISB,ICFD)=IFACE
        IVCELLS(ISB,ICFD,1)=Ii+1; IVCELLS(ISB,ICFD,2)=Iif+1
        JVCELLS(ISB,ICFD,1)=Ji+1; JVCELLS(ISB,ICFD,2)=Jf+1
        KVCELLS(ISB,ICFD,1)=Ki+1; KVCELLS(ISB,ICFD,2)=Kf+1
        call NEW2OLD
      endif

C Set boundary type string.
    5 ltmp='  '
      if (IVTYPE(ISB,ICFD).eq.30) then
        ltmp='Blockage - heat flux'
      elseif (IVTYPE(ISB,ICFD).eq.31) then
        ltmp='Blockage - temperature'
      elseif (IVTYPE(ISB,ICFD).eq.32) then
        ltmp='Blockage - surface'
      elseif (IVTYPE(ISB,ICFD).eq.33) then
        ltmp='Blockage - surfaces'
      elseif (IVTYPE(ISB,ICFD).eq.34) then
        ltmp='Blockage - casual gain'
      elseif (IVTYPE(ISB,ICFD).eq.35) then
        ltmp='Blockage - dynamic person'
      endif

C Create a menu showing definitions for different conflation schemes.
      xd=XU(Iif+2)-XU(Ii+1)  ! need to add 1 to indicies
      yd=YV(Jf+2)-YV(Ji+1)
      zd=ZW(Kf+2)-ZW(Ki+1)
      write (ITMSS(1),'(a,a)')   'a name: ',VOLNAME(ISB,ICFD)
      write (ITMSS(2),'(3a)')   'b type: ',ltmp
      write (ITMSS(3),'(a)')     ' -----------------------------'
      write (ITMSS(4),'(a,a)')   '         Is  If  Js  Jf  Ks  Kf '
      write (ITMSS(5),'(a,6(i4))') 'c cells:',Ii,Iif,Ji,Jf,Ki,Kf
      write (ITMSS(6),'(a)')     ' -----------------------------'
      write (ITMSS(7),'(a,f6.3,a,f6.3,a,f5.3)') '  Z cords: ',XU(Ii+1),
     &                  ' to ',XU(Iif+2),' delta ',xd
      write (ITMSS(8),'(a,f6.3,a,f6.3,a,f5.3)') '  Y cords: ',YV(Ji+1),
     &                  ' to ',YV(Jf+2),' delta ',yd
      write (ITMSS(9),'(a,f6.3,a,f6.3,a,f5.3)') '  Z cords: ',ZW(Ki+1),
     &                  ' to ',ZW(Kf+2),' delta ',zd
      write(ITMSS(10),'(a)')     ' -----------------------------'
      if (IVTYPE(ISB,ICFD).eq.30) then
        write(ITMSS(11),'(a,f6.2)')'e heat flux (W/m^2):',
     &                             VOLHeat(ISB,ICFD)
        iitms=11
      elseif (IVTYPE(ISB,ICFD).eq.31) then
        write(ITMSS(11),'(a,f6.2)')'e temperature (C):',
     &                             VOLTemp(ISB,ICFD)
        iitms=11
      elseif (IVTYPE(ISB,ICFD).eq.32) then
        write(ITMSS(11),'(2a)')'e coupled to surface: ',
     &                             VCsurf(ISB,ICFD)
        iitms=11
      elseif (IVTYPE(ISB,ICFD).eq.33) then
        write(ITMSS(11),'(a)')' Coupled surfaces'
        write(ITMSS(12),'(2a)')'f east face: ',BLKSURF(ISB,ICFD,1)
        write(ITMSS(13),'(2a)')'g west face: ',BLKSURF(ISB,ICFD,2)
        write(ITMSS(14),'(2a)')'h north face: ',BLKSURF(ISB,ICFD,3)
        write(ITMSS(15),'(2a)')'i south face: ',BLKSURF(ISB,ICFD,4)
        write(ITMSS(16),'(2a)')'j high face: ',BLKSURF(ISB,ICFD,5)
        write(ITMSS(17),'(2a)')'k low face: ',BLKSURF(ISB,ICFD,6)
        iitms=17
      elseif (IVTYPE(ISB,ICFD).eq.34) then
        write(ITMSS(11),'(a,i1)')'e gain index: ',IDcasgn(ISB,ICFD)
        write(ITMSS(12),'(a,f4.2)')'f gain fraction: ',Fcasgn(ISB,ICFD)
        iitms=12
      elseif (IVTYPE(ISB,ICFD).eq.35) then
        write(ITMSS(11),'(2a)')'e occupant type: ',BLKSURF(ISB,ICFD,1)
        write(ITMSS(12),'(2a)')'f occupant part: ',BLKSURF(ISB,ICFD,2)
        iitms=12
      endif
      ITMSS(iitms+1) =' -----------------------------'
      ITMSS(iitms+2)='? Help'
      ITMSS(iitms+3)='- exit'
      NITMS=iitms+3

      IF(MMOD.EQ.8)THEN
        do 639 I=1,NBLK(ICFD)
          if (ISB.eq.INBLK(I,ICFD)) then
            ISHBLK_TMP=ISHBLK
            ISHGEO_TMP=ISHGEO
            ISHBLK=I
            ISHGEO=0
            CALL redraw(IER)
            ISHBLK=ISHBLK_TMP
            ISHGEO=ISHGEO_TMP
          endif
639     continue   
      ENDIF

C Help text for this menu.
      helptopic='cfd_blockage_menu'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Display menu.
      INO=-1
      CALL EMENU('Blockage edit',ITMSS,NITMS,INO)

C Get boundary name.
      if (INO.eq.1) then
        t12=VOLNAME(ISB,ICFD)
        d12=' '
        call EASKS(t12,'Blockage name? ','  ',12,d12,'blk name',
     &    IER,nbhelp)
        if (IER.eq.0) then
          call st2name(t12,d12)
          VOLNAME(ISB,ICFD)=d12
        endif

C Set blockage type.
      elseif (INO.eq.2) then
        helptopic='blockage_type'
        call gethelptext(helpinsub,helptopic,nbhelp)

        if (IFCFD(ICFD).eq.0) then
          CALL EASKABC(' ','Select blockage type','heat','temperature',
     &      'cancel',IW,nbhelp)
          if (IW.lt.3) IVTYPE(ISB,ICFD)=29+IW
        else
          
C Check if dynamic occupants are available.
          if (isdynamicocup(ICP).gt.0) then
            t20='dynamic person'
          else
            t20='-'
          endif

  82      CALL EASKATOG(' ','Select blockage type','heat','temperature',
     &      'surface','surfaces','casual gain',t20,'cancel',IW,
     &      nbhelp)

          if (IW.eq.6 .and. isdynamicocup(ICP).eq.0) goto 82

C Set defaults if the type has changed.
          if (29+IW.ne.IVTYPE(ISB,ICFD)) then
            if (IW.eq.1) then
              VOLHEAT(ISB,ICFD) = 0.0
            elseif (IW.eq.2) then
              VOLTemp(ISB,ICFD) = 0.0
            elseif (IW.eq.3) then
              VCsurf(ISB,ICFD) = 'NONE        '
            elseif (IW.eq.4) then
              do i=1,6
                BLKSURF(ISB,ICFD,i) = 'NONE        '
              enddo
            elseif (IW.eq.5) then
              IDcasgn(ISB,ICFD) = 1
              Fcasgn(ISB,ICFD) = 1.0
            elseif (IW.eq.6) then
              BLKSURF(ISB,ICFD,1) = 'woman       ' ! use BLKSURF to store occupant charateristics
              BLKSURF(ISB,ICFD,2) = 'whole_body  '
            endif

C Also make sure we clear IDcasgn if necessary.
            if (IVTYPE(ISB,ICFD).eq.34) then
              IDcasgn(ISB,ICFD)=0
            endif            
          endif
          
          if (IW.lt.7) IVTYPE(ISB,ICFD)=29+IW
        endif

      elseif (INO.eq.5) then
 79     t20='Blockage'
        if(IVOLF(ISB,ICFD).ne.0)IFACE=IVOLF(ISB,ICFD)
        call PIKCELS(Ii,Iif,Ji,Jf,Ki,Kf,IFACE,IER)
        IVOLF(ISB,ICFD)=IFACE
        IVCELLS(ISB,ICFD,1)=Ii+1; IVCELLS(ISB,ICFD,2)=Iif+1
        JVCELLS(ISB,ICFD,1)=Ji+1; JVCELLS(ISB,ICFD,2)=Jf+1
        KVCELLS(ISB,ICFD,1)=Ki+1; KVCELLS(ISB,ICFD,2)=Kf+1
        call NEW2OLD

C Check for illegal volume (vol must be 1 cells thick in all directions).
        ERROR=.false.
        if (IVCELLS(ISB,ICFD,1).gt.IVCELLS(ISB,ICFD,2)) then
          call usrmsg('Blockage cell order reversed',
     &                'in the x direction.','W')
          ERROR=.true.
        endif
        if (JVCELLS(ISB,ICFD,1).gt.JVCELLS(ISB,ICFD,2)) then
          call usrmsg('Blockage cell order reversed',
     &                'in the y direction.','W')
          ERROR=.true.
        endif
        if (KVCELLS(ISB,ICFD,1).gt.KVCELLS(ISB,ICFD,2)) then
          call usrmsg('Blockage cell order reversed',
     &                'in the z direction.','W')
          ERROR=.true.
        endif
        if (ERROR) goto 79

      elseif (INO.eq.11) then
        if (IVTYPE(ISB,ICFD).eq.30) then
          VAL=VOLHeat(ISB,ICFD)
          call EASKR(VAL,'Heat flux (W/m^2) on blockage surfaces?',
     &      ' ',-1000.,'W',1000.,'W',10.,'block heat flux',IER,nbhelp)
          if (IER.eq.0) VOLHeat(ISB,ICFD)=VAL
        elseif (IVTYPE(ISB,ICFD).eq.31) then
          VAL=VOLTemp(ISB,ICFD)
          call EASKR(VAL,'Temperature (C) on blockage surfaces?',
     &      ' ',-100.,'W',100.,'W',20.,'block temperature',IER,nbhelp)
          if (IER.eq.0) VOLTemp(ISB,ICFD)=VAL
        elseif (IVTYPE(ISB,ICFD).eq.32) then
          CALL EASKSUR(ICP,IS,'-','Select a surface.',' ',IER)
          if (IER.eq.0 .and. IS.gt.0) VCsurf(ISB,ICFD)=SNAME(ICP,IS)
        elseif (IVTYPE(ISB,ICFD).eq.34) then
          ival=IDcasgn(ISB,ICFD)
          call PIKCGTYP(ICP,ival,nbhelp)
          IDcasgn(ISB,ICFD)=ival
        elseif (IVTYPE(ISB,ICFD).eq.35) then
          i=2
          CALL EASKABC(' ','Occupant type','man','woman','child',i,
     &      nbhelp)
          if (i.eq.1) then
            BLKSURF(ISB,ICFD,1) = 'man'
          elseif (i.eq.2) then
            BLKSURF(ISB,ICFD,1) = 'woman'
          elseif (i.eq.3) then
            BLKSURF(ISB,ICFD,1) = 'child'
          endif
        endif

      elseif (INO.eq.12) then
        if (IVTYPE(ISB,ICFD).eq.33) then
          CALL EASKSUR(ICP,IS,'-','Select a surface.',' ',IER)
          if (IER.eq.0) then
            if (IS.eq.0) then
              BLKSURF(ISB,ICFD,1)='NONE        '
            else
              BLKSURF(ISB,ICFD,1)=SNAME(ICP,IS)
            endif
          endif
        elseif (IVTYPE(ISB,ICFD).eq.34) then
          val=Fcasgn(ISB,ICFD)
          call EASKR(val,'What fraction of casual gain is generated? ',
     &      ' ',0.,'W',1.,'W',1.,'casgn fraction',IER,nbhelp)
          if (IER.eq.0) FCasgn(ISB,ICFD)=val
        elseif (IVTYPE(ISB,ICFD).eq.35) then
          if (isdynamicocup(ICP).eq.4) then
            ival=7
            call easkatog('What body part does this represent?',' ',
     &        'head','trunk','left arm','right_arm','left leg',
     &        'right leg','whole body',ival,nbhelp)
            if (ival.eq.1) then
              BLKSURF(ISB,ICFD,2)='head'
            elseif (ival.eq.2) then
              BLKSURF(ISB,ICFD,2)='trunk'
            elseif (ival.eq.3) then
              BLKSURF(ISB,ICFD,2)='left_arm'
            elseif (ival.eq.4) then
              BLKSURF(ISB,ICFD,2)='right_arm'
            elseif (ival.eq.5) then
              BLKSURF(ISB,ICFD,2)='left_leg'
            elseif (ival.eq.6) then
              BLKSURF(ISB,ICFD,2)='right_leg'
            elseif (ival.eq.7) then
              BLKSURF(ISB,ICFD,2)='whole_body'
            endif
          else
            call usrmsg('Segmented occupant model not in use.',
     &        'Option not available.','w')
          endif
        endif
C Help.
      elseif (INO.eq.(NITMS-1)) then
        helptopic='cfd_blockage_menu'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('blocakage edit',nbhelp,'-',0,0,IER)

C Exit menu.
      elseif (INO.eq.NITMS) then
        return

C Select coupled surface for any face.
      elseif (INO.gt.12.and.INO.le.17) then
        if (IVTYPE(ISB,ICFD).eq.33) then
          CALL EASKSUR(ICP,IS,'-','Select a surface.',' ',IER)
          if (IER.eq.0) then
            if (IS.eq.0) then
              BLKSURF(ISB,ICFD,INO-11)='NONE        '
            else
              BLKSURF(ISB,ICFD,INO-11)=SNAME(ICP,IS)
            endif
          endif
        endif
      endif

      goto 5
      end


C ******************** HANDSHK ********************
C Controls the editing of the handshaking mechanism.

      SUBROUTINE HANDSHK(IHST,IER)
#include "building.h"
#include "cfd.h"

      COMMON/ICFNOD/ICFD,ICP
      common/METHDS/ITURB(MNZ),IBUOY(MNZ)

      dimension IHSID(13)
      character ITMSS(13)*60
      integer NITMS,INO ! max items and current menu item

C Set up menu - display only applicable handshaking mechanisms based on 
C current turbulence modelling choice.
C Return if not k-e or MIT.
      IER=0
      if (ITURB(ICFD).ne.1.AND.ITURB(ICFD).ne.3) then
        call usrmsg('Building surface boundary conditions are only',
     &             'applicable for k-e or MIT turbulence models. ','W')
        IER=1
        return
      endif
      ITMSS(1)=   'Direction|Wall function|HTCs / Qsurf     |Tref'
      if (ITURB(ICFD).eq.1) then
        ITMSS(2)= 'a One-way|log-law n/a  |CFD              |n/a'
        IHSID(2)=1
        ITMSS(3)= 'b One-way|Yuan    n/a  |CFD              |n/a'
        IHSID(3)=3
        ITMSS(4)= 'c One-way|Yuan    n/a  |A*HC*(Tsurf-Tref)|BSim '
        IHSID(4)=4
        ITMSS(5)= 'd One-way|Yuan    n/a  |A*HC*(Tsurf-Tref)|CFD ave.'
        IHSID(5)=5
        ITMSS(6)= 'e One-way|log-law n/a  |A*HC*(Tsurf-Tref)|BSim'
        IHSID(6)=6
        ITMSS(7)= 'f One-way|log-law n/a  |A*HC*(Tsurf-Tref)|CFD ave.'
        IHSID(7)=7
        ITMSS(8)= 'g One-way|log-law local|A*HC*(Tsurf-Tref)|CFD local'
        IHSID(8)=8
        ITMSS(9)= 'h Two-way|log-law n/a  |CFD              |n/a'
        IHSID(9)=9
        ITMSS(10)='i Two-way|Yuan    n/a  |CFD              |n/a'
        IHSID(10)=11
        ITMSS(11)='j Two-way|Yuan    local|A*HC*(Tsurf-Tref)|CFD local'
        IHSID(11)=12
        ITMSS(12)='k Two-way|log-law n/a  |CFD              |n/a'
        IHSID(12)=13
        ITMSS(12)='l Two-way|log-law local|A*HC*(Tsurf-Tp)  |local'
        IHSID(12)=14
        NITMS=12
      else
        ITMSS(2)= 'a One-way|n/a n/a      |CFD              |n/a'
        IHSID(2)=2
        ITMSS(3)= 'b Two-way|n/a n/a      |CFD              |n/a'
        IHSID(3)=10
        NITMS=3
      endif

C Display menu and then decode returned value.
      CALL EMENU('Handshaking mechanism',ITMSS,NITMS,INO)
      if (INO.eq.0) then
        ier=1
        return
      else
        IHST=IHSID(INO)
      endif

      return
      end

C ******************** PIKCELS ********************
C Controls the editing of the cells defining a boundary region.

      SUBROUTINE PIKCELS(Ii,Iif,Ji,Jf,Ki,Kf,IFACE,IER)
#include "building.h"
#include "cfd.h"
#include "help.h"

      integer iCountWords

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      common/grdmax/NTCX,NTCY,NTCZ

C XP YP ZP are the centre of the cells.
C XU YV ZW are the start of the cells.
      COMMON/GEOM/XP(ntcelx),YP(ntcely),ZP(ntcelz),
     1            DXEP(ntcelx),DXPW(ntcelx),DYNP(ntcely),DYPS(ntcely),
     2            DZHP(ntcelz),DZPL(ntcelz),
     3            SEW(ntcelx),SNS(ntcely),SHL(ntcelz),
     4            XU(ntcelx),YV(ntcely),ZW(ntcelz)
      COMMON/ALL/NI,NJ,NK,NIM1,NJM1,NKM1,NIM2,NJM2,NKM2

      character s30*30,dflt*30,lface*7,t30*30
      character outs*124
      integer IFACE   ! for radio button

      helpinsub='edcfd'  ! set for subroutine

 5    IER=0
      t30='  '
      
C Ask for face first if necessary.
      helptopic='cfd_cell_selection'
      call gethelptext(helpinsub,helptopic,nbhelp)
      if (IFACE.eq.1) then
        lface='West'
      elseif (IFACE.eq.2) then
        lface='East'
      elseif (IFACE.eq.3) then
        lface='South'
      elseif (IFACE.eq.4) then
        lface='North'
      elseif (IFACE.eq.5) then
        lface='Low'
      elseif (IFACE.eq.6) then
        lface='High'
      else
        lface='UNKNOWN'
      endif
      if (IFACE.lt.7) then
        write (t30,'(2a)') 'Current face: ',lface
        IFACE=1
        CALL EASKATOG('Boundary face is on which side?',t30,'West',
     &    'East','South','North','Low','High',' ',IFACE,nbhelp)
      endif
      IONE=1

C Remind the user of the cell coordinates for the particular
C fact they they selected.
      XZ=0.; YZ=0.; ZZ=0.
      if (IFACE.eq.1.or.IFACE.eq.2) then

C List out the Y-Z coordinates for west face and east face.
        call edisp(iuout,'Grid in the Y axis... ')
        call edisp(iuout,'Cell index, Y start & end,   distance(m)')
        DO 102 I=2,(NJ-1)
          yd=YV(I+1)-YV(I)
          write(outs,'(i7,3f10.4)') I-1,YV(I)+YZ,YV(I+1)+YZ,yd 
          call edisp(iuout,outs)
  102   CONTINUE
        call edisp(iuout,' ')
        call edisp(iuout,'Grid in the Z axis... ')
        call edisp(iuout,'Cell index, Z start & end,   distance(m)')
        DO 103 I=2,(NK-1)
          zd=ZW(I+1)-ZW(I)
          write(outs,'(i7,3f10.4)') I-1,ZW(I)+ZZ,ZW(I+1)+ZZ,zd
          call edisp(iuout,outs)
  103   CONTINUE
      elseif (IFACE.eq.3.or.IFACE.eq.4) then

C List out the X-Z coordinates for south and north face.
        call edisp(iuout,'Grid in the X axis... ')
        call edisp(iuout,'Cell index, X start & end,   distance(m)')
        DO 106 I=2,(NI-1)
          xd=XU(I+1)-XU(I)
          write(outs,'(i7,3f10.4)') I-1,XU(I)+XZ,XU(I+1)+XZ,xd
          call edisp(iuout,outs)
  106   CONTINUE
        call edisp(iuout,' ')
        call edisp(iuout,'Grid in the Z axis... ')
        call edisp(iuout,'Cell index, Z start & end,   distance(m)')
        DO 107 I=2,(NK-1)
          zd=ZW(I+1)-ZW(I)
          write(outs,'(i7,4f10.4)') I-1,ZW(I)+ZZ,ZW(I+1)+ZZ,zd
          call edisp(iuout,outs)
  107   CONTINUE
      elseif (IFACE.eq.5.or.IFACE.eq.6) then

C List out X and Y cell coordinates for low and high face.
        call edisp(iuout,'Grid in the X axis... ')
        call edisp(iuout,'Cell index, X start & end,   distance(m)')
        DO 110 I=2,(NI-1)
          xd=XU(I+1)-XU(I)
          write(outs,'(i7,3f10.4)') I-1,XU(I)+XZ,XU(I+1)+XZ,xd
          call edisp(iuout,outs)
  110   CONTINUE
        call edisp(iuout,'  ')
        call edisp(iuout,'Grid in the Y axis... ')
        call edisp(iuout,'Cell index, Y start & end,   distance(m)')
        DO 111 I=2,(NJ-1)
          yd=YV(I+1)-YV(I)
          write(outs,'(i7,3f10.4)') I-1,YV(I)+YZ,YV(I+1)+YZ,yd 
          call edisp(iuout,outs)
  111   CONTINUE
      endif

C Setup Ii and If and dflt and t30 for each of the faces.
      if (IFACE.eq.1) then

C West face.
        Ii=1; Iif=1
        write(dflt,'(6i4)') Ii,Iif,IONE,NTCY,IONE,NTCZ
        t30='(current face: west)'
      elseif (IFACE.eq.2) then

C East face.
        Ii=NTCX; Iif=NTCX
        write(dflt,'(6i4)') Ii,Iif,IONE,NTCY,IONE,NTCZ
        t30='(current face: east)'
      elseif (IFACE.eq.3) then

C South face.
        Ji=1; Jf=1
        write(dflt,'(6i4)') IONE,NTCX,Ji,Jf,IONE,NTCZ
        t30='(current face: south)'
      elseif (IFACE.eq.4) then

C North face.
        Ji=NTCY; Jf=NTCY
        write(dflt,'(6i4)') IONE,NTCX,Ji,Jf,IONE,NTCZ
        t30='(current face: north)'
      elseif (IFACE.eq.5) then

C Low face.
        Ki=1; Kf=1
        write(dflt,'(6i4)') IONE,NTCX,IONE,NTCY,Ki,Kf
        t30='(current face: low)'
      elseif (IFACE.eq.6) then

C High face.
        Ki=NTCZ; Kf=NTCZ
        write(dflt,'(6i4)') IONE,NTCX,IONE,NTCY,Ki,Kf
        t30='(current face: high)'
      else

C Unknown face.
        write(dflt,'(6i4)') IONE,NTCX,IONE,NTCY,IONE,NTCZ
        t30='(current face: UNKNOWN)'
      endif
      if (Ii.lt.0.or.Iif.lt.0.or.
     &    Ji.lt.0.or.Jf.lt.0.or.
     &    Ki.lt.0.or.Kf.lt.0) then 
        s30=dflt
      else

C Ask for cells.
        write(s30,'(6i4)') Ii,Iif,Ji,Jf,Ki,Kf
      endif
 10   call EASKS(s30,'Boundary cells (Ii If Ji Jf Ki Kf)?',
     &  t30,30,dflt,'bndry cells',IER,nbhelp)
      NITMS = iCountWords(s30)
      if (NITMS.ne.6) goto 10

C Read six cell locations.
      K=0
      call EGETWI(s30,K,IVAL,1,NTCX,'F','x cells start',IER)
      if (IER.eq.0) Ii=IVAL
      call EGETWI(s30,K,IVAL,1,NTCX,'F','x cells end',IER)
      if (IER.eq.0) Iif=IVAL
      call EGETWI(s30,K,IVAL,1,NTCY,'F','y cells start',IER)
      if (IER.eq.0) Ji=IVAL
      call EGETWI(s30,K,IVAL,1,NTCY,'F','y cells end',IER)
      if (IER.eq.0) Jf=IVAL
      call EGETWI(s30,K,IVAL,1,NTCZ,'F','z cells start',IER)
      if (IER.eq.0) Ki=IVAL
      call EGETWI(s30,K,IVAL,1,NTCZ,'F','z cells end',IER)
      if (IER.eq.0) Kf=IVAL

      if (IER.ne.0) goto 10
      
C Check for incompatable face/ cell definition
      if (IFACE.eq.1) then
        if (Ii.ne.1.or.Iif.ne.1) IER=2
      elseif (IFACE.eq.2) then
        if (Ii.ne.NTCX.or.Iif.ne.NTCX) IER=2
      elseif (IFACE.eq.3) then
        if (Ji.ne.1.or.Jf.ne.1) IER=2
      elseif (IFACE.eq.4) then
        if (Ji.ne.NTCY.or.Jf.ne.NTCY) IER=2
      elseif (IFACE.eq.5) then
        if (Ki.ne.1.or.Kf.ne.1) IER=2
      elseif (IFACE.eq.6) then
        if (Ki.ne.NTCZ.or.Kf.ne.NTCZ) IER=2
      endif
      if (IER.ne.0) then
        call usrmsg('Your cells are not compatible with the',
     &              'chosen face.  Please redefine.','W') 
        goto 5
      endif

      return
      end

C ******************** PIKFACE ********************
C Controls the editing of the face of the boundary region.
C Check which two start/end locations are the same. Keeps
C count of number of matches.

      SUBROUTINE PIKFACE(BTYPE,BNAME,Ii,Iif,Ji,Jf,Ki,Kf,IFACE,IER)
#include "help.h"

      common/grdmax/NTCX,NTCY,NTCZ

      character BTYPE*20,BNAME*12

      helpinsub='edcfd'  ! set for subroutine

 5    IER=0

C Check that face is needed.
      if (IFACE.eq.8.or.IFACE.eq.9) return

C Try to guess which face.
      IMTCH=0
      if (Ii.eq.Iif) then
        if (Ii.eq.1) then

C West face.
          IMTCH=IMTCH+1; IFACE=1
        elseif (Ii.eq.NTCX) then

C East face.
          IMTCH=IMTCH+1; IFACE=2
        endif
      endif
      if (Ji.eq.Jf) then
        if (Ji.eq.1) then

C South face.
          IMTCH=IMTCH+1; IFACE=3
        elseif (Ji.eq.NTCY) then

C North face.
          IMTCH=IMTCH+1; IFACE=4
        endif
      endif
      if (Ki.eq.Kf) then
        if (Ki.eq.1) then

C Base face.
          IMTCH=IMTCH+1; IFACE=5
        elseif (Ki.eq.NTCZ) then

C Top face.
          IMTCH=IMTCH+1; IFACE=6
        endif
      endif
      helptopic='cfd_face_selection'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Check for conflicts.
      if (IMTCH.eq.0) then
        call usrmsg ('Cells must be at a boundary face.','  ','W')
        call PIKCELS(Ii,Iif,Ji,Jf,Ki,Kf,IFACE,IER)
      elseif (IMTCH.gt.1) then
        call usrmsg ('Cannot resolve boundary face.',
     &               'Please choose from the following list.','W')
        IFACE=1
        CALL EASKATOG('Boundary face is on which side?',' ','West',
     &    'East','South','North','Low','High',' ',IFACE,nbhelp)
      endif

C Check for incompatable face/ cell definition.
      if (IFACE.eq.1) then
        if (Ii.ne.1.or.Iif.ne.1) IER=2
      elseif (IFACE.eq.2) then
        if (Ii.ne.NTCX.or.Iif.ne.NTCX) IER=2
      elseif (IFACE.eq.3) then
        if (Ji.ne.1.or.Jf.ne.1) IER=2
      elseif (IFACE.eq.4) then
        if (Ji.ne.NTCY.or.Jf.ne.NTCY) IER=2
      elseif (IFACE.eq.5) then
        if (Ki.ne.1.or.Kf.ne.1) IER=2
      elseif (IFACE.eq.6) then
        if (Ki.ne.NTCZ.or.Kf.ne.NTCZ) IER=2
      endif
      if (IER.ne.0) then
        call usrmsg('Your cells are not compatible with the',
     &              'chosen face.  Please redefine.','W') 
        goto 5
      endif

      return
      end

C ******************** TOPBOT ********************
C Define upper and lower curvilinear x-section for use
C in grid generation.

      SUBROUTINE TOPBOT(ICOMP,IER)
#include "building.h"
#include "geometry.h"
#include "prj3dv.h"
#include "help.h"

      integer iCountWords

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)

C Curvilinear boundary vertex numbers.
      common/CLVTXNO/ivxhigh(MV),ivxlow(MV),ihigh,ilow

      logical clkok,found

      DIMENSION COG1(3),COG2(3),JJVN(MV)
      integer ISTRW

      CHARACTER HOLD*96,outs*124
#ifdef OSI
      integer iix,iiy,iixx,iiyy,ixd,iyd,iik
#else
      integer*8 iix,iiy,iixx,iiyy,ixd,iyd,iik
#endif

      helpinsub='edcfd'  ! set for subroutine

C Tell the user the rules of selection.
      ier=0
      helptopic='cfd_curvilinear_grid'
      call gethelptext(helpinsub,helptopic,nbhelp)
      CALL PHELPD('gen start popup',nbhelp,'-',0,0,IER)

C Redraw the zone with vertices.
      MODIFYVIEW=.TRUE.
      MODBND=.TRUE.
      CALL INLNST(1)
      ITVNO=0
      itsnm=0
      nzg=1
      nznog(1)=ICOMP
      izgfoc=ICOMP
      CALL ADJVIEW(IER)

C Ask for and display the top defining vertices (west to east).
C Allow user to type in the list of vertices as a string.  The
C number of items is the number of vertices and then parse out
C the individual vertex numbers from the list.
   41 HOLD=' '
      if(MMOD.ne.8)then
        CALL EASKS(HOLD,'Give high vertices (west to east)','  ',
     &    72,' ','associated vertices',IER,nbhelp)
      else
        ISTRW=72
        CALL EASKSCMD(HOLD,'Give high vertices (west to east)',
     &   ' ','via mouse', clkok,ISTRW,' 1 2 3','assoc vert+mouse',
     &   IER,nbhelp)
        if(clkok)then
          ijvn=0
          call edisp(iuout,
     &        'Select points via cursor...type `e` to finish.')
  46      CALL trackview(iik,iixx,iiyy)
          if(iik.eq.69.or.iik.eq.101)goto 47
          found=.false.
          do 45 i=1,NTV
            COG1(1)=X(I); COG1(2)=Y(I); COG1(3)=Z(I)
            CALL VECTRN(COG1,TSMAT,COG2,IER)
            call u2pixel(COG2(1),COG2(2),iix,iiy)
            ixd=iix-iixx; iyd=iiy-iiyy
            if(abs(ixd).lt.5.and.abs(iyd).lt.5)then
              if(found)then
                call edisp(iuout,'Close points...try again.')
                goto 46
              endif
              WRITE(outs,'(a,i5,a,i5,a,i3)')' The point @ x=',iixx,
     &          ' & y=',iiyy,' matches vertex ',i
              call edisp(iuout,outs)
              found=.true.

C << Should check that points are in the same plane and that X is increasing. >>
              ijvn=ijvn+1; jjvn(ijvn)=i
              goto 46
            endif
  45      continue
          if(.NOT.found)goto 46
  47      if(ijvn.lt.2)then
            call edisp(iuout,'Not enough points.')
            goto 41
          endif

C This write assumes not-very-complex zones and surfaces.
C << needs to support more edges >>
          HOLD=' '
          WRITE(HOLD,'(32I3)')(JJVN(J),J=1,ijvn)
          CALL EASKS(HOLD,' Associated high vertices (confirm):',
     &      '  ',96,' ','associated vertices',IIER,nbhelp)
        endif
      endif
      NV = iCountWords(HOLD)
      K=0
      DO 94 J=1,NV
        CALL EGETWI(HOLD,K,JV,1,NTV,'W','vertex list',IER)
        ivxhigh(J)=JV
   94 CONTINUE
      ihigh=NV

C Ask for and display the bottom defining vertices (west to east).
   51 HOLD=' '
      if(MMOD.ne.8)then
        CALL EASKS(HOLD,' Give low vertices (west to east)','  ',
     &    72,' ','associated vertices',IER,nbhelp)
      else
        ISTRW=72
        CALL EASKSCMD(HOLD,' Give low vertices (west to east)',
     &   ' ','via mouse', clkok,ISTRW,' 1 2 3','assoc vert+mouse',
     &   IER,nbhelp)
        if(clkok)then
          ijvn=0
          call edisp(iuout,
     &        'Select points via cursor...type `e` to finish.')
  56      CALL trackview(iik,iixx,iiyy)
          if(iik.eq.69.or.iik.eq.101)goto 57
          found=.false.
          do 55 i=1,NTV
            COG1(1)=X(I); COG1(2)=Y(I); COG1(3)=Z(I)
            CALL VECTRN(COG1,TSMAT,COG2,IER)
            call u2pixel(COG2(1),COG2(2),iix,iiy)
            ixd=iix-iixx; iyd=iiy-iiyy
            if(abs(ixd).lt.5.and.abs(iyd).lt.5)then
              if(found)then
                call edisp(iuout,'Close points...try again.')
                goto 56
              endif
              WRITE(outs,'(a,i5,a,i5,a,i3)')' The point @ x=',iixx,
     &          ' & y=',iiyy,' matches vertex ',i
              call edisp(iuout,outs)
              found=.true.
              ijvn=ijvn+1; jjvn(ijvn)=i
              goto 56
            endif
  55      continue
          if(.NOT.found)goto 56
  57      if(ijvn.lt.2)then
            call edisp(iuout,'Not enough points.')
            goto 51
          endif

C This write statement assumes not-very-complex zones and polygons jjvn. 
          HOLD=' '
          WRITE(HOLD,'(32I3)')(JJVN(J),J=1,ijvn)
          CALL EASKS(HOLD,' Associated low vertices (confirm):',
     &      '  ',96,' ','associated vertices',IIER,nbhelp)
        endif
      endif
      NV = iCountWords(HOLD)
      K=0
      DO 95 J=1,NV
        CALL EGETWI(HOLD,K,JV,1,NTV,'W','vertex list',IER)
        ivxlow(J)=JV
   95 CONTINUE
      ilow=NV
      return
      end

C ******************** GRIDDEF ********************
C Define gridding via regions and specify number of cells
C and cell distribution for each region.

      SUBROUTINE GRIDDEF(IAX,NRG,NTC,TOTDIS,NCEL,REG,plaw,ierr)
#include "building.h"
#include "geometry.h"
#include "cfd.h"
#include "epara.h"
#include "help.h"

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      common/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/MFTRA/IMFTU
      COMMON/SHOUT/ICOUT
      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/ICFNOD/ICFD,ICP

      dimension NCEL(MNREG,MNZ),plaw(MNREG,MNZ)
      dimension NRG(4,MNZ),REG(MNREG,MNZ)

      logical OK,GRMODE

      character ITEM(MNREG+5)*44,KEY*1,HOLD*24,outs*124,TITLE*30

      integer icellt    ! for local editing.
      integer MVERT,INO ! max items and current menu item

#ifdef OSI
      integer iigl,iigr,iigt,iigb,iigw,iigwh
      integer iiw1,iiw2,iiw3,iiw4,iimenu
#else
      integer*8 iigl,iigr,iigt,iigb,iigw,iigwh
      integer*8 iiw1,iiw2,iiw3,iiw4,iimenu
#endif

      helpinsub='edcfd'  ! set for subroutine

C Set additional output units to stdout. Then redirect warning 
C messages to stderr in case of rule script program control.
      IMFTU=IUOUT
      IF(MMOD.EQ.-6)then
        ICOUT=0
      else
        ICOUT=IUOUT
      endif

C Reset the display bounds for grid display.
      GRMODE=.false.
      IF(MMOD.EQ.8)THEN
        call startbuffer()

C Setup and pass in parameters to win3d.
        iiw1=6; iiw2=6; iiw3=2; iiw4=4; iimenu=menuchw
        iigl=igl; iigr=igr; iigt=igt; iigb=igb; iigw=igw; iigwh=igwh
        CALL win3d(iimenu,iiw1,iiw2,iiw3,iiw4,
     &    iigl,iigr,iigt,iigb,iigw,iigwh)
        igl=int(iigl); igr=int(iigr); igt=int(iigt); igb=int(iigb)
        igw=int(iigw); igwh=int(iigwh)
        GRMODE=.true.
      ENDIF

C Reset error flag.
      ierr=0

C Initialise menu size variables based on window size. 
C IVERT is the menu position, MVERT the current number of menu lines.
 5    INO=-4
      MHEAD=5
      MCTL=4
      ILEN=NRG(IAX,ICFD)
      IPACT=CREATE
      CALL EKPAGE(IPACT)

C Check current defined distance and total no of cells.
      CTDIS=0.0
      NTC=0
      do 8 I=1,NRG(IAX,ICFD)
        CTDIS=CTDIS+REG(I,ICFD)
        NTC=NTC+ABS(NCEL(I,ICFD))
 8    continue
      ierr=0
      if (NTC.gt.1) then
        if (abs(CTDIS-TOTDIS).gt.0.001)then
          if (IAX.eq.1) then
            write(outs,'(a,f6.3,a,f6.3,a)') 'The current length ',
     &       CTDIS,' & TOTDIS',TOTDIS,' are not close for X axis.'
          elseif(IAX.eq.2)then
            write(outs,'(a,f6.3,a,f6.3,a)') 'The current length ',
     &        CTDIS,' & TOTDIS',TOTDIS,' are not close for Y axis.'
          elseif(IAX.eq.3)then
            write(outs,'(a,f6.3,a,f6.3,a)') 'The current length ',
     &        CTDIS,' & TOTDIS',TOTDIS,' are not close for Z axis.'
          elseif(IAX.eq.4)then
            write(outs,'(a,f6.3,a,f6.3,a)') 'The current length',
     &        CTDIS,' & TOTDIS',TOTDIS,' are not close for Z eaxis.'
          endif
          call edisp(iuout,outs)
          ierr=1
        endif
      endif

C Draw grid in 1D.
      if (GRMODE) then
        xoff=0.; yoff=0.
        if (IAX.eq.1) then
          TITLE='X-axis gridding'
        elseif (IAX.eq.2) then
          TITLE='Y-axis gridding'
        elseif (IAX.eq.3) then
          TITLE='Z-axis gridding'
        elseif (IAX.eq.4) then
          TITLE='Ze-axis gridding'
        endif
        call VGRID1D(xoff,yoff,TOTDIS,CTDIS,NRG,IAX,ICFD,NCEL,REG,plaw,
     &               TITLE,'Length (m)')
      endif

C Create menu displaying data for all regions.
      write (ITEM(1),'(a,i4)')' Number of regions: ',NRG(IAX,ICFD)
      write (ITEM(2),'(a,f8.3)') ' Total domain length:    ',TOTDIS
      write (ITEM(3),'(a,f8.3)') ' Current defined length: ',CTDIS
      ITEM(4) = ' ------------------------------------'
      ITEM(5) = '   Region  Cells  Length  P-law coeff  sym'
      M=MHEAD
      do 10 L=1,ILEN
        IF(L.GE.IST.AND.(L.LE.(IST+MIFULL)))THEN
          M=M+1
          CALL EMKEY(L,KEY,IER)
          if (NCEL(L,ICFD).lt.0) then
            write(ITEM(M),'(a,2i7,2f9.3,6x,a)')KEY,L,abs(NCEL(L,ICFD)),
     &                                   REG(L,ICFD),plaw(L,ICFD),'yes'
          else
            write(ITEM(M),'(a,2i7,2f9.3,6x,a)')KEY,L,abs(NCEL(L,ICFD)),
     &                                   REG(L,ICFD),plaw(L,ICFD),' no'
          endif
        endif
 10   CONTINUE

C If a long list include page facility text.      
      IF(IPFLG.EQ.0)THEN  
        ITEM(M+1) = ' ------------------------------------'
      ELSE
        WRITE(ITEM(M+1),15)IPM,MPM 
 15     FORMAT   ('0 page: ',I2,' of ',I2,' ---------')
      ENDIF
      ITEM(M+2) = '+ add/delete region'
      ITEM(M+3)  ='? help                              '
      ITEM(M+4)  ='- exit                              '

C Number of actual items displayed.
      MVERT=M+MCTL

C Help text for this menu.
      helptopic='cfd_cells_in_region'
      call gethelptext(helpinsub,helptopic,nbhelp)

      if (IAX.eq.1) then
        CALL EMENU('x-axis gridding',ITEM,MVERT,INO)
      elseif (IAX.eq.2) then
        CALL EMENU('y-axis gridding',ITEM,MVERT,INO)
      elseif (IAX.eq.3) then
        CALL EMENU('z-axis gridding',ITEM,MVERT,INO)
      elseif (IAX.eq.4) then
        CALL EMENU('ze-axis gridding',ITEM,MVERT,INO)
      endif

      if (INO.GT.MHEAD.AND.INO.LT.(MVERT-MCTL+1)) then

C Edit a regions data.
        CALL KEYIND(MVERT,INO,IFOC,IO)
        write (HOLD,'(i4,2f8.3)') abs(NCEL(IFOC,ICFD)),REG(IFOC,ICFD),
     &                            plaw(IFOC,ICFD)
        write(outs,'(2a)')'Please specify: number of cells; length of',
     &                    ' region; and power law coefficient.'

        call EASKS(HOLD,outs,' ',32,' 1 1.0 1.0 ','cell data',
     &    IER,nbhelp)
        K=0
        if (IAX.eq.1) then
          call EGETWI(HOLD,K,ic,1,NTCELX,'F','no of x cells',IIER)
C Here check the total number of cells along x-axis 
          numcel=0 
          do 20 ihelp=1,NRG(1,ICFD)
C Exclude current region from cell counting 
            if(ihelp.ne.IFOC) then 
              numcel=numcel+NCEL(ihelp,ICFD)
            endif
 20       continue
          if((ic+numcel).gt.(NTCELX-2)) then 
            iier=1 
            write(outs,'(a26,i4,a16,i4)')
     &        'The total No of cells now:'
     &        ,ic+numcel,' is bigger than ',NTCELX-2  
            CALL USRMSG(outs,
     &    ' which is the maximum allowed number for x-axis ','W')       
          endif 

        elseif (IAX.eq.2) then
          call EGETWI(HOLD,K,ic,1,NTCELY,'F','no of y cells',IIER)
C Here check the total number of cells along y-axis 
          numcel=0 
          do 30 ihelp=1,NRG(2,ICFD)
C Exclude current region from cell counting 
            if(ihelp.ne.IFOC) then 
              numcel=numcel+NCEL(ihelp,ICFD)
            endif
 30       continue
          if((ic+numcel).gt.(NTCELY-2)) then 
            iier=1 
            write(outs,'(a26,i4,a16,i4)')
     &        'The total No of cells now:'
     &        ,ic+numcel,' is bigger than ',NTCELY-2  
            CALL USRMSG(outs,
     &    ' which is the maximum allowed number for y-axis ','W')       
          endif 

        else
          call EGETWI(HOLD,K,ic,1,NTCELZ,'F','no of z cells',IIER)
C Here check the total number of cells along z-axis 
          numcel=0 
          do 40 ihelp=1,NRG(3,ICFD)
C Exclude current region from cell counting 
            if(ihelp.ne.IFOC) then 
              numcel=numcel+NCEL(ihelp,ICFD)
            endif
 40       continue
          if((ic+numcel).gt.(NTCELZ-2)) then 
            iier=1 
            write(outs,'(a26,i4,a16,i4)')
     &        'The total No of cells now:'
     &        ,ic+numcel,' is bigger than ',NTCELZ-2  
            CALL USRMSG(outs,
     &    ' which is the maximum allowed number for z-axis ','W')       
          endif 
        endif

C Only copy data if no errors.
        if (iier.eq.0) NCEL(IFOC,ICFD)=ic

C If more that one cell then ask if distribution is symmetrical.
        if (NCEL(IFOC,ICFD).gt.1) then
          call EGETWR(HOLD,K,ci,0.0,TOTDIS,'W','length of reg',IIER)
          if (iier.eq.0) REG(IFOC,ICFD)=ci
          call EGETWR(HOLD,K,ci,-99.0,99.0,'F','pwr law coef',IIER)
          if (iier.eq.0) plaw(IFOC,ICFD)=ci
          CALL EASKOK(' ','Symmetrical gridding?',OK,nbhelp)
          if (OK) NCEL(IFOC,ICFD) = -1*NCEL(IFOC,ICFD)
        else
          plaw(IFOC,ICFD)=1.0
        endif

      elseif (INO.eq.(MVERT-3)) then

C Change page.
      elseif (INO.eq.(MVERT-2)) then

C Add delete regions.
        call EASKABC('Region operations:','  ','add region',
     &    'delete region','cancel',IRO,nbhelp)
        if (IRO.eq.1) then
          if (NRG(IAX,ICFD)+1.gt.MNREG) then
            call usrmsg ('Maximum number of regions exceeded',
     &                   'returning to menu','W')
          else
            NRG(IAX,ICFD)=NRG(IAX,ICFD)+1
            IFOC=NRG(IAX,ICFD)

C Ask for length of current region (supply TOTDIS-CTDIS as default)
            CALL EASKABC('Specify region length:',' ','by vertices',
     &                   'by size','cancel',IW,nbhelp)
            if(iw.eq.1)then

C Locate regions by vertices.
              HOLD='  1   2'
              call EASKS(HOLD,'Specify vertex at start & end of region',
     &                   ' ',32,' 1 2 ','vertices',IER,nbhelp)
              K=0
              call EGETWI(HOLD,K,is,1,NTV,'F','vertex',IIER)
              call EGETWI(HOLD,K,ie,1,NTV,'F','vertex',IIER)
              if (IAX.eq.1) then
                REG(IFOC,ICFD) = abs(X(ie)-X(is))
              elseif (IAX.eq.2) then
                REG(IFOC,ICFD) = abs(Y(ie)-Y(is))
              else
                REG(IFOC,ICFD) = abs(Z(ie)-Z(is))
              endif
            elseif(iw.eq.2)then
              VAL=TOTDIS-CTDIS
              CALL EASKR(VAL,' ','Length of current region (m) ? ',
     &          0.0,'F',TOTDIS,'W',1.,'reg length',IER,nbhelp)
              REG(IFOC,ICFD) = VAL
            endif

C Got length of region now ask for no of cells and power law and symmetry.
C Offer the user a cancel option.
            if (IW.ne.3) then
              icell=NCEL(IFOC,ICFD)
              icellt=icell
              CALL EASKI(icellt,' Specify number of cells in region',
     &          ' ',1,'F',NTCELX,'F',10,'single region',IERI,nbhelp)
              if(ieri.eq.-3) then
                goto 5
              else
                icell=icellt
                NCEL(IFOC,ICFD) = icell
              endif

C If the number of cells is greater than 1 then ask for details, otherwise 
C set default size and power law.
              if(NCEL(IFOC,ICFD).gt.1) then
                VAL=1.0
                CALL EASKR(VAL,'  ','Power-law coefficient?',
     &              -4.0,'W',4.0,'W',1.0,'power coeff',IER,nbhelp)
                plaw(IFOC,ICFD)=VAL
                CALL EASKOK(' ','Symmetrical gridding?',OK,nbhelp)
                if(OK) NCEL(IFOC,ICFD) = -1*NCEL(IFOC,ICFD)
              else
                plaw(IFOC,ICFD)=1.0
              endif
            endif
          endif
        elseif (IRO.eq.2) then
          IDEL=NRG(IAX,ICFD)
          CALL EASKI(IDEL,' ','Region to delete?',
     &      0,'F',NRG(IAX,ICFD),'F',1,'del reg',IERI,nbhelp)
          if(ieri.eq.-3) goto 5

          if (IDEL.gt.0) then
            do 100 ID=IDEL,NRG(IAX,ICFD)
              if ((ID+1).le.MNREG) then
                REG(ID,ICFD)=REG(ID+1,ICFD)
                plaw(ID,ICFD)=plaw(ID+1,ICFD)
                NCEL(ID,ICFD)=NCEL(ID+1,ICFD)
              endif
 100        continue
            NRG(IAX,ICFD)=NRG(IAX,ICFD)-1
          endif
        endif
      elseif (INO.eq.(MVERT-1)) then

C Help.
        helptopic='cfd_cells_in_region'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('CFD inputs',nbhelp,'-',0,0,IER)
      elseif (INO.eq.(MVERT)) then
        call startbuffer()
        return
      endif
      goto 5
      end

C ******************** ASKMFNOD ********************
C Asks for one or more mass flow nodes.
C IOPT - restrict node types on display:
C        = 0 - display all nodes;
C        = 1 - display internal nodes only.
C PROMPT1 and PROMPT2 are text prompt strings.
C NH is number of help strings.

      SUBROUTINE ASKMFNOD(IOPT,NNPK,INPK,PROMPT1,PROMPT2,NH)
#include "building.h"
#include "net_flow.h"
#include "net_flow_data.h"

      DIMENSION INPK(MNOD)

      CHARACTER VERT(MNOD)*12
      CHARACTER PROMPT1*72, PROMPT2*72

C Set up array of menu strings.
      do 10 I=1,NNOD
        VERT(I)=NDNAM(I)
 10   continue
      call EPICKS(NNPK,INPK,PROMPT1,PROMPT2,
     &  12,NNOD,VERT,'Node select',IER,NH)

      return
      end

C ******************** ASKMFCON ********************
C Asks for one or more mass flow connections. If INODE 
C is greater than zero then user specified list of connections
C associated with this node. If INODE = zero then allow display
C of all connections in the network. If INODE is less than zero
C then silently return all connections associated with the node
C (absolute value of INODE).

      SUBROUTINE ASKMFCON(INODE,NNPK,ICPK,PROMPT1,PROMPT2)
#include "building.h"
#include "net_flow.h"
#include "net_flow_data.h"

      DIMENSION IPK(MCNN),ICPK(MCNN),ICID(MCNN)

      CHARACTER VRT(MCNN)*43
      CHARACTER PROMPT1*72, PROMPT2*72

C Set up array of menu strings.
      NITMS=0
      do 10 I=1,NCNN
        IP=NODPS(I); IN=NODNE(I); IC=ITPCON(I)
        if (abs(INODE).gt.0) then
          if (abs(INODE).eq.IP.or.abs(INODE).eq.IN) then

C A node has been specified -> only list its connections.
            NITMS=NITMS+1; ICID(NITMS)=I
            write(VRT(NITMS),'(5a)')
     &        NDNAM(IP),'->',NDNAM(IN),' via ',CMNAM(IC)
          endif
        else

C List all connections.
          NITMS=NITMS+1; ICID(NITMS)=I
          write(VRT(NITMS),'(5a)')
     &      NDNAM(IP),'->',NDNAM(IN),' via ',CMNAM(IC)
        endif
 10   continue

      if (INODE.ge.0) then
        if (NITMS.lt.NNPK) NNPK=NITMS
        call EPICKS(NNPK,IPK,PROMPT1,PROMPT2,
     &    43,NITMS,VRT,'Connection selection',IER,0)

C Decode menu picks to connections.
        do 20 I=1,NNPK
          ICPK(I)=ICID(IPK(I))
 20     continue
      else

C Act silently.
        NNPK=NITMS
        do 40 I=1,NNPK
          ICPK(I)=ICID(I)
 40     continue
      endif

      return
      end
