C This file is part of the ESP-r system.
C Copyright Energy Systems Research Unit, University of
C Strathclyde, Glasgow Scotland, 2001.

C ESP-r is free software.  You can redistribute it and/or
C modify it under the terms of the GNU General Public
C License as published by the Free Software Foundation 
C (version 2 or later).

C ESP-r is distributed in the hope that it will be useful
C but WITHOUT ANY WARRANTY; without even the implied
C warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
C PURPOSE. See the GNU General Public License for more
C details.

C You should have received a copy of the GNU General Public
C License along with ESP-r. If not, write to the Free
C Software Foundation, Inc., 59 Temple Place, Suite 330,
C Boston, MA 02111-1307 USA.

C This source file contains:
C CFDCOMP - controls the editing of CFD input files.
C CFDGGRD - define domain gridding.
C CFDSVAR - controls the editing of CFD solution variables.
C CFDEDAT - controls the editing of CFD equations.
C CFDBVAR - controls the editing of CFD boundary variables.
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

C << TODO
C << The introduction of cancel options for easki calls should be
C << extended to ensure that data in a sequence of edits is only
C << instanciated if no cancels are called in the sequence. Currently
C << the code does not re-establish some of the earlier values in
C << a mixed integer and real sequence of edits.

C ************* CFDCOMP 
C CFDCOMP controls the editing of CFD input files.
C ITRU unit number for user output, IUF unit number for CFD input file.
C IER=0 indicates no error.

      SUBROUTINE CFDCOMP(izone,iuf,IER)
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "cfd.h"
C #include "esprdbfile.h"
      
      integer lnblnk  ! function definition

      common/OUTIN/IUOUT,IUIN
      common/gzonpik/izgfoc,nzg,nznog(mcom)
      common/RAY2/ITDSP,ITBND,ITEPT,ITZNM,ITSNM,ITVNO,ITORG,ITSNR,
     &            ITOBS,ITHLS,ITHLZ,ITGRD,GRDIS,ITPPSW
      common/RAY3/MODIFY,MODLEN,MODBND
      COMMON/DEFLT3/DFCFD,DECMPDBFL,DICONDBFL
      common/cfdfil/LCFD(MCOM),IFCFD(MCOM)
      COMMON/ICFNOD/ICFD,ICP
      common/param2/TITLE(MNZ),CFTRFL(MNZ),LPHI(MNZ)
      common/GRIDEFN/origin,xgrid,ygrid,zgrid,zegrid,ortho

      logical origin,xgrid,ygrid,zgrid,zegrid,ortho
      logical OK,dok,MODIFY,MODLEN,MODBND,XST,CFDsave

      character OUTSTR*124,WORD*124
      CHARACTER ITEM(11)*34
      character ltmp*72,dtmp*72,LCFD*72
      CHARACTER*72 TITLE,CFTRFL,LPHI
      character*72 DFCFD,DECMPDBFL,DICONDBFL
      integer IWO  ! for radio button
      integer NITMS,INO ! max items and current menu item

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='CFDCOMP'  ! set for subroutine

C Note: to make this more general, we need to have prj set the value of ICFD.
C ICFD=1 for the first CFD domain in the building, 2 for the second CFD domain, etc.
C For now, this is constrained to a single CFD domain in the building.
C ICFD is set to one independantly in VERMAN in prj.F, any updates here should 
C be corresponded there as well
      ICFD = 1
      ICP = izone
      TITLE(ICFD) = '...'

C Set the flag to indicate that the data has not been saved to the CFD input file.
      CFDsave=.false.

C Set flags to indiate the origin of CFD domain and gridding of axes
C have not yet been defined.
      origin=.false.
      xgrid=.false.
      ygrid=.false.
      zgrid=.false.

C= Assume orthogonal domain.
      ortho=.true.

C Draw the zone and display the vertex numbers.
      call tstamp('>','PRJ: edit CFD domain') 
      nzg=1
      nznog(1)=izone
      izgfoc=izone
      CALL ESCZONE(izone)
      CALL BNDOBJ(0,IER)
      CALL ERCZONE(izone)
      MODIFY=.TRUE.
      MODLEN=.TRUE.
      MODBND=.TRUE.
      itvno=0
      itsnm=0
      CALL INLNST(1)
      CALL ADJVIEW(IER)

C Setup help text for main menu.
      helptopic='cfd_opening_menu'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Get the name of the CFD input file.
      if(LCFD(izone)(1:2).eq.'  ')then
        if(netpth(1:2).eq.'  '.or.netpth(1:2).eq.'./')then
          WRITE(LCFD(izone),'(A,A4)')
     &      zname(izone)(1:lnzname(izone)),'.dfd'
        else
          WRITE(LCFD(izone),'(A,A,A,A4)') netpth(1:lnblnk(netpth)),'/',
     &      zname(izone)(1:lnzname(izone)),'.dfd'
        endif
      endif
      ltmp=LCFD(izone)
      CALL EASKS(ltmp,' CFD input file?','  ',72,DFCFD,'dfd file',
     &  IER,nbhelp)
      if(ltmp(1:2).ne.'  ')LCFD(izone)=ltmp

C If the CFD input file exists, read in its contents.
C This is a two-stage process: first open the file and get the conflation
C flag.  Then read in the data based on the conflation type.
      CALL ERPFREE(IUF,ISTAT)
      call FINDFIL(LCFD(izone),XST)
      if(XST)then
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
            IBLD=0
            IMFN=0
            if(IFCFD(izone).eq.1) IBLD=1
            if(IFCFD(izone).eq.2) IBLD=1
            if(IFCFD(izone).eq.3) IMFN=1
            CALL EFOPSEQ(IUF,LCFD(izone),1,IER)

C << Force reporting. >>
            CALL CFDDTA(2,iuout,izone,IBLD,IMFN,IER)

C Set flags to indicate that CFD origin and gridding has been defined.
            origin=.true.
            xgrid=.true.
            ygrid=.true.
            zgrid=.true.
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
      endif
      CALL ERPFREE(IUF,ISTAT)

C Present menu of options for editing CFD input file.
   10 INO=-4
      ITEM(1) ='a Title: '
      write (ITEM(2),'(a,a)') '  ',TITLE(ICFD)(1:30)
      ITEM(3) ='  -------------------------- '
      ITEM(4) ='a BSim-CFD conflation '
      ITEM(5) ='b Geometry and gridding '
      ITEM(6) ='c Solution variables '
      ITEM(7) ='d Boundary conditions '
      ITEM(8) ='  -------------------------- '
      ITEM(9) ='> Save CFD input file '
      ITEM(10)='? Help'
      ITEM(11)='- exit this menu'
      NITMS=11

C Help text for this menu.
  12  helptopic='cfd_opening_menu'
      call gethelptext(helpinsub,helptopic,nbhelp)
 
      CALL EMENU('Zone air movement description',ITEM,NITMS,INO)

      IF(INO.EQ.0)THEN

C Wrong pick.
        INO=-1
        GOTO 12
      ELSEIF(INO.EQ.1)THEN
        dtmp='Model of room air flow'
        ltmp=TITLE(ICFD)
        call EASKS(ltmp,'Description of CFD model:',' ',72,dtmp,
     &             'CFD description',IER,42)
        TITLE(ICFD)=ltmp
      ELSEIF(INO.EQ.4)THEN

C Conflation options.
        helptopic='cfd_conflation_options'
        call gethelptext(helpinsub,helptopic,nbhelp)
        IWO=0
        call EASKATOG('Conflation options:',' ','CFD only',
     &    'thermal only','adaptive thermal','air-flow',
     &    'cancel','  ','  ',IWO,nbhelp)
        if(IWO.le.2)IFCFD(izone)=IWO-1
	if(IWO.eq.3)IFCFD(izone)=4
	if(IWO.eq.4)IFCFD(izone)=5
      ELSEIF(INO.EQ.5)THEN 

C Geometry and Gridding
        call CFDGGRD(izone,IER)
      ELSEIF(INO.EQ.6)THEN 

C Solution Variables and Initial Conditions
C User to specify equations to solve and initial variables.
        call CFDSVAR(IER)
      ELSEIF(INO.EQ.7)THEN 

C Boundary conditions.
        call CFDBVAR(IZONE,IER)
      ELSEIF(INO.EQ.9)THEN 

C Save Data to dfs input file.
        call DFDSV(IUF,IZONE,IER)
        if (IER.eq.0) CFDsave=.true.

C Help.
      ELSEIF(INO.EQ.(NITMS-1))THEN
        helptopic='cfd_opening_menu'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('CFD inputs',nbhelp,'-',0,0,IER)
      ELSEIF(INO.EQ.NITMS)THEN

C Exit CFD editing menu.
        if(.not.CFDsave)then
          dok=.false.
          CALL ASKOK('Data not yet saved to CFD input file.',
     &               'Are you sure you want to exit? ',OK,dok,nbhelp)
          IF(.NOT.OK)GOTO 10
        endif
        return
      ENDIF
      GOTO 10

C Error handling
    1 CALL USRMSG('Problem with CFD input file line:',OUTSTR,'W')
      call edisp(iuout,' Plese check your CFD input file.')
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

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/FILEP/IFIL
      common/OUTIN/IUOUT,IUIN
      integer menuchw,igl,igr,igt,igb,igw,igwh
      common/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      common/gzonpik/izgfoc,nzg,nznog(mcom)
      common/RAY2/ITDSP,ITBND,ITEPT,ITZNM,ITSNM,ITVNO,ITORG,ITSNR,
     &            ITOBS,ITHLS,ITHLZ,ITGRD,GRDIS,ITPPSW
      common/RAY3/MODIFY,MODLEN,MODBND

C NREG(1,*) is X, NREG(2,*) is Y, NREG(3,*) is west, NREG(4,*) is east
C (the east (4,*) is for curvilinear gridding).
      common/GRIDFN/NCELX(MNREG,MNZ),NCELY(MNREG,MNZ),NCELZ(MNREG,MNZ),
     &  NCELZE(MNREG,MNZ),XREG(MNREG,MNZ),YREG(MNREG,MNZ),
     &  ZREG(MNREG,MNZ),ZREGE(MNREG,MNZ),Xplaw(MNREG,MNZ),
     &  Yplaw(MNREG,MNZ),Zplaw(MNREG,MNZ),Zplawe(MNREG,MNZ),NREG(4,MNZ)
      common/GRDVRTS/iorg,ixend,iyend,izend,izende

      common/GRIDEFN/origin,xgrid,ygrid,zgrid,zegrid,ortho
      COMMON/ICFNOD/ICFD,ICP
      common/grdmax/NTCX,NTCY,NTCZ

      common/EQTION/CALLU(MNZ),CALLV(MNZ),CALLW(MNZ),CALLT(MNZ),
     &             CALLC(MNZ),KEMDL(MNZ),BUOY(MNZ),BOUSSI(MNZ),
     &             ZEROT(MNZ),ZandKE(MNZ),MITzero(MNZ)

C Arrays for sorting domains from (see code for comments).
      dimension XS(MTV),XSS(MTV),XSI(MTV),YS(MTV),YSS(MTV),YSI(MTV)
      dimension ZS(MTV),ZSI(MTV),ZSS(MTV)

      LOGICAL CALLU,CALLV,CALLW,CALLT,CALLC,KEMDL,BUOY,BOUSSI
      logical MODIFY,MODLEN,MODBND,close,griderr
      logical origin,xgrid,ygrid,zgrid,zegrid,ortho
      LOGICAL ZEROT,ZandKE,MITzero,vclose

      character ITEM(14)*36
      character outs*124

      integer iglib   ! if 1 then X11, if 2 then GTK, if 3 then text only.
      integer iorgt,ixendt,iyendt,izendt,izendet  ! for local editing
      integer NITMS,INO ! max items and current menu item

#ifdef OSI
      integer iigl,iigr,iigt,iigb,iigw,iigwh
      integer iiw1,iiw2,iiw3,iiw4,iimenu
#else
      integer*8 iigl,iigr,iigt,iigb,iigw,iigwh
      integer*8 iiw1,iiw2,iiw3,iiw4,iimenu
#endif

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='CFDGGRD'  ! set for subroutine

C Assume axes defined and set initial values for general resolution
C of the X Y Z axis.
C << Todo: set these based on a higher level concept of
C << low/medium/high resolution and/or the size of the zone.
      origin=.TRUE.
      icellx=20
      icelly=20
      icellz=20
      itrc=0   ! silent feedback initially

C Help text for this menu.
      helptopic='cfd_gridding_setup'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Check if we have vertex at ends of axis data.
      if (iorg.eq.0) then
        call usrmsg('You will have to define the CFD axis relationship',
     &              'to the zone geometry via vertex numbers.','W')
        iorgt=iorg
        CALL EASKI(iorgt,' Specify vertex at origin of CFD domain. ',
     &    ' Which `v`? ',1,'F',NTV,'F',1,'orig vertex',IERI,nbhelp)
        if(ieri.eq.-3) then
          origin=.false.
          return
        endif
        ixendt=ixend
        CALL EASKI(ixendt,' Specify vertex at end of X axis. ',
     &    ' Which `v`? ',1,'F',NTV,'F',1,'x-axis vertex',IERI,nbhelp)
        if(ieri.eq.-3) then
          origin=.false.
          return
        endif

        iyendt=iyend
        CALL EASKI(iyendt,' Specify vertex at end of Y axis. ',
     &    ' Which `v`? ',1,'F',NTV,'F',1,'y-axis vertex',IERI,nbhelp)
        if(ieri.eq.-3) then
          origin=.false.
          return
        endif

        izendt=izend
        CALL EASKI(izendt,' Specify vertex at end of Z axis. ',
     &    ' Which `v`? ',1,'F',NTV,'F',1,'z-axis vertex',IERI,nbhelp)
        if(ieri.eq.-3) then
          origin=.false.
          return
        endif

C No cancels so instanciate variables.
        iorg=iorgt
        ixend=ixendt
        iyend=iyendt
        izend=izendt
      endif

C Make sure axes are orthogonal, as dfs only supports Cartesian coordinates.
C Check X and Y.
      call ang3vtx(X(ixend),Y(ixend),Z(ixend),X(iorg),Y(iorg),
     &                       Z(iorg),X(iyend),Y(iyend),Z(iyend),ang3)
      call eclose(ang3,90.00,0.1,close)
      if(.NOT.close) then
        call usrmsg(
     &    'X & Y axes are not orthogonal. Please check and respecify.',
     &    '  ','W')
        origin=.FALSE.
        xgrid=.FALSE.
      endif

C Check X and Z.
      call ang3vtx(X(ixend),Y(ixend),Z(ixend),X(iorg),Y(iorg),
     &                       Z(iorg),X(izend),Y(izend),Z(izend),ang3)
      call eclose(ang3,90.00,0.1,close)
      if(.NOT.close) then
        call usrmsg(
     &    'X & Z axes are not orthogonal. Please check and respecify.',
     &    '  ','W')
        origin=.FALSE.
        ygrid=.FALSE.
      endif

C Check Y and Z.
      call ang3vtx(X(iyend),Y(iyend),Z(iyend),X(iorg),Y(iorg),
     &                       Z(iorg),X(izend),Y(izend),Z(izend),ang3)
      call eclose(ang3,90.00,0.1,close)
      if(.NOT.close) then
        call usrmsg(
     &    'Y & Z axes are not orthogonal. Please check and respecify.',
     &    '  ','W')
        origin=.FALSE.
        zgrid=.FALSE.
      endif

C Determine length of each axis.
      xdis=crowxyz(X(iorg),Y(iorg),Z(iorg),X(ixend),Y(ixend),Z(ixend))
      ydis=crowxyz(X(iorg),Y(iorg),Z(iorg),X(iyend),Y(iyend),Z(iyend))
      zdis=crowxyz(X(iorg),Y(iorg),Z(iorg),X(izend),Y(izend),Z(izend))

C Non orthogonal.
      if (.NOT.ortho) then

C Check Y and Ze.
        call ang3vtx(X(iyend),Y(iyend),Z(iyend),X(iorg),Y(iorg),
     &               Z(iorg),X(izende),Y(izende),Z(izende),ang3)
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
      if (xgrid.AND.ygrid.AND.zgrid) then
        griderr=.FALSE.
      else
        griderr=.TRUE.
      endif

C Need to define counter for variable length menu.
      NCP=0

C Create menu text.
      ITEM(1) ='a Define origin and axes.'
      ITEM(2) ='  Vertex ids:'
      if (ortho) then
        write (ITEM(3),'(4(a,i2))') ' O=',iorg,': Vx=',ixend,', Vy=',
     &                                    iyend,', Vz=',izend
      else
        write (ITEM(3),'(5(a,i2))') ' O=',iorg,': Vx=',ixend,', Vy=',
     &                             iyend,', Vz=',izend,' Vze=',izende
      endif
      ITEM(4) =' ------------------------------ '
      ITEM(5) ='b Estimate regions from geometry'
      ITEM(6) =' Axis Regions Total cells'
      write (ITEM(7),'(a,2i6)') 'c X  ',NREG(1,ICFD),NTCX
      write (ITEM(8),'(a,2i6)') 'd Y  ',NREG(2,ICFD),NTCY
      write (ITEM(9),'(a,2i6)') 'e Z  ',NREG(3,ICFD),NTCZ
      if (.NOT.ortho) then
        write (ITEM(10),'(a,2i6)') 'f Ze ',NREG(4,ICFD),NTCZe
        NCP=1
      endif
      if (griderr) then
        ITEM(10+NCP) ='  Gridding incomplete or errors! '
      else
        ITEM(10+NCP) ='g Visualize gridding             '
      endif
      ITEM(11+NCP) =' ------------------------------ '
      if(itrc.eq.0)then
        ITEM(12+NCP) ='> feedback silent             '
      elseif(itrc.eq.1)then
        ITEM(12+NCP) ='> feedback brief              '
      elseif(itrc.eq.2)then
        ITEM(12+NCP) ='> feedback verbose            '
      endif
      ITEM(13+NCP)   ='? help                        '
      ITEM(14+NCP)   ='- exit                        '
      NITMS=14+NCP

C Help text for this menu.
      helptopic='cfd_gridding_setup'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Display menu.
      CALL EMENU('Geometry and gridding',ITEM,NITMS,INO)
 9    if(INO.EQ.1)then

C Specify origin and axes of CFD domain.
        iorgt=iorg
        CALL EASKI(iorgt,' Specify vertex at origin of CFD domain. ',
     &    ' Which `v`? ',1,'F',NTV,'F',1,'orig vertex',IERI,nbhelp)
        if(ieri.eq.-3) then
          origin=.false.
          return
        endif

        ixendt=ixend
        CALL EASKI(ixendt,' Specify vertex at end of X axis. ',
     &    ' Which `v`? ',1,'F',NTV,'F',1,'x-axis vertex',IERI,nbhelp)
        if(ieri.eq.-3) then
          origin=.false.
          return
        endif

        iyendt=iyend
        CALL EASKI(iyendt,' Specify vertex at end of Y axis. ',
     &    ' Which `v`? ',1,'F',NTV,'F',1,'y-axis vertex',IERI,nbhelp)
        if(ieri.eq.-3) then
          origin=.false.
          return
        endif

        izendt=izend
        CALL EASKI(izendt,' Specify vertex at end of Z axis. ',
     &    ' Which `v`? ',1,'F',NTV,'F',1,'z-axis vertex',IERI,nbhelp)
        if(ieri.eq.-3) then
          origin=.false.
          return
        endif

C No cancels so instanciate variables.
        iorg=iorgt
        ixend=ixendt
        iyend=iyendt
        izend=izendt

C Make sure axes are orthogonal, as dfs only supports Cartesian coordinates.
C Check X and Y.
        call ang3vtx(X(ixend),Y(ixend),Z(ixend),X(iorg),Y(iorg),
     &               Z(iorg),X(iyend),Y(iyend),Z(iyend),ang3)
        call eclose(ang3,90.00,0.1,close)
        if(.NOT.close) then
          call edisp(iuout,' X & Y axes are not orthogonal. Respecify.')
          goto 9
        endif

C Check X and Z.
        call ang3vtx(X(ixend),Y(ixend),Z(ixend),X(iorg),Y(iorg),
     &               Z(iorg),X(izend),Y(izend),Z(izend),ang3)
        call eclose(ang3,90.00,0.1,close)
        if(.NOT.close) then
          call edisp(iuout,' X & Z axes are not orthogonal. Respecify.')
          goto 9
        endif

C Check Y and Z.
        call ang3vtx(X(iyend),Y(iyend),Z(iyend),X(iorg),Y(iorg),
     &               Z(iorg),X(izend),Y(izend),Z(izend),ang3)
        call eclose(ang3,90.00,0.1,close)
        if(.NOT.close) then
          call edisp(iuout,' Y & Z axes are not orthogonal. Respecify.')
          goto 9
        endif

C Determine length of each axis.
        xdis=crowxyz(X(iorg),Y(iorg),Z(iorg),X(ixend),Y(ixend),Z(ixend))
        ydis=crowxyz(X(iorg),Y(iorg),Z(iorg),X(iyend),Y(iyend),Z(iyend))
        zdis=crowxyz(X(iorg),Y(iorg),Z(iorg),X(izend),Y(izend),Z(izend))

C Set flag to indicate that origin has been defined.
        origin=.true.
      elseif(INO.EQ.5)then

C Setup regions via scan of geometry.
C Scan X,Y,Z coordinates and then sort in ascending order. Make use of
C XS,YS,ZS for array to sort and XSI,YSI,ZSI for their link back to
C initial position. Use XSS,YSS,ZSS for the compacted sorted list (with
C duplicates removed).

C Take into account if the zone is offset from the site origin.
C Use the zone bounds (ZXMN ZYMN ZZMN)
        do 42 il=1,NZTV(izone)
          XS(il)=X(il); YS(il)=Y(il); ZS(il)=Z(il)
          XSS(il)=ZXMN(izone); YSS(il)=ZYMN(izone); ZSS(il)=ZZMN(izone)
          XSI(il)=il; YSI(il)=il; ZSI(il)=il
  42    continue

C Sort each axis in ascending order.
        KFLAG = 2
        call SORTR(XS,XSI,NZTV(izone),KFLAG)
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
          write(outs,'(a,3f9.3)') 'Smallest cells tollerance: ',
     &      tolx,toly,tolz
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

C Debug.
        if(itrc.gt.1)then
          write(6,*) 'x axis ',ilx,(XSS(j),j=1,ilx)
          write(6,*) 'y axis ',ily,(YSS(j),j=1,ily)
          write(6,*) 'z axis ',ilz,(ZSS(j),j=1,ilz)
        endif

        if(NREG(1,ICFD).eq.0)NREG(1,ICFD)=ilx-1
        if(NREG(2,ICFD).eq.0)NREG(2,ICFD)=ily-1
        if(NREG(3,ICFD).eq.0)NREG(3,ICFD)=ilz-1

        write(outs,'(a,i3,a,f6.2,a)') 
     &    'How many total cells along X axis (max is ',NTCELX,
     &    'over ',xdis,'m) would you like to have?'
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
        endif

C Debug.
        if(itrc.gt.1)then
          write(6,*) xd,tolx,int(xd/tolx)
          write(6,*) 'ncellx ',NCELX
        endif
        write(outs,'(a,i3,a,f6.2,a)') 
     &    'How many total cells along Y axis (max is ',NTCELY,
     &    'over ',ydis,'m) would you like to have?'
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
        endif

C Debug.
        if(itrc.gt.1)then
          write(6,*) yd,toly,int(yd/toly)
          write(6,*) 'ncelly ',NCELY
        endif

        write(outs,'(a,i3,a,f6.2,a)') 
     &    'How many total cells along Z axis (max is ',NTCELZ,
     &    'over ',zdis,'m) would you like to have?'
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
        endif

C Debug.
        if(itrc.gt.1)then
          write(6,*) 'Z distance ',zd,' tollerance ',tolz,int(zd/tolz)
          write(6,*) 'ncellz ',NCELZ
        endif 
      elseif(INO.EQ.7)then

C Grid X-axis.
        call GRIDDEF(1,NREG,NTCX,xdis,NCELX,XREG,Xplaw,ier)
        if (ier.ne.0) then
          griderr=.TRUE.
          xgrid=.FALSE.
        else
          xgrid=.TRUE.
        endif
      elseif(INO.EQ.8)then

C Grid Y-axis.
        call GRIDDEF(2,NREG,NTCY,ydis,NCELY,YREG,Yplaw,ier)
        if (ier.ne.0) then
          griderr=.TRUE.
          ygrid=.FALSE.
        else
          ygrid=.TRUE.
        endif
      elseif(INO.EQ.9)then

C Have user subdivide Z-axis into regions and specify number of cells
C and cell distribution for each region. Additional questions asked
C if curvilinear.

C Ask whether Z is Orthogonal or curvilinear.
        CALL EASKABC('Options for Z gridding:',' ','orthogonal',
     &               'curvilinear','cancel',IWO,nbhelp)
        if(iwo.ne.3)then
          ortho=.TRUE.

C Grid Zwest-axis for both cases.
          call GRIDDEF(3,NREG,NTCZ,zdis,NCELZ,ZREG,Zplaw,ier)
          if (ier.ne.0) then
            griderr=.TRUE.
            zgrid=.FALSE.
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
            izendet=izende
            CALL EASKI(izendet,'Specify vertex at end of east Z axis.',
     &        'Which `v`? ',1,'F',NTV,'F',1,'ez axis vertex',
     &        IERI,nbhelp)
            if(ieri.eq.-3) then
              zegrid=.FALSE.
              return
            endif

C User did not cancel so instanciate values.
            izende=izendet

            zedis=crowxyz(X(ieorg),Y(ieorg),Z(ieorg),X(izende),
     &              Y(izende),Z(izende))

C Need to ask for vertices which represent the
C top and bottom boundaries of the curvilinear x-section.
            call TOPBOT(izone,'-',IER)

C Grid Zeast-axis.
            call GRIDDEF(4,NREG,NTCZe,zedis,NCELZe,ZREGe,Zplawe,ier)
            if (ier.ne.0) then
              griderr=.TRUE.
              zegrid=.FALSE.
            else
              zegrid=.TRUE.
            endif
          endif
        endif

      elseif(INO.EQ.(10+NCP).and.(.NOT.griderr))then

C Visualize gridding.
        if(origin.and.xgrid.and.ygrid.and.zgrid)then
          call cgd(izone,ierr)

C Restore standard graphics window variables.
          if(MMOD.EQ.8)then
            iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
            if(iglib.eq.1)then
              CALL feedbox(menuchw,2,igfw,igfh)
              CALL opengdisp(menuchw,LIMTTY,2,igdw,igdh)
            endif

C Setup and pass in parameters to win3d.
            iiw1=4; iiw2=1; iiw3=1; iiw4=3; iimenu=menuchw
            iigl=igl; iigr=igr; iigt=igt; iigb=igb; iigw=igw; iigwh=igwh
            CALL win3d(iimenu,iiw1,iiw2,iiw3,iiw4,
     &        iigl,iigr,iigt,iigb,iigw,iigwh)
            igl=int(iigl); igr=int(iigr); igt=int(iigt); igb=int(iigb)
            igw=int(iigw); igwh=int(iigwh)
            call startbuffer()

C Redraw zone and vertex points. Note, if user has returned
C from 3D grid visualisation then G1 common blocks will need
C to be refreshed.
            nzg=1
            nznog(1)=izone
            izgfoc=izone
            call georead(IFIL+1,LGEOM(nznog(1)),nznog(1),1,IUOUT,IER)
            CALL ESCZONE(izone)
            CALL BNDOBJ(0,IER)
            CALL ERCZONE(izone)
            MODIFY=.TRUE.
            MODLEN=.TRUE.
            MODBND=.TRUE.
            itvno=0
            itsnm=0
            CALL INLNST(1)
            CALL ADJVIEW(IER)
          endif
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

      elseif(INO.EQ.NITMS)then

C Return to higher level menu.
        if (griderr) CALL USRMSG(
     &    'The current gridding scheme is incomplete!','  ','W')

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
    

C ************************ CFDSVAR ************************
C CFDSVAR: controls the editing of CFD solution variables.
C IER=0 indicates no error.

      SUBROUTINE CFDSVAR(IER)
#include "cfd.h"

      COMMON/ICFNOD/ICFD,ICP
      common/grdmax/NTCX,NTCY,NTCZ

C Equations solved.
      common/EQTION/CALLU(MNZ),CALLV(MNZ),CALLW(MNZ),CALLT(MNZ),
     &             CALLC(MNZ),KEMDL(MNZ),BUOY(MNZ),BOUSSI(MNZ),
     &             ZEROT(MNZ),ZandKE(MNZ),MITzero(MNZ)
      common/EQTION3/CALLMA(MNZ),CALPOL(MCTM,MNZ),POLNAM(MCTM,MNZ),NCTM,
     &      JHUMINDX

C Solution methods.
      common/METHDS/ITURB(MNZ),IBUOY(MNZ)

      character ITEM(12+MCTM)*40, POLNAM*12
      LOGICAL CALLU,CALLV,CALLW,CALLT,CALLC,KEMDL,BUOY,BOUSSI
      LOGICAL ZEROT,ZandKE,MITzero
      LOGICAL CALPOL,CALLMA,OK,dok
      integer NITMS,INO ! max items and current menu item

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='CFDSVAR'  ! set for subroutine

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

C Create menu showing 4 main solution categories and allow togelling 
C to activate various solution cats/ methods.  Once a solution category 
C has been activated then display associated state variables and allow 
C definition of initial values and relaxation factors.
C First clear menu.
 5    do 10 I=1,14
        ITEM(I)='  '
 10   continue

C Clear display area.
      call usrmsg('  ','  ','-')

C Now create menu.
      ITEM(1)= '  Velocity: solved'
      if (.NOT.CALLT(ICFD)) then
        ITEM(2)= 'a Temperature >> not solved'
      else
        ITEM(2)= 'a Temperature >> solved'
      endif
      if (ITURB(ICFD).eq.0) then
        ITEM(3)= 'b Turbulence >> none (laminar)'
      elseif (ITURB(ICFD).eq.1) then
        ITEM(3)= 'b Turbulence >> k-e turbulence'
      elseif (ITURB(ICFD).eq.2) then
        ITEM(3)= 'b Turbulence >> fixed eddy viscosity'
      elseif (ITURB(ICFD).eq.3) then
        ITEM(3)= 'b Turbulence >> MIT zero-equation'
      elseif (ITURB(ICFD).eq.4) then
        ITEM(3)= 'b Turbulence >> fixed visc -> k-e'
      endif
      if (IBUOY(ICFD).eq.0) then
        ITEM(4)= 'c Buoyancy >> not solved'
      elseif (IBUOY(ICFD).eq.1) then
        ITEM(4)= 'c Buoyancy >> ideal gas'
      elseif (IBUOY(ICFD).eq.2) then
        ITEM(4)= 'c Buoyancy >> Boussinesq approx'
      endif
      DO 121 ICTM=1,NCTM
        IF(CALPOL(ICTM,ICFD))THEN
          WRITE(ITEM(4+ICTM),'(A,1X,A,1X,A,1X)')CHAR(99+ICTM),
     &         POLNAM(ICTM,ICFD),'>> solved'
        ELSE
          WRITE(ITEM(4+ICTM),'(A,1X,A,1X,A,1X)')CHAR(99+ICTM),
     &         POLNAM(ICTM,ICFD),'>> not solved'
        ENDIF
 121  CONTINUE
      if (.NOT.CALLMA(ICFD)) then
        WRITE(ITEM(5+NCTM),'(2A)')CHAR(100+NCTM),
     &       ' Mean age of air >> not solved'
      else
        WRITE(ITEM(5+NCTM),'(2A)')CHAR(100+NCTM),
     &       ' Mean age of air >> solved'
      endif
      ITEM(6+NCTM)= ' ------------------------------- '
      ITEM(7+NCTM)= '* Reset to defaults'
      ITEM(8+NCTM)= ' ------------------------------- '
      ITEM(9+NCTM)='# Edit equation setup'
      ITEM(10+NCTM)= ' ------------------------------- '
      ITEM(11+NCTM)='? Help'
      ITEM(12+NCTM)='- exit this menu'
      NITMS=12+NCTM

C Help text for this menu.
      helptopic='cfd_equations_to_solve'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Display menu.
      CALL EMENU('Required solutions',ITEM,NITMS,INO)

C Check for pick of empty box.
      if(INO.EQ.2)then

C (De)activate temperature solution.
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

C Buoyancy.
        IBUOY(ICFD)=IBUOY(ICFD)+1
        if (IBUOY(ICFD).gt.2) IBUOY(ICFD)=0
      elseif(INO.GT.4.AND.INO.LE.(4+NCTM))then

C Contaminants
        IF(CALPOL(INO-4,ICFD))THEN
          CALPOL(INO-4,ICFD)=.FALSE.
        ELSE
          CALPOL(INO-4,ICFD)=.TRUE.
        ENDIF
      elseif(INO.EQ.5+NCTM)then

C Mean age of air.
        if (CALLMA(ICFD)) then
          CALLMA(ICFD)=.false.
        else
          CALLMA(ICFD)=.true.
        endif
      elseif(INO.EQ.7+NCTM)then

C Reset defaults.
        dok=.false.
        call askok('Resetting solution parameters to default values.',
     &             'Are you sure? (see help)',OK,dok,nbhelp)
        if (OK) call CFDDEFLT
      elseif(INO.EQ.9+NCTM)then

C Edit equation data.
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
C CFDEDAT: controls the editing of CFD equations.

      SUBROUTINE CFDEDAT
C #include "building.h"
#include "cfd.h"

C      common/OUTIN/IUOUT,IUIN
      COMMON/ICFNOD/ICFD,ICP
      common/grdmax/NTCX,NTCY,NTCZ

C Equations solved.
      common/EQTION/CALLU(MNZ),CALLV(MNZ),CALLW(MNZ),CALLT(MNZ),
     &             CALLC(MNZ),KEMDL(MNZ),BUOY(MNZ),BOUSSI(MNZ),
     &             ZEROT(MNZ),ZandKE(MNZ),MITzero(MNZ)
      common/EQTION3/CALLMA(MNZ),CALPOL(MCTM,MNZ),POLNAM(MCTM,MNZ),NCTM,
     &      JHUMINDX

C Solution methods.
      common/METHDS/ITURB(MNZ),IBUOY(MNZ)

C Initial values.
      COMMON/INITIA/UINIT(MNZ),VINIT(MNZ),WINIT(MNZ),PINIT(MNZ),
     &              TINIT(MNZ),TEINIT(MNZ),EDINIT(MNZ)

C Under-relaxation factors.
      COMMON/LINRFC/URFCU(MNZ),URFCV(MNZ),URFCW(MNZ),URFCP(MNZ),
     &              URFCT(MNZ),URFCK(MNZ),URFCE(MNZ),URFCVS(MNZ)

C Convergence criteria.
      common/param1/MAXITR(MNZ),IMONT(MNZ),JMONT(MNZ),KMONT(MNZ),
     &             IPPHI(MNZ),SRMAX(MNZ)

C Additional data needed for some models.
      COMMON/ZTURB/rMOOT,nZtoKE
      COMMON/BUOYAN/BUOYA,BOUSSA,TBAR
      COMMON/MFS/IMFACT,NOSTOP,MITMFS
 
      character ITEM(34)*40,ltmp*16,dtmp*16, POLNAM*12

      LOGICAL CALLU,CALLV,CALLW,CALLT,CALLC,KEMDL,BUOY,BOUSSI
      LOGICAL ZEROT,ZandKE,MITzero
      LOGICAL BUOYA,BOUSSA
      LOGICAL CALPOL,CALLMA
      integer NITMS,INO ! max items and current menu item

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='CFDEDAT'  ! set for subroutine

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

C Create menu showing 4 main solution categories and allow togelling 
C to activate various solution cats/ methods.  Once a solution category 
C has been activated then display associated state variables and allow 
C definition of initial values and relaxation factors.
C First clear menu.
 5    INO=-4
      do 10 I=1,34
        ITEM(I)='  '
 10   continue

C Clear display area.
      call usrmsg('  ','  ','-')

C Now create menu.
      ITEM(1)= '  Velocity:'
      if (CALLU(ICFD)) write(ITEM(2),'(a,f6.3,a,f5.2)') 
     &         'b  X: Init V',UINIT(ICFD),'; relax fact',URFCU(ICFD)
      if (CALLV(ICFD)) write(ITEM(3),'(a,f6.3,a,f5.2)') 
     &         'c  Y: Init V',VINIT(ICFD),'; relax fact',URFCV(ICFD)
      if (CALLW(ICFD)) write(ITEM(4),'(a,f6.3,a,f5.2)') 
     &         'd  Z: Init V',WINIT(ICFD),'; relax fact',URFCW(ICFD)
      ITEM(5)= ' ------------------------------- '
      if (.NOT.CALLT(ICFD)) then
        ITEM(6)= 'e Temperature >> not solved'
      else
        ITEM(6)= 'e Temperature >> solved:'
        write(ITEM(7),'(a,f6.2,a,f5.2)') 'f  Init T',TINIT(ICFD),
     &                             '; relax fact',URFCT(ICFD)
      endif
      ITEM(8)= ' ------------------------------- '
      if (ITURB(ICFD).eq.0) then
        ITEM(9)= 'g Turbulence >> none (laminar)'
      elseif (ITURB(ICFD).eq.1) then
        ITEM(9)= 'g Turbulence >> k-e turbulence'
        write(ITEM(10),'(a,f6.4,a,f5.2)') 'h Init k',TEINIT(ICFD),
     &                             '; relax fact',URFCK(ICFD)
        write(ITEM(11),'(a,f6.4,a,f5.2)') 'i Init e',EDINIT(ICFD),
     &                             '; relax fact',URFCE(ICFD)
      elseif (ITURB(ICFD).eq.2) then
        ITEM(9)= 'g Turbulence >> fixed eddy viscosity'
        write (ITEM(10),'(a,f6.2)') 'h  eddy viscosity: ',rMOOT
      elseif (ITURB(ICFD).eq.3) then
        ITEM(9)= 'g Turbulence >> MIT zero-equation'
      elseif (ITURB(ICFD).eq.4) then
        ITEM(9)= 'g Turbulence >> fixed visc -> k-e'
        write(ITEM(10),'(a,f6.2,a,i5)') 'h  eddy visc',rMOOT,
     &                                  '; numb iters',nZtoKE
      endif
      ITEM(12)= ' ------------------------------- '
      if (IBUOY(ICFD).eq.0) then
        ITEM(13)= 'i Buoyancy >> not solved'
      elseif (IBUOY(ICFD).eq.1) then
        ITEM(13)= 'i Buoyancy >> ideal gas'
      elseif (IBUOY(ICFD).eq.2) then
        ITEM(13)= 'i Buoyancy >> Boussinesq approx'
        write(ITEM(14),'(a,f6.2)') 'j  ref temperature ',TBAR 
      endif
      ITEM(15)= ' ------------------------------- '
      IWRTDEF=0
      DO 121 ICTM=1,NCTM
        IF(CALPOL(ICTM,ICFD))THEN
          WRITE(ITEM(15+ICTM),'(A,1X,A,1X,A,1X)')CHAR(106+ICTM),
     &         POLNAM(ICTM,ICFD),'>> solved'
          IWRTDEF=1
        ELSE
          WRITE(ITEM(15+ICTM),'(A,1X,A,1X,A,1X)')CHAR(106+ICTM),
     &         POLNAM(ICTM,ICFD),'>> not solved'
        ENDIF
 121  CONTINUE
      IF(IWRTDEF.EQ.1)THEN
        ITEM(16+NCTM)= ' Defaults set for init and relax values'
      ELSE
        ITEM(16+NCTM)= ' '
      ENDIF
      ITEM(17+NCTM)= ' ------------------------------- '
      if (.NOT.CALLMA(ICFD)) then
        ITEM(18+NCTM)= 'w Mean age of air >> not solved'
      else
        ITEM(18+NCTM)= 'w Mean age of air >> solved'
        ITEM(19+NCTM)= ' Defaults set for init and relax values'
      endif
      ITEM(20+NCTM)=' ------------------------------- '
      ITEM(21+NCTM)=' Convergence criteria:'
      write (ITEM(22+NCTM),'(a,i5)')  'x Maximum iterations: ',
     &      MAXITR(ICFD)
      write (ITEM(23+NCTM),'(a,f7.5)')'y Max sum of residuals: ',
     &      SRMAX(ICFD)
      write (ITEM(24+NCTM),'(a,i5)')'u Non-convergence mfs-dfs: ',
     &      NOSTOP
      write (ITEM(25+NCTM),'(a,i5)')'v Max. iterations mfs-dfs: ',
     &      MITMFS
      ITEM(26+NCTM)=' ------------------------------- '
      write (ITEM(27+NCTM),'(a,3i4)') 'z Monitoring cell (i,j,k):',
     &                        IMONT(ICFD)-1,JMONT(ICFD)-1,KMONT(ICFD)-1
      ITEM(28+NCTM)= ' ------------------------------- '
      ITEM(29+NCTM)='? Help'
      ITEM(30+NCTM)='- exit this menu'
      NITMS=30+NCTM

C Help text for this menu.
      helptopic='cfd_equations_to_edit'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Display menu.
      CALL EMENU('Solutions required and data',ITEM,NITMS,INO)

C Check for pick of empty box.
      if(INO.EQ.2)then

C Ask for initial value and relaxation factor in X dir.
        write (ltmp,'(f8.4,f6.2)') UINIT(ICFD),URFCU(ICFD)
        write (dtmp,'(a)') '  0.001  0.50 '
        call EASKS(ltmp,'Initial value and relaxation factor',
     &    'for X velocity? ',16,dtmp,'U def and relx',IER,nbhelp)
        K=0
        call EGETWR(ltmp,K,VAL,-99.0,99.0,'F','Uvel',IER)
        if (IER.eq.0) UINIT(ICFD)=VAL
        call EGETWR(ltmp,K,VAL,0.0,1.5,'F','Urlxf',IER)
        if (IER.eq.0) URFCU(ICFD)=VAL
      elseif(INO.EQ.3)then

C Ask for initial value and relaxation factor in Y dir.
        write (ltmp,'(f8.4,f6.2)') VINIT(ICFD),URFCW(ICFD)
        write (dtmp,'(a)') '  0.001  0.50 '
        call EASKS(ltmp,'Initial value and relaxation factor',
     &    'for Y velocity? ',16,dtmp,'U def and relx',IER,nbhelp)
        K=0
        call EGETWR(ltmp,K,VAL,-99.0,99.0,'F','Vvel',IER)
        if (IER.eq.0) VINIT(ICFD)=VAL
        call EGETWR(ltmp,K,VAL,0.0,1.5,'F','Vrlxf',IER)
        if (IER.eq.0) URFCV(ICFD)=VAL
      elseif(INO.EQ.4)then

C Ask for initial value and relaxation factor in Z dir.
        write (ltmp,'(f8.4,f6.2)') WINIT(ICFD),URFCW(ICFD)
        write (dtmp,'(a)') '  0.001  0.50 '
        call EASKS(ltmp,'Initial value and relaxation factor',
     &    'for W velocity? ',16,dtmp,'W def and relx',IER,nbhelp)
        K=0
        call EGETWR(ltmp,K,VAL,-99.0,99.0,'F','Wvel',IER)
        if (IER.eq.0) WINIT(ICFD)=VAL
        call EGETWR(ltmp,K,VAL,0.0,1.5,'F','Wrlxf',IER)
        if (IER.eq.0) URFCW(ICFD)=VAL
      elseif(INO.EQ.6)then

C (De)activate temperature solution.
        if (.NOT.CALLT(ICFD)) then
          CALLT(ICFD)=.TRUE.
        else
          CALLT(ICFD)=.FALSE.
        endif
      elseif(INO.EQ.7)then

C Ask for initial value and relaxation factor for temperatures.
        write (ltmp,'(2f6.2)') TINIT(ICFD),URFCT(ICFD)
        write (dtmp,'(a)') '  20.00  0.90 '
        call EASKS(ltmp,'Initial value and relaxation factor',
     &    'for temperature? ',16,dtmp,'W def and relx',IER,nbhelp)
        K=0
        call EGETWR(ltmp,K,VAL,-50.0,99.0,'F','Wvel',IER)
        if (IER.eq.0) TINIT(ICFD)=VAL
        call EGETWR(ltmp,K,VAL,0.0,1.5,'F','Wrlxf',IER)
        if (IER.eq.0) URFCT(ICFD)=VAL
      elseif(INO.EQ.9)then

C Toggle through turbulence methods.
        ITURB(ICFD)=ITURB(ICFD)+1
        if (ITURB(ICFD).gt.4) ITURB(ICFD)=0
      elseif(INO.EQ.10)then

C Edit turbulence data according to current method.
        if (ITURB(ICFD).eq.1) then

C Ask for initial value and relaxation factor for k.
          write (ltmp,'(f8.4,f6.2)') TEINIT(ICFD),URFCK(ICFD)
          write (dtmp,'(a)') ' 0.005  0.7'
          call EASKS(ltmp,'Initial value and relaxation factor',
     &      'for turbulent energy? ',16,dtmp,'k def and relx',
     &      IER,nbhelp)
          K=0
          call EGETWR(ltmp,K,VAL,0.0,10.0,'F','k init',IER)
          if (IER.eq.0) TEINIT(ICFD)=VAL
          call EGETWR(ltmp,K,VAL,0.0,1.5,'F','k rlxf',IER)
          if (IER.eq.0) URFCK(ICFD)=VAL
        elseif (ITURB(ICFD).eq.2) then

C Ask for fixed eddy viscosity.
          write (ltmp,'(f6.2)') rMOOT
          write (dtmp,'(a)') '  90.00'
          call EASKS(ltmp,'Fixed value for eddy viscosity?',
     &               ' ',16,dtmp,'eddy viscosity',IER,nbhelp)
          K=0
          call EGETWR(ltmp,K,VAL,0.0,500.0,'F','eddy viscos',IER)
          if (IER.eq.0) rMOOT=VAL
        elseif(ITURB(ICFD).eq.4) then

C Ask for fixed eddy viscosity and number of iterations to apply this for.
          write (ltmp,'(f6.2,i6)') rMOOT,nZtoKE
          write (dtmp,'(a)') '  90.00  10 '
          call EASKS(ltmp,'Fixed value for eddy viscosity and number ',
     &      'of iterations to apply for?',16,dtmp,'eddy viscosity',
     &      IER,nbhelp)
          K=0
          call EGETWR(ltmp,K,VAL,0.0,500.0,'F','eddy viscos',IER)
          if (IER.eq.0) rMOOT=VAL
          call EGETWI(ltmp,K,IVAL,0,500,'F','no of iters',IER)
          if (IER.eq.0) nZtoKE=IVAL
        endif
      elseif(INO.EQ.11)then
        if (ITURB(ICFD).eq.1) then

C Ask for initial value and relaxation factor for epsilon.
          write (ltmp,'(f8.4,f6.2)') EDINIT(ICFD),URFCE(ICFD)
          write (dtmp,'(a)') ' 0.005  0.7'
          call EASKS(ltmp,'Initial value and relaxation factor',
     &      'for energy dissipation? ',16,dtmp,'e def and relx',
     &      IER,nbhelp)
          K=0
          call EGETWR(ltmp,K,VAL,0.0,10.0,'F','e init',IER)
          if (IER.eq.0) EDINIT(ICFD)=VAL
          call EGETWR(ltmp,K,VAL,0.0,1.5,'F','e rlxf',IER)
          if (IER.eq.0) URFCE(ICFD)=VAL
        endif
      elseif(INO.EQ.13)then

C Buoyancy.
        IBUOY(ICFD)=IBUOY(ICFD)+1
        if (IBUOY(ICFD).gt.2) IBUOY(ICFD)=0
      elseif(INO.EQ.14)then

C Ask for reference temperature.
        write (ltmp,'(f6.2)') TBAR
        write (dtmp,'(a)') ' 20.00'
        call EASKS(ltmp,'Reference temperature? ','  ',16,
     &             dtmp,'ref temperature',IER,nbhelp)
        K=0
        call EGETWR(ltmp,K,VAL,-50.0,99.0,'F','ref temper',IER)
        if (IER.eq.0) TBAR=VAL
      elseif(INO.GT.15.AND.INO.LE.(15+NCTM))then

C Contaminants
        IF(CALPOL(INO-15,ICFD))THEN
          CALPOL(INO-15,ICFD)=.FALSE.
        ELSE
          CALPOL(INO-15,ICFD)=.TRUE.
        ENDIF
      elseif(INO.EQ.18+NCTM)then

C Mean age of air.
        if (CALLMA(ICFD)) then
          CALLMA(ICFD)=.false.
        else
          CALLMA(ICFD)=.true.
        endif
      elseif (INO.eq.22+NCTM) then

C Convergence criteria.
        helptopic='cfd_convergence_crit'
        call gethelptext(helpinsub,helptopic,nbhelp)
        ival=MAXITR(ICFD)
        CALL EASKI(ival,' ',' Maximum iterations to perform?',
     &    0,'F',10000,'F',500,'iterations',IERI,nbhelp)
        if(ieri.eq.-3)then
          continue
        else
          MAXITR(ICFD)=ival
        endif
      elseif (INO.eq.23+NCTM) then
        helptopic='cfd_convergence_crit'
        call gethelptext(helpinsub,helptopic,nbhelp)
        VAL=SRMAX(ICFD)
        CALL EASKR(VAL,' ',' Maximum equation residual?',
     &    0.,'F',1.,'F',0.02,'iterations',IER,nbhelp)
        SRMAX(ICFD)=VAL

      elseif (INO.eq.24+NCTM) then

C "Override non-convergence" in coupling mode ...
        helptopic='cfd_override_stop_in_coupling'
        call gethelptext(helpinsub,helptopic,nbhelp)
        ival=NOSTOP
        CALL EASKI(ival,' ',' Disregard non-convergence?',
     &             0,'F',4,'F',0,'non-convergence',IERI,nbhelp)
        if(ieri.eq.-3)then
          continue
        else
          NOSTOP=ival
        endif
      elseif (INO.eq.25+NCTM) then

C "Number of iterations per time step in coulpling mode"
        helptopic='cfd_numiter_in_coupling'
        call gethelptext(helpinsub,helptopic,nbhelp)
        ival=MITMFS
        CALL EASKI(ival,' ',' Number of iterations betw. mfs-dfs?',
     &             1,'F',30,'F',20,'iter mfs-dfs',IERI,nbhelp)
        if(ieri.eq.-3)then
          continue
        else
          MITMFS=ival
        endif
      elseif (INO.eq.27+NCTM) then

C Monitoring cell.
        write (ltmp,'(3i4)') IMONT(ICFD)-1,JMONT(ICFD)-1,KMONT(ICFD)-1
        write (dtmp,'(a)') ' 1  1  1'
        call EASKS(ltmp,'Monitoring cell (i,j,k)? ','  ',16,
     &             dtmp,'monit cell',IER,nbhelp)
        K=0
        call EGETWI(ltmp,K,IVAL,0,NTCELX,'W','I mon cell',IER)
        if (IER.eq.0) IMONT(ICFD)=IVAL+1
        call EGETWI(ltmp,K,IVAL,0,NTCELY,'W','J mon cell',IER)
        if (IER.eq.0) JMONT(ICFD)=IVAL+1
        call EGETWI(ltmp,K,IVAL,0,NTCELZ,'W','K mon cell',IER)
        if (IER.eq.0) KMONT(ICFD)=IVAL+1
      elseif(INO.eq.(NITMS-1)) then

C Help.
        helptopic='cfd_equations_to_edit'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('solution variables',nbhelp,'-',0,0,IER)
      elseif(INO.eq.NITMS) then
        return
      endif
      goto 5

      end

C ************************** CFDBVAR **************************
C CFDBVAR controls the editing of CFD boundary variables.
C ITRU unit number for user output.
C IER=0 indicates no error.

      SUBROUTINE CFDBVAR(IZONE,IER)
#include "building.h"
#include "model.h"
C #include "geometry.h"
#include "cfd.h"

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/FILEP/IFIL
      integer menuchw,igl,igr,igt,igb,igw,igwh
      common/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      common/gzonpik/izgfoc,nzg,nznog(mcom)

      common/RAY2/ITDSP,ITBND,ITEPT,ITZNM,ITSNM,ITVNO,ITORG,ITSNR,
     &            ITOBS,ITHLS,ITHLZ,ITGRD,GRDIS,ITPPSW
      common/RAY3/MODIFY,MODLEN,MODBND

      COMMON/ICFNOD/ICFD,ICP
      common/cfdconf/ICFBLD(MNZ),ICFMFS(MNZ)
      common/KEYVOLS/NVOL(MNZ),IVOLF(MNVLS,MNZ),IVCELLS(MNVLS,MNZ,2),
     &               JVCELLS(MNVLS,MNZ,2),KVCELLS(MNVLS,MNZ,2)
      common/KEYVOLN/VOLNAME(MNVLS,MNZ),VCsurf(MNVLS,MNZ)
      common/KEYVDAT/IVTYPE(MNVLS,MNZ),VOLTemp(MNVLS,MNZ),
     &          VOLHeat(MNVLS,MNZ),IVConfl(MNVLS,MNZ),VOLHum(MNVLS,MNZ),
     &          VOLCO2(MNVLS,MNZ),VOLVel(MNVLS,MNZ),VOLDir(MNVLS,MNZ,2),
     &          VOLArea(MNVLS,MNZ),VOLPres(MNVLS,MNZ),
     &          VOLPol(MCTM,MNVLS,MNZ)
      common/GRIDEFN/origin,xgrid,ygrid,zgrid,zegrid,ortho

      dimension IDEL(MNVLS)

      character ITEM(MNVLS+8)*40
      character KEY*1,t24*24
      character VOLNAME*12,VCsurf*12,t12*12,NAMES(MNVLS)*12
      logical origin,xgrid,ygrid,zgrid,zegrid,ortho
      logical MODIFY,MODLEN,MODBND

      integer iglib   ! if 1 then X11, if 2 then GTK, if 3 then text only.
      integer INB     ! for radio button
      integer NITMS,INO ! max items and current menu item

#ifdef OSI
      integer iigl,iigr,iigt,iigb,iigw,iigwh
      integer iiw1,iiw2,iiw3,iiw4,iimenu
#else
      integer*8 iigl,iigr,iigt,iigb,iigw,iigwh
      integer*8 iiw1,iiw2,iiw3,iiw4,iimenu
#endif

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='CFDBVAR'  ! set for subroutine

C << Note: for more complex domains grow this menu needs
C << to be one that can support multiple pages.

C Reset the display bounds for grid display over the zone
C and if in graphic mode go ahead and display.
      IF(MMOD.EQ.8)THEN

C Set up grid information in case it was not already done
C and clear graphic display.
        CALL INICNT
        CALL GRID(ier)
        call startbuffer()

C Setup and pass in parameters to win3d.
        iiw1=6; iiw2=6; iiw3=2; iiw4=4; iimenu=menuchw
        iigl=igl; iigr=igr; iigt=igt; iigb=igb; iigw=igw; iigwh=igwh
        CALL win3d(iimenu,iiw1,iiw2,iiw3,iiw4,
     &    iigl,iigr,iigt,iigb,iigw,iigwh)
        igl=int(iigl); igr=int(iigr); igt=int(iigt); igb=int(iigb)
        igw=int(iigw); igwh=int(iigwh)
        call VGRID3D('zn ','xyz',izone)
      ENDIF

C Create menu text.
 5    M=0
      do 10 I=1,NVOL(ICFD)
        M=M+1
        call EMKEY(M,KEY,IER)
        t24='  '
        if (IVTYPE(I,ICFD).eq.1) then
          t24='Solid  | Temp'
        elseif (IVTYPE(I,ICFD).eq.2) then
          t24='Solid  | Heat'
        elseif (IVTYPE(I,ICFD).eq.3) then
          t24='Solid  | Symmetrical'
        elseif (IVTYPE(I,ICFD).eq.4) then
          t24='Solid  | Conflated'
        elseif (IVTYPE(I,ICFD).eq.5) then
          t24='Solid  | Conflated'
        elseif (IVTYPE(I,ICFD).eq.6) then
          t24='Solid  | Conflated'
        elseif (IVTYPE(I,ICFD).eq.10) then
          t24='Opening| Pressure'
          IF (ICFMFS(ICFD).EQ.1)T24='Opening| Mass flow nwk'
        elseif (IVTYPE(I,ICFD).eq.11) then
          t24='Opening| Velocity'
          IF (ICFMFS(ICFD).EQ.1)T24='Opening| Mass flow nwk'
        elseif (IVTYPE(I,ICFD).eq.12) then
          t24='Opening| Zero gradient'
          IF (ICFMFS(ICFD).EQ.1)T24='Opening| Mass flow nwk'
        elseif (IVTYPE(I,ICFD).eq.13) then
          t24='Opening| Mass flow nwk'
          IF (ICFMFS(ICFD).EQ.1)T24='Opening| Mass flow nwk'
        elseif (IVTYPE(I,ICFD).eq.20) then
          t24='Source | Contaminant'
        elseif (IVTYPE(I,ICFD).eq.30) then
          t24='Blockage'
        else
          t24='UNKNOWN'
        endif
        write (ITEM(M),'(a,1x,3a)') KEY,VOLNAME(I,ICFD),':',t24
 10   continue
      ITEM(M+1)=' --------------------'
      ITEM(M+2)='+ Add/ delete a boundary definition'
      ITEM(M+3)='! View current gridding'
      ITEM(M+4)='? Help'
      ITEM(M+5)='- exit'
      NITMS=M+5

C Help text for this menu.
      helptopic='cfd_boundary_variables'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Display menu.
      INO=-1
      CALL EMENU('Boundary conditions',ITEM,NITMS,INO)

C Do things in reverse order here.
      if (INO.eq.NITMS) then

C Return
        return
      elseif (INO.eq.(NITMS-1)) then

C Help.
        helptopic='cfd_boundary_variables'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('boundary conditions',nbhelp,'-',0,0,IER)
      elseif (INO.eq.(NITMS-2)) then

C View the current gridding.
        if (origin.and.xgrid.AND.ygrid.AND.zgrid) then
          call cgd(izone,ierr)

C Restore standard graphics window variables.
          if(MMOD.EQ.8)then
            iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
            if(iglib.eq.1)then
              CALL feedbox(menuchw,2,igfw,igfh)
              CALL opengdisp(menuchw,LIMTTY,2,igdw,igdh)
            endif

C Setup and pass in parameters to win3d.
            iiw1=4; iiw2=1; iiw3=1; iiw4=3; iimenu=menuchw
            iigl=igl; iigr=igr; iigt=igt; iigb=igb; iigw=igw;iigwh=igwh
            CALL win3d(iimenu,iiw1,iiw2,iiw3,iiw4,
     &        iigl,iigr,iigt,iigb,iigw,iigwh)
            igl=int(iigl); igr=int(iigr); igt=int(iigt); igb=int(iigb)
            igw=int(iigw); igwh=int(iigwh)
            call startbuffer()

C Redraw zone and vertex points. Note, if user has returned
C from 3D grid visualisation then G1 common blocks will need
C to be refreshed.
            nzg=1
            nznog(1)=izone
            izgfoc=izone
            call georead(IFIL+1,LGEOM(nznog(1)),nznog(1),1,IUOUT,IER)
            CALL ESCZONE(izone)
            CALL BNDOBJ(0,IER)
            CALL ERCZONE(izone)
            MODIFY=.TRUE.
            MODLEN=.TRUE.
            MODBND=.TRUE.
            itvno=0
            itsnm=0
            CALL INLNST(1)
            CALL ADJVIEW(IER)
          endif
        else
          CALL USRMSG(' Define origin of CFD domain ',
     &         ' and grid axes first ','W')
        endif
      elseif (INO.eq.(NITMS-3)) then

C Add a new/ delete a boundary definition.
        call easkabc('Add/ delete a boundary condition?','  ','Add',
     &               'Delete','Cancel',IAD,nbhelp)
        if (IAD.eq.1) then
          INB=1
          call EASKATOG('Which type of boundary?','  ',
     &       'Air flow opening','Solid surface','Source',
     &       'Blockage','Cancel',' ',' ',INB,nbhelp)

C Set up some defaults and call relavent editing routine.
          if (INB.eq.1) then
            NVOL(ICFD)=NVOL(ICFD)+1
            IVTYPE(NVOL(ICFD),ICFD)=11
            IVOLF(NVOL(ICFD),ICFD)=0
            do 20 I=1,2
              IVCELLS(NVOL(ICFD),ICFD,I)=0
              JVCELLS(NVOL(ICFD),ICFD,I)=0
              KVCELLS(NVOL(ICFD),ICFD,I)=0
 20         continue
            write (t12,'(a,i3)') 'Open',NVOL(ICFD)
            call st2file(t12,VOLNAME(NVOL(ICFD),ICFD))
            VCsurf(NVOL(ICFD),ICFD)='  '
            VOLTemp(NVOL(ICFD),ICFD)=20.
            call EDAIRO(NVOL(ICFD),IER)
          elseif (INB.eq.2) then
            NVOL(ICFD)=NVOL(ICFD)+1
            IVTYPE(NVOL(ICFD),ICFD)=1
            IVOLF(NVOL(ICFD),ICFD)=0
            do 21 I=1,2
              IVCELLS(NVOL(ICFD),ICFD,I)=0
              JVCELLS(NVOL(ICFD),ICFD,I)=0
              KVCELLS(NVOL(ICFD),ICFD,I)=0
 21         continue
            write (t12,'(a,i3)') 'Wall',NVOL(ICFD)
            call st2file(t12,VOLNAME(NVOL(ICFD),ICFD))
            VCsurf(NVOL(ICFD),ICFD)='  '
            call EDSLDB(IZONE,NVOL(ICFD),IER)
          elseif (INB.eq.3) then
            NVOL(ICFD)=NVOL(ICFD)+1
            IVTYPE(NVOL(ICFD),ICFD)=20
            IVOLF(NVOL(ICFD),ICFD)=9
            do 22 I=1,2
              IVCELLS(NVOL(ICFD),ICFD,I)=0
              JVCELLS(NVOL(ICFD),ICFD,I)=0
              KVCELLS(NVOL(ICFD),ICFD,I)=0
 22         continue
            write (t12,'(a,i3)') 'Source',NVOL(ICFD)
            call st2file(t12,VOLNAME(NVOL(ICFD),ICFD))
            VCsurf(NVOL(ICFD),ICFD)='  '
            call EDSRC(NVOL(ICFD),IER)
          elseif (INB.eq.4) then
            NVOL(ICFD)=NVOL(ICFD)+1
            IVTYPE(NVOL(ICFD),ICFD)=30
            IVOLF(NVOL(ICFD),ICFD)=8
            do 23 I=1,2
              IVCELLS(NVOL(ICFD),ICFD,I)=0
              JVCELLS(NVOL(ICFD),ICFD,I)=0
              KVCELLS(NVOL(ICFD),ICFD,I)=0
 23         continue
            write (t12,'(a,i3)') 'Block',NVOL(ICFD)
            call st2file(t12,VOLNAME(NVOL(ICFD),ICFD))
            VCsurf(NVOL(ICFD),ICFD)='  '
            call EDBLK(NVOL(ICFD),IER)
          endif
        elseif (IAD.eq.2) then

C Delete a boundary condition. First, ask which one.
          if (NVOL(ICFD).gt.0) then
            INDEL=1
            do 233 I=1,NVOL(ICFD)
              NAMES(I)=VOLNAME(I,ICFD)
 233        continue
            INVOL=NVOL(ICFD)
            call EPICKS(INDEL,IDEL,' ',' ',12,INVOL,NAMES,
     &        'Key volume name',IER,nbhelp)
     
C Move all definitions greater than selected volume up one slot.
            do 333 I=IDEL(1),NVOL(ICFD)
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
              else
                IVOLF(I,ICFD)=0
                IVCELLS(I,ICFD,1)=0
                IVCELLS(I,ICFD,2)=0
                JVCELLS(I,ICFD,1)=0
                JVCELLS(I,ICFD,2)=0
                KVCELLS(I,ICFD,1)=0
                KVCELLS(I,ICFD,2)=0
                VOLNAME(I,ICFD)=' '
                VCsurf(I,ICFD)=' '
                IVTYPE(I,ICFD)=0
                VOLTemp(I,ICFD)=0.
                VOLHeat(I,ICFD)=0.
                IVConfl(I,ICFD)=0
                VOLHum(I,ICFD)=0.
                VOLCO2(I,ICFD)=0.
                VOLVel(I,ICFD)=0.
                VOLDir(I,ICFD,1)=0.
                VOLDir(I,ICFD,2)=0.
                VOLArea(I,ICFD)=0.
                VOLPres(I,ICFD)=0.
              endif
 333        continue
          NVOL(ICFD)=NVOL(ICFD)-1
          endif
        endif
          
      elseif (INO.le.NVOL(ICFD).and.INO.ge.1) then

C Existing volume, edit according to type.
        if (IVTYPE(INO,ICFD).ge.1.AND.IVTYPE(INO,ICFD).le.6) then
          call EDSLDB(IZONE,INO,IER)
        elseif (IVTYPE(INO,ICFD).ge.10.AND.IVTYPE(INO,ICFD).le.13) then
          call EDAIRO(INO,IER)
        elseif (IVTYPE(INO,ICFD).eq.20) then
          call EDSRC(INO,IER)
        elseif (IVTYPE(INO,ICFD).eq.30) then
          call EDBLK(INO,IER)
        endif
      endif
      goto 5

      end


C ************************** EDAIRO **************************
C EDAIRO controls the editing of CFD air flow opening boundary variables.
C IER=0 indicates no error.

      SUBROUTINE EDAIRO(IAO,IER)
C#include "building.h"
#include "cfd.h"

      COMMON/ICFNOD/ICFD,ICP

      common/cfdconf/ICFBLD(MNZ),ICFMFS(MNZ)
      common/KEYVOLS/NVOL(MNZ),IVOLF(MNVLS,MNZ),IVCELLS(MNVLS,MNZ,2),
     &               JVCELLS(MNVLS,MNZ,2),KVCELLS(MNVLS,MNZ,2)
      common/KEYVOLN/VOLNAME(MNVLS,MNZ),VCsurf(MNVLS,MNZ)
      common/KEYVDAT/IVTYPE(MNVLS,MNZ),VOLTemp(MNVLS,MNZ),
     &          VOLHeat(MNVLS,MNZ),IVConfl(MNVLS,MNZ),VOLHum(MNVLS,MNZ),
     &          VOLCO2(MNVLS,MNZ),VOLVel(MNVLS,MNZ),VOLDir(MNVLS,MNZ,2),
     &          VOLArea(MNVLS,MNZ),VOLPres(MNVLS,MNZ),
     &          VOLPol(MCTM,MNVLS,MNZ)

      character ITMSS(13)*50
      character face*6,ltmp*17,t12*12,d12*12
      character VOLNAME*12, VCsurf*12,t20*20
      integer IWO  ! for radio button
      integer NITMS,INO ! max items and current menu item

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='EDAIRO'  ! set for subroutine

C Adjust each cell by -1 as dfs adds fictitious cells around the domain.
      Ii=IVCELLS(IAO,ICFD,1)-1
      If=IVCELLS(IAO,ICFD,2)-1
      Ji=JVCELLS(IAO,ICFD,1)-1
      Jf=JVCELLS(IAO,ICFD,2)-1
      Ki=KVCELLS(IAO,ICFD,1)-1
      Kf=KVCELLS(IAO,ICFD,2)-1

C Check if new opening.
      if (Ii.lt.0.or.If.lt.0.or.
     &    Ji.lt.0.or.Jf.lt.0.or.
     &    Ki.lt.0.or.Kf.lt.0) then 
        t20='Air opening'
        call PIKCELS(t20,VOLNAME(IAO,ICFD),Ii,If,Ji,Jf,Ki,Kf,
     &                                           IVOLF(IAO,ICFD),IER)
        IVCELLS(IAO,ICFD,1)=Ii+1
        IVCELLS(IAO,ICFD,2)=If+1
        JVCELLS(IAO,ICFD,1)=Ji+1
        JVCELLS(IAO,ICFD,2)=Jf+1
        KVCELLS(IAO,ICFD,1)=Ki+1
        KVCELLS(IAO,ICFD,2)=Kf+1
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
      write (ITMSS(1),'(a,a)')   'a Name: ',VOLNAME(IAO,ICFD)
      write (ITMSS(2),'(a,a)')   'b Type: ',ltmp
      write (ITMSS(3),'(a)')     ' -----------------------------'
      write (ITMSS(4),'(a,a)')   '         Is  If  Js  Jf  Ks  Kf '
      write (ITMSS(5),'(a,6(i3,1x))') 'c Cells:',Ii,If,Ji,Jf,Ki,Kf
      write (ITMSS(7),'(a)')     ' -----------------------------'
      if (IVTYPE(IAO,ICFD).eq.10) then
        write(ITMSS(6),'(a)')   '  '
        write(ITMSS(8),'(a,f6.2)')'e Pressure (Pa):',VOLPres(IAO,ICFD)
      elseif (IVTYPE(IAO,ICFD).eq.11) then
        write(ITMSS(6),'(a,a)')   '  Face: ',FACE
        write(ITMSS(8),'(a,f6.2,a,2f6.2)')'e Flow rate (kg/s):',
     &        VOLVel(IAO,ICFD),', Dir (deg):',(VOLDir(IAO,ICFD,I),I=1,2)
      elseif (IVTYPE(IAO,ICFD).eq.12) then
        write(ITMSS(6),'(a,a)')   '  Face: ',FACE
        write(ITMSS(8),'(a)')   '  '
      elseif (IVTYPE(IAO,ICFD).eq.13) then
        write(ITMSS(6),'(a)')   '  '
        write(ITMSS(8),'(a,f6.2)')'e Mass (kg/s):',VOLVel(IAO,ICFD)
      endif
      write(ITMSS(9),'(a,f6.2)')'f Temperature (degC):',
     &                                             VOLTemp(IAO,ICFD)
      if (VOLArea(IAO,ICFD).gt.0.) then
        write(ITMSS(10),'(a,f6.2)')'g Real area (m^2):',
     &                                             VOLArea(IAO,ICFD)
      else
        write(ITMSS(10),'(a)')'g Real area (m^2): cell area'
      endif
      ITMSS(11)=' -----------------------------'
      ITMSS(12)='? Help'
      ITMSS(13)='- exit'
      NITMS=13

C Help text for this menu.
      helptopic='cfd_air_boundary'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Display menu.
      INO=-1
      CALL EMENU('Air flow opening edit',ITMSS,NITMS,INO)
      if (INO.eq.1) then
        t12=VOLNAME(IAO,ICFD)
        d12='  '
        call EASKS(t12,'Opening name? ','  ',12,d12,'open name',
     &    IER,nbhelp)
        if (IER.eq.0) then
          call st2name(t12,d12)
          VOLNAME(IAO,ICFD)=d12
        endif
      elseif (INO.eq.2) then

C Choose an opening type.
        IWO=1
        CALL EASKATOG('Opening types: (see help)',' ','pressure',
     &    'velocity','zero gradient','from flow network',
     &    'cancel','  ','  ',IWO,nbhelp)
        IF(IWO.EQ.4)THEN
          ICFMFS(ICFD)=1
          CALL MFCONF(ICFD)
        ENDIF

C Set BC type.
        if (IWO.ge.1.AND.IWO.le.3) THEN
          IVTYPE(IAO,ICFD)=IWO+9
          ICFMFS(ICFD)=0
        ENDIF

C Reset location to whole of volume for non face choices.
        if (IWO.eq.1.OR.IWO.eq.3) then
          IVOLF(IAO,ICFD)=7
        else
          t20='Air opening'
          call PIKFACE(t20,VOLNAME(IAO,ICFD),Ii,If,Ji,Jf,Ki,Kf,
     &      IVOLF(IAO,ICFD),IER)
        endif
      elseif (INO.eq.5) then
        t20='Air opening'
        call PIKCELS(t20,VOLNAME(IAO,ICFD),Ii,If,Ji,Jf,Ki,Kf,
     &    IVOLF(IAO,ICFD),IER)
        IVCELLS(IAO,ICFD,1)=Ii+1
        IVCELLS(IAO,ICFD,2)=If+1
        JVCELLS(IAO,ICFD,1)=Ji+1
        JVCELLS(IAO,ICFD,2)=Jf+1
        KVCELLS(IAO,ICFD,1)=Ki+1
        KVCELLS(IAO,ICFD,2)=Kf+1
      elseif (INO.eq.8) then
        if (IVTYPE(IAO,ICFD).eq.10) then
          VAL=VOLPres(IAO,ICFD)
          call EASKR(VAL,' ',' Pressure (Pa) of cells?',
     &      0.,'W',10000.,'W',50.0,'pressure',IER,nbhelp)
          if (IER.eq.0) VOLPres(IAO,ICFD)=VAL
        elseif (IVTYPE(IAO,ICFD).eq.11) then
          VAL=VOLVel(IAO,ICFD)
          call EASKR(VAL,' ',
     &      'Mass flow rate (kg/s) at chosen face of cells?',
     &       -50.,'W',50.,'W',0.5,'flow rate',IER,nbhelp)
          if (IER.eq.0) VOLVel(IAO,ICFD)=VAL
        elseif (IVTYPE(IAO,ICFD).eq.13) then
          VAL=VOLVel(IAO,ICFD)
          call EASKR(VAL,' ','Mass flow rate (kg/s) entering cells?',
     &      -10.,'W',10.,'W',0.02,'mass flow',IER,nbhelp)
          if (IER.eq.0) VOLVel(IAO,ICFD)=VAL
        endif
      elseif (INO.eq.9) then
        VAL=VOLTemp(IAO,ICFD)
        call EASKR(VAL,' ','Temperature (degC) of air entering domian?',
     &     -50.,'W',50.,'W',20.,'temperature',IER,nbhelp)
        if (IER.eq.0) VOLTemp(IAO,ICFD)=VAL
      elseif (INO.eq.9) then
        VAL=VOLArea(IAO,ICFD)
        call EASKR(VAL,' ','Real area (m^2) of opening?',
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

C ************************** MFCONF **************************
C MFCONF defines conflation of CFD and mfs 

      SUBROUTINE MFCONF(ICFD)
#include "cfd.h"
#include "net_flow.h"

      common/KEYVOLS/NVOL(MNZ),IVOLF(MNVLS,MNZ),IVCELLS(MNVLS,MNZ,2),
     &               JVCELLS(MNVLS,MNZ,2),KVCELLS(MNVLS,MNZ,2)
      common/KEYVOLN/VOLNAME(MNVLS,MNZ),VCsurf(MNVLS,MNZ)
      common/KEYVDAT/IVTYPE(MNVLS,MNZ),VOLTemp(MNVLS,MNZ),
     &          VOLHeat(MNVLS,MNZ),IVConfl(MNVLS,MNZ),VOLHum(MNVLS,MNZ),
     &          VOLCO2(MNVLS,MNZ),VOLVel(MNVLS,MNZ),VOLDir(MNVLS,MNZ,2),
     &          VOLArea(MNVLS,MNZ),VOLPres(MNVLS,MNZ),
     &          VOLPol(MCTM,MNVLS,MNZ)
      COMMON/CFDMFS1/ICFDNOD(MNZ),ICFDCNN(MNVLS,MNZ)
      DIMENSION ICPK(MCNN),INPK(MNOD)

      character VOLNAME*12, VCsurf*12
      CHARACTER PROMPT1*72, PROMPT2*72

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='MFCONF'  ! set for subroutine

C Note: use verions of askmfnod and askmfcon defined in edcfd.F
C rather than the ones defined in esrures/mfoutp.F

C Select mfs node representative of CFD domain
      helptopic='cfd_mass_flow_node'
      call gethelptext(helpinsub,helptopic,nbhelp)
      INOPT=0
      NNPK=1
      PROMPT1='Which node is represetative of CFD domain'
      PROMPT2='  '
      call ASKMFNOD(INOPT,NNPK,INPK,PROMPT1,PROMPT2,nbhelp)
      if (NNPK.gt.0.and.INPK(1).gt.0) then
        ICFDNOD(ICFD)=INPK(1)
      endif

C Select mfs connections representative of all CFD openings
      INODE=INPK(1)
      DO 100 I=1,NVOL(ICFD)
        IF(IVTYPE(I,ICFD).GE.10.AND.IVTYPE(I,ICFD).LE.13)THEN
          WRITE(PROMPT1,'(2A)')
     &    'Which connection is represented by opening ',VOLNAME(I,ICFD)
          CALL ASKMFCON(INODE,NNPK,ICPK,PROMPT1,PROMPT2)
          if (NNPK.gt.0.and.ICPK(1).gt.0) then
            ICFDCNN(I,ICFD)=ICPK(1)
          endif
        ENDIF
 100  CONTINUE

      RETURN
      END


C ************************** EDSLDB **************************
C EDSLDB controls the editing of CFD solid boundary variables.
C IER=0 indicates no error.

      SUBROUTINE EDSLDB(IZONE,ISB,IER)
#include "building.h"
#include "geometry.h"
#include "cfd.h"

      COMMON/ICFNOD/ICFD,ICP

      common/METHDS/ITURB(MNZ),IBUOY(MNZ)
      common/KEYVOLS/NVOL(MNZ),IVOLF(MNVLS,MNZ),IVCELLS(MNVLS,MNZ,2),
     &               JVCELLS(MNVLS,MNZ,2),KVCELLS(MNVLS,MNZ,2)
      common/KEYVOLN/VOLNAME(MNVLS,MNZ),VCsurf(MNVLS,MNZ)
      common/KEYVDAT/IVTYPE(MNVLS,MNZ),VOLTemp(MNVLS,MNZ),
     &          VOLHeat(MNVLS,MNZ),IVConfl(MNVLS,MNZ),VOLHum(MNVLS,MNZ),
     &          VOLCO2(MNVLS,MNZ),VOLVel(MNVLS,MNZ),VOLDir(MNVLS,MNZ,2),
     &          VOLArea(MNVLS,MNZ),VOLPres(MNVLS,MNZ),
     &          VOLPol(MCTM,MNVLS,MNZ)

      character ITMSS(12)*50
      character face*6,ltmp*16,t12*12,d12*12
      character VOLNAME*12, VCsurf*12
      CHARACTER t20*20
      integer IWO  ! for radio button
      integer NITMS,INO ! max items and current menu item

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='EDSLDB'  ! set for subroutine

C Adjust each cell by -1 as dfs adds fictitious cells around the domain.
      Ii=IVCELLS(ISB,ICFD,1)-1
      If=IVCELLS(ISB,ICFD,2)-1
      Ji=JVCELLS(ISB,ICFD,1)-1
      Jf=JVCELLS(ISB,ICFD,2)-1
      Ki=KVCELLS(ISB,ICFD,1)-1
      Kf=KVCELLS(ISB,ICFD,2)-1

C Check if new solid boundary.
      if (Ii.lt.0.or.If.lt.0.or.
     &    Ji.lt.0.or.Jf.lt.0.or.
     &    Ki.lt.0.or.Kf.lt.0) then 
        t20='Solid boundary'
        call PIKCELS(t20,VOLNAME(ISB,ICFD),Ii,If,Ji,Jf,Ki,Kf,
     &                                           IVOLF(ISB,ICFD),IER)
        IVCELLS(ISB,ICFD,1)=Ii+1
        IVCELLS(ISB,ICFD,2)=If+1
        JVCELLS(ISB,ICFD,1)=Ji+1
        JVCELLS(ISB,ICFD,2)=Jf+1
        KVCELLS(ISB,ICFD,1)=Ki+1
        KVCELLS(ISB,ICFD,2)=Kf+1
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
      write (ITMSS(1),'(a,a)')   'a Name: ',VOLNAME(ISB,ICFD)
      write (ITMSS(2),'(a,a)')   'b Type: ',ltmp
      write (ITMSS(3),'(a)')     ' -----------------------------'
      write (ITMSS(4),'(a,a)')   '         Is  If  Js  Jf  Ks  Kf '
      write (ITMSS(5),'(a,6(i3,1x))') 'c Cells:',Ii,If,Ji,Jf,Ki,Kf
      write (ITMSS(6),'(a,a)')   '  Face: ',FACE
      write (ITMSS(7),'(a)')     ' -----------------------------'
      if (IVTYPE(ISB,ICFD).eq.1) then
        write(ITMSS(8),'(a,f6.2)')'e Temperature (degC): ',
     &                                              VOLTemp(ISB,ICFD)
        write(ITMSS(9),'(a)')     '  '
      elseif (IVTYPE(ISB,ICFD).eq.2) then
        write(ITMSS(8),'(a,f7.0)')'e Heat flux (W) ',VOLHeat(ISB,ICFD)
        write(ITMSS(9),'(a)')     '  '
      elseif (IVTYPE(ISB,ICFD).eq.3) then
        write(ITMSS(8),'(a)')   '  '
        write(ITMSS(9),'(a)')   '  '
      elseif (IVTYPE(ISB,ICFD).eq.4) then
        write(ITMSS(8),'(a,a)')'e Conflated to surface: ',
     &                                              VCsurf(ISB,ICFD)
        write(ITMSS(9),'(a,i4)')'f Handshaking: ',IVConfl(ISB,ICFD)
      endif
      ITMSS(10) =' -----------------------------'
      ITMSS(11)='? Help'
      ITMSS(12)='- exit'
      NITMS=12

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
        call EASKS(t12,'Boundary name? ','  ',12,d12,'bnd name',
     &    IER,nbhelp)
        if (IER.eq.0) then
          call st2name(t12,d12)
          VOLNAME(ISB,ICFD)=d12
        endif
      elseif (INO.eq.2) then

C Choose an opening type.
        IWO=1
        CALL EASKATOG('Boundary types:',' ','temperature','heat flux',
     &    'symmetrical','building surface','cancel','  ','  ',
     &    IWO,nbhelp)

C Try to remember non conflated option if building surface chosen.
        if (IWO.eq.4) then
          if (ITURB(ICFD).eq.0) then
            call usrmsg('Building surface boundary conditions are only',
     &                'applicable in turbulent flows. ','W')
            call usrmsg('Please select a method for modelling  ',
     &         'turbulence via the solution variables menu pick. ','W')
          else
            IVTYPE(ISB,ICFD)=IVTYPE(ISB,ICFD)+3
          endif
        else
          IVTYPE(ISB,ICFD)=IWO
        endif
      elseif (INO.eq.5) then
        t20='Solid boundary'
        call PIKCELS(t20,VOLNAME(ISB,ICFD),Ii,If,Ji,Jf,Ki,Kf,
     &                                          IVOLF(ISB,ICFD),IER)
        IVCELLS(ISB,ICFD,1)=Ii+1
        IVCELLS(ISB,ICFD,2)=If+1
        JVCELLS(ISB,ICFD,1)=Ji+1
        JVCELLS(ISB,ICFD,2)=Jf+1
        KVCELLS(ISB,ICFD,1)=Ki+1
        KVCELLS(ISB,ICFD,2)=Kf+1
      elseif (INO.eq.8) then
        if (IVTYPE(ISB,ICFD).eq.1) then
          VAL=VOLTemp(ISB,ICFD)
          call EASKR(VAL,'temperature (degC) at chosen face of cells?',
     &      ' ',-100.,'W',100.,'W',10.,'temperature',IER,nbhelp)
          if (IER.eq.0) VOLTemp(ISB,ICFD)=VAL
        elseif (IVTYPE(ISB,ICFD).eq.2) then
          VAL=VOLHeat(ISB,ICFD)
          call EASKR(VAL,'Heat flux (W) at chosen face of cells?',
     &      ' ',-1000.,'W',1000.,'W',10.,'heat flux',IER,nbhelp)
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

C ************************** EDSRC **************************
C EDSRC controls the editing of CFD sources - humidity, CO2, etc.
C IER=0 indicates no error.

      SUBROUTINE EDSRC(ISB,IER)
#include "cfd.h"
#include "net_flow.h"

      COMMON/ICFNOD/ICFD,ICP
      common/KEYVOLS/NVOL(MNZ),IVOLF(MNVLS,MNZ),IVCELLS(MNVLS,MNZ,2),
     &               JVCELLS(MNVLS,MNZ,2),KVCELLS(MNVLS,MNZ,2)
      common/KEYVOLN/VOLNAME(MNVLS,MNZ),VCsurf(MNVLS,MNZ)
      common/KEYVDAT/IVTYPE(MNVLS,MNZ),VOLTemp(MNVLS,MNZ),
     &          VOLHeat(MNVLS,MNZ),IVConfl(MNVLS,MNZ),VOLHum(MNVLS,MNZ),
     &          VOLCO2(MNVLS,MNZ),VOLVel(MNVLS,MNZ),VOLDir(MNVLS,MNZ,2),
     &          VOLArea(MNVLS,MNZ),VOLPres(MNVLS,MNZ),
     &          VOLPol(MCTM,MNVLS,MNZ)
      common/KEYCASGN/IDcasgn(MNVLS,MNZ),Fcasgn(MNVLS,MNZ)
      common/EQTION3/CALLMA(MNZ),CALPOL(MCTM,MNZ),POLNAM(MCTM,MNZ),NCTM,
     &      JHUMINDX
      COMMON/SCHMTT/SCHMT(MCTM),GFM(MCTM),VCRIT(MCTM),TBOIL(MCTM),
     &       TCRIT(MCTM),ISCHMT(MCTM)
      COMMON/CTDFAF/ICTDFAF,SRCE(MNVLS,MCTM,MNZ),ICCSRC
      COMMON/CONTM0/NCONTM,NOCNTM,CONTMNAM(MCONTM)
      COMMON/CONTM5/SPMSUP(MSPMNO,MCSD),SSLINK2(MSPMNO,MNOD),
     &SPMTYP(MSPMNO),SSNAME(MSPMNO),NSPMNO,SSLINK1(MSPMNO,MCONTM)
      common/cfdconf/ICFBLD(MNZ),ICFMFS(MNZ)
      COMMON/MFLW10/NODPS(MCNN),HGTPS(MCNN),NODNE(MCNN),HGTNE(MCNN),
     &              ITPCON(MCNN),NDSCNN(MCNN,MCNS)
      COMMON/CFDMFS1/ICFDNOD(MNZ),ICFDCNN(MNVLS,MNZ)

      character ITMSS(14+MCTM)*60,ITUM(MCTM+1)*50,WORD*50
      character ltmp*26,t12*12,d12*12,t20*20,FACE*7,HOLD*32,WORD1*12
      character*12 VOLNAME,VCsurf,SRCNAM,POLNAM,CONTMNAM,SSNAME,SRCE
      CHARACTER CNAM*12,CNAME*3,NNAME*3,ITMSS1(14+MCTM)*60
      CHARACTER*60 ITMSS2(14+MCTM)*60,ITMSS3(14+MCTM)*60
      character outs*124
      LOGICAL CALPOL,CALLMA,OK,DOK
      INTEGER SPMTYP,SSLINK1,SSLINK2
      integer NITMS,INO,NITMSS1,IIO,NITMSS2,IXO,NITUM,INUO,NITMSS3 ! max items and current menu item

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='EDSRC'  ! set for subroutine

C Initialise SRCE 
      IF(ICTDFAF.NE.1)THEN
        DO 420 ICTM=1,NCTM
          DO 520 II=1,NVOL(ICFD)
            SRCE(II,ICTM,ICFD)=' '
 520      CONTINUE
 420    CONTINUE
      ENDIF

C Adjust each cell by -1 as dfs adds fictitious cells around the domain.
      Ii=IVCELLS(ISB,ICFD,1)-1
      If=IVCELLS(ISB,ICFD,2)-1
      Ji=JVCELLS(ISB,ICFD,1)-1
      Jf=JVCELLS(ISB,ICFD,2)-1
      Ki=KVCELLS(ISB,ICFD,1)-1
      Kf=KVCELLS(ISB,ICFD,2)-1

C Check if new source.
      if (Ii.lt.0.or.If.lt.0.or.
     &    Ji.lt.0.or.Jf.lt.0.or.
     &    Ki.lt.0.or.Kf.lt.0) then 
        t20='Source definition'
        call PIKCELS(t20,VOLNAME(ISB,ICFD),Ii,If,Ji,Jf,Ki,Kf,
     &                                           IVOLF(ISB,ICFD),IER)
        IVCELLS(ISB,ICFD,1)=Ii+1
        IVCELLS(ISB,ICFD,2)=If+1
        JVCELLS(ISB,ICFD,1)=Ji+1
        JVCELLS(ISB,ICFD,2)=Jf+1
        KVCELLS(ISB,ICFD,1)=Ki+1
        KVCELLS(ISB,ICFD,2)=Kf+1
      endif

C Generate text for location index
 5    FACE='  '
      if (IVOLF(ISB,ICFD).eq.1) then
        FACE='West   '
      elseif (IVOLF(ISB,ICFD).eq.2) then
        FACE='East   '
      elseif (IVOLF(ISB,ICFD).eq.3) then
        FACE='South  '
      elseif (IVOLF(ISB,ICFD).eq.4) then
        FACE='North  '
      elseif (IVOLF(ISB,ICFD).eq.5) then
        FACE='Low    '
      elseif (IVOLF(ISB,ICFD).eq.6) then
        FACE='High   '
      elseif (IVOLF(ISB,ICFD).eq.7) then
        FACE='Whole  '
      elseif (IVOLF(ISB,ICFD).eq.8) then
        FACE='Block  '
      elseif (IVOLF(ISB,ICFD).eq.9) then
        FACE='Source '
      endif

C Set boundary type string.
      ltmp=FACE
 144  if (IVTYPE(ISB,ICFD).eq.20)then
        ltmp='Static source'
        IF(ICTDFAF.EQ.1)LTMP='From ctm network'
      endif

C Create a menu showing definitions for different sources.
      write (ITMSS(1),'(a,a)')   'a Name: ',VOLNAME(ISB,ICFD)
      write (ITMSS(2),'(a,a)')   'b Type:',ltmp
      write (ITMSS(3),'(a)')     ' -----------------------------'
      write (ITMSS(4),'(a,a)')   '         Is  If  Js  Jf  Ks  Kf '
      write (ITMSS(5),'(a,6(i3,1x))') 'c Cells:',Ii,If,Ji,Jf,Ki,Kf
      write (ITMSS(6),'(a)')     ' -----------------------------'
      write(ITMSS(7),'(a,f8.0)')'e Heat flux(W)',VOLHeat(ISB,ICFD)
      write(ITMSS(8),'(a,i3)') 'f Cas gain index  ',IDCasgn(ISB,ICFD)
      write(ITMSS(9),'(a,f4.2)')'g Cas gain fraction ',FCasgn(ISB,ICFD)
      write (ITMSS(10),'(a)')    '  Contaminant | source strength '
      WRITE (ITMSS(11),'(A)')    '     name     | (kg/s) or name  '
      IF(ICTDFAF.EQ.1)THEN
        DO 141 ICTM=1,NCTM
          CALPOL(ICTM,ICFD)=.TRUE.
          WORD1=SRCE(ISB,ICTM,ICFD)
          IF(WORD1(:1).EQ.' ')WORD1='none'
          IF(JHUMINDX.NE.ICTM)THEN
            WRITE(ITMSS(11+ICTM),'(A,2X,A,1X,A)')CHAR(103+ICTM),
     &      POLNAM(ICTM,ICFD),WORD1
          ELSE
            WRITE(ITMSS(11+ICTM),'(A,2X,A,1X,2A)')CHAR(103+ICTM),
     &      POLNAM(ICTM,ICFD),WORD1,' (humidity) '
          ENDIF
 141    CONTINUE   
      ELSE
        DO 131 ICTM=1,NCTM
          CALPOL(ICTM,ICFD)=.TRUE.
          IF(JHUMINDX.NE.ICTM)THEN
            WRITE(ITMSS(11+ICTM),'(A,1X,A,1X,F12.10)')CHAR(103+ICTM),
     &      POLNAM(ICTM,ICFD),VOLPOL(ICTM,ISB,ICFD)
          ELSE
            WRITE(ITMSS(11+ICTM),'(A,1X,A,1X,F12.10,A)')CHAR(103+ICTM),
     &      POLNAM(ICTM,ICFD),VOLPOL(ICTM,ISB,ICFD),' (humidity) '
          ENDIF
 131    CONTINUE   
      ENDIF
      ITMSS(12+NCTM) =' -----------------------------'
      ITMSS(13+NCTM) = '+ Add/Delete Contaminant'
      ITMSS(14+NCTM)='? Help'
      ITMSS(15+NCTM)='- exit'
      NITMS=15+NCTM

C Help text for this menu.
  7   helptopic='cfd_source_boundary_edit'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Display menu.
      INO=-1
      IF(VOLNAME(ISB,ICFD)(1:4).EQ.'SRC_')THEN
        CALL PHELPD('Warning',nbhelp,'-',0,0,IER)
      ENDIF
      CALL EMENU('Sources edit',ITMSS,NITMS,INO)
      if (ITMSS(INO)(1:1).eq.' ') then
        goto 7
      elseif (INO.eq.1) then

C Get boundary name.
        t12=VOLNAME(ISB,ICFD)
        d12='  '
        call EASKS(t12,'Source name? ','  ',12,d12,'src name',
     &    IER,nbhelp)
        if (IER.eq.0) then
          call st2name(t12,d12)
          VOLNAME(ISB,ICFD)=d12
        endif

C Set coupling with contaminants network on/off
      elseif (INO.eq.2) then
        CALL EASKABC('How to specify coundary condition',' ','static',
     &    'from contaminants network','cancel',IHOWL,nbhelp)
        IF(IHOWL.EQ.1)THEN
          ICTDFAF=0
        ELSEIF(IHOWL.EQ.2)THEN

C Take contaminant definitions from contaminant flow network
          DOK=.TRUE.
          CALL ASKOK('Link CFD with contaminant network (see help) ?',
     &      ' ',OK,dok,nbhelp)
          IF(.NOT.OK)GOTO 144
          IF(ICFMFS(ICFD).NE.1)THEN
            CALL PHELPD(' ',nbhelp,'-',0,0,IER)
          ENDIF
          ICTDFAF=1
          NCTM=NCONTM
          IF(ICCSRC.NE.1)THEN
            ICCSRC=1

C Add one more source type boundary condition for each opening type
C boundary conditions and define the source strength to be from 
            NOBC=NVOL(ICFD)
            DO 3056 IV=1,NOBC
              IF(IVTYPE(IV,ICFD).GE.10.AND.IVTYPE(IV,ICFD).LE.13)THEN
                NVOL(ICFD)=NVOL(ICFD)+1
                NUMVOL=NVOL(ICFD)
                WRITE(VOLNAME(NUMVOL,ICFD),'(2A)')'SRC_',
     &          VOLNAME(IV,ICFD)(1:8)
                IVTYPE(NUMVOL,ICFD)=20
                IVOLF(NUMVOL,ICFD)=9
                DO 3057 L=1,2
                  IVCELLS(NUMVOL,ICFD,L)=IVCELLS(IV,ICFD,L)
                  JVCELLS(NUMVOL,ICFD,L)=JVCELLS(IV,ICFD,L)
                  KVCELLS(NUMVOL,ICFD,L)=KVCELLS(IV,ICFD,L)
 3057           CONTINUE
                VOLHEAT(NUMVOL,ICFD)=0.0
                IDCASGN(NUMVOL,ICFD)=0
                FCASGN(NUMVOL,ICFD)=0.0
                DO 3058 ICTM=1,NCTM
                  VOLPOL(ICTM,NUMVOL,ICFD)=0.0
                  CALL INTSTR(ICTM,CNAM,ILEN,IER)
                  IF(ILEN.EQ.1)THEN
                    WRITE(CNAME,'(2A)')'00',CNAM(:1)
                  ELSEIF(ILEN.EQ.2)THEN                
                    WRITE(CNAME,'(2A)')'0',CNAM(:2)
                  ELSEIF(ILEN.EQ.3)THEN
                    WRITE(CNAME,'(A)')CNAM(:3)
                  ENDIF
                  ICNN=ICFDCNN(IV,ICFD)
                  IF(NODPS(ICNN).EQ.ICFDNOD(ICFD))THEN
                    IEXTNOD=NODNE(ICNN)
                  ELSEIF(NODNE(ICNN).EQ.ICFDNOD(ICFD))THEN
                    IEXTNOD=NODPS(ICNN)
                  ELSE
                    GOTO 3059
                  ENDIF
                  CALL INTSTR(IEXTNOD,CNAM,ILEN,IER)
                  IF(ILEN.EQ.1)THEN
                    WRITE(NNAME,'(2A)')'00',CNAM(:1)
                  ELSEIF(ILEN.EQ.2)THEN                
                    WRITE(NNAME,'(2A)')'0',CNAM(:2)
                  ELSEIF(ILEN.EQ.3)THEN
                    WRITE(NNAME,'(A)')CNAM(:3)
                  ENDIF
                  WRITE(SRCE(NUMVOL,ICTM,ICFD),'(3A)')'SRC',CNAME,NNAME
 3059             CONTINUE
 3058           CONTINUE
              ENDIF
 3056       CONTINUE
          ENDIF


C Ask which contaminant (or none) is moisture 
          ITMSS1(1) = ' name:  '
          ITMSS1(2) ='         '
          DO 2122 ICTM=1,NCTM
            WRITE(ITMSS1(2+ICTM),'(2A,A12)')CHAR(96+ICTM),' ',
     &      CONTMNAM(ICTM)
 2122     CONTINUE
          ITMSS1(NCTM+3) ='# none of the above'
          ITMSS1(NCTM+4) =' ------------------------'
          ITMSS1(NCTM+5) ='? help                   '
          ITMSS1(NCTM+6) ='- exit                   '
          NITMSS1=NCTM+6
 2121     IIO=-1

C Help text for this menu.
          CALL EMENU('Moisture is ... ',ITMSS1,NITMSS1,IIO)
          IF(IIO.EQ.NCTM+6)THEN
            IF(JHUMINDX.EQ.0)THEN
              call edisp(iuout,'None of the contaminants is moisture')
              GOTO 2121
            ENDIF
          ELSEIF(IIO.EQ.NCTM+5)THEN
            call edisp(iuout,'Choose a moisture contaminant.')
            GOTO 2121
          ELSEIF(IIO.EQ.NCTM+3)THEN
            JHUMINDX=0
          ELSEIF(IIO.GT.2.AND.IIO.LE.NCTM+2)THEN
            JHUMINDX=IIO-2
          ELSE
            GOTO 2121
          ENDIF

C Take contaminant name from contaminant network
          DO 789 ICTM=1,NCTM
            POLNAM(ICTM,ICFD)=CONTMNAM(ICTM)

C Ask for information to calculate Schmidt number
            helptopic='cfd_contam_distribution'
            call gethelptext(helpinsub,helptopic,nbhelp)
            CALL EASKABC(
     &        'How to specify turbulent Schmidt #(see help)?',
     &        POLNAM(ICTM,ICFD),'parameters','manual','default',
     &        ISCHM,nbhelp)
            IF(ISCHM.EQ.1)THEN
              ISCHMT(ICTM)=1
              CALL EASKS(HOLD,
     &          'enter mol mass(g/mol),crit vol(cm^3/mol)',
     &          'boiling pt (K) and crit pt (K)',32,
     &          ' 44.0 94.0 217.0 304.0',' ',IER,nbhelp)
              K=0
              CALL EGETWR(HOLD,K,VL1,1.,1000.,'W','molar mass',IER)
              CALL EGETWR(HOLD,K,VL2,1.,1000.,'W','crit vol',IER)
              CALL EGETWR(HOLD,K,VL3,100.,500.,'W','T_boil',IER)
              CALL EGETWR(HOLD,K,VL4,100.,1000.,'W','T_crit',IER)
              GFM(ICTM)=VL1
              VCRIT(ICTM)=VL2
              TBOIL(ICTM)=VL3
              TCRIT(ICTM)=VL4
            ELSEIF(ISCHM.EQ.2)THEN
              ISCHMT(ICTM)=0
              call EASKR(VAL,'What is Turbulent Schmidt Number for',
     &          POLNAM(ICTM,ICFD),0.1,'W',2.5,'W',1.,'schmidt #',
     &          IER,nbhelp)
              SCHMT(ICTM)=VAL
            ELSEIF(ISCHM.EQ.3)THEN
              ISCHMT(ICTM)=0
              SCHMT(ICTM)=1.0
              IF(JHUMINDX.EQ.ICTM)SCHMT(ICTM)=0.59
            ENDIF
 789      CONTINUE
          helptopic='cfd_contam_sinks'
          call gethelptext(helpinsub,helptopic,nbhelp)
          DOK=.TRUE.
          CALL ASKOK('Link contaminant network source/sink with this',
     &      'CFD source type boundary condition now ?',OK,dok,nbhelp)
          IF(OK)THEN
            DO 3040 ICTM=1,NCTM
              IX=2
              ITMSS2(1)= ' Source / sink name:'
              DO 3050 ISPMNO=1,NSPMNO
                IF(SSLINK1(ISPMNO,ICTM).NE.0)THEN
                  WRITE(ITMSS2(IX),'(A,1X,A)')CHAR(96+IX-1),
     &            SSNAME(SSLINK1(ISPMNO,ICTM))
                  IX=IX+1
                ENDIF
 3050         CONTINUE
              ITMSS2(IX)  = ' '
              ITMSS2(IX+1)= '0 none'
              ITMSS2(IX+2)= ' ------------------'
              ITMSS2(IX+3)= '? help'
              ITMSS2(IX+4)= '- exit'
              NITMSS2=IX+4
              WRITE(outs,'(2A)')  'Sources and sinks for contaminant ',
     &                       CONTMNAM(ICTM)
              call edisp(iuout,outs)
              call edisp(iuout,
     &          'are displayed. Choose which one represents this')
              call edisp(iuout,
     &          'source type CFD boundary condition (if at all)')
 3041         IXO=-1
              WRITE(WORD,'(2A)')'Choose source type for contaminant ',
     &                       CONTMNAM(ICTM)
              IF(IX.EQ.2)GOTO 3042
              CALL EMENU(WORD,ITMSS2,NITMSS2,IXO)
              IF(IXO.EQ.NITMSS2)THEN
                CONTINUE
              ELSEIF(IXO.EQ.NITMSS2-1)THEN
                helptopic='cfd_contam_sinks'
                call gethelptext(helpinsub,helptopic,nbhelp)
                CALL PHELPD('Choose source/sink',nbhelp,'-',0,0,IER)
                GOTO 3041
              ELSEIF(IXO.EQ.NITMSS2-3)THEN
                SRCE(ISB,ICTM,ICFD)=' '
              ELSEIF(IXO.GT.1.AND.IXO.LT.NITMSS2-3)THEN
                SRCE(ISB,ICTM,ICFD)=SSNAME(SSLINK1(IXO-1,ICTM))
              ELSE
                GOTO 3041
              ENDIF
 3040       CONTINUE
            GOTO 3044
 3042       call edisp(iuout,
     &        'No sources/sinks defined for this contaminant')
 3044       continue
        ENDIF
        ENDIF
        GOTO 144

C Define CFD grid cells for source type boundary condition
      elseif (INO.eq.5) then
        helptopic='cfd_source_boundary_edit'
        call gethelptext(helpinsub,helptopic,nbhelp)
        t20='Source definition'
        call PIKCELS(t20,VOLNAME(ISB,ICFD),Ii,If,Ji,Jf,Ki,Kf,
     &                                          IVOLF(ISB,ICFD),IER)
        IVCELLS(ISB,ICFD,1)=Ii+1
        IVCELLS(ISB,ICFD,2)=If+1
        JVCELLS(ISB,ICFD,1)=Ji+1
        JVCELLS(ISB,ICFD,2)=Jf+1
        KVCELLS(ISB,ICFD,1)=Ki+1
        KVCELLS(ISB,ICFD,2)=Kf+1
      elseif (INO.eq.7) then
        helptopic='cfd_source_boundary_edit'
        call gethelptext(helpinsub,helptopic,nbhelp)
        VAL=VOLHeat(ISB,ICFD)
        call EASKR(VAL,'Heat flux (W) in source volume?',
     &    ' ',-1000.,'W',1000.,'W',10.,'heat flux',IER,nbhelp)
        if (IER.eq.0) VOLHeat(ISB,ICFD)=VAL
      elseif (INO.eq.8) then

C<< Should ideally popup a list of casual gain types from zone
C operations file here with some regard for standalone running of dfs
        helptopic='cfd_source_boundary_edit'
        call gethelptext(helpinsub,helptopic,nbhelp)
        IVAL=IDCasgn(ISB,ICFD)
        call EASKI(IVAL,'Which casual gain index represents heat ',
     &  'generation?',0,'W',3,'W',1,'casgn index',IERI,nbhelp)
        if(ieri.eq.-3) goto 7
        if (IER.eq.0) IDCasgn(ISB,ICFD)=IVAL
      elseif (INO.eq.9) then
        helptopic='cfd_source_boundary_edit'
        call gethelptext(helpinsub,helptopic,nbhelp)
        VAL=FCasgn(ISB,ICFD)
        call EASKR(VAL,'What fraction of casual gain is generated? ',
     &  ' ',0.,'W',1.,'W',1.,'casgn fraction',IER,nbhelp)
        if (IER.eq.0) FCasgn(ISB,ICFD)=VAL
      elseif (INO.eq.13+NCTM) then
        helptopic='cfd_contam_sinks'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKABC('Add/Delete contaminant source',' ','add',
     &      'delete','cancel',IOWL,nbhelp)
        IF(IOWL.EQ.1)THEN
          IF(ICTDFAF.EQ.1)THEN
            helptopic='cfd_contam_delete_warn'
            call gethelptext(helpinsub,helptopic,nbhelp)
            CALL PHELPD('Not allowed',nbhelp,'-',0,0,IER)
          ELSE
            helptopic='cfd_contam_sinks'
            call gethelptext(helpinsub,helptopic,nbhelp)
            SRCNAM=' '
            d12='Contaminant'
            call EASKS(SRCNAM,'Contaminant name? ','  ',12,d12,
     &      'cntmnt name',IER,nbhelp)
            NCTM=NCTM+1
            ICTM=NCTM
            POLNAM(NCTM,ICFD)=SRCNAM
            call EASKR(VAL,'What is source strength (kg/s) ',
     &      ' ',0.,'W',10.,'W',1.,'src strenght',IER,nbhelp)
            VOLPOL(NCTM,ISB,ICFD)=VAL
            CALPOL(NCTM,ICFD)=.TRUE.
            CALL ASKOK(' ','Is this contaminant water',OK,dok,0)
            IF(OK)JHUMINDX=ICTM

C Ask for information to calculate Schmidt number
            helptopic='cfd_contam_schmidt_no'
            call gethelptext(helpinsub,helptopic,nbhelp)
            CALL EASKABC('How to specify turbulent Schmidt #(see help)?'
     &       ,' ','parameters','manual','default',ISCHM,nbhelp)
            IF(ISCHM.EQ.1)THEN
              ISCHMT(ICTM)=1
              CALL EASKS(HOLD,'enter mol mass(g/mol),crit vol(cm^3/mol)'
     &        ,'boiling pt (K) and crit pt (K)',32
     &        ,' 44.0 94.0 217.0 304.0',' ',IER,nbhelp)
              K=0
              CALL EGETWR(HOLD,K,VL1,1.,1000.,'W','molar mass',IER)
              CALL EGETWR(HOLD,K,VL2,1.,1000.,'W','crit vol',IER)
              CALL EGETWR(HOLD,K,VL3,100.,500.,'W','T_boil',IER)
              CALL EGETWR(HOLD,K,VL4,100.,1000.,'W','T_crit',IER)
              GFM(ICTM)=VL1
              VCRIT(ICTM)=VL2
              TBOIL(ICTM)=VL3
              TCRIT(ICTM)=VL4
            ELSEIF(ISCHM.EQ.2)THEN
              ISCHMT(ICTM)=0
              call EASKR(VAL,'What is Turbulent Schmidt Number ',
     &           ' ',0.1,'W',2.5,'W',1.,'schmidt #',IER,nbhelp)
              SCHMT(ICTM)=VAL
            ELSEIF(ISCHM.EQ.3)THEN
              ISCHMT(ICTM)=0
              SCHMT(ICTM)=1.0
              IF(JHUMINDX.EQ.ICTM)SCHMT(ICTM)=0.59
            ENDIF
          ENDIF

C Delete Contaminant
        ELSEIF(IOWL.EQ.2)THEN
          IF(ICTDFAF.EQ.1)THEN
            helptopic='cfd_contam_delete_warn'
            call gethelptext(helpinsub,helptopic,nbhelp)
            CALL PHELPD('Not allowed',nbhelp,'-',0,0,IER)
          ELSE
            WRITE(ITUM(1),'(a)')' Contaminant name: '
            DO 132 ICTM=1,NCTM
              WRITE(ITUM(1+ICTM),'(3A)')
     &        CHAR(96+ICTM),' ',POLNAM(ICTM,ICFD)
 132        CONTINUE
            NITUM=1+NCTM
            INUO=-1
            CALL EMENU('Choose contaminant to delete',ITUM,NITUM,INUO)
            DO 133 ICTM=INUO-1,NCTM-1
              POLNAM(ICTM,ICFD)=POLNAM(ICTM+1,ICFD)
              CALPOL(ICTM,ICFD)=CALPOL(ICTM+1,ICFD)
              SCHMT(ICTM)=SCHMT(ICTM+1)
              ISCHMT(ICTM)=ISCHMT(ICTM+1)
              GFM(ICTM)=GFM(ICTM+1)
              VCRIT(ICTM)=VCRIT(ICTM+1)
              TBOIL(ICTM)=TBOIL(ICTM+1)
              TCRIT(ICTM)=TCRIT(ICTM+1)

C Reset humidity / water index if applicable
              IF(JHUMINDX.EQ.INUO)THEN
                JHUMINDX=0
              ELSEIF(JHUMINDX.GT.INUO)THEN
                JHUMINDX=JHUMINDX-1
              ENDIF

C Delete this contaminant from all key volumes
              DO 135 I=1,NVOL(ICFD)
                VOLPOL(ICTM,I,ICFD)=VOLPOL(ICTM+1,I,ICFD)
 135          CONTINUE
 133        CONTINUE
            POLNAM(NCTM,ICFD)=' '
            CALPOL(NCTM,ICFD)=.FALSE.
            DO 134 I=1,NVOL(ICFD)
              IF(IVTYPE(I,ICFD).EQ.20)VOLPOL(NCTM,I,ICFD)=0.
 134        CONTINUE
            NCTM=NCTM-1
          ENDIF
        ENDIF
        IF(ICTDFAF.NE.1)GOTO 144

C Edit contaminants 
      elseif (INO.gt.11.and.INO.lt.12+NCTM) then
        ICTM=INO-11
        IF(ICTDFAF.EQ.1)THEN
          helptopic='cfd_link_contam_sink'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL PHELPD('Contaminants Edit',10,'-',0,0,IER)
        ELSE
          helptopic='cfd_link_contam_sink'
          call gethelptext(helpinsub,helptopic,nbhelp)
          SRCNAM=POLNAM(ICTM,ICFD)
          d12='Contaminant'
          call EASKS(SRCNAM,'Contaminant name? ','  ',12,d12,
     &    'cntmnt name',IER,nbhelp)
          POLNAM(ICTM,ICFD)=SRCNAM
        ENDIF
        
        VAL=VOLPOL(ICTM,ISB,ICFD)
        call EASKR(VAL,'What is source strength (kg/s) ',
     &    ' ',0.,'W',10.,'W',1.,'src strenght',IER,nbhelp)
        VOLPOL(ICTM,ISB,ICFD)=VAL
        CALL ASKOK(' ','Is this contaminant water(humidity)',
     &    OK,dok,nbhelp)
        IF(OK)THEN
          JHUMINDX=ICTM
        ELSE
          IF(JHUMINDX.EQ.ICTM)JHUMINDX=0
        ENDIF

C Ask for information to calculate Schmidt number
        helptopic='cfd_contam_schmidt_no'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKABC('How to specify turbulent Schmidt # (see help) ?'
     &     ,' ','parameters','manual','default',ISCHM,nbhelp)
        IF(ISCHM.EQ.1)THEN
          ISCHMT(ICTM)=1
          CALL EASKS(HOLD,'enter mol mass (g/mol) crit vol (cm^3/mol)'
     &      ,'boiling pt (K) and crit pt (K)',32
     &      ,' 44.0 94.0 217.0 304.0',' ',IER,nbhelp)
          K=0
          CALL EGETWR(HOLD,K,VL1,1.,1000.,'W','molar mass',IER)
          CALL EGETWR(HOLD,K,VL2,1.,1000.,'W','crit vol',IER)
          CALL EGETWR(HOLD,K,VL3,100.,500.,'W','T_boil',IER)
          CALL EGETWR(HOLD,K,VL4,100.,1000.,'W','T_crit',IER)
          GFM(ICTM)=VL1
          VCRIT(ICTM)=VL2
          TBOIL(ICTM)=VL3
          TCRIT(ICTM)=VL4
        ELSEIF(ISCHM.EQ.2)THEN
          ISCHMT(ICTM)=0
          call EASKR(VAL,'What is Turbulent Schmidt Number ',
     &         ' ',0.5,'W',1.5,'W',1.,'schmidt #',IER,nbhelp)
          SCHMT(ICTM)=VAL
        ELSEIF(ISCHM.EQ.3)THEN
          ISCHMT(ICTM)=0
          SCHMT(ICTM)=1.0
          IF(JHUMINDX.EQ.ICTM)SCHMT(ICTM)=0.59
        ENDIF

C Now get source sink information from contaminants network
        IF(ICTDFAF.EQ.1)THEN
          helptopic='cfd_link_contam_sink'
          call gethelptext(helpinsub,helptopic,nbhelp)
          DOK=.FALSE.
          CALL ASKOK('Link contaminant network source/sink with this',
     &      'CFD source type boundary condition now ?',OK,dok,nbhelp)
          IF(OK)THEN
            IX=2
            ITMSS3(1)= ' Source / sink name:'
            DO 2050 ISPMNO=1,NSPMNO
              IF(SSLINK1(ISPMNO,ICTM).NE.0)THEN
                WRITE(ITMSS3(IX),'(A,1X,A)')CHAR(96+IX-1),
     &          SSNAME(SSLINK1(ISPMNO,ICTM))
                IX=IX+1
              ENDIF
 2050       CONTINUE
            ITMSS3(IX)  = ' '
            ITMSS3(IX+1)= '0 none'
            ITMSS3(IX+2)= ' ------------------'
            ITMSS3(IX+3)= '? help'
            ITMSS3(IX+4)= '- exit'
            NITMSS3=IX+4
            WRITE(outs,'(2A)')  'Sources and sinks for contaminant ',
     &                       CONTMNAM(ICTM)
            call edisp(iuout,outs)
            call edisp(iuout,
     &        'are displayed. Choose which one represents this')
            call edisp(iuout,
     &        'source type CFD boundary condition (if at all)')
 2041       IXO=-1
            WRITE(WORD,'(2A)')'Choose source type for contaminant ',
     &                     CONTMNAM(ICTM)
            IF(IX.EQ.2)GOTO 3043
            CALL EMENU(WORD,ITMSS3,NITMSS3,IXO)
            IF(IXO.EQ.NITMSS3)THEN
              CONTINUE
            ELSEIF(IXO.EQ.NITMSS3-1)THEN
              helptopic='cfd_contam_sinks'
              call gethelptext(helpinsub,helptopic,nbhelp)
              CALL PHELPD('Choose source/sink',nbhelp,'-',0,0,IER)
              GOTO 2041
            ELSEIF(IXO.EQ.NITMSS3-3)THEN
                SRCE(ISB,ICTM,ICFD)=' '
            ELSEIF(IXO.GT.1.AND.IXO.LT.NITMSS3-3)THEN
              SRCE(ISB,ICTM,ICFD)=SSNAME(SSLINK1(IXO-1,ICTM))
            ELSE
              GOTO 2041
            ENDIF
          ENDIF
          GOTO 3045
 3043     call edisp(iuout,
     &      'No sources/sinks defined for this contaminant')
 3045     continue
        ENDIF
      elseif (INO.eq.(NITMS-1)) then

C Create help for 'sources edit' menu
        helptopic='cfd_source_boundary_edit'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('sources edit help',nbhelp,'-',0,0,IER)
      elseif (INO.eq.NITMS) then

C Return.
        return
      else
        goto 7
      endif
        goto 5
      end


C ************************** EDBLK **************************
C EDBLK controls the editing of CFD blockages to flow
C IER=0 indicates no error.

      SUBROUTINE EDBLK(ISB,IER)
#include "cfd.h"

      COMMON/ICFNOD/ICFD,ICP

      common/KEYVOLS/NVOL(MNZ),IVOLF(MNVLS,MNZ),IVCELLS(MNVLS,MNZ,2),
     &               JVCELLS(MNVLS,MNZ,2),KVCELLS(MNVLS,MNZ,2)
      common/KEYVOLN/VOLNAME(MNVLS,MNZ),VCsurf(MNVLS,MNZ)
      common/KEYVDAT/IVTYPE(MNVLS,MNZ),VOLTemp(MNVLS,MNZ),
     &          VOLHeat(MNVLS,MNZ),IVConfl(MNVLS,MNZ),VOLHum(MNVLS,MNZ),
     &          VOLCO2(MNVLS,MNZ),VOLVel(MNVLS,MNZ),VOLDir(MNVLS,MNZ,2),
     &          VOLArea(MNVLS,MNZ),VOLPres(MNVLS,MNZ),
     &          VOLPol(MCTM,MNVLS,MNZ)

      character ITMSS(10)*50,FACE*6
      character ltmp*16,t12*12,d12*12,t20*20
      character VOLNAME*12, VCsurf*12

      logical ERROR
      integer NITMS,INO ! max items and current menu item

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='EDBLK'  ! set for subroutine
      helptopic='cfd_blockage_menu'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Adjust each cell by -1 as dfs adds fictitious cells around the domain.
      Ii=IVCELLS(ISB,ICFD,1)-1
      If=IVCELLS(ISB,ICFD,2)-1
      Ji=JVCELLS(ISB,ICFD,1)-1
      Jf=JVCELLS(ISB,ICFD,2)-1
      Ki=KVCELLS(ISB,ICFD,1)-1
      Kf=KVCELLS(ISB,ICFD,2)-1

C Check if new blockage.
      if (Ii.lt.0.or.If.lt.0.or.
     &    Ji.lt.0.or.Jf.lt.0.or.
     &    Ki.lt.0.or.Kf.lt.0) then 
        t20='Blockage'
        call PIKCELS(t20,VOLNAME(ISB,ICFD),Ii,If,Ji,Jf,Ki,Kf,
     &                                           IVOLF(ISB,ICFD),IER)
        IVCELLS(ISB,ICFD,1)=Ii+1
        IVCELLS(ISB,ICFD,2)=If+1
        JVCELLS(ISB,ICFD,1)=Ji+1
        JVCELLS(ISB,ICFD,2)=Jf+1
        KVCELLS(ISB,ICFD,1)=Ki+1
        KVCELLS(ISB,ICFD,2)=Kf+1
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
      if (IVTYPE(ISB,ICFD).eq.30) then
        ltmp='Blockage'
      endif

C Create a menu showing definitions for different conflation schemes.
      write (ITMSS(1),'(a,a)')   'a Name: ',VOLNAME(ISB,ICFD)
      write (ITMSS(2),'(3a)')   '  Type: ',ltmp
      write (ITMSS(3),'(a)')     ' -----------------------------'
      write (ITMSS(4),'(a,a)')   '         Is  If  Js  Jf  Ks  Kf '
      write (ITMSS(5),'(a,6(i4))') 'c Cells:',Ii,If,Ji,Jf,Ki,Kf
      write (ITMSS(6),'(a)')     ' -----------------------------'
      write(ITMSS(7),'(a,f6.2)')'e Heat flux (W/m^2):',VOLHeat(ISB,ICFD)
      ITMSS(8) =' -----------------------------'
      ITMSS(9)='? Help'
      ITMSS(10)='- exit'
      NITMS=10

C Help text for this menu.
      helptopic='cfd_blockage_menu'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Display menu.
      INO=-1
      CALL EMENU('Blockage edit',ITMSS,NITMS,INO)
      if (INO.eq.1) then

C Get boundary name.
        t12=VOLNAME(ISB,ICFD)
        d12=' '
        call EASKS(t12,'Blockage name? ','  ',12,d12,'blk name',
     &    IER,nbhelp)
        if (IER.eq.0) then
          call st2name(t12,d12)
          VOLNAME(ISB,ICFD)=d12
        endif
      elseif (INO.eq.5) then
 79     t20='Blockage'
        call PIKCELS(t20,VOLNAME(ISB,ICFD),Ii,If,Ji,Jf,Ki,Kf,
     &                                          IVOLF(ISB,ICFD),IER)
        IVCELLS(ISB,ICFD,1)=Ii+1
        IVCELLS(ISB,ICFD,2)=If+1
        JVCELLS(ISB,ICFD,1)=Ji+1
        JVCELLS(ISB,ICFD,2)=Jf+1
        KVCELLS(ISB,ICFD,1)=Ki+1
        KVCELLS(ISB,ICFD,2)=Kf+1

C Check for illegal volume (vol must be 2 cells thick in all directions).
        ERROR=.false.
        if (IVCELLS(ISB,ICFD,1).eq.IVCELLS(ISB,ICFD,2)) then
          call usrmsg('Blockage must be at least 2 cells thick',
     &                'in the x direction.','W')
          ERROR=.true.
        endif
        if (JVCELLS(ISB,ICFD,1).eq.JVCELLS(ISB,ICFD,2)) then
          call usrmsg('Blockage must be at least 2 cells thick',
     &                'in the y direction.','W')
          ERROR=.true.
        endif
        if (KVCELLS(ISB,ICFD,1).eq.KVCELLS(ISB,ICFD,2)) then
          call usrmsg('Blockage must be at least 2 cells thick',
     &                'in the z direction.','W')
          ERROR=.true.
        endif
        if (ERROR) goto 79
      elseif (INO.eq.7) then
        VAL=VOLHeat(ISB,ICFD)
        call EASKR(VAL,'Heat flux (W/m^2) on blockage surfaces?',
     &    ' ',-1000.,'W',1000.,'W',10.,'heat flux',IER,nbhelp)
        if (IER.eq.0) VOLHeat(ISB,ICFD)=VAL
      elseif (INO.eq.(NITMS-1)) then

C Help.
        helptopic='cfd_blockage_menu'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('blocakage edit',nbhelp,'-',0,0,IER)
      elseif (INO.eq.NITMS) then

C Return.
        return
      endif

      goto 5
      end


C ************************** HANDSHK **************************
C HANDSHK controls the editing of the handshaking mechanism.

      SUBROUTINE HANDSHK(IHST,IER)
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
      ITMSS(1)= ' Direction   Wall fns  HTCs  Qsurf  Tref'
      if (ITURB(ICFD).eq.1) then
        ITMSS(2)= 'a One-way log-law  n/a   CFD  n/a'
        IHSID(2)=1
        ITMSS(3)= 'b One-way Yuan  n/a  CFD  n/a'
        IHSID(3)=3
        ITMSS(4)= 'c One-way Yuan  n/a  A*HC*(Tsurf-Tref)  BSim '
        IHSID(4)=4
        ITMSS(5)= 'd One-way Yuan  n/a  A*HC*(Tsurf-Tref)  CFD average'
        IHSID(5)=5
        ITMSS(6)= 'e One-way log-law  n/a  A*HC*(Tsurf-Tref)  BSim'
        IHSID(6)=6
        ITMSS(7)= 'f One-way log-law n/a  A*HC*(Tsurf-Tref) CFD average'
        IHSID(7)=7
        ITMSS(8)= 'g One-way log-law local  A*HC*(Tsurf-Tref) CFD local'
        IHSID(8)=8
        ITMSS(9)= 'h Two-way log-law   n/a CFD n/a'
        IHSID(9)=9
        ITMSS(10)='i Two-way Yuan   n/a CFD n/a'
        IHSID(10)=11
        ITMSS(11)='j Two-way Yuan   local  A*HC*(Tsurf-Tref) CFD local'
        IHSID(11)=12
        ITMSS(12)='k Two-way log-law  n/a CFD n/a'
        IHSID(12)=13
        ITMSS(12)='l Two-way log-law  local A*HC*(Tsurf-Tp) local'
        IHSID(12)=14
        NITMS=12
      else
        ITMSS(2)= 'a One-way   n/a   n/a  CFD  n/a'
        IHSID(2)=2
        ITMSS(3)= 'b Two-way   n/a   n/a  CFD  n/a'
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


C ************************** PIKCELS **************************
C PIKCELS controls the editing of the cells defining a boundary region.

      SUBROUTINE PIKCELS(BTYPE,BNAME,Ii,If,Ji,Jf,Ki,Kf,IFACE,IER)

#include "cfd.h"
      integer iCountWords

      COMMON/OUTIN/IUOUT,IUIN
      common/grdmax/NTCX,NTCY,NTCZ
      COMMON/GEOM/XP(ntcelx),YP(ntcely),ZP(ntcelz),
     1            DXEP(ntcelx),DXPW(ntcelx),DYNP(ntcely),DYPS(ntcely),
     2            DZHP(ntcelz),DZPL(ntcelz),
     3            SEW(ntcelx),SNS(ntcely),SHL(ntcelz),
     4            XU(ntcelx),YV(ntcely),ZW(ntcelz)
      COMMON/ALL/NI,NJ,NK,NIM1,NJM1,NKM1,NIM2,NJM2,NKM2

      character BTYPE*20,BNAME*12,s30*30,dflt*30,lface*7,t30*30
      character outs*124
      integer IFACE   ! for radio button

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='PIKCELS'  ! set for subroutine

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
      XZ=0.
      YZ=0.
      ZZ=0.
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
        Ii=1
        If=1
        write(dflt,'(6i4)') Ii,If,IONE,NTCY,IONE,NTCZ
        t30='(current face: west)'
      elseif (IFACE.eq.2) then

C East face.
        Ii=NTCX
        If=NTCX
        write(dflt,'(6i4)') Ii,If,IONE,NTCY,IONE,NTCZ
        t30='(current face: east)'
      elseif (IFACE.eq.3) then

C South face.
        Ji=1
        Jf=1
        write(dflt,'(6i4)') IONE,NTCX,Ji,Jf,IONE,NTCZ
        t30='(current face: south)'
      elseif (IFACE.eq.4) then

C North face.
        Ji=NTCY
        Jf=NTCY
        write(dflt,'(6i4)') IONE,NTCX,Ji,Jf,IONE,NTCZ
        t30='(current face: north)'
      elseif (IFACE.eq.5) then

C Low face.
        Ki=1
        Kf=1
        write(dflt,'(6i4)') IONE,NTCX,IONE,NTCY,Ki,Kf
        t30='(current face: low)'
      elseif (IFACE.eq.6) then

C High face.
        Ki=NTCZ
        Kf=NTCZ
        write(dflt,'(6i4)') IONE,NTCX,IONE,NTCY,Ki,Kf
        t30='(current face: high)'
      else

C Unknown face.
        write(dflt,'(6i4)') IONE,NTCX,IONE,NTCY,IONE,NTCZ
        t30='(current face: UNKNOWN)'
      endif
      if (Ii.lt.0.or.If.lt.0.or.
     &    Ji.lt.0.or.Jf.lt.0.or.
     &    Ki.lt.0.or.Kf.lt.0) then 
        s30=dflt
      else

C Ask for cells.
        write(s30,'(6i4)') Ii,If,Ji,Jf,Ki,Kf
      endif
 10   call EASKS(s30,'Boundary cells?',t30,30,dflt,'bndry cells',
     &  IER,nbhelp)
      NITMS = iCountWords(s30)
      if (NITMS.ne.6) goto 10

C Read six cell locations.
      K=0
      call EGETWI(s30,K,IVAL,1,NTCX,'F','x cells start',IER)
      if (IER.eq.0) Ii=IVAL
      call EGETWI(s30,K,IVAL,1,NTCX,'F','x cells end',IER)
      if (IER.eq.0) If=IVAL
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
        if (Ii.ne.1.or.If.ne.1) IER=2
      elseif (IFACE.eq.2) then
        if (Ii.ne.NTCX.or.If.ne.NTCX) IER=2
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

C ************************** PIKFACE **************************
C PIKFACE controls the editing of the face of the boundary region.

      SUBROUTINE PIKFACE(BTYPE,BNAME,Ii,If,Ji,Jf,Ki,Kf,IFACE,IER)
C Check which two start/ end locations are the same.  Keep count of 
C number of matches so as confusion can be removed.

      common/grdmax/NTCX,NTCY,NTCZ

      character BTYPE*20,BNAME*12

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='PIKFACE'  ! set for subroutine

 5    IER=0

C Check that face is needed.
      if (IFACE.eq.8.or.IFACE.eq.9) return

C Try to guess which face.
      IMTCH=0
      if (Ii.eq.If) then
        if (Ii.eq.1) then

C West face.
          IMTCH=IMTCH+1
          IFACE=1
        elseif (Ii.eq.NTCX) then

C East face.
          IMTCH=IMTCH+1
          IFACE=2
        endif
      endif
      if (Ji.eq.Jf) then
        if (Ji.eq.1) then

C South face.
          IMTCH=IMTCH+1
          IFACE=3
        elseif (Ji.eq.NTCY) then

C North face.
          IMTCH=IMTCH+1
          IFACE=4
        endif
      endif
      if (Ki.eq.Kf) then
        if (Ki.eq.1) then

C Base face.
          IMTCH=IMTCH+1
          IFACE=5
        elseif (Ki.eq.NTCZ) then

C Top face.
          IMTCH=IMTCH+1
          IFACE=6
        endif
      endif
      helptopic='cfd_face_selection'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Check for conflicts.
      if (IMTCH.eq.0) then
        call usrmsg ('Cells must be at a boundary face.','  ','W')
        call PIKCELS(BTYPE,BNAME,Ii,If,Ji,Jf,Ki,Kf,IFACE,IER)
      elseif (IMTCH.gt.1) then
        call usrmsg ('Cannot resolve boundary face.',
     &               'Please choose from the following list.','W')
        IFACE=1
        CALL EASKATOG('Boundary face is on which side?',' ','West',
     &    'East','South','North','Low','High',' ',IFACE,nbhelp)
      endif

C Check for incompatable face/ cell definition
      if (IFACE.eq.1) then
        if (Ii.ne.1.or.If.ne.1) IER=2
      elseif (IFACE.eq.2) then
        if (Ii.ne.NTCX.or.If.ne.NTCX) IER=2
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

C ************* TOPBOT 
C TOPBOT: Define upper and lower curvilinear x-section for use in
C gridding generation.
      SUBROUTINE TOPBOT(ICOMP,act,IER)
#include "building.h"
#include "geometry.h"
#include "prj3dv.h"
      integer iCountWords

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)
      COMMON/RAY2/ITDSP,ITBND,ITEPT,ITZNM,ITSNM,ITVNO,ITORG,ITSNR,
     &            ITOBS,ITHLS,ITHLZ,ITGRD,GRDIS,ITPPSW
      COMMON/RAY3/MODIFY,MODLEN,MODBND

C Curvilinear boundary vertex numbers.
      common/CLVTXNO/ivxhigh(MV),ivxlow(MV),ihigh,ilow

      LOGICAL MODIFY,MODLEN,MODBND
      logical clkok,found

      DIMENSION COG1(3),COG2(3),COG3(3),JJVN(MV)

      CHARACTER act*1,HOLD*72,outs*124
#ifdef OSI
      integer iix,iiy,ixd,iyd
#else
      integer*8 iix,iiy,ixd,iyd
#endif

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='TOPBOT'  ! set for subroutine

C Tell the user the rules of selection.
      ier=0
      helptopic='cfd_curvilinear_grid'
      call gethelptext(helpinsub,helptopic,nbhelp)
      CALL PHELPD('gen start popup',nbhelp,'-',0,0,IER)

C Redraw the zone with vertices.
      MODIFY=.TRUE.
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
        CALL EASKS(HOLD,' Give high vertices (west to east)','  ',
     &    72,' ','associated vertices',IER,nbhelp)
      else
        CALL EASKSCMD(HOLD,' Give high vertices (west to east)',
     &   ' ','via mouse', clkok,72,' 1 2 3','assoc vert+mouse',
     &   IER,nbhelp)
        if(clkok)then
          ijvn=0
          call edisp(iuout,
     &        'Select points via cursor...type `e` to finish.')
  46      CALL trackview(ii,ixx,iyy)
          if(ii.eq.69.or.ii.eq.101)goto 47
          found=.false.
          do 45 i=1,NTV
            COG1(1)=X(I)
            COG1(2)=Y(I)
            COG1(3)=Z(I)
            CALL VECTRN(COG1,TEMAT,COG2,IER)
            CALL VECTRN(COG2,ETSMAT,COG3,IER)
            call u2pixel(COG3(1),COG3(2),iix,iiy)
            ixd=iix-ixx
            iyd=iiy-iyy
            if(abs(ixd).lt.5.and.abs(iyd).lt.5)then
              if(found)then
                call edisp(iuout,'Close points...try again.')
                goto 46
              endif
              WRITE(outs,'(a,i5,a,i5,a,i3)')' The point @ x=',ixx,
     &          ' & y=',iyy,' matches vertex ',i
              call edisp(iuout,outs)
              found=.true.

C << Should Check that points are in the same plane and that X is increasing. >>
              ijvn=ijvn+1
              jjvn(ijvn)=i
              goto 46
            endif
  45      continue
          if(.NOT.found)goto 46
  47      if(ijvn.lt.2)then
            call edisp(iuout,'Not enough points.')
            goto 41
          endif

C This write assumes not-very-complex zones and surfaces.
          HOLD=' '
          WRITE(HOLD,'(24I3)')(JJVN(J),J=1,ijvn)
          CALL EASKS(HOLD,' Associated high vertices (confirm):',
     &      '  ',72,' ','associated vertices',IIER,nbhelp)
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
        CALL EASKSCMD(HOLD,' Give low vertices (west to east)',
     &   ' ','via mouse', clkok,72,' 1 2 3','assoc vert+mouse',
     &   IER,nbhelp)
        if(clkok)then
          ijvn=0
          call edisp(iuout,
     &        'Select points via cursor...type `e` to finish.')
  56      CALL trackview(ii,ixx,iyy)
          if(ii.eq.69.or.ii.eq.101)goto 57
          found=.false.
          do 55 i=1,NTV
            COG1(1)=X(I)
            COG1(2)=Y(I)
            COG1(3)=Z(I)
            CALL VECTRN(COG1,TEMAT,COG2,IER)
            CALL VECTRN(COG2,ETSMAT,COG3,IER)
            call u2pixel(COG3(1),COG3(2),iix,iiy)
            ixd=iix-ixx
            iyd=iiy-iyy
            if(abs(ixd).lt.5.and.abs(iyd).lt.5)then
              if(found)then
                call edisp(iuout,'Close points...try again.')
                goto 56
              endif
              WRITE(outs,'(a,i5,a,i5,a,i3)')' The point @ x=',ixx,
     &          ' & y=',iyy,' matches vertex ',i
              call edisp(iuout,outs)
              found=.true.
              ijvn=ijvn+1
              jjvn(ijvn)=i
              goto 56
            endif
  55      continue
          if(.NOT.found)goto 56
  57      if(ijvn.lt.2)then
            call edisp(iuout,'Not enough points.')
            goto 51
          endif

C This write statement assumes not-very-complex zones and polygons. 
          HOLD=' '
          WRITE(HOLD,'(24I3)')(JJVN(J),J=1,ijvn)
          CALL EASKS(HOLD,' Associated low vertices (confirm):',
     &      '  ',72,' ','associated vertices',IIER,nbhelp)
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

C ******************* GRIDDEF *******************
C GRIDDEF - define gridding via regions and specify number of cells
C and cell distribution for each region.

      SUBROUTINE GRIDDEF(IAX,NRG,NTC,TOTDIS,NCEL,REG,plaw,ierr)
#include "building.h"
#include "geometry.h"
#include "cfd.h"
#include "epara.h"

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      common/OUTIN/IUOUT,IUIN
      COMMON/MFTRA/IMFTU
      COMMON/SHOUT/ICOUT
      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/ICFNOD/ICFD,ICP

      dimension NCEL(MNREG,MNZ),plaw(MNREG,MNZ)
      dimension NRG(4,MNZ),REG(MNREG,MNZ)

      logical OK,DOK,GRMODE

      character ITEM(MNREG+5)*44,KEY*1,HOLD*24,outs*124,TITLE*30

      integer icellt   ! for local editing.
      integer MVERT,INO ! max items and current menu item

#ifdef OSI
      integer iigl,iigr,iigt,iigb,iigw,iigwh
      integer iiw1,iiw2,iiw3,iiw4,iimenu
#else
      integer*8 iigl,iigr,iigt,iigb,iigw,iigwh
      integer*8 iiw1,iiw2,iiw3,iiw4,iimenu
#endif

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='GRIDDEF'  ! set for subroutine

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
        xoff=0.
        yoff=0.
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
          dok=.true.
          CALL ASKOK(' ',' Symmetrical gridding?',OK,dok,nbhelp)
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
                dok=.true.
                CALL ASKOK('  ','Symmetrical gridding?',OK,dok,nbhelp)
                if(OK) NCEL(IFOC,ICFD) = -1*NCEL(IFOC,ICFD)
              else
                plaw(IFOC,ICFD)=1.0
              endif
            endif
          endif
        elseif (IRO.eq.2) then
          IDEL=NRG(IAX,ICFD)
          CALL EASKI(IDEL,' ','Which region to delete ? ',
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

C ************************ ASKMFNOD ************************
C This subroutine is copied here from esrures/mfoutp.F
C ASKMFNOD: Asks for one or more mass flow nodes.
C IOPT - restrict node types on display:
C  IOPT=0 - display all nodes
C  IOPT=1 - display internal nodes only.
C PROMPT1 and PROMPT2 are text prompt strings
C NH is number of help strings.

      SUBROUTINE ASKMFNOD(IOPT,NNPK,INPK,PROMPT1,PROMPT2,NH)
#include "net_flow.h"

      COMMON/MFLOW1/NNOD,NCMP,NCNN
      COMMON/MFLOW2/NDNAM(0:MNOD)

      DIMENSION INPK(MNOD)

      CHARACTER VERT(MNOD)*12, NDNAM*12
      CHARACTER PROMPT1*72, PROMPT2*72

C Set up array of menu strings.
      do 10 I=1,NNOD
        VERT(I)=NDNAM(I)
 10   continue
      call EPICKS(NNPK,INPK,PROMPT1,PROMPT2,
     &  12,NNOD,VERT,'Node select',IER,NH)

      return
      end

C ************************ ASKMFCON ************************
C This subroutine is copied here from esrures/mfoutp.F
C ASKMFCON: Asks for one or more mass flow connections.  If INODE 
C is greater than zero then user specified list of connections
C associated with this node. If INODE = zero then allow display
C of all connections in the network. If INODE is less than zero
C then silently return all connections associated with the node
C (absolute value of INODE).

      SUBROUTINE ASKMFCON(INODE,NNPK,ICPK,PROMPT1,PROMPT2)
#include "net_flow.h"

      COMMON/MFLOW1/NNOD,NCMP,NCNN
      COMMON/MFLOW2/NDNAM(0:MNOD)
      COMMON/MFLOW8/CMNAM(MCMP),LTPCMP(MCMP)
      COMMON/MFLW10/NODPS(MCNN),HGTPS(MCNN),NODNE(MCNN),HGTNE(MCNN),
     &              ITPCON(MCNN),NDSCNN(MCNN,MCNS)

      DIMENSION IPK(MCNN),ICPK(MCNN),ICID(MCNN)

      CHARACTER VRT(MCNN)*43
      CHARACTER LTPCMP*60,CMNAM*12,NDNAM*12
      CHARACTER PROMPT1*72, PROMPT2*72

C Set up array of menu strings.
      NITMS=0
      do 10 I=1,NCNN
        IP=NODPS(I)
        IN=NODNE(I)
        IC=ITPCON(I)
        if (abs(INODE).gt.0) then
          if (abs(INODE).eq.IP.or.abs(INODE).eq.IN) then

C A node has been specified -> only list its connections.
            NITMS=NITMS+1
            ICID(NITMS)=I
            write(VRT(NITMS),'(5a)')
     &        NDNAM(IP),'->',NDNAM(IN),' via ',CMNAM(IC)
          endif
        else

C List all connections.
          NITMS=NITMS+1
          ICID(NITMS)=I
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

C Debug.
C      do 30 I=1,NNPK
C        IP=NODPS(ICPK(I))
C        IN=NODNE(ICPK(I))
C        IC=ITPCON(ICPK(I))
C        write(6,'(5a)') NDNAM(IP),'->',NDNAM(IN),' via ',CMNAM(IC)
C 30   continue

      return
      end
