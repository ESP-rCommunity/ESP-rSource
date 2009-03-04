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

C The file edzone.f is a collection of support facilities for 
C creating zones and adding/ subtracting vertices and surfaces:
C  SCRZONE: creates initial form of an enclosure as prism or extrusion.
C  NEWZONE: controls creation of a new zone via prj or cad or file.
C  DELZONE: systematically removes a thermal zone.
C  WARNMOD: provide warnings when model evolves.
C  SUMRCHG: provides a summary of updates needed after model changes.
C comissionish: invokes the shading module in a number of modes
C  POINTTOLINE: determines distance from a 3D point to a 3D line.
C  silentmodel creates an/or updates a model from passed parameters.
C  silentzone: creates a zone (or limited complexity) based on parameters
C              passed to it.
C  SILENTREAD: Reads silent (compact) zones geometry data.


C ************* SCRZONE 
C SCRZONE provides initial form of an enclosure as either a prism or
C an extrusion. IER=0 OK. 
      SUBROUTINE SCRZONE(ICOMP,IER)
#include "building.h"

C Use geometry for common g0/g2/g4/prec16/precz/c20.
#include "geometry.h"
#include "esprdbfile.h"
      
      integer lnblnk  ! function definition

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV

      COMMON/G5/SNAME(MCOM,MS),SOTF(MS),SMLCN(MS),SVFC(MS),SOTHER(MS,3),
     &          SUSE(MS,2),SPARENT(MS)
      COMMON/G6/SSNAME(MCON),SSOTF(MCON),SSMLCN(MCON),SSVFC(MCON),
     &          SSOTHER(MCON,3),SSPARENT(MCON),SSUSE(MCON,2)
      COMMON/G7/SSNA(MCON),SSPAZI(MCON),SSPELV(MCON),SSPERIM(MCON),
     &          SSUREQN(MCON,4),SSURCOG(MCON,3),SSURVN(MCON,3)
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      integer nccode,indutl
      character LSNAM*72,LPROJ*72,LGEOM*72,LSHAD*72,LTHRM*72,LUTIL*72
      COMMON/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      common/C21/IFCFG,cfgroot,LCFGF
      COMMON/C24/IZSTOCN(MCOM,MS)
      common/user/browse
      common/paths/zonepth,netpth,ctlpth,imgpth,radpth,docpth,tmppth,
     &             dbspth
      common/cctlnm/ctldoc,lctlf
      common/cctl/icascf(mcom)

      DIMENSION XX(MS),YY(MS)
      CHARACTER SOTHER*24,SNAME*12,SMLCN*32,SVFC*4,SOTF*32,SUSE*8
      CHARACTER SPARENT*12
      CHARACTER HOLD*32
      CHARACTER SSMLCN*32,SSVFC*4,SSOTF*32,SSOTHER*24,SSNAME*12,SSUSE*8
      character MODE*4,SSPARENT*12,fs*1
      CHARACTER OUTSTR*124,H*72,ZN*12
      character ctldoc*248,LCTLF*72,zd*64,DFILE*72,CFILE*72,OFILE*72
      character zonepth*24,netpth*24,ctlpth*24,imgpth*24,radpth*24
      character docpth*24,tmppth*24,dbspth*24,cfgroot*24,LCFGF*72
      LOGICAL OK,dok,browse,OKC,XST,unixok
      logical newgeo  ! to use for testing if new/old geometry file.

      integer iglib   ! if 1 then X11, if 2 then GTK, if 3 then text only.

C << Consider how a more robust cancel facility can
C << be established. Possibly one that deletes the new zone
C << from the model or that finds ways to uninstantiate the
C << common block data that was set prior to the cancel instruction.

C If browsing then user cannot create a new zone.
      if(browse)then
        call usrmsg('Cannot update model while in browse',
     &    'mode, you must `own` the model!','W')
        return
      endif
      newgeo=.false.  ! assume older format geometry.

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

C Clear local coordinate array.
      DO 345 IW=1,MS
        XX(iw)=0.0
        YY(iw)=0.0
 345  continue

C Creation of a new zone, first ask its name, trapping out
C illegal characters.
      call tstamp('>','PRJ: new zone')
      ZN=' '
 42   H(1)='The zone description is used for descriptive and'
      H(2)='book-keeping purposes. It should be unique, be less'
      H(3)='than 12 characters and contain no spaces.'
      CALL EASKS(ZN,'New zone description?','(<12 chars, no spaces)',
     &           12,'New zone','zone name',IER,3)
      IF(ZN.eq.' '.or.ier.ne.0)goto 42
      call st2name(ZN,zname(ICOMP))
      lnzname(icomp)=lnblnk(zname(ICOMP))  ! update this string length.

C << Unix vs NT needs to be updated >>
      if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
        WRITE(DFILE,'(2a)') zname(ICOMP)(1:lnzname(ICOMP)),'.geo'
        WRITE(CFILE,'(2a)') zname(ICOMP)(1:lnzname(ICOMP)),'.con'
        WRITE(OFILE,'(2a)') zname(ICOMP)(1:lnzname(ICOMP)),'.opr'
      else
        WRITE(DFILE,'(4a)') zonepth(1:lnblnk(zonepth)),fs,
     &     zname(ICOMP)(1:lnzname(ICOMP)),'.geo'
        WRITE(CFILE,'(4a)') zonepth(1:lnblnk(zonepth)),fs,
     &     zname(ICOMP)(1:lnzname(ICOMP)),'.con'
        WRITE(OFILE,'(4a)') zonepth(1:lnblnk(zonepth)),fs,
     &     zname(ICOMP)(1:lnzname(ICOMP)),'.opr'
      endif
      LGEOM(ICOMP)=DFILE
      LTHRM(ICOMP)=CFILE
      LPROJ(ICOMP)=OFILE

      H(1)='The zone description allows further information about'
      H(2)='zone form and composition to be recorded.'
      write(zd,'(2a)') zname(ICOMP)(1:lnzname(ICOMP)),' describes a'
 43   CALL EASKS(zd,'What does it represent?','(<64 characters)',
     &            64,'no description entered','zone descr',IER,2)
      IF(zd.eq.' '.or.ier.ne.0)goto 43
      zdesc(ICOMP)=zd
      lnzdesc(ICOMP)=lnblnk(zd)  ! update the length of this string.

C Allow user choices of beginning as a simple shape (with image feedback). 
      IER=0
      H(1)='You have the option to describe zone geometry as an'
      H(2)='extruded rectangle, a floor plan extrusion or a general'
      H(3)='polyhedra. Keeping the geometry as simple as possible'
      H(4)='will simplify the constructional attribution and result'
      H(5)='interpretation tasks.'
      H(6)=' '
      H(7)='Points from a bitmap:                  '
      H(8)=' Zone geometry can be defined by taking points from a'
      H(9)=' scanned image of a site plan or floor plan.         '
      H(10)='                  '
      H(11)=' To do this you need to scan the relevant source and'
      H(12)=' then convert it to an XBM (X11 monochrome bitmap) file,'
      H(13)=' which should be placed in the project `cfg` folder.'
      H(14)=' '
      h(15)=' Please ensure that the bitmap is large enough to'
      h(16)=' allow for accurate positioning of the mouse. The bitmap'
      h(17)=' can be larger than the graphic display area, and where'
      H(18)=' it is you can pan left/right and up/down.'
      H(19)='                  '
      H(20)=' The source image should have indications on it of an'
      H(21)=' origin (typically X=0.0 Y=0.0), the North direction'
      H(22)=' and a line of known length (to get scaling from).'
      CALL EASKABCD(' ','Base zone geometry on:',
     &    'rectangular plan','polygon plan','general 3D',
     &    'bitmap',ishape,22)
 144  IF(ishape.eq.1)THEN
        XO=0.
        YO=0.
        ZO=0.
        DX=5.0
        DY=3.0
        DZ=2.7
        H(1)='The specification of a rectangular extrusion requires'
        H(2)='the specification of an origin (relative to the site'
        H(3)='coordinate system, and a length, depthand height.'
        H(4)='Assuming there is no rotation, the length dimension'
        H(5)='is along the X axis, the depth dimension is along the'
        H(6)='Y axis and the height dimension is along the Z axis.'
        H(7)='The rectangle may then be rotated by an angle measured'
        H(8)='from the X axis, anticlockwise positive.'
        HOLD=' '
        WRITE(HOLD,'(1x,3f8.3)')XO,YO,ZO
        CALL EASKS(HOLD,' ','Origin X,Y,Z?',
     &           32,' 0. 0. 0. ','origin coord',IER,8)
        K=0
        CALL EGETWR(HOLD,K,XO,-999.9,999.9,'W','X origin',IER)
        CALL EGETWR(HOLD,K,YO,-999.9,999.9,'W','Y origin',IER)
        CALL EGETWR(HOLD,K,ZO,-9.9,999.9,'W','Z origin',IER)

 242    H(1)='The length (with no rotation) is along the X axis.'
        H(2)='The width (with no rotation) is along the Y axis.'
        H(3)='The height is along the Z axis.'
        HOLD=' '
        WRITE(HOLD,'(1x,3f8.3)')DX,DY,DZ
        CALL EASKS(HOLD,' ','Rectangle length, width and height?',
     &             32,' 2.5 3.5 2.7 ','rectangle size',IER,3)
        K=0
        CALL EGETWR(HOLD,K,DX,0.001,99.9,'W','box width',IER)
        CALL EGETWR(HOLD,K,DY,0.001,99.9,'W','box depth',IER)
        CALL EGETWR(HOLD,K,DZ,0.001,99.9,'W','box height',IER)
        if(DX.lt.0.001.or.DY.lt.0.001)goto 242

        H(1)='Orientation is the angle between the length (DX) side'
        H(2)='of the rectangle and the X-axis, anticlockwise +ve.'
        ANG=0.
        CALL EASKR(ANG,' ','Rectangle orientation?',
     &       -359.0,'W',359.0,'W',0.0,'box orientation',IER,2)

C Convert box into a gen description.
        CALL ERECC(XO,YO,ZO,DX,DY,DZ,ANG)
        NSUR=6
        IUZBASEA(icomp)=0
        IBASES(ICOMP,1)=6
      ELSEIF(ishape.eq.2)THEN

C User begins with an extruded shape.
        Z1=0.
        Z2=2.4
        NW=4
        H(1)='The base surface (probably floor) height relative to'
        H(1)='the site coordinate system.'
        CALL EASKR(Z1,' ','Z value of the base surface?',
     &       0.000,'W',99.9,'W',0.0,'elevation of base',IER,2)
        H(1)='The top surface (probably ceiling) height relative to'
        H(2)='the site coordinate system. Note that this is not'
        H(3)='necessarily the same as the base-top distance!'
        CALL EASKR(Z2,' ','Z value of the top surface?',
     &       0.000,'W',99.9,'W',2.7,'elevation of top',IER,3)
        H(1)='The number of walls excluding the base and'
        H(2)='the top.'
        CALL EASKI(NW,' ','Number of walls?',
     &           3,'F',MS-2,'F',4,'no of walls',IERI,2)
        if(ieri.eq.-3) goto 42
C        call edisp(iuout,'Input the coordinates around the base ')
C        call edisp(iuout,'polygon ANTI-clockwise looking from the')
C        call edisp(iuout,'top, preferably beginning from the lower ')
C        call edisp(iuout,'left corner of the enclosure. ')

        DO 45 IW=1,NW
          WRITE(OUTSTR,'(A,I2,A)')' For base vertex',IW,'  X & Y:'
          HOLD=' '
          WRITE(HOLD,'(1x,2f8.3)')XX(IW),YY(IW)
          H(1)='The coordinates should be entered in anti-clockwise'
          H(2)='order when viewed from the top.'
          CALL EASKS(HOLD,OUTSTR,'  ',32,' 0.  0.  ','floor xy',IER,2)
          K=0
          CALL EGETWR(HOLD,K,XX(IW),-999.,999.,'W','flr X',IER)
          CALL EGETWR(HOLD,K,YY(IW),-999.,999.,'W','flr Y',IER)
   45   CONTINUE
        dok=.true.
        h(1)='If you have any doubt as to whether you entered the'
        h(2)='correct vertex coordinates you can save time by'
        h(3)='respecifying them now. Usually this takes less time'
        h(4)='than trying to correct problems later.'
        CALL ASKOK(' ','Accept base vertices?',OK,dok,4)
        if(.NOT.ok)then
          ishape=2
          goto 144
        endif

        H(1)='Rotation is the angle between the site y-axis and'
        H(2)='north, anticlockwise positive.'
        AR=0.
        CALL EASKR(AR,' ','Rotation angle?',
     &       -359.0,'W',359.0,'W',0.0,'extrusion rotation',IER,2)

C Convert into a gen body and rotate if required.
        CALL EREGC(NW,Z1,Z2,XX,YY)
        IF(AR.LT.-.01.OR.AR.GT..01)then
          x1=XX(1)
          y1=YY(1)
          CALL ESCROT(AR,x1,y1)
        endif
        NSUR=NW+2
        IUZBASEA(icomp)=0
        IBASES(ICOMP,1)=NSUR
      ENDIF

C Begin with default assumptions for each surface then overwrite
C this if user supplied information exists.
      IF(ishape.ne.3.and.ishape.ne.4)THEN

C << does this fill all of the G5 variables? >>
        CALL FILSUR(iuout,ICOMP,0)

C Update the connection list and hash.
        ICCC=NCON
        DO 32 ICC=1,NSUR
          ICCC=ICCC+1
          IC1(ICCC)=ICOMP
          IE1(ICCC)=ICC
          ICT(ICCC)=-1   ! mark as unknown at this time.
          IC2(ICCC)=0
          IE2(ICCC)=0
          SSNAME(iccc)=SNAME(icomp,icc)
          SSOTF(iccc)=SOTF(icc)
          SSMLCN(iccc)=SMLCN(icc)
          SSVFC(iccc)=SVFC(icc)
          SSOTHER(iccc,1)=SOTHER(icc,1)
          SSOTHER(iccc,2)=SOTHER(icc,2)
          SSOTHER(iccc,3)=SOTHER(icc,3)
          SSUSE(iccc,1)=SUSE(icc,1)
          SSUSE(iccc,2)=SUSE(icc,2)
          SSPARENT(iccc)=SPARENT(icc)
          IZSTOCN(icomp,icc)=iccc
   32   CONTINUE
        NCON=ICCC
      endif
      if(ishape.eq.3)then

C User will begin with a minimal GEN body.
        H(1)='A general polyhedra is appropriate for complex'
        H(2)='shapes. A set of vertices is defined and then'
        H(3)='linked together to form surfaces that bound the'
        H(4)='zone. Vertices are linked anti-clockwise when'
        H(5)='viewed from outside the zone. It is convention'
        H(6)='to start at the lower left vertex.'
        H(7)=' '
        H(8)='After you supply the zone origin, a 2m x 2m'
        H(9)='initial (floor) surface will be placed there.'
        H(10)='These vertices may then be modified and new'
        H(11)='ones added/linked to create the enclosed zone.'
        H(12)='Initial warning messages may be ignored. These'
        H(13)='will cease after you have defined a bounded zone.'
        H(14)='The initial surface may be deleted if no longer'
        H(15)='needed.'
        CALL PHELPD('gen start popup',15,'-',0,0,IER)
        NTV=4
        NSUR=1
        AR=0.0
        H(1)='The zone origin is located at the first surface.'
        HOLD=' '
        WRITE(HOLD,'(1x,3f8.3)')X(1),Y(1),Z(1)
        CALL EASKS(HOLD,' ','Origin X Y Z?',
     &      32,' 0. 0. 0. ','origin coord',IER,1)
        K=0
        CALL EGETWR(HOLD,K,X0,0.,0.,'-','X origin',IER)
        CALL EGETWR(HOLD,K,Y0,0.,0.,'-','Y origin',IER)
        CALL EGETWR(HOLD,K,Z0,0.,0.,'-','Z origin',IER)
        X(1)=X0+2.0
        Y(1)=Y0
        Z(1)=Z0
        X(2)=X0
        Y(2)=Y0
        Z(2)=Z0
        X(3)=X0
        Y(3)=Y0+2.0
        Z(3)=Z0
        X(4)=X0+2.0
        Y(4)=Y0+2.0
        Z(4)=Z0
        NVER(1)=4
        JVN(1,1)=1
        JVN(1,2)=2
        JVN(1,3)=3
        JVN(1,4)=4
        NZTV(icomp)=NTV
        SNAME(ICOMP,1)='first'
        SOTF(1)='OPAQ'
        SMLCN(1)='UNKN'
        SVFC(1)='FLOR'
        SOTHER(1,1)='UNKNOWN'
        SOTHER(1,2)='0'
        SOTHER(1,3)='0'
        SUSE(1,1)='-'
        SUSE(1,2)='-'
        SPARENT(1)='-'
        IUZBASEA(icomp)=0
        IBASES(ICOMP,1)=1

C Update the connection list and hash for initial surface.
        ICCC=NCON
        ICCC=ICCC+1
        IC1(ICCC)=ICOMP
        IE1(ICCC)=1
        ICT(ICCC)=0
        IC2(ICCC)=0
        IE2(ICCC)=0
        SSNAME(iccc)=SNAME(ICOMP,1)
        SSOTF(iccc)=SOTF(1)
        SSMLCN(iccc)=SMLCN(1)
        SSVFC(iccc)=SVFC(1)
        SSOTHER(iccc,1)=SOTHER(1,1)
        SSOTHER(iccc,2)=SOTHER(1,2)
        SSOTHER(iccc,3)=SOTHER(1,3)
        SSUSE(iccc,1)=SUSE(1,1)
        SSUSE(iccc,2)=SUSE(1,2)
        SSPARENT(iccc)=SPARENT(1)
        IZSTOCN(icomp,icc)=iccc
        NCON=ICCC
      elseif(ishape.eq.4)then

C Point and click on a bitmap. After return, re-scan the system
C configuration file to ensure that all the derived information
C in the new geometry files is established.
        if(MMOD.eq.8)then
          iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
          if(iglib.eq.1)then
            call clickonbitmap(icomp,ier) 
            if(ier.ne.0)then
              itrc=0
              MODE='ALL '
              IAPROB=IPRODB
              CALL ERSYS(LCFGF,IFCFG,IAPROB,MODE,itrc,IER)
            endif
            return
          elseif(iglib.eq.2)then
            call usrmsg('The clickonbitmap facility is not yet working',
     &      'with new graphic library.','W')
          endif
        else
          call usrmsg('Must be in graphic mode to use click on bitmap',
     &      'facility to define vertices or floor plan corners. ','W')
          return
        endif
      endif

C High level info for all cases except clickonbitmap.
      IF(zname(ICOMP)(1:2).EQ.'  ')THEN
        IF(ICOMP.LE.9)WRITE(zname(ICOMP),'(A5,I1)')'Zone-',ICOMP
        IF(ICOMP.GT.9)WRITE(zname(ICOMP),'(A5,I2)')'Zone-',ICOMP
        lnzname(ICOMP)=lnblnk(zname(ICOMP))  ! update length for this string.
      ENDIF
      if(zdesc(ICOMP)(1:2).EQ.'  ')then
        write(zdesc(ICOMP),'(2a)') zname(ICOMP)(1:lnzname(ICOMP)),
     &    ' describes a '
        lnzdesc(ICOMP)=lnblnk(zdesc(ICOMP))  ! update the length of this string.
      endif
      CTYPE(icomp)='GEN '
      NDP(ICOMP)=3
      IDPN(ICOMP,1)=0
      IDPN(ICOMP,2)=0
      IDPN(ICOMP,3)=0
      NZSUR(ICOMP)=NSUR  ! update nzsur() it is needed by zgupdate.
      NZTV(ICOMP)=NTV

C Update the G7 common blocks and then assign ZBASEA.
      call zgupdate(2,ICOMP,ier)
      if(ishape.eq.1)then
        ibcs=IZSTOCN(icomp,IBASES(ICOMP,1))
        ZBASEA(icomp)= SSNA(ibcs)
      elseif(ishape.eq.2)then
        ibcs=IZSTOCN(icomp,IBASES(ICOMP,1))
        ZBASEA(icomp)= SSNA(ibcs)
      elseif(ishape.eq.3)then
        ibcs=IZSTOCN(icomp,IBASES(ICOMP,1))
        ZBASEA(icomp)= SSNA(ibcs)
      endif
 
C Save this to file before passing into the geometry editing facility.
      call eclose(gversion(icomp),1.1,0.01,newgeo)
      if(igupgrade.eq.2.and.(.NOT.newgeo))then
        gversion(icomp) =1.1
        newgeo = .true.
      endif
      if(newgeo)then
        call geowrite(IFIL+2,LGEOM(ICOMP),ICOMP,iuout,3,IER)
      else
        call emkgeo(IFIL+2,LGEOM(ICOMP),ICOMP,iuout,3,IER)
      endif
      IF(IER.EQ.1)THEN
        dok=.true.
        h(1)='When attempting to write the geometry a problem was'
        h(2)='encountered. Check that you have permission to write'
        h(3)='this type of file and that you have sufficient disk'
        h(4)='space.'
        CALL ASKOK('Problem creating geometry file.','Try again?',
     &               OK,dok,4)
        IF(OK)GOTO 42
      ENDIF

C Read in control file if defined.
      OKC=.false.
      if(LCTLF(1:1).ne.' '.and.LCTLF(1:4).ne.'UNKN')OKC=.true.
      if(OKC)then
        ICTLF=IFIL+1
        CALL ERPFREE(ICTLF,ISTAT)
        call FINDFIL(LCTLF,XST)
        IF(XST) CALL EZCTLR(ICTLF,ITRC,IUOUT,IER)
      endif

      NCOMP=NCOMP+1
      NCCODE(ICOMP)=NCOMP
      if(OKC)then
        icascf(NCOMP)=0
        call usrmsg(' updating control for additional zone...',' ','-')
        call CTLWRT(ICTLF,IER)
        call usrmsg(' ',' ','-')
      endif
      CALL EMKCFG('-',IER)

      RETURN
      END

C ************* NEWZONE 
C NEWZONE controls the creation of a new zone. IER=0 OK. 
      SUBROUTINE NEWZONE(ITRC,IC,IER)
#include "building.h"
#include "geometry.h"
      
      integer lnblnk  ! function definition

      COMMON/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)

      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      COMMON/G5/SNAME(MCOM,MS),SOTF(MS),SMLCN(MS),SVFC(MS),SOTHER(MS,3),
     &          SUSE(MS,2),SPARENT(MS)
      COMMON/G6/SSNAME(MCON),SSOTF(MCON),SSMLCN(MCON),SSVFC(MCON),
     &          SSOTHER(MCON,3),SSPARENT(MCON),SSUSE(MCON,2)
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      integer nccode,indutl
      character LSNAM*72,LPROJ*72,LGEOM*72,LSHAD*72,LTHRM*72,LUTIL*72
      COMMON/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      COMMON/C24/IZSTOCN(MCOM,MS)
      COMMON/RAY7/ZXMN(MCOM),ZYMN(MCOM),ZZMN(MCOM),ZXMX(MCOM),
     &            ZYMX(MCOM),ZZMX(MCOM),ZBFLG(MCOM)
      common/cctlnm/ctldoc,lctlf
      common/cctl/icascf(mcom)
      common/paths/zonepth,netpth,ctlpth,imgpth,radpth,docpth,tmppth,
     &             dbspth

      CHARACTER*72 LTMP,DFILE
      CHARACTER SSMLCN*32,SSVFC*4,SSOTF*32,SSOTHER*24,SSNAME*12,SSUSE*8
      character SSPARENT*12
      CHARACTER SOTHER*24,SNAME*12,SMLCN*32,SVFC*4,SOTF*32,SUSE*8
      CHARACTER SPARENT*12
      CHARACTER OUTSTR*124,H*72,ctldoc*248,LCTLF*72
      character zonepth*24,netpth*24,ctlpth*24,imgpth*24,radpth*24
      character docpth*24,tmppth*24,dbspth*24
      character sfile*72,snpfile*72,fs*1,outs*124

      LOGICAL OK,DOK,XST,OKC,unixok
      logical newgeo  ! to use for testing if new/old geometry file.
      logical altered ! to signal change in boundary conditions of added existing zone.

C Set folder separator (fs) to \ or / as required.
      newgeo=.false.  ! assume older format geometry.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

 293  H(1)='You may read in an existing ESP-r zone geometry file,'
      H(2)='convert a cflow3/IES zip file into one or more zones'
      H(3)='(i.e whole model conversion)or create a zone by inputing'
      H(4)='data related to primitive shapes.'
      CALL EASKABCD(' ','New zone options:','input dimensions',
     &  'load existing (ESP-r)','load existing (CAD)','cancel',
     &  INZOPT,48)

C Input dimensions.
      if(INZOPT.eq.1)then

C Provide scratch description and the
C default assumptions for other zone files.
        CALL SCRZONE(IC,IER)
        CALL EDZONE(ITRC,IC,IER)
        ZBFLG(IC)=0
        return

C Load existing ESP-r.
      elseif(INZOPT.eq.2)then

C Find out if there are geometry files in the ../zones folder.
        sfile=' '
        snpfile=' '
        call edisp(iuout,' ')
        call browsefilelist('?','zon','geo',sfile,snpfile,nfile,iier)
        if(nfile.gt.0)then
          sfile=' '
          snpfile=' '
          call browsefilelist('b','zon','geo',sfile,snpfile,nfile,iier)

          if(snpfile(1:2).ne.'  ')then
            write(ltmp,'(3a)')zonepth(1:lnblnk(zonepth)),fs,
     &        snpfile(1:lnblnk(snpfile))

C Debug.
C            write(6,*) ltmp
          else

C User did not select a file. Take them back to the question.
            call usrmsg('You did not select any existing file.',
     &         'Please select or cancel.','W')
            goto 293 ! ask for choice again
          endif
        else
          call usrmsg('You did not select any existing file.',
     &       'Please select or cancel.','W')
          goto 293 ! ask for choice again
        endif

C Existing ESP-r geom file.
        h(1)='If there was not existing geometry file found in the'
        h(2)='../zones folder then you must supply a file name. '
        h(3)='It should be relative to the cfg folder. '
        DFILE=' '
        WRITE(OUTSTR,'(a,i2,a)')' For zone ',IC,
     &   ' the geometry is defined in:'
        CALL EASKS(LTMP,OUTSTR,' ',72,DFILE,'geom file',IER,3)
        IF(LTMP(1:2).EQ.'  ')GOTO 293
        call FINDFIL(LTMP,XST)
        IF(.NOT.XST)THEN
          dok=.true.
          CALL ASKOK(' ','Geometry file not found, try again?',OK,
     &      dok,3)
          if(OK)then
            GOTO 293 ! as for choice again
          else
            ier = -3 ! signal user cancel
            return   ! stop this before the model gets messed up.
          endif
        ENDIF
        LGEOM(IC)=LTMP

C Scan geometry file and update control and connections.
C Try reading in the geometry to fill in the data below.
        call eclose(gversion(IC),1.1,0.01,newgeo)
        if(newgeo)then
          call georead(IFIL+1,LGEOM(IC),IC,1,ITRC,iuout,IER)
        else
          call egomin(IFIL+1,LGEOM(IC),IC,1,ITRC,iuout,IER)
        endif
        IF(IER.NE.0)THEN
          dok=.true.
          h(1)='While scanning the geometry file a problem was'
          h(2)='encountered. Check that the file is of the correct'
          h(3)='type and that it has not been corrupted. '
          CALL ASKOK(OUTSTR,
     &      'Problem scanning geometry file...try again?',OK,dok,3)
          if(OK)then
            GOTO 293
          else
            ier = -3 ! signal user cancel
            return   ! stop this before the model gets messed up.
          endif
        ENDIF

C Read in control file if defined.
        OKC=.false.
        if(LCTLF(1:1).ne.' '.and.LCTLF(1:4).ne.'UNKN')OKC=.true.
        if(OKC)then
          ICTLF=IFIL+1
          CALL ERPFREE(ICTLF,ISTAT)
          call FINDFIL(LCTLF,XST)
          IF(XST) CALL EZCTLR(ICTLF,ITRC,IUOUT,IER)
        endif

C Update the connection list and then the model configuration.
        NCOMP=NCOMP+1
        NZSUR(NCOMP)=NSUR
        NZTV(NCOMP)=NTV
        NCCODE(NCOMP)=NCOMP
        INDUTL(NCOMP)=0
        LUTIL(NCOMP)=' '
        if(OKC)then
          icascf(NCOMP)=0
          call CTLWRT(ICTLF,IER)
        endif
        ICCC=NCON

C If surface attributes are adaibetic or similar then use these in
C the connections list, then remind user to double check topology.
        altered = .false.
        DO 132 ICC=1,NSUR
          ICCC=ICCC+1
          IC1(ICCC)=IC
          IE1(ICCC)=ICC
          if(SOTHER(icc,1)(1:8).eq.'EXTERIOR')then
            ICT(ICCC)=0
            IC2(ICCC)=0
            IE2(ICCC)=0
          elseif(SOTHER(icc,1)(1:7).eq.'SIMILAR')then
            write(outs,'(3a)') 'Note: ',SNAME(NCOMP,icc),
     &        ' is a SIMILAR connection. Use the'
            call usrmsg(outs,
     &       'surface attribute facility to confirm its details.','W')
            ICT(ICCC)=1
            IC2(ICCC)=0
            IE2(ICCC)=0
          elseif(SOTHER(icc,1)(1:9).eq.'ADIABATIC')then
            ICT(ICCC)=5
            IC2(ICCC)=0
            IE2(ICCC)=0
          elseif(SOTHER(icc,1)(1:8).eq.'CONSTANT')then
            write(outs,'(3a)') 'Note: ',SNAME(NCOMP,icc),
     &        ' is a CONSTANT connection. Use the'
            call usrmsg(outs,
     &       'surface attribute facility to confirm its details.','W')
            ICT(ICCC)=0
            IC2(ICCC)=0
            IE2(ICCC)=0
          elseif(SOTHER(icc,1)(1:6).eq.'GROUND')then
            write(outs,'(3a)') 'Note: ',SNAME(NCOMP,icc),
     &        ' is a GROUND connection. Use the'
            call usrmsg(outs,
     &       'surface attribute facility to confirm its details.','W')
            ICT(ICCC)=0
            IC2(ICCC)=0
            IE2(ICCC)=0
          elseif(SOTHER(icc,1)(1:7).eq.'UNKNOWN')then
            write(outs,'(3a)') 'Note: ',SNAME(NCOMP,icc),
     &        ' has an undefined boundary. Confirm its'
            call usrmsg(outs,
     &        'surface attributes or use topology tool.','W')
            ICT(ICCC)=-1
            IC2(ICCC)=0
            IE2(ICCC)=0
          elseif(SOTHER(icc,1)(1:7).eq.'ANOTHER')then

C Existing partitions in an imported zone are reset to UNKNOWN.
            write(outs,'(3a)') 'Note: ',SNAME(NCOMP,icc),
     &        ' is a partition. Confirm its'
            call usrmsg(outs,
     &        'surface attributes or use topology tool.','W')
            SOTHER(icc,1)='UNKNOWN'
            ICT(ICCC)=-1
            IC2(ICCC)=0
            IE2(ICCC)=0
            altered = .true.   ! will need to write out geometry
          else

C Existing partitions in an imported zone are reset to UNKNOWN.
            write(outs,'(3a)') 'Note: ',SNAME(NCOMP,icc),
     &        ' might be a partition. Confirm its'
            call usrmsg(outs,
     &        'surface attributes or use topology tool.','W')
            SOTHER(icc,1)='UNKNOWN'
            ICT(ICCC)=-1
            IC2(ICCC)=0
            IE2(ICCC)=0
            altered = .true.   ! will need to write out geometry
          endif
	  
C Update the connection based data structures.
          SSNAME(iccc)=SNAME(NCOMP,icc)
          SSOTF(iccc)=SOTF(icc)
          SSMLCN(iccc)=SMLCN(icc)
          SSVFC(iccc)=SVFC(icc)
          SSOTHER(iccc,1)=SOTHER(icc,1)
          SSOTHER(iccc,2)=SOTHER(icc,2)
          SSOTHER(iccc,3)=SOTHER(icc,3)
          SSUSE(iccc,1)=SUSE(icc,1)
          SSUSE(iccc,2)=SUSE(icc,2)
          SSPARENT(iccc)=SPARENT(icc)
          IZSTOCN(ncomp,icc)=iccc
  132   CONTINUE
        NCON=ICCC

C If any of the boundary conditions were altered then write out
C the geometry file.
        if(altered)then
          if(newgeo)then
            call geowrite(IFIL+1,LGEOM(ic),ic,iuout,3,IER)
          else
            call emkgeo(IFIL+1,LGEOM(ic),ic,iuout,3,IER)
          endif
        endif

C Update the G7 common blocks and then write out configuration file.
        call zgupdate(0,NCOMP,ier)
        CALL EMKCFG('-',IER)
        call usrmsg(
     &  'NOTE: ensure that the model topology is correct by using ',
     &  'the topology facility under [zones definition].','W')

C Check to see if there are other zone files which can be associated
C with this new zone.
        if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
          WRITE(DFILE,'(A,A4)')zname(IC)(1:lnzname(IC)),'.opr'
        else
          WRITE(DFILE,'(3A,A4)') zonepth(1:lnblnk(zonepth)),'/',
     &      zname(IC)(1:lnzname(IC)),'.opr'
        endif
        dok=.false.
        h(1)='If you are extending a model via the use of existing'
        h(2)='zone geometry files you might also have existing zone'
        h(3)='schedules to reference. You can do that here. '
        CALL ASKOK(' ','Also use an existing operation file?',OK,dok,3)
        if(OK)then
          if(LPROJ(IC)(1:4).eq.'UNKN'.or.LPROJ(IC)(1:2).eq.'  ')then

C Find out if there are operation files in the model ../zones folder.
            sfile=' '
            snpfile=' '
            call edisp(iuout,' ')
            call browsefilelist('?','zon','opr',sfile,snpfile,nfile,
     &        iier)
            if(nfile.gt.0)then
              sfile=' '
              snpfile=' '
              call browsefilelist('b','zon','opr',sfile,snpfile,nfile,
     &          iier)
              if(snpfile(1:2).ne.'  ')then
                write(ltmp,'(3a)')zonepth(1:lnblnk(zonepth)),fs,
     &            snpfile(1:lnblnk(snpfile))

C Debug.
C                write(6,*) ltmp
              else
                LTMP=DFILE
              endif
            else
              LTMP=DFILE
            endif
          else
            LTMP=LPROJ(IC)
          endif
          h(1)='If there was not existing operations file found in the'
          h(2)='../zones folder then you must supply a file name. '
          h(3)='It should be relative to the cfg folder. '
          WRITE(OUTSTR,'(a,i2,a)')' For zone ',IC,
     &      ' the operations are defined in:'
 295      CALL EASKS(LTMP,OUTSTR,' ',72,DFILE,'opr file',IER,3)
          IF(LTMP(1:2).EQ.'  ')GOTO 295
          call FINDFIL(LTMP,XST)
          IF(.NOT.XST)THEN
            dok=.true.
            CALL ASKOK(' ','Operations file not found, try again?',
     &        OK,dok,3)
            IF(OK)GOTO 295
          endif
          LPROJ(IC)=LTMP
        else
          LPROJ(IC)='UNKNOWN'
        endif

        dok=.false.
        h(1)='If you are extending a model via the use of existing'
        h(2)='zone geometry files you might also have existing zone'
        h(3)='constructions to reference. You can do that here. '
        CALL ASKOK(' ','Also use an existing constructions file? ',
     &    OK,dok,3)
        if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
          WRITE(DFILE,'(A,A4)')zname(IC)(1:lnzname(IC)),'.con'
        else
          WRITE(DFILE,'(3A,A4)') zonepth(1:lnblnk(zonepth)),'/',
     &      zname(IC)(1:lnzname(IC)),'.con'
        endif
        if(OK)then
          if(LTHRM(IC)(1:4).eq.'UNKN'.or.LTHRM(IC)(1:2).eq.'  ')then

C Find out if there are construction files in the model ../zones folder.
            sfile=' '
            snpfile=' '
            call edisp(iuout,' ')
            call browsefilelist('?','zon','con',sfile,snpfile,nfile,
     &        iier)
            if(nfile.gt.0)then
              sfile=' '
              snpfile=' '
              call browsefilelist('b','zon','con',sfile,snpfile,nfile,
     &          iier)
              if(snpfile(1:2).ne.'  ')then
                write(ltmp,'(3a)')zonepth(1:lnblnk(zonepth)),fs,
     &            snpfile(1:lnblnk(snpfile))

C Debug.
C                write(6,*) ltmp
              else
                LTMP=DFILE
              endif
            else
              LTMP=DFILE
            endif
          else
            LTMP=LTHRM(IC)
          endif
          h(1)='If there was not existing constructions file found in '
          h(2)='the ../zones folder then you must supply a file name.'
          h(3)='It should be relative to the cfg folder. '
          WRITE(OUTSTR,'(a,i2,a)')' For zone ',IC,
     &      ' the constructions are defined in:'
 297      CALL EASKS(LTMP,OUTSTR,' ',72,DFILE,'constr file',IER,3)
          IF(LTMP(1:2).EQ.'  ')GOTO 297
          call FINDFIL(LTMP,XST)
          IF(.NOT.XST)THEN
            dok=.true.
            CALL ASKOK(' ','Constructions file not found, try again?',
     &        OK,dok,3)
            IF(OK)GOTO 297
          endif
          LTHRM(IC)=LTMP
        else
          LTHRM(IC)=DFILE
        endif

C << still to do are asking for any other optional files for this zone.>>
        return

C Load existing CAD.
      elseif(INZOPT.eq.3)then

C Find out if there are CAD (zip) files in ../zones folder.
        sfile=' '
        snpfile=' '
        call edisp(iuout,' ')
        call browsefilelist('?','zon','zip',sfile,snpfile,nfile,iier)
        if(nfile.gt.0)then
          sfile=' '
          snpfile=' '
          call browsefilelist('b','zon','zip',sfile,snpfile,nfile,iier)

          if(snpfile(1:2).ne.'  ')then
            write(ltmp,'(3a)')zonepth(1:lnblnk(zonepth)),fs,
     &        snpfile(1:lnblnk(snpfile))

C Debug.
C            write(6,*) ltmp
          else
            write(ltmp,'(a)')zonepth(1:lnblnk(zonepth))
          endif
        else
          write(ltmp,'(a)')zonepth(1:lnblnk(zonepth))
        endif

C Existing zip geom file.
        h(1)='If there is no existing geometry file found in the'
        h(2)='../zones folder then you must supply a file name'
        h(3)='including a path relative to the cfg folder.'
        DFILE=' '
        WRITE(OUTSTR,'(a,i2,a)')'For zone ',IC,
     &    ' the geometry is to be scanned from:'
 396    CALL EASKS(LTMP,' ',OUTSTR,72,DFILE,'zip file',IER,3)
        IF(LTMP(1:2).EQ.'  ')GOTO 293
        call FINDFIL(LTMP,XST)
        IF(.NOT.XST)THEN
          dok=.true.
          CALL ASKOK('Zip file not found!','Try again?',OK,
     &      dok,3)
          IF(OK)GOTO 396
        ENDIF

C Scan zip file and update control and connections.
        CALL EZIPIN(IFIL+1,ltmp,IC,1,ITRC,iuout,IER)
        IF(IER.NE.0)THEN
          dok=.true.
          h(1)='While scanning the geometry file a problem was'
          h(2)='encountered. Check that the file is of the correct'
          h(3)='type and that it has not been corrupted.'
          CALL ASKOK('Problem scanning CAD file!',
     &                    'Try again?',OK,dok,3)
          IF(OK)GOTO 396
        ENDIF

      elseif(INZOPT.EQ.4)then
        ier = -3  ! signal user cancel
        return
      endif
      end

C ************* DELZONE
C DELZONE systematicly removes a thermal zone.
      SUBROUTINE DELZONE(ITRC,IWHICH,IER)
#include "building.h"
#include "geometry.h"
#include "net_flow.h"
#include "esprdbfile.h"
#include "espriou.h"
C espriou.h provides currentfile.
#include "control.h"
      
      integer lnblnk  ! function definition

      COMMON/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)

      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      COMMON/G5/SNAME(MCOM,MS),SOTF(MS),SMLCN(MS),SVFC(MS),SOTHER(MS,3),
     &          SUSE(MS,2),SPARENT(MS)
      COMMON/G6/SSNAME(MCON),SSOTF(MCON),SSMLCN(MCON),SSVFC(MCON),
     &          SSOTHER(MCON,3),SSPARENT(MCON),SSUSE(MCON,2)

      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      integer nccode,indutl
      character LSNAM*72,LPROJ*72,LGEOM*72,LSHAD*72,LTHRM*72,LUTIL*72
      COMMON/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      common/C21/IFCFG,cfgroot,LCFGF
      COMMON/C24/IZSTOCN(MCOM,MS)

      COMMON/P2N/NAC(MDTY),IACS(MDTY,MA),IACF(MDTY,MA),ACI(MDTY,MA),
     &ACV(MDTY,MA),IPT(MDTY,MA),TA(MDTY,MA)
      INTEGER NAC,IACS,IACF,IPT
      REAL ACI,ACV,TA
      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER NBDAYTYPE,NBCALDAYS,ICALENDER
      COMMON/P2CTL/ITCTL(MCOM),TLO(MCOM),TUP(MCOM),THI(MCOM),ACIL(MCOM),
     &             ACVL(MCOM),IVL(MCOM),TAL(MCOM),ACIU(MCOM),ACVU(MCOM),
     &             IVU(MCOM),TAU(MCOM),ACIH(MCOM),ACVH(MCOM),
     &             IVH(MCOM),TAH(MCOM)

      common/UDESC/LVIEW(MCOM),LHCCO(MCOM),
     &             LTWIN(MCOM),LCGCIN(MCOM),ZOBS(MCOM)

      COMMON/INDICS/IVF(MCOM),ISI(MCOM),IHC(MCOM),
     &              ITW(MCOM),ICGC(MCOM),IOBS(MCOM)

C Anchor data.
      common/anchora/NALOC,ALOC(20),ALOCLBL(20),ALOCTYP(20)
      common/anchorb/IALOC(20),lstanchr(20,60)

      common/cctlnm/ctldoc,lctlf
      common/cctl/icascf(mcom)
      common/user/browse
      COMMON/PRECTC/ITMCFL(MCOM,MS),TMCT(MCOM,MTMC,5),
     &       TMCA(MCOM,MTMC,ME,5),TMCREF(MCOM,MTMC),TVTR(MCOM,MTMC)
      COMMON/AFN/IAIRN,LAPROB,ICAAS(MCOM)
      COMMON/MFLOW2/NDNAM(0:MNOD)
      COMMON/RAY5/ZCOG(MCOM,3),XMN,YMN,ZMN,XMX,YMX,ZMX
      COMMON/RAY7/ZXMN(MCOM),ZYMN(MCOM),ZZMN(MCOM),ZXMX(MCOM),
     &            ZYMX(MCOM),ZZMX(MCOM),ZBFLG(MCOM)

      LOGICAL DOK,OK,XST,changed,OKC,OKM,modopr,browse
      logical newgeo  ! to use for testing if new/old geometry file.

      CHARACTER*72 LVIEW,LHCCO,LTWIN,LCGCIN,ZOBS
      CHARACTER LAPROB*72,LCFGF*72
      CHARACTER SOTHER*24,SNAME*12,SMLCN*32,SVFC*4,SOTF*32,SUSE*8
      CHARACTER SPARENT*12
      CHARACTER cfgroot*24
      CHARACTER SSMLCN*32,SSVFC*4,SSOTF*32,SSOTHER*24,SSNAME*12,SSUSE*8
      character SSPARENT*12
      CHARACTER H*72,ctldoc*248,LCTLF*72,outs*124
      character ltmp*72,NDNAM*12
      character ALOC*12,ALOCLBL*12,ALOCTYP*4
      character OUTSTR*124

      if(browse)then
        call usrmsg('Cannot remove a zone while in browse ',
     &              'mode, you must `own` the model! ','W')
        return
      endif
      IW=IWHICH
      IW2=IWHICH
      newgeo=.false.  ! assume older format geometry.

C General warning.
      H(1)='             - W A R N I N G - '
      H(2)='Because of the many inter-connections within a model, the '
      H(3)='removal of a zone requires a number of steps and you will'
      H(4)='be asked to confirm some actions. '
      H(5)=' '
      H(6)='Upon completion, carefully review the model to ensure that'
      H(7)='the new inter-connections are appropriate. '
      H(8)=' '
      H(9)='If in doubt ARCHIVE of the model before embarking on a'
      H(10)='major restructuring - there is no `undo` command! '
      write(outs,'(3a)')' Continue with deletion of ',zname(IW),'?'
      dok=.false.
      CALL ASKOK(outs,'(see help)',OK,dok,10)
      if(.NOT.OK)return

C Read in mass flow network if defined and see if there is a node which
C is linked to the zone being deleted.
      call tstamp('>','PRJ: deleting a zone')
      if(IAIRN.ge.1)then

C Temporarily use same file unit as profiles db.
        if(IPRODB.eq.IFIL+6)then
          IUM=IPRODB
        else
          IUM=IFIL+6
        endif
        CALL ERPFREE(IUM,ISTAT)
        CALL MFCDAT

C Read the file header and check for first-line tag. If 4 items
C then an older file so rewind the file and call emfread.
C Otherwise check and see if it is a graphic network file which
C should be scanned and converted into flow common blocks.
        CALL EFOPSEQ(IUM,LAPROB,1,IER)
        CALL STRIPC(IUM,OUTSTR,99,ND,0,'1st line of file',IER)
        if(ier.eq.0)then
          write(currentfile,'(a)') LAPROB(1:lnblnk(LAPROB))
          IF(ND.GE.4)THEN
            REWIND(IUM,ERR=999)
            CALL EMFREAD(IUM,ITRC,IER)
            CALL ERPFREE(IUM,ISTAT)
          elseif((ND.eq.1.or.ND.eq.2).and.
     &            OUTSTR(1:18).EQ.'* Graphical_network')then

C Found a graphic network file, scan it and then convert it
C into network flow common blocks.
            CALL ERPFREE(IUM,ISTAT)
            call NETREAD(IUM,'R',IER)
            CALL NETTOFLW(ier)
          endif
C          CALL EMFREAD(IUM,ITRC,IER)
C          CALL ERPFREE(IUM,ISTAT)
          if(ier.ne.0)then
            CALL USRMSG(' ','Problem in flow network!','F')
            OKM=.false.
          else

C Reset strings which hold mass flow links so that subsequent
C write of configuration file will generate proper text.
C If a node is linked to zone to be deleted then check with user.
            if(ICAAS(IW).gt.0)then
              write(outs,'(3A)')zname(IW)(1:lnzname(IW)),
     &          ' is linked to ventilation network node: ',
     &          NDNAM(ICAAS(IW))
              DOK=.true.
              h(1)='A linked network ventilation node will take the'
              h(2)='current zone temperature during assessments. If,'
              h(3)='after deleting a zone, the link points to a non-'
              h(4)='existant zone then flows may not be correctly'
              h(5)='predicted. Unlinking will result in the node using'
              h(6)='a fixed temperature during assessments.'
              h(7)='Because a node may have several connections within'
              h(8)='the network you must edit the network to remove'
              h(9)='such links before it can be removed. '
              CALL ASKOK(outs,'Remove this link? (see help)',OK,DOK,9)
              if(OK)then
                ICAAS(IW)=0
              endif
            endif

C Any node-zone links to subsequent zones need to be decremented.
            do 246 IX=IW,NCOMP
              if(ICAAS(IX).gt.iw)then
                ICAAS(IX)=ICAAS(IX)-1
              endif
  246       continue
          endif
        endif
      endif

C Read in control file if defined. Use IW2 index from this point.
      h(1)='The removal of a zone may result in zone or ventilation'
      h(2)='control loops which specifically sense the removed zone'
      h(3)='causing incorrect assessments. Also references to sub- '
      h(4)='sequent zones in control loops need to be adjusted. '
      if(LCTLF(1:2).eq.'  '.or.LCTLF(1:4).eq.'UNKN')then
        DOK=.false.
        CALL ASKOK(' System zone-based controls should also be',
     &   ' updated. Is one associated with the model ?',OKC,DOK,4)
      else
        DOK=.true.
        CALL ASKOK(' System zone-based controls should also be',
     &    ' updated. Do this ?',OKC,DOK,4)
      endif
      if(OKC)then
        H(1)='The system control file holds the definition of all of'
        H(2)='the building/plant controls.  This file is optional.'
        ltmp=LCTLF
        CALL EASKS(ltmp,' Control file ? ',
     &   ' ',72,'std.ctl','Control file',IER,1)
        if(ltmp(1:2).ne.'  '.and.ltmp(1:4).ne.'UNKN')then
          lctlf=ltmp
          ICTLF=IFIL+1
          CALL ERPFREE(ICTLF,ISTAT)
          call FINDFIL(LCTLF,XST)
          if(XST)then
            CALL EZCTLR(ICTLF,ITRC,IUOUT,IER)
            if(ncf.gt.0)then

C If one of the control loops references the zone to be deleted warn
C the user and ask if it should be deleted.
              do 77 ik=1,ncf
                if(ibsn(ik,1).eq.IW2.or.iban(ik,1).eq.IW2)then
                  call LSTCNTL(iuout,0,ik,IER)
                  write(outs,'(a,i2,a,a)') 'Control loop ',ik,
     &              ' is specific to ',zname(IW2)
                  DOK=.true.
                  h(1)='The control sensor or actuator references this'
                  h(2)='specific zone. '
                  CALL ASKOK(outs,' Remove this control loop?',OK,DOK,2)
                  if(ok)then

C User requests deletion of control loop. Do this and then decrement all
C icascf links to subsequent controls.
                    icfoc=0
                    call ADDCNTL(icfoc,'D',IER)
                    do 245 IXI=IW2,NCOMP
                      if(icascf(IXI).gt.ik)then
                        write(outs,'(3A)') ' shifting zone-ctl link ',
     &                    zname(IXI),'...'
                        icascf(IXI)=icascf(IXI)-1
                      endif
  245               continue
                  endif
                endif
  77          continue

C Shift sensor/actuator zone references > IW2 down one.
              do 78 ik=1,ncf
                if(ibsn(ik,1).gt.IW2)then
                  ibsn(ik,1)=ibsn(ik,1)-1
                  write(outs,'(a,i2)') 
     &              'Shifting sensor position in control loop',ik
                  call edisp(iuout,outs)
                endif
                if(iban(ik,1).gt.IW2)then
                  iban(ik,1)=iban(ik,1)-1
                  write(outs,'(a,i2)') 
     &              'Shifting actuator position in control loop',ik
                  call edisp(iuout,outs)
                endif
  78          continue
            endif
            if(ncc.gt.0)then

C If one of the ventilation control loops references the zone to be deleted warn
C the user and ask if it should be deleted.
              do 79 ik=1,ncc
                if(ifsn(ik,1).eq.IW2.or.ifan(ik,1).eq.IW2)then
                  call LSTCNTL(itru,2,ik,IER)
                  write(outs,'(a,i2,a,a)') 'Ventilation control loop ',
     &              ik,' is specific to ',zname(IW2)
                  DOK=.true.
                  h(1)='The vnetilation control sensor or actuator '
                  h(2)='references this specific zone. '
                  CALL ASKOK(outs,' Remove this control loop?',OK,DOK,2)
                  if(ok)then
                    icfoc=2
                    call ADDCNTL(icfoc,'D',IER)
                  endif
                endif
  79          continue

C Shift sensor/actuator zone references > IW2 down one.
              do 80 ik=1,ncc
                if(ifsn(ik,1).gt.IW2)then
                  ifsn(ik,1)=ifsn(ik,1)-1
                  write(outs,'(a,i2)') 
     &              'Shifting sensor position in control loop',ik
                  call edisp(iuout,outs)
                endif
                if(ifan(ik,1).gt.IW2)then
                  ifan(ik,1)=ifan(ik,1)-1
                  write(outs,'(a,i2)') 
     &              'Shifting actuator position in control loop',ik
                  call edisp(iuout,outs)
                endif
  80          continue
            endif
          endif
        endif
      endif
      
C Loop through all other zones and if a surface attribute
C faces the current zone then reset it to UNKNOWN. Also update
C the values of ICT, IE2 and IC2.
      call usrmsg('Unlinking surfaces referencing this zone...',' ','-')
      do 25 IX=1,NCOMP
        if(IX.ne.IW2)then
          changed=.FALSE.
          call eclose(gversion(IX),1.1,0.01,newgeo)
          if(newgeo)then
            call georead(IFIL+1,LGEOM(IX),IX,1,0,iuout,IER)
          else
            call egomin(IFIL+1,LGEOM(IX),IX,1,0,iuout,IER)
          endif
          do 28 ISX=1,NSUR
            ioc=IZSTOCN(ix,isx)
            if(SOTHER(ISX,1)(1:12).eq.zname(IW2)(1:12))then
              SOTHER(ISX,1)='UNKNOWN'
              SOTHER(ISX,2)='0'
              SOTHER(ISX,3)='0'
              SSOTHER(ioc,1)='UNKNOWN'
              SSOTHER(ioc,2)='0'
              SSOTHER(ioc,3)='0'
              ICT(ioc)=-1
              IE2(ioc)=0
              IC2(ioc)=0
              changed = .true.
            elseif(SOTHER(ISX,1)(1:4).eq.'WITH')then
              SOTHER(ISX,1)='UNKNOWN'
              SOTHER(ISX,2)='0'
              SOTHER(ISX,3)='0'
              SSOTHER(ioc,1)='UNKNOWN'
              SSOTHER(ioc,2)='0'
              SSOTHER(ioc,3)='0'
              ICT(ioc)=-1
              IE2(ioc)=0
              IC2(ioc)=0
              changed = .true.
            endif
   28     continue
          if(changed)then
            write(outs,'(A,A)') ' updating topology in ',zname(ix)
            call usrmsg('  ',outs,'-')
            call eclose(gversion(ix),1.1,0.01,newgeo)
            if(igupgrade.eq.2.and.(.NOT.newgeo))then
              gversion(ix) =1.1
              newgeo = .true.
            endif
            if(newgeo)then
              call geowrite(IFIL+2,LGEOM(IX),IX,iuout,3,IER)
            else
              call emkgeo(IFIL+2,LGEOM(IX),IX,iuout,3,IER)
            endif
          endif
          call usrmsg('  ','  ','-')
        endif

C Check in operations file that ventilation connections are not
C to a zone that is to be removed.
        IUO=IFIL+1
        call FINDFIL(LPROJ(IX),XST)
        IF(XST)THEN
          CALL ERPFREE(IUO,ISTAT)
          CALL EROPER(0,iuout,IUO,IX,IER)
        ENDIF

C Disconnect lower range alternative ventilation if pointing to focus.
        modopr=.false.
        if(IVL(IX).eq.IW2)then
          modopr=.true.
          IVL(IX)=0
          ACVL(IX)=0.0
        endif

C Disconnect middle range alternative ventilation if pointing to focus.
        if(IVU(IX).eq.IW2)then
          modopr=.true.
          IVU(IX)=0
          ACVU(IX)=0.0
        endif

C Disconnect high range alternative ventilation if pointing to focus.
        if(IVH(IX).eq.IW2)then
          modopr=.true.
          IVH(IX)=0
          ACVH(IX)=0.0
        endif

C Air flows
        DO 400 IDTY=1,NBDAYTYPE
          IF(NAC(IDTY).GT.0)THEN
            DO 10 I=1,NAC(IDTY)
              IF(IPT(IDTY,I).EQ.IW2)THEN
                MODOPR=.TRUE.
                IPT(IDTY,I)=0
                ACV(IDTY,I)=0.0
              ENDIF
 10         CONTINUE
          ENDIF
 400    CONTINUE

C Update operations file if any changes.
        if(modopr)then
          call edisp(iuout,' ')
          write(outs,'(1X,A,A)') zname(ix)(1:lnzname(ix)),
     &      ' has inter-zone ventilation with the'
          call edisp(iuout,outs)
          call edisp(iuout,' zone to be deleted.  Links have been')
          call edisp(iuout,' removed and the operation file is now')
          call edisp(iuout,' being updated...')
          CALL EMKOPER(IUO,LPROJ(IX),IX,iuout,IER)
        endif
  25  continue

C Reread selected zone data then remove each of the surfaces
C in reverse order (adjusting the connections as required).
      write(outs,'(2A)') ' removing surfaces in ',zname(IW2)
      call usrmsg('  ',outs,'-')
      call eclose(gversion(IW2),1.1,0.01,newgeo)
      if(newgeo)then
        call georead(IFIL+1,LGEOM(IW2),IW2,1,0,iuout,IER)
      else
        call egomin(IFIL+1,LGEOM(IW2),IW2,1,0,iuout,IER)
      endif
      IX=NSUR+1
  35  CONTINUE
      IX=IX-1
      icb=IZSTOCN(IW2,ix)
      iopt=0
      CALL ADDSUR(ITRC,IW2,IX,'D','A',iopt,IER)

C If there are any anchor points which point to this surface then the list
C must be compacted.
      if(NALOC.gt.0)then
        do 135 ia=1,NALOC
          IRVA=IALOC(ia)
          ifound=0
          do 136 ib=1,IRVA
            if(lstanchr(ia,ib).eq.icb)ifound=ib
 136      continue
          if(ifound.gt.0)then
            IALOC(ia)=IALOC(ia)-1
            do 137 ib=ifound,IALOC(ia)
              lstanchr(ia,ib)=lstanchr(ia,ib+1)
 137        continue
          endif

C Decrement any anchors to subsequent surfaces.
          do 138 ib=1,IALOC(ia)
            if(lstanchr(ia,ib).gt.icb)lstanchr(ia,ib)=lstanchr(ia,ib)-1
 138      continue
 135    continue
      endif
      IF(IX.GT.1)GOTO 35

C Perform connection shift (zone reference) for later zones.
C Also update the other side IC2.
      write(outs,'(A)') ' shifting model topology... '
      call usrmsg('  ',outs,'-')
      do 38 IX=1,NCON
        if(IC1(IX).gt.IW2)IC1(IX)=IC1(IX)-1
        if(ICT(IX).eq.3.and.IC2(IX).gt.IW2)IC2(IX)=IC2(IX)-1
   38 continue

C Ask if user wishes to remove all files related to the zone.
      dok=.false.
      h(1)='You can de-reference a zone in the current model, and'
      h(2)='you have the option to also remove the files. If you'
      h(3)='might have a use for the files later keep them. '
      CALL ASKOK(' In addition to de-referencing this zone do you',
     & ' also wish to delete the descriptive files ?',OK,dok,3)
      if(OK)then
        IUO=IFIL+1
        CALL ERPFREE(IUO,ISTAT)
        call FINDFIL(LPROJ(IW2),XST)
        if(XST)then
          CALL EFOPSEQ(IUO,LPROJ(IW2),0,IER)
          CALL EFDELET(IUO,ISTAT)
        endif

        CALL ERPFREE(IUO,ISTAT)
        call FINDFIL(LGEOM(IW2),XST)
        if(XST)then
          CALL EFOPSEQ(IUO,LGEOM(IW2),1,IER)
          if(ier.eq.0)CALL EFDELET(IUO,ISTAT)
        endif

        CALL ERPFREE(IUO,ISTAT)
        call FINDFIL(LTHRM(IW2),XST)
        if(XST)then
          CALL EFOPSEQ(IUO,LTHRM(IW2),1,IER)
          if(ier.eq.0)CALL EFDELET(IUO,ISTAT)
        endif
        if(IVF(IW2).eq.1)then
          CALL ERPFREE(IUO,ISTAT)
          CALL EFOPSEQ(IUO,LVIEW(IW2),1,IER)
          if(ier.eq.0)CALL EFDELET(IUO,ISTAT)
        endif
        if(IHC(IW2).eq.1)then
          CALL ERPFREE(IUO,ISTAT)
          CALL EFOPSEQ(IUO,LHCCO(IW2),1,IER)
          if(ier.eq.0)CALL EFDELET(IUO,ISTAT)
        endif
        if(ITW(IW2).eq.1)then
          CALL ERPFREE(IUO,ISTAT)
          CALL EFOPSEQ(IUO,LTWIN(IW2),1,IER)
          if(ier.eq.0)CALL EFDELET(IUO,ISTAT)
        endif
        if(ICGC(IW2).eq.1)then
          CALL ERPFREE(IUO,ISTAT)
          CALL EFOPSEQ(IUO,LCGCIN(IW2),1,IER)
          if(ier.eq.0)CALL EFDELET(IUO,ISTAT)
        endif
      endif

C For next zones shift surface names.
      if(IW2.eq.NCOMP)goto 46
      do 45 IX=IW2,NCOMP-1
        write(outs,'(3A)') ' shifting zone ',zname(IX),'....'
        call usrmsg('  ',outs,'-')
        DO 66 IS=1,NZSUR(IX+1)
          SNAME(IX,IS)=SNAME(IX+1,IS)
          ITMCFL(IX,IS)=ITMCFL(IX+1,IS)
   66   continue

C For each zone `above' the selected one reset the various file names
C and zone based data.
        NZSUR(IX)=NZSUR(IX+1)
        NZTV(IX)=NZTV(IX+1)
        NCCODE(IX)=NCCODE(IX+1)-1
        zname(IX)=zname(IX+1)
        lnzname(IX)=lnzname(IX+1)
        zdesc(IX)=zdesc(IX+1)
        lnzdesc(IX)=lnzdesc(IX+1)
        LPROJ(IX)=LPROJ(IX+1)
        LGEOM(IX)=LGEOM(IX+1)
        LTHRM(IX)=LTHRM(IX+1)
        LSHAD(IX)=LSHAD(IX+1)
        INDUTL(IX)=INDUTL(IX+1)
        LUTIL(IX)=LUTIL(IX+1)
        LVIEW(IX)=LVIEW(IX+1)
        LHCCO(IX)=LHCCO(IX+1)
        LTWIN(IX)=LTWIN(IX+1)
        LCGCIN(IX)=LCGCIN(IX+1)
        ZOBS(IX)=ZOBS(IX+1)
        IVF(IX)=IVF(IX+1)
        ISI(IX)=ISI(IX+1)
        IHC(IX)=IHC(IX+1)
        ITW(IX)=ITW(IX+1)
        ICGC(IX)=ICGC(IX+1)
        IOBS(IX)=IOBS(IX+1)
        icascf(IX)=icascf(IX+1)
        ICAAS(IX)=ICAAS(IX+1)

C Shift viewing bounds.
        ZXMN(IX)=ZXMN(IX+1)
        ZYMN(IX)=ZYMN(IX+1)
        ZZMN(IX)=ZZMN(IX+1)
        ZXMX(IX)=ZXMX(IX+1)
        ZYMX(IX)=ZYMX(IX+1)
        ZZMX(IX)=ZZMX(IX+1)
        ZBFLG(IX)=ZBFLG(IX+1)
        ZCOG(IX,1)=ZCOG(IX+1,1)
        ZCOG(IX,2)=ZCOG(IX+1,2)
        ZCOG(IX,3)=ZCOG(IX+1,3)

C Shift zone base surfaces and base areas.
        ZBASEA(IX)=ZBASEA(IX+1)
        IBASES(IX,1)=IBASES(IX+1,1)
        IBASES(IX,2)=IBASES(IX+1,2)
        IBASES(IX,3)=IBASES(IX+1,3)
        IBASES(IX,4)=IBASES(IX+1,4)
        IBASES(IX,5)=IBASES(IX+1,5)
        IBASES(IX,6)=IBASES(IX+1,6)

C Shift zone default solar distributions and volumes.
        NDP(IX)=NDP(IX+1)
        IDPN(IX,1)=IDPN(IX+1,1)
        IDPN(IX,2)=IDPN(IX+1,2)
        IDPN(IX,3)=IDPN(IX+1,3)
        VOL(IX)=VOL(IX+1)
   45 continue
   46 NCOMP=NCOMP-1

C Update common blocks for Geometry & operation
C commons via reading in related files.
      write(outs,'(A)') ' rebuilding model... '
      call usrmsg('  ',outs,'-')
      do 48 ir=1,NCOMP
        call eclose(gversion(ir),1.1,0.01,newgeo)
        if(newgeo)then
          call georead(IFIL+1,LGEOM(ir),ir,1,0,IUOUT,IER)
        else
          call egomin(IFIL+1,LGEOM(ir),ir,1,0,IUOUT,IER)
        endif
        XST=.false.
        call FINDFIL(LPROJ(ir),XST)
        IF(XST)THEN
          CALL ERPFREE(IFIL+1,ISTAT)
          CALL EROPER(0,iuout,IFIL+1,ir,IER)
        ENDIF
        XST=.false.
        call FINDFIL(LTHRM(ir),XST)
        IF(XST)THEN
          CALL ERPFREE(IFIL+1,ISTAT)
          CALL ECONST(LTHRM(ir),IFIL+1,ir,0,IUOUT,IER)
        ENDIF
        XST=.false.
        call FINDFIL(LTWIN(ir),XST)
        IF(XST)THEN
          CALL ERPFREE(IFIL+1,ISTAT)
          CALL ERTWIN(0,IUOUT,IFIL+1,LTWIN(ir),ir,IER)
        ENDIF
  48  continue
      call usrmsg('  ','  ','-')

C Warn user about updating the mass flow.
      if(IAIRN.ge.1.and.OKM)then
        H(1)='             - W A R N I N G - '
        H(2)='Ventilation network nodes linked to the deleted zone'
        H(3)='have been unlinked (i.e. they are assumed to be at a'
        H(4)='static temperature).'
        H(5)='If connections related to this node need to be revised '
        H(6)='use the ventilation network facility.'
        CALL PHELPD('mfs update popup',6,'-',0,0,IER)
      endif

C Write out control file to update control/zone list.
      if(OKC)then
        H(1)='             - W A R N I N G - '
        H(2)='Links to the deleted zone by zone and/or ventilation'
        H(3)='control loops have been adjusted and are being saved.'
        CALL PHELPD('control update popup',3,'-',0,0,IER)
        ICTLF=IFIL+1
        CALL CTLWRT(ICTLF,IER)
      endif

      H(1)='The current model description will be placed'
      H(2)='into this file.'
      ltmp=LCFGF
 49   CALL EASKS(ltmp,' Updated configuration file? ',
     &   ' ',72,'revised.cfg','updated config file',IER,2)
      if(ltmp(1:2).ne.'  '.and.ltmp(1:4).ne.'UNKN')then
        LCFGF=ltmp
      else
        goto 49
      endif
      CALL EMKCFG('-',IER)
      return

C File rewind errors.
  999 CALL USRMSG('Error rewinding flow network file:',
     &  LAPROB,'W')
      IER=1
      return 
      end

C ************* WARNMOD 
C WARNMOD provides warning of updates needed when model is changed.
C Initially deals with change in number of surfaces. 
C Act is action to be tested. act = 'sf+' or 'sf-' surface added:deleted.
C act = 'sat' surface attribute. act = 'str' surf transform.
C act = 'ob+' or 'ob-' obstruction changes.
C IER=0 OK. 
      SUBROUTINE WARNMOD(ICOMP,act)
#include "building.h"
      COMMON/OUTIN/IUOUT,IUIN
      integer nccode,indutl
      character LSNAM*72,LPROJ*72,LGEOM*72,LSHAD*72,LTHRM*72,LUTIL*72
      COMMON/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)
      COMMON/INDICS/IVF(MCOM),ISI(MCOM),IHC(MCOM),
     &              ITW(MCOM),ICGC(MCOM),IOBS(MCOM)
      common/UDESC/LVIEW(MCOM),LHCCO(MCOM),
     &             LTWIN(MCOM),LCGCIN(MCOM),ZOBS(MCOM)
      common/pmchange/comold,tmcold,vwfold,ishold
      CHARACTER*72 LVIEW,LHCCO,LTWIN,LCGCIN,ZOBS
      CHARACTER act*3

      LOGICAL XST,comold,tmcold,vwfold,ishold

      if(act(1:3).eq.'sf-'.or.act(1:3).eq.'sf+'.or.
     &act(1:3).eq.'sat')then
        call FINDFIL(LTHRM(ICOMP),XST)
        if(XST)then
          comold = .true.
        else
          comold = .false.
        endif
      endif

C Check for implications of surface addition or removal or attribution change.
C << refine testing for setting tmcold >>
      if(act(1:3).eq.'sf-'.or.act(1:3).eq.'sf+'.or.
     &act(1:3).eq.'sat')then
        if(ITW(ICOMP).eq.1)then
          call FINDFIL(LTWIN(ICOMP),XST)
          if(XST)then
            tmcold = .true.
          else
            tmcold = .false.
          endif
        endif
      endif
      if(act(1:3).eq.'sf-'.or.act(1:3).eq.'sf+'.or.
     &act(1:3).eq.'str')then
        if(IVF(ICOMP).eq.1)then
          call FINDFIL(LVIEW(ICOMP),XST)
          if(XST)then
            vwfold = .true.
          else
            vwfold = .false.
          endif
        endif
      endif
      if(act(1:3).eq.'sf-'.or.act(1:3).eq.'sf+'.or.
     &act(1:3).eq.'str')then
        if(ISI(ICOMP).eq.1)then
          call FINDFIL(LSHAD(ICOMP),XST)
          if(XST)then
            ishold = .true.
          else
            ishold = .false.
          endif
        endif
      endif
      if(act(1:3).eq.'ob-'.or.act(1:3).eq.'ob+')then
        if(ISI(ICOMP).eq.1)then
          call FINDFIL(LSHAD(ICOMP),XST)
          if(XST)then
            ishold = .true.
          else
            ishold = .false.
          endif
        endif
      endif

      RETURN
      END

C ************* SUMRCHG 
C SUMRCHG provides a summary of updates needed after model changes.
C Indications are collected from previous calls to WARNMOD. SUMRCHG
C should be called with act = 'i' (initialise) when entering a zone and
C with act = 'r' (report) when leaving the zone.
C IER=0 OK. 
      SUBROUTINE SUMRCHG(ICOMP,act,silent)
#include "building.h"
#include "geometry.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      integer nccode,indutl
      character LSNAM*72,LPROJ*72,LGEOM*72,LSHAD*72,LTHRM*72,LUTIL*72
      COMMON/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)
      common/UDESC/LVIEW(MCOM),LHCCO(MCOM),
     &             LTWIN(MCOM),LCGCIN(MCOM),ZOBS(MCOM)
      common/pmchange/comold,tmcold,vwfold,ishold
      common/C21/IFCFG,cfgroot,LCFGF

      CHARACTER*72 LVIEW,LHCCO,LTWIN,LCGCIN,ZOBS
      CHARACTER H*72,act*1
      character cfgroot*24,LCFGF*72
      logical comold,tmcold,vwfold,ishold,ok,dok,QUIET
      logical unixok
      logical silent  ! if true then use silent recalc

C Check if Unix-based or DOS based.
      call isunix(unixok)

      nh = 0
      if(act(1:1).eq.'i'.or.act(1:1).eq.'I')then
        comold = .false.
        tmcold = .false.
        vwfold = .false.
        ishold = .false.
        return
      elseif(act(1:1).eq.'r'.or.act(1:1).eq.'R')then
        if(comold.or.tmcold.or.vwfold.or.ishold)then
          nh=nh+1
          write(h(nh),'(3a)') 'For ',
     &    zname(icomp)(1:lnzname(icomp)),' the last action makes:'
        endif
        if(comold)then
          nh=nh+1
          write(h(nh),'(3a)')' the construction file ',
     &      LTHRM(ICOMP)(1:lnblnk(LTHRM(ICOMP))),' out of date.'
        endif
        if(tmcold)then
          nh=nh+1
          write(h(nh),'(3a)')' the TMC file ',
     &          LTWIN(ICOMP)(1:lnblnk(LTWIN(ICOMP))),' out of date.'
          nh=nh+1
          h(nh)=' (rebuild via construction facility) '
        endif
        if(vwfold)then
          nh=nh+1
          write(h(nh),'(3a)')' the view factor file ',
     &          LVIEW(ICOMP)(1:lnblnk(LVIEW(ICOMP))),' out of date.'
          nh=nh+1
          h(nh)=' (recalculate via `Options viewfactors`)'
        endif
        if(ishold)then
          nh=nh+1
          write(h(nh),'(3a)')' the shading db ',
     &        LSHAD(ICOMP)(1:lnblnk(LSHAD(ICOMP))),' out of date.'
        endif
        if(nh.gt.1)then
	  if(.NOT.silent)then
            CALL PHELPD('files out of date',nh,'-',0,0,IER)
	  endif  
        endif
      endif
      if(ishold)then
        if(silent)then
	  IRT=1
	else
          DOK=.true.
          h(1)='Shading should be recalculated if the changes you'
          h(2)='made affect isolation distribution. When in doubt'
          h(3)='it is best to recalculate.'
          CALL EASKABC(' Reflect zone change in shading options:',' ',
     &   'recalculate (silent)','recalculate (interactive)','continue',
     &    IRT,3)
        endif
        if(IRT.eq.1.or.IRT.eq.2)then
          if(IRT.eq.1) call edisp(iuout,
     & 'When finished control will be returned to the project manager.')

C If existing db out of date the perform recalculation.
          call tstamp('>','PRJ: start shading recalculation')
          if(IRT.eq.1)then
            call comissionish(icomp,'sra',ier)
            ishold=.false.
          else
            call comissionish(icomp,'ira',ier)
            ishold=.false.
          endif
        endif
      endif
      if(comold.or.tmcold)then
        if(silent)then
	  ok= .true.
	else
          DOK=.true.
          h(1)='Zone construction files should be rebuilt after '
          h(2)='surface attributes change or surfaces are added or'
          h(3)='deleted. '
          CALL ASKOK('Update the zone construction and/ or TMC files',
     &    'to reflect the changes that you have made?',OK,DOK,3)
        endif
        if(OK)then
          QUIET=.true.
          itru=iuout
          CALL EDCON(0,itru,ICOMP,QUIET,IER)
          QUIET=.false.
        endif
      endif

      RETURN
      END

C******* comissionish
C comissionish invokes the shading module in a number of modes
C act = 'sr '  silent recalculation (existing files)
C     = 'sra'  silent recalculation (of all files)
C     = 'ir '  interactive recalculation (existing files)
C     = 'ira'  interactive recalculation (of all files)
C     = 'in '  interactive new
      subroutine comissionish(iz,act,ier)
#include "building.h"
#include "geometry.h"
      
      integer lnblnk  ! function definition

      common/C21/IFCFG,cfgroot,LCFGF
      common/OUTIN/IUOUT,IUIN
      COMMON/INDICS/IVF(MCOM),ISI(MCOM),IHC(MCOM),
     &              ITW(MCOM),ICGC(MCOM),IOBS(MCOM)
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      integer nccode,indutl
      character LSNAM*72,LPROJ*72,LGEOM*72,LSHAD*72,LTHRM*72,LUTIL*72
      common/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)
      common/appw/iappw,iappx,iappy

      character LCFGF*72
      CHARACTER ZN*12,longtfile*144,longtfiledos*144
      character TMODE*8,outs*248,DOIT*248,CFGROOT*24
      character act*3  ! the parameter passed
      logical XST,concat,unixok

C Check if Unix-based or DOS based.
      call isunix(unixok)

      if(iz.eq.0.or.iz.gt.ncomp)then
        ier=1
        return  ! do not do if iz is zero
      endif

      if(ISI(iz).eq.0)then
        ier=2
        return  ! do not do if ISI has not been setup yet
      endif

C Check if file exists and get local zone name for feedback.
      call FINDFIL(LSHAD(iz),XST)
      write(ZN,'(A)') zname(iz)
      LNZ=max(1,lnzname(iz))  ! find current lenght of at least on character

C Clear command and setup terminal type.
      doit = ' '
      call tchild(ICPMOD)
      call termode(ICPMOD,tmode)

C Setup file name depending on the machine type.
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

      if(act(1:3).eq.'sr ')then

        if(XST)then
          write(doit,'(5a)') 'ish -mode text -file ',
     &      longtfile(1:lnblnk(longtfile)),' -zone ',
     &      ZN(1:lnz),' -act update_silent'
          call usrmsg('starting shading analysis via',doit,'-')

C Since silent, tell runit to use the current command window
C rather than starting up a new xterm.
          call runit(doit,'-')
          write(outs,'(3a)') 'Shading for ',ZN(1:lnz),
     &        ' has been recalculated.'
          call edisp(iuout,outs)
        else
          ier=3
          return  ! if shading file does exist
        endif

      elseif(act(1:3).eq.'sra')then

        write(doit,'(5a)') 'ish -mode text -file ',
     &    longtfile(1:lnblnk(longtfile)),' -zone ',
     &    ZN(1:lnz),' -act update_silent'
        call usrmsg('starting shading analysis via',doit,'-')

C Since silent, tell runit to use the current command window
C rather than starting up a new xterm.
        call runit(doit,'-')
        write(outs,'(3a)') 'Shading for ',ZN(1:lnz),
     &      ' has been recalculated.'
        call edisp(iuout,outs)

      elseif(act(1:3).eq.'ir ')then

C If prj initial size is a % of default pass this on to child with
C an offset from prj start position.
        if(XST)then
          if(iappw.gt.0.and.iappw.le.200)then
            write(doit,'(3a,3i4,5a)') 'ish -mode ',
     &          tmode,' -s ',iappw,iappx+15,iappy+50,' -file ',
     &          longtfile(1:lnblnk(longtfile)),' -zone ',
     &          ZN(1:lnz),' -act recalculate'
          else
            write(doit,'(7a)') 'ish -mode ',tmode,
     &         ' -s 0 0 0 -file ',longtfile(1:lnblnk(longtfile)),
     &         ' -zone ',ZN(1:lnz),' -act recalculate'
          endif
          call usrmsg('starting shading analysis via',doit,'-')
          call runit(doit,tmode)
          write(outs,'(3a)') 'Shading for ',ZN(1:lnz),
     &      ' has been recalculated.'
          call edisp(iuout,outs)
        else
          ier=3
          return  ! if shading file does exist
        endif
      elseif(act(1:3).eq.'ira')then

C If prj initial size is a % of default pass this on to child with
C an offset from prj start position.
        if(iappw.gt.0.and.iappw.le.200)then
          write(doit,'(3a,3i4,5a)') 'ish -mode ',
     &        tmode,' -s ',iappw,iappx+15,iappy+50,' -file ',
     &        longtfile(1:lnblnk(longtfile)),' -zone ',
     &        ZN(1:lnz),' -act recalculate'
        else
          write(doit,'(7a)') 'ish -mode ',tmode,
     &       ' -s 0 0 0 -file ',longtfile(1:lnblnk(longtfile)),
     &       ' -zone ',ZN(1:lnz),' -act recalculate'
        endif
        call usrmsg('starting shading analysis via',doit,'-')
        call runit(doit,tmode)
        write(outs,'(3a)') 'Shading for ',ZN(1:lnz),
     &    ' has been recalculated.'
        call edisp(iuout,outs)

      elseif(act(1:3).eq.'in ')then

        if(iappw.gt.0.and.iappw.le.200)then
          write(doit,'(3a,3i4,4a)') 'ish -mode ',tmode,
     &      ' -s ',iappw,iappx+15,iappy+50,' -file ',
     &      longtfile(:lnblnk(longtfile)),' -zone ',
     &      ZN(1:lnblnk(ZN))
        else
          write(doit,'(6a)') 'ish -mode ',tmode,
     &      ' -s 0 0 0 -file ',longtfile(:lnblnk(longtfile)),
     &      ' -zone ',ZN(1:lnblnk(ZN))
        endif
        call usrmsg('starting shading analysis via',doit,'-')
        call runit(doit,tmode)
        write(outs,'(3a)') 'Shading for ',ZN(1:lnz),
     &    ' has been recalculated.'
        call edisp(iuout,outs)
       
      endif

      return
      end

C POINTTOLINE: determines distance from a 3D point to a 3D line.
C where ipoint is the index of the test vertex, iwhich1 is the index
C of the vertex at the start of the line, iwhich2 is the index of the
C index at the end of the line, offset is the distance (m), match is
C a logical set to true if close enough.
C Only returns match=true if point was found along the line between
C the two vertices (i.e. it discards matches beyond the end points.
C It assumes that calling code will decide whether the distance
C can be used. 
      subroutine pointtoline(ipoint,iwhich1,iwhich2,offset,match)
#include "building.h"
      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      dimension vd(3),vd1(3),vd2(3)
      logical match

C If any of the indices is zero then return with match=false.
      match=.false.
      iwhich3=ipoint
      if(iwhich1.eq.0.or.iwhich2.eq.0.or.iwhich3.eq.0)then
        match=.false.
        return
      endif

C Report length of line. Use method of Ward/Radiance in fvect.c
      vd(1)= X(IWHICH2)-X(IWHICH1)
      vd(2)= Y(IWHICH2)-Y(IWHICH1)
      vd(3)= Z(IWHICH2)-Z(IWHICH1)
      call dot3(vd,vd,vdis)
      vd1(1)= X(IWHICH3)-X(IWHICH1)
      vd1(2)= Y(IWHICH3)-Y(IWHICH1)
      vd1(3)= Z(IWHICH3)-Z(IWHICH1)
      call dot3(vd1,vd1,vdis1)
      vd2(1)= X(IWHICH3)-X(IWHICH2)
      vd2(2)= Y(IWHICH3)-Y(IWHICH2)
      vd2(3)= Z(IWHICH3)-Z(IWHICH2)
      call dot3(vd2,vd2,vdis2)
      if(vdis2.gt.vdis1)then
        if((vdis2 - vdis1).gt.vdis)then
          match=.false.
          offset=0.0
          return
        endif
      else
        if((vdis1 - vdis2).gt.vdis)then
          match=.false.
          offset=0.0
          return
        endif
      endif
      d2l=(vdis1-(vdis+vdis1-vdis2)*
     &    (vdis+vdis1-vdis2)/vdis/4.0)
      if(abs(d2l).lt.0.003)then
        offset=d2l
      else
        offset=SQRT(d2l)
      endif
      match=.true.
      return
      end


C TWODPOINTTOLINE: determines distance from a 2D point to the 2D lines
C associated with the current zone wireframe. Where:
C itx & ity are the test pixel location,
C icomp is the zone to check within and ioffset is the number of pixels
C  of tolerance allowed.
C match is a logical signaling that something was found
C ifsurf1 & ifsurf2 are surfaces associated with the line (both zero 
C   if nothing found) and
C ifvrt1 and ifvert2 are nearest two vertices on the edge (zero if
C   nothing found).
C      subroutine twodpointtoline(itx,ity,icomp,ioffset,ifsurf1,ifsurf2,
C     &  ifvrt1,ifvrt2,match)
C#include "building.h"

C << logic needs to be written >>

C      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
C      dimension vd(3),vd1(3),vd2(3)
C      logical match

C If any of the indices is zero then return with match=false.
C      match=.false.
C      iwhich3=ipoint
C      if(iwhich1.eq.0.or.iwhich2.eq.0.or.iwhich3.eq.0)then
C        match=.false.
C        return
C      endif

C Report length of line. Use method of Ward/Radiance in fvect.c
C      vd(1)= X(IWHICH2)-X(IWHICH1)
C      vd(2)= Y(IWHICH2)-Y(IWHICH1)
C      vd(3)= Z(IWHICH2)-Z(IWHICH1)
C      call dot3(vd,vd,vdis)
C      vd1(1)= X(IWHICH3)-X(IWHICH1)
C      vd1(2)= Y(IWHICH3)-Y(IWHICH1)
C      vd1(3)= Z(IWHICH3)-Z(IWHICH1)
C      call dot3(vd1,vd1,vdis1)
C      vd2(1)= X(IWHICH3)-X(IWHICH2)
C      vd2(2)= Y(IWHICH3)-Y(IWHICH2)
C      vd2(3)= Z(IWHICH3)-Z(IWHICH2)
C      call dot3(vd2,vd2,vdis2)
C      if(vdis2.gt.vdis1)then
C        if((vdis2 - vdis1).gt.vdis)then
C          match=.false.
C          offset=0.0
C          return
C        endif
C      else
C        if((vdis1 - vdis2).gt.vdis)then
C          match=.false.
C          offset=0.0
C          return
C        endif
C      endif
C      d2l=(vdis1-(vdis+vdis1-vdis2)*
C     &    (vdis+vdis1-vdis2)/vdis/4.0)
C      if(abs(d2l).lt.0.003)then
C        offset=d2l
C      else
C        offset=SQRT(d2l)
C      endif
C      match=.true.
C      return
C      end

C ************* silentmodel
C silentmodel creates an/or updates a model from passed parameters.
C The parameters are:
C action (8 char) as follows: 'new' creates model folders based on information
C    in root, mpath folder parameters. 'cont' continues with current model,
C    'update' updates the current model weather data, 'within' assumes that
C    the current folder will be used.
C root   (24 char) the root name of the model
C mpath  (72 char) the path to the folder with cfg file.
C folder (12 char) model folder layout: 'single' or 'distributed'
C weather (32 char) the weather file name (no path included)

C Current functionality is to update the climate & site data.

      subroutine silentmodel(action,root,mpath,folder,menu,weather,ier)
#include "building.h"
#include "espriou.h"
#include "esprdbfile.h"
C esprdbfile.h supplies lclim.
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      common/FILEP/IFIL

C Where ESP-r was installed (as recorded when it was compiled).
      common/deflt4/dinstpath
      character dinstpath*48

C Climate data.
      COMMON/CLMDT0/CLAT,CLONG

      COMMON/C4/XLAT,XLON
      COMMON/C14/UGRDTP(12,MGRDP),NGRDP
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME

      common/XMLDATA/XMLMETRICS(200),inumXMLmetrics
      character XMLMETRICS*248

      common/paths/zonepth,netpth,ctlpth,imgpth,radpth,docpth,tmppth,
     &             dbspth
      character zonepth*24,netpth*24,ctlpth*24,imgpth*24,radpth*24
      character docpth*24,tmppth*24,dbspth*24

C Passed in parameters.
      character action*8,root*24,mpath*72,folder*12,menu*32
      character fs*1           ! file separator
      character weather*32     ! file name for the location (no path)
      character weathergr*36   ! file name for weather related ground temps (no path)
      character groundfile*72  ! file name for weather related gorund temps (with path)
      character subpath*72

      character outs*124,OUTSTR*124
      logical unixok
      logical haveground ! set true if ground temp file has been found
      integer lnw   ! length of weather string.
      integer lnwg   ! length of weather ground string.
      integer igrfile ! unit number of ground file
      DIMENSION GVA(12)
      character inpxmlfl*248 !to write out the input.xml file
      
      IER=0

C Determine operating system
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif
      subpath=' '
      haveground=.false.

C If the action is `new` then setup model folder(s) and the registration
C level configuration file via call to pregist in `sn` mode.
C If action is `cont` then there is no pre-registration task. If action
C is `update` then there is no pre-registration task.
C Debug
C      write(6,*)  'action ',action,' ',root,' ',folder
C      write(6,*)  'menu ',menu
C      write(6,*)  'mpath ',mpath
C      write(6,*)  'weather ',weather
      if(action(1:3).eq.'new')then
        call pregist('sn',root,mpath,folder,menu,ier)
      elseif(action(1:6).eq.'within')then

C Assume the model folders exist when calling pregist.
        call pregist('sw',root,mpath,folder,menu,ier)
      elseif(action(1:4).eq.'cont')then

C Assume the folder exists
        root=' '
        mpath=' '
        folder=' '
      elseif(action(1:6).eq.'update')then
        root=' '
        mpath=' '
        folder=' '
      endif

C If there is a line with the root name of a climate file look
C for a match in the current climate location and if there is
C one set the name of that file, scan it and find its site.
      if(weather(1:4).eq.'none')then
        continue
      else

C First check if the climate file exists in the subpath. Also
C make up a variant of the weather file name in case there
C might be a small 12 item file for ground temperatures which
C has a matching name.gnd.
        if(ICLIM.eq.0)then
          ICLIM=IFIL
        endif
        igrfile=IFIL+2  ! hopefully not used
        lnw=lnblnk(weather)
	write(weathergr,'(2a)') weather(1:lnw),'.gnd'
        lnwg=lnblnk(weathergr)

        write(subpath,'(4a)')
     &    dinstpath(1:lnblnk(dinstpath)),fs,'climate',fs
        write(LCLIM,'(2a)') subpath(1:lnblnk(subpath)),
     &    weather(1:lnw)
        write(groundfile,'(2a)') subpath(1:lnblnk(subpath)),
     &    weathergr(1:lnwg)
        write(currentfile,'(a)') LCLIM(1:lnblnk(LCLIM))
        CALL ERPFREE(ICLIM,ISTAT)
        CALL FPRAND(ICLIM,ISTAT,144,0,LCLIM)
        if(ISTAT.ge.0)then
          CALL ERPFREE(ICLIM,ISTAT)
	  
C Also check to see if a ground temperature file exists
          write(currentfile,'(a)') groundfile(1:lnblnk(groundfile))
          call FPOPEN(igrfile,ISTAT,1,0,groundfile)
          if(ISTAT.ge.0)then
            haveground=.true.
            CALL ERPFREE(igrfile,ISTAT)
          endif
          goto 75
        else

C Also look in the folder where the model is being created.
          write(LCLIM,'(3a)') mpath(1:lnblnk(mpath)),fs,
     &      weather(1:lnw)
          write(currentfile,'(a)') LCLIM(1:lnblnk(LCLIM))
          write(groundfile,'(3a)') mpath(1:lnblnk(mpath)),fs,
     &      weathergr(1:lnwg)

          CALL ERPFREE(ICLIM,ISTAT)
          CALL FPRAND(ICLIM,ISTAT,144,0,LCLIM)
          if(ISTAT.ge.0)then
            CALL ERPFREE(ICLIM,ISTAT)
	  
C Also check to see if a ground temperature file exists
            write(currentfile,'(a)') groundfile(1:lnblnk(groundfile))
            call FPOPEN(igrfile,ISTAT,1,0,groundfile)
            if(ISTAT.ge.0)then
              haveground=.true.
              CALL ERPFREE(igrfile,ISTAT)
            endif
            goto 75
          else

C Also look in the model dbs folder.
            write(LCLIM,'(5a)') mpath(1:lnblnk(mpath)),fs,
     &        dbspth(1:lnblnk(dbspth)),fs,weather(1:lnw)
            write(currentfile,'(a)') LCLIM(1:lnblnk(LCLIM))
            write(groundfile,'(5a)') mpath(1:lnblnk(mpath)),fs,
     &        dbspth(1:lnblnk(dbspth)),fs,weathergr(1:lnwg)
     
            CALL ERPFREE(ICLIM,ISTAT)
            CALL FPRAND(ICLIM,ISTAT,144,0,LCLIM)
            if(ISTAT.ge.0)then
              CALL ERPFREE(ICLIM,ISTAT)
	  
C Also check to see if a ground temperature file exists
              write(currentfile,'(a)') groundfile(1:lnblnk(groundfile))
              call FPOPEN(igrfile,ISTAT,1,0,groundfile)
              if(ISTAT.ge.0)then
                haveground=.true.
                CALL ERPFREE(igrfile,ISTAT)
              endif
              goto 75
            else
              write(currentfile,'(a)') DCLIM(1:lnblnk(DCLIM))
              CALL ERPFREE(ICLIM,ISTAT)
              CALL FPRAND(ICLIM,ISTAT,144,0,DCLIM)
              if(ISTAT.ge.0)then
                CALL ERPFREE(ICLIM,ISTAT)
                LCLIM=DCLIM
                goto 75
              else

C Fell through, no change in climate and location.
                call edisp(iuout,'No climate data located.')
                goto 74
              endif
            endif
          endif
        endif

  75    continue
        CALL ERPFREE(ICLIM,ISTAT)
        CALL FPRAND(ICLIM,ISTAT,144,1,LCLIM)
        IREC=366
        READ(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=1000)ICYEAR,ICDNGH
        IREC=368
        READ(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=1000)CLAT,CLONG
        CALL ERPFREE(ICLIM,ISTAT)
        XLAT=CLAT
        XLON=CLONG
        IYEAR=ICYEAR
        write(outs,'(A,F7.3,A,F7.3,a,i4,i5,a,2F7.3,a)')
     &    'The climate lat is ',clat,' & long diff is ',
     &    clong,' & Year DN flag ',IYEAR,ICDNGH,
     &    '. Previously used ',xlat,xlon,'.'
        call edisp(iuout,outs)

C << place to define a calendar if this has not already been
C << done.
	
C If there is ground temperature information scan that file and
C put the data in UGRDTP.
        if(haveground)then
          write(currentfile,'(a)') groundfile(1:lnblnk(groundfile))
          call FPOPEN(igrfile,ISTAT,1,0,groundfile)
          NGRDP=1
          CALL STRIPC(igrfile,OUTSTR,0,ND,1,'ground data',IER)
	  
C          CALL EGETWRA(igrfile,GVA,12,-9.,99.,'W','Gr tmp',IER)
          DO 29 J=1,12
            CALL EGETWR(OUTSTR,K,valy,0.,0.,'-','ground temp',IER)
C            UGRDTP(J,NGRDP)=GVA(J)
            UGRDTP(J,NGRDP)=valy
   29     CONTINUE

C Debug.
          CALL EDISP(iuout,' From file Ground temp profile Jan-Dec:')
          WRITE(OUTS,'(12F6.1)')(UGRDTP(J,1),J=1,12)
          CALL EDISP(iuout,OUTS)

          CALL ERPFREE(igrfile,ISTAT)
        else
          NGRDP=0    ! no user defined ground temperature profiles.
        endif
        CALL EMKCFG('-',IER)

C Also write the xml input file if there is a metric defined in the flat file
        if(inumXMLmetrics.gt.0)then
          if(unixok)then
            fs = char(47)
          else
            fs = char(92)
          endif

C Write the XML input file
          write(inpxmlfl,'(3a)') mpath(1:lnblnk(mpath)),fs,
     &    'cfg/input.xml'
          iuf=ifil+2
          call efopseq(iuf,inpxmlfl,3,ier)
          write(iuf,'(a)',iostat=ios) 
     &      '<?xml version="1.0" encoding="UTF-8"?>'
          write(iuf,'(a)',iostat=ios)'<configuration>'
          write(iuf,'(a)',iostat=ios) 
     &      '  <apply_style_sheet>false</apply_style_sheet>'
          write(iuf,'(a)',iostat=ios) 
     &      '  <dump_all_data>false</dump_all_data>'  
          write(iuf,'(a)',iostat=ios) 
     &      '  <hierarchy>flat</hierarchy>'  
          write(iuf,'(a)',iostat=ios) 
     &      '  <link_style_sheet>false</link_style_sheet>'  
          write(iuf,'(a)',iostat=ios) 
     &      '  <output_dictionary>true</output_dictionary>'  
          write(iuf,'(a)',iostat=ios) 
     &'  <report_startup_period_data>false</report_startup_period_data>'
          write(iuf,'(a)',iostat=ios) 
     &      '  <save_to_disk>false</save_to_disk>'  
          write(iuf,'(a)',iostat=ios) 
     &      '  <time_step_averaging>true</time_step_averaging>'  
          write(iuf,'(a)',iostat=ios) 
     &      '  <enable_xml_wildcards>true</enable_xml_wildcards>'

C Loop around the possible defined xml outputs
          do 200 iwrxmlout=1,inumXMLmetrics
          write(iuf,'(3a)',iostat=ios)'  <step_variable>', 
     &      XMLMETRICS(iwrxmlout)(1:lnblnk(XMLMETRICS(iwrxmlout))),
     &     '</step_variable>'
 200      continue       
          write(iuf,'(a)',iostat=ios)'</configuration>'
          call erpfree(iuf,istat)
        endif
      
C Fall through point if no climates located.
  74    continue
      endif

C Other logic here....

      return

C Error trap. Set ier as two.
 1000 WRITE(outs,774)ISTAT
  774 FORMAT(' Error ',I7,' reading Climate db information.')
      call edisp(iuout,outs)
      call edisp(iuout,' ')
      ier=2
      RETURN

      end

C ************* silentzone 
C silentzone creates a zone (or limited complexity) based on parameters passed
C to it.
C shape (8 char) box/extrude
C nbwalls (integer) how many walls (e.g. 4 for a box, 3-20 for extrude). Note
C   the top surface is assumed to be index nbwalls+1 and the base surface is
C   assumed to be index nbwalls+2
C nbmass (integer) how many pairs of mass walls to append to zone. Note these
C   surfaces are not included in the *surface list
C origin (real * 3) XYZ of the origin in the case of shape=box. For shape=extrude
C   the 1st value is Z point of base, 2nd is the ceiling Z value, 3rd not used.
C size (real *3) for shape=box this is the length/width/height (m) and for
C   shape=extrude this should be zero
C nbpolyc (integer) number of initial coordinates
C coords (real array (nbwalls,3)) for shape=box ignore, for shape=extrude the
C   1st value is X and 2nd is Y (in world coordinates) following the normal
C   floor plan extrusion conventions. For shape=poly the Z is used.
C list (integer array (nbwalls,24) equivalent to jvn() for shape=poly
C nbedges (integer array (nbwalls) equivalent to nver() for shape=poly
C rotateit (real) angle to rotate the zone using the normal conventions and assuming
C   rotation is about the site origin
C hasglaze (real array) for each surface in the initial shape a zero has no glazing
C   and non-zero is the percentage of the parent surface for the glazing. It is
C   assumed that a default shape for the glazing is ok.
C hasdoor (real array) for each surface in the initial shape a zero indicates
C   no door and a non-zero indicates the width of the door (m).
C hasname (string array) the surface name for each surface
C hasconstruction (string array) the name of the MLC to use for each surface
C boundarytype (integer (MS,3) the connection data for each surface.
C datamass (real 4*7) values to create mass
C textmass (char *32 4*3) text associated with mass
C use integer*3 directives for zone use pattern.,
c usefile char*32 file name (w/o path) of pattern operation file.
C IER=0 OK. 
      subroutine silentzone(ICOMP,name,shape,nbwalls,nbmass,nbedges,
     &  nbpolyc,list,origin,size,coords,rotateit,
     &  nbglz,hasglaze,nbdoor,hasdoor,hasname,hasconstruction,
     &  boundarytype,datamass,textmass,use,usefile,IER)
#include "building.h"
#include "geometry.h"
C#include "esprdbfile.h"
      
      integer lnblnk  ! function definition

      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER NBDAYTYPE,NBCALDAYS,ICALENDER
      COMMON/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV

      COMMON/G5/SNAME(MCOM,MS),SOTF(MS),SMLCN(MS),SVFC(MS),SOTHER(MS,3),
     &          SUSE(MS,2),SPARENT(MS)
      COMMON/G6/SSNAME(MCON),SSOTF(MCON),SSMLCN(MCON),SSVFC(MCON),
     &          SSOTHER(MCON,3),SSPARENT(MCON),SSUSE(MCON,2)
      COMMON/G7/SSNA(MCON),SSPAZI(MCON),SSPELV(MCON),SSPERIM(MCON),
     &          SSUREQN(MCON,4),SSURCOG(MCON,3),SSURVN(MCON,3)

      character DESC*48
      integer NMLC,IPR,LAYERS
      real DTHK,DRAIR
      COMMON/MLC/NMLC,DESC(MMLC),DTHK(MMLC,ME),IPR(MMLC,ME),
     &           LAYERS(MMLC),DRAIR(MMLC,ME,3)
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      integer nccode,indutl
      character LSNAM*72,LPROJ*72,LGEOM*72,LSHAD*72,LTHRM*72,LUTIL*72
      COMMON/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      COMMON/C14/UGRDTP(12,MGRDP),NGRDP
      COMMON/C24/IZSTOCN(MCOM,MS)
      common/user/browse
      common/paths/zonepth,netpth,ctlpth,imgpth,radpth,docpth,tmppth,
     &             dbspth
      common/cctlnm/ctldoc,lctlf
      common/cctl/icascf(mcom)
      COMMON/INDICS/IVF(MCOM),ISI(MCOM),IHC(MCOM),
     &              ITW(MCOM),ICGC(MCOM),IOBS(MCOM)
      common/UDESC/LVIEW(MCOM),LHCCO(MCOM),
     &             LTWIN(MCOM),LCGCIN(MCOM),ZOBS(MCOM)

C Where ESP-r was installed (as recorded when it was compiled).
      common/deflt4/dinstpath
      character dinstpath*48

C Version of operations file. ip3ver=0 standard, =1 sorted with header
C =2 with more than 3 day types
      common/p3ver/ip3ver(MCOM)
      COMMON/P2N/NAC(MDTY),IACS(MDTY,MA),IACF(MDTY,MA),ACI(MDTY,MA),
     &ACV(MDTY,MA),IPT(MDTY,MA),TA(MDTY,MA)
      INTEGER NAC,IACS,IACF,IPT
      REAL ACI,ACV,TA
      COMMON/P3N/NCAS(MDTY),ICGS(MDTY,MC),ICGF(MDTY,MC),
     &CMGS(MDTY,MC),CMGL(MDTY,MC),RADC(MDTY,MC),CONC(MDTY,MC)
      INTEGER NCAS,ICGS,ICGF
      REAL CMGS,CMGL,RADC,CONC

      CHARACTER oprdesc*248,ventdesc*248,ctlstr*24
      COMMON/P1/oprdesc(MCOM),ventdesc(MCOM),ctlstr(MCOM,MDTY)

      common/loadlabel/lodlabel(mcom,7)
      CHARACTER*6 lodlabel

      COMMON/Vld20/Vldtng
      logical Vldtng      
      common/SLNTFULL/ifullysilent
      integer ifullysilent !flag to drive the silent model creation without
                           !questions about the operation files data

C Paremeters passed in:
      character name*12   ! name given to the zone
      character shape*8   ! box or extrude
      integer nbwalls     ! nb of vertical walls
      integer nbedges(MS) ! nb of edges per surface in poly shaped zone
      integer list(MS,24) ! indices of coords making up edges of each surface
      real origin(3)      ! values dependant on shape
      real datamass(4,7)  ! origin and size and orientations of mass (4 per zone)
      real size(3)        ! values dependant on shape
      real coords(MS,3)
      real rotateit       ! angle of rotation
      real hasglaze(MS)   ! percentage of glazing in each surface of intial shape
      real hasdoor(MS)    ! width of door in each surface of the initial shape
      character hasconstruction*32
      dimension hasconstruction(MS)  ! construction to use for each surface
      character hasname*12
      dimension hasname(MS)  ! name to use for each surface
      integer boundarytype(MS,3)
      character use*8,usefile*32  ! usage pattern directives
      dimension use(3)

C Stings associated with internal mass. 1=surface name, 2=construction, 3=optics
      character textmass*32 
      dimension textmass(4,3)
     
      DIMENSION XX(MS),YY(MS)
      CHARACTER LVIEW*72,LHCCO*72,LTWIN*72,LCGCIN*72,ZOBS*72
      CHARACTER SOTHER*24,SNAME*12,SMLCN*32,SVFC*4,SOTF*32,SUSE*8
      CHARACTER SPARENT*12
      CHARACTER SSMLCN*32,SSVFC*4,SSOTF*32,SSOTHER*24,SSNAME*12,SSUSE*8
      character SSPARENT*12
      character fs*1
      character ctldoc*248,LCTLF*72,zd*64
      character DFILE*72,CFILE*72,OFILE*72,TFILE*72
      character zonepth*24,netpth*24,ctlpth*24,imgpth*24,radpth*24
      character docpth*24,tmppth*24,dbspth*24
      character act*2
      character subpath*72,sfile*144
      character icact*8,iwatt*8,afact*8,iflow*8,vflow*8
      character sn*12,constr*12,opt*12
      LOGICAL browse,OKC,XST,unixok
      logical willneedtmc  !  if a tmc file will be required.
      logical closev       !  mass is close to vertical
      LOGICAL QUIET

C Strings for surface attributes to pass to insrec.
      character rsname*12,rsotf*32,rsmlcn*32,rsuse1*8,rsuse2*8
      character rsparent*12
      integer newsurf   ! index of surface when adding door/glazing
      integer ibcs      ! connection associated with base surface

C If browsing then user cannot create a new zone.
      if(browse)then
        call usrmsg('Cannot update model while in browse',
     &    'mode, you must `own` the model!','W')
        return
      endif

C Initialise flag
      ifullysilent=0
      
C Set folder separator (fs) to \ or / as required.
      willneedtmc=.false.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

C Clear local coordinate array.
      DO 345 IW=1,MS
        XX(iw)=0.0
        YY(iw)=0.0
 345  continue

C Creation of a new zone, begin with name, trapping out
C illegal characters.
      call tstamp('>','PRJ: new zone')
      call st2name(name,zname(ICOMP))
      lnzname(ICOMP)=lnblnk(zname(ICOMP))

C << Unix vs Windows needs to be updated >>
      if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
        WRITE(DFILE,'(2a)') zname(ICOMP)(1:lnzname(ICOMP)),'.geo'
        WRITE(CFILE,'(2a)') zname(ICOMP)(1:lnzname(ICOMP)),'.con'
        WRITE(TFILE,'(2a)') zname(ICOMP)(1:lnzname(ICOMP)),'.tmc'
        WRITE(OFILE,'(2a)') zname(ICOMP)(1:lnzname(ICOMP)),'.opr'
      else
        WRITE(DFILE,'(4a)') zonepth(1:lnblnk(zonepth)),fs,
     &    zname(ICOMP)(1:lnzname(ICOMP)),'.geo'
        WRITE(CFILE,'(4a)') zonepth(1:lnblnk(zonepth)),fs,
     &    zname(ICOMP)(1:lnzname(ICOMP)),'.con'
        WRITE(TFILE,'(4a)') zonepth(1:lnblnk(zonepth)),fs,
     &    zname(ICOMP)(1:lnzname(ICOMP)),'.tmc'
        WRITE(OFILE,'(4a)') zonepth(1:lnblnk(zonepth)),fs,
     &    zname(ICOMP)(1:lnzname(ICOMP)),'.opr'
      endif
      LGEOM(ICOMP)=DFILE

      write(zd,'(4a)') 
     &  zname(ICOMP)(1:lnzname(ICOMP)),' auto-generated for shape ',
     &  shape(1:lnblnk(shape)),' & attributes.'
      zdesc(ICOMP)=zd
      lnzdesc(ICOMP)=lnblnk(zd)  ! update the length of this string.

C Choices based on the value of shape. 
      IER=0
      if(shape(1:3).eq.'box')then
        ishape=1
      elseif(shape(1:7).eq.'extrude')then
        ishape=2
      elseif(shape(1:4).eq.'poly')then
        ishape=3
      else
        ier=1
        return
      endif
      IF(ishape.eq.1)THEN
        XOC=origin(1)
        YOC=origin(2)
        ZOC=origin(3)
        DXC=size(1)
        DYC=size(2)
        DZC=size(3)

        ANG=rotateit

C Convert box into a gen description.
        CALL ERECC(XOC,YOC,ZOC,DXC,DYC,DZC,ANG)
        NSUR=6
        IUZBASEA(icomp)=0
        IBASES(ICOMP,1)=6
        IZBASELIST(ICOMP)=1
      ELSEIF(ishape.eq.2)THEN

C User begins with an extruded shape.
        Z1=origin(1)
        Z2=origin(2)
        NW=nbwalls

        DO 45 IW=1,NW
          XX(IW)=coords(IW,1)
          YY(IW)=coords(IW,2)
   45   CONTINUE

        AR=rotateit

C Convert into a gen body and rotate if required.
        CALL EREGC(NW,Z1,Z2,XX,YY)
        IF(AR.LT.-.01.OR.AR.GT..01)then
          x1=XX(1)
          y1=YY(1)
          CALL ESCROT(AR,x1,y1)
        endif
        NSUR=NW+2
        IUZBASEA(icomp)=0
        IBASES(ICOMP,1)=NSUR
        IZBASELIST(ICOMP)=1
      ELSEIF(ishape.eq.3)THEN

C For a poly shaped zone use similar logic to reading geometry file.
        NSUR=nbwalls
        NZSUR(ICOMP)=NSUR
        NTV=nbpolyc
        NZTV(ICOMP)=NTV
        AR=rotateit
        DO 62 I=1,NTV
          X(I)=coords(I,1)
          Y(I)=coords(I,2)
          Z(I)=coords(I,3)
   62   CONTINUE
        DO 10 I=1,NSUR
          NVER(I)=nbedges(I)
          DO 12 KV=1,NVER(I)
            JVN(I,KV)=list(I,KV)
   12     CONTINUE
   10   CONTINUE
        IUZBASEA(icomp)=0
        IBASES(ICOMP,1)=NSUR  ! initial guess, need to check
        IZBASELIST(ICOMP)=1

C Rotate if required.
        if(AR.LT.-.01.OR.AR.GT..01)then
          x1=X(1)
          y1=Y(1)
          CALL ESCROT(AR,x1,y1)
        endif

      ENDIF

C Begin with default assumptions for each surface then overwrite.
      CALL FILSUR(iuout,ICOMP,0)

C Update the connection list.
      ICCC=NCON
      DO 32 ICC=1,NSUR
        ICCC=ICCC+1
        IC1(ICCC)=ICOMP
        IE1(ICCC)=ICC
        ICT(ICCC)=boundarytype(icc,1)  ! used passed in boundarytype
        IC2(ICCC)=boundarytype(icc,2)
        IE2(ICCC)=boundarytype(icc,3)

C Instanciate the surface names and the constructions.
        write(sname(icomp,icc),'(a)') hasname(icc)
        write(smlcn(icc),'(a)') hasconstruction(icc)

C Loop thru all of the constructions to find the matching construction
C and then see if it is opaque or transparent and copy out the relevant
C section of the string DESC.
        imatch=0
        do 5 ii=1,nmlc
          if(SMLCN(icc)(1:12).eq.DESC(ii)(1:12)) then
            imatch=ii          
          endif
  5     continue
        if(imatch.eq.0) then
          call edisp(iuout,'Warning: no matching MLC defined!')
          SOTF(icc)='UNKNOWN'
        else
          if(DESC(imatch)(15:18).EQ.'OPAQ')then
            SOTF(icc)='OPAQUE'
          else
            WRITE(SOTF(icc),'(A)') DESC(imatch)(21:32)
            willneedtmc=.true.
          endif
        endif
        SPARENT(icc)='-'
        SUSE(icc,1)='-'
        SUSE(icc,2)='-'

C Use boundarytype to instanciate sother & ssother << to be done >>
        IF(boundarytype(icc,1).EQ.0)THEN
          SOTHER(icc,1)='EXTERIOR'
        ELSEIF(boundarytype(icc,1).EQ.1)THEN
          SOTHER(icc,1)='SIMILAR'
        ELSEIF(boundarytype(icc,1).EQ.2)THEN
          SOTHER(icc,1)='CONSTANT'
        ELSEIF(boundarytype(icc,1).EQ.3)THEN
          SOTHER(icc,1)='ANOTHER'
        ELSEIF(boundarytype(icc,1).EQ.4)THEN
          SOTHER(icc,1)='GROUND'
        ELSEIF(boundarytype(icc,1).EQ.5)THEN
          SOTHER(icc,1)='ADIABATIC'
        ELSEIF(boundarytype(icc,1).EQ.6)THEN
          SOTHER(icc,1)='BASESIMP'
        ELSEIF(boundarytype(icc,1).EQ.7)THEN
          SOTHER(icc,1)='IDENT_CEN'
        ELSE

C Reset to represent UNKNOWN
          SOTHER(icc,1)='UNKNOWN'
        ENDIF
        write(SOTHER(icc,2),'(i2.2)') boundarytype(icc,2)
        write(SOTHER(icc,3),'(i2.2)') boundarytype(icc,3)

        SSNAME(iccc)=SNAME(icomp,icc)
        SSOTF(iccc)=SOTF(icc)
        SSMLCN(iccc)=SMLCN(icc)
        SSVFC(iccc)=SVFC(icc)
        SSOTHER(iccc,1)=SOTHER(icc,1)
        SSOTHER(iccc,2)=SOTHER(icc,2)
        SSOTHER(iccc,3)=SOTHER(icc,3)
        SSUSE(iccc,1)=SUSE(icc,1)
        SSUSE(iccc,2)=SUSE(icc,2)
        SSPARENT(iccc)=SPARENT(icc)
        IZSTOCN(icomp,icc)=iccc
   32 CONTINUE
      NCON=ICCC

      CTYPE(icomp)='GEN '
      NDP(ICOMP)=3
      IDPN(ICOMP,1)=0
      IDPN(ICOMP,2)=0
      IDPN(ICOMP,3)=0
      NZSUR(ICOMP)=NSUR  ! update nzsur() it is needed by zgupdate.
      NZTV(ICOMP)=NTV

C Update the G7 common blocks and then if assign ZBASEA.
      call zgupdate(0,ICOMP,ier)
      if(ishape.eq.1)then
        ibcs=IZSTOCN(icomp,IBASES(ICOMP,1))
        ZBASEA(icomp)= SSNA(ibcs)
      elseif(ishape.eq.2)then
        ibcs=IZSTOCN(icomp,IBASES(ICOMP,1))
        ZBASEA(icomp)= SSNA(ibcs)
      elseif(ishape.eq.3)then

C For poly shape loop to find the floor in order to set up the floor area
        do 222 iSurface=1,NSUR
          if(SVFC(iSurface).eq.'FLOR')then
            IBASES(ICOMP,1)=iSurface
            ibcs=IZSTOCN(icomp,IBASES(ICOMP,1))
            ZBASEA(icomp)= SSNA(ibcs)
          endif
 222    continue
      endif
 
C Save this to file before passing into the geometry editing facility.
C Use gversion 1.1.
      if(igupgrade.lt.2)then
        igupgrade=2
        gversion(icomp) =1.1
      endif
      call geowrite(IFIL+2,LGEOM(ICOMP),ICOMP,iuout,3,IER)
      IF(IER.EQ.1)THEN
        call usrmsg('Problem creating geometry file.','giving up','W')
        return
      ENDIF

C Read in control file if defined.
      OKC=.false.
      if(LCTLF(1:1).ne.' '.and.LCTLF(1:4).ne.'UNKN')OKC=.true.
      if(OKC)then
        ICTLF=IFIL+1
        CALL ERPFREE(ICTLF,ISTAT)
        call FINDFIL(LCTLF,XST)
        IF(XST) CALL EZCTLR(ICTLF,0,IUOUT,IER)
      endif

      NCOMP=NCOMP+1
      NCCODE(ICOMP)=NCOMP
      if(OKC)then
        icascf(NCOMP)=0
        call usrmsg(' updating control for additional zone...',' ','-')
        call CTLWRT(ICTLF,IER)
        call usrmsg(' ',' ','-')
      endif

C Update the G7 common blocks and then the configuration file.
C Use the 's' parameter to emkcfg to avoid asking for cnn file name.
      call zgupdate(0,ICOMP,ier)
      CALL EMKCFG('s',IER)

C Next, if there are doors make standard calls to insert them into
C the parent surface. To avoid clashes with any subsequent windows
C place the door offset to the right (get bounding box via a call
C to SURLEHI with some space between it and the right edge of the
C parent surface.  Later add logic to better place the door..
      if(nbdoor.gt.0)then
        do 78 ids=1,NZSUR(ICOMP)
          if(hasdoor(ids).gt.0.0)then
            call georead(IFIL+2,LGEOM(ICOMP),ICOMP,1,0,iuout,IER)
            newsurf=nsur+1
            isurf=ids
            CALL SURLEHI(isurf,XYMAX,ZMAX)  ! get width of parent
            XOC=XYMAX-(hasdoor(ids)+0.3)
            ZOC=0.0
            XW=hasdoor(ids)
            if(ZMAX.gt.2.3)then
              ZH=2.1
            else
              ZH=ZMAX-0.3
            endif
            act='sd'
            write(rsname,'(a)') hasname(newsurf)
            write(rsmlcn,'(a)') hasconstruction(newsurf)

C Loop thru all of the constructions to find the matching construction
C and then see if it is opaque or transparent and copy out the relevant
C section of the string DESC.
            imatch=0
            do 56 ii=1,nmlc
              if(rsmlcn(1:12).eq.DESC(ii)(1:12)) then
                imatch=ii          
              endif
 56         continue
            if(imatch.eq.0) then
              rsotf='UNKNOWN'
            else
              if(DESC(imatch)(15:18).EQ.'OPAQ')then
                rsotf='OPAQUE'
              else
                write(rsotf,'(A)') DESC(imatch)(21:32)
                willneedtmc=.true.
              endif
            endif
            rsuse1='DOOR'
            rsuse2='CLOSED'
            write(rsparent,'(a)') sname(icomp,isurf)
            call INSREC(0,iuout,ICOMP,isurf,act,XOC,ZOC,XW,ZH,
     &        rsname,rsotf,rsmlcn,rsuse1,rsuse2,rsparent,IER)

C Connection information needs to be updated.
            ibcs=IZSTOCN(icomp,newsurf)
            ICT(ibcs)=boundarytype(newsurf,1)  ! used passed in boundarytype
            IC2(ibcs)=boundarytype(newsurf,2)
            IE2(ibcs)=boundarytype(newsurf,3)

C Use boundarytype to instanciate sother & ssother << to be done >>
            IF(boundarytype(newsurf,1).EQ.0)THEN
              SOTHER(newsurf,1)='EXTERIOR'
            ELSEIF(boundarytype(newsurf,1).EQ.1)THEN
              SOTHER(newsurf,1)='SIMILAR'
            ELSEIF(boundarytype(newsurf,1).EQ.2)THEN
              SOTHER(newsurf,1)='CONSTANT'
            ELSEIF(boundarytype(newsurf,1).EQ.3)THEN
              SOTHER(newsurf,1)='ANOTHER'
            ELSEIF(boundarytype(newsurf,1).EQ.4)THEN
              SOTHER(newsurf,1)='GROUND'
            ELSEIF(boundarytype(newsurf,1).EQ.5)THEN
              SOTHER(newsurf,1)='ADIABATIC'
            ELSEIF(boundarytype(newsurf,1).EQ.6)THEN
              SOTHER(newsurf,1)='BASESIMP'
            ELSEIF(boundarytype(newsurf,1).EQ.7)THEN
              SOTHER(newsurf,1)='IDENT_CEN'
            ELSE

C Reset to represent UNKNOWN
              SOTHER(newsurf,1)='UNKNOWN'
            ENDIF
            write(SOTHER(newsurf,2),'(i2.2)') boundarytype(newsurf,2)
            write(SOTHER(newsurf,3),'(i2.2)') boundarytype(newsurf,3)
            SSOTHER(ibcs,1)=SOTHER(newsurf,1)
            SSOTHER(ibcs,2)=SOTHER(newsurf,2)
            SSOTHER(ibcs,3)=SOTHER(newsurf,3)

            call geowrite(IFIL+2,LGEOM(ICOMP),ICOMP,iuout,3,IER)
          endif
  78    continue
      endif

C Next, if there are windows make standard calls to insert them into
C the parent surface as a percentage of area.
      if(nbglz.gt.0)then
        do 77 ids=1,NZSUR(ICOMP)
          if(hasglaze(ids).gt.0.0)then
            call georead(IFIL+2,LGEOM(ICOMP),ICOMP,1,0,iuout,IER)
            newsurf=nsur+1
            isurf=ids
            act='sp'
            XOC=hasglaze(ids)
            ZOC=0.0
            XW=0.0
            ZH=0.0
            write(rsname,'(a)') hasname(newsurf)
            write(rsmlcn,'(a)') hasconstruction(newsurf)

C Loop thru all of the constructions to find the matching construction
C and then see if it is opaque or transparent and copy out the relevant
C section of the string DESC.
            imatch=0
            do 55 ii=1,nmlc
              if(rsmlcn(1:12).eq.DESC(ii)(1:12)) then
                imatch=ii          
              endif
 55         continue
            if(imatch.eq.0) then
              rsotf='UNKNOWN'
            else
              if(DESC(imatch)(15:18).EQ.'OPAQ')then
                rsotf='OPAQUE'
              else
                write(rsotf,'(A)') DESC(imatch)(21:32)
                willneedtmc=.true.
              endif
            endif
            rsuse1='C-WINDOW'
            rsuse2='CLOSED'
            write(rsparent,'(a)') sname(icomp,isurf)
            call INSREC(0,iuout,ICOMP,isurf,act,XOC,ZOC,XW,ZH,
     &        rsname,rsotf,rsmlcn,rsuse1,rsuse2,rsparent,IER)

C Connection information needs to be updated.
            ibcs=IZSTOCN(icomp,newsurf)
            ICT(ibcs)=boundarytype(newsurf,1)  ! used passed in boundarytype
            IC2(ibcs)=boundarytype(newsurf,2)
            IE2(ibcs)=boundarytype(newsurf,3)

C Use boundarytype to instanciate sother & ssother << to be done >>
            IF(boundarytype(newsurf,1).EQ.0)THEN
              SOTHER(newsurf,1)='EXTERIOR'
            ELSEIF(boundarytype(newsurf,1).EQ.1)THEN
              SOTHER(newsurf,1)='SIMILAR'
            ELSEIF(boundarytype(newsurf,1).EQ.2)THEN
              SOTHER(newsurf,1)='CONSTANT'
            ELSEIF(boundarytype(newsurf,1).EQ.3)THEN
              SOTHER(newsurf,1)='ANOTHER'
            ELSEIF(boundarytype(newsurf,1).EQ.4)THEN
              SOTHER(newsurf,1)='GROUND'
            ELSEIF(boundarytype(newsurf,1).EQ.5)THEN
              SOTHER(newsurf,1)='ADIABATIC'
            ELSEIF(boundarytype(newsurf,1).EQ.6)THEN
              SOTHER(newsurf,1)='BASESIMP'
            ELSEIF(boundarytype(newsurf,1).EQ.7)THEN
              SOTHER(newsurf,1)='IDENT_CEN'
            ELSE

C Reset to represent UNKNOWN
              SOTHER(newsurf,1)='UNKNOWN'
            ENDIF
            write(SOTHER(newsurf,2),'(i2.2)') boundarytype(newsurf,2)
            write(SOTHER(newsurf,3),'(i2.2)') boundarytype(newsurf,3)
            SSOTHER(ibcs,1)=SOTHER(newsurf,1)
            SSOTHER(ibcs,2)=SOTHER(newsurf,2)
            SSOTHER(ibcs,3)=SOTHER(newsurf,3)

            call geowrite(IFIL+2,LGEOM(ICOMP),ICOMP,iuout,3,IER)
          endif
  77    continue
      endif

C Next, if there are mass pairs to add make standard calls to insert them.
      if(nbmass.gt.0)then
        do 79 imu=1,nbmass
          call eclose(datamass(imu,1),90.0,0.01,closev)
          write(SN,'(a)') textmass(imu,1)(1:lnblnk(textmass(imu,1)))
          write(constr,'(a)') textmass(imu,2)(1:lnblnk(textmass(imu,2)))
          write(opt,'(a)') textmass(imu,3)(1:lnblnk(textmass(imu,3)))
          if(closev)then
            call addmass(1,ICOMP,'VM','s',datamass(imu,2),
     &        datamass(imu,3),datamass(imu,4),datamass(imu,7),
     &        datamass(imu,5),datamass(imu,6),SN,constr,OPT,INVT)
          else
            call addmass(1,ICOMP,'HM','s',datamass(imu,2),
     &        datamass(imu,3),datamass(imu,4),datamass(imu,7),
     &        datamass(imu,5),datamass(imu,6),SN,constr,OPT,INVT)
          endif

          call geowrite(IFIL+2,LGEOM(ICOMP),ICOMP,iuout,3,IER)
  79    continue
      endif
      CALL EMKCFG('-',IER)

C If no errors then proceed to create constructions files. The
C file name is already known, assume that attribution is complete
C so write the header of construction file so something exists and then
C call edcon to complete the process. If the file already exists
C it should be overwritten.
      IUF=IFIL+2
      LTHRM(ICOMP)=CFILE
      CALL EFOPSEQ(IUF,LTHRM(ICOMP),3,IER)
      WRITE(IUF,31,IOSTAT=IOS,ERR=146)
     &  zname(ICOMP)(1:lnzname(ICOMP)),CFILE(1:lnblnk(CFILE))
  31  FORMAT('# thermophysical properties of ',a,' defined in ',a,/,
     &'# no of |air |surface(from geo)| multilayer construction',/,
     &'# layers|gaps|  no.  name      | database name ')
      CALL ERPFREE(IUF,ISTAT)

      if(willneedtmc)then
        ITW(icomp)=1
        LTWIN(ICOMP)=TFILE
      endif
      QUIET=.true.
      CALL EDCON(0,iuout,ICOMP,QUIET,IER)  ! should auto create files.
      if(ier.ne.0)then 
        call usrmsg('Problem autogenerating zone construction',
     &    'files. Please check','W')
      endif

C Operational details for this zone need to be handled. One
C option is to rely on a file within the pattern folder for
C the initial description (usefile) and use(3) parameters.
      if(use(1)(1:7).eq.'pattern')then
        LPROJ(ICOMP)=OFILE
        call isunix(unixok)
        if(unixok)then
          fs = char(47)
        else
          fs = char(92)
        endif
        write(subpath,'(6a)',iostat=ios,err=1)
     &    dinstpath(1:lnblnk(dinstpath)),fs,'training',fs,'pattern',fs
        write(sfile,'(2a)')subpath(1:lnblnk(subpath)),
     &    usefile(1:lnblnk(usefile))
C Debug.
C        write(6,*) 'pattern file is ',sfile(1:lnblnk(sfile))

C Clear commons.
        DO 401 IDTY=1,NBDAYTYPE
          NAC(IDTY)=0
          NCAS(IDTY)=0
 401    CONTINUE
        ctlstr(icomp,1)='no control of air flow  '
        ctlstr(icomp,2)='no control of air flow  '
        ctlstr(icomp,3)='no control of air flow  '
        oprdesc(icomp)='no operations notes (yet)'
        lodlabel(icomp,1)='Occupt'
        lodlabel(icomp,2)='Lights'
        lodlabel(icomp,3)='Equipt'  
        lodlabel(icomp,4)='N/A   '  
        lodlabel(icomp,5)='Ann.El'  
        lodlabel(icomp,6)='N/A   ' 
        lodlabel(icomp,7)='N/A   '      

C Fill common blocks for new zone based on information in the
C pattern file.

C Set to current format
        ip3ver(icomp)=2
        afact = use(2)
        iflow='ach-ach '
        vflow='ach-ach '
        isource=0
        ifullysilent=1
        idest=icomp
        Vldtng=.true.
        call COPYAIRFL(isource,idest,afact,iflow,vflow,sfile,
     &    itrc,'s',ier)

        isource=0
        idest=icomp
        icact = use(3)
        iwatt='W-W     '
        itrc=1
        call IMPORTZCASG(isource,idest,icact,iwatt,sfile,itrc,'s',ier)

C Just in case sort the data.
        call PROCESSOLDCAS(ICOMP,0,iuout,IER)
        IUO=IFIL+2
        CALL EMKOPER(IUO,LPROJ(ICOMP),ICOMP,ITRU,IER)
      endif

C Update configuration file to know about the construction files.
      CALL EMKCFG('-',IER)

      RETURN

  146 if(IOS.eq.2)then
        CALL USRMSG(' No permission to write constructions',' ','W')
      else
        CALL USRMSG(' File write error in constructions',' ','W')
      endif
      return

C Errors when creating string buffers.
   1  write(6,*) 'error creating subpath: ',subpath
      ier=2
      return

      END
 
C ******************** SILENTREAD 
C SILENTREAD reads compact zones data from a file. Data input is
C based on 3rd party generators. If act is '?' then return a
C menu and descriptive text block for the file. Otherwise scan
C and act on the information within the file.
 
       SUBROUTINE SILENTREAD(IUNIT,LFILE,act,silentmenu,silentdoc,
     &   silentreturndirec,IER)
       integer MSZ   ! number of silent zones array sizes
       PARAMETER (MSZ=10) 
#include "building.h"
#include "esprdbfile.h"
#include "espriou.h"
C espriou.h provides currentfile.
#include "control.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      COMMON/C4/XLAT,XLON
      COMMON/FILEP/IFIL

C GREF    - External ground reflectivity.
C GREF12  - (12) Monthly values of ground reflectivity
C ITGREF  - Type of ground reflectivity model
C           1= constant albedo
C           2= simple model (monthly albedo, number of days with snow on ground
C           3= advanced model (monthly albedo, snow depth read from file)
C SREF    - Snow reflectivity
C NSNOW   - (12) Monthly values of number of days with snow on ground
C SNFNAM  - *72 Name of the file containing hourly snow depth information
      COMMON/C5/IXPOS,GREF,ITGREF,GREF12(12),SREF,NSNOW(12),SNFNAM
      CHARACTER*72 SNFNAM
      COMMON/C5R/SKYR,GRDR,BLDR
      COMMON/C14/UGRDTP(12,MGRDP),NGRDP
      COMMON/PREC8/SLAT,SLON
      COMMON/INDICS/IVF(MCOM),ISI(MCOM),IHC(MCOM),
     &              ITW(MCOM),ICGC(MCOM),IOBS(MCOM)

C NB number of obstruction blocks.
C X0 Y0 Z0 coordinates of each block origin.
C DX DY DZ width depth and height of each block
C BANG block rotation angle.
      COMMON/GS5/NB,XO(MB),YO(MB),ZO(MB),DX(MB),DY(MB),DZ(MB),BANG(MB)

C NOX NOZ gridding resolution of surfaces for shading
C NGX NGZ not used.
C BLKNAME (8 char) name of obstruction block.
C BLKMAT (12 char) obstruction block construction attribute (for Radiance).
      COMMON/GS6/NOX,NOZ,NGX,NGZ,BLKNAME(MB),BLKMAT(MB)
      character BLKNAME*8,BLKMAT*12

      common/rpath/path
      character*72 path,odir    ! where are folders

      common/dllerr/dllsubr,dllmesg
      character dllsubr*12,dllmesg*124  ! messages if errors in dll

C Simulation parameter sets.
      common/spfldat/nsset,isset,isstup,isbnstep,ispnstep,issave,isavgh

C IPV description.
      common/IPVSEA/nipvassmt,nipvdispjd,ipvastjd(MIPVA),
     &              ipvafnjd(MIPVA),ipvdispjd(10)

C Season definitions (from climatelist file). 2 periods for winter
C (i.e. nov-dec and jan-feb), transition (i.e. mar-may & sep-oct)
C and one period for summer.
      common/typsea/is1wins,is1winf,is2wins,is2winf,is1sprs,is1sprf,
     &              is2sprs,is2sprf,is1sums,is1sumf

C Typical periods win1|trn1|sum|trn2|win2 with start,finish
      common/typper/ia1wins,ia1winf,ia1sprs,ia1sprf,
     &  iasums,iasumf,ia2sprs,ia2sprf,ia2wins,ia2winf

C Control documentation common
      common/ctlnm/znctldoc,plctldoc,flctldoc,elctldoc,glctldoc,
     &             opticdoc
      character znctldoc*248,plctldoc*248,flctldoc*248,elctldoc*248
      character glctldoc*248,opticdoc*248     
  
      common/cctlnm/ctldoc,lctlf
      character LCTLF*72,CTLDOC*248  
      
      common/cctl/icascf(mcom)
      integer icascf

      common/paths/zonepth,netpth,ctlpth,imgpth,radpth,docpth,tmppth,
     &             dbspth
      common/C21/IFCFG,cfgroot,LCFGF

C XMLMETRICS are the metrics that could be included in the input.xml
C in case the flag: <dump_all_data> is false
      common/XMLDATA/XMLMETRICS(200),inumXMLmetrics
      character XMLMETRICS*248
      integer inumXMLmetrics !counts for the number of xml metrics defined
      character zonepth*24,netpth*24,ctlpth*24,imgpth*24,radpth*24
      character docpth*24,tmppth*24,dbspth*24,cfgroot*24,LCFGF*72
      integer IFCFG
               
      CHARACTER OUTSTR*248,WORD*32,outs*124
      character dstmp*24
      character word248*248

      character act*1  ! action requested.
      character ipvact*1 ! action for clearipvdat
      character simact*6 ! action for creating assessments.
      character subpath*72

C At the head of the file is a menu string and a description
C silentmenu (char 32) and silentdoc (char 248) which can be
C used by scanning code to identify the contents of a silent file.
      character silentmenu*32,silentdoc*248
      character weather*32     ! file name for the location (no path)
      character*(*) lfile      ! name of the file
      character fs*1           ! file separator

C For passing to silentmodel.
      character actions*8,root*24,mpath*72,folder*12

C Same as local variable actions which is passed back to calling code.
      character silentreturndirec*8

      logical dll,unixok,XST,ok
      real valx,valy      ! local variable for extrude co-ords.

C Paremeters passed to:silentzone
      character name*12   ! name given to the zone
      dimension name(MSZ)
      character shape*8   ! box or extrude or poly
      dimension shape(MSZ)
      character use*8,szuse*8,usefile*32  ! usage pattern directives
      dimension use(3),szuse(MSZ,3),usefile(MSZ)
      integer nbwalls(MSZ)     ! nb of vertical walls
      integer nbmass(MSZ)      ! nb of paired mass rectangles
      integer nbedges(MS) ! nb of edges per surface in poly shaped zone
      integer isznbedges(MSZ,MS) ! nb of edges per surface in poly shaped zone
      integer list(MS,24) ! indices of coords making up edges of each surface
      integer iszlist(MSZ,MS,24) ! indices of coords making up edges of each surface
      integer nbglz(MSZ)       ! nb of windows
      integer nbdoor(MSZ)      ! nb of doors
      integer nbcord(MSZ)      ! nb of coordinates (for extruded or poly shape)
      real rotateit(MSZ)        ! angle of rotation
      real origin(3),szorigin(MSZ,3)      ! values dependant on shape
      real datamass(4,7),sdatamass(MSZ,4,7) ! origin and size and orientations of mass (4 per zone)
      real size(3),szsize(MSZ,3)        ! values dependant on shape
      real coords(MS,3),szcoords(MSZ,MS,3) 
      real hasglaze(MS),szhasglaze(MSZ,MS)   ! percentage of glazing in each surface of intial shape
      real hasdoor(MS),szhasdoor(MSZ,MS)    ! width of door in each surface of the initial shape

      integer nbob  ! nbob number of obstruction blocks.
      real XOB,YOB,ZOB  ! coordinates of each block origin.
      real DXOB,DYOB,DZOB  ! width depth and height of each block
      dimension nbob(MSZ),XOB(MSZ,MB),YOB(MSZ,MB),ZOB(MSZ,MB)
      dimension DXOB(MSZ,MB),DYOB(MSZ,MB),DZOB(MSZ,MB),BANGOB(MSZ,MB)

C BLKNAME (8 char) name of obstruction block.
C BLKMAT (12 char) obstruction block construction attribute (for Radiance).
      character BLOCKNAME*8,BLOCKMAT*12
      dimension BLOCKNAME(MSZ,MB),BLOCKMAT(MSZ,MB)

C Stings associated with internal mass. 1=surface name, 2=construction, 3=optics
      character textmass*32,stextmass*32
      dimension textmass(4,3),stextmass(MSZ,4,3)

      character hasconstruction*32,szhasconstruction*32
      dimension hasconstruction(MS),szhasconstruction(MSZ,MS)  ! construction to use for each surface
      character hasname*12,szhasname*12
      dimension hasname(MS),szhasname(MSZ,MS)  ! name to use for each surface
      integer boundarytype(MS,3),szboundarytype(MSZ,MS,3)

C Site related local variables.
      real sitelat,sitelong  ! latitude and longitude difference
      logical havesite       ! true if tokens included
      real groundreflanual   ! ground reflectance (single value)
      logical havegroundreflanual ! true if ground reflectance included
      real groundreflmonth(12) ! ground reflectance (monthly vlues)
      integer groundreflmodel  ! index to ground reflectance model
      real snowrefl            ! snow reflectivity
      integer dayswithsnow(12) ! days with snow in each month
      logical havesnowdays     ! true if days with snow defined
      integer siteexposureindex  ! same as ixpos
      character hourlysnowfile*72  ! same as SNFNAM
      logical havehourlysnowfile   ! true if set
      real hoursGTM   ! hours before or after GTM 
      real skyview,groundview,buildingview ! user defined viewfactors
      real groundtemps(12,MGRDP)
      real GVA(12)  ! temporary array to hold a dozen values.
      integer groundtempsets
      logical havegroundtempsets
      logical havesimparameters
      integer simstartup   ! use for isstup
      integer simzonetimestep ! use for isbnstep
      integer simplanttimestep ! use for ispnstep
      integer simsavelevel  ! use for issave
      integer i_ctl_link  !flag to indicate the zone has a basic control
      real ht_Setpoint,cl_Setpoint !heating and cooling set-points
      dimension i_ctl_link(MCOM),ht_Setpoint(MCOM),cl_Setpoint(MCOM)
      integer isilentncf !silent decide how many control functions needed
        
      IER=0

C Determine operating system
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

C Set icomp to zero if there are no zones in the model.
      if(ncomp.eq.0)then
        icomp=0
      endif

C Get the current folder and display options to the user.
C first list any files with .cfg in name.
      odir=' '
      call usrdir(odir)

C If subpath is less than 72 char with odir + path use that
C otherwise just use path.
C Debug...
C      write(6,*) 'usrdir is ',odir
C      write(6,*) 'path is ',path
      subpath=' '
      lnodir=lnblnk(odir)
      lnpath=lnblnk(path)
      if(lnodir+lnpath+1.le.72)then
        write(subpath,'(3a)') odir(1:lnodir),fs,
     &  path(1:lnpath)
      else
        write(subpath,'(a)') path(1:lnpath)
      endif
      write(outs,'(2a)') 'configuration files in ',
     &  subpath(1:lnblnk(subpath))
C      write(6,*) outs(1:lnblnk(outs))

C Clear the sz array structures. The logic is to scan all of the
C zone details and then process the zones after *end is detected
C in the file.
      if(act(1:1).ne.'?')then
        actions=' '
        root=' '
        folder=' '
        mpath=' '
        silentmenu=' '
        silentdoc=' '
        weather=' '
        hourlysnowfile=' '
        groundreflmodel=0
        skyview=0.0
        groundview=0.0
        buildingview=0.0
        havesite=.false.
        havegroundreflanual=.false.
        havesnowdays=.false.
        havehourlysnowfile=.false.
        havegroundtempsets=.false.
        groundtempsets=0
        havesimparameters=.false.
        simstartup=0
        simzonetimestep=0
        simplanttimestep=0
        simsavelevel=0
        isilentncf=0
        do 42 iz=1,MSZ
          name(iz) = ' '
          shape(iz) = ' '
          szuse(iz,1) = ' '
          szuse(iz,2) = ' '
          szuse(iz,3) = ' '
          usefile(iz) = ' '
          nbwalls(iz) = 0
          nbmass(iz) = 0
          nbglz(iz) = 0
          nbdoor(iz) = 0
          nbcord(iz) = 0
          rotateit(iz) = 0
          szorigin(iz,1) = 0.0
          szorigin(iz,2) = 0.0
          szorigin(iz,3) = 0.0
          szsize(iz,1) = 0.0
          szsize(iz,2) = 0.0
          szsize(iz,3) = 0.0
          nbob(iz) = 0
          ht_Setpoint(iz)= 0.0
          cl_Setpoint(iz)= 0.0
          i_ctl_link(iz)=0
          do 55 ibu=1,MB
            XOB(iz,ibu) = 0.0
            YOB(iz,ibu) = 0.0
            ZOB(iz,ibu) = 0.0
            DXOB(iz,ibu) = 0.0
            DYOB(iz,ibu) = 0.0
            DZOB(iz,ibu) = 0.0
            BANGOB(iz,ibu) = 0.0
            BLOCKNAME(iz,ibu) = ' '
            BLOCKMAT(iz,ibu) = ' '
  55      continue
          do 52 isu=1,MS
            isznbedges(iz,isu)=0
            do 54 ivu=1,MV
              iszlist(iz,isu,ivu)=0
  54        continue
            szcoords(iz,isu,1)=0.0
            szcoords(iz,isu,2)=0.0
            szcoords(iz,isu,3)=0.0
            szhasglaze(iz,isu)=0.0
            szhasdoor(iz,isu)=0.0
            szhasconstruction(iz,isu)=' '
            szhasname(iz,isu)=' '
            szboundarytype(iz,isu,1)=0
            szboundarytype(iz,isu,2)=0
            szboundarytype(iz,isu,3)=0
  52      continue
          do 59 imu=1,4
            sdatamass(iz,imu,1)= 0.0
            sdatamass(iz,imu,2)= 0.0
            sdatamass(iz,imu,3)= 0.0
            sdatamass(iz,imu,4)= 0.0
            sdatamass(iz,imu,5)= 0.0
            sdatamass(iz,imu,6)= 0.0
            sdatamass(iz,imu,7)= 0.0
            stextmass(iz,imu,1)= ' '
            stextmass(iz,imu,2)= ' '
            stextmass(iz,imu,3)= ' '
  59      continue
  42    continue
      endif

      NS=0     ! temporary array for counting surfaces.
      nsz=0
      weather='none'

C Check if running in dll mode.
      call isadll(dll)

C Initialise geometry data file. and set currentfile.
      CALL EFOPSEQ(IUNIT,LFILE,1,IER)
      IF(IER.LT.0)THEN
        write(outs,'(3a)') 'Silent file ',LFILE(1:lnblnk(LFILE)),
     &      ' could not be opened.'
        if(dll)then
          dllsubr='SILENTREAD'
          dllmesg=outs
          ier=2
          return
        else
          call edisp(iuout,outs)
          IER=1
          RETURN
        endif
      ENDIF
      write(currentfile,'(a)') LFILE(1:lnblnk(LFILE))

C Read header lines from file, the 1.1 version looks like: 
C *silent_input
      CALL LSTRIPC(IUNIT,OUTSTR,99,ND,1,'silent line 1',IER)
      IF(IER.NE.0)goto 1001
      if(OUTSTR(1:13).eq.'*silent_input')then
        continue
      else

C If we have reached this position then it is an older geometry
C file so close the file and set error message.
        CALL ERPFREE(IUNIT,ios)
        ier=2
        call usrmsg('Not a silent instruction file.',LFILE,'W')
        return
      endif

C Read data line. If end of file found then jump
C to 1000 to see if there are zones to process.
  43  CALL LSTRIPC(IUNIT,OUTSTR,99,ND,1,'silent tags',IER)
      IF(IER.NE.0)goto 1000
      K=0
      CALL EGETW(OUTSTR,K,WORD,'W','*vertex or *surface tags',IER)
      if(WORD(1:5).eq.'*Date'.or.WORD(1:5).eq.'*date')then
        CALL EGETRM(OUTSTR,K,dstmp,'W','date stamp',IER)

      elseif(WORD(1:5).eq.'*menu')then
        CALL EGETRM(OUTSTR,K,silentmenu,'W','silent menu',IER)
        goto 43
      elseif(WORD(1:4).eq.'*doc')then
        CALL EGETRM(OUTSTR,K,silentdoc,'W','silent documentation',IER)
        if(act(1:1).eq.'?')then
          return
        else
          goto 43
        endif
      elseif(WORD(1:7).eq.'*action')then

C An action line is like this:
C *action,new,boxer,single,/tmp/boxer
        CALL EGETW(OUTSTR,K,actions,'W','action string',IER)
        silentreturndirec=actions
        if(actions(1:3).eq.'new')then
          CALL EGETW(OUTSTR,K,root,'W','root name',IER)
          CALL EGETW(OUTSTR,K,folder,'W','folder directive',IER)
          CALL EGETRM(OUTSTR,K,mpath,'W','model folder',IER)

C Debug.
C          write(6,*)  's action ',actions,' ',root,' ',folder
C          write(6,*)  's folder ',folder
C          write(6,*)  's mpath ',mpath
        elseif(actions(1:4).eq.'cont')then
          continue
        endif
        goto 43
      elseif(WORD(1:11).eq.'*start_zone')then

C If this is a new model increment icomp. If the model already has
C zones then set icomp to the next available zone.
        if(ncomp.eq.0)then
          icomp=icomp+1      ! new model, ncomp not yet set
        else
          if(icomp.eq.0)then
            icomp=ncomp+1    ! increment existing zone index
          elseif(icomp.gt.0)then
            icomp=icomp+1    ! increment index for subsequent zones
          endif
        endif
        nsz=nsz+1
        CALL EGETW(OUTSTR,K,WORD,'W','Z name',IFLAG)
        write(name(nsz),'(a)') WORD(1:12)
        goto 43
      elseif(WORD(1:9).eq.'*end_zone')then
        goto 43
      elseif(WORD(1:5).eq.'*end ')then

C Jump to process the collected data.
        goto 1000
      elseif(WORD(1:5).eq.'*cord')then
        nbcord(nsz)=nbcord(nsz)+1
        nbc=nbcord(nsz)
        CALL EGETWR(OUTSTR,K,valx,0.,0.,'-','cord-x',IER)
        CALL EGETWR(OUTSTR,K,valy,0.,0.,'-','cord-y',IER)
        szcoords(nsz,nbc,1)=valx
        szcoords(nsz,nbc,2)=valy
        if(shape(nsz)(1:4).eq.'poly')then
          CALL EGETWR(OUTSTR,K,valz,0.,0.,'-','cord-z',IER)
          szcoords(nsz,nbc,3)=valz
        else
          szcoords(nsz,nbc,3)=0.0
        endif
        goto 43
      elseif(WORD(1:5).eq.'*door')then
        CALL EGETWI(OUTSTR,K,nbd,0,MS,'W','no of doors',IER)

C Scan for pairs of parent surface index and the door width.
        nbdoor(nsz)=nbd
        if(nbdoor(nsz).gt.0)then
          do 47 igl=1,nbdoor(nsz)
            CALL EGETWI(OUTSTR,K,iparen,1,MS,'W','parent surf',IER)
            CALL EGETWR(OUTSTR,K,val,0.,0.,'-','width',IER)
            szhasdoor(nsz,iparen)=val
  47      continue
        endif
        goto 43
      elseif(WORD(1:6).eq.'*glaze')then
        CALL EGETWI(OUTSTR,K,nbg,0,MS,'W','no of glzaings',IER)

C Scan for pairs of parent surface index and the percentage of the parent
C surface area to have as glazing.
        nbglz(nsz)=nbg
        if(nbglz(nsz).gt.0)then
          do 144 igl=1,nbglz(nsz)
            CALL EGETWI(OUTSTR,K,iparen,1,MS,'W','parent surf',IER)
            CALL EGETWR(OUTSTR,K,val,0.,0.,'-','percent',IER)
            szhasglaze(nsz,iparen)=val
 144      continue
        endif
        goto 43
      elseif(WORD(1:6).eq.'*shape')then
        CALL EGETW(OUTSTR,K,WORD,'W','shape type',IFLAG)
        if(WORD(1:3).eq.'box')then
          write(shape(nsz),'(a)') 'box '
          nbwalls(nsz)=6
        elseif(WORD(1:7).eq.'extrude')then
          write(shape(nsz),'(a)') 'extrude'
        elseif(WORD(1:4).eq.'poly')then
          write(shape(nsz),'(a)') 'poly'
        endif
        NS=0     ! reset number of surfaces and clear arrays.
        goto 43
      elseif(WORD(1:8).eq.'*nbwalls')then

C Number of parent surfaces has a meaning for extruded shapes and set to 6 for
C box shapes.
        if(shape(nsz)(1:7).eq.'extrude')then
          CALL EGETWI(OUTSTR,K,nbw,1,MS,'W','nb parent surfs',IER)
          nbwalls(nsz)=nbw
        elseif(shape(nsz)(1:4).eq.'poly')then
          CALL EGETWI(OUTSTR,K,nbw,1,MS,'W','nb parent surfs',IER)
          nbwalls(nsz)=nbw
        else
          nbwalls(nsz)=4
        endif
        goto 43
      elseif(WORD(1:7).eq.'*origin')then
        if(shape(nsz)(1:3).eq.'box')then
          CALL EGETWR(OUTSTR,K,origin(1),0.,0.,'-','box X origin',IER)
          CALL EGETWR(OUTSTR,K,origin(2),0.,0.,'-','box Y origin',IER)
          CALL EGETWR(OUTSTR,K,origin(3),0.,0.,'-','box Z origin',IER)
          szorigin(nsz,1)=origin(1)
          szorigin(nsz,2)=origin(2)
          szorigin(nsz,3)=origin(3)
        elseif(shape(nsz)(1:7).eq.'extrude')then
          CALL EGETWR(OUTSTR,K,origin(1),0.,0.,'-','extrude Z base',IER)
          CALL EGETWR(OUTSTR,K,origin(2),0.,0.,'-','extrude Z top',IER)
          CALL EGETWR(OUTSTR,K,origin(3),0.,0.,'-','extrude ignore',IER)
          szorigin(nsz,1)=origin(1)
          szorigin(nsz,2)=origin(2)
          szorigin(nsz,3)=origin(3)
        elseif(shape(nsz)(1:4).eq.'poly')then
          szorigin(nsz,1)=0.0
          szorigin(nsz,2)=0.0
          szorigin(nsz,3)=0.0
        endif
        goto 43
      elseif(WORD(1:7).eq.'*rotate')then
        CALL EGETWR(OUTSTR,K,rot,0.,0.,'-','rotation angle',IER)
        rotateit(nsz)=rot
        goto 43
      elseif(WORD(1:5).eq.'*size')then
        if(shape(nsz)(1:3).eq.'box')then
          CALL EGETWR(OUTSTR,K,size(1),0.,0.,'-','box length',IER)
          CALL EGETWR(OUTSTR,K,size(2),0.,0.,'-','box width',IER)
          CALL EGETWR(OUTSTR,K,size(3),0.,0.,'-','box height',IER)
          szsize(nsz,1)=size(1)
          szsize(nsz,2)=size(2)
          szsize(nsz,3)=size(3)
        elseif(shape(nsz)(1:7).eq.'extrude')then
          szsize(nsz,1)=0.0   ! this is ignored for extruded shape
          szsize(nsz,2)=0.0
          szsize(nsz,3)=0.0
        elseif(shape(nsz)(1:4).eq.'poly')then
          szsize(nsz,1)=0.0   ! this is ignored for extruded shape
          szsize(nsz,2)=0.0
          szsize(nsz,3)=0.0
        endif
        goto 43
      elseif(WORD(1:8).eq.'*surface')then

C Expected order is the walls first then the top then the base and
C then any windows and then any doors.
        NS=NS+1

C Surface name, allow for future spaces in name.
        CALL EGETP(OUTSTR,K,WORD,'W','surface name',IER)
        write(szhasname(nsz,NS),'(a)') WORD(1:lnblnk(WORD))

C Surface construction name, allow for spaces.
        CALL EGETP(OUTSTR,K,WORD,'W','surface construction',IER)
        write(szhasconstruction(nsz,NS),'(a)') WORD(1:lnblnk(WORD))

C Read three indices representing boundary condition.
        CALL EGETWI(OUTSTR,K,ival1,-1,4,'W','connection type',IER)
        CALL EGETWI(OUTSTR,K,ival2,0,MCOM,'W','connection ic2',IER)
        CALL EGETWI(OUTSTR,K,ival3,0,MS,'W','connection ie2',IER)
        szboundarytype(nsz,NS,1)=ival1
        szboundarytype(nsz,NS,2)=ival2
        szboundarytype(nsz,NS,3)=ival3

C Note: if shape=poly for the each of the declaired walls the
C following line should start with the token *list.
        goto 43
      elseif(WORD(1:5).eq.'*list')then

C The current value of NS should still point to the surface data
C that was just scanned. The *list line holds the number of
C edges and then the index of each coordinate (as in the normal
C geometry file).
        CALL EGETWI(OUTSTR,K,ival1,3,MV,'W','nb of edges',IER)
        isznbedges(nsz,NS)=ival1

C Now proceed to read vertices on one or more lines.
        DO 12 KV=1,ival1
          CALL EGETWI(OUTSTR,K,ival2,0,MTV,'F','vertex',IERV)
          IF(IERV.NE.0) THEN
            call edisp(ITRU,' reading continuation line...')
            CALL STRIPC(IUNIT,OUTSTR,0,ND,0,'vertex XYZ',IER)
            K=0
            CALL EGETWI(OUTSTR,K,ival2,0,MTV,'F','vertex',IERV)
          ENDIF
          iszlist(nsz,NS,KV)=ival2
   12   CONTINUE
        goto 43
      elseif(WORD(1:4).eq.'*obs')then

C An obstruction block to be associated with the current zone.
        nbob(nsz)=nbob(nsz)+1
        nob=nbob(nsz)
        CALL EGETWR(OUTSTR,K,val1,-999.,998.,'W','X org',IER)
        XOB(nsz,nob)=val1
        CALL EGETWR(OUTSTR,K,val1,-999.,998.,'W','Y org',IER)
        YOB(nsz,nob)=val1
        CALL EGETWR(OUTSTR,K,val1, -99., 99.,'W','Z org',IER)
        ZOB(nsz,nob)=val1
        CALL EGETWR(OUTSTR,K,val1,   0., 99.,'W','X dis',IER)
        DXOB(nsz,nob)=val1
        CALL EGETWR(OUTSTR,K,val1,   0., 99.,'W','Y dis',IER)
        DYOB(nsz,nob)=val1
        CALL EGETWR(OUTSTR,K,val1,   0., 99.,'W','Z dis',IER)
        DZOB(nsz,nob)=val1
        CALL EGETWR(OUTSTR,K,val1,-359.,359.,'W','rot a',IER)
        BANGOB(nsz,nob)=val1
        CALL EGETW(OUTSTR,K,WORD,'W','obs blk name',IFLAG)
        write(BLOCKNAME(nsz,nob),'(a)') WORD(1:lnblnk(WORD))
        CALL EGETW(OUTSTR,K,WORD,'W','obs mat name',IFLAG)
        write(BLOCKMAT(nsz,nob),'(a)') WORD(1:lnblnk(WORD))
        goto 43

      elseif(WORD(1:5).eq.'*mass')then

C The *mass keyword is followed by 9 tokens as follows:
        nbmass(nsz)=nbmass(nsz)+1
        nma=nbmass(nsz)
        CALL EGETW(OUTSTR,K,WORD,'W','mass vertical or horiz',IER)
        if(WORD(1:2).eq.'VM'.or.WORD(1:2).eq.'vm')then
          sdatamass(nsz,nma,1)= 90.0  ! signal vertical
        elseif(WORD(1:2).eq.'HM'.or.WORD(1:2).eq.'hm')then
          sdatamass(nsz,nma,1)= 0.0   ! signal horizontal
        endif
        CALL EGETWR(OUTSTR,K,val1,-999.,998.,'W','mass X org',IER)
        sdatamass(nsz,nma,2)=val1
        CALL EGETWR(OUTSTR,K,val1,-999.,998.,'W','mass Y org',IER)
        sdatamass(nsz,nma,3)=val1
        CALL EGETWR(OUTSTR,K,val1, -99., 99.,'W','mass Z org',IER)
        sdatamass(nsz,nma,4)=val1
        CALL EGETWR(OUTSTR,K,val1,   0., 99.,'W','mass X dis',IER)
        sdatamass(nsz,nma,5)=val1
        CALL EGETWR(OUTSTR,K,val1,   0., 99.,'W','mass Z dis',IER)
        sdatamass(nsz,nma,6)=val1
        CALL EGETWR(OUTSTR,K,val1,-359.,359.,'W','mass rotate',IER)
        sdatamass(nsz,nma,7)=val1
        CALL EGETW(OUTSTR,K,WORD,'W','mass name',IFLAG)
        write(stextmass(nsz,nma,1),'(a)') WORD(1:lnblnk(WORD))
        CALL EGETW(OUTSTR,K,WORD,'W','mass constr name',IFLAG)
        write(stextmass(nsz,nma,2),'(a)') WORD(1:lnblnk(WORD))
        write(stextmass(nsz,nma,3),'(a)') 'OPAQUE'   ! set as opaque
        goto 43

      elseif(WORD(1:6).eq.'*usage')then

C After *usage the next token is 'pattern' (tag indicating that
C an operations file in the training/pattern folder to use to
C get infiltration and casual gains). Note usefile is only
C 32 char buffer because other source code knowns the path
C to the training/pattern folder.
        CALL EGETW(OUTSTR,K,use(1),'W','pattern',IFLAG)
        if(use(1)(1:7).eq.'pattern')then
          szuse(nsz,1) = use(1)
          CALL EGETW(OUTSTR,K,use(2),'W','pattern infil',IFLAG)
          szuse(nsz,2) = use(2)
          CALL EGETW(OUTSTR,K,use(3),'W','pattern casual',IFLAG)
          szuse(nsz,3) = use(3)
          CALL EGETRM(OUTSTR,K,WORD,'W','pattern file',IER)
          write(usefile(nsz),'(a)') WORD(1:lnblnk(WORD))
        else
          call usrmsg('Usage 2nd parameter not recognised',' ','W')
        endif
        goto 43
      elseif(WORD(1:9).eq.'*occupant')then
        goto 43
      elseif(WORD(1:9).eq.'*lighting')then
        goto 43
      elseif(WORD(1:10).eq.'*equipment')then
        goto 43
      elseif(WORD(1:12).eq.'*environment')then
        goto 43
      elseif(WORD(1:8).eq.'*heating')then
        CALL EGETWR(OUTSTR,K,ht_Setpoint(nsz),-102.,102.,'W',
     &       'heat setp',IER)
        goto 43
      elseif(WORD(1:8).eq.'*cooling')then
        CALL EGETWR(OUTSTR,K,cl_Setpoint(nsz),-102.,102.,'W',
     &       'cool Setp',IER)
        goto 43
      elseif(WORD(1:14).eq.'*ideal_control')then
        i_ctl_link(nsz)=1
        goto 43
      elseif(WORD(1:16).eq.'*weather_station')then

C If there is a line with the root name of a climate file look
C for a match in the current climate location and if there is
C one set the name of that file, scan it and find its site.
        CALL EGETRM(OUTSTR,K,weather,'W','weather name',IER)
        goto 43
      elseif(WORD(1:5).eq.'*site')then

C The site line includes latitute, longitude difference from time
C meridian, time zone (hours before or after GMT), site exposure
C and if siteexposureindex is 8 then read three more numbers
C for the general exposure to sky, ground and other buildings. 
        CALL EGETWR(OUTSTR,K,sitelat,-90.,90.,'W','Latitude',IER)
        CALL EGETWR(OUTSTR,K,sitelong,-15.,15.,'W','Longitude dif',IER)
        CALL EGETWR(OUTSTR,K,hoursGTM,-12.,12.,'W','Hours +-GMT',IER)
        CALL EGETWI(OUTSTR,K,siteexposureindex,1,8,'W',
     &    'Site exposure',IER)
        if(siteexposureindex.eq.8)then
          CALL EGETWR(OUTSTR,K,skyview,0.,1.,'W','Sky exposure',IER)
          CALL EGETWR(OUTSTR,K,groundview,0.,1.,'W',
     &      'Ground exposure',IER)
          CALL EGETWR(OUTSTR,K,buildingview,0.,1.,'W',
     &      'Building exposure',IER)
        endif
        havesite=.true.
        goto 43
      elseif(WORD(1:19).eq.'*ground_refl_annual')then
        CALL EGETWR(OUTSTR,K,groundreflanual,0.,1.,'W',
     &    'Ground reflectance annual value',IER)
        havegroundreflanual=.true.
        goto 43
      elseif(WORD(1:20).eq.'*ground_refl_monthly')then

C Ground reflectivity model, no-snow monthly albedos, snow albedo
        CALL EGETWI(OUTSTR,K,groundreflmodel,1,3,'F',
     &    'ground refl. model',IER)
        DO 441 I=1,12
          CALL EGETWR(OUTSTR,K,groundreflmonth(I),0.,1.,'W',
     &      'monthly gr. refl.',IER)
  441   CONTINUE
        CALL EGETWR(OUTSTR,K,snowrefl,0.,1.,'W','snow refl.',IER)  
        goto 43
      elseif(WORD(1:18).eq.'*monthly_snow_days')then

C Number of days with snow on the ground (if ground refl model 2)
        DO 442 I=1,12
          CALL EGETWI(OUTSTR,K,dayswithsnow(I),0,31,'W',
     &      'days with snow',IER)
  442   CONTINUE
        havesnowdays=.true.
        goto 43
      elseif(WORD(1:18).eq.'*hourly_snow_depth')then

C If ground refl model is 3 then this file has hourly data.
        CALL EGETRM(OUTSTR,K,hourlysnowfile,'W','snow file',IER)
        havehourlysnowfile=.true.
        goto 43
      elseif(WORD(1:16).eq.'*monthly_profile')then

C Ground monthly temperature profiles. 2nd item is the number of profiles.
C And this is followed by one line each for each profile.
        CALL EGETWI(OUTSTR,K,groundtempsets,1,MGRDP,'F','grn prfls',IER)
        IF(groundtempsets.eq.0) goto 43
        DO 28 IGRDP=1,groundtempsets
          CALL EGETWRA(IUNIT,GVA,12,-9.,99.,'W','Gr tmp',IER)
          DO 29 J=1,12
            groundtemps(J,IGRDP)=GVA(J)
   29     CONTINUE
  28    CONTINUE
        havegroundtempsets=.true.
        goto 43
      elseif(WORD(1:15).eq.'*sim_parameters')then

C Get the assessment independant simulation parameters.
        CALL EGETWI(OUTSTR,K,simstartup,0,300,'W','startup',IER)
        CALL EGETWI(OUTSTR,K,simzonetimestep,1,60,'W','zone_ts',IER)
        CALL EGETWI(OUTSTR,K,simplanttimestep,0,100,'W','plant_ts',IER)
        CALL EGETWI(OUTSTR,K,simsavelevel,0,6,'W','save_lv',IER)
        havesimparameters=.true.
        goto 43
      elseif(WORD(1:12).eq.'*assessments')then

C Get tags related to assessments: after the tag *assessments the
C following choices are possible:
C   `annual` (no further tokens)
C   `single` followed typically by `user` followed by pairs of julian dates
C   `winter` followed by `typical_week` or `season` or 'fortnight'
C   `spring` followed by `typical_week` or `season` or 'fortnight'
C   `summer` followed by `typical_week` or `season` or 'fortnight'
C   `autumn` followed by `typical_week` or `season` or 'fortnight'
C   `three_season` followed by `typical_week` `forthnight` `user` or `season` 
C   `five_season` followed by `typical_week` `forthnight` `user` or `season` 
        CALL EGETW(OUTSTR,K,WORD,'W','*assessment tags',IER)
        if(WORD(1:7).eq.'Default'.or.WORD(1:7).eq.'default')then
          nipvassmt=1
          simsavelevel=-1   ! signal to ipv2simpar to use defaults
          simact='i1d   '
        elseif(WORD(1:6).eq.'Annual'.or.WORD(1:6).eq.'annual')then
          nipvassmt=1
          simact='ias   '
        elseif(WORD(1:6).eq.'Single'.or.WORD(1:6).eq.'single')then
          CALL EGETW(OUTSTR,K,WORD,'W','Single tags',IER)
          if(WORD(1:4).eq.'User'.or.
     &       WORD(1:4).eq.'user')then
            CALL EGETWI(OUTSTR,K,iday1,1,365,'W','start julian',IER)
            CALL EGETWI(OUTSTR,K,iday2,1,365,'W','end julian',IER)
          endif
          nipvassmt=1
          simact='i1d   '
        elseif(WORD(1:6).eq.'Winter'.or.WORD(1:6).eq.'winter')then
          CALL EGETW(OUTSTR,K,WORD,'W','Winter tags',IER)
          if(WORD(1:12).eq.'Typical_week'.or.
     &       WORD(1:12).eq.'typical_week')then
            simact='icwint'
          elseif(WORD(1:9).eq.'Fortnight'.or.
     &           WORD(1:9).eq.'fortnight')then
            simact='icwinf'
          elseif(WORD(1:9).eq.'Season'.or.
     &           WORD(1:9).eq.'season')then
            simact='icwins'
          endif
          nipvassmt=1
        elseif(WORD(1:6).eq.'Spring'.or.WORD(1:6).eq.'spring')then
          CALL EGETW(OUTSTR,K,WORD,'W','Spring tags',IER)
          if(WORD(1:12).eq.'Typical_week'.or.
     &       WORD(1:12).eq.'typical_week')then
            simact='icsprt'
          elseif(WORD(1:9).eq.'Fortnight'.or.
     &           WORD(1:9).eq.'fortnight')then
            simact='icsprf'
          elseif(WORD(1:9).eq.'Season'.or.
     &           WORD(1:9).eq.'season')then
            simact='icsprs'
          endif
          nipvassmt=1
        elseif(WORD(1:6).eq.'Summer'.or.WORD(1:6).eq.'summer')then
          CALL EGETW(OUTSTR,K,WORD,'W','Summer tags',IER)
          if(WORD(1:12).eq.'Typical_week'.or.
     &       WORD(1:12).eq.'typical_week')then
            simact='icsumt'
          elseif(WORD(1:9).eq.'Fortnight'.or.
     &           WORD(1:9).eq.'fortnight')then
            simact='icsumf'
          elseif(WORD(1:9).eq.'Season'.or.
     &           WORD(1:9).eq.'season')then
            simact='icsums'
          endif
          nipvassmt=1
        elseif(WORD(1:6).eq.'Autumn'.or.WORD(1:6).eq.'autumn')then
          CALL EGETW(OUTSTR,K,WORD,'W','Autumn tags',IER)
          if(WORD(1:12).eq.'Typical_week'.or.
     &       WORD(1:12).eq.'typical_week')then
            simact='icautt'
          elseif(WORD(1:9).eq.'Fortnight'.or.
     &           WORD(1:9).eq.'fortnight')then
            simact='icautf'
          elseif(WORD(1:9).eq.'Season'.or.
     &           WORD(1:9).eq.'season')then
            simact='icauts'
          endif
          nipvassmt=1
        elseif(WORD(1:12).eq.'Three_season'.or.
     &         WORD(1:12).eq.'three_season')then
          CALL EGETW(OUTSTR,K,WORD,'W','three_season tags',IER)
          if(WORD(1:12).eq.'Typical_week'.or.
     &       WORD(1:12).eq.'typical_week')then
            simact='i3t   '
          elseif(WORD(1:4).eq.'User'.or.WORD(1:4).eq.'user')then
            CALL EGETWI(OUTSTR,K,iday1,1,365,'W','start julian',IER)
            CALL EGETWI(OUTSTR,K,iday2,1,365,'W','end julian',IER)
            CALL EGETWI(OUTSTR,K,iday3,1,365,'W','start julian',IER)
            CALL EGETWI(OUTSTR,K,iday4,1,365,'W','end julian',IER)
            CALL EGETWI(OUTSTR,K,iday5,1,365,'W','start julian',IER)
            CALL EGETWI(OUTSTR,K,iday6,1,365,'W','end julian',IER)
            simact='i3u   '
          elseif(WORD(1:9).eq.'season'.or.
     &           WORD(1:9).eq.'season')then
            simact='i3s   '
          endif
          nipvassmt=3
        elseif(WORD(1:11).eq.'Five_season'.or.
     &         WORD(1:11).eq.'five_season')then
          CALL EGETW(OUTSTR,K,WORD,'W','five_season tags',IER)
          if(WORD(1:12).eq.'Typical_week'.or.
     &       WORD(1:12).eq.'typical_week')then
            simact='i5t   '
          elseif(WORD(1:4).eq.'User'.or.WORD(1:4).eq.'user')then
            CALL EGETWI(OUTSTR,K,iday1,1,365,'W','start julian',IER)
            CALL EGETWI(OUTSTR,K,iday2,1,365,'W','end julian',IER)
            CALL EGETWI(OUTSTR,K,iday3,1,365,'W','start julian',IER)
            CALL EGETWI(OUTSTR,K,iday4,1,365,'W','end julian',IER)
            CALL EGETWI(OUTSTR,K,iday5,1,365,'W','start julian',IER)
            CALL EGETWI(OUTSTR,K,iday6,1,365,'W','end julian',IER)
            CALL EGETWI(OUTSTR,K,iday7,1,365,'W','start julian',IER)
            CALL EGETWI(OUTSTR,K,iday8,1,365,'W','end julian',IER)
            CALL EGETWI(OUTSTR,K,iday9,1,365,'W','start julian',IER)
            CALL EGETWI(OUTSTR,K,iday10,1,365,'W','end julian',IER)
            simact='i5u   '
          elseif(WORD(1:9).eq.'season'.or.
     &           WORD(1:9).eq.'season')then
            simact='i5s   '
          endif
          nipvassmt=5
        else
          nipvassmt=0
          simact='i1d   '
        endif
        write(6,*) 'the value of simact is ',simact
        goto 43
      elseif(WORD(1:4).eq.'*ipv')then
        goto 43
      elseif(WORD(1:10).eq.'*start_xml')then  
        inumXMLmetrics=0
        CALL LSTRIPC(IUNIT,OUTSTR,99,ND,1,'read metrics',IER)  
        K=0
        CALL EGETW(OUTSTR,K,WORD248,'W','xml data',IER)
        do while(WORD248(1:8).ne.'*end_xml')                           
          inumXMLmetrics=1+inumXMLmetrics        
          XMLMETRICS(inumXMLmetrics)=WORD248
          CALL LSTRIPC(IUNIT,OUTSTR,99,ND,1,'read metrics',IER)  
          K=0
          CALL EGETW(OUTSTR,K,WORD248,'W','xml data',IER)            
        end do
        if(WORD(1:8).eq.'*end_xml'.or.WORD248(1:8).eq.'*end_xml')then
          goto 43
        endif       
        goto 43
      else
        goto 43
      endif

C Now close silent data file.
      CALL ERPFREE(IUNIT,ios)
      RETURN

C Process the data if the end of the file reached or if the
C *end mark was found.
 1000 CALL ERPFREE(IUNIT,ios)

C Create new model. Note for a single folder option the
C configuration will be in /tmp/box if mpath is /tmp/box. For the
C distributed folder option the onfiguration file will be in the
C folder /tmp/box/cfg if mpath is /tmp/box.
      call silentmodel(actions,root,mpath,folder,silentmenu,weather,
     &  ier)

C Add in site information if tokens were included.
      if(havesite)then
        XLAT=sitelat
        SLAT=XLAT
        XLON=sitelong
        SLON=XLON
        IXPOS=siteexposureindex
        if(IXPOS.EQ.8)then
          SKYR=skyview
          GRDR=groundview
          BLDR=buildingview
        endif
      endif
      if(havegroundreflanual)GREF=groundreflanual
      if(groundreflmodel.gt.0)then
        ITGREF=groundreflmodel
        do 443 i=1,12
          GREF12(I)=groundreflmonth(I)
  443   continue
        SREF=snowrefl
      endif
      if(havesnowdays)then
        do 444 i=1,12
          NSNOW(I)=dayswithsnow(I)
  444   continue
      endif
      if(havehourlysnowfile)then
        SNFNAM=hourlysnowfile
      endif
      if(havegroundtempsets)then
        NGRDP=groundtempsets
        do 128 IGRDP=1,NGRDP
          DO 129 J=1,12
            UGRDTP(J,IGRDP)=groundtemps(J,IGRDP)
  129     CONTINUE
  128   continue
      endif

C Add in simulation parameter and assessment data if included.
      if(havesimparameters)then
        isstup=simstartup
        isbnstep=simzonetimestep
        ispnstep=simplanttimestep
        issave=simsavelevel

C Assign assessment information
C Rescan the `climatelist` file. Check if this climate is
C in the list. If not instanciate season and typical start and end dates.
        INQUIRE (FILE=cdblfil,EXIST=XST)
        if(XST)then
          IUF=IFIL+2
          call scancdblist(IUF,LCLIM,ok,ier)
          if(ok)then
            continue
          else

C Set default early winter, spring, summer, autumn, late winter periods.
            CALL EDAY(9,1,ia1wins)
            CALL EDAY(15,1,ia1winf)
            CALL EDAY(6,3,ia1sprs)
            CALL EDAY(12,3,ia1sprf)
            CALL EDAY(11,7,iasums)
            CALL EDAY(17,7,iasumf)
            CALL EDAY(2,10,ia2sprs)
            CALL EDAY(8,10,ia2sprf)
            CALL EDAY(20,11,ia2wins)
            CALL EDAY(26,11,ia2winf)

C Default season definitions.
            CALL EDAY(1,1,is1wins)
            CALL EDAY(28,2,is1winf)
            CALL EDAY(1,11,is2wins)
            CALL EDAY(31,12,is2winf)
            CALL EDAY(1,3,is1sprs)
            CALL EDAY(30,4,is1sprf)
            CALL EDAY(1,9,is2sprs)
            CALL EDAY(31,10,is2sprf)
            CALL EDAY(1,5,is1sums)
            CALL EDAY(31,8,is1sumf)
          endif
        endif

C Clear the IPV data structures and initialize based on the
C assessment regime. Then use the IPV data and transfer it to
C equivalent in simulation parameter data structure via ipv2simpar
        ipvact='i'
        call clearipvdat(ipvact)
        call ipvdatinit(simact)
        call ipv2simpar(simact)

C << assessment expansion to be done >>

      endif

C << location for further system level data >>                       

C Update configuration file to know about the construction files.
      CALL EMKCFG('-',IER)

C If there are no zones return.
      if(nsz.eq.0)then
        return
      else
        if(ncomp.eq.0)then
          icomp=0      ! reset because it will be incrmented later
        elseif(ncomp.gt.0)then
          icomp=ncomp  ! for existing model set icomp to next avail zone
        endif
        do 53 isz=1,nsz

C Call silent zone and then loop back for more possible definiitons.
C Copy from sz* arrays into parameter arrays prior to silentzone call.
          origin(1)=szorigin(isz,1)
          origin(2)=szorigin(isz,2)
          origin(3)=szorigin(isz,3)
          size(1)=szsize(isz,1)
          size(2)=szsize(isz,2)
          size(3)=szsize(isz,3)
          use(1)= szuse(isz,1)
          use(2)= szuse(isz,2)
          use(3)= szuse(isz,3)
C        usefile(iz) = ' '
          do 48 isu=1,nbcord(isz)
            coords(isu,1)=szcoords(isz,isu,1)
            coords(isu,2)=szcoords(isz,isu,2)
            coords(isu,3)=szcoords(isz,isu,3)
  48      continue
          do 49 isu=1,MS
            nbedges(isu)=isznbedges(isz,isu)
            do 57 ivu=1,MV
              list(isu,ivu)=iszlist(isz,isu,ivu)
  57        continue
            hasglaze(isu)=szhasglaze(isz,isu)
            hasdoor(isu)=szhasdoor(isz,isu)
            hasconstruction(isu)=szhasconstruction(isz,isu)
            hasname(isu)=szhasname(isz,isu)
            boundarytype(isu,1)=szboundarytype(isz,isu,1)
            boundarytype(isu,2)=szboundarytype(isz,isu,2)
            boundarytype(isu,3)=szboundarytype(isz,isu,3)
  49      continue
          do 158 imu=1,4
            datamass(imu,1)=sdatamass(isz,imu,1)
            datamass(imu,2)=sdatamass(isz,imu,2)
            datamass(imu,3)=sdatamass(isz,imu,3)
            datamass(imu,4)=sdatamass(isz,imu,4)
            datamass(imu,5)=sdatamass(isz,imu,5)
            datamass(imu,6)=sdatamass(isz,imu,6)
            datamass(imu,7)=sdatamass(isz,imu,7)
            textmass(imu,1)=stextmass(isz,imu,1)
            textmass(imu,2)=stextmass(isz,imu,2)
            textmass(imu,3)=stextmass(isz,imu,3)
 158      continue
          icomp=icomp+1

C If there are obstructions fill in relevant common blocks. Set iobs()=2
C to signal that the common blocks have been instanciated and data should
C be included in the version 1.1 geometry file.
          if(nbob(isz).gt.0)then
            NB=nbob(isz)
            NOX=20
            NOZ=20
            do 58 ib=1,nbob(isz)
              XO(ib)=XOB(isz,ib)
              YO(ib)=YOB(isz,ib)
              ZO(ib)=ZOB(isz,ib)
              DX(ib)=DXOB(isz,ib)
              DY(ib)=DYOB(isz,ib)
              DZ(ib)=DZOB(isz,ib)
              BANG(ib)=BANGOB(isz,ib)
              BLKNAME(ib)=BLOCKNAME(isz,ib)
              BLKMAT(ib)=BLOCKMAT(isz,ib)
  58        continue
            iobs(icomp)=2
          else
            iobs(icomp)=0
            NB=0
          endif
          if(i_ctl_link(isz).gt.0)then
            isilentncf=1+isilentncf
            ncf=isilentncf
            IBSN(isilentncf,1)=0
            IBSN(isilentncf,2)=0     
            IBSN(isilentncf,3)=0
            IBSN(isilentncf,4)=0 
            IBAN(isilentncf,1)=0
            IBAN(isilentncf,2)=0   
            IBAN(isilentncf,3)=0
            NBCDT(isilentncf)=1
            IBCDV(isilentncf,1,1)=1
            IBCDV(isilentncf,1,2)=365
            NBCDP(isilentncf,1)=1
            IBCTYP(isilentncf,1,1)=0
            IBCLAW(isilentncf,1,1)=1
            TBCPS(isilentncf,1,1)=0
            BMISCD(isilentncf,1,1,1)=7
            BMISCD(isilentncf,1,1,2)=999000
            BMISCD(isilentncf,1,1,3)=0.0      
            BMISCD(isilentncf,1,1,4)=999000
            BMISCD(isilentncf,1,1,5)=0.0
            BMISCD(isilentncf,1,1,6)= ht_Setpoint(isz)
            BMISCD(isilentncf,1,1,7)=cl_Setpoint(isz)
            BMISCD(isilentncf,1,1,8)=0.0
            znctldoc='basic ideal controls'
            ICASCF(isz)=ncf
          endif
          
          write(outs,'(2a)') 'Processing ',name(isz)
          call edisp(iuout,outs)
          call silentzone(ICOMP,name(isz),shape(isz),nbwalls(isz),
     &      nbmass(isz),nbedges,nbcord(isz),list,origin,size,
     &      coords,rotateit(isz),nbglz(isz),hasglaze,
     &      nbdoor(isz),hasdoor,hasname,hasconstruction,
     &      boundarytype,datamass,textmass,use,usefile(isz),IER)
  53    continue

        if(unixok)then
          fs = char(47)
        else
          fs = char(92)
        endif
        LN=max(1,LNBLNK(cfgroot))
        if(ctlpth(1:2).eq.'  '.or.ctlpth(1:2).eq.'./')then
          WRITE(LCTLF,'(2a)')cfgroot(1:ln),'.ctl'
        else
          WRITE(LCTLF,'(4a)') ctlpth(1:lnblnk(ctlpth)),fs,
     &       cfgroot(1:ln),'.ctl'
        endif      
        ICTLF=IFIL+1
        call CTLWRT(ICTLF,IER)

C Update cfg file to know about the control file
        CALL EMKCFG('-',IER)         
      endif
      RETURN

 1001 write(outs,'(3a)') 'Conversion error in...',OUTSTR(1:50),'...'
      if(dll)then
        dllsubr='SILENTREAD'
        dllmesg=outs
        ier=2
        CALL ERPFREE(IUNIT,ios)
        return
      else
        call edisp(iuout,outs)
        IER=1
        CALL ERPFREE(IUNIT,ios)
        RETURN
      endif

      END

