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
C  POINTTOLINE: determines distance from a 3D point to a 3D line.


C ************* SCRZONE 
C SCRZONE provides initial form of an enclosure as either a prism or
C an extrusion. IER=0 OK. 
      SUBROUTINE SCRZONE(ICOMP,IER)
#include "building.h"
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/G0/CTYPE
      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      COMMON/G2/NGL(MS)
      COMMON/G5/SNAME(MCOM,MS),SOTF(MS),SMLCN(MS),SVFC(MS),SOTHER(MS)
      COMMON/G6/SSNAME(MCON),SSOTF(MCON),SSMLCN(MCON),SSVFC(MCON),
     &          SSOTHER(MCON),SSPARENT(MCON)
      COMMON/G4/NDP(MCOM),IDPN(MCOM,3)
      COMMON/precz/zname(MCOM),zdesc(MCOM)
      COMMON/PRODB/LPRFDB,IPRODB
      common/PREC17/ZBASEA(MCOM),IBASES(MCOM,6),IUZBASEA(MCOM)
      COMMON/C1/NCOMP,NCON
      COMMON/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      COMMON/C20/NZSUR(MCOM),NZTV(MCOM)
      common/C21/IFCFG,cfgroot,LCFGF
      common/user/browse
      common/paths/zonepth,netpth,ctlpth,imgpth,radpth,docpth,tmppth,
     &             dbspth
      common/cctlnm/ctldoc,lctlf
      common/cctl/icascf(mcom)

      DIMENSION XX(MS),YY(MS)
      CHARACTER*72 LSNAM,LPROJ,LGEOM,LSHAD,LTHRM,LUTIL,LPRFDB
      CHARACTER SNAME*12,SMLCN*12,SVFC*4,SOTF*4,SOTHER*15,HOLD*32
      CHARACTER SSMLCN*12,SSVFC*4,SSOTF*4,SSOTHER*15,SSNAME*12
      character MODE*4,SSPARENT*12
      CHARACTER OUTSTR*124,H*72,zname*12,CTYPE*3,ZN*12,zdesc*64
      character ctldoc*248,LCTLF*72,zd*64,DFILE*72,CFILE*72,OFILE*72
      character zonepth*24,netpth*24,ctlpth*24,imgpth*24,radpth*24
      character docpth*24,tmppth*24,dbspth*24,cfgroot*24,LCFGF*72
      LOGICAL OK,dok,browse,OKC,XST

C If browsing then user cannot create a new zone.
      if(browse)then
        call usrmsg('Cannot update model while in browse ',
     &    'mode with a new zone, you must `own` the model! ','W')
        return
      endif

C Clear local coordinate array.
      DO 345 IW=1,MS
        XX(iw)=0.0
        YY(iw)=0.0
 345  continue

C Creation of a new zone, first ask its name, trapping out
C illegal characters.
      call tstamp('>','PRJ: new zone')
      H(1)='The name of the zone is used both for descriptive'
      H(2)='and bookkeeping purposes.  Each name should be'
      H(3)='unique and should not contain blanks. '
 42   ZN=' '
      CALL EASKS(ZN,' What do you want to call this new zone ',
     &  ' ( <12 char, no blanks) ?',12,'new_zone','zone name',IER,3)
      IF(ZN.eq.' '.or.ier.ne.0)goto 42
      call st2name(ZN,zname(ICOMP))

C << Unix vs NT needs to be updated >>
      if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
        WRITE(DFILE,'(2a)') zname(ICOMP)(1:lnblnk(zname(ICOMP))),
     &    '.geo'
        WRITE(CFILE,'(2a)') zname(ICOMP)(1:lnblnk(zname(ICOMP))),
     &    '.con'
        WRITE(OFILE,'(2a)') zname(ICOMP)(1:lnblnk(zname(ICOMP))),
     &    '.opr'
      else
        WRITE(DFILE,'(4a)') zonepth(1:lnblnk(zonepth)),'/',
     &     zname(ICOMP)(1:lnblnk(zname(ICOMP))),'.geo'
        WRITE(CFILE,'(4a)') zonepth(1:lnblnk(zonepth)),'/',
     &     zname(ICOMP)(1:lnblnk(zname(ICOMP))),'.con'
        WRITE(OFILE,'(4a)') zonepth(1:lnblnk(zonepth)),'/',
     &     zname(ICOMP)(1:lnblnk(zname(ICOMP))),'.opr'
      endif
      LGEOM(ICOMP)=DFILE
      LTHRM(ICOMP)=CFILE
      LPROJ(ICOMP)=OFILE

      H(1)='The zone description allows further information about'
      H(2)='the zone form and composition to be recorded.  '
      write(zd,'(a,a)') 
     &    zname(ICOMP)(1:lnblnk(zname(ICOMP))),' describes a...'
 43   CALL EASKS(zd,' What does it represent ',
     &  ' ( <64 char) ?',64,'no description entered','zone descr',IER,2)
      IF(zd.eq.' '.or.ier.ne.0)goto 43
      zdesc(ICOMP)=zd

C Allow user choices of beginning as a simple shape (with image feedback). 
      IER=0
      H(1)='You have the option to describe the thermal zone as an'
      H(2)='extruded rectangle, a floor plan extrusion or a general'
      H(3)='polyhedra.  Keeping the geometry as simple as possible'
      H(4)='will simplify the constructional attribution and result'
      H(5)='interpretation tasks.'
      H(6)=' '
      H(7)='Points from a bitmap:                  '
      H(8)=' Zone geometry can be defined by taking points from a'
      H(9)=' scanned image of a site plan or floor plan.         '
      H(10)='                  '
      H(11)=' To do this you need to scan the relevant source and '
      H(12)=' then convert it to an XBM (X11 monochrome bitmap) file'
      H(13)=' which should be placed in the project `cfg` folder.'
      H(14)=' '
      h(15)=' Please ensure that the bitmap is large enough to'
      h(16)=' allow for accurate positioning of the mouse. The bitmap'
      h(17)=' can be larger than the graphic display area, and if it'
      H(18)=' then you can pan left and right and up and down within'
      H(19)=' it.'
      H(20)='                  '
      H(21)=' The source image should have indications on it of an'
      H(22)=' origin (typically X=0.0 Y=0.0), the direction north,'
      H(23)=' a line of known length (to get scaling from).'
      CALL EASKABCD('Begin enclosure description with :',' ',
     &    'extruded rectangle','floor plan extrusion','polyhedra',
     &    'click on bitmap',IW2,23)
 144  IF(IW2.eq.1)THEN
        XO=0.
        YO=0.
        ZO=0.
        DX=3.0
        DY=4.0
        DZ=2.7
        H(1)='The specification of a box requires an origin on the'
        H(2)='site as well as width, depth and height. Assuming '
        H(3)='there is no rotation the width of the box is its '
        H(4)='distance along the X axis, the depth is along the '
        H(5)='Y axis, and the height is along the Z axis.  The '
        H(6)='box may then be rotated with the angle being measured'
        H(7)='anticlockwise from the X axis.  '
        H(8)=' '
        H(9)='The origin of the box is in site coords (metres).'
        HOLD=' '
        WRITE(HOLD,'(1x,3f8.3)')XO,YO,ZO
        CALL EASKS(HOLD,' Origin  X  Y  Z (in metres): ',
     &     '  ',32,' 0. 0. 0. ','origin coord',IER,9)
        K=0
        CALL EGETWR(HOLD,K,XO,-999.9,999.9,'W','X origin',IER)
        CALL EGETWR(HOLD,K,YO,-999.9,999.9,'W','Y origin',IER)
        CALL EGETWR(HOLD,K,ZO,-9.9,999.9,'W','Z origin',IER)

 242    H(1)='The width (with no rotation) is along the X axis.'
        H(2)='The depth (with no rotation) is along the Y axis.'
        H(3)='The height is along the Z axis (metres).'
        H(4)='Dimensions should be positive.'
        HOLD=' '
        WRITE(HOLD,'(1x,3f8.3)')DX,DY,DZ
        CALL EASKS(HOLD,' Prism width, depth, height (in metres): ',
     &    '  ',32,' 2.5 3.5 2.7 ','box suze',IER,4)
        K=0
        CALL EGETWR(HOLD,K,DX,0.001,99.9,'W','box width',IER)
        CALL EGETWR(HOLD,K,DY,0.001,99.9,'W','box depth',IER)
        CALL EGETWR(HOLD,K,DZ,0.001,99.9,'W','box height',IER)
        if(DX.lt.0.001.or.DY.lt.0.001)goto 242

        H(1)='Orientation is anticlockwise degrees between the '
        H(2)='length side of the obstruction and the X axis.'
        ANG=0.
        CALL EASKR(ANG,' ',' Box orientation? ',
     &       -359.0,'W',359.0,'W',0.0,'box orientation',IER,2)

C Convert box into a gen description.
        CALL ERECC(XO,YO,ZO,DX,DY,DZ,ANG)
        NSUR=6
        IUZBASEA(icomp)=0
        IBASES(ICOMP,1)=6
      ELSEIF(IW2.eq.2)THEN

C User begins with an extruded shape.
        Z1=0.
        Z2=2.4
        NW=4
        H(1)='The base (floor) elevation is in metres.'
        CALL EASKR(Z1,' ',' Elevation of the base ? ',
     &       0.000,'W',99.9,'W',0.0,'elevation of base',IER,1)
        H(1)='The top surface height is along the Z axis (metres).'
        H(2)='Note: this is not the base-top distance!'
        CALL EASKR(Z2,' ',' Elevation of the top ? ',
     &       0.000,'W',99.9,'W',2.7,'elevation of top',IER,2)
        H(1)='The number of walls does not include the base '
        H(2)='and the top.'
        CALL EASKI(NW,' ',' Number of walls? ',
     &           3,'F',MS-2,'F',4,'no of walls',IER,2)
        call edisp(iuout,'Input the coordinates around the base ')
        call edisp(iuout,'polygon ANTI-clockwise looking from the')
        call edisp(iuout,'top, preferably beginning from the lower ')
        call edisp(iuout,'left corner of the enclosure. ')

        DO 45 IW=1,NW
          WRITE(OUTSTR,'(A,I2,A)')' For base vertex',IW,'  X & Y:'
          HOLD=' '
          WRITE(HOLD,'(1x,2f8.3)')XX(IW),YY(IW)
          H(1)='The coordiantes should be in metres and should be'
          H(2)='given anti-clockwise looking from the top.'
          CALL EASKS(HOLD,OUTSTR,'  ',32,' 0.  0.  ','floor xy',IER,1)
          K=0
          CALL EGETWR(HOLD,K,XX(IW),-999.,999.,'W','flr X',IER)
          CALL EGETWR(HOLD,K,YY(IW),-999.,999.,'W','flr Y',IER)
   45   CONTINUE
        dok=.true.
        h(1)='If you have any doubt as to whether you put in the'
        h(2)='correct points you can save time by respecifying them'
        h(3)='now. Usually this takes less time than trying to '
        h(4)='correct problems later. '
        CALL ASKOK('Accept base vertices?','(see help)',OK,dok,4)
        if(.NOT.ok)then
          IW2=2
          goto 144
        endif

        H(1)='Rotation is the angle between the site y-axis and'
        H(2)='north for the enclosure (anticlockwise is a'
        H(3)='positive rotation). NOTE THIS IS DIFFERENT FROM'
        H(4)='THE ORIENTATION associated with simple BOXES.'
        AR=0.
        CALL EASKR(AR,' ',' Extrusion rotation? ',
     &       -359.0,'W',359.0,'W',0.0,'extrusion rotation',IER,4)

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
      IF(IW2.ne.3.and.IW2.ne.4)THEN

        CALL FILSUR(iuout,ICOMP,0)
        DO 44 I=1,NSUR
          NGL(I)=0
   44   CONTINUE

C Update the connection list.
        ICCC=NCON
        DO 32 ICC=1,NSUR
          ICCC=ICCC+1
          IC1(ICCC)=ICOMP
          IE1(ICCC)=ICC
          ICT(ICCC)=0
          IC2(ICCC)=0
          IE2(ICCC)=0
          SSNAME(iccc)=SNAME(icomp,icc)
          SSOTF(iccc)=SOTF(icc)
          SSMLCN(iccc)=SMLCN(icc)
          SSVFC(iccc)=SVFC(icc)
          SSOTHER(iccc)=SOTHER(icc)
   32   CONTINUE
        NCON=ICCC
      endif
      if(IW2.eq.3)then

C User will begin with a minimal GEN body.
        H(1)='A general polyhedra enclosure is appropriate for complex'
        H(2)='shapes. You define a set of vertices and then link them'
        H(3)='together to form surfaces (linked anti-clockwise from'
        H(4)='the lower-left when viewing the outside of surface). '
        H(5)=' '
        H(6)='Supply an origin point (metres) and a 2m x 2m'
        H(7)='initial (floor) surface will be placed there. From'
        H(8)='this initial surface, modify the existing vertices'
        H(9)='and/ or add vertices to create the zone enclosure.'
        H(10)=' '
        H(11)='Please ignore initial warning messages (these will stop'
        H(12)='after you have defined a few surfaces. The initial '
        H(13)='surface may be deleted when no longer needed.'
        CALL PHELPD('gen start popup',13,'-',0,0,IER)
        NTV=4
        NSUR=1
        AR=0.0
        H(1)='The origin of the zone is the X,Y,Z of the first'
        H(2)='surface (from which the rest of the zone will be'
        H(3)='built).'
        HOLD=' '
        WRITE(HOLD,'(1x,3f8.3)')X(1),Y(1),Z(1)
        CALL EASKS(HOLD,' Origin  X  Y  Z (in metres): ',
     &     '  ',32,' 0. 0. 0. ','origin coord',IER,3)
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
        SOTHER(1)='UNKNOWN'
        NGL(1)=0
        IUZBASEA(icomp)=0
        IBASES(ICOMP,1)=1

C Update the connection list for initial surface.
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
        SSOTHER(iccc)=SOTHER(1)
        NCON=ICCC
      elseif(IW2.eq.4)then

C Point and click on a bitmap. After return, re-scan the system
C configuration file to ensure that all the derived information
C in the new geometry files is established.
        if(MMOD.eq.8)then
#ifdef X11
          call clickonbitmap(icomp,ier) 
          if(ier.ne.0)then
            itrc=0
            MODE='ALL '
            IAPROB=IPRODB
            CALL ERSYS(LCFGF,IFCFG,IAPROB,MODE,itrc,IER)
          endif
          return
#else
          call usrmsg('The clickonbitmap facility is not yet working',
     &      'with new graphic library.','W')
#endif
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
      ENDIF
      if(zdesc(ICOMP)(1:2).EQ.'  ')then
        write(zdesc(ICOMP),'(a,a)') 
     &    zname(ICOMP)(1:lnblnk(zname(ICOMP))),' describes a...'
      endif
      CTYPE='GEN'
      NDP(ICOMP)=3
      IDPN(ICOMP,1)=0
      IDPN(ICOMP,2)=0
      IDPN(ICOMP,3)=0
 
C Save this to file before passing into the geometry editing facility.
      call edisp(iuout,' updating zone description...')
      CALL EMKGEO(IFIL+2,LGEOM(ICOMP),ICOMP,iuout,3,IER)
      IF(IER.EQ.1)THEN
        dok=.true.
        h(1)='When attempting to write the geometry a problem was'
        h(2)='encountered. Check that you have permission to write'
        h(3)='this type of file and that you have sufficient disk'
        h(4)='space.'
        CALL ASKOK(' ','Problem creating geometry file...try again?',
     &    OK,dok,4)
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
      NZSUR(ICOMP)=NSUR
      NZTV(ICOMP)=NTV
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
      COMMON/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      COMMON/G5/SNAME(MCOM,MS),SOTF(MS),SMLCN(MS),SVFC(MS),SOTHER(MS)
      COMMON/G6/SSNAME(MCON),SSOTF(MCON),SSMLCN(MCON),SSVFC(MCON),
     &          SSOTHER(MCON),SSPARENT(MCON)
      COMMON/C1/NCOMP,NCON
      COMMON/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      COMMON/C20/NZSUR(MCOM),NZTV(MCOM)
      COMMON/C24/IZSTOCN(MCOM,MS)
      COMMON/RAY7/ZXMN(MCOM),ZYMN(MCOM),ZZMN(MCOM),ZXMX(MCOM),
     &            ZYMX(MCOM),ZZMX(MCOM),ZBFLG(MCOM)
      common/cctlnm/ctldoc,lctlf
      common/cctl/icascf(mcom)
      COMMON/precz/zname(MCOM),zdesc(MCOM)
      common/paths/zonepth,netpth,ctlpth,imgpth,radpth,docpth,tmppth,
     &             dbspth

      CHARACTER*72 LSNAM,LPROJ,LGEOM,LSHAD,LTHRM,LUTIL,LTMP,DFILE
      character SSMLCN*12,SSVFC*4,SSOTF*4,SSOTHER*15,SSNAME*12
      character SMLCN*12,SVFC*4,SOTF*4,SOTHER*15,SNAME*12,SSPARENT*12
      CHARACTER OUTSTR*124,H*72,ctldoc*248,LCTLF*72
      character zonepth*24,netpth*24,ctlpth*24,imgpth*24,radpth*24
      character docpth*24,tmppth*24,dbspth*24,zname*12,zdesc*64
      character sfile*72,snpfile*72,fs*1,outs*124

      LOGICAL OK,DOK,XST,OKC,unixok

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

      H(1)='You may read in an existing geometry file or use the '
      H(2)='inbuilt facilities to create a zone from dimensional '
      H(3)='data (starting with a prisim, extrusion or set of '
      H(4)='vertices). '
      H(5)=' '
      H(6)='Importing CAD descriptions must currently be done for'
      H(7)='a whole model. '
      CALL EASKABC(' New zone choices:',' ','loading existing',
     &     'create via dimension input','cancel',INZOPT,7)

      if(INZOPT.eq.1)then

C Find out if there are geometry files in the model ../zones folder.
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
            write(6,*) ltmp
          else
            write(ltmp,'(a)')zonepth(1:lnblnk(zonepth))
          endif
        else
          write(ltmp,'(a)')zonepth(1:lnblnk(zonepth))
        endif

C Existing ESP-r geom file.
        h(1)='If there was not existing geometry file found in the'
        h(2)='../zones folder then you must supply a file name. '
        h(3)='It should be relative to the cfg folder. '
        DFILE=' '
        WRITE(OUTSTR,292)IC
 292    FORMAT(' For zone ',I2,' the geometry is defined in:')
 293    CALL EASKS(LTMP,OUTSTR,' ',72,DFILE,'geom file',IER,3)
        IF(LTMP(1:2).EQ.'  ')GOTO 293
        call FINDFIL(LTMP,XST)
        IF(.NOT.XST)THEN
          dok=.true.
          CALL ASKOK(' ','Geometry file not found, try again?',OK,
     &      dok,3)
          IF(OK)GOTO 293
        ENDIF
        LGEOM(IC)=LTMP

C Scan geometry file and update control and connections.
C Try reading in the geometry to fill in the data below.
        CALL EGOMIN(IFIL+1,LGEOM(IC),IC,1,ITRC,iuout,IER)
        IF(IER.NE.0)THEN
          dok=.true.
          h(1)='While scanning the geometry file a problem was'
          h(2)='encountered. Check that the file is of the correct'
          h(3)='type and that it has not been corrupted. '
          CALL ASKOK(OUTSTR,
     &      'Problem scanning geometry file...try again?',OK,dok,3)
          IF(OK)GOTO 293
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
        DO 132 ICC=1,NSUR
          ICCC=ICCC+1
          IC1(ICCC)=IC
          IE1(ICCC)=ICC
          if(SOTHER(icc)(1:8).eq.'EXTERIOR')then
            ICT(ICCC)=0
            IC2(ICCC)=0
            IE2(ICCC)=0
          elseif(SOTHER(icc)(1:7).eq.'SIMILAR')then
            ICT(ICCC)=1
            IC2(ICCC)=0
            IE2(ICCC)=0
          elseif(SOTHER(icc)(1:9).eq.'ADIABATIC')then
            ICT(ICCC)=5
            IC2(ICCC)=0
            IE2(ICCC)=0
          elseif(SOTHER(icc)(1:8).eq.'CONSTANT')then
            write(outs,'(3a)') 'Note: ',SNAME(NCOMP,icc),
     &        ' is a CONSTANT connection. Use the'
            call usrmsg(outs,
     &       'surface attribute facility to confirm its details.','W')
            ICT(ICCC)=0
            IC2(ICCC)=0
            IE2(ICCC)=0
          elseif(SOTHER(icc)(1:6).eq.'GROUND')then
            write(outs,'(3a)') 'Note: ',SNAME(NCOMP,icc),
     &        ' is a GROUND connection. Use the'
            call usrmsg(outs,
     &       'surface attribute facility to confirm its details.','W')
            ICT(ICCC)=0
            IC2(ICCC)=0
            IE2(ICCC)=0
          else
            write(outs,'(3a)') 'Note: ',SNAME(NCOMP,icc),
     &        ' is a possible partition. Confirm its'
            call usrmsg(outs,
     &        'surface attributes or use topology tool.','W')
            ICT(ICCC)=0
            IC2(ICCC)=0
            IE2(ICCC)=0
          endif
          SSNAME(iccc)=SNAME(NCOMP,icc)
          SSOTF(iccc)=SOTF(icc)
          SSMLCN(iccc)=SMLCN(icc)
          SSVFC(iccc)=SVFC(icc)
          SSOTHER(iccc)=SOTHER(icc)
          IZSTOCN(ncomp,icc)=iccc
  132   CONTINUE
        NCON=ICCC

C Update the G7 common blocks and then write out configuration file.
        call zgupdate(0,NCOMP,ier)
        CALL EMKCFG('-',IER)
        call usrmsg(
     &  'NOTE: ensure that the model topology is correct by using ',
     &  'the topology facility under [zones definition].','W')

C Check to see if there are other zone files which can be associated
C with this new zone.
        if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
          WRITE(DFILE,'(A,A4)')zname(IC)(1:lnblnk(zname(IC))),'.opr'
        else
          WRITE(DFILE,'(3A,A4)') zonepth(1:lnblnk(zonepth)),'/',
     &      zname(IC)(1:lnblnk(zname(IC))),'.opr'
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
                write(6,*) ltmp
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
          WRITE(OUTSTR,294)IC
 294      FORMAT(' For zone ',I2,' the operations are defined in:')
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
          WRITE(DFILE,'(A,A4)')zname(IC)(1:lnblnk(zname(IC))),'.con'
        else
          WRITE(DFILE,'(3A,A4)') zonepth(1:lnblnk(zonepth)),'/',
     &      zname(IC)(1:lnblnk(zname(IC))),'.con'
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
                write(6,*) ltmp
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
          WRITE(OUTSTR,296)IC
 296      FORMAT(' For zone ',I2,' the constructions are defined in:')
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
      elseif(INZOPT.eq.2)then

C Provide scratch description and the
C default assumptions for other zone files.
        CALL SCRZONE(IC,IER)
        CALL EDZONE(ITRC,IC,IER)
        ZBFLG(IC)=0
        return
      elseif(INZOPT.EQ.3)then
        return
      endif
      end

C ************* DELZONE
C DELZONE systematicly removes a thermal zone.
      SUBROUTINE DELZONE(ITRC,IWHICH,IER)
#include "building.h"
#include "net_flow.h"
      COMMON/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      COMMON/G4/NDP(MCOM),IDPN(MCOM,3)
      COMMON/G5/SNAME(MCOM,MS),SOTF(MS),SMLCN(MS),SVFC(MS),SOTHER(MS)
      COMMON/G6/SSNAME(MCON),SSOTF(MCON),SSMLCN(MCON),SSVFC(MCON),
     &          SSOTHER(MCON),SSPARENT(MCON)
      COMMON/precz/zname(MCOM),zdesc(MCOM)
      COMMON/PREC2/VOL(MCOM)
      common/PREC17/ZBASEA(MCOM),IBASES(MCOM,6),IUZBASEA(MCOM)

      COMMON/C1/NCOMP,NCON
      COMMON/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      COMMON/C20/NZSUR(MCOM),NZTV(MCOM)
      common/C21/IFCFG,cfgroot,LCFGF
      COMMON/C24/IZSTOCN(MCOM,MS)

      COMMON/P2/NAC1,IACS1(MA),IACF1(MA),ACI1(MA),ACV1(MA),
     &          IPT1(MA),TA1(MA),NAC2,IACS2(MA),IACF2(MA),ACI2(MA),
     &          ACV2(MA),IPT2(MA),TA2(MA),NAC3,IACS3(MA),IACF3(MA),
     &          ACI3(MA),ACV3(MA),IPT3(MA),TA3(MA)
      COMMON/P2CTL/ITCTL(MCOM),TLO(MCOM),TUP(MCOM),THI(MCOM),ACIL(MCOM),
     &             ACVL(MCOM),IVL(MCOM),TAL(MCOM),ACIU(MCOM),ACVU(MCOM),
     &             IVU(MCOM),TAU(MCOM),ACIH(MCOM),ACVH(MCOM),
     &             IVH(MCOM),TAH(MCOM)

C Controls.
      common/bctl/ncf,ibsn(mcf,4),iban(mcf,3),nbcdt(mcf),
     &       ibcdv(mcf,mbcdt,2),nbcdp(mcf,mbcdt),tbcps(mcf,mbcdt,mbcdp),
     &       ibctyp(mcf,mbcdt,mbcdp),ibclaw(mcf,mbcdt,mbcdp),
     &       bmiscd(mcf,mbcdt,mbcdp,misc)
      common/fctl/ncc,ifsn(mcc,4),ifan(mcc,3),nfcdt(mcc),
     &       ifcdv(mcc,mfcdt,2),nfcdp(mcc,mfcdt),tfcps(mcc,mfcdt,mfcdp),
     &       ifctyp(mcc,mfcdt,mfcdp),ifclaw(mcc,mfcdt,mfcdp),
     &       fmiscd(mcc,mfcdt,mfcdp,misc)

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
      COMMON/PRODB/LPRFDB,IPRODB
      COMMON/PRECTC/ITMCFL(MCOM,MS),TMCT(MCOM,MTMC,5),
     &       TMCA(MCOM,MTMC,ME,5),TMCREF(MCOM,MTMC),TVTR(MCOM,MTMC)
      COMMON/AFN/IAIRN,LAPROB,LAPRES,LAFRES,ICAAS(MCOM)
      COMMON/MFLOW2/NDNAM(0:MNOD)
      COMMON/AFNZN/zmfn1,zmfn2
      COMMON/RAY5/ZCOG(MCOM,3),XMN,YMN,ZMN,XMX,YMX,ZMX
      COMMON/RAY7/ZXMN(MCOM),ZYMN(MCOM),ZZMN(MCOM),ZXMX(MCOM),
     &            ZYMX(MCOM),ZZMX(MCOM),ZBFLG(MCOM)

C Current file (for use by low level I/O calls)
      common/curfile/currentfile

      LOGICAL DOK,OK,XST,changed,OKC,OKM,modopr,browse

      CHARACTER*72 LVIEW,LHCCO,LTWIN,LCGCIN,ZOBS
      CHARACTER*72 LAPROB,LAPRES,LAFRES,LCFGF,LPRFDB
      CHARACTER*72 LSNAM,LPROJ,LGEOM,LSHAD,LTHRM,LUTIL
      CHARACTER SNAME*12,SMLCN*12,SVFC*4,SOTF*4,SOTHER*15,cfgroot*24
      CHARACTER SSMLCN*12,SSVFC*4,SSOTF*4,SSOTHER*15,SSNAME*12
      character SSPARENT*12
      CHARACTER H*72,zname*12,zdesc*64,ctldoc*248,LCTLF*72,outs*124
      character zmfn1*124,zmfn2*124,ltmp*72,NDNAM*12
      character ALOC*12,ALOCLBL*12,ALOCTYP*4,currentfile*72
      character OUTSTR*124

      if(browse)then
        call usrmsg('Cannot remove a zone while in browse ',
     &              'mode, you must `own` the model! ','W')
        return
      endif
      IW=IWHICH
      IW2=IWHICH

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
          currentfile=LAPROB
          IF(ND.GE.4)THEN
            REWIND(IUM,ERR=999)
            CALL EMFREAD(IUM,ITRC,IER)
            CALL ERPFREE(IUM,ISTAT)
          elseif((ND.eq.1.or.ND.eq.2).and.
     &            OUTSTR(1:18).EQ.'*Graphical_network')then

C Found a graphic network file, scan it and then convert it
C into network flow common blocks.
            CALL ERPFREE(IUM,ISTAT)
            call NETREAD(IUM,'R',IER)
            CALL NETTOFLW(ier)
          endif
C          CALL EMFREAD(IUM,ITRC,IER)
C          CALL ERPFREE(IUM,ISTAT)
          if(ier.ne.0)then
            CALL USRMSG(' ','problem in ventilation network','F')
            OKM=.false.
          else

C Reset strings which hold mass flow links so that subsequent
C write of configuration file will generate proper text.
C If a node is linked to zone to be deleted then check with user.
            if(ICAAS(IW).gt.0)then
              write(outs,'(3A)')zname(IW)(1:lnblnk(zname(IW))),
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
            zmfn1=' '
            zmfn2=' '
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
C faces the current zone then reset it to OUTSIDE.
      call usrmsg('Unlinking surfaces referencing this zone...',' ','-')
      do 25 IX=1,NCOMP
        if(IX.ne.IW2)then
          changed=.FALSE.
          CALL EGOMIN(IFIL+1,LGEOM(IX),IX,1,0,iuout,IER)
          do 28 ISX=1,NSUR
            ioc=IZSTOCN(ix,isx)
            if(SOTHER(ISX)(1:12).eq.zname(IW2)(1:12))then
              SOTHER(ISX)='EXTERIOR'
              SSOTHER(ioc)='EXTERIOR'
              changed = .true.
            endif
   28     continue
          if(changed)then
            write(outs,'(A,A)') ' updating topology in ',zname(ix)
            call usrmsg('  ',outs,'-')
            CALL EMKGEO(IFIL+2,LGEOM(IX),IX,iuout,3,IER)
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

C Weekday air flows.
        if(NAC1.gt.0)then
          DO 10 I=1,NAC1
            if(IPT1(I).eq.IW2)then
              modopr=.true.
              IPT1(I)=0
              ACV1(I)=0.0
            endif
10        continue
        endif

C Saturday air flows.
        if(NAC2.gt.0)then
          DO 11 I=1,NAC2
            if(IPT2(I).eq.IW2)then
              modopr=.true.
              IPT2(I)=0
              ACV2(I)=0.0
            endif
11        continue
        endif

C Sunday air flows.
        if(NAC3.gt.0)then
          DO 12 I=1,NAC3
            if(IPT3(I).eq.IW2)then
              modopr=.true.
              IPT3(I)=0
              ACV3(I)=0.0
            endif
12        continue
        endif

C Update operations file if any changes.
        if(modopr)then
          call edisp(iuout,' ')
          write(outs,'(1X,A,A)') zname(ix)(1:lnblnk(zname(ix))),
     &                          ' has inter-zone ventilation with the'
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
      CALL EGOMIN(IFIL+1,LGEOM(IW2),IW2,1,0,iuout,IER)
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
        zdesc(IX)=zdesc(IX+1)
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
        CALL EGOMIN(IFIL+1,LGEOM(ir),ir,1,0,IUOUT,IER)
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
        H(3)='control loops have been adjusted and are being saved...'
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
      COMMON/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)
      COMMON/INDICS/IVF(MCOM),ISI(MCOM),IHC(MCOM),
     &              ITW(MCOM),ICGC(MCOM),IOBS(MCOM)
      common/UDESC/LVIEW(MCOM),LHCCO(MCOM),
     &             LTWIN(MCOM),LCGCIN(MCOM),ZOBS(MCOM)
      common/pmchange/comold,tmcold,vwfold,ishold
      CHARACTER*72 LVIEW,LHCCO,LTWIN,LCGCIN,ZOBS
      CHARACTER*72 LSNAM,LPROJ,LGEOM,LSHAD,LTHRM,LUTIL
      CHARACTER act*3

      LOGICAL XST,comold,tmcold,vwfold,ishold

      if(act(1:3).eq.'sf-'.or.act(1:3).eq.'sf+'.or.
     &act(1:3).eq.'sat')then
        call FINDFIL(LTHRM(ICOMP),XST)
        if(XST)comold = .true.
      endif

C Check for implications of surface addition or removal or attribution change.
C << refine testing for setting tmcold >>
      if(act(1:3).eq.'sf-'.or.act(1:3).eq.'sf+'.or.
     &act(1:3).eq.'sat')then
        if(ITW(ICOMP).eq.1)then
          call FINDFIL(LTWIN(ICOMP),XST)
          if(XST)tmcold = .true.
        endif
      endif
      if(act(1:3).eq.'sf-'.or.act(1:3).eq.'sf+'.or.
     &act(1:3).eq.'str')then
        if(IVF(ICOMP).eq.1)then
          call FINDFIL(LVIEW(ICOMP),XST)
          if(XST)vwfold = .true.
        endif
      endif
      if(act(1:3).eq.'sf-'.or.act(1:3).eq.'sf+'.or.
     &act(1:3).eq.'str')then
        if(ISI(ICOMP).eq.1)then
          call FINDFIL(LSHAD(ICOMP),XST)
          if(XST)ishold = .true.
        endif
      endif
      if(act(1:3).eq.'ob-'.or.act(1:3).eq.'ob+')then
        if(ISI(ICOMP).eq.1)then
          call FINDFIL(LSHAD(ICOMP),XST)
          if(XST)ishold = .true.
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
      SUBROUTINE SUMRCHG(ICOMP,act)
#include "building.h"
      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)
      common/UDESC/LVIEW(MCOM),LHCCO(MCOM),
     &             LTWIN(MCOM),LCGCIN(MCOM),ZOBS(MCOM)
      COMMON/precz/zname(MCOM),zdesc(MCOM)
      common/pmchange/comold,tmcold,vwfold,ishold
      common/C21/IFCFG,cfgroot,LCFGF
      common/appw/iappw,iappx,iappy
      CHARACTER*72 LVIEW,LHCCO,LTWIN,LCGCIN,ZOBS
      CHARACTER*72 LSNAM,LPROJ,LGEOM,LSHAD,LTHRM,LUTIL
      CHARACTER H*72,zname*12,zdesc*64,act*1,ZN*12
      character doit*124,tmode*8,tfile*72,cfgroot*24,LCFGF*72
      logical comold,tmcold,vwfold,ishold,ok,dok,concat,QUIET,XST

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
     &    zname(icomp)(1:lnblnk(zname(icomp))),' the last action makes:'
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
          CALL PHELPD('files out of date',nh,'-',0,0,IER)
        endif
      endif
      if(ishold)then
        write(ZN,'(A)') zname(ICOMP)
        DOK=.true.
        h(1)='Shading should be recalculated if the changes you '
        h(2)='made affect isolation distribution. When in doubt '
        h(3)='it is best to recalculate. '
        CALL EASKABC(' Reflect zone change in shading options:',' ',
     &   'recalculate (silent)','recalculate (interactive)','continue',
     &    IRT,3)
        if(IRT.eq.1.or.IRT.eq.2)then
          if(IRT.eq.1) call edisp(iuout,
     & 'When finished control will be returned to the project manager.')

C Get logical name of terminal type, expand model name
C to include the path and create a string to drive ish.
          call tstamp('>','PRJ: start shading recalculation')
          doit = ' '
          call tchild(ICPMOD)
          call termode(ICPMOD,tmode)
          call addpath(LCFGF,tfile,concat)

C If prj initial size is a % of default pass this on to child with
C an offset from prj start position.
          if(IRT.eq.1)then
            write(doit,'(5a)') 'ish -mode text -file ',
     &        tfile(1:lnblnk(tfile)),' -zone ',
     &        ZN(1:lnblnk(ZN)),' -act update_silent'
            call usrmsg('starting shading analysis via',doit,'-')
            call runit(doit,'text')
            ishold=.false.
          else
            if(iappw.gt.0.and.iappw.le.200)then
              write(doit,'(3a,3i4,5a)') 'ish -mode ',tmode,
     &          ' -s ',iappw,iappx+15,iappy+50,' -file ',
     &          tfile(:lnblnk(tfile)),' -zone ',ZN(1:lnblnk(ZN)),
     &          ' -act update_silent'
            else
              write(doit,'(7a)') 'ish -mode ',tmode,
     &          ' -s 0 0 0 -file ',tfile(:lnblnk(tfile)),
     &          ' -zone ',ZN(1:lnblnk(ZN)),' -act update_silent'
            endif
            call usrmsg('starting shading analysis via',doit,'-')
            call runit(doit,tmode)
            ishold=.false.
          endif
        endif
      endif
      if(comold.or.tmcold)then
        DOK=.true.
        h(1)='Zone construction files should be rebuilt after '
        h(2)='surface attributes change or surfaces are added or'
        h(3)='deleted. '
        CALL ASKOK('Update the zone construction and/ or TMC files',
     &    'to reflect the changes that you have made?',OK,DOK,3)
        if(OK)then
          QUIET=.true.
          itru=iuout
          CALL EDCON(0,itru,ICOMP,QUIET,IER)
          QUIET=.false.
        endif
      endif

      RETURN
      END

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

