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

C egeometry.f provides the following facilities:
C  EGOMIN:  Reads zone geometry data as ASCII strings, with or without
C           range checking and printed summary.
C  EMKGEO:  Write an annotated geometry file (GEN type) based  
C           on common blocks G0 G1 G2 G2OPT G3 G4 G5 
C  ERECC:   Converts REC (rectilinear) into a G1 description.
C  CNVBLK:  Converts REC (rectilinear) description into GB1 common block.
C  EREGC:   Converts REG into a GEN description.
C  ESCROT:  Rotate a zone by ANG degrees around point x1,y1.
C  SURINFO: Display surface details and attributes in a tabular format.
C  ZINFO:   Returns descriptive information zone geometry.
C  ZGUPDATE: takes the geometry common G1 and updates G7 & PREC2
C  SUREDGEADJ: detects polygon edge associations (how many assoc. surfs etc.)
C  zdata:   scan zones to build master geometry structures.
C  FILSUR:  Fills the common block G5 with default assumptions.
C  INSINFO: English description of default insolation options.
C  VERINFO: Prints vertex/surface list summaries.
C  SURLIST: takes an list of surfaces (lstsf) returns descriptive
C           string (sdescr) to be used in headers.

 
C ******************** EGOMIN 
C EGOMIN reads zone geometry data from a user-constructed data
C file. There are three levels of geometry data input corresponding
C to RECtangular (REC), REGular (REG) and GENeral (GEN) shaped zones.
C If IR=1 then range checking is enabled, otherwise only minimal checking
C is performed on the data as read in.

C REC - Each of the 4 component walls are rectangular, vertical and of 
C       equal height. Ceiling and floor are rectangular and horizontal.

C REG - each of 'N' zone walls are rectangular, vertical and of equal
C       height. Ceiling and floor are horizontal polygons.

C GEN - any polyhedral-shaped zone comprised of 'M' planes.

C Zone geometry input requirements vary depending on the zone shape
C type but in each case is defined relative to some aebitrary site
C cartesian coordinate system. Geometry requirements are:

C REC - the X, Y and Z coordinates of the 'bottom left-hand' corner
C       (when viewed from the south), the length (from this point
C       towards east), width and height of the zone (all internal
C       dimensions) and the rotation angle (the angle between the
C       'length' side and east - anticlockwise +ve).

C REG - the number of walls, the X and Y coordinates of the bottom
C       corner of each wall in an anticlockwise direction, the floor
C       and ceiling heights and the rotation angle from the site
C       X-axis to east (anticlockwise +ve).

C GEN - the total number of vertices in the body, the number of
C       surfaces, the X, Y and Z coordinates of each vertex (in any
C       convenient order), the number of vertices in each surface,
C       an associated ordered list of the vertex numbers in an
C       anticlockwise order which comprise each surface and the
C       rotation angle from the site X-axis to east (anticlockwise
C       +ve).

C For GEN type input the 'G1' common block is filled immediately.
C With REC and REG type input the simpler input data is read and
C passed to subroutines ERECC and EREGC respectively where
C conversion to the 'G1' format takes place.
 
       SUBROUTINE EGOMIN(IUNIT,LGEOM,ICOMP,IR,ITRC,ITRU,IER)
      INCLUDE 'building.h'
      INCLUDE 'espriou.h'
C espriou.h provides currentfile.

C CTYPE (3 char) - zone shape type (REC, REG or GEN)
      COMMON/G0/CTYPE

C X,Y & Z   - coordinates of the vertices defining the zone
C NSUR      - Number of faces comprising zone
C JVN       - contains an ordered description (anticlockwise) of the
C             vertices in each face
C NTV       - total number of vertices in zone
C NVER      - contains the number of vertices in each face
      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV

C NGL       - contains the number of windows in each face (set to zero)
C             this is depreciated as the default window data structure is no
C             longer used.        
      COMMON/G2/NGL(MS)

C Default solar distribution and shading directives.
      COMMON/G4/NDP(MCOM),IDPN(MCOM,3)

C SNAME (12 char) - surface name attribute
C ZBASEA    - area of base (m^2)
C IBASES    - surfaces (up to 6) associated with base area. If all 0 then
C             base area has been supplied by the user.
C SOTF (4 char) - surface attribute indicating OPAQ or TRAN.
C SVFC (4 char) - string indicating whether the surface is to be
C     counted as a FLOR (floor), VERT (wall), CEIL (ceiling),
C     SLOP (not vertical) for purposes of floor area and daylighting.
C SMLCN (12 char) surface construction attribute.
C SOTHER (15 char) surface attribute for `other` side. UNKNOWN indicates
C     that no attribute has been set. EXTERIOR or SIMILAR or ADIABATIC or
C     BASESIMP or GROUND or a zone name are allowed.
      COMMON/G5/SNAME(MCOM,MS),SOTF(MS),SMLCN(MS),SVFC(MS),SOTHER(MS)

C ZNAME (12 char) - the zone name.
C ZDESC ( char) - zone notes.
      COMMON/precz/zname(MCOM),zdesc(MCOM)

C ZBASEA is the floor area of the zone, IBASES is a list of surfaces
C which make up the floor, IUZBASEA signals that the user selected
C specific surfaces (two), has edited the floor area (one) or it was
C calculated based on FLOR orientations (zero).
      common/PREC17/ZBASEA(MCOM),IBASES(MCOM,6),IUZBASEA(MCOM)
      
C Save total transparent surface area for surfaces connected to external BC.
      common/PREC18/ZTRANA(MCOM)
      COMMON/C20/NZSUR(MCOM),NZTV(MCOM)

      COMMON/GR3D07/Y0S(MS),Y0SS(MSSZ),Y0SE(MSEZ)

C Error subroutine and error details for dll mode.
      common/dllerr/dllsubr,dllmesg

      DIMENSION XX(MS),YY(MS),IVA(MS)
      DIMENSION SNA(MS),PAZI(MS),PELV(MS)
      CHARACTER OUTSTR*124,LGEOM*72,WORD*20,CTYPE*3,zname*12,SOTHER*15
      CHARACTER*12 SNAME,SMLCN
      CHARACTER*4 SVFC,tmpvfc,SOTF,tmpsot
      character ZN*12,phrase*64,zdesc*64,outs*124
      character dllsubr*12,dllmesg*124
      logical dll,checkbase

      IER=0
      ZN=' '
      phrase=' '
      checkbase=.false.

C Check if running in dll mode.
      call isadll(dll)

C Initialise geometry data file. and set currentfile.
      CALL EFOPSEQ(IUNIT,LGEOM,1,IER)
      IF(IER.LT.0)THEN
        write(outs,'(3a)') 'Geometry file ',LGEOM(1:lnblnk(LGEOM)),
     &      ' could not be opened.'
        if(dll)then
          dllsubr='EGOMIN'
          dllmesg=outs
          ier=2
          return
        else
          call edisp(itru,outs)
          IER=1
          RETURN
        endif
      ENDIF
      write(currentfile,'(a)') LGEOM(1:lnblnk(LGEOM))

C Read lines from file, if one item then assumed to 
C be CTYPE, if 2 then the second is the zone name which
C is tested for illegal characters.
      CALL STRIPC(IUNIT,OUTSTR,99,ND,1,'line 1',IER)
      IF(IER.NE.0)goto 1001
      K=0
      IF(ND.EQ.1)THEN
        CALL EGETW(OUTSTR,K,WORD,'W','CTYPE',IFLAG)
        CTYPE=WORD(1:3)
        zname(ICOMP)=' '
        zdesc(ICOMP)=' '
      ELSEIF(ND.EQ.2)THEN
        CALL EGETW(OUTSTR,K,WORD,'W','CTYPE',IFLAG)
        CTYPE=WORD(1:3)
        CALL EGETW(OUTSTR,K,WORD,'W','Z name',IFLAG)
        ZN=WORD(1:12)
        call st2name(ZN,zname(ICOMP))
        zdesc(ICOMP)=' '
      ELSEIF(ND.gt.2)THEN
        CALL EGETW(OUTSTR,K,WORD,'W','CTYPE',IFLAG)
        CTYPE=WORD(1:3)
        CALL EGETW(OUTSTR,K,WORD,'W','Z name',IFLAG)
        ZN=WORD(1:12)
        call st2name(ZN,zname(ICOMP))
        call egetrm(outstr,K,phrase,'W','Z description',IER)
        ZDESC(ICOMP)=phrase
      ENDIF
      IF(IFLAG.NE.0)GOTO 1001

C Fill in a default zone name and description if blank.
      if(zname(ICOMP)(1:2).EQ.'  '.OR.
     &     zname(ICOMP)(1:7).EQ.'UNKNOWN')then
        IF(ICOMP.LE.9)WRITE(zname(ICOMP),'(A5,I1)')'Zone-',ICOMP
        IF(ICOMP.GT.9)WRITE(zname(ICOMP),'(A5,I2)')'Zone-',ICOMP
      endif
      if(zdesc(ICOMP)(1:1).EQ.' ')then
        write(zdesc(ICOMP),'(a,a)') 
     &    zname(ICOMP)(1:lnblnk(zname(ICOMP))),' describes a...'
      endif

C Zone is of type REC, look for 7 items on one or two lines.
      IF(CTYPE.EQ.'REC')THEN
        CALL STRIPC(IUNIT,OUTSTR,99,ND,1,'line 2',IER)
        IF(IER.NE.0)goto 1001
        K=0
        CALL EGETWR(OUTSTR,K,XO,0.,0.,'-','X origin',IER)
        CALL EGETWR(OUTSTR,K,YO,0.,0.,'-','Y origin',IER)
        CALL EGETWR(OUTSTR,K,ZO,0.,0.,'-','Z origin',IER)

C If only 3 items on first line read another OUTSTR and try to continue.
        IF(ND.EQ.3) THEN
          K=0
          CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'line 2',IER)
          IF(IER.NE.0)goto 1001
        ENDIF
        CALL EGETWR(OUTSTR,K,DX,0.,0.,'-','length',IER)
        CALL EGETWR(OUTSTR,K,DY,0.,0.,'-','width',IER)
        CALL EGETWR(OUTSTR,K,DZ,0.,0.,'-','height',IER)
        CALL EGETWR(OUTSTR,K,AR,-360.,360.,'W','rotation angle',IER)

C Now convert to a gen description.
        CALL ERECC(XO,YO,ZO,DX,DY,DZ,AR)
        NZSUR(ICOMP)=NSUR
        NZTV(ICOMP)=NTV
        CTYPE='GEN'

C Zone is of type REG.
      ELSEIF(CTYPE.EQ.'REG')THEN
        CALL STRIPC(IUNIT,OUTSTR,4,ND,1,'line 2',IER)
        IF(IER.NE.0)goto 1001
        K=0
        CALL EGETWI(OUTSTR,K,NW,3,MS-2,'W','no of walls',IER)
        CALL EGETWR(OUTSTR,K,Z1,0.,1000.,'W','floor height',IER)
        CALL EGETWR(OUTSTR,K,Z2,Z1,1000.,'F','ceiling ht',IER)
        CALL EGETWR(OUTSTR,K,AR,-360.,360.,'W','rot angle',IER)

C Read base vertex data.
        DO 60 IW=1,NW
          CALL STRIPC(IUNIT,OUTSTR,2,ND,1,'vertex data',IER)
          IF(IER.NE.0)goto 1001
          K=0
          CALL EGETWR(OUTSTR,K,XX(IW),0.,0.,'-','Base X',IER)
          CALL EGETWR(OUTSTR,K,YY(IW),0.,0.,'-','Base Y',IER)
   60   CONTINUE

C Now convert to GEN body type and place into in common block.
        CALL EREGC(NW,Z1,Z2,XX,YY)
        IF(AR.LT.-.01.OR.AR.GT..01)then
          x1=X(1)
          y1=Y(1)
          CALL ESCROT(AR,x1,y1)
        endif
        NZSUR(ICOMP)=NSUR
        NZTV(ICOMP)=NTV
        CTYPE='GEN'

C Zone is of type GEN.
      ELSEIF(CTYPE.EQ.'GEN')THEN
        CALL STRIPC(IUNIT,OUTSTR,3,ND,1,'NTV NSUR AR',IER)
        IF(IER.NE.0)goto 1001
        K=0
        CALL EGETWI(OUTSTR,K,NTV,4,MTV,'F','no of vertices',IER)
        CALL EGETWI(OUTSTR,K,NSUR,3,MS,'F','no of surfaces',IER)
        NZSUR(ICOMP)=NSUR
        NZTV(ICOMP)=NTV
        CALL EGETWR(OUTSTR,K,AR,-360.,360.,'W','rot angle',IER)

C Read each vertex data line, strip any comments, see if 3 items and
C place in X(),Y(),Z().
        DO 62 I=1,NTV
          CALL STRIPC(IUNIT,OUTSTR,3,ND,1,'vertex data',IER)
          IF(IER.NE.0)goto 1001
          K=0
          CALL EGETWR(OUTSTR,K,X(I),0.,0.,'-','X coord',IER)
          CALL EGETWR(OUTSTR,K,Y(I),0.,0.,'-','Y coord',IER)
          CALL EGETWR(OUTSTR,K,Z(I),0.,0.,'-','Z coord',IER)
   62   CONTINUE

C Read vertex list for each surface, strip comments, begin by finding
C the number of expected vertices (first item on list).
        DO 10 I=1,NSUR
          CALL STRIPC(IUNIT,OUTSTR,99,ND,1,'vertex list',IER)
          IF(IER.NE.0)goto 1001
          IF(ND.GE.4)THEN
            K=0
            CALL EGETWI(OUTSTR,K,J,3,MV,'F','nb assoc vertices',IERV)
            NVER(I)=J

C Now proceed to read vertices on one or more lines.
            DO 12 KV=1,NVER(I)
              CALL EGETWI(OUTSTR,K,IVAL,0,MTV,'F','vertex',IERV)
              IF(IERV.NE.0) THEN
                call edisp(ITRU,' reading continuation line...')
                CALL STRIPC(IUNIT,OUTSTR,0,ND,0,'vertex XYZ',IER)
                IF(IER.NE.0)goto 1001
                K=0
                CALL EGETWI(OUTSTR,K,IVAL,0,MTV,'F','vertex',IERV)
              ENDIF
              IF(IERV.NE.0) GOTO 1001
              JVN(I,KV)=IVAL
   12       CONTINUE
          ENDIF
   10   CONTINUE

C Rotate if required.
        if(AR.LT.-.01.OR.AR.GT..01)then
          x1=X(1)
          y1=Y(1)
          CALL ESCROT(AR,x1,y1)
        endif
      ELSE
        write(outs,'(2a)') ' Geometry shape type illegal in ',
     &    LGEOM(1:lnblnk(LGEOM))
        if(dll)then
          dllsubr='EGOMIN'
          dllmesg=outs
          ier=2
          CALL ERPFREE(IUNIT,ios)
          return
        else
          call edisp(itru,outs)
          IER=1
          CALL ERPFREE(IUNIT,ios)
          RETURN
        endif
      ENDIF

C Read line of unused indices (one per surface) if any index is
C non zero then it represents a depreciated default window, warn the
C user and set NGL to 0.
      IRVA=NSUR
      CALL EGETWIA(IUNIT,IVA,IRVA,0,MG,'F','def window list',IER)
      DO 14 KS=1,NSUR
        NGL(KS)=0
   14 CONTINUE

C Read the window dimension details and advise user (if IVA non-zero).
      DO 16 KS=1,NSUR
        IF(IVA(KS).GT.0)THEN
          DO 18 KW=1,IVA(KS)
            CALL STRIPC(IUNIT,OUTSTR,99,ND,1,'window data',IER)
            IF(IER.NE.0)goto 1001
            IF(ND.GE.4)THEN
              K=0
              CALL EGETWR(OUTSTR,K,VALX,0.,0.,'-','win X off',IER)
              CALL EGETWR(OUTSTR,K,VALZ,0.,0.,'-','win Z off',IER)
              CALL EGETWR(OUTSTR,K,VALW,0.,100.,'W','win width',IER)
              CALL EGETWR(OUTSTR,K,VALH,0.,100.,'W','win ht',IER)
              call usrmsg(
     &  'A default window description has been detected. This is not',
     &  'supported, note its demensions and make an equiv surface.','W')
              write(outs,'(a,f6.3,a,f6.3,a,f6.3,a,f6.3)') 
     &          'Note def win: X off is ',VALX,' Z off is ',VALZ,
     &          ' width is ',VALW,' height is ',VALH 
              call edisp(itru,outs)
            ENDIF
   18     CONTINUE
        ENDIF 
   16 CONTINUE

C Read surface indentations (one per surface).
      IRVA=NSUR
      CALL EGETWRA(IUNIT,YY,IRVA,0.,1.,'W','surfaces recess',IER)
      DO 22 KS=1,NSUR
        Y0S(KS)=YY(KS)
   22 CONTINUE

C Read default insolation surface numbers. NN = insolation defining 
C index:  1; one plane, 2; two planes, 3; all planes (diffuse).
C IDPN defines the default plane numbers.
      CALL STRIPC(IUNIT,OUTSTR,4,ND,1,'insolation data',IER)
      IF(IER.NE.0)goto 1001
      K=0
      CALL EGETWI(OUTSTR,K,IV,1,3,'W','Def insol index',IER)
      NDP(ICOMP)=IV
      CALL EGETWI(OUTSTR,K,IV,0,NSUR,'W','1st recv surf',IER)
      IDPN(ICOMP,1)=IV
      CALL EGETWI(OUTSTR,K,IV,0,NSUR,'W','2nd recv surf',IER)
      IDPN(ICOMP,2)=IV
      CALL EGETWI(OUTSTR,K,IV,-1,NSUR,'W','3rd insol data',IER)
      IDPN(ICOMP,3)=IV
      IF(IR.EQ.1)THEN
        DO 40 I=1,3
          IX=0
          IF(NDP(ICOMP).EQ.1.AND.I.EQ.1)IX=1
          IF(NDP(ICOMP).EQ.2.AND.I.LE.2)IX=1
          if(IDPN(ICOMP,I).NE.-1.AND.IDPN(ICOMP,I).NE.0)then
            if(IDPN(ICOMP,I).LT.IX.OR.IDPN(ICOMP,I).GT.NSUR)then
              CALL USRMSG(' Nonexistent insolation plane!',OUTSTR,'W')
            endif
          endif
   40   CONTINUE
      ENDIF

C Surface attributes:
C 1st item - surface number ISN, 
C 2nd item - 12 char name SNAME()
C 3rd item is a string (4 char) indicating whether the surface is
C     OPAQ (opaque) TRAN (transparent) FICT (ficticious). 
C 4th item SOTF() is a string (4 char) indicating whether the surface 
C     is to be counted as a FLOR (floor), VERT (wall), CEIL (ceiling),
C     FURN (furniture) for purposes of floor area and daylighting.
C 5th item is the name of the construction ( 12 char).
C 6th item is a 15 char string indicating the environment faced by the
C     other side of the surface.  Recognized strings are: EXTERIOR 
C     IDENTICAL GROUND SIMILAR ADIBATIC OTHER_ZONE.  Any other string
C     is assumed to be the name of another zone in the simulation.
C If end of file then assume no names, do not error.

C Begin with default assumptions for each surface then overwrite
C this if user supplied information exists.
      CALL FILSUR(itru,ICOMP,0)

      DO 42 I=1,NSUR
        CALL STRIPC(IUNIT,OUTSTR,0,ND,0,'surface attributes',IER)
        IF(IER.EQ.2)THEN

C End of file sensed, however this is not an error at this point.
          IER=0
          GOTO 44
        ENDIF
        IF(IER.EQ.1)goto 1001

C Read in the various attributes as in the following line.  With this
C method both construction and surface names can have blanks.
C        1         2         3         4         5         6         7
C23456789012345678901234567890123456789012345678901234567890123456789012
C 1, Surf-1        OPAQ  VERT  UNKN         EXTERIOR       
        K=0
        CALL EGETWI(OUTSTR,K,ISN,1,NSUR,'F','surface no',IER)
        IF(IER.NE.0)GOTO 44

        SNAME(ICOMP,ISN)=OUTSTR(6:17)

        tmpsot=OUTSTR(20:23)
        IF(tmpsot.EQ.'OPAQ'.OR.tmpsot.EQ.'TRAN'.OR.
     &     tmpsot.EQ.'FICT')SOTF(ISN)=tmpsot

        tmpvfc=OUTSTR(26:29)
        IF(tmpvfc.EQ.'VERT'.OR.tmpvfc.EQ.'CEIL'.OR.
     &     tmpvfc.EQ.'FLOR'.OR.tmpvfc.EQ.'FURN'.OR.
     &     tmpvfc.EQ.'SLOP')SVFC(ISN)=tmpvfc

        SMLCN(ISN)=OUTSTR(32:43)
        SOTHER(ISN)=OUTSTR(45:59)
   42 CONTINUE

C Check if base area has been defined.
      CALL STRIPC(IUNIT,OUTSTR,99,ND,0,'base',IER)
      IF(IER.EQ.2)THEN

C End of file sensed before zone base area was defined. Estimate.
        checkbase=.true.
        lastlist=0
        IBASES(ICOMP,1)=0
        IBASES(ICOMP,2)=0
        IBASES(ICOMP,3)=0
        IBASES(ICOMP,4)=0
        IBASES(ICOMP,5)=0
        IBASES(ICOMP,6)=0
        ZBASEA(ICOMP)=0.00
        IER=0
        GOTO 44
      ENDIF

C Gather information about surfaces associated with floor area.
      IBASES(ICOMP,1)=0
      IBASES(ICOMP,2)=0
      IBASES(ICOMP,3)=0
      IBASES(ICOMP,4)=0
      IBASES(ICOMP,5)=0
      IBASES(ICOMP,6)=0
      ZBASEA(ICOMP)=0.00
      K=0
      CALL EGETWI(OUTSTR,K,IB1,0,MS,'-','base 1',IER)
      CALL EGETWI(OUTSTR,K,IB2,0,MS,'-','base 2',IER)
      CALL EGETWI(OUTSTR,K,IB3,0,MS,'-','base 3',IER)
      CALL EGETWI(OUTSTR,K,IB4,0,MS,'-','base 4',IER)
      if(ND.eq.6)then
        CALL EGETWI(OUTSTR,K,IB5,0,MS,'-','base 5',IER)
      elseif(ND.gt.6)then
        CALL EGETWI(OUTSTR,K,IB5,0,MS,'-','base 5',IER)
        CALL EGETWI(OUTSTR,K,IB6,0,MS,'-','base 6',IER)
      endif
      CALL EGETWR(OUTSTR,K,VAL,0.,99999.,'W','base area',IER)

C If there is an 8th item on the line it is the value for iuzbasea
C (user might have selected the list of surfaces assoc with base).
      if(ND.eq.8)then
        CALL EGETWI(OUTSTR,K,IUB,0,2,'-','base user flag',IER)
      else
        IUB=0
      endif
      IBASES(ICOMP,1)=IB1
      IBASES(ICOMP,2)=IB2
      IBASES(ICOMP,3)=IB3
      IBASES(ICOMP,4)=IB4
      IBASES(ICOMP,5)=IB5
      IBASES(ICOMP,6)=IB6

C If list is full of zeros and there is a no-zero user defined area
C set iuzbasea() to 1.
      if(ib1.eq.0.and.ib2.eq.0.and.ib3.eq.0.and.ib4.eq.0.and.ib5.eq.0
     &           .and.ib6.eq.0)then
        if(VAL.gt.0.00)then
          iuzbasea(icomp)=1
          ZBASEA(ICOMP)=VAL
        else
          iuzbasea(icomp)=0
          ZBASEA(ICOMP)=0.00
          if(ITRC.ne.0) call edisp(itru,
     &      'Zone base area description missing.')
        endif
      else
        if(iub.eq.0)then
          iuzbasea(icomp)=0
        elseif(iub.eq.2)then
          iuzbasea(icomp)=2
        endif
        ZBASEA(ICOMP)=VAL
      endif

C Now close geometry data file.
   44 CALL ERPFREE(IUNIT,ios)

C If base area has not yet been calculated, do this now and calculate 
C transparent area for surfaces connected to external type BC.
C Get surface areas via call to zinfo.
      call zinfo(iuout,sna,zoa,pazi,pelv,vol)
      ZTRANA(ICOMP)=0.
      do 43 ijj=1,NSUR
        if(checkbase)then
          if(SVFC(ijj).eq.'FLOR')then

C If surface `flor` not included in the list add it.
            lastlist=lastlist+1
            if(lastlist.le.6)then
              IBASES(ICOMP,lastlist)=ijj
              ZBASEA(ICOMP)=ZBASEA(ICOMP)+SNA(ijj)
            endif
          endif
        endif
        if (SOTF(ijj).eq.'TRAN'.and.SOTHER(ijj)(1:8).eq.'EXTERIOR') then
          ZTRANA(ICOMP)=ZTRANA(ICOMP)+SNA(ijj)
        endif
  43  continue
      if(checkbase)then
        write(outs,'(a,f6.2,3a)') 'Base area estimated at ',
     &     ZBASEA(ICOMP),'m^2 for ',zname(icomp),
     &    ' (probably an older file).'
        call edisp(itru,outs)
        if(lastlist.gt.0)iuzbasea(icomp)=0
      endif     

C Leave it to calling code to report on contents of zone geometry
C as extended reporting needs to know of the context of the zone.
C Close geometry data file before exiting.
      CALL ERPFREE(IUNIT,ios)
      RETURN

 1001 write(outs,'(3a)') 'Conversion error in...',OUTSTR(1:50),'...'
      if(dll)then
        dllsubr='EGOMIN'
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


C ************* EMKGEO 
C Generic routine to write a geometry file (GEN type) based on infor-
C mation currently held in common blocks G0 G1 G3 G4 G5.  It is 
C assumed that this information has been checked.
C GENFIL is the name of the file to be written to (any existing file
C by this name is overwritten).  
C ICOMP is the zone number, ATR=true attribute file ATR=false plain file.
C IR=1 range checking IR=0 no range checking, ITRC unit number for
C user output, IER=0 OK IER=1 problem. IWF = 3 create/overwrite,=4 check
C with user before overwriting.

      SUBROUTINE EMKGEO(IFILG,GENFIL,ICOMP,ITRU,iwf,IER)
      INCLUDE 'building.h'

      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      COMMON/G4/NDP(MCOM),IDPN(MCOM,3)
      COMMON/G5/SNAME(MCOM,MS),SOTF(MS),SMLCN(MS),SVFC(MS),SOTHER(MS)
      COMMON/precz/zname(MCOM),zdesc(MCOM)
      common/PREC17/ZBASEA(MCOM),IBASES(MCOM,6),IUZBASEA(MCOM)
      COMMON/GR3D07/Y0S(MS),Y0SS(MSSZ),Y0SE(MSEZ)

C Dummy array for default windows.
      dimension NGLD(MS)

      CHARACTER GENFIL*72,zname*12,zdesc*64,louts*248
      CHARACTER SNAME*12,SMLCN*12,SVFC*4,SOTF*4,SOTHER*15

      IER=0

C Clear dummy array.
      do 42 i=1,NSUR
        NGLD(i)=0
  42  continue

C Since GEN coords are nominally in site coordinates there is no need
C to have any rotation.
      AR=0.0

C Open any existing file by this name (ask user for confirmation to
C over-write) or create a new file.
      if(iwf.eq.4)then
        CALL EFOPSEQ(IFILG,GENFIL,4,IER)
      else
        CALL EFOPSEQ(IFILG,GENFIL,3,IER)
      endif
      IF(IER.LT.0)THEN
        IER=1
        RETURN
      ENDIF

      WRITE(IFILG,30,IOSTAT=ios,ERR=13)
     &  zname(ICOMP)(1:lnblnk(zname(ICOMP))),GENFIL(1:lnblnk(GENFIL))
  30  FORMAT('# geometry of ',a,' defined in: ',a)
      lz=lnblnk(zname(ICOMP))
      ld=lnblnk(zdesc(ICOMP))

C File is always a GEN type as this is representation in common.
      WRITE(IFILG,'(a3,2x,A,2x,a,2x,a)',IOSTAT=ios,ERR=13) 'GEN',
     &  zname(ICOMP)(1:lz),zdesc(ICOMP)(1:ld),'# type, name, descr'

C Vertex coordinates.
      WRITE(IFILG,'(I8,I8,F8.3,4x,A)',IOSTAT=ios,ERR=13)
     &  NTV,NSUR,AR,'# vertices, surfaces, rotation angle'
      WRITE(IFILG,32,IOSTAT=ios,ERR=13)
  32  FORMAT('#  X co-ord, Y co-ord, Z co-ord')
      DO 960 I = 1,NTV
        WRITE(IFILG,33,IOSTAT=ios,ERR=13)X(I),Y(I),Z(I),I
  33    FORMAT(1X,3F12.5,'  # vert ',I3)
960   CONTINUE

C Vertex list.
      WRITE(IFILG,34,IOSTAT=ios,ERR=13)
  34  FORMAT('# no of vertices followed by list of associated vert')
      DO 970 I = 1,NSUR
      WRITE(IFILG,5650,IOSTAT=ios,ERR=13)NVER(I),(JVN(I,J),J=1,NVER(I))
5650    FORMAT(1X,25(I3,','))
970   CONTINUE

C Unused index. Generate packed strings to write out on as many lines as reqd.
      WRITE(IFILG,'(a)',IOSTAT=ios,ERR=13) '# unused index'
      itrunc=1
      ipos=1
      do while (itrunc.ne.0)
        call ailist(ipos,nsur,ngld,MS,'C',louts,loutlen,itrunc)
        write(ifilg,'(1x,a)',IOSTAT=ios,ERR=14) louts(1:loutlen)
        ipos=itrunc+1
      end do

C Surfaces indentation. Call to arlist should generate long lines of
C packed text. Code should be good for any number of surfaces.
      WRITE(IFILG,'(a)',IOSTAT=ios,ERR=13) '# surfaces indentation (m)'
      itrunc=1
      ipos=1
      do while (itrunc.ne.0)
        call arlist(ipos,nsur,Y0S,MS,'C',louts,loutln,itrunc)
        write(ifilg,'(1x,a)',IOSTAT=ios,ERR=14) louts(1:loutln)
        ipos=itrunc+1
      end do

C Default internal insolation.
      WRITE(IFILG,'(I5,3I4,4x,a)',IOSTAT=ios,ERR=13)NDP(ICOMP),
     &  (IDPN(ICOMP,J),J=1,3),'# default insolation distribution'

C Surface attributes.  Begin by inserting default information
C if for some reason there are blanks remaining.
      WRITE(IFILG,41,IOSTAT=ios,ERR=13)
  41  FORMAT('# surface attributes follow: ',/,
     &  '# id  surface      geom  loc/  construction environment',/,
     &  '# no  name         type  posn  name         other side')
      DO 102 I=1,NSUR
        IF(SNAME(ICOMP,I)(1:LNBLNK(SNAME(ICOMP,I))).EQ.' ')THEN
          IF(I.LE.9)WRITE(SNAME(ICOMP,I),76)I
          IF(I.GT.9)WRITE(SNAME(ICOMP,I),77)I
   76     FORMAT('Surf-',I1)
   77     FORMAT('Surf-',I2)
        ENDIF
        IF(SOTF(I)(1:LNBLNK(SOTF(I))).EQ.' ')SOTF(I)='OPAQ'
        IF(SVFC(I)(1:LNBLNK(SVFC(I))).EQ.' ')SVFC(I)='UNKN'
        IF(SMLCN(I)(1:LNBLNK(SMLCN(I))).EQ.' ')SMLCN(I)='UNKN'
        IF(SOTHER(I)(1:LNBLNK(SOTHER(I))).EQ.' ')SOTHER(I)='UNKNOWN'
        WRITE(IFILG,75,IOSTAT=ios,ERR=13)I,SNAME(ICOMP,I),
     &        SOTF(I),SVFC(I),SMLCN(I),SOTHER(I)
   75   FORMAT(I3,', ',A12,2X,A4,2X,A4,2X,A12,1X,A15)
  102 CONTINUE

C Surfaces associated with base.
      WRITE(IFILG,'(a)',IOSTAT=ios,ERR=13)'# base'
      WRITE(IFILG,'(6i3,F9.2,i2)',IOSTAT=ios,ERR=13)IBASES(ICOMP,1),
     &  IBASES(ICOMP,2),IBASES(ICOMP,3),IBASES(ICOMP,4),
     &  IBASES(ICOMP,5),IBASES(ICOMP,6),ZBASEA(ICOMP),IUZBASEA(ICOMP)

   99 CALL ERPFREE(IFILG,ios)
      RETURN

C Error messages.
   13 if(IOS.eq.2)then
        CALL USRMSG(' No permission to write ',GENFIL,'W')
      else
        CALL USRMSG(' File write error in ',GENFIL,'W')
      endif
      IER=1
      GOTO 99
   14 if(IOS.eq.2)then
        CALL USRMSG(' No prmission to write array in ',GENFIL,'W')
      else
        CALL USRMSG(' Long arrary write error in ',GENFIL,'W')
      endif
      IER=1
      GOTO 99
      
      END

C ******************** ERECC 
C ERECC converts a REC (rectilinear) description into a GEN description.
C The X,Y, & Z coordinates of the 'Bottom left-hand' corner (when viewed
C from the south), the length (from the BL-HC towards the east), width,
C height of the enclosure and angle of rotation between the length side
C and due east (anticlockwise positive) are taken from the
C calling subroutine EGOMIN and converted into the common block G1.
      SUBROUTINE ERECC(XO,YO,ZO,DX,DY,DZ,A)
      INCLUDE 'building.h'
      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      PI = 4.0 * ATAN(1.0)
      R=PI/180.
      SA=SIN(A*R)
      CA=COS(A*R)
      NSUR=6
      DO 10 I=1,NSUR
        NVER(I)=4
   10 CONTINUE

C Assign bottom 4 points X(1) to X(4) etc. anticlockwise and assign the
C top 4 points X(5) to X(8) etc. again anticlockwise.  Point 1 is point
C X0,Y0,Z0 with point 5 is directly above.
      X(1)=XO
      X(2)=X(1)+(DX*CA)
      X(3)=X(2)-(DY*SA)
      X(4)=X(3)-(DX*CA)
      Y(1)=YO
      Y(2)=Y(1)+(DX*SA)
      Y(3)=Y(2)+(DY*CA)
      Y(4)=Y(3)-(DX*SA)
      DO 20 I=5,8
        X(I)=X(I-4)
        Y(I)=Y(I-4)
        Z(I-4)=ZO
        Z(I)=ZO+DZ
   20 CONTINUE

C Number the vertices in each face.  Vertical faces numbered anticlockwise
C from bottom left-hand point when face viewed from the outside.  Ceiling
C numbered anitclockwise from point 5 and when viewed from the outside.
C Floor numbered anticlockwise from point 1 when viewed from outside 
C (below).
      DO 30 I=1,NSUR
        IF(I.LE.4)THEN
          DO 40 J=1,4
            IF(J.LE.2)THEN
              IF(J.EQ.1)JVN(I,J)=I
              IF(J.NE.1)JVN(I,J)=I+1
              IF(I.EQ.4.AND.J.EQ.2)JVN(I,J)=1
            ELSE
              JVN(I,J)=8+I-J
              IF(I.EQ.4.AND.J.EQ.3)JVN(I,J)=5
            ENDIF
   40     CONTINUE
        ELSEIF(I.EQ.6)THEN
          JVN(I,1)=1
          DO 50 J=2,4
            K=6-J
            JVN(I,J)=K
   50     CONTINUE
        ELSEIF(I.EQ.5)THEN
          K=I-1
          DO 60 J=1,4
            K=K+1
            JVN(I,J)=K
   60     CONTINUE
        ENDIF
   30 CONTINUE
      NTV=8
      RETURN
      END

C ******************** CNVBLK 
C CNVBLK converts a REC (rectilinear) description into GB1 common block.
C The X,Y, & Z coordinates of the 'Bottom left-hand' corner (when viewed
C from the south), the length (from the BL-HC towards the east), width,
C height of the enclosure and angle of rotation between the length side
C and due east (anticlockwise positive) are converted.
      SUBROUTINE CNVBLK(XO,YO,ZO,DX,DY,DZ,A)
      COMMON/GB1/XB(12),YB(12),ZB(12),JVNB(6,4)
      PI = 4.0 * ATAN(1.0)
      R=PI/180.
      SA=SIN(A*R)
      CA=COS(A*R)
      NSB=6

C Assign bottom 4 points XB(1) to XB(4) etc. anticlockwise and assign the
C top 4 points XB(5) to XB(8) etc. again anticlockwise.  Point 1 is point
C X0,Y0,Z0 with point 5 is directly above.
      XB(1)=XO
      XB(2)=XB(1)+(DX*CA)
      XB(3)=XB(2)-(DY*SA)
      XB(4)=XB(3)-(DX*CA)
      YB(1)=YO
      YB(2)=YB(1)+(DX*SA)
      YB(3)=YB(2)+(DY*CA)
      YB(4)=YB(3)-(DX*SA)
      DO 20 I=5,8
        XB(I)=XB(I-4)
        YB(I)=YB(I-4)
        ZB(I-4)=ZO
        ZB(I)=ZO+DZ
   20 CONTINUE

C Number the vertices in each face anticlockwise from the bottom
C left-hand point when face viewed from the outside.  Top is
C numbered anitclockwise from point 5 and when viewed from the outside.
C base is numbered anticlockwise from point 1 when viewed from outside 
C (below).
      DO 30 I=1,NSB
        IF(I.LE.4)THEN
          DO 40 J=1,4
            IF(J.LE.2)THEN
              IF(J.EQ.1)JVNB(I,J)=I
              IF(J.NE.1)JVNB(I,J)=I+1
              IF(I.EQ.4.AND.J.EQ.2)JVNB(I,J)=1
            ELSE
              JVNB(I,J)=8+I-J
              IF(I.EQ.4.AND.J.EQ.3)JVNB(I,J)=5
            ENDIF
   40     CONTINUE
        ELSEIF(I.EQ.6)THEN
          JVNB(I,1)=1
          DO 50 J=2,4
            K=6-J
            JVNB(I,J)=K
   50     CONTINUE
        ELSEIF(I.EQ.5)THEN
          K=I-1
          DO 60 J=1,4
            K=K+1
            JVNB(I,J)=K
   60     CONTINUE
        ENDIF
   30 CONTINUE
      RETURN
      END

C ******************** EREGC 
C EREGC converts a REG (extruded) geometry description into a Gen form.
C The number of vertical walls, X & Y coords of the bottom corner of 
C each wall in an anticlockwise direction (defining a polygon in a plan
C view) as well as the floor & ceiling heights are passed to EREGC and
C the GEN description returned via common G1.
      SUBROUTINE EREGC(NW,Z1,Z2,XX,YY)
      INCLUDE 'building.h'
      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      DIMENSION XX(MS),YY(MS)

C Assign bottom points X(1) to X(NW) etc. anticlockwise and assign top
C points X(NW+1) to X(2*NW) etc. again anticlockwise.  Point 1 is point
C XX(1),YY(1),Z1 and point NW+1 is directly above.
      NSUR=NW+2
      DO 10 I=1,NW
        X(I)=XX(I)
        Y(I)=YY(I)
        Z(I)=Z1
   10 CONTINUE
      J1=NW+1
      J2=2*NW
      DO 20 I=J1,J2
        X(I)=X(I-NW)
        Y(I)=Y(I-NW)
        Z(I)=Z2
   20 CONTINUE

C Number the vertices in each face using the following conventions:
C Vertical surface are numbered anticlockwise from the bottom left-hand
C point when viewed from the outside; ceilings are numbered anticlockwise
C from point'NW+1' when viewed from the outside (above); floors are
C numbered anticlockwise from point 1 when viewed from the outside (below).
      J2=NSUR-1
      DO 30 I=1,NSUR
        IF(I.GE.J2)goto 1
        NVER(I)=4
        DO 40 J=1,4
          IF(J.GT.2)goto 2
          IF(J.EQ.1)JVN(I,J)=I
          IF(J.NE.1)JVN(I,J)=I+1
          IF(I.EQ.NW.AND.J.EQ.2)JVN(I,J)=1
          goto 40
    2     JVN(I,J)=NSUR+2+I-J
          IF(I.EQ.NW.AND.J.EQ.3)JVN(I,J)=NW+1
   40   CONTINUE
        goto 30

C In case the number of walls is greater than the number of
C edges that can be accommodated in a single floor or ceiling
C reduce NW.
    1   NTV=2*NW
        if(NW.gt.MV)then
          call edisp(iuout,
     &    'Reducing the number of edges in top/bottom (floor plan')
          call edisp(iuout,
     &    'extrusion had more walls than edges allowed.)')
          NVER(I)=MV
        else
          NVER(I)=NW
        endif
        IF(I.EQ.J2)goto 3
        JVN(I,1)=1
        DO 50 J=2,NVER(I)
          K=NW+2-J
          JVN(I,J)=K
   50   CONTINUE
        goto 30
    3   K=I-1
        DO 60 J=1,NVER(I)
          K=K+1
          JVN(I,J)=K
   60   CONTINUE
   30 CONTINUE
      RETURN
      END

C ******************** ESCROT 
C Rotate a zone by ANG degrees and return information via common G1.
C X1,Y1 is the point to rotate about.

      SUBROUTINE ESCROT(ANG,X1,Y1)
      INCLUDE 'building.h'
      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV

      PI = 4.0 * ATAN(1.0)
      A=-ANG*PI/180.
      CA=COS(A)
      SA=SIN(A)
      DO 10 I=1,NTV
        XXX=X(I)-X1
        YYY=Y(I)-Y1
        XR=XXX*CA+YYY*SA
        YR=YYY*CA-XXX*SA
        X(I)=XR+X1
        Y(I)=YR+Y1
   10 CONTINUE
      RETURN
      END


C ************* SURINFO 
C Display surface details and attributes in a tabular format. Make use 
C of information currently in Common blocks G1 G6 G7.
      SUBROUTINE SURINFO(ICOMP,ITRU,context)
      INCLUDE 'building.h'

      common/SFIG/NSIGFIG
      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      COMMON/G6/SSNAME(MCON),SSOTF(MCON),SSMLCN(MCON),SSVFC(MCON),
     &          SSOTHER(MCON),SSPARENT(MCON)
      COMMON/G7/SSNA(MCON),SSPAZI(MCON),SSPELV(MCON),SSPERIM(MCON),
     &          SSUREQN(MCON,4),SSURCOG(MCON,3),SSURVN(MCON,3)
      COMMON/precz/zname(MCOM),zdesc(MCOM)
      COMMON/C24/IZSTOCN(MCOM,MS)

      logical context

      CHARACTER zname*12,OUTSTR*124,zdesc*64,OTHSTR*30,SSPARENT*12
      CHARACTER SSMLCN*12,SSVFC*4,SSOTF*4,SSOTHER*15,SSNAME*12
      character SIGSTR*12

C Surface summary: print header, followed by surface information.
      call edisp(ITRU,' ')
      WRITE(OUTSTR,92)zname(ICOMP)(1:LNBLNK(zname(ICOMP))),ICOMP
   92 FORMAT(' A summary of the surfaces in ',a,'(',I2,') follows:')
      call edisp(ITRU,OUTSTR)
      call edisp(ITRU,' ')

      WRITE(OUTSTR,193)
  193 FORMAT(' Sur| Area  |Azim|Elev| surface    |geometry|',
     &       ' construction |environment')
      call edisp(ITRU,OUTSTR)
      WRITE(OUTSTR,194)
  194 FORMAT('    | m^2   |deg |deg | name       |type|loc|',
     &       ' name         |other side ')
      call edisp(ITRU,OUTSTR)
      if(.NOT.context)then
        DO 893 I=1,NSUR
          ic=izstocn(icomp,i)
 894      FORMAT(I3,2X,A7,F5.0,F5.0,1X,A12,1X,A4,1X,A4,1X,A12,2X,A13)
          call SIGFIG(SSNA(ic),NSIGFIG,RNO,SIGSTR,LSTR)
          WRITE(OUTSTR,894)I,SIGSTR(1:7),SSPAZI(ic),SSPELV(ic),
     &        SSNAME(ic),SSOTF(ic),SSVFC(ic),SSMLCN(ic),SSOTHER(ic)
          call edisp(ITRU,OUTSTR)
 893    CONTINUE
      else
        DO 793 I=1,NSUR
          ic=izstocn(icomp,i)
          call OTHERINFO(icomp,i,OTHSTR)
          call SIGFIG(SSNA(ic),NSIGFIG,RNO,SIGSTR,LSTR)
          WRITE(OUTSTR,794)I,SIGSTR(1:7),SSPAZI(ic),SSPELV(ic),
     &        SSNAME(ic),SSOTF(ic),SSVFC(ic),SSMLCN(ic),OTHSTR
 794      FORMAT(I3,2X,A7,F5.0,F5.0,1X,A12,1X,A4,1X,A4,1X,A12,1X,A)
          call edisp(ITRU,OUTSTR)
 793    CONTINUE
      endif
      call edisp(ITRU,' ')
      RETURN
      END

C **************** OTHERINFO 
C OTHERINFO returns an English description in OTHSTR of the boundary 
C condition at the other side of a surface (based on information in
C the connections file).
      SUBROUTINE OTHERINFO(izone,isurf,OTHSTR)
      INCLUDE 'building.h'
      COMMON/precz/zname(MCOM),zdesc(MCOM)
      COMMON/C1/NCOMP,NCON
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      COMMON/C20/NZSUR(MCOM),NZTV(MCOM)
      COMMON/C24/IZSTOCN(MCOM,MS)
      COMMON/G5/SNAME(MCOM,MS),SOTF(MS),SMLCN(MS),SVFC(MS),SOTHER(MS)
      CHARACTER OTHSTR*30,SST2*12,ZST2*12,zdesc*64
      CHARACTER zname*12,SOTHER*15,SNAME*12,SMLCN*12,SVFC*4,SOTF*4

      if(izone.ne.0.and.izone.le.NCOMP)then
        if(isurf.le.NZSUR(izone).and.isurf.ne.0)then
          icon=IZSTOCN(izone,isurf)
          IF(ICT(ICON).EQ.0)THEN
            WRITE(OTHSTR,'(a)') '||< external'
          ELSEIF(ICT(ICON).EQ.1)THEN
            if(IC2(ICON).eq.0.and.IE2(ICON).eq.0)then
              WRITE(OTHSTR,'(a)') '||< identical environment '
            else
              WRITE(OTHSTR,'(a,i3,a,i4,a)') '||< similar+- ',IC2(ICON),
     &        'dC &',IE2(ICON),'W rad'
            endif
          ELSEIF(ICT(ICON).EQ.2)THEN
            WRITE(OTHSTR,'(a,i3,a,i4,a)')   '||< constant @',IC2(ICON),
     &        'dC &',IE2(ICON),'W rad'
          ELSEIF(ICT(ICON).EQ.3)THEN
            SST2=SNAME(IC2(ICON),IE2(ICON))
            ZST2=zname(IC2(ICON))
            WRITE(OTHSTR,'(4a)')'||< ',SST2(1:LNBLNK(SST2)),':',
     &        ZST2(1:LNBLNK(ZST2))
          ELSEIF(ICT(ICON).EQ.4)THEN
            IF(IC2(ICON).GT.0)THEN
              WRITE(OTHSTR,'(a,i2)') '||< ground profile ',IC2(ICON)
            ELSEIF(IC2(ICON).EQ.-3)THEN
              WRITE(OTHSTR,'(a)') '||< 3D ground model'
            ELSE
              WRITE(OTHSTR,'(a,i2)') '||< user def grnd profile ',
     &          IE2(ICON)
            ENDIF
          ELSEIF(ICT(ICON).EQ.5)THEN
            WRITE(OTHSTR,'(a)') '||< adiabatic'

C BASESIMP begin.
          ELSEIF(ICT(ICON).EQ.6)THEN
            WRITE(OTHSTR,'(a,i3)') '||< BASESIMP config type ',IC2(ICON)
C BASESIMP end.

C CEN 13791 partition start.
          ELSEIF(ICT(ICON).EQ.7)THEN
            if(IC2(ICON).eq.0.and.IE2(ICON).eq.0)then
              WRITE(OTHSTR,'(a)') '||< Identical CEN 13791 '
            else
              WRITE(OTHSTR,'(a,i3,a,i4,a)') '||< CEN13791+- ',IC2(ICON),
     &        'dC &',IE2(ICON),'W rad'
            endif
C CEN 13791 partition end.
          ENDIF
        endif
      endif
      RETURN
      END

C ********************** ZINFOREP
C 'ZINFOREP' takes data from the zone geometry commons and produces
C a high level report channel (itu). It assumes that common blocks
C (G1,G7,PREC2,PREC2,PREC17,PREC20,C20) have been filled.
      SUBROUTINE ZINFOREP(itu,icomp)
      INCLUDE 'building.h'

      common/OUTIN/IUOUT,IUIN
      common/SFIG/NSIGFIG
      COMMON/G7/SSNA(MCON),SSPAZI(MCON),SSPELV(MCON),SSPERIM(MCON),
     &          SSUREQN(MCON,4),SSURCOG(MCON,3),SSURVN(MCON,3)
      COMMON/precz/zname(MCOM),zdesc(MCOM)
      COMMON/PREC2/VOL(MCOM)
      common/PREC17/ZBASEA(MCOM),IBASES(MCOM,6),IUZBASEA(MCOM)
      COMMON/C20/NZSUR(MCOM),NZTV(MCOM)
      COMMON/C24/IZSTOCN(MCOM,MS)
      character zname*12,zdesc*64
      character outstr*124,t10*10,t10a*10,T12*12,T12A*12

      zoa=0.0
      do 42 ij=1,nzsur(icomp)
        ic=izstocn(icomp,ij)
        zoa=zoa+ssna(ic)
  42  continue
      call edisp(itu,' ')

C Convert nsur,ntv,zvol etc into strings with no leading spaces.
      CALL INTSTR(NZSUR(icomp),t10,lna,IER)
      CALL INTSTR(NZTV(icomp),t10a,lnaa,IER)
      WRITE(outstr,'(3a,i2,5a)')' Zone ',
     &   zname(ICOMP)(1:LNBLNK(zname(ICOMP))),
     &   ' (',ICOMP,') is composed of ',t10(1:lna),' surfaces and ',
     &   t10a(1:lnaa),' vertices.'
      call edisp(itu,outstr)
      
C Report data to required number of significant figures.
      call SIGFIG(VOL(icomp),NSIGFIG,RNO,T12,LSTR)
      WRITE(outstr,'(3a)')' It encloses a volume of ',T12(1:LSTR),
     &   'm^3 of space, with a total surface'
      call edisp(itu,outstr)
      call SIGFIG(ZOA,NSIGFIG,RNO,T12,LSTR)
      call SIGFIG(ZBASEA(ICOMP),NSIGFIG,RNO,T12A,LSTRA)
      if(IUZBASEA(icomp).eq.0)then
        write(outstr,'(5a)') ' area of ',T12(1:LSTR),
     &     'm^2 & approx floor area of ',T12A(1:LSTRA),'m^2'
      elseif(IUZBASEA(icomp).eq.1)then
        write(outstr,'(5a)') ' area of ',T12(1:LSTR),
     &     'm^2 & user edited floor area of ',T12A(1:LSTRA),'m^2'
      elseif(IUZBASEA(icomp).eq.2)then
        write(outstr,'(5a)') ' area of ',T12(1:LSTR),
     &     'm^2 & user list floor area of ',T12A(1:LSTRA),'m^2'
      endif
      call edisp(itu,outstr)
      WRITE(outstr,'(1x,A)') zdesc(ICOMP)
      call edisp(itu,outstr)
      return
      end

C ********************** ZINFO
C 'ZINFO' takes data from the zone geometry commons G1,G2,G3 and returns
C the following parameters:
C SNA   - surface area of each polygon
C ZOA   - total surface area associated with zone.
C PAZI  - plane azimuth angle
C PELV  - plane elevation angle
C VOL   - zone bounded volume
C << IOUT is not used >>
      SUBROUTINE ZINFO(IOUT,SNA,ZOA,PAZI,PELV,VOL)
      INCLUDE 'building.h'

      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      DIMENSION SNA(MS),XSUM(MS),YSUM(MS),ZSUM(MS)
      DIMENSION PAZI(MS),PELV(MS)
      LOGICAL CLOSE

C Set area summations to zero & consider each surface of the zone in turn.
      PI = 4.0 * ATAN(1.0)
      R=PI/180.
      ZOA=0.
      DO 10 I=1,NSUR
        XS=0.
        YS=0.
        ZS=0.
        ZMAX=-1.E+10
        ZMIN=1.E+10

C Compute gross surface area
        ADDL=0.
        DO 20 J=1,NVER(I)
          K=J+1
          IF(J.EQ.NVER(I))K=1
          IP1=JVN(I,J)
          IP2=JVN(I,K)
          ZMAX=AMAX1(ZMAX,Z(IP1))
          ZMIN=AMIN1(ZMIN,Z(IP1))
          XS=XS+Y(IP1)*Z(IP2)-Z(IP1)*Y(IP2)
          YS=YS+Z(IP1)*X(IP2)-X(IP1)*Z(IP2)
          ZS=ZS+X(IP1)*Y(IP2)-Y(IP1)*X(IP2)
          ADDL=ADDL+SQRT((X(IP2)-X(IP1))**2+(Y(IP2)-Y(IP1))**2+
     &       (Z(IP2)-Z(IP1))**2)
   20   CONTINUE
        XSUM(I)=XS
        YSUM(I)=YS
        ZSUM(I)=ZS
        ZAREA=0.5*SQRT(XS*XS+YS*YS+ZS*ZS)

C Surface area is given by:
        SNA(I)=ZAREA

C Check for -ve surface area: windows wrongly specified.
        IF(SNA(I).LT.0.00001)THEN
          CALL USRMSG(' Window area must be < than face',' ','W')
          RETURN
        ENDIF

C And surface area summations by:
        ZOA=ZOA+SNA(I)
   10 CONTINUE

C This section computes the volume of any polyhedral zone by computing
C the algebraic sum of the volumes of a prism formed by joining the
C coordinate system origin point (0,0,0) with each vertex of each
C face in turn.   The prism volume of a face whose associated outward
C normal (when the face vertices are defined in an anticlockwise
C direction when viewed from 'outside') faces away from the origin is
C computed as a positive value.   Conversely a negative value is
C obtained when the outward normal to the face points towards the origin.
      V=0.0
      DO 60 K=1,NSUR
        JJ=JVN(K,1)
        PV=.16667*(X(JJ)*XSUM(K)+Y(JJ)*YSUM(K)+Z(JJ)*ZSUM(K))
        V=V+PV

C This section computes the azimuth and elevation angles of the outward
C facing normal for each plane in turn. The azimuth is the angle -
C measured from north (Y-axis) in degrees - clockwise positive.
C The elevation angle is the angle between the plane normal and the
C horizontal measured vertically in degrees.
        PAZI(K)=90.0
        IF(XSUM(K).LT.0.0)PAZI(K)=-90.0
        CALL ECLOSE(XSUM(K),0.0,0.0001,CLOSE)
        IF(CLOSE)PAZI(K)=0.0
        PELV(K)=90.0
        IF(ZSUM(K).LT.0.0)PELV(K)=-90.0
        CALL ECLOSE(ZSUM(K),0.0,0.0001,CLOSE)
        IF(CLOSE)PELV(K)=0.0
        CALL ECLOSE(YSUM(K),0.0,0.0001,CLOSE)
        IF(CLOSE)goto 61
        AZI=ATAN2(XSUM(K),YSUM(K))
        PAZI(K)=AZI/R
   61   SRX2Y2=SQRT(XSUM(K)*XSUM(K)+YSUM(K)*YSUM(K))
        IF(PAZI(K).LT.0.)PAZI(K)=PAZI(K)+360.
        CALL ECLOSE(SRX2Y2,0.0,0.0001,CLOSE)
        IF(CLOSE)goto 60
        ELV=ATAN2(ZSUM(K),SRX2Y2)
        PELV(K)=ELV/R
   60 CONTINUE
      VOL=ABS(V)

C Check for small volume.
      IF(VOL.LE.0.00001)THEN
        CALL USRMSG(' Negative or zero zone volume detected. Vertex', 
     &    ' information may be in error or surfaces incomplete.','W')
        RETURN
      ENDIF

      RETURN
      END

C ********************** ZGUPDATE
C 'ZGUPDATE' takes the geometry commons G1 and updates G7 & PREC2
C SSNA(MCON)    - surface area of each polygon
C SSPAZI(MCON)  - plane azimuth angle
C SSPELV(MCON)  - plane elevation angle
C SSPERIM(MCON)  - perimeter of each surface.
C SUREQN(MCON,4)- equation of each polygon
C SSURCOG(MCON,3) - vertex weighted COG of polygon,
C SSURVN(MCON,3)  - unit normal vector from COG of polygon.
C VOL(MCOM)     - zone bounded volume
      SUBROUTINE ZGUPDATE(itrc,icomp,ier)
      INCLUDE 'building.h'

      common/OUTIN/IUOUT,IUIN
      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      COMMON/G6/SSNAME(MCON),SSOTF(MCON),SSMLCN(MCON),SSVFC(MCON),
     &          SSOTHER(MCON),SSPARENT(MCON)
      COMMON/G7/SSNA(MCON),SSPAZI(MCON),SSPELV(MCON),SSPERIM(MCON),
     &          SSUREQN(MCON,4),SSURCOG(MCON,3),SSURVN(MCON,3)
      COMMON/C24/IZSTOCN(MCOM,MS)
      COMMON/PREC2/VOL(MCOM)
      DIMENSION XSUM(MS),YSUM(MS),ZSUM(MS)
      DIMENSION XX(MV),YY(MV),ZZ(MV),CG(3),EQN(4)
      LOGICAL CLOSE
      CHARACTER SSMLCN*12,SSVFC*4,SSOTF*4,SSOTHER*15,SSNAME*12
      character SSPARENT*12,outs*124

C ZGUPDATE assumes that the zone geometry file has been read prior to
C its invocation and that the read of the model configuration file has
C instanciated the G6 common block.

C Set area summations to zero & consider each surface of the zone in turn.
      ier=0
      PI = 4.0 * ATAN(1.0)
      R=PI/180.
      DO 10 I=1,NSUR

C Recover the connection associated with this zone and surface.
        icc=IZSTOCN(icomp,i)
        if(icc.eq.0)then
          write(outs,*) 'Zone ',icomp,' & surface ',i,
     &      ' did not resolve to a known item in connection list.'
          call edisp(iuout,outs)
          ier=3
          goto 10
        endif
        SSPERIM(icc)=0.0
        XS=0.
        YS=0.
        ZS=0.
        ZMAX=-1.E+10
        ZMIN=1.E+10

C Compute gross surface area
        ADDL=0.
        DO 20 J=1,NVER(I)
          K=J+1
          IF(J.EQ.NVER(I))K=1
          IP1=JVN(I,J)
          IP2=JVN(I,K)
          ZMAX=AMAX1(ZMAX,Z(IP1))
          ZMIN=AMIN1(ZMIN,Z(IP1))
          XS=XS+Y(IP1)*Z(IP2)-Z(IP1)*Y(IP2)
          YS=YS+Z(IP1)*X(IP2)-X(IP1)*Z(IP2)
          ZS=ZS+X(IP1)*Y(IP2)-Y(IP1)*X(IP2)
          ADDL=ADDL+SQRT((X(IP2)-X(IP1))**2+(Y(IP2)-Y(IP1))**2+
     &       (Z(IP2)-Z(IP1))**2)
   20   CONTINUE
        XSUM(I)=XS
        YSUM(I)=YS
        ZSUM(I)=ZS

C Compute surface area and check for negative or near zero area.
        ZAREA=0.5*SQRT(XS*XS+YS*YS+ZS*ZS)
        SSNA(icc)=ZAREA
        IF(SSNA(icc).LT.0.00001)THEN
          CALL USRMSG('ZGUPDATE: Surface is < 0.00001m^2!',
     &      'Other geometric properties may be incorrect.','W')
          ier=2
        ENDIF

C Compute perimeter of the surface.
        list=NVER(i)-1
        do 42 ii=1,list
          jj=JVN(i,ii)
          kk=JVN(i,ii+1)
          vdis=0.0
          vdis= crowxyz(x(jj),y(jj),z(jj),x(kk),y(kk),z(kk))
          SSPERIM(icc)=SSPERIM(icc)+vdis
  42    continue

C Link back to start vertex.
        jj=JVN(i,NVER(i))
        kk=JVN(i,1)
        vdis=0.0
        vdis= crowxyz(x(jj),y(jj),z(jj),x(kk),y(kk),z(kk))
        SSPERIM(icc)=SSPERIM(icc)+vdis
C Debug...
C        write(6,*) 'perimeter of ',ssname(icc),icc,' is ',ssperim(icc)

C Derive the equation of the polygon.
        DO 60 KK=1,NVER(i)
          XX(KK) = X(JVN(I,KK))
          YY(KK) = Y(JVN(I,KK))
          ZZ(KK) = Z(JVN(I,KK))
   60   CONTINUE
        N = NVER(i)
        call PLEQN(XX,YY,ZZ,N,CG,EQN,IERR)

C Update the G7 common block.
        SSUREQN(icc,1)=EQN(1)
        SSUREQN(icc,2)=EQN(2)
        SSUREQN(icc,3)=EQN(3)
        SSUREQN(icc,4)=EQN(4)
        SSURCOG(icc,1)=CG(1)
        SSURCOG(icc,2)=CG(2)
        SSURCOG(icc,3)=CG(3)
        SSURVN(icc,1)=CG(1)+EQN(1)
        SSURVN(icc,2)=CG(2)+EQN(2)
        SSURVN(icc,3)=CG(3)+EQN(3)
   10 CONTINUE

C This section computes the volume of any polyhedral zone by computing
C the algebraic sum of the volumes of a prism formed by joining the
C coordinate system origin point (0,0,0) with each vertex of each
C face in turn.   The prism volume of a face whose associated outward
C normal (when the face vertices are defined in an anticlockwise
C direction when viewed from 'outside') faces away from the origin is
C computed as a positive value.   Conversely a negative value is
C obtained when the outward normal to the face points towards the origin.
      V=0.0
      DO 62 K=1,NSUR
        icc=IZSTOCN(icomp,k)
        if(icc.eq.0)goto 62
        JJ=JVN(K,1)
        PV=.16667*(X(JJ)*XSUM(K)+Y(JJ)*YSUM(K)+Z(JJ)*ZSUM(K))
        V=V+PV

C This section computes the azimuth and elevation angles of the outward
C facing normal for each plane in turn. The azimuth is the angle -
C measured from north (Y-axis) in degrees - clockwise positive.
C The elevation angle is the angle between the plane normal and the
C horizontal measured vertically in degrees.
        SSPAZI(icc)=90.0
        IF(XSUM(K).LT.0.0) SSPAZI(icc)=-90.0
        CALL ECLOSE(XSUM(K),0.0,0.0001,CLOSE)
        IF(CLOSE) SSPAZI(icc)=0.0
        SSPELV(icc)=90.0
        IF(ZSUM(K).LT.0.0) SSPELV(icc)=-90.0
        CALL ECLOSE(ZSUM(K),0.0,0.0001,CLOSE)
        IF(CLOSE) SSPELV(icc)=0.0
        CALL ECLOSE(YSUM(K),0.0,0.0001,CLOSE)
        IF(CLOSE) goto 61
        AZI=ATAN2(XSUM(K),YSUM(K))
        SSPAZI(icc)=AZI/R
   61   SRX2Y2=SQRT(XSUM(K)*XSUM(K)+YSUM(K)*YSUM(K))
        IF(SSPAZI(icc).LT.0.) SSPAZI(icc)=SSPAZI(icc)+360.
        CALL ECLOSE(SRX2Y2,0.0,0.0001,CLOSE)
        IF(CLOSE) goto 63
        ELV=ATAN2(ZSUM(K),SRX2Y2)
        SSPELV(icc)=ELV/R
   63   if(ITRC.gt.1)then 
          write(outs,*) 'name azi elv m2 ',SSNAME(icc),SSPAZI(icc),
     &      SSPELV(icc),SSNA(icc)
          call edisp(iuout,outs)
          write(outs,*) 'SUREQN ',icc,SSUREQN(icc,1),SSUREQN(icc,2),
     &      SSUREQN(icc,3),SSUREQN(icc,4)
          call edisp(iuout,outs)
          write(outs,*) 'SURCOG ',SSURCOG(icc,1),SSURCOG(icc,2),
     &      SSURCOG(icc,3)
          call edisp(iuout,outs)
          write(outs,*) 'SURVN ',SSURVN(icc,1),SSURVN(icc,2),
     &      SSURVN(icc,3)
          call edisp(iuout,outs)
        endif
   62 CONTINUE
      ZVOL=ABS(V)

C Check for small volume.
      if(ZVOL.LE.0.00001)then
        CALL USRMSG(' Negative or zero zone volume detected. Vertex', 
     &    ' information may be in error or surfaces incomplete.','W')
        ier=4
      else
        VOL(icomp)=ZVOL
      endif

      RETURN
      END


C ******* suredgeadj
C Suredgeadj detects polygon edge associations (how many associated surfs etc.)
C Used to assist in drawing wireframe and for determining parent and child
c relationships. Fills common block G8. Assumes that the surface is
C in the current zones common blocks.
C act is '-' derive geometric data or 'c' also determine parent
      subroutine suredgeadj(itrc,act,izone,ier)
      INCLUDE 'building.h'

      common/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      COMMON/G6/SSNAME(MCON),SSOTF(MCON),SSMLCN(MCON),SSVFC(MCON),
     &          SSOTHER(MCON),SSPARENT(MCON)
      COMMON/G7/SSNA(MCON),SSPAZI(MCON),SSPELV(MCON),SSPERIM(MCON),
     &          SSUREQN(MCON,4),SSURCOG(MCON,3),SSURVN(MCON,3)
      COMMON/precz/zname(MCOM),zdesc(MCOM)

C nbedgdup(MCON) number of duplicate edges in surface edge list.
C iedgdup(MCON,MV) for each edge, the connection and edge duplicated.
C nbedgshr(MCON) number of surfaces (same orient) which share an edge:
C   if only one then it must be parent, if two then it might be the
C   case of a door so determine which is primary.
C iedgshr(MCON,MV) for each edge, the connection of surface (similarly
C   oriented) sharing an edge. Used to detect parent/child. Zero denotes
C   this does not apply.
C imatshr(MCON,MV) for each edge, the connection of surface (similarly
C   oriented) which has the same material. Zero denotes this does not
C   apply.used to enhance wire frame drawings of discritized zones (e.g.
C   where a surface such as a floor has been subdivided)
      common/G8/nbedgdup(MCON),iedgdup(MCON,MV),nbedgshr(MCON),
     &          iedgshr(MCON,MV),imatshr(MCON,MV)
      COMMON/C1/NCOMP,NCON
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      COMMON/C20/NZSUR(MCOM),NZTV(MCOM)
      COMMON/C24/IZSTOCN(MCOM,MS)

      CHARACTER zname*12,zdesc*64
      CHARACTER SSMLCN*12,SSVFC*4,SSOTF*4,SSOTHER*15,SSNAME*12
      character SSPARENT*12,outs*124,act*1,h*72
      logical closeazi,closeelv,foundone,ok,dok,same

C iwhich is local variable to sort most often referenced adjacent surf.
      dimension iwhich(MV),iwhichs(MV),iwhichc(MV),iwhichb(MV)

      if(izone.ne.0.and.izone.le.NCOMP)then
        do 43 isurf=1,NZSUR(izone)
          icc=izstocn(izone,isurf)
          if(icc.eq.0)then
            write(outs,'(a,i2,a,i3,a)') ' Zone ',izone,' surface ',
     &        isurf,' is an unknown connection, skipping suredgeadj.'
            call edisp(iuout,outs)
            ier=1
            return
          endif
          nbedgdup(icc)=0

C Confirm whether there are any duplicate references to vertices
c (indicates a polygon with a hole in it). Outer loop takes each
C edge in turn. Inner loop checks from next edge onwards. If a
C duplicate edge is found also remember it.
          do 40 m=1,NVER(isurf)
            jes=JVN(isurf,m)
            if(m.lt.NVER(isurf))then
              jee=JVN(isurf,m+1)
            else
              jee=JVN(isurf,1)
            endif
            im=m+1
            do 41 n=im,NVER(isurf)
              if(n.lt.NVER(isurf))then
                kes=JVN(isurf,n+1)
              else
                kes=JVN(isurf,1)
              endif
              kee=JVN(isurf,n)
C Debug...
C              write(6,*) 'edges for conn ',icc,' are ',
C     &          jes,jee,kes,kee
              if(jes.EQ.kes.AND.jee.EQ.kee)then
                nbedgdup(icc)=nbedgdup(icc)+2
                iedgdup(icc,m)=1
                iedgdup(icc,n)=1

C Debug...
                if(itrc.gt.0)then
                  write(outs,*) 'dupl edges for conn ',icc,' is ',
     &              iedgdup(icc,m),' m n ',m,n
                  call edisp(iuout,outs)
                endif
              endif
  41        continue
  40      continue

C Debug...
          if(itrc.gt.0)then
            write(outs,*) 'duplicate edges for conn ',icc,' ',
     &        ssname(icc)(1:lnblnk(ssname(icc))),
     &        ' is ',nbedgdup(icc)
            call edisp(iuout,outs)
          endif

C Step through the vertices of each edge in turn and check to see
C what other surfaces and materials are associated with edge. j & k
C are the vertices at the ends of the edge being tested. When the
C last vertext is reached point the end to the start point.
          do 46 ijj=1,MV
            iwhich(ijj)=0
            iwhichs(ijj)=0
            iwhichc(ijj)=0
  46      continue
          list=NVER(isurf)
          nbedgshr(icc)=0
          do 42 i=1,list
            iedgshr(icc,i)=0
            imatshr(icc,i)=0
            iwhich(i)=0
            j=JVN(isurf,i)
            if(i.lt.NVER(isurf))then
              k=JVN(isurf,i+1)
            else
              k=JVN(isurf,1)
            endif
            do 44 iosurf=1,NZSUR(izone)
              foundone=.false.
              icco=izstocn(izone,iosurf)
              if(iosurf.eq.isurf) goto 44

C For other surface, loop through each edge and check if they match.
C (by definition, they will be in reversed order so test jj against k).
C If an edge matches, test if other surface is facing the same way.
              listo=NVER(iosurf)
              do 45 mm=1,listo
                jj=JVN(iosurf,mm)
                if(mm.lt.NVER(iosurf))then
                  kk=JVN(iosurf,mm+1)
                else
                  kk=JVN(iosurf,1)
                endif
                if(j.eq.kk.and.k.eq.jj)then
C Debug...
                  if(itrc.gt.0)then
                    write(outs,*) 'edges for conns ',icc,' & ',icco,
     &                ' are ',j,k,' & ',jj,kk
                    call edisp(iuout,outs)
                  endif

C Test if azimuth are within two degrees. For the special case of the
C difference between the two being ~360deg call it closeazi
                  call eclose(SSPAZI(icc),SSPAZI(icco),2.0,closeazi)
                  if(.NOT.closeazi)then
                    zazi = abs(SSPAZI(icc)-SSPAZI(icco))
                    call eclose(zazi,360.0,2.0,closeazi)
                  endif
                  call eclose(SSPELV(icc),SSPELV(icco),2.0,closeelv)
                  if(closeazi.and.closeelv)then

C Point the current edge to the connection of the other surface.
                    iedgshr(icc,i)=icco
                    iwhich(i)=icco

C Check if same material (but ignore UNKNOWN).
                    if(SSMLCN(icc).eq.SSMLCN(icco))then
                      if(SSMLCN(icc)(1:4).ne.'UNKN')then
                        imatshr(icc,i)=icco
                      endif
                    endif

C Find out if this other surface has already been noticed (so as to 
C increment nbedgshr once even if other surface shares several edges).
                    if(.NOT.foundone)then
                      foundone=.true.
                      nbedgshr(icc)=nbedgshr(icc)+1
                    endif
                    goto 44
                  endif
                endif
  45          continue
  44        continue

C Debug...
            if(itrc.gt.0)then
              write(outs,*) 'iedgshr ',iedgshr(icc,i),
     &          ' shared construction ',imatshr(icc,i)
              call edisp(iuout,outs)
            endif
  42      continue

C Debug...
          if(itrc.gt.0)then
            write(outs,*) 'nbedgshr for ',
     &        ssname(icc)(1:lnblnk(ssname(icc))),
     &        ' is ',nbedgshr(icc)
            call edisp(iuout,outs)
          endif

C If most the edges are shared with other similarly facing surfaces
C then if act='c' check to see if one of them is a parent. Do
C this by sorting the iwhich array and then doing a frequency bin
C on the sorted data and taking the most often referenced connection.
C Usually when a door has been defined, it will include one edge to
C a surface which is not co-planer.
          if(act.eq.'c')then
            if(nbedgshr(icc).eq.NVER(isurf).or.
     &         nbedgshr(icc).eq.(NVER(isurf)-1))then
              KFLAG = -1
              call SORTI(iwhich,iwhichs,list,KFLAG)

C If all of iwhich is the same number excluding zero then use it.
              same=.true.
              do 442 im=1,list-1
                 if(iwhich(im).ne.0.and.iwhich(im+1).ne.0)then
                   if(iwhich(im).ne.iwhich(im+1)) same=.false.
                 endif
  442         continue
              if(same)then
                ibinval=iwhich(1)
              else

C Check if (other than zero) how many other surfaces are adjaent
C to this surface. If there are more than two adjacent surfaces
C then it cannot be a subsurface. Each unique gets stuffed into iwhichs
C and inner loop runs through these stored values to see is iwhich() is unique.
                do 445 imj=1,list
                  iwhichs(imj)=0
                  iwhichb(imj)=0
  445           continue
                idif=0
                do 446 im=1,list
                  itest=iwhich(im)
                  do 447 imm=1,list
                    if(iwhich(imm).eq.itest) iwhichb(im)=iwhichb(im)+1
  447             continue
  446           continue

C Debug...
C                write(6,*) 'iwhichb=',iwhichb

C Find out how many of the binns (iwhichb) are non-zero.
C Also find out which bin has the most entries.
                nidif=0
                midif=0
                do 448, immm=1,list
                  if(iwhichb(immm).gt.0)nidif=nidif+1
                  if(iwhichb(immm).gt.midif) midif=immm
  448           continue
                if(itrc.gt.0)then
                  write(outs,*)'nidif is ',nidif,' max @ array index',
     &              midif,' for connection ',icc
                  call edisp(iuout,outs)
                endif
                if(nidif.gt.2)then
C for testing.
C                if(nidif.gt.3)then
                  ibinval=0
                else
                  if(itrc.gt.0)then
                    write(outs,*)'primary adj surf is ',iwhich(midif)
                    call edisp(iuout,outs)
                  endif
                  ibinval=iwhich(midif)
                endif
              endif

C If current surface already marked and is the same as ibinval then do not
C bother the user. If ibinval was set to zero then clear SSPARENT.
              if(ibinval.ne.0)then
                if(SSPARENT(icc).eq.ssname(ibinval))then
                  ok=.true.
                else
                  write(outs,'(5a)') 'Is ',ssname(icc),
     &              ' a subsurface of ',ssname(ibinval),'?'
                  dok=.true.
                  h(1)='The shape & edges of the two surfaces suggest'
                  h(2)='that one is a subsurface. If not say no. '
                  call askok(outs,' ',ok,dok,2)
                endif
                if(ok)then
                  SSPARENT(icc)=ssname(ibinval)
                endif
              else
                dok=.false.
                h(1)='Is its parent surface in the current'
                h(2)='zone. If not say no. '
                izz=ic1(icc)
                write(outs,'(5a)') 'Does ',ssname(icc),
     &              ' have a parent surface in ',zname(izz),'?'
                call askok(outs,
     &            ' (saying no sets parent to blank)',ok,dok,2)
                if(ok)then
                  write(outs,'(5a)') 'Specify parent surface for ',
     &               ssname(icc),' in ',zname(izz),'.'
                  call easksur(izz,iss,'-',outs,' ',ier)
                  ibinval=izstocn(izz,iss)
                  if(ibinval.ne.0)then
                    SSPARENT(icc)=ssname(ibinval)
                  else
                    SSPARENT(icc)=' '
                  endif
                else
                  SSPARENT(icc)=' '
                endif
              endif
            endif
          endif
 43     continue
      endif
      return
      end

C ***************** ZDATA 
C ZDATA reads NZONES geometry and uses common blocks G1 G2 etc to fill
C common blocks ZNDATA so wireframes can be drawn without the
C need to re-read geometry files.
C VCOORD : X,Y & Z coordinates of each vertice in zone.
C NZNVER : number of vertices associated with each connection.
C NZJVN : topology of vertices associated with each connection.
      SUBROUTINE ZDATA(ITRC,IER,NZONES,ISFSUM)
      INCLUDE 'building.h'

      common/pophelp/h(60)
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL
      COMMON/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)
      COMMON/C20/NZSUR(MCOM),NZTV(MCOM)
      COMMON/C24/IZSTOCN(MCOM,MS)
      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      COMMON/G6/SSNAME(MCON),SSOTF(MCON),SSMLCN(MCON),SSVFC(MCON),
     &          SSOTHER(MCON),SSPARENT(MCON)

C SSURCOG is centre of gravity of surface (vertex weighted).
C SSUREQN is the surface equation A*X + B*Y + C*Z = D
C SSURVN is point 1 unit vectpr off of surface COG along the normal.
      COMMON/G7/SSNA(MCON),SSPAZI(MCON),SSPELV(MCON),SSPERIM(MCON),
     &          SSUREQN(MCON,4),SSURCOG(MCON,3),SSURVN(MCON,3)
      COMMON/RAY7/ZXMN(MCOM),ZYMN(MCOM),ZZMN(MCOM),ZXMX(MCOM),
     &            ZYMX(MCOM),ZZMX(MCOM),ZBFLG(MCOM)
      COMMON/ZNDATA/VCOORD(MCOM,MTV,3),NZNVER(MCON),NZJVN(MCON,MV)

      LOGICAL OK,dok
      CHARACTER*72 LSNAM,LPROJ,LGEOM,LSHAD,LTHRM,LUTIL
      CHARACTER SSNAME*12,SSMLCN*12,SSVFC*4,SSOTF*4,SSOTHER*15
      character SSPARENT*12,h*72,outs*124

C Assuming this routine is called in order: read in the geometry file
C and pass across into the appropriate array.
      DO 30 ICOMP=1,NZONES
   25   IER=0
        if(itrc.gt.1)then
          CALL USRMSG(' ',' Reading : '//LGEOM(ICOMP),'-')
        endif
        CALL EGOMIN(IFIL+1,LGEOM(ICOMP),ICOMP,1,ITRC,IUOUT,IER)
        IF(IER.NE.0)THEN
          dok=.true.
          h(1)='When scanning the zone geometry file to build up'
          h(2)='an overall list of surfaces a problem was encountered.'
          CALL ASKOK(' ',' Problem found... try again? ',OK,dok,2)
          IF(OK)goto 25
          goto 99
        ENDIF

C Find current zone geometric information and fill common G7 & PREC2
C NOTE: this was done in ersys call so it might not be needed here.
        itrc=0
        call zgupdate(itrc,icomp,ier)

C Find co-planer surfaces and edges of similar materials.
        call suredgeadj(itrc,'-',icomp,ier)

        ISFSUM=ISFSUM+ NZSUR(ICOMP)
        DO 40 J=1, NZTV(ICOMP)
          VCOORD(ICOMP,J,1)=X(J)
          VCOORD(ICOMP,J,2)=Y(J)
          VCOORD(ICOMP,J,3)=Z(J)
   40   CONTINUE
        DO 50 J=1, nzsur(ICOMP)
          icc=IZSTOCN(icomp,j)
          if(icc.ne.0)then
            NZNVER(icc)=NVER(J)
            N = NVER(J)
            DO 60 K=1,N
              NZJVN(icc,K)=JVN(J,K)
   60       CONTINUE
            if(ITRC.gt.1)then 
              write(outs,*) 'name azi elv m2 ',SSNAME(icc),SSPAZI(icc),
     &          SSPELV(icc),SSNA(icc)
              call edisp(iuout,outs)
              write(outs,*) 'SUREQN ',icc,SSUREQN(icc,1),SSUREQN(icc,2),
     &          SSUREQN(icc,3),SSUREQN(icc,4)
              call edisp(iuout,outs)
              write(outs,*) 'SURCOG ',SSURCOG(icc,1),SSURCOG(icc,2),
     &          SSURCOG(icc,3)
              call edisp(iuout,outs)
              write(outs,*) 'SURVN ',SSURVN(icc,1),SSURVN(icc,2),
     &          SSURVN(icc,3)
              call edisp(iuout,outs)
            endif
          endif
   50   CONTINUE
   30 CONTINUE

C Set to check zone bounds.
      INPIC=NZONES
      DO 42 I=1,INPIC
        ZBFLG(I)=0
  42  CONTINUE

      RETURN

   99 IER=1
      RETURN
      END

C ********************** FILSUR
C 'FILSUR' Fills the common block G5 with default assumptions.
C If ISUR=0 do all surfaces otherwise if ISUR>0 for one surface.
C FILSUR is called from << >>.
      SUBROUTINE FILSUR(itru,ICOMP,ISUR)
      INCLUDE 'building.h'

      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      COMMON/G5/SNAME(MCOM,MS),SOTF(MS),SMLCN(MS),SVFC(MS),SOTHER(MS)

      DIMENSION SNA(MS),PAZI(MS),PELV(MS)
      CHARACTER SNAME*12,SMLCN*12,SVFC*4,SOTF*4,SOTHER*15

C Begin with default assumptions for each surface then overwrite
C this if user supplied information exists.  Gather misc zone infor-
C mation so make a probable guess at locations.
      CALL ZINFO(ITRU,SNA,ZOA,PAZI,PELV,VOL)
      if(ISUR.eq.0)then
        DO 44 I=1,NSUR
          IF(I.LE.9)WRITE(SNAME(ICOMP,I),'(A5,I1)') 'Surf-',I
          IF(I.GT.9)WRITE(SNAME(ICOMP,I),'(A5,I2)') 'Surf-',I
          SOTF(I)='OPAQ'
          SMLCN(I)='UNKN'

C Guess at location based on elevation of surface.
          SVFC(I)='UNKN'
          IF(PELV(I).GE.-0.02.AND.PELV(I).LE.0.02)SVFC(I)='VERT'
          IF(PELV(I).GE.89.98.AND.PELV(I).LE.90.02)SVFC(I)='CEIL'
          IF(PELV(I).GE.-90.02.AND.PELV(I).LE.-89.98)SVFC(I)='FLOR'
          SOTHER(I)='UNKNOWN'
   44   CONTINUE
      elseif(ISUR.gt.0)then
        I=ISUR
        IF(I.LE.9)WRITE(SNAME(ICOMP,I),'(A5,I1)') 'Surf-',I
        IF(I.GT.9)WRITE(SNAME(ICOMP,I),'(A5,I2)') 'Surf-',I
        SOTF(I)='OPAQ'
        SMLCN(I)='UNKN'
        SVFC(I)='UNKN'
        IF(PELV(I).GE.-0.02.AND.PELV(I).LE.0.02)SVFC(I)='VERT'
        IF(PELV(I).GE.89.98.AND.PELV(I).LE.90.02)SVFC(I)='CEIL'
        IF(PELV(I).GE.-90.02.AND.PELV(I).LE.-89.98)SVFC(I)='FLOR'
        SOTHER(I)='UNKNOWN'
      endif

      RETURN
      END

C ******************** INSINFO 
C INSINFO provides an English description of default insolation 
C options chosen by the user in the geometry file based on 
C NDP(ICOMP) and IDPN(ICOMP,?) from common G4.
      SUBROUTINE INSINFO(ICOMP,ITRU)
      INCLUDE 'building.h'
      COMMON/G4/NDP(MCOM),IDPN(MCOM,3)
      COMMON/G5/SNAME(MCOM,MS),SOTF(MS),SMLCN(MS),SVFC(MS),SOTHER(MS)
      common/INDICS/IVF(MCOM),ISI(MCOM),IHC(MCOM),
     &              ITW(MCOM),ICGC(MCOM),IOBS(MCOM)
      CHARACTER OUTSTR*124,SNAME*12,SMLCN*12,SVFC*4,SOTF*4,SOTHER*15

C Print out information about the default insolation planes.
      call edisp(ITRU,' ')
      if(ISI(ICOMP).eq.1)then
        call edisp(ITRU,
     &' An hourly solar radiation distribution is used for this zone.')
        return
      endif
      IF(NDP(ICOMP).EQ.1)THEN
        WRITE(OUTSTR,'(3a)')' Solar radiation is focused on surface ',
     &    SNAME(ICOMP,IDPN(ICOMP,1)),'.'
        call edisp(ITRU,OUTSTR)
      ELSEIF(NDP(ICOMP).EQ.2.AND.IDPN(ICOMP,3).EQ.0)THEN
        WRITE(OUTSTR,'(5a)')' Solar radiation is focused on surfaces ',
     &   SNAME(ICOMP,IDPN(ICOMP,1)),' and ',SNAME(ICOMP,IDPN(ICOMP,2)),
     &   '.'
      ELSEIF(NDP(ICOMP).EQ.3)THEN
        call edisp(ITRU,
     &  ' All surfaces will receive diffuse insolation.')
      ENDIF
      RETURN
      END

C ********************** VERINFO 
C VERINFO prints two summaries, the first includes vertex coordinates
C and a list of surfaces associated with each vertex.  The second
C prints a list of surface numbers and the list of vertices which 
C define them.  Information is gathered from common block G1. 
      SUBROUTINE VERINFO(ITRU)
      INCLUDE 'building.h'
      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      DIMENSION ISASSO(MS)
      CHARACTER OUTSTR*124

      call edisp(ITRU,' ')
      call edisp(ITRU,
     &' The zone is defined by the following general polygon vertices:')
      call edisp(ITRU,
     &' Vertex|X-coord.| Y-coord.| Z-coord.|Associated surfaces     |')
      DO 8790 I=1,NTV
        IHIT=0
        DO 8792 IS=1,NSUR
          DO 8794 IV=1,NVER(IS)
            IF(I.EQ.JVN(IS,IV))THEN
              IHIT=IHIT+1
              ISASSO(IHIT)=IS
            ENDIF
 8794     CONTINUE
 8792   CONTINUE
        WRITE(OUTSTR,9993)I,X(I),Y(I),Z(I),(ISASSO(IH),IH=1,IHIT)
 9993   FORMAT(I5,3F10.3,'  ',10(I2,', '))
        call edisp(ITRU,OUTSTR)
 8790 CONTINUE

C Surfaces with associated vertices.
      call edisp(ITRU,' ')
      call edisp(ITRU,
     &' Each surface (polygon) is composed of vertices as follows:')
      call edisp(ITRU,
     &' Surface|No.of   | vertex list (anticlockwise viewed from ext)')
      call edisp(ITRU,'        |Vertices|')
      DO 9991 I=1,NSUR
        NV=NVER(I)
        WRITE(OUTSTR,9990)I,NV,(JVN(I,J),J=1,NV)
 9990   FORMAT(I6,I9,I5,40(',',I3))
        call edisp(ITRU,OUTSTR)
 9991 CONTINUE
      RETURN
      END

C ********************** ZINDEX 
C ZINDEX is passed a string (which might be a zone name or an index) and
C returns an index to the matching zone.  If no match then returns 0. 
      SUBROUTINE ZINDEX(STRING,index)
      INCLUDE 'building.h'
      COMMON/C1/NCOMP,NCON
      COMMON/precz/zname(MCOM),zdesc(MCOM)
      COMMON/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK
      LOGICAL        CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK
      CHARACTER zname*12,ZN*12,zdesc*64
      character*(*) STRING

C If ALL then return negative of NCOMP.
      if(STRING(1:3).eq.'ALL')then
        index = 0 - NCOMP
        return
      endif

C Loop through each of the zones and see if a match.
      index = 0
      lstr = lnblnk(STRING)
      do 42 i = 1, NCOMP
        ZN = zname(i)
        lzn = lnblnk(ZN)
        if(lstr.eq.lzn)then
          if(STRING(1:lstr).eq.ZN(1:lzn))then
            index = i
            return
          endif
        endif
42    continue

C If reached this point then STRING was not recognisable as a
C zone name.  See if UNKNOWN, if so present a list of zones.
      if(STRING(1:7).eq.'UNKNOWN')then
        IC=-1
 254    CALL EASKGEOF('Select a zone from the list.',CFGOK,IC,'-',IER)
        IF(IC.EQ.0.OR.IC.EQ.-1)then
          call usrmsg('Please select a zone... ',' ','W')
          goto 254
        else
          index = ic
          return
        endif
      else

C See if the string is actually a number.
        read(STRING,*,ERR=20)indext
        if(indext.eq.0)then
          IC=-1
 255      CALL EASKGEOF('Select a zone from list.',CFGOK,IC,'-',IER)
          IF(IC.EQ.0.OR.IC.EQ.-1)then
            call usrmsg('Please select a zone... ',' ','W')
            goto 255
          else
            index = ic
            return
          endif
        elseif(indext.le.NCOMP)then
          index = indext
          return
        else
          call USRMSG('index > number of zones...',STRING,'W')
          index = 0
          return
        endif
      endif

 20   CALL USRMSG('Did not understand zone id...',STRING,'W')
      index = 0
      return

      end

C ********************** SNAMDUP 
C SNAMDUP is passed a proposed surface name and checks to see that
C it is unique (thus OK = TRUE) if duplicate then OK = FALSE.
C If 'is' is passed as 0 then check all, otherwise exclude surface is. 
      SUBROUTINE SNAMDUP(STRING,icomp,is,ok)
      INCLUDE 'building.h'
      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      COMMON/G5/SNAME(MCOM,MS),SOTF(MS),SMLCN(MS),SVFC(MS),SOTHER(MS)
      CHARACTER SOTHER*15,SNAME*12,SMLCN*12,SVFC*4,SOTF*4
      character string*12,SN*12
      logical ok
      
      lstr=lnblnk(string)
      ok=.true.
      do 42 i=1,NSUR
        SN=SNAME(ICOMP,i)
        if(string(1:lstr).eq.SN(1:lnblnk(SN)))then
          if(is.eq.0)then
            ok=.false.
            return
          else
            if(i.ne.is)then
              ok=.false.
              return
            endif
          endif
        endif
  42  continue
      return
      end

C ******************** SURLIST ********************
C SURLIST takes an list of surfaces (lstsf) and builds a descriptive
C string (sdescr) to be used in headers.
C icomp is the zone, nbsur is the number of surfaces in the list,
C lstsf is the list of surface indices, sdescr is the string returned
C and length is its maximum length. 
C << G6 could be used here >>
      subroutine surlist(icomp,nbsur,lstsf,sdescr,length,ierr)
      INCLUDE 'building.h'
      COMMON/G5/SNAME(MCOM,MS),SOTF(MS),SMLCN(MS),SVFC(MS),SOTHER(MS)
      
      dimension lstsf(mcom,ms),iva(ms)
      CHARACTER SNAME*12,SMLCN*12,SVFC*4,SOTF*4,SOTHER*15
      CHARACTER sdescr*80,sn*12

C Find out overall length and build 1D array iva.
      length=0
      do 42 i=1,nbsur
        iva(i)=lstsf(icomp,i)
        lna=lnblnk(SNAME(icomp,lstsf(icomp,i)))
        length=length+lna+1
  42  continue
      if(length.lt.80)then

C Write out a packed string (space separated) of each surface name.
        sdescr=' '
        ix=1
        ixl=0
        do 43 i=1,nbsur
          sn=SNAME(icomp,lstsf(icomp,i))
          lna=max(1,lnblnk(sn))
          if(lna.eq.1)then
            ixl=ix
          else
            ixl=ix+(lna-1)
          endif
          write(sdescr(ix:ixl),'(a)',iostat=ios,err=1)sn(1:lna)
          if(i.lt.nbsur)then
            write(sdescr(ixl+1:ixl+1),'(a)') ' '
            ix=ix+lna+1
          else
            ix=ix+lna+1
          endif
  43    continue
        return
      else
        if(nbsur.eq.1)then
         WRITE(SDESCR,'(A)',iostat=ios,err=1) 
     &     SNAME(icomp,lstsf(icomp,1))
        elseif(nbsur.gt.1)THEN

C Write indices into one packed string sdescr (i.e. truncate).
          ipos=1
          call ailist(ipos,nbsur,iva,MS,'S',sdescr,loutlen,itrunc)
        endif
        return
      endif

   1  write(6,*) 'SURLIST: error writing surface names: ',sdescr
      ierr=1
      return
      END
