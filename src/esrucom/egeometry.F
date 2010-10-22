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
C           on common blocks G0 G1 G2 G2OPT G3 G4 G6 
C  GEOREAD: Reads V1.1 zone geometry data as ASCII strings, with or without
C           range checking and printed summary.
C GEOWRITE: Write a geometry file (GEN V1.1 type) based on infor-
C           mation currently held in common blocks G0 G1 G3 G4 G6.
C  ERECC:   Converts REC (rectilinear) into a G1 description.
C  ERECC3A: Converts REC (rectilinear with 2 rotations) into a G1 common block.
C  CNVBLK:  Converts REC (rectilinear) into GB1 common block.
C  CNVBLK3A:Converts REC (rectilinear with 2 rotations) into GB1 common block.
C  EREGC:   Converts REG into a GEN description.
C  ESCROT:  Rotate a zone by ANG degrees around point x1,y1.
C  SURINFO: Display surface details and attributes in a tabular format.
C  ZINFO:   Returns descriptive information zone geometry.
C  ZGUPDATE: takes the geometry common G1 and updates G7 & PREC2
C  SUREDGEADJ: detects polygon edge associations (how many assoc. surfs etc.)
C  SURREL: checks each surface I in a zone and relates it to
C          another surface J in that zone.
C  zdata:   scan zones to build master geometry structures.
C  FILSUR:  Fills the common block G5 with default assumptions.
C  INSINFO: English description of default insolation options.
C  VERINFO: Prints vertex/surface list summaries.
C  SURLIST: takes an list of surfaces (lstsf) returns descriptive
C           string (sdescr) to be used in headers.

 
C ******************** EGOMIN 
C EGOMIN reads zone geometry data from a user-constructed data
C file (LGEOMF). There are three levels of geometry data input corresponding
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
 
       SUBROUTINE EGOMIN(IUNIT,LGEOMF,ICOMP,IR,ITRC,ITRU,IER)
#include "building.h"

C geometry.h provides commons G0/G2/G4/prec2/prec17/precz/c20.
#include "geometry.h"
#include "esprdbfile.h"
#include "material.h"

C espriou.h provides current file.
#include "espriou.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN

C Include ishdirc common in legacy file read so that some inference
C can be made in case the data is saved to version 1.1 file.

C << add logic to check for existance of obstructions file and
C << shading file in subsequent read and then update the values
C << as appropriate

C SNAME (12 char) - surface name attribute. Other G5 common block
c variables are defined in geometry.h
C ZBASEA    - area of base (m^2)
C IBASES    - surfaces (up to 12) associated with base area. If all 0 then
C             base area has been supplied by the user.

C ZNAME (12 char) - the zone name (from geometry.h).
C ZDESC (64 char) - zone notes (from geometry.h).

C zbasea(),ibases(),iuzbasea(),izbaselist() are defined in geometry.h
      
C Save total transparent surface area for surfaces connected to external BC.
      common/PREC18/ZTRANA(MCOM)
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      COMMON/C24/IZSTOCN(MCOM,MS)

      COMMON/GR3D07/Y0S(MS),Y0SS(MSSZ),Y0SE(MSEZ)

C Error subroutine and error details for dll mode.
      common/dllerr/dllsubr,dllmesg

      DIMENSION XX(MS),YY(MS),IVA(MS)
      DIMENSION SNA(MS),PAZI(MS),PELV(MS)
      
      CHARACTER LGEOMF*72,WORD*20,loutstr*248
      CHARACTER tmpvfc*4,tmpsot*32,tother*15
      character ZN*12,phrase*64,outs*124,outs2*124
      character dllsubr*12,dllmesg*124
      character OTF*4,OPT*12 ! for use with parsemlcdesc
      logical dll,checkbase
      logical havec24
      real XO1,YO1,ZO1,DX1,DY1,DZ1 ! to prevent name clash with geometry.h
      integer lsn   ! length of currentfile
      integer iflag ! for read errors

      IER=0    ! initial values of read state
      iflag=0
      ZN=' '
      phrase=' '
      checkbase=.false.
      havec24=.false.

C Check if running in dll mode.
      call isadll(dll)

C Initialise geometry data file. and set currentfile.
      CALL EFOPSEQ(IUNIT,LGEOMF,1,IER)
      IF(IER.LT.0)THEN
        write(outs,'(3a)') 'Geometry file ',LGEOMF(1:lnblnk(LGEOMF)),
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
      write(currentfile,'(a)') LGEOMF(1:lnblnk(LGEOMF))
      
C Read lines from file, if one item then assumed to 
C be CTYPE, if 2 then the second is the zone name which
C is tested for illegal characters, if '*Geometry 1.1'
C the code should use georead to scan the file so return
C ier as a 3.
      call lstripc(IUNIT,loutstr,99,ND,1,'geo line 1',IER)
      IF(IER.NE.0)goto 1001
      if(loutstr(1:13).eq.'*Geometry 1.1')then
        K=13
        CALL EGETW(loutstr,K,WORD,'W','CTYPE',IFLAG)
        write(CTYPE(icomp),'(a)') WORD(1:lnblnk(WORD))
        zname(ICOMP)=' '
        CALL EGETW(loutstr,K,WORD,'W','Z name',IFLAG)
        ZN=WORD(1:12)
        call st2name(ZN,zname(ICOMP))
        lnzname(ICOMP)=lnblnk(zname(ICOMP))  ! update string length
        gversion(icomp) = 1.1   ! set the version number.
        CALL ERPFREE(IUNIT,ios)
        write(outs,'(3a)') 'Geometry file ',LGEOMF(1:lnblnk(LGEOMF)),
     &      ' was newer format than egeomin can read.'
        if(dll)then
          dllsubr='EGOMIN'
          dllmesg=outs
          ier=2
          return
        else
          call edisp(itru,outs)
          IER=2
          RETURN
        endif

      else

C We can deal with this format.
        K=0
        IF(ND.EQ.1)THEN
          CALL EGETW(loutstr,K,WORD,'W','CTYPE',IFLAG)
          write(CTYPE(icomp),'(a)') WORD(1:lnblnk(WORD))
          zname(ICOMP)=' '
          lnzname(ICOMP)=0
          zdesc(ICOMP)=' '
          lnzdesc(ICOMP)=0
        ELSEIF(ND.EQ.2)THEN
          CALL EGETW(loutstr,K,WORD,'W','CTYPE',IFLAG)
          write(CTYPE(icomp),'(a)') WORD(1:lnblnk(WORD))
          CALL EGETW(loutstr,K,WORD,'W','Z name',IFLAG)
          ZN=WORD(1:12)
          call st2name(ZN,zname(ICOMP))
          lnzname(ICOMP)=lnblnk(zname(ICOMP))  ! update string length
          zdesc(ICOMP)=' '
          lnzdesc(ICOMP)=0
        ELSEIF(ND.gt.2)THEN
          CALL EGETW(loutstr,K,WORD,'W','CTYPE',IFLAG)
          write(CTYPE(icomp),'(a)') WORD(1:lnblnk(WORD))
          CALL EGETW(loutstr,K,WORD,'W','Z name',IFLAG)
          ZN=WORD(1:12)
          call st2name(ZN,zname(ICOMP))

          lnzname(ICOMP)=lnblnk(zname(ICOMP))  ! update string length
          call egetrm(loutstr,K,phrase,'W','Z description',IER)
          ZDESC(ICOMP)=phrase
          lnzdesc(ICOMP)=lnblnk(phrase)  ! update the length of this string.
        ENDIF
        IF(IFLAG.NE.0)GOTO 1001
        gversion(icomp) = 1.0   ! set the version number.
      endif

C The file format is prior to 1.1 so continue scanning.
C Fill in a default zone name and description if blank.
      if(zname(ICOMP)(1:2).EQ.'  '.OR.
     &   zname(ICOMP)(1:7).EQ.'UNKNOWN')then
        IF(ICOMP.LE.9)WRITE(zname(ICOMP),'(A5,I1)')'Zone-',ICOMP
        IF(ICOMP.GT.9)WRITE(zname(ICOMP),'(A5,I2)')'Zone-',ICOMP
        lnzname(ICOMP)=lnblnk(zname(ICOMP))  ! update string length
      endif
      if(zdesc(ICOMP)(1:1).EQ.' ')then
        write(zdesc(ICOMP),'(2a)') 
     &    zname(ICOMP)(1:lnzname(ICOMP)),' describes a '
        lnzdesc(ICOMP)=lnblnk(zdesc(ICOMP))  ! update the length of this string.
      endif

C Zone is of type REC, look for 7 items on one or two lines.
      IF(CTYPE(icomp)(1:3).EQ.'REC')THEN
        call lstripc(IUNIT,loutstr,99,ND,1,'line 2',IER)
        IF(IER.NE.0)goto 1001
        K=0
        CALL EGETWR(loutstr,K,XO1,0.,0.,'-','rec X origin',IER)
        CALL EGETWR(loutstr,K,YO1,0.,0.,'-','rec Y origin',IER)
        CALL EGETWR(loutstr,K,ZO1,0.,0.,'-','rec Z origin',IER)
        zorigin(icomp,1)=XO1
        zorigin(icomp,2)=YO1
        zorigin(icomp,3)=ZO1

C If only 3 items on first line read another loutstr and try to continue.
        IF(ND.EQ.3) THEN
          K=0
          call lstripc(IUNIT,loutstr,0,ND,1,'line 2',IER)
          IF(IER.NE.0)goto 1001
        ENDIF
        CALL EGETWR(loutstr,K,DX1,0.,0.,'-','length',IER)
        CALL EGETWR(loutstr,K,DY1,0.,0.,'-','width',IER)
        CALL EGETWR(loutstr,K,DZ1,0.,0.,'-','height',IER)
        zsize(icomp,1)=DX1
        zsize(icomp,2)=DY1
        zsize(icomp,3)=DZ1
        CALL EGETWR(loutstr,K,AR1,-360.,360.,'W','rotation angle',IER)
        rotateit(ICOMP,1)=AR1   ! remember the pending rotation
        rotateit(ICOMP,2)=XO1   ! and the origin of the zone
        rotateit(ICOMP,3)=YO1
        nbwalls(ICOMP)=4        ! there are 4 walls in box

C Now convert to a gen description. Pass in critical dimensions
C and expect the data back via common G1. After this move the
C rotateit from pending to previous.
        CALL ERECC(XO1,YO1,ZO1,DX1,DY1,DZ1,AR1)
        NZSUR(ICOMP)=NSUR
        NZTV(ICOMP)=NTV
        CTYPE(icomp)='GEN '
        rotateit(ICOMP,4)=rotateit(ICOMP,1)   ! shift the pending rotation
        rotateit(ICOMP,5)=rotateit(ICOMP,2)   ! and the origin of the zone
        rotateit(ICOMP,6)=rotateit(ICOMP,3)
        rotateit(ICOMP,1)=0.0
        rotateit(ICOMP,2)=0.0
        rotateit(ICOMP,3)=0.0

C Zone is of type REG.
      ELSEIF(CTYPE(icomp)(1:3).EQ.'REG')THEN
        call lstripc(IUNIT,loutstr,4,ND,1,'line 2',IER)
        IF(IER.NE.0)goto 1001
        K=0
        CALL EGETWI(loutstr,K,NW,3,MS-2,'W','no of walls',IER)
        nbwalls(ICOMP)=NW
        CALL EGETWR(loutstr,K,Z1,0.,1000.,'W','floor height',IER)
        CALL EGETWR(loutstr,K,Z2,Z1,1000.,'F','ceiling ht',IER)
        zorigin(icomp,1)=Z1
        zorigin(icomp,2)=Z2
        zorigin(icomp,3)=0.0
        zsize(icomp,1)=0.0
        zsize(icomp,2)=0.0
        zsize(icomp,3)=0.0
        CALL EGETWR(loutstr,K,AR1,-360.,360.,'W','rot angle',IER)
        rotateit(ICOMP,1)=AR1   ! remmeber the pending rotation

C Read base vertex data.
        DO 60 IW=1,NW
          call lstripc(IUNIT,loutstr,2,ND,1,'vertex data',IER)
          IF(IER.NE.0)goto 1001
          K=0
          CALL EGETWR(loutstr,K,XX(IW),0.,0.,'-','Base X',IER)
          CALL EGETWR(loutstr,K,YY(IW),0.,0.,'-','Base Y',IER)
   60   CONTINUE
        rotateit(ICOMP,2)=XX(1)   ! and the origin of the zone
        rotateit(ICOMP,3)=YY(1)

C Now convert to GEN body type and place into in common block.
        CALL EREGC(NW,Z1,Z2,XX,YY)
        IF(AR1.LT.-.01.OR.AR1.GT..01)then
          x1=X(1)
          y1=Y(1)
          CALL ESCROT(AR1,x1,y1)
          rotateit(ICOMP,4)=rotateit(ICOMP,1)   ! shift the pending rotation
          rotateit(ICOMP,5)=rotateit(ICOMP,2)   ! and the origin of the zone
          rotateit(ICOMP,6)=rotateit(ICOMP,3)
          rotateit(ICOMP,1)=0.0
          rotateit(ICOMP,2)=0.0
          rotateit(ICOMP,3)=0.0
        endif
        NZSUR(ICOMP)=NSUR
        NZTV(ICOMP)=NTV
        CTYPE(icomp)='GEN '

C Zone is of type GEN.
      ELSEIF(CTYPE(icomp)(1:3).EQ.'GEN')THEN
        call lstripc(IUNIT,loutstr,3,ND,1,
     &       '# of verticies, surfices, rotation angle',IER)
        IF(IER.NE.0)goto 1001
        K=0

C Read verticies        
        CALL EGETWI(loutstr,K,NTV,4,MTV,'F','# of vertices',IER)

C Read surfaces
C (Shouldn't four planar surfaces be required to fully-bound a zone?)
        CALL EGETWI(loutstr,K,NSUR,3,MS,'F','# of surfaces',IER)
        nbwalls(ICOMP)=NSUR   ! remember so can work with META
        NZSUR(ICOMP)=NSUR
        NZTV(ICOMP)=NTV
        zorigin(icomp,1)=0.0
        zorigin(icomp,2)=0.0
        zorigin(icomp,3)=0.0
        zsize(icomp,1)=0.0
        zsize(icomp,2)=0.0
        zsize(icomp,3)=0.0

C Read rotation angle.  If there are enough items on the line also
C look for the rotation point.
        CALL EGETWR(loutstr,K,AR1,-360.,360.,'W','rotation angle',IER)
        rotateit(icomp,1)=AR1   ! remember pending rotation angle
        if(ND.gt.3)then
          CALL EGETWR(loutstr,K,AR1,0.,0.,'-','rotation point X',IER)
          rotateit(icomp,2)=AR1
          CALL EGETWR(loutstr,K,AR1,0.,0.,'-','rotation point Y',IER)
          rotateit(icomp,3)=AR1
        else
          rotateit(icomp,2)=0.0  ! assume site origin to rotate around
          rotateit(icomp,3)=0.0
        endif

C Read each vertex data line, strip any comments, see if 3 items and
C place in X(),Y(),Z().
        DO 62 I=1,NZTV(ICOMP)
          call lstripc(IUNIT,loutstr,3,ND,1,'vertex data',IER)
          IF(IER.NE.0)goto 1001
          K=0
          CALL EGETWR(loutstr,K,X(I),0.,0.,'-','X coord',IER)
          CALL EGETWR(loutstr,K,Y(I),0.,0.,'-','Y coord',IER)
          CALL EGETWR(loutstr,K,Z(I),0.,0.,'-','Z coord',IER)
   62   CONTINUE 

C Read vertex list for each surface, strip comments, begin by finding
C the number of expected vertices (first item on list).
        DO 10 I=1,NZSUR(ICOMP)
          
          call lstripc(IUNIT,loutstr,99,ND,1,'vertex list',IER)
          IF(IER.NE.0)goto 1001
          IF(ND.GE.4)THEN
            K=0
            CALL EGETWI(loutstr,K,J,3,MV,'F','nb assoc vertices',IERV)
            NVER(I)=J

C Now proceed to read vertices on one or more lines.
            DO 12 KV=1,NVER(I)
              CALL EGETWI(loutstr,K,IVAL,0,MTV,'F','vertex',IERV)
              IF(IERV.NE.0) THEN
                call edisp(ITRU,' reading continuation line...')
                call lstripc(IUNIT,loutstr,0,ND,0,'vertex XYZ',IER)
                IF(IER.NE.0)goto 1001
                K=0
                CALL EGETWI(loutstr,K,IVAL,0,MTV,'F','vertex',IERV)
              ENDIF
              IF(IERV.NE.0) GOTO 1001
              JVN(I,KV)=IVAL
   12       CONTINUE
          ENDIF
   10   CONTINUE

C Rotate if required. then shift pending to prior rotateit
        if(AR1.LT.-.01.OR.AR1.GT..01)then
          x1=X(1)
          y1=Y(1)
          CALL ESCROT(AR1,x1,y1)
          rotateit(ICOMP,4)=rotateit(ICOMP,1)   ! remmeber the pending rotation
          rotateit(ICOMP,5)=rotateit(ICOMP,2)   ! and the origin of the zone
          rotateit(ICOMP,6)=rotateit(ICOMP,3)
          rotateit(ICOMP,1)=0.0
          rotateit(ICOMP,2)=0.0
          rotateit(ICOMP,3)=0.0
        endif
      
      ELSE

C Unrecognized keyword.      
        write(outs,'(2a)') ' Geometry shape type illegal in ',
     &    LGEOMF(1:lnblnk(LGEOMF))
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
C non zero then it represents a depreciated default window.
      IRVA=NZSUR(ICOMP)
      CALL EGETWIA(IUNIT,IVA,IRVA,0,MG,'F','def window list',IER)

C Read the window dimension details and advise user (if IVA non-zero).
      
      DO 16 KS=1,NZSUR(ICOMP)
        IF( IVA(KS).GT.0 )THEN
          DO 18 KW=1,IVA(KS)
            call lstripc(IUNIT,loutstr,99,ND,1,'window data',IER)
            IF(IER.NE.0)goto 1001
            IF(ND.GE.4)THEN
              K=0
              CALL EGETWR(loutstr,K,VALX,0.,0.,'-','win X off',IER)
              CALL EGETWR(loutstr,K,VALZ,0.,0.,'-','win Z off',IER)
              CALL EGETWR(loutstr,K,VALW,0.,100.,'W','win width',IER)
              CALL EGETWR(loutstr,K,VALH,0.,100.,'W','win ht',IER)
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
      IRVA=NZSUR(ICOMP)
      CALL EGETWRA(IUNIT,YY,IRVA,0.,1.,'W','surfaces recess',IER)
      DO 22 KS=1,NZSUR(ICOMP)
        Y0S(KS)=YY(KS)
   22 CONTINUE

C Read default insolation surface numbers. NN = insolation defining 
C index:  1; one plane, 2; two planes, 3; all planes (diffuse).
C IDPN defines the default plane numbers.
      call lstripc(IUNIT,loutstr,4,ND,1,'insolation data',IER)
      IF(IER.NE.0)goto 1001
      K=0
      CALL EGETWI(loutstr,K,IV,1,3,'W','Def insol index',IER)
      NDP(ICOMP)=IV
      CALL EGETWI(loutstr,K,IV,0,NSUR,'W','1st recv surf',IER)
      IDPN(ICOMP,1)=IV
      CALL EGETWI(loutstr,K,IV,0,NSUR,'W','2nd recv surf',IER)
      IDPN(ICOMP,2)=IV
      CALL EGETWI(loutstr,K,IV,-1,NSUR,'W','3rd insol data',IER)
      IDPN(ICOMP,3)=IV
      IF(IR.EQ.1)THEN
        DO 40 I=1,3
          IX=0
          IF(NDP(ICOMP).EQ.1.AND.I.EQ.1)IX=1
          IF(NDP(ICOMP).EQ.2.AND.I.LE.2)IX=1
          if(IDPN(ICOMP,I).NE.-1.AND.IDPN(ICOMP,I).NE.0)then
            if(IDPN(ICOMP,I).LT.IX.OR.IDPN(ICOMP,I).GT.NSUR)then
              CALL USRMSG(' Nonexistent insolation plane!',loutstr,'W')
            endif
          endif
   40   CONTINUE
      ENDIF

C Surface attributes:
C 1st item - surface number ISN, 
C 2nd item - 12 char name SNAME()
C 3rd item is a string (32 char) indicating whether the surface is
C     OPAQ/OPAQAUE (opaque) TRAN/optical set name or CFC. 
C 4th item SOTF() is a string (4 char) indicating whether the surface 
C     is to be counted as a FLOR (floor), VERT (wall), CEIL (ceiling).
C 5th item is the name of the construction ( 32 char).
C 6th item is a 24*3 char string indicating the environment faced by the
C     other side of the surface. See notes about scanning conversion.
C If end of file then assume no names, do not error.

C Begin with default assumptions for each surface then overwrite
C this if user supplied information exists.

C Instantiate defaults for the surfaces to be read in.
      CALL FILSUR(ICOMP,0)

      icn1=izstocn(icomp,1)
      if(icn1.gt.0)then
C Debug.
C        call edisp(iuout,'Subsequent geometry scan...')
      endif

      DO 42 I=1,NZSUR(ICOMP)

C Check and see if izstocn returns non-zeros and set havec24.
        
        icn1=izstocn(icomp,I)
        if(icn1.gt.0)then
          havec24=.true.
        else
          havec24=.false.
        endif
        call lstripc(IUNIT,loutstr,0,ND,0,'surface attributes',IER)
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
        CALL EGETWI(loutstr,K,ISN,1,NSUR,'F','surface no',IER)
        IF(IER.NE.0)GOTO 44

C The value of I and ISN should be the same. If not warn.
        if(I.ne.ISN)then
          if(ITRC.ne.0) call edisp(itru,
     &      'Disagreement between surface index and position in list.')
        endif

        SNAME(ICOMP,ISN)=loutstr(6:17)
        if(havec24)then
          icn=izstocn(icomp,isn)
          ssname(icn)=loutstr(6:17)
        endif

C Interpret positional portion of the string.
        tmpvfc=loutstr(26:29)
        IF(tmpvfc.EQ.'VERT'.OR.tmpvfc.EQ.'SLOP')then
          write(SVFC(ISN),'(a)') tmpvfc
          if(havec24) write(SSVFC(icn),'(a)') tmpvfc
        ELSEIF(tmpvfc(1:4).EQ.'CEIL')then
          write(SVFC(ISN),'(a)') 'CEIL'
          if(havec24) write(SSVFC(icn),'(a)') 'CEIL'
        ELSEIF(tmpvfc(1:4).EQ.'FLOR')then
          write(SVFC(ISN),'(a)') 'FLOR'
          if(havec24) write(SSVFC(icn),'(a)') 'FLOR'
        ENDIF

C Interpret the construction name.
        write(SMLCN(ISN),'(a)') loutstr(32:43)

C Check if there is a matching MLC name and which one it is.
        ICF=-1
        if(NMLC.gt.0)then                
          lstr=lnblnk(SMLCN(ISN))
          DO 20 IC=1,NMLC
            if(lstr.lt.12)then
              IF(DESC(IC)(1:lstr).EQ.SMLCN(ISN)(1:lstr))ICF=IC
            else
              IF(DESC(IC)(1:12).EQ.SMLCN(ISN)(1:12))ICF=IC
            endif
  20      CONTINUE
        endif
        if(havec24)then
          write(SSMLCN(icn),'(a)') loutstr(32:43)
          if(ICF.gt.0)then
            ssthick(icn)=THKMLC(icf)

C Debug.
C            write(outs,'(a,2i3,f7.3)') 'set thick of ',icf,icn,
C     &        ssthick(icn)
C            call edisp(iuout,outs)
          endif
        endif

C Now that we know the construction try and work out the optics.
        write(tmpsot,'(a)') loutstr(20:23)
        if(tmpsot(1:4).eq.'OPAQ')then
          write(SOTF(ISN),'(a)') 'OPAQUE'  ! write OPAQUE
          if(havec24) write(SSOTF(icn),'(a)') 'OPAQUE' 
        elseif(tmpsot(1:4).eq.'CFC ')then
          write(SOTF(ISN),'(a)') 'CFC '    !write CFC
          if(havec24) write(SSOTF(icn),'(a)') 'CFC '
        elseif(tmpsot(1:4).ne.'OPAQ'.and.tmpsot(1:4).ne.'CFC ')then

C We need to find the name of the optical set for the construction.
C If there is a matching MLC entry attempt to discover
C the name of the optical property. Otherwise leave SOFT as TRAN
          if(NMLC.gt.0)then                
            if(ICF.gt.0)then
              OPT=' '
              WRITE(OTF,'(A)') DESC(ICF)(15:18)
              IF(OTF.EQ.' ')OTF='OPAQ'
              IF(OTF(1:4).NE.'OPAQ'.AND.OTF(1:4).NE.'CFC ')THEN
                WRITE(OPT,'(A)') DESC(ICF)(21:32)
                IF(OPT.EQ.' ')OPT='UNKNOWN'
              ELSE
                OPT='OPAQUE'
              ENDIF

            else

C No matching database record. Use if file specifies 'TRAN', use that.
C otherwise, use 'OPAQUE'
              user_specd_TRAN: if ( tmpsot(1:4).eq.'TRAN' ) then
C Use user-provided TRAN definition. Ensures that 
C the model can be read even if it contains optical 
C data that does not correspond with the optics databaes.
C [Note that several applications now stream ESP-r input 
C files independently of the optical databases, and must 
C be supported.]

                OPT='TRAN'

              else
              
C Definition is not 'TRAN' or 'OPAQ', default to OPAQUE.
C [An even better approach would produce a fatal error, since
C  the user --- or third-party app --- has used an unsupported 
C  keyword.]
                OPT='OPAQUE'
              
              endif user_specd_TRAN
              
            endif
            write(SOTF(ISN),'(a)') OPT
            if(havec24) write(SSOTF(icn),'(a)') OPT
          else
            write(SOTF(ISN),'(a)') 'TRAN'
            if(havec24) write(SSOTF(icn),'(a)') 'TRAN'
          endif
        endif
C Depending on what the 'other' column is file the SOTHER array.
C Note when scanning an older file we will not yet know the
C C3 based information and so sother(2) and sother(3) are placeholders.
        write(tother,'(a)') loutstr(45:59)
        if(tother(1:7).eq.'UNKNOWN')then
          write(sother(isn,1),'(a)') 'UNKNOWN'
          write(sother(isn,2),'(a)') '0'
          write(sother(isn,3),'(a)') '0'
          if(havec24)then
            write(ssother(icn,1),'(a)') 'UNKNOWN'
            write(ssother(icn,2),'(a)') '0'
            write(ssother(icn,3),'(a)') '0'
          endif
        elseif(tother(1:8).eq.'EXTERIOR')then
          write(sother(isn,1),'(a)') 'EXTERIOR'
          write(sother(isn,2),'(a)') '0'
          write(sother(isn,3),'(a)') '0'
          if(havec24)then
            write(ssother(icn,1),'(a)') 'EXTERIOR'
            write(ssother(icn,2),'(a)') '0'
            write(ssother(icn,3),'(a)') '0'
          endif
        elseif(tother(1:9).eq.'ADIABATIC')then
          write(sother(isn,1),'(a)') 'ADIABATIC'
          write(sother(isn,2),'(a)') '0'
          write(sother(isn,3),'(a)') '0'
          if(havec24)then
            write(ssother(icn,1),'(a)') 'ADIABATIC'
            write(ssother(icn,2),'(a)') '0'
            write(ssother(icn,3),'(a)') '0'
          endif
        elseif(tother(1:7).eq.'SIMILAR')then

C In this case the 2nd and 3rd values are not held in a legacy geometry
C file. And if this is not the first scan of the geometry file then
C the C24 and C3 data structures will have data that can be used.
          write(sother(isn,1),'(a)') 'SIMILAR'
          write(sother(isn,2),'(a)') '-'   ! must be filled from C3
          write(sother(isn,3),'(a)') '-'   ! must be filled from C3
          if(havec24)then
            write(ssother(icn,1),'(a)') 'SIMILAR'
            write(ssother(icn,2),'(i2.2)') IC2(icn)
            write(ssother(icn,3),'(i2.2)') IE2(icn)
            write(sother(isn,2),'(i2.2)') IC2(icn)
            write(sother(isn,3),'(i2.2)') IE2(icn)
          endif
        elseif(tother(1:8).eq.'CONSTANT')then

C In this case the 2nd and 3rd values are not held in a legacy geometry
C file. And if this is not the first scan of the geometry file then
C the C24 and C3 data structures will have data that can be used.
          write(sother(isn,1),'(a)') 'CONSTANT'
          write(sother(isn,2),'(a)') '-'   ! must be filled from C3
          write(sother(isn,3),'(a)') '-'   ! must be filled from C3
          if(havec24)then
            write(ssother(icn,1),'(a)') 'CONSTANT'
            write(ssother(icn,2),'(i2.2)') IC2(icn)
            write(ssother(icn,3),'(i2.2)') IE2(icn)
            write(sother(isn,2),'(i2.2)') IC2(icn)
            write(sother(isn,3),'(i2.2)') IE2(icn)
          endif
        elseif(tother(1:8).eq.'BASESIMP')then
          write(sother(isn,1),'(a)') 'BASESIMP'
          write(sother(isn,2),'(a)') '-'   ! must be filled from C3
          write(sother(isn,3),'(a)') '-'   ! must be filled from C3
          if(havec24)then
            write(ssother(icn,1),'(a)') 'BASESIMP'
            write(ssother(icn,2),'(i2.2)') IC2(icn)
            write(ssother(icn,3),'(i2.2)') IE2(icn)
            write(sother(isn,2),'(i2.2)') IC2(icn)
            write(sother(isn,3),'(i2.2)') IE2(icn)
          endif
        elseif(tother(1:6).eq.'GROUND')then
          write(sother(isn,1),'(a)') 'GROUND'
          write(sother(isn,2),'(a)') '-'   ! must be filled from C3
          write(sother(isn,3),'(a)') '-'   ! must be filled from C3
          if(havec24)then
            write(ssother(icn,1),'(a)') 'GROUND'
            write(ssother(icn,2),'(i2.2)') IC2(icn)
            write(ssother(icn,3),'(i2.2)') IE2(icn)
            write(sother(isn,2),'(i2.2)') IC2(icn)
            write(sother(isn,3),'(i2.2)') IE2(icn)
          endif
        elseif(tother(1:7).eq.'ANOTHER')then
          write(sother(isn,1),'(a)') 'ANOTHER'
          write(sother(isn,2),'(a)') '-'   ! must be filled from C3
          write(sother(isn,3),'(a)') '-'   ! must be filled from C3
          if(havec24)then
            write(ssother(icn,1),'(a)') 'ANOTHER'
            write(ssother(icn,2),'(i2.2)') IC2(icn)
            write(ssother(icn,3),'(i2.2)') IE2(icn)
            write(sother(isn,2),'(i2.2)') IC2(icn)
            write(sother(isn,3),'(i2.2)') IE2(icn)
          endif
        elseif(tother(1:9).eq.'IDENT_CEN')then
          write(sother(isn,1),'(a)') 'IDENT_CEN'
          write(sother(isn,2),'(a)') '0'
          write(sother(isn,3),'(a)') '0'
          if(havec24)then
            write(ssother(icn,1),'(a)') 'IDENT_CEN'
            write(ssother(icn,2),'(a)') '0'
            write(ssother(icn,3),'(a)') '0'
          endif
        else

C Assume partition so setup sother arrays with this in mind.
          write(sother(isn,1),'(a)') 'ANOTHER'
          write(sother(isn,2),'(a)') '-'   ! must be filled from C3
          write(sother(isn,3),'(a)') tother(1:lnblnk(tother))
          if(havec24)then
            write(ssother(icn,1),'(a)') 'ANOTHER'
            write(ssother(icn,2),'(i2.2)') IC2(icn)
            write(ssother(icn,3),'(i2.2)') IE2(icn)
            write(sother(isn,2),'(i2.2)') IC2(icn)
            write(sother(isn,3),'(i2.2)') IE2(icn)
          endif
        endif

C Usage is unknown so write place holder.
        write(suse(isn,1),'(a)') '-'
        write(suse(isn,2),'(a)') '-'
        if(havec24)then
          write(ssuse(icn,1),'(a)') '-'
          write(ssuse(icn,2),'(a)') '-'
        endif

C Child/parent unknown in legacy file so write place holder.
        write(sparent(isn),'(a)') '-'
        if(havec24)then
          write(ssparent(icn),'(a)') '-'
        endif
   42 CONTINUE

C Check if base area has been defined.
      call lstripc(IUNIT,loutstr,99,ND,0,'base',IER)
      IF(IER.EQ.2)THEN

C End of file sensed before zone base area was defined. Estimate.
        checkbase=.true.
        lastlist=0
        do 61 ibcount=1,12
          IBASES(ICOMP,ibcount)=0
  61    continue
        ZBASEA(ICOMP)=0.00
        IER=0
        GOTO 44
      ENDIF

C Gather information about surfaces associated with floor area.
      do 59 ibcount=1,12
        IBASES(ICOMP,ibcount)=0
  59  continue
      ZBASEA(ICOMP)=0.00
      K=0
      CALL EGETWI(loutstr,K,IB1,0,MS,'-','zn base 1st index',IER)
      CALL EGETWI(loutstr,K,IB2,0,MS,'-','zn base 2nd index',IER)
      CALL EGETWI(loutstr,K,IB3,0,MS,'-','zn base 3rd index',IER)
      CALL EGETWI(loutstr,K,IB4,0,MS,'-','zn base 4th index',IER)
      if(ND.eq.6)then
        CALL EGETWI(loutstr,K,IB5,0,MS,'-','zn base 5th index',IER)
      elseif(ND.gt.6)then
        CALL EGETWI(loutstr,K,IB5,0,MS,'-','zn base 5th index',IER)
        CALL EGETWI(loutstr,K,IB6,0,MS,'-','zn base 6th index',IER)
      endif
      CALL EGETWR(loutstr,K,VAL,0.,99999.,'W','zn base area m2',IER)

C If there is an 8th item on the line it is the value for iuzbasea
C (user might have selected the list of surfaces assoc with base).
      if(ND.eq.8)then
        CALL EGETWI(loutstr,K,IUB,0,2,'-','zn base area user flag',IER)
      else
        IUB=0
      endif
      lastlist=0
      IBASES(ICOMP,1)=IB1
      if(ibases(icomp,1).ne.0) lastlist=1
      IBASES(ICOMP,2)=IB2
      if(ibases(icomp,2).ne.0) lastlist=2
      IBASES(ICOMP,3)=IB3
      if(ibases(icomp,3).ne.0) lastlist=3
      IBASES(ICOMP,4)=IB4
      if(ibases(icomp,4).ne.0) lastlist=4
      IBASES(ICOMP,5)=IB5
      if(ibases(icomp,5).ne.0) lastlist=5
      IBASES(ICOMP,6)=IB6
      if(ibases(icomp,6).ne.0) lastlist=6

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
        izbaselist(icomp)=lastlist  ! remember how many items in list.
      else
        if(iub.eq.0)then
          iuzbasea(icomp)=0
        elseif(iub.eq.2)then
          iuzbasea(icomp)=2
        endif
        ZBASEA(ICOMP)=VAL
        izbaselist(icomp)=lastlist  ! remember how many items in list.
      endif

C Now close geometry data file.
   44 CALL ERPFREE(IUNIT,ios)

C If base area has not yet been calculated, do this now and calculate 
C transparent area for surfaces connected to external boundary conditions.
C Get surface areas via call to zinfo.
      call zinfo(sna,zoa,pazi,pelv,zvol)
      vol(icomp)=zvol
      zonetotsurfacearea(icomp)=zoa
      ZTRANA(ICOMP)=0.
      lastlist=0
      do 43 ijj=1,NZSUR(ICOMP)
        if(checkbase)then
          if(SVFC(ijj)(1:4).eq.'FLOR')then

C If surface `flor` not included in the list add it.
            lastlist=lastlist+1
            if(lastlist.le.6)then
              IBASES(ICOMP,lastlist)=ijj
              ZBASEA(ICOMP)=ZBASEA(ICOMP)+SNA(ijj)
            endif
          endif
        endif
        if (SOTF(ijj)(1:4).ne.'OPAQ'.and.SOTF(ijj)(1:4).ne.'CFC '.and.
     &      SOTHER(ijj,1)(1:8).eq.'EXTERIOR') then
          ZTRANA(ICOMP)=ZTRANA(ICOMP)+SNA(ijj)
        endif
  43  continue
      if(checkbase)then
        write(outs,'(a,f6.2,3a)') 'Base area estimated at ',
     &     ZBASEA(ICOMP),'m^2 for ',zname(icomp),
     &    ' (probably an older file).'
        call edisp(itru,outs)
        if(lastlist.gt.0)then
          iuzbasea(icomp)=0           ! signal area from orientation scan
          izbaselist(icomp)=lastlist  ! remember how many items in list.
        endif
      endif     

C Leave it to calling code to report on contents of zone geometry
C as extended reporting needs to know of the context of the zone.
C Close geometry data file before exiting.
      CALL ERPFREE(IUNIT,ios)
      RETURN

 1001 write(outs,'(3a)') 'EGOMIN: conversion error in...',
     &  loutstr(1:50),'...'
      lsn=MIN0(lnblnk(currentfile),110)
      write(outs2,'(2a)') 'in: ',currentfile(1:lsn)
      if(dll)then
        dllsubr='EGOMIN'
        dllmesg=outs
        ier=2
        CALL ERPFREE(IUNIT,ios)
        return
      else
        call edisp(iuout,outs)
        call edisp(iuout,outs2)
        IER=1
        CALL ERPFREE(IUNIT,ios)
        RETURN
      endif

      END


C ************* EMKGEO 
C Generic routine to write a geometry file (GEN type) based on infor-
C mation currently held in common blocks G0 G1 G3 G4 G6.  It is 
C assumed that this information has been checked.
C GENFIL is the name of the file to be written to (any existing file
C by this name is overwritten).  

C << consider how to move away from common G1 >>

      SUBROUTINE EMKGEO(IFILG,GENFIL,ICOMP,iwf,IER)
#include "building.h"

C geometry.h provides commons G0/G1/G2/G4/prec17/precz/c20.
#include "geometry.h"
      
      integer lnblnk  ! function definition

C Parameters
      integer IFILG        ! file unit
      character GENFIL*72  ! file name
      integer ICOMP        ! the zone number
      integer iwf          ! 3 create/overwrite, 4 confirm before overwriting.
      integer IER          ! IER 0 OK IER 1 problem

      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      COMMON/C24/IZSTOCN(MCOM,MS)

C NOTE: G1 is only valid for the zone that has currently been
C scanned and is being worked on. Common block ZNDATA
C holds this information globally. 

C << todo: emkgeo could use ZNDATA if there was a way for the
C << calling code to signal that ZNDATA could be used instead.

      COMMON/GR3D07/Y0S(MS),Y0SS(MSSZ),Y0SE(MSEZ)

C Dummy array for default windows.
      dimension NGLD(MS)

      CHARACTER louts*248,outs*124
      CHARACTER SO*15    ! to write out legacy
      logical newgeo  ! to use for testing if new/old geometry file.
      character msg*48

      IER=0

C Clear dummy array.
      do 42 i=1,NZSUR(icomp)
        NGLD(i)=0
  42  continue
      newgeo=.false.  ! assume older format geometry.

C Since GEN coords are nominally in site coordinates there is no need
C to have any rotation (only the newer version geometry file saves both
C the rotation angle and the point of rotation.
      AR=rotateit(icomp,1)

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

      msg='first comment line'
      WRITE(IFILG,30,IOSTAT=ios,ERR=13)
     &  zname(ICOMP)(1:lnzname(ICOMP)),GENFIL(1:lnblnk(GENFIL))
  30  FORMAT('# geometry of ',a,' defined in: ',a)
      lz=lnzname(ICOMP)

C File is always a GEN type as this is representation in common.
      msg='2nd line starting with GEN'
      WRITE(IFILG,'(a3,2x,A,2x,a,2x,a)',IOSTAT=ios,ERR=13) 'GEN',
     &  zname(ICOMP)(1:lz),zdesc(ICOMP)(1:lnzdesc(ICOMP)),
     &  '# type, name, descr'

C Vertex coordinates. << convert from G1 >>
      msg='3rd line with nb of surfs and vert'
      WRITE(IFILG,'(I8,I8,F8.3,4x,A)',IOSTAT=ios,ERR=13)
     &  NZTV(ICOMP),NZSUR(ICOMP),AR,
     &  '# vertices, surfaces, rotation angle'
      WRITE(IFILG,32,IOSTAT=ios,ERR=13)
  32  FORMAT('#  X co-ord, Y co-ord, Z co-ord')
      DO 960 I = 1,NZTV(ICOMP)
        msg='vertex data line'
        WRITE(IFILG,33,IOSTAT=ios,ERR=13)X(I),Y(I),Z(I),I
  33    FORMAT(1X,3F12.5,'  # vert ',I3)
960   CONTINUE

C Vertex list. << convert from G1 >>
      WRITE(IFILG,34,IOSTAT=ios,ERR=13)
  34  FORMAT('# no of vertices followed by list of associated vert')
      DO 970 I = 1,NZSUR(ICOMP)
      msg='edge list data line'
      WRITE(IFILG,5650,IOSTAT=ios,ERR=13)NVER(I),(JVN(I,J),J=1,NVER(I))
5650    FORMAT(1X,42(I3,','))
970   CONTINUE

C Unused index. Generate packed strings to write out on as many lines as reqd.
      WRITE(IFILG,'(a)',IOSTAT=ios,ERR=13) '# unused index'
      itrunc=1
      ipos=1
      do while (itrunc.ne.0)
        call ailist(ipos,nzsur(icomp),ngld,MS,'C',louts,loutlen,itrunc)
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
      msg='default insolation line'
      WRITE(IFILG,'(I5,3I4,4x,a)',IOSTAT=ios,ERR=13)NDP(ICOMP),
     &  (IDPN(ICOMP,J),J=1,3),'# default insolation distribution'

C Surface attributes.  Begin by inserting default information
C if for some reason there are blanks remaining.
      WRITE(IFILG,41,IOSTAT=ios,ERR=13)
  41  FORMAT('# surface attributes follow: ',/,
     &  '# id  surface      geom  loc/  construction environment',/,
     &  '# no  name         type  posn  name         other side')
      DO 102 I=1,NZSUR(icomp)
        icc=izstocn(icomp,i)
        IF(SSNAME(icc)(1:2).EQ.'  ')THEN
          IF(I.LE.9)WRITE(SSNAME(icc),76)I
          IF(I.GT.9)WRITE(SSNAME(icc),77)I
   76     FORMAT('Surf-',I1)
   77     FORMAT('Surf-',I2)
        ENDIF

C This version of the geometry does not have a slot for the full
C name of the optical property so if SOTF is not OPAQUE then
C set it to TRAN within the legacy geometry file.
        IF(SSOTF(icc)(1:2).EQ.'  ')then
          SSOTF(icc)='OPAQUE'
        ELSEIF(SSOTF(icc)(1:4).EQ.'OPAQ'.OR.
     &         SSOTF(icc)(1:4).EQ.'CFC ')then
          continue
        ELSE
          SSOTF(icc)='TRAN'
        endif
        IF(SSVFC(icc)(1:2).EQ.'  ')SSVFC(icc)='UNKN'
        IF(SSMLCN(icc)(1:2).EQ.'  ')SSMLCN(icc)='UNKN'
        IF(SSPARENT(icc)(1:2).EQ.'  ')SSPARENT(icc)='-'
        IF(SSUSE(icc,1)(1:2).EQ.'  ')SSUSE(icc,1)='-'
        IF(SSUSE(icc,2)(1:2).EQ.'  ')SSUSE(icc,2)='-'

C Based on the current contents of SOTHER(I,1) create a buffer SO
C to write out.
        IF(SSOTHER(icc,1)(1:2).EQ.'  ')then
          SO='UNKNOWN'
        ELSEIF(SSOTHER(icc,1)(1:8).EQ.'EXTERIOR')then
          SO='EXTERIOR'
        ELSEIF(SSOTHER(icc,1)(1:9).EQ.'ADIABATIC')then
          SO='ADIABATIC'
        ELSEIF(SSOTHER(icc,1)(1:7).EQ.'SIMILAR')then
          SO='SIMILAR'
        ELSEIF(SSOTHER(icc,1)(1:8).EQ.'CONSTANT')then
          SO='CONSTANT'
        ELSEIF(SSOTHER(icc,1)(1:8).EQ.'BASESIMP')then
          SO='BASESIMP'
        ELSEIF(SSOTHER(icc,1)(1:6).EQ.'GROUND')then
          SO='GROUND'
        ELSEIF(SSOTHER(icc,1)(1:9).EQ.'IDENT_CEN')then
          SO='IDENT_CEN'
        ELSEIF(SSOTHER(icc,1)(1:7).EQ.'ANOTHER')then
          SO='ANOTHER'
        ELSE
          if(IC2(icc).ne.0)then
            write(SO,'(a)') zname(IC2(icc))  ! write other zone name
          else
            SO='UNKNOWN'
          endif
        ENDIF
        msg='surface data line'
        WRITE(IFILG,75,IOSTAT=ios,ERR=13)I,SSNAME(icc),
     &        SSOTF(icc)(1:4),SSVFC(icc),SSMLCN(icc)(1:12),SO
   75   FORMAT(I3,', ',A12,2X,A4,2X,A4,2X,A12,1X,A15)
  102 CONTINUE

C Surfaces associated with base.
      msg='base area data line'
      WRITE(IFILG,'(a)',IOSTAT=ios,ERR=13)'# base'
      WRITE(IFILG,'(6i3,F9.2,i2)',IOSTAT=ios,ERR=13)IBASES(ICOMP,1),
     &  IBASES(ICOMP,2),IBASES(ICOMP,3),IBASES(ICOMP,4),
     &  IBASES(ICOMP,5),IBASES(ICOMP,6),ZBASEA(ICOMP),IUZBASEA(ICOMP)

   99 CALL ERPFREE(IFILG,ios)

C If file written with this source and the version number is 1.1
C then reset it to 1.0.
      call eclose(gversion(icomp),1.1,0.01,newgeo)
      if(newgeo) gversion(icomp) = 1.0
      RETURN

C Error messages.
   13 if(IOS.eq.2)then
        write(outs,'(2a)')'No permission to write ',msg(1:lnblnk(msg))
        CALL USRMSG(outs,GENFIL,'W')
      else
        write(outs,'(2a)')'File write error in ',msg(1:lnblnk(msg))
        CALL USRMSG(outs,GENFIL,'W')
      endif
      IER=1
      GOTO 99
   14 if(IOS.eq.2)then
        CALL USRMSG('No prmission to write array in ',GENFIL,'W')
      else
        CALL USRMSG('Long arrary write error in ',GENFIL,'W')
      endif
      IER=1
      GOTO 99
      
      END
 
C ******************** GEOREAD 
C GEOREAD reads V1.1 zone geometry data (LGEOMF) from a user-constructed data
C file. Data input is based on an extended GENeral (GEN) shaped zones.

C GEN - any polyhedral-shaped zone comprised of 'M' planes.

C Zone geometry input requirements vary depending on the zone 
C shape relative to some aebitrary site cartesian coordinate system.

C Geometry requirements are:

C GEN - the total number of vertices in the body, the number of
C       surfaces, the X, Y and Z coordinates of each vertex (in any
C       convenient order), the number of vertices in each surface,
C       an associated ordered list of the vertex numbers in an
C       anticlockwise order which comprise each surface and the
C       rotation angle from the site X-axis to east (anticlockwise
C       +ve).

C Also now can include a token to remember users prior rotateit request.

      SUBROUTINE GEOREAD(IUNIT,LGEOMF,ICOMP,IR,ITRU,IER)
#include "building.h"
#include "model.h"

C geometry.h provides commons G0/G1/G2/G4/G5/prec2/prec17/precz/c20/GS5/GS6.
#include "geometry.h"
#include "esprdbfile.h"
#include "material.h"

C espriou.h provides currentfile.
#include "espriou.h"
      
      integer lnblnk  ! function definition

C Parameters
      integer IUNIT  ! file unit to read
      integer ICOMP  ! zone number
      integer IR     ! IR 1 range checking is enabled, otherwise only minimal checking
      integer ITRU   ! file unit for feedback
      integer IER    ! zero is ok

      integer iuout,iuin
      common/OUTIN/IUOUT,IUIN

C iaplic(1) toggle for shading; iaplic(2) toggle for insolation.
C   toggle for assessment where:
C   1 is 'all_applicable', 0 is manual selection of surfaces.
C nsurfcalc nb of shaded surfaces, lstsfcalc() list of applicable surfaces.
C nsurfinso nb of insolation sources, isurfinso() list of insolation sources.
      common/ishdirec/iaplic(MCOM,2),nsurfcalc(MCOM),lstsfcalc(MCOM,MS),
     &     nsurfinso(MCOM),isurfinso(MCOM,MS)

C SNAME (12 char) - surface name attribute
C SOTF (32 char) - surface attribute indicating OPAQUE or an optical property.
C SVFC (4 char) - string indicating whether the surface is to be
C     counted as a FLOR (face up), VERT (wall), CEIL (face down),
C     SLOP (not vertical) for purposes of floor area and daylighting.
C 
C SMLCN (32 char) surface construction attribute and other G5 common block
C variables are defined in geometry.h  Some additonal information on the
C variables SSUSE and SUSE follow:

C    Surfaces which function as opaque portions of a facade are
C    marked as `WALL`. If they are vertical they will be composed
C    of a code complient construction. If they are not vertical
C    e.g. within 20 degree of vertical they could be composed of
C    a code-complent roofing material. The 2nd array index is not
C    used.

C    The first array index for the case of doors is as follows:
C    `DOOR` marks a normal door (inside or outside) which might be
C       altered to conform to building regulations.
C    'P-DOOR' marks a door for personel egress (e.g. a fire door) which
C       has specific code requirments
C    'H-DOOR' marks an entrance or other high-usage door.
C    'V-DOOR' marks a door used for vehicle access
C    The 2nd array index for the case of doors indicates the users
C    preference for treating air movement and the allowed tags are:
C    CLOSED UNDERCUT  OPEN  BIDIR

C    Thie first array index for a frame associated with a door or window:
C    `FRAME` marks a frame associated with an inside door or glazing,
C    'F-FRAME' marks a frame within the facade which might need to be
C       of a paricular area or construction for code compliance.
C    The 2nd array index for frames is CLOSED CRACK OPEN

C    The first array index for a glazing:
C    'C-WINDOW' is a code complient window (e.g.construction or size)
C    'D-WINDOW' is a display window which is not typically resized to
C       meet code compliance
C   .'S-WINDOW' is a code complient skylight (e.g.construction or size) 
C    'WINDOW' any other window inside or exterior
C    The 2nd array index for frames is CLOSED CRACK OPEN SASH BIDIR

C    The first array index for a surface which the user wishes to 
C    mark as an air supply or extract is 'GRILL':
C    The 2nd array index for grills is SOURCE EXTRACT

C    A suface which represents a fire smoke vent location is marked
C    with 'SMOKEVNT' and the 2nd array index is un-used. For code
C    complience such surfaces would be converted to the relevant
C    wall or roof construction.

C    The opaque portion of roofs are maked as 'ROOF' and the 2nd
C    array index is 'FLAT' (0-20 degree incline) or 'PITCHED'. Note
C    that a wall which is not vertical may be converted to a roof
C    construction type.

C    Surfaces which `are-not-there` e.g. fictitious are marked 'FICT'
C    and the 2nd array index can be: CLOSED CRACK OPEN BIDIREC

C    Surfaces which represent internal or ground-contact floors are
C    marked with 'FLOOR' and the 2nd index is 'EXTGRND' or '- ' to
C    signal ground connection or internal.  A raised floor above a
C    ground connected crawl-space is ambiguious.

C    Surfaces which represent internal mass are marked 'FURNI' and
C    the 2nd array index is un-used.

C    Surface which are used as internal partitions are marked 'PARTN'
C    and the 2nd array index is un-used.

C    Interior surfaces can be marked as 'STRUC' for structural elements
C    such as beams and columns and the 2nd array index is un-used.

C    otherwise  -,-  is the initial state for newly created surfaces
C    and surfaces which the user has not yet expressed an opinion. This
C    can also be used for internal surfaces which may not be of concern
C    within building code complence assessments.

C ZNAME (12 char) - the zone name (from geometry.h).
C ZDESC (64 char) - zone notes (from geometry.h).

C prec17 common described in geometry.h
      
C Save total transparent surface area for surfaces connected to external BC.

C << do we need transparent surface area facing different orientations? >>
      common/PREC18/ZTRANA(MCOM)
      COMMON/C24/IZSTOCN(MCOM,MS)

C Error subroutine and error details for dll mode.
      common/dllerr/dllsubr,dllmesg

C Thermal bridge common block is described in esruprj/edgeo.F
      integer nbrdg, ibrdg
      real psi,lngth,losspercent,totheatloss,thbrpercent
      real uavtotal
      common/THRBRDG/nbrdg(MCOM),psi(MCOM,16),lngth(MCOM,16),
     &               ibrdg(MCOM,16),losspercent(MCOM),totheatloss(MCOM),
     &               thbrpercent,uavtotal(MCOM)

C This information was previously/also held in zone obstruction
C file. The intent is that older models when scanned can be upgraded
C to hold such information in the zone geometry file. Common GS5/GS6
C GS7/GS8 are defined in geometry.h

      DIMENSION IVA(MS)
      DIMENSION SNA(MS),PAZI(MS),PELV(MS)
      CHARACTER LGEOMF*72,WORD*32
      CHARACTER tmpvfc*4,tother*15,tother1*15,tother2*15
      character ZN*12,phrase*64,outs*124,outs2*124
      character dllsubr*12,dllmesg*124,loutstr*248
      character dstmp*24
      character OTF*4,OPT*12 ! for use with parsemlcdesc
      logical dll,checkbase
      logical havec24
      integer nbo   ! local value of nbobs
      integer lsn   ! length of currentfile
      integer lno   ! length of optics name
      real VX,VY,VZ ! local values for XOB YOB ZOB
      integer iflag ! for read error state

C Set initial values.
      IER=0
      iflag=0
      itrct=0
      ZN=' '
      phrase=' '
      checkbase=.false.
      havec24=.false.  ! true if connections list has data
      NTV=0    ! counter for vertices, assume no vertices
      NEDGE=0  ! counter for surfaces in the edge list, assume no surfaces
      NS=0     ! temporary array for counting surfaces.
      NSUR=0   ! reset surface count

C Treatment of obstructions needs to take into account several
C factors. The version 1.1 geometry file might not yet have
C embedded legacy zone obstruction file entities. If there
C is still a legacy zone obstruction file associated with this
C zone then the calling code will need to do that instanciation
C in a subsequent call and that will be updating nbobs.
      if(iobs(icomp).eq.1)then
        continue          ! scan of zone obs file will reset
      else
        nbobs(icomp)=0    ! reset obstructions
      endif

C Check if running in dll mode.
      call isadll(dll)

C Initialise geometry data file. and set currentfile.
      CALL EFOPSEQ(IUNIT,LGEOMF,1,IER)
      IF(IER.LT.0)THEN
        write(outs,'(3a)') 'Geometry file ',LGEOMF(1:lnblnk(LGEOMF)),
     &      ' could not be opened.'
        if(dll)then
          dllsubr='GEOREAD'
          dllmesg=outs
          ier=2
          return
        else
          call edisp(itru,outs)
          IER=1
          RETURN
        endif
      ENDIF
      write(currentfile,'(a)') LGEOMF(1:lnblnk(LGEOMF))

C Read header lines from file, the 1.1 version looks like: 
C *Geometry 1.1,GEN,manager  # tag version, format, zone name (tbd allow spaces)
C An older file header looks like:
C # geometry of manager defined in: ../zones/manager.geo
C GEN  manager  manager describes cellular office  # type, name, descr
C      22      10   0.000    # vertices, surfaces, rotation angle
      CALL LSTRIPC(IUNIT,LOUTSTR,99,ND,1,'geo line 1',IER)
      IF(IER.NE.0)goto 1002
      if(LOUTSTR(1:13).eq.'*Geometry 1.1')then

C Decode first line of version 1.1 geometry file.
        K=13
        CALL EGETW(LOUTSTR,K,WORD,'W','CTYPE',IFLAG)
        write(CTYPE(icomp),'(a)') WORD(1:lnblnk(WORD))
        zname(ICOMP)=' '
        CALL EGETW(LOUTSTR,K,WORD,'W','Z name',IFLAG)
        ZN=WORD(1:12)
        call st2name(ZN,zname(ICOMP))
        lnzname(ICOMP)=lnblnk(zname(ICOMP))  ! update string length
        gversion(icomp) = 1.1   ! set the version number.
      else

C Check if it matches the syntax of older geometry file format.
        K=0
        IF(ND.EQ.1)THEN
          CALL EGETW(LOUTSTR,K,WORD,'W','CTYPE',IFLAG)
          write(CTYPE(icomp),'(a)') WORD(1:lnblnk(WORD))
          zname(ICOMP)=' '
          lnzname(ICOMP)=0
          zdesc(ICOMP)=' '
          lnzdesc(ICOMP)=0
        ELSEIF(ND.EQ.2)THEN
          CALL EGETW(LOUTSTR,K,WORD,'W','CTYPE',IFLAG)
          write(CTYPE(icomp),'(a)') WORD(1:lnblnk(WORD))
          CALL EGETW(LOUTSTR,K,WORD,'W','Z name',IFLAG)
          ZN=WORD(1:12)
          call st2name(ZN,zname(ICOMP))
          lnzname(ICOMP)=lnblnk(zname(ICOMP))  ! update string length
          zdesc(ICOMP)=' '
          lnzdesc(ICOMP)=0
        ELSEIF(ND.gt.2)THEN
          CALL EGETW(LOUTSTR,K,WORD,'W','CTYPE',IFLAG)
          write(CTYPE(icomp),'(a)') WORD(1:lnblnk(WORD))
          CALL EGETW(LOUTSTR,K,WORD,'W','Z name',IFLAG)
          ZN=WORD(1:12)
          call st2name(ZN,zname(ICOMP))
          call egetrm(loutstr,K,phrase,'W','Z description',IER)
          ZDESC(ICOMP)=phrase
          lnzdesc(ICOMP)=lnblnk(phrase)  ! update the length of this string.
        ENDIF
        IF(IFLAG.NE.0)GOTO 1002

C If we have reached this position then it is an older geometry
C file that seems to be readable so close the file and call egomin.
        CALL ERPFREE(IUNIT,ios)
        gversion(icomp) = 1.0   ! set the version number.
        itrct=0                 ! set to silent
        call egomin(IUNIT,LGEOMF,ICOMP,IR,ITRCT,ITRU,IER)
        return
      endif

      IF(CTYPE(icomp)(1:3).EQ.'REC')THEN

C Zone is of type REC, subroutine EGOMIN should have been used.
        write(outs,'(3a)') 'Geometry file ',LGEOMF(1:lnblnk(LGEOMF)),
     &      ' has REC format so it must be a legacy file.'
        if(dll)then
          dllsubr='GEOREAD'
          dllmesg=outs
          ier=2
          return
        else
          call edisp(itru,outs)
          IER=1
          RETURN
        endif
      ELSEIF(CTYPE(icomp)(1:3).EQ.'REG')THEN

C Zone is of type REG, subroutine EGOMIN should have been used.
        write(outs,'(3a)') 'Geometry file ',LGEOMF(1:lnblnk(LGEOMF)),
     &      ' has REG format so it must be a legacy file.'
        if(dll)then
          dllsubr='GEOREAD'
          dllmesg=outs
          ier=2
          return
        else
          call edisp(itru,outs)
          IER=1
          RETURN
        endif
      ELSEIF(CTYPE(icomp)(1:3).EQ.'GEN')THEN

C Zone is of type GEN. First read the date stamp line followed
C by the zone description on the next line. For now zdesc string
C is short, but will be extended to 248 char.
        CALL LSTRIPC(IUNIT,LOUTSTR,0,ND,1,'date stamp',IER)
        IF(IER.NE.0) goto 1002
        K=0
        CALL EGETW(LOUTSTR,K,WORD,'W','header tags',IER)
        IF(IER.NE.0) goto 1002
        if(WORD(1:5).eq.'*Date'.or.WORD(1:5).eq.'*date')then
          CALL EGETRM(LOUTSTR,K,dstmp,'W','date stamp',IER)
        endif
        zdesc(ICOMP)=' '
        CALL LSTRIPC(IUNIT,LOUTSTR,0,ND,1,'Z description',IER)
        IF(IER.NE.0) goto 1002
        write(zdesc(ICOMP),'(a)') LOUTSTR(1:64)
        lnzdesc(ICOMP)=lnblnk(zdesc(ICOMP))  ! update the length of this string.
        zorigin(icomp,1)=0.0   ! clear the zorigin and zsize arrays
        zorigin(icomp,2)=0.0
        zorigin(icomp,3)=0.0
        zsize(icomp,1)=0.0
        zsize(icomp,2)=0.0
        zsize(icomp,3)=0.0

C Next look for *vertex lines, incrementing the value of NTV
C as each of these lines is scanned. If the tag is *edges then
C increment the value of NSUR and then scan JVN data. If ier
C equals 2 then end of file reached so close.
   62   CALL LSTRIPC(IUNIT,LOUTSTR,99,ND,0,'*vertex *edges tags',IER)
        if(IER.EQ.2)then

C End of file sensed, however this is not an error at this point.
          IER=0
          goto 44
        elseif(IER.eq.0)then
          continue
        else
          goto 1002
        endif
        K=0
        CALL EGETW(LOUTSTR,K,WORD,'W','*vertex or *surface tags',IER)
        if(WORD(1:7).eq.'*vertex')then
          NTV=NTV+1
          if(NTV.le.MTV)then
            CALL EGETWR(LOUTSTR,K,X(NTV),0.,0.,'-','X coord',IER)
            CALL EGETWR(LOUTSTR,K,Y(NTV),0.,0.,'-','Y coord',IER)
            CALL EGETWR(LOUTSTR,K,Z(NTV),0.,0.,'-','Z coord',IER)
            NZTV(ICOMP)=NTV
          else
            call edisp(iuout,
     &        'Exceeded number of vertices. Skipping input line.')
          endif
          goto 62
        elseif(WORD(1:7).eq.'*rotate')then

C Optional tag to hold pending request to rotate the zone.
          CALL EGETWR(loutstr,K,AR1,-360.,360.,'W','rotation angle',IER)
          rotateit(ICOMP,1)=AR1   ! remember the requested rotation
          if(ND.gt.2)then
            CALL EGETWR(loutstr,K,AR1,0.,0.,'-','rotation point X',IER)
            rotateit(icomp,2)=AR1
            CALL EGETWR(loutstr,K,AR1,0.,0.,'-','rotation point Y',IER)
            rotateit(icomp,3)=AR1
          else
            rotateit(icomp,2)=0.0  ! assume site origin to rotate around
            rotateit(icomp,3)=0.0
          endif
          goto 62
        elseif(WORD(1:16).eq.'*previous_rotate')then

C Optional tag to hold most recent request to rotate the zone (in
C order to support an un-do.
          CALL EGETWR(loutstr,K,AR1,-360.,360.,'W','prior rotation',IER)
          rotateit(ICOMP,4)=AR1   ! remember the previous rotation
          if(ND.gt.2)then
            CALL EGETWR(loutstr,K,AR1,0.,0.,'-','prev rotation X',IER)
            rotateit(icomp,5)=AR1
            CALL EGETWR(loutstr,K,AR1,0.,0.,'-','prev rotation Y',IER)
            rotateit(icomp,6)=AR1
          else
            rotateit(icomp,5)=0.0  ! assume site origin to rotate around
            rotateit(icomp,6)=0.0
          endif
          goto 62
        elseif(WORD(1:6).eq.'*edges')then
          NEDGE=NEDGE+1
          if(NEDGE.le.MS)then
            CALL EGETWI(LOUTSTR,K,J,3,MV,'F','nb assoc vertices',IERV)
            NVER(NEDGE)=J

C Now proceed to read vertices on one or more lines.
            DO 12 KV=1,NVER(NEDGE)
              CALL EGETWI(LOUTSTR,K,IVAL,0,MTV,'F','vertex',IERV)
              IF(IERV.NE.0) THEN
                call edisp(ITRU,' reading continuation line...')
                CALL LSTRIPC(IUNIT,LOUTSTR,0,ND,0,'vertex XYZ',IER)
                IF(IER.NE.0)goto 1002
                K=0
                CALL EGETWI(LOUTSTR,K,IVAL,0,MTV,'F','vertex',IERV)
              ENDIF
              IF(IERV.NE.0) GOTO 1002
              JVN(NEDGE,KV)=IVAL
   12       CONTINUE

            NZSUR(ICOMP)=NEDGE  ! update the number of surfaces.
          else
            call edisp(iuout,
     &        'Exceeded number of surfaces. Skipping input line.')
          endif
          goto 62
        elseif(WORD(1:5).eq.'*surf')then

C Increment counter for surfaces (NS).
          NS=NS+1
          if(NS.eq.1)then

C If this is the first *surf encountered then set NSUR equal to the
C number of edge sets and call filsur to instanciate default data.
            NSUR=NEDGE
            CALL FILSUR(ICOMP,0)

C Check if the master connection list has been filled, if so
C this is a 'subsequent scan' and the connections based commons
C can probably be filled. Each surface needs to be tested
C in case the master list is corrupt.
            icn1=izstocn(icomp,1)
            if(icn1.gt.0)then
C Debug.
C              call edisp(iuout,'Subsequent geometry 1.1 scan...')
            endif
          endif
          if(NS.le.MS)then

C Check and see if izstocn returns non-zeros and set havec24.
            icn=izstocn(icomp,NS)
            if(icn.gt.0)then
              havec24=.true.
            else
              havec24=.false.
            endif

C Surface attributes in a typical line:
C *surf,door,VERT,-,DOOR,UNDERCUT,door,OPAQUE,ANOTHER,3,6 # >|< door in coridor      

C (1) surf name (12 char, tbd - allow spaces)
C (2) surf posn (tags VERT, CEIL (faces down), FLOR (faces up), SLOP (other)
C (3) child of (name of surface, otherwise a -)
C (4-5) useage: a pair of tags for code complience and to assist with air
C  flow network creation. A full description is at the top of this source
C  file.
C (6) construction name (from MLC database, (tbd allow spaces))
C (7) optical name (OPAQUE or the name of the optical set name, (tbd allow spaces))
C (8-10) boundary condition (including indices from cnn file):
C   ANOTHER, i index of zone, j index of surface in that zone
C   EXTERIOR,i,j    where i is 0 and j is 0
C   SIMILAR,i,j     where i is offset degC and j is offset Watts
C   IDENT_CEN,i,j     where i is 0 and j is 0
C   CONSTANT,i,j     where i is degC and j is Watts
C   ADIABETIC,i,j     where i is 0 and j is 0
C   BASESIMP,i,j     where i is configuration index and j is % to this surface
C   GROUND_STD,i,j     where i is profile index and j is 0
C   GROUND_USR,i,j     where i is 0 and j is profile index
C   UNKNOWN,i,j     where i is 0 and j is 0

C Surface name, allow for future spaces in name.
            CALL EGETP(LOUTSTR,K,WORD,'W','surface name',IER)
            write(SNAME(ICOMP,NS),'(a)') WORD(1:lnblnk(WORD))
            if(havec24)then
              write(ssname(icn),'(a)') WORD(1:lnblnk(WORD))
            endif

C Surface position
            CALL EGETW(LOUTSTR,K,tmpvfc,'W','surface position',IER)
            IF(tmpvfc.EQ.'VERT'.OR.tmpvfc.EQ.'SLOP')then
              write(SVFC(NS),'(a)') tmpvfc
              if(havec24) write(SSVFC(icn),'(a)') tmpvfc
            ELSEIF(tmpvfc(1:4).EQ.'CEIL')then
              write(SVFC(NS),'(a)') 'CEIL'
              if(havec24) write(SSVFC(icn),'(a)') 'CEIL'
            ELSEIF(tmpvfc(1:4).EQ.'FLOR')then
              write(SVFC(NS),'(a)') 'FLOR'
              if(havec24) write(SSVFC(icn),'(a)') 'FLOR'
            ENDIF

C Parent name, allow for future spaces in parent name.
            CALL EGETP(LOUTSTR,K,WORD,'W','surface parent',IER)
            write(sparent(NS),'(a)') WORD(1:lnblnk(WORD))
            if(havec24) write(ssparent(icn),'(a)') WORD(1:lnblnk(WORD))

C Surface usage - two tokens.
            CALL EGETW(LOUTSTR,K,WORD,'W','surface use 1',IER)
            write(SUSE(NS,1),'(a)') WORD(1:lnblnk(WORD))
            if(havec24) write(SSUSE(icn,1),'(a)') WORD(1:lnblnk(WORD))
            CALL EGETW(LOUTSTR,K,WORD,'W','surface use 2',IER)
            write(SUSE(NS,2),'(a)') WORD(1:lnblnk(WORD))
            if(havec24) write(SSUSE(icn,2),'(a)') WORD(1:lnblnk(WORD))

C Surface construction name, allow for spaces.
            CALL EGETP(LOUTSTR,K,WORD,'W','surface construction',IER)
            write(SMLCN(NS),'(a)') WORD(1:lnblnk(WORD))

C Check if there is a matching MLC name find out which one it is.
            ICF=-1
            if(NMLC.gt.0)then                
              lstr=lnblnk(SMLCN(NS))
              DO 20 IC=1,NMLC
                if(lstr.lt.12)then
                  IF(DESC(IC)(1:lstr).EQ.SMLCN(NS)(1:lstr))ICF=IC
                else
                  IF(DESC(IC)(1:12).EQ.SMLCN(NS)(1:12))ICF=IC
                endif
  20          CONTINUE
            endif

            if(havec24)then
              write(SSMLCN(icn),'(a)') WORD(1:lnblnk(WORD))
              if(ICF.gt.0)then
                ssthick(icn)=THKMLC(icf)

C Debug.
C                write(outs,'(a,2i3,f7.2)') 'set thick of ',icf,icn,
C     &            ssthick(icn)
C                call edisp(iuout,outs)
              endif
            endif

C Surface optics set name or OPAQ/TRAN, allow for spaces.
            CALL EGETP(LOUTSTR,K,WORD,'W','surface optics',IER)
            if(WORD(1:4).eq.'OPAQ')then
              write(SOTF(NS),'(a)') 'OPAQUE'  ! write OPAQUE
              if(havec24) write(SSOTF(icn),'(a)') 'OPAQUE'
            elseif(WORD(1:4).eq.'CFC ')then
              write(SOTF(NS),'(a)') 'CFC '  ! write CFC
              if(havec24) write(SSOTF(icn),'(a)') 'CFC '
            elseif(WORD(1:4).ne.'OPAQ'.and.WORD(1:4).ne.'CFC ')then

C This token could be the name of the optical property. Find out if it
C matches then name of the optical set for the construction.
C If the construction database has been scanned then attempt to discover
C the name of the optical property. Otherwise leave SOFT as TRAN
              if(NMLC.gt.0)then

C Use logic similar to parsemlcdesc to find the optical name within
C the constructions database.
                if(ICF.gt.0)then
                  OPT=' '
                  WRITE(OTF,'(A)') DESC(ICF)(15:18)
                  IF(OTF.EQ.' ')OTF='OPAQ'
                  IF(OTF(1:4).NE.'OPAQ'.AND.OTF(1:4).NE.'CFC ')THEN
                    WRITE(OPT,'(A)') DESC(ICF)(21:32)
                    IF(OPT.EQ.' ')OPT='UNKNOWN'
                  ELSE
                    OPT='OPAQUE'
                  ENDIF
                else
                  OPT='OPAQUE'
                endif
		if(WORD(1:lnblnk(WORD)).ne.OPT(1:lnblnk(OPT)))then
                  lno=lnblnk(word)
		  write(outs,*) 'Optics in geo file ',WORD(1:lno),
     &              ' does not match optics of the MLC ',OPT
                  call edisp(iuout,outs)
                endif
                write(SOTF(NS),'(a)') OPT
                if(havec24) write(SSOTF(icn),'(a)') OPT
              else

C During initial scan of configuration file the databases will
C not yet have been scanned and so this block of code will be
C active. Lets assume that this token really is the name of the
C optical set.
                write(SOTF(NS),'(a)') WORD(1:lnblnk(WORD))
                if(havec24)write(SSOTF(icn),'(a)') WORD(1:lnblnk(WORD))
              endif
            endif

C Surface other side - three tokens.
            CALL EGETW(LOUTSTR,K,tother,'W','surface other 1',IER)
            CALL EGETW(LOUTSTR,K,tother1,'W','surface other 2',IER)
            CALL EGETW(LOUTSTR,K,tother2,'W','surface other 3',IER)
            if(tother(1:7).eq.'UNKNOWN')then
              write(sother(ns,1),'(a)') 'UNKNOWN'
              write(sother(ns,2),'(a)') '0'
              write(sother(ns,3),'(a)') '0'
              if(havec24)then
                write(ssother(icn,1),'(a)') 'UNKNOWN'
                write(ssother(icn,2),'(a)') '0'
                write(ssother(icn,3),'(a)') '0'
              endif
            elseif(tother(1:8).eq.'EXTERIOR')then
              write(sother(ns,1),'(a)') 'EXTERIOR'
              write(sother(ns,2),'(a)') '0'
              write(sother(ns,3),'(a)') '0'
              if(havec24)then
                write(ssother(icn,1),'(a)') 'EXTERIOR'
                write(ssother(icn,2),'(a)') '0'
                write(ssother(icn,3),'(a)') '0'
              endif
            elseif(tother(1:9).eq.'ADIABATIC')then
              write(sother(ns,1),'(a)') 'ADIABATIC'
              write(sother(ns,2),'(a)') '0'
              write(sother(ns,3),'(a)') '0'
              if(havec24)then
                write(ssother(icn,1),'(a)') 'ADIABATIC'
                write(ssother(icn,2),'(a)') '0'
                write(ssother(icn,3),'(a)') '0'
              endif
            elseif(tother(1:7).eq.'SIMILAR')then
              write(sother(ns,1),'(a)') 'SIMILAR'
              write(sother(ns,2),'(a)') tother1
              write(sother(ns,3),'(a)') tother2
              if(havec24)then
                write(ssother(icn,1),'(a)') 'SIMILAR'
                write(ssother(icn,2),'(a)') tother1
                write(ssother(icn,3),'(a)') tother2
              endif
            elseif(tother(1:8).eq.'CONSTANT')then
              write(sother(ns,1),'(a)') 'CONSTANT'
              write(sother(ns,2),'(a)') tother1
              write(sother(ns,3),'(a)') tother2
              if(havec24)then
                write(ssother(icn,1),'(a)') 'CONSTANT'
                write(ssother(icn,2),'(a)') tother1
                write(ssother(icn,3),'(a)') tother2
              endif
            elseif(tother(1:8).eq.'BASESIMP')then
              write(sother(ns,1),'(a)') 'BASESIMP'
              write(sother(ns,2),'(a)') tother1
              write(sother(ns,3),'(a)') tother2
              if(havec24)then
                write(ssother(icn,1),'(a)') 'BASESIMP'
                write(ssother(icn,2),'(a)') tother1
                write(ssother(icn,3),'(a)') tother2
              endif
            elseif(tother(1:6).eq.'GROUND')then
              write(sother(ns,1),'(a)') 'GROUND'
              write(sother(ns,2),'(a)') tother1
              write(sother(ns,3),'(a)') tother2
              if(havec24)then
                write(ssother(icn,1),'(a)') 'GROUND'
                write(ssother(icn,2),'(a)') tother1
                write(ssother(icn,3),'(a)') tother2
              endif
            elseif(tother(1:9).eq.'IDENT_CEN')then
              write(sother(ns,1),'(a)') 'IDENT_CEN'
              write(sother(ns,2),'(a)') tother1
              write(sother(ns,3),'(a)') tother2
              if(havec24)then
                write(ssother(icn,1),'(a)') 'IDENT_CEN'
                write(ssother(icn,2),'(a)') tother1
                write(ssother(icn,3),'(a)') tother2
              endif
            elseif(tother(1:7).eq.'ANOTHER')then
              write(sother(ns,1),'(a)') 'ANOTHER'
              write(sother(ns,2),'(a)') tother1
              write(sother(ns,3),'(a)') tother2
              if(havec24)then
                write(ssother(icn,1),'(a)') 'ANOTHER'
                write(ssother(icn,2),'(a)') tother1
                write(ssother(icn,3),'(a)') tother2
              endif
            else

C Assume partition so setup sother arrays with this in mind.
              write(sother(ns,1),'(a)') 'ANOTHER'
              write(sother(ns,2),'(a)') tother1
              write(sother(ns,3),'(a)') tother2
              if(havec24)then
                write(ssother(icn,1),'(a)') 'ANOTHER'
                write(ssother(icn,2),'(a)') tother1
                write(ssother(icn,3),'(a)') tother2
              endif
            endif
          else
            call edisp(iuout,
     &        'Exceeded number of surfaces. Skipping input line.')
          endif
          goto 62
        elseif(WORD(1:10).eq.'*base_list')then

C The list-based definition of base surfaces. 
C A typical entry would look like:
C *base_list,1,6,13.50 0
C were the 1st item is the number of associated surfaces, then the
C list of surfaces followed by the base area followed by user flag.
          CALL EGETWI(LOUTSTR,K,IB0,0,12,'-','zn base list',IER)
          izbaselist(icomp)=IB0
          if(IB0.gt.0)then

C There are items in the list so scan the surface indices.
            do 63 inthelist=1,IB0
              IBASES(ICOMP,inthelist)=0
              CALL EGETWI(LOUTSTR,K,IB1,0,12,'-',
     &          'zn base surface list item',IER)
              IBASES(ICOMP,inthelist)=IB1
  63        continue
          else

C No items in the list so user must have edited the base area.
            iuzbasea(icomp)=1
          endif
          CALL EGETWR(LOUTSTR,K,VAL,0.,99999.,'W','zn base area',IER)
          ZBASEA(ICOMP)=VAL
          CALL EGETWI(LOUTSTR,K,IUB,0,2,'-',
     &      'zn base area user flag',IER)
          if(iub.eq.0)then
            iuzbasea(icomp)=0
          elseif(iub.eq.2)then
            iuzbasea(icomp)=2
          endif
          goto 62

        elseif(WORD(1:10).eq.'*shad_calc')then

C Shading calculation instructions.
          CALL EGETW(LOUTSTR,K,WORD,'W','shad directive',IFLAG)
          if(WORD(1:4).eq.'none')then
            iaplic(icomp,1)=0
            nsurfcalc(icomp)=0
          else
            if(WORD(1:14).eq.'all_applicable')then
              iaplic(icomp,1)=1
            elseif(WORD(1:4).eq.'list')then
              iaplic(icomp,1)=0
            endif

C Read number of surfaces and then the list
            CALL EGETWI(LOUTSTR,K,iv,0,MS,'-','nsurfcalc',IER)
            nsurfcalc(icomp)=iv
            IRVA=nsurfcalc(icomp)
            CALL EGETWIA(IUNIT,IVA,IRVA,0,MS,'F','lstsfcalc',IER)
            DO 342 ks=1,nsurfcalc(icomp)
              lstsfcalc(icomp,ks)=IVA(ks)
 342        CONTINUE
          endif
          goto 62
        elseif(WORD(1:11).eq.'*insol_calc')then

C Insolation calculation instructions.
          CALL EGETW(LOUTSTR,K,WORD,'W','insolation directive',IFLAG)
          if(WORD(1:4).eq.'none')then
            iaplic(icomp,2)=0
            nsurfinso(icomp)=0
          else
            if(WORD(1:14).eq.'all_applicable')then
              iaplic(icomp,2)=1
            elseif(WORD(1:4).eq.'list')then
              iaplic(icomp,2)=0
            endif

C Applicable surfaces are the current exterior facing surfaces.
            CALL EGETWI(LOUTSTR,K,iv,0,MS,'-','nsurfinso',IER)
            nsurfinso(icomp)=iv
            IRVA=nsurfinso(icomp)
            CALL EGETWIA(IUNIT,IVA,IRVA,0,MS,'F','isurfinso',IER)
            DO 344 ks=1,nsurfinso(icomp)
              isurfinso(icomp,ks)=IVA(ks)
 344        CONTINUE
          endif
          goto 62
        elseif(WORD(1:7).eq.'*insol ')then

C There are two tags that begin with *insol so include the space
C in the tag test to clarify.
C Read default insolation surface numbers. NN = insolation defining 
C index:  1; one plane, 2; two planes, 3; all planes (diffuse).
C IDPN defines the default plane numbers.
          CALL EGETWI(LOUTSTR,K,IV,1,3,'W','Def insol index',IER)
          NDP(ICOMP)=IV
          CALL EGETWI(LOUTSTR,K,IV,0,NSUR,'W','1st recv surf',IER)
          IDPN(ICOMP,1)=IV
          CALL EGETWI(LOUTSTR,K,IV,0,NSUR,'W','2nd recv surf',IER)
          IDPN(ICOMP,2)=IV
          CALL EGETWI(LOUTSTR,K,IV,-1,NSUR,'W','3rd insol data',IER)
          IDPN(ICOMP,3)=IV
          IF(IR.EQ.1)THEN
            DO 40 I=1,3
              IX=0
              IF(NDP(ICOMP).EQ.1.AND.I.EQ.1)IX=1
              IF(NDP(ICOMP).EQ.2.AND.I.LE.2)IX=1
              if(IDPN(ICOMP,I).NE.-1.AND.IDPN(ICOMP,I).NE.0)then
                if(IDPN(ICOMP,I).LT.IX.OR.IDPN(ICOMP,I).GT.NSUR)then
                  CALL LUSRMSG(' Nonexistent insolation plane!',
     &              LOUTSTR,'W')
                endif
              endif
   40       CONTINUE
          ENDIF
          goto 62
        elseif(WORD(1:13).eq.'*bridge_start')then

C Simplified thermal bridge data starts here. Scan three reals after tag.
          nbrdg(icomp)=0
          CALL EGETWR(LOUTSTR,K,VAL,0.,0.,'-','user fraction thbr',IER)
          thbrpercent=VAL
          CALL EGETWR(LOUTSTR,K,VAL,0.,0.,'-','W/K from loss %',IER)
          losspercent(icomp)=VAL
          CALL EGETWR(LOUTSTR,K,VAL,0.,0.,'-','W/K total',IER)
          totheatloss(icomp)=VAL
          CALL EGETWR(LOUTSTR,K,VAL,0.,0.,'-','W/K total',IER)
          uavtotal(icomp)=VAL
          goto 62
        elseif(WORD(1:11).eq.'*ukt_bridge')then
          nbrdg(icomp)=nbrdg(icomp)+1
          CALL EGETP(LOUTSTR,K,phrase,'W','bridge tag',IER)
          if(phrase(1:10).eq.'roof-wall ')then
            ibrdg(icomp,nbrdg(icomp))=1
          elseif(phrase(1:18).eq.'wall-ground floor ')then
            ibrdg(icomp,nbrdg(icomp))=2
          elseif(phrase(1:25).eq.'wall-wall (convex corner) ')then
            ibrdg(icomp,nbrdg(icomp))=3
          elseif(phrase(1:26).eq.'wall-wall (concave corner) ')then
            ibrdg(icomp,nbrdg(icomp))=4
          elseif(phrase(1:30).eq.'wall-floor (not ground floor) ')then
            ibrdg(icomp,nbrdg(icomp))=5
          elseif(phrase(1:28).eq.'lintel above window or door ')then
            ibrdg(icomp,nbrdg(icomp))=6
          elseif(phrase(1:18).eq.'Sill below window ')then
            ibrdg(icomp,nbrdg(icomp))=7
          elseif(phrase(1:23).eq.'jamb at window or door ')then
            ibrdg(icomp,nbrdg(icomp))=8
          elseif(phrase(1:12).eq.'roof-wall-mc')then
            ibrdg(icomp,nbrdg(icomp))=9
          elseif(phrase(1:20).eq.'wall-ground floor-mc')then
            ibrdg(icomp,nbrdg(icomp))=10
          elseif(phrase(1:21).eq.'wall-wall (corner)-mc')then
            ibrdg(icomp,nbrdg(icomp))=11
          elseif(phrase(1:32).eq.
     &      'wall-floor (not ground floor)-mc')then
            ibrdg(icomp,nbrdg(icomp))=12
          elseif(phrase(1:30).eq.'lintel above window or door-mc')then
            ibrdg(icomp,nbrdg(icomp))=13
          elseif(phrase(1:20).eq.'Sill below window-mc')then
            ibrdg(icomp,nbrdg(icomp))=14
          elseif(phrase(1:25).eq.'jamb at window or door-mc')then
            ibrdg(icomp,nbrdg(icomp))=15
          else
            ibrdg(icomp,nbrdg(icomp))=16
          endif
          CALL EGETWR(LOUTSTR,K,VAL,0.,0.,'-','length of bridge',IER)
          lngth(icomp,nbrdg(icomp))=VAL
          CALL EGETWR(LOUTSTR,K,VAL,0.,0.,'-','bridge psi',IER)
          psi(icomp,nbrdg(icomp))=VAL
          goto 62
        elseif(WORD(1:11).eq.'*end_bridge')then
          goto 62
        elseif(WORD(1:12).eq.'*block_start')then

C A number of block shapes can be associated with a model as follows:
C *obs - solar obstruction blocks and
C *mrt - mean radiant temperature sensor blocks (not yet implemented)
C *photo - lighting sensor position and vector (not yet implemented)

C If there is a zone obstruction file (iobs() = 1) then there should
C not be *obs tags in this file.
C If iobs() = 2 then obstructions were previously imported from a
C zone obstructions file or are held in this geometry file.
C If iobs() = 0 then no obstructions yet defined so the value should
C be incremented to iobs() = 2.
          if(iobs(icomp).eq.1)then
            call usrmsg(
     &      'There is a zone obstructions file as well as block',
     &      'definitions in this geometry file. Skipping data.','W')
          endif 

C Following the tag *block_start will be the grid for X and Z. If
C there are insufficient tokens on line set both to 20.
          if(ND.gt.2)then
            CALL EGETWI(LOUTSTR,K,NOX(icomp),4,20,'F','opq grid X',IER)
            CALL EGETWI(LOUTSTR,K,NOZ(icomp),4,20,'F','opq grid Z',IER)
          else
            NOX(icomp)=20
            NOZ(icomp)=20
          endif

C Read another line to get the actual blocks. There are slightly different
C formats depending on whether the key is '*obs' or '*obs3'.
   72     CALL LSTRIPC(IUNIT,LOUTSTR,99,ND,0,'*obs tags',IER)
          k=0
          CALL EGETP(LOUTSTR,K,phrase,'W','block tag',IER)
          if(phrase(1:5).eq.'*obs3')then
            if(iobs(icomp).eq.1) goto 72  ! keep reading till *end_block
            if(iobs(icomp).eq.0) iobs(icomp)=2
            nbobs(icomp)=nbobs(icomp)+1
            nbo=nbobs(icomp)
            CALL EGETWR(LOUTSTR,K,VX,-999.,998.,'W','obs X org',IER)
            CALL EGETWR(LOUTSTR,K,VY,-999.,998.,'W','obs Y org',IER)
            CALL EGETWR(LOUTSTR,K,VZ, -99., 99.,'W','obs Z org',IER)
            XOB(icomp,nbo)=VX
            YOB(icomp,nbo)=VY
            ZOB(icomp,nbo)=VZ
            CALL EGETWR(LOUTSTR,K,VX,0., 99.,'W','obs X dis',IER)
            CALL EGETWR(LOUTSTR,K,VY,0., 99.,'W','obs Y dis',IER)
            CALL EGETWR(LOUTSTR,K,VZ,0., 99.,'W','obs Z dis',IER)
            DXOB(icomp,nbo)=VX
            DYOB(icomp,nbo)=VY
            DZOB(icomp,nbo)=VZ
            CALL EGETWR(LOUTSTR,K,VX,-359.,359.,'W','obs rot ang a',IER)
            BANGOB(icomp,nbo,1)=VX
            CALL EGETWR(LOUTSTR,K,VX,-359.,359.,'W','obs rot ang b',IER)
            BANGOB(icomp,nbo,2)=VX
            CALL EGETWR(LOUTSTR,K,VX,-359.,359.,'W','obs rot ang c',IER)
            BANGOB(icomp,nbo,3)=VX
            if(ND.ge.13)then  ! if enough items for opacity
              CALL EGETWR(LOUTSTR,K,VX,0.,1.,'W','obs opacity',IER)
              OPOB(icomp,nbo)=VX
            else
              OPOB(icomp,nbo)=1.0  ! set to opaque if not specified
            endif
            CALL EGETW(LOUTSTR,K,WORD,'W','obs blk name',IFLAG)
            BLOCKNAME(icomp,nbo)=WORD(1:12)
            LNBLOCKNAME(icomp,nbo)=lnblnk(BLOCKNAME(icomp,nbo))

C The name of the construction might contain spaces so use EGETP.
            CALL EGETP(LOUTSTR,K,WORD,'W','obs mat name',IFLAG)
            write(BLOCKMAT(icomp,nbo),'(a)') WORD(1:lnblnk(WORD))
            LNBLOCKMAT(icomp,nbo)=lnblnk(BLOCKMAT(icomp,nbo))
            BLOCKTYP(icomp,nbo)='obs3'
            goto 72

          elseif(phrase(1:5).eq.'*obsp')then

C A general polygon obstruction to be associated with the current zone.
C The first line includes (current fixed) integer number of vertices
C followed by number of faces and the name and material.
C The 2nd line has the first 4 coordinates and the 3rd line has the 
C next 4 coordinates. The surface and edge ordering is as in a 
C standard obstruction block when converted into a GB1 common block.
            if(iobs(icomp).eq.1) goto 72  ! keep reading till *end_block
            if(iobs(icomp).eq.0) iobs(icomp)=2
            nbobs(icomp)=nbobs(icomp)+1
            nbo=nbobs(icomp)
            CALL EGETWI(LOUTSTR,K,ival,8,8,'F','obs nb vertices',IER)
            CALL EGETWI(LOUTSTR,K,ival,6,6,'F','obs nb faces',IER)
            if(ND.ge.6)then  ! if enough items for opacity
              CALL EGETWR(LOUTSTR,K,VX,0.,1.,'W','obs opacity',IER)
              OPOB(icomp,nbo)=VX
            else
              OPOB(icomp,nbo)=1.0  ! set to opaque if not specified
            endif
            CALL EGETW(LOUTSTR,K,WORD,'W','obs blk name',IFLAG)
            write(BLOCKNAME(icomp,nbo),'(a)') WORD(1:lnblnk(WORD))
            LNBLOCKNAME(icomp,nbo)=lnblnk(BLOCKNAME(icomp,nbo))
            CALL EGETW(LOUTSTR,K,WORD,'W','obs mat name',IFLAG)
            write(BLOCKMAT(icomp,nbo),'(a)') WORD(1:lnblnk(WORD))
            LNBLOCKMAT(icomp,nbo)=lnblnk(BLOCKMAT(icomp,nbo))
            BLOCKTYP(icomp,nbo)='obsp'

            CALL LSTRIPC(IUNIT,LOUTSTR,99,ND,1,'first 4 coord',IER)
            IF(IER.NE.0)goto 1002
            K=0
            CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','XBP 1',IER)
            XBP(icomp,nbo,1)=val1
            CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','YBP 1',IER)
            YBP(icomp,nbo,1)=val1
            CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','ZBP 1',IER)
            ZBP(icomp,nbo,1)=val1
            CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','XBP 2',IER)
            XBP(icomp,nbo,2)=val1
            CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','YBP 2',IER)
            YBP(icomp,nbo,2)=val1
            CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','ZBP 2',IER)
            ZBP(icomp,nbo,2)=val1
            CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','XBP 3',IER)
            XBP(icomp,nbo,3)=val1
            CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','YBP 3',IER)
            YBP(icomp,nbo,3)=val1
            CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','ZBP 3',IER)
            ZBP(icomp,nbo,3)=val1
            CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','XBP 4',IER)
            XBP(icomp,nbo,4)=val1
            CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','YBP 4',IER)
            YBP(icomp,nbo,4)=val1
            CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','ZBP 4',IER)
            ZBP(icomp,nbo,4)=val1

            CALL LSTRIPC(IUNIT,LOUTSTR,99,ND,1,'2nd 4 coord',IER)
            IF(IER.NE.0)goto 1002
            K=0
            CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','XBP 5',IER)
            XBP(icomp,nbo,5)=val1
            CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','YBP 5',IER)
            YBP(icomp,nbo,5)=val1
            CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','ZBP 5',IER)
            ZBP(icomp,nbo,5)=val1
            CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','XBP 6',IER)
            XBP(icomp,nbo,6)=val1
            CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','YBP 6',IER)
            YBP(icomp,nbo,6)=val1
            CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','ZBP 6',IER)
            ZBP(icomp,nbo,6)=val1
            CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','XBP 7',IER)
            XBP(icomp,nbo,7)=val1
            CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','YBP 7',IER)
            YBP(icomp,nbo,7)=val1
            CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','ZBP 7',IER)
            ZBP(icomp,nbo,7)=val1
            CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','XBP 8',IER)
            XBP(icomp,nbo,8)=val1
            CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','YBP 8',IER)
            YBP(icomp,nbo,8)=val1
            CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','ZBP 8',IER)
            ZBP(icomp,nbo,8)=val1
            goto 72

          elseif(phrase(1:4).eq.'*obs')then

            if(iobs(icomp).eq.1) goto 72  ! keep reading till *end_block
            if(iobs(icomp).eq.0) iobs(icomp)=2
            nbobs(icomp)=nbobs(icomp)+1
            nbo=nbobs(icomp)
            CALL EGETWR(LOUTSTR,K,VX,-999.,998.,'W','obs X org',IER)
            CALL EGETWR(LOUTSTR,K,VY,-999.,998.,'W','obs Y org',IER)
            CALL EGETWR(LOUTSTR,K,VZ, -99., 99.,'W','obs Z org',IER)
            XOB(icomp,nbo)=VX
            YOB(icomp,nbo)=VY
            ZOB(icomp,nbo)=VZ
            CALL EGETWR(LOUTSTR,K,VX,0., 99.,'W','obs X dis',IER)
            CALL EGETWR(LOUTSTR,K,VY,0., 99.,'W','obs Y dis',IER)
            CALL EGETWR(LOUTSTR,K,VZ,0., 99.,'W','obs Z dis',IER)
            DXOB(icomp,nbo)=VX
            DYOB(icomp,nbo)=VY
            DZOB(icomp,nbo)=VZ
            CALL EGETWR(LOUTSTR,K,VX,-359.,359.,'W','obs rot ang',IER)
            BANGOB(icomp,nbo,1)=VX
            BANGOB(icomp,nbo,2)=0.0   ! there is no 2nd rotation
            BANGOB(icomp,nbo,3)=0.0   ! there is no 3rd rotation
            if(ND.ge.11)then  ! if enough items for opacity
              CALL EGETWR(LOUTSTR,K,VX,0.,1.,'W','obs opacity',IER)
              OPOB(icomp,nbo)=VX
            else
              OPOB(icomp,nbo)=1.0  ! set to opaque if not specified
            endif
            CALL EGETW(LOUTSTR,K,WORD,'W','obs blk name',IFLAG)
            BLOCKNAME(icomp,nbo)=WORD(1:12)
            LNBLOCKNAME(icomp,nbo)=lnblnk(BLOCKNAME(icomp,nbo))

C The name of the construction might contain spaces so use EGETP.
            CALL EGETP(LOUTSTR,K,WORD,'W','obs mat name',IFLAG)
            write(BLOCKMAT(icomp,nbo),'(a)') WORD(1:lnblnk(WORD))
            LNBLOCKMAT(icomp,nbo)=lnblnk(BLOCKMAT(icomp,nbo))
            BLOCKTYP(icomp,nbo)='obs '
            goto 72
          elseif(phrase(1:4).eq.'*mrt')then

C << not yet implemented >>
            goto 72
          elseif(phrase(1:6).eq.'*photo')then

C << not yet implemented >>
            goto 72
          elseif(phrase(1:10).eq.'*end_block')then
            goto 62
          endif
        elseif(WORD(1:13).eq.'*visual_start')then

C << to be done >> 
          goto 62
        elseif(WORD(1:22).eq.'*zone_viewfactor_start')then

C << to be done >> 
          goto 62
        elseif(WORD(1:24).eq.'*sensor_viewfactor_start')then

C  << to be done >>
          goto 62
        elseif(WORD(1:24).eq.'*sensor_viewfactor_start')then

C  << to be done >>
        else

C Fall through position. Warn and loop back to see if more.
          write(outs,'(2a)') ' Unknown tag in geometry file ',word
          call edisp(iuout,outs)
          goto 62
        endif

      ELSE
        write(outs,'(2a)') ' Geometry shape type illegal in ',
     &    LGEOMF(1:lnblnk(LGEOMF))
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

C << to this point in the logic... >>

C Now close geometry data file.
   44 CALL ERPFREE(IUNIT,ios)

C If base area has not yet been calculated, do this now and calculate 
C transparent area for surfaces connected to external boundary.
C Get surface areas via call to zinfo.
      call zinfo(sna,zoa,pazi,pelv,zvol)
      vol(ICOMP)=zvol
      zonetotsurfacearea(ICOMP)=zoa
      ZTRANA(ICOMP)=0.
      do 43 ijj=1,NSUR
        if(checkbase)then
          if(SVFC(ijj)(1:4).eq.'FLOR')then

C If surface `flor` not included in the list add it.
            lastlist=lastlist+1
            if(lastlist.le.10)then
              IBASES(ICOMP,lastlist)=ijj
              ZBASEA(ICOMP)=ZBASEA(ICOMP)+SNA(ijj)
            endif
          endif
        endif
        if (SOTF(ijj)(1:4).ne.'OPAQ'.and.SOTF(ijj)(1:4).ne.'CFC '.and.
     &      SOTHER(ijj,1)(1:8).eq.'EXTERIOR') then
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

C Errors for loutstr reads.
 1002 write(outs,'(3a)') 'GEOREAD: conversion error in...',
     &  LOUTSTR(1:50),'...'
      lsn=MIN0(lnblnk(currentfile),110)
      write(outs2,'(2a)') 'in: ',currentfile(1:lsn)
      if(dll)then
        dllsubr='EGOMIN'
        dllmesg=outs
        ier=2
        CALL ERPFREE(IUNIT,ios)
        return
      else
        call edisp(iuout,outs)
        call edisp(iuout,outs2)
        IER=1
        CALL ERPFREE(IUNIT,ios)
        RETURN
      endif

      END

C ************* GEOWRITE
C GEOWRITE to write a geometry file (GEN V1.1 type) based on infor-
C mation currently held in common blocks G0 G1 G3 G4 G6.  It is 
C assumed that this information has been checked.
C GENFIL is the name of the file to be written to (any existing file
C by this name is overwritten).  
C ICOMP is the zone number, 
C ITRC unit number for user output,
C IER=0 OK IER=1 problem. IWF = 3 create/overwrite,=4 check
C with user before overwriting.

C If iobs()=2 then there are blocks to write out within the
C geometry file.

C If the user requested a rotation then remember it via rotateit array.

      SUBROUTINE GEOWRITE(IFILG,GENFIL,ICOMP,ITRU,iwf,IER)
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "espriou.h"
      
      integer lnblnk  ! function definition

C NOTE: G1 is only valid for the zone that has currently been
C scanned and is being worked on. ZNDATA holds this information globally. 

C << todo: geowrite could use ZNDATA if there was a way for the
C << calling code to signal that ZNDATA could be used instead.

C iaplic(1) toggle for shading; iaplic(2) toggle for insolation.
C   toggle for assessment where:
C   1 is 'all_applicable', 0 is manual selection of surfaces.
C nsurfcalc nb of shaded surfaces, lstsfcalc() list of applicable surfaces.
C nsurfinso nb of insolation sources, isurfinso() list of insolation sources.
      common/ishdirec/iaplic(MCOM,2),nsurfcalc(MCOM),lstsfcalc(MCOM,MS),
     &       nsurfinso(MCOM),isurfinso(MCOM,MS)

C ZNAME (12 char) - the zone name (from geometry.h).
C ZDESC (64 char) - zone notes (from geometry.h).
      COMMON/C24/IZSTOCN(MCOM,MS)
               
C Thermal bridge common block is described in esruprj/edgeo.F
      integer nbrdg, ibrdg
      real psi,lngth,losspercent,totheatloss,thbrpercent
      real uavtotal
      common/THRBRDG/nbrdg(MCOM),psi(MCOM,16),lngth(MCOM,16),
     &               ibrdg(MCOM,16),losspercent(MCOM),totheatloss(MCOM),
     &               thbrpercent,uavtotal(MCOM)

C Error subroutine and error details for dll mode.
      character dllsubr*12,dllmesg*124
      common/dllerr/dllsubr,dllmesg

C Obstruction blocks common GS5/GS6 is found in geometry.h.

      CHARACTER GENFIL*72
      character outs*144,outsd*144,outs72*72,OTHSTR*30
      character louts*248,loutsd*248
      character tab*1    ! separator
      character dstmp*24
      character phrase*36 ! for thermal bridge tag
      logical dll
      integer  lso,lso2,lso3  ! string lengths of sother()
      integer  lsml           ! string length of SMLCN()
      integer  lspa           ! string length of sparent()
      integer lsna            ! string length of sname()
      integer lsot            ! string length of soft()
      logical newgeo  ! to use for testing if new/old geometry file.
      logical havebrdge ! to use for thermal bridge existance.
      logical hasobstr  !  true if IOBS() is set to 1 and NB > 0
      logical rotationknown  ! true if we need to remember a rotation

      IER=0
      tab=','    ! make the separator a comma.
      newgeo=.false.  ! assume older format geometry.
      havebrdge=.false.
      hasobstr=.false.

C Check if running in dll mode and get current time stamp.
      call isadll(dll)
      call dstamp(dstmp)

C Open any existing file by this name (ask user for confirmation to
C over-write) or create a new file.
      if(iwf.eq.4)then
        CALL EFOPSEQ(IFILG,GENFIL,4,IER)
      else
        CALL EFOPSEQ(IFILG,GENFIL,3,IER)
      endif
      IF(IER.LT.0)THEN
        write(outs,'(3a)') 'Geometry file ',GENFIL(1:lnblnk(GENFIL)),
     &      ' could not be written.'
        if(dll)then
          dllsubr='GEOWRITE'
          write(dllmesg,'(a)') outs(1:124)
          ier=2
          return
        else
          call edisp(itru,outs)
          IER=1
          RETURN
        endif
      ENDIF
      write(currentfile,'(a)') GENFIL(1:lnblnk(GENFIL))

C Write out the header including date stamp and documentation.
      lz=lnzname(ICOMP)
      write(ifilg,'(6a)',IOSTAT=ios,ERR=13) '*Geometry 1.1',tab,
     &  'GEN',tab,zname(ICOMP)(1:lz),
     &  ' # tag version, format, zone name'
      write(ifilg,'(3a)',IOSTAT=IOS,ERR=13) '*date ',dstmp,
     &  '  # latest file modification '

      write(ifilg,'(a)',IOSTAT=IOS,ERR=13)zdesc(ICOMP)(1:lnzdesc(ICOMP))

C If rotateit array is all zero then skip. Otherwise write out the last
C directive that the user gave.
      call eclose3(rotateit(icomp,4),rotateit(icomp,5),
     &  rotateit(icomp,6),0.0,0.0,0.0,0.01,rotationknown)
      if(rotationknown)then
        continue
      else
        write(ifilg,'(a,f6.2,a,f8.3,a,f8.3,a,f8.3,a)',IOSTAT=IOS,
     &    ERR=13) '*previous_rotate ',
     &    rotateit(icomp,4),tab,rotateit(icomp,5),tab,
     &    rotateit(icomp,6),'  # prior rotation angle X Y'
      endif

      write(ifilg,'(a)',IOSTAT=IOS,ERR=13) 
     &  '# tag, X co-ord, Y co-ord, Z co-ord'

C Write vertices with minimal white space comma separated.
      DO 960 I = 1,NZTV(ICOMP)
        WRITE(outs,'(a,3F12.5)',IOSTAT=ios,ERR=13)'*vertex ',
     &    X(I),Y(I),Z(I)
        call SDELIM(outs,outsd,'C',IW)
        write(ifilg,'(2a,i3)',IOSTAT=IOS,ERR=13) 
     &    outsd(1:lnblnk(outsd)),'  # ',I
960   CONTINUE

C Edge list. Write as a packed list comma separated.
      write(ifilg,'(a)',IOSTAT=IOS,ERR=13) '# '
      write(ifilg,'(a)',IOSTAT=IOS,ERR=13) 
     &  '# tag, number of vertices followed by list of associated vert'

      DO 970 I = 1,NZSUR(ICOMP)
        write(louts,'(a,42I4)',IOSTAT=ios,ERR=13) '*edges ',NVER(I),
     &     (JVN(I,J),J=1,NVER(I))
        call SDELIM(louts,loutsd,'C',IW)
        write(ifilg,'(2a,i2)',IOSTAT=IOS,ERR=13) 
     &    loutsd(1:lnblnk(loutsd)),'  # ',I
970   CONTINUE

C Surface list. Write as a packed list comma separated.
      write(ifilg,'(a)',IOSTAT=IOS,ERR=13) '# '
      write(ifilg,'(a)',IOSTAT=IOS,ERR=13) '# surf attributes:'
      write(ifilg,'(a)',IOSTAT=IOS,ERR=13) 
     &  '#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN'
      write(ifilg,'(a)',IOSTAT=IOS,ERR=13) 
     &  '#  child of (surface name), useage (pair of tags) '
      write(ifilg,'(a)',IOSTAT=IOS,ERR=13) 
     &  '#  construction name, optical name'
      write(ifilg,'(a)',IOSTAT=IOS,ERR=13) 
     &  '#  boundary condition tag followed by two data items'

      DO 102 I=1,NZSUR(icomp)
        call OTHERINFO(icomp,i,OTHSTR)
        icc=izstocn(icomp,i)

C Based on the current contents of SOTHER(I,1) create a buffer SO
C to write out.
        lso=lnblnk(SSOTHER(icc,1))
        lso2=lnblnk(SSOTHER(icc,2))
        lso3=lnblnk(SSOTHER(icc,3))
        lsml=lnblnk(SSMLCN(icc))
        lspa=lnblnk(SSPARENT(icc))
        lsna=lnblnk(SSNAME(icc))
        lsot=lnblnk(SSOTF(icc))
        loth=lnblnk(OTHSTR)
        WRITE(outs,'(20a)',IOSTAT=ios,ERR=13) '*surf ',
     &    SSNAME(icc)(1:lsna),' ',SSVFC(icc),' ',
     &    SSPARENT(icc)(1:lspa),' ',SSUSE(icc,1),' ',SSUSE(icc,2),
     &    ' ',SSMLCN(icc)(1:lsml),' ',SSOTF(icc)(1:lsot),' ',
     &    SSOTHER(icc,1)(1:lso),' ',SSOTHER(icc,2)(1:lso2),
     &    ' ',SSOTHER(icc,3)(1:lso3)
        call SDELIM(outs,outsd,'C',IW)
        write(ifilg,'(2a,i3,2a)',IOSTAT=IOS,ERR=13) 
     &    outsd(1:lnblnk(outsd)),'  # ',I,' ',OTHSTR(1:loth)
  102 CONTINUE

C Default insolation distribution as packed string.
      write(ifilg,'(a)',IOSTAT=IOS,ERR=13) '# '
      WRITE(outs,'(a,4I4)',IOSTAT=ios,ERR=14)
     &  '*insol ',NDP(ICOMP),(IDPN(ICOMP,J),J=1,3)
      call SDELIM(outs,outsd,'C',IW)
      write(ifilg,'(2a)',IOSTAT=IOS,ERR=13) 
     &  outsd(1:lnblnk(outsd)),'  # default insolation distribution'

C Shading calculation directives in the form of:
C *shad_calc,all_applic,20,20
      write(ifilg,'(a)',IOSTAT=IOS,ERR=13) '# '
      if(nsurfcalc(icomp).eq.0)then
        write(ifilg,'(a)',IOSTAT=IOS,ERR=13) '# shading directives'
        write(ifilg,'(a)',IOSTAT=ios,ERR=13)
     &    '*shad_calc,none  # no temporal shading requested'
      elseif(nsurfcalc(icomp).gt.0)then
        write(ifilg,'(a)',IOSTAT=IOS,ERR=13) '# shading directives'
        if(iaplic(icomp,1).eq.1)then

C << still need variables for the grid density >>
C << could fall over if there are more than 42 ext surfaces, consider >>
C << using an array writing call instead >>
          write(ifilg,'(a,i3,a)',IOSTAT=ios,ERR=13)
     &      '*shad_calc,all_applicable ',nsurfcalc(icomp),
     &      ' # list of surfs'
          write(ifilg,'(42i3)',IOSTAT=ios,ERR=14)
     &      (lstsfcalc(ICOMP,J),J=1,nsurfcalc(icomp))
        elseif(iaplic(icomp,1).eq.0)then
          write(ifilg,'(a,i3,a)',IOSTAT=ios,ERR=13)
     &      '*shad_directive,list',nsurfcalc(icomp),
     &      ' # list of surfs'
          write(ifilg,'(42i3)',IOSTAT=ios,ERR=14)
     &      (lstsfcalc(ICOMP,J),J=1,nsurfcalc(icomp))
        endif
      endif

C Insolation calculation directives.

C << could fall over if there are more than 42 sources, consider >>
C << using an array writing call instead >>

      write(ifilg,'(a)',IOSTAT=IOS,ERR=13) '# '
      if(nsurfinso(icomp).eq.0)then
        write(ifilg,'(a)',IOSTAT=ios,ERR=13)
     &    '*insol_calc,none  # no insolation requested'
      elseif(nsurfinso(icomp).gt.0)then
        if(iaplic(icomp,2).eq.1)then
          write(ifilg,'(a,i3,a)',IOSTAT=ios,ERR=13)
     &      '*insol_calc,all_applicable ',nsurfinso(icomp),
     &      ' # insolation sources'
          write(ifilg,'(42i3)',IOSTAT=ios,ERR=14)
     &      (isurfinso(ICOMP,J),J=1,nsurfinso(icomp))
        elseif(iaplic(icomp,2).eq.0)then
          write(ifilg,'(a,i3,a)',IOSTAT=ios,ERR=13)
     &      '*insol_calc,list',nsurfinso(icomp),
     &      ' # insolation sources'
          write(ifilg,'(42i3)',IOSTAT=ios,ERR=14)
     &      (isurfinso(ICOMP,J),J=1,nsurfinso(icomp))
        endif
      endif

C Surfaces associated with base.
      write(ifilg,'(a)',IOSTAT=IOS,ERR=13) '# '
      if(izbaselist(icomp).eq.0)then
        WRITE(outs,'(a,i2,F9.2,i2)',IOSTAT=ios,ERR=13) '*base_list ',
     &    izbaselist(icomp),ZBASEA(ICOMP),IUZBASEA(ICOMP)
        call SDELIM(outs,outsd,'C',IW)
        write(ifilg,'(2a)',IOSTAT=IOS,ERR=13) 
     &    outsd(1:lnblnk(outsd)),'  # zone base'
      else

C Write the list and then the data after the list.
        WRITE(outs,'(a,12i4)',IOSTAT=ios,ERR=13) '*base_list ',
     &    izbaselist(icomp),(IBASES(icomp,J),J=1,izbaselist(icomp))
        write(outs72,'(F9.2,i2)') ZBASEA(ICOMP),IUZBASEA(ICOMP)
        call SDELIM(outs,outsd,'C',IW)
        write(ifilg,'(4a)',IOSTAT=IOS,ERR=13) 
     &    outsd(1:lnblnk(outsd)),',',outs72(1:lnblnk(outs72)),
     &    '  # zone base list'
      endif

C Write out thermal bridge info if there are any thermal bridges
C or the losspercent is non-zero.
      if(losspercent(icomp).gt.0.0) havebrdge=.true.
      if(nbrdg(icomp).gt.0.or.havebrdge)then
        write(ifilg,'(a)',IOSTAT=IOS,ERR=13) '# '
        WRITE(outs,'(a,F6.3,3F9.3)',IOSTAT=ios,ERR=13) '*bridge_start ',
     &    thbrpercent,losspercent(icomp),totheatloss(icomp),
     &    uavtotal(icomp)
        call SDELIM(outs,outsd,'C',IW)
        write(ifilg,'(2a)',IOSTAT=IOS,ERR=13) 
     &    outsd(1:lnblnk(outsd)),'  # user fraction,losspercent W/K, tot
     &al heat loss W/K and total UA W/K'
        if(nbrdg(icomp).gt.0)then
          do 88 itb=1,nbrdg(icomp)
            if(ibrdg(icomp,itb).eq.1)then
              phrase='roof-wall '
            elseif(ibrdg(icomp,itb).eq.2)then
              phrase='wall-ground floor '
            elseif(ibrdg(icomp,itb).eq.3)then
              phrase='wall-wall (convex corner) '
            elseif(ibrdg(icomp,itb).eq.4)then
              phrase='wall-wall (concave corner) '
            elseif(ibrdg(icomp,itb).eq.5)then
              phrase='wall-floor (not ground floor) '
            elseif(ibrdg(icomp,itb).eq.6)then
              phrase='lintel above window or door '
            elseif(ibrdg(icomp,itb).eq.7)then
              phrase='Sill below window '
            elseif(ibrdg(icomp,itb).eq.8)then
              phrase='jamb at window or door '
            elseif(ibrdg(icomp,itb).eq.9)then
              phrase='roof-wall-mc'
            elseif(ibrdg(icomp,itb).eq.10)then
              phrase='wall-ground floor-mc'
            elseif(ibrdg(icomp,itb).eq.11)then
              phrase='wall-wall (corner)-mc'
            elseif(ibrdg(icomp,itb).eq.12)then
              phrase='wall-floor (not ground floor)-mc'
            elseif(ibrdg(icomp,itb).eq.13)then
              phrase='lintel above window or door-mc'
            elseif(ibrdg(icomp,itb).eq.14)then
              phrase='Sill below window-mc'
            elseif(ibrdg(icomp,itb).eq.15)then
              phrase='jamb at window or door-mc'
            elseif(ibrdg(icomp,itb).eq.16)then
              phrase='user-defined'
            endif

C The phrase can have spaces in it so do not pack string.
            WRITE(outs,'(4a,F7.3,a,F8.4,a)',IOSTAT=ios,ERR=13) 
     &        '*ukt_bridge',tab,phrase(1:lnblnk(phrase)),tab,
     &        lngth(icomp,itb),tab,psi(icomp,itb),
     &        '  # type length psi value'
            write(ifilg,'(a)',IOSTAT=IOS,ERR=13) outs(1:lnblnk(outs))
  88      continue
        endif
        write(ifilg,'(a)',IOSTAT=IOS,ERR=13) '*end_bridge'
      endif

C iobs is 2 if there are block descriptions to include in this zone
C geometry file also write the NOX and NOZ values.
      if(IOBS(icomp).eq.2.and.nbobs(icomp).gt.0)then
        hasobstr=.true.
      endif
      if(hasobstr)then
        write(ifilg,'(a)',IOSTAT=IOS,ERR=13) '# '
        write(ifilg,'(a)',IOSTAT=IOS,ERR=13) '# block entities:'
        write(ifilg,'(a)',IOSTAT=IOS,ERR=13) '#  *obs = obstructions'
        write(ifilg,'(2a,i2,i3,a)',IOSTAT=IOS,ERR=13) 
     &  '*block_start',tab,NOX(icomp),NOZ(icomp),' # geometric blocks'
        do 89 ib=1,nbobs(icomp)
          if(BLOCKTYP(icomp,ib)(1:4).eq.'obs3')then
            WRITE(outs,'(2a,9F9.3,F6.2,1X,A,1X,A)',IOSTAT=ios,ERR=13) 
     &        '*obs3',tab,XOB(icomp,ib),YOB(icomp,ib),ZOB(icomp,ib),
     &        DXOB(icomp,ib),DYOB(icomp,ib),DZOB(icomp,ib),
     &        BANGOB(icomp,ib,1),BANGOB(icomp,ib,2),BANGOB(icomp,ib,3),
     &        OPOB(icomp,ib),BLOCKNAME(icomp,ib),BLOCKMAT(icomp,ib)
            call SDELIM(outs,outsd,'C',IW)
            write(ifilg,'(2a,i2)',IOSTAT=IOS,ERR=13) 
     &        outsd(1:lnblnk(outsd)),'  # block ',ib
          elseif(BLOCKTYP(icomp,ib)(1:4).eq.'obsp')then
            WRITE(outs,'(2a,F6.2,1X,A,1X,A)',IOSTAT=ios,ERR=13) 
     &        '*obsp',' 8 6 ',OPOB(icomp,ib),BLOCKNAME(icomp,ib),
     &        BLOCKMAT(icomp,ib)
            call SDELIM(outs,outsd,'C',IW)
            write(ifilg,'(2a,i2,a)',IOSTAT=IOS,ERR=13) 
     &        outsd(1:lnblnk(outsd)),'  # block ',ib,
     &        ' coords follow:'

            WRITE(outs,'(12F9.3)',IOSTAT=ios,ERR=13) 
     &        XBP(icomp,ib,1),YBP(icomp,ib,1),ZBP(icomp,ib,1),
     &        XBP(icomp,ib,2),YBP(icomp,ib,2),ZBP(icomp,ib,2),
     &        XBP(icomp,ib,3),YBP(icomp,ib,3),ZBP(icomp,ib,3),
     &        XBP(icomp,ib,4),YBP(icomp,ib,4),ZBP(icomp,ib,4)
            call SDELIM(outs,outsd,'C',IW)
            write(ifilg,'(2A)',IOSTAT=IOS,ERR=13) 
     &        outsd(1:lnblnk(outsd)),'  # 1-4 '

            WRITE(outs,'(12F9.3)',IOSTAT=ios,ERR=13) 
     &        XBP(icomp,ib,5),YBP(icomp,ib,5),ZBP(icomp,ib,5),
     &        XBP(icomp,ib,6),YBP(icomp,ib,6),ZBP(icomp,ib,6),
     &        XBP(icomp,ib,7),YBP(icomp,ib,7),ZBP(icomp,ib,7),
     &        XBP(icomp,ib,8),YBP(icomp,ib,8),ZBP(icomp,ib,8)
            call SDELIM(outs,outsd,'C',IW)
            write(ifilg,'(2A)',IOSTAT=IOS,ERR=13) 
     &        outsd(1:lnblnk(outsd)),'  # 5-8 '

          elseif(BLOCKTYP(icomp,ib)(1:4).eq.'obs ')then
            WRITE(outs,'(2a,7F9.3,F6.2,1X,A,1X,A)',IOSTAT=ios,ERR=13) 
     &        '*obs',tab,XOB(icomp,ib),YOB(icomp,ib),ZOB(icomp,ib),
     &        DXOB(icomp,ib),DYOB(icomp,ib),DZOB(icomp,ib),
     &        BANGOB(icomp,ib,1),OPOB(icomp,ib),BLOCKNAME(icomp,ib),
     &        BLOCKMAT(icomp,ib)
            call SDELIM(outs,outsd,'C',IW)
            write(ifilg,'(2a,i2)',IOSTAT=IOS,ERR=13) 
     &        outsd(1:lnblnk(outsd)),'  # block ',ib
          endif
  89    continue
        write(ifilg,'(a)',IOSTAT=IOS,ERR=13) '*end_block'
      endif

C If file written with this source and the version number is 1.0
C then reset it to 1.1.
      call eclose(gversion(icomp),1.1,0.01,newgeo)
      if(.NOT.newgeo) gversion(icomp) = 1.1

   99 CALL ERPFREE(IFILG,ios)
      RETURN

C Error messages.
   13 if(IOS.eq.2)then
        CALL USRMSG('No permission to write ',GENFIL,'W')
      else
        CALL USRMSG('File write error in ',GENFIL,'W')
      endif
      IER=1
      GOTO 99
   14 if(IOS.eq.2)then
        CALL USRMSG('No prmission to write array in ',GENFIL,'W')
      else
        CALL USRMSG('Long arrary write error in ',GENFIL,'W')
      endif
      IER=1
      GOTO 99
      
      END
 

C ******************** ERECC 
C ERECC converts a REC (rectilinear) description into a GEN description.
C The X,Y, & Z block origin coordinates of the 'Bottom left-hand' corner
C (when viewed from the south),
C the length (metres from the BLHC towards the east),
C width (metres from the BLHC towards the north),
C height (metres from the BLHC upwards),
C angle of rotation A degrees between the length side and due east axis
C (anticlockwise is positive).
C Information is converted into the common block G1.

C << add zone index to passed parameters so g1 no longer needed >>

      SUBROUTINE ERECC(XO,YO,ZO,DX,DY,DZ,A)
#include "building.h"
#include "geometry.h"
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

C ******************** ERECC3A 
C ERECC converts a REC (rectilinear) description into a GEN description.
C The X,Y, & Z block origin coordinates of the 'Bottom left-hand' corner
C (when viewed from the south),
C the length (metres from the BLHC towards the east),
C width (metres from the BLHC towards the north),
C height (metres from the BLHC upwards),
C angle of rotation A degrees between the length side and due east axis
C (anticlockwise is positive). This alters the X and Y coordinates but
C not the Z coordinate.
C The angle of rotation B is between the length side and the horizon
C (positive lifts the edge). This alters the X and Z coordinates but
C not the Y coordinate.
C The angle of rotation C is between the width side and the horizon
C (positive lifts the edge) NOT YET implemented. This alters the Y
C and Z coordinates but not the X.
C Information is converted into the common block G1.

C << add zone index to passed parameters so g1 no longer needed >>

      SUBROUTINE ERECC3A(XO,YO,ZO,DX,DY,DZ,A,B,C)
#include "building.h"
#include "geometry.h"
C Parameters
      real XO,YO,ZO  ! origin of the box
      real DX,DY,DZ  ! length, width, height of box
      real A,B,C     ! angles of rotation

      real PI,R,SA,CA,SB,CB ! local variables
C     real SC,CC
      logical CorrectAngle,bCorrectAngle,cCorrectAngle
      
      PI = 4.0 * ATAN(1.0)
      R=PI/180.
      SA=SIN(A*R)  ! for a angle
      CA=COS(A*R)
      SB=SIN(B*R)  ! for b angle
      CB=COS(B*R)
C      SC=SIN(C*R)  ! for c angle
C      CC=COS(C*R)

      NSUR=6
      DO 10 I=1,NSUR
        NVER(I)=4
   10 CONTINUE

C To prevent infinity set CB to zero if B close to 90 or 270 degrees.
      call eclose(B,90.0,0.02,CorrectAngle)
      call eclose(B,270.0,0.02,bCorrectAngle)
      if(CorrectAngle.or.bCorrectAngle)then
        CB=0.0
      endif
   
C To prevent infinity set SB to zero if B close to 0 180 or 360 degrees.
      call eclose(B,0.0,0.02,CorrectAngle)
      call eclose(B,180.0,0.02,bCorrectAngle)
      call eclose(B,360.0,0.02,cCorrectAngle)      
      if(CorrectAngle.or.bCorrectAngle.or.cCorrectAngle)then
        SB=0.0
      endif

C To prevent infinity set CA to zero if A close to 90 or 270 degrees.
      call eclose(A,90.0,0.02,CorrectAngle)
      call eclose(A,270.0,0.02,bCorrectAngle)
      if(CorrectAngle.or.bCorrectAngle)then
        CA=0.0
      endif
   
C To prevent infinity set SA to zero if A close to 0 180 or 360 degrees.
      call eclose(A,0.0,0.02,CorrectAngle)
      call eclose(A,180.0,0.02,bCorrectAngle)
      call eclose(A,360.0,0.02,cCorrectAngle)      
      if(CorrectAngle.or.bCorrectAngle.or.cCorrectAngle)then
        SA=0.0
      endif

C Assign corners of the box assuming no rotation.
      X(1)=XO; X(2)=X(1)+DX; X(3)=X(2); X(4)=X(3)-DX 
      X(5)=XO; X(6)=X(2); X(7)=X(3); X(8)=X(4)
      Y(1)=YO; Y(2)=Y(1); Y(3)=Y(2)+DY; Y(4)=Y(3)
      Y(5)=YO; Y(6)=Y(2); Y(7)=Y(3); Y(8)=Y(4)
      Z(1)=ZO; Z(2)=ZO; Z(3)=ZO; Z(4)=ZO
      Z(5)=ZO+DZ; Z(6)=ZO+DZ; Z(7)=ZO+DZ; Z(8)=ZO+DZ

C Do a B angle of rotation first. No Y changes required.
C X(1) and X(4) do not change. 
C X(2) and X(3) change based on DX length.

C Debug.
C      write(6,*) CB
C      write(6,*) DX

      X(2)=X(1)+(DX*CB); X(3)=X(4)+(DX*CB)

C X(5) & X(8) change based on DZ length. 
      X(5)=X(5)-(DZ*SB); X(8)=X(8)-(DZ*SB)

C X(6) & X(7) change based on DX length.
      X(6)=X(5)+(DX*CB); X(7)=X(8)+(DX*CB)

C Z(1) & Z(4) do not change. 
C Z(2) & Z(3) change based on DX length.
      Z(2)=Z(2)+(DX*SB); Z(3)=Z(3)+(DX*SB) 

C Z(5) & Z(8) change based on DZ length.
      Z(5)=ZO+(CB*DZ); Z(8)=ZO+(CB*DZ) 
      
C Z(6) & Z(7) change based on DZ length.
      Z(6)=Z(2)+(DZ*CB); Z(7)=Z(3)+(DZ*CB) 

C Moving now to the Y rotation
C Do an A angle of rotation. No Z changes required.
C X(1) does not change.       
C X(2) changes based on [X(2)-X(1)] length

C Debug.
C      write(6,*) CA
C      write(6,*) DX

      X(2)=X(1)+(CB*DX)*CA
      X(3)=X(2)-(SA*DY)
      X(4)=X(1)-(SA*DY)
      X(5)=X(1)-(SB*DZ)*CA
      X(6)=X(5)+(CB*DX)*CA
      X(8)=X(4)-(SB*DZ)*CA
      X(7)=X(8)+(CB*DX)*CA

C Y(1) does not change.         
      Y(2)=Y(1)+((CB*DX)*SA)
      Y(4)=Y(1)+(CA*DY)
      Y(3)=Y(4)+((CB*DX)*SA)           
      Y(5)=Y(1)-(SB*DZ)*SA
      Y(6)=Y(5)+(CB*DX)*SA
      Y(8)=Y(4)-(SB*DZ)*SA
      Y(7)=Y(8)+((CB*DX)*SA)           

C Debug      
C      write(6,*) 'x1: ',X(1),'...','y1: ',Y(1),'...','z1: ',Z(1)
C      write(6,*) 'x2: ',X(2),'...','y2: ',Y(2),'...','z2: ',Z(2)
C      write(6,*) 'x3: ',X(3),'...','y3: ',Y(3),'...','z3: ',Z(3)
C      write(6,*) 'x4: ',X(4),'...','y4: ',Y(4),'...','z4: ',Z(4)
C      write(6,*) 'x5: ',X(5),'...','y5: ',Y(5),'...','z5: ',Z(5)
C      write(6,*) 'x6: ',X(6),'...','y6: ',Y(6),'...','z6: ',Z(6)      
C      write(6,*) 'x7: ',X(7),'...','y7: ',Y(7),'...','z7: ',Z(7)      
C      write(6,*) 'x8: ',X(8),'...','y8: ',Y(8),'...','z8: ',Z(8)      
C End debug          

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

C ******************** CNVBLK3A 
C CNVBLK3A converts a REC (rectilinear) description into GB1 common block.
C The X,Y, & Z coordinates of the 'Bottom left-hand' corner (when viewed
C from the south), the length (from the BL-HC towards the east), width,
C height of the enclosure.
C The angle of rotation A is between the length side and due east
C (anticlockwise positive). This alters the X and Y coordinates but
C not the Z coordinate.
C The angle of rotation B is between the length side and the horizon
C (positive lifts the edge). This alters the X and Z coordinates but
C not the Y coordinate.
C The angle of rotation C is between the width side and the horizon
C (positive lifts the edge) not yet implemented. This alters the Y
C and Z coordinates but not the X.

      SUBROUTINE CNVBLK3A(XO,YO,ZO,DX,DY,DZ,A,B,C)

C Parameters
      real XO,YO,ZO  ! origin of the box
      real DX,DY,DZ  ! length, width, height of box
      real A,B,C     ! angles of rotation
      
      real XB,YB,ZB
      integer JVNB
      COMMON/GB1/XB(12),YB(12),ZB(12),JVNB(6,4)
      real PI,R,SA,CA,SB,CB ! local variables
C     real SC,CC
      integer NSB     ! number of sides
      logical CorrectAngle,bCorrectAngle,cCorrectAngle
     
      PI = 4.0 * ATAN(1.0)
      R=PI/180.
      SA=SIN(A*R)  ! for a angle
      CA=COS(A*R)
      SB=SIN(B*R)  ! for b angle
      CB=COS(B*R)
C      SC=SIN(C*R)  ! for c angle
C      CC=COS(C*R)
      NSB=6

C To prevent infinity set CB to zero if B close to 90 or 270 degrees.
      call eclose(B,90.0,0.02,CorrectAngle)
      call eclose(B,270.0,0.02,bCorrectAngle)
      if(CorrectAngle.or.bCorrectAngle)then
        CB=0.0
      endif
   
C To prevent infinity set SB to zero if B close to 0 180 or 360 degrees.
      call eclose(B,0.0,0.02,CorrectAngle)
      call eclose(B,180.0,0.02,bCorrectAngle)
      call eclose(B,360.0,0.02,cCorrectAngle)      
      if(CorrectAngle.or.bCorrectAngle.or.cCorrectAngle)then
        SB=0.0
      endif

C To prevent infinity set CA to zero if A close to 90 or 270 degrees.
      call eclose(A,90.0,0.02,CorrectAngle)
      call eclose(A,270.0,0.02,bCorrectAngle)
      if(CorrectAngle.or.bCorrectAngle)then
        CA=0.0
      endif
   
C To prevent infinity set SA to zero if A close to 0 180 or 360 degrees.
      call eclose(A,0.0,0.02,CorrectAngle)
      call eclose(A,180.0,0.02,bCorrectAngle)
      call eclose(A,360.0,0.02,cCorrectAngle)      
      if(CorrectAngle.or.bCorrectAngle.or.cCorrectAngle)then
        SA=0.0
      endif

C Assign corners of the box assuming no rotation.
      XB(1)=XO; XB(2)=XB(1)+DX; XB(3)=XB(2); XB(4)=XB(3)-DX 
      XB(5)=XO; XB(6)=XB(2); XB(7)=XB(3); XB(8)=XB(4)
      YB(1)=YO; YB(2)=YB(1); YB(3)=YB(2)+DY; YB(4)=YB(3)
      YB(5)=YO; YB(6)=YB(2); YB(7)=YB(3); YB(8)=YB(4)
      ZB(1)=ZO; ZB(2)=ZO; ZB(3)=ZO; ZB(4)=ZO
      ZB(5)=ZO+DZ; ZB(6)=ZO+DZ; ZB(7)=ZO+DZ; ZB(8)=ZO+DZ

C Do a B angle of rotation first. No Y changes required.
C XB(1) and XB(4) do not change. 
C XB(2) and XB (3) change based on DX length.

C Debug.
C      write(6,*) CB
C      write(6,*) DX

      XB(2)=XB(1)+(DX*CB); XB(3)=XB(4)+(DX*CB)

C XB(5) & XB(8) change based on DZ length. 
      XB(5)=XB(5)-(DZ*SB); XB(8)=XB(8)-(DZ*SB)

C XB(6) & XB(7) change based on DX length.
      XB(6)=XB(5)+(DX*CB); XB(7)=XB(8)+(DX*CB)

C ZB(1) & ZB(4) do not change. 
C ZB(2) & ZB(3) change based on DX length.
      ZB(2)=ZB(2)+(DX*SB); ZB(3)=ZB(3)+(DX*SB) 

C ZB(5) & ZB(8) change based on DZ length.
      ZB(5)=ZO+(CB*DZ); ZB(8)=ZO+(CB*DZ) 
      
C ZB(6) & ZB(7) change based on DZ length.
      ZB(6)=ZB(2)+(DZ*CB);  ZB(7)=ZB(3)+(DZ*CB) 
      
C Moving now to the Y rotation
C Do an A angle of rotation. No Z changes required.
C XB(1) does not change.       
C XB(2) changes based on [XB(2)-XB(1)] length

C Debug.
C      write(6,*) CA
C      write(6,*) DX

      XB(2)=XB(1)+(CB*DX)*CA
      XB(3)=XB(2)-(SA*DY)
      XB(4)=XB(1)-(SA*DY)
      XB(5)=XB(1)-(SB*DZ)*CA
      XB(6)=XB(5)+(CB*DX)*CA
      XB(8)=XB(4)-(SB*DZ)*CA
      XB(7)=XB(8)+(CB*DX)*CA

C YB(1) does not change.         
      YB(2)=YB(1)+((CB*DX)*SA)
      YB(4)=YB(1)+(CA*DY)
      YB(3)=YB(4)+((CB*DX)*SA)           
      YB(5)=YB(1)-(SB*DZ)*SA
      YB(6)=YB(5)+(CB*DX)*SA
      YB(8)=YB(4)-(SB*DZ)*SA
      YB(7)=YB(8)+((CB*DX)*SA)           

C Debug      
C      write(6,*) 'xb1: ',XB(1),'...','yb1: ',YB(1),'...','zb1: ',ZB(1)
C      write(6,*) 'xb2: ',XB(2),'...','yb2: ',YB(2),'...','zb2: ',ZB(2)
C      write(6,*) 'xb3: ',XB(3),'...','yb3: ',YB(3),'...','zb3: ',ZB(3)
C      write(6,*) 'xb4: ',XB(4),'...','yb4: ',YB(4),'...','zb4: ',ZB(4)
C      write(6,*) 'xb5: ',XB(5),'...','yb5: ',YB(5),'...','zb5: ',ZB(5)
C      write(6,*) 'xb6: ',XB(6),'...','yb6: ',YB(6),'...','zb6: ',ZB(6)      
C      write(6,*) 'xb7: ',XB(7),'...','yb7: ',YB(7),'...','zb7: ',ZB(7)      
C      write(6,*) 'xb8: ',XB(8),'...','yb8: ',YB(8),'...','zb8: ',ZB(8)      

C End debug          

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

C ******************** CNVBLKP 
C CNVBLK3A converts a six sided obstuction 'obsp' into GB1 common block.
C The first 8 point of common block GB1 are filled and the standard
C JVNB edges are filled.
      SUBROUTINE CNVBLKP(IZ,IB)
#include "building.h"
#include "geometry.h"

C Parameters
      integer iz,ib  ! IZ is the zone index and IB is the block inde
            
      real XB,YB,ZB
      integer JVNB
      COMMON/GB1/XB(12),YB(12),ZB(12),JVNB(6,4)
      integer NSB     ! number of sides
     
      NSB=6
      DO 20 I=1,8
        XB(I)=XBP(IZ,IB,I)
        YB(I)=YBP(IZ,IB,I)
        ZB(I)=ZBP(IZ,IB,I)
  20  CONTINUE

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

C << consider passing zone index to avoid G1 common block >>

      SUBROUTINE EREGC(NW,Z1,Z2,XX,YY)
#include "building.h"
#include "geometry.h"
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

C << consider zone index so no longer necessary to use G1 >>

      SUBROUTINE ESCROT(ANG,X1,Y1)
#include "building.h"
#include "geometry.h"

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
C of information currently in Common blocks G0 G6 G7 G20.
C If context is .true. then give verbose description of the boundary.
      SUBROUTINE SURINFO(ICOMP,ITRU,context)
#include "building.h"
#include "geometry.h"

      common/SFIG/NSIGFIG

      COMMON/C24/IZSTOCN(MCOM,MS)

      logical context
      logical newgeo  ! to use for testing if new/old geometry file.

      CHARACTER OUTSTR*124,OTHSTR*30
      CHARACTER SIGSTR*12
C      character SO*15  ! buffer to use for sother() info.
      integer lzn      ! length of zone name

C Check version number of file.
      newgeo=.false.  ! assume older format geometry.
      call eclose(gversion(icomp),1.1,0.01,newgeo)

C Surface summary: print header, followed by surface information.
      lzn=lnzname(ICOMP)
      call edisp(ITRU,' ')
      write(outstr,'(3a,i2,a)')' A summary of the surfaces in ',
     &  zname(ICOMP)(1:lzn),'(',icomp,') follows:'
      call edisp(ITRU,outstr)
      call edisp(ITRU,' ')

C Write header based on current version number.
      if(newgeo)then
        write(outstr,'(2a)')
     &    ' Sur| Area  |Azim|Elev| surface    |    geometry       |',
     &    ' construction |environment'
        call edisp(ITRU,OUTSTR)
        write(outstr,'(2a)')
     &    '    | m^2   |deg |deg | name       |optical|locat| use |',
     &    ' name         |other side '
        call edisp(ITRU,OUTSTR)
      else
        write(outstr,'(2a)')
     &    ' Sur| Area  |Azim|Elev| surface    | geometry |',
     &    ' construction |environment'
        call edisp(ITRU,OUTSTR)
        write(outstr,'(2a)')
     &    '    | m^2   |deg |deg | name       |type |loca|',
     &    ' name         |other side '
        call edisp(ITRU,OUTSTR)
      endif
      if(.NOT.context)then
        DO 893 I=1,NZSUR(icomp)
          ic=izstocn(icomp,i)

          call SIGFIG(SSNA(ic),NSIGFIG,RNO,SIGSTR,LSTR)
          if(newgeo)then
 895        FORMAT(I3,2X,A7,F5.0,F5.0,1X,A,1X,A,1X,A,1X,A,1X,A,2X,A)
            WRITE(OUTSTR,895)I,SIGSTR(1:7),SSPAZI(ic),SSPELV(ic),
     &        SSNAME(ic),SSOTF(ic)(1:8),SSVFC(ic)(1:4),
     &        SSUSE(ic,1)(1:5),SSMLCN(ic)(1:12),SSOTHER(ic,1)(1:15)
          else
 894        FORMAT(I3,2X,A7,F5.0,F5.0,1X,A12,1X,A6,1X,A4,1X,A12,2X,A13)
            WRITE(OUTSTR,894)I,SIGSTR(1:7),SSPAZI(ic),SSPELV(ic),
     &        SSNAME(ic),SSOTF(ic)(1:6),SSVFC(ic),SSMLCN(ic)(1:12),
     &        SSOTHER(ic,1)(1:13)
          endif
          call edisp(ITRU,OUTSTR)
 893    CONTINUE
      else
        DO 793 I=1,NZSUR(icomp)
          ic=izstocn(icomp,i)
          call OTHERINFO(icomp,i,OTHSTR)
          call SIGFIG(SSNA(ic),NSIGFIG,RNO,SIGSTR,LSTR)
          if(newgeo)then
            WRITE(OUTSTR,795)I,SIGSTR(1:7),SSPAZI(ic),SSPELV(ic),
     &      SSNAME(ic),SSOTF(ic)(1:8),SSVFC(ic)(1:4),
     &      SSUSE(ic,1)(1:5),SSMLCN(ic)(1:12),OTHSTR
 795        FORMAT(I3,2X,A7,F5.0,F5.0,1X,A,1X,A,1X,A,1X,A,1X,A,1X,A)
          else
            WRITE(OUTSTR,794)I,SIGSTR(1:7),SSPAZI(ic),SSPELV(ic),
     &      SSNAME(ic),SSOTF(ic)(1:6),SSVFC(ic),SSMLCN(ic)(1:12),OTHSTR
 794        FORMAT(I3,2X,A7,F5.0,F5.0,1X,A12,1X,A6,1X,A4,1X,A12,1X,A)
          endif
          call edisp(ITRU,OUTSTR)
 793    CONTINUE
      endif
      call edisp(ITRU,' ')

C << if newgeo is true then this might be the place to write
C << out the obstruction information (after data structure has
C << been updated so the zone does not need to be scanned.

      RETURN
      END

C **************** OTHERINFO 
C OTHERINFO returns an English description in OTHSTR of the boundary 
C condition at the other side of a surface (based on information in
C the connections file).

      SUBROUTINE OTHERINFO(izone,isurf,OTHSTR)
#include "building.h"
#include "geometry.h"
      
      integer lnblnk  ! function definition
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      COMMON/C24/IZSTOCN(MCOM,MS)
      CHARACTER OTHSTR*30,SST2*12,ZST2*12

      if(izone.ne.0.and.izone.le.NCOMP)then
        if(isurf.le.NZSUR(izone).and.isurf.ne.0)then
          icon=IZSTOCN(izone,isurf)
          if(icon.eq.0)then
            WRITE(OTHSTR,'(a)') '|| izstocn index unknown'
            return
          endif
          IF(ICT(ICON).EQ.-1)THEN
            WRITE(OTHSTR,'(a)') '||< not yet defined'
          ELSEIF(ICT(ICON).EQ.0)THEN
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
            icc=IZSTOCN(IC2(ICON),IE2(ICON))
            if(icc.gt.0)then
              SST2=SSNAME(icc)
              ZST2=zname(IC2(ICON))
            else
              SST2='not_known'
              ZST2='not_known'
            endif
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
        else

C The passed surface number was out of range.
          WRITE(OTHSTR,'(a)') '|| surf unknown'
        endif
      else

C The passed zone number was out of range.
        WRITE(OTHSTR,'(a)') '|| zone unknown'
      endif
      RETURN
      END

C ********************** ZINFOREP
C 'ZINFOREP' takes data from the zone geometry commons and produces
C a high level report channel (itu). It assumes that common blocks
C (G7,PREC2,PREC2,PREC17,C20,C24) have been filled. It does not
C require a recent scan of zone geometry.
      SUBROUTINE ZINFOREP(itu,icomp)
#include "building.h"
#include "geometry.h"

      common/SFIG/NSIGFIG
      character outstr*124,t10*10,t10a*10,T12*12,T12A*12

      call edisp(itu,' ')

C Convert nzsur,nztv,zvol etc into strings with no leading spaces.
      CALL INTSTR(NZSUR(icomp),t10,lna,IER)
      CALL INTSTR(NZTV(icomp),t10a,lnaa,IER)
      WRITE(outstr,'(3a,i2,5a)')' Zone ',
     &   zname(ICOMP)(1:lnzname(ICOMP)),
     &   ' (',ICOMP,') is composed of ',t10(1:lna),' surfaces and ',
     &   t10a(1:lnaa),' vertices.'
      call edisp(itu,outstr)
      
C Report data to required number of significant figures.
      call SIGFIG(VOL(icomp),NSIGFIG,RNO,T12,LSTR)
      WRITE(outstr,'(3a)')' It encloses a volume of ',T12(1:LSTR),
     &   'm^3 of space, with a total surface'
      call edisp(itu,outstr)
      call SIGFIG(zonetotsurfacearea(icomp),NSIGFIG,RNO,T12,LSTR)
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
      WRITE(outstr,'(1x,A)') zdesc(ICOMP)(1:lnzdesc(ICOMP))
      call edisp(itu,outstr)
      return
      end

C ********************** ZINFO
C 'ZINFO' takes data from the zone geometry commons G1,G2,G3 and returns
C several derived values.
      SUBROUTINE ZINFO(SNA,ZOA,PAZI,PELV,ZVOL)
#include "building.h"
#include "geometry.h"
#include "sbem.h"

C Parameters
      real SNA  ! returned area for each surface
      dimension SNA(MS)
      real ZOA  ! returned total surface area associated with zone
      real PAZI ! returned plane azimuth angle
      real PELV ! returned plane elevation angle
      dimension PAZI(MS),PELV(MS)
      real ZVOL ! returned zone bounded volume

      DIMENSION XSUM(MS),YSUM(MS),ZSUM(MS)
      LOGICAL CLOSE
      character outs*124

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
C Suppress warning message if UK NCM notional model
        IF(SNA(I).LT.0.00001.AND.INOTI.NE.1)THEN
	  write(outs,'(a,i3,a)') ' Area of zone surf ',i,
     &      ' is less than 0.00001m^2!'
          CALL USRMSG(outs,
     &      'Other geometric properties may be incorrect.','W')
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

C << this could be a place to insert special logic for horizontal
C << surfaces as done in pangs in esruish.
C << note logic is slightly different for the pazi and pelv in
C << pangs - it might be better?
C << also - why not include surface tilt angle?
	
   60 CONTINUE
      ZVOL=ABS(V)

C Check for small volume.
      IF(ZVOL.LE.0.00001)THEN
        CALL USRMSG(' Negative or zero zone volume detected. Vertex', 
     &    ' information may be in error or surfaces incomplete.','W')
        RETURN
      ENDIF

      RETURN
      END

C ********************** ZGUPDATE
C 'ZGUPDATE' takes the geometry commons G1 and updates G7 & PREC2
C SSNA(MCON)    - surface area of each polygon (from geometry.h)
C SSPAZI(MCON)  - plane azimuth angle (from geometry.h)
C SSPELV(MCON)  - plane elevation angle (from geometry.h)
C SSPERIM(MCON)  - perimeter of each surface (from geometry.h)
C SUREQN(MCON,4)- equation of each polygon (from geometry.h)
C SSURCOG(MCON,3) - vertex weighted COG of polygon (from geometry.h)
C SSURVN(MCON,3)  - unit normal vector from COG of polygon.
C VOL(MCOM)     - zone bounded volume
      SUBROUTINE ZGUPDATE(itrc,icomp,ier)
#include "building.h"
#include "geometry.h"
#include "sbem.h"
      
      integer lnblnk  ! function definition

C << could it use ?? instead of G1? >>

      common/OUTIN/IUOUT,IUIN
      COMMON/C24/IZSTOCN(MCOM,MS)
      DIMENSION XSUM(MS),YSUM(MS),ZSUM(MS)
      DIMENSION XX(MV),YY(MV),ZZ(MV),CG(3),EQN(4)
      LOGICAL CLOSE,closeaz,closeel
      CHARACTER outs*124
      integer lln   ! length of surface name

C ZGUPDATE assumes that the zone geometry file has been read prior to
C its invocation and that the read of the model configuration file has
C instanciated the G6 common block.

C Set area summations to zero & consider each surface of the zone in turn.
      ier=0
      PI = 4.0 * ATAN(1.0)
      R=PI/180.
      DO 10 I=1,NZSUR(icomp)

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
          if(IP1.gt.0.and.IP2.gt.0)then
            ZMAX=AMAX1(ZMAX,Z(IP1))
            ZMIN=AMIN1(ZMIN,Z(IP1))
            XS=XS+Y(IP1)*Z(IP2)-Z(IP1)*Y(IP2)
            YS=YS+Z(IP1)*X(IP2)-X(IP1)*Z(IP2)
            ZS=ZS+X(IP1)*Y(IP2)-Y(IP1)*X(IP2)
            ADDL=ADDL+SQRT((X(IP2)-X(IP1))**2+(Y(IP2)-Y(IP1))**2+
     &        (Z(IP2)-Z(IP1))**2)
          endif
   20   CONTINUE
        XSUM(I)=XS
        YSUM(I)=YS
        ZSUM(I)=ZS

C Compute surface area and check for negative or near zero area.
C Suppress warning message if UK NCM notional model
        ZAREA=0.5*SQRT(XS*XS+YS*YS+ZS*ZS)
        SSNA(icc)=ZAREA
        IF(SSNA(icc).LT.0.00001.AND.INOTI.NE.1)THEN
	  write(outs,'(a,i3,a)') ' Area of surf connection ',icc,
     &      ' is less than 0.00001m^2!'
          CALL USRMSG(outs,
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

C Debug.
C        write(6,*) 'perimeter of ',ssname(icc),icc,' is ',ssperim(icc)

C Derive the equation of the polygon.
        DO 60 KK=1,NVER(i)
          XX(KK) = X(JVN(I,KK))
          YY(KK) = Y(JVN(I,KK))
          ZZ(KK) = Z(JVN(I,KK))
   60   CONTINUE
        N = NVER(i)
        call PLEQN(XX,YY,ZZ,N,CG,EQN,IERR)

C See if any points are out of the plane.
        iwhich=0
        ofby=0.0
	lln=lnblnk(ssname(icc)) 
	call CHECKWARP(1,iuout,i,N,XX,YY,ZZ,iwhich,ofby,ivoff)
	if(ivoff.gt.0)then
	  write(outs,'(3a)') ' Surface ',ssname(icc)(1:lln),
     &      ' is probably warped. Please check via surface transforms.'
          call edisp(iuout,outs)
        endif

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
      DO 62 K=1,NZSUR(icomp)
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

C Check that the azimuth and elevation based on surface normal are
C the same as computed above.
   63   vdx=SSUREQN(icc,1)
        vdy=SSUREQN(icc,2)
        vdz=SSUREQN(icc,3)
        call UV2AZ(vdx,vdy,vdz,uazim,uelev)
	call eclose(uazim,SSPAZI(icc),0.001,closeaz)
	call eclose(uelev,SSPELV(icc),0.001,closeel)	

C << this could be a place to insert special logic for horizontal
C << surfaces as done in pangs in esruish.
C << note logic is slightly different for the pazi and pelv in
C << pangs - it might be better?
C << also - why not include surface tilt angle?
	
        if(ITRC.gt.1)then 
          write(outs,*) 'name azi elv m2 ',SSNAME(icc),SSPAZI(icc),
     &      SSPELV(icc),SSNA(icc)
          call edisp(iuout,outs)
          write(outs,*) 'Unit vector azi elv         ',uazim,uelev,
     &      closeaz,closeel
          call edisp(iuout,outs)
          write(outs,*) 'SUREQN of icc ',icc,SSUREQN(icc,1),
     &      SSUREQN(icc,2),SSUREQN(icc,3),SSUREQN(icc,4)
          call edisp(iuout,outs)
          write(outs,*) 'SURCOG ',SSURCOG(icc,1),SSURCOG(icc,2),
     &      SSURCOG(icc,3),' SURVN ',SSURVN(icc,1),SSURVN(icc,2),
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
#include "building.h"
#include "geometry.h"

      common/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)

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
     &          iedgshr(MCON,MV),imatshr(MCON,MV),ibridgeshr(MCOM,MV)

      real bridgelen ! Length (m) of potential thermal bridges in each zone.
                    ! See code below for what each means.
      common/gbridge/bridgelen(MCOM,10)
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      COMMON/C24/IZSTOCN(MCOM,MS)

      CHARACTER outs*124,act*1,h*72
      logical closeazi,closeelv,foundone,ok,dok,same

C iwhich is local variable to sort most often referenced adjacent surf.
      dimension iwhich(MV),iwhichs(MV)
      real va,vb,pa,pb
      dimension va(3),vb(3),pa(3),pb(3),vaa(3),vbb(3)
      real ux,uy,uz  ! unit vector along the edge
      real dista,distc
C      real distb ! currently not used
      logical closeux,closeuy,closeuz

      if(izone.ne.0.and.izone.le.NCOMP)then
        if(act.eq.'c')then

C Do test call to surrel to see if it picks up any parent child
C relationships. Use strict test.
C          call surrel('-',izone,ier)
          call surrel('s',izone,ier)
        endif

C Zero the lengths of various thermal bridge type lengths.
        do 644 ij=1,10
          bridgelen(izone,ij)=0.0
 644    continue

        do 43 isurf=1,NZSUR(izone)
          icc=izstocn(izone,isurf)
          if(icc.eq.0)then
            write(outs,'(a,i2,a,i3,a)') ' Zone ',izone,' surface ',
     &        isurf,' is an unknown connection, skipping suredgeadj.'
            call edisp(iuout,outs)
            ier=1
            return
          endif

C Clear assumptions.
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

C Debug.
              if(itrc.gt.1)then
C                write(outs,'(a,i3,a,4i3)') 'edges for conn ',icc,
C     &            ' are ',jes,jee,kes,kee
C                call edisp(iuout,outs)
              endif

              if(jes.EQ.kes.AND.jee.EQ.kee)then
                nbedgdup(icc)=nbedgdup(icc)+2
                iedgdup(icc,m)=1
                iedgdup(icc,n)=1

C Debug...
                if(itrc.gt.1)then
C                  write(outs,*) 'dupl edges for conn ',icc,' is ',
C     &              iedgdup(icc,m),' m n ',m,n
C                  call edisp(iuout,outs)
                endif
              endif
  41        continue
  40      continue

C Debug...
          if(itrc.gt.1)then
C            write(outs,*) 'duplicate edges for conn ',icc,' ',
C     &        ssname(icc)(1:lnblnk(ssname(icc))),
C     &        ' is ',nbedgdup(icc)
C            call edisp(iuout,outs)
          endif

C Step through the vertices of each edge in turn and check to see
C what other surfaces and materials are associated with edge. j & k
C are the vertices at the ends of the edge being tested. When the
C last vertext is reached point the end to the start point.
          do 46 ijj=1,MV
            iwhich(ijj)=0
            iwhichs(ijj)=0
  46      continue
          list=NVER(isurf)
          nbedgshr(icc)=0
          do 42 i=1,list
            iedgshr(icc,i)=0
            imatshr(icc,i)=0
            ibridgeshr(icc,i)=0 ! begin by assuming no bridge locations
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
              if(icco.eq.0) goto 44
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
                  if(itrc.gt.1)then
C                    write(outs,'(a,i3,a,i3,a,2i3,a,2i3)')
C     &                'edges for conns ',icc,' & ',icco,
C     &                ' are ',j,k,' & ',jj,kk
C                    call edisp(iuout,outs)
                  endif

C Find length of the current edge.
                  edgelen= crowxyz(X(j),Y(j),Z(j),X(k),Y(k),Z(k))

C Tests for ibridgeshr (edges that might represent thermal bridges.
                  if(ICT(icc).eq.0.and.ICT(icco).eq.0)then

                    if(SSVFC(icc)(1:4).eq.'VERT'.and.(
     &                 SSVFC(icco)(1:4).eq.'CEIL'.or.
     &                 SSVFC(icco)(1:4).eq.'SLOP'))then

C One is roof-wall so both surfaces must face the outside, first must
C be roughly vertical and the other flat (up) or sloped.
                      ibridgeshr(icc,i)=1
                      bridgelen(izone,1)= bridgelen(izone,1)+edgelen

                    elseif(SSVFC(icc)(1:4).eq.'VERT'.and.
     &                     SSVFC(icco)(1:4).eq.'VERT')then

C Test if azimuth are within two degrees. For the special case of the
C difference between the two being ~360deg call it closeazi
                      call eclose(SSPAZI(icc),SSPAZI(icco),2.0,closeazi)
                      if(.NOT.closeazi)then
                        zazi = abs(SSPAZI(icc)-SSPAZI(icco))
                        call eclose(zazi,360.0,2.0,closeazi)
                      endif
                      if(closeazi)then

C Find which of the unit vector components (ux uy uz) are near zero
C in order to see if edge if vertical or horizontal.
                        call uvxyz(X(j),Y(j),Z(j),X(k),Y(k),Z(k),
     &                    ux,uy,uz)
                        call eclose(ux,0.00,0.002,closeux)
                        call eclose(uy,0.00,0.002,closeuy)
                        call eclose(uz,0.00,0.002,closeuz)
C Debug.
C                        write(outs,*) ux,closeux,uy,closeuy,uz,closeuz
C                        call edisp(iuout,outs)

C Six is lintel above window or door so both surfaces must face
C the outside and face the same direction and one must be the
C parent and the edge must be roughly horizontal and above the
C centre of the surface (and marked as a door or window)
                        if(closeuz)then
                          if(Z(j).gt.SSURCOG(icc,3))then
                            if(ssparent(icc)(1:2).eq.'-')then
                              continue
                            else
                              ibridgeshr(icc,i)=6
                              bridgelen(izone,6)= bridgelen(izone,6)+
     &                          edgelen
                            endif

C Seven is Sill below window so both surfaces must face
C the outside and face the same direction and one must be the
C parent and the edge must be roughly horizontal and below the
C centre of the surface (and marked as a door or window)
                          else
                            if(ssparent(icc)(1:2).eq.'-')then
                              continue
                            else
                              ibridgeshr(icc,i)=7
                              bridgelen(izone,7)= bridgelen(izone,7)+
     &                          edgelen
                            endif
                          endif
                        endif

C Eight is jamb at window or door so both surfaces must face
C the outside and face the same direction and one must be the
C parent and the edge must be roughly vertical (and marked as
C a door or window)
                        if(closeux.and.closeuy)then
                          if(ssparent(icc)(1:2).eq.'-')then
                            continue
                          else
                            ibridgeshr(icc,i)=8  ! is vertical
                            bridgelen(izone,8)= bridgelen(izone,8)+
     &                        edgelen
                          endif
                        endif
                      else

C Determin concave or convex between the two surfaces based on the distance
C between points on the two surfaces and points a slight distance
C away from the two surraces.
                        PA(1)=SSURCOG(icc,1)  ! point on plane
                        PA(2)=SSURCOG(icc,2)
                        PA(3)=SSURCOG(icc,3)
                        VA(1)=SSURVN(icc,1)   ! point 1m away
                        VA(2)=SSURVN(icc,2)
                        VA(3)=SSURVN(icc,3)

C Use ratio calculation to get point 100mm away.
                        r2 = 1.0 - 0.1
                        r1 = 0.1
                        vaa(1) = ((r2 * PA(1)) + (r1 * VA(1)))/1.0
                        vaa(2) = ((r2 * PA(2)) + (r1 * VA(2)))/1.0
                        vaa(3) = ((r2 * PA(3)) + (r1 * VA(3)))/1.0

                        PB(1)=SSURCOG(icco,1)  ! point on plane
                        PB(2)=SSURCOG(icco,2)
                        PB(3)=SSURCOG(icco,3)
                        VB(1)=SSURVN(icco,1)   ! point 1m away
                        VB(2)=SSURVN(icco,2)
                        VB(3)=SSURVN(icco,3)
                        vbb(1) = ((r2 * PB(1)) + (r1 * VB(1)))/1.0
                        vbb(2) = ((r2 * PB(2)) + (r1 * VB(2)))/1.0
                        vbb(3) = ((r2 * PB(3)) + (r1 * VB(3)))/1.0
                        dista = crow(PA,PB)  ! dist cog-to-cog
C                        distb = crow(VA,VB)  ! dist offset-to-offset
                        distc = crow(VAA,VBB)! dist slight offset-to-offset

C Debug.
C                        write(outs,'(9f7.2)') dista,distb,distc,
C     &                    pa(1),va(1),vaa(1),pb(1),vb(1),vbb(1)
C                        call edisp(iuout,outs)

C If dista is greater than distc then concave.
                        if(dista.lt.distc)then

C Three is wall-wall (convex corner) so both must face the outside
C and the angle between the surface normals is greater than zero.
                          ibridgeshr(icc,i)=3
                          bridgelen(izone,3)= bridgelen(izone,3)+
     &                      edgelen
                        else

C Four is wall-wall (concave corner) so both must face the outside
C and the angle between the surface normals less than zero
                          ibridgeshr(icc,i)=4
                          bridgelen(izone,4)= bridgelen(izone,4)+
     &                      edgelen
                        endif
                      endif

                    elseif(SSVFC(icc)(1:4).eq.'VERT'.and.
     &                     SSVFC(icco)(1:4).eq.'FLOR')then

C Five is wall-floor (not ground) so both surfaces must face the 
C outside one roughly vertical and the other flat (down)
                      ibridgeshr(icc,i)=5
                      bridgelen(izone,5)= bridgelen(izone,5)+
     &                  edgelen
                    endif

                  elseif(ICT(icc).eq.4.and.ICT(icco).eq.0)then
                    if(SSVFC(icc)(1:4).eq.'FLOR'.and.
     &                 SSVFC(icco)(1:4).eq.'VERT')then

C Two is wall-ground floor so one surface must face outside and the
C other must face the ground
                      ibridgeshr(icc,i)=2
                      bridgelen(izone,2)= bridgelen(izone,2)+
     &                  edgelen
                    endif
                  endif

C Debug.
                  if(itrc.gt.1)then
C                    write(outs,'(a,i3,a,2i3,2f6.1,2a,3f7.3)')
C     &                'type for conns ',icc,' & ',icco,
C     &                ibridgeshr(icc,i),SSPAZI(icc),SSPAZI(icco),
C     &                ':',ssparent(icc),dista,distb,distc
C                    call edisp(iuout,outs)
C                    write(outs,'(a,8f7.2)') 'brl ',bridgelen(izone,1),
C     &                bridgelen(izone,2),bridgelen(izone,3),
C     &                bridgelen(izone,4),bridgelen(izone,5),
C     &                bridgelen(izone,6),bridgelen(izone,7),
C     &                bridgelen(izone,8)
C                    call edisp(iuout,outs)
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
            if(itrc.gt.1)then
C              write(outs,*) 'iedgshr ',iedgshr(icc,i),
C     &          ' shared construction ',imatshr(icc,i)
C              call edisp(iuout,outs)
            endif
  42      continue

C Debug...
          if(itrc.gt.1)then
C            write(outs,*) 'nbedgshr for ',
C     &        ssname(icc)(1:lnblnk(ssname(icc))),
C     &        ' is ',nbedgshr(icc)
C            call edisp(iuout,outs)
          endif

C If most the edges are shared with other similarly facing surfaces
C then if act='c' and the call surrel did not find it ask the user
C what to do.  If there are duplicate surfaces in the zone then
C the other surface might have exactly the same number of edges.
          if(act.eq.'c')then
            if(SSPARENT(icc)(1:2).eq.'  ')then
              if(nbedgshr(icc).eq.NVER(isurf).or.
     &           nbedgshr(icc).eq.(NVER(isurf)-1))then

C If the two surfaces have the same number of edges then it is
C likely that these are duplicate surfaces. 
                if(NVER(iosurf).eq.NVER(isurf))then

C Debug
C                  write(6,*) 'Probable dup surfaces ',iosurf,isurf,
C     &              NVER(iosurf),NVER(isurf)

                  continue
                else
                  KFLAG = -1
                  call SORTI(iwhich,iwhichs,list,KFLAG)

C If all of iwhich is the same number (excluding zero) then use it.
                  same=.true.
                  do 442 im=1,list-1
                    if(iwhich(im).ne.0.and.iwhich(im+1).ne.0)then
                      if(iwhich(im).ne.iwhich(im+1)) same=.false.
                    endif
  442             continue
                  if(same)then
                    ibinval=iwhich(1)
                  else
                    ibinval=0
                  endif

C If current surface already marked and is the same as ibinval then do not
C bother the user. If ibinval was set to zero then clear SSPARENT.
                  if(ibinval.ne.0)then
                    if(SSPARENT(icc).eq.ssname(ibinval))then
                      ok=.true.
                    else
                      write(outs,'(5a)') 'Is ',ssname(icc),
     &                  ' a subsurface of ',ssname(ibinval),'?'
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
     &                ' have a parent surface in ',zname(izz),'?'
                    call askok(outs,
     &                ' (saying no sets parent to blank)',ok,dok,2)
                    if(ok)then
                      write(outs,'(5a)') 'Specify parent surface for ',
     &                  ssname(icc),' in ',zname(izz),'.'
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
            endif
          endif
 43     continue

C Report on likely thermal bridge lengths. Reduce the concave and
C convex bridge lengths because they will have been counted from
C the edges going in both directions.
        bridgelen(izone,3)=bridgelen(izone,3)*0.5
        bridgelen(izone,4)=bridgelen(izone,4)*0.5
        if(itrc.ge.1)then
          write(outs,'(a,9f7.2)') 'bridge len ',bridgelen(izone,1),
     &      bridgelen(izone,2),bridgelen(izone,3),
     &      bridgelen(izone,4),bridgelen(izone,5),
     &      bridgelen(izone,6),bridgelen(izone,7),
     &      bridgelen(izone,8),bridgelen(izone,9)
          call edisp(iuout,outs)
        endif
      endif
      return
      end

**************** SURREL ****************************
C Subroutine SURREL checks each surface I in a zone and relates it to
C another surface J in that zone. as parent if one less than all vertices
C in I are also in surface J. One less because the child surface may be
C at the edge of the parent as shown in the example below:
C   xxxxxxxxxx
C   x        x
C   x        x
C   xxxxx    x
C   x   x    x
C   x   x    x
C   xxxxxxxxxx
C Act = '-' a loose definition of child which allows for what is
C   drawn above as well as the case of a door normally inserted
C   which is not at the edge of the parent.
C Act = 's' for a strict definition where a child must be fully
C   bounded by the parent.

C Note: if surface I and J both have the name number of vertices then
C a match should be discounted because the two surfaces are likely to
C be duplicates.

      SUBROUTINE SURREL(ACT,ICOMP,IER)
#include "building.h"
#include "geometry.h"

C G9 holds information on children of a surface.
      integer nbchild   ! how many children (up to 4)
      integer nbgchild  ! how many grand children (up to 4)
      integer ichild    ! list of children
      integer igchild   ! list of grand children
      common/G9/nbchild(MCON),nbgchild(MCON),ichild(MCON,4),
     &igchild(MCON,4)

      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      COMMON/C24/IZSTOCN(MCOM,MS)

      DIMENSION IDVN(MV),nTbchild(MCON)
      DIMENSION ICOMVER(MS)  ! icomver is the number of vertices in common
      dimension idupv(MS,6)    ! duplicate vertex indices
      dimension iduplist(MS,6) ! position in list
      LOGICAL closeazi,closeelv
      CHARACTER ACT*1,CHILDRIN*12,PARENTIN*12
      integer isur   ! loop counter for parent surface loop
      integer jsur   ! loop counter for child surface loop
      integer iver   ! loop counter for parent surface vertices
      integer jver   ! loop counter for child surface vertices
      integer iiver,jjver,next

C Initialise various variables
      do 10 isur=1,NZSUR(ICOMP)
        icc=IZSTOCN(icomp,isur)
        ssparent(icc)='-'
        do 20 i=1,6
          idupv(isur,i)=0     ! clear duplicate vertex index
          iduplist(isur,i)=0  ! clear duplicate position
 20     continue
        do 21 i=1,4
          ichild(iCC,i)=0    ! clear array holding children
          igchild(iCC,i)=0   ! clear array holding grand children
 21     continue
        nbchild(iCC)=0     ! assume no children
        nbgchild(iCC)=0    ! assume no grand children
        next=0  ! counter to update to get to 2nd dimension.

C First identify any surfaces with vertices that are
C referenced more than once (e.g. surface with hole in it)
C Check for 2nd references of a vertex with the fast loop looping
C through only the untested part of the surface edge list.
        do 11 iver=1,nver(isur)
          IIVER=JVN(isur,iver)  ! the vertex to check in slow loop
          do 12 jver=iver,nver(isur)  ! fast loop is diagonal check
            JJVER=JVN(isur,jver)  ! the fast loop vertex
            if(iver.ne.jver)then  ! don't compare with same position in jvn()
              if(JJVER.eq.IIVER)then
                next=next+1
                idupv(isur,next)=jjver  ! index of duplicate
                iduplist(isur,next)=jver  ! duplicate position in list
              endif 
            endif
  12      continue

C Debug.
C         if(iduplist(isur,1).gt.0.or.iduplist(isur,2).gt.0)then
C           write(6,*) isur,iver,iiver,next,idupv(isur,1),
C     &        iduplist(isur,1),idupv(isur,2),iduplist(isur,2),
C     &        idupv(isur,3),iduplist(isur,3),idupv(isur,4),
C     &        iduplist(isur,4)
C         endif
  11    continue
  10  continue

      DO 101 ISUR=1,NZSUR(ICOMP)   ! for each possible parent
        DO 102 JSUR=1,NZSUR(ICOMP) ! for each possible child

C Initialise temporary holding array for common vertices. 
          ICOMVER(JSUR)=0
          IF(JSUR.EQ.ISUR)GOTO 102 ! skip if same
          DO 103 IVER=1,NVER(ISUR)   ! for each item in parent vert list
            if(iver.eq.iduplist(isur,1))then
              continue  ! this is a 2nd reference
            elseif(iver.eq.iduplist(isur,2))then
              continue  ! this is a 2nd reference
            elseif(iver.eq.iduplist(isur,3))then
              continue  ! this is a 3rd reference
            elseif(iver.eq.iduplist(isur,4))then
              continue  ! this is a 4th reference
            else
              DO 104 JVER=1,NVER(JSUR) ! for each item in child vert list

C Compare vertex numbers for possible parent and child avoiding any
C 2nd duplicate references (e.g. iver is in iduplist)
                IIVER=JVN(ISUR,IVER)
                JJVER=JVN(JSUR,JVER)
                IF(IIVER.EQ.JJVER)ICOMVER(JSUR)=ICOMVER(JSUR)+1
 104          CONTINUE
            endif
 103      CONTINUE
          if(NVER(JSUR).eq.3)then

C If the number of vertices in jsur is 3 (triangular) then icomver
C must be >= nver(jsur).
            if(ICOMVER(JSUR).GE.NVER(JSUR))then
              continue
            else
              goto 105
            endif
          elseif(NVER(JSUR).gt.3)then

C If the number of vertices in jsur is 4 or more, icomver must be >= nver()
C for the strict case and >= nver()-1 for the loose or blank action.
C so as to pick up the door-in-corner.
            if(act.eq.'-'.and.(ICOMVER(JSUR).GE.NVER(JSUR)-1))then
              continue
            elseif(act.eq.' '.and.(ICOMVER(JSUR).GE.NVER(JSUR)-1))then
              continue
            elseif(act.eq.'s'.and.(ICOMVER(JSUR).GE.NVER(JSUR)))then
              continue
            else
              goto 105
            endif

C If isur and jsur both have the same number of edges then it is likely
C that we have duplicate surfaces.
            if(NVER(JSUR).eq.NVER(ISUR))then

C Debug.
C              write(6,*) 'Probable dup surfaces ',jsur,isur,
C     &          NVER(jsur),NVER(isur)

              goto 105
            endif
          endif

C If there are the same shared vertices check orientation as well.
          icc=IZSTOCN(icomp,isur)
          jcc=IZSTOCN(icomp,jsur)

C Parent and child must have the same azimuth and elevation angles,
C allow tolerance of 2deg and possible rotation of 360deg. 
C<< Division mod 360 may be a more generic way for angle checking
          call eclose(SSPAZI(icc),SSPAZI(JCC),2.0,closeazi)
          if(.NOT.closeazi)then
            zazi = abs(SSPAZI(icc)-SSPAZI(JCC))
            call eclose(zazi,360.0,2.0,closeazi)
          endif
          call eclose(SSPELV(icc),SSPELV(JCC),2.0,closeelv)
          if(closeazi.and.closeelv)then

            SSPARENT(JCC)=SSNAME(ICC)
C Debug.
c            WRITE(6,*)'Parent of ',jcc,' ',SSNAME(JCC),' is ',icc,' ',
c     &        SSPARENT(JCC)!,' Matched vertices= ',ICOMVER(JSUR),
c     &        ' No of vertices in child= ',NVER(JSUR)
c            write(6,*)' '

C << Having found a child add it to the list of children of
C << the parent surface.
            nbchild(iCC)=nbchild(iCC)+1
            ichild(iCC,nbchild(iCC))=jCC

C Debug...
c            WRITE(6,*)'Child of ',SSNAME(ICC),' is ',
c     &        childname(isur,1)
          endif
 105      continue
 102    CONTINUE
 101  CONTINUE

C Now that parents are known also work out grand parents. This is done
C by a similar logic to how parents are worked out (by inspecting vertex
C numbers of surfaces).
C Only need to work out whether parents are themselves children of some
C other surface (grandparent)
      DO 2001 ICC=1,MCON
        nTbchild(iCC)=nbchild(iCC)
 2001 CONTINUE
      DO 110 ISUR=1,NZSUR(ICOMP)
        icc=IZSTOCN(icomp,isur)
        DO 2002 IV=1,MV
          IDVN(IV)=0
 2002   CONTINUE
        DO 111 ICHLD=1,nTbchild(iCC)
          NCV=0
          icv=0 ; ido=0
          CHILDRIN=SSNAME(ICHILD(ICC,ICHLD))
          DO 112 JSUR=1,NZSUR(ICOMP)
            jcc=izstocn(icomp,jsur)
            PARENTIN=SSNAME(JCC)
            ISNL=LNBLNK(PARENTIN)
            ICHL=LNBLNK(CHILDRIN)
            IF(CHILDRIN(1:ICHL).EQ.PARENTIN(1:ISNL))THEN
              DO 113 IVER=1,NVER(ISUR) 
                IIVER=JVN(ISUR,IVER)
                ICV=0
                DO 114 JVER=1,NVER(JSUR)
                  JJVER=JVN(JSUR,JVER)
                  IF(IIVER.EQ.JJVER)THEN
                    ICV=1
                  ENDIF
 114            CONTINUE
		IF(ICV.NE.1)THEN
                  IDO=0
                  DO 118 ICCV=1,MV
                    IF(IDVN(ICCV).EQ.IIVER)THEN
                      IDO=1
                    ENDIF
 118              CONTINUE
                  IF(IDO.EQ.0)THEN
                    NCV=NCV+1
                    IDVN(NCV)=IIVER
                  ENDIF
		ENDIF
 113          CONTINUE
            ENDIF
 112      CONTINUE

C Now IDVN holds list of all vertices that define the parents but are
C not contained in the child. Use these to work out parent of parent
          DO 115 JSUR=1,NZSUR(ICOMP)
            IF(ISUR.NE.JSUR)THEN
              NCOMV=0
              DO 116 ICV=1,NCV
                DO 117 JVER=1,NVER(JSUR)
                  JJVER=JVN(JSUR,JVER)
                  IF(IDVN(ICV).EQ.JJVER)then
                    NCOMV=NCOMV+1
                  ENDIF
 117            CONTINUE
 116          CONTINUE
              IF(NCV.GT.0.AND.NCV.LE.NCOMV)THEN
                Jcc=IZSTOCN(icomp,Jsur)
                SSPARENT(iCC)=SSNAME(jCC)
                nbchild(jCC)=nbchild(jCC)+1
                ichild(jCC,nbchild(jCC))=iCC
              ENDIF
            ENDIF
 115      CONTINUE
 111    CONTINUE
 110  CONTINUE

C Now fill grand children commons
      DO 201 ISUR=1,NZSUR(ICOMP)
        icc=IZSTOCN(icomp,isur)
        DO 202 ICHL=1,NBCHILD(ICC)
          JCC=ICHILD(ICC,ICHL)
          DO 203 IGCD=1,NBCHILD(JCC)
            KCC=ICHILD(JCC,IGCD)
            IGCHILD(ICC,IGCD)=KCC
             NBGCHILD(ICC)=NBGCHILD(ICC)+1
 203      CONTINUE
 202    CONTINUE
 201  CONTINUE
      RETURN
      END


C ***************** ZDATA 
C ZDATA reads NZONES geometry and uses common blocks G1 G2 etc to fill
C common blocks ZNDATA so wireframes can be drawn without the
C need to re-read geometry files.

C << what if we know that connetions are correct, could this be
C << dones as part of the geometry file read?

C << there are many places where VCOORD, NZNVER and NZJVN
C << could be used insted of G1 common block.

C VCOORD : X,Y & Z coordinates of each vertice in zone.
C NZNVER : number of vertices associated with each connection.
C NZJVN : topology of vertices associated with each connection.

C << Could some of these common blocks be filled when scanning the
C << zone geometry file if havec24 is true?

      SUBROUTINE ZDATA(ITRC,IER,NZONES,ISFSUM)
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "prj3dv.h"
      
      integer lnblnk  ! function definition

      common/pophelp/h(60)
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL
      COMMON/C24/IZSTOCN(MCOM,MS)

C Variables from geometry.h:
C SSURCOG is centre of gravity of surface (vertex weighted).
C SSUREQN is the surface equation A*X + B*Y + C*Z = D
C SSURVN is point 1 unit vectpr off of surface COG along the normal.

      LOGICAL OK,dok
      CHARACTER h*72,outs*124

      integer lng     ! length of geometry file name
      integer itrct   ! temporary trace level

C Assuming this routine is called in order: read in the geometry file
C using georead or egomin and pass across into the appropriate array.
      DO 30 ICOMP=1,NZONES
   25   IER=0
        if(itrc.gt.1)then
          lng=lnblnk(LGEOM(ICOMP))
          write(outs,'(2a)') ' Reading : ',LGEOM(ICOMP)(1:lng)
          CALL USRMSG(' ',outs,'-')
        endif
        call georead(IFIL+1,LGEOM(ICOMP),ICOMP,1,iuout,ier)
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
        itrct=0
        call zgupdate(itrct,icomp,ier)

C Find co-planer surfaces and edges of similar materials.
        call suredgeadj(itrc,'-',icomp,ier)

C ISFSUM is the total number of surfaces in the model. Nothing
C seems to use this information (yet).
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
        iZBFLG(I)=0
  42  CONTINUE

      RETURN

   99 IER=1
      RETURN
      END

C ********************** FILSUR
C 'FILSUR' Fills the common block G5 with default assumptions.
C If ISUR=0 do all surfaces otherwise if ISUR>0 for one surface.

C << should this also fill G6?? >>.

      SUBROUTINE FILSUR(ICOMP,ISUR)
#include "building.h"
#include "geometry.h"

C Parameters
      integer icomp ! zone number
      integer isur  ! zero do all surfaces otherwise index of a surface

      common/SurfInfo/iSurfOrient(MCOM,MS)
      integer iSurfOrient, iSurfWall, iSurfCeiling, iSurfFloor
      parameter (iSurfWall=1, iSurfFloor=2, iSurfCeiling=3)
      DIMENSION SNA(MS),PAZI(MS),PELV(MS)

C Begin with default assumptions for each surface then overwrite
C this if user supplied information exists.  Gather misc zone infor-
C mation so make a probable guess at locations.
      CALL ZINFO(SNA,ZOA,PAZI,PELV,ZVOL)
      if(ISUR.eq.0)then
        DO 44 I=1,NSUR
          SOTF(I)='OPAQUE'
          SMLCN(I)='UNKNOWN'

C Guess at location based on elevation of surface. Use same
C logic as in subroutine updatesvfc.
          SVFC(I)='UNKN'
          if(PELV(I).GE.-1.5.AND.PELV(I).LE.1.5)then
            SVFC(I)='VERT'
            IF(I.LE.9)WRITE(SNAME(ICOMP,I),'(A5,I1)') 'Wall-',I
            IF(I.GT.9)WRITE(SNAME(ICOMP,I),'(A5,I2)') 'Wall-',I
          elseif(PELV(I).GE.88.5.AND.PELV(I).LE.91.5)then
            SVFC(I)='CEIL'
            IF(I.LE.9)WRITE(SNAME(ICOMP,I),'(A4,I1)') 'Top-',I
            IF(I.GT.9)WRITE(SNAME(ICOMP,I),'(A4,I2)') 'Top-',I
          elseif(PELV(I).GE.-91.5.AND.PELV(I).LE.-88.5)then
            SVFC(I)='FLOR'
            IF(I.LE.9)WRITE(SNAME(ICOMP,I),'(A5,I1)') 'Base-',I
            IF(I.GT.9)WRITE(SNAME(ICOMP,I),'(A5,I2)') 'Base-',I
          else
            SVFC(I)='SLOP'
            IF(I.LE.9)WRITE(SNAME(ICOMP,I),'(A5,I1)') 'Surf-',I
            IF(I.GT.9)WRITE(SNAME(ICOMP,I),'(A5,I2)') 'Surf-',I
          endif
          SOTHER(I,1)='UNKNOWN'
          SOTHER(I,2)='0'
          SOTHER(I,3)='0'
          SUSE(I,1)='-'
          SUSE(I,2)='-'
          SPARENT(I)='-'

C Set flags for wall/celing/floor
          if(PELV(I).GE.-0.02.AND.PELV(I).LE.30.0)then
            iSurfOrient(ICOMP,I) = iSurfWall
          elseif(PELV(I).GT.30.0.AND.PELV(I).LE.90.02)then
            iSurfOrient(ICOMP,I) = iSurfCeiling
          elseif(PELV(I).GE.-90.02.AND.PELV(I).LE.-89.98)then
            iSurfOrient(ICOMP,I) = iSurfFloor
          endif

   44   CONTINUE
      elseif(ISUR.gt.0)then
        I=ISUR
        SOTF(I)='OPAQUE'
        SMLCN(I)='UNKNOWN'
        SVFC(I)='UNKN'
        if(PELV(I).GE.-1.5.AND.PELV(I).LE.1.5)then
          SVFC(I)='VERT'
          IF(I.LE.9)WRITE(SNAME(ICOMP,I),'(A5,I1)') 'Wall-',I
          IF(I.GT.9)WRITE(SNAME(ICOMP,I),'(A5,I2)') 'Wall-',I
        elseif(PELV(I).GE.88.5.AND.PELV(I).LE.91.5)then
          SVFC(I)='CEIL'
          IF(I.LE.9)WRITE(SNAME(ICOMP,I),'(A4,I1)') 'Top-',I
          IF(I.GT.9)WRITE(SNAME(ICOMP,I),'(A4,I2)') 'Top-',I
        elseif(PELV(I).GE.-91.5.AND.PELV(I).LE.-88.5)then
          SVFC(I)='FLOR'
          IF(I.LE.9)WRITE(SNAME(ICOMP,I),'(A5,I1)') 'Base-',I
          IF(I.GT.9)WRITE(SNAME(ICOMP,I),'(A5,I2)') 'Base-',I
        else
          SVFC(I)='SLOP'
          IF(I.LE.9)WRITE(SNAME(ICOMP,I),'(A5,I1)') 'Surf-',I
          IF(I.GT.9)WRITE(SNAME(ICOMP,I),'(A5,I2)') 'Surf-',I
        endif
        SOTHER(I,1)='UNKNOWN'
        SOTHER(I,2)='0'
        SOTHER(I,3)='0'
        SUSE(I,1)='-'
        SUSE(I,2)='-'
        SPARENT(I)='-'
      endif

      RETURN
      END

C ******************** INSINFO 
C INSINFO provides an English description of default insolation 
C options chosen by the user in the geometry file based on 
C NDP(ICOMP) and IDPN(ICOMP,?) from common G4 and G6.

C Note: the version of the geometry file should be taken into
C account as the QA report can lead to conflicting information
C as an older geometry file does not have the ishdirec common
C data held in the geometry file but inferred from the obstructions
C file.

C << update documentation as well as reporting based on files >>
      SUBROUTINE INSINFO(ICOMP,ITRU)
#include "building.h"
#include "model.h"
#include "geometry.h"

      COMMON/C24/IZSTOCN(MCOM,MS)
      common/ishdirec/iaplic(MCOM,2),nsurfcalc(MCOM),lstsfcalc(MCOM,MS),
     &     nsurfinso(MCOM),isurfinso(MCOM,MS)

      CHARACTER OUTSTR*124
      character sdescr*80

C Print out information about the default insolation planes if
C the user has not requested a shading analysis.
      call edisp(ITRU,' ')
      if(ISI(ICOMP).eq.1)then
        call edisp(ITRU,
     &' An hourly solar radiation distribution is used for this zone.')
      else
        IF(NDP(ICOMP).EQ.1)THEN
          ICM=IZSTOCN(ICOMP,IDPN(ICOMP,1))
          WRITE(OUTSTR,'(3a)')' Solar insolation focused on surface ',
     &    SSNAME(ICM),' (if shading not calculated).'
          call edisp(ITRU,OUTSTR)
        ELSEIF(NDP(ICOMP).EQ.2.AND.IDPN(ICOMP,3).EQ.0)THEN
          ICM1=IZSTOCN(ICOMP,IDPN(ICOMP,1))
          ICM2=IZSTOCN(ICOMP,IDPN(ICOMP,2))
          WRITE(OUTSTR,'(5a)')' Solar insolation focused on ',
     &    SSNAME(ICM1),' & ',SSNAME(ICM2),
     &    ' (if shading not calculated).'
          call edisp(ITRU,OUTSTR)
        ELSEIF(NDP(ICOMP).EQ.3)THEN
          write(outstr,'(2a)') 
     &    ' All surfaces will receive diffuse insolation (if shading',
     &    ' not calculated).'
          call edisp(ITRU,OUTSTR)
        ENDIF
      endif

C If there are existing ish directives print them out. An older
C geometry file will not have this information. 
      if(nsurfcalc(icomp).gt.0)then
        ns=nsurfcalc(icomp)
        call surlist(icomp,ns,lstsfcalc,sdescr,length,ierr)
        if(iaplic(icomp,1).eq.0)then
          call edisp(ITRU,
     &      ' Surfaces (user list) for shading analysis: ')
        else
          call edisp(ITRU,
     &      ' Surfaces (all applicable) for shading analysis: ')
        endif
        call edisp(ITRU,sdescr)
      else
        if(ISI(ICOMP).eq.1)then
          continue   ! obviously requested
        else
          call edisp(ITRU,' No shading analysis requested.')
        endif
      endif

      if(nsurfinso(icomp).gt.0)then
        ns=nsurfinso(icomp)
        call surlist(icomp,ns,isurfinso,sdescr,length,ierr)
        if(iaplic(icomp,2).eq.0)then
          call edisp(ITRU,' Insolation sources (user list): ')
        else
          call edisp(ITRU,' Insolation sources (all applicable):')
        endif
        call edisp(ITRU,sdescr)
      else
        if(ISI(ICOMP).eq.1)then
          continue    ! obviously requested
        else
          call edisp(ITRU,' No insolation analysis requested.')
        endif
      endif

      RETURN
      END

C ********************** VERINFO 
C VERINFO prints two summaries, the first includes vertex coordinates
C and a list of surfaces associated with each vertex.  The second
C prints a list of surface numbers and the list of vertices which 
C define them.  Information is gathered from common block G1.
 
      SUBROUTINE VERINFO(ICOMP,ITRU)
#include "building.h"
#include "geometry.h"

C Parameters
      integer icomp  ! zone index
      integer itru   ! unit to write report to
      DIMENSION ISASSO(MS)
      CHARACTER OUTSTR*124,LOUTSTR*248

      call edisp(ITRU,' ')
      call edisp(ITRU,
     &' The zone is defined by the following general polygon vertices:')
      call edisp(ITRU,
     &' Vertex|X-coord.| Y-coord.| Z-coord.|Associated surfaces     |')
      DO 8790 I=1,NZTV(ICOMP)
        IHIT=0
        DO 8792 IS=1,NZSUR(ICOMP)
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
      DO 9991 I=1,NZSUR(ICOMP)
        NV=NVER(I)
        if(NV.le.27)then
          WRITE(OUTSTR,9990)I,NV,(JVN(I,J),J=1,NV)
          call edisp(ITRU,OUTSTR)
        else
          WRITE(LOUTSTR,9990)I,NV,(JVN(I,J),J=1,NV)
          call edisp248(ITRU,LOUTSTR,90)
        endif 
 9990   FORMAT(I6,I9,I5,42(',',I3))
 9991 CONTINUE
      RETURN
      END

C ********************** ZINDEX 
C ZINDEX is passed a string (which might be a zone name or an index) and
C returns an index to the matching zone.  If no match then returns 0. 
      SUBROUTINE ZINDEX(STRING,index)
#include "building.h"
#include "geometry.h"
      
      integer lnblnk  ! function definition

      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      COMMON/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK
      LOGICAL        CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK
      CHARACTER ZN*12
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
        lzn = lnzname(i)
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
#include "building.h"
#include "geometry.h"
      
      integer lnblnk  ! function definition

      COMMON/C24/IZSTOCN(MCOM,MS)
      character string*12,SN*12
      logical ok
      integer lstr  ! length of passed in surface name
      integer icn   ! which connection
      
      lstr=lnblnk(string)
      ok=.true.
      do 42 i=1,NZSUR(icomp)-1   ! last surface corresponds to new surface for which a connection does not yet exist so don't count it
        icn=izstocn(icomp,i)
        SN=SSNAME(icn)           ! assign local name for other surface
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

C ********************** MATCHSNAME 
C matchsname is passed a surface name and zone index and checks which
C surface index in the zone matches the name.
C If 'is' is returned as 0 if no match. 
      subroutine matchsname(STRING,icomp,is)
#include "building.h"
#include "geometry.h"
      
      integer lnblnk  ! function definition

      COMMON/C24/IZSTOCN(MCOM,MS)
      character string*12,SN*12
      
      lstr=lnblnk(string)
      is=0
      do 42 i=1,NZSUR(icomp)
        icn=izstocn(icomp,i)
        SN=SSNAME(icn)
        if(string(1:lstr).eq.SN(1:lnblnk(SN)))then
          is=i
          return
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
      subroutine surlist(icomp,nbsur,lstsf,sdescr,length,ierr)
#include "building.h"
#include "geometry.h"
      
      integer lnblnk  ! function definition

      COMMON/C24/IZSTOCN(MCOM,MS)
      
      dimension lstsf(mcom,ms),iva(ms)
      CHARACTER sdescr*80,sn*12
      logical unixok

C Find out overall length and build 1D array iva.
      length=0
      do 42 i=1,nbsur
        icc=izstocn(icomp,lstsf(icomp,i))
        iva(i)=lstsf(icomp,i)
        lna=lnblnk(SSNAME(icc))
        length=length+lna+1
  42  continue
      if(length.lt.80)then

C Write out a packed string (space separated) of each surface name.
        sdescr=' '
        ix=1
        ixl=0
        do 43 i=1,nbsur
          ico=izstocn(icomp,lstsf(icomp,i))
          sn=SSNAME(ico)
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
          ic1=izstocn(icomp,lstsf(icomp,1))
          WRITE(SDESCR,'(A)',iostat=ios,err=1) SSNAME(ic1)
        elseif(nbsur.gt.1)THEN

C Write indices into one packed string sdescr (i.e. truncate).
          ipos=1
          call ailist(ipos,nbsur,iva,MS,'S',sdescr,loutlen,itrunc)
        endif
        return
      endif

C Only give feedback if non-DOS.
   1  call isunix(unixok)
      if(unixok)then
        if(IOS.eq.2)then
          write(6,*) 'SURLIST: permission error writing surf names:',
     &      sdescr
        else
          write(6,*) 'SURLIST: error writing surface names: ',sdescr
        endif
      endif
      ierr=1
      return
      END
