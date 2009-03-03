C This header relates to geometric entities in ESP-r. It is
C dependant on building.h and include statements to geometry.h should
C follow building.h so that parameters will have been defined.

C ctype (4 char) - zone shape type (REC, REG or GEN) see egomin for definition
      character CTYPE*4
      common/g0/CTYPE(MCOM)

C gversion (real) version of the geometry file (1.0 legacy, 1.1 current).
C igupgrade (integer) 0 = do nothing, 1 = leave at 1.0, 2 = upgrade to 1.1
      real gversion
      integer igupgrade
      common/g2/gversion(MCOM),igupgrade

C Default solar distribution and shading directives.
      integer ndp,idpn
      common/g4/ndp(MCOM),idpn(MCOM,3)

C NZSUR is the number of surfaces in each zone.
C NZTV is the number of vertices in each zone.
      integer nzsur,nztv
      common/c20/nzsur(MCOM),nztv(MCOM)

C ZBASEA is the floor area of the zone, IBASES is a list of surfaces
C which make up the floor, IUZBASEA signals that the user selected
C specific surfaces (two), has edited the floor area (one) or it was
C calculated based on FLOR orientations (zero). IZBASELIST is the
C number of items in the list. 
      real zbasea
      integer ibases,iuzbasea,izbaselist
      common/prec17/zbasea(MCOM),ibases(MCOM,12),iuzbasea(MCOM),
     &  izbaselist(MCOM)

C The following section will hold derived data such as volume and surface
C areas which are of general interest to many subroutines.
C VOL   - zone bounded volume (m^3)
      real vol
      common/prec2/vol(MCOM)

C zname (12 char) - the zone name.
C zedsc (64 char) - zone notes.
C lnzname,lnzdesc - length of strings.
      character zname*12,zdesc*64
      COMMON/precz/zname(MCOM),zdesc(MCOM)
      integer lnzname,lnzdesc
      common/preczln/lnzname(MCOM),lnzdesc(MCOM)


