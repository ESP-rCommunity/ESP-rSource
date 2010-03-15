C This header relates to geometric entities in ESP-r. It is
C dependant on building.h and include statements to geometry.h should
C follow building.h so that parameters will have been defined.

C ctype (4 char) - zone shape type (REC, REG or GEN) see egomin for definition
C shape (8 char) - META file zone shape (extrude, poly, box)
      character CTYPE*4,shape*8
      common/g0/CTYPE(MCOM),shape(MCOM)

C rotateit (zone,1) pending angle of rotation using std conventions for REC zones or any shape
C rotateit (zone,2) pending X coordinate to rotate around
C rotateit (zone,3) pending Y coordinate to rotate around
C rotateit (zone,4) prior angle of rotation to support un-do operations
C rotateit (zone,5) prior X coordinate to rotate around
C rotateit (zone,6) prior Y coordinate to rotate around
      real rotateit
      common/metadat/rotateit(mcom,6)

C gversion (real) version of the geometry file (1.0 legacy, 1.1 current).
C igupgrade (integer) 0 = do nothing, 1 = leave at 1.0, 2 = upgrade to 1.1
      real gversion
      integer igupgrade
      common/g2/gversion(MCOM),igupgrade

C Default solar distribution and shading directives.
      integer ndp,idpn
      common/g4/ndp(MCOM),idpn(MCOM,3)

C NZSUR (integer) is the number of surfaces in each zone.
C NZTV (integer) is the number of vertices in each zone.
C nbwalls (integer) how many vertical walls (e.g. 4 for a box, 3+ for extrude).
C   For box and extrude the top surface is assumed to be index nbwalls+1 and
C   the base surface is assumed to be index nbwalls+2. For poly type zones
C   nbwalls is the initial number of surfaces prior to addition of META
C   objects.
      integer nzsur,nztv,nbwalls
      common/c20/nzsur(MCOM),nztv(MCOM),nbwalls(MCOM)

C Arrays of verious types which hold data in (zone,surface) size primarily
C for use with META files.
      integer szboundarytype  ! equivalent to ICT/IC2/IE2 in common c3
      common/metac3/szboundarytype(MCOM,MS,3)

C ZBASEA is the floor area of the zone, IBASES is a list of surfaces
C which make up the floor, IUZBASEA signals that the user selected
C specific surfaces (two), has edited the floor area (one) or it was
C calculated based on FLOR orientations (zero). IZBASELIST is the
C number of items in the list. 
      real zbasea
      integer ibases,iuzbasea,izbaselist
      common/prec17/zbasea(MCOM),ibases(MCOM,12),iuzbasea(MCOM),
     &  izbaselist(MCOM)

C Global coordinates for whole model.
      real VCOORD    ! X,Y & Z coordinates of vertices in all zones.
      integer NZNVER ! number of vertices associated with each connection.
      integer NZJVN  ! topology of vertices associated with each connection.
      COMMON/ZNDATA/VCOORD(MCOM,MTV,3),NZNVER(MCON),NZJVN(MCON,MV)

C The following section will hold derived data such as volume and surface
C areas which are of general interest to many subroutines.
C vol   - zone bounded volume (m^3)
C zonetotsurfacearea - area (m2) of surfaces associated with each zone.
      real vol,zonetotsurfacearea
      common/prec2/vol(MCOM),zonetotsurfacearea(MCOM)

      real ssna    ! surface area of each polygon
      real sspazi  ! plane azimuth angle (degrees 0=north, 90=west)
      real sspelv  ! plane elevation angle (degrees 0=wall, 90=ceiling -90=floor
      real ssperim ! perimeter of each surface (m).
      real ssureqn ! equation of each polygon A*X + B*Y + C*Z = D
      real ssurcog ! vertex weighted COG of polygon,
      real ssurvn  ! unit normal vector from COG of polygon.
      common/g7/ssna(MCON),sspazi(MCON),sspelv(MCON),ssperim(MCON),
     &          ssureqn(MCON,4),ssurcog(MCON,3),ssurvn(MCON,3)

C zname (12 char) - the zone name.
C zedsc (64 char) - zone notes.
C lnzname,lnzdesc - length of strings.
      character zname*12,zdesc*64
      COMMON/precz/zname(MCOM),zdesc(MCOM)
      integer lnzname,lnzdesc
      common/preczln/lnzname(MCOM),lnzdesc(MCOM)

C Obstruction block commons (whole model)
      integer NOX,NOZ      ! gridding X and Z resolution of surfaces for shading
      common/GS6/NOX(MCOM),NOZ(MCOM)

      integer nbobs     ! number of zone obstructions
      real XOB,YOB,ZOB  ! coordinates of each block origin.
      real DXOB,DYOB,DZOB  ! width depth and height of each block
      real BANGOB       ! three rotation angles of block
      common/GS7/nbobs(MCOM),XOB(MCOM,MB),YOB(MCOM,MB),ZOB(MCOM,MB),
     &  DXOB(MCOM,MB),DYOB(MCOM,MB),DZOB(MCOM,MB),BANGOB(MCOM,MB,3)

C Eight coordinates for general polygon obstruction 'obsp'. If an
C obstruction is of type 'obsp' then the XOB,YOB,ZOB and DXOB,DYOB
C DZOB and BANGOB values will all be zero. This type of obstruction
C has six sides and eight vertices and each face has 4 edges.
C The edge ordering is the same as in the GB1 common block.
      real XBP,YBP,ZBP   ! 8 XYZ coordinates with standard edge linkages.
      common/GS9/XBP(MCOM,MB,8),YBP(MCOM,MB,8),ZBP(MCOM,MB,8)

C BLOCKNAME (12 char) name of obstruction block.
C BLOCKMAT (32 char) obstruction block construction (for Radiance).
C BLOCKTYP (4 char) type of block:
C   'obs ' - standard block (origin, three dimensions, one rotation)
C   'obs3' - general block (origin, three dimensions, three rotations)
C   'obsp' - general polygon (six sides formed from 8 vertices)
C LNBLOCKNAME,LNBLOCKMAT - length of strings.
      character BLOCKNAME*12,BLOCKMAT*12,BLOCKTYP*4
      common/GS8/BLOCKNAME(MCOM,MB),BLOCKMAT(MCOM,MB),BLOCKTYP(MCOM,MB)
      integer LNBLOCKNAME,LNBLOCKMAT
      common/GS8LN/LNBLOCKNAME(MCOM,MB),LNBLOCKMAT(MCOM,MB)


