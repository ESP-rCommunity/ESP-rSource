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

C Surface attributes for the current zone.
      character SNAME*12   ! surface name attribute
      character SOTF*32    ! see SSOTF below
      character SVFC*4     ! see SSVFC below
      character SMLCN*32   ! see SSMLCN below
      character SOTHER*24  ! see SSOTHER below
      character SUSE*8     ! see SSUSE below
      character SPARENT*12 ! see SSPARENT below
      COMMON/G5/SNAME(MCOM,MS),SOTF(MS),SMLCN(MS),SVFC(MS),SOTHER(MS,3),
     &          SUSE(MS,2),SPARENT(MS)

C Surface polygon information for the current zone.
      real X,Y,Z   ! coordinate in space (m)
      integer NSUR ! number of surfaces in the zone
      integer JVN  ! list of edges (vertex indices anticlockwise
                   ! looking from outside) for each surface
      integer NVER ! number of edges that make up each surface
      integer NTV  ! total number of vertices in the zone 
      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV

C NZSUR (integer) is the number of surfaces in each zone.
C NZTV (integer) is the number of vertices in each zone.
C nbwalls (integer) how many vertical walls (e.g. 4 for a box, 3+ for extrude).
C   For box and extrude the top surface is assumed to be index nbwalls+1 and
C   the base surface is assumed to be index nbwalls+2. For poly type zones
C   nbwalls is the initial number of surfaces prior to addition of META
C   objects.
C zorigin (real * 3) XYZ of the origin in the case of shape=box. For shape=extrude
C   the 1st value is Z point of base, 2nd is the ceiling Z value, 3rd not used.
C   And for shape=poly this is ignored.
C zsize (real * 3) initial length/width/height of box shaped zones (not used for
C   any other zone shapes).
      integer nzsur,nztv,nbwalls
      real zorigin,zsize
      common/c20/nzsur(MCOM),nztv(MCOM),nbwalls(MCOM),zorigin(MCOM,3),
     &  zsize(MCOM,3)

C Arrays of verious types which hold data in (zone,surface) size primarily
C for use with META files.
      integer zboundarytype  ! equivalent to ICT/IC2/IE2 in common c3
      common/metac3/zboundarytype(MCOM,MS,3)

      integer znbglz   ! nb of windows (META directive)
      integer znbdoor  ! nb of doors (META directive)
      real zhasglaze   ! percentage of glazing (default position) in each surface of
                       ! the initial room shape and where zero is no glazing
      real zhasdoor    ! width of door in each surface of the initial room shape
                       ! where zero is no door
      integer znbmass  ! number of META paired mass rectangles in room
                       ! Note these pairs of surfaces are not included in the *surface list
      real zdatamass   ! origin, size, orientations of META mass objects (4 per zone).
      common/metahas/znbglz(MCOM),zhasglaze(MCOM,MS),
     &               znbdoor(MCOM),zhasdoor(MCOM,MS),
     &               znbmass(MCOM),zdatamass(MCOM,4,7)


C Stings associated with internal mass. 1=surface name, 2=construction, 3=optics
      character ztextmass*32
      common/metathas/ztextmass(MCOM,4,3)

C ZBASEA is the floor area of the zone, IBASES is a list of surfaces
C which make up the floor, IUZBASEA signals that the user selected
C specific surfaces (two), has edited the floor area (one) or it was
C calculated based on FLOR orientations (zero). IZBASELIST is the
C number of items in the list. 
      real zbasea
      integer ibases,iuzbasea,izbaselist
      common/prec17/zbasea(MCOM),ibases(MCOM,12),iuzbasea(MCOM),
     &  izbaselist(MCOM)

C nbedgshr(MCON) number of surfaces (same orient) which share an edge:
C   if only one then it must be parent, if two then it might be the
C   case of a door so determine which is primary.
C iedgshr(MCON,MV) for each edge, the connection of surface (similarly
C   oriented) sharing an edge. Used to detect parent/child. Zero denotes
C   this does not apply.
C imatshr(MCON,MV) for each edge, the connection of surface (similarly
C   oriented) which has the same material. Zero denotes this does not
C   apply. Used to enhance wire frame drawings of discritized zones (e.g.
C   where a surface such as a floor has been subdivided)
C ibridgeshr(MCON,MV) is:
C   zero is not a thermal bridge, one is roof-wall,
C   two is wall-ground floor, three is wall-wall (convex corner)
C   four is wall-wall (concave corner), five is wall-floor (not ground),
C   six is lintel above window or door, seven is Sill below window
C   eight is jamb at window or door
      integer nbedgdup  ! number of duplicate edges in surface edge list
      integer iedgdup   ! for each edge, the connection and edge duplicated
      integer nbedgshr  ! nb surface which share an edge
      integer iedgshr   ! for each edge, the adjacent surface
      integer imatshr   ! for each edge, the adjacent material
      integer ibridgeshr ! for each edge likely type of thermal bridge
      common/G8/nbedgdup(MCON),iedgdup(MCON,MV),nbedgshr(MCON),
     &          iedgshr(MCON,MV),imatshr(MCON,MV),ibridgeshr(MCON,MV)

C G9 holds information on children of a surface and its parent.
      integer nbchild   ! how many children (up to 4) for each connection
      integer nbgchild  ! how many grand children (up to 4) for each connection
      integer ichild    ! list of children for each connection
      integer igchild   ! list of grand children for each connection
      integer iparent   ! parent surface connection (zero is no parent)
      integer igparent  ! grandparent surface connection (zero is no grandparent)
      common/G9/nbchild(MCON),nbgchild(MCON),ichild(MCON,4),
     &          igchild(MCON,4),iparent(MCON),igparent(MCON)

C Global coordinates for whole model (connection based).
      real VCOORD    ! X,Y & Z coordinates of vertices in all zones.
      integer NZNVER ! number of vertices associated with each connection.
      integer NZJVN  ! topology of vertices associated with each connection.
      COMMON/ZNDATA/VCOORD(MCOM,MTV,3),NZNVER(MCON),NZJVN(MCON,MV)

C Global coordinates for whole model (zone & surface based). These are used
C to accumulate information prior to the generation of the connection list.
      integer isznbedges ! nb of edges per surface in poly shaped zone
                         ! equivalent to nznver and nver
      integer iszlist    ! indices of coords making up edges of each surface
                         ! equivalent to NZJVN and jvn
      real szcoords      ! coordinates associated with each zone, equivalent
                         ! to X Y Z and VCOORD for shape=box ignore,
                         ! for shape=extrude the 1st value is X and 2nd is Y
                         ! For shape=poly the Z is also used.
      common/metageo/isznbedges(MCOM,MS),iszlist(MCOM,MS,MV),
     &               szcoords(MCOM,MTV,3)

C The following section holds derived data such as volume and surface
C areas which are of general interest to many subroutines.
      real vol                ! zone bounded volume (m^3)
      real zonetotsurfacearea ! area (m2) of surfaces associated with each zone
      common/prec2/vol(MCOM),zonetotsurfacearea(MCOM)

      character zname*12  ! the zone name
      character zdesc*64  ! zone notes
      COMMON/precz/zname(MCOM),zdesc(MCOM)
      
c Long zone name for H3K reports
      common/H3KSTORE_ZONE/zoneLabel(MCOM)
      CHARACTER zoneLabel*32      

      integer lnzname,lnzdesc ! length of zname and zdesc strings
      common/preczln/lnzname(MCOM),lnzdesc(MCOM)

C Obstruction block commons (whole model)
      integer NOX,NOZ      ! gridding X and Z resolution of surfaces for shading
      common/GS6/NOX(MCOM),NOZ(MCOM)

      integer nbobs     ! number of zone obstructions
      real XOB,YOB,ZOB  ! coordinates of each block origin.
      real DXOB,DYOB,DZOB  ! width depth and height of each block
      real BANGOB       ! three rotation angles of block
      real OPOB         ! opacity of obstruction 0.0 is transparent 1.0 is opaque
      common/GS7/nbobs(MCOM),XOB(MCOM,MB),YOB(MCOM,MB),ZOB(MCOM,MB),
     &  DXOB(MCOM,MB),DYOB(MCOM,MB),DZOB(MCOM,MB),BANGOB(MCOM,MB,3),
     &  OPOB(MCOM,MB)

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

C The following section hold arrays associated with each zone connection
C in the model. These data structures are typically used when the model
C composition is fixed (surfaces are not being added or subtracted) and
C all surfaces in the model need to be accessed.

C Surface attributes:
      character SSNAME*12  ! surface name attribute (see also SNAME)
      character SSOTF*32   ! indicates OPAQUE or optical property name (see SOFT).
      character SSVFC*4    ! indicates whether the surface is to be counted
                           ! as a FLOR (face up), VERT (wall), CEIL (face down),
                           ! SLOP (not vertical) or UNKN (not yet defined) 
                           ! (see also SVFC). 
      character SSMLCN*32  ! surface construction attribute currently only
                           ! only the first 12 char are used. (see also SMLCN)
      character SSOTHER*24 ! three surface attributes for `other` side as below 

C SSOTHER(i,1) = UNKNOWN indicatesthat no attribute has been set and
C                for this case SSOTHER(i,2) and SSOTHER(i,3) are '0'
C SSOTHER(i,1) = EXTERIOR means SSOTHER(i,2) and SSOTHER(i,3) are '0'
C SSOTHER(i,1) = ADIABATIC means SSOTHER(i,2) and SSOTHER(i,3) are '0'
C SSOTHER(i,1) = SIMILAR means that SSOTHER(i,2) and SSOTHER(i,3) are as IC2 & IE2
C SSOTHER(i,1) = BASESIMP means that SSOTHER(i,2) and SSOTHER(i,3) are as IC2 & IE2
C SSOTHER(i,1) = GROUND means that SSOTHER(i,2) and SSOTHER(i,3) are as IC2 & IE2
C SSOTHER(i,1) = ANOTHER then SSOTHER(2) is zone index and SSOTHER(i,3) is
C                the surface index in that zone (e.g. as IC2 and IE2).

      character SSUSE*8     ! two attributes of the usage of the surface:

C DOOR,CLOSED  DOOR,UNDERCUT  DOOR,OPEN  DOOR,BIDIR
C FRAME,CLOSED FRAME,CRACK    FRAME,OPEN
C WINDOW,CLOSED WINDOW,CRACK  WINDOW,OPEN  WINDOW,SASH  WINDOW,BIDIR
C GRILL,SOURCE  GRILL,EXTRACT
C FICT,CLOSED  FICT,CRACK  FICT,OPEN  FICT,BIDIREC
C BLIND,FIXED  BLIND,MOVE (??explicit blinds)
C otherwise  -,- )
C see geometry.F for a full explanation). Not found in older geometry files
      character SSPARENT*12 ! the name of the parent surface or '-'
      COMMON/G6/SSNAME(MCON),SSOTF(MCON),SSMLCN(MCON),SSVFC(MCON),
     &          SSOTHER(MCON,3),SSPARENT(MCON),SSUSE(MCON,2)

      real ssna    ! surface area of each polygon
      real sspazi  ! plane azimuth angle (degrees 0=north, 90=west)
      real sspelv  ! plane elevation angle (degrees 0=wall, 90=ceiling -90=floor
      real ssperim ! perimeter of each surface (m).
      real ssureqn ! equation of each polygon A*X + B*Y + C*Z = D
      real ssurcog ! vertex weighted COG of polygon,
      real ssurvn  ! unit normal vector from COG of polygon.
      real ssthick ! thickness of surface (m) based on MLC and zero if no MLC
      common/g7/ssna(MCON),sspazi(MCON),sspelv(MCON),ssperim(MCON),
     &          ssureqn(MCON,4),ssurcog(MCON,3),ssurvn(MCON,3),
     &          ssthick(MCON)

C izsfloor is the index (within the zone) of a floor surface in each zone
C  (zero if no surface is close to horizontal facing up).
C izsceil is the index (within the zone) of a ceiling surface in each zone 
C  (zero if no surface is close to horizontal facing down).
      integer izsfloor,izsceil
      COMMON/PREC16/izsfloor(MCOM),izsceil(MCOM)

C Althought ssmlcn(mcon) holds the name of the construction, also knowing the
C matching index in the database can save search time.
      integer ssmlcindex  ! for each connection points to MLC db item or zero
      common/precmlc/ssmlcindex(MCON)

