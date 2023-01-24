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
      real rotateit
      common/metadat/rotateit(mcom,3)

C gversion (real) version of the geometry file (1.0 legacy, 1.1 current).
C igupgrade (integer) 0 = do nothing, 1 = leave at 1.0, 2 = upgrade to 1.1
      real gversion
      integer igupgrade
      common/g2/gversion(MCOM),igupgrade

C Default solar distribution and shading directives.
      integer ndp,idpn
      common/g4/ndp(MCOM),idpn(MCOM,3)

C Surface attributes for the current zone.
      character SNAME*12   ! surface name
      character SOTF*32    ! OPAQUE or optical property name
      character SVFC*4     ! indicates whether the surface is to be counted
                           ! as a FLOR (face up), VERT (wall), CEIL (face down),
                           ! SLOP (not vertical) or UNKN (not yet defined) 
      character SMLCN*32   ! surface construction name (in MLC database)
      character SOTHER*24  ! three surface attributes for `other` side:
        ! SOTHER(,,1) = UNKNOWN indicates that no attribute has been set
        !               and for this case SOTHER(,,2) and SOTHER(,,3) are '0'
        ! SOTHER(,,1) = EXTERIOR means SOTHER(,,2) & SOTHER(,,3) are '0'
        ! SOTHER(,,1) = ADIABATIC means SOTHER(,,2) & SOTHER(,,3) are '0'
        ! SOTHER(,,1) = SIMILAR means that SOTHER(,,2) & SOTHER(,,3) are as IC2 & IE2
        ! SOTHER(,,1) = BASESIMP means that SOTHER(,,2) & SOTHER(,,3) are as IC2 & IE2
        ! SOTHER(,,1) = GROUND means that SOTHER(,,2) & SOTHER(,,3) are as IC2 & IE2
        ! SOTHER(,,1) = ANOTHER then SOTHER(,,2) is zone index & SOTHER(,,3) is
        !               the surface index in that zone (e.g. as IC2 and IE2).
      character SUSE*12    ! two attributes of the usage of the surface
      character SPARENT*12 ! the name of the parent surface or '-'
      COMMON/G5/SNAME(MCOM,MS),SOTF(MCOM,MS),SMLCN(MCOM,MS),
     &          SVFC(MCOM,MS),SOTHER(MCOM,MS,3),SUSE(MCOM,MS,2),
     &          SPARENT(MCOM,MS)

      integer lnsname,lnsotf ! length of sname and sotf strings
      common/G5LN/lnsname(MCOM,MS),lnsotf(MCOM,MS)

C Althought smlcn(mcom,ms) holds the name of the construction, also knowing the
C matching index in the database can save search time.
      integer smlcindex  ! for each surface points to MLC db item or zero
      common/precmlc/smlcindex(MCOM,MS)

C Surface polygon information for the current zone.
      real X,Y,Z   ! coordinate in space (m)
      integer NSUR ! number of surfaces in the zone
      integer JVN  ! list of edges (vertex indices anticlockwise
                   ! looking from outside) for each surface
      integer NVER ! number of edges that make up each surface
      integer NTV  ! total number of vertices in the zone which
                   ! should be the same as NZTV
      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV

C Arrays of various types which hold data in (zone,surface) size primarily
C for use with META files.
      integer nzsur    ! The number of surfaces in each zone.
      integer nztv     ! The number of vertices in each zone.
      integer nbwalls  ! How many vertical walls (e.g. 4 for a box, 3+ for extrude).
                       ! For box and extrude the top surface is index nbwalls+1 and
                       ! the base surface is index nbwalls+2. For poly type zones
                       ! nbwalls is the initial number of surfaces prior to addition
                       ! of META objects.
      real zorigin     ! XYZ of the origin in the case of shape=box. For shape=extrude
                       ! the 1st value is Z point of base, 2nd is the ceiling Z value, 
                       ! 3rd not used. And for shape=poly this is ignored.
      real zsize       ! Initial length/width/height of box shaped zones (not used for   
                       ! any other zone shapes).
      common/c20/nzsur(MCOM),nztv(MCOM),nbwalls(MCOM),zorigin(MCOM,3),
     &  zsize(MCOM,3)

      integer zboundarytype  ! equivalent to ICT/IC2/IE2 in common c3
      common/metac3/zboundarytype(MCOM,MS,3)

      integer znbmass    ! Number of META paired mass rectangles in room.
                         ! Note these pairs of surfaces are not included in the *surface list.
      real zdatamass     ! Origin, size, orientations of META mass objects (4 per zone).
      common/metahas/znbmass(MCOM),zdatamass(MCOM,4,7)

C Stings associated with internal mass. 1=surface name, 2=construction, 3=optics
      character ztextmass*32
      common/metathas/ztextmass(MCOM,4,3)

      real zbasea        ! the floor area of the zone
      integer ibases     ! list of surfaces which make up the floor
      integer iuzbasea   ! signals that the user selected specific surfaces (2),
                         ! has edited the floor area (one) or it was calculated
                         ! based on FLOR orientations (zero).
      integer izbaselist ! the number of items in the ibases list. 
      common/prec17/zbasea(MCOM),ibases(MCOM,MBL),iuzbasea(MCOM),
     &  izbaselist(MCOM)

C ibridgeshr(MCON,MV) is:
      integer nbedgdup  ! number of duplicate edges in surface edge list
      integer iedgdup   ! for each edge, the connection and edge duplicated
      integer nbedgshr  ! number of adjacent surfaces sharing an edge
                        ! if 1 then it must be parent, if 2 then it might be a
                        ! door so determine which is primary.
      integer iedgshr   ! for each edge, the adjacent surface connection index
                        ! (similarly oriented) sharing the edge. Used to detect
                        ! parent/child. Zero denotes this does not apply.
      integer imatshr   ! for each edge, the adjacent surface MLC index. Zero
                        ! denotes this does not apply. Used to enhance wire frame
                        ! drawings of highly discretised zones.
      integer ibridgeshr ! for each edge likely type of thermal bridge:
         ! 0 is not a thermal bridge, 1 is roof-wall,
         ! 2 is wall-ground floor, 3 is wall-wall (convex corner)
         ! 4 is wall-wall (concave corner), 5 is wall-floor (not ground),
         ! 6 is lintel above window or door, 7 is Sill below window
         ! 8 is jamb at window or door
      
      common/G8/nbedgdup(MCON),iedgdup(MCON,MV),nbedgshr(MCON),
     &          iedgshr(MCON,MV),imatshr(MCON,MV),ibridgeshr(MCON,MV)

C G9 holds information on children of a surface and its parent.
      integer,PARAMETER :: MCHILD=12
      integer nbchild   ! how many children (up to MCHILD) for each connection
      integer nbgchild  ! how many grand children (up to MCHILD) for each connection
      integer ichild    ! list of children for each connection
      integer igchild   ! list of grand children for each connection
      integer iparent   ! parent surface connection (zero is no parent)
      integer igparent  ! grandparent surface connection (zero is no grandparent)
      common/G9/nbchild(MCON),nbgchild(MCON),ichild(MCON,MCHILD),
     &          igchild(MCON,MCHILD),iparent(MCON),igparent(MCON)

C Global coordinates for whole model (zone & surface based). These are used
C to accumulate information prior to the generation of the connection list.
      integer isznver   ! nb of edges per surface in poly shaped zone
                        ! equivalent to nver
      integer iszjvn    ! indices of coords making up edges of each surface
                        ! equivalent to jvn
      real szcoords     ! X Y Z coordinates associated with each zone.
      common/metageo/isznver(MCOM,MS),iszjvn(MCOM,MS,MV),
     &               szcoords(MCOM,MTV,3)

C The following section holds derived data such as volume and surface
C areas which are of general interest to many subroutines.
      real vol                ! zone bounded volume (m^3)
      real zonetotsurfacearea ! area (m2) of surfaces associated with each zone
      common/prec2/vol(MCOM),zonetotsurfacearea(MCOM)

      character zname*12  ! the zone name
      character zdesc*64  ! zone notes
      COMMON/precz/zname(MCOM),zdesc(MCOM)

      integer lnzname,lnzdesc ! length of zname and zdesc strings
      common/preczln/lnzname(MCOM),lnzdesc(MCOM)
      
c Long zone name for H3K reports
      common/H3KSTORE_ZONE/zoneLabel(MCOM)
      CHARACTER zoneLabel*32      

C Obstruction block commons (whole model)
      integer NOX,NOZ      ! gridding X and Z resolution of surfaces for shading
      common/GS6/NOX(MCOM),NOZ(MCOM)

      integer nbobs        ! number of zone obstructions
      real XOB,YOB,ZOB     ! coordinates of each block origin.
      real DXOB,DYOB,DZOB  ! width depth and height of each block
      real BANGOB          ! three rotation angles of block
      real OPOB            ! opacity of obstruction 0.0 is transparent 1.0 is opaque
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
      character BLOCKNAME*12,BLOCKMAT*32,BLOCKTYP*4
      common/GS8/BLOCKNAME(MCOM,MB),BLOCKMAT(MCOM,MB),BLOCKTYP(MCOM,MB)
      integer LNBLOCKNAME,LNBLOCKMAT
      common/GS8LN/LNBLOCKNAME(MCOM,MB),LNBLOCKMAT(MCOM,MB)

C Visual entities to pass to Radiance and for model decoration.
      integer nbvis        ! number of visual entities in a zone
      real XOV,YOV,ZOV     ! coordinates of each visual origin.
      real DXOV,DYOV,DZOV  ! width depth and height of each visual block
      real BANGOV          ! three rotation angles of a visual block
      real OPOV            ! opacity of visual 0.0 is transparent 1.0 is opaque
      common/GSVB/nbvis(MCOM),XOV(MCOM,MB),YOV(MCOM,MB),ZOV(MCOM,MB),
     &  DXOV(MCOM,MB),DYOV(MCOM,MB),DZOV(MCOM,MB),BANGOV(MCOM,MB,3),
     &  OPOV(MCOM,MB)

C Eight coordinates for general polygon visual 'visp'. If an
C of type 'visp' then the XOV,YOV,ZOV and DXOV, DYOV, DZOV
C and BANGOV values will all be zero. It has six sides and eight 
C vertices and each face has 4 edges.
C The edge ordering is the same as in the GS9 common block.
      real XVP,YVP,ZVP   ! 8 XYZ coordinates with standard edge linkages.
      common/GSVP/XVP(MCOM,MB,8),YVP(MCOM,MB,8),ZVP(MCOM,MB,8)

C VISNAME (12 char) name of visual block.
C VISMAT (32 char) visual block construction (for Radiance).
C VISTYP (4 char) type of block:
C   'vis ' - standard block (origin, three dimensions, one rotation)
C   'vis3' - general block (origin, three dimensions, three rotations)
C   'visp' - general polygon (six sides formed from 8 vertices)
C LNVISNAME,LNVISMAT - length of strings.
      character VISNAME*12,VISMAT*32,VISTYP*4
      common/GSVN/VISNAME(MCOM,MB),VISMAT(MCOM,MB),VISTYP(MCOM,MB)
      integer LNVISNAME,LNVISMAT
      common/GSVLN/LNVISNAME(MCOM,MB),LNVISMAT(MCOM,MB)

C Group visual items to create a visual objects (up to MVOBJ) from
C up to MVOBJE visual primitives.
C VOBJNAME (12 char) name of visual object
C VOBJDESC (32 char) description of visual object
C VOBJLIST (12 char) associated (up to MVOBJE) visual blocks
C VOBJILIST (integer) indices of associated visual blocks
C LNVOBJNAME,LNVOBJDESC,LNVOBJLIST - length of strings.
C NBVOBJ - number of visual objects in each zone
C NBVOJBLIST - number of primitives in ojbect
      character VOBJNAME*12,VOBJDESC*32,VOBJLIST*12
      common/GSVOBJN/VOBJNAME(MCOM,MVOBJ),VOBJDESC(MCOM,MVOBJ),
     &  VOBJLIST(MCOM,MVOBJ,MVOBJE)
      integer NBVOBJ,LNVOBJNAME,LNVOBJDESC,LNVOBJLIST,NBVOBJLIST,
     &        VOBJILIST
      common/GSVOBJI/NBVOBJ(MCOM),LNVOBJNAME(MCOM,MVOBJ),
     &  LNVOBJDESC(MCOM,MVOBJ),LNVOBJLIST(MCOM,MVOBJ,MVOBJE),
     &  NBVOBJLIST(MCOM,MVOBJ),VOBJILIST(MCOM,MVOBJ,MVOBJE)

C MRT sensors for the model.
      integer ncub  ! for each zone number of mrt sensors
      integer ivfok
      real XOC,YOC,ZOC,DXC,DYC,DZC,CANG
      character CUBN*6
      common/MRTC/NCUB(MCOM),XOC(MCUB),YOC(MCUB),
     &  ZOC(MCUB),DXC(MCUB),DYC(MCUB),
     &  DZC(MCUB),CANG(MCUB),IVFOK(MCUB),CUBN(MCUB)

C Derived attributes of surfaces.
      real sna     ! surface area of each polygon
      real spazi   ! plane azimuth angle (degrees 0=north, 90=west)
      real spelv   ! plane elevation angle (degrees 0=wall, 90=ceiling -90=floor
      real sureqn  ! equation of each polygon A*X + B*Y + C*Z = D
      real surcog  ! vertex weighted COG of polygon,
      real survn  ! unit normal vector from COG of polygon.
      common/derived/sna(MCOM,MS),spazi(MCOM,MS),spelv(MCOM,MS),
     &  sureqn(MCOM,MS,4),surcog(MCOM,MS,3),survn(MCOM,MS,3)

C izsfloor is the index (within the zone) of a floor surface in each zone
C  (zero if no surface is close to horizontal facing up).
C izsceil is the index (within the zone) of a ceiling surface in each zone 
C  (zero if no surface is close to horizontal facing down).
      integer izsfloor,izsceil
      COMMON/PREC16/izsfloor(MCOM),izsceil(MCOM)

