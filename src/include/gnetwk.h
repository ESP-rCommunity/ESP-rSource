C Maximum number of icons
      INTEGER, PARAMETER :: MICN=100

C Maximum number of connection points for an icon
      INTEGER, PARAMETER :: MCNP=6

C Maximum number of network connections and connection intermediate points
      INTEGER, PARAMETER :: MNCNN=200,MCIP=6

C Maximum number of attribute sets and data files of an icon.
      INTEGER, PARAMETER :: MIATRC=10,MIATRB=40

C Maximum and minimum zoom factor
      REAL, PARAMETER :: VIEWMZ=8.0,VIEWMN=0.4

C Maximum number of layers
      INTEGER, PARAMETER :: MLYRS=10

C Grid maximum domain X-length, Y-length, Z-length (m)
      INTEGER, PARAMETER :: MGRXL=30,MGRYL=30,MGRZL=30

C Grid initial (scale 1.0) display X-view, Y-view, Z-view extents (m)
      INTEGER, PARAMETER :: MGRXE=10,MGRYE=10,MGRZE=10

C Icon width and height
      REAL, PARAMETER :: XSIZIC=0.75,YSIZIC=0.75

C Icon vertices, edges, dots/labels
      INTEGER, PARAMETER :: MICNV=30,MICNE=30,MICND=15

C Connection point size
      REAL, PARAMETER :: XYSIZCP=0.1

C Icon label offset 
      REAL, PARAMETER :: OFFLAB=0.65

C Maximum number of network types and icon domains.
      INTEGER, PARAMETER :: MNWKTYP=6

C Maximum number of categories per icon domain and icons per category.
      INTEGER, PARAMETER :: MICNCAT=24,MICNICAT=24

C Connection commons [dd] signals connections within the network as
C opposed to [d] templates from the icon database.
      integer NICNN      ! connection number 
      integer ICNS       ! connection start component
      integer ICNE       ! connection end component
      integer ICNNT      ! connection type (see below)
C type 0=none, 1=air, 2=water, 3=steam, 4=refrigerant, 5=fuel,
C      6=combustion product, 7=signal (0-10v)
      integer ICNSP      ! which start attachment point of the component
      integer ICNEP      ! which end attachment point of the component
      real CNWNP         ! connection way points (x,y,z)
      integer NCONWP     ! number of connection way points
      integer idatrdom   ! number of connection attributes
      character ddtagatr*12 ! (see below)
C ddtagatr is an array of tags (12 char) where:
C  the first is the group (thermal/flow/control/output/location/environment/electrical)
C  the 2nd signals data type, the 3rd is `external` or `-`, the 4th
C  is an external key word or `-`, and the 5th is key word 'static' or 'user'.
C  Note `external` signals that an external domain summary file should 
C  be consulted for information associated with the external key word.

      character ddatrib*12 ! (see below)
C ddatrib is an array of strings holding attribute data by type:
C   for integers and real ,datrib(?,?,1) is a value
C   datrib(?,?,2) is minimum, atrib(?,?,3) is maximum, for text data 
C   datrib(?,?,1) is the string and datrib(?,?,2) & datrib(?,?,3) blank.
      character ddmenuatr*32 ! array of menu entries (32 char)
      COMMON/NWKCON/NICNN,ICNS(MNCNN),ICNE(MNCNN),ICNNT(MNCNN),
     & ICNSP(MNCNN),ICNEP(MNCNN),CNWNP(MNCNN,MCIP,3),
     & NCONWP(MNCNN),idatrdom(MNCNN),ddtagatr(MNCNN,MIATRB,5),
     & ddatrib(MNCNN,MIATRB,3),ddmenuatr(MNCNN,MIATRB)


C Icon commons for entities within the network description
      integer NNICN        ! number of components in network
      integer ICONTP       ! icon type (determines what is drawn in screen)
      real XYZICON         ! icon x,y,z-position
      integer NICONATR     ! number of attributes associated with each icon
      character*12 ATRICN  ! icon attributes (array of attribute strings)
      character*12 ATRTAG  ! icon attribute tags (static or user editable)
      character*32 ATRMENU ! icon attribute menu entry
      integer NCONP        ! number of attachment points for each icon
      real CONCP           ! x & y of attachment point connection <-> icon
      integer ICNCT        ! attachment point type (to check against ICNNT(MNCNN))
      real VCICON          ! icon vertex co-ordinates (used in drawing icon)
      integer IVEICN       ! icon vertex edges (used in drawing icon) see below
C IVEICN(n,n,1) is start vertex index, (n,n,2) is end vertex index
C  (n,n,3) is icon line colour (i.e. zone or grey colour index),
C  (n,n,4) colour scale (0=interface, 1=greyscale, 2=zone scale),
C  (n,n,5) line type (1=solid, 2=dotted, 3=dashed, 4=double)
      integer NIVC         ! number of icon vertex co-ordinates
      integer NIVE         ! number of icon vertex edges
      integer NIVD         ! number of dot vertex in an icon
      integer IVDOT        ! icon dot structure see below
C IVDOT((n,n,1) is vertex index
C  (n,n,2) is colour (i.e. zone or grey colour index),
C  (n,n,3) colour scale (0=interface, 1=greyscale, 2=zone scale),
C  (n,n,4) size (0=small, 1=4x4pixel) 
      integer NIVA         ! number of arcs/circles in icon
      integer IVARC        ! icon arc structure see below:
C IVARC((n,n,1) is centre vertex index,
C  (n,n,2) is index of a vertex on the radius,
C  (n,n,3) is angle (integer degrees anticlockwise from 3o'clock position) to the
C  start of arc, (n,n,4) is angle (integer degrees anticlockwise from 
C  3o'clock position) to the end of the arc, 
C  (n,n,5) is colour (i.e. zone or grey colour index),
C  (n,n,6) colour scale (0=interface, 1=greyscale, 2=zone scale),
C  (n,n,7) fill index (0=no fill, 1=black fill).
      integer NIVL         ! number of internal labels in the icon.
      integer IVLBL        ! icon internal label structure where
C IVLBL(n,n,1) is vertex index
C   (n,n,2) is colour (i.e. zone or grey colour index),
C   (n,n,3) colour scale (0=interface, 1=greyscale, 2=zone scale)
      COMMON/NWKICN/NNICN,ICONTP(MICN),XYZICON(MICN,3),NICONATR(MICN),
     & ATRICN(MICN,MIATRB,3),ATRTAG(MICN,MIATRB,5),ATRMENU(MICN,MIATRB),
     & NCONP(MICN),CONCP(MICN,MCNP,2),ICNCT(MICN,MCNP),
     & VCICON(MICN,MICNV,3),IVEICN(MICN,MICNE,5),NIVC(MICN),
     & NIVE(MICN),NIVD(MICN),IVDOT(MICN,MICND,4),NIVA(MICN),
     & IVARC(MICN,MICND,7),NIVL(MICN),IVLBL(MICN,MICND,3)

C Additional strings associated with icons.
      character NWICNM*12   ! the icon name (from user),
      character NWICNHIS*40 ! icon origin tags (domain, category and name from icon db)
      character NWICNLBL*4  ! internal label text (4 character)
      character NWICNME*36  ! descriptive phrase (from icon db)
      COMMON/NWKICS/NWICNM(MICN),NWICNHIS(MICN),NWICNLBL(MICN,MICND),
     &  NWICNME(MICN)

C Variables describing the available entities in the icon database
C which are instantiated in call to scanicondb.
      integer idomain  ! the domain for each possible network type
      integer nbcat   ! number of categories for each possible network type
      character cattag*12   ! tag (must be unique) for category
      character catmenu*32  ! menu entry for the category
      character icontag*12  ! identifier of db item (must be unique)
      integer nbicons       ! number of entries in each category
      character iconmenu*40 ! descriptive phrase for db item
      common/icondbscan1/idomain(MNWKTYP),nbcat(MNWKTYP),
     &  cattag(MNWKTYP,MICNCAT),catmenu(MNWKTYP,MICNCAT),
     &  nbicons(MNWKTYP,MICNCAT),icontag(MNWKTYP,MICNCAT,MICN),
     &  iconmenu(MNWKTYP,MICNCAT,MICN)
