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

C Grid X-length, Y-length, Z-length (m)
        INTEGER, PARAMETER :: MGRXL=30,MGRYL=30,MGRZL=30

C Grid X-view, Y-view, Z-view extents (m)
        INTEGER, PARAMETER :: MGRXE=10,MGRYE=10,MGRZE=10

C Icon width and height
        REAL, PARAMETER :: XSIZIC=0.75,YSIZIC=0.75

C Icon vertices, edges, dots/labels
        INTEGER, PARAMETER :: MICNV=30,MICNE=30,MICND=10

C Connection point size
        REAL, PARAMETER :: XYSIZCP=0.1

C Icon label offset 
        REAL, PARAMETER :: OFFLAB=0.65

C Maximum number of network types and icon domains.
        INTEGER, PARAMETER :: MNWKTYP=6

C Maximum number of categories per icon domain and icons per category.
        INTEGER, PARAMETER :: MICNCAT=24,MICNICAT=24

