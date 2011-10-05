C This header relates to data found in DXF files. It is
C loosely dependant on building.h.

C Maximum number of layers from DXF. Each zone in esp-r might require
C up to three layers (opaque transparent obstructions) so should be updated
C to reflect MCOM in building.h.
      parameter (MAXLAY = 240)

C Transforms to be applied.
      real xscale,yscale,zscale  ! scaling to apply to each entity axis
      real rotangl    ! entity rotation angle ?
      integer lineno  ! line number in the dxf file
      integer minseg  ! minimum number of segments
      real entelev,entthick  ! entity elevation and thickness
      real dfltelev,dfltthick ! default entity elevation and thickness
      common/trn/xscale,yscale,zscale,rotangl,lineno,minseg,
     &  entelev,entthick,dfltelev,dfltthick

C Descriptors of most recently scanned gcode entry.
      integer gcode ! code that identifies data type
      integer innum ! typically used to signal colour
      character instring*72
      real realin   ! a real number to be processed
      real coorx,coory,coorz  ! current coordinate
      real blkxorg,blkyorg,blkzorg  ! origin of block entity
      common/rd/gcode,instring,innum,realin,coorx,coory,coorz,
     &           blkxorg,blkyorg,blkzorg

C Flags used to identify 3D entities.
      real flag3d      ! zero is 2D one is 3D
      real dxfconv     ! supports dimension conversion
      integer flagins  ! sets state of entity currently unused
      common/fg/flag3d,flagins,dxfconv

C Current and prior layers and colours.
      integer currlay,oldlay ! current and past index of layer
      integer currcol,oldcol ! current and past colour number
      common/attrs/currlay,currcol,oldlay,oldcol

C Indicator is layer has been used.
      integer layuse  ! zero if not touched nonzero if referenced
      common/laytoo/layuse(MAXLAY)

C Layer name and colour.
      character dxflayname*72  ! as extracted from dxf file
      integer numlay      ! number of layers found
      integer laycolour   ! index of layer colour
      integer layflag     ! zero or the value if gcode is 70
      common/layers/dxflayname(MAXLAY), numlay, laycolour(MAXLAY),
     &       layflag(MAXLAY)

C String arrays and counters for block objects.
      character chartobs*124  ! string buffer for obstructions
      character chartblk*124  ! string buffer for blocks
      common/dxfc/chartobs(500),chartblk(500)
      integer ichartobs       ! how many obstructions found
      integer ichartblk       ! how many block lines found
      common/dxfci/ichartobs,ichartblk 

C Arrays for clickon points and lines.
      integer nbdxfcoords,nbdxflines  ! total number of coordinates and lines
      real dxfx,dxfy,dxfz   ! x y z of a dxf coordinate
      integer idxfedge      ! array of start and end index of 2 coords of a line
      integer idxfedgelay,idxfedgecol  ! index of layer and colour
      character*12 dxfedgetype
      common/dxfclick/nbdxfcoords,nbdxflines,dxfx(5000),dxfy(5000),
     &       dxfz(5000),idxfedge(5000,2),idxfedgelay(5000),
     &       idxfedgecol(5000)

C Bounds of the dxf data.
      real dxfminx,dxfminy,dxfminz,dxfmaxx,dxfmaxy,dxfmaxz
      common/dxfbounds/dxfminx,dxfminy,dxfminz,dxfmaxx,dxfmaxy,dxfmaxz
