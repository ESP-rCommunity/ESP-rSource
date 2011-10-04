C This header relates to data found in DXF files. It is
C loosely dependant on building.h.

C Maximum number of layers from DXF. Each zone in esp-r might require
C up to three layers (opaque transparent obstructions) so should be updated
C to reflect MCOM in building.h.
      parameter (MAXLAY = 80)

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

