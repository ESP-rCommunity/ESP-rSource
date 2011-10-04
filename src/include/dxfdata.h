C This header relates to data found in DXF files. It is
C dependant on ?? and should follow ? so that
C parameters will have been defined.

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
