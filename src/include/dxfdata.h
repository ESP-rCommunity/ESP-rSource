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
