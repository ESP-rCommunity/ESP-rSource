# composite construction db defined in ../dbs/CFC.db2
# based on materials db ../dbs/material.db3.a
    9     # no of composites 
# layers  description   optics name   symmetry tag
    4    extern_wall   OPAQ  OPAQUE        inv_ext_wall  
# mat ref thickness (m) mat descr & air gap R
    6    0.1000  lt brown brick : Light brown brick
  211    0.0750  glasswool : Glasswool (generic)
    0    0.0500  air  0.170 0.170 0.170
    2    0.1000  breeze block : Breeze block
# layers  description   optics name   symmetry tag
    5    shd_clr_clr   CFC   USE GSLedit   NONSYMMETRIC  
# mat ref thickness (m) mat descr & air gap R
  251    0.0150  shade : CFC shading layer dispGlz_not_modCond)
    0    0.0500  air  0.170 0.170 0.170
  242    0.0060  plate glass : Plate glass with placeholder single layer optics
    0    0.0127  air  0.170 0.170 0.170
  242    0.0060  plate glass : Plate glass with placeholder single layer optics
# layers  description   optics name   symmetry tag
    5    clr_shd_clr   CFC   USE GSLedit   SYMMETRIC     
# mat ref thickness (m) mat descr & air gap R
  242    0.0060  plate glass : Plate glass with placeholder single layer optics
    0    0.0200  air  0.170 0.170 0.170
  251    0.0150  shade : CFC shading layer dispGlz_not_modCond)
    0    0.0200  air  0.170 0.170 0.170
  242    0.0060  plate glass : Plate glass with placeholder single layer optics
# layers  description   optics name   symmetry tag
    5    clr_clr_shd   CFC   USE GSLedit   NONSYMMETRIC  
# mat ref thickness (m) mat descr & air gap R
  242    0.0060  plate glass : Plate glass with placeholder single layer optics
    0    0.0127  air  0.170 0.170 0.170
  242    0.0060  plate glass : Plate glass with placeholder single layer optics
    0    0.0500  air  0.170 0.170 0.170
  251    0.0150  shade : CFC shading layer dispGlz_not_modCond)
# layers  description   optics name   symmetry tag
    3    dbl_cfc       CFC   USE GSLedit   SYMMETRIC     
# mat ref thickness (m) mat descr & air gap R
  242    0.0060  plate glass : Plate glass with placeholder single layer optics
    0    0.0127  air  0.170 0.170 0.170
  242    0.0060  plate glass : Plate glass with placeholder single layer optics
# layers  description   optics name   symmetry tag
    7    trip_btp_shd  CFC   USE GSLedit   NONSYMMETRIC  
# mat ref thickness (m) mat descr & air gap R
  242    0.0060  plate glass : Plate glass with placeholder single layer optics
    0    0.0200  air  0.170 0.170 0.170
  251    0.0150  shade : CFC shading layer dispGlz_not_modCond)
    0    0.0200  air  0.170 0.170 0.170
  242    0.0060  plate glass : Plate glass with placeholder single layer optics
    0    0.0127  air  0.170 0.170 0.170
  242    0.0060  plate glass : Plate glass with placeholder single layer optics
# layers  description   optics name   symmetry tag
    7    trip_in_shd   CFC   USE GSLedit   NONSYMMETRIC  
# mat ref thickness (m) mat descr & air gap R
  242    0.0060  plate glass : Plate glass with placeholder single layer optics
    0    0.0127  air  0.170 0.170 0.170
  242    0.0060  plate glass : Plate glass with placeholder single layer optics
    0    0.0127  air  0.170 0.170 0.170
  242    0.0060  plate glass : Plate glass with placeholder single layer optics
    0    0.0050  air  0.170 0.170 0.170
  251    0.0150  shade : CFC shading layer dispGlz_not_modCond)
# layers  description   optics name   symmetry tag
    7    trip_out_shd  CFC   USE GSLedit   NONSYMMETRIC  
# mat ref thickness (m) mat descr & air gap R
  251    0.0150  shade : CFC shading layer dispGlz_not_modCond)
    0    0.0500  air  0.170 0.170 0.170
  242    0.0060  plate glass : Plate glass with placeholder single layer optics
    0    0.0127  air  0.170 0.170 0.170
  242    0.0060  plate glass : Plate glass with placeholder single layer optics
    0    0.0127  air  0.170 0.170 0.170
  242    0.0060  plate glass : Plate glass with placeholder single layer optics
# layers  description   optics name   symmetry tag
    1    partition     OPAQ  OPAQUE        SYMMETRIC     
# mat ref thickness (m) mat descr & air gap R
    6    0.1000  lt brown brick : Light brown brick
