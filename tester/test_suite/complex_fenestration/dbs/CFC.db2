# composite construction db defined in ../dbs/CFC.db2
# based on materials db ../dbs/CFCmaterial.db3.a# and based on CFClayers db /usr/esru/esp-r/databases/CFClayers.db1.a
   33     # no of composites 
# layers  description  type  optics name   symmetry tag
    4    extern_wall   OPAQ  OPAQUE        inv_ext_wall  
# material ref thickness (m) description & air gap R
    6    0.1000  lt brown brick : Light brown brick
  211    0.0750  glasswool : Glasswool (generic)
    0    0.0500  gap  0.170 0.170 0.170
    2    0.1000  breeze block : Breeze block
# layers  description  type  optics name   symmetry tag
    5    shd_clr_clr   CFC   N/A           NONSYMMETRIC  
# material ref thickness (m) description & air gap R
  251    0.0150  shade : CFC shading layer dispGlz_not_modCond)
    0    0.0500  gap  0.170 0.170 0.170
  242    0.0060  plate glass : Plate glass with placeholder single layer optics
    0    0.0127  gap  0.170 0.170 0.170
  242    0.0060  plate glass : Plate glass with placeholder single layer optics
# layers  description  type  optics name   symmetry tag
    5    clr_shd_clr   CFC   N/A           SYMMETRIC     
# material ref thickness (m) description & air gap R
  242    0.0060  plate glass : Plate glass with placeholder single layer optics
    0    0.0200  gap  0.170 0.170 0.170
  251    0.0150  shade : CFC shading layer dispGlz_not_modCond)
    0    0.0200  gap  0.170 0.170 0.170
  242    0.0060  plate glass : Plate glass with placeholder single layer optics
# layers  description  type  optics name   symmetry tag
    5    clr_clr_shd   CFC   N/A           NONSYMMETRIC  
# material ref thickness (m) description & air gap R
  242    0.0060  plate glass : Plate glass with placeholder single layer optics
    0    0.0127  gap  0.170 0.170 0.170
  242    0.0060  plate glass : Plate glass with placeholder single layer optics
    0    0.0500  gap  0.170 0.170 0.170
  251    0.0150  shade : CFC shading layer dispGlz_not_modCond)
# layers  description  type  optics name   symmetry tag
    3    dbl_cfc       CFC   N/A           SYMMETRIC     
# material ref thickness (m) description & air gap R
  242    0.0060  plate glass : Plate glass with placeholder single layer optics
    0    0.0127  gap  0.170 0.170 0.170
  242    0.0060  plate glass : Plate glass with placeholder single layer optics
# layers  description  type  optics name   symmetry tag
    7    trip_btp_shd  CFC   N/A           NONSYMMETRIC  
# material ref thickness (m) description & air gap R
  242    0.0060  plate glass : Plate glass with placeholder single layer optics
    0    0.0200  gap  0.170 0.170 0.170
  251    0.0150  shade : CFC shading layer dispGlz_not_modCond)
    0    0.0200  gap  0.170 0.170 0.170
  242    0.0060  plate glass : Plate glass with placeholder single layer optics
    0    0.0127  gap  0.170 0.170 0.170
  242    0.0060  plate glass : Plate glass with placeholder single layer optics
# layers  description  type  optics name   symmetry tag
    7    trip_in_shd   CFC   N/A           NONSYMMETRIC  
# material ref thickness (m) description & air gap R
  242    0.0060  plate glass : Plate glass with placeholder single layer optics
    0    0.0127  gap  0.170 0.170 0.170
  242    0.0060  plate glass : Plate glass with placeholder single layer optics
    0    0.0127  gap  0.170 0.170 0.170
  242    0.0060  plate glass : Plate glass with placeholder single layer optics
    0    0.0050  gap  0.170 0.170 0.170
  251    0.0150  shade : CFC shading layer dispGlz_not_modCond)
# layers  description  type  optics name   symmetry tag
    7    trip_out_shd  CFC   N/A           NONSYMMETRIC  
# material ref thickness (m) description & air gap R
  251    0.0150  shade : CFC shading layer dispGlz_not_modCond)
    0    0.0500  gap  0.170 0.170 0.170
  242    0.0060  plate glass : Plate glass with placeholder single layer optics
    0    0.0127  gap  0.170 0.170 0.170
  242    0.0060  plate glass : Plate glass with placeholder single layer optics
    0    0.0127  gap  0.170 0.170 0.170
  242    0.0060  plate glass : Plate glass with placeholder single layer optics
# layers  description  type  optics name   symmetry tag
    1    partition     OPAQ  OPAQUE        SYMMETRIC     
# material ref thickness (m) description & air gap R
    6    0.1000  lt brown brick : Light brown brick
# layers  description  type  optics name   symmetry tag
    5    glz_glz_VBD   CFC2  N/A           SYMMETRIC     
# material ref thickness (m) description & air gap R
 1524    0.0057  CLEAR_6.DAT : 103(IGDB v11.4), Generic Clear Glass
   35    0.0120  95% argon : CFC gap, 95% argon fill gas
 1524    0.0057  CLEAR_6.DAT : 103(IGDB v11.4), Generic Clear Glass
   31    0.0200  air : CFC gap, no fill gas
    1    0.0150  VB 1/2in light Al : VB 1/2in wide light Al
# layers  description  type  optics name   symmetry tag
    5    glz_glz_VBD2  CFC2  N/A           NONSYMMETRIC  
# material ref thickness (m) description & air gap R
 1524    0.0057  CLEAR_6.DAT : 103(IGDB v11.4), Generic Clear Glass
   35    0.0120  95% argon : CFC gap, 95% argon fill gas
 1524    0.0057  CLEAR_6.DAT : 103(IGDB v11.4), Generic Clear Glass
   31    0.0200  air : CFC gap, no fill gas
    3    0.0150  VB 1/2in dark Al : VB 1/2in wide dark Al
# layers  description  type  optics name   symmetry tag
    5    glz_VBD_glz   CFC2  N/A           NONSYMMETRIC  
# material ref thickness (m) description & air gap R
 1524    0.0057  CLEAR_6.DAT : 103(IGDB v11.4), Generic Clear Glass
   35    0.0120  95% argon : CFC gap, 95% argon fill gas
    3    0.0150  VB 1/2in dark Al : VB 1/2in wide dark Al
   35    0.0120  95% argon : CFC gap, 95% argon fill gas
 1524    0.0057  CLEAR_6.DAT : 103(IGDB v11.4), Generic Clear Glass
# layers  description  type  optics name   symmetry tag
    5    glz_VBD2_glz  CFC2  N/A           SYMMETRIC     
# material ref thickness (m) description & air gap R
 1524    0.0057  CLEAR_6.DAT : 103(IGDB v11.4), Generic Clear Glass
   35    0.0120  95% argon : CFC gap, 95% argon fill gas
    7    0.0150  VB 1in dark Al : VB 1in wide dark Al
   35    0.0120  95% argon : CFC gap, 95% argon fill gas
 1524    0.0057  CLEAR_6.DAT : 103(IGDB v11.4), Generic Clear Glass
# layers  description  type  optics name   symmetry tag
    5    VBD_glz_glz   CFC2  N/A           SYMMETRIC     
# material ref thickness (m) description & air gap R
    3    0.0150  VB 1/2in dark Al : VB 1/2in wide dark Al
   31    0.0250  air : CFC gap, no fill gas
 1708    0.0060  Clear_23.GRD : 3012(IGDB v19.0), 2.3mm Clear Float Glass
   33    0.0100  krypton : CFC gap, krypton fill gas
 1524    0.0057  CLEAR_6.DAT : 103(IGDB v11.4), Generic Clear Glass
# layers  description  type  optics name   symmetry tag
    5    VBD2_glz_glz  CFC2  N/A           NONSYMMETRIC  
# material ref thickness (m) description & air gap R
    2    0.0150  VB 1/2in med Al : VB 1/2in wide medium Al
   31    0.0250  air : CFC gap, no fill gas
 1708    0.0060  Clear_23.GRD : 3012(IGDB v19.0), 2.3mm Clear Float Glass
   33    0.0100  krypton : CFC gap, krypton fill gas
 1524    0.0057  CLEAR_6.DAT : 103(IGDB v11.4), Generic Clear Glass
# layers  description  type  optics name   symmetry tag
    5    glz_glz_DRP   CFC2  N/A           NONSYMMETRIC  
# material ref thickness (m) description & air gap R
 1524    0.0057  CLEAR_6.DAT : 103(IGDB v11.4), Generic Clear Glass
   34    0.0080  xenon : CFC gap, xenon fill gas
 1524    0.0057  CLEAR_6.DAT : 103(IGDB v11.4), Generic Clear Glass
   31    0.0200  air : CFC gap, no fill gas
   11    0.0150  DRP closed dark : Pleated drape, closed fabric, dark
# layers  description  type  optics name   symmetry tag
    5    glz_glz_DRP2  CFC2  N/A           NONSYMMETRIC  
# material ref thickness (m) description & air gap R
 1524    0.0057  CLEAR_6.DAT : 103(IGDB v11.4), Generic Clear Glass
   34    0.0080  xenon : CFC gap, xenon fill gas
 1524    0.0057  CLEAR_6.DAT : 103(IGDB v11.4), Generic Clear Glass
   31    0.0200  air : CFC gap, no fill gas
   18    0.0150  DRP shear crm : Pleated drape, shear fabric, cream
# layers  description  type  optics name   symmetry tag
    5    glz_DRP_glz   CFC2  N/A           SYMMETRIC     
# material ref thickness (m) description & air gap R
 1524    0.0057  CLEAR_6.DAT : 103(IGDB v11.4), Generic Clear Glass
   35    0.0120  95% argon : CFC gap, 95% argon fill gas
   14    0.0150  DRP closed med : Pleated drape, closed fabric, medium
   35    0.0120  95% argon : CFC gap, 95% argon fill gas
 1524    0.0057  CLEAR_6.DAT : 103(IGDB v11.4), Generic Clear Glass
# layers  description  type  optics name   symmetry tag
    5    glz_DRP2_glz  CFC2  N/A           SYMMETRIC     
# material ref thickness (m) description & air gap R
 1524    0.0057  CLEAR_6.DAT : 103(IGDB v11.4), Generic Clear Glass
   35    0.0120  95% argon : CFC gap, 95% argon fill gas
   16    0.0150  DRP semi-open lit : Pleated drape, semi-open fabric, light
   35    0.0120  95% argon : CFC gap, 95% argon fill gas
 1524    0.0057  CLEAR_6.DAT : 103(IGDB v11.4), Generic Clear Glass
# layers  description  type  optics name   symmetry tag
    5    DRP_glz_glz   CFC2  N/A           NONSYMMETRIC  
# material ref thickness (m) description & air gap R
   17    0.0150  DRP closed lit : Pleated drape, closed fabric, light
   31    0.0250  air : CFC gap, no fill gas
 1717    0.0060  Green_23.GRD : 3022(IGDB v18.1), 2.3mm Green Float Glass
   33    0.0100  krypton : CFC gap, krypton fill gas
 1524    0.0057  CLEAR_6.DAT : 103(IGDB v11.4), Generic Clear Glass
# layers  description  type  optics name   symmetry tag
    5    DRP2_glz_glz  CFC2  N/A           NONSYMMETRIC  
# material ref thickness (m) description & air gap R
   14    0.0150  DRP closed med : Pleated drape, closed fabric, medium
   31    0.0250  air : CFC gap, no fill gas
 1717    0.0060  Green_23.GRD : 3022(IGDB v18.1), 2.3mm Green Float Glass
   33    0.0100  krypton : CFC gap, krypton fill gas
 1524    0.0057  CLEAR_6.DAT : 103(IGDB v11.4), Generic Clear Glass
# layers  description  type  optics name   symmetry tag
    5    glz_glz_RLD   CFC2  N/A           NONSYMMETRIC  
# material ref thickness (m) description & air gap R
 1524    0.0057  CLEAR_6.DAT : 103(IGDB v11.4), Generic Clear Glass
   34    0.0080  xenon : CFC gap, xenon fill gas
 1524    0.0057  CLEAR_6.DAT : 103(IGDB v11.4), Generic Clear Glass
   31    0.0200  air : CFC gap, no fill gas
   19    0.0150  RLD black : Roller blind, black, 14% open
# layers  description  type  optics name   symmetry tag
    5    glz_glz_RLD2  CFC2  N/A           NONSYMMETRIC  
# material ref thickness (m) description & air gap R
 1524    0.0057  CLEAR_6.DAT : 103(IGDB v11.4), Generic Clear Glass
   34    0.0080  xenon : CFC gap, xenon fill gas
 1524    0.0057  CLEAR_6.DAT : 103(IGDB v11.4), Generic Clear Glass
   31    0.0200  air : CFC gap, no fill gas
   20    0.0150  RLD chalk : Roller blind, chalk, 5% open
# layers  description  type  optics name   symmetry tag
    5    glz_RLD_glz   CFC2  N/A           SYMMETRIC     
# material ref thickness (m) description & air gap R
 1524    0.0057  CLEAR_6.DAT : 103(IGDB v11.4), Generic Clear Glass
   35    0.0120  95% argon : CFC gap, 95% argon fill gas
   19    0.0150  RLD black : Roller blind, black, 14% open
   35    0.0120  95% argon : CFC gap, 95% argon fill gas
 1524    0.0057  CLEAR_6.DAT : 103(IGDB v11.4), Generic Clear Glass
# layers  description  type  optics name   symmetry tag
    5    glz_RLD2_glz  CFC2  N/A           SYMMETRIC     
# material ref thickness (m) description & air gap R
 1524    0.0057  CLEAR_6.DAT : 103(IGDB v11.4), Generic Clear Glass
   35    0.0120  95% argon : CFC gap, 95% argon fill gas
   24    0.0150  RLD diffuse : Roller blind, diffuse
   35    0.0120  95% argon : CFC gap, 95% argon fill gas
 1524    0.0057  CLEAR_6.DAT : 103(IGDB v11.4), Generic Clear Glass
# layers  description  type  optics name   symmetry tag
    5    RLD_glz_glz   CFC2  N/A           NONSYMMETRIC  
# material ref thickness (m) description & air gap R
   23    0.0150  RLD white : Roller blind, white, 14% open
   31    0.0250  air : CFC gap, no fill gas
 1717    0.0060  Green_23.GRD : 3022(IGDB v18.1), 2.3mm Green Float Glass
   33    0.0100  krypton : CFC gap, krypton fill gas
 1524    0.0057  CLEAR_6.DAT : 103(IGDB v11.4), Generic Clear Glass
# layers  description  type  optics name   symmetry tag
    5    RLD2_glz_glz  CFC2  N/A           NONSYMMETRIC  
# material ref thickness (m) description & air gap R
   22    0.0150  RLD sunblock : Roller blind, sunblock, opaque
   31    0.0250  air : CFC gap, no fill gas
 1717    0.0060  Green_23.GRD : 3022(IGDB v18.1), 2.3mm Green Float Glass
   33    0.0100  krypton : CFC gap, krypton fill gas
 1524    0.0057  CLEAR_6.DAT : 103(IGDB v11.4), Generic Clear Glass
# layers  description  type  optics name   symmetry tag
    5    glz_glz_BUG   CFC2  N/A           NONSYMMETRIC  
# material ref thickness (m) description & air gap R
 1524    0.0057  CLEAR_6.DAT : 103(IGDB v11.4), Generic Clear Glass
   34    0.0080  xenon : CFC gap, xenon fill gas
 1524    0.0057  CLEAR_6.DAT : 103(IGDB v11.4), Generic Clear Glass
   31    0.0200  air : CFC gap, no fill gas
   25    0.0150  BUG 150 mesh ss : Insect screen, 150 mesh, stainless steel
# layers  description  type  optics name   symmetry tag
    5    glz_glz_BUG2  CFC2  N/A           NONSYMMETRIC  
# material ref thickness (m) description & air gap R
 1524    0.0057  CLEAR_6.DAT : 103(IGDB v11.4), Generic Clear Glass
   34    0.0080  xenon : CFC gap, xenon fill gas
 1524    0.0057  CLEAR_6.DAT : 103(IGDB v11.4), Generic Clear Glass
   31    0.0200  air : CFC gap, no fill gas
   27    0.0150  BUG 20 mesh ss : Insect screen, 20 mesh, stainless steel
# layers  description  type  optics name   symmetry tag
    5    glz_BUG_glz   CFC2  N/A           SYMMETRIC     
# material ref thickness (m) description & air gap R
 1524    0.0057  CLEAR_6.DAT : 103(IGDB v11.4), Generic Clear Glass
   31    0.0150  air : CFC gap, no fill gas
   30    0.0150  BUG charcoal : Insect screen, 26 mesh, charcoal
   31    0.0150  air : CFC gap, no fill gas
 1524    0.0057  CLEAR_6.DAT : 103(IGDB v11.4), Generic Clear Glass
# layers  description  type  optics name   symmetry tag
    5    glz_BUG2_glz  CFC2  N/A           SYMMETRIC     
# material ref thickness (m) description & air gap R
 1524    0.0057  CLEAR_6.DAT : 103(IGDB v11.4), Generic Clear Glass
   31    0.0150  air : CFC gap, no fill gas
   27    0.0150  BUG 20 mesh ss : Insect screen, 20 mesh, stainless steel
   31    0.0150  air : CFC gap, no fill gas
 1524    0.0057  CLEAR_6.DAT : 103(IGDB v11.4), Generic Clear Glass
# layers  description  type  optics name   symmetry tag
    5    BUG_glz_glz   CFC2  N/A           NONSYMMETRIC  
# material ref thickness (m) description & air gap R
   30    0.0150  BUG charcoal : Insect screen, 26 mesh, charcoal
   31    0.0250  air : CFC gap, no fill gas
 1717    0.0060  Green_23.GRD : 3022(IGDB v18.1), 2.3mm Green Float Glass
   33    0.0100  krypton : CFC gap, krypton fill gas
 1524    0.0057  CLEAR_6.DAT : 103(IGDB v11.4), Generic Clear Glass
# layers  description  type  optics name   symmetry tag
    5    BUG2_glz_glz  CFC2  N/A           NONSYMMETRIC  
# material ref thickness (m) description & air gap R
   26    0.0150  BUG 120 mesh ss : Insect screen, 120 mesh, stainless steel
   31    0.0250  air : CFC gap, no fill gas
 1717    0.0060  Green_23.GRD : 3022(IGDB v18.1), 2.3mm Green Float Glass
   33    0.0100  krypton : CFC gap, krypton fill gas
 1524    0.0057  CLEAR_6.DAT : 103(IGDB v11.4), Generic Clear Glass
