# geometry of loft defined in: ../zones/loft-av_2010pv.geo
GEN  loft  roof space  # type, name, descr
      31      15   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      0.00000     0.00000     4.80000  # vert   1
      5.52000     0.00000     4.80000  # vert   2
      5.52000     8.24000     4.80000  # vert   3
      0.00000     8.24000     4.80000  # vert   4
      0.00000     4.12000     8.38000  # vert   5
      5.52000     4.12000     8.38000  # vert   6
      2.60000     0.00000     4.80000  # vert   7
      5.52000     3.20000     4.80000  # vert   8
      2.12000     3.20000     4.80000  # vert   9
      2.12000     1.40000     4.80000  # vert  10
      2.60000     1.40000     4.80000  # vert  11
      0.00000     3.20000     4.80000  # vert  12
      5.52000     4.70000     4.80000  # vert  13
      4.52000     4.70000     4.80000  # vert  14
      3.92000     4.20000     4.80000  # vert  15
      2.12000     4.20000     4.80000  # vert  16
      2.12000     8.24000     4.80000  # vert  17
      2.12000     5.64000     4.80000  # vert  18
      0.00000     5.64000     4.80000  # vert  19
     -1.00000     5.64000     4.80000  # vert  20
     -1.00000     3.20000     4.80000  # vert  21
      0.00000     3.20000     5.20000  # vert  22
      0.00000     5.64000     5.20000  # vert  23
      0.28500     0.17814     4.95479  # vert  24
      5.23500     0.17814     4.95479  # vert  25
      5.23500     2.68724     7.13502  # vert  26
      0.28500     2.68724     7.13502  # vert  27
      0.76000     2.86840     7.29243  # vert  28
      4.76000     2.86841     7.29243  # vert  29
      4.76000     3.62325     7.94834  # vert  30
      0.76000     3.62324     7.94834  # vert  31
# no of vertices followed by list of associated vert
  17,  1,  7,  2,  6, 29, 28, 31, 30, 29,  6,  5,  1, 24, 27, 26, 25, 24,
   5,  2,  8, 13,  3,  6,
   7,  4, 19, 23, 22, 12,  1,  5,
   5,  5,  6,  3, 17,  4,
   6,  2,  7, 11, 10,  9,  8,
   6,  7,  1, 12,  9, 10, 11,
   6,  8,  9, 16, 15, 14, 13,
   7, 15, 16, 18, 17,  3, 13, 14,
   4, 18, 19,  4, 17,
   7, 20, 19, 18, 16,  9, 12, 21,
   3, 21, 12, 22,
   3, 19, 20, 23,
   4, 20, 21, 22, 23,
   4, 24, 25, 26, 27,
   4, 28, 29, 30, 31,
# unused index
 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
# surfaces indentation (m)
 0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  construction environment
# no  name         type  posn  name         other side
  1, roof_s        OPAQ  SLOP  slate_rf     EXTERIOR       
  2, loft_w_e      OPAQ  VERT  Wall_EW_2002 EXTERIOR       
  3, loft_w_w      OPAQ  VERT  Wall_EW_2002 EXTERIOR       
  4, roof_n        OPAQ  SLOP  slate_rf     EXTERIOR       
  5, br2_ceil      OPAQ  FLOR  attic_300mm  ANOTHER        
  6, br3_ceil      OPAQ  FLOR  attic_300mm  ANOTHER        
  7, ensuite_ceil  OPAQ  FLOR  attic_300mm  ANOTHER        
  8, br1_ceiling   OPAQ  FLOR  attic_300mm  ANOTHER        
  9, up_bath_ceil  OPAQ  FLOR  attic_300mm  ANOTHER        
 10, nliv_roof     OPAQ  FLOR  attic_300mm  ANOTHER        
 11, stair_a       OPAQ  VERT  Wall_EW_2002 EXTERIOR       
 12, stair_c       OPAQ  VERT  Wall_EW_2002 EXTERIOR       
 13, stair_roof    OPAQ  SLOP  slate_rf     EXTERIOR       
 14, PV_array      TRAN  SLOP  slate_rf_pv  EXTERIOR       
 15, solar_therma  OPAQ  SLOP  slate_rf     EXTERIOR       
# base
  5  6  7  8  9 10    47.92 2
