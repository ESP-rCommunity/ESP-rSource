# geometry of L3_toilet defined in: ../zones/L3_toilet.geo
GEN  L3_toilet  L3_toilet describes toilet on level three at srv road  # type, name, descr
      16      10   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      0.00000    35.70000    56.35000  # vert   1
      8.35979    35.70000    56.35000  # vert   2
     12.00000    35.70000    56.35000  # vert   3
     12.00000    40.70000    56.35000  # vert   4
      7.83730    40.70000    56.35000  # vert   5
      3.56241    40.70000    56.35000  # vert   6
      0.00000    40.70000    56.35000  # vert   7
      0.00000    36.28000    56.35000  # vert   8
      0.00000    35.70000    59.49300  # vert   9
      8.35979    35.70000    59.49300  # vert  10
     12.00000    35.70000    59.49300  # vert  11
     12.00000    40.70000    59.49300  # vert  12
      7.83730    40.70000    59.49300  # vert  13
      3.56241    40.70000    59.49300  # vert  14
      0.00000    40.70000    59.49300  # vert  15
      0.00000    36.28000    59.49300  # vert  16
# no of vertices followed by list of associated vert
   4,  1,  2, 10,  9,
   4,  2,  3, 11, 10,
   4,  3,  4, 12, 11,
   4,  4,  5, 13, 12,
   4,  5,  6, 14, 13,
   4,  6,  7, 15, 14,
   4,  7,  8, 16, 15,
   4,  8,  1,  9, 16,
   8,  9, 10, 11, 12, 13, 14, 15, 16,
   8,  1,  8,  7,  6,  5,  4,  3,  2,
# unused index
 0,0,0,0,0,0,0,0,0,0
# surfaces indentation (m)
 0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  construction environment
# no  name         type  posn  name         other side
  1, ptn_cora_l3   OPAQ  VERT  UNKNOWN      ANOTHER        
  2, ptn_corb_l3   OPAQ  VERT  UNKNOWN      ANOTHER        
  3, ptn_corc_l3   OPAQ  VERT  UNKNOWN      ANOTHER        
  4, fac_a_l3      OPAQ  VERT  UNKNOWN      EXTERIOR       
  5, fac_b_l3      OPAQ  VERT  UNKNOWN      EXTERIOR       
  6, fac_c_l3      OPAQ  VERT  UNKNOWN      EXTERIOR       
  7, fac_d_l3      OPAQ  VERT  UNKNOWN      EXTERIOR       
  8, ptn_vd_l3     OPAQ  VERT  UNKNOWN      ANOTHER        
  9, ceiling       OPAQ  CEIL  UNKNOWN      UNKNOWN        
 10, floor         OPAQ  FLOR  UNKNOWN      UNKNOWN        
# base
 10  0  0  0  0  0    60.00 0
