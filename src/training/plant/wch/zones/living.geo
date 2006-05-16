# geometry of living defined in: ../zones/living.geo
GEN  living  living describes a...  # type, name, descr
      18       9   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      0.00000     2.71000     0.00000  # vert   1
      2.56000     2.71000     0.00000  # vert   2
      5.40000     2.71000     0.00000  # vert   3
      5.40000     8.67000     0.00000  # vert   4
      0.00000     8.67000     0.00000  # vert   5
      0.00000     2.71000     2.70000  # vert   6
      2.56000     2.71000     2.70000  # vert   7
      5.40000     2.71000     2.70000  # vert   8
      5.40000     8.67000     2.70000  # vert   9
      0.00000     8.67000     2.70000  # vert  10
      4.40000     8.67000     0.38000  # vert  11
      3.87000     8.67000     0.38000  # vert  12
      3.87000     8.67000     2.00000  # vert  13
      4.40000     8.67000     2.00000  # vert  14
      2.90000     8.67000     0.55000  # vert  15
      0.75000     8.67000     0.55000  # vert  16
      0.75000     8.67000     2.00000  # vert  17
      2.90000     8.67000     2.00000  # vert  18
# no of vertices followed by list of associated vert
   4,  1,  2,  7,  6,
   4,  2,  3,  8,  7,
   4,  3,  4,  9,  8,
  16,  4,  5, 16, 15, 18, 17, 16,  5, 10,  9,  4, 11, 14, 13, 12, 11,
   4,  5,  1,  6, 10,
   5,  6,  7,  8,  9, 10,
   5,  1,  5,  4,  3,  2,
   4, 11, 12, 13, 14,
   4, 15, 16, 17, 18,
# unused index
 0,0,0,0,0,0,0,0,0
# surfaces indentation (m)
 0.00,0.00,0.00,1.0000,0.00,0.00,0.00,0.00,0.00
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  construction environment
# no  name         type  posn  name         other side
  1, ptn_hall      OPAQ  VERT  Inner_wall   CONSTANT       
  2, ptn_kitch     OPAQ  VERT  Inner_wall   CONSTANT       
  3, east_neigh    OPAQ  VERT  Partition    SIMILAR        
  4, north         OPAQ  VERT  Facade       EXTERIOR       
  5, west_neigh    OPAQ  VERT  Partition    SIMILAR        
  6, ceiling       OPAQ  CEIL  Inner_floor  CONSTANT       
  7, floor         OPAQ  FLOR  Ground_floor GROUND         
  8, north_gla     TRAN  VERT  double_glz   EXTERIOR       
  9, north_glb     TRAN  VERT  double_glz   EXTERIOR       
# base
  7  0  0  0  0  0    32.18 0
