# geometry of factory_area defined in: ../zones/factory_area.geo
GEN  factory_area           # type   zone name
      27      17   0.000    # vertices, surfaces, rotation angle 
#  X co-ord, Y co-ord, Z co-ord 
      0.00000    10.00000     0.00000  # vert   1
     62.40000    10.00000     0.00000  # vert   2
     62.40000    30.00000     0.00000  # vert   3
      0.00000    30.00000     0.00000  # vert   4
      0.00000    10.00000    25.05000  # vert   5
     62.40000    10.00000    25.05000  # vert   6
     62.40000    30.00000    25.05000  # vert   7
      0.00000    30.00000    25.05000  # vert   8
     62.40000    10.00000    12.40000  # vert   9
      0.00000    10.00000    12.40000  # vert  10
      0.00000    10.00000    16.55000  # vert  11
     62.40000    10.00000    16.55000  # vert  12
      0.00000    10.00000    19.77000  # vert  13
     62.40000    10.00000    19.77000  # vert  14
     62.40000    10.00000    22.99000  # vert  15
      0.00000    10.00000    22.99000  # vert  16
     55.01000    10.00000    25.05000  # vert  17
     55.01000    10.00000    22.99000  # vert  18
     55.01000    10.00000    19.77000  # vert  19
     55.01000    10.00000    16.55000  # vert  20
     55.01000    10.00000    12.40000  # vert  21
     41.38000    10.00000     0.00000  # vert  22
      0.00000    10.00000     3.00000  # vert  23
     41.38000    10.00000     3.00000  # vert  24
      0.00000    10.00000     7.70000  # vert  25
     41.38000    10.00000     7.70000  # vert  26
     41.38000    10.00000    12.40000  # vert  27
# no of vertices followed by list of associated vert
   7, 22,  2,  9, 21, 27, 26, 24,
   8,  2,  3,  7,  6, 15, 14, 12,  9,
   4,  3,  4,  8,  7,
  10,  4,  1, 23, 25, 10, 11, 13, 16,  5,  8,
   5,  5, 17,  6,  7,  8,
   5,  1,  4,  3,  2, 22,
   5, 10, 27, 21, 20, 11,
   4, 11, 20, 19, 13,
   4, 13, 19, 18, 16,
   4, 16, 18, 17,  5,
   4, 21,  9, 12, 20,
   4, 20, 12, 14, 19,
   4, 19, 14, 15, 18,
   4, 18, 15,  6, 17,
   4,  1, 22, 24, 23,
   4, 23, 24, 26, 25,
   4, 25, 26, 27, 10,
# number of default windows within each surface 
   0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
# surfaces indentation (m)
 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  mlc db       environment
# no  name         type  posn  name         other side
  1, Surf-1        OPAQ  VERT  wall_clad    EXTERIOR       
  2, Surf-2        OPAQ  VERT  wall_clad    EXTERIOR       
  3, Surf-3        OPAQ  VERT  wall_clad    EXTERIOR       
  4, Surf-4        OPAQ  VERT  wall_clad    EXTERIOR       
  5, Surf-5        OPAQ  CEIL  roof_clad    EXTERIOR       
  6, Surf-6        OPAQ  FLOR  Floor_slab   GROUND         
  7, pv_back1      OPAQ  VERT  pv_back_wall pv_layer1      
  8, pv_back2      OPAQ  VERT  pv_back_wall pv_layer2      
  9, pv_back3      OPAQ  VERT  pv_back_wall pv_layer3      
 10, pv_back4      OPAQ  VERT  pv_back_wall pv_layer4      
 11, pv_glaz1      TRAN  VERT  clear_glaz   pv_layer1      
 12, pv_glaz2      TRAN  VERT  clear_glaz   pv_layer2      
 13, pv_glaz3      TRAN  VERT  clear_glaz   pv_layer3      
 14, pv_glaz4      TRAN  VERT  clear_glaz   pv_layer4      
 15, off1-back     OPAQ  VERT  wall_clad    office_fl1     
 16, off2-back     OPAQ  VERT  wall_clad    office_fl2     
 17, off3-back     OPAQ  VERT  wall_clad    office_fl3     
# base
   6   0   0   0  1248.00
