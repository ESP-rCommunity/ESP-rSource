# geometry of pv_layer3 defined in: ../zones/pv_layer3.geo
GEN  pv_layer3              # type   zone name
      15       9   0.000    # vertices, surfaces, rotation angle 
#  X co-ord, Y co-ord, Z co-ord 
      0.00000     9.77000    19.77000  # vert   1
     62.40000     9.77000    19.77000  # vert   2
     62.40000    10.00000    19.77000  # vert   3
      0.00000    10.00000    19.77000  # vert   4
      0.00000     9.77000    22.99000  # vert   5
     62.40000     9.77000    22.99000  # vert   6
     62.40000    10.00000    22.99000  # vert   7
      0.00000    10.00000    22.99000  # vert   8
      0.00000     9.77000    20.72000  # vert   9
     62.40000     9.77000    20.72000  # vert  10
     55.01000     9.77000    22.99000  # vert  11
     55.01000     9.77000    20.72000  # vert  12
     55.01000     9.77000    19.77000  # vert  13
     55.01000    10.00000    19.77000  # vert  14
     55.01000    10.00000    22.99000  # vert  15
# no of vertices followed by list of associated vert
   4,  9, 12, 11,  5,
   5,  2,  3,  7,  6, 10,
   4, 14,  4,  8, 15,
   5,  4,  1,  9,  5,  8,
   6,  5, 11,  6,  7, 15,  8,
   6,  1,  4, 14,  3,  2, 13,
   4,  1, 13, 12,  9,
   6, 12, 13,  2, 10,  6, 11,
   4,  3, 14, 15,  7,
# number of default windows within each surface 
   0  0  0  0  0  0  0  0  0
# surfaces indentation (m)
 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  mlc db       environment
# no  name         type  posn  name         other side
  1, Surf-1        TRAN  VERT  a-Si_PV      EXTERIOR       
  2, Surf-2        OPAQ  VERT  frame        EXTERIOR       
  3, Surf-3        OPAQ  VERT  pv_back_wall factory_area   
  4, Surf-4        OPAQ  VERT  frame        EXTERIOR       
  5, Surf-5        OPAQ  CEIL  frame        pv_layer4      
  6, Surf-6        OPAQ  FLOR  frame        pv_layer2      
  7, pv_base_fm    OPAQ  VERT  frame        EXTERIOR       
  8, pv1-glaz      TRAN  VERT  clear_glaz   EXTERIOR       
  9, pv_glaz3      TRAN  VERT  clear_glaz   factory_area   
# base
   6   0   0   0    14.35
