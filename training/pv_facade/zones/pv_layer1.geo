# geometry of pv_layer1 defined in: ../zones/pv_layer1.geo
GEN  pv_layer1              # type   zone name
      14       9   0.000    # vertices, surfaces, rotation angle 
#  X co-ord, Y co-ord, Z co-ord 
      0.00000     9.77000    12.40000  # vert   1
     62.40000     9.77000    12.40000  # vert   2
     62.40000    10.00000    12.40000  # vert   3
      0.00000    10.00000    12.40000  # vert   4
      0.00000     9.77000    16.55000  # vert   5
     62.40000     9.77000    16.55000  # vert   6
     62.40000    10.00000    16.55000  # vert   7
      0.00000    10.00000    16.55000  # vert   8
      0.00000     9.77000    13.35000  # vert   9
     62.40000     9.77000    13.35000  # vert  10
     55.01000     9.77000    16.55000  # vert  11
     55.01000     9.77000    13.35000  # vert  12
     55.01000    10.00000    12.40000  # vert  13
     55.01000    10.00000    16.55000  # vert  14
# no of vertices followed by list of associated vert
   4,  9, 12, 11,  5,
   5,  2,  3,  7,  6, 10,
   4, 13,  4,  8, 14,
   5,  4,  1,  9,  5,  8,
   6,  5, 11,  6,  7, 14,  8,
   5,  1,  4, 13,  3,  2,
   5,  1,  2, 10, 12,  9,
   4, 12, 10,  6, 11,
   4,  3, 13, 14,  7,
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
  5, Surf-5        OPAQ  CEIL  frame        pv_layer2      
  6, Surf-6        OPAQ  FLOR  frame        EXTERIOR       
  7, pv_base_fm    OPAQ  VERT  opq_glaz     EXTERIOR       
  8, pv1-glaz      TRAN  VERT  clear_glaz   EXTERIOR       
  9, pv_glaz1      TRAN  VERT  clear_glaz   factory_area   
# base
   6   0   0   0    14.35
