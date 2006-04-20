# geometry of changing defined in: ../zone/changing.geo
GEN  changing               # type   zone name
      30      13   0.000    # vertices, surfaces, rotation angle 
#  X co-ord, Y co-ord, Z co-ord 
      0.00000    20.00000     0.00000  # vert  1
     10.00000    20.00000     0.00000  # vert  2
     10.00000    40.00000     0.00000  # vert  3
      0.00000    40.00000     0.00000  # vert  4
      0.00000    20.00000     6.00000  # vert  5
     10.00000    20.00000    10.00000  # vert  6
     10.00000    40.00000    10.00000  # vert  7
      0.00000    40.00000     6.00000  # vert  8
      0.00000    20.00000     3.00000  # vert  9
     10.00000    20.00000     5.00000  # vert 10
      2.00000    40.00000     3.00000  # vert 11
     10.00000    40.00000     3.00000  # vert 12
     10.00000    20.00000     3.00000  # vert 13
      2.00000    20.00000     3.00000  # vert 14
      0.00000    38.00000     0.00000  # vert 15
      0.00000    36.00000     0.00000  # vert 16
      0.00000    36.00000     5.00000  # vert 17
      0.00000    38.00000     5.00000  # vert 18
      0.00000    32.00000     0.00000  # vert 19
      0.00000    30.00000     0.00000  # vert 20
      0.00000    30.00000     5.00000  # vert 21
      0.00000    32.00000     5.00000  # vert 22
      0.00000    26.00000     0.00000  # vert 23
      0.00000    24.00000     0.00000  # vert 24
      0.00000    24.00000     5.00000  # vert 25
      0.00000    26.00000     5.00000  # vert 26
     10.00000    21.00000     4.00000  # vert 27
     10.00000    39.00000     4.00000  # vert 28
     10.00000    39.00000     5.80000  # vert 29
     10.00000    21.00000     5.80000  # vert 30
# no of vertices followed by list of associated vert
   4,  9, 10,  6,  5,
  11,  2,  3,  7,  6, 10,  2, 27, 30, 29, 28, 27,
   4,  3,  4,  8,  7,
  17,  4, 15, 18, 17, 16, 19, 22, 21, 20, 23, 26, 25, 24,  1,  9,  5,  8,
   4,  5,  6,  7,  8,
  10,  4,  3,  2,  1, 24, 23, 20, 19, 16, 15,
   4,  1,  2, 10,  9,
   4, 14, 13, 12, 11,
   4, 11, 12, 13, 14,
   4, 15, 16, 17, 18,
   4, 19, 20, 21, 22,
   4, 23, 24, 25, 26,
   4, 27, 28, 29, 30,
# number of default windows within each surface 
   0  0  0  0  0  0  0  0  0  0  0  0  0
# surfaces indentation (m)
 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000
    1   6   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  mlc db       environment
# no  name         type  posn  name         other side
  1, Surf-1        OPAQ  VERT  intern_wall  EXTERIOR       
  2, Surf-2        OPAQ  VERT  intern_wall  main-hall      
  3, Surf-3        OPAQ  VERT  intern_wall  gym            
  4, Surf-4        OPAQ  VERT  extern_wall  EXTERIOR       
  5, Surf-5        OPAQ  CEIL  roof         EXTERIOR       
  6, Surf-6        OPAQ  FLOR  oth_ground   GROUND         
  7, recep-r       OPAQ  VERT  intern_wall  entrance       
  8, xSurf-6       OPAQ  CEIL  susp_floor   ADIABATIC      
  9, Surf-9        OPAQ  FLOR  susp_flr_re  ADIABATIC      
 10, glaz-1        TRAN  VERT  d_glz        EXTERIOR       
 11, glaz-2        TRAN  VERT  d_glz        EXTERIOR       
 12, glaz-3        TRAN  VERT  d_glz        EXTERIOR       
 13, glaz_hall2    TRAN  VERT  d_glz        main-hall      
