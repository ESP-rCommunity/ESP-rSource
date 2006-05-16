# geometry of gym defined in: ../zone/gym.geo
GEN  gym                    # type   zone name
      28      12   0.000    # vertices, surfaces, rotation angle 
#  X co-ord, Y co-ord, Z co-ord 
      0.00000    40.00000     0.00000  # vert  1
     10.00000    40.00000     0.00000  # vert  2
     10.00000    60.00000     0.00000  # vert  3
      0.00000    60.00000     0.00000  # vert  4
      0.00000    40.00000     6.00000  # vert  5
     10.00000    40.00000    10.00000  # vert  6
     10.00000    60.00000    10.00000  # vert  7
      0.00000    60.00000     6.00000  # vert  8
      2.00000    60.00000     3.00000  # vert  9
     10.00000    60.00000     3.00000  # vert 10
     10.00000    40.00000     3.00000  # vert 11
      2.00000    40.00000     3.00000  # vert 12
      0.00000    58.00000     0.00000  # vert 13
      0.00000    56.00000     0.00000  # vert 14
      0.00000    56.00000     5.00000  # vert 15
      0.00000    58.00000     5.00000  # vert 16
      0.00000    52.00000     0.00000  # vert 17
      0.00000    50.00000     0.00000  # vert 18
      0.00000    50.00000     5.00000  # vert 19
      0.00000    52.00000     5.00000  # vert 20
      0.00000    46.00000     0.00000  # vert 21
      0.00000    44.00000     0.00000  # vert 22
      0.00000    44.00000     5.00000  # vert 23
      0.00000    46.00000     5.00000  # vert 24
     10.00000    41.00000     4.00000  # vert 25
     10.00000    59.00000     4.00000  # vert 26
     10.00000    59.00000     5.80000  # vert 27
     10.00000    41.00000     5.80000  # vert 28
# no of vertices followed by list of associated vert
   4,  1,  2,  6,  5,
  10,  2,  3,  7,  6,  2, 25, 28, 27, 26, 25,
   4,  3,  4,  8,  7,
  16,  4, 13, 16, 15, 14, 17, 20, 19, 18, 21, 24, 23, 22,  1,  5,  8,
   4,  5,  6,  7,  8,
  10,  4,  3,  2,  1, 22, 21, 18, 17, 14, 13,
   4, 10,  9, 12, 11,
   4, 11, 12,  9, 10,
   4, 13, 14, 15, 16,
   4, 17, 18, 19, 20,
   4, 21, 22, 23, 24,
   4, 25, 26, 27, 28,
# number of default windows within each surface 
   0  0  0  0  0  0  0  0  0  0  0  0
# surfaces indentation (m)
 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000
    1   6   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  mlc db       environment
# no  name         type  posn  name         other side
  1, Surf-1        OPAQ  VERT  intern_wall  changing       
  2, Surf-2        OPAQ  VERT  intern_wall  main-hall      
  3, Surf-3        OPAQ  VERT  extern_wall  EXTERIOR       
  4, Surf-4        OPAQ  VERT  extern_wall  EXTERIOR       
  5, Surf-5        OPAQ  CEIL  roof         EXTERIOR       
  6, Surf-6        OPAQ  FLOR  oth_ground   GROUND         
  7, xSurf-6       OPAQ  CEIL  susp_floor   ADIABATIC      
  8, Surf-8        OPAQ  FLOR  susp_flr_re  ADIABATIC      
  9, glaz-1        TRAN  VERT  d_glz        EXTERIOR       
 10, glaz-2        TRAN  VERT  d_glz        EXTERIOR       
 11, glaz-3        TRAN  VERT  d_glz        EXTERIOR       
 12, glaz_hall     TRAN  VERT  d_glz        main-hall      
