# geometry of entrance defined in: ../zone/entrance.geo
GEN  entrance               # type   zone name
      32      14   0.000    # vertices, surfaces, rotation angle 
#  X co-ord, Y co-ord, Z co-ord 
      0.00000     0.00000     0.00000  # vert  1
     20.00000     0.00000     0.00000  # vert  2
     20.00000    10.00000     0.00000  # vert  3
     10.00000    10.00000     0.00000  # vert  4
     10.00000    20.00000     0.00000  # vert  5
      0.00000    20.00000     0.00000  # vert  6
      0.00000     0.00000     3.00000  # vert  7
     20.00000     0.00000     3.00000  # vert  8
     20.00000    10.00000     5.00000  # vert  9
     10.00000    10.00000     5.00000  # vert 10
     10.00000    20.00000     5.00000  # vert 11
      0.00000    20.00000     3.00000  # vert 12
      0.00000    19.00000     0.00000  # vert 13
      0.00000    18.00000     0.00000  # vert 14
      0.00000    18.00000     2.00000  # vert 15
      0.00000    19.00000     2.00000  # vert 16
      0.00000    13.00000     0.00000  # vert 17
      0.00000    12.00000     0.00000  # vert 18
      0.00000    12.00000     2.00000  # vert 19
      0.00000    13.00000     2.00000  # vert 20
      0.00000     8.00000     0.00000  # vert 21
      0.00000     7.00000     0.00000  # vert 22
      0.00000     7.00000     2.00000  # vert 23
      0.00000     8.00000     2.00000  # vert 24
      7.00000     0.00000     0.00000  # vert 25
      8.00000     0.00000     0.00000  # vert 26
      8.00000     0.00000     2.50000  # vert 27
      7.00000     0.00000     2.50000  # vert 28
     12.00000     0.00000     0.00000  # vert 29
     13.00000     0.00000     0.00000  # vert 30
     13.00000     0.00000     2.50000  # vert 31
     12.00000     0.00000     2.50000  # vert 32
# no of vertices followed by list of associated vert
  12,  1, 25, 28, 27, 26, 29, 32, 31, 30,  2,  8,  7,
   4,  2,  3,  9,  8,
   4,  3,  4, 10,  9,
   4,  4,  5, 11, 10,
   4,  5,  6, 12, 11,
  16,  6, 13, 16, 15, 14, 17, 20, 19, 18, 21, 24, 23, 22,  1,  7, 12,
   4,  7,  8,  9, 10,
  16,  6,  5,  4,  3,  2, 30, 29, 26, 25,  1, 22, 21, 18, 17, 14, 13,
   4, 12,  7, 10, 11,
   4, 13, 14, 15, 16,
   4, 17, 18, 19, 20,
   4, 21, 22, 23, 24,
   4, 25, 26, 27, 28,
   4, 29, 30, 31, 32,
# number of default windows within each surface 
   0  0  0  0  0  0  0  0  0  0  0  0  0  0
# surfaces indentation (m)
 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000
    1   8   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  mlc db       environment
# no  name         type  posn  name         other side
  1, Surf-1        OPAQ  VERT  extern_wall  EXTERIOR       
  2, Surf-2        OPAQ  VERT  extern_wall  EXTERIOR       
  3, Surf-3        OPAQ  VERT  intern_wall  main-hall      
  4, Surf-4        OPAQ  VERT  intern_wall  main-hall      
  5, Surf-5        OPAQ  VERT  intern_wall  changing       
  6, Surf-6        OPAQ  VERT  extern_wall  EXTERIOR       
  7, Surf-7        OPAQ  CEIL  roof         EXTERIOR       
  8, Surf-8        OPAQ  FLOR  g_flr_entry  GROUND         
  9, Surf-9        OPAQ  UNKN  roof         EXTERIOR       
 10, glaz-1        TRAN  VERT  d_glz        EXTERIOR       
 11, glaz-2        TRAN  VERT  d_glz        EXTERIOR       
 12, glaz-3        TRAN  VERT  d_glz        EXTERIOR       
 13, glaz-4        TRAN  VERT  d_glz        EXTERIOR       
 14, glaz-5        TRAN  VERT  d_glz        EXTERIOR       
