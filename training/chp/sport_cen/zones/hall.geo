# geometry of main-hall defined in: ../zone/hall.geo
GEN  main-hall              # type   zone name
      43      20   0.000    # vertices, surfaces, rotation angle 
#  X co-ord, Y co-ord, Z co-ord 
     10.00000    10.00000     0.00000  # vert  1
     40.00000    10.00000     0.00000  # vert  2
     40.00000    60.00000     0.00000  # vert  3
     10.00000    60.00000     0.00000  # vert  4
     10.00000    10.00000    10.00000  # vert  5
     40.00000    10.00000    10.00000  # vert  6
     40.00000    60.00000    10.00000  # vert  7
     10.00000    60.00000    10.00000  # vert  8
     20.00000    20.00000    15.00000  # vert  9
     30.00000    20.00000    15.00000  # vert 10
     20.00000    50.00000    15.00000  # vert 11
     30.00000    50.00000    15.00000  # vert 12
     10.00000    20.00000     0.00000  # vert 13
     10.00000    20.00000     5.00000  # vert 14
     10.00000    10.00000     5.00000  # vert 15
     20.00000    10.00000     0.00000  # vert 16
     20.00000    10.00000     5.00000  # vert 17
     10.00000    40.00000     0.00000  # vert 18
     10.00000    40.00000    10.00000  # vert 19
     10.00000    20.00000    10.00000  # vert 20
     25.00000    60.00000     0.00000  # vert 21
     25.00000    60.00000     5.00000  # vert 22
     10.00000    60.00000     5.00000  # vert 23
     40.00000    13.00000     0.00000  # vert 24
     40.00000    16.00000     0.00000  # vert 25
     40.00000    16.00000     8.00000  # vert 26
     40.00000    13.00000     8.00000  # vert 27
     40.00000    31.00000     0.00000  # vert 28
     40.00000    34.00000     0.00000  # vert 29
     40.00000    34.00000     8.00000  # vert 30
     40.00000    31.00000     8.00000  # vert 31
     40.00000    49.00000     0.00000  # vert 32
     40.00000    52.00000     0.00000  # vert 33
     40.00000    52.00000     8.00000  # vert 34
     40.00000    49.00000     8.00000  # vert 35
     10.00000    21.00000     4.00000  # vert 36
     10.00000    39.00000     4.00000  # vert 37
     10.00000    39.00000     5.80000  # vert 38
     10.00000    21.00000     5.80000  # vert 39
     10.00000    41.00000     4.00000  # vert 40
     10.00000    59.00000     4.00000  # vert 41
     10.00000    59.00000     5.80000  # vert 42
     10.00000    41.00000     5.80000  # vert 43
# no of vertices followed by list of associated vert
   6, 16,  2,  6,  5, 15, 17,
  16,  2, 24, 27, 26, 25, 28, 31, 30, 29, 32, 35, 34, 33,  3,  7,  6,
   6,  3, 21, 22, 23,  8,  7,
   4, 14, 15,  5, 20,
   6,  5,  9, 11,  8, 19, 20,
  14,  4, 21,  3, 33, 32, 29, 28, 25, 24,  2, 16,  1, 13, 18,
   4,  5,  6, 10,  9,
   4,  6,  7, 12, 10,
   4,  7,  8, 11, 12,
   4, 11,  9, 10, 12,
   4, 13,  1, 15, 14,
   4,  1, 16, 17, 15,
  11, 18, 37, 38, 39, 36, 37, 18, 13, 14, 20, 19,
  11,  4, 41, 42, 43, 40, 41,  4, 18, 19,  8, 23,
   4, 21,  4, 23, 22,
   4, 24, 25, 26, 27,
   4, 28, 29, 30, 31,
   4, 32, 33, 34, 35,
   4, 37, 36, 39, 38,
   4, 41, 40, 43, 42,
# number of default windows within each surface 
   0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
# surfaces indentation (m)
 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000
 0.000 0.000
    1   6   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  mlc db       environment
# no  name         type  posn  name         other side
  1, Surf-1        OPAQ  VERT  extern_wall  EXTERIOR       
  2, Surf-2        OPAQ  VERT  extern_wall  EXTERIOR       
  3, Surf-3        OPAQ  VERT  extern_wall  EXTERIOR       
  4, Surf-4        OPAQ  VERT  extern_wall  EXTERIOR       
  5, Surf-5        OPAQ  CEIL  roof         EXTERIOR       
  6, Surf-6        OPAQ  FLOR  hall_floor   EXTERIOR       
  7, Surf-7        OPAQ  UNKN  roof         GROUND         
  8, Surf-8        OPAQ  UNKN  roof         EXTERIOR       
  9, Surf-9        OPAQ  UNKN  roof         EXTERIOR       
 10, Surf-10       OPAQ  CEIL  roof         EXTERIOR       
 11, xSurf-4       OPAQ  VERT  intern_wall  entrance       
 12, xSurf-3       OPAQ  VERT  intern_wall  entrance       
 13, change-r      OPAQ  VERT  intern_wall  changing       
 14, gym-r         OPAQ  VERT  intern_wall  gym            
 15, plant-r       OPAQ  VERT  intern_wall  plant          
 16, glaz-1        TRAN  VERT  d_glz        EXTERIOR       
 17, glaz-2        TRAN  VERT  d_glz        EXTERIOR       
 18, glaz-3        TRAN  VERT  d_glz        EXTERIOR       
 19, xglaz_hall2   TRAN  VERT  d_glz        changing       
 20, xglaz_hall    TRAN  VERT  d_glz        gym            
