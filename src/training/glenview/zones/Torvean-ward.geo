# geometry of Torvean-war defined in: ../zones/Torvean-ward.geo
GEN  Torvean-war  Torvean-war describes a...  # type, name, descr
      20      15   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
     11.82506     8.46459     0.00000  # vert   1
      8.89993     5.73629     0.00000  # vert   2
      8.89993     5.73629     2.60000  # vert   3
     10.36249     7.10044     3.21146  # vert   4
     11.82506     8.46459     2.60000  # vert   5
      8.08144     6.61383     0.00000  # vert   6
     11.00657     9.34213     0.00000  # vert   7
     11.00657     9.34213     2.60000  # vert   8
      8.08144     6.61383     2.60000  # vert   9
      6.18011     4.84044     0.00000  # vert  10
     -2.68686    14.34711    -0.00001  # vert  11
     20.30000    18.01000     0.00000  # vert  12
      6.18000     4.84000     2.60000  # vert  13
     -2.68700    14.34700     2.60000  # vert  14
     20.30000    18.01000     2.60000  # vert  15
      1.74659     9.59341     6.60000  # vert  16
      8.86635     8.70452     3.21146  # vert  17
     20.30000    35.78723     0.00000  # vert  18
     20.30000    35.78700     2.60000  # vert  19
     20.30000    26.89854     6.60000  # vert  20
# no of vertices followed by list of associated vert
   4, 11, 10, 13, 14,
   4, 12, 18, 19, 15,
   4, 18, 11, 14, 19,
   4, 10,  6,  9, 13,
   4,  6,  2,  3,  9,
   8, 10, 11, 18, 12,  7,  1,  2,  6,
   5,  2,  1,  5,  4,  3,
   4,  7, 12, 15,  8,
   4,  5,  8, 17,  4,
   4,  9,  3,  4, 17,
   4,  1,  7,  8,  5,
   7, 13,  9, 17,  8, 15, 20, 16,
   4, 19, 14, 16, 20,
   3, 14, 13, 16,
   3, 15, 19, 20,
# unused index
  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
# surfaces indentation (m)
 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  mlc db       environment
# no  name         type  posn  name         other side
  1, Wall-v-s2     OPAQ  VERT  Ward-wall    EXTERIOR       
  2, Wall-v-n3     OPAQ  VERT  Ward-wall    Drumoss-war    
  3, Wall-v-n4     OPAQ  VERT  Ward-wall    EXTERIOR       
  4, Wall-v-s3     OPAQ  VERT  Ward-wall    EXTERIOR       
  5, Wall-v-s4     OPAQ  VERT  Ward-wall    EXTERIOR       
  6, Floor         OPAQ  FLOR  Ward-wall    EXTERIOR       
  7, Wall-v-s5     OPAQ  VERT  Ward-wall    West_1         
  8, Wall-v-s6     OPAQ  VERT  Ward-wall    EXTERIOR       
  9, Roof-n1       OPAQ  SLOP  Ward-roof    EXTERIOR       
 10, Roof-s1       OPAQ  SLOP  Ward-roof    EXTERIOR       
 11, Wall-v-n1     OPAQ  VERT  Ward-wall    EXTERIOR       
 12, Roof-s2       OPAQ  SLOP  Ward-roof    EXTERIOR       
 13, Roof-n2       OPAQ  SLOP  Ward-roof    EXTERIOR       
 14, Wall-v-s1     OPAQ  VERT  Ward-wall    EXTERIOR       
 15, Wall-v-n2     OPAQ  VERT  Ward-wall    Drumoss-war    
# base
  6  0  0  0  0  0   334.62
