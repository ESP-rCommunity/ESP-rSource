# geometry of passage defined in: ../zones/passage.geo
GEN  passage  passage describes a...  # type, name, descr
      38      15   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      1.00000     4.50000     0.00000  # vert   1
      5.50000     4.50000     0.00000  # vert   2
     10.00000     4.50000     0.00000  # vert   3
     14.50000     4.50000     0.00000  # vert   4
     19.00000     4.50000     0.00000  # vert   5
     19.00000     6.00000     0.00000  # vert   6
      1.00000     6.00000     0.00000  # vert   7
      1.00000     4.50000     2.70000  # vert   8
      5.50000     4.50000     2.70000  # vert   9
     10.00000     4.50000     2.70000  # vert  10
     14.50000     4.50000     2.70000  # vert  11
     19.00000     4.50000     2.70000  # vert  12
     19.00000     6.00000     2.70000  # vert  13
      1.00000     6.00000     2.70000  # vert  14
      4.20000     4.50000     0.00000  # vert  15
      5.00000     4.50000     0.00000  # vert  16
      5.00000     4.50000     2.10000  # vert  17
      4.20000     4.50000     2.10000  # vert  18
      8.70000     4.50000     0.00000  # vert  19
      9.50000     4.50000     0.00000  # vert  20
      9.50000     4.50000     2.10000  # vert  21
      8.70000     4.50000     2.10000  # vert  22
     13.20000     4.50000     0.00000  # vert  23
     14.00000     4.50000     0.00000  # vert  24
     14.00000     4.50000     2.10000  # vert  25
     13.20000     4.50000     2.10000  # vert  26
     17.70000     4.50000     0.00000  # vert  27
     18.50000     4.50000     0.00000  # vert  28
     18.50000     4.50000     2.10000  # vert  29
     17.70000     4.50000     2.10000  # vert  30
      1.00000     5.80000     0.00000  # vert  31
      1.00000     5.00000     0.00000  # vert  32
      1.00000     5.00000     2.10000  # vert  33
      1.00000     5.80000     2.10000  # vert  34
     18.00000     6.00000     1.70000  # vert  35
     11.00000     6.00000     1.70000  # vert  36
     11.00000     6.00000     2.10000  # vert  37
     18.00000     6.00000     2.10000  # vert  38
# no of vertices followed by list of associated vert
   8,  1, 15, 18, 17, 16,  2,  9,  8,
   8,  2, 19, 22, 21, 20,  3, 10,  9,
   8,  3, 23, 26, 25, 24,  4, 11, 10,
   8,  4, 27, 30, 29, 28,  5, 12, 11,
   4,  5,  6, 13, 12,
  10,  6,  7, 14, 13,  6, 35, 38, 37, 36, 35,
   8,  7, 31, 34, 33, 32,  1,  8, 14,
   7,  8,  9, 10, 11, 12, 13, 14,
  17,  1, 32, 31,  7,  6,  5, 28, 27,  4, 24, 23,  3, 20, 19,  2, 16, 15,
   4, 15, 16, 17, 18,
   4, 19, 20, 21, 22,
   4, 23, 24, 25, 26,
   4, 27, 28, 29, 30,
   4, 31, 32, 33, 34,
   4, 35, 36, 37, 38,
# unused index
 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
# surfaces indentation (m)
 0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  construction environment
# no  name         type  posn  name         other side
  1, prt_dg        OPAQ  VERT  block_part   direct_gain    
  2, prt_rad       OPAQ  VERT  block_part   rad_test       
  3, prt_exch      OPAQ  VERT  block_part   heatexch       
  4, prt_mix       OPAQ  VERT  block_part   mixed_test     
  5, right         OPAQ  VERT  ext_wall     EXTERIOR       
  6, north         OPAQ  VERT  ext_wall     EXTERIOR       
  7, left          OPAQ  VERT  ext_wall     EXTERIOR       
  8, ceil          OPAQ  CEIL  ceiling      roof           
  9, floor         OPAQ  FLOR  passg_floor  GROUND         
 10, door_dg       OPAQ  VERT  door         direct_gain    
 11, door_rad      OPAQ  VERT  door         rad_test       
 12, door_exch     OPAQ  VERT  door         heatexch       
 13, door_mix      OPAQ  VERT  door         mixed_test     
 14, door          OPAQ  VERT  door         EXTERIOR       
 15, glaz          TRAN  VERT  dbl_glz      EXTERIOR       
# base
  9  0  0  0  0  0    27.00 0
