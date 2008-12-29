# geometry of main defined in: ../zones/main.geo
GEN  main  this the main floor zone  # type, name, descr
      32      13   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      0.00000     0.00000     0.00000  # vert   1
     10.00000     0.00000     0.00000  # vert   2
     10.00000     9.00000     0.00000  # vert   3
      5.00000     9.00000     0.00000  # vert   4
      5.00000    15.00000     0.00000  # vert   5
      0.00000    15.00000     0.00000  # vert   6
      0.00000     0.00000     2.70000  # vert   7
     10.00000     0.00000     2.70000  # vert   8
     10.00000     9.00000     2.70000  # vert   9
      5.00000     9.00000     2.70000  # vert  10
      5.00000    15.00000     2.70000  # vert  11
      0.00000    15.00000     2.70000  # vert  12
      0.25000     0.00000     0.00000  # vert  13
      2.25000     0.00000     0.00000  # vert  14
      2.25000     0.00000     2.00000  # vert  15
      0.25000     0.00000     2.00000  # vert  16
      4.00000     0.00000     0.50000  # vert  17
      8.00000     0.00000     0.50000  # vert  18
      8.00000     0.00000     2.30000  # vert  19
      4.00000     0.00000     2.30000  # vert  20
      4.50000    15.00000     0.00000  # vert  21
      3.00000    15.00000     0.00000  # vert  22
      3.00000    15.00000     2.00000  # vert  23
      4.50000    15.00000     2.00000  # vert  24
      5.00000     9.50000     0.00000  # vert  25
      5.00000    10.50000     0.00000  # vert  26
      5.00000    10.50000     2.00000  # vert  27
      5.00000     9.50000     2.00000  # vert  28
      1.50000    15.00000     0.50000  # vert  29
      0.30000    15.00000     0.50000  # vert  30
      0.30000    15.00000     2.00000  # vert  31
      1.50000    15.00000     2.00000  # vert  32
# no of vertices followed by list of associated vert
  14,  1, 13, 16, 15, 14,  2,  8,  7,  1, 17, 20, 19, 18, 17,
   4,  2,  3,  9,  8,
   4,  3,  4, 10,  9,
   8,  4, 25, 28, 27, 26,  5, 11, 10,
  14,  5, 21, 24, 23, 22,  6, 12, 11,  5, 29, 32, 31, 30, 29,
   4,  6,  1,  7, 12,
   6,  7,  8,  9, 10, 11, 12,
  12,  1,  6, 22, 21,  5, 26, 25,  4,  3,  2, 14, 13,
   4, 13, 14, 15, 16,
   4, 17, 18, 19, 20,
   4, 21, 22, 23, 24,
   4, 25, 26, 27, 28,
   4, 29, 30, 31, 32,
# unused index
  0 0 0 0 0 0 0 0 0 0 0 0 0
# surfaces indentation (m)
 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  mlc db       environment
# no  name         type  posn  name         other side
  1, Southwall     OPAQ  VERT  extern_wall  EXTERIOR       
  2, eastwall      OPAQ  VERT  extern_wall  EXTERIOR       
  3, garge-s       OPAQ  VERT  internl_wal  garage         
  4, garge-w       OPAQ  VERT  internl_wal  garage         
  5, northwall     OPAQ  VERT  extern_wall  EXTERIOR       
  6, westwall      OPAQ  VERT  extern_wall  EXTERIOR       
  7, ceiling       OPAQ  CEIL  ceiling      attic          
  8, floor         OPAQ  FLOR  slabONgrade  GROUND         
  9, win-patio     TRAN  VERT  d_glz        EXTERIOR       
 10, win-mbr       TRAN  VERT  d_glz        EXTERIOR       
 11, ext-door      OPAQ  VERT  ext-door     EXTERIOR       
 12, garage-idor   OPAQ  VERT  int_doors    garage         
 13, win-bedr      TRAN  VERT  d_glz        EXTERIOR       
# base
  8  0  0  0  0  0   120.00
