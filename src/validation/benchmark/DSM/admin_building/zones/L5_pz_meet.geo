# geometry of L5_pz_meet defined in: ../zones/L5_pz_meet.geo
GEN  L5_pz_meet  L5_pz_meet is the L5 print zone plus meeting room adj stair2  # type, name, descr
      22      13   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
     46.25000    36.00000    64.15000  # vert   1
     46.25000    32.75000    64.15000  # vert   2
     46.25000    30.50000    64.15000  # vert   3
     46.25000    28.50000    64.15000  # vert   4
     46.25000    22.50000    64.15000  # vert   5
     51.00000    22.50000    64.15000  # vert   6
     51.00000    28.50000    64.15000  # vert   7
     51.00000    30.50000    64.15000  # vert   8
     50.00000    30.50000    64.15000  # vert   9
     50.00000    36.00000    64.15000  # vert  10
     48.75000    36.00000    64.15000  # vert  11
     46.25000    36.00000    67.37500  # vert  12
     46.25000    32.75000    67.37500  # vert  13
     46.25000    30.50000    67.37500  # vert  14
     46.25000    28.50000    67.37500  # vert  15
     46.25000    22.50000    67.37500  # vert  16
     51.00000    22.50000    67.37500  # vert  17
     51.00000    28.50000    67.37500  # vert  18
     51.00000    30.50000    67.37500  # vert  19
     50.00000    30.50000    67.37500  # vert  20
     50.00000    36.00000    67.37500  # vert  21
     48.75000    36.00000    67.37500  # vert  22
# no of vertices followed by list of associated vert
   4,  1,  2, 13, 12,
   4,  2,  3, 14, 13,
   4,  3,  4, 15, 14,
   4,  4,  5, 16, 15,
   4,  5,  6, 17, 16,
   4,  6,  7, 18, 17,
   4,  7,  8, 19, 18,
   4,  8,  9, 20, 19,
   4,  9, 10, 21, 20,
   4, 10, 11, 22, 21,
   4, 11,  1, 12, 22,
  11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22,
  11,  1, 11, 10,  9,  8,  7,  6,  5,  4,  3,  2,
# unused index
 0,0,0,0,0,0,0,0,0,0,0,0,0
# surfaces indentation (m)
 0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  construction environment
# no  name         type  posn  name         other side
  1, Wall-1        OPAQ  VERT  UNKNOWN      UNKNOWN        
  2, Wall-2        OPAQ  VERT  UNKNOWN      UNKNOWN        
  3, Wall-3        OPAQ  VERT  UNKNOWN      UNKNOWN        
  4, Wall-4        OPAQ  VERT  UNKNOWN      UNKNOWN        
  5, Wall-5        OPAQ  VERT  UNKNOWN      UNKNOWN        
  6, Wall-6        OPAQ  VERT  UNKNOWN      UNKNOWN        
  7, Wall-7        OPAQ  VERT  UNKNOWN      UNKNOWN        
  8, Wall-8        OPAQ  VERT  UNKNOWN      UNKNOWN        
  9, Wall-9        OPAQ  VERT  UNKNOWN      UNKNOWN        
 10, Wall-10       OPAQ  VERT  UNKNOWN      UNKNOWN        
 11, Wall-11       OPAQ  VERT  UNKNOWN      UNKNOWN        
 12, Top-12        OPAQ  CEIL  UNKNOWN      UNKNOWN        
 13, Base-13       OPAQ  FLOR  UNKNOWN      UNKNOWN        
# base
 13  0  0  0  0  0    58.63 0
