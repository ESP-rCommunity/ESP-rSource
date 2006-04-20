# geometry of loft defined in: ../zones/loft.geo
GEN  loft  loft describes a...  # type, name, descr
      19      10   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      9.77300    11.33700     4.80000  # vert   1
      7.45500    10.71600     4.80000  # vert   2
      4.75700     9.99300     4.80000  # vert   3
      1.36900     9.08500     4.80000  # vert   4
      1.92800     7.00000     4.80000  # vert   5
      3.00000     3.00000     4.80000  # vert   6
      5.60800     3.69900     4.80000  # vert   7
      8.06900     4.35800     4.80000  # vert   8
     11.40400     5.25200     4.80000  # vert   9
     10.58800     8.29400     4.80000  # vert  10
      8.27000     7.67300     4.80000  # vert  11
      7.25400     7.40100     4.80000  # vert  12
      5.31600     7.90800     4.80000  # vert  13
      5.01900     7.82800     4.80000  # vert  14
      5.27600     6.87100     4.80000  # vert  15
      5.83200     4.79400     4.80000  # vert  16
      5.34900     4.66500     4.80000  # vert  17
     10.58800     8.29400     7.26500  # vert  18
      2.18500     6.04300     7.26500  # vert  19
# no of vertices followed by list of associated vert
   6,  1,  2,  3,  4, 19, 18,
   4,  4,  5,  6, 19,
   6,  6,  7,  8,  9, 18, 19,
   4,  9, 10,  1, 18,
   4, 10, 11,  2,  1,
   7, 11, 12, 15, 14, 13,  3,  2,
   5, 13, 14,  5,  4,  3,
   7, 14, 15, 16, 17,  7,  6,  5,
   6,  8,  7, 17, 16, 15, 12,
   5,  9,  8, 12, 11, 10,
# unused index
 0,0,0,0,0,0,0,0,0,0
# surfaces indentation (m)
 0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  construction environment
# no  name         type  posn  name         other side
  1, north         OPAQ  SLOP  roof         EXTERIOR       
  2, west          OPAQ  VERT  ext_wall     EXTERIOR       
  3, south         OPAQ  SLOP  roof         EXTERIOR       
  4, east          OPAQ  VERT  ext_wall     EXTERIOR       
  5, ceil_b3       OPAQ  FLOR  1st_ceil_rev bedroom_3      
  6, ceil_hall     OPAQ  FLOR  1st_ceil_rev Upper_hall     
  7, ceil_bath     OPAQ  FLOR  1st_ceil_rev bathroom       
  8, ceil_b1       OPAQ  FLOR  1st_ceil_rev bedroom_1      
  9, ceil_b4       OPAQ  FLOR  1st_ceil_rev bedroom_4      
 10, ceil_b2       OPAQ  FLOR  1st_ceil_rev bedroom_2      
# base
  0  0  0  0  0  0    54.81 1
