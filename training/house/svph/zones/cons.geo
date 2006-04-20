# geometry of conservatory defined in: ../zones/cons.geo
GEN  conservatory  conservatory describes a...  # type, name, descr
      29      13   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
     11.40400     5.25100     0.00000  # vert   1
      9.80500     4.82300     0.00000  # vert   2
      9.80500     4.82300     2.10000  # vert   3
      6.32800     3.89100     2.10000  # vert   4
      6.32800     3.89100     0.00000  # vert   5
      6.09100     3.82800     0.00000  # vert   6
      6.09100     3.82800     0.40000  # vert   7
      6.09100     3.82800     2.70000  # vert   8
     10.63100     5.04400     2.70000  # vert   9
     11.40400     5.25100     2.70000  # vert  10
     11.92200     3.31900     0.00000  # vert  11
      7.38200     2.10300     0.00000  # vert  12
      6.60900     1.89600     0.00000  # vert  13
      6.60900     1.89600     0.40000  # vert  14
      6.60900     1.89600     2.00000  # vert  15
      7.38200     2.10300     2.00000  # vert  16
     11.14900     3.11200     2.00000  # vert  17
     11.92200     3.31900     2.00000  # vert  18
     11.92200     3.31900     0.40000  # vert  19
     11.40400     5.25100     0.40000  # vert  20
     11.40400     5.25100     2.40000  # vert  21
      8.07000     4.35800     2.40000  # vert  22
      8.07000     4.35800     2.70000  # vert  23
      6.09100     3.82800     2.40000  # vert  24
     11.12456     3.20317     2.03303  # vert  25
      7.38200     2.10300     0.20000  # vert  26
     11.92200     3.31900     0.20000  # vert  27
      6.77775     2.03890     2.03303  # vert  28
      6.28419     3.87974     2.70000  # vert  29
# no of vertices followed by list of associated vert
  11,  1,  2,  3,  4,  5,  6,  7, 24, 22, 21, 20,
   4,  2,  5,  4,  3,
   5,  7, 14, 15,  8, 24,
   6, 26, 27, 19, 18, 17, 16,
   5, 10, 18, 19, 20, 21,
   5, 28, 25,  9, 23, 29,
   4,  6, 13, 14,  7,
   8, 13, 12, 11, 27, 26, 16, 15, 14,
   5, 11,  1, 20, 19, 27,
  10, 17, 18, 10,  9, 25, 28, 29,  8, 15, 16,
   7,  1, 11, 12, 13,  6,  5,  2,
   5, 21, 22, 23,  9, 10,
   5, 22, 24,  8, 29, 23,
# unused index
 0,0,0,0,0,0,0,0,0,0,0,0,0
# surfaces indentation (m)
 0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  construction environment
# no  name         type  posn  name         other side
  1, liv_con       OPAQ  VERT  ext_wall_rev living_room    
  2, glazing       TRAN  VERT  double_glaz  living_room    
  3, w_glaz        TRAN  VERT  single_glaz  EXTERIOR       
  4, s_glaz        TRAN  VERT  single_glaz  EXTERIOR       
  5, e_glaz        TRAN  VERT  single_glaz  EXTERIOR       
  6, r_glazing     TRAN  SLOP  single_glaz  EXTERIOR       
  7, w_frame       OPAQ  VERT  cons_frame   EXTERIOR       
  8, s_frame       OPAQ  VERT  cons_frame   EXTERIOR       
  9, e_frame       OPAQ  VERT  cons_frame   EXTERIOR       
 10, r_frame       OPAQ  SLOP  cons_frame   EXTERIOR       
 11, floor         OPAQ  FLOR  cons_floor   GROUND         
 12, b2_con        OPAQ  VERT  ext_wall_rev bedroom_2      
 13, b4_con        OPAQ  VERT  ext_wall_rev bedroom_4      
# base
  0  0  0  0  0  0    11.00 1
