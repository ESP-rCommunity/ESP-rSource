# geometry of L4_toil_psg defined in: ../zones/L4_toil_psg.geo
GEN  L4_toil_psg  L4_toil_psg describes toilet and passage on level four  # type, name, descr
      22      12   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
     51.60000    23.17941    60.25000  # vert   1
     58.30000    23.08441    60.25000  # vert   2
     58.30000    28.59427    60.25000  # vert   3
     58.30000    29.50000    60.25000  # vert   4
     58.30000    30.10000    60.25000  # vert   5
     58.30000    30.50000    60.25000  # vert   6
     56.04857    30.50000    60.25000  # vert   7
     53.10365    30.50000    60.25000  # vert   8
     51.00000    28.49928    60.25000  # vert   9
     51.00000    23.17941    60.25000  # vert  10
     51.60000    23.17941    63.39300  # vert  11
     58.30000    23.08441    63.39300  # vert  12
     58.30000    28.59427    63.39300  # vert  13
     58.30000    29.50000    63.39300  # vert  14
     58.30000    30.10000    63.39300  # vert  15
     58.30000    30.50000    63.39300  # vert  16
     56.04857    30.50000    63.39300  # vert  17
     53.10365    30.50000    63.39300  # vert  18
     51.00000    28.49928    63.39300  # vert  19
     51.00000    23.17941    63.39300  # vert  20
     51.00000    30.50000    60.25000  # vert  21
     51.00000    30.50000    63.39300  # vert  22
# no of vertices followed by list of associated vert
   4,  1,  2, 12, 11,
   4,  2,  3, 13, 12,
   4,  4,  5, 15, 14,
   4,  5,  6, 16, 15,
   4,  6,  7, 17, 16,
   4, 21,  9, 19, 22,
   4,  9, 10, 20, 19,
   4, 10,  1, 11, 20,
  11, 11, 12, 13, 14, 15, 16, 17, 18, 22, 19, 20,
  11,  1, 10,  9, 21,  8,  7,  6,  5,  4,  3,  2,
   4,  3,  4, 14, 13,
   6,  8, 21, 22, 18, 17,  7,
# unused index
 0,0,0,0,0,0,0,0,0,0,0,0
# surfaces indentation (m)
 0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  construction environment
# no  name         type  posn  name         other side
  1, pt_meeting    OPAQ  VERT  UNKNOWN      ANOTHER        
  2, pta_fac_l4    OPAQ  VERT  UNKNOWN      ANOTHER        
  3, gl_facade_l4  OPAQ  VERT  UNKNOWN      ANOTHER        
  4, ptnc_fac_l4   OPAQ  VERT  UNKNOWN      ANOTHER        
  5, ptna_str_l4   OPAQ  VERT  UNKNOWN      ANOTHER        
  6, ptn_cel_b     OPAQ  VERT  UNKNOWN      ANOTHER        
  7, ptna_opn_l4   OPAQ  VERT  UNKNOWN      ANOTHER        
  8, ptnb_opn_l4   OPAQ  VERT  UNKNOWN      ANOTHER        
  9, top           OPAQ  CEIL  UNKNOWN      UNKNOWN        
 10, floor         OPAQ  FLOR  UNKNOWN      UNKNOWN        
 11, ptnb_fac_l4   OPAQ  VERT  UNKNOWN      ANOTHER        
 12, ptnb_str_l4   OPAQ  VERT  UNKNOWN      ANOTHER        
# base
 15 10  0  0  0  0    53.76 0
