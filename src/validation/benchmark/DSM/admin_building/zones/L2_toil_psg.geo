# geometry of L2_toil_psg defined in: ../zones/L2_toil_psg.geo
GEN  L2_toil_psg  L2_toil_psg describes L2 toilet and passage to stair_2  # type, name, descr
      22      12   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
     51.60000    23.17941    52.45000  # vert   1
     58.30000    23.08441    52.45000  # vert   2
     58.30000    28.59427    52.45000  # vert   3
     58.30000    29.50000    52.45000  # vert   4
     58.30000    30.10000    52.45000  # vert   5
     58.30000    30.50000    52.45000  # vert   6
     56.04857    30.50000    52.45000  # vert   7
     53.10365    30.50000    52.45000  # vert   8
     51.00000    28.49928    52.45000  # vert   9
     51.00000    23.17941    52.45000  # vert  10
     51.60000    23.17941    55.59300  # vert  11
     58.30000    23.08441    55.59300  # vert  12
     58.30000    28.59427    55.59300  # vert  13
     58.30000    29.50000    55.59300  # vert  14
     58.30000    30.10000    55.59300  # vert  15
     58.30000    30.50000    55.59300  # vert  16
     56.04857    30.50000    55.59300  # vert  17
     53.10365    30.50000    55.59300  # vert  18
     51.00000    28.49928    55.59300  # vert  19
     51.00000    23.17941    55.59300  # vert  20
     51.00000    30.50000    52.45000  # vert  21
     51.00000    30.50000    55.59300  # vert  22
# no of vertices followed by list of associated vert
   4,  1,  2, 12, 11,
   4,  2,  3, 13, 12,
   4,  3,  4, 14, 13,
   4,  4,  5, 15, 14,
   4,  5,  6, 16, 15,
   4,  6,  7, 17, 16,
   4, 21,  9, 19, 22,
   4,  9, 10, 20, 19,
   4, 10,  1, 11, 20,
  11, 11, 12, 13, 14, 15, 16, 17, 18, 22, 19, 20,
  11,  1, 10,  9, 21,  8,  7,  6,  5,  4,  3,  2,
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
  2, pta_fac_l2    OPAQ  VERT  UNKNOWN      ANOTHER        
  3, ptb_fac_l2    OPAQ  VERT  UNKNOWN      ANOTHER        
  4, gl_facade_l2  OPAQ  VERT  UNKNOWN      ANOTHER        
  5, ptnc_fac_l2   OPAQ  VERT  UNKNOWN      UNKNOWN        
  6, ptna_str_l2   OPAQ  VERT  UNKNOWN      ANOTHER        
  7, ptn_cel_b     OPAQ  VERT  UNKNOWN      ANOTHER        
  8, ptna_opn_l2   OPAQ  VERT  UNKNOWN      ANOTHER        
  9, ptnb_opn_l2   OPAQ  VERT  UNKNOWN      ANOTHER        
 10, top           OPAQ  CEIL  UNKNOWN      UNKNOWN        
 11, floor         OPAQ  FLOR  UNKNOWN      UNKNOWN        
 12, ptnb_str_l2   OPAQ  VERT  UNKNOWN      ANOTHER        
# base
 15 11  0  0  0  0    53.76 0
