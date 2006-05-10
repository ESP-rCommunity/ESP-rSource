# geometry of loft defined in: ../zone/loft.geo
GEN  loft  loft describes the roof space over all rooms  # type, name, descr
      33      20   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      0.00000    -0.48000     4.92500  # vert   1
      6.90000    -0.48000     4.92500  # vert   2
      6.90000     0.00000     4.92500  # vert   3
      0.00000     0.00000     4.92500  # vert   4
      0.00000     0.00000     5.10000  # vert   5
      2.30000     0.00000     4.92500  # vert   6
      2.30000     0.00000     5.10000  # vert   7
      6.90000     0.00000     5.10000  # vert   8
      2.30000     2.10000     5.10000  # vert   9
      3.30000     2.10000     5.10000  # vert  10
      3.30000     3.10000     5.10000  # vert  11
      0.00000     3.10000     5.10000  # vert  12
      6.90000     3.10000     5.10000  # vert  13
      3.30000     6.40000     5.10000  # vert  14
      0.00000     6.40000     5.10000  # vert  15
      6.90000     6.40000     5.10000  # vert  16
      0.00000     6.88000     4.92500  # vert  17
      6.90000     6.88000     4.92500  # vert  18
      0.00000     3.45000     6.60000  # vert  19
      6.90000     3.45000     6.60000  # vert  20
      3.30000     6.40000     4.92500  # vert  21
      0.00000     6.40000     4.92500  # vert  22
      6.90000     6.40000     4.92500  # vert  23
      3.30000     6.33600     5.10000  # vert  24
      0.00000     6.33500     5.10000  # vert  25
      1.50000     0.89990     5.51312  # vert  26
      4.00000     0.89990     5.51312  # vert  27
      4.00000     2.73976     6.29729  # vert  28
      1.50000     2.73976     6.29729  # vert  29
      1.50000     0.91990     5.46712  # vert  30
      4.00000     0.91990     5.46712  # vert  31
      4.00000     2.75976     6.25129  # vert  32
      1.50000     2.75976     6.25129  # vert  33
# no of vertices followed by list of associated vert
   5,  2,  1,  4,  6,  3,
   4,  5,  7,  6,  4,
   4,  7,  8,  3,  6,
   6,  7,  5, 12, 11, 10,  9,
   6,  8,  7,  9, 10, 11, 13,
   4, 11, 12, 25, 24,
   5, 13, 11, 24, 14, 16,
   4, 22, 21, 14, 15,
   4, 23, 16, 14, 21,
   5, 17, 18, 23, 21, 22,
  10,  1,  2, 20, 19,  1, 26, 29, 28, 27, 26,
   4, 17, 19, 20, 18,
   9,  1, 19, 17, 22, 15, 25, 12,  5,  4,
   8,  2,  3,  8, 13, 16, 23, 18, 20,
   4, 15, 14, 24, 25,
   4, 30, 31, 32, 33,
   4, 31, 27, 28, 32,
   4, 32, 28, 29, 33,
   4, 29, 26, 30, 33,
   4, 26, 27, 31, 30,
# unused index
  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
# surfaces indentation (m)
 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000
 0.000 0.000 0.000 0.000
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  mlc db       environment
# no  name         type  posn  name         other side
  1, soffit_s      OPAQ  FLOR  soffit       EXTERIOR       
  2, hallup_sof    OPAQ  VERT  ext_wall_upr hall_up        
  3, bed1_sof      OPAQ  VERT  ext_wall_upr bed_1          
  4, hallup        OPAQ  FLOR  ceil_loft_r  hall_up        
  5, bed_1         OPAQ  FLOR  ceil_loft_r  bed_1          
  6, bed3          OPAQ  FLOR  ceil_loft_r  bed_3          
  7, bed2          OPAQ  FLOR  ceil_loft_r  bed_2          
  8, bed3_sof      OPAQ  VERT  air_gap_extr t_bridge       
  9, bed2_sof      OPAQ  VERT  ext_wall_upr bed_2          
 10, soffit_n      OPAQ  FLOR  soffit       EXTERIOR       
 11, roof_s        OPAQ  SLOP  roof         EXTERIOR       
 12, roof_n        OPAQ  SLOP  roof         EXTERIOR       
 13, roof_v_ext    OPAQ  VERT  steel_sheet  EXTERIOR       
 14, Surf-14       OPAQ  VERT  intern_wall  SIMILAR        
 15, t_bridge      OPAQ  FLOR  ceil_loft_r  t_bridge       
 16, xcollector    OPAQ  SLOP  collector    collector      
 17, coll-e        OPAQ  VERT  door         collector      
 18, coll-up       OPAQ  SLOP  door         collector      
 19, coll-w        OPAQ  VERT  door         collector      
 20, coll-dn       OPAQ  SLOP  door         collector      
# base
  1  4  5  6  7 10    50.57
