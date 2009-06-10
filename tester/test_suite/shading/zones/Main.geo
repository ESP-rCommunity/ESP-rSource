# geometry of Main defined in: user/obs_4_eachface/zones/Main.geo
GEN  Main  simplified elevation of main zone  # type, name, descr
      34       12 0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
    -1.00000  0.75000  0.53000   # vert 1  
    -1.00000  0.75000  3.02000   # vert 2  
    -10.74600  0.75000  3.02000   # vert 3  
    -10.74600  0.75000  0.53000   # vert 4  
    -10.74600  10.49600  3.02000   # vert 5  
    -10.74600  10.49600  0.53000   # vert 6  
    -1.00000  10.49600  3.02000   # vert 7  
    -1.00000  10.49600  0.53000   # vert 8  
    -3.75000  0.75000  1.49656   # vert 9  
    -7.75000  0.75000  1.49656   # vert 10 
    -7.75000  0.75000  1.94656   # vert 11 
    -3.75000  0.75000  1.94656   # vert 12 
    -10.74600  2.98212  0.53000   # vert 13
    -10.74600  3.85212  0.53000   # vert 14
    -10.74600  3.85212  2.56000   # vert 15 
    -10.74600  2.98212  2.56000   # vert 16 
    -10.74600  5.25000  1.49656   # vert 17 
    -10.74600  8.25000  1.49656   # vert 18 
    -10.74600  8.25000  2.29656   # vert 19 
    -10.74600  5.25000  2.29656   # vert 20 
    -5.74600  10.49600  1.03000   # vert 21 
    -5.24600  10.49600  1.03000   # vert 22 
    -5.24600  10.49600  2.53000   # vert 23 
    -5.74600  10.49600  2.53000   # vert 24 
    -1.00000  8.26388  0.53000   # vert 25 
    -1.00000  7.39388  0.53000   # vert 26 
    -1.00000  7.39388  2.56000   # vert 27 
    -1.00000  8.26388  2.56000   # vert 28 
    -1.00000  5.16177  1.49656   # vert 29 
    -1.00000  4.16177  1.49656   # vert 30 
    -1.00000  4.16177  2.49656   # vert 31 
    -1.00000  5.16177  2.49656   # vert 32 
    -3.75000  0.75000  0.53000   # vert 33 
    -5.74600  10.49600  0.53000   # vert 34 
# no of vertices followed by list of associated vert
  12,   1,   2,   3,   4,   1,  33,   9,  10,  11,  12,   9,  33,
  16,   6,   4,  13,  14,  15,  17,  18,  19,  20,  17,  15,  16,  13,  4,   3,   5,
  12,   8,   6,  34,  21,  22,  23,  24,  21,  34,  6,   5,   7,
  16,   1,   8,  25,  26,  27,  29,  30,  31,  32,  29,  27,  28,  25, 8,   7,   2, 
   4,   1,   4,   6,   8,
   4,   7,   5,   3,   2,
   4,   9,  12,  11,  10,
   4,  14,13,  16,  15,
   4,  18,17,  20,  19,  
   4,  22,21,  24,  23,  
   4,  26,25,  28,  27,  
   4,  30,29,  32,  31,  
# unused index
 0   0   0   0   0   0   0   0   0   0   0   0  
# surfaces indentation (m)
0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 
    3    0    0    0  # default insolation distribution
# surface attributes follow: 
# id surface       geom  loc/   mlc db      environment
# no name          type  posn   name        other side
  1, Mainfloor2    OPAQ  VERT  ext_wall     EXTERIOR       
  2, Mainfloor1    OPAQ  VERT  ext_wall     EXTERIOR       
  3, Mainfloor4    OPAQ  VERT  ext_wall     EXTERIOR       
  4, Mainfloor3    OPAQ  VERT  ext_wall     EXTERIOR       
  5, to_bsm        OPAQ  FLOR  floors       Foundation-1   
  6, to_attic      OPAQ  CEIL  ceiling      Ceiling01      
  7, Right0001     TRAN  VERT  window       EXTERIOR       
  8, Door-01       OPAQ  VERT  ext_door     EXTERIOR       
  9, Front0001     TRAN  VERT  window       EXTERIOR       
 10, Left0001      TRAN  VERT  window       EXTERIOR       
 11, Door-02       OPAQ  VERT  ext_door     EXTERIOR       
 12, Back0001      TRAN  VERT  window       EXTERIOR       
# base
5   0   0   0   0   0   94.98    
