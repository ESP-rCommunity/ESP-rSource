# geometry of room defined in: ../zones/room.geo
GEN  room  room describes a basic room  # type, name, descr
      16       8   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      0.00000     0.00000     0.00000  # vert   1
      4.20000     0.00000     0.00000  # vert   2
      4.20000     3.60000     0.00000  # vert   3
      0.00000     3.60000     0.00000  # vert   4
      0.00000     0.00000     3.00000  # vert   5
      4.20000     0.00000     3.00000  # vert   6
      4.20000     3.60000     3.00000  # vert   7
      0.00000     3.60000     3.00000  # vert   8
      0.00000     1.95000     2.05000  # vert   9
      0.00000     1.65000     2.05000  # vert  10
      0.00000     1.65000     2.25000  # vert  11
      0.00000     1.95000     2.25000  # vert  12
      3.75000     1.65000     3.00000  # vert  13
      3.95000     1.65000     3.00000  # vert  14
      3.95000     1.95000     3.00000  # vert  15
      3.75000     1.95000     3.00000  # vert  16
# no of vertices followed by list of associated vert
   4,  1,  2,  6,  5,
   4,  2,  3,  7,  6,
   4,  3,  4,  8,  7,
  10,  4,  1,  5,  8,  4,  9, 12, 11, 10,  9,
  10,  5,  6,  7,  8,  5, 13, 16, 15, 14, 13,
   4,  1,  4,  3,  2,
   4,  9, 10, 11, 12,
   4, 13, 14, 15, 16,
# unused index
  0 0 0 0 0 0 0 0
# surfaces indentation (m)
 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  mlc db       environment
# no  name         type  posn  name         other side
  1, south         OPAQ  VERT  extern_wall  EXTERIOR       
  2, east          OPAQ  VERT  extern_wall  EXTERIOR       
  3, north         OPAQ  VERT  extern_wall  EXTERIOR       
  4, west          OPAQ  VERT  partition    SIMILAR        
  5, ceiling       OPAQ  CEIL  ceiling      SIMILAR        
  6, floor         OPAQ  FLOR  floor_1      GROUND         
  7, inlet         OPAQ  VERT  dummy_pnls   SIMILAR        
  8, outlet        OPAQ  CEIL  dummy_pnls   SIMILAR        
# base
  6  0  0  0  0  0    15.12
