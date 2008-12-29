# geometry of plant defined in: ../zone/plant.geo
GEN  plant                  # type   zone name
       8       6   0.000    # vertices, surfaces, rotation angle 
#  X co-ord, Y co-ord, Z co-ord 
     10.00000    60.00000     0.00000  # vert  1
     25.00000    60.00000     0.00000  # vert  2
     25.00000    66.00000     0.00000  # vert  3
     10.00000    66.00000     0.00000  # vert  4
     10.00000    60.00000     5.00000  # vert  5
     25.00000    60.00000     5.00000  # vert  6
     25.00000    66.00000     3.00000  # vert  7
     10.00000    66.00000     3.00000  # vert  8
# no of vertices followed by list of associated vert
   4,  1,  2,  6,  5,
   4,  2,  3,  7,  6,
   4,  3,  4,  8,  7,
   4,  4,  1,  5,  8,
   4,  5,  6,  7,  8,
   4,  1,  4,  3,  2,
# number of default windows within each surface 
   0  0  0  0  0  0
# surfaces indentation (m)
 0.000 0.000 0.000 0.000 0.000 0.000
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  mlc db       environment
# no  name         type  posn  name         other side
  1, Surf-1        OPAQ  VERT  intern_wall  main-hall      
  2, Surf-2        OPAQ  VERT  extern_wall  EXTERIOR       
  3, Surf-3        OPAQ  VERT  extern_wall  EXTERIOR       
  4, Surf-4        OPAQ  VERT  extern_wall  EXTERIOR       
  5, Surf-5        OPAQ  CEIL  roof         EXTERIOR       
  6, Surf-6        OPAQ  FLOR  oth_ground   GROUND         
