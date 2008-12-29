# geometry of TIM defined in: TIM.geo
GEN  TIM                    # type   zone name
       8       6   0.000    # vertices, surfaces, rotation angle 
#  X co-ord, Y co-ord, Z co-ord 
      3.000     0.000     0.000  # vert  1
      6.000     0.000     0.000  # vert  2
      6.000     0.010     0.000  # vert  3
      3.000     0.010     0.000  # vert  4
      3.000     0.000     2.200  # vert  5
      6.000     0.000     2.200  # vert  6
      6.000     0.010     2.200  # vert  7
      3.000     0.010     2.200  # vert  8
# no of vertices followed by list of associated vert
   4,  1,  2,  6,  5,
   4,  2,  3,  7,  6,
   4,  3,  4,  8,  7,
   4,  4,  1,  5,  8,
   4,  5,  6,  7,  8,
   4,  1,  4,  3,  2,
# number of default windows within each surface 
  0, 0, 0, 0, 0, 0,
# surfaces indentation (m)
 0.000 0.000 0.000 0.000 0.000 0.000
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  mlc db       environment
# no  name         type  posn  name         other side
  1, south         TRAN  VERT  TI_glazing   EXTERIOR       
  2, east          OPAQ  VERT  dummy_surf   ADIABATIC      
  3, living        OPAQ  VERT  mass_wall    liv_room       
  4, west          OPAQ  VERT  dummy_surf   ADIABATIC      
  5, roof          OPAQ  CEIL  dummy_surf   ADIABATIC      
  6, floor         OPAQ  FLOR  dummy_surf   ADIABATIC      
