*Geometry 1.1,GEN,TIM # tag version, format, zone name
*date Fri Jul 17 13:52:33 2009  # latest file modification 
TIM describes a
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,3.00000,0.00000,0.00000  #   1
*vertex,6.00000,0.00000,0.00000  #   2
*vertex,6.00000,0.01000,0.00000  #   3
*vertex,3.00000,0.01000,0.00000  #   4
*vertex,3.00000,0.00000,2.20000  #   5
*vertex,6.00000,0.00000,2.20000  #   6
*vertex,6.00000,0.01000,2.20000  #   7
*vertex,3.00000,0.01000,2.20000  #   8
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,6,5  #  1
*edges,4,2,3,7,6  #  2
*edges,4,3,4,8,7  #  3
*edges,4,4,1,5,8  #  4
*edges,4,5,6,7,8  #  5
*edges,4,1,4,3,2  #  6
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,south,VERT,-,-,-,TI_glazing,DCTIM_blind,EXTERIOR,0,0  #   1 ||< external
*surf,east,VERT,-,-,-,dummy_surf,OPAQUE,ADIABATIC,0,0  #   2 ||< adiabatic
*surf,living,VERT,-,-,-,mass_wall,OPAQUE,ANOTHER,05,11  #   3 ||< TIM:liv_room
*surf,west,VERT,-,-,-,dummy_surf,OPAQUE,ADIABATIC,0,0  #   4 ||< adiabatic
*surf,roof,CEIL,-,-,-,dummy_surf,OPAQUE,ADIABATIC,0,0  #   5 ||< adiabatic
*surf,floor,FLOR,-,-,-,dummy_surf,OPAQUE,ADIABATIC,0,0  #   6 ||< adiabatic
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,6,     0.03 0  # zone base list
