*Geometry 1.1,GEN,Zone_2 # tag version, format, zone name
*date Sat May  1 11:23:51 2021  # latest file modification 
Zone_2 describes a...
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,-3.00000,0.00000,0.00000  #   1
*vertex,0.00000,0.00000,0.00000  #   2
*vertex,0.00000,4.00000,0.00000  #   3
*vertex,-3.00000,4.00000,0.00000  #   4
*vertex,-3.00000,0.00000,2.70000  #   5
*vertex,0.00000,0.00000,2.70000  #   6
*vertex,0.00000,4.00000,2.70000  #   7
*vertex,-3.00000,4.00000,2.70000  #   8
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,6,5  #   1
*edges,4,2,3,7,6  #   2
*edges,4,3,4,8,7  #   3
*edges,4,4,1,5,8  #   4
*edges,4,5,6,7,8  #   5
*edges,4,1,4,3,2  #   6
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,Surf-1,VERT,-,-,-,int_part,OPAQUE,ADIABATIC,000,000  #   1 ||< adiabatic
*surf,Surf-2,VERT,-,-,-,int_part,OPAQUE,ANOTHER,001,004  #   2 ||< Surf-4:Zone_1
*surf,Surf-3,VERT,-,-,-,int_part,OPAQUE,ADIABATIC,000,000  #   3 ||< adiabatic
*surf,Surf-4,VERT,-,-,-,int_part,OPAQUE,ADIABATIC,000,000  #   4 ||< adiabatic
*surf,Surf-5,CEIL,-,-,-,int_part,OPAQUE,ADIABATIC,000,000  #   5 ||< adiabatic
*surf,Surf-6,FLOR,-,-,-,int_part,OPAQUE,ADIABATIC,000,000  #   6 ||< adiabatic
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,6,    12.00 0  # zone base list
