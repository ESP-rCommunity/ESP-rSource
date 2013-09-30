*Geometry 1.1,GEN,tube_a # tag version, format, zone name
*date Thu Aug 23 17:43:01 2007  # latest file modification 
tube_a describes the a section of earth tube
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,-3.96110,-3.18252,-1.00000  #   1
*vertex,7.50000,-3.00000,-1.00000  #   2
*vertex,7.50000,-2.00000,-1.00000  #   3
*vertex,7.00000,-2.00000,-1.00000  #   4
*vertex,7.00000,-2.50000,-1.00000  #   5
*vertex,-3.99333,-2.69990,-1.00000  #   6
*vertex,-3.96110,-3.18252,-0.80000  #   7
*vertex,7.50000,-3.00000,-0.80000  #   8
*vertex,7.50000,-2.00000,-0.80000  #   9
*vertex,7.00000,-2.00000,-0.80000  #  10
*vertex,7.00000,-2.50000,-0.80000  #  11
*vertex,-3.99333,-2.69990,-0.80000  #  12
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,8,7  #  1
*edges,4,2,3,9,8  #  2
*edges,4,3,4,10,9  #  3
*edges,4,4,5,11,10  #  4
*edges,4,5,6,12,11  #  5
*edges,4,6,1,7,12  #  6
*edges,6,7,8,9,10,11,12  #  7
*edges,6,1,6,5,4,3,2  #  8
# 
# surf attributes:
#  surf name, surf position VERT/CIIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,side_a,VERT,-,-,-,earth_side,OPAQUE,GROUND,00,01  #   1 ||< user def grnd profile  1
*surf,side_b,VERT,-,-,-,earth_side,OPAQUE,GROUND,00,01  #   2 ||< user def grnd profile  1
*surf,side_c,VERT,-,-,-,insul_frame,OPAQUE,ANOTHER,05,01  #   3 ||< end_a:tube_b
*surf,side_e,VERT,-,-,-,earth_side,OPAQUE,GROUND,00,01  #   4 ||< user def grnd profile  1
*surf,side_f,VERT,-,-,-,earth_side,OPAQUE,GROUND,00,01  #   5 ||< user def grnd profile  1
*surf,side_g,VERT,-,-,-,earth_side,OPAQUE,GROUND,00,01  #   6 ||< user def grnd profile  1
*surf,top,CEIL,-,-,-,earth_top,OPAQUE,EXTERIOR,0,0  #   7 ||< external
*surf,base,FLOR,-,-,-,below,OPAQUE,GROUND,00,01  #   8 ||< user def grnd profile  1
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,8,     5.94 0  # zone base list
