*Geometry 1.1,GEN,ground # tag version, format, zone name
*date Fri Jul 17 13:39:46 2009  # latest file modification 
ground describes a
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,-5.00000,-5.00000,-3.00000  #   1
*vertex,11.00000,-5.00000,-3.00000  #   2
*vertex,11.00000,12.00000,-3.00000  #   3
*vertex,-5.00000,12.00000,-3.00000  #   4
*vertex,-5.00000,-5.00000,0.00000  #   5
*vertex,11.00000,-5.00000,0.00000  #   6
*vertex,11.00000,12.00000,0.00000  #   7
*vertex,-5.00000,12.00000,0.00000  #   8
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
*surf,south,VERT,-,-,-,fict_grnd,OPAQUE,EXTERIOR,0,0  #   1 ||< external
*surf,east,VERT,-,-,-,fict_grnd,OPAQUE,EXTERIOR,0,0  #   2 ||< external
*surf,north,VERT,-,-,-,fict_grnd,OPAQUE,EXTERIOR,0,0  #   3 ||< external
*surf,west,VERT,-,-,-,fict_grnd,OPAQUE,EXTERIOR,0,0  #   4 ||< external
*surf,top,CEIL,-,-,-,fict_grnd,OPAQUE,EXTERIOR,0,0  #   5 ||< external
*surf,bottom,FLOR,-,-,-,fict_grnd,OPAQUE,EXTERIOR,0,0  #   6 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,6,   272.00 0  # zone base list
