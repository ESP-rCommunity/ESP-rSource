*Geometry 1.1,GEN,zone1 # tag version, format, zone name
*date Tue Jan 14 17:08:07 2014  # latest file modification 
zone1 describes a...
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,0.00000,0.00000,0.00000  #   1
*vertex,8.00000,0.00000,0.00000  #   2
*vertex,8.00000,6.00000,0.00000  #   3
*vertex,0.00000,6.00000,0.00000  #   4
*vertex,0.00000,0.00000,2.70000  #   5
*vertex,8.00000,0.00000,2.70000  #   6
*vertex,8.00000,6.00000,2.70000  #   7
*vertex,0.00000,6.00000,2.70000  #   8
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
*surf,wall-1,VERT,-,-,-,wall,OPAQUE,EXTERIOR,0,0  #   1 ||< external
*surf,wall-2,VERT,-,-,-,wall,OPAQUE,EXTERIOR,0,0  #   2 ||< external
*surf,wall-3,VERT,-,-,-,wall,OPAQUE,EXTERIOR,0,0  #   3 ||< external
*surf,wall-4,VERT,-,-,-,wall,OPAQUE,EXTERIOR,0,0  #   4 ||< external
*surf,roof,CEIL,-,-,-,roof,OPAQUE,EXTERIOR,0,0  #   5 ||< external
*surf,floor,FLOR,-,-,-,floor,OPAQUE,EXTERIOR,0,0  #   6 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,6,    48.00 0  # zone base list
