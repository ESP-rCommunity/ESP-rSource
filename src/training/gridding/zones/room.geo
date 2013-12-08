*Geometry 1.1,GEN,room # tag version, format, zone name
*date Sun Dec  8 16:40:35 2013  # latest file modification 
room describes a...
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,0.00000,0.00000,0.00000  #   1
*vertex,5.00000,0.00000,0.00000  #   2
*vertex,5.00000,5.00000,0.00000  #   3
*vertex,0.00000,5.00000,0.00000  #   4
*vertex,0.00000,0.00000,3.00000  #   5
*vertex,5.00000,0.00000,3.00000  #   6
*vertex,5.00000,5.00000,3.00000  #   7
*vertex,0.00000,5.00000,3.00000  #   8
*vertex,1.00000,0.00000,0.00000  #   9
*vertex,1.00000,0.00000,3.00000  #  10
*vertex,0.00000,1.00000,0.00000  #  11
*vertex,0.00000,1.00000,3.00000  #  12
# 
# tag, number of vertices followed by list of associated vert
*edges,4,9,2,6,10  #  1
*edges,4,2,3,7,6  #  2
*edges,4,4,11,12,8  #  3
*edges,4,5,6,7,8  #  4
*edges,4,1,4,3,2  #  5
*edges,4,3,4,8,7  #  6
*edges,4,1,9,10,5  #  7
*edges,4,11,1,5,12  #  8
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,south,VERT,-,-,-,intern_wall,OPAQUE,EXTERIOR,0,0  #   1 ||< external
*surf,east,VERT,-,-,-,intern_wall,OPAQUE,EXTERIOR,0,0  #   2 ||< external
*surf,west,VERT,-,-,-,intern_wall,OPAQUE,EXTERIOR,0,0  #   3 ||< external
*surf,roof,CEIL,-,-,-,roof_2,OPAQUE,EXTERIOR,0,0  #   4 ||< external
*surf,floor,FLOR,-,-,-,floor_1,OPAQUE,GROUND,01,00  #   5 ||< ground profile  1
*surf,north_1,VERT,-,-,-,intern_wall,OPAQUE,EXTERIOR,0,0  #   6 ||< external
*surf,edge_s,VERT,-,-,-,intern_wall,OPAQUE,EXTERIOR,0,0  #   7 ||< external
*surf,edge_w,VERT,-,-,-,intern_wall,OPAQUE,EXTERIOR,0,0  #   8 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,5,    25.00 0  # zone base list
