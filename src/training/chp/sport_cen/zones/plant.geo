*Geometry 1.1,GEN,plant # tag version, format, zone name
*date Tue Jul 21 18:22:40 2009  # latest file modification 
plant describes a
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,10.00000,60.00000,0.00000  #   1
*vertex,25.00000,60.00000,0.00000  #   2
*vertex,25.00000,66.00000,0.00000  #   3
*vertex,10.00000,66.00000,0.00000  #   4
*vertex,10.00000,60.00000,5.00000  #   5
*vertex,25.00000,60.00000,5.00000  #   6
*vertex,25.00000,66.00000,3.00000  #   7
*vertex,10.00000,66.00000,3.00000  #   8
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
*surf,Surf-1,VERT,-,-,-,intern_wall,OPAQUE,ANOTHER,03,15  #   1 ||< plant-r:main-hall
*surf,Surf-2,VERT,-,-,-,extern_wall,OPAQUE,EXTERIOR,0,0  #   2 ||< external
*surf,Surf-3,VERT,-,-,-,extern_wall,OPAQUE,EXTERIOR,0,0  #   3 ||< external
*surf,Surf-4,VERT,-,-,-,extern_wall,OPAQUE,EXTERIOR,0,0  #   4 ||< external
*surf,Surf-5,CEIL,-,-,-,roof,OPAQUE,EXTERIOR,0,0  #   5 ||< external
*surf,Surf-6,FLOR,-,-,-,oth_ground,OPAQUE,GROUND,01,00  #   6 ||< ground profile  1
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,6,    90.00 0  # zone base list
