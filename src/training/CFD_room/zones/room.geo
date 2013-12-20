*Geometry 1.1,GEN,room # tag version, format, zone name
*date Sun Dec  8 15:19:44 2013  # latest file modification 
room describes a basic room
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,0.00000,0.00000,0.00000  #   1
*vertex,4.20000,0.00000,0.00000  #   2
*vertex,4.20000,3.60000,0.00000  #   3
*vertex,0.00000,3.60000,0.00000  #   4
*vertex,0.00000,0.00000,3.00000  #   5
*vertex,4.20000,0.00000,3.00000  #   6
*vertex,4.20000,3.60000,3.00000  #   7
*vertex,0.00000,3.60000,3.00000  #   8
*vertex,0.00000,1.95000,2.05000  #   9
*vertex,0.00000,1.65000,2.05000  #  10
*vertex,0.00000,1.65000,2.25000  #  11
*vertex,0.00000,1.95000,2.25000  #  12
*vertex,3.75000,1.65000,3.00000  #  13
*vertex,3.95000,1.65000,3.00000  #  14
*vertex,3.95000,1.95000,3.00000  #  15
*vertex,3.75000,1.95000,3.00000  #  16
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,6,5  #  1
*edges,4,2,3,7,6  #  2
*edges,4,3,4,8,7  #  3
*edges,10,4,1,5,8,4,9,12,11,10,9  #  4
*edges,10,5,6,7,8,5,13,16,15,14,13  #  5
*edges,4,1,4,3,2  #  6
*edges,4,9,10,11,12  #  7
*edges,4,13,14,15,16  #  8
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,south,VERT,-,-,-,extern_wall,OPAQUE,EXTERIOR,0,0  #   1 ||< external
*surf,east,VERT,-,-,-,extern_wall,OPAQUE,EXTERIOR,0,0  #   2 ||< external
*surf,north,VERT,-,-,-,extern_wall,OPAQUE,EXTERIOR,0,0  #   3 ||< external
*surf,west,VERT,-,-,-,partition,OPAQUE,SIMILAR,00,00  #   4 ||< identical environment
*surf,ceiling,CEIL,-,-,-,ceiling,OPAQUE,SIMILAR,00,00  #   5 ||< identical environment
*surf,floor,FLOR,-,-,-,floor_1,OPAQUE,GROUND,01,00  #   6 ||< ground profile  1
*surf,inlet,VERT,-,-,-,dummy_pnls,OPAQUE,SIMILAR,00,00  #   7 ||< identical environment
*surf,outlet,CEIL,-,-,-,dummy_pnls,OPAQUE,SIMILAR,00,00  #   8 ||< identical environment
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,6,    15.12 0  # zone base list
