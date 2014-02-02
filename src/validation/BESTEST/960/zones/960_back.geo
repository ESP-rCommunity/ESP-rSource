*Geometry 1.1,GEN,960_back # tag version, format, zone name
*date Sat Dec 21 22:16:31 2013  # latest file modification 
960_back describes a...
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,0.00000,0.00000,0.00000  #   1
*vertex,8.00000,0.00000,0.00000  #   2
*vertex,0.00000,0.00000,2.70000  #   3
*vertex,8.00000,0.00000,2.70000  #   4
*vertex,0.00000,6.00000,0.00000  #   5
*vertex,8.00000,6.00000,0.00000  #   6
*vertex,0.00000,6.00000,2.70000  #   7
*vertex,8.00000,6.00000,2.70000  #   8
*vertex,4.00000,0.00000,2.70000  #   9
*vertex,4.00000,0.00000,0.00000  #  10
# 
# tag, number of vertices followed by list of associated vert
*edges,4,2,6,8,4  #  1
*edges,4,6,5,7,8  #  2
*edges,4,5,1,3,7  #  3
*edges,5,3,9,4,8,7  #  4
*edges,5,5,6,2,10,1  #  5
*edges,6,1,10,2,4,9,3  #  6
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,East_Wall,VERT,-,-,-,LWextwall7,OPAQUE,EXTERIOR,0,0  #   1 ||< external
*surf,North_Wall,VERT,-,-,-,LWextwall7,OPAQUE,EXTERIOR,0,0  #   2 ||< external
*surf,West_Wall,VERT,-,-,-,LWextwall7,OPAQUE,EXTERIOR,0,0  #   3 ||< external
*surf,Roof,CEIL,-,-,-,LWroof7,OPAQUE,EXTERIOR,0,0  #   4 ||< external
*surf,Floor,FLOR,-,-,-,LWfloor7,OPAQUE,GROUND,00,01  #   5 ||< user def grnd profile  1
*surf,CommonWall,VERT,-,-,-,CommonWalll,OPAQUE,ANOTHER,02,03  #   6 ||< CommonWall:960_sun
# 
*insol,1,5,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,5,    48.00 0  # zone base list
