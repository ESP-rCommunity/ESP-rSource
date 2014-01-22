*Geometry 1.1,GEN,430 # tag version, format, zone name
*date Sat Dec 21 21:55:59 2013  # latest file modification 
430 describes a...
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,0.00000,0.00000,0.00000  #   1
*vertex,8.00000,0.00000,0.00000  #   2
*vertex,0.00000,0.00000,2.70000  #   3
*vertex,8.00000,0.00000,2.70000  #   4
*vertex,0.50000,0.00000,0.20000  #   5
*vertex,3.50000,0.00000,0.20000  #   6
*vertex,0.50000,0.00000,2.20000  #   7
*vertex,3.50000,0.00000,2.20000  #   8
*vertex,4.50000,0.00000,0.20000  #   9
*vertex,7.50000,0.00000,0.20000  #  10
*vertex,4.50000,0.00000,2.20000  #  11
*vertex,7.50000,0.00000,2.20000  #  12
*vertex,0.00000,6.00000,0.00000  #  13
*vertex,8.00000,6.00000,0.00000  #  14
*vertex,0.00000,6.00000,2.70000  #  15
*vertex,8.00000,6.00000,2.70000  #  16
*vertex,4.00000,0.00000,2.70000  #  17
*vertex,4.00000,0.00000,0.00000  #  18
# 
# tag, number of vertices followed by list of associated vert
*edges,10,1,18,17,8,6,5,7,8,17,3  #  1
*edges,10,18,2,4,12,10,9,11,12,4,17  #  2
*edges,4,2,14,16,4  #  3
*edges,4,14,13,15,16  #  4
*edges,4,13,1,3,15  #  5
*edges,4,5,6,8,7  #  6
*edges,4,9,10,12,11  #  7
*edges,5,3,17,4,16,15  #  8
*edges,5,13,14,2,18,1  #  9
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,South_Wall,VERT,-,-,-,LWextwall7,OPAQUE,EXTERIOR,0,0  #   1 ||< external
*surf,South_Wall2,VERT,-,-,-,LWextwall7,OPAQUE,EXTERIOR,0,0  #   2 ||< external
*surf,East_Wall,VERT,-,-,-,LWextwall7,OPAQUE,EXTERIOR,0,0  #   3 ||< external
*surf,North_Wall,VERT,-,-,-,LWextwall7,OPAQUE,EXTERIOR,0,0  #   4 ||< external
*surf,West_Wall,VERT,-,-,-,LWextwall7,OPAQUE,EXTERIOR,0,0  #   5 ||< external
*surf,Window,VERT,-,-,-,op_glass7,OPAQUE,EXTERIOR,0,0  #   6 ||< external
*surf,Window2,VERT,-,-,-,op_glass7,OPAQUE,EXTERIOR,0,0  #   7 ||< external
*surf,Roof,CEIL,-,-,-,LWroof7,OPAQUE,EXTERIOR,0,0  #   8 ||< external
*surf,Floor,FLOR,-,-,-,LWfloor7,OPAQUE,GROUND,00,01  #   9 ||< user def grnd profile  1
# 
*insol,1,9,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,9,    48.00 0  # zone base list
