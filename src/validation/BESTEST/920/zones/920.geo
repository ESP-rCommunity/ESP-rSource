*Geometry 1.1,GEN,920 # tag version, format, zone name
*date Sat Dec 21 21:18:32 2013  # latest file modification 
920 describes a...
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
*vertex,8.00000,1.50000,0.20000  #  11
*vertex,8.00000,4.50000,0.20000  #  12
*vertex,8.00000,4.50000,2.20000  #  13
*vertex,8.00000,1.50000,2.20000  #  14
*vertex,0.00000,4.50000,0.20000  #  15
*vertex,0.00000,1.50000,0.20000  #  16
*vertex,0.00000,1.50000,2.20000  #  17
*vertex,0.00000,4.50000,2.20000  #  18
# 
# tag, number of vertices followed by list of associated vert
*edges,10,2,6,8,4,2,11,14,13,12,11  #  1
*edges,4,6,5,7,8  #  2
*edges,10,5,1,3,7,5,15,18,17,16,15  #  3
*edges,5,3,9,4,8,7  #  4
*edges,5,5,6,2,10,1  #  5
*edges,6,1,10,2,4,9,3  #  6
*edges,4,11,12,13,14  #  7
*edges,4,15,16,17,18  #  8
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,East_Wall,VERT,-,-,-,HW_extwall,OPAQUE,EXTERIOR,0,0  #   1 ||< external
*surf,North_Wall,VERT,-,-,-,HW_extwall,OPAQUE,EXTERIOR,0,0  #   2 ||< external
*surf,West_Wall,VERT,-,-,-,HW_extwall,OPAQUE,EXTERIOR,0,0  #   3 ||< external
*surf,Roof,CEIL,-,-,-,HWroof,OPAQUE,EXTERIOR,0,0  #   4 ||< external
*surf,Floor,FLOR,-,-,-,HWfloor,OPAQUE,GROUND,00,01  #   5 ||< user def grnd profile  1
*surf,South_Wall,VERT,-,-,-,HW_extwall,OPAQUE,EXTERIOR,0,0  #   6 ||< external
*surf,EastWindow,VERT,-,-,-,dg_glazing,DG_Bestest,EXTERIOR,0,0  #   7 ||< external
*surf,WestWindow,VERT,-,-,-,dg_glazing,DG_Bestest,EXTERIOR,0,0  #   8 ||< external
# 
*insol,1,5,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,all_applicable   2 # insolation sources
  7  8
# 
*base_list,1,5,    48.00 0  # zone base list
