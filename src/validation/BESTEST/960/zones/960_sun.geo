*Geometry 1.1,GEN,960_sun # tag version, format, zone name
*date Sat Dec 21 22:16:31 2013  # latest file modification 
960_sun describes a...
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,0.00000,-2.00000,0.00000  #   1
*vertex,8.00000,-2.00000,0.00000  #   2
*vertex,8.00000,0.00000,0.00000  #   3
*vertex,0.00000,0.00000,0.00000  #   4
*vertex,0.00000,-2.00000,2.70000  #   5
*vertex,8.00000,-2.00000,2.70000  #   6
*vertex,8.00000,0.00000,2.70000  #   7
*vertex,0.00000,0.00000,2.70000  #   8
*vertex,0.50000,-2.00000,0.50000  #   9
*vertex,3.50000,-2.00000,0.50000  #  10
*vertex,3.50000,-2.00000,2.50000  #  11
*vertex,0.50000,-2.00000,2.50000  #  12
*vertex,4.50000,-2.00000,0.50000  #  13
*vertex,7.50000,-2.00000,0.50000  #  14
*vertex,7.50000,-2.00000,2.50000  #  15
*vertex,4.50000,-2.00000,2.50000  #  16
# 
# tag, number of vertices followed by list of associated vert
*edges,16,1,2,6,5,1,9,12,11,10,9,1,13,16,15,14,13  #  1
*edges,4,2,3,7,6  #  2
*edges,4,3,4,8,7  #  3
*edges,4,4,1,5,8  #  4
*edges,4,5,6,7,8  #  5
*edges,4,1,4,3,2  #  6
*edges,4,9,10,11,12  #  7
*edges,4,13,14,15,16  #  8
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,South,VERT,-,-,-,HW_extwall,OPAQUE,EXTERIOR,0,0  #   1 ||< external
*surf,East,VERT,-,-,-,HW_extwall,OPAQUE,EXTERIOR,0,0  #   2 ||< external
*surf,CommonWall,VERT,-,-,-,CommonWalll,OPAQUE,ANOTHER,01,06  #   3 ||< CommonWall:960_back
*surf,West,VERT,-,-,-,HW_extwall,OPAQUE,EXTERIOR,0,0  #   4 ||< external
*surf,Roof,CEIL,-,-,-,HWroof,OPAQUE,EXTERIOR,0,0  #   5 ||< external
*surf,Floor,FLOR,-,-,-,HWfloor,OPAQUE,GROUND,00,01  #   6 ||< user def grnd profile  1
*surf,Window,VERT,-,-,-,dg_glazing,DG_Bestest,EXTERIOR,0,0  #   7 ||< external
*surf,Window2,VERT,-,-,-,dg_glazing,DG_Bestest,EXTERIOR,0,0  #   8 ||< external
# 
*insol,1,6,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,all_applicable   2 # insolation sources
  7  8
# 
*base_list,1,6,    16.00 0  # zone base list
