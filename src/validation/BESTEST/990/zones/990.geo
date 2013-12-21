*Geometry 1.1,GEN,990 # tag version, format, zone name
*date Sat Dec 21 22:15:56 2013  # latest file modification 
990 describes a...
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,0.00000,0.00000,-1.35000  #   1
*vertex,8.00000,0.00000,-1.35000  #   2
*vertex,8.00000,6.00000,-1.35000  #   3
*vertex,0.00000,6.00000,-1.35000  #   4
*vertex,0.00000,0.00000,0.00000  #   5
*vertex,8.00000,0.00000,0.00000  #   6
*vertex,8.00000,6.00000,0.00000  #   7
*vertex,0.00000,6.00000,0.00000  #   8
*vertex,0.00000,0.00000,1.35000  #   9
*vertex,8.00000,0.00000,1.35000  #  10
*vertex,8.00000,6.00000,1.35000  #  11
*vertex,0.00000,6.00000,1.35000  #  12
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,6,5  #  1
*edges,4,2,3,7,6  #  2
*edges,4,3,4,8,7  #  3
*edges,4,4,1,5,8  #  4
*edges,4,5,6,10,9  #  5
*edges,4,6,7,11,10  #  6
*edges,4,7,8,12,11  #  7
*edges,4,8,5,9,12  #  8
*edges,4,9,10,11,12  #  9
*edges,4,4,3,2,1  # 10
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,S-Ground,VERT,-,-,-,GroundWall,OPAQUE,GROUND,00,01  #   1 ||< user def grnd profile  1
*surf,E-Ground,VERT,-,-,-,GroundWall,OPAQUE,GROUND,00,01  #   2 ||< user def grnd profile  1
*surf,N-Ground,VERT,-,-,-,GroundWall,OPAQUE,GROUND,00,01  #   3 ||< user def grnd profile  1
*surf,W-Ground,VERT,-,-,-,GroundWall,OPAQUE,GROUND,00,01  #   4 ||< user def grnd profile  1
*surf,Window,VERT,-,-,-,dg_glazing,DG_Bestest,EXTERIOR,0,0  #   5 ||< external
*surf,E-Wall,VERT,-,-,-,HW_extwall,OPAQUE,EXTERIOR,0,0  #   6 ||< external
*surf,N-Wall,VERT,-,-,-,HW_extwall,OPAQUE,EXTERIOR,0,0  #   7 ||< external
*surf,W-Wall,VERT,-,-,-,HW_extwall,OPAQUE,EXTERIOR,0,0  #   8 ||< external
*surf,Roof,CEIL,-,-,-,HWroof,OPAQUE,EXTERIOR,0,0  #   9 ||< external
*surf,Floor,FLOR,-,-,-,HWfloor,OPAQUE,GROUND,00,01  #  10 ||< user def grnd profile  1
# 
*insol,1,10,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,all_applicable   1 # insolation sources
  5
# 
*base_list,1,10,    48.00 0  # zone base list
