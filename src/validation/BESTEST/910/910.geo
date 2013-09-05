*Geometry 1.1,GEN,910 # tag version, format, zone name
*date Wed Jul 22 17:47:42 2009  # latest file modification 
910 describes a...
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
*surf,South_Wall,VERT,-,-,-,HW_extwall,OPAQUE,EXTERIOR,0,0  #   1 ||< external
*surf,South_Wall2,VERT,-,-,-,HW_extwall,OPAQUE,EXTERIOR,0,0  #   2 ||< external
*surf,East_Wall,VERT,-,-,-,HW_extwall,OPAQUE,EXTERIOR,0,0  #   3 ||< external
*surf,North_Wall,VERT,-,-,-,HW_extwall,OPAQUE,EXTERIOR,0,0  #   4 ||< external
*surf,West_Wall,VERT,-,-,-,HW_extwall,OPAQUE,EXTERIOR,0,0  #   5 ||< external
*surf,Window,VERT,-,-,-,dg_glazing,DG_Bestest,EXTERIOR,0,0  #   6 ||< external
*surf,Window2,VERT,-,-,-,dg_glazing,DG_Bestest,EXTERIOR,0,0  #   7 ||< external
*surf,Roof,CEIL,-,-,-,HWroof,OPAQUE,EXTERIOR,0,0  #   8 ||< external
*surf,Floor,FLOR,-,-,-,HWfloor,OPAQUE,GROUND,00,01  #   9 ||< user def grnd profile  1
# 
*insol,1,9,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,all_applicable   8 # list of surfs
  1  2  3  4  5  6  7  8
# 
*insol_calc,all_applicable   2 # insolation sources
  6  7
# 
*base_list,1,9,    48.00 0  # zone base list
# 
# block entities:
#  *obs = obstructions
*block_start,20 20 # geometric blocks
*obs,8.000,-0.001,2.700,8.000,1.000,0.001,180.000,B1,NONE  # block  1
*end_block
