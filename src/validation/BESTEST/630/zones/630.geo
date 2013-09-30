*Geometry 1.1,GEN,630 # tag version, format, zone name
*date Wed Jul 22 17:52:21 2009  # latest file modification 
630 describes a...
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
*surf,East_Wall,VERT,-,-,-,LWextwall7,OPAQUE,EXTERIOR,0,0  #   1 ||< external
*surf,North_Wall,VERT,-,-,-,LWextwall7,OPAQUE,EXTERIOR,0,0  #   2 ||< external
*surf,West_Wall,VERT,-,-,-,LWextwall7,OPAQUE,EXTERIOR,0,0  #   3 ||< external
*surf,Roof,CEIL,-,-,-,LWroof7,OPAQUE,EXTERIOR,0,0  #   4 ||< external
*surf,Floor,FLOR,-,-,-,LWfloor7,OPAQUE,GROUND,00,01  #   5 ||< user def grnd profile  1
*surf,South_Wall,VERT,-,-,-,LWextwall7,OPAQUE,EXTERIOR,0,0  #   6 ||< external
*surf,EastWindow,VERT,-,-,-,dg_glazing,DG_Bestest,EXTERIOR,0,0  #   7 ||< external
*surf,WestWindow,VERT,-,-,-,dg_glazing,DG_Bestest,EXTERIOR,0,0  #   8 ||< external
# 
*insol,1,5,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,all_applicable   7 # list of surfs
  1  2  3  4  6  7  8
# 
*insol_calc,all_applicable   2 # insolation sources
  7  8
# 
*base_list,1,5,    48.00 0  # zone base list
# 
# block entities:
#  *obs = obstructions
*block_start,20 20 # geometric blocks
*obs,9.001,1.500,2.700,3.000,1.000,0.001,90.000,B1,NONE  # block  1
*obs,9.001,1.500,0.000,0.001,1.000,2.700,90.000,B2,NONE  # block  2
*obs,9.001,4.500,0.000,0.001,1.000,2.700,90.000,B3,NONE  # block  3
*obs,-0.001,1.500,2.700,3.000,1.000,0.001,90.000,B4,NONE  # block  4
*obs,-0.001,1.499,0.000,0.001,1.000,2.700,90.000,B5,NONE  # block  5
*obs,-0.001,4.500,0.000,0.001,1.000,2.700,90.000,B6,NONE  # block  6
*end_block
