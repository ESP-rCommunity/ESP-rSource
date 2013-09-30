*Geometry 1.1,GEN,test-6 # tag version, format, zone name
*date Wed Jul 22 18:22:27 2009  # latest file modification 
test-6 describes a...
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,15.00000,12.00000,0.00000  #   1
*vertex,20.00000,12.00000,0.00000  #   2
*vertex,20.00000,17.00000,0.00000  #   3
*vertex,15.00000,17.00000,0.00000  #   4
*vertex,15.00000,12.00000,5.00000  #   5
*vertex,20.00000,12.00000,5.00000  #   6
*vertex,20.00000,17.00000,5.00000  #   7
*vertex,15.00000,17.00000,5.00000  #   8
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,6,5  #  1
*edges,4,2,3,7,6  #  2
*edges,4,3,4,8,7  #  3
*edges,4,4,1,5,8  #  4
*edges,4,5,6,7,8  #  5
*edges,4,4,3,2,1  #  6
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,Surf-1,VERT,-,-,-,extern_wall,OPAQUE,EXTERIOR,0,0  #   1 ||< external
*surf,Surf-2,VERT,-,-,-,extern_wall,OPAQUE,EXTERIOR,0,0  #   2 ||< external
*surf,Surf-3,VERT,-,-,-,extern_wall,OPAQUE,EXTERIOR,0,0  #   3 ||< external
*surf,Surf-4,VERT,-,-,-,extern_wall,OPAQUE,EXTERIOR,0,0  #   4 ||< external
*surf,Surf-5,CEIL,-,-,-,extern_wall,OPAQUE,EXTERIOR,0,0  #   5 ||< external
*surf,Surf-6,FLOR,-,-,-,extern_wall,OPAQUE,EXTERIOR,0,0  #   6 ||< external
# 
*insol,1,6,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,all_applicable   6 # list of surfs
  1  2  3  4  5  6
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,6,    25.00 0  # zone base list
# 
# block entities:
#  *obs = obstructions
*block_start,20 20 # geometric blocks
*obs,25.000,7.000,0.000,1.000,15.000,10.000,0.000,First,NONE  # block  1
*end_block
