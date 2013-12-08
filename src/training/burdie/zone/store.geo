*Geometry 1.1,GEN,store # tag version, format, zone name
*date Sun Dec  8 13:46:26 2013  # latest file modification 
store describes the back entrance and store
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,5.20000,3.70000,0.00000  #   1
*vertex,6.90000,3.70000,0.00000  #   2
*vertex,6.90000,6.40000,0.00000  #   3
*vertex,5.20000,6.40000,0.00000  #   4
*vertex,5.20000,3.70000,2.70000  #   5
*vertex,6.90000,3.70000,2.70000  #   6
*vertex,6.90000,6.40000,2.70000  #   7
*vertex,5.20000,6.40000,2.70000  #   8
*vertex,6.50000,6.40000,0.00000  #   9
*vertex,5.50000,6.40000,0.00000  #  10
*vertex,5.50000,6.40000,2.00000  #  11
*vertex,6.50000,6.40000,2.00000  #  12
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,6,5  #  1
*edges,4,2,3,7,6  #  2
*edges,8,3,9,12,11,10,4,8,7  #  3
*edges,4,4,1,5,8  #  4
*edges,4,5,6,7,8  #  5
*edges,6,4,10,9,3,2,1  #  6
*edges,4,9,10,11,12  #  7
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,liv_r,VERT,-,-,-,partition,OPAQUE,ANOTHER,01,09  #   1 ||< store_r:living_room
*surf,store_sim_e,VERT,-,-,-,intern_wall,OPAQUE,SIMILAR,00,00  #   2 ||< identical environment
*surf,store_n_ex,VERT,-,-,-,ext_wall_low,OPAQUE,EXTERIOR,0,0  #   3 ||< external
*surf,kitch_r,VERT,-,-,-,partition,OPAQUE,ANOTHER,04,02  #   4 ||< store_r:kitchen
*surf,bed_2_r,CEIL,-,-,-,ceiling,OPAQUE,ANOTHER,08,08  #   5 ||< store_r:bed_2
*surf,floor,FLOR,-,-,-,grnd_floor,OPAQUE,GROUND,01,00  #   6 ||< ground profile  1
*surf,back_door,VERT,-,-,-,door,OPAQUE,EXTERIOR,0,0  #   7 ||< external
# 
*insol,1,6,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,6,     4.59 0  # zone base list
