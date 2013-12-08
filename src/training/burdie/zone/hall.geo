*Geometry 1.1,GEN,hall # tag version, format, zone name
*date Sun Dec  8 13:46:26 2013  # latest file modification 
hall describes the lower portion of the entry
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,0.00000,0.00000,0.00000  #   1
*vertex,2.10000,0.00000,0.00000  #   2
*vertex,2.10000,4.70000,0.00000  #   3
*vertex,0.00000,4.70000,0.00000  #   4
*vertex,0.00000,0.00000,2.70000  #   5
*vertex,2.10000,0.00000,2.70000  #   6
*vertex,2.10000,4.70000,2.70000  #   7
*vertex,0.00000,4.70000,2.70000  #   8
*vertex,0.00000,3.10000,2.70000  #   9
*vertex,2.10000,3.10000,2.70000  #  10
*vertex,2.10000,3.70000,0.00000  #  11
*vertex,2.10000,3.70000,2.70000  #  12
*vertex,1.00000,0.00000,0.00000  #  13
*vertex,2.00000,0.00000,0.00000  #  14
*vertex,2.00000,0.00000,2.00000  #  15
*vertex,1.00000,0.00000,2.00000  #  16
*vertex,0.00000,4.40000,0.90000  #  17
*vertex,0.00000,3.30000,0.90000  #  18
*vertex,0.00000,3.30000,2.20000  #  19
*vertex,0.00000,4.40000,2.20000  #  20
# 
# tag, number of vertices followed by list of associated vert
*edges,8,1,13,16,15,14,2,6,5  #  1
*edges,5,2,11,12,10,6  #  2
*edges,4,3,4,8,7  #  3
*edges,11,4,1,5,9,8,4,17,20,19,18,17  #  4
*edges,5,9,10,12,7,8  #  5
*edges,7,4,3,11,2,14,13,1  #  6
*edges,4,10,9,5,6  #  7
*edges,4,11,3,7,12  #  8
*edges,4,13,14,15,16  #  9
*edges,4,17,18,19,20  # 10
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,hall_ex_s,VERT,-,-,-,ext_wall_low,OPAQUE,EXTERIOR,0,0  #   1 ||< external
*surf,livrm_r,VERT,-,-,-,partition,OPAQUE,ANOTHER,01,04  #   2 ||< hall_r:living_room
*surf,bathrm_r,VERT,-,-,-,partition,OPAQUE,ANOTHER,03,01  #   3 ||< hall_r:bathroom
*surf,hall_ex_w,VERT,-,-,-,ext_wall_low,OPAQUE,EXTERIOR,0,0  #   4 ||< external
*surf,bed_3_r,CEIL,-,-,-,ceiling,OPAQUE,ANOTHER,09,08  #   5 ||< hall_r:bed_3
*surf,floor,FLOR,-,-,-,grnd_floor,OPAQUE,GROUND,01,00  #   6 ||< ground profile  1
*surf,hall_up_r,CEIL,-,-,-,ceiling,OPAQUE,ANOTHER,06,08  #   7 ||< hall_r:hall_up
*surf,kitchrev,VERT,-,-,-,partition,OPAQUE,ANOTHER,04,04  #   8 ||< hall_r:kitchen
*surf,front_door,VERT,-,-,-,door,OPAQUE,EXTERIOR,0,0  #   9 ||< external
*surf,w_glaz,VERT,-,-,-,glazing,SC_8985_04nb,EXTERIOR,0,0  #  10 ||< external
# 
*insol,1,6,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,6,     9.87 0  # zone base list
# 
# block entities:
#  *obs = obstructions
*block_start, 15 15 # geometric blocks
*obs,0.000,-1.000,2.400,2.100,1.000,0.100,0.000,1.00,First,NONE  # block   1
*end_block
