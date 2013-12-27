*Geometry 1.1,GEN,kitchen # tag version, format, zone name
*date Sun Dec  8 13:46:26 2013  # latest file modification 
kitchen describes the north facing kitchen
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,2.10000,3.70000,0.00000  #   1
*vertex,5.20000,3.70000,0.00000  #   2
*vertex,5.20000,6.40000,0.00000  #   3
*vertex,2.10000,6.40000,0.00000  #   4
*vertex,2.10000,3.70000,2.70000  #   5
*vertex,5.20000,3.70000,2.70000  #   6
*vertex,5.20000,6.40000,2.70000  #   7
*vertex,2.10000,6.40000,2.70000  #   8
*vertex,2.10000,4.70000,0.00000  #   9
*vertex,2.10000,4.70000,2.70000  #  10
*vertex,3.30000,3.70000,2.70000  #  11
*vertex,3.30000,6.40000,2.70000  #  12
*vertex,5.10000,6.40000,0.70000  #  13
*vertex,2.90000,6.40000,0.70000  #  14
*vertex,2.90000,6.40000,2.30000  #  15
*vertex,5.10000,6.40000,2.30000  #  16
*vertex,2.10000,6.33600,2.70000  #  17
*vertex,3.30000,6.33600,2.70000  #  18
# 
# tag, number of vertices followed by list of associated vert
*edges,5,1,2,6,11,5  #  1
*edges,4,2,3,7,6  #  2
*edges,11,3,4,8,12,7,3,13,16,15,14,13  #  3
*edges,4,9,1,5,10  #  4
*edges,5,11,6,7,12,18  #  5
*edges,5,4,3,2,1,9  #  6
*edges,5,4,9,10,17,8  #  7
*edges,5,5,11,18,17,10  #  8
*edges,4,13,14,15,16  #  9
*edges,4,8,17,18,12  # 10
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,liv_r,VERT,-,-,-,partition,OPAQUE,ANOTHER,01,03  #   1 ||< kitch_r:living_room
*surf,store_r,VERT,-,-,-,partition,OPAQUE,ANOTHER,05,04  #   2 ||< kitch_r:store
*surf,kitch_ex_n,VERT,-,-,-,ext_wall_low,OPAQUE,EXTERIOR,0,0  #   3 ||< external
*surf,hall_r,VERT,-,-,-,partition,OPAQUE,ANOTHER,02,08  #   4 ||< kitchrev:hall
*surf,bed_2_r,CEIL,-,-,-,ceiling,OPAQUE,ANOTHER,08,09  #   5 ||< kitch_r:bed_2
*surf,floor,FLOR,-,-,-,grnd_floor,OPAQUE,GROUND,01,00  #   6 ||< ground profile  1
*surf,bathrm_r,VERT,-,-,-,partition,OPAQUE,ANOTHER,03,02  #   7 ||< kitch_r:bathroom
*surf,bed_1_r,CEIL,-,-,-,ceiling,OPAQUE,ANOTHER,09,06  #   8 ||< kitch_r:bed_3
*surf,n_glaz,VERT,-,-,-,glazing,SC_8985_04nb,EXTERIOR,0,0  #   9 ||< external
*surf,xfirst,CEIL,-,-,-,ceiling,OPAQUE,ANOTHER,13,01  #  10 ||< first:bed_3_air_g
# 
*insol,1,6,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,6,     8.37 0  # zone base list
