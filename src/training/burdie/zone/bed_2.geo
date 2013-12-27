*Geometry 1.1,GEN,bed_2 # tag version, format, zone name
*date Sun Dec  8 13:46:26 2013  # latest file modification 
bed_2 describes a north facing bedroom
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,3.30000,3.10000,2.70000  #   1
*vertex,6.90000,3.10000,2.70000  #   2
*vertex,6.90000,6.40000,2.70000  #   3
*vertex,3.30000,6.40000,2.70000  #   4
*vertex,3.30000,3.10000,5.10000  #   5
*vertex,6.90000,3.10000,5.10000  #   6
*vertex,6.90000,6.40000,5.10000  #   7
*vertex,3.30000,6.40000,5.10000  #   8
*vertex,6.90000,6.40000,4.92500  #   9
*vertex,3.30000,6.40000,4.92500  #  10
*vertex,5.20000,3.70000,2.70000  #  11
*vertex,6.90000,3.70000,2.70000  #  12
*vertex,5.20000,6.40000,2.70000  #  13
*vertex,3.30000,3.70000,2.70000  #  14
*vertex,5.60000,6.40000,3.40000  #  15
*vertex,3.40000,6.40000,3.40000  #  16
*vertex,3.40000,6.40000,4.90000  #  17
*vertex,5.60000,6.40000,4.90000  #  18
*vertex,3.30000,6.33600,4.92500  #  19
*vertex,3.30000,6.33600,2.70000  #  20
*vertex,3.30000,6.33600,5.10000  #  21
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,6,5  #  1
*edges,6,2,12,3,9,7,6  #  2
*edges,11,3,13,4,10,9,3,15,18,17,16,15  #  3
*edges,6,20,14,1,5,21,19  #  4
*edges,5,5,6,7,8,21  #  5
*edges,5,14,11,12,2,1  #  6
*edges,4,7,9,10,8  #  7
*edges,4,12,11,13,3  #  8
*edges,5,11,14,20,4,13  #  9
*edges,4,15,16,17,18  # 10
*edges,4,19,10,4,20  # 11
*edges,4,10,19,21,8  # 12
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,bed1_r,VERT,-,-,-,partition,OPAQUE,ANOTHER,07,03  #   1 ||< bed_2_r:bed_1
*surf,bed2_sim_e,VERT,-,-,-,intern_wall,OPAQUE,SIMILAR,00,00  #   2 ||< identical environment
*surf,bed2_ex_n,VERT,-,-,-,ext_wall_up,OPAQUE,EXTERIOR,0,0  #   3 ||< external
*surf,bed_3_r,VERT,-,-,-,partition,OPAQUE,ANOTHER,09,02  #   4 ||< bed_2_r:bed_3
*surf,loft_r,CEIL,-,-,-,ceil_loft,OPAQUE,ANOTHER,11,07  #   5 ||< bed2:loft
*surf,liv_r,FLOR,-,-,-,ceiling_rev,OPAQUE,ANOTHER,01,10  #   6 ||< bed_2_r:living_room
*surf,loft_n_r,VERT,-,-,-,ext_wall_up,OPAQUE,ANOTHER,11,09  #   7 ||< bed2_sof:loft
*surf,store_r,FLOR,-,-,-,ceiling_rev,OPAQUE,ANOTHER,05,05  #   8 ||< bed_2_r:store
*surf,kitch_r,FLOR,-,-,-,ceiling_rev,OPAQUE,ANOTHER,04,05  #   9 ||< bed_2_r:kitchen
*surf,n_glaz,VERT,-,-,-,glazing,SC_8985_04nb,EXTERIOR,0,0  #  10 ||< external
*surf,xSurf-10,VERT,-,-,-,partition,OPAQUE,ANOTHER,13,10  #  11 ||< Surf-10:bed_3_air_g
*surf,xbed_2_r,VERT,-,-,-,partition,OPAQUE,ANOTHER,10,02  #  12 ||< bed_2_r:t_bridge
# 
*insol,1,6,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,3,6,8,9,    11.88 0  # zone base list
