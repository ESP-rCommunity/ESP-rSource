*Geometry 1.1,GEN,bed_1 # tag version, format, zone name
*date Sun Dec  8 13:54:50 2013  # latest file modification 
bed_1 describes a south facing bedroom
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,2.30000,0.00000,2.70000  #   1
*vertex,6.90000,0.00000,2.70000  #   2
*vertex,6.90000,3.10000,2.70000  #   3
*vertex,3.30000,3.10000,2.70000  #   4
*vertex,3.30000,2.10000,2.70000  #   5
*vertex,2.30000,2.10000,2.70000  #   6
*vertex,2.30000,0.00000,5.10000  #   7
*vertex,6.90000,0.00000,5.10000  #   8
*vertex,6.90000,3.10000,5.10000  #   9
*vertex,3.30000,3.10000,5.10000  #  10
*vertex,3.30000,2.10000,5.10000  #  11
*vertex,2.30000,2.10000,5.10000  #  12
*vertex,2.30000,0.00000,4.92500  #  13
*vertex,6.90000,0.00000,4.92500  #  14
*vertex,3.40000,0.00000,3.40000  #  15
*vertex,5.60000,0.00000,3.40000  #  16
*vertex,5.60000,0.00000,4.90000  #  17
*vertex,3.40000,0.00000,4.90000  #  18
# 
# tag, number of vertices followed by list of associated vert
*edges,10,1,2,14,13,1,15,18,17,16,15  #  1
*edges,5,2,3,9,8,14  #  2
*edges,4,3,4,10,9  #  3
*edges,4,4,5,11,10  #  4
*edges,4,5,6,12,11  #  5
*edges,5,6,1,13,7,12  #  6
*edges,6,7,8,9,10,11,12  #  7
*edges,6,6,5,4,3,2,1  #  8
*edges,4,13,14,8,7  #  9
*edges,4,15,16,17,18  # 10
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,bed1_ex_s,VERT,-,-,-,ext_wall_up,OPAQUE,EXTERIOR,0,0  #   1 ||< external
*surf,bed1_sim_e,VERT,-,-,-,intern_wall,OPAQUE,SIMILAR,00,00  #   2 ||< identical environment
*surf,bed_2_r,VERT,-,-,-,partition,OPAQUE,ANOTHER,08,01  #   3 ||< bed1_r:bed_2
*surf,hallup_r_1,VERT,-,-,-,partition,OPAQUE,ANOTHER,06,04  #   4 ||< bed_1_r3:hall_up
*surf,hallup_r_2,VERT,-,-,-,partition,OPAQUE,ANOTHER,06,03  #   5 ||< bed_1_r2:hall_up
*surf,hallup_r_3,VERT,-,-,-,partition,OPAQUE,ANOTHER,06,02  #   6 ||< bed_1_r:hall_up
*surf,loft_r,CEIL,-,-,-,ceil_loft,OPAQUE,ANOTHER,11,05  #   7 ||< bed_1:loft
*surf,liv_r,FLOR,-,-,-,ceiling_rev,OPAQUE,ANOTHER,01,08  #   8 ||< bed_1_r:living_room
*surf,loft_s_r,VERT,-,-,-,ext_wall_up,OPAQUE,ANOTHER,11,03  #   9 ||< bed1_sof:loft
*surf,s_glaz,VERT,-,-,-,glazing,DC_8074_04nb,EXTERIOR,0,0  #  10 ||< external
# 
*insol,1,8,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,8,    13.26 0  # zone base list
