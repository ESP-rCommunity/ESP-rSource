*Geometry 1.1,GEN,bed_3 # tag version, format, zone name
*date Sun Dec  8 13:46:26 2013  # latest file modification 
bed_3 describes room with moisture details
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,0.00000,3.10000,2.70000  #   1
*vertex,3.30000,3.10000,2.70000  #   2
*vertex,3.30000,6.33200,2.70000  #   3
*vertex,0.00000,6.33200,2.70000  #   4
*vertex,0.00000,3.10000,5.10000  #   5
*vertex,3.30000,3.10000,5.10000  #   6
*vertex,3.30000,6.33200,5.10000  #   7
*vertex,0.00000,6.33200,5.10000  #   8
*vertex,0.00000,4.70000,2.70000  #   9
*vertex,2.10000,4.70000,2.70000  #  10
*vertex,2.10000,6.33200,2.70000  #  11
*vertex,2.10000,3.10000,2.70000  #  12
*vertex,3.30000,3.70000,2.70000  #  13
*vertex,2.10000,3.70000,2.70000  #  14
*vertex,3.30000,6.33200,4.92500  #  15
*vertex,0.00000,6.33200,4.92500  #  16
*vertex,2.10000,6.33200,3.40000  #  17
*vertex,1.10000,6.33200,3.40000  #  18
*vertex,1.10000,6.33200,4.90000  #  19
*vertex,2.10000,6.33200,4.90000  #  20
*vertex,0.00000,4.60000,3.40000  #  21
*vertex,0.00000,3.50000,3.40000  #  22
*vertex,0.00000,3.50000,4.90000  #  23
*vertex,0.00000,4.60000,4.90000  #  24
# 
# tag, number of vertices followed by list of associated vert
*edges,5,1,12,2,6,5  #  1
*edges,6,2,13,3,15,7,6  #  2
*edges,11,3,11,4,16,15,3,17,20,19,18,17  #  3
*edges,12,1,5,8,16,4,9,1,21,24,23,22,21  #  4
*edges,4,5,6,7,8  #  5
*edges,5,13,14,10,11,3  #  6
*edges,4,10,9,4,11  #  7
*edges,5,12,1,9,10,14  #  8
*edges,4,2,12,14,13  #  9
*edges,4,15,16,8,7  # 10
*edges,4,17,18,19,20  # 11
*edges,4,21,22,23,24  # 12
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,hallup_r,VERT,-,-,-,partition,OPAQUE,ANOTHER,06,05  #   1 ||< bed_1:hall_up
*surf,bed_2_r,VERT,-,-,-,partition,OPAQUE,ANOTHER,08,04  #   2 ||< bed_3_r:bed_2
*surf,bed3_ex_n,VERT,-,-,-,air_gap_int,OPAQUE,ANOTHER,13,02  #   3 ||< xbed3_ex_n:bed_3_air_g
*surf,bed3_ex_w,VERT,-,-,-,ext_wall_up,OPAQUE,EXTERIOR,0,0  #   4 ||< external
*surf,loft_r,CEIL,-,-,-,ceil_loft,OPAQUE,ANOTHER,11,06  #   5 ||< bed3:loft
*surf,kitch_r,FLOR,-,-,-,ceiling_rev,OPAQUE,ANOTHER,04,08  #   6 ||< bed_1_r:kitchen
*surf,bath_r,FLOR,-,-,-,ceiling_rev,OPAQUE,ANOTHER,03,05  #   7 ||< bed_1_r:bathroom
*surf,hall_r,FLOR,-,-,-,ceiling_rev,OPAQUE,ANOTHER,02,05  #   8 ||< bed_3_r:hall
*surf,liv_r,FLOR,-,-,-,ceiling_rev,OPAQUE,ANOTHER,01,05  #   9 ||< bed_3_r:living_room
*surf,loft_n_r,VERT,-,-,-,t_bridge,OPAQUE,ANOTHER,10,01  #  10 ||< bed_3_r:t_bridge
*surf,n_glaz,VERT,-,-,-,glazing,SC_8985_04nb,EXTERIOR,0,0  #  11 ||< external
*surf,w_glaz,VERT,-,-,-,glazing,SC_8985_04nb,EXTERIOR,0,0  #  12 ||< external
# 
*insol,1,6,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,4,6,7,8,9,    10.67 0  # zone base list
