*Geometry 1.1,GEN,hall_up # tag version, format, zone name
*date Sun Dec  8 13:46:26 2013  # latest file modification 
hall_up describes the upper portion of the hall and stairs
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,0.00000,0.00000,2.70000  #   1
*vertex,2.30000,0.00000,2.70000  #   2
*vertex,2.30000,2.10000,2.70000  #   3
*vertex,3.30000,2.10000,2.70000  #   4
*vertex,3.30000,3.10000,2.70000  #   5
*vertex,0.00000,3.10000,2.70000  #   6
*vertex,0.00000,0.00000,5.10000  #   7
*vertex,2.30000,0.00000,5.10000  #   8
*vertex,2.30000,2.10000,5.10000  #   9
*vertex,3.30000,2.10000,5.10000  #  10
*vertex,3.30000,3.10000,5.10000  #  11
*vertex,0.00000,3.10000,5.10000  #  12
*vertex,2.30000,0.00000,4.92500  #  13
*vertex,0.00000,0.00000,4.92500  #  14
*vertex,2.10000,0.00000,2.70000  #  15
*vertex,2.10000,3.10000,2.70000  #  16
*vertex,1.00000,0.00000,3.40000  #  17
*vertex,2.10000,0.00000,3.40000  #  18
*vertex,2.10000,0.00000,4.80000  #  19
*vertex,1.00000,0.00000,4.80000  #  20
# 
# tag, number of vertices followed by list of associated vert
*edges,11,1,15,2,13,14,1,17,20,19,18,17  #  1
*edges,5,2,3,9,8,13  #  2
*edges,4,3,4,10,9  #  3
*edges,4,4,5,11,10  #  4
*edges,5,5,16,6,12,11  #  5
*edges,5,6,1,14,7,12  #  6
*edges,6,7,8,9,10,11,12  #  7
*edges,4,1,6,16,15  #  8
*edges,6,15,16,5,4,3,2  #  9
*edges,4,14,13,8,7  # 10
*edges,4,17,18,19,20  # 11
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,hallup_ex_s,VERT,-,-,-,ext_wall_up,OPAQUE,EXTERIOR,0,0  #   1 ||< external
*surf,bed_1_r,VERT,-,-,-,partition,OPAQUE,ANOTHER,07,06  #   2 ||< hallup_r_3:bed_1
*surf,bed_1_r2,VERT,-,-,-,partition,OPAQUE,ANOTHER,07,05  #   3 ||< hallup_r_2:bed_1
*surf,bed_1_r3,VERT,-,-,-,partition,OPAQUE,ANOTHER,07,04  #   4 ||< hallup_r_1:bed_1
*surf,bed_1,VERT,-,-,-,partition,OPAQUE,ANOTHER,09,01  #   5 ||< hallup_r:bed_3
*surf,hallup_ex_w,VERT,-,-,-,ext_wall_up,OPAQUE,EXTERIOR,0,0  #   6 ||< external
*surf,ceil,CEIL,-,-,-,ceil_loft,OPAQUE,ANOTHER,11,04  #   7 ||< hallup:loft
*surf,hall_r,FLOR,-,-,-,ceiling_rev,OPAQUE,ANOTHER,02,07  #   8 ||< hall_up_r:hall
*surf,liv_r,FLOR,-,-,-,ceiling_rev,OPAQUE,ANOTHER,01,07  #   9 ||< hall_up_r:living_room
*surf,loft,VERT,-,-,-,ext_wall_up,OPAQUE,ANOTHER,11,02  #  10 ||< hallup_sof:loft
*surf,s_glaz,VERT,-,-,-,glazing,SC_8985_04nb,EXTERIOR,0,0  #  11 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,2,8,9,     8.13 0  # zone base list
