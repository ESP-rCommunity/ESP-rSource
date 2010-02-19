*Geometry 1.1,GEN,roof # tag version, format, zone name
*date Tue Jul 21 13:27:29 2009  # latest file modification 
roof describes a...
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,1.00000,1.00000,2.70000  #   1
*vertex,5.50000,1.00000,2.70000  #   2
*vertex,5.50000,4.50000,2.70000  #   3
*vertex,1.00000,4.50000,2.70000  #   4
*vertex,10.00000,1.00000,2.70000  #   5
*vertex,14.50000,1.00000,2.70000  #   6
*vertex,19.00000,1.00000,2.70000  #   7
*vertex,19.00000,4.50000,2.70000  #   8
*vertex,19.00000,6.00000,2.70000  #   9
*vertex,1.00000,6.00000,2.70000  #  10
*vertex,14.50000,4.50000,2.70000  #  11
*vertex,10.00000,4.50000,2.70000  #  12
*vertex,1.00000,3.50000,5.00000  #  13
*vertex,19.00000,3.50000,5.00000  #  14
*vertex,10.00000,3.50000,5.00000  #  15
*vertex,2.00000,1.73600,3.37700  #  16
*vertex,8.60000,1.73600,3.37700  #  17
*vertex,8.60000,2.84000,4.39300  #  18
*vertex,2.00000,2.84000,4.39300  #  19
*vertex,11.00000,1.73600,3.37700  #  20
*vertex,17.60000,1.73600,3.37700  #  21
*vertex,17.60000,2.84000,4.39300  #  22
*vertex,11.00000,2.84000,4.39300  #  23
*vertex,13.20000,1.73600,3.37700  #  24
*vertex,15.40000,1.73600,3.37700  #  25
*vertex,15.40000,2.84000,4.39300  #  26
*vertex,13.20000,2.84000,4.39300  #  27
# 
# tag, number of vertices followed by list of associated vert
*edges,4,2,1,4,3  #  1
*edges,4,5,2,3,12  #  2
*edges,4,6,5,12,11  #  3
*edges,4,7,6,11,8  #  4
*edges,7,8,11,12,3,4,10,9  #  5
*edges,11,1,2,5,15,13,1,16,19,18,17,16  #  6
*edges,5,9,10,13,15,14  #  7
*edges,4,10,4,1,13  #  8
*edges,4,7,8,9,14  #  9
*edges,15,5,6,7,14,15,5,20,23,27,26,22,21,25,24,20  # 10
*edges,4,16,17,18,19  # 11
*edges,4,20,24,27,23  # 12
*edges,4,24,25,26,27  # 13
*edges,4,25,21,22,26  # 14
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,ceil_dg,FLOR,-,-,-,ceiling_rev,OPAQUE,ANOTHER,01,05  #   1 ||< ceiling:direct_gain
*surf,ceil_rad,FLOR,-,-,-,ceiling_rev,OPAQUE,ANOTHER,02,05  #   2 ||< ceiling:rad_test
*surf,ceil_exch,FLOR,-,-,-,ceiling_rev,OPAQUE,ANOTHER,03,05  #   3 ||< ceiling:heatexch
*surf,ceil_mix,FLOR,-,-,-,ceiling_rev,OPAQUE,ANOTHER,04,05  #   4 ||< ceiling:mixed_test
*surf,ceil_pasg,FLOR,-,-,-,ceiling_rev,OPAQUE,ANOTHER,05,08  #   5 ||< ceil:passage
*surf,south_rfl,SLOP,-,-,-,roof,OPAQUE,EXTERIOR,0,0  #   6 ||< external
*surf,north_rf,SLOP,-,-,-,roof,OPAQUE,EXTERIOR,0,0  #   7 ||< external
*surf,left_rf,VERT,-,-,-,ext_wall,OPAQUE,EXTERIOR,0,0  #   8 ||< external
*surf,right_rf,VERT,-,-,-,ext_wall,OPAQUE,EXTERIOR,0,0  #   9 ||< external
*surf,south_rfr,SLOP,-,-,-,roof,OPAQUE,EXTERIOR,0,0  #  10 ||< external
*surf,liqd_col,SLOP,-,-,-,colct_frame,OPAQUE,EXTERIOR,0,0  #  11 ||< external
*surf,air_l,SLOP,-,-,-,colct_frame,OPAQUE,ANOTHER,07,01  #  12 ||< base:air_col_left
*surf,air_m,SLOP,-,-,-,colct_frame,OPAQUE,ANOTHER,08,01  #  13 ||< base:air_col_mid
*surf,air_r,SLOP,-,-,-,colct_frame,OPAQUE,ANOTHER,09,01  #  14 ||< base:air_col_righ
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,5,1,2,3,4,5,    90.00 0  # zone base list
