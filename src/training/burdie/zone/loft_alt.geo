*Geometry 1.1,GEN,loft # tag version, format, zone name
*date Sun Dec  8 13:54:50 2013  # latest file modification 
loft describes the roof space over all rooms
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,0.00000,-0.48000,4.92500  #   1
*vertex,6.90000,-0.48000,4.92500  #   2
*vertex,6.90000,0.00000,4.92500  #   3
*vertex,0.00000,0.00000,4.92500  #   4
*vertex,0.00000,0.00000,5.10000  #   5
*vertex,2.30000,0.00000,4.92500  #   6
*vertex,2.30000,0.00000,5.10000  #   7
*vertex,6.90000,0.00000,5.10000  #   8
*vertex,2.30000,2.10000,5.10000  #   9
*vertex,3.30000,2.10000,5.10000  #  10
*vertex,3.30000,3.10000,5.10000  #  11
*vertex,0.00000,3.10000,5.10000  #  12
*vertex,6.90000,3.10000,5.10000  #  13
*vertex,3.30000,6.40000,5.10000  #  14
*vertex,0.00000,6.40000,5.10000  #  15
*vertex,6.90000,6.40000,5.10000  #  16
*vertex,0.00000,6.88000,4.92500  #  17
*vertex,6.90000,6.88000,4.92500  #  18
*vertex,0.00000,3.45000,6.60000  #  19
*vertex,6.90000,3.45000,6.60000  #  20
*vertex,3.30000,6.40000,4.92500  #  21
*vertex,0.00000,6.40000,4.92500  #  22
*vertex,6.90000,6.40000,4.92500  #  23
*vertex,3.30000,6.33600,5.10000  #  24
*vertex,0.00000,6.33500,5.10000  #  25
*vertex,1.50000,0.89990,5.51312  #  26
*vertex,4.00000,0.89990,5.51312  #  27
*vertex,4.00000,2.73976,6.29729  #  28
*vertex,1.50000,2.73976,6.29729  #  29
*vertex,1.50000,0.91990,5.46712  #  30
*vertex,4.00000,0.91990,5.46712  #  31
*vertex,4.00000,2.75976,6.25129  #  32
*vertex,1.50000,2.75976,6.25129  #  33
# 
# tag, number of vertices followed by list of associated vert
*edges,5,2,1,4,6,3  #  1
*edges,4,5,7,6,4  #  2
*edges,4,7,8,3,6  #  3
*edges,6,7,5,12,11,10,9  #  4
*edges,6,8,7,9,10,11,13  #  5
*edges,4,11,12,25,24  #  6
*edges,5,13,11,24,14,16  #  7
*edges,4,22,21,14,15  #  8
*edges,4,23,16,14,21  #  9
*edges,5,17,18,23,21,22  # 10
*edges,10,1,2,20,19,1,26,29,28,27,26  # 11
*edges,4,17,19,20,18  # 12
*edges,9,1,19,17,22,15,25,12,5,4  # 13
*edges,8,2,3,8,13,16,23,18,20  # 14
*edges,4,15,14,24,25  # 15
*edges,4,30,31,32,33  # 16
*edges,4,31,27,28,32  # 17
*edges,4,32,28,29,33  # 18
*edges,4,29,26,30,33  # 19
*edges,4,26,27,31,30  # 20
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,soffit_s,FLOR,-,-,-,soffit,OPAQUE,EXTERIOR,0,0  #   1 ||< external
*surf,hallup_sof,VERT,-,-,-,ext_wall_upr,OPAQUE,ANOTHER,06,10  #   2 ||< loft:hall_up
*surf,bed1_sof,VERT,-,-,-,ext_wall_upr,OPAQUE,ANOTHER,07,09  #   3 ||< loft_s_r:bed_1
*surf,hallup,FLOR,-,-,-,ceil_loft_r,OPAQUE,ANOTHER,06,07  #   4 ||< ceil:hall_up
*surf,bed_1,FLOR,-,-,-,ceil_loft_r,OPAQUE,ANOTHER,07,07  #   5 ||< loft_r:bed_1
*surf,bed3,FLOR,-,-,-,ceil_loft_r,OPAQUE,ANOTHER,09,05  #   6 ||< loft_r:bed_3
*surf,bed2,FLOR,-,-,-,ceil_loft_r,OPAQUE,ANOTHER,08,05  #   7 ||< loft_r:bed_2
*surf,bed3_sof,VERT,-,-,-,air_gap_extr,OPAQUE,ANOTHER,10,03  #   8 ||< loft_n_r:t_bridge
*surf,bed2_sof,VERT,-,-,-,ext_wall_upr,OPAQUE,ANOTHER,08,07  #   9 ||< loft_n_r:bed_2
*surf,soffit_n,FLOR,-,-,-,soffit,OPAQUE,EXTERIOR,0,0  #  10 ||< external
*surf,roof_s,SLOP,-,-,-,roof,OPAQUE,EXTERIOR,0,0  #  11 ||< external
*surf,roof_n,SLOP,-,-,-,roof,OPAQUE,EXTERIOR,0,0  #  12 ||< external
*surf,roof_v_ext,VERT,-,-,-,steel_sheet,OPAQUE,EXTERIOR,0,0  #  13 ||< external
*surf,Surf-14,VERT,-,-,-,intern_wall,OPAQUE,SIMILAR,00,00  #  14 ||< identical environment
*surf,t_bridge,FLOR,-,-,-,ceil_loft_r,OPAQUE,ANOTHER,10,05  #  15 ||< loft_up:t_bridge
*surf,xcollector,SLOP,-,-,-,collector,OPAQUE,ANOTHER,12,02  #  16 ||< xxcollector:collector
*surf,coll-e,VERT,-,-,-,door,OPAQUE,ANOTHER,12,03  #  17 ||< xcoll-e:collector
*surf,coll-up,SLOP,-,-,-,door,OPAQUE,ANOTHER,12,04  #  18 ||< xcoll-up:collector
*surf,coll-w,VERT,-,-,-,door,OPAQUE,ANOTHER,12,05  #  19 ||< xcoll-w:collector
*surf,coll-dn,SLOP,-,-,-,door,OPAQUE,ANOTHER,12,06  #  20 ||< xcoll-dn:collector
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,6,1,4,5,6,7,10,    50.57 0  # zone base list
