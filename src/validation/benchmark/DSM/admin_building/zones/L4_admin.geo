*Geometry 1.1,GEN,L4_admin # tag version, format, zone name
*date Mon Aug 15 16:08:06 2011  # latest file modification 
L4_admin describes mix of office types at srv facade level four
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,12.00000,35.70000,60.25000  #   1
*vertex,17.38456,35.70000,60.25000  #   2
*vertex,24.03439,35.70000,60.25000  #   3
*vertex,24.08189,40.70000,60.25000  #   4
*vertex,23.84940,40.70000,60.25000  #   5
*vertex,21.84944,40.70000,60.25000  #   6
*vertex,20.84900,40.70000,60.25000  #   7
*vertex,19.84900,40.70000,60.25000  #   8
*vertex,16.84900,40.70000,60.25000  #   9
*vertex,14.84900,40.70000,60.25000  #  10
*vertex,13.84900,40.70000,60.25000  #  11
*vertex,12.00000,40.70000,60.25000  #  12
*vertex,12.00000,35.70000,63.39300  #  13
*vertex,17.38456,35.70000,63.39300  #  14
*vertex,24.03439,35.70000,63.39300  #  15
*vertex,24.08189,40.70000,63.39300  #  16
*vertex,23.84940,40.70000,63.39300  #  17
*vertex,21.84944,40.70000,63.39300  #  18
*vertex,20.84900,40.70000,63.39300  #  19
*vertex,19.84900,40.70000,63.39300  #  20
*vertex,16.84900,40.70000,63.39300  #  21
*vertex,14.84900,40.70000,63.39300  #  22
*vertex,13.84900,40.70000,63.39300  #  23
*vertex,12.00000,40.70000,63.39300  #  24
*vertex,12.84900,40.70000,60.25000  #  25
*vertex,12.84900,40.70000,63.39300  #  26
*vertex,23.84940,40.70000,61.36000  #  27
*vertex,21.84944,40.70000,61.36000  #  28
*vertex,20.84900,40.70000,61.36000  #  29
*vertex,19.84900,40.70000,61.36000  #  30
*vertex,16.84900,40.70000,61.36000  #  31
*vertex,14.84900,40.70000,61.36000  #  32
*vertex,13.84900,40.70000,61.36000  #  33
*vertex,12.84900,40.70000,61.36000  #  34
*vertex,23.84940,40.70000,62.50000  #  35
*vertex,21.84944,40.70000,62.50000  #  36
*vertex,20.84900,40.70000,62.50000  #  37
*vertex,19.84900,40.70000,62.50000  #  38
*vertex,16.84900,40.70000,62.50000  #  39
*vertex,14.84900,40.70000,62.50000  #  40
*vertex,13.84900,40.70000,62.50000  #  41
*vertex,12.84900,40.70000,62.50000  #  42
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,14,13  #  1
*edges,4,2,3,15,14  #  2
*edges,4,3,4,16,15  #  3
*edges,6,4,5,27,35,17,16  #  4
*edges,4,27,28,36,35  #  5
*edges,8,6,7,29,37,19,18,36,28  #  6
*edges,4,29,30,38,37  #  7
*edges,8,8,9,31,39,21,20,38,30  #  8
*edges,4,31,32,40,39  #  9
*edges,8,10,11,33,41,23,22,40,32  # 10
*edges,4,33,34,42,41  # 11
*edges,4,12,1,13,24  # 12
*edges,13,13,14,15,16,17,18,19,20,21,22,23,26,24  # 13
*edges,13,1,12,25,11,10,9,8,7,6,5,4,3,2  # 14
*edges,6,25,12,24,26,42,34  # 15
*edges,4,5,6,28,27  # 16
*edges,4,35,36,18,17  # 17
*edges,4,7,8,30,29  # 18
*edges,4,37,38,20,19  # 19
*edges,4,9,10,32,31  # 20
*edges,4,39,40,22,21  # 21
*edges,4,11,25,34,33  # 22
*edges,4,41,42,26,23  # 23
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,ptn_corid_a,VERT,-,-,-,gyp_aco_gyp,OPAQUE,UNKNOWN,0,0  #   1 ||< external
*surf,ptn_corid_b,VERT,-,-,-,gyp_aco_gyp,OPAQUE,UNKNOWN,0,0  #   2 ||< external
*surf,ptn_print,VERT,-,-,-,gyp_aco_gyp,OPAQUE,UNKNOWN,0,0  #   3 ||< external
*surf,stone_c,VERT,-,-,-,stone_gyp,OPAQUE,EXTERIOR,0,0  #   4 ||< external
*surf,glaz_d,VERT,-,-,-,db_lpasol,DSF4554_06nb,EXTERIOR,0,0  #   5 ||< external
*surf,vent_b,VERT,-,-,-,stone_vent,OPAQUE,EXTERIOR,0,0  #   6 ||< external
*surf,glaz_c,VERT,-,-,-,db_lpasol,DSF4554_06nb,EXTERIOR,0,0  #   7 ||< external
*surf,stone_b,VERT,-,-,-,stone_gyp,OPAQUE,EXTERIOR,0,0  #   8 ||< external
*surf,glaz_b,VERT,-,-,-,db_lpasol,DSF4554_06nb,EXTERIOR,0,0  #   9 ||< external
*surf,vent_a,VERT,-,-,-,stone_vent,OPAQUE,EXTERIOR,0,0  #  10 ||< external
*surf,glaz_a,VERT,-,-,-,db_lpasol,DSF4554_06nb,EXTERIOR,0,0  #  11 ||< external
*surf,ptn_toilet,VERT,-,-,-,gyp_blk_ptn,OPAQUE,UNKNOWN,0,0  #  12 ||< external
*surf,Top-13,CEIL,-,-,-,slab_275,OPAQUE,EXTERIOR,0,0  #  13 ||< external
*surf,Base-14,FLOR,-,-,-,flr_ov_pln,OPAQUE,UNKNOWN,0,0  #  14 ||< external
*surf,stone_a,VERT,-,-,-,stone_masnry,OPAQUE,EXTERIOR,0,0  #  15 ||< external
*surf,glaz_d_low,VERT,-,-,-,db_lpasol,DSF4554_06nb,EXTERIOR,0,0  #  16 ||< external
*surf,glaz_d_hi,VERT,-,-,-,db_lpasol,DSF4554_06nb,EXTERIOR,0,0  #  17 ||< external
*surf,glaz_c_low,VERT,-,-,-,db_lpasol,DSF4554_06nb,EXTERIOR,0,0  #  18 ||< external
*surf,glaz_c_hi,VERT,-,-,-,db_lpasol,DSF4554_06nb,EXTERIOR,0,0  #  19 ||< external
*surf,glaz_b_low,VERT,-,-,-,db_lpasol,DSF4554_06nb,EXTERIOR,0,0  #  20 ||< external
*surf,glaz_b_hi,VERT,-,-,-,db_lpasol,DSF4554_06nb,EXTERIOR,0,0  #  21 ||< external
*surf,glaz_a_low,VERT,-,-,-,db_lpasol,DSF4554_06nb,EXTERIOR,0,0  #  22 ||< external
*surf,glaz_a_hi,VERT,-,-,-,db_lpasol,DSF4554_06nb,EXTERIOR,0,0  #  23 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,14,    60.29 0  # zone base list
