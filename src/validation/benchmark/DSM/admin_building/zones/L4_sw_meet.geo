*Geometry 1.1,GEN,L4_sw_meet # tag version, format, zone name
*date Sat Aug 13 16:10:54 2011  # latest file modification 
L4_sw_meet describes meeting rds 406 407 on level four
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,0.00000,7.25000,60.25000  #   1
*vertex,4.50000,8.25000,60.25000  #   2
*vertex,4.50000,14.00000,60.25000  #   3
*vertex,4.50000,21.00000,60.25000  #   4
*vertex,4.75000,24.00000,60.25000  #   5
*vertex,12.00000,24.25000,60.25000  #   6
*vertex,12.00000,30.50000,60.25000  #   7
*vertex,0.00000,30.50000,60.25000  #   8
*vertex,0.00000,28.50000,60.25000  #   9
*vertex,3.25000,28.50000,60.25000  #  10
*vertex,3.25000,24.00000,60.25000  #  11
*vertex,0.00000,24.00000,60.25000  #  12
*vertex,0.00000,21.00000,60.25000  #  13
*vertex,0.00000,14.00000,60.25000  #  14
*vertex,0.00000,7.25000,63.39300  #  15
*vertex,4.50000,8.25000,63.39300  #  16
*vertex,4.50000,14.00000,63.39300  #  17
*vertex,4.50000,21.00000,63.39300  #  18
*vertex,4.75000,24.00000,63.39300  #  19
*vertex,12.00000,24.25000,63.39300  #  20
*vertex,12.00000,30.50000,63.39300  #  21
*vertex,0.00000,30.50000,63.39300  #  22
*vertex,0.00000,28.50000,63.39300  #  23
*vertex,3.25000,28.50000,63.39300  #  24
*vertex,3.25000,24.00000,63.39300  #  25
*vertex,0.00000,24.00000,63.39300  #  26
*vertex,0.00000,21.00000,63.39300  #  27
*vertex,0.00000,14.00000,63.39300  #  28
*vertex,8.35979,30.50000,60.25000  #  29
*vertex,8.35979,30.50000,63.39300  #  30
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,16,15  #  1
*edges,4,2,3,17,16  #  2
*edges,4,3,4,18,17  #  3
*edges,4,4,5,19,18  #  4
*edges,4,5,6,20,19  #  5
*edges,4,6,7,21,20  #  6
*edges,4,8,9,23,22  #  7
*edges,4,9,10,24,23  #  8
*edges,4,10,11,25,24  #  9
*edges,4,11,12,26,25  # 10
*edges,4,12,13,27,26  # 11
*edges,4,13,14,28,27  # 12
*edges,4,14,1,15,28  # 13
*edges,15,15,16,17,18,19,20,21,30,22,23,24,25,26,27,28  # 14
*edges,15,1,14,13,12,11,10,9,8,29,7,6,5,4,3,2  # 15
*edges,4,29,8,22,30  # 16
*edges,4,7,29,30,21  # 17
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,Wall-1,VERT,-,-,-,gyp_aco_gyp,OPAQUE,UNKNOWN,0,0  #   1 ||< not yet defined
*surf,Wall-2,VERT,-,-,-,gyp_aco_gyp,OPAQUE,UNKNOWN,0,0  #   2 ||< not yet defined
*surf,Wall-3,VERT,-,-,-,glaz_ptn,SCF8477_10nb,UNKNOWN,0,0  #   3 ||< not yet defined
*surf,Wall-4,VERT,-,-,-,gyp_aco_gyp,OPAQUE,UNKNOWN,0,0  #   4 ||< not yet defined
*surf,Wall-5,VERT,-,-,-,gyp_aco_gyp,OPAQUE,UNKNOWN,0,0  #   5 ||< not yet defined
*surf,Wall-6,VERT,-,-,-,gyp_aco_gyp,OPAQUE,UNKNOWN,0,0  #   6 ||< not yet defined
*surf,Wall-8,VERT,-,-,-,glaz_ptn,SCF8477_10nb,UNKNOWN,0,0  #   7 ||< not yet defined
*surf,Wall-9,VERT,-,-,-,conc_250,OPAQUE,UNKNOWN,0,0  #   8 ||< not yet defined
*surf,Wall-10,VERT,-,-,-,conc_250,OPAQUE,UNKNOWN,0,0  #   9 ||< not yet defined
*surf,Wall-11,VERT,-,-,-,conc_250,OPAQUE,UNKNOWN,0,0  #  10 ||< not yet defined
*surf,Wall-12,VERT,-,-,-,glaz_ptn,SCF8477_10nb,UNKNOWN,0,0  #  11 ||< not yet defined
*surf,Wall-13,VERT,-,-,-,glaz_ptn,SCF8477_10nb,UNKNOWN,0,0  #  12 ||< not yet defined
*surf,Wall-14,VERT,-,-,-,glaz_ptn,SCF8477_10nb,UNKNOWN,0,0  #  13 ||< not yet defined
*surf,Top-15,CEIL,-,-,-,slab_275,OPAQUE,UNKNOWN,0,0  #  14 ||< not yet defined
*surf,Base-16,FLOR,-,-,-,flr_ov_pln,OPAQUE,UNKNOWN,0,0  #  15 ||< not yet defined
*surf,ptn_cora_l4,VERT,-,-,-,conc_250,OPAQUE,UNKNOWN,0,0  #  16 ||< external
*surf,ptn_corb_l4,VERT,-,-,-,conc_250,OPAQUE,UNKNOWN,-,-  #  17 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,15,   135.97 0  # zone base list
