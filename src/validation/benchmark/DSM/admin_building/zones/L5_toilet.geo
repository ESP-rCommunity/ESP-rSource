*Geometry 1.1,GEN,L5_toilet # tag version, format, zone name
*date Wed Aug 17 06:44:11 2011  # latest file modification 
L5_toilet describes toilet adjacent to stair one on level 5
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,13.25000,30.50000,64.15000  #   1
*vertex,17.00000,30.50000,64.15000  #   2
*vertex,17.00000,36.00000,64.15000  #   3
*vertex,16.25000,36.00000,64.15000  #   4
*vertex,13.75000,36.00000,64.15000  #   5
*vertex,12.00000,36.00000,64.15000  #   6
*vertex,13.25000,30.50000,67.37500  #   7
*vertex,17.00000,30.50000,67.37500  #   8
*vertex,17.00000,36.00000,67.37500  #   9
*vertex,16.25000,36.00000,67.37500  #  10
*vertex,13.75000,36.00000,67.37500  #  11
*vertex,12.00000,36.00000,67.37500  #  12
*vertex,16.96041,36.00000,64.32024  #  13
*vertex,16.28959,36.00000,64.32024  #  14
*vertex,16.28959,36.00000,67.20477  #  15
*vertex,16.96041,36.00000,67.20477  #  16
*vertex,12.00000,30.50000,64.15000  #  17
*vertex,12.00000,35.70000,64.15000  #  18
*vertex,12.00000,35.70000,67.37500  #  19
*vertex,12.00000,30.50000,67.37500  #  20
# 
# tag, number of vertices followed by list of associated vert
*edges,4,17,1,7,20  #  1
*edges,4,1,2,8,7  #  2
*edges,4,2,3,9,8  #  3
*edges,10,3,4,10,9,3,13,16,15,14,13  #  4
*edges,4,4,5,11,10  #  5
*edges,4,5,6,12,11  #  6
*edges,8,20,7,8,9,10,11,12,19  #  7
*edges,8,18,6,5,4,3,2,1,17  #  8
*edges,4,13,14,15,16  #  9
*edges,4,18,17,20,19  # 10
*edges,4,6,18,19,12  # 11
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,ptn_loba,VERT,-,-,-,gyp_aco_gyp,OPAQUE,UNKNOWN,0,0  #   1 ||< not yet defined
*surf,ptn_lobb,VERT,-,-,-,gyp_aco_gyp,OPAQUE,UNKNOWN,0,0  #   2 ||< not yet defined
*surf,ptn_k2,VERT,-,-,-,gyp_aco_gyp,OPAQUE,ANOTHER,68,06  #   3 ||< ptn_toilet:L5_lit_srv
*surf,fac_fr_a,VERT,-,-,-,mull_90,OPAQUE,EXTERIOR,00,00  #   4 ||< external
*surf,facade_b,VERT,-,-,-,stone_masnry,OPAQUE,EXTERIOR,00,00  #   5 ||< external
*surf,facade_c,VERT,-,-,-,stone_masnry,OPAQUE,EXTERIOR,00,00  #   6 ||< external
*surf,roof,CEIL,-,-,-,roof_0.2,OPAQUE,EXTERIOR,00,00  #   7 ||< external
*surf,floor_toil,FLOR,-,-,-,flr_ov_pln,OPAQUE,UNKNOWN,0,0  #   8 ||< not yet defined
*surf,fac_gl_a,VERT,fac_fr_a,C-WINDOW,CLOSED,db_lpasol,DSF4554_06nb,EXTERIOR,00,00  #   9 ||< external
*surf,ptn_corc_l5,VERT,-,-,-,conc_250,OPAQUE,ANOTHER,01,48  #  10 ||< ptn_corc_l5:stair_1
*surf,shift_fac,VERT,-,-,-,UNKNOWN,OPAQUE,EXTERIOR,00,00  #  11 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,8,    27.50 0  # zone base list
