*Geometry 1.1,GEN,L5_toilet # tag version, format, zone name
*date Mon Aug 15 19:23:21 2011  # latest file modification 
L5_toilet describes toilet adjacent to stair one on level 5
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,10.25000,30.50000,64.15000  #   1
*vertex,13.25000,30.50000,64.15000  #   2
*vertex,17.00000,30.50000,64.15000  #   3
*vertex,17.00000,36.00000,64.15000  #   4
*vertex,16.25000,36.00000,64.15000  #   5
*vertex,13.75000,36.00000,64.15000  #   6
*vertex,11.25000,36.00000,64.15000  #   7
*vertex,10.25000,36.00000,64.15000  #   8
*vertex,10.25000,30.50000,67.37500  #   9
*vertex,13.25000,30.50000,67.37500  #  10
*vertex,17.00000,30.50000,67.37500  #  11
*vertex,17.00000,36.00000,67.37500  #  12
*vertex,16.25000,36.00000,67.37500  #  13
*vertex,13.75000,36.00000,67.37500  #  14
*vertex,11.25000,36.00000,67.37500  #  15
*vertex,10.25000,36.00000,67.37500  #  16
*vertex,16.96041,36.00000,64.32024  #  17
*vertex,16.28959,36.00000,64.32024  #  18
*vertex,16.28959,36.00000,67.20477  #  19
*vertex,16.96041,36.00000,67.20477  #  20
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,10,9  #  1
*edges,4,2,3,11,10  #  2
*edges,4,3,4,12,11  #  3
*edges,10,4,5,13,12,4,17,20,19,18,17  #  4
*edges,4,5,6,14,13  #  5
*edges,4,6,7,15,14  #  6
*edges,4,7,8,16,15  #  7
*edges,4,8,1,9,16  #  8
*edges,8,9,10,11,12,13,14,15,16  #  9
*edges,8,1,8,7,6,5,4,3,2  # 10
*edges,4,17,18,19,20  # 11
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,ptn_loba,VERT,-,-,-,gyp_aco_gyp,OPAQUE,UNKNOWN,0,0  #   1 ||< not yet defined
*surf,ptn_lobb,VERT,-,-,-,gyp_aco_gyp,OPAQUE,UNKNOWN,0,0  #   2 ||< not yet defined
*surf,ptn_k2,VERT,-,-,-,gyp_aco_gyp,OPAQUE,UNKNOWN,0,0  #   3 ||< not yet defined
*surf,fac_fr_a,VERT,-,-,-,mull_90,OPAQUE,UNKNOWN,0,0  #   4 ||< not yet defined
*surf,facade_b,VERT,-,-,-,stone_masnry,OPAQUE,UNKNOWN,0,0  #   5 ||< not yet defined
*surf,facade_c,VERT,-,-,-,stone_masnry,OPAQUE,UNKNOWN,0,0  #   6 ||< not yet defined
*surf,facade_d,VERT,-,-,-,stone_masnry,OPAQUE,UNKNOWN,0,0  #   7 ||< not yet defined
*surf,ptn_stair,VERT,-,-,-,conc_250,OPAQUE,UNKNOWN,0,0  #   8 ||< not yet defined
*surf,roof,CEIL,-,-,-,roof_0.2,OPAQUE,UNKNOWN,0,0  #   9 ||< not yet defined
*surf,floor_toil,FLOR,-,-,-,flr_ov_pln,OPAQUE,UNKNOWN,0,0  #  10 ||< not yet defined
*surf,fac_gl_a,VERT,fac_fr_a,C-WINDOW,CLOSED,db_lpasol,TRAN,UNKNOWN,0,0  #  11 ||< not yet defined
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,10,    37.13 0  # zone base list
