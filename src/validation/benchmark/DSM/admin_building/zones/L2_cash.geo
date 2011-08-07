*Geometry 1.1,GEN,L2_cash # tag version, format, zone name
*date Sun Aug  7 18:18:28 2011  # latest file modification 
L2_cash describes cash office and cahier desk on level two
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,13.70000,28.50000,52.45000  #   1
*vertex,17.33706,28.50000,52.45000  #   2
*vertex,19.33201,28.50000,52.45000  #   3
*vertex,22.79942,28.50000,52.45000  #   4
*vertex,22.75192,33.70000,52.45000  #   5
*vertex,17.43206,33.70000,52.45000  #   6
*vertex,13.70000,33.70000,52.45000  #   7
*vertex,13.70000,30.50000,52.45000  #   8
*vertex,13.70000,28.50000,55.59300  #   9
*vertex,17.33706,28.50000,55.59300  #  10
*vertex,19.33201,28.50000,55.59300  #  11
*vertex,22.79942,28.50000,55.59300  #  12
*vertex,22.75192,33.70000,55.59300  #  13
*vertex,17.43206,33.70000,55.59300  #  14
*vertex,13.70000,33.70000,55.59300  #  15
*vertex,13.70000,30.50000,55.59300  #  16
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,10,9  #  1
*edges,4,2,3,11,10  #  2
*edges,4,3,4,12,11  #  3
*edges,4,4,5,13,12  #  4
*edges,4,5,6,14,13  #  5
*edges,4,6,7,15,14  #  6
*edges,4,7,8,16,15  #  7
*edges,4,8,1,9,16  #  8
*edges,8,9,10,11,12,13,14,15,16  #  9
*edges,8,1,8,7,6,5,4,3,2  # 10
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,Wall-1,VERT,-,-,-,gyp_aco_gyp,OPAQUE,ANOTHER,23,13  #   1 ||< ptn_cash_b:L2_cust_srv
*surf,Wall-2,VERT,-,-,-,gyp_aco_gyp,OPAQUE,ANOTHER,23,12  #   2 ||< ptn_cash:L2_cust_srv
*surf,Wall-3,VERT,-,-,-,gyp_aco_gyp,OPAQUE,ANOTHER,22,25  #   3 ||< ptn_cell_h:L2_open_pln
*surf,Wall-4,VERT,-,-,-,gyp_aco_gyp,OPAQUE,ANOTHER,22,24  #   4 ||< ptn_cell_g:L2_open_pln
*surf,Wall-5,VERT,-,-,-,gyp_aco_gyp,OPAQUE,ANOTHER,27,25  #   5 ||< Wall-25:L2_coridor
*surf,Wall-6,VERT,-,-,-,gyp_aco_gyp,OPAQUE,ANOTHER,27,24  #   6 ||< Wall-24:L2_coridor
*surf,Wall-7,VERT,-,-,-,gyp_aco_gyp,OPAQUE,ANOTHER,27,23  #   7 ||< Wall-23:L2_coridor
*surf,Wall-8,VERT,-,-,-,gyp_aco_gyp,OPAQUE,ANOTHER,23,14  #   8 ||< ptn_cash_c:L2_cust_srv
*surf,ceil_cash,CEIL,-,-,-,slab_275,OPAQUE,UNKNOWN,0,0  #   9 ||< not yet defined
*surf,floor_cash,FLOR,-,-,-,flr_ov_pln,OPAQUE,ANOTHER,17,51  #  10 ||< floor_cash:L2_uf_plen
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,10,    47.19 0  # zone base list
