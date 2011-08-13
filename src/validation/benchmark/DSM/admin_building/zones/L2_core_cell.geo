*Geometry 1.1,GEN,L2_core_cel # tag version, format, zone name
*date Sat Aug 13 14:50:48 2011  # latest file modification 
L2_core_cel describes set of cellular offices adj to open plan
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,34.24663,28.50000,52.45000  #   1
*vertex,37.99903,28.50000,52.45000  #   2
*vertex,41.37145,28.50000,52.45000  #   3
*vertex,44.69636,28.50000,52.45000  #   4
*vertex,51.00000,28.50000,52.45000  #   5
*vertex,47.78378,33.70000,52.45000  #   6
*vertex,44.69636,33.70000,52.45000  #   7
*vertex,41.37145,33.70000,52.45000  #   8
*vertex,37.99903,33.70000,52.45000  #   9
*vertex,34.34163,33.70000,52.45000  #  10
*vertex,34.24663,28.50000,55.59300  #  11
*vertex,37.99903,28.50000,55.59300  #  12
*vertex,41.37145,28.50000,55.59300  #  13
*vertex,44.69636,28.50000,55.59300  #  14
*vertex,51.00000,28.50000,55.59300  #  15
*vertex,47.78378,33.70000,55.59300  #  16
*vertex,44.69636,33.70000,55.59300  #  17
*vertex,41.37145,33.70000,55.59300  #  18
*vertex,37.99903,33.70000,55.59300  #  19
*vertex,34.34163,33.70000,55.59300  #  20
*vertex,51.00000,30.50000,52.45000  #  21
*vertex,51.00000,30.50000,55.59300  #  22
*vertex,50.00000,30.50000,52.45000  #  23
*vertex,50.00000,30.50000,55.59300  #  24
*vertex,50.00000,33.67664,52.45000  #  25
*vertex,50.00000,33.67664,55.59300  #  26
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,12,11  #  1
*edges,4,2,3,13,12  #  2
*edges,4,3,4,14,13  #  3
*edges,4,4,5,15,14  #  4
*edges,4,25,6,16,26  #  5
*edges,4,6,7,17,16  #  6
*edges,4,7,8,18,17  #  7
*edges,4,8,9,19,18  #  8
*edges,4,9,10,20,19  #  9
*edges,4,10,1,11,20  # 10
*edges,13,11,12,13,14,15,22,24,26,16,17,18,19,20  # 11
*edges,13,1,10,9,8,7,6,25,23,21,5,4,3,2  # 12
*edges,4,5,21,22,15  # 13
*edges,4,21,23,24,22  # 14
*edges,4,23,25,26,24  # 15
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,Wall-1,VERT,-,-,-,gyp_aco_gyp,OPAQUE,ANOTHER,31,03  #   1 ||< l3_lw2_bk:L2_light_w2
*surf,Wall-2,VERT,-,-,-,gyp_aco_gyp,OPAQUE,ANOTHER,22,16  #   2 ||< ptn_cell_c:L2_open_pln
*surf,Wall-3,VERT,-,-,-,gyp_aco_gyp,OPAQUE,ANOTHER,22,15  #   3 ||< ptn_cell_b:L2_open_pln
*surf,Wall-4,VERT,-,-,-,gyp_aco_gyp,OPAQUE,ANOTHER,22,14  #   4 ||< ptn_cell_a:L2_open_pln
*surf,Wall-8,VERT,-,-,-,gyp_aco_gyp,OPAQUE,ANOTHER,27,07  #   5 ||< Wall-7:L2_coridor
*surf,Wall-9,VERT,-,-,-,gyp_aco_gyp,OPAQUE,ANOTHER,27,06  #   6 ||< Wall-6:L2_coridor
*surf,Wall-10,VERT,-,-,-,gyp_aco_gyp,OPAQUE,ANOTHER,27,05  #   7 ||< Wall-5:L2_coridor
*surf,Wall-11,VERT,-,-,-,gyp_aco_gyp,OPAQUE,ANOTHER,27,04  #   8 ||< Wall-4:L2_coridor
*surf,Wall-12,VERT,-,-,-,gyp_aco_gyp,OPAQUE,ANOTHER,27,03  #   9 ||< Wall-3:L2_coridor
*surf,Wall-13,VERT,-,-,-,gyp_aco_gyp,OPAQUE,ANOTHER,25,03  #  10 ||< Wall-3:L2_print_zn
*surf,ceil_l2_cl_a,CEIL,-,-,-,slab_275,OPAQUE,ANOTHER,50,22  #  11 ||< ceil_l2_cl_a:L3_ufloor
*surf,floor_l2_cl_,FLOR,-,-,-,flr_ov_pln,OPAQUE,ANOTHER,17,53  #  12 ||< floor_l2_cl_:L2_uf_plen
*surf,ptn_cel_b,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,33,07  #  13 ||< ptn_cel_b:L2_toil_psg
*surf,ptnc_str_l2,VERT,-,-,-,conc_250,OPAQUE,ANOTHER,02,33  #  14 ||< ptnc_str_l2:stair_2
*surf,Wall-15,VERT,-,-,-,conc_250,OPAQUE,ANOTHER,02,06  #  15 ||< Wall-15:stair_2
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,12,    83.64 0  # zone base list
