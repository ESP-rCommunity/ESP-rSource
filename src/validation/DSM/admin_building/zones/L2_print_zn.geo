*Geometry 1.1,GEN,L2_print_zn # tag version, format, zone name
*date Sat Aug 13 14:50:31 2011  # latest file modification 
L2_print_zn describes the print zone adj to open plan & interact
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,27.64430,28.50000,52.45000  #   1
*vertex,31.01671,28.50000,52.45000  #   2
*vertex,34.24663,28.50000,52.45000  #   3
*vertex,34.34163,33.70000,52.45000  #   4
*vertex,30.96921,33.70000,52.45000  #   5
*vertex,27.64430,33.70000,52.45000  #   6
*vertex,27.64430,28.50000,55.59300  #   7
*vertex,31.01671,28.50000,55.59300  #   8
*vertex,34.24663,28.50000,55.59300  #   9
*vertex,34.34163,33.70000,55.59300  #  10
*vertex,30.96921,33.70000,55.59300  #  11
*vertex,27.64430,33.70000,55.59300  #  12
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,8,7  #  1
*edges,4,2,3,9,8  #  2
*edges,4,3,4,10,9  #  3
*edges,4,4,5,11,10  #  4
*edges,4,5,6,12,11  #  5
*edges,4,6,1,7,12  #  6
*edges,6,7,8,9,10,11,12  #  7
*edges,6,1,6,5,4,3,2  #  8
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,Wall-1,VERT,-,-,-,gyp_aco_gyp,OPAQUE,ANOTHER,22,21  #   1 ||< ptn_cell_e:L2_open_pln
*surf,Wall-2,VERT,-,-,-,gyp_aco_gyp,OPAQUE,ANOTHER,22,20  #   2 ||< ptn_cell_d:L2_open_pln
*surf,Wall-3,VERT,-,-,-,gyp_aco_gyp,OPAQUE,ANOTHER,26,10  #   3 ||< Wall-13:L2_core_cel
*surf,Wall-4,VERT,-,-,-,gyp_aco_gyp,OPAQUE,ANOTHER,27,02  #   4 ||< Wall-2:L2_coridor
*surf,Wall-5,VERT,-,-,-,gyp_aco_gyp,OPAQUE,ANOTHER,27,01  #   5 ||< Wall-1:L2_coridor
*surf,Wall-6,VERT,-,-,-,gyp_aco_gyp,OPAQUE,ANOTHER,22,22  #   6 ||< ptn_cell_f:L2_open_pln
*surf,ceil_pz_a,CEIL,-,-,-,slab_275,OPAQUE,ANOTHER,50,21  #   7 ||< ceil_pz_a:L3_ufloor
*surf,floor_pz_a,FLOR,-,-,-,flr_ov_pln,OPAQUE,ANOTHER,17,52  #   8 ||< floor_pz_a:L2_uf_plen
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,8,    34.58 0  # zone base list
