*Geometry 1.1,GEN,L2_print_zn # tag version, format, zone name
*date Wed Jul 27 23:29:22 2011  # latest file modification 
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
*surf,Wall-1,VERT,-,-,-,UNKNOWN,OPAQUE,ANOTHER,03,22  #   1 ||< ptn_cell_e:L2_op_pln
*surf,Wall-2,VERT,-,-,-,UNKNOWN,OPAQUE,ANOTHER,03,21  #   2 ||< ptn_cell_d:L2_op_pln
*surf,Wall-3,VERT,-,-,-,gyp_aco_gyp,OPAQUE,ANOTHER,04,10  #   3 ||< Wall-13:L2_cel_a
*surf,Wall-4,VERT,-,-,-,UNKNOWN,OPAQUE,ANOTHER,06,02  #   4 ||< Wall-2:L2_corid_a
*surf,Wall-5,VERT,-,-,-,UNKNOWN,OPAQUE,ANOTHER,06,01  #   5 ||< Wall-1:L2_corid_a
*surf,Wall-6,VERT,-,-,-,UNKNOWN,OPAQUE,UNKNOWN,0,0  #   6 ||< not yet defined
*surf,ceil_pz_a,CEIL,-,-,-,UNKNOWN,OPAQUE,UNKNOWN,0,0  #   7 ||< not yet defined
*surf,floor_pz_a,FLOR,-,-,-,UNKNOWN,OPAQUE,UNKNOWN,0,0  #   8 ||< not yet defined
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,8,    34.58 0  # zone base list
