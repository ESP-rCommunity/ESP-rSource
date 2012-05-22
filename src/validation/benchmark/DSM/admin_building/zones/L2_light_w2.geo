*Geometry 1.1,GEN,L2_light_w2 # tag version, format, zone name
*date Wed Jul 27 23:29:14 2011  # latest file modification 
L2_light_w2 describes light well two at level two
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,34.25000,21.30000,52.45000  #   1
*vertex,38.00000,21.30000,52.45000  #   2
*vertex,38.00000,28.50000,52.45000  #   3
*vertex,34.25000,28.50000,52.45000  #   4
*vertex,34.25000,21.30000,55.59300  #   5
*vertex,38.00000,21.30000,55.59300  #   6
*vertex,38.00000,28.50000,55.59300  #   7
*vertex,34.25000,28.50000,55.59300  #   8
*vertex,34.25000,21.30000,53.45000  #   9
*vertex,38.00000,21.30000,53.45000  #  10
*vertex,38.00000,28.50000,53.45000  #  11
*vertex,34.25000,28.50000,53.45000  #  12
*vertex,34.25000,21.30000,51.69300  #  13
*vertex,38.00000,21.30000,51.69300  #  14
*vertex,38.00000,28.50000,51.69300  #  15
*vertex,34.25000,28.50000,51.69300  #  16
# 
# tag, number of vertices followed by list of associated vert
*edges,4,10,6,5,9  #  1
*edges,4,11,7,6,10  #  2
*edges,6,3,4,12,8,7,11  #  3
*edges,4,9,5,8,12  #  4
*edges,4,5,6,7,8  #  5
*edges,4,13,16,15,14  #  6
*edges,4,4,1,9,12  #  7
*edges,4,1,2,10,9  #  8
*edges,4,2,3,11,10  #  9
*edges,4,14,15,3,2  # 10
*edges,4,13,14,2,1  # 11
*edges,4,16,13,1,4  # 12
*edges,4,15,16,4,3  # 13
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,l3_lw2_fr,VERT,-,-,-,UNKNOWN,OPAQUE,ANOTHER,03,19  #   1 ||< front_shaft:L2_op_pln
*surf,l3_lw2_ri,VERT,-,-,-,UNKNOWN,OPAQUE,ANOTHER,03,18  #   2 ||< right_shaft:L2_op_pln
*surf,l3_lw2_bk,VERT,-,-,-,gyp_aco_gyp,OPAQUE,ANOTHER,04,01  #   3 ||< Wall-1:L2_cel_a
*surf,l3_lw2_lef,VERT,-,-,-,UNKNOWN,OPAQUE,ANOTHER,03,20  #   4 ||< left_shaft:L2_op_pln
*surf,up_l3_lw2,CEIL,-,-,-,UNKNOWN,OPAQUE,ANOTHER,23,06  #   5 ||< l3_lw2_base:L3_light_w2
*surf,l3_lw2_base,FLOR,-,-,-,UNKNOWN,OPAQUE,UNKNOWN,0,0  #   6 ||< not yet defined
*surf,left_rail,VERT,-,-,-,UNKNOWN,OPAQUE,ANOTHER,03,43  #   7 ||< left_rail:L2_op_pln
*surf,front_rail,VERT,-,-,-,UNKNOWN,OPAQUE,ANOTHER,03,44  #   8 ||< front_rail:L2_op_pln
*surf,right_rail,VERT,-,-,-,UNKNOWN,OPAQUE,ANOTHER,03,45  #   9 ||< right_rail:L2_op_pln
*surf,l3_flrv_r,VERT,-,-,-,UNKNOWN,OPAQUE,UNKNOWN,0,0  #  10 ||< not yet defined
*surf,l3_flrv_f,VERT,-,-,-,UNKNOWN,OPAQUE,UNKNOWN,0,0  #  11 ||< not yet defined
*surf,l3_flrv_l,VERT,-,-,-,UNKNOWN,OPAQUE,UNKNOWN,0,0  #  12 ||< not yet defined
*surf,l3_flrv_b,VERT,-,-,-,UNKNOWN,OPAQUE,UNKNOWN,0,0  #  13 ||< not yet defined
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,6,    27.00 0  # zone base list
