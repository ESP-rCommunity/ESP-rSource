*Geometry 1.1,GEN,L2_toilet # tag version, format, zone name
*date Wed Jul 27 23:41:19 2011  # latest file modification 
L2_toilet describes toilets adjacent to stair one
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,0.00000,35.70000,52.45000  #   1
*vertex,8.35979,35.70000,52.45000  #   2
*vertex,12.00000,35.70000,52.45000  #   3
*vertex,12.00000,40.70000,52.45000  #   4
*vertex,7.83730,40.70000,52.45000  #   5
*vertex,3.56241,40.70000,52.45000  #   6
*vertex,0.00000,40.70000,52.45000  #   7
*vertex,0.00000,36.28000,52.45000  #   8
*vertex,0.00000,35.70000,55.59300  #   9
*vertex,8.35979,35.70000,55.59300  #  10
*vertex,12.00000,35.70000,55.59300  #  11
*vertex,12.00000,40.70000,55.59300  #  12
*vertex,7.83730,40.70000,55.59300  #  13
*vertex,3.56241,40.70000,55.59300  #  14
*vertex,0.00000,40.70000,55.59300  #  15
*vertex,0.00000,36.28000,55.59300  #  16
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
*surf,ptn_cora_l2,VERT,-,-,-,conc_250,OPAQUE,ANOTHER,16,05  #   1 ||< ptnb_toil_l2:sw_stair
*surf,ptn_corb_l2,VERT,-,-,-,conc_250,OPAQUE,ANOTHER,16,04  #   2 ||< ptna_toil_l2:sw_stair
*surf,ptn_corc_l2,VERT,-,-,-,conc_250,OPAQUE,ANOTHER,08,29  #   3 ||< ptn_toil:l2_cell_c
*surf,fac_a_l2,VERT,-,-,-,stone_masnry,OPAQUE,UNKNOWN,0,0  #   4 ||< not yet defined
*surf,fac_b_l2,VERT,-,-,-,stone_masnry,OPAQUE,EXTERIOR,0,0  #   5 ||< external
*surf,fac_c_l2,VERT,-,-,-,stone_masnry,OPAQUE,EXTERIOR,0,0  #   6 ||< external
*surf,fac_d_l2,VERT,-,-,-,stone_cast,OPAQUE,EXTERIOR,0,0  #   7 ||< external
*surf,ptn_vd_l2,VERT,-,-,-,stone_cast,OPAQUE,ANOTHER,18,15  #   8 ||< ptn_toilet:L2_se_void
*surf,ceiling,CEIL,-,-,-,UNKNOWN,OPAQUE,UNKNOWN,0,0  #   9 ||< not yet defined
*surf,floor,FLOR,-,-,-,UNKNOWN,OPAQUE,UNKNOWN,0,0  #  10 ||< not yet defined
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,10,    60.00 0  # zone base list
