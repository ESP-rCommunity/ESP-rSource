*Geometry 1.1,GEN,ceil_j # tag version, format, zone name
*date Thu Aug 23 18:32:19 2007  # latest file modification 
ceil_j describes a...
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,10.00000,0.00000,2.70000  #   1
*vertex,34.00000,0.00000,2.70000  #   2
*vertex,34.00000,9.50000,2.70000  #   3
*vertex,29.00000,9.50000,2.70000  #   4
*vertex,29.00000,12.00000,2.70000  #   5
*vertex,12.00000,12.00000,2.70000  #   6
*vertex,9.99900,12.00000,2.70000  #   7
*vertex,10.00000,0.00000,3.80000  #   8
*vertex,34.00000,0.00000,3.80000  #   9
*vertex,34.00000,9.50000,3.80000  #  10
*vertex,29.00000,9.50000,3.80000  #  11
*vertex,29.00000,12.00000,3.80000  #  12
*vertex,12.00000,12.00000,3.80000  #  13
*vertex,10.00000,12.00000,3.80000  #  14
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,9,8  #  1
*edges,4,2,3,10,9  #  2
*edges,4,3,4,11,10  #  3
*edges,4,4,5,12,11  #  4
*edges,4,5,6,13,12  #  5
*edges,4,6,7,14,13  #  6
*edges,4,7,1,8,14  #  7
*edges,7,8,9,10,11,12,13,14  #  8
*edges,7,1,7,6,5,4,3,2  #  9
# 
# surf attributes:
#  surf name, surf position VERT/CIIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,south,VERT,-,-,-,insul_mtl_p,OPAQUE,EXTERIOR,0,0  #   1 ||< external
*surf,east,VERT,-,-,-,insul_mtl_p,OPAQUE,EXTERIOR,0,0  #   2 ||< external
*surf,str_3,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,15,08  #   3 ||< prt_cj1:stair_deij
*surf,str_4,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,15,10  #   4 ||< prt_cj:stair_deij
*surf,prt_i-j,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,12,07  #   5 ||< prt_ij:ceil_chi
*surf,cor_dj,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,12,06  #   6 ||< cor_j:ceil_chi
*surf,west,VERT,-,-,-,insul_mtl_p,OPAQUE,EXTERIOR,0,0  #   7 ||< external
*surf,upper,CEIL,-,-,-,susp_flr_re,OPAQUE,ANOTHER,08,09  #   8 ||< floor:Unit_e
*surf,lower,FLOR,-,-,-,susp_ceil,OPAQUE,ANOTHER,05,08  #   9 ||< ceil_j:Unit_j
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,9,   275.51 0  # zone base list
