*Geometry 1.1,GEN,Unit_j # tag version, format, zone name
*date Thu Aug 23 18:32:18 2007  # latest file modification 
Unit_j describes a...
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,10.00000,0.00000,0.00000  #   1
*vertex,34.00000,0.00000,0.00000  #   2
*vertex,34.00000,9.50000,0.00000  #   3
*vertex,29.00000,9.50000,0.00000  #   4
*vertex,29.00000,12.00000,0.00000  #   5
*vertex,12.00000,12.00000,0.00000  #   6
*vertex,10.00000,12.00000,0.00000  #   7
*vertex,10.00000,0.00000,2.70000  #   8
*vertex,34.00000,0.00000,2.70000  #   9
*vertex,34.00000,9.50000,2.70000  #  10
*vertex,29.00000,9.50000,2.70000  #  11
*vertex,29.00000,12.00000,2.70000  #  12
*vertex,12.00000,12.00000,2.70000  #  13
*vertex,10.00000,12.00000,2.70000  #  14
*vertex,34.00000,0.50000,1.20000  #  15
*vertex,34.00000,9.00000,1.20000  #  16
*vertex,34.00000,9.00000,2.10000  #  17
*vertex,34.00000,0.50000,2.10000  #  18
*vertex,10.50000,0.00000,1.20000  #  19
*vertex,33.50000,0.00000,1.20000  #  20
*vertex,33.50000,0.00000,2.10000  #  21
*vertex,10.50000,0.00000,2.10000  #  22
*vertex,10.00000,11.50000,0.73000  #  23
*vertex,10.00000,0.50000,0.73000  #  24
*vertex,10.00000,0.50000,2.23000  #  25
*vertex,10.00000,11.50000,2.23000  #  26
# 
# tag, number of vertices followed by list of associated vert
*edges,10,1,2,9,8,1,19,22,21,20,19  #  1
*edges,10,2,3,10,9,2,15,18,17,16,15  #  2
*edges,4,3,4,11,10  #  3
*edges,4,4,5,12,11  #  4
*edges,4,5,6,13,12  #  5
*edges,4,6,7,14,13  #  6
*edges,10,7,1,8,14,7,23,26,25,24,23  #  7
*edges,7,8,9,10,11,12,13,14  #  8
*edges,7,1,7,6,5,4,3,2  #  9
*edges,4,15,16,17,18  # 10
*edges,4,19,20,21,22  # 11
*edges,4,23,24,25,26  # 12
# 
# surf attributes:
#  surf name, surf position VERT/CIIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,south,VERT,-,-,-,insul_mtl_p,OPAQUE,EXTERIOR,0,0  #   1 ||< external
*surf,east,VERT,-,-,-,insul_mtl_p,OPAQUE,EXTERIOR,0,0  #   2 ||< external
*surf,str_3,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,15,05  #   3 ||< prt_j1:stair_deij
*surf,str_4,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,15,04  #   4 ||< prt_j:stair_deij
*surf,prt_i-j,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,09,02  #   5 ||< prt_ij:Unit_hi
*surf,door,VERT,-,-,-,door,OPAQUE,ANOTHER,02,02  #   6 ||< door:corid_g
*surf,west,VERT,-,-,-,insul_mtl_p,OPAQUE,EXTERIOR,0,0  #   7 ||< external
*surf,ceil_j,CEIL,-,-,-,susp_ceil,OPAQUE,ANOTHER,13,09  #   8 ||< lower:ceil_j
*surf,floor,FLOR,-,-,-,grnd_floor,OPAQUE,GROUND,01,00  #   9 ||< ground profile  1
*surf,glz_e,VERT,-,-,-,dbl_glz,DCF7671_06nb,EXTERIOR,0,0  #  10 ||< external
*surf,glz_s,VERT,-,-,-,dbl_glz,DCF7671_06nb,EXTERIOR,0,0  #  11 ||< external
*surf,glz_w,VERT,-,-,-,dbl_glz,DCF7671_06nb,EXTERIOR,0,0  #  12 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,9,   275.50 0  # zone base list
