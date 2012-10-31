*Geometry 1.1,GEN,ceil_fg # tag version, format, zone name
*date Thu Aug 23 18:32:19 2007  # latest file modification 
ceil_fg describes a...
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,16.00000,36.00000,2.70000  #   1
*vertex,34.00000,36.00000,2.70000  #   2
*vertex,34.00000,45.50000,2.70000  #   3
*vertex,29.50000,45.50000,2.70000  #   4
*vertex,29.50000,48.00000,2.70000  #   5
*vertex,29.50000,50.50000,2.70000  #   6
*vertex,34.00000,50.50000,2.70000  #   7
*vertex,34.00000,60.00000,2.70000  #   8
*vertex,10.00000,60.00000,2.70000  #   9
*vertex,10.00000,48.00000,2.70000  #  10
*vertex,12.00000,48.00000,2.70000  #  11
*vertex,16.00000,48.00000,2.70000  #  12
*vertex,16.00000,36.00000,3.80000  #  13
*vertex,34.00000,36.00000,3.80000  #  14
*vertex,34.00000,45.50000,3.80000  #  15
*vertex,29.50000,45.50000,3.80000  #  16
*vertex,29.50000,48.00000,3.80000  #  17
*vertex,29.50000,50.50000,3.80000  #  18
*vertex,34.00000,50.50000,3.80000  #  19
*vertex,34.00000,60.00000,3.80000  #  20
*vertex,10.00000,60.00000,3.80000  #  21
*vertex,10.00000,48.00000,3.80000  #  22
*vertex,12.00000,48.00000,3.80000  #  23
*vertex,16.00000,48.00000,3.80000  #  24
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,14,13  #  1
*edges,4,2,3,15,14  #  2
*edges,4,3,4,16,15  #  3
*edges,4,4,5,17,16  #  4
*edges,4,5,6,18,17  #  5
*edges,4,6,7,19,18  #  6
*edges,4,7,8,20,19  #  7
*edges,4,8,9,21,20  #  8
*edges,4,9,10,22,21  #  9
*edges,4,10,11,23,22  # 10
*edges,4,11,12,24,23  # 11
*edges,4,12,1,13,24  # 12
*edges,6,13,14,15,16,17,24  # 13
*edges,6,1,12,5,4,3,2  # 14
*edges,8,21,22,23,24,17,18,19,20  # 15
*edges,8,10,9,8,7,6,5,12,11  # 16
# 
# surf attributes:
#  surf name, surf position VERT/CIIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,prt_g-h,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,12,11  #   1 ||< prt_gh:ceil_chi
*surf,east_s,VERT,-,-,-,insul_mtl_p,OPAQUE,EXTERIOR,0,0  #   2 ||< external
*surf,str_4,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,14,08  #   3 ||< prt_gc1:stair_abfg
*surf,str_5,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,14,10  #   4 ||< prt_gc:stair_abfg
*surf,str_6,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,14,12  #   5 ||< prt_ac1:stair_abfg
*surf,str_7,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,14,14  #   6 ||< prt_ac:stair_abfg
*surf,east_n,VERT,-,-,-,insul_mtl_p,OPAQUE,EXTERIOR,0,0  #   7 ||< external
*surf,north,VERT,-,-,-,insul_mtl_p,OPAQUE,EXTERIOR,0,0  #   8 ||< external
*surf,west,VERT,-,-,-,insul_mtl_p,OPAQUE,EXTERIOR,0,0  #   9 ||< external
*surf,cor_d,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,12,14  #  10 ||< prt_fcor:ceil_chi
*surf,prt_t1,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,16,13  #  11 ||< prt_cat:toilets
*surf,prt_t2,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,16,11  #  12 ||< prt_ctb:toilets
*surf,floor_b,CEIL,-,-,-,susp_flr_re,OPAQUE,ANOTHER,07,08  #  13 ||< floor:Unit_b
*surf,ceil_g,FLOR,-,-,-,susp_ceil,OPAQUE,ANOTHER,04,07  #  14 ||< ceil:Unit_g
*surf,floor_a,CEIL,-,-,-,susp_flr_re,OPAQUE,ANOTHER,06,09  #  15 ||< floor:Unit_a
*surf,ceil_f,FLOR,-,-,-,susp_ceil,OPAQUE,ANOTHER,03,08  #  16 ||< ceil_f:Unit_f
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,0,481.50,1  # zone base
