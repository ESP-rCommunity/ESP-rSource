*Geometry 1.1,GEN,ceil_chi # tag version, format, zone name
*date Thu Aug 23 18:32:19 2007  # latest file modification 
ceil_chi describes a...
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,10.00000,48.00000,2.70000  #   1
*vertex,10.00000,46.00000,2.70000  #   2
*vertex,7.28300,42.13900,2.70000  #   3
*vertex,10.00000,38.00000,2.70000  #   4
*vertex,10.00000,36.00000,2.70000  #   5
*vertex,10.00000,12.00000,2.70000  #   6
*vertex,12.00000,12.00000,2.70000  #   7
*vertex,29.00000,12.00000,2.70000  #   8
*vertex,29.00000,14.50000,2.70000  #   9
*vertex,34.00000,14.50000,2.70000  #  10
*vertex,34.00000,36.00000,2.70000  #  11
*vertex,16.00000,36.00000,2.70000  #  12
*vertex,12.00000,36.00000,2.70000  #  13
*vertex,12.00000,48.00000,2.70000  #  14
*vertex,10.00000,48.00000,3.80000  #  15
*vertex,10.00000,46.00000,3.80000  #  16
*vertex,7.28300,42.13900,3.80000  #  17
*vertex,10.00000,38.00000,3.80000  #  18
*vertex,10.00000,36.00000,3.80000  #  19
*vertex,10.00000,12.00000,3.80000  #  20
*vertex,12.00000,12.00000,3.80000  #  21
*vertex,29.00000,12.00000,3.80000  #  22
*vertex,29.00000,14.50000,3.80000  #  23
*vertex,34.00000,14.50000,3.80000  #  24
*vertex,34.00000,36.00000,3.80000  #  25
*vertex,16.00000,36.00000,3.80000  #  26
*vertex,12.00000,36.00000,3.80000  #  27
*vertex,12.00000,48.00000,3.80000  #  28
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,16,15  #  1
*edges,4,2,3,17,16  #  2
*edges,4,3,4,18,17  #  3
*edges,4,4,5,19,18  #  4
*edges,4,5,6,20,19  #  5
*edges,4,6,7,21,20  #  6
*edges,4,7,8,22,21  #  7
*edges,4,8,9,23,22  #  8
*edges,4,9,10,24,23  #  9
*edges,4,10,11,25,24  # 10
*edges,4,11,12,26,25  # 11
*edges,4,12,13,27,26  # 12
*edges,4,13,14,28,27  # 13
*edges,4,14,1,15,28  # 14
*edges,7,21,22,23,24,25,26,27  # 15
*edges,7,13,12,11,10,9,8,7  # 16
*edges,9,6,5,4,3,2,1,14,13,7  # 17
*edges,9,15,16,17,18,19,20,21,27,28  # 18
# 
# surf attributes:
#  surf name, surf position VERT/CIIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,nw_ext,VERT,-,-,-,insul_mtl_p,OPAQUE,EXTERIOR,0,0  #   1 ||< external
*surf,ent_a,VERT,-,-,-,dbl_glz,DCF7671_06nb,EXTERIOR,0,0  #   2 ||< external
*surf,ent_b,VERT,-,-,-,dbl_glz,DCF7671_06nb,EXTERIOR,0,0  #   3 ||< external
*surf,ent_c,VERT,-,-,-,dbl_glz,DCF7671_06nb,EXTERIOR,0,0  #   4 ||< external
*surf,west,VERT,-,-,-,insul_mtl_p,OPAQUE,EXTERIOR,0,0  #   5 ||< external
*surf,cor_j,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,13,06  #   6 ||< cor_dj:ceil_j
*surf,prt_ij,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,13,05  #   7 ||< prt_i-j:ceil_j
*surf,str_2,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,15,12  #   8 ||< prt_ci1:stair_deij
*surf,str_3,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,15,14  #   9 ||< prt_ci:stair_deij
*surf,east,VERT,-,-,-,insul_mtl_p,OPAQUE,EXTERIOR,0,0  #  10 ||< external
*surf,prt_gh,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,11,01  #  11 ||< prt_g-h:ceil_fg
*surf,prt_th,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,16,09  #  12 ||< prt_cth:toilets
*surf,prt_tcor,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,16,07  #  13 ||< prt_tcorc:toilets
*surf,prt_fcor,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,11,10  #  14 ||< cor_d:ceil_fg
*surf,upper,CEIL,-,-,-,susp_flr_re,OPAQUE,ANOTHER,10,09  #  15 ||< floor:Unit_cd
*surf,ceil,FLOR,-,-,-,susp_ceil,OPAQUE,ANOTHER,09,08  #  16 ||< ceil:Unit_hi
*surf,cor_ceil,FLOR,-,-,-,susp_ceil,OPAQUE,ANOTHER,02,09  #  17 ||< ceil:corid_g
*surf,up_cor,CEIL,-,-,-,susp_flr_re,OPAQUE,ANOTHER,01,10  #  18 ||< floor:corid_1
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,2,16,17,   598.37 0  # zone base list
