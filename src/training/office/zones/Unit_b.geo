*Geometry 1.1,GEN,Unit_b # tag version, format, zone name
*date Thu Aug 23 18:32:19 2007  # latest file modification 
Unit_b describes a...
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,16.00000,36.00000,3.80000  #   1
*vertex,34.00000,36.00000,3.80000  #   2
*vertex,34.00000,45.50000,3.80000  #   3
*vertex,29.50000,45.50000,3.80000  #   4
*vertex,29.50000,48.00000,3.80000  #   5
*vertex,16.00000,48.00000,3.80000  #   6
*vertex,16.00000,36.00000,6.50000  #   7
*vertex,34.00000,36.00000,6.50000  #   8
*vertex,34.00000,45.50000,6.50000  #   9
*vertex,29.50000,45.50000,6.50000  #  10
*vertex,29.50000,48.00000,6.50000  #  11
*vertex,16.00000,48.00000,6.50000  #  12
*vertex,34.00000,36.50000,5.00000  #  13
*vertex,34.00000,44.50000,5.00000  #  14
*vertex,34.00000,44.50000,5.90000  #  15
*vertex,34.00000,36.50000,5.90000  #  16
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,8,7  #  1
*edges,10,2,3,9,8,2,13,16,15,14,13  #  2
*edges,4,3,4,10,9  #  3
*edges,4,4,5,11,10  #  4
*edges,4,5,6,12,11  #  5
*edges,4,6,1,7,12  #  6
*edges,6,7,8,9,10,11,12  #  7
*edges,6,1,6,5,4,3,2  #  8
*edges,4,13,14,15,16  #  9
# 
# surf attributes:
#  surf name, surf position VERT/CIIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,prt_b-c,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,10,06  #   1 ||< prt_bc:Unit_cd
*surf,east,VERT,-,-,-,insul_mtl_p,OPAQUE,EXTERIOR,0,0  #   2 ||< external
*surf,str_4,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,14,09  #   3 ||< prt_b1:stair_abfg
*surf,str_5,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,14,11  #   4 ||< prt_b:stair_abfg
*surf,prt_a-b,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,06,02  #   5 ||< prt_a-b:Unit_a
*surf,prt_t_b,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,16,12  #   6 ||< prt_tb:toilets
*surf,ceil_b,CEIL,-,-,-,susp_ceil,OPAQUE,ANOTHER,17,16  #   7 ||< ceil_b:roof
*surf,floor,FLOR,-,-,-,susp_floor,OPAQUE,ANOTHER,11,13  #   8 ||< floor_b:ceil_fg
*surf,glz_e,VERT,-,-,-,dbl_glz,DCF7671_06nb,EXTERIOR,0,0  #   9 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,8,   204.75 0  # zone base list
