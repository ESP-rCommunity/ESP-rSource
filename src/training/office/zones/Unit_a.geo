*Geometry 1.1,GEN,Unit_a # tag version, format, zone name
*date Thu Aug 23 18:32:18 2007  # latest file modification 
Unit_a describes a...
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,12.00000,48.00000,3.80000  #   1
*vertex,16.00000,48.00000,3.80000  #   2
*vertex,29.50000,48.00000,3.80000  #   3
*vertex,29.50000,50.50000,3.80000  #   4
*vertex,34.00000,50.50000,3.80000  #   5
*vertex,34.00000,60.00000,3.80000  #   6
*vertex,9.99900,60.00000,3.80000  #   7
*vertex,12.00000,48.00000,6.50000  #   8
*vertex,16.00000,48.00000,6.50000  #   9
*vertex,29.50000,48.00000,6.50000  #  10
*vertex,29.50000,50.50000,6.50000  #  11
*vertex,34.00000,50.50000,6.50000  #  12
*vertex,34.00000,60.00000,6.50000  #  13
*vertex,10.00000,60.00000,6.50000  #  14
*vertex,10.00000,48.00000,3.80000  #  15
*vertex,10.00000,48.00000,6.50000  #  16
*vertex,34.00000,51.00000,5.00000  #  17
*vertex,34.00000,59.00000,5.00000  #  18
*vertex,34.00000,59.00000,5.90000  #  19
*vertex,34.00000,51.00000,5.90000  #  20
*vertex,33.50000,60.00000,5.00000  #  21
*vertex,10.50000,60.00000,5.00000  #  22
*vertex,10.50000,60.00000,5.90000  #  23
*vertex,33.50000,60.00000,5.90000  #  24
*vertex,10.00000,59.50000,4.53000  #  25
*vertex,10.00000,48.50000,4.53000  #  26
*vertex,10.00000,48.50000,6.03000  #  27
*vertex,10.00000,59.50000,6.03000  #  28
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,9,8  #  1
*edges,4,2,3,10,9  #  2
*edges,4,3,4,11,10  #  3
*edges,4,4,5,12,11  #  4
*edges,10,5,6,13,12,5,17,20,19,18,17  #  5
*edges,10,6,7,14,13,6,21,24,23,22,21  #  6
*edges,10,7,15,16,14,7,25,28,27,26,25  #  7
*edges,8,16,8,9,10,11,12,13,14  #  8
*edges,8,1,15,7,6,5,4,3,2  #  9
*edges,4,15,1,8,16  # 10
*edges,4,17,18,19,20  # 11
*edges,4,21,22,23,24  # 12
*edges,4,25,26,27,28  # 13
# 
# surf attributes:
#  surf name, surf position VERT/CIIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,prt_t,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,16,14  #   1 ||< prt_ta:toilets
*surf,prt_a-b,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,07,05  #   2 ||< prt_a-b:Unit_b
*surf,str_3,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,14,13  #   3 ||< prt_a1:stair_abfg
*surf,str_4,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,14,15  #   4 ||< prt_a:stair_abfg
*surf,east,VERT,-,-,-,insul_mtl_p,OPAQUE,EXTERIOR,0,0  #   5 ||< external
*surf,north,VERT,-,-,-,insul_mtl_p,OPAQUE,EXTERIOR,0,0  #   6 ||< external
*surf,west,VERT,-,-,-,insul_mtl_p,OPAQUE,EXTERIOR,0,0  #   7 ||< external
*surf,ceil_a,CEIL,-,-,-,susp_ceil,OPAQUE,ANOTHER,17,09  #   8 ||< ceil_a:roof
*surf,floor,FLOR,-,-,-,susp_floor,OPAQUE,ANOTHER,11,15  #   9 ||< floor_a:ceil_fg
*surf,door,VERT,-,-,-,door,OPAQUE,ANOTHER,01,05  #  10 ||< door:corid_1
*surf,glz_e,VERT,-,-,-,dbl_glz,DCF7671_06nb,EXTERIOR,0,0  #  11 ||< external
*surf,glz_n,VERT,-,-,-,dbl_glz,DCF7671_06nb,EXTERIOR,0,0  #  12 ||< external
*surf,glz_w,VERT,-,-,-,dbl_glz,DCF7671_06nb,EXTERIOR,0,0  #  13 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,9,   276.76 0  # zone base list
