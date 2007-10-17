*Geometry 1.1,GEN,stair_deij # tag version, format, zone name
*date Thu Aug 23 18:32:19 2007  # latest file modification 
stair_deij describes a...
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,34.00000,9.50000,0.00000  #   1
*vertex,34.00000,14.50000,0.00000  #   2
*vertex,29.00000,14.50000,0.00000  #   3
*vertex,29.00000,12.00000,0.00000  #   4
*vertex,29.00000,9.50000,0.00000  #   5
*vertex,34.00000,9.50000,6.50000  #   6
*vertex,34.00000,14.50000,6.50000  #   7
*vertex,29.00000,14.50000,6.50000  #   8
*vertex,29.00000,12.00000,6.50000  #   9
*vertex,29.00000,9.50000,6.50000  #  10
*vertex,34.00000,9.50000,2.70000  #  11
*vertex,34.00000,9.50000,3.80000  #  12
*vertex,34.00000,14.50000,2.70000  #  13
*vertex,34.00000,14.50000,3.80000  #  14
*vertex,29.00000,14.50000,2.70000  #  15
*vertex,29.00000,14.50000,3.80000  #  16
*vertex,29.00000,12.00000,2.70000  #  17
*vertex,29.00000,12.00000,3.80000  #  18
*vertex,29.00000,9.50000,2.70000  #  19
*vertex,29.00000,9.50000,3.80000  #  20
*vertex,34.00000,10.50000,0.00000  #  21
*vertex,34.00000,11.30000,0.00000  #  22
*vertex,34.00000,11.30000,2.10000  #  23
*vertex,34.00000,10.50000,2.10000  #  24
# 
# tag, number of vertices followed by list of associated vert
*edges,12,1,21,24,23,22,2,13,14,7,6,12,11  #  1
*edges,4,2,3,15,13  #  2
*edges,4,3,4,17,15  #  3
*edges,4,4,5,19,17  #  4
*edges,4,5,1,11,19  #  5
*edges,5,6,7,8,9,10  #  6
*edges,7,1,5,4,3,2,22,21  #  7
*edges,4,19,11,12,20  #  8
*edges,4,20,12,6,10  #  9
*edges,4,17,19,20,18  # 10
*edges,4,18,20,10,9  # 11
*edges,4,15,17,18,16  # 12
*edges,4,16,18,9,8  # 13
*edges,4,13,15,16,14  # 14
*edges,4,14,16,8,7  # 15
*edges,4,21,22,23,24  # 16
# 
# surf attributes:
#  surf name, surf position VERT/CIIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,east,VERT,-,-,-,insul_mtl_p,OPAQUE,EXTERIOR,0,0  #   1 ||< external
*surf,prt_i,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,09,04  #   2 ||< str_3:Unit_hi
*surf,prt_i1,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,09,03  #   3 ||< str_2:Unit_hi
*surf,prt_j,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,05,04  #   4 ||< str_4:Unit_j
*surf,prt_j1,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,05,03  #   5 ||< str_3:Unit_j
*surf,ceil,CEIL,-,-,-,susp_ceil,OPAQUE,ANOTHER,17,07  #   6 ||< stair_de:roof
*surf,floor,FLOR,-,-,-,grnd_floor,OPAQUE,GROUND,01,00  #   7 ||< ground profile  1
*surf,prt_cj1,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,13,03  #   8 ||< str_3:ceil_j
*surf,prt_e1,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,08,03  #   9 ||< str_3:Unit_e
*surf,prt_cj,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,13,04  #  10 ||< str_4:ceil_j
*surf,prt_e,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,08,04  #  11 ||< str_4:Unit_e
*surf,prt_ci1,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,12,08  #  12 ||< str_2:ceil_chi
*surf,prt_d1,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,10,03  #  13 ||< str_2:Unit_cd
*surf,prt_ci,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,12,09  #  14 ||< str_3:ceil_chi
*surf,prt_d,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,10,04  #  15 ||< str_3:Unit_cd
*surf,door,VERT,-,-,-,door,OPAQUE,EXTERIOR,0,0  #  16 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,7,    25.00 0  # zone base list
