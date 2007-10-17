*Geometry 1.1,GEN,stair_abfg # tag version, format, zone name
*date Thu Aug 23 18:32:19 2007  # latest file modification 
stair_abfg describes a...
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,34.00000,45.50000,0.00000  #   1
*vertex,34.00000,50.50000,0.00000  #   2
*vertex,29.50000,50.50000,0.00000  #   3
*vertex,29.50000,48.00000,0.00000  #   4
*vertex,29.50000,45.50000,0.00000  #   5
*vertex,34.00000,45.50000,6.50000  #   6
*vertex,34.00000,50.50000,6.50000  #   7
*vertex,29.50000,50.50000,6.50000  #   8
*vertex,29.50000,48.00000,6.50000  #   9
*vertex,29.50000,45.50000,6.50000  #  10
*vertex,34.00000,45.50000,2.70000  #  11
*vertex,34.00000,45.50000,3.80000  #  12
*vertex,34.00000,50.50000,2.70000  #  13
*vertex,34.00000,50.50000,3.80000  #  14
*vertex,29.50000,50.50000,2.70000  #  15
*vertex,29.50000,50.50000,3.80000  #  16
*vertex,29.50000,48.00000,2.70000  #  17
*vertex,29.50000,48.00000,3.80000  #  18
*vertex,29.50000,45.50000,2.70000  #  19
*vertex,29.50000,45.50000,3.80000  #  20
*vertex,34.00000,46.50000,0.00000  #  21
*vertex,34.00000,47.30000,0.00000  #  22
*vertex,34.00000,47.30000,2.10000  #  23
*vertex,34.00000,46.50000,2.10000  #  24
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
*surf,prt_f,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,03,04  #   2 ||< str_4:Unit_f
*surf,prt_f1,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,03,03  #   3 ||< str_3:Unit_f
*surf,prt_g,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,04,04  #   4 ||< str_5:Unit_g
*surf,prt_g1,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,04,03  #   5 ||< str_4:Unit_g
*surf,ceil,CEIL,-,-,-,susp_ceil,OPAQUE,ANOTHER,17,10  #   6 ||< stair_ab:roof
*surf,floor,FLOR,-,-,-,grnd_floor,OPAQUE,GROUND,01,00  #   7 ||< ground profile  1
*surf,prt_gc1,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,11,03  #   8 ||< str_4:ceil_fg
*surf,prt_b1,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,07,03  #   9 ||< str_4:Unit_b
*surf,prt_gc,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,11,04  #  10 ||< str_5:ceil_fg
*surf,prt_b,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,07,04  #  11 ||< str_5:Unit_b
*surf,prt_ac1,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,11,05  #  12 ||< str_6:ceil_fg
*surf,prt_a1,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,06,03  #  13 ||< str_3:Unit_a
*surf,prt_ac,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,11,06  #  14 ||< str_7:ceil_fg
*surf,prt_a,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,06,04  #  15 ||< str_4:Unit_a
*surf,door,VERT,-,-,-,door,OPAQUE,EXTERIOR,0,0  #  16 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,7,    22.50 0  # zone base list
