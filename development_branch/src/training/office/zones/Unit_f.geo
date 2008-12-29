*Geometry 1.1,GEN,Unit_f # tag version, format, zone name
*date Thu Aug 23 18:32:18 2007  # latest file modification 
Unit_f describes a...
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,12.00000,48.00000,0.00000  #   1
*vertex,16.00000,48.00000,0.00000  #   2
*vertex,29.50000,48.00000,0.00000  #   3
*vertex,29.50000,50.50000,0.00000  #   4
*vertex,34.00000,50.50000,0.00000  #   5
*vertex,34.00000,60.00000,0.00000  #   6
*vertex,10.00000,60.00000,0.00000  #   7
*vertex,12.00000,48.00000,2.70000  #   8
*vertex,16.00000,48.00000,2.70000  #   9
*vertex,29.50000,48.00000,2.70000  #  10
*vertex,29.50000,50.50000,2.70000  #  11
*vertex,34.00000,50.50000,2.70000  #  12
*vertex,34.00000,60.00000,2.70000  #  13
*vertex,10.00000,60.00000,2.70000  #  14
*vertex,10.00000,48.00000,0.00000  #  15
*vertex,10.00000,48.00000,2.70000  #  16
*vertex,34.00000,51.00000,1.20000  #  17
*vertex,34.00000,59.00000,1.20000  #  18
*vertex,34.00000,59.00000,2.10000  #  19
*vertex,34.00000,51.00000,2.10000  #  20
*vertex,33.50000,60.00000,1.20000  #  21
*vertex,10.50000,60.00000,1.20000  #  22
*vertex,10.50000,60.00000,2.10000  #  23
*vertex,33.50000,60.00000,2.10000  #  24
*vertex,10.00000,59.50000,0.73000  #  25
*vertex,10.00000,48.50000,0.73000  #  26
*vertex,10.00000,48.50000,2.23000  #  27
*vertex,10.00000,59.50000,2.23000  #  28
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
*surf,prt_f-t,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,16,04  #   1 ||< prt_ft:toilets
*surf,prt_f-g,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,04,05  #   2 ||< prt_f-g:Unit_g
*surf,str_3,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,14,03  #   3 ||< prt_f1:stair_abfg
*surf,str_4,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,14,02  #   4 ||< prt_f:stair_abfg
*surf,east,VERT,-,-,-,insul_mtl_p,OPAQUE,EXTERIOR,0,0  #   5 ||< external
*surf,north,VERT,-,-,-,insul_mtl_p,OPAQUE,EXTERIOR,0,0  #   6 ||< external
*surf,west,VERT,-,-,-,insul_mtl_p,OPAQUE,EXTERIOR,0,0  #   7 ||< external
*surf,ceil_f,CEIL,-,-,-,susp_ceil,OPAQUE,ANOTHER,11,16  #   8 ||< ceil_f:ceil_fg
*surf,floor,FLOR,-,-,-,grnd_floor,OPAQUE,GROUND,01,00  #   9 ||< ground profile  1
*surf,door,VERT,-,-,-,door,OPAQUE,ANOTHER,02,05  #  10 ||< door_f:corid_g
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
*base_list,1,9,   276.75 0  # zone base list
