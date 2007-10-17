*Geometry 1.1,GEN,toilets # tag version, format, zone name
*date Thu Aug 23 18:32:19 2007  # latest file modification 
toilets describes a...
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,12.00000,48.00000,0.00000  #   1
*vertex,12.00000,36.00000,0.00000  #   2
*vertex,16.00000,36.00000,0.00000  #   3
*vertex,16.00000,48.00000,0.00000  #   4
*vertex,12.00000,48.00000,6.50000  #   5
*vertex,12.00000,36.00000,6.50000  #   6
*vertex,16.00000,36.00000,6.50000  #   7
*vertex,16.00000,48.00000,6.50000  #   8
*vertex,12.00000,48.00000,2.70000  #   9
*vertex,12.00000,48.00000,3.80000  #  10
*vertex,12.00000,36.00000,2.70000  #  11
*vertex,12.00000,36.00000,3.80000  #  12
*vertex,16.00000,36.00000,2.70000  #  13
*vertex,16.00000,36.00000,3.80000  #  14
*vertex,16.00000,48.00000,2.70000  #  15
*vertex,16.00000,48.00000,3.80000  #  16
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,11,9  #  1
*edges,4,2,3,13,11  #  2
*edges,4,3,4,15,13  #  3
*edges,4,4,1,9,15  #  4
*edges,4,5,6,7,8  #  5
*edges,4,1,4,3,2  #  6
*edges,4,9,11,12,10  #  7
*edges,4,10,12,6,5  #  8
*edges,4,11,13,14,12  #  9
*edges,4,12,14,7,6  # 10
*edges,4,13,15,16,14  # 11
*edges,4,14,16,8,7  # 12
*edges,4,15,9,10,16  # 13
*edges,4,16,10,5,8  # 14
# 
# surf attributes:
#  surf name, surf position VERT/CIIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,prt_tcor,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,02,04  #   1 ||< prt_t_cor:corid_g
*surf,prt_th,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,09,07  #   2 ||< prt_th:Unit_hi
*surf,prt_tg,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,04,06  #   3 ||< prt_t:Unit_g
*surf,prt_ft,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,03,01  #   4 ||< prt_f-t:Unit_f
*surf,ceiling,CEIL,-,-,-,susp_ceil,OPAQUE,ANOTHER,17,08  #   5 ||< ceil_t:roof
*surf,floor,FLOR,-,-,-,grnd_floor,OPAQUE,GROUND,01,00  #   6 ||< ground profile  1
*surf,prt_tcorc,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,12,13  #   7 ||< prt_tcor:ceil_chi
*surf,prt_tcor1,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,01,04  #   8 ||< prt_t_cor:corid_1
*surf,prt_cth,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,12,12  #   9 ||< prt_th:ceil_chi
*surf,prt_ctc,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,10,07  #  10 ||< prt_tc:Unit_cd
*surf,prt_ctb,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,11,12  #  11 ||< prt_t2:ceil_fg
*surf,prt_tb,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,07,06  #  12 ||< prt_t_b:Unit_b
*surf,prt_cat,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,11,11  #  13 ||< prt_t1:ceil_fg
*surf,prt_ta,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,06,01  #  14 ||< prt_t:Unit_a
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,6,    48.00 0  # zone base list
