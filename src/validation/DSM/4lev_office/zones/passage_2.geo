*Geometry 1.1,GEN,passage_2 # tag version, format, zone name
*date Mon Apr  2 12:47:18 2012  # latest file modification 
passage_2 is the passages of level 2 wraps around core
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,34.00000,14.00000,8.05000  #   1
*vertex,25.00000,14.00000,8.05000  #   2
*vertex,34.00000,14.00000,10.85000  #   3
*vertex,25.00000,14.00000,10.85000  #   4
*vertex,25.00000,20.00000,8.05000  #   5
*vertex,25.00000,20.00000,10.85000  #   6
*vertex,34.00000,20.00000,10.85000  #   7
*vertex,34.00000,20.00000,8.05000  #   8
*vertex,25.00000,13.00000,8.05000  #   9
*vertex,25.00000,13.00000,10.85000  #  10
*vertex,35.00000,13.00000,8.05000  #  11
*vertex,35.00000,13.00000,10.85000  #  12
*vertex,35.00000,21.00000,8.05000  #  13
*vertex,35.00000,21.00000,10.85000  #  14
*vertex,25.00000,21.00000,10.85000  #  15
*vertex,25.00000,21.00000,8.05000  #  16
*vertex,15.00000,13.00000,8.05000  #  17
*vertex,15.00000,13.00000,10.85000  #  18
*vertex,16.00000,14.00000,8.05000  #  19
*vertex,16.00000,14.00000,10.85000  #  20
*vertex,16.00000,20.00000,8.05000  #  21
*vertex,16.00000,20.00000,10.85000  #  22
*vertex,15.00000,21.00000,8.05000  #  23
*vertex,15.00000,21.00000,10.85000  #  24
*vertex,27.00000,13.00000,8.05000  #  25
*vertex,23.00000,13.00000,8.05000  #  26
*vertex,27.00000,13.00000,10.85000  #  27
*vertex,23.00000,13.00000,10.85000  #  28
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,4,3  #  1
*edges,9,14,15,6,7,3,4,10,27,12  #  2
*edges,9,13,11,25,9,2,1,8,5,16  #  3
*edges,4,5,8,7,6  #  4
*edges,4,8,1,3,7  #  5
*edges,6,9,25,11,12,27,10  #  6
*edges,4,13,16,15,14  #  7
*edges,4,11,13,14,12  #  8
*edges,6,17,26,9,10,28,18  #  9
*edges,4,2,19,20,4  # 10
*edges,4,19,21,22,20  # 11
*edges,4,16,23,24,15  # 12
*edges,4,21,5,6,22  # 13
*edges,4,23,17,18,24  # 14
*edges,9,18,28,10,4,20,22,6,15,24  # 15
*edges,9,9,26,17,23,16,5,21,19,2  # 16
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,north_2,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,09,03  #   1 ||< north_2:core_two
*surf,top,CEIL,-,-,-,ceiling_dr,OPAQUE,ANOTHER,18,20  #   2 ||< top_epag:ceiling_3
*surf,bottom,FLOR,-,-,-,inter_flr,OPAQUE,ANOTHER,17,21  #   3 ||< bottom_ep2:ceiling_2
*surf,ptn_core_a,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,09,01  #   4 ||< ptn_core_a:core_two
*surf,ptn_cor_b,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,09,02  #   5 ||< ptn_cor_b:core_two
*surf,corid_a,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,12,17  #   6 ||< corid_ae:office_2_sth
*surf,corid_c,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,11,13  #   7 ||< corid_ce:office_2_nor
*surf,corid_b,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,13,09  #   8 ||< corid_b:office_e2e
*surf,corid_aw,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,12,05  #   9 ||< corid_a:office_2_sth
*surf,north_2w,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,09,06  #  10 ||< ptn_f:core_two
*surf,ptn_cor_bw,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,09,05  #  11 ||< ptn_e:core_two
*surf,corid_cw,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,11,06  #  12 ||< corid_c:office_2_nor
*surf,ptn_core_aw,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,09,04  #  13 ||< ptn_d:core_two
*surf,west_p_cor,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,14,09  #  14 ||< corid_b:office_w2w
*surf,top_w,CEIL,-,-,-,ceiling_dr,OPAQUE,ANOTHER,18,21  #  15 ||< top_wpag:ceiling_3
*surf,bottom_w,FLOR,-,-,-,inter_flr,OPAQUE,ANOTHER,17,20  #  16 ||< bottom_wp2:ceiling_2
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,2,3,16,    52.00 2  # zone base list
