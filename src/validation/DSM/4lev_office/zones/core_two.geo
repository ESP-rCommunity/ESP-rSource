*Geometry 1.1,GEN,core_two # tag version, format, zone name
*date Mon Apr  2 09:22:44 2012  # latest file modification 
core_mid is the core at the middle level
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,25.00000,20.00000,4.20000  #   1
*vertex,25.00000,20.00000,7.00000  #   2
*vertex,29.00000,20.00000,4.20000  #   3
*vertex,29.00000,20.00000,7.00000  #   4
*vertex,29.00000,14.00000,4.20000  #   5
*vertex,29.00000,14.00000,7.00000  #   6
*vertex,25.00000,14.00000,4.20000  #   7
*vertex,25.00000,14.00000,7.00000  #   8
*vertex,21.00000,20.00000,4.20000  #   9
*vertex,21.00000,20.00000,7.00000  #  10
*vertex,21.00000,14.00000,4.20000  #  11
*vertex,21.00000,14.00000,7.00000  #  12
*vertex,29.00000,20.00000,8.05000  #  13
*vertex,25.00000,20.00000,8.05000  #  14
*vertex,29.00000,14.00000,8.05000  #  15
*vertex,21.00000,20.00000,8.05000  #  16
*vertex,21.00000,14.00000,8.05000  #  17
*vertex,25.00000,14.00000,8.05000  #  18
# 
# tag, number of vertices followed by list of associated vert
*edges,4,3,1,2,4  #  1
*edges,4,5,3,4,6  #  2
*edges,4,7,5,6,8  #  3
*edges,4,1,9,10,2  #  4
*edges,4,9,11,12,10  #  5
*edges,4,11,7,8,12  #  6
*edges,4,4,2,14,13  #  7
*edges,4,6,4,13,15  #  8
*edges,4,2,10,16,14  #  9
*edges,4,10,12,17,16  # 10
*edges,4,12,8,18,17  # 11
*edges,4,8,6,15,18  # 12
*edges,6,5,7,11,9,1,3  # 13
*edges,6,17,18,15,13,14,16  # 14
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,ptn_core_a,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,03,14  #   1 ||< ptn_core_a:office_e_1
*surf,ptn_cor_b,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,03,15  #   2 ||< ptn_cor_b:office_e_1
*surf,north_2,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,03,04  #   3 ||< north_2:office_e_1
*surf,ptn_d,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,04,14  #   4 ||< ptn_core_a:office_w_1
*surf,ptn_e,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,04,15  #   5 ||< ptn_cor_b:office_w_1
*surf,ptn_f,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,04,04  #   6 ||< north_2:office_w_1
*surf,core_c1_a,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,17,12  #   7 ||< core_c1_a:ceiling_2
*surf,core_c1_b,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,17,13  #   8 ||< core_c1_b:ceiling_2
*surf,core_c1w_a,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,17,14  #   9 ||< core_c1w_a:ceiling_2
*surf,east_c1,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,17,09  #  10 ||< east_c1:ceiling_2
*surf,north_c1,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,17,10  #  11 ||< north_c1:ceiling_2
*surf,north_c2,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,17,04  #  12 ||< north_c2:ceiling_2
*surf,base,FLOR,-,-,-,inter_flr,OPAQUE,ANOTHER,07,17  #  13 ||< upper_at_slb:entry_core
*surf,top,CEIL,-,-,-,inter_flr,OPAQUE,ANOTHER,09,15  #  14 ||< base_cor2:core_three
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,13,    48.00 2  # zone base list
