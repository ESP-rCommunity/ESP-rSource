*Geometry 1.1,GEN,core_two # tag version, format, zone name
*date Mon Apr  2 12:39:57 2012  # latest file modification 
core_two is core zone of 2nd level of office building
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,25.00000,20.00000,8.05000  #   1
*vertex,25.00000,20.00000,10.85000  #   2
*vertex,34.00000,20.00000,8.05000  #   3
*vertex,34.00000,20.00000,10.85000  #   4
*vertex,34.00000,14.00000,8.05000  #   5
*vertex,34.00000,14.00000,10.85000  #   6
*vertex,25.00000,14.00000,8.05000  #   7
*vertex,25.00000,14.00000,10.85000  #   8
*vertex,16.00000,20.00000,8.05000  #   9
*vertex,16.00000,20.00000,10.85000  #  10
*vertex,16.00000,14.00000,8.05000  #  11
*vertex,16.00000,14.00000,10.85000  #  12
*vertex,34.00000,20.00000,11.90000  #  13
*vertex,25.00000,20.00000,11.90000  #  14
*vertex,34.00000,14.00000,11.90000  #  15
*vertex,16.00000,20.00000,11.90000  #  16
*vertex,16.00000,14.00000,11.90000  #  17
*vertex,25.00000,14.00000,11.90000  #  18
*vertex,21.00000,14.00000,8.05000  #  19
*vertex,29.00000,14.00000,8.05000  #  20
*vertex,29.00000,20.00000,8.05000  #  21
*vertex,21.00000,20.00000,8.05000  #  22
*vertex,29.00000,14.00000,11.90000  #  23
*vertex,21.00000,14.00000,11.90000  #  24
*vertex,21.00000,20.00000,11.90000  #  25
*vertex,29.00000,20.00000,11.90000  #  26
# 
# tag, number of vertices followed by list of associated vert
*edges,5,3,21,1,2,4  #  1
*edges,4,5,3,4,6  #  2
*edges,5,7,20,5,6,8  #  3
*edges,5,1,22,9,10,2  #  4
*edges,4,9,11,12,10  #  5
*edges,5,11,19,7,8,12  #  6
*edges,5,4,2,14,26,13  #  7
*edges,4,6,4,13,15  #  8
*edges,5,2,10,16,25,14  #  9
*edges,4,10,12,17,16  # 10
*edges,5,12,8,18,24,17  # 11
*edges,5,8,6,15,23,18  # 12
*edges,4,5,20,21,3  # 13
*edges,4,17,24,25,16  # 14
*edges,6,7,19,22,1,21,20  # 15
*edges,4,19,11,9,22  # 16
*edges,6,18,23,26,14,25,24  # 17
*edges,4,23,15,13,26  # 18
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,ptn_core_a,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,15,04  #   1 ||< ptn_core_a:passage_2
*surf,ptn_cor_b,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,15,05  #   2 ||< ptn_cor_b:passage_2
*surf,north_2,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,15,01  #   3 ||< north_2:passage_2
*surf,ptn_d,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,15,13  #   4 ||< ptn_core_aw:passage_2
*surf,ptn_e,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,15,11  #   5 ||< ptn_cor_bw:passage_2
*surf,ptn_f,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,15,10  #   6 ||< north_2w:passage_2
*surf,core_c2_a,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,18,14  #   7 ||< core_c1_ap:ceiling_3
*surf,core_c2_b,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,18,10  #   8 ||< core_c1_b:ceiling_3
*surf,core_c2w_a,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,18,13  #   9 ||< core_c1w_ap:ceiling_3
*surf,east_c2,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,18,08  #  10 ||< east_c1:ceiling_3
*surf,north_c2wc,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,18,11  #  11 ||< north_c1p:ceiling_3
*surf,north_c2_c,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,18,12  #  12 ||< north_c2p:ceiling_3
*surf,base_e,FLOR,-,-,-,inter_flr,OPAQUE,ANOTHER,17,23  #  13 ||< core3_be:ceiling_2
*surf,top,CEIL,-,-,-,inter_flrev,OPAQUE,ANOTHER,06,16  #  14 ||< top_core3w:office_w_3
*surf,base_cor2,FLOR,-,-,-,inter_flrev,OPAQUE,ANOTHER,08,14  #  15 ||< top:core_one
*surf,base_w,FLOR,-,-,-,inter_flr,OPAQUE,ANOTHER,17,22  #  16 ||< base_w:ceiling_2
*surf,base_core_t,CEIL,-,-,-,inter_flr,OPAQUE,ANOTHER,10,08  #  17 ||< base:core_top
*surf,top_e,CEIL,-,-,-,inter_flrev,OPAQUE,ANOTHER,05,16  #  18 ||< top_e:office_e_3
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,3,15,16,13,   108.00 2  # zone base list
