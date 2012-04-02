*Geometry 1.1,GEN,core_top # tag version, format, zone name
*date Mon Apr  2 12:41:15 2012  # latest file modification 
core_top is the core at the top level
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,25.00000,20.00000,11.90000  #   1
*vertex,29.00000,20.00000,11.90000  #   2
*vertex,29.00000,20.00000,14.70000  #   3
*vertex,25.00000,20.00000,14.70000  #   4
*vertex,29.00000,14.00000,11.90000  #   5
*vertex,29.00000,14.00000,14.70000  #   6
*vertex,25.00000,14.00000,11.90000  #   7
*vertex,25.00000,14.00000,14.70000  #   8
*vertex,21.00000,20.00000,11.90000  #   9
*vertex,21.00000,20.00000,14.70000  #  10
*vertex,21.00000,14.00000,11.90000  #  11
*vertex,21.00000,14.00000,14.70000  #  12
# 
# tag, number of vertices followed by list of associated vert
*edges,4,2,1,4,3  #  1
*edges,4,5,2,3,6  #  2
*edges,4,7,5,6,8  #  3
*edges,4,1,9,10,4  #  4
*edges,4,9,11,12,10  #  5
*edges,4,11,7,8,12  #  6
*edges,6,8,6,3,4,10,12  #  7
*edges,6,5,7,11,9,1,2  #  8
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,ptn_core_a,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,05,14  #   1 ||< ptn_core_a:office_e_3
*surf,ptn_cor_b,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,05,15  #   2 ||< ptn_cor_b:office_e_3
*surf,ptn_f,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,05,04  #   3 ||< north_2:office_e_3
*surf,ptn_core_c,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,06,14  #   4 ||< ptn_core_a:office_w_3
*surf,ptn_cor_d,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,06,15  #   5 ||< ptn_cor_b:office_w_3
*surf,ptn_e,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,06,04  #   6 ||< north_2:office_w_3
*surf,core_top,CEIL,-,-,-,ceiling_dr,OPAQUE,ANOTHER,19,09  #   7 ||< core_top:roof
*surf,base,FLOR,-,-,-,inter_flrev,OPAQUE,ANOTHER,09,17  #   8 ||< base_core_t:core_two
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,8,    48.00 2  # zone base list
