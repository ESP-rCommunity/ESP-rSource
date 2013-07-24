*Geometry 1.1,GEN,entry_core # tag version, format, zone name
*date Mon Apr  2 15:18:10 2012  # latest file modification 
entry_core is the entrance and core at ground level
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,26.00000,20.00000,0.35000  #   1
*vertex,26.00000,20.00000,3.15000  #   2
*vertex,29.00000,20.00000,0.35000  #   3
*vertex,29.00000,20.00000,3.15000  #   4
*vertex,29.00000,14.00000,0.35000  #   5
*vertex,29.00000,14.00000,3.15000  #   6
*vertex,25.00000,14.00000,0.35000  #   7
*vertex,25.00000,14.00000,3.15000  #   8
*vertex,25.00000,20.00000,3.15000  #   9
*vertex,29.00000,20.00000,4.20000  #  10
*vertex,25.00000,20.00000,4.20000  #  11
*vertex,29.00000,14.00000,4.20000  #  12
*vertex,21.00000,20.00000,3.15000  #  13
*vertex,24.00000,20.00000,3.15000  #  14
*vertex,21.00000,20.00000,4.20000  #  15
*vertex,21.00000,14.00000,3.15000  #  16
*vertex,21.00000,14.00000,4.20000  #  17
*vertex,25.00000,14.00000,4.20000  #  18
*vertex,24.00000,20.00000,0.35000  #  19
*vertex,21.00000,20.00000,0.35000  #  20
*vertex,21.00000,14.00000,0.35000  #  21
*vertex,21.20000,14.20000,3.15000  #  22
*vertex,28.80000,14.20000,3.15000  #  23
*vertex,28.80000,19.80000,3.15000  #  24
*vertex,21.20000,19.80000,3.15000  #  25
*vertex,24.00000,23.00000,0.35000  #  26
*vertex,26.00000,23.00000,0.35000  #  27
*vertex,25.89443,23.00000,0.49780  #  28
*vertex,24.10557,23.00000,0.49780  #  29
*vertex,24.10557,23.00000,3.00220  #  30
*vertex,25.89443,23.00000,3.00220  #  31
*vertex,26.00000,23.00000,3.15000  #  32
*vertex,24.00000,23.00000,3.15000  #  33
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,27,32,2  #  1
*edges,4,3,1,2,4  #  2
*edges,4,5,3,4,6  #  3
*edges,4,7,5,6,8  #  4
*edges,5,2,9,11,10,4  #  5
*edges,4,6,4,10,12  #  6
*edges,5,14,13,15,11,9  #  7
*edges,5,9,2,32,33,14  #  8
*edges,4,16,8,18,17  #  9
*edges,4,13,16,17,15  # 10
*edges,4,8,6,12,18  # 11
*edges,4,26,19,14,33  # 12
*edges,4,19,20,13,14  # 13
*edges,4,20,21,16,13  # 14
*edges,4,21,7,8,16  # 15
*edges,9,5,7,21,20,19,26,27,1,3  # 16
*edges,6,17,18,12,10,11,15  # 17
*edges,4,22,23,24,25  # 18
*edges,4,23,22,25,24  # 19
*edges,10,27,26,33,32,27,28,31,30,29,28  # 20
*edges,4,28,29,30,31  # 21
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,ptn_a,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,01,03  #   1 ||< west_1:office_e_0
*surf,ptn_b,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,01,14  #   2 ||< ptn_core_a:office_e_0
*surf,ptn_c,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,01,15  #   3 ||< ptn_cor_b:office_e_0
*surf,north_2,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,01,04  #   4 ||< north_2:office_e_0
*surf,core_c1_a,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,16,12  #   5 ||< core_c1_a:ceiling_g
*surf,core_c1_b,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,16,13  #   6 ||< core_c1_b:ceiling_g
*surf,core_c1w_a,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,16,14  #   7 ||< core_c1w_a:ceiling_g
*surf,over_entr,CEIL,-,-,-,ceiling_dr,OPAQUE,ANOTHER,16,17  #   8 ||< over_entr:ceiling_g
*surf,north_c1,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,16,10  #   9 ||< north_c1:ceiling_g
*surf,east_c1,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,16,09  #  10 ||< east_c1:ceiling_g
*surf,north_c2,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,16,04  #  11 ||< north_c1_f:ceiling_g
*surf,west_1,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,02,03  #  12 ||< west_1:office_w_0
*surf,ptn_e,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,02,14  #  13 ||< ptn_core_a:office_w_0
*surf,ptn_f,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,02,15  #  14 ||< ptn_cor_b:office_w_0
*surf,ptn_g,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,02,04  #  15 ||< north_2:office_w_0
*surf,slab_floor,FLOR,-,-,-,foundation,OPAQUE,GROUND,00,01  #  16 ||< user def grnd profile  1
*surf,upper_at_slb,CEIL,-,-,-,inter_flrev,OPAQUE,ANOTHER,08,13  #  17 ||< base:core_one
*surf,drop_ceil,CEIL,-,-,-,ceiling_dr,OPAQUE,ANOTHER,07,19  #  18 ||< drop_ceilr:entry_core
*surf,drop_ceilr,FLOR,-,-,-,ceiling_dr,OPAQUE,ANOTHER,07,18  #  19 ||< drop_ceil:entry_core
*surf,inner_frame,VERT,-,-,-,window_frame,OPAQUE,ANOTHER,22,04  #  20 ||< inner_frame:vestibule
*surf,inner_door,VERT,inner_frame,-,-,double_glz,DC_8074_04nb,ANOTHER,22,05  #  21 ||< inner_door:vestibule
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,0,52.00,1  # zone base
