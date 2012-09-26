*Geometry 1.1,GEN,entry_core # tag version, format, zone name
*date Mon Apr  2 09:22:27 2012  # latest file modification 
entry_core is the entrance and core at ground level
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,26.00000,26.00000,0.35000  #   1
*vertex,26.00000,20.00000,0.35000  #   2
*vertex,26.00000,20.00000,3.15000  #   3
*vertex,26.00000,26.00000,3.15000  #   4
*vertex,29.00000,20.00000,0.35000  #   5
*vertex,29.00000,20.00000,3.15000  #   6
*vertex,29.00000,14.00000,0.35000  #   7
*vertex,29.00000,14.00000,3.15000  #   8
*vertex,25.00000,14.00000,0.35000  #   9
*vertex,25.00000,14.00000,3.15000  #  10
*vertex,25.00000,20.00000,3.15000  #  11
*vertex,29.00000,20.00000,4.20000  #  12
*vertex,25.00000,20.00000,4.20000  #  13
*vertex,29.00000,14.00000,4.20000  #  14
*vertex,21.00000,20.00000,3.15000  #  15
*vertex,24.00000,20.00000,3.15000  #  16
*vertex,21.00000,20.00000,4.20000  #  17
*vertex,24.00000,26.00000,3.15000  #  18
*vertex,21.00000,14.00000,3.15000  #  19
*vertex,21.00000,14.00000,4.20000  #  20
*vertex,25.00000,14.00000,4.20000  #  21
*vertex,24.00000,20.00000,0.35000  #  22
*vertex,24.00000,26.00000,0.35000  #  23
*vertex,21.00000,20.00000,0.35000  #  24
*vertex,21.00000,14.00000,0.35000  #  25
*vertex,25.89443,26.00000,0.49780  #  26
*vertex,24.10557,26.00000,0.49780  #  27
*vertex,24.10557,26.00000,3.00220  #  28
*vertex,25.89443,26.00000,3.00220  #  29
*vertex,21.20000,14.20000,3.15000  #  30
*vertex,28.80000,14.20000,3.15000  #  31
*vertex,28.80000,19.80000,3.15000  #  32
*vertex,21.20000,19.80000,3.15000  #  33
# 
# tag, number of vertices followed by list of associated vert
*edges,4,2,1,4,3  #  1
*edges,4,5,2,3,6  #  2
*edges,4,7,5,6,8  #  3
*edges,4,9,7,8,10  #  4
*edges,5,3,11,13,12,6  #  5
*edges,4,8,6,12,14  #  6
*edges,5,16,15,17,13,11  #  7
*edges,5,11,3,4,18,16  #  8
*edges,4,19,10,21,20  #  9
*edges,4,15,19,20,17  # 10
*edges,4,10,8,14,21  # 11
*edges,4,23,22,16,18  # 12
*edges,4,22,24,15,16  # 13
*edges,4,24,25,19,15  # 14
*edges,4,25,9,10,19  # 15
*edges,9,7,9,25,24,22,23,1,2,5  # 16
*edges,6,20,21,14,12,13,17  # 17
*edges,10,1,23,18,4,1,26,29,28,27,26  # 18
*edges,4,26,27,28,29  # 19
*edges,4,30,31,32,33  # 20
*edges,4,31,30,33,32  # 21
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
*surf,core_c1_a,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,16,12  #   5 ||< core_c1_a:ceiling_1
*surf,core_c1_b,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,16,13  #   6 ||< core_c1_b:ceiling_1
*surf,core_c1w_a,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,16,14  #   7 ||< core_c1w_a:ceiling_1
*surf,over_entr,CEIL,-,-,-,ceiling_dr,OPAQUE,ANOTHER,16,17  #   8 ||< over_entr:ceiling_1
*surf,north_c1,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,16,10  #   9 ||< north_c1:ceiling_1
*surf,east_c1,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,16,09  #  10 ||< east_c1:ceiling_1
*surf,north_c2,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,16,04  #  11 ||< north_c2:ceiling_1
*surf,west_1,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,02,03  #  12 ||< west_1:office_w_0
*surf,ptn_e,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,02,14  #  13 ||< ptn_core_a:office_w_0
*surf,ptn_f,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,02,15  #  14 ||< ptn_cor_b:office_w_0
*surf,ptn_g,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,02,04  #  15 ||< north_2:office_w_0
*surf,slab_floor,FLOR,-,-,-,foundation,OPAQUE,GROUND,00,01  #  16 ||< user def grnd profile  1
*surf,upper_at_slb,CEIL,-,-,-,inter_flrev,OPAQUE,ANOTHER,08,13  #  17 ||< base:core_two
*surf,entry_frame,VERT,-,-,-,window_frame,OPAQUE,EXTERIOR,0,0  #  18 ||< external
*surf,entry_door,VERT,-,-,-,double_glz,DC_8074_04nb,EXTERIOR,0,0  #  19 ||< external
*surf,drop_ceil,CEIL,-,-,-,ceiling_dr,OPAQUE,ANOTHER,07,21  #  20 ||< drop_ceilr:entry_core
*surf,drop_ceilr,FLOR,-,-,-,ceiling_dr,OPAQUE,ANOTHER,07,20  #  21 ||< drop_ceil:entry_core
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,16,    52.00 2  # zone base list
