*Geometry 1.1,GEN,lifts # tag version, format, zone name
*date Wed Aug 17 06:11:22 2011  # latest file modification 
lifts describes the elevator shaft
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,0.00000,24.00000,52.45000  #   1
*vertex,3.50000,24.00000,52.45000  #   2
*vertex,3.50000,28.50000,52.45000  #   3
*vertex,0.00000,28.50000,52.45000  #   4
*vertex,0.00000,24.00000,55.59300  #   5
*vertex,3.50000,24.00000,55.59300  #   6
*vertex,3.50000,28.50000,55.59300  #   7
*vertex,0.00000,28.50000,55.59300  #   8
*vertex,0.00000,24.00000,47.95000  #   9
*vertex,3.50000,24.00000,47.95000  #  10
*vertex,3.50000,24.00000,51.69300  #  11
*vertex,0.00000,24.00000,51.69300  #  12
*vertex,3.50000,28.50000,47.95000  #  13
*vertex,3.50000,28.50000,51.69300  #  14
*vertex,0.00000,28.50000,47.95000  #  15
*vertex,0.00000,28.50000,51.69300  #  16
*vertex,0.00000,24.00000,56.35000  #  17
*vertex,3.50000,24.00000,56.35000  #  18
*vertex,3.50000,24.00000,59.49300  #  19
*vertex,0.00000,24.00000,59.49300  #  20
*vertex,3.50000,28.50000,56.35000  #  21
*vertex,3.50000,28.50000,59.49300  #  22
*vertex,0.00000,28.50000,56.35000  #  23
*vertex,0.00000,28.50000,59.49300  #  24
*vertex,0.00000,24.00000,60.25000  #  25
*vertex,3.50000,24.00000,60.25000  #  26
*vertex,3.50000,24.00000,63.39300  #  27
*vertex,0.00000,24.00000,63.39300  #  28
*vertex,3.50000,28.50000,60.25000  #  29
*vertex,3.50000,28.50000,63.39300  #  30
*vertex,0.00000,28.50000,60.25000  #  31
*vertex,0.00000,28.50000,63.39300  #  32
*vertex,0.00000,24.00000,64.15000  #  33
*vertex,3.50000,24.00000,64.15000  #  34
*vertex,3.50000,24.00000,67.29300  #  35
*vertex,0.00000,24.00000,67.29300  #  36
*vertex,3.50000,28.50000,64.15000  #  37
*vertex,3.50000,28.50000,67.29300  #  38
*vertex,0.00000,28.50000,64.15000  #  39
*vertex,0.00000,28.50000,67.29300  #  40
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,6,5  #  1
*edges,4,2,3,7,6  #  2
*edges,4,3,4,8,7  #  3
*edges,4,4,1,5,8  #  4
*edges,4,9,10,11,12  #  5
*edges,4,10,13,14,11  #  6
*edges,4,13,15,16,14  #  7
*edges,4,15,9,12,16  #  8
*edges,4,17,18,19,20  #  9
*edges,4,18,21,22,19  # 10
*edges,4,21,23,24,22  # 11
*edges,4,23,17,20,24  # 12
*edges,4,25,26,27,28  # 13
*edges,4,26,29,30,27  # 14
*edges,4,29,31,32,30  # 15
*edges,4,31,25,28,32  # 16
*edges,4,33,34,35,36  # 17
*edges,4,34,37,38,35  # 18
*edges,4,37,39,40,38  # 19
*edges,4,39,33,36,40  # 20
*edges,4,12,11,2,1  # 21
*edges,4,5,6,18,17  # 22
*edges,4,20,19,26,25  # 23
*edges,4,28,27,34,33  # 24
*edges,4,11,14,3,2  # 25
*edges,4,6,7,21,18  # 26
*edges,4,19,22,29,26  # 27
*edges,4,27,30,37,34  # 28
*edges,4,14,16,4,3  # 29
*edges,4,7,8,23,21  # 30
*edges,4,22,24,31,29  # 31
*edges,4,30,32,39,37  # 32
*edges,4,15,13,10,9  # 33
*edges,4,16,12,1,4  # 34
*edges,4,8,5,17,23  # 35
*edges,4,24,20,25,31  # 36
*edges,4,32,28,33,39  # 37
*edges,4,36,35,38,40  # 38
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,left,VERT,-,-,-,conc_250,OPAQUE,ANOTHER,23,20  #   1 ||< elev_left:L2_cust_srv
*surf,front,VERT,-,-,-,conc_250,OPAQUE,ANOTHER,23,19  #   2 ||< elev_front:L2_cust_srv
*surf,right,VERT,-,-,-,conc_250,OPAQUE,ANOTHER,23,18  #   3 ||< elev_right:L2_cust_srv
*surf,back,VERT,-,-,-,stn_stair,OPAQUE,ANOTHER,34,12  #   4 ||< ptn_elev:L2_se_void
*surf,left_1,VERT,-,-,-,conc_250,OPAQUE,ANOTHER,11,21  #   5 ||< left_1:L1_training
*surf,front_1,VERT,-,-,-,conc_250,OPAQUE,ANOTHER,11,22  #   6 ||< front_1:L1_training
*surf,right_1,VERT,-,-,-,conc_250,OPAQUE,ANOTHER,11,23  #   7 ||< right_1:L1_training
*surf,back_1,VERT,-,-,-,stn_stair,OPAQUE,ANOTHER,15,12  #   8 ||< ptn_elev:L1_sw_void
*surf,left_3,VERT,-,-,-,conc_250,OPAQUE,ANOTHER,48,04  #   9 ||< left_3:L3_open_lft
*surf,front_3,VERT,-,-,-,conc_250,OPAQUE,ANOTHER,48,05  #  10 ||< front_3:L3_open_lft
*surf,right_3,VERT,-,-,-,conc_250,OPAQUE,ANOTHER,48,06  #  11 ||< right_3:L3_open_lft
*surf,back_3,VERT,-,-,-,stn_stair,OPAQUE,ANOTHER,47,12  #  12 ||< ptn_l3el:L3_sw_void
*surf,left_4,VERT,-,-,-,conc_250,OPAQUE,UNKNOWN,0,0  #  13 ||< not yet defined
*surf,front_4,VERT,-,-,-,conc_250,OPAQUE,UNKNOWN,0,0  #  14 ||< not yet defined
*surf,right_4,VERT,-,-,-,conc_250,OPAQUE,UNKNOWN,0,0  #  15 ||< not yet defined
*surf,back_4,VERT,-,-,-,stn_stair,OPAQUE,ANOTHER,51,12  #  16 ||< Wall-12:L4_sw_void
*surf,left_5,VERT,-,-,-,conc_250,OPAQUE,ANOTHER,66,07  #  17 ||< ptn_elev:L5_kitch2
*surf,front_5,VERT,-,-,-,conc_250,OPAQUE,ANOTHER,67,11  #  18 ||< ptn_el_fr:L5_lobby
*surf,right_5,VERT,-,-,-,conc_250,OPAQUE,ANOTHER,67,10  #  19 ||< ptn_el_r:L5_lobby
*surf,back_5,VERT,-,-,-,stn_stair,OPAQUE,ANOTHER,77,12  #  20 ||< ptn_elev:L5_sw_void
*surf,rflra_l2,VERT,-,-,-,conc_250,OPAQUE,ANOTHER,17,06  #  21 ||< rflra_l2:L2_uf_plen
*surf,rflra_l3,VERT,-,-,-,conc_250,OPAQUE,ANOTHER,50,06  #  22 ||< rflra_l3:L3_ufloor
*surf,rflra_l4,VERT,-,-,-,conc_250,OPAQUE,UNKNOWN,0,0  #  23 ||< not yet defined
*surf,rflra_l5,VERT,-,-,-,conc_250,OPAQUE,UNKNOWN,0,0  #  24 ||< not yet defined
*surf,rflrb_l2,VERT,-,-,-,conc_250,OPAQUE,ANOTHER,17,08  #  25 ||< rflrb_l2:L2_uf_plen
*surf,rflrb_l3,VERT,-,-,-,conc_250,OPAQUE,ANOTHER,50,07  #  26 ||< rflrb_l3:L3_ufloor
*surf,rflrb_l4,VERT,-,-,-,conc_250,OPAQUE,UNKNOWN,0,0  #  27 ||< not yet defined
*surf,rflrb_l5,VERT,-,-,-,conc_250,OPAQUE,UNKNOWN,0,0  #  28 ||< not yet defined
*surf,rflrc_l2,VERT,-,-,-,conc_250,OPAQUE,ANOTHER,17,07  #  29 ||< rflrc_l2:L2_uf_plen
*surf,rflrc_l3,VERT,-,-,-,conc_250,OPAQUE,ANOTHER,50,08  #  30 ||< rflrc_l3:L3_ufloor
*surf,rflrc_l4,VERT,-,-,-,conc_250,OPAQUE,UNKNOWN,0,0  #  31 ||< not yet defined
*surf,rflrc_l5,VERT,-,-,-,conc_250,OPAQUE,UNKNOWN,0,0  #  32 ||< not yet defined
*surf,base,FLOR,-,-,-,foundation,OPAQUE,GROUND,01,00  #  33 ||< ground profile  1
*surf,ptn_vd2,VERT,-,-,-,slab_275,OPAQUE,ANOTHER,15,33  #  34 ||< elev_rflr:L1_sw_void
*surf,ptn_vd3,VERT,-,-,-,stn_stair,OPAQUE,ANOTHER,47,22  #  35 ||< ptn_vd3:L3_sw_void
*surf,ptn_vd4,VERT,-,-,-,stn_stair,OPAQUE,ANOTHER,51,22  #  36 ||< ptn_vd3:L4_sw_void
*surf,ptn_vd5,VERT,-,-,-,stn_stair,OPAQUE,ANOTHER,77,22  #  37 ||< ptn_vd3:L5_sw_void
*surf,roof,CEIL,-,-,-,roof_0.2,OPAQUE,EXTERIOR,0,0  #  38 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,33,    15.75 0  # zone base list
