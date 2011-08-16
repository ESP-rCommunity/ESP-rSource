*Geometry 1.1,GEN,L5_lobby # tag version, format, zone name
*date Wed Aug 17 06:41:30 2011  # latest file modification 
L5_lobby describes elevator lobby at level five
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,10.00000,17.75000,64.15000  #   1
*vertex,13.70000,17.75000,64.15000  #   2
*vertex,13.70000,21.30000,64.15000  #   3
*vertex,13.70000,28.50000,64.15000  #   4
*vertex,17.45000,28.50000,64.15000  #   5
*vertex,17.45000,30.50000,64.15000  #   6
*vertex,13.25000,30.50000,64.15000  #   7
*vertex,10.25000,30.50000,64.15000  #   8
*vertex,0.00000,30.50000,64.15000  #   9
*vertex,0.00000,28.50000,64.15000  #  10
*vertex,3.50000,28.50000,64.15000  #  11
*vertex,3.50000,24.00000,64.15000  #  12
*vertex,4.05000,24.00000,64.15000  #  13
*vertex,4.05000,21.25000,64.15000  #  14
*vertex,6.50000,21.25000,64.15000  #  15
*vertex,10.00000,21.25000,64.15000  #  16
*vertex,10.00000,17.75000,67.37500  #  17
*vertex,13.70000,17.75000,67.37500  #  18
*vertex,13.70000,21.30000,67.37500  #  19
*vertex,13.70000,28.50000,67.37500  #  20
*vertex,17.45000,28.50000,67.37500  #  21
*vertex,17.45000,30.50000,67.37500  #  22
*vertex,13.25000,30.50000,67.37500  #  23
*vertex,10.25000,30.50000,67.37500  #  24
*vertex,0.00000,30.50000,67.37500  #  25
*vertex,0.00000,28.50000,67.37500  #  26
*vertex,3.50000,28.50000,67.37500  #  27
*vertex,3.50000,24.00000,67.37500  #  28
*vertex,4.05000,24.00000,67.37500  #  29
*vertex,4.05000,21.25000,67.37500  #  30
*vertex,6.50000,21.25000,67.37500  #  31
*vertex,10.00000,21.25000,67.37500  #  32
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,18,17  #  1
*edges,4,2,3,19,18  #  2
*edges,4,3,4,20,19  #  3
*edges,4,4,5,21,20  #  4
*edges,4,5,6,22,21  #  5
*edges,4,6,7,23,22  #  6
*edges,4,7,8,24,23  #  7
*edges,4,8,9,25,24  #  8
*edges,4,9,10,26,25  #  9
*edges,4,10,11,27,26  # 10
*edges,4,11,12,28,27  # 11
*edges,4,12,13,29,28  # 12
*edges,4,13,14,30,29  # 13
*edges,4,14,15,31,30  # 14
*edges,4,15,16,32,31  # 15
*edges,4,16,1,17,32  # 16
*edges,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32  # 17
*edges,16,1,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2  # 18
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,ptn_conf,VERT,-,-,-,gyp_aco_gyp,OPAQUE,ANOTHER,65,08  #   1 ||< ptn_lobby:L5_confer
*surf,door_open,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,69,22  #   2 ||< door_2:L5_open_lft
*surf,ptn_open_a,VERT,-,-,-,glaz_ptn,SCF8477_10nb,UNKNOWN,0,0  #   3 ||< not yet defined
*surf,ptn_lw1,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,75,03  #   4 ||< l3_lw2_bk:L5_light_w1
*surf,door2_open,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,69,19  #   5 ||< door:L5_open_lft
*surf,ptn_k2,VERT,-,-,-,gyp_aco_gyp,OPAQUE,UNKNOWN,0,0  #   6 ||< not yet defined
*surf,ptn_toil,VERT,-,-,-,gyp_aco_gyp,OPAQUE,UNKNOWN,0,0  #   7 ||< not yet defined
*surf,ptn_stair,VERT,-,-,-,conc_250,OPAQUE,UNKNOWN,0,0  #   8 ||< not yet defined
*surf,gptn_open,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,77,13  #   9 ||< ptn_lob_dr:L5_sw_void
*surf,ptn_el_r,VERT,-,-,-,conc_250,OPAQUE,ANOTHER,03,19  #  10 ||< right_5:lifts
*surf,ptn_el_fr,VERT,-,-,-,conc_250,OPAQUE,ANOTHER,03,18  #  11 ||< front_5:lifts
*surf,ptn_k1_d,VERT,-,-,-,gyp_aco_gyp,OPAQUE,ANOTHER,66,06  #  12 ||< ptn_lby_e:L5_kitch2
*surf,ptn_k1_c,VERT,-,-,-,gyp_aco_gyp,OPAQUE,ANOTHER,66,05  #  13 ||< ptn_lby_d:L5_kitch2
*surf,ptn_k1b,VERT,-,-,-,gyp_aco_gyp,OPAQUE,ANOTHER,66,04  #  14 ||< ptn_lby_c:L5_kitch2
*surf,ptn_k1a,VERT,-,-,-,gyp_aco_gyp,OPAQUE,ANOTHER,66,03  #  15 ||< ptn_lby_b:L5_kitch2
*surf,ptn_k1,VERT,-,-,-,gyp_aco_gyp,OPAQUE,ANOTHER,66,02  #  16 ||< ptn_lby_a:L5_kitch2
*surf,roof,CEIL,-,-,-,roof_0.2,OPAQUE,EXTERIOR,00,00  #  17 ||< external
*surf,floor-lob,FLOR,-,-,-,flr_ov_pln,OPAQUE,UNKNOWN,0,0  #  18 ||< not yet defined
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,18,   120.29 0  # zone base list
