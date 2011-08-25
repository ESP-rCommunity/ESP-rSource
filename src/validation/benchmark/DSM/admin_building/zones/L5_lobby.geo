*Geometry 1.1,GEN,L5_lobby # tag version, format, zone name
*date Fri Aug 26 06:12:25 2011  # latest file modification 
L5_lobby describes elevator lobby at level five
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,10.00000,17.75000,64.15000  #   1
*vertex,13.70000,17.75000,64.15000  #   2
*vertex,13.70000,21.30000,64.15000  #   3
*vertex,13.70000,28.50000,64.15000  #   4
*vertex,17.45000,28.50000,64.15000  #   5
*vertex,17.45000,30.50000,64.15000  #   6
*vertex,13.25000,30.50000,64.15000  #   7
*vertex,0.00000,30.50000,64.15000  #   8
*vertex,0.00000,28.50000,64.15000  #   9
*vertex,3.50000,28.50000,64.15000  #  10
*vertex,3.50000,24.00000,64.15000  #  11
*vertex,4.05000,24.00000,64.15000  #  12
*vertex,4.05000,21.25000,64.15000  #  13
*vertex,6.50000,21.25000,64.15000  #  14
*vertex,10.00000,21.25000,64.15000  #  15
*vertex,10.00000,17.75000,67.37500  #  16
*vertex,13.70000,17.75000,67.37500  #  17
*vertex,13.70000,21.30000,67.37500  #  18
*vertex,13.70000,28.50000,67.37500  #  19
*vertex,17.45000,28.50000,67.37500  #  20
*vertex,17.45000,30.50000,67.37500  #  21
*vertex,13.25000,30.50000,67.37500  #  22
*vertex,0.00000,30.50000,67.37500  #  23
*vertex,0.00000,28.50000,67.37500  #  24
*vertex,3.50000,28.50000,67.37500  #  25
*vertex,3.50000,24.00000,67.37500  #  26
*vertex,4.05000,24.00000,67.37500  #  27
*vertex,4.05000,21.25000,67.37500  #  28
*vertex,6.50000,21.25000,67.37500  #  29
*vertex,10.00000,21.25000,67.37500  #  30
*vertex,8.35979,30.50000,64.15000  #  31
*vertex,8.35979,30.50000,67.37500  #  32
*vertex,12.00000,30.50000,64.15000  #  33
*vertex,12.00000,30.50000,67.37500  #  34
*vertex,17.00000,30.50000,64.15000  #  35
*vertex,17.00000,30.50000,67.37500  #  36
*vertex,13.70000,21.30000,65.15000  #  37
*vertex,13.70000,28.50000,65.15000  #  38
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,17,16  #  1
*edges,5,2,3,37,18,17  #  2
*edges,5,4,5,20,19,38  #  3
*edges,4,5,6,21,20  #  4
*edges,4,8,9,24,23  #  5
*edges,4,9,10,25,24  #  6
*edges,4,10,11,26,25  #  7
*edges,4,11,12,27,26  #  8
*edges,4,12,13,28,27  #  9
*edges,4,13,14,29,28  # 10
*edges,4,14,15,30,29  # 11
*edges,4,15,1,16,30  # 12
*edges,18,16,17,18,19,20,21,36,22,34,32,23,24,25,26,27,28,29,30  # 13
*edges,18,1,15,14,13,12,11,10,9,8,31,33,7,35,6,5,4,3,2  # 14
*edges,4,31,8,23,32  # 15
*edges,4,33,31,32,34  # 16
*edges,4,7,33,34,22  # 17
*edges,4,35,7,22,36  # 18
*edges,4,18,37,38,19  # 19
*edges,4,3,4,38,37  # 20
*edges,4,6,35,36,21  # 21
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,ptn_conf,VERT,-,-,-,gyp_aco_gyp,OPAQUE,ANOTHER,65,08  #   1 ||< ptn_lobby:L5_confer
*surf,door_open,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,69,19  #   2 ||< door_2:L5_open_lft
*surf,ptn_lw1,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,75,03  #   3 ||< l3_lw2_bk:L5_light_w1
*surf,door2_open,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,69,18  #   4 ||< door:L5_open_lft
*surf,gptn_open,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,77,13  #   5 ||< ptn_lob_dr:L5_sw_void
*surf,ptn_el_r,VERT,-,-,-,conc_250,OPAQUE,ANOTHER,03,19  #   6 ||< right_5:lifts
*surf,ptn_el_fr,VERT,-,-,-,conc_250,OPAQUE,ANOTHER,03,18  #   7 ||< front_5:lifts
*surf,ptn_k1_d,VERT,-,-,-,gyp_aco_gyp,OPAQUE,ANOTHER,66,06  #   8 ||< ptn_lby_e:L5_kitch2
*surf,ptn_k1_c,VERT,-,-,-,gyp_aco_gyp,OPAQUE,ANOTHER,66,05  #   9 ||< ptn_lby_d:L5_kitch2
*surf,ptn_k1b,VERT,-,-,-,gyp_aco_gyp,OPAQUE,ANOTHER,66,04  #  10 ||< ptn_lby_c:L5_kitch2
*surf,ptn_k1a,VERT,-,-,-,gyp_aco_gyp,OPAQUE,ANOTHER,66,03  #  11 ||< ptn_lby_b:L5_kitch2
*surf,ptn_k1,VERT,-,-,-,gyp_aco_gyp,OPAQUE,ANOTHER,66,02  #  12 ||< ptn_lby_a:L5_kitch2
*surf,roof,CEIL,-,-,-,roof_0.2,OPAQUE,EXTERIOR,0,0  #  13 ||< external
*surf,floor-lob,FLOR,-,-,-,top_pln,OPAQUE,ANOTHER,81,13  #  14 ||< floor-lob:L5_uf_plen
*surf,ptn_cora_l5,VERT,-,-,-,conc_250,OPAQUE,ANOTHER,01,46  #  15 ||< ptn_cora_l5:stair_1
*surf,ptn_corb_l5,VERT,-,-,-,conc_250,OPAQUE,ANOTHER,01,47  #  16 ||< ptn_corb_l5:stair_1
*surf,ptn_loba,VERT,-,-,-,gyp_aco_gyp,OPAQUE,ANOTHER,73,01  #  17 ||< ptn_loba:L5_toilet
*surf,ptn_lobb,VERT,-,-,-,gyp_aco_gyp,OPAQUE,ANOTHER,73,02  #  18 ||< ptn_lobb:L5_toilet
*surf,l3_lw2_lef,VERT,-,-,-,fictitious,SC_fictit,ANOTHER,75,04  #  19 ||< l3_lw2_lef:L5_light_w1
*surf,left_rail,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,75,07  #  20 ||< left_rail:L5_light_w1
*surf,short,VERT,-,-,-,gyp_aco_gyp,OPAQUE,ANOTHER,68,11  #  21 ||< short:L5_lit_srv
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,14,   120.29 0  # zone base list
