*Geometry 1.1,GEN,L5_light_w1 # tag version, format, zone name
*date Sun Feb 12 08:20:10 2012  # latest file modification 
L5_light_w1 describes light well one at level 5
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,13.70000,21.30000,64.15000  #   1
*vertex,17.45000,21.30000,64.15000  #   2
*vertex,17.45000,28.50000,64.15000  #   3
*vertex,13.70000,28.50000,64.15000  #   4
*vertex,13.70000,21.30000,67.37500  #   5
*vertex,17.45000,21.30000,67.37500  #   6
*vertex,17.45000,28.50000,67.37500  #   7
*vertex,13.70000,28.50000,67.37500  #   8
*vertex,13.70000,21.30000,65.15000  #   9
*vertex,17.45000,21.30000,65.15000  #  10
*vertex,17.45000,28.50000,65.15000  #  11
*vertex,13.70000,28.50000,65.15000  #  12
*vertex,13.70000,21.30000,63.39300  #  13
*vertex,17.45000,21.30000,63.39300  #  14
*vertex,17.45000,28.50000,63.39300  #  15
*vertex,13.70000,28.50000,63.39300  #  16
*vertex,13.70000,21.30000,68.10000  #  17
*vertex,17.45000,21.30000,68.10000  #  18
*vertex,17.45000,28.50000,68.10000  #  19
*vertex,13.70000,28.50000,68.10000  #  20
*vertex,13.70000,21.30000,69.10000  #  21
*vertex,13.70000,28.50000,69.10000  #  22
*vertex,17.45000,21.30000,70.10000  #  23
*vertex,17.45000,28.50000,70.10000  #  24
*vertex,13.89795,28.11994,69.15279  #  25
*vertex,13.89795,21.68006,69.15279  #  26
*vertex,17.25205,21.68006,70.04721  #  27
*vertex,17.25205,28.11994,70.04721  #  28
*vertex,13.70000,28.11994,68.15279  #  29
*vertex,13.70000,21.68006,68.15279  #  30
*vertex,13.70000,21.68006,69.04721  #  31
*vertex,13.70000,28.11994,69.04721  #  32
*vertex,17.45000,21.68006,68.20557  #  33
*vertex,17.45000,28.11994,68.20557  #  34
*vertex,17.45000,28.11994,69.99442  #  35
*vertex,17.45000,21.68006,69.99442  #  36
# 
# tag, number of vertices followed by list of associated vert
*edges,4,10,6,5,9  #  1
*edges,4,11,7,6,10  #  2
*edges,6,3,4,12,8,7,11  #  3
*edges,4,9,5,8,12  #  4
*edges,10,21,23,24,22,21,25,28,27,26,25  #  5
*edges,4,13,16,15,14  #  6
*edges,4,4,1,9,12  #  7
*edges,4,1,2,10,9  #  8
*edges,4,2,3,11,10  #  9
*edges,4,14,15,3,2  # 10
*edges,4,13,14,2,1  # 11
*edges,4,16,13,1,4  # 12
*edges,4,15,16,4,3  # 13
*edges,4,5,6,18,17  # 14
*edges,4,17,18,23,21  # 15
*edges,4,6,7,19,18  # 16
*edges,10,18,19,24,23,18,33,36,35,34,33  # 17
*edges,4,7,8,20,19  # 18
*edges,4,19,20,22,24  # 19
*edges,4,8,5,17,20  # 20
*edges,10,20,17,21,22,20,29,32,31,30,29  # 21
*edges,4,25,26,27,28  # 22
*edges,4,29,30,31,32  # 23
*edges,4,33,34,35,36  # 24
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,l3_lw2_fr,VERT,-,-,-,fictitious,SC_fictit,ANOTHER,69,31  #   1 ||< l3_lw2_fr:L5_open_lft
*surf,l3_lw2_ri,VERT,-,-,-,fictitious,SC_fictit,ANOTHER,69,32  #   2 ||< l3_lw2_ri:L5_open_lft
*surf,l3_lw2_bk,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,67,03  #   3 ||< ptn_lw1:L5_lobby
*surf,l3_lw2_lef,VERT,-,-,-,fictitious,SC_fictit,ANOTHER,67,19  #   4 ||< l3_lw2_lef:L5_lobby
*surf,up_l3_lw2,SLOP,-,ROOF,FLAT,mull_90,OPAQUE,EXTERIOR,0,0  #   5 ||< external
*surf,l3_lw2_base,FLOR,-,-,-,fictitious,SC_fictit,ANOTHER,59,05  #   6 ||< up_l3_lw2:L4_light_w1
*surf,left_rail,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,67,20  #   7 ||< left_rail:L5_lobby
*surf,front_rail,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,69,33  #   8 ||< front_rail:L5_open_lft
*surf,right_rail,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,69,34  #   9 ||< right_rail:L5_open_lft
*surf,l3_flrv_r,VERT,-,-,-,slab_275,OPAQUE,ANOTHER,81,20  #  10 ||< l3_flrv_r:L5_uf_plen
*surf,l3_flrv_f,VERT,-,-,-,slab_275,OPAQUE,ANOTHER,81,21  #  11 ||< l3_flrv_f:L5_uf_plen
*surf,l3_flrv_l,VERT,-,-,-,slab_275,OPAQUE,ANOTHER,81,22  #  12 ||< l3_flrv_l:L5_uf_plen
*surf,l3_flrv_b,VERT,-,-,-,slab_275,OPAQUE,ANOTHER,81,23  #  13 ||< l3_flrv_b:L5_uf_plen
*surf,parap_f,VERT,-,WALL,-,stone_masnry,OPAQUE,EXTERIOR,0,0  #  14 ||< external
*surf,glaz_fr,VERT,-,C-WINDOW,CLOSED,db_lpasol,DSF4554_06nb,EXTERIOR,0,0  #  15 ||< external
*surf,parap_ri,VERT,-,WALL,-,stone_masnry,OPAQUE,EXTERIOR,0,0  #  16 ||< external
*surf,frame_ri,VERT,-,WALL,-,mull_90,OPAQUE,EXTERIOR,0,0  #  17 ||< external
*surf,parap_bk,VERT,-,WALL,-,stone_masnry,OPAQUE,EXTERIOR,0,0  #  18 ||< external
*surf,glaz_bk,VERT,-,C-WINDOW,CLOSED,db_lpasol,DSF4554_06nb,EXTERIOR,0,0  #  19 ||< external
*surf,parap_left,VERT,-,WALL,-,stone_masnry,OPAQUE,EXTERIOR,0,0  #  20 ||< external
*surf,frame_left,VERT,-,WALL,-,mull_90,OPAQUE,EXTERIOR,0,0  #  21 ||< external
*surf,up_glz,SLOP,up_l3_lw2,C-WINDOW,CLOSED,db_lpasol,DSF4554_06nb,EXTERIOR,0,0  #  22 ||< external
*surf,glaz_left,VERT,frame_left,C-WINDOW,CLOSED,db_lpasol,DSF4554_06nb,EXTERIOR,0,0  #  23 ||< external
*surf,glaz_right,VERT,frame_ri,C-WINDOW,CLOSED,db_lpasol,DSF4554_06nb,EXTERIOR,0,0  #  24 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,all_applicable   5 # insolation sources
 15 19 22 23 24
# 
*base_list,1,6,    27.00 0  # zone base list
