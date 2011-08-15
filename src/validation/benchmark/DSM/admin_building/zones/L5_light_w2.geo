*Geometry 1.1,GEN,L5_light_w2 # tag version, format, zone name
*date Mon Aug 15 19:40:38 2011  # latest file modification 
L5_light_w2 describes light well two at level 5
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,34.25000,21.30000,64.15000  #   1
*vertex,38.00000,21.30000,64.15000  #   2
*vertex,38.00000,28.50000,64.15000  #   3
*vertex,34.25000,28.50000,64.15000  #   4
*vertex,34.25000,21.30000,67.37500  #   5
*vertex,38.00000,21.30000,67.37500  #   6
*vertex,38.00000,28.50000,67.37500  #   7
*vertex,34.25000,28.50000,67.37500  #   8
*vertex,34.25000,21.30000,65.15000  #   9
*vertex,38.00000,21.30000,65.15000  #  10
*vertex,38.00000,28.50000,65.15000  #  11
*vertex,34.25000,28.50000,65.15000  #  12
*vertex,34.25000,21.30000,63.39300  #  13
*vertex,38.00000,21.30000,63.39300  #  14
*vertex,38.00000,28.50000,63.39300  #  15
*vertex,34.25000,28.50000,63.39300  #  16
*vertex,34.25000,21.30000,68.10000  #  17
*vertex,38.00000,21.30000,68.10000  #  18
*vertex,38.00000,28.50000,68.10000  #  19
*vertex,34.25000,28.50000,68.10000  #  20
*vertex,34.25000,21.30000,69.10000  #  21
*vertex,34.25000,28.50000,69.10000  #  22
*vertex,38.00000,21.30000,70.10000  #  23
*vertex,38.00000,28.50000,70.10000  #  24
*vertex,34.44794,28.11994,69.15279  #  25
*vertex,34.44794,21.68006,69.15279  #  26
*vertex,37.80205,21.68006,70.04722  #  27
*vertex,37.80205,28.11994,70.04722  #  28
*vertex,38.00000,21.68006,68.20557  #  29
*vertex,38.00000,28.11994,68.20557  #  30
*vertex,38.00000,28.11994,69.99442  #  31
*vertex,38.00000,21.68006,69.99442  #  32
*vertex,34.25000,28.11994,68.15279  #  33
*vertex,34.25000,21.68006,68.15279  #  34
*vertex,34.25000,21.68006,69.04721  #  35
*vertex,34.25000,28.11994,69.04721  #  36
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
*edges,10,18,19,24,23,18,29,32,31,30,29  # 17
*edges,4,7,8,20,19  # 18
*edges,4,19,20,22,24  # 19
*edges,4,8,5,17,20  # 20
*edges,10,20,17,21,22,20,33,36,35,34,33  # 21
*edges,4,25,26,27,28  # 22
*edges,4,29,30,31,32  # 23
*edges,4,33,34,35,36  # 24
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,l3_lw2_fr,VERT,-,-,-,fictitious,SC_fictit,UNKNOWN,0,0  #   1 ||< not yet defined
*surf,l3_lw2_ri,VERT,-,-,-,fictitious,SC_fictit,UNKNOWN,0,0  #   2 ||< not yet defined
*surf,l3_lw2_bk,VERT,-,-,-,glaz_ptn,SCF8477_10nb,UNKNOWN,0,0  #   3 ||< not yet defined
*surf,l3_lw2_lef,VERT,-,-,-,fictitious,SC_fictit,UNKNOWN,0,0  #   4 ||< not yet defined
*surf,up_l3_lw2,SLOP,-,-,-,mull_90,OPAQUE,EXTERIOR,0,0  #   5 ||< external
*surf,l3_lw2_base,FLOR,-,-,-,fictitious,SC_fictit,UNKNOWN,48,05  #   6 ||< not yet defined
*surf,left_rail,VERT,-,-,-,glaz_ptn,SCF8477_10nb,UNKNOWN,0,0  #   7 ||< not yet defined
*surf,front_rail,VERT,-,-,-,glaz_ptn,SCF8477_10nb,UNKNOWN,0,0  #   8 ||< not yet defined
*surf,right_rail,VERT,-,-,-,glaz_ptn,SCF8477_10nb,UNKNOWN,0,0  #   9 ||< not yet defined
*surf,l3_flrv_r,VERT,-,-,-,slab_275,OPAQUE,UNKNOWN,0,0  #  10 ||< not yet defined
*surf,l3_flrv_f,VERT,-,-,-,slab_275,OPAQUE,UNKNOWN,0,0  #  11 ||< not yet defined
*surf,l3_flrv_l,VERT,-,-,-,slab_275,OPAQUE,UNKNOWN,0,0  #  12 ||< not yet defined
*surf,l3_flrv_b,VERT,-,-,-,slab_275,OPAQUE,UNKNOWN,0,0  #  13 ||< not yet defined
*surf,parap_f,VERT,-,-,-,stone_masnry,OPAQUE,UNKNOWN,0,0  #  14 ||< not yet defined
*surf,glaz_fr,VERT,-,-,-,db_lpasol,DSF4554_06nb,UNKNOWN,0,0  #  15 ||< not yet defined
*surf,parap_ri,VERT,-,-,-,stone_masnry,OPAQUE,EXTERIOR,0,0  #  16 ||< external
*surf,frame_ri,VERT,-,-,-,mull_90,OPAQUE,EXTERIOR,0,0  #  17 ||< external
*surf,parap_bk,VERT,-,-,-,stone_masnry,OPAQUE,EXTERIOR,0,0  #  18 ||< external
*surf,glaz_bk,VERT,-,-,-,db_lpasol,DSF4554_06nb,EXTERIOR,0,0  #  19 ||< external
*surf,parap_left,VERT,-,-,-,stone_masnry,OPAQUE,EXTERIOR,0,0  #  20 ||< external
*surf,frame_left,VERT,-,-,-,mull_90,OPAQUE,EXTERIOR,0,0  #  21 ||< external
*surf,up_glaz,SLOP,up_l3_lw2,C-WINDOW,CLOSED,db_lpasol,TRAN,EXTERIOR,0,0  #  22 ||< external
*surf,glaz_ri,VERT,frame_ri,C-WINDOW,CLOSED,db_lpasol,TRAN,EXTERIOR,0,0  #  23 ||< external
*surf,glaz_left,VERT,frame_left,C-WINDOW,CLOSED,db_lpasol,TRAN,EXTERIOR,0,0  #  24 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,6,    27.00 0  # zone base list
