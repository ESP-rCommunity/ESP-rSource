*Geometry 1.1,GEN,L1_support # tag version, format, zone name
*date Sat Aug  6 12:28:20 2011  # latest file modification 
L1_support is the level one plant rooms and delivery and securit
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,12.00000,30.50000,47.95000  #   1
*vertex,24.25000,30.50000,47.95000  #   2
*vertex,24.25000,28.50000,47.95000  #   3
*vertex,38.00000,28.50000,47.95000  #   4
*vertex,51.00000,28.50000,47.95000  #   5
*vertex,51.00000,40.70000,47.95000  #   6
*vertex,48.75000,40.70000,47.95000  #   7
*vertex,46.75000,40.70000,47.95000  #   8
*vertex,44.50000,40.70000,47.95000  #   9
*vertex,40.25000,40.70000,47.95000  #  10
*vertex,37.50000,40.70000,47.95000  #  11
*vertex,35.25000,40.70000,47.95000  #  12
*vertex,33.25000,40.70000,47.95000  #  13
*vertex,30.50000,40.70000,47.95000  #  14
*vertex,26.00000,40.70000,47.95000  #  15
*vertex,22.75000,40.70000,47.95000  #  16
*vertex,19.50000,40.70000,47.95000  #  17
*vertex,16.75000,40.70000,47.95000  #  18
*vertex,14.50000,40.70000,47.95000  #  19
*vertex,12.00000,40.70000,47.95000  #  20
*vertex,12.00000,35.75000,47.95000  #  21
*vertex,12.00000,30.50000,51.69300  #  22
*vertex,24.25000,30.50000,51.69300  #  23
*vertex,24.25000,28.50000,51.69300  #  24
*vertex,38.00000,28.50000,51.69300  #  25
*vertex,51.00000,28.50000,51.69300  #  26
*vertex,51.00000,40.70000,51.69300  #  27
*vertex,48.75000,40.70000,51.69300  #  28
*vertex,46.75000,40.70000,51.69300  #  29
*vertex,44.50000,40.70000,51.69300  #  30
*vertex,40.25000,40.70000,51.69300  #  31
*vertex,37.50000,40.70000,51.69300  #  32
*vertex,35.25000,40.70000,51.69300  #  33
*vertex,33.25000,40.70000,51.69300  #  34
*vertex,30.50000,40.70000,51.69300  #  35
*vertex,26.00000,40.70000,51.69300  #  36
*vertex,22.75000,40.70000,51.69300  #  37
*vertex,19.50000,40.70000,51.69300  #  38
*vertex,16.75000,40.70000,51.69300  #  39
*vertex,14.50000,40.70000,51.69300  #  40
*vertex,12.00000,40.70000,51.69300  #  41
*vertex,12.00000,35.75000,51.69300  #  42
*vertex,50.00000,36.00000,47.95000  #  43
*vertex,50.00000,33.67664,47.95000  #  44
*vertex,50.00000,33.67664,51.69300  #  45
*vertex,50.00000,36.00000,51.69300  #  46
*vertex,50.00000,30.50000,47.95000  #  47
*vertex,50.00000,30.50000,51.69300  #  48
*vertex,51.00000,30.50000,47.95000  #  49
*vertex,51.00000,30.50000,51.69300  #  50
*vertex,51.00000,36.00000,51.69300  #  51
*vertex,51.00000,36.00000,47.95000  #  52
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,23,22  #  1
*edges,4,2,3,24,23  #  2
*edges,4,3,4,25,24  #  3
*edges,4,4,5,26,25  #  4
*edges,4,6,7,28,27  #  5
*edges,4,7,8,29,28  #  6
*edges,4,8,9,30,29  #  7
*edges,4,9,10,31,30  #  8
*edges,4,10,11,32,31  #  9
*edges,4,11,12,33,32  # 10
*edges,4,12,13,34,33  # 11
*edges,4,13,14,35,34  # 12
*edges,4,14,15,36,35  # 13
*edges,4,15,16,37,36  # 14
*edges,4,16,17,38,37  # 15
*edges,4,17,18,39,38  # 16
*edges,4,18,19,40,39  # 17
*edges,4,19,20,41,40  # 18
*edges,4,20,21,42,41  # 19
*edges,4,21,1,22,42  # 20
*edges,26,22,23,24,25,26,50,48,45,46,51,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42  # 21
*edges,26,1,21,20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,52,43,44,47,49,5,4,3,2  # 22
*edges,6,44,43,46,45,48,47  # 23
*edges,4,5,49,50,26  # 24
*edges,4,49,47,48,50  # 25
*edges,4,43,52,51,46  # 26
*edges,4,52,6,27,51  # 27
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,ptn_del_b,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,11,12  #   1 ||< ptn_901a:L1_training
*surf,ptn_del_a,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,11,11  #   2 ||< ptn_open_b:L1_training
*surf,ptn_open_b,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,10,16  #   3 ||< ptn_901:L1_open_off
*surf,ptn_open_a,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,10,15  #   4 ||< ptn_bulk:L1_open_off
*surf,stone_b,VERT,-,-,-,stone_masnry,OPAQUE,EXTERIOR,0,0  #   5 ||< external
*surf,louv_a,VERT,-,-,-,mull_90,OPAQUE,EXTERIOR,0,0  #   6 ||< external
*surf,stone_c,VERT,-,-,-,stone_masnry,OPAQUE,EXTERIOR,0,0  #   7 ||< external
*surf,louv_b,VERT,-,-,-,mull_90,OPAQUE,EXTERIOR,0,0  #   8 ||< external
*surf,stone_d,VERT,-,-,-,stone_masnry,OPAQUE,EXTERIOR,0,0  #   9 ||< external
*surf,door_b,VERT,-,-,-,db_lpasol,DSF4554_06nb,EXTERIOR,0,0  #  10 ||< external
*surf,louv_c,VERT,-,-,-,mull_90,OPAQUE,EXTERIOR,0,0  #  11 ||< external
*surf,stone_e,VERT,-,-,-,stone_masnry,OPAQUE,EXTERIOR,0,0  #  12 ||< external
*surf,louv_d,VERT,-,-,-,mull_90,OPAQUE,EXTERIOR,0,0  #  13 ||< external
*surf,stone_f,VERT,-,-,-,stone_masnry,OPAQUE,EXTERIOR,0,0  #  14 ||< external
*surf,louvre_e,VERT,-,-,-,mull_90,OPAQUE,EXTERIOR,0,0  #  15 ||< external
*surf,stone_g,VERT,-,-,-,stone_masnry,OPAQUE,EXTERIOR,0,0  #  16 ||< external
*surf,door_plant,VERT,-,-,-,mull_90,OPAQUE,EXTERIOR,0,0  #  17 ||< external
*surf,louvre_pl,VERT,-,-,-,mull_90,OPAQUE,EXTERIOR,0,0  #  18 ||< external
*surf,ptn_toilsw,VERT,-,-,-,conc_250,OPAQUE,ANOTHER,12,03  #  19 ||< ptn_corc_l1:L1_toilet
*surf,ptn_stairsw,VERT,-,-,-,conc_250,OPAQUE,ANOTHER,01,35  #  20 ||< ptn_corc_one:stair_1
*surf,ceiling,CEIL,-,-,-,slab_275,OPAQUE,ANOTHER,17,32  #  21 ||< ceiling_sp:L2_uf_plen
*surf,floor,FLOR,-,-,-,foundation,OPAQUE,GROUND,01,00  #  22 ||< ground profile  1
*surf,Wall-51,VERT,-,-,-,conc_250,OPAQUE,ANOTHER,02,26  #  23 ||< Wall-51:stair_2
*surf,ptn_toil,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,13,07  #  24 ||< ptn_cel_b:L1_toil_psg
*surf,ptn_str_a,VERT,-,-,-,conc_250,OPAQUE,ANOTHER,02,32  #  25 ||< ptnc_str_l1:stair_2
*surf,ptn_str_b,VERT,-,-,-,conc_250,OPAQUE,ANOTHER,02,54  #  26 ||< ptn_str_b:stair_2
*surf,stone_a,VERT,-,-,-,stone_masnry,OPAQUE,EXTERIOR,0,0  #  27 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,22,   445.80 0  # zone base list
