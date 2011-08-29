*Geometry 1.1,GEN,L1_training # tag version, format, zone name
*date Sat Aug  6 12:26:59 2011  # latest file modification 
L1_training describes delivery and training rooms on level one
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,0.00000,13.75000,47.95000  #   1
*vertex,3.25000,13.75000,47.95000  #   2
*vertex,3.25000,21.50000,47.95000  #   3
*vertex,9.25000,21.50000,47.95000  #   4
*vertex,9.25000,13.75000,47.95000  #   5
*vertex,15.25000,13.75000,47.95000  #   6
*vertex,15.25000,21.25000,47.95000  #   7
*vertex,19.50000,21.25000,47.95000  #   8
*vertex,19.50000,14.50000,47.95000  #   9
*vertex,24.25000,15.75000,47.95000  #  10
*vertex,24.25000,21.25000,47.95000  #  11
*vertex,24.25000,30.50000,47.95000  #  12
*vertex,12.00000,30.50000,47.95000  #  13
*vertex,0.00000,30.50000,47.95000  #  14
*vertex,0.00000,35.70000,47.95000  #  15
*vertex,-4.25000,35.70000,47.95000  #  16
*vertex,-4.00000,28.50000,47.95000  #  17
*vertex,0.00000,28.50000,47.95000  #  18
*vertex,0.00000,24.00000,47.95000  #  19
*vertex,0.00000,21.50000,47.95000  #  20
*vertex,0.00000,13.75000,51.69300  #  21
*vertex,3.25000,13.75000,51.69300  #  22
*vertex,3.25000,21.50000,51.69300  #  23
*vertex,9.25000,21.50000,51.69300  #  24
*vertex,9.25000,13.75000,51.69300  #  25
*vertex,15.25000,13.75000,51.69300  #  26
*vertex,15.25000,21.25000,51.69300  #  27
*vertex,19.50000,21.25000,51.69300  #  28
*vertex,19.50000,14.50000,51.69300  #  29
*vertex,24.25000,15.75000,51.69300  #  30
*vertex,24.25000,21.25000,51.69300  #  31
*vertex,24.25000,30.50000,51.69300  #  32
*vertex,12.00000,30.50000,51.69300  #  33
*vertex,0.00000,30.50000,51.69300  #  34
*vertex,0.00000,35.70000,51.69300  #  35
*vertex,-4.25000,35.70000,51.69300  #  36
*vertex,-4.00000,28.50000,51.69300  #  37
*vertex,0.00000,28.50000,51.69300  #  38
*vertex,0.00000,24.00000,51.69300  #  39
*vertex,0.00000,21.50000,51.69300  #  40
*vertex,3.50000,24.00000,47.95000  #  41
*vertex,3.50000,24.00000,51.69300  #  42
*vertex,3.50000,28.50000,47.95000  #  43
*vertex,3.50000,28.50000,51.69300  #  44
*vertex,0.00000,16.10209,47.95000  #  45
*vertex,0.00000,18.66702,47.95000  #  46
*vertex,0.00000,18.66702,51.69300  #  47
*vertex,0.00000,16.10209,51.69300  #  48
*vertex,0.00000,21.13696,47.95000  #  49
*vertex,0.00000,21.13696,51.69300  #  50
*vertex,24.25000,28.50000,47.95000  #  51
*vertex,24.25000,28.50000,51.69300  #  52
*vertex,8.35979,30.50000,47.95000  #  53
*vertex,8.35979,30.50000,51.69300  #  54
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,22,21  #  1
*edges,4,2,3,23,22  #  2
*edges,4,3,4,24,23  #  3
*edges,4,4,5,25,24  #  4
*edges,4,5,6,26,25  #  5
*edges,4,6,7,27,26  #  6
*edges,4,7,8,28,27  #  7
*edges,4,8,9,29,28  #  8
*edges,4,9,10,30,29  #  9
*edges,4,10,11,31,30  # 10
*edges,4,51,12,32,52  # 11
*edges,4,12,13,33,32  # 12
*edges,4,13,53,54,33  # 13
*edges,4,14,15,35,34  # 14
*edges,4,15,16,36,35  # 15
*edges,4,16,17,37,36  # 16
*edges,4,17,18,38,37  # 17
*edges,6,19,20,49,50,40,39  # 18
*edges,24,21,22,23,24,25,26,27,28,29,30,31,52,32,33,54,34,38,44,42,39,40,50,47,48  # 19
*edges,27,1,45,46,49,20,19,41,43,18,17,16,15,14,53,13,12,51,11,10,9,8,7,6,5,4,3,2  # 20
*edges,4,41,19,39,42  # 21
*edges,4,43,41,42,44  # 22
*edges,4,18,43,44,38  # 23
*edges,4,46,45,48,47  # 24
*edges,4,49,46,47,50  # 25
*edges,4,45,1,21,48  # 26
*edges,4,11,51,52,31  # 27
*edges,4,53,14,34,54  # 28
*edges,5,34,35,36,37,38  # 29
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,ptn_entry_a,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,06,34  #   1 ||< ptn_entry_a:L1_ent_recp
*surf,ptn_entry_b,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,06,35  #   2 ||< ptn_entry_b:L1_ent_recp
*surf,ptn_entry_c,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,06,36  #   3 ||< ptn_entry_c:L1_ent_recp
*surf,ptn_entry_d,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,06,37  #   4 ||< ptn_entry_d:L1_ent_recp
*surf,ptn_entry_e,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,06,38  #   5 ||< ptn_entry_e:L1_ent_recp
*surf,ptn_entry_f,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,06,39  #   6 ||< ptn_entry_f:L1_ent_recp
*surf,ptn_entry_g,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,06,40  #   7 ||< ptn_entry_g:L1_ent_recp
*surf,ptn_entry_h,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,06,41  #   8 ||< ptn_entry_h:L1_ent_recp
*surf,ptn_entry_i,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,06,42  #   9 ||< ptn_entry_i:L1_ent_recp
*surf,ptn_open_a,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,10,18  #  10 ||< ptn_del_b:L1_open_off
*surf,ptn_open_b,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,14,02  #  11 ||< ptn_del_a:L1_support
*surf,ptn_901a,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,14,01  #  12 ||< ptn_del_b:L1_support
*surf,ptn_staira,VERT,-,-,-,conc_250,OPAQUE,ANOTHER,01,34  #  13 ||< ptn_corb_one:stair_1
*surf,ptn_stairb,VERT,-,-,-,conc_250,OPAQUE,ANOTHER,01,29  #  14 ||< ptn_voide_l1:stair_1
*surf,fac_serv,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,15,41  #  15 ||< fac_serv:L1_sw_void
*surf,ptn_void_sw,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,15,39  #  16 ||< ptn_void_sw:L1_sw_void
*surf,ptn_v_sw_b,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,15,40  #  17 ||< ptn_v_sw_b:L1_sw_void
*surf,ptn_v_sw_c,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,15,11  #  18 ||< ptn_door_b:L1_sw_void
*surf,ceiling_del,CEIL,-,-,-,slab_275,OPAQUE,ANOTHER,17,29  #  19 ||< ceiling_del:L2_uf_plen
*surf,floor,FLOR,-,-,-,flr_ov_pln,OPAQUE,ANOTHER,16,02  #  20 ||< floor_tr:L1_uf_plen
*surf,left_1,VERT,-,-,-,conc_250,OPAQUE,ANOTHER,03,05  #  21 ||< left_1:lifts
*surf,front_1,VERT,-,-,-,conc_250,OPAQUE,ANOTHER,03,06  #  22 ||< front_1:lifts
*surf,right_1,VERT,-,-,-,conc_250,OPAQUE,ANOTHER,03,07  #  23 ||< right_1:lifts
*surf,ptn_rd302,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,15,09  #  24 ||< ptn_rd302:L1_sw_void
*surf,ptn_rd301,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,15,10  #  25 ||< ptn_rd301:L1_sw_void
*surf,ptn_rd302a,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,15,08  #  26 ||< ptn_gl_c:L1_sw_void
*surf,ptn_del_a,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,10,17  #  27 ||< ptn_del_a:L1_open_off
*surf,ptn_cora_one,VERT,-,-,-,conc_250,OPAQUE,ANOTHER,01,33  #  28 ||< ptn_cora_one:stair_1
*surf,upper_srv_tr,CEIL,-,-,-,conc_250,OPAQUE,ANOTHER,15,42  #  29 ||< upper_srv_tr:L1_sw_void
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,20,   335.23 0  # zone base list
# 
# block entities:
#  *obs = obstructions
*block_start, 20 20 # geometric blocks
*obs,0.500,21.250,47.950,0.250,0.250,15.440,0.000,1.00,col_i,conc_250  # block  35
*obs,7.000,21.250,47.950,0.250,0.250,15.440,0.000,1.00,col_j,conc_250  # block  36
*obs,13.750,21.250,47.950,0.250,0.250,15.440,0.000,1.00,col_k,conc_250  # block  37
*obs,17.250,21.250,47.950,0.250,0.250,15.440,0.000,1.00,col_l,conc_250  # block  38
*obs,13.750,28.500,47.950,0.250,0.250,15.440,0.000,1.00,col_m,conc_250  # block  39
*obs,17.250,28.500,47.950,0.250,0.250,15.440,0.000,1.00,col_n,conc_250  # block  40
*end_block
