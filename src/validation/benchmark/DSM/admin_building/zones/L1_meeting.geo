*Geometry 1.1,GEN,L1_meeting # tag version, format, zone name
*date Sat Feb 11 16:55:08 2012  # latest file modification 
L1_meeting describes corner meeting adj career services
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,51.60000,23.17941,47.95000  #   1
*vertex,51.60000,21.00000,47.95000  #   2
*vertex,51.60000,19.23701,47.95000  #   3
*vertex,51.60000,18.19204,47.95000  #   4
*vertex,51.60000,17.28956,47.95000  #   5
*vertex,51.60000,17.00000,47.95000  #   6
*vertex,52.01118,17.14706,47.95000  #   7
*vertex,54.10112,17.66955,47.95000  #   8
*vertex,56.57106,18.23954,47.95000  #   9
*vertex,58.30000,18.66702,47.95000  #  10
*vertex,58.30000,19.90000,47.95000  #  11
*vertex,58.30000,20.50000,47.95000  #  12
*vertex,58.30000,23.08441,47.95000  #  13
*vertex,51.60000,23.17941,51.69300  #  14
*vertex,51.60000,21.00000,51.69300  #  15
*vertex,51.60000,19.23701,51.69300  #  16
*vertex,51.60000,18.19204,51.69300  #  17
*vertex,51.60000,17.28956,51.69300  #  18
*vertex,51.60000,17.00000,51.69300  #  19
*vertex,52.01118,17.14706,51.69300  #  20
*vertex,54.10112,17.66955,51.69300  #  21
*vertex,56.57106,18.23954,51.69300  #  22
*vertex,58.30000,18.66702,51.69300  #  23
*vertex,58.30000,19.90000,51.69300  #  24
*vertex,58.30000,20.50000,51.69300  #  25
*vertex,58.30000,23.08441,51.69300  #  26
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,15,14  #  1
*edges,4,2,3,16,15  #  2
*edges,4,3,4,17,16  #  3
*edges,4,4,5,18,17  #  4
*edges,4,5,6,19,18  #  5
*edges,4,6,7,20,19  #  6
*edges,4,7,8,21,20  #  7
*edges,4,8,9,22,21  #  8
*edges,4,9,10,23,22  #  9
*edges,4,10,11,24,23  # 10
*edges,4,11,12,25,24  # 11
*edges,4,12,13,26,25  # 12
*edges,4,13,1,14,26  # 13
*edges,13,14,15,16,17,18,19,20,21,22,23,24,25,26  # 14
*edges,13,1,13,12,11,10,9,8,7,6,5,4,3,2  # 15
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,ptn_open_a,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,10,23  #   1 ||< ptn_meet:L1_open_off
*surf,ptn_open_b,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,08,20  #   2 ||< ptn_open_b:L1_careers
*surf,door,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,08,21  #   3 ||< door_mt:L1_careers
*surf,ptn_open_c,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,08,22  #   4 ||< ptn_open_c:L1_careers
*surf,ptn_frame,VERT,-,-,-,mull_90,OPAQUE,ANOTHER,08,23  #   5 ||< ptn_frame:L1_careers
*surf,fac_frame,VERT,-,WALL,-,mull_90,OPAQUE,EXTERIOR,0,0  #   6 ||< external
*surf,fac_glaz_a,VERT,-,C-WINDOW,CLOSED,db_lpasol,DSF4554_06nb,EXTERIOR,0,0  #   7 ||< external
*surf,fac_glaz_b,VERT,-,C-WINDOW,CLOSED,db_lpasol,DSF4554_06nb,EXTERIOR,0,0  #   8 ||< external
*surf,fac_glaz_c,VERT,-,C-WINDOW,CLOSED,db_lpasol,DSF4554_06nb,EXTERIOR,0,0  #   9 ||< external
*surf,pa_fac_l2,VERT,-,WALL,-,db_lpasol,DSF4554_06nb,EXTERIOR,0,0  #  10 ||< external
*surf,gl_fac_l2,VERT,-,C-WINDOW,CLOSED,db_lpasol,DSF4554_06nb,EXTERIOR,0,0  #  11 ||< external
*surf,pb_fac_l2,VERT,-,WALL,-,db_lpasol,DSF4554_06nb,EXTERIOR,0,0  #  12 ||< external
*surf,ptn_toilet,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,13,01  #  13 ||< pt_meeting:L1_toil_psg
*surf,ceiling,CEIL,-,-,-,slab_275,OPAQUE,ANOTHER,17,27  #  14 ||< ceiling_mt:L2_uf_plen
*surf,floor,FLOR,-,-,-,foundation,OPAQUE,GROUND,01,00  #  15 ||< ground profile  1
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,all_applicable   7 # list of surfs
  6  7  8  9 10 11 12
# 
*insol_calc,all_applicable   6 # insolation sources
  7  8  9 10 11 12
# 
*base_list,1,15,    35.33 0  # zone base list
# 
# block entities:
#  *obs = obstructions
*block_start, 20 20 # geometric blocks
*obs,58.400,17.700,51.700,0.100,0.900,16.500,0.000,1.00,stone_fin,stone_eldon  # block   1
*end_block
