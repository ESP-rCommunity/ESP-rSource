*Geometry 1.1,GEN,L1_open_off # tag version, format, zone name
*date Sat Aug  6 12:26:21 2011  # latest file modification 
L1_open_off describes core open plan office behind curved wall
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,24.25000,15.75000,47.95000  #   1
*vertex,26.50000,15.75000,47.95000  #   2
*vertex,27.93000,16.67250,47.95000  #   3
*vertex,29.25000,17.25000,47.95000  #   4
*vertex,31.20750,17.33750,47.95000  #   5
*vertex,33.20250,16.81500,47.95000  #   6
*vertex,34.96000,16.19750,47.95000  #   7
*vertex,37.09750,15.91250,47.95000  #   8
*vertex,39.18750,15.96000,47.95000  #   9
*vertex,41.37250,16.29250,47.95000  #  10
*vertex,43.27250,16.95750,47.95000  #  11
*vertex,45.36250,18.14500,47.95000  #  12
*vertex,47.00000,19.50000,47.95000  #  13
*vertex,48.25000,21.00000,47.95000  #  14
*vertex,51.60000,21.00000,47.95000  #  15
*vertex,51.00000,28.50000,47.95000  #  16
*vertex,38.00000,28.50000,47.95000  #  17
*vertex,24.25000,28.50000,47.95000  #  18
*vertex,24.25000,21.25000,47.95000  #  19
*vertex,24.25000,15.75000,51.69300  #  20
*vertex,26.50000,15.75000,51.69300  #  21
*vertex,27.93000,16.67250,51.69300  #  22
*vertex,29.25000,17.25000,51.69300  #  23
*vertex,31.20750,17.33750,51.69300  #  24
*vertex,33.20250,16.81500,51.69300  #  25
*vertex,34.96000,16.19750,51.69300  #  26
*vertex,37.09750,15.91250,51.69300  #  27
*vertex,39.18750,15.96000,51.69300  #  28
*vertex,41.37250,16.29250,51.69300  #  29
*vertex,43.27250,16.95750,51.69300  #  30
*vertex,45.36250,18.14500,51.69300  #  31
*vertex,47.00000,19.50000,51.69300  #  32
*vertex,48.25000,21.00000,51.69300  #  33
*vertex,51.60000,21.00000,51.69300  #  34
*vertex,51.00000,28.50000,51.69300  #  35
*vertex,38.00000,28.50000,51.69300  #  36
*vertex,24.25000,28.50000,51.69300  #  37
*vertex,24.25000,21.25000,51.69300  #  38
*vertex,51.00000,23.17941,47.95000  #  39
*vertex,51.00000,23.17941,51.69300  #  40
*vertex,51.60000,23.17941,47.95000  #  41
*vertex,51.60000,23.17941,51.69300  #  42
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,21,20  #  1
*edges,4,2,3,22,21  #  2
*edges,4,3,4,23,22  #  3
*edges,4,4,5,24,23  #  4
*edges,4,5,6,25,24  #  5
*edges,4,6,7,26,25  #  6
*edges,4,7,8,27,26  #  7
*edges,4,8,9,28,27  #  8
*edges,4,9,10,29,28  #  9
*edges,4,10,11,30,29  # 10
*edges,4,11,12,31,30  # 11
*edges,4,12,13,32,31  # 12
*edges,4,13,14,33,32  # 13
*edges,4,14,15,34,33  # 14
*edges,4,16,17,36,35  # 15
*edges,4,17,18,37,36  # 16
*edges,4,18,19,38,37  # 17
*edges,4,19,1,20,38  # 18
*edges,21,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,42,40,35,36,37,38  # 19
*edges,21,1,19,18,17,16,39,41,15,14,13,12,11,10,9,8,7,6,5,4,3,2  # 20
*edges,4,39,16,35,40  # 21
*edges,4,41,39,40,42  # 22
*edges,4,15,41,42,34  # 23
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,door_open,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,06,46  #   1 ||< door_open:L1_ent_recp
*surf,seg_a,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,06,47  #   2 ||< ptn_seg_a:L1_ent_recp
*surf,seg_b,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,06,48  #   3 ||< ptn_seg_b:L1_ent_recp
*surf,seg_c,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,08,17  #   4 ||< ptn_gla_j:L1_careers
*surf,seg_d,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,08,16  #   5 ||< ptn_gla_i:L1_careers
*surf,seg_e,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,08,15  #   6 ||< ptn_gla_h:L1_careers
*surf,seg_f,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,08,14  #   7 ||< ptn_gla_g:L1_careers
*surf,seg_g,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,08,13  #   8 ||< ptn_gla_f:L1_careers
*surf,seg_h,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,08,12  #   9 ||< ptn_gla_e:L1_careers
*surf,seg_i,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,08,11  #  10 ||< ptn_gla_d:L1_careers
*surf,seg_j,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,08,10  #  11 ||< ptn_gla_c:L1_careers
*surf,seg_k,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,08,09  #  12 ||< ptn_gla_b:L1_careers
*surf,seg_l,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,08,08  #  13 ||< ptn_gla_a:L1_careers
*surf,door_serv,VERT,-,-,-,gyp_aco_gyp,OPAQUE,ANOTHER,08,07  #  14 ||< door_serv:L1_careers
*surf,ptn_bulk,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,14,04  #  15 ||< ptn_open_a:L1_support
*surf,ptn_901,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,14,03  #  16 ||< ptn_open_b:L1_support
*surf,ptn_del_a,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,11,27  #  17 ||< ptn_del_a:L1_training
*surf,ptn_del_b,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,11,10  #  18 ||< ptn_open_a:L1_training
*surf,ceiling,CEIL,-,-,-,slab_275,OPAQUE,ANOTHER,17,28  #  19 ||< ceiling_op:L2_uf_plen
*surf,floor,FLOR,-,-,-,flr_ov_pln,OPAQUE,ANOTHER,16,01  #  20 ||< floor_op:L1_uf_plen
*surf,ptna_opn_l1,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,13,08  #  21 ||< ptna_opn_l1:L1_toil_psg
*surf,ptnb_opn_l1,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,13,09  #  22 ||< ptnb_opn_l1:L1_toil_psg
*surf,ptn_meet,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,09,01  #  23 ||< ptn_open_a:L1_meeting
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,23,   308.89 0  # zone base list
# 
# block entities:
#  *obs = obstructions
*block_start, 20 20 # geometric blocks
*obs,24.000,28.500,47.950,0.250,0.250,15.440,0.000,1.00,col_o,door  # block  41
*obs,27.500,28.500,47.950,0.250,0.250,15.440,0.000,1.00,col_p,door  # block  42
*obs,24.000,21.250,47.950,0.250,0.250,15.440,0.000,1.00,col_q,door  # block  43
*obs,27.500,21.250,47.950,0.250,0.250,15.440,0.000,1.00,col_r,door  # block  44
*obs,34.250,21.250,47.950,0.250,0.250,15.440,0.000,1.00,col_s,door  # block  45
*obs,38.000,21.250,47.950,0.250,0.250,15.440,0.000,1.00,col_t,door  # block  46
*obs,34.250,28.500,47.950,0.250,0.250,15.440,0.000,1.00,col_u,door  # block  47
*obs,38.000,28.500,47.950,0.250,0.250,15.440,0.000,1.00,col_v,door  # block  48
*obs,44.500,28.500,47.950,0.250,0.250,15.440,0.000,1.00,col_w,door  # block  49
*obs,44.500,21.250,47.950,0.250,0.250,15.440,0.000,1.00,col_x,door  # block  50
*end_block
