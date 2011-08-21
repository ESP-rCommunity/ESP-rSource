*Geometry 1.1,GEN,L1_circular # tag version, format, zone name
*date Sat Aug  6 12:24:39 2011  # latest file modification 
L1_circular describes a circular vent shaft at level one
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,28.92676,7.59981,47.95000  #   1
*vertex,27.88179,8.35979,47.95000  #   2
*vertex,27.88179,8.35979,51.69300  #   3
*vertex,28.92676,7.59981,51.69300  #   4
*vertex,27.21681,9.30976,47.95000  #   5
*vertex,27.21681,9.30976,51.69300  #   6
*vertex,26.93181,10.54473,47.95000  #   7
*vertex,26.93181,10.54473,51.69300  #   8
*vertex,26.97931,11.68470,47.95000  #   9
*vertex,26.97931,11.68470,51.69300  #  10
*vertex,27.40680,12.77717,47.95000  #  11
*vertex,27.40680,12.77717,51.69300  #  12
*vertex,28.07178,13.63215,47.95000  #  13
*vertex,28.07178,13.63215,51.69300  #  14
*vertex,29.16426,14.39213,47.95000  #  15
*vertex,29.16426,14.39213,51.69300  #  16
*vertex,30.39923,14.67713,47.95000  #  17
*vertex,30.39923,14.67713,51.69300  #  18
*vertex,31.53920,14.62963,47.95000  #  19
*vertex,31.53920,14.62963,51.69300  #  20
*vertex,32.96416,14.01214,47.95000  #  21
*vertex,32.96416,14.01214,51.69300  #  22
*vertex,34.25000,13.06217,47.95000  #  23
*vertex,34.25000,13.06217,51.69300  #  24
*vertex,34.50000,12.11219,47.95000  #  25
*vertex,34.50000,12.11219,51.69300  #  26
*vertex,34.60000,10.97222,47.95000  #  27
*vertex,34.60000,10.97222,51.69300  #  28
*vertex,34.38913,9.92725,47.95000  #  29
*vertex,34.38913,9.92725,51.69300  #  30
*vertex,33.86664,8.78728,47.95000  #  31
*vertex,33.86664,8.78728,51.69300  #  32
*vertex,31.35752,8.18413,47.95000  #  33
*vertex,31.35752,8.18413,51.69300  #  34
*vertex,31.50336,8.21918,47.95000  #  35
*vertex,31.50336,8.21919,51.69299  #  36
# 
# tag, number of vertices followed by list of associated vert
*edges,4,2,1,4,3  #  1
*edges,4,5,2,3,6  #  2
*edges,4,7,5,6,8  #  3
*edges,4,9,7,8,10  #  4
*edges,4,11,9,10,12  #  5
*edges,4,13,11,12,14  #  6
*edges,4,15,13,14,16  #  7
*edges,4,17,15,16,18  #  8
*edges,4,19,17,18,20  #  9
*edges,4,21,19,20,22  # 10
*edges,4,23,21,22,24  # 11
*edges,4,25,23,24,26  # 12
*edges,4,27,25,26,28  # 13
*edges,4,29,27,28,30  # 14
*edges,4,31,29,30,32  # 15
*edges,4,1,33,34,4  # 16
*edges,4,33,35,36,34  # 17
*edges,4,35,31,32,36  # 18
*edges,18,4,34,36,32,30,28,26,24,22,20,18,16,14,12,10,8,6,3  # 19
*edges,18,1,2,5,7,9,11,13,15,17,19,21,23,25,27,29,31,35,33  # 20
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,seg_a,VERT,-,-,-,gyp_ins_gyp,OPAQUE,ANOTHER,06,06  #   1 ||< seg_a:L1_ent_recp
*surf,seg_b,VERT,-,-,-,gyp_ins_gyp,OPAQUE,ANOTHER,06,07  #   2 ||< seg_b:L1_ent_recp
*surf,seg_c,VERT,-,-,-,gyp_ins_gyp,OPAQUE,ANOTHER,06,08  #   3 ||< seg_c:L1_ent_recp
*surf,seg_d,VERT,-,-,-,gyp_ins_gyp,OPAQUE,ANOTHER,06,09  #   4 ||< seg_d:L1_ent_recp
*surf,seg_e,VERT,-,-,-,gyp_ins_gyp,OPAQUE,ANOTHER,06,10  #   5 ||< seg_e:L1_ent_recp
*surf,seg_f,VERT,-,-,-,gyp_ins_gyp,OPAQUE,ANOTHER,06,11  #   6 ||< seg_f:L1_ent_recp
*surf,seg_g,VERT,-,-,-,gyp_ins_gyp,OPAQUE,ANOTHER,06,50  #   7 ||< seg_g:L1_ent_recp
*surf,seg_h,VERT,-,-,-,gyp_ins_gyp,OPAQUE,ANOTHER,06,51  #   8 ||< seg_h:L1_ent_recp
*surf,seg_i,VERT,-,-,-,gyp_ins_gyp,OPAQUE,ANOTHER,08,30  #   9 ||< seg_i:L1_careers
*surf,seg_j,VERT,-,-,-,gyp_ins_gyp,OPAQUE,ANOTHER,08,31  #  10 ||< seg_j:L1_careers
*surf,seg_k,VERT,-,-,-,gyp_ins_gyp,OPAQUE,ANOTHER,08,32  #  11 ||< seg_k:L1_careers
*surf,seg_l,VERT,-,-,-,gyp_ins_gyp,OPAQUE,ANOTHER,08,29  #  12 ||< seg_l:L1_careers
*surf,seg_m,VERT,-,-,-,gyp_ins_gyp,OPAQUE,ANOTHER,08,28  #  13 ||< seg_m:L1_careers
*surf,seg_n,VERT,-,-,-,gyp_ins_gyp,OPAQUE,ANOTHER,08,27  #  14 ||< seg_n:L1_careers
*surf,seg_o,VERT,-,-,-,gyp_ins_gyp,OPAQUE,ANOTHER,08,26  #  15 ||< seg_o:L1_careers
*surf,facade_a,VERT,-,-,-,db_lpasol,DSF4554_06nb,EXTERIOR,0,0  #  16 ||< external
*surf,frame,VERT,-,-,-,mull_90,OPAQUE,EXTERIOR,0,0  #  17 ||< external
*surf,facade_b,VERT,-,-,-,db_lpasol,DSF4554_06nb,EXTERIOR,0,0  #  18 ||< external
*surf,Top,CEIL,-,-,-,slab_275,OPAQUE,ANOTHER,17,45  #  19 ||< Top:L2_uf_plen
*surf,Base,FLOR,-,-,-,foundation,OPAQUE,GROUND,01,00  #  20 ||< ground profile  1
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,0,42.00,0  # zone base
# 
# block entities:
#  *obs = obstructions
*block_start, 20 20 # geometric blocks
*obs,28.800,7.200,47.950,0.100,0.200,15.440,13.000,1.00,sol_vm,door  # block  13
*obs,31.200,7.800,47.950,0.100,0.200,15.440,13.000,1.00,sol_vn,door  # block  14
*obs,33.800,8.400,47.950,0.100,0.200,15.440,13.000,1.00,sol_vo,door  # block  15
*end_block
