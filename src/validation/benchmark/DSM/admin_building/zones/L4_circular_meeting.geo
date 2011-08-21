*Geometry 1.1,GEN,L4_circular # tag version, format, zone name
*date Sun Aug 21 16:03:44 2011  # latest file modification 
L4_circular describes circular meeting room at level four
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,28.92676,7.59981,60.25000  #   1
*vertex,27.88179,8.35979,60.25000  #   2
*vertex,27.88179,8.35979,64.15000  #   3
*vertex,28.92676,7.59981,64.15000  #   4
*vertex,27.21681,9.30976,60.25000  #   5
*vertex,27.21681,9.30976,64.15000  #   6
*vertex,26.93181,10.54473,60.25000  #   7
*vertex,26.93181,10.54473,64.15000  #   8
*vertex,26.97931,11.68470,60.25000  #   9
*vertex,26.97931,11.68470,64.15000  #  10
*vertex,27.40680,12.77717,60.25000  #  11
*vertex,27.40680,12.77717,64.15000  #  12
*vertex,28.07178,13.63215,60.25000  #  13
*vertex,28.07178,13.63215,64.15000  #  14
*vertex,29.16426,14.39213,60.25000  #  15
*vertex,29.16426,14.39213,64.15000  #  16
*vertex,30.39923,14.67713,60.25000  #  17
*vertex,30.39923,14.67713,64.15000  #  18
*vertex,31.53920,14.62963,60.25000  #  19
*vertex,31.53920,14.62963,64.15000  #  20
*vertex,32.96416,14.01214,60.25000  #  21
*vertex,32.96416,14.01214,64.15000  #  22
*vertex,34.25000,13.06217,60.25000  #  23
*vertex,34.25000,13.06217,64.15000  #  24
*vertex,34.50000,12.11219,60.25000  #  25
*vertex,34.50000,12.11219,64.15000  #  26
*vertex,34.60000,10.97222,60.25000  #  27
*vertex,34.60000,10.97222,64.15000  #  28
*vertex,34.38913,9.92725,60.25000  #  29
*vertex,34.38913,9.92725,64.15000  #  30
*vertex,33.86664,8.78728,60.25000  #  31
*vertex,33.86664,8.78728,64.15000  #  32
*vertex,31.35752,8.18413,60.25000  #  33
*vertex,31.35752,8.18413,64.15000  #  34
*vertex,31.50336,8.21918,60.25000  #  35
*vertex,31.50336,8.21919,64.14999  #  36
*vertex,30.39923,14.67713,63.39300  #  37
*vertex,29.16426,14.39213,63.39300  #  38
*vertex,31.53920,14.62963,63.39300  #  39
*vertex,32.96416,14.01214,63.39300  #  40
*vertex,34.25000,13.06200,61.25000  #  41
*vertex,34.25000,13.06200,63.39300  #  42
*vertex,28.07178,13.63215,63.39300  #  43
# 
# tag, number of vertices followed by list of associated vert
*edges,4,2,1,4,3  #  1
*edges,4,5,2,3,6  #  2
*edges,4,7,5,6,8  #  3
*edges,4,9,7,8,10  #  4
*edges,4,11,9,10,12  #  5
*edges,5,13,11,12,14,43  #  6
*edges,4,38,43,14,16  #  7
*edges,4,37,38,16,18  #  8
*edges,4,39,37,18,20  #  9
*edges,4,40,39,20,22  # 10
*edges,4,42,40,22,24  # 11
*edges,6,25,23,41,42,24,26  # 12
*edges,4,27,25,26,28  # 13
*edges,4,29,27,28,30  # 14
*edges,4,31,29,30,32  # 15
*edges,4,1,33,34,4  # 16
*edges,4,33,35,36,34  # 17
*edges,4,35,31,32,36  # 18
*edges,18,4,34,36,32,30,28,26,24,22,20,18,16,14,12,10,8,6,3  # 19
*edges,18,1,2,5,7,9,11,13,15,17,19,21,23,25,27,29,31,35,33  # 20
*edges,4,17,15,38,37  # 21
*edges,4,19,17,37,39  # 22
*edges,4,21,19,39,40  # 23
*edges,5,23,21,40,42,41  # 24
*edges,4,15,13,43,38  # 25
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,seg_a,VERT,-,-,-,gyp_ins_gyp,OPAQUE,ANOTHER,77,21  #   1 ||< seg_a:L5_atroofltl
*surf,seg_b,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,77,22  #   2 ||< seg_b:L5_atroofltl
*surf,seg_c,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,77,23  #   3 ||< seg_c:L5_atroofltl
*surf,seg_d,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,77,24  #   4 ||< seg_d:L5_atroofltl
*surf,seg_e,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,77,25  #   5 ||< seg_e:L5_atroofltl
*surf,seg_f,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,77,26  #   6 ||< seg_f:L5_atroofltl
*surf,seg_g,VERT,-,-,-,gyp_ins_gyp,OPAQUE,UNKNOWN,0,0  #   7 ||< not yet defined
*surf,seg_h,VERT,-,-,-,gyp_ins_gyp,OPAQUE,UNKNOWN,0,0  #   8 ||< not yet defined
*surf,seg_i,VERT,-,-,-,gyp_ins_gyp,OPAQUE,UNKNOWN,0,0  #   9 ||< not yet defined
*surf,seg_j,VERT,-,-,-,gyp_ins_gyp,OPAQUE,UNKNOWN,0,0  #  10 ||< not yet defined
*surf,seg_k,VERT,-,-,-,gyp_ins_gyp,OPAQUE,UNKNOWN,0,0  #  11 ||< not yet defined
*surf,seg_l,VERT,-,-,-,glaz_ptn,SCF8477_10nb,UNKNOWN,0,0  #  12 ||< not yet defined
*surf,seg_m,VERT,-,-,-,glaz_ptn,SCF8477_10nb,UNKNOWN,0,0  #  13 ||< not yet defined
*surf,seg_n,VERT,-,-,-,glaz_ptn,SCF8477_10nb,UNKNOWN,0,0  #  14 ||< not yet defined
*surf,seg_o,VERT,-,-,-,gyp_ins_gyp,OPAQUE,UNKNOWN,0,0  #  15 ||< not yet defined
*surf,facade_a,VERT,-,-,-,db_lpasol,DSF4554_06nb,EXTERIOR,0,0  #  16 ||< external
*surf,frame,VERT,-,-,-,mull_90,OPAQUE,EXTERIOR,0,0  #  17 ||< external
*surf,facade_b,VERT,-,-,-,db_lpasol,DSF4554_06nb,EXTERIOR,0,0  #  18 ||< external
*surf,Top,CEIL,-,-,-,roof_0.2,OPAQUE,UNKNOWN,0,0  #  19 ||< not yet defined
*surf,Base,FLOR,-,-,-,slab_275,OPAQUE,UNKNOWN,0,0  #  20 ||< not yet defined
*surf,seg_hl,VERT,-,-,-,gyp_ins_gyp,OPAQUE,ANOTHER,57,10  #  21 ||< seg_h:L4_open_plr
*surf,seg_il,VERT,-,-,-,gyp_ins_gyp,OPAQUE,ANOTHER,57,11  #  22 ||< seg_i:L4_open_plr
*surf,seg_jl,VERT,-,-,-,gyp_ins_gyp,OPAQUE,ANOTHER,57,12  #  23 ||< seg_j:L4_open_plr
*surf,seg_kl,VERT,-,-,-,gyp_ins_gyp,OPAQUE,ANOTHER,57,13  #  24 ||< seg_k:L4_open_plr
*surf,seg_gl,VERT,-,-,-,gyp_ins_gyp,OPAQUE,ANOTHER,57,14  #  25 ||< seg_g:L4_open_plr
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
*obs,28.800,7.200,47.950,0.100,0.200,15.440,13.000,1.00,sol_vm,door  # block   1
*obs,31.200,7.800,47.950,0.100,0.200,15.440,13.000,1.00,sol_vn,door  # block   2
*obs,33.800,8.400,47.950,0.100,0.200,15.440,13.000,1.00,sol_vo,door  # block   3
*end_block
