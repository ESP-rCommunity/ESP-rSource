*Geometry 1.1,GEN,L2_circular # tag version, format, zone name
*date Wed Aug 24 05:54:13 2011  # latest file modification 
L2_circular describes circular meeting room on level two
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,28.92676,7.59981,52.45000  #   1
*vertex,27.88179,8.35979,52.45000  #   2
*vertex,27.88179,8.35979,56.35000  #   3
*vertex,28.92676,7.59981,56.35000  #   4
*vertex,27.21681,9.30976,52.45000  #   5
*vertex,27.21681,9.30976,56.35000  #   6
*vertex,26.93181,10.54473,52.45000  #   7
*vertex,26.93181,10.54473,56.35000  #   8
*vertex,26.97931,11.68470,52.45000  #   9
*vertex,26.97931,11.68470,56.35000  #  10
*vertex,27.40680,12.77717,52.45000  #  11
*vertex,27.40680,12.77717,56.35000  #  12
*vertex,28.07178,13.63215,52.45000  #  13
*vertex,28.07178,13.63215,56.35000  #  14
*vertex,29.16426,14.39213,52.45000  #  15
*vertex,30.39923,14.67713,52.45000  #  16
*vertex,31.53920,14.62963,52.45000  #  17
*vertex,32.96416,14.01214,52.45000  #  18
*vertex,34.25000,13.06217,52.45000  #  19
*vertex,34.25000,13.06217,56.35000  #  20
*vertex,34.50000,12.11219,52.45000  #  21
*vertex,34.50000,12.11219,56.35000  #  22
*vertex,34.60000,10.97222,52.45000  #  23
*vertex,34.60000,10.97222,56.35000  #  24
*vertex,34.38913,9.92725,52.45000  #  25
*vertex,34.38913,9.92725,56.35000  #  26
*vertex,33.86664,8.78728,52.45000  #  27
*vertex,33.86664,8.78728,56.35000  #  28
*vertex,31.35752,8.18413,52.45000  #  29
*vertex,31.35752,8.18413,56.35000  #  30
*vertex,31.50336,8.21918,52.45000  #  31
*vertex,31.50336,8.21919,56.34999  #  32
*vertex,29.16426,14.39213,55.59300  #  33
*vertex,28.07178,13.63215,55.59300  #  34
*vertex,30.39923,14.67713,55.59300  #  35
*vertex,31.53920,14.62963,55.59300  #  36
*vertex,32.96416,14.01214,55.59300  #  37
*vertex,34.25000,13.06217,53.45000  #  38
*vertex,34.25000,13.06217,55.59300  #  39
# 
# tag, number of vertices followed by list of associated vert
*edges,4,2,1,4,3  #  1
*edges,4,5,2,3,6  #  2
*edges,4,7,5,6,8  #  3
*edges,4,9,7,8,10  #  4
*edges,4,11,9,10,12  #  5
*edges,5,13,11,12,14,34  #  6
*edges,6,33,34,39,37,36,35  #  7
*edges,4,39,34,14,20  #  8
*edges,6,21,19,38,39,20,22  #  9
*edges,4,23,21,22,24  # 10
*edges,4,25,23,24,26  # 11
*edges,4,27,25,26,28  # 12
*edges,4,1,29,30,4  # 13
*edges,4,29,31,32,30  # 14
*edges,4,31,27,28,32  # 15
*edges,14,4,30,32,28,26,24,22,20,14,12,10,8,6,3  # 16
*edges,18,1,2,5,7,9,11,13,15,16,17,18,19,21,23,25,27,31,29  # 17
*edges,4,15,13,34,33  # 18
*edges,4,16,15,33,35  # 19
*edges,4,17,16,35,36  # 20
*edges,4,18,17,36,37  # 21
*edges,5,19,18,37,39,38  # 22
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,seg_a,VERT,-,-,-,gyp_ins_gyp,OPAQUE,ANOTHER,18,13  #   1 ||< seg_a:L2_ent_void
*surf,seg_b,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,18,14  #   2 ||< seg_b:L2_ent_void
*surf,seg_c,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,18,15  #   3 ||< seg_c:L2_ent_void
*surf,seg_d,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,18,16  #   4 ||< seg_d:L2_ent_void
*surf,seg_e,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,18,17  #   5 ||< seg_e:L2_ent_void
*surf,seg_f,VERT,-,-,-,gyp_ins_gyp,OPAQUE,ANOTHER,18,18  #   6 ||< seg_f:L2_ent_void
*surf,segment_tL2,CEIL,-,-,-,slab_275,OPAQUE,ANOTHER,50,56  #   7 ||< seg_g:L3_ufloor
*surf,seg_k,VERT,-,-,-,slab_275,OPAQUE,ANOTHER,50,57  #   8 ||< seg_kk:L3_ufloor
*surf,seg_l,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,20,13  #   9 ||< seg_a:L2_careers
*surf,seg_m,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,20,14  #  10 ||< seg_b:L2_careers
*surf,seg_n,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,20,15  #  11 ||< seg_c:L2_careers
*surf,seg_o,VERT,-,-,-,gyp_ins_gyp,OPAQUE,ANOTHER,20,16  #  12 ||< seg_d:L2_careers
*surf,facade_a,VERT,-,-,-,db_lpasol,DSF4554_06nb,EXTERIOR,0,0  #  13 ||< external
*surf,frame,VERT,-,-,-,mull_90,OPAQUE,EXTERIOR,0,0  #  14 ||< external
*surf,facade_b,VERT,-,-,-,db_lpasol,DSF4554_06nb,EXTERIOR,0,0  #  15 ||< external
*surf,ceil_l2_cir,CEIL,-,-,-,slab_275,OPAQUE,ANOTHER,36,20  #  16 ||< L3_cir_flor:L3_circular
*surf,L2_cir_floor,FLOR,-,-,-,flr_ov_pln,OPAQUE,ANOTHER,17,46  #  17 ||< Base:L2_uf_plen
*surf,seg_al,VERT,-,-,-,gyp_ins_gyp,OPAQUE,ANOTHER,22,03  #  18 ||< seg_a:L2_open_pln
*surf,seg_bl,VERT,-,-,-,gyp_ins_gyp,OPAQUE,ANOTHER,22,04  #  19 ||< seg_b:L2_open_pln
*surf,seg_cl,VERT,-,-,-,gyp_ins_gyp,OPAQUE,ANOTHER,22,05  #  20 ||< seg_c:L2_open_pln
*surf,seg_dl,VERT,-,-,-,gyp_ins_gyp,OPAQUE,ANOTHER,22,06  #  21 ||< seg_d:L2_open_pln
*surf,seg_el,VERT,-,-,-,gyp_ins_gyp,OPAQUE,ANOTHER,22,07  #  22 ||< seg_e:L2_open_pln
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
*obs,28.800,7.200,47.950,0.100,0.200,15.440,13.000,1.00,sol_vm,mull_90  # block   1
*obs,31.200,7.800,47.950,0.100,0.200,15.440,13.000,1.00,sol_vn,mull_90  # block   2
*obs,33.800,8.400,47.950,0.100,0.200,15.440,13.000,1.00,sol_vo,mull_90  # block   3
*obs,28.999,6.900,51.980,7.250,0.450,0.050,14.721,1.00,hor_e,mull_90  # block   8
*obs,28.999,6.900,52.260,7.250,0.450,0.050,14.721,1.00,hor_f,mull_90  # block   9
*obs,28.999,6.900,52.540,7.250,0.450,0.050,14.721,1.00,hor_g,mull_90  # block  10
*obs,28.999,6.900,52.820,7.250,0.450,0.050,14.721,1.00,hor_h,mull_90  # block  11
*obs,28.999,6.900,53.100,7.250,0.450,0.050,14.721,1.00,hor_i,mull_90  # block  12
*obs,28.999,6.900,53.380,7.250,0.450,0.050,14.721,1.00,hor_j,mull_90  # block  13
*obs,28.999,6.900,53.660,7.250,0.450,0.050,14.721,1.00,hor_k,mull_90  # block  14
*obs,28.999,6.900,53.940,7.250,0.450,0.050,14.721,1.00,hor_l,mull_90  # block  15
*obs,28.999,6.900,54.220,7.250,0.450,0.050,14.721,1.00,hor_m,mull_90  # block  16
*obs,28.999,6.900,54.500,7.250,0.450,0.050,14.721,1.00,hor_n,mull_90  # block  17
*obs,28.999,6.900,54.780,7.250,0.450,0.050,14.721,1.00,hor_o,mull_90  # block  18
*obs,28.999,6.900,55.060,7.250,0.450,0.050,14.721,1.00,hor_p,mull_90  # block  19
*obs,28.999,6.900,55.340,7.250,0.450,0.050,14.721,1.00,hor_q,mull_90  # block  20
*obs,28.999,6.900,55.620,7.250,0.450,0.050,14.721,1.00,hor_r,mull_90  # block  21
*obs,28.999,6.900,55.900,7.250,0.450,0.050,14.721,1.00,hor_s,mull_90  # block  22
*obs,28.999,6.900,56.180,7.250,0.450,0.050,14.721,1.00,hor_t,mull_90  # block  23
*obs,28.999,6.900,56.460,7.250,0.450,0.050,14.721,1.00,hor_u,mull_90  # block  24
*obs,28.999,6.900,56.740,7.250,0.450,0.050,14.721,1.00,hor_v,mull_90  # block  25
*end_block
