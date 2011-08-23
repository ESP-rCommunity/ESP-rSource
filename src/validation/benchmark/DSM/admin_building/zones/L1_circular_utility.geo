*Geometry 1.1,GEN,L1_circular # tag version, format, zone name
*date Tue Aug 23 20:43:57 2011  # latest file modification 
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
*obs,28.800,7.200,47.950,0.100,0.200,16.680,13.000,1.00,sol_vm,mull_90  # block   1
*obs,31.200,7.800,47.950,0.100,0.200,16.680,13.000,1.00,sol_vn,mull_90  # block   2
*obs,33.800,8.400,47.950,0.100,0.200,16.680,13.000,1.00,sol_vo,mull_90  # block   3
*obs,28.999,6.900,50.860,7.250,0.450,0.050,14.721,1.00,hor_a,mull_90  # block   4
*obs,28.999,6.900,51.140,7.250,0.450,0.050,14.721,1.00,hor_b,mull_90  # block   5
*obs,28.999,6.900,51.420,7.250,0.450,0.050,14.721,1.00,hor_c,mull_90  # block   6
*obs,28.999,6.900,51.700,7.250,0.450,0.050,14.721,1.00,hor_d,mull_90  # block   7
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
*obs,28.999,6.900,57.020,7.250,0.450,0.050,14.721,1.00,hor_w,mull_90  # block  26
*obs,28.999,6.900,57.300,7.250,0.450,0.050,14.721,1.00,hor_x,mull_90  # block  27
*obs,28.999,6.900,57.580,7.250,0.450,0.050,14.721,1.00,hor_y,mull_90  # block  28
*obs,28.999,6.900,57.860,7.250,0.450,0.050,14.721,1.00,hor_z,mull_90  # block  29
*obs,28.999,6.900,58.140,7.250,0.450,0.050,14.721,1.00,hor_za,mull_90  # block  30
*obs,28.999,6.900,58.420,7.250,0.450,0.050,14.721,1.00,hor_zb,mull_90  # block  31
*obs,28.999,6.900,58.700,7.250,0.450,0.050,14.721,1.00,hor_zc,mull_90  # block  32
*obs,28.999,6.900,58.980,7.250,0.450,0.050,14.721,1.00,hor_zd,mull_90  # block  33
*obs,28.999,6.900,59.260,7.250,0.450,0.050,14.721,1.00,hor_ze,mull_90  # block  34
*obs,28.999,6.900,59.540,7.250,0.450,0.050,14.721,1.00,hor_zf,mull_90  # block  35
*obs,28.999,6.900,59.820,7.250,0.450,0.050,14.721,1.00,hor_zg,mull_90  # block  36
*obs,28.999,6.900,60.100,7.250,0.450,0.050,14.721,1.00,hor_zh,mull_90  # block  37
*obs,28.999,6.900,60.380,7.250,0.450,0.050,14.721,1.00,hor_zi,mull_90  # block  38
*obs,28.999,6.900,60.660,7.250,0.450,0.050,14.721,1.00,hor_zj,mull_90  # block  39
*obs,28.999,6.900,60.940,7.250,0.450,0.050,14.721,1.00,hor_zk,mull_90  # block  40
*obs,28.999,6.900,61.220,7.250,0.450,0.050,14.721,1.00,hor_zl,mull_90  # block  41
*obs,28.999,6.900,61.500,7.250,0.450,0.050,14.721,1.00,hor_zm,mull_90  # block  42
*obs,28.999,6.900,61.780,7.250,0.450,0.050,14.721,1.00,hor_zn,mull_90  # block  43
*obs,28.999,6.900,62.060,7.250,0.450,0.050,14.721,1.00,hor_zo,mull_90  # block  44
*obs,28.999,6.900,62.340,7.250,0.450,0.050,14.721,1.00,hor_zp,mull_90  # block  45
*obs,28.999,6.900,62.620,7.250,0.450,0.050,14.721,1.00,hor_zq,mull_90  # block  46
*obs,28.999,6.900,62.900,7.250,0.450,0.050,14.721,1.00,hor_zr,mull_90  # block  47
*obs,28.999,6.900,63.180,7.250,0.450,0.050,14.721,1.00,hor_zs,mull_90  # block  48
*obs,28.999,6.900,63.460,7.250,0.450,0.050,14.721,1.00,hor_zt,mull_90  # block  49
*obs,28.999,6.900,63.740,7.250,0.450,0.050,14.721,1.00,hor_zu,mull_90  # block  50
*obs,28.999,6.900,64.020,7.250,0.450,0.050,14.721,1.00,hor_zv,mull_90  # block  51
*obs,28.999,6.900,64.300,7.250,0.450,0.050,14.721,1.00,hor_zw,mull_90  # block  52
*obs,28.999,6.900,64.580,7.250,0.450,0.050,14.721,1.00,hor_zx,mull_90  # block  53
*end_block
