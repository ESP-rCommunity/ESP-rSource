*Geometry 1.1,GEN,L3_void_b # tag version, format, zone name
*date Tue Jan 31 08:47:16 2012  # latest file modification 
L3_void_b describes void above the careers services area
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,45.64634,11.68470,56.35000  #   1
*vertex,45.97883,11.73220,56.35000  #   2
*vertex,48.44877,12.30219,56.35000  #   3
*vertex,51.21083,12.90755,56.35000  #   4
*vertex,51.60000,13.00000,56.35000  #   5
*vertex,51.58369,13.77465,56.35000  #   6
*vertex,51.58369,16.29209,56.35000  #   7
*vertex,51.63119,17.00457,56.35000  #   8
*vertex,51.60000,17.28956,56.35000  #   9
*vertex,44.69636,15.62710,56.35000  #  10
*vertex,45.64634,11.68470,59.49300  #  11
*vertex,45.97883,11.73220,60.25000  #  12
*vertex,48.44877,12.30219,60.25000  #  13
*vertex,51.21083,12.90755,60.25000  #  14
*vertex,51.60000,13.00000,60.25000  #  15
*vertex,51.58369,13.77465,60.25000  #  16
*vertex,51.58369,16.29209,60.25000  #  17
*vertex,51.63119,17.00457,60.25000  #  18
*vertex,51.58369,17.28956,59.49300  #  19
*vertex,44.69636,15.62710,59.49300  #  20
*vertex,44.69636,15.62710,60.25000  #  21
*vertex,45.64634,11.68470,60.25000  #  22
*vertex,51.63119,17.00457,63.39300  #  23
*vertex,51.58369,16.29209,63.39300  #  24
*vertex,51.58369,13.77465,63.39300  #  25
*vertex,51.60000,13.00000,63.39300  #  26
*vertex,51.21083,12.90755,63.39300  #  27
*vertex,48.44877,12.30219,63.39300  #  28
*vertex,45.97883,11.73220,63.39300  #  29
*vertex,45.64634,11.68470,63.39300  #  30
*vertex,44.69636,15.62710,63.39300  #  31
*vertex,45.64634,11.68470,61.25000  #  32
*vertex,44.69636,15.62710,61.25000  #  33
*vertex,51.60000,17.25000,61.25000  #  34
*vertex,51.60000,17.25000,63.39300  #  35
*vertex,51.60000,17.25000,60.25000  #  36
# 
# tag, number of vertices followed by list of associated vert
*edges,5,1,2,12,22,11  #  1
*edges,4,2,3,13,12  #  2
*edges,4,3,4,14,13  #  3
*edges,4,4,5,15,14  #  4
*edges,4,5,6,16,15  #  5
*edges,4,6,7,17,16  #  6
*edges,4,7,8,18,17  #  7
*edges,5,8,9,19,36,18  #  8
*edges,4,9,10,20,19  #  9
*edges,4,10,1,11,20  # 10
*edges,10,1,10,9,8,7,6,5,4,3,2  # 11
*edges,4,19,20,21,36  # 12
*edges,4,20,11,22,21  # 13
*edges,4,15,16,25,26  # 14
*edges,4,16,17,24,25  # 15
*edges,4,17,18,23,24  # 16
*edges,5,18,36,34,35,23  # 17
*edges,5,22,12,29,30,32  # 18
*edges,4,12,13,28,29  # 19
*edges,4,13,14,27,28  # 20
*edges,4,14,15,26,27  # 21
*edges,10,27,26,25,24,23,35,31,30,29,28  # 22
*edges,4,33,32,30,31  # 23
*edges,4,34,33,31,35  # 24
*edges,4,36,21,33,34  # 25
*edges,4,21,22,32,33  # 26
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,stone,VERT,-,WALL,-,stone_m_ell,OPAQUE,EXTERIOR,0,0  #   1 ||< external
*surf,fac_gla,VERT,-,D-WINDOW,CLOSED,db_lpasol,DSF4554_06nb,EXTERIOR,0,0  #   2 ||< external
*surf,fac_glb,VERT,-,D-WINDOW,CLOSED,db_lpasol,DSF4554_06nb,EXTERIOR,0,0  #   3 ||< external
*surf,fac_frame,VERT,-,WALL,-,mull_90,OPAQUE,EXTERIOR,0,0  #   4 ||< external
*surf,side_gl_a,VERT,-,D-WINDOW,CLOSED,db_lpasol,DSF4554_06nb,EXTERIOR,0,0  #   5 ||< external
*surf,side_gl_b,VERT,-,D-WINDOW,CLOSED,db_lpasol,DSF4554_06nb,EXTERIOR,0,0  #   6 ||< external
*surf,side_gl_c,VERT,-,D-WINDOW,CLOSED,db_lpasol,DSF4554_06nb,EXTERIOR,0,0  #   7 ||< external
*surf,side_fr,VERT,-,-,-,mull_90,OPAQUE,ANOTHER,39,14  #   8 ||< ptn_frame:L3_meeting
*surf,int_glaz,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,49,06  #   9 ||< int_glaz:L3_open_rig
*surf,int_glazb,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,49,05  #  10 ||< int_glazb:L3_open_rig
*surf,fict_low,FLOR,-,-,-,fictitious,SC_fictit,ANOTHER,20,18  #  11 ||< fict_low:L2_careers
*surf,struc_b,SLOP,-,-,-,slab_275,OPAQUE,ANOTHER,78,15  #  12 ||< struc_bvb:L4_flrvoid
*surf,struc_a,VERT,-,-,-,slab_275,OPAQUE,ANOTHER,78,16  #  13 ||< struc_avb:L4_flrvoid
*surf,side_gl_au,VERT,-,D-WINDOW,CLOSED,db_lpasol,DSF4554_06nb,EXTERIOR,0,0  #  14 ||< external
*surf,side_gl_bu,VERT,-,D-WINDOW,CLOSED,db_lpasol,DSF4554_06nb,EXTERIOR,0,0  #  15 ||< external
*surf,side_gl_cu,VERT,-,D-WINDOW,CLOSED,db_lpasol,DSF4554_06nb,EXTERIOR,0,0  #  16 ||< external
*surf,side_fru,VERT,-,-,-,mull_90,OPAQUE,ANOTHER,53,14  #  17 ||< ptn_frame:L4_meeting
*surf,stone_u,VERT,-,WALL,-,stone_m_ell,OPAQUE,EXTERIOR,0,0  #  18 ||< external
*surf,fac_gla_u,VERT,-,D-WINDOW,CLOSED,db_lpasol,DSF4554_06nb,EXTERIOR,0,0  #  19 ||< external
*surf,fac_glb_u,VERT,-,D-WINDOW,CLOSED,db_lpasol,DSF4554_06nb,EXTERIOR,0,0  #  20 ||< external
*surf,fac_fra_u,VERT,-,WALL,-,mull_90,OPAQUE,EXTERIOR,0,0  #  21 ||< external
*surf,roof,CEIL,-,ROOF,FLAT,roof_0.2,OPAQUE,EXTERIOR,0,0  #  22 ||< external
*surf,ptn_vb_a,VERT,-,-,-,stone_atr_pt,OPAQUE,ANOTHER,57,04  #  23 ||< ptn_vb_a:L4_open_plr
*surf,ptn_vb_b,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,57,05  #  24 ||< ptn_vb_b:L4_open_plr
*surf,balc_vb_b,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,57,34  #  25 ||< balc_vb_b:L4_open_plr
*surf,balc_vb_a,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,57,35  #  26 ||< balc_vb_a:L4_open_plr
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,11,    27.18 0  # zone base list
# 
# block entities:
#  *obs = obstructions
*block_start, 20 20 # geometric blocks
*obs,50.500,13.500,47.950,0.400,0.400,15.440,0.000,1.00,col_at_g,door  # block   1
*obs,44.500,15.750,47.950,0.250,0.250,15.440,0.000,1.00,col_y,door  # block   2
*obs,38.000,14.250,47.950,0.250,0.250,15.440,0.000,1.00,col_z,door  # block   3
*obs,45.900,11.100,47.950,0.100,0.200,16.680,13.000,1.00,sol_vp,mull_90  # block   4
*obs,45.700,10.882,50.860,7.250,0.450,0.050,14.721,1.00,hor_a,mull_90  # block   5
*obs,45.700,10.882,51.140,7.250,0.450,0.050,14.721,1.00,hor_b,mull_90  # block   6
*obs,45.700,10.882,51.420,7.250,0.450,0.050,14.721,1.00,hor_c,mull_90  # block   7
*obs,45.700,10.882,51.700,7.250,0.450,0.050,14.721,1.00,hor_d,mull_90  # block   8
*obs,45.700,10.882,51.980,7.250,0.450,0.050,14.721,1.00,hor_e,mull_90  # block   9
*obs,45.700,10.882,52.260,7.250,0.450,0.050,14.721,1.00,hor_f,mull_90  # block  10
*obs,45.700,10.882,52.540,7.250,0.450,0.050,14.721,1.00,hor_g,mull_90  # block  11
*obs,45.700,10.882,52.820,7.250,0.450,0.050,14.721,1.00,hor_h,mull_90  # block  12
*obs,45.700,10.882,53.100,7.250,0.450,0.050,14.721,1.00,hor_i,mull_90  # block  13
*obs,45.700,10.882,53.380,7.250,0.450,0.050,14.721,1.00,hor_j,mull_90  # block  14
*obs,45.700,10.882,53.660,7.250,0.450,0.050,14.721,1.00,hor_k,mull_90  # block  15
*obs,45.700,10.882,53.940,7.250,0.450,0.050,14.721,1.00,hor_l,mull_90  # block  16
*obs,45.700,10.882,54.220,7.250,0.450,0.050,14.721,1.00,hor_m,mull_90  # block  17
*obs,45.700,10.882,54.500,7.250,0.450,0.050,14.721,1.00,hor_n,mull_90  # block  18
*obs,45.700,10.882,54.780,7.250,0.450,0.050,14.721,1.00,hor_o,mull_90  # block  19
*obs,45.700,10.882,55.060,7.250,0.450,0.050,14.721,1.00,hor_p,mull_90  # block  20
*obs,45.700,10.882,55.340,7.250,0.450,0.050,14.721,1.00,hor_q,mull_90  # block  21
*obs,45.700,10.882,55.620,7.250,0.450,0.050,14.721,1.00,hor_r,mull_90  # block  22
*obs,45.700,10.882,55.900,7.250,0.450,0.050,14.721,1.00,hor_s,mull_90  # block  23
*obs,45.700,10.882,56.180,7.250,0.450,0.050,14.721,1.00,hor_t,mull_90  # block  24
*obs,45.700,10.882,56.460,7.250,0.450,0.050,14.721,1.00,hor_u,mull_90  # block  25
*obs,45.700,10.882,56.740,7.250,0.450,0.050,14.721,1.00,hor_v,mull_90  # block  26
*obs,45.700,10.882,57.020,7.250,0.450,0.050,14.721,1.00,hor_w,mull_90  # block  27
*obs,45.700,10.882,57.300,7.250,0.450,0.050,14.721,1.00,hor_x,mull_90  # block  28
*obs,45.700,10.882,57.580,7.250,0.450,0.050,14.721,1.00,hor_y,mull_90  # block  29
*obs,45.700,10.882,57.860,7.250,0.450,0.050,14.721,1.00,hor_z,mull_90  # block  30
*obs,45.700,10.882,58.140,7.250,0.450,0.050,14.721,1.00,hor_za,mull_90  # block  31
*obs,45.700,10.882,58.420,7.250,0.450,0.050,14.721,1.00,hor_zb,mull_90  # block  32
*obs,45.700,10.882,58.700,7.250,0.450,0.050,14.721,1.00,hor_zc,mull_90  # block  33
*obs,45.700,10.882,58.980,7.250,0.450,0.050,14.721,1.00,hor_zd,mull_90  # block  34
*obs,45.700,10.882,59.260,7.250,0.450,0.050,14.721,1.00,hor_ze,mull_90  # block  35
*obs,45.700,10.882,59.540,7.250,0.450,0.050,14.721,1.00,hor_zf,mull_90  # block  36
*obs,45.700,10.882,59.820,7.250,0.450,0.050,14.721,1.00,hor_zg,mull_90  # block  37
*obs,45.700,10.882,60.100,7.250,0.450,0.050,14.721,1.00,hor_zh,mull_90  # block  38
*obs,45.700,10.882,60.380,7.250,0.450,0.050,14.721,1.00,hor_zi,mull_90  # block  39
*obs,45.700,10.882,60.660,7.250,0.450,0.050,14.721,1.00,hor_zj,mull_90  # block  40
*obs,45.700,10.882,60.940,7.250,0.450,0.050,14.721,1.00,hor_zk,mull_90  # block  41
*obs,45.700,10.882,61.220,7.250,0.450,0.050,14.721,1.00,hor_zl,mull_90  # block  42
*obs,45.700,10.882,61.500,7.250,0.450,0.050,14.721,1.00,hor_zm,mull_90  # block  43
*obs,45.700,10.882,61.780,7.250,0.450,0.050,14.721,1.00,hor_zn,mull_90  # block  44
*obs,45.700,10.882,62.060,7.250,0.450,0.050,14.721,1.00,hor_zo,mull_90  # block  45
*obs,45.700,10.882,62.340,7.250,0.450,0.050,14.721,1.00,hor_zp,mull_90  # block  46
*obs,45.700,10.882,62.620,7.250,0.450,0.050,14.721,1.00,hor_zq,mull_90  # block  47
*obs,45.700,10.882,62.900,7.250,0.450,0.050,14.721,1.00,hor_zr,mull_90  # block  48
*obs,45.700,10.882,63.180,7.250,0.450,0.050,14.721,1.00,hor_zs,mull_90  # block  49
*obs,45.700,10.882,63.460,7.250,0.450,0.050,14.721,1.00,hor_zt,mull_90  # block  50
*obs,45.700,10.882,63.740,7.250,0.450,0.050,14.721,1.00,hor_zu,mull_90  # block  51
*obs,45.700,10.882,64.020,7.250,0.450,0.050,14.721,1.00,hor_zv,mull_90  # block  52
*obs,45.700,10.882,64.300,7.250,0.450,0.050,14.721,1.00,hor_zw,mull_90  # block  53
*obs,45.700,10.882,64.580,7.250,0.450,0.050,14.721,1.00,hor_zx,mull_90  # block  54
*obs,48.400,11.800,47.950,0.100,0.200,16.680,13.000,1.00,sol_vq,mull_90  # block  55
*end_block
