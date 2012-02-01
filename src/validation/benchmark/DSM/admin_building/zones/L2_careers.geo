*Geometry 1.1,GEN,L2_careers # tag version, format, zone name
*date Mon Jan 30 17:44:11 2012  # latest file modification 
L2_careers describes open space over the careers service area
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,34.25000,13.06217,52.45000  #   1
*vertex,34.50000,12.11219,52.45000  #   2
*vertex,34.60000,10.97222,52.45000  #   3
*vertex,34.38913,9.92725,52.45000  #   4
*vertex,33.86664,8.78728,52.45000  #   5
*vertex,51.21083,12.90755,52.45000  #   6
*vertex,51.60000,13.00000,52.45000  #   7
*vertex,51.60000,17.28956,52.45000  #   8
*vertex,34.50000,12.11219,56.35000  #   9
*vertex,34.25000,13.06217,56.35000  #  10
*vertex,51.60000,17.28956,56.35000  #  11
*vertex,51.60000,13.00000,56.35000  #  12
*vertex,33.86664,8.78728,56.35000  #  13
*vertex,34.38913,9.92725,56.35000  #  14
*vertex,34.60000,10.97222,56.35000  #  15
*vertex,34.25000,13.06217,55.59300  #  16
*vertex,51.60000,17.28956,55.59300  #  17
*vertex,34.25000,13.06217,53.45000  #  18
*vertex,51.60000,17.28956,53.45000  #  19
*vertex,35.91461,13.46776,56.35000  #  20
*vertex,36.76406,9.49976,56.35000  #  21
*vertex,36.24158,9.35726,56.35000  #  22
*vertex,45.64634,11.68470,56.35000  #  23
*vertex,44.69636,15.62710,56.35000  #  24
*vertex,51.63119,17.00457,56.35000  #  25
*vertex,51.58369,16.29209,56.35000  #  26
*vertex,51.58369,13.77465,56.35000  #  27
*vertex,48.44877,12.30219,56.35000  #  28
*vertex,45.97883,11.73220,56.35000  #  29
*vertex,34.25559,8.88063,56.34999  #  30
*vertex,34.25559,8.88063,52.45000  #  31
*vertex,36.24158,9.35726,52.45000  #  32
*vertex,48.44877,12.30219,52.45000  #  33
*vertex,45.97883,11.73220,52.45000  #  34
*vertex,51.21083,12.90755,56.35000  #  35
*vertex,51.63119,17.00457,52.45000  #  36
# 
# tag, number of vertices followed by list of associated vert
*edges,13,2,1,8,36,7,6,33,34,32,31,5,4,3  #  1
*edges,4,21,23,24,20  #  2
*edges,4,5,31,30,13  #  3
*edges,4,31,32,22,30  #  4
*edges,6,32,34,29,23,21,22  #  5
*edges,4,34,33,28,29  #  6
*edges,4,33,6,35,28  #  7
*edges,4,6,7,12,35  #  8
*edges,6,7,36,25,26,27,12  #  9
*edges,4,8,1,18,19  # 10
*edges,6,17,16,10,20,24,11  # 11
*edges,4,19,18,16,17  # 12
*edges,6,1,2,9,10,16,18  # 13
*edges,4,2,3,15,9  # 14
*edges,4,3,4,14,15  # 15
*edges,4,4,5,13,14  # 16
*edges,9,20,10,9,15,14,13,30,22,21  # 17
*edges,10,24,23,29,28,35,12,27,26,25,11  # 18
*edges,6,36,8,19,17,11,25  # 19
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,lower_fict,FLOR,-,-,-,fictitious,SC_fictit,ANOTHER,08,40  #   1 ||< upper_fict:L1_careers
*surf,upper_slab,CEIL,-,-,-,slab_275,OPAQUE,ANOTHER,49,29  #   2 ||< ovhang:L3_open_rig
*surf,fac_frame_a,VERT,-,WALL,-,mull_90,OPAQUE,EXTERIOR,0,0  #   3 ||< external
*surf,fac_glaz_a,VERT,-,D-WINDOW,CLOSED,db_lpasol,DSF4554_06nb,EXTERIOR,0,0  #   4 ||< external
*surf,stone_wall,VERT,-,WALL,-,stone_m_ell,OPAQUE,EXTERIOR,0,0  #   5 ||< external
*surf,fac_glaz_b,VERT,-,D-WINDOW,CLOSED,db_lpasol,DSF4554_06nb,EXTERIOR,0,0  #   6 ||< external
*surf,fac_glaz_c,VERT,-,D-WINDOW,CLOSED,db_lpasol,DSF4554_06nb,EXTERIOR,0,0  #   7 ||< external
*surf,fac_frame_b,VERT,-,WALL,-,mull_90,OPAQUE,EXTERIOR,0,0  #   8 ||< external
*surf,end_glazing,VERT,-,D-WINDOW,CLOSED,db_lpasol,DSF4554_06nb,EXTERIOR,0,0  #   9 ||< external
*surf,balcony,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,22,46  #  10 ||< balcony:L2_open_pln
*surf,struc,VERT,-,-,-,slab_275,OPAQUE,ANOTHER,50,15  #  11 ||< struc:L3_ufloor
*surf,fict_opening,VERT,-,-,-,fictitious,SC_fictit,ANOTHER,22,41  #  12 ||< ptn_void_c:L2_open_pln
*surf,seg_a,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,19,09  #  13 ||< seg_l:L2_circular
*surf,seg_b,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,19,10  #  14 ||< seg_m:L2_circular
*surf,seg_c,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,19,11  #  15 ||< seg_n:L2_circular
*surf,seg_d,VERT,-,-,-,gyp_ins_gyp,OPAQUE,ANOTHER,19,12  #  16 ||< seg_o:L2_circular
*surf,L3_vc_low,CEIL,-,-,-,fictitious,SC_fictit,ANOTHER,37,09  #  17 ||< L3_vc_low:L3_L4_cir_vd
*surf,fict_low,CEIL,-,-,-,fictitious,SC_fictit,ANOTHER,38,11  #  18 ||< fict_low:L3_void_b
*surf,side_fr,VERT,-,-,-,mull_90,OPAQUE,ANOTHER,21,05  #  19 ||< frame_open:L2_meeting
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,0,72.40,0  # zone base
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
