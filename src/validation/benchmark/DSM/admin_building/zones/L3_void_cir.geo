*Geometry 1.1,GEN,L3_L4_cir_vd # tag version, format, zone name
*date Mon Jan 30 17:54:01 2012  # latest file modification 
L3_void_cir describes a void adjacent to circular meeting at L3
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,34.25000,13.06200,56.35000  #   1
*vertex,34.50000,12.11219,56.35000  #   2
*vertex,34.60000,10.97222,56.35000  #   3
*vertex,34.38913,9.92725,56.35000  #   4
*vertex,33.86664,8.78728,56.35000  #   5
*vertex,36.24158,9.35726,56.35000  #   6
*vertex,36.76406,9.49976,56.35000  #   7
*vertex,35.91460,13.46780,56.35000  #   8
*vertex,34.25000,13.06200,60.25000  #   9
*vertex,34.50000,12.11219,60.25000  #  10
*vertex,34.60000,10.97222,60.25000  #  11
*vertex,34.38913,9.92725,60.25000  #  12
*vertex,33.86664,8.78728,60.25000  #  13
*vertex,36.24158,9.35726,60.25000  #  14
*vertex,36.76406,9.49976,60.25000  #  15
*vertex,35.91460,13.46780,60.25000  #  16
*vertex,34.25000,13.06200,57.35000  #  17
*vertex,35.91460,13.46780,57.35000  #  18
*vertex,36.76406,9.49976,57.35000  #  19
*vertex,34.25000,13.06200,59.49300  #  20
*vertex,36.76406,9.49976,59.49300  #  21
*vertex,35.91460,13.46780,59.49300  #  22
*vertex,34.50000,12.11219,64.15000  #  23
*vertex,34.25000,13.06200,64.15000  #  24
*vertex,34.60000,10.97222,64.15000  #  25
*vertex,34.38913,9.92725,64.15000  #  26
*vertex,33.86664,8.78728,64.15000  #  27
*vertex,36.24158,9.35726,64.15000  #  28
*vertex,36.76406,9.49976,64.15000  #  29
*vertex,35.91460,13.46780,64.15000  #  30
*vertex,34.25559,8.88063,56.34999  #  31
*vertex,34.25559,8.88063,60.25000  #  32
*vertex,34.25559,8.88063,64.15000  #  33
*vertex,36.76406,9.49976,61.25000  #  34
*vertex,35.91460,13.46780,61.25000  #  35
*vertex,34.25000,13.06200,61.25000  #  36
*vertex,36.76406,9.49976,63.39300  #  37
*vertex,35.91460,13.46780,63.39300  #  38
*vertex,34.25000,13.06200,63.39300  #  39
# 
# tag, number of vertices followed by list of associated vert
*edges,6,1,2,10,9,20,17  #  1
*edges,4,2,3,11,10  #  2
*edges,4,3,4,12,11  #  3
*edges,4,4,5,13,12  #  4
*edges,4,31,6,14,32  #  5
*edges,6,6,7,19,21,15,14  #  6
*edges,4,19,18,22,21  #  7
*edges,4,18,17,20,22  #  8
*edges,9,1,8,7,6,31,5,4,3,2  #  9
*edges,4,7,8,18,19  # 10
*edges,4,21,22,16,15  # 11
*edges,4,8,1,17,18  # 12
*edges,4,22,20,9,16  # 13
*edges,6,9,10,23,24,39,36  # 14
*edges,4,10,11,25,23  # 15
*edges,4,11,12,26,25  # 16
*edges,4,12,13,27,26  # 17
*edges,4,32,14,28,33  # 18
*edges,6,14,15,34,37,29,28  # 19
*edges,4,34,35,38,37  # 20
*edges,4,35,36,39,38  # 21
*edges,9,24,23,25,26,27,33,28,29,30  # 22
*edges,4,5,31,32,13  # 23
*edges,4,13,32,33,27  # 24
*edges,4,15,16,35,34  # 25
*edges,4,16,9,36,35  # 26
*edges,4,37,38,30,29  # 27
*edges,4,38,39,24,30  # 28
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,seg_ac,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,36,12  #   1 ||< seg_l:L3_circular
*surf,seg_bc,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,36,13  #   2 ||< seg_m:L3_circular
*surf,seg_cc,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,36,14  #   3 ||< seg_n:L3_circular
*surf,seg_dc,VERT,-,-,-,gyp_ins_gyp,OPAQUE,ANOTHER,36,15  #   4 ||< seg_o:L3_circular
*surf,fac_glaz,VERT,-,D-WINDOW,CLOSED,db_lpasol,DSF4554_06nb,EXTERIOR,0,0  #   5 ||< external
*surf,fac_stone,VERT,-,WALL,-,stone_m_ell,OPAQUE,EXTERIOR,0,0  #   6 ||< external
*surf,fict_open,VERT,-,-,-,fictitious,SC_fictit,ANOTHER,49,04  #   7 ||< fict_open:L3_open_rig
*surf,fict_open_b,VERT,-,-,-,fictitious,SC_fictit,ANOTHER,48,32  #   8 ||< fict_open_b:L3_open_lft
*surf,L3_vc_low,FLOR,-,-,-,fictitious,SC_fictit,ANOTHER,20,17  #   9 ||< L3_vc_low:L2_careers
*surf,balcony_a,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,49,03  #  10 ||< balcony_a:L3_open_rig
*surf,struc_a,VERT,-,-,-,slab_275,OPAQUE,ANOTHER,78,13  #  11 ||< struc_av:L4_flrvoid
*surf,balcony_b,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,48,33  #  12 ||< balcony_b:L3_open_lft
*surf,struc_b,VERT,-,-,-,slab_275,OPAQUE,ANOTHER,78,14  #  13 ||< struc_bv:L4_flrvoid
*surf,seg_acu,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,52,12  #  14 ||< seg_l:L4_circular
*surf,seg_bcu,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,52,13  #  15 ||< seg_m:L4_circular
*surf,seg_ccu,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,52,14  #  16 ||< seg_n:L4_circular
*surf,seg_dcu,VERT,-,-,-,gyp_ins_gyp,OPAQUE,ANOTHER,52,15  #  17 ||< seg_o:L4_circular
*surf,fac_glu,VERT,-,D-WINDOW,CLOSED,db_lpasol,DSF4554_06nb,EXTERIOR,0,0  #  18 ||< external
*surf,fac_stoneu,VERT,-,WALL,-,stone_m_ell,OPAQUE,EXTERIOR,0,0  #  19 ||< external
*surf,Wall-7,VERT,-,-,-,stone_atr_pt,OPAQUE,ANOTHER,57,02  #  20 ||< ptn_vc_b:L4_open_plr
*surf,Wall-8,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,57,01  #  21 ||< ptn_vc_a:L4_open_plr
*surf,Top-9,CEIL,-,ROOF,FLAT,roof_0.2,OPAQUE,EXTERIOR,0,0  #  22 ||< external
*surf,fac_fr_l,VERT,-,WALL,-,mull_90,OPAQUE,EXTERIOR,0,0  #  23 ||< external
*surf,fac_fr_u,VERT,-,WALL,-,mull_90,OPAQUE,EXTERIOR,0,0  #  24 ||< external
*surf,balcony_au,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,57,32  #  25 ||< balcony_au:L4_open_plr
*surf,balcony_bu,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,57,33  #  26 ||< balcony_bu:L4_open_plr
*surf,parapet_a,VERT,-,WALL,-,stone_cast_i,OPAQUE,EXTERIOR,0,0  #  27 ||< external
*surf,parapet_b,VERT,-,WALL,-,roof_0.2,OPAQUE,EXTERIOR,0,0  #  28 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,9,     8.16 0  # zone base list
# 
# block entities:
#  *obs = obstructions
*block_start, 20 20 # geometric blocks
*obs,33.800,8.400,47.950,0.100,0.200,16.680,13.000,1.00,sol_vo,mull_90  # block   1
*obs,28.999,6.900,56.180,7.250,0.450,0.050,14.721,1.00,hor_t,mull_90  # block   2
*obs,28.999,6.900,56.460,7.250,0.450,0.050,14.721,1.00,hor_u,mull_90  # block   3
*obs,28.999,6.900,56.740,7.250,0.450,0.050,14.721,1.00,hor_v,mull_90  # block   4
*obs,28.999,6.900,57.020,7.250,0.450,0.050,14.721,1.00,hor_w,mull_90  # block   5
*obs,28.999,6.900,57.300,7.250,0.450,0.050,14.721,1.00,hor_x,mull_90  # block   6
*obs,28.999,6.900,57.580,7.250,0.450,0.050,14.721,1.00,hor_y,mull_90  # block   7
*obs,28.999,6.900,57.860,7.250,0.450,0.050,14.721,1.00,hor_z,mull_90  # block   8
*obs,28.999,6.900,58.140,7.250,0.450,0.050,14.721,1.00,hor_za,mull_90  # block   9
*obs,28.999,6.900,58.420,7.250,0.450,0.050,14.721,1.00,hor_zb,mull_90  # block  10
*obs,28.999,6.900,58.700,7.250,0.450,0.050,14.721,1.00,hor_zc,mull_90  # block  11
*obs,28.999,6.900,58.980,7.250,0.450,0.050,14.721,1.00,hor_zd,mull_90  # block  12
*obs,28.999,6.900,59.260,7.250,0.450,0.050,14.721,1.00,hor_ze,mull_90  # block  13
*obs,28.999,6.900,59.540,7.250,0.450,0.050,14.721,1.00,hor_zf,mull_90  # block  14
*obs,28.999,6.900,59.820,7.250,0.450,0.050,14.721,1.00,hor_zg,mull_90  # block  15
*obs,28.999,6.900,60.100,7.250,0.450,0.050,14.721,1.00,hor_zh,mull_90  # block  16
*obs,28.999,6.900,60.380,7.250,0.450,0.050,14.721,1.00,hor_zi,mull_90  # block  17
*obs,28.999,6.900,60.660,7.250,0.450,0.050,14.721,1.00,hor_zj,mull_90  # block  18
*obs,28.999,6.900,60.940,7.250,0.450,0.050,14.721,1.00,hor_zk,mull_90  # block  19
*obs,28.999,6.900,61.220,7.250,0.450,0.050,14.721,1.00,hor_zl,mull_90  # block  20
*obs,28.999,6.900,61.500,7.250,0.450,0.050,14.721,1.00,hor_zm,mull_90  # block  21
*obs,28.999,6.900,61.780,7.250,0.450,0.050,14.721,1.00,hor_zn,mull_90  # block  22
*obs,28.999,6.900,62.060,7.250,0.450,0.050,14.721,1.00,hor_zo,mull_90  # block  23
*obs,28.999,6.900,62.340,7.250,0.450,0.050,14.721,1.00,hor_zp,mull_90  # block  24
*obs,28.999,6.900,62.620,7.250,0.450,0.050,14.721,1.00,hor_zq,mull_90  # block  25
*obs,28.999,6.900,62.900,7.250,0.450,0.050,14.721,1.00,hor_zr,mull_90  # block  26
*obs,28.999,6.900,63.180,7.250,0.450,0.050,14.721,1.00,hor_zs,mull_90  # block  27
*obs,28.999,6.900,63.460,7.250,0.450,0.050,14.721,1.00,hor_zt,mull_90  # block  28
*obs,28.999,6.900,63.740,7.250,0.450,0.050,14.721,1.00,hor_zu,mull_90  # block  29
*obs,28.999,6.900,64.020,7.250,0.450,0.050,14.721,1.00,hor_zv,mull_90  # block  30
*obs,28.999,6.900,64.300,7.250,0.450,0.050,14.721,1.00,hor_zw,mull_90  # block  31
*obs,28.999,6.900,64.580,7.250,0.450,0.050,14.721,1.00,hor_zx,mull_90  # block  32
*end_block
