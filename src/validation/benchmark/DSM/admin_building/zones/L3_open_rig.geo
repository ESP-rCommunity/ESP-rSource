*Geometry 1.1,GEN,L3_open_rig # tag version, format, zone name
*date Thu Aug 11 21:26:02 2011  # latest file modification 
L3_open_rig describes right portion of open plan on L3
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,50.00000,30.50000,56.35000  #   1
*vertex,51.00000,30.50000,56.35000  #   2
*vertex,51.00000,30.50000,59.49300  #   3
*vertex,50.00000,30.50000,59.49300  #   4
*vertex,36.76406,9.49976,56.35000  #   5
*vertex,35.91460,13.46780,56.35000  #   6
*vertex,35.91460,13.46780,57.35000  #   7
*vertex,36.76406,9.49976,57.35000  #   8
*vertex,35.91460,13.46780,59.49300  #   9
*vertex,36.76406,9.49976,59.49300  #  10
*vertex,44.69636,15.62710,56.35000  #  11
*vertex,45.64634,11.68470,56.35000  #  12
*vertex,45.64634,11.68470,59.49300  #  13
*vertex,44.69636,15.62710,59.49300  #  14
*vertex,51.60000,23.17941,56.35000  #  15
*vertex,51.60000,20.42448,56.35000  #  16
*vertex,51.60000,20.42448,59.49300  #  17
*vertex,51.60000,23.17941,59.49300  #  18
*vertex,51.60000,19.23701,56.35000  #  19
*vertex,51.60000,19.23701,59.49300  #  20
*vertex,51.60000,18.19204,56.35000  #  21
*vertex,51.60000,18.19204,59.49300  #  22
*vertex,51.60000,17.28956,56.35000  #  23
*vertex,51.60000,17.28956,59.49300  #  24
*vertex,34.34163,35.70000,56.35000  #  25
*vertex,38.04653,35.70000,56.35000  #  26
*vertex,38.04653,35.70000,59.49300  #  27
*vertex,34.34163,35.70000,59.49300  #  28
*vertex,41.37145,35.70000,56.35000  #  29
*vertex,41.37145,35.70000,59.49300  #  30
*vertex,44.69636,35.70000,56.35000  #  31
*vertex,44.69636,35.70000,59.49300  #  32
*vertex,48.02128,35.70000,56.35000  #  33
*vertex,48.02128,35.70000,59.49300  #  34
*vertex,50.00000,35.70000,56.35000  #  35
*vertex,50.00000,35.70000,59.49300  #  36
*vertex,34.25000,21.30000,56.35000  #  37
*vertex,38.00000,21.30000,56.35000  #  38
*vertex,38.00000,21.30000,57.35000  #  39
*vertex,34.25000,21.30000,57.35000  #  40
*vertex,38.00000,28.50000,56.35000  #  41
*vertex,38.00000,28.50000,57.35000  #  42
*vertex,38.00000,21.30000,59.49300  #  43
*vertex,34.25000,21.30000,59.49300  #  44
*vertex,38.00000,28.50000,59.49300  #  45
*vertex,34.25000,28.50000,56.35000  #  46
*vertex,34.25000,28.50000,57.35000  #  47
*vertex,34.25000,28.50000,59.49300  #  48
*vertex,51.00000,28.49928,56.35000  #  49
*vertex,51.00000,23.17941,56.35000  #  50
*vertex,51.00000,23.17941,59.49300  #  51
*vertex,51.00000,28.49928,59.49300  #  52
*vertex,45.64634,11.68470,57.35000  #  53
# 
# tag, number of vertices followed by list of associated vert
*edges,4,2,1,4,3  #  1
*edges,4,1,35,36,4  #  2
*edges,4,6,5,8,7  #  3
*edges,4,7,8,10,9  #  4
*edges,5,12,11,14,13,53  #  5
*edges,4,11,23,24,14  #  6
*edges,4,16,15,18,17  #  7
*edges,4,19,16,17,20  #  8
*edges,4,21,19,20,22  #  9
*edges,4,23,21,22,24  # 10
*edges,4,26,25,28,27  # 11
*edges,4,29,26,27,30  # 12
*edges,4,31,29,30,32  # 13
*edges,4,33,31,32,34  # 14
*edges,4,35,33,34,36  # 15
*edges,4,38,37,40,39  # 16
*edges,4,41,38,39,42  # 17
*edges,4,43,39,40,44  # 18
*edges,4,45,42,39,43  # 19
*edges,6,46,41,42,45,48,47  # 20
*edges,4,50,49,52,51  # 21
*edges,4,15,50,51,18  # 22
*edges,4,49,2,3,52  # 23
*edges,6,37,6,7,9,44,40  # 24
*edges,5,25,46,47,48,28  # 25
*edges,6,5,12,53,13,10,8  # 26
*edges,23,14,24,22,20,17,18,51,52,3,4,36,34,32,30,27,28,48,45,43,44,9,10,13  # 27
*edges,21,23,11,6,37,38,41,46,25,26,29,31,33,35,1,2,49,50,15,16,19,21  # 28
*edges,4,6,11,12,5  # 29
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,ptnc_str_l3,VERT,-,-,-,conc_250,OPAQUE,UNKNOWN,0,0  #   1 ||< external
*surf,ptnd_str_l3,VERT,-,-,-,conc_250,OPAQUE,UNKNOWN,0,0  #   2 ||< external
*surf,balcony_a,VERT,-,-,-,glaz_ptn,SCF8477_10nb,UNKNOWN,0,0  #   3 ||< external
*surf,fict_open,VERT,-,-,-,fictitious,SC_fictit,UNKNOWN,0,0  #   4 ||< external
*surf,int_glazb,VERT,-,-,-,glaz_ptn,SCF8477_10nb,UNKNOWN,0,0  #   5 ||< external
*surf,int_glaz,VERT,-,-,-,glaz_ptn,SCF8477_10nb,UNKNOWN,0,0  #   6 ||< external
*surf,ptn_open_a,VERT,-,-,-,gyp_aco_gyp,OPAQUE,UNKNOWN,0,0  #   7 ||< external
*surf,ptn_open_b,VERT,-,-,-,glaz_ptn,SCF8477_10nb,UNKNOWN,0,0  #   8 ||< external
*surf,door,VERT,-,-,-,glaz_ptn,SCF8477_10nb,UNKNOWN,0,0  #   9 ||< external
*surf,ptn_open_c,VERT,-,-,-,glaz_ptn,SCF8477_10nb,UNKNOWN,0,0  #  10 ||< external
*surf,ptn_open_cc,VERT,-,-,-,gyp_aco_gyp,OPAQUE,UNKNOWN,0,0  #  11 ||< external
*surf,ptn_open_d,VERT,-,-,-,gyp_aco_gyp,OPAQUE,UNKNOWN,0,0  #  12 ||< external
*surf,ptn_open_e,VERT,-,-,-,gyp_aco_gyp,OPAQUE,UNKNOWN,0,0  #  13 ||< external
*surf,ptn_open_f,VERT,-,-,-,gyp_aco_gyp,OPAQUE,UNKNOWN,0,0  #  14 ||< external
*surf,ptn_open_g,VERT,-,-,-,gyp_aco_gyp,OPAQUE,UNKNOWN,0,0  #  15 ||< external
*surf,front_rail,VERT,-,-,-,glaz_ptn,SCF8477_10nb,UNKNOWN,0,0  #  16 ||< external
*surf,right_rail,VERT,-,-,-,glaz_ptn,SCF8477_10nb,UNKNOWN,0,0  #  17 ||< external
*surf,l3_lw2_fr,VERT,-,-,-,fictitious,SC_fictit,UNKNOWN,0,0  #  18 ||< external
*surf,l3_lw2_ri,VERT,-,-,-,fictitious,SC_fictit,UNKNOWN,0,0  #  19 ||< external
*surf,l3_lw2_bk,VERT,-,-,-,fictitious,SC_fictit,UNKNOWN,0,0  #  20 ||< external
*surf,ptna_opn_l3,VERT,-,-,-,gyp_blk_ptn,OPAQUE,UNKNOWN,0,0  #  21 ||< external
*surf,ptnb_opn_l3,VERT,-,-,-,gyp_blk_ptn,OPAQUE,UNKNOWN,0,0  #  22 ||< external
*surf,ptn_cel_b,VERT,-,-,-,gyp_blk_ptn,OPAQUE,UNKNOWN,0,0  #  23 ||< external
*surf,fict_lr_a,VERT,-,-,-,fictitious,SC_fictit,UNKNOWN,0,0  #  24 ||< external
*surf,fict_lr_b,VERT,-,-,-,fictitious,SC_fictit,UNKNOWN,0,0  #  25 ||< external
*surf,facade_st,VERT,-,-,-,stone_m_ell,OPAQUE,UNKNOWN,0,0  #  26 ||< external
*surf,up_open_rig,CEIL,-,-,-,slab_275,OPAQUE,UNKNOWN,0,0  #  27 ||< external
*surf,floor_open_r,FLOR,-,-,-,flr_ov_pln,OPAQUE,UNKNOWN,0,0  #  28 ||< external
*surf,ovhang,FLOR,-,-,-,slab_275,OPAQUE,UNKNOWN,0,0  #  29 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,0,345.93,0  # zone base
