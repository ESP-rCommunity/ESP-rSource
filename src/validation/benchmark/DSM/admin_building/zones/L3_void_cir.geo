*Geometry 1.1,GEN,L3_L4_cir_vd # tag version, format, zone name
*date Sat Aug 20 09:43:09 2011  # latest file modification 
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
*edges,5,9,10,23,24,36  # 14
*edges,4,10,11,25,23  # 15
*edges,4,11,12,26,25  # 16
*edges,4,12,13,27,26  # 17
*edges,4,32,14,28,33  # 18
*edges,5,14,15,34,29,28  # 19
*edges,4,34,35,30,29  # 20
*edges,4,35,36,24,30  # 21
*edges,9,24,23,25,26,27,33,28,29,30  # 22
*edges,4,5,31,32,13  # 23
*edges,4,13,32,33,27  # 24
*edges,4,15,16,35,34  # 25
*edges,4,16,9,36,35  # 26
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
*surf,fac_glaz,VERT,-,-,-,db_lpasol,DSF4554_06nb,EXTERIOR,0,0  #   5 ||< external
*surf,fac_stone,VERT,-,-,-,stone_m_ell,OPAQUE,EXTERIOR,0,0  #   6 ||< external
*surf,fict_open,VERT,-,-,-,fictitious,SC_fictit,ANOTHER,49,04  #   7 ||< fict_open:L3_open_rig
*surf,fict_open_b,VERT,-,-,-,fictitious,SC_fictit,ANOTHER,48,32  #   8 ||< fict_open_b:L3_open_lft
*surf,L3_vc_low,FLOR,-,-,-,fictitious,SC_fictit,ANOTHER,20,17  #   9 ||< L3_vc_low:L2_careers
*surf,balcony_a,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,49,03  #  10 ||< balcony_a:L3_open_rig
*surf,struc_a,VERT,-,-,-,slab_275,OPAQUE,UNKNOWN,0,0  #  11 ||< not yet defined
*surf,balcony_b,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,48,33  #  12 ||< balcony_b:L3_open_lft
*surf,struc_b,VERT,-,-,-,slab_275,OPAQUE,UNKNOWN,0,0  #  13 ||< not yet defined
*surf,seg_acu,VERT,-,-,-,glaz_ptn,SCF8477_10nb,UNKNOWN,0,0  #  14 ||< not yet defined
*surf,seg_bcu,VERT,-,-,-,glaz_ptn,SCF8477_10nb,UNKNOWN,0,0  #  15 ||< not yet defined
*surf,seg_ccu,VERT,-,-,-,glaz_ptn,SCF8477_10nb,UNKNOWN,0,0  #  16 ||< not yet defined
*surf,seg_dcu,VERT,-,-,-,gyp_ins_gyp,OPAQUE,UNKNOWN,0,0  #  17 ||< not yet defined
*surf,fac_glu,VERT,-,-,-,db_lpasol,DSF4554_06nb,UNKNOWN,0,0  #  18 ||< not yet defined
*surf,fac_stoneu,VERT,-,-,-,stone_m_ell,OPAQUE,UNKNOWN,0,0  #  19 ||< not yet defined
*surf,Wall-7,VERT,-,-,-,stone_atr_pt,OPAQUE,UNKNOWN,0,0  #  20 ||< not yet defined
*surf,Wall-8,VERT,-,-,-,glaz_ptn,SCF8477_10nb,UNKNOWN,0,0  #  21 ||< not yet defined
*surf,Top-9,CEIL,-,-,-,roof_0.2,OPAQUE,UNKNOWN,0,0  #  22 ||< not yet defined
*surf,fac_fr_l,VERT,-,-,-,mull_90,OPAQUE,EXTERIOR,0,0  #  23 ||< external
*surf,fac_fr_u,VERT,-,-,-,mull_90,OPAQUE,EXTERIOR,0,0  #  24 ||< external
*surf,balcony_au,VERT,-,-,-,glaz_ptn,SCF8477_10nb,UNKNOWN,0,0  #  25 ||< not yet defined
*surf,balcony_bu,VERT,-,-,-,glaz_ptn,SCF8477_10nb,UNKNOWN,0,0  #  26 ||< not yet defined
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,9,     8.16 0  # zone base list
