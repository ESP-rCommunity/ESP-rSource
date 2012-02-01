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
