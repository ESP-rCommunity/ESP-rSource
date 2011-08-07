*Geometry 1.1,GEN,L2_circular # tag version, format, zone name
*date Sun Aug  7 11:50:00 2011  # latest file modification 
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
*vertex,29.16426,14.39213,56.35000  #  16
*vertex,30.39923,14.67713,52.45000  #  17
*vertex,30.39923,14.67713,56.35000  #  18
*vertex,31.53920,14.62963,52.45000  #  19
*vertex,31.53920,14.62963,56.35000  #  20
*vertex,32.96416,14.01214,52.45000  #  21
*vertex,32.96416,14.01214,56.35000  #  22
*vertex,34.25000,13.06217,52.45000  #  23
*vertex,34.25000,13.06217,56.35000  #  24
*vertex,34.50000,12.11219,52.45000  #  25
*vertex,34.50000,12.11219,56.35000  #  26
*vertex,34.60000,10.97222,52.45000  #  27
*vertex,34.60000,10.97222,56.35000  #  28
*vertex,34.38913,9.92725,52.45000  #  29
*vertex,34.38913,9.92725,56.35000  #  30
*vertex,33.86664,8.78728,52.45000  #  31
*vertex,33.86664,8.78728,56.35000  #  32
*vertex,31.35752,8.18413,52.45000  #  33
*vertex,31.35752,8.18413,56.35000  #  34
*vertex,31.50336,8.21918,52.45000  #  35
*vertex,31.50336,8.21919,56.34999  #  36
*vertex,29.16426,14.39213,55.59300  #  37
*vertex,28.07178,13.63215,55.59300  #  38
*vertex,30.39923,14.67713,55.59300  #  39
*vertex,31.53920,14.62963,55.59300  #  40
*vertex,32.96416,14.01214,55.59300  #  41
*vertex,34.25000,13.06217,53.45000  #  42
*vertex,34.25000,13.06217,55.59300  #  43
# 
# tag, number of vertices followed by list of associated vert
*edges,4,2,1,4,3  #  1
*edges,4,5,2,3,6  #  2
*edges,4,7,5,6,8  #  3
*edges,4,9,7,8,10  #  4
*edges,4,11,9,10,12  #  5
*edges,5,13,11,12,14,38  #  6
*edges,4,37,38,14,16  #  7
*edges,4,39,37,16,18  #  8
*edges,4,40,39,18,20  #  9
*edges,4,41,40,20,22  # 10
*edges,4,43,41,22,24  # 11
*edges,6,25,23,42,43,24,26  # 12
*edges,4,27,25,26,28  # 13
*edges,4,29,27,28,30  # 14
*edges,4,31,29,30,32  # 15
*edges,4,1,33,34,4  # 16
*edges,4,33,35,36,34  # 17
*edges,4,35,31,32,36  # 18
*edges,18,4,34,36,32,30,28,26,24,22,20,18,16,14,12,10,8,6,3  # 19
*edges,18,1,2,5,7,9,11,13,15,17,19,21,23,25,27,29,31,35,33  # 20
*edges,4,15,13,38,37  # 21
*edges,4,17,15,37,39  # 22
*edges,4,19,17,39,40  # 23
*edges,4,21,19,40,41  # 24
*edges,5,23,21,41,43,42  # 25
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
*surf,seg_g,VERT,-,-,-,slab_275,OPAQUE,UNKNOWN,0,0  #   7 ||< not yet defined
*surf,seg_h,VERT,-,-,-,slab_275,OPAQUE,UNKNOWN,0,0  #   8 ||< not yet defined
*surf,seg_i,VERT,-,-,-,slab_275,OPAQUE,UNKNOWN,0,0  #   9 ||< not yet defined
*surf,seg_j,VERT,-,-,-,slab_275,OPAQUE,UNKNOWN,0,0  #  10 ||< not yet defined
*surf,seg_k,VERT,-,-,-,slab_275,OPAQUE,UNKNOWN,0,0  #  11 ||< not yet defined
*surf,seg_l,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,20,13  #  12 ||< seg_a:L2_careers
*surf,seg_m,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,20,14  #  13 ||< seg_b:L2_careers
*surf,seg_n,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,20,15  #  14 ||< seg_c:L2_careers
*surf,seg_o,VERT,-,-,-,gyp_ins_gyp,OPAQUE,ANOTHER,20,16  #  15 ||< seg_d:L2_careers
*surf,facade_a,VERT,-,-,-,db_lpasol,DSF4554_06nb,EXTERIOR,0,0  #  16 ||< external
*surf,frame,VERT,-,-,-,mull_90,OPAQUE,EXTERIOR,0,0  #  17 ||< external
*surf,facade_b,VERT,-,-,-,db_lpasol,DSF4554_06nb,EXTERIOR,0,0  #  18 ||< external
*surf,Top,CEIL,-,-,-,slab_275,OPAQUE,UNKNOWN,0,0  #  19 ||< not yet defined
*surf,Base,FLOR,-,-,-,flr_ov_pln,OPAQUE,ANOTHER,17,46  #  20 ||< Base:L2_uf_plen
*surf,seg_al,VERT,-,-,-,gyp_ins_gyp,OPAQUE,ANOTHER,22,03  #  21 ||< seg_a:L2_open_pln
*surf,seg_bl,VERT,-,-,-,gyp_ins_gyp,OPAQUE,ANOTHER,22,04  #  22 ||< seg_b:L2_open_pln
*surf,seg_cl,VERT,-,-,-,gyp_ins_gyp,OPAQUE,ANOTHER,22,05  #  23 ||< seg_c:L2_open_pln
*surf,seg_dl,VERT,-,-,-,gyp_ins_gyp,OPAQUE,ANOTHER,22,06  #  24 ||< seg_d:L2_open_pln
*surf,seg_el,VERT,-,-,-,gyp_ins_gyp,OPAQUE,ANOTHER,22,07  #  25 ||< seg_e:L2_open_pln
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,0,42.00,0  # zone base
