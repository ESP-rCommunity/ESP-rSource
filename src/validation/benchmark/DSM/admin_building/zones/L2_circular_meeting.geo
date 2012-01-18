*Geometry 1.1,GEN,L2_circular # tag version, format, zone name
*date Thu Jul 14 19:51:08 2011  # latest file modification 
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
*surf,seg_a,VERT,-,-,-,UNKNOWN,OPAQUE,ANOTHER,01,13  #   1 ||< seg_a:l2_void_a
*surf,seg_b,VERT,-,-,-,UNKNOWN,OPAQUE,ANOTHER,01,14  #   2 ||< seg_b:l2_void_a
*surf,seg_c,VERT,-,-,-,UNKNOWN,OPAQUE,ANOTHER,01,15  #   3 ||< seg_c:l2_void_a
*surf,seg_d,VERT,-,-,-,UNKNOWN,OPAQUE,ANOTHER,01,16  #   4 ||< seg_d:l2_void_a
*surf,seg_e,VERT,-,-,-,UNKNOWN,OPAQUE,ANOTHER,01,17  #   5 ||< seg_e:l2_void_a
*surf,seg_f,VERT,-,-,-,UNKNOWN,OPAQUE,ANOTHER,01,18  #   6 ||< seg_f:l2_void_a
*surf,seg_g,VERT,-,-,-,UNKNOWN,OPAQUE,UNKNOWN,00,00  #   7 ||< not yet defined
*surf,seg_h,VERT,-,-,-,UNKNOWN,OPAQUE,UNKNOWN,00,00  #   8 ||< not yet defined
*surf,seg_i,VERT,-,-,-,UNKNOWN,OPAQUE,UNKNOWN,00,00  #   9 ||< not yet defined
*surf,seg_j,VERT,-,-,-,UNKNOWN,OPAQUE,UNKNOWN,00,00  #  10 ||< not yet defined
*surf,seg_k,VERT,-,-,-,UNKNOWN,OPAQUE,UNKNOWN,00,00  #  11 ||< not yet defined
*surf,seg_l,VERT,-,-,-,UNKNOWN,OPAQUE,ANOTHER,58,13  #  12 ||< seg_a:L2_careers
*surf,seg_m,VERT,-,-,-,UNKNOWN,OPAQUE,ANOTHER,58,14  #  13 ||< seg_b:L2_careers
*surf,seg_n,VERT,-,-,-,UNKNOWN,OPAQUE,ANOTHER,58,15  #  14 ||< seg_c:L2_careers
*surf,seg_o,VERT,-,-,-,UNKNOWN,OPAQUE,ANOTHER,58,16  #  15 ||< seg_d:L2_careers
*surf,facade_a,VERT,-,-,-,UNKNOWN,OPAQUE,EXTERIOR,0,0  #  16 ||< external
*surf,frame,VERT,-,-,-,UNKNOWN,OPAQUE,EXTERIOR,0,0  #  17 ||< external
*surf,facade_b,VERT,-,-,-,UNKNOWN,OPAQUE,EXTERIOR,0,0  #  18 ||< external
*surf,Top,CEIL,-,-,-,UNKNOWN,OPAQUE,ANOTHER,42,20  #  19 ||< Base:L3_circul
*surf,Base,FLOR,-,-,-,UNKNOWN,OPAQUE,UNKNOWN,0,0  #  20 ||< not yet defined
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,0,42.00,0  # zone base
