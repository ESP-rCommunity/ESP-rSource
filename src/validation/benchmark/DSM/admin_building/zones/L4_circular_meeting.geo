*Geometry 1.1,GEN,L4_circular # tag version, format, zone name
*date Thu Jul 14 19:51:55 2011  # latest file modification 
L4_circular describes circular meeting room at level four
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,28.92676,7.59981,60.25000  #   1
*vertex,27.88179,8.35979,60.25000  #   2
*vertex,27.88179,8.35979,64.15000  #   3
*vertex,28.92676,7.59981,64.15000  #   4
*vertex,27.21681,9.30976,60.25000  #   5
*vertex,27.21681,9.30976,64.15000  #   6
*vertex,26.93181,10.54473,60.25000  #   7
*vertex,26.93181,10.54473,64.15000  #   8
*vertex,26.97931,11.68470,60.25000  #   9
*vertex,26.97931,11.68470,64.15000  #  10
*vertex,27.40680,12.77717,60.25000  #  11
*vertex,27.40680,12.77717,64.15000  #  12
*vertex,28.07178,13.63215,60.25000  #  13
*vertex,28.07178,13.63215,64.15000  #  14
*vertex,29.16426,14.39213,60.25000  #  15
*vertex,29.16426,14.39213,64.15000  #  16
*vertex,30.39923,14.67713,60.25000  #  17
*vertex,30.39923,14.67713,64.15000  #  18
*vertex,31.53920,14.62963,60.25000  #  19
*vertex,31.53920,14.62963,64.15000  #  20
*vertex,32.96416,14.01214,60.25000  #  21
*vertex,32.96416,14.01214,64.15000  #  22
*vertex,34.25000,13.06217,60.25000  #  23
*vertex,34.25000,13.06217,64.15000  #  24
*vertex,34.50000,12.11219,60.25000  #  25
*vertex,34.50000,12.11219,64.15000  #  26
*vertex,34.60000,10.97222,60.25000  #  27
*vertex,34.60000,10.97222,64.15000  #  28
*vertex,34.38913,9.92725,60.25000  #  29
*vertex,34.38913,9.92725,64.15000  #  30
*vertex,33.86664,8.78728,60.25000  #  31
*vertex,33.86664,8.78728,64.15000  #  32
*vertex,31.35752,8.18413,60.25000  #  33
*vertex,31.35752,8.18413,64.15000  #  34
*vertex,31.50336,8.21918,60.25000  #  35
*vertex,31.50336,8.21919,64.14999  #  36
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
*surf,seg_a,VERT,-,-,-,UNKNOWN,OPAQUE,UNKNOWN,0,0  #   1 ||< not yet defined
*surf,seg_b,VERT,-,-,-,UNKNOWN,OPAQUE,UNKNOWN,0,0  #   2 ||< not yet defined
*surf,seg_c,VERT,-,-,-,UNKNOWN,OPAQUE,UNKNOWN,0,0  #   3 ||< not yet defined
*surf,seg_d,VERT,-,-,-,UNKNOWN,OPAQUE,UNKNOWN,0,0  #   4 ||< not yet defined
*surf,seg_e,VERT,-,-,-,UNKNOWN,OPAQUE,UNKNOWN,0,0  #   5 ||< not yet defined
*surf,seg_f,VERT,-,-,-,UNKNOWN,OPAQUE,UNKNOWN,0,0  #   6 ||< not yet defined
*surf,seg_g,VERT,-,-,-,UNKNOWN,OPAQUE,UNKNOWN,0,0  #   7 ||< not yet defined
*surf,seg_h,VERT,-,-,-,UNKNOWN,OPAQUE,UNKNOWN,0,0  #   8 ||< not yet defined
*surf,seg_i,VERT,-,-,-,UNKNOWN,OPAQUE,UNKNOWN,0,0  #   9 ||< not yet defined
*surf,seg_j,VERT,-,-,-,UNKNOWN,OPAQUE,UNKNOWN,0,0  #  10 ||< not yet defined
*surf,seg_k,VERT,-,-,-,UNKNOWN,OPAQUE,UNKNOWN,0,0  #  11 ||< not yet defined
*surf,seg_l,VERT,-,-,-,UNKNOWN,OPAQUE,ANOTHER,40,01  #  12 ||< Wall-1:L4_void_c
*surf,seg_m,VERT,-,-,-,UNKNOWN,OPAQUE,ANOTHER,40,02  #  13 ||< Wall-2:L4_void_c
*surf,seg_n,VERT,-,-,-,UNKNOWN,OPAQUE,ANOTHER,40,03  #  14 ||< Wall-3:L4_void_c
*surf,seg_o,VERT,-,-,-,UNKNOWN,OPAQUE,ANOTHER,40,04  #  15 ||< Wall-4:L4_void_c
*surf,facade_a,VERT,-,-,-,UNKNOWN,OPAQUE,EXTERIOR,0,0  #  16 ||< external
*surf,frame,VERT,-,-,-,UNKNOWN,OPAQUE,EXTERIOR,0,0  #  17 ||< external
*surf,facade_b,VERT,-,-,-,UNKNOWN,OPAQUE,EXTERIOR,0,0  #  18 ||< external
*surf,Top,CEIL,-,-,-,UNKNOWN,OPAQUE,UNKNOWN,0,0  #  19 ||< not yet defined
*surf,Base,FLOR,-,-,-,UNKNOWN,OPAQUE,ANOTHER,42,19  #  20 ||< Top:L3_circul
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,0,42.00,0  # zone base
