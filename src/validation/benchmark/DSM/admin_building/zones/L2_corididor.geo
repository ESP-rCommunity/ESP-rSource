*Geometry 1.1,GEN,L2_coridor # tag version, format, zone name
*date Wed Jul 27 23:29:21 2011  # latest file modification 
L2_coridor describes corridor between banks of cellular offices
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,27.64430,33.70000,52.45000  #   1
*vertex,30.96921,33.70000,52.45000  #   2
*vertex,34.34163,33.70000,52.45000  #   3
*vertex,37.99903,33.70000,52.45000  #   4
*vertex,41.37145,33.70000,52.45000  #   5
*vertex,44.69636,33.70000,52.45000  #   6
*vertex,47.78378,33.70000,52.45000  #   7
*vertex,50.00000,33.70000,52.45000  #   8
*vertex,50.00000,35.70000,52.45000  #   9
*vertex,48.02128,35.70000,52.45000  #  10
*vertex,44.69636,35.70000,52.45000  #  11
*vertex,41.37145,35.70000,52.45000  #  12
*vertex,38.04653,35.70000,52.45000  #  13
*vertex,34.34163,35.70000,52.45000  #  14
*vertex,31.01671,35.70000,52.45000  #  15
*vertex,27.69180,35.70000,52.45000  #  16
*vertex,24.03439,35.70000,52.45000  #  17
*vertex,20.70947,35.70000,52.45000  #  18
*vertex,17.38456,35.70000,52.45000  #  19
*vertex,14.48713,35.70000,52.45000  #  20
*vertex,12.00000,35.70000,52.45000  #  21
*vertex,12.00000,30.50000,52.45000  #  22
*vertex,13.70000,30.50000,52.45000  #  23
*vertex,13.70000,33.70000,52.45000  #  24
*vertex,17.43206,33.70000,52.45000  #  25
*vertex,22.75192,33.70000,52.45000  #  26
*vertex,27.64430,33.70000,55.59300  #  27
*vertex,30.96921,33.70000,55.59300  #  28
*vertex,34.34163,33.70000,55.59300  #  29
*vertex,37.99903,33.70000,55.59300  #  30
*vertex,41.37145,33.70000,55.59300  #  31
*vertex,44.69636,33.70000,55.59300  #  32
*vertex,47.78378,33.70000,55.59300  #  33
*vertex,50.00000,33.70000,55.59300  #  34
*vertex,50.00000,35.70000,55.59300  #  35
*vertex,48.02128,35.70000,55.59300  #  36
*vertex,44.69636,35.70000,55.59300  #  37
*vertex,41.37145,35.70000,55.59300  #  38
*vertex,38.04653,35.70000,55.59300  #  39
*vertex,34.34163,35.70000,55.59300  #  40
*vertex,31.01671,35.70000,55.59300  #  41
*vertex,27.69180,35.70000,55.59300  #  42
*vertex,24.03439,35.70000,55.59300  #  43
*vertex,20.70947,35.70000,55.59300  #  44
*vertex,17.38456,35.70000,55.59300  #  45
*vertex,14.48713,35.70000,55.59300  #  46
*vertex,12.00000,35.70000,55.59300  #  47
*vertex,12.00000,30.50000,55.59300  #  48
*vertex,13.70000,30.50000,55.59300  #  49
*vertex,13.70000,33.70000,55.59300  #  50
*vertex,17.43206,33.70000,55.59300  #  51
*vertex,22.75192,33.70000,55.59300  #  52
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,28,27  #  1
*edges,4,2,3,29,28  #  2
*edges,4,3,4,30,29  #  3
*edges,4,4,5,31,30  #  4
*edges,4,5,6,32,31  #  5
*edges,4,6,7,33,32  #  6
*edges,4,7,8,34,33  #  7
*edges,4,8,9,35,34  #  8
*edges,4,9,10,36,35  #  9
*edges,4,10,11,37,36  # 10
*edges,4,11,12,38,37  # 11
*edges,4,12,13,39,38  # 12
*edges,4,13,14,40,39  # 13
*edges,4,14,15,41,40  # 14
*edges,4,15,16,42,41  # 15
*edges,4,16,17,43,42  # 16
*edges,4,17,18,44,43  # 17
*edges,4,18,19,45,44  # 18
*edges,4,19,20,46,45  # 19
*edges,4,20,21,47,46  # 20
*edges,4,21,22,48,47  # 21
*edges,4,22,23,49,48  # 22
*edges,4,23,24,50,49  # 23
*edges,4,24,25,51,50  # 24
*edges,4,25,26,52,51  # 25
*edges,4,26,1,27,52  # 26
*edges,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52  # 27
*edges,26,1,26,25,24,23,22,21,20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2  # 28
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,Wall-1,VERT,-,-,-,UNKNOWN,OPAQUE,ANOTHER,05,05  #   1 ||< Wall-5:L2_prz_a
*surf,Wall-2,VERT,-,-,-,UNKNOWN,OPAQUE,ANOTHER,05,04  #   2 ||< Wall-4:L2_prz_a
*surf,Wall-3,VERT,-,-,-,gyp_aco_gyp,OPAQUE,ANOTHER,04,09  #   3 ||< Wall-12:L2_cel_a
*surf,Wall-4,VERT,-,-,-,gyp_aco_gyp,OPAQUE,ANOTHER,04,08  #   4 ||< Wall-11:L2_cel_a
*surf,Wall-5,VERT,-,-,-,gyp_aco_gyp,OPAQUE,ANOTHER,04,07  #   5 ||< Wall-10:L2_cel_a
*surf,Wall-6,VERT,-,-,-,gyp_aco_gyp,OPAQUE,ANOTHER,04,06  #   6 ||< Wall-9:L2_cel_a
*surf,Wall-7,VERT,-,-,-,UNKNOWN,OPAQUE,UNKNOWN,0,0  #   7 ||< not yet defined
*surf,Wall-8,VERT,-,-,-,UNKNOWN,OPAQUE,UNKNOWN,0,0  #   8 ||< not yet defined
*surf,Wall-9,VERT,-,-,-,UNKNOWN,OPAQUE,ANOTHER,13,02  #   9 ||< ptn_corb:L2_cel319
*surf,Wall-10,VERT,-,-,-,UNKNOWN,OPAQUE,ANOTHER,13,01  #  10 ||< ptn_cora:L2_cel319
*surf,Wall-11,VERT,-,-,-,UNKNOWN,OPAQUE,ANOTHER,12,01  #  11 ||< Wall-1:l2_kitch
*surf,Wall-12,VERT,-,-,-,UNKNOWN,OPAQUE,ANOTHER,08,09  #  12 ||< ptn_cor_i:l2_cell_c
*surf,Wall-13,VERT,-,-,-,UNKNOWN,OPAQUE,ANOTHER,08,08  #  13 ||< ptn_cor_h:l2_cell_c
*surf,Wall-14,VERT,-,-,-,UNKNOWN,OPAQUE,ANOTHER,08,07  #  14 ||< ptn_cor_g:l2_cell_c
*surf,Wall-15,VERT,-,-,-,UNKNOWN,OPAQUE,ANOTHER,08,06  #  15 ||< ptn_cor_f:l2_cell_c
*surf,Wall-16,VERT,-,-,-,UNKNOWN,OPAQUE,ANOTHER,08,05  #  16 ||< ptn_cor_e:l2_cell_c
*surf,Wall-17,VERT,-,-,-,UNKNOWN,OPAQUE,ANOTHER,08,04  #  17 ||< ptn_cor_d:l2_cell_c
*surf,Wall-18,VERT,-,-,-,UNKNOWN,OPAQUE,ANOTHER,08,03  #  18 ||< ptn_cor_c:l2_cell_c
*surf,Wall-19,VERT,-,-,-,UNKNOWN,OPAQUE,ANOTHER,08,02  #  19 ||< ptn_cor_b:l2_cell_c
*surf,Wall-20,VERT,-,-,-,UNKNOWN,OPAQUE,ANOTHER,08,01  #  20 ||< ptn_cor_a:l2_cell_c
*surf,Wall-21,VERT,-,-,-,UNKNOWN,OPAQUE,ANOTHER,16,03  #  21 ||< ptn_off_l2:sw_stair
*surf,Wall-22,VERT,-,-,-,UNKNOWN,OPAQUE,UNKNOWN,0,0  #  22 ||< not yet defined
*surf,Wall-23,VERT,-,-,-,UNKNOWN,OPAQUE,UNKNOWN,0,0  #  23 ||< not yet defined
*surf,Wall-24,VERT,-,-,-,UNKNOWN,OPAQUE,ANOTHER,07,06  #  24 ||< Wall-6:L2_cash
*surf,Wall-25,VERT,-,-,-,UNKNOWN,OPAQUE,ANOTHER,07,05  #  25 ||< Wall-5:L2_cash
*surf,Wall-26,VERT,-,-,-,UNKNOWN,OPAQUE,UNKNOWN,0,0  #  26 ||< not yet defined
*surf,ceil_l2_cor,CEIL,-,-,-,UNKNOWN,OPAQUE,UNKNOWN,0,0  #  27 ||< not yet defined
*surf,floor_l2cor,FLOR,-,-,-,UNKNOWN,OPAQUE,UNKNOWN,0,0  #  28 ||< not yet defined
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,28,    81.83 0  # zone base list
