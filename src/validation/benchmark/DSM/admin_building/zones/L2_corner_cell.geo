*Geometry 1.1,GEN,L2_cor_cell # tag version, format, zone name
*date Thu Jul 14 19:35:02 2011  # latest file modification 
L2_cor_cell describes cellular offices at corner of level two
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,44.69636,35.70000,52.45000  #   1
*vertex,48.02128,35.70000,52.45000  #   2
*vertex,51.00000,36.00000,52.45000  #   3
*vertex,51.00000,40.70000,52.45000  #   4
*vertex,49.99200,40.70000,52.45000  #   5
*vertex,47.99200,40.70000,52.45000  #   6
*vertex,46.99200,40.70000,52.45000  #   7
*vertex,45.99200,40.70000,52.45000  #   8
*vertex,44.69600,40.69985,52.45000  #   9
*vertex,44.69636,35.70000,55.59300  #  10
*vertex,48.02128,35.70000,55.59300  #  11
*vertex,51.00000,36.00000,55.59300  #  12
*vertex,51.00000,40.70000,55.59300  #  13
*vertex,49.99200,40.70000,55.59300  #  14
*vertex,47.99200,40.70000,55.59300  #  15
*vertex,46.99200,40.70000,55.59300  #  16
*vertex,45.99200,40.70000,55.59300  #  17
*vertex,44.74386,40.70000,55.59300  #  18
*vertex,50.00000,36.00000,52.45000  #  19
*vertex,50.00000,36.00000,55.59300  #  20
*vertex,50.00000,35.70000,52.45000  #  21
*vertex,50.00000,35.70000,55.59300  #  22
*vertex,49.99200,40.70000,53.56000  #  23
*vertex,47.99200,40.70000,53.56000  #  24
*vertex,46.99200,40.70000,53.56000  #  25
*vertex,45.99200,40.70000,53.56000  #  26
*vertex,49.99200,40.70000,54.70000  #  27
*vertex,47.99200,40.70000,54.70000  #  28
*vertex,46.99200,40.70000,54.70000  #  29
*vertex,45.99200,40.70000,54.70000  #  30
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,11,10  #  1
*edges,4,2,21,22,11  #  2
*edges,4,3,4,13,12  #  3
*edges,6,4,5,23,27,14,13  #  4
*edges,4,23,24,28,27  #  5
*edges,8,6,7,25,29,16,15,28,24  #  6
*edges,4,25,26,30,29  #  7
*edges,6,8,9,18,17,30,26  #  8
*edges,4,9,1,10,18  #  9
*edges,11,10,11,22,20,12,13,14,15,16,17,18  # 10
*edges,11,1,9,8,7,6,5,4,3,19,21,2  # 11
*edges,4,19,3,12,20  # 12
*edges,4,21,19,20,22  # 13
*edges,4,5,6,24,23  # 14
*edges,4,27,28,15,14  # 15
*edges,4,7,8,26,25  # 16
*edges,4,29,30,17,16  # 17
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,ptn_cora,VERT,-,-,-,UNKNOWN,OPAQUE,ANOTHER,06,10  #   1 ||< Wall-10:L2_corid_a
*surf,ptn_corb,VERT,-,-,-,UNKNOWN,OPAQUE,ANOTHER,06,09  #   2 ||< Wall-9:L2_corid_a
*surf,ptn_stair_a,VERT,-,-,-,UNKNOWN,OPAQUE,UNKNOWN,00,00  #   3 ||< not yet defined
*surf,stone_g,VERT,-,-,-,UNKNOWN,OPAQUE,EXTERIOR,0,0  #   4 ||< external
*surf,glaz_g,VERT,-,-,-,UNKNOWN,OPAQUE,EXTERIOR,0,0  #   5 ||< external
*surf,vent_g,VERT,-,-,-,UNKNOWN,OPAQUE,EXTERIOR,0,0  #   6 ||< external
*surf,glaz_f,VERT,-,-,-,UNKNOWN,OPAQUE,EXTERIOR,0,0  #   7 ||< external
*surf,stone_f,VERT,-,-,-,UNKNOWN,OPAQUE,EXTERIOR,0,0  #   8 ||< external
*surf,Wall-11,VERT,-,-,-,UNKNOWN,OPAQUE,UNKNOWN,0,0  #   9 ||< not yet defined
*surf,cel319_ceil,CEIL,-,-,-,UNKNOWN,OPAQUE,UNKNOWN,0,0  #  10 ||< not yet defined
*surf,cel319_floor,FLOR,-,-,-,UNKNOWN,OPAQUE,UNKNOWN,0,0  #  11 ||< not yet defined
*surf,struc_l2,VERT,-,-,-,UNKNOWN,OPAQUE,ANOTHER,11,04  #  12 ||< struc_l2:stair_twr_n
*surf,filler,VERT,-,-,-,UNKNOWN,OPAQUE,UNKNOWN,0,0  #  13 ||< not yet defined
*surf,glaz_g_low,VERT,-,-,-,UNKNOWN,OPAQUE,EXTERIOR,00,00  #  14 ||< external
*surf,glaz_g_hi,VERT,-,-,-,UNKNOWN,OPAQUE,EXTERIOR,00,00  #  15 ||< external
*surf,glaz_f_low,VERT,-,-,-,UNKNOWN,OPAQUE,EXTERIOR,00,00  #  16 ||< external
*surf,glaz_f_hi,VERT,-,-,-,UNKNOWN,OPAQUE,EXTERIOR,00,00  #  17 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,11,    31.22 0  # zone base list
