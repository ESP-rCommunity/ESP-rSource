*Geometry 1.1,GEN,L5_atroofltl # tag version, format, zone name
*date Wed Aug 17 06:47:37 2011  # latest file modification 
L5_atroofltl describes the main roof light over atria
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,4.00000,3.75000,64.15000  #   1
*vertex,6.50000,4.25000,64.15000  #   2
*vertex,9.75000,5.00000,64.15000  #   3
*vertex,13.00000,5.75000,64.15000  #   4
*vertex,16.25000,6.50000,64.15000  #   5
*vertex,19.25000,7.25000,64.15000  #   6
*vertex,23.50000,8.25000,64.15000  #   7
*vertex,23.50000,11.25000,64.15000  #   8
*vertex,19.25000,10.25000,64.15000  #   9
*vertex,16.25000,9.50000,64.15000  #  10
*vertex,13.00000,8.75000,64.15000  #  11
*vertex,9.75000,8.00000,64.15000  #  12
*vertex,6.50000,7.25000,64.15000  #  13
*vertex,4.00000,6.50000,64.15000  #  14
*vertex,4.00000,3.75000,65.15000  #  15
*vertex,6.50000,4.25000,65.15000  #  16
*vertex,9.75000,5.00000,65.15000  #  17
*vertex,13.00000,5.75000,65.15000  #  18
*vertex,16.25000,6.50000,65.15000  #  19
*vertex,19.25000,7.25000,65.15000  #  20
*vertex,23.50000,8.25000,65.15000  #  21
*vertex,23.50000,11.25000,66.15000  #  22
*vertex,19.25000,10.25000,66.15000  #  23
*vertex,16.25000,9.50000,66.15000  #  24
*vertex,13.00000,8.75000,66.15000  #  25
*vertex,9.75000,8.00000,66.15000  #  26
*vertex,6.50000,7.25000,66.15000  #  27
*vertex,4.00000,6.50000,66.15000  #  28
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,16,15  #  1
*edges,4,2,3,17,16  #  2
*edges,4,3,4,18,17  #  3
*edges,4,4,5,19,18  #  4
*edges,4,5,6,20,19  #  5
*edges,4,6,7,21,20  #  6
*edges,4,7,8,22,21  #  7
*edges,4,8,9,23,22  #  8
*edges,4,9,10,24,23  #  9
*edges,4,10,11,25,24  # 10
*edges,4,11,12,26,25  # 11
*edges,4,12,13,27,26  # 12
*edges,4,13,14,28,27  # 13
*edges,4,14,1,15,28  # 14
*edges,14,1,14,13,12,11,10,9,8,7,6,5,4,3,2  # 15
*edges,3,15,16,28  # 16
*edges,3,27,28,16  # 17
*edges,3,16,17,27  # 18
*edges,3,26,27,17  # 19
*edges,3,17,18,26  # 20
*edges,3,25,26,18  # 21
*edges,3,18,19,25  # 22
*edges,3,24,25,19  # 23
*edges,3,19,20,24  # 24
*edges,3,23,24,20  # 25
*edges,3,20,21,23  # 26
*edges,3,22,23,21  # 27
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,low_wall_a,VERT,-,-,-,stone_masnry,OPAQUE,EXTERIOR,00,00  #   1 ||< external
*surf,low_wall_b,VERT,-,-,-,stone_masnry,OPAQUE,EXTERIOR,00,00  #   2 ||< external
*surf,low_wall_c,VERT,-,-,-,stone_masnry,OPAQUE,EXTERIOR,00,00  #   3 ||< external
*surf,low_wall_d,VERT,-,-,-,stone_masnry,OPAQUE,EXTERIOR,00,00  #   4 ||< external
*surf,low_wall_e,VERT,-,-,-,stone_masnry,OPAQUE,EXTERIOR,00,00  #   5 ||< external
*surf,low_wall_f,VERT,-,-,-,stone_masnry,OPAQUE,EXTERIOR,00,00  #   6 ||< external
*surf,right,VERT,-,-,-,stone_masnry,OPAQUE,EXTERIOR,00,00  #   7 ||< external
*surf,back_wall_a,VERT,-,-,-,stone_masnry,OPAQUE,EXTERIOR,00,00  #   8 ||< external
*surf,back_wall_b,VERT,-,-,-,stone_masnry,OPAQUE,EXTERIOR,00,00  #   9 ||< external
*surf,back_wall_d,VERT,-,-,-,stone_masnry,OPAQUE,EXTERIOR,00,00  #  10 ||< external
*surf,back_wall_e,VERT,-,-,-,stone_masnry,OPAQUE,EXTERIOR,00,00  #  11 ||< external
*surf,back_wall_f,VERT,-,-,-,stone_masnry,OPAQUE,EXTERIOR,00,00  #  12 ||< external
*surf,back_wall_g,VERT,-,-,-,stone_masnry,OPAQUE,EXTERIOR,00,00  #  13 ||< external
*surf,left,VERT,-,-,-,stone_masnry,OPAQUE,EXTERIOR,00,00  #  14 ||< external
*surf,fict_base,FLOR,-,-,-,fictitious,SC_fictit,UNKNOWN,0,0  #  15 ||< not yet defined
*surf,roof_a,SLOP,-,-,-,db_lpasol,DSF4554_06nb,UNKNOWN,00,00  #  16 ||< not yet defined
*surf,roof_b,SLOP,-,-,-,db_lpasol,DSF4554_06nb,EXTERIOR,00,00  #  17 ||< external
*surf,roof_c,SLOP,-,-,-,db_lpasol,DSF4554_06nb,EXTERIOR,00,00  #  18 ||< external
*surf,roof_e,SLOP,-,-,-,db_lpasol,DSF4554_06nb,EXTERIOR,00,00  #  19 ||< external
*surf,roof_f,SLOP,-,-,-,db_lpasol,DSF4554_06nb,EXTERIOR,00,00  #  20 ||< external
*surf,roof_g,SLOP,-,-,-,db_lpasol,DSF4554_06nb,EXTERIOR,00,00  #  21 ||< external
*surf,roof_h,SLOP,-,-,-,db_lpasol,DSF4554_06nb,EXTERIOR,00,00  #  22 ||< external
*surf,roof_i,SLOP,-,-,-,db_lpasol,DSF4554_06nb,EXTERIOR,00,00  #  23 ||< external
*surf,roof_j,SLOP,-,-,-,db_lpasol,DSF4554_06nb,EXTERIOR,00,00  #  24 ||< external
*surf,roof_l,SLOP,-,-,-,db_lpasol,DSF4554_06nb,EXTERIOR,00,00  #  25 ||< external
*surf,roof_m,SLOP,-,-,-,db_lpasol,DSF4554_06nb,EXTERIOR,00,00  #  26 ||< external
*surf,roof_n,SLOP,-,-,-,db_lpasol,DSF4554_06nb,UNKNOWN,00,00  #  27 ||< not yet defined
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,15,    58.19 0  # zone base list
