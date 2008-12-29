*Geometry 1.1,GEN,roof # tag version, format, zone name
*date Thu Aug 23 18:32:19 2007  # latest file modification 
roof describes a...
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,10.00000,0.00000,6.50000  #   1
*vertex,34.00000,0.00000,6.50000  #   2
*vertex,34.00000,9.50000,6.50000  #   3
*vertex,34.00000,14.50000,6.50000  #   4
*vertex,34.00000,36.00000,6.50000  #   5
*vertex,34.00000,45.50000,6.50000  #   6
*vertex,34.00000,50.50000,6.50000  #   7
*vertex,34.00000,60.00000,6.50000  #   8
*vertex,10.00000,60.00000,6.50000  #   9
*vertex,10.00000,48.00000,6.50000  #  10
*vertex,10.00000,46.00000,6.50000  #  11
*vertex,10.00000,38.00000,6.50000  #  12
*vertex,10.00000,36.00000,6.50000  #  13
*vertex,10.00000,12.00000,6.50000  #  14
*vertex,22.00000,12.00000,10.00000  #  15
*vertex,22.00000,48.00000,10.00000  #  16
*vertex,12.00000,12.00000,6.50000  #  17
*vertex,29.00000,12.00000,6.50000  #  18
*vertex,29.00000,9.50000,6.50000  #  19
*vertex,29.00000,14.50000,6.50000  #  20
*vertex,12.00000,36.00000,6.50000  #  21
*vertex,16.00000,36.00000,6.50000  #  22
*vertex,29.50000,45.50000,6.50000  #  23
*vertex,29.50000,48.00000,6.50000  #  24
*vertex,29.50000,50.50000,6.50000  #  25
*vertex,16.00000,48.00000,6.50000  #  26
*vertex,12.00000,48.00000,6.50000  #  27
*vertex,7.28300,42.13900,6.50000  #  28
*vertex,7.00000,48.00000,6.50000  #  29
*vertex,7.00000,36.00000,6.50000  #  30
*vertex,16.00000,42.00000,9.00000  #  31
# 
# tag, number of vertices followed by list of associated vert
*edges,3,1,2,15  #  1
*edges,9,2,3,4,5,6,7,8,16,15  #  2
*edges,3,8,9,16  #  3
*edges,8,9,10,31,13,14,1,15,16  #  4
*edges,7,21,22,5,4,20,18,17  #  5
*edges,7,1,14,17,18,19,3,2  #  6
*edges,5,4,3,19,18,20  #  7
*edges,4,21,27,26,22  #  8
*edges,8,10,9,8,7,25,24,26,27  #  9
*edges,5,23,24,25,7,6  # 10
*edges,9,14,13,12,28,11,10,27,21,17  # 11
*edges,7,30,29,10,11,28,12,13  # 12
*edges,3,10,29,31  # 13
*edges,3,29,30,31  # 14
*edges,3,30,13,31  # 15
*edges,6,22,26,24,23,6,5  # 16
# 
# surf attributes:
#  surf name, surf position VERT/CIIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,south,SLOP,-,-,-,roof,OPAQUE,EXTERIOR,0,0  #   1 ||< external
*surf,east,SLOP,-,-,-,roof,OPAQUE,EXTERIOR,0,0  #   2 ||< external
*surf,north,SLOP,-,-,-,roof,OPAQUE,EXTERIOR,0,0  #   3 ||< external
*surf,west,SLOP,-,-,-,roof,OPAQUE,EXTERIOR,0,0  #   4 ||< external
*surf,ceil_c,FLOR,-,-,-,susp_ceil,OPAQUE,ANOTHER,10,08  #   5 ||< ceil:Unit_cd
*surf,ceil_e,FLOR,-,-,-,susp_ceil,OPAQUE,ANOTHER,08,08  #   6 ||< ceil_e:Unit_e
*surf,stair_de,FLOR,-,-,-,susp_ceil,OPAQUE,ANOTHER,15,06  #   7 ||< ceil:stair_deij
*surf,ceil_t,FLOR,-,-,-,susp_ceil,OPAQUE,ANOTHER,16,05  #   8 ||< ceiling:toilets
*surf,ceil_a,FLOR,-,-,-,susp_ceil,OPAQUE,ANOTHER,06,08  #   9 ||< ceil_a:Unit_a
*surf,stair_ab,FLOR,-,-,-,susp_ceil,OPAQUE,ANOTHER,14,06  #  10 ||< ceil:stair_abfg
*surf,corid_c,FLOR,-,-,-,susp_ceil,OPAQUE,ANOTHER,01,09  #  11 ||< ceil:corid_1
*surf,sofit,FLOR,-,-,-,insul_mtl_p,OPAQUE,EXTERIOR,0,0  #  12 ||< external
*surf,ent_r_n,SLOP,-,-,-,roof,OPAQUE,EXTERIOR,0,0  #  13 ||< external
*surf,ent_r_w,SLOP,-,-,-,roof,OPAQUE,EXTERIOR,0,0  #  14 ||< external
*surf,ent_r_s,SLOP,-,-,-,roof,OPAQUE,EXTERIOR,0,0  #  15 ||< external
*surf,ceil_b,FLOR,-,-,-,susp_ceil,OPAQUE,ANOTHER,07,07  #  16 ||< ceil_b:Unit_b
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,0,1163.00,1  # zone base
