*Geometry 1.1,GEN,L5_light_w1 # tag version, format, zone name
*date Thu Jul 14 19:55:44 2011  # latest file modification 
L5_light_w1 describes light well one at level 5
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,13.70000,21.30000,64.15000  #   1
*vertex,17.45000,21.30000,64.15000  #   2
*vertex,17.45000,28.50000,64.15000  #   3
*vertex,13.70000,28.50000,64.15000  #   4
*vertex,13.70000,21.30000,67.37500  #   5
*vertex,17.45000,21.30000,67.37500  #   6
*vertex,17.45000,28.50000,67.37500  #   7
*vertex,13.70000,28.50000,67.37500  #   8
*vertex,13.70000,21.30000,65.15000  #   9
*vertex,17.45000,21.30000,65.15000  #  10
*vertex,17.45000,28.50000,65.15000  #  11
*vertex,13.70000,28.50000,65.15000  #  12
*vertex,13.70000,21.30000,63.39300  #  13
*vertex,17.45000,21.30000,63.39300  #  14
*vertex,17.45000,28.50000,63.39300  #  15
*vertex,13.70000,28.50000,63.39300  #  16
*vertex,13.70000,21.30000,68.10000  #  17
*vertex,17.45000,21.30000,68.10000  #  18
*vertex,17.45000,28.50000,68.10000  #  19
*vertex,13.70000,28.50000,68.10000  #  20
*vertex,13.70000,21.30000,69.10000  #  21
*vertex,13.70000,28.50000,69.10000  #  22
*vertex,17.45000,21.30000,70.10000  #  23
*vertex,17.45000,28.50000,70.10000  #  24
# 
# tag, number of vertices followed by list of associated vert
*edges,4,10,6,5,9  #  1
*edges,4,11,7,6,10  #  2
*edges,6,3,4,12,8,7,11  #  3
*edges,4,9,5,8,12  #  4
*edges,4,21,23,24,22  #  5
*edges,4,13,16,15,14  #  6
*edges,4,4,1,9,12  #  7
*edges,4,1,2,10,9  #  8
*edges,4,2,3,11,10  #  9
*edges,4,14,15,3,2  # 10
*edges,4,13,14,2,1  # 11
*edges,4,16,13,1,4  # 12
*edges,4,15,16,4,3  # 13
*edges,4,5,6,18,17  # 14
*edges,4,17,18,23,21  # 15
*edges,4,6,7,19,18  # 16
*edges,4,18,19,24,23  # 17
*edges,4,7,8,20,19  # 18
*edges,4,19,20,22,24  # 19
*edges,4,8,5,17,20  # 20
*edges,4,20,17,21,22  # 21
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,l3_lw2_fr,VERT,-,-,-,UNKNOWN,OPAQUE,UNKNOWN,0,0  #   1 ||< not yet defined
*surf,l3_lw2_ri,VERT,-,-,-,UNKNOWN,OPAQUE,UNKNOWN,0,0  #   2 ||< not yet defined
*surf,l3_lw2_bk,VERT,-,-,-,UNKNOWN,OPAQUE,UNKNOWN,0,0  #   3 ||< not yet defined
*surf,l3_lw2_lef,VERT,-,-,-,UNKNOWN,OPAQUE,UNKNOWN,0,0  #   4 ||< not yet defined
*surf,up_l3_lw2,SLOP,-,-,-,UNKNOWN,OPAQUE,EXTERIOR,0,0  #   5 ||< external
*surf,l3_lw2_base,FLOR,-,-,-,UNKNOWN,OPAQUE,ANOTHER,51,05  #   6 ||< up_l3_lw2:L4_lw1
*surf,left_rail,VERT,-,-,-,UNKNOWN,OPAQUE,UNKNOWN,0,0  #   7 ||< not yet defined
*surf,front_rail,VERT,-,-,-,UNKNOWN,OPAQUE,UNKNOWN,0,0  #   8 ||< not yet defined
*surf,right_rail,VERT,-,-,-,UNKNOWN,OPAQUE,UNKNOWN,0,0  #   9 ||< not yet defined
*surf,l3_flrv_r,VERT,-,-,-,UNKNOWN,OPAQUE,UNKNOWN,0,0  #  10 ||< not yet defined
*surf,l3_flrv_f,VERT,-,-,-,UNKNOWN,OPAQUE,UNKNOWN,0,0  #  11 ||< not yet defined
*surf,l3_flrv_l,VERT,-,-,-,UNKNOWN,OPAQUE,UNKNOWN,0,0  #  12 ||< not yet defined
*surf,l3_flrv_b,VERT,-,-,-,UNKNOWN,OPAQUE,UNKNOWN,0,0  #  13 ||< not yet defined
*surf,parap_f,VERT,-,-,-,UNKNOWN,OPAQUE,EXTERIOR,0,0  #  14 ||< external
*surf,glaz_fr,VERT,-,-,-,UNKNOWN,OPAQUE,EXTERIOR,0,0  #  15 ||< external
*surf,parap_ri,VERT,-,-,-,UNKNOWN,OPAQUE,EXTERIOR,0,0  #  16 ||< external
*surf,glaz_ri,VERT,-,-,-,UNKNOWN,OPAQUE,EXTERIOR,0,0  #  17 ||< external
*surf,parap_bk,VERT,-,-,-,UNKNOWN,OPAQUE,EXTERIOR,0,0  #  18 ||< external
*surf,glaz_bk,VERT,-,-,-,UNKNOWN,OPAQUE,EXTERIOR,0,0  #  19 ||< external
*surf,parap_left,VERT,-,-,-,UNKNOWN,OPAQUE,EXTERIOR,0,0  #  20 ||< external
*surf,glaz_left,VERT,-,-,-,UNKNOWN,OPAQUE,EXTERIOR,0,0  #  21 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,6,    27.00 0  # zone base list
