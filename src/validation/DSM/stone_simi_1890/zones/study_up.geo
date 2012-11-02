*Geometry 1.1,GEN,study # tag version, format, zone name
*date Mon Apr 23 16:58:57 2012  # latest file modification 
study describes small north facing room
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,7.60000,14.00000,0.00000  #   1
*vertex,9.40000,14.00000,0.00000  #   2
*vertex,9.40000,17.20000,0.00000  #   3
*vertex,9.00000,17.20000,0.00000  #   4
*vertex,8.89110,17.60000,0.00000  #   5
*vertex,8.09154,17.60000,0.00000  #   6
*vertex,8.00000,17.20000,0.00000  #   7
*vertex,7.60000,17.20000,0.00000  #   8
*vertex,7.60000,14.00000,3.20000  #   9
*vertex,9.40000,14.00000,3.20000  #  10
*vertex,9.40000,17.20000,3.20000  #  11
*vertex,9.00000,17.20000,3.20000  #  12
*vertex,8.89110,17.60000,3.20000  #  13
*vertex,8.09154,17.60000,3.20000  #  14
*vertex,8.00000,17.20000,3.20000  #  15
*vertex,7.60000,17.20000,3.20000  #  16
*vertex,9.40000,17.20000,0.20000  #  17
*vertex,9.00000,17.20000,0.20000  #  18
*vertex,8.89110,17.60000,0.20000  #  19
*vertex,8.09154,17.60000,0.20000  #  20
*vertex,8.00000,17.20000,0.20000  #  21
*vertex,7.60000,17.20000,0.20000  #  22
*vertex,8.89110,17.60000,0.70000  #  23
*vertex,8.09154,17.60000,0.70000  #  24
*vertex,8.89110,17.60000,3.10000  #  25
*vertex,8.09154,17.60000,3.10000  #  26
*vertex,8.83754,17.60000,0.86077  #  27
*vertex,8.14510,17.60000,0.86077  #  28
*vertex,8.14510,17.60000,2.93923  #  29
*vertex,8.83754,17.60000,2.93923  #  30
*vertex,8.10000,14.00000,0.00000  #  31
*vertex,8.90000,14.00000,0.00000  #  32
*vertex,8.90000,14.00000,2.10000  #  33
*vertex,8.10000,14.00000,2.10000  #  34
# 
# tag, number of vertices followed by list of associated vert
*edges,8,1,31,34,33,32,2,10,9  #  1
*edges,5,2,3,17,11,10  #  2
*edges,4,17,18,12,11  #  3
*edges,6,18,19,23,25,13,12  #  4
*edges,4,19,20,24,23  #  5
*edges,6,20,21,15,14,26,24  #  6
*edges,4,21,22,16,15  #  7
*edges,5,8,1,9,16,22  #  8
*edges,8,9,10,11,12,13,14,15,16  #  9
*edges,10,1,8,7,6,5,4,3,2,32,31  # 10
*edges,4,3,4,18,17  # 11
*edges,4,4,5,19,18  # 12
*edges,4,5,6,20,19  # 13
*edges,4,6,7,21,20  # 14
*edges,4,7,8,22,21  # 15
*edges,10,23,24,26,25,23,27,30,29,28,27  # 16
*edges,4,25,26,14,13  # 17
*edges,4,27,28,29,30  # 18
*edges,4,31,32,33,34  # 19
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,sptn_hall,VERT,-,-,-,door,OPAQUE,ANOTHER,07,09  #   1 ||< hptn_study:hall
*surf,sptn_liv,VERT,-,-,-,pla_pla_ptn,OPAQUE,ANOTHER,06,11  #   2 ||< ptn_study:living
*surf,north_a,VERT,-,-,-,stone_0.7m,OPAQUE,EXTERIOR,0,0  #   3 ||< external
*surf,angle_b,VERT,-,-,-,stone_0.4m,OPAQUE,EXTERIOR,0,0  #   4 ||< external
*surf,sill_c,VERT,-,-,-,stone_0.4m,OPAQUE,EXTERIOR,0,0  #   5 ||< external
*surf,angle_d,VERT,-,-,-,stone_0.4m,OPAQUE,EXTERIOR,0,0  #   6 ||< external
*surf,north_e,VERT,-,-,-,stone_0.7m,OPAQUE,EXTERIOR,0,0  #   7 ||< external
*surf,sptn_fam,VERT,-,-,-,pla_pla_ptn,OPAQUE,ANOTHER,09,04  #   8 ||< fptn_stdy:family
*surf,ceiling_std,CEIL,-,-,-,ceil_to_roof,OPAQUE,ANOTHER,13,03  #   9 ||< ceiling_std:upper_ne
*surf,floor_std,FLOR,-,-,-,floor_to_cli,OPAQUE,ANOTHER,11,02  #  10 ||< floor_std:lower_ne
*surf,skirt_a,VERT,-,-,-,stone_0.7m,OPAQUE,EXTERIOR,0,0  #  11 ||< external
*surf,skirt_b,VERT,-,-,-,stone_0.4m,OPAQUE,EXTERIOR,0,0  #  12 ||< external
*surf,skirt_c,VERT,-,-,-,stone_0.4m,OPAQUE,EXTERIOR,0,0  #  13 ||< external
*surf,skirt_d,VERT,-,-,-,stone_0.4m,OPAQUE,EXTERIOR,0,0  #  14 ||< external
*surf,skirt_e,VERT,-,-,-,stone_0.7m,OPAQUE,EXTERIOR,0,0  #  15 ||< external
*surf,frame_c,VERT,-,-,-,sash_frame,OPAQUE,EXTERIOR,0,0  #  16 ||< external
*surf,head_c,VERT,-,-,-,stone_0.4m,OPAQUE,EXTERIOR,0,0  #  17 ||< external
*surf,glass_c,VERT,frame_c,-,-,Window_ext,TRAN,EXTERIOR,0,0  #  18 ||< external
*surf,door_st,VERT,sptn_hall,-,-,door,OPAQUE,ANOTHER,07,24  #  19 ||< door_st:hall
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,all_applicable   1 # insolation sources
 18
# 
*base_list,1,10,     6.12 0  # zone base list
