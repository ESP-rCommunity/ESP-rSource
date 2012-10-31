*Geometry 1.1,GEN,bedroom2 # tag version, format, zone name
*date Mon Apr 23 16:58:51 2012  # latest file modification 
bedroom2 describes east facing bedroom
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,11.80000,9.20000,0.00000  #   1
*vertex,15.20000,9.20000,0.00000  #   2
*vertex,15.20000,10.20000,0.00000  #   3
*vertex,15.60000,10.40000,0.00000  #   4
*vertex,15.60000,11.30000,0.00000  #   5
*vertex,15.20000,11.50000,0.00000  #   6
*vertex,15.20000,13.00000,0.00000  #   7
*vertex,10.60000,13.00000,0.00000  #   8
*vertex,10.60000,9.20000,0.00000  #   9
*vertex,11.80000,9.20000,3.20000  #  10
*vertex,15.20000,9.20000,3.20000  #  11
*vertex,15.20000,10.20000,3.20000  #  12
*vertex,15.60000,10.40000,3.20000  #  13
*vertex,15.60000,11.30000,3.20000  #  14
*vertex,15.20000,11.50000,3.20000  #  15
*vertex,15.20000,13.00000,3.20000  #  16
*vertex,10.60000,13.00000,3.20000  #  17
*vertex,10.60000,9.20000,3.20000  #  18
*vertex,15.20000,9.20000,0.20000  #  19
*vertex,15.20000,10.20000,0.20000  #  20
*vertex,15.60000,10.40000,0.20000  #  21
*vertex,15.60000,11.30000,0.20000  #  22
*vertex,15.20000,11.50000,0.20000  #  23
*vertex,15.20000,13.00000,0.20000  #  24
*vertex,15.20000,10.20000,0.70000  #  25
*vertex,15.60000,10.40000,0.70000  #  26
*vertex,15.60000,11.30000,0.70000  #  27
*vertex,15.20000,11.50000,0.70000  #  28
*vertex,15.60000,10.40000,3.10000  #  29
*vertex,15.60000,11.30000,3.10000  #  30
*vertex,15.20000,11.50000,3.10000  #  31
*vertex,15.60000,10.46029,0.86077  #  32
*vertex,15.60000,11.23971,0.86077  #  33
*vertex,15.60000,11.23971,2.93923  #  34
*vertex,15.60000,10.46029,2.93923  #  35
*vertex,10.60000,10.40000,0.00000  #  36
*vertex,10.60000,9.50000,0.00000  #  37
*vertex,10.60000,9.50000,2.10000  #  38
*vertex,10.60000,10.40000,2.10000  #  39
# 
# tag, number of vertices followed by list of associated vert
*edges,5,1,2,19,11,10  #  1
*edges,5,19,20,25,12,11  #  2
*edges,4,3,4,21,20  #  3
*edges,4,4,5,22,21  #  4
*edges,4,5,6,23,22  #  5
*edges,6,23,24,16,15,31,28  #  6
*edges,5,7,8,17,16,24  #  7
*edges,8,8,36,39,38,37,9,18,17  #  8
*edges,4,9,1,10,18  #  9
*edges,9,10,11,12,13,14,15,16,17,18  # 10
*edges,11,1,9,37,36,8,7,6,5,4,3,2  # 11
*edges,4,2,3,20,19  # 12
*edges,4,6,7,24,23  # 13
*edges,7,20,21,26,29,13,12,25  # 14
*edges,4,21,22,27,26  # 15
*edges,10,26,27,30,29,26,32,35,34,33,32  # 16
*edges,4,29,30,14,13  # 17
*edges,8,22,23,28,31,15,14,30,27  # 18
*edges,4,32,33,34,35  # 19
*edges,4,36,37,38,39  # 20
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,ptn_bath,VERT,-,-,-,pla_pla_ptn,OPAQUE,ANOTHER,04,06  #   1 ||< ptn_br2:bath
*surf,east_a,VERT,-,-,-,stone_0.7m,OPAQUE,EXTERIOR,0,0  #   2 ||< external
*surf,skirt_b,VERT,-,-,-,stone_0.4m,OPAQUE,EXTERIOR,0,0  #   3 ||< external
*surf,skirt_c,VERT,-,-,-,stone_0.4m,OPAQUE,EXTERIOR,0,0  #   4 ||< external
*surf,skirt_d,VERT,-,-,-,stone_0.4m,OPAQUE,EXTERIOR,0,0  #   5 ||< external
*surf,east_e,VERT,-,-,-,stone_0.7m,OPAQUE,EXTERIOR,0,0  #   6 ||< external
*surf,ptn_liv,VERT,-,-,-,pla_pla_ptn,OPAQUE,ANOTHER,06,01  #   7 ||< ptn_br2:living
*surf,ptn_hall,VERT,-,-,-,door,OPAQUE,ANOTHER,07,06  #   8 ||< hptn_br2b:hall
*surf,ptn_hallb,VERT,-,-,-,pla_pla_ptn,OPAQUE,ANOTHER,07,05  #   9 ||< hptn_br2a:hall
*surf,ceiling_b2,CEIL,-,-,-,ceil_to_roof,OPAQUE,ANOTHER,13,01  #  10 ||< ceiling_b2:upper_ne
*surf,floor_b2,FLOR,-,-,-,floor_to_cli,OPAQUE,ANOTHER,11,04  #  11 ||< floor_b2:lower_ne
*surf,skirt_a,VERT,-,-,-,stone_0.7m,OPAQUE,EXTERIOR,0,0  #  12 ||< external
*surf,skirt_e,VERT,-,-,-,stone_0.7m,OPAQUE,EXTERIOR,0,0  #  13 ||< external
*surf,angl_b,VERT,-,-,-,stone_0.4m,OPAQUE,EXTERIOR,0,0  #  14 ||< external
*surf,sill_c,VERT,-,-,-,stone_0.4m,OPAQUE,EXTERIOR,0,0  #  15 ||< external
*surf,frame_c,VERT,-,-,-,sash_frame,OPAQUE,EXTERIOR,0,0  #  16 ||< external
*surf,head_c,VERT,-,-,-,stone_0.4m,OPAQUE,EXTERIOR,0,0  #  17 ||< external
*surf,angle_d,VERT,-,-,-,stone_0.4m,OPAQUE,EXTERIOR,0,0  #  18 ||< external
*surf,glass_c,VERT,frame_c,-,-,Window_ext,TRAN,EXTERIOR,0,0  #  19 ||< external
*surf,door_br2,VERT,ptn_hall,-,-,door,OPAQUE,ANOTHER,07,28  #  20 ||< door_br2:hall
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,all_applicable   1 # insolation sources
 19
# 
*base_list,1,11,    17.92 0  # zone base list
