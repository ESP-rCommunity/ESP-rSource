*Geometry 1.1,GEN,family # tag version, format, zone name
*date Mon Apr 23 16:58:59 2012  # latest file modification 
family describes family room on the north
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,4.00000,12.60000,0.00000  #   1
*vertex,7.00000,12.60000,0.00000  #   2
*vertex,7.60000,12.60000,0.00000  #   3
*vertex,7.60000,14.00000,0.00000  #   4
*vertex,7.60000,17.20000,0.00000  #   5
*vertex,6.60000,17.20000,0.00000  #   6
*vertex,6.40000,17.60000,0.00000  #   7
*vertex,4.80000,17.60000,0.00000  #   8
*vertex,4.60000,17.20000,0.00000  #   9
*vertex,4.00000,17.20000,0.00000  #  10
*vertex,4.00000,12.60000,3.20000  #  11
*vertex,7.00000,12.60000,3.20000  #  12
*vertex,7.60000,12.60000,3.20000  #  13
*vertex,7.60000,14.00000,3.20000  #  14
*vertex,7.60000,17.20000,3.20000  #  15
*vertex,6.60000,17.20000,3.20000  #  16
*vertex,6.40000,17.60000,3.20000  #  17
*vertex,4.80000,17.60000,3.20000  #  18
*vertex,4.60000,17.20000,3.20000  #  19
*vertex,4.00000,17.20000,3.20000  #  20
*vertex,4.00000,12.60000,0.20000  #  21
*vertex,7.60000,17.20000,0.20000  #  22
*vertex,6.60000,17.20000,0.20000  #  23
*vertex,6.40000,17.60000,0.20000  #  24
*vertex,4.80000,17.60000,0.20000  #  25
*vertex,4.60000,17.20000,0.20000  #  26
*vertex,4.00000,17.20000,0.20000  #  27
*vertex,7.00000,12.60000,0.20000  #  28
*vertex,7.60000,12.60000,0.20000  #  29
*vertex,6.40000,17.60000,0.70000  #  30
*vertex,4.80000,17.60000,0.70000  #  31
*vertex,6.40000,17.60000,3.10000  #  32
*vertex,4.80000,17.60000,3.10000  #  33
*vertex,6.29282,17.60000,0.86077  #  34
*vertex,4.90718,17.60000,0.86077  #  35
*vertex,4.90718,17.60000,2.93923  #  36
*vertex,6.29282,17.60000,2.93923  #  37
*vertex,7.60000,12.80000,0.00000  #  38
*vertex,7.60000,13.70000,0.00000  #  39
*vertex,7.60000,13.70000,2.10000  #  40
*vertex,7.60000,12.80000,2.10000  #  41
# 
# tag, number of vertices followed by list of associated vert
*edges,4,21,28,12,11  #  1
*edges,4,28,29,13,12  #  2
*edges,9,3,38,41,40,39,4,14,13,29  #  3
*edges,5,4,5,22,15,14  #  4
*edges,4,22,23,16,15  #  5
*edges,6,23,24,30,32,17,16  #  6
*edges,4,24,25,31,30  #  7
*edges,6,25,26,19,18,33,31  #  8
*edges,4,26,27,20,19  #  9
*edges,4,27,21,11,20  # 10
*edges,10,11,12,13,14,15,16,17,18,19,20  # 11
*edges,12,1,10,9,8,7,6,5,4,39,38,3,2  # 12
*edges,4,5,6,23,22  # 13
*edges,4,6,7,24,23  # 14
*edges,4,7,8,25,24  # 15
*edges,4,8,9,26,25  # 16
*edges,4,9,10,27,26  # 17
*edges,10,30,31,33,32,30,34,37,36,35,34  # 18
*edges,4,32,33,18,17  # 19
*edges,4,10,1,21,27  # 20
*edges,4,1,2,28,21  # 21
*edges,4,2,3,29,28  # 22
*edges,4,34,35,36,37  # 23
*edges,4,38,39,40,41  # 24
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,f_ptn_g,VERT,-,-,-,masonry0.2,OPAQUE,ANOTHER,01,04  #   1 ||< fam_ptn_g:guest_room
*surf,f_ptn_h,VERT,-,-,-,masonry0.2,OPAQUE,ANOTHER,01,06  #   2 ||< fa_ptn_h:guest_room
*surf,fptn_hall,VERT,-,-,-,pla_pla_ptn,OPAQUE,ANOTHER,07,10  #   3 ||< hptn_fam:hall
*surf,fptn_stdy,VERT,-,-,-,pla_pla_ptn,OPAQUE,ANOTHER,08,08  #   4 ||< sptn_fam:study
*surf,north_a,VERT,-,-,-,stone_0.7m,OPAQUE,EXTERIOR,0,0  #   5 ||< external
*surf,angle_b,VERT,-,-,-,stone_0.4m,OPAQUE,EXTERIOR,0,0  #   6 ||< external
*surf,sill_c,VERT,-,-,-,stone_0.4m,OPAQUE,EXTERIOR,0,0  #   7 ||< external
*surf,angle_d,VERT,-,-,-,stone_0.4m,OPAQUE,EXTERIOR,0,0  #   8 ||< external
*surf,north_e,VERT,-,-,-,stone_0.7m,OPAQUE,EXTERIOR,0,0  #   9 ||< external
*surf,f_ptn_f,VERT,-,-,-,masonry0.3,OPAQUE,EXTERIOR,0,0  #  10 ||< external
*surf,ceiling_fam,CEIL,-,-,-,ceil_to_roof,OPAQUE,ANOTHER,13,04  #  11 ||< ceiling_fam:upper_ne
*surf,floor_fam,FLOR,-,-,-,floor_to_cli,OPAQUE,ANOTHER,11,01  #  12 ||< floor_fam:lower_ne
*surf,skirt_a,VERT,-,-,-,stone_0.7m,OPAQUE,EXTERIOR,0,0  #  13 ||< external
*surf,skirt_b,VERT,-,-,-,stone_0.4m,OPAQUE,EXTERIOR,0,0  #  14 ||< external
*surf,skirt_c,VERT,-,-,-,stone_0.4m,OPAQUE,EXTERIOR,0,0  #  15 ||< external
*surf,skirt_d,VERT,-,-,-,stone_0.4m,OPAQUE,EXTERIOR,0,0  #  16 ||< external
*surf,skirt_e,VERT,-,-,-,stone_0.7m,OPAQUE,EXTERIOR,0,0  #  17 ||< external
*surf,frame_c,VERT,-,-,-,sash_frame,OPAQUE,EXTERIOR,0,0  #  18 ||< external
*surf,head_c,VERT,-,-,-,stone_0.4m,OPAQUE,EXTERIOR,0,0  #  19 ||< external
*surf,skirt_f,VERT,-,-,-,masonry0.3,OPAQUE,EXTERIOR,0,0  #  20 ||< external
*surf,skirt_g,VERT,-,-,-,masonry0.2,OPAQUE,ANOTHER,01,03  #  21 ||< skirt_g:guest_room
*surf,skirt_h,VERT,-,-,-,masonry0.2,OPAQUE,ANOTHER,01,05  #  22 ||< skirt_h:guest_room
*surf,glass_c,VERT,frame_c,-,-,Window_ext,TRAN,EXTERIOR,0,0  #  23 ||< external
*surf,door_fam,VERT,fptn_hall,-,-,door,OPAQUE,ANOTHER,07,27  #  24 ||< door_fam:hall
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,all_applicable   1 # insolation sources
 23
# 
*base_list,1,12,    17.28 0  # zone base list
