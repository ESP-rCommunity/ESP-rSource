*Geometry 1.1,GEN,bath # tag version, format, zone name
*date Mon Apr 23 16:58:50 2012  # latest file modification 
bath describes long thin bathroom
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,11.80000,7.60000,0.00000  #   1
*vertex,15.20000,7.60000,0.00000  #   2
*vertex,15.20000,8.00000,0.00000  #   3
*vertex,15.60000,8.00000,0.00000  #   4
*vertex,15.60000,8.90000,0.00000  #   5
*vertex,15.20000,9.20000,0.00000  #   6
*vertex,11.80000,9.20000,0.00000  #   7
*vertex,11.80000,7.60000,3.20000  #   8
*vertex,15.20000,7.60000,3.20000  #   9
*vertex,15.20000,8.00000,3.20000  #  10
*vertex,15.60000,8.00000,3.20000  #  11
*vertex,15.60000,8.90000,3.20000  #  12
*vertex,15.20000,9.20000,3.20000  #  13
*vertex,11.80000,9.20000,3.20000  #  14
*vertex,15.20000,7.60000,0.20000  #  15
*vertex,15.20000,8.00000,0.20000  #  16
*vertex,15.60000,8.00000,0.20000  #  17
*vertex,15.60000,8.90000,0.20000  #  18
*vertex,15.20000,9.20000,0.20000  #  19
*vertex,15.60000,8.00000,1.10000  #  20
*vertex,15.60000,8.90000,1.10000  #  21
*vertex,15.60000,8.00000,3.10000  #  22
*vertex,15.60000,8.90000,3.10000  #  23
*vertex,15.60000,8.05770,1.22822  #  24
*vertex,15.60000,8.84230,1.22822  #  25
*vertex,15.60000,8.84230,2.97178  #  26
*vertex,15.60000,8.05770,2.97178  #  27
*vertex,11.80000,9.00000,0.00000  #  28
*vertex,11.80000,8.20000,0.00000  #  29
*vertex,11.80000,8.20000,2.10000  #  30
*vertex,11.80000,9.00000,2.10000  #  31
# 
# tag, number of vertices followed by list of associated vert
*edges,5,1,2,15,9,8  #  1
*edges,4,15,16,10,9  #  2
*edges,6,16,17,20,22,11,10  #  3
*edges,4,17,18,21,20  #  4
*edges,6,18,19,13,12,23,21  #  5
*edges,5,6,7,14,13,19  #  6
*edges,8,7,28,31,30,29,1,8,14  #  7
*edges,7,8,9,10,11,12,13,14  #  8
*edges,9,1,29,28,7,6,5,4,3,2  #  9
*edges,4,2,3,16,15  # 10
*edges,4,3,4,17,16  # 11
*edges,4,4,5,18,17  # 12
*edges,4,5,6,19,18  # 13
*edges,10,20,21,23,22,20,24,27,26,25,24  # 14
*edges,4,22,23,12,11  # 15
*edges,4,24,25,26,27  # 16
*edges,4,28,29,30,31  # 17
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,ptn_br1,VERT,-,-,-,pla_pla_ptn,OPAQUE,ANOTHER,03,12  #   1 ||< ptn_batha:bedroom1
*surf,fill_a,VERT,-,-,-,stone_0.7m,OPAQUE,EXTERIOR,0,0  #   2 ||< external
*surf,angle_b,VERT,-,-,-,stone_0.9m,OPAQUE,EXTERIOR,0,0  #   3 ||< external
*surf,sill_c,VERT,-,-,-,stone_0.4m,OPAQUE,EXTERIOR,0,0  #   4 ||< external
*surf,angle_d,VERT,-,-,-,stone_0.4m,OPAQUE,EXTERIOR,0,0  #   5 ||< external
*surf,ptn_br2,VERT,-,-,-,pla_pla_ptn,OPAQUE,ANOTHER,05,01  #   6 ||< ptn_bath:bedroom2
*surf,ptn_hall,VERT,-,-,-,door,OPAQUE,ANOTHER,07,04  #   7 ||< hptn_bath:hall
*surf,ceiling_bth,CEIL,-,-,-,ceil_to_roof,OPAQUE,ANOTHER,12,04  #   8 ||< ceiling_bth:upper
*surf,floor_bth,FLOR,-,-,-,floor_to_cli,OPAQUE,ANOTHER,10,04  #   9 ||< floor_bth:lower
*surf,skirt_a,VERT,-,-,-,stone_0.7m,OPAQUE,EXTERIOR,0,0  #  10 ||< external
*surf,skirt_b,VERT,-,-,-,stone_0.9m,OPAQUE,EXTERIOR,0,0  #  11 ||< external
*surf,skirt_c,VERT,-,-,-,stone_0.4m,OPAQUE,EXTERIOR,0,0  #  12 ||< external
*surf,skirt_d,VERT,-,-,-,stone_0.4m,OPAQUE,EXTERIOR,0,0  #  13 ||< external
*surf,frame_c,VERT,-,-,-,sash_frame,OPAQUE,EXTERIOR,0,0  #  14 ||< external
*surf,head_c,VERT,-,-,-,stone_0.4m,OPAQUE,EXTERIOR,0,0  #  15 ||< external
*surf,glass_c,VERT,frame_c,-,-,Window_ext,TRAN,EXTERIOR,0,0  #  16 ||< external
*surf,door_bath,VERT,ptn_hall,-,-,door,OPAQUE,ANOTHER,07,25  #  17 ||< door_bath:hall
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,all_applicable   1 # insolation sources
 16
# 
*base_list,1,9,     5.86 0  # zone base list
