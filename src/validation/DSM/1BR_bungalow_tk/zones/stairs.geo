*Geometry 1.1,GEN,stairs # tag version, format, zone name
*date Mon Apr 16 19:33:08 2012  # latest file modification 
stairs describes the public access stairs in building
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,0.50000,1.90000,0.20000  #   1
*vertex,1.50000,1.90000,0.20000  #   2
*vertex,2.50000,1.90000,0.20000  #   3
*vertex,2.60000,1.90000,0.20000  #   4
*vertex,2.60000,2.70000,0.20000  #   5
*vertex,2.60000,3.50000,0.20000  #   6
*vertex,2.60000,4.40000,0.20000  #   7
*vertex,2.60000,5.70000,0.20000  #   8
*vertex,1.50000,5.70000,0.20000  #   9
*vertex,0.50000,5.70000,0.20000  #  10
*vertex,0.50000,1.90000,2.40000  #  11
*vertex,2.60000,1.90000,3.10000  #  12
*vertex,2.60000,5.70000,3.10000  #  13
*vertex,0.50000,5.70000,2.40000  #  14
*vertex,2.60000,1.90000,2.40000  #  15
*vertex,2.60000,2.70000,2.40000  #  16
*vertex,2.60000,4.40000,2.40000  #  17
*vertex,2.60000,5.70000,2.40000  #  18
*vertex,2.60000,3.50000,2.40000  #  19
*vertex,2.60000,1.90000,2.60000  #  20
*vertex,2.60000,5.70000,2.60000  #  21
*vertex,1.50000,2.00000,0.30000  #  22
*vertex,1.50000,5.60000,0.30000  #  23
*vertex,1.50000,2.00000,2.70000  #  24
*vertex,1.50000,4.60000,2.70000  #  25
*vertex,1.50000,5.60000,2.70000  #  26
*vertex,0.60000,1.90000,0.20000  #  27
*vertex,1.40000,1.90000,0.20000  #  28
*vertex,1.40000,1.90000,2.30000  #  29
*vertex,0.60000,1.90000,2.30000  #  30
*vertex,1.60000,1.90000,0.20000  #  31
*vertex,2.40000,1.90000,0.20000  #  32
*vertex,2.40000,1.90000,2.30000  #  33
*vertex,1.60000,1.90000,2.30000  #  34
*vertex,0.50000,5.40000,1.30000  #  35
*vertex,0.50000,4.53000,1.30000  #  36
*vertex,0.50000,4.53000,2.30000  #  37
*vertex,0.50000,5.40000,2.30000  #  38
*vertex,1.50000,5.70000,2.73333  #  39
*vertex,1.50000,1.90000,2.73333  #  40
*vertex,2.50000,1.90000,3.06667  #  41
# 
# tag, number of vertices followed by list of associated vert
*edges,8,1,27,30,29,28,2,40,11  #  1
*edges,8,2,31,34,33,32,3,41,40  #  2
*edges,6,3,4,15,20,12,41  #  3
*edges,4,20,21,13,12  #  4
*edges,6,8,9,39,13,21,18  #  5
*edges,4,9,10,14,39  #  6
*edges,10,10,1,11,14,10,35,38,37,36,35  #  7
*edges,7,11,40,41,12,13,39,14  #  8
*edges,14,1,10,9,8,7,6,5,4,3,32,31,2,28,27  #  9
*edges,4,4,5,16,15  # 10
*edges,4,7,8,18,17  # 11
*edges,4,6,7,17,19  # 12
*edges,4,5,6,19,16  # 13
*edges,7,17,18,21,20,15,16,19  # 14
*edges,5,22,23,26,25,24  # 15
*edges,5,23,22,24,25,26  # 16
*edges,4,27,28,29,30  # 17
*edges,4,31,32,33,34  # 18
*edges,4,35,36,37,38  # 19
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,front_a,VERT,-,WALL,-,tk_block_rn,OPAQUE,EXTERIOR,0,0  #   1 ||< external
*surf,front_b,VERT,-,WALL,-,tk_block_rn,OPAQUE,EXTERIOR,0,0  #   2 ||< external
*surf,front_c,VERT,-,WALL,-,tk_block_rn,OPAQUE,EXTERIOR,0,0  #   3 ||< external
*surf,ptn_stor,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,08,16  #   4 ||< ptn_stor:roof
*surf,back_b,VERT,-,WALL,-,tk_block_rn,OPAQUE,EXTERIOR,0,0  #   5 ||< external
*surf,back_a,VERT,-,WALL,-,tk_block_rn,OPAQUE,EXTERIOR,0,0  #   6 ||< external
*surf,left_side,VERT,-,WALL,-,tk_block_rn,OPAQUE,EXTERIOR,0,0  #   7 ||< external
*surf,ceiling,SLOP,-,ROOF,PITCHED,Roof_stairs,OPAQUE,EXTERIOR,0,0  #   8 ||< external
*surf,floor,FLOR,-,-,-,Floorti_low,OPAQUE,ANOTHER,15,01  #   9 ||< floor:crawl
*surf,ptn_lowstore,VERT,-,-,-,Door_fire,OPAQUE,ANOTHER,02,08  #  10 ||< ptn_stair:store2
*surf,ptn_lowbr,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,06,08  #  11 ||< ptn_stair_a:low_bedroom
*surf,ptn_lwwardr,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,06,09  #  12 ||< ptn_stair_b:low_bedroom
*surf,hall_door,VERT,-,-,-,Door_fire,OPAQUE,ANOTHER,05,08  #  13 ||< door_stair:low_hall
*surf,mid_floor,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,08,17  #  14 ||< mid_floor:roof
*surf,stair_wl,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,01,16  #  15 ||< stair_wl_:stairs
*surf,stair_wl_,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,01,15  #  16 ||< stair_wl:stairs
*surf,door_to_up,VERT,front_a,P-DOOR,CLOSED,Door_fire,OPAQUE,EXTERIOR,0,0  #  17 ||< external
*surf,door_low,VERT,front_b,P-DOOR,CLOSED,Door_fire,OPAQUE,EXTERIOR,0,0  #  18 ||< external
*surf,left_glz,VERT,left_side,C-WINDOW,CLOSED,Window_ext,DC_8074_04nb,EXTERIOR,0,0  #  19 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,9,     7.98 0  # zone base list
