*Geometry 1.1,GEN,stairs # tag version, format, zone name
*date Mon Mar 12 09:46:22 2012  # latest file modification 
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
*vertex,0.50000,1.90000,4.10000  #  11
*vertex,2.60000,1.90000,4.80000  #  12
*vertex,2.60000,2.70000,4.80000  #  13
*vertex,2.60000,3.50000,4.80000  #  14
*vertex,2.60000,4.40000,4.80000  #  15
*vertex,2.60000,5.70000,4.80000  #  16
*vertex,0.50000,5.70000,4.10000  #  17
*vertex,2.60000,1.90000,2.40000  #  18
*vertex,2.60000,2.70000,2.40000  #  19
*vertex,2.60000,4.40000,2.40000  #  20
*vertex,2.60000,5.70000,2.40000  #  21
*vertex,2.60000,3.50000,2.40000  #  22
*vertex,2.60000,1.90000,2.60000  #  23
*vertex,2.60000,2.70000,2.60000  #  24
*vertex,2.60000,4.40000,2.60000  #  25
*vertex,2.60000,5.70000,2.60000  #  26
*vertex,2.60000,3.50000,2.60000  #  27
*vertex,1.50000,2.00000,0.30000  #  28
*vertex,1.50000,5.60000,0.30000  #  29
*vertex,1.50000,2.00000,4.20000  #  30
*vertex,1.50000,4.60000,4.20000  #  31
*vertex,1.50000,4.60000,2.20000  #  32
*vertex,1.50000,5.60000,2.20000  #  33
*vertex,0.60000,1.90000,0.20000  #  34
*vertex,1.40000,1.90000,0.20000  #  35
*vertex,1.40000,1.90000,2.30000  #  36
*vertex,0.60000,1.90000,2.30000  #  37
*vertex,1.60000,1.90000,0.20000  #  38
*vertex,2.40000,1.90000,0.20000  #  39
*vertex,2.40000,1.90000,2.30000  #  40
*vertex,1.60000,1.90000,2.30000  #  41
*vertex,0.50000,5.40000,3.00000  #  42
*vertex,0.50000,4.53000,3.00000  #  43
*vertex,0.50000,4.53000,4.00000  #  44
*vertex,0.50000,5.40000,4.00000  #  45
*vertex,1.50000,5.70000,4.43333  #  46
*vertex,1.50000,1.90000,4.43333  #  47
*vertex,2.50000,1.90000,4.76667  #  48
# 
# tag, number of vertices followed by list of associated vert
*edges,8,1,34,37,36,35,2,47,11  #  1
*edges,8,2,38,41,40,39,3,48,47  #  2
*edges,6,3,4,18,23,12,48  #  3
*edges,4,23,24,13,12  #  4
*edges,4,24,27,14,13  #  5
*edges,4,27,25,15,14  #  6
*edges,4,25,26,16,15  #  7
*edges,6,8,9,46,16,26,21  #  8
*edges,4,9,10,17,46  #  9
*edges,10,10,1,11,17,10,42,45,44,43,42  # 10
*edges,10,11,47,48,12,13,14,15,16,46,17  # 11
*edges,14,1,10,9,8,7,6,5,4,3,39,38,2,35,34  # 12
*edges,4,4,5,19,18  # 13
*edges,4,7,8,21,20  # 14
*edges,4,6,7,20,22  # 15
*edges,4,5,6,22,19  # 16
*edges,10,20,21,26,25,27,24,23,18,19,22  # 17
*edges,6,28,29,33,32,31,30  # 18
*edges,6,29,28,30,31,32,33  # 19
*edges,4,34,35,36,37  # 20
*edges,4,38,39,40,41  # 21
*edges,4,42,43,44,45  # 22
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,front_a,VERT,-,WALL,-,Extwall_ti,OPAQUE,EXTERIOR,0,0  #   1 ||< external
*surf,front_b,VERT,-,WALL,-,Extwall_ti,OPAQUE,EXTERIOR,0,0  #   2 ||< external
*surf,front_c,VERT,-,WALL,-,Extwall_ti,OPAQUE,EXTERIOR,0,0  #   3 ||< external
*surf,ptn_stor,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,08,08  #   4 ||< ptn_stair:storeup
*surf,ptn_uphall,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,11,08  #   5 ||< ptn_stair:hall_up
*surf,ptn_upbrward,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,12,09  #   6 ||< ptn_stair_d:bedroom_up
*surf,ptn_upbrb,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,12,08  #   7 ||< ptn_stair_c:bedroom_up
*surf,back_b,VERT,-,WALL,-,Extwall_ti,OPAQUE,EXTERIOR,0,0  #   8 ||< external
*surf,back_a,VERT,-,WALL,-,Extwall_ti,OPAQUE,EXTERIOR,0,0  #   9 ||< external
*surf,left_side,VERT,-,WALL,-,Extwall_ti,OPAQUE,EXTERIOR,0,0  #  10 ||< external
*surf,ceiling,SLOP,-,ROOF,PITCHED,Roof_stairs,OPAQUE,EXTERIOR,0,0  #  11 ||< external
*surf,floor,FLOR,-,-,-,Floorti_low,OPAQUE,ANOTHER,15,01  #  12 ||< floor:crawl
*surf,ptn_lowstore,VERT,-,-,-,Door_fire,OPAQUE,ANOTHER,02,08  #  13 ||< ptn_stair:store2
*surf,ptn_lowbr,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,06,08  #  14 ||< ptn_stair_a:low_bedroom
*surf,ptn_lwwardr,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,06,09  #  15 ||< ptn_stair_b:low_bedroom
*surf,hall_door,VERT,-,-,-,Door_fire,OPAQUE,ANOTHER,05,08  #  16 ||< door_stair:low_hall
*surf,mid_floor,VERT,-,-,-,Parttim,OPAQUE,ADIABATIC,0,0  #  17 ||< adiabatic
*surf,stair_wl,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,01,19  #  18 ||< stair_wl_:stairs
*surf,stair_wl_,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,01,18  #  19 ||< stair_wl:stairs
*surf,door_to_up,VERT,front_a,P-DOOR,CLOSED,Door_fire,OPAQUE,EXTERIOR,0,0  #  20 ||< external
*surf,door_low,VERT,front_b,P-DOOR,CLOSED,Door_fire,OPAQUE,EXTERIOR,0,0  #  21 ||< external
*surf,left_glz,VERT,left_side,C-WINDOW,CLOSED,Window_ext,DC_8074_04nb,EXTERIOR,0,0  #  22 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,12,     7.98 0  # zone base list
