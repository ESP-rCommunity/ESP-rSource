*Geometry 1.1,GEN,roof # tag version, format, zone name
*date Mon Mar 12 10:32:43 2012  # latest file modification 
roof describes cold roof space
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,2.60000,1.90000,4.80000  #   1
*vertex,2.60000,2.70000,4.80000  #   2
*vertex,2.60000,3.50000,4.80000  #   3
*vertex,2.60000,4.40000,4.80000  #   4
*vertex,2.60000,5.70000,4.80000  #   5
*vertex,2.80000,0.30000,4.80000  #   6
*vertex,4.50000,0.30000,4.80000  #   7
*vertex,4.50000,2.60000,4.80000  #   8
*vertex,4.30000,2.60000,4.80000  #   9
*vertex,3.50000,2.60000,4.80000  #  10
*vertex,2.80000,2.60000,4.80000  #  11
*vertex,2.80000,2.70000,4.80000  #  12
*vertex,2.80000,1.90000,4.80000  #  13
*vertex,4.70000,0.30000,4.80000  #  14
*vertex,5.40000,0.30000,4.80000  #  15
*vertex,6.50000,0.30000,4.80000  #  16
*vertex,6.50000,2.60000,4.80000  #  17
*vertex,6.10000,2.60000,4.80000  #  18
*vertex,5.50000,2.60000,4.80000  #  19
*vertex,4.70000,2.60000,4.80000  #  20
*vertex,7.80000,0.30000,4.80000  #  21
*vertex,8.40000,0.30000,4.80000  #  22
*vertex,9.60000,0.30000,4.80000  #  23
*vertex,9.60000,2.60000,4.80000  #  24
*vertex,8.60000,2.60000,4.80000  #  25
*vertex,7.40000,2.60000,4.80000  #  26
*vertex,6.10000,4.40000,4.80000  #  27
*vertex,5.50000,4.40000,4.80000  #  28
*vertex,4.60000,4.40000,4.80000  #  29
*vertex,4.10000,4.40000,4.80000  #  30
*vertex,4.10000,3.50000,4.80000  #  31
*vertex,6.10000,7.30000,4.80000  #  32
*vertex,5.40000,7.30000,4.80000  #  33
*vertex,4.70000,7.30000,4.80000  #  34
*vertex,2.80000,7.30000,4.80000  #  35
*vertex,2.80000,5.70000,4.80000  #  36
*vertex,9.60000,7.30000,4.80000  #  37
*vertex,8.60000,7.30000,4.80000  #  38
*vertex,7.00000,7.30000,4.80000  #  39
*vertex,9.60000,3.80000,6.80000  #  40
*vertex,2.80000,1.90000,5.71429  #  41
*vertex,9.60000,2.60000,6.11429  #  42
*vertex,2.80000,5.70000,5.71429  #  43
*vertex,2.60000,3.80000,6.80000  #  44
*vertex,2.60000,1.90000,5.71429  #  45
*vertex,2.60000,5.70000,5.71429  #  46
# 
# tag, number of vertices followed by list of associated vert
*edges,10,7,6,13,1,2,12,11,10,9,8  #  1
*edges,9,14,7,8,20,19,18,17,16,15  #  2
*edges,8,21,16,17,26,25,24,23,22  #  3
*edges,15,18,19,20,8,9,10,11,12,2,3,31,30,29,28,27  #  4
*edges,13,27,28,29,30,31,3,4,5,36,35,34,33,32  #  5
*edges,10,24,25,26,17,18,27,32,39,38,37  #  6
*edges,13,22,23,42,40,44,45,41,6,7,14,15,16,21  #  7
*edges,11,34,35,43,46,44,40,37,38,39,32,33  #  8
*edges,4,1,13,41,45  #  9
*edges,3,13,6,41  # 10
*edges,3,23,24,42  # 11
*edges,4,24,37,40,42  # 12
*edges,3,35,36,43  # 13
*edges,4,36,5,46,43  # 14
*edges,8,2,1,45,44,46,5,4,3  # 15
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,ovr_store,FLOR,-,-,-,Floorti_rf,OPAQUE,ANOTHER,08,11  #   1 ||< stor_ceil:storeup
*surf,ovr_bath,FLOR,-,-,-,Floorti_rf,OPAQUE,ANOTHER,09,10  #   2 ||< bath_ceil:bathup
*surf,ovr_kitchen,FLOR,-,-,-,Floorti_rf,OPAQUE,ANOTHER,10,09  #   3 ||< kitch_ceil:kitchenup
*surf,ovr_hall,FLOR,-,-,-,Floorti_rf,OPAQUE,ANOTHER,11,16  #   4 ||< hall_ceiling:hall_up
*surf,ovr_bedroom,FLOR,-,-,-,Floorti_rf,OPAQUE,ANOTHER,12,14  #   5 ||< br_ceiling:bedroom_up
*surf,ovr_living,FLOR,-,-,-,Floorti_rf,OPAQUE,ANOTHER,13,11  #   6 ||< liv_ceiling:living_up
*surf,roof_a,SLOP,-,ROOF,PITCHED,Roof,OPAQUE,EXTERIOR,0,0  #   7 ||< external
*surf,roof_back,SLOP,-,ROOF,PITCHED,Roof,OPAQUE,EXTERIOR,0,0  #   8 ||< external
*surf,front_a,VERT,-,WALL,-,Extwall_ti,OPAQUE,EXTERIOR,0,0  #   9 ||< external
*surf,left_b,VERT,-,WALL,-,Extwall_ti,OPAQUE,EXTERIOR,0,0  #  10 ||< external
*surf,right_a,VERT,-,WALL,-,Extwall_ti,OPAQUE,EXTERIOR,0,0  #  11 ||< external
*surf,right_b,VERT,-,WALL,-,Extwall_ti,OPAQUE,EXTERIOR,0,0  #  12 ||< external
*surf,left_c,VERT,-,WALL,-,Extwall_ti,OPAQUE,EXTERIOR,0,0  #  13 ||< external
*surf,back_a,VERT,-,WALL,-,Extwall_ti,OPAQUE,EXTERIOR,0,0  #  14 ||< external
*surf,left,VERT,-,WALL,-,Extwall_ti,OPAQUE,EXTERIOR,0,0  #  15 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,0,48.36,0  # zone base
