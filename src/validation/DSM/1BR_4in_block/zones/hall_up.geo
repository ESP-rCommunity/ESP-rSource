*Geometry 1.1,GEN,hall_up # tag version, format, zone name
*date Mon Mar 12 10:14:48 2012  # latest file modification 
hall_up describes the upper entrance hall
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,5.50000,2.60000,2.60000  #   1
*vertex,6.10000,2.60000,2.60000  #   2
*vertex,6.10000,4.40000,2.60000  #   3
*vertex,5.50000,4.40000,2.60000  #   4
*vertex,4.60000,4.40000,2.60000  #   5
*vertex,4.10000,4.40000,2.60000  #   6
*vertex,4.10000,3.50000,2.60000  #   7
*vertex,2.60000,3.50000,2.60000  #   8
*vertex,2.60000,2.70000,2.60000  #   9
*vertex,2.80000,2.70000,2.60000  #  10
*vertex,2.80000,2.60000,2.60000  #  11
*vertex,3.50000,2.60000,2.60000  #  12
*vertex,4.30000,2.60000,2.60000  #  13
*vertex,4.50000,2.60000,2.60000  #  14
*vertex,4.70000,2.60000,2.60000  #  15
*vertex,5.50000,2.60000,4.80000  #  16
*vertex,6.10000,2.60000,4.80000  #  17
*vertex,6.10000,4.40000,4.80000  #  18
*vertex,5.50000,4.40000,4.80000  #  19
*vertex,4.60000,4.40000,4.80000  #  20
*vertex,4.10000,4.40000,4.80000  #  21
*vertex,4.10000,3.50000,4.80000  #  22
*vertex,2.60000,3.50000,4.80000  #  23
*vertex,2.60000,2.70000,4.80000  #  24
*vertex,2.80000,2.70000,4.80000  #  25
*vertex,2.80000,2.60000,4.80000  #  26
*vertex,3.50000,2.60000,4.80000  #  27
*vertex,4.30000,2.60000,4.80000  #  28
*vertex,4.50000,2.60000,4.80000  #  29
*vertex,4.70000,2.60000,4.80000  #  30
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,17,16  #  1
*edges,4,2,3,18,17  #  2
*edges,4,3,4,19,18  #  3
*edges,4,4,5,20,19  #  4
*edges,4,5,6,21,20  #  5
*edges,4,6,7,22,21  #  6
*edges,4,7,8,23,22  #  7
*edges,4,8,9,24,23  #  8
*edges,4,9,10,25,24  #  9
*edges,4,10,11,26,25  # 10
*edges,4,11,12,27,26  # 11
*edges,4,12,13,28,27  # 12
*edges,4,13,14,29,28  # 13
*edges,4,14,15,30,29  # 14
*edges,4,15,1,16,30  # 15
*edges,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30  # 16
*edges,15,1,15,14,13,12,11,10,9,8,7,6,5,4,3,2  # 17
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,ptn_bath_a,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,09,06  #   1 ||< ptn_hall_b:bathup
*surf,ptn_liv,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,13,07  #   2 ||< ptn_hall:living_up
*surf,pth_br_a,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,12,01  #   3 ||< ptn_hall_a:bedroom_up
*surf,door_br,VERT,-,-,-,Doorint_sol,OPAQUE,ANOTHER,12,13  #   4 ||< door_hall:bedroom_up
*surf,ptn_br_b,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,12,12  #   5 ||< ptn_hall_c:bedroom_up
*surf,ptn_br_c,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,12,11  #   6 ||< ptn_hall_d:bedroom_up
*surf,ptn_br_d,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,12,10  #   7 ||< ptn_hall_e:bedroom_up
*surf,ptn_stair,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,01,05  #   8 ||< ptn_uphall:stairs
*surf,ptn_stor_a,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,08,07  #   9 ||< ptn_hall_d:storeup
*surf,ptn_stor_b,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,08,06  #  10 ||< ptn_hall_c:storeup
*surf,ptn_stor_c,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,08,05  #  11 ||< ptn_hall_b:storeup
*surf,door_stoor,VERT,-,-,-,Doorint_sol,OPAQUE,ANOTHER,08,04  #  12 ||< door_hall:storeup
*surf,ptn_stor_d,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,08,03  #  13 ||< ptn_hall_a:storeup
*surf,ptn_bath_b,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,09,08  #  14 ||< ptn_hall_c:bathup
*surf,door_bath,VERT,-,-,-,Doorint_sol,OPAQUE,ANOTHER,09,07  #  15 ||< door_hall:bathup
*surf,hall_ceiling,CEIL,-,-,-,Ceilti_up,OPAQUE,ANOTHER,14,04  #  16 ||< ovr_hall:roof
*surf,hall_floor,FLOR,-,-,-,Floorti_upc,OPAQUE,ANOTHER,05,16  #  17 ||< hall_ceil:low_hall
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,17,     4.93 0  # zone base list
