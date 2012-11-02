*Geometry 1.1,GEN,low_hall # tag version, format, zone name
*date Thu Apr 19 07:57:57 2012  # latest file modification 
low_hall describes circulation space in lower flat
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,5.50000,2.60000,0.20000  #   1
*vertex,6.10000,2.60000,0.20000  #   2
*vertex,6.10000,4.40000,0.20000  #   3
*vertex,5.50000,4.40000,0.20000  #   4
*vertex,4.60000,4.40000,0.20000  #   5
*vertex,4.10000,4.40000,0.20000  #   6
*vertex,4.10000,3.50000,0.20000  #   7
*vertex,2.60000,3.50000,0.20000  #   8
*vertex,2.60000,2.70000,0.20000  #   9
*vertex,2.80000,2.70000,0.20000  #  10
*vertex,2.80000,2.60000,0.20000  #  11
*vertex,3.50000,2.60000,0.20000  #  12
*vertex,4.30000,2.60000,0.20000  #  13
*vertex,4.50000,2.60000,0.20000  #  14
*vertex,4.70000,2.60000,0.20000  #  15
*vertex,5.50000,2.60000,2.40000  #  16
*vertex,6.10000,2.60000,2.40000  #  17
*vertex,6.10000,4.40000,2.40000  #  18
*vertex,5.50000,4.40000,2.40000  #  19
*vertex,4.60000,4.40000,2.40000  #  20
*vertex,4.10000,4.40000,2.40000  #  21
*vertex,4.10000,3.50000,2.40000  #  22
*vertex,2.60000,3.50000,2.40000  #  23
*vertex,2.60000,2.70000,2.40000  #  24
*vertex,2.80000,2.70000,2.40000  #  25
*vertex,2.80000,2.60000,2.40000  #  26
*vertex,3.50000,2.60000,2.40000  #  27
*vertex,4.30000,2.60000,2.40000  #  28
*vertex,4.50000,2.60000,2.40000  #  29
*vertex,4.70000,2.60000,2.40000  #  30
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
*surf,door_bath,VERT,-,-,-,Doorint_sol,OPAQUE,ANOTHER,03,06  #   1 ||< door_hall:bathroom
*surf,ptn_liv,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,07,07  #   2 ||< ptn_hall:lowliving
*surf,ptn_br_a,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,06,01  #   3 ||< ptn_hall_a:low_bedroom
*surf,door_br,VERT,-,-,-,Doorint_sol,OPAQUE,ANOTHER,06,13  #   4 ||< door_hall:low_bedroom
*surf,ptn_br_b,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,06,12  #   5 ||< ptn_hall_c:low_bedroom
*surf,ptn_br_c,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,06,11  #   6 ||< ptn_hall_d:low_bedroom
*surf,ptn_br_d,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,06,10  #   7 ||< ptn_hall_e:low_bedroom
*surf,door_stair,VERT,-,-,-,Door_fire,OPAQUE,ANOTHER,01,13  #   8 ||< hall_door:stairs
*surf,ptn_stor_a,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,02,07  #   9 ||< ptn_hall_d:store2
*surf,ptn_stor_b,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,02,06  #  10 ||< ptn_hall_c:store2
*surf,ptn_stor_c,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,02,05  #  11 ||< ptn_hall_b:store2
*surf,door_stor,VERT,-,-,-,Doorint_sol,OPAQUE,ANOTHER,02,04  #  12 ||< door_hall:store2
*surf,ptn_stor_d,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,02,03  #  13 ||< ptn_hall_a:store2
*surf,ptn_bath_a,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,03,08  #  14 ||< ptn_hall_c:bathroom
*surf,ptn_bath_b,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,03,07  #  15 ||< ptn_hall_b:bathroom
*surf,hall_ceil,CEIL,-,-,-,Ceilti_up,OPAQUE,ANOTHER,08,04  #  16 ||< ovr_hall:roof
*surf,hall_floor,FLOR,-,-,-,Floorti_lowc,OPAQUE,ANOTHER,15,05  #  17 ||< hall_floor:crawl
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,17,     4.93 0  # zone base list
