*Geometry 1.1,GEN,hall # tag version, format, zone name
*date Mon Mar 12 15:23:22 2012  # latest file modification 
hall describes circulation space connecting br and lounge
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,2.00000,2.00000,0.20000  #   1
*vertex,2.90000,2.00000,0.20000  #   2
*vertex,3.10000,2.00000,0.20000  #   3
*vertex,4.10000,2.00000,0.20000  #   4
*vertex,4.10000,2.30000,0.20000  #   5
*vertex,4.10000,3.60000,0.20000  #   6
*vertex,4.10000,3.80000,0.20000  #   7
*vertex,3.40000,3.80000,0.20000  #   8
*vertex,3.20000,3.80000,0.20000  #   9
*vertex,2.40000,3.80000,0.20000  #  10
*vertex,2.00000,3.80000,0.20000  #  11
*vertex,2.00000,3.70000,0.20000  #  12
*vertex,2.00000,2.70000,0.20000  #  13
*vertex,2.00000,2.00000,2.40000  #  14
*vertex,2.90000,2.00000,2.40000  #  15
*vertex,3.10000,2.00000,2.40000  #  16
*vertex,4.10000,2.00000,2.40000  #  17
*vertex,4.10000,2.30000,2.40000  #  18
*vertex,4.10000,3.60000,2.40000  #  19
*vertex,4.10000,3.80000,2.40000  #  20
*vertex,3.40000,3.80000,2.40000  #  21
*vertex,3.20000,3.80000,2.40000  #  22
*vertex,2.40000,3.80000,2.40000  #  23
*vertex,2.00000,3.80000,2.40000  #  24
*vertex,2.00000,3.70000,2.40000  #  25
*vertex,2.00000,2.70000,2.40000  #  26
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,15,14  #  1
*edges,4,2,3,16,15  #  2
*edges,4,3,4,17,16  #  3
*edges,4,4,5,18,17  #  4
*edges,4,5,6,19,18  #  5
*edges,4,6,7,20,19  #  6
*edges,4,7,8,21,20  #  7
*edges,4,8,9,22,21  #  8
*edges,4,9,10,23,22  #  9
*edges,4,10,11,24,23  # 10
*edges,4,11,12,25,24  # 11
*edges,4,12,13,26,25  # 12
*edges,4,13,1,14,26  # 13
*edges,13,14,15,16,17,18,19,20,21,22,23,24,25,26  # 14
*edges,13,1,13,12,11,10,9,8,7,6,5,4,3,2  # 15
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,door_br2,VERT,-,-,-,Doorint_sol,OPAQUE,ANOTHER,01,07  #   1 ||< door_hall:BR_two
*surf,ptn_br2,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,01,06  #   2 ||< ptn_hall_a:BR_two
*surf,ptn_bath,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,07,06  #   3 ||< ptn_hall:bath
*surf,ptn_liv_a,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,06,14  #   4 ||< ptn_hall_b:livingdin
*surf,door_liv,VERT,-,-,-,Doorint_sol,OPAQUE,ANOTHER,06,13  #   5 ||< door_hall:livingdin
*surf,ptn_liv_b,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,06,12  #   6 ||< ptn_hall_a:livingdin
*surf,ptn_stor,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,05,01  #   7 ||< ptn_hall:storage
*surf,ptn_br1_a,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,04,04  #   8 ||< ptn_hall_b:BR_one
*surf,door_br1,VERT,-,-,-,Doorint_sol,OPAQUE,ANOTHER,04,03  #   9 ||< door_hall:BR_one
*surf,ptn_br1_b,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,04,02  #  10 ||< ptn_hall_a:BR_one
*surf,ptn_vest_a,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,03,04  #  11 ||< ptn_hall_b:lobby_stor
*surf,door_vest,VERT,-,-,-,Doorint_sol,OPAQUE,ANOTHER,03,03  #  12 ||< door_hall:lobby_stor
*surf,ptn_vest_b,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,03,02  #  13 ||< ptn_hall_a:lobby_stor
*surf,hall_ceiling,CEIL,-,-,-,Ceilti_up,OPAQUE,ANOTHER,09,02  #  14 ||< hall_ceiling:roof
*surf,hall_floor,FLOR,-,-,-,Floorti_lowc,OPAQUE,ANOTHER,10,02  #  15 ||< hall_floor:crawl
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,15,     3.78 0  # zone base list
