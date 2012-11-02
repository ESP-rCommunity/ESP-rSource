*Geometry 1.1,GEN,lobby_stor # tag version, format, zone name
*date Mon Mar 12 15:34:01 2012  # latest file modification 
lobby_stor describes entrance lobby plus store
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,0.90000,2.00000,0.20000  #   1
*vertex,2.00000,2.00000,0.20000  #   2
*vertex,2.00000,2.70000,0.20000  #   3
*vertex,2.00000,3.70000,0.20000  #   4
*vertex,2.00000,3.80000,0.20000  #   5
*vertex,0.00000,3.80000,0.20000  #   6
*vertex,0.00000,3.70000,0.20000  #   7
*vertex,0.00000,2.90000,0.20000  #   8
*vertex,0.00000,2.70000,0.20000  #   9
*vertex,0.00000,2.00000,0.20000  #  10
*vertex,0.90000,2.00000,2.40000  #  11
*vertex,2.00000,2.00000,2.40000  #  12
*vertex,2.00000,2.70000,2.40000  #  13
*vertex,2.00000,3.70000,2.40000  #  14
*vertex,2.00000,3.80000,2.40000  #  15
*vertex,0.00000,3.80000,2.40000  #  16
*vertex,0.00000,3.70000,2.40000  #  17
*vertex,0.00000,2.90000,2.40000  #  18
*vertex,0.00000,2.70000,2.40000  #  19
*vertex,0.00000,2.00000,2.40000  #  20
*vertex,0.10000,2.70000,0.30000  #  21
*vertex,1.90000,2.70000,0.30000  #  22
*vertex,1.90000,2.70000,2.30000  #  23
*vertex,0.10000,2.70000,2.30000  #  24
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,12,11  #  1
*edges,4,2,3,13,12  #  2
*edges,4,3,4,14,13  #  3
*edges,4,4,5,15,14  #  4
*edges,4,5,6,16,15  #  5
*edges,4,6,7,17,16  #  6
*edges,4,7,8,18,17  #  7
*edges,4,8,9,19,18  #  8
*edges,4,9,10,20,19  #  9
*edges,4,10,1,11,20  # 10
*edges,10,11,12,13,14,15,16,17,18,19,20  # 11
*edges,10,1,10,9,8,7,6,5,4,3,2  # 12
*edges,4,21,22,23,24  # 13
*edges,4,22,21,24,23  # 14
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,ptn_br2_a,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,01,08  #   1 ||< ptn_vest_a:BR_two
*surf,ptn_hall_a,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,02,13  #   2 ||< ptn_vest_b:hall
*surf,door_hall,VERT,-,-,-,Doorint_sol,OPAQUE,ANOTHER,02,12  #   3 ||< door_vest:hall
*surf,ptn_hall_b,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,02,11  #   4 ||< ptn_vest_a:hall
*surf,ptn_br1,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,04,01  #   5 ||< ptn_vest:BR_one
*surf,facade_a,VERT,-,WALL,-,Extwall_ti,OPAQUE,EXTERIOR,0,0  #   6 ||< external
*surf,ext_door,VERT,-,P-DOOR,CLOSED,Door_ext,OPAQUE,EXTERIOR,0,0  #   7 ||< external
*surf,facade_b,VERT,-,WALL,-,Extwall_ti,OPAQUE,EXTERIOR,0,0  #   8 ||< external
*surf,facade_c,VERT,-,WALL,-,Extwall_ti,OPAQUE,EXTERIOR,0,0  #   9 ||< external
*surf,ptn_br2_b,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,01,09  #  10 ||< ptn_vest_b:BR_two
*surf,vest_ceiling,CEIL,-,-,-,Ceilti_up,OPAQUE,ANOTHER,09,03  #  11 ||< vest_ceiling:roof
*surf,vest_floor,FLOR,-,-,-,Floorti_low,OPAQUE,ANOTHER,10,03  #  12 ||< vest_floor:crawl
*surf,int_door,VERT,-,-,-,Doorint_sol,OPAQUE,ANOTHER,03,14  #  13 ||< int_door_:lobby_stor
*surf,int_door_,VERT,-,-,-,Doorint_sol,OPAQUE,ANOTHER,03,13  #  14 ||< int_door:lobby_stor
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,12,     3.60 0  # zone base list
