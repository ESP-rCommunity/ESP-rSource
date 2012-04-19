*Geometry 1.1,GEN,store2 # tag version, format, zone name
*date Thu Apr 19 07:57:51 2012  # latest file modification 
store2 describes storage room adj entrance
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,2.80000,0.30000,0.20000  #   1
*vertex,4.50000,0.30000,0.20000  #   2
*vertex,4.50000,2.60000,0.20000  #   3
*vertex,4.30000,2.60000,0.20000  #   4
*vertex,3.50000,2.60000,0.20000  #   5
*vertex,2.80000,2.60000,0.20000  #   6
*vertex,2.80000,2.70000,0.20000  #   7
*vertex,2.60000,2.70000,0.20000  #   8
*vertex,2.60000,1.90000,0.20000  #   9
*vertex,2.80000,1.90000,0.20000  #  10
*vertex,2.80000,0.30000,2.40000  #  11
*vertex,4.50000,0.30000,2.40000  #  12
*vertex,4.50000,2.60000,2.40000  #  13
*vertex,4.30000,2.60000,2.40000  #  14
*vertex,3.50000,2.60000,2.40000  #  15
*vertex,2.80000,2.60000,2.40000  #  16
*vertex,2.80000,2.70000,2.40000  #  17
*vertex,2.60000,2.70000,2.40000  #  18
*vertex,2.60000,1.90000,2.40000  #  19
*vertex,2.80000,1.90000,2.40000  #  20
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
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,fr_facade,VERT,-,WALL,-,Extwall_ti,OPAQUE,EXTERIOR,0,0  #   1 ||< external
*surf,ptn_bath,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,03,09  #   2 ||< ptn_store:bathroom
*surf,ptn_hall_a,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,05,13  #   3 ||< ptn_stor_d:low_hall
*surf,door_hall,VERT,-,-,-,Doorint_sol,OPAQUE,ANOTHER,05,12  #   4 ||< door_stor:low_hall
*surf,ptn_hall_b,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,05,11  #   5 ||< ptn_stor_c:low_hall
*surf,ptn_hall_c,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,05,10  #   6 ||< ptn_stor_b:low_hall
*surf,ptn_hall_d,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,05,09  #   7 ||< ptn_stor_a:low_hall
*surf,ptn_stair,VERT,-,-,-,Door_fire,OPAQUE,ANOTHER,01,10  #   8 ||< ptn_lowstore:stairs
*surf,ext,VERT,-,WALL,-,Extwall_ti,OPAQUE,EXTERIOR,0,0  #   9 ||< external
*surf,left_fac,VERT,-,WALL,-,Extwall_ti,OPAQUE,EXTERIOR,0,0  #  10 ||< external
*surf,stor2_ceil,CEIL,-,-,-,Ceilti_up,OPAQUE,ANOTHER,08,01  #  11 ||< ovr_store:roof
*surf,store2_floor,FLOR,-,-,-,Floorti_low,OPAQUE,ANOTHER,15,02  #  12 ||< Base-12:crawl
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,12,     4.07 0  # zone base list
