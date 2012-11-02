*Geometry 1.1,GEN,livingdin # tag version, format, zone name
*date Mon Mar 12 15:42:25 2012  # latest file modification 
livingdin describes the combined living and dining room
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,4.10000,2.00000,0.20000  #   1
*vertex,5.10000,2.00000,0.20000  #   2
*vertex,6.00000,2.00000,0.20000  #   3
*vertex,7.10000,2.00000,0.20000  #   4
*vertex,8.10000,2.00000,0.20000  #   5
*vertex,8.10000,3.70000,0.20000  #   6
*vertex,8.10000,6.60000,0.20000  #   7
*vertex,6.80000,6.60000,0.20000  #   8
*vertex,5.40000,6.60000,0.20000  #   9
*vertex,4.10000,6.60000,0.20000  #  10
*vertex,4.10000,5.10000,0.20000  #  11
*vertex,4.10000,3.80000,0.20000  #  12
*vertex,4.10000,3.60000,0.20000  #  13
*vertex,4.10000,2.30000,0.20000  #  14
*vertex,4.10000,2.00000,2.40000  #  15
*vertex,5.10000,2.00000,2.40000  #  16
*vertex,6.00000,2.00000,2.40000  #  17
*vertex,7.10000,2.00000,2.40000  #  18
*vertex,8.10000,2.00000,2.40000  #  19
*vertex,8.10000,3.70000,2.40000  #  20
*vertex,8.10000,6.60000,2.40000  #  21
*vertex,6.80000,6.60000,2.40000  #  22
*vertex,5.40000,6.60000,2.40000  #  23
*vertex,4.10000,6.60000,2.40000  #  24
*vertex,4.10000,5.10000,2.40000  #  25
*vertex,4.10000,3.80000,2.40000  #  26
*vertex,4.10000,3.60000,2.40000  #  27
*vertex,4.10000,2.30000,2.40000  #  28
*vertex,6.80000,6.60000,0.60000  #  29
*vertex,5.40000,6.60000,0.60000  #  30
*vertex,6.80000,6.60000,2.10000  #  31
*vertex,5.40000,6.60000,2.10000  #  32
*vertex,6.76000,6.60000,0.64000  #  33
*vertex,5.44000,6.60000,0.64000  #  34
*vertex,5.44000,6.60000,2.06000  #  35
*vertex,6.76000,6.60000,2.06000  #  36
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,16,15  #  1
*edges,4,2,3,17,16  #  2
*edges,4,3,4,18,17  #  3
*edges,4,4,5,19,18  #  4
*edges,4,5,6,20,19  #  5
*edges,4,6,7,21,20  #  6
*edges,6,7,8,29,31,22,21  #  7
*edges,4,8,9,30,29  #  8
*edges,6,9,10,24,23,32,30  #  9
*edges,4,10,11,25,24  # 10
*edges,4,11,12,26,25  # 11
*edges,4,12,13,27,26  # 12
*edges,4,13,14,28,27  # 13
*edges,4,14,1,15,28  # 14
*edges,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28  # 15
*edges,14,1,14,13,12,11,10,9,8,7,6,5,4,3,2  # 16
*edges,4,31,32,23,22  # 17
*edges,10,29,30,32,31,29,33,36,35,34,33  # 18
*edges,4,33,34,35,36  # 19
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,ptn_bath,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,07,05  #   1 ||< ptn_liv:bath
*surf,ptn_kit_a,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,08,07  #   2 ||< ptn_liv_b:kitchen
*surf,door_kit,VERT,-,-,-,Doorint_sol,OPAQUE,ANOTHER,08,06  #   3 ||< door_liv:kitchen
*surf,ptn_kit_b,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,08,05  #   4 ||< ptn_liv_a:kitchen
*surf,right_oth_a,VERT,-,WALL,-,Partywall_ti,OPAQUE,SIMILAR,00,00  #   5 ||< identical environment
*surf,right_oth_b,VERT,-,WALL,-,Partywall_ti,OPAQUE,SIMILAR,00,00  #   6 ||< identical environment
*surf,bk_fac_a,VERT,-,WALL,-,Extwall_ti,OPAQUE,EXTERIOR,0,0  #   7 ||< external
*surf,sill_v,VERT,-,WALL,-,Extwall_ti,OPAQUE,EXTERIOR,0,0  #   8 ||< external
*surf,bk_fac_b,VERT,-,WALL,-,Extwall_ti,OPAQUE,EXTERIOR,0,0  #   9 ||< external
*surf,ptn_stor_a,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,05,03  #  10 ||< ptn_liv_b:storage
*surf,ptn_stor_b,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,05,02  #  11 ||< ptn_liv_a:storage
*surf,ptn_hall_a,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,02,06  #  12 ||< ptn_liv_b:hall
*surf,door_hall,VERT,-,-,-,Doorint_sol,OPAQUE,ANOTHER,02,05  #  13 ||< door_liv:hall
*surf,ptn_hall_b,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,02,04  #  14 ||< ptn_liv_a:hall
*surf,liv_ceiling,CEIL,-,-,-,Ceilti_up,OPAQUE,ANOTHER,09,06  #  15 ||< liv_ceiling:roof
*surf,liv_floor,FLOR,-,-,-,Floorti_lowc,OPAQUE,ANOTHER,10,06  #  16 ||< liv_floor:crawl
*surf,head,VERT,-,WALL,-,Extwall_ti,OPAQUE,EXTERIOR,0,0  #  17 ||< external
*surf,frame,VERT,-,F-FRAME,CLOSED,Window_frme,OPAQUE,EXTERIOR,0,0  #  18 ||< external
*surf,glazing,VERT,frame,C-WINDOW,CLOSED,Window_ext,DC_8074_04nb,EXTERIOR,0,0  #  19 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,16,    18.40 0  # zone base list
