*Geometry 1.1,GEN,kitchen # tag version, format, zone name
*date Mon Mar 12 15:45:42 2012  # latest file modification 
kitchen describes a corner kitchen
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,5.10000,0.00000,0.20000  #   1
*vertex,6.20000,0.00000,0.20000  #   2
*vertex,7.00000,0.00000,0.20000  #   3
*vertex,8.10000,0.00000,0.20000  #   4
*vertex,8.10000,2.00000,0.20000  #   5
*vertex,7.10000,2.00000,0.20000  #   6
*vertex,6.00000,2.00000,0.20000  #   7
*vertex,5.10000,2.00000,0.20000  #   8
*vertex,5.10000,0.00000,2.40000  #   9
*vertex,6.20000,0.00000,2.40000  #  10
*vertex,7.00000,0.00000,2.40000  #  11
*vertex,8.10000,0.00000,2.40000  #  12
*vertex,8.10000,2.00000,2.40000  #  13
*vertex,7.10000,2.00000,2.40000  #  14
*vertex,6.00000,2.00000,2.40000  #  15
*vertex,5.10000,2.00000,2.40000  #  16
*vertex,6.20000,0.00000,0.90000  #  17
*vertex,7.00000,0.00000,0.90000  #  18
*vertex,6.20000,0.00000,2.10000  #  19
*vertex,7.00000,0.00000,2.10000  #  20
*vertex,6.24000,0.00000,0.94000  #  21
*vertex,6.96000,0.00000,0.94000  #  22
*vertex,6.96000,0.00000,2.06000  #  23
*vertex,6.24000,0.00000,2.06000  #  24
# 
# tag, number of vertices followed by list of associated vert
*edges,6,1,2,17,19,10,9  #  1
*edges,4,2,3,18,17  #  2
*edges,6,3,4,12,11,20,18  #  3
*edges,4,4,5,13,12  #  4
*edges,4,5,6,14,13  #  5
*edges,4,6,7,15,14  #  6
*edges,4,7,8,16,15  #  7
*edges,4,8,1,9,16  #  8
*edges,8,9,10,11,12,13,14,15,16  #  9
*edges,8,1,8,7,6,5,4,3,2  # 10
*edges,4,19,20,11,10  # 11
*edges,10,17,18,20,19,17,21,24,23,22,21  # 12
*edges,4,21,22,23,24  # 13
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,facade-a,VERT,-,WALL,-,Extwall_ti,OPAQUE,EXTERIOR,0,0  #   1 ||< external
*surf,sill_v,VERT,-,WALL,-,Extwall_ti,OPAQUE,EXTERIOR,0,0  #   2 ||< external
*surf,facade_b,VERT,-,WALL,-,Extwall_ti,OPAQUE,EXTERIOR,0,0  #   3 ||< external
*surf,right_fac,VERT,-,WALL,-,Partywall_ti,OPAQUE,SIMILAR,00,00  #   4 ||< identical environment
*surf,ptn_liv_a,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,06,04  #   5 ||< ptn_kit_b:livingdin
*surf,door_liv,VERT,-,-,-,Doorint_sol,OPAQUE,ANOTHER,06,03  #   6 ||< door_kit:livingdin
*surf,ptn_liv_b,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,06,02  #   7 ||< ptn_kit_a:livingdin
*surf,ptn_bath,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,07,04  #   8 ||< ptn_kit:bath
*surf,kit_ceiling,CEIL,-,-,-,Ceilti_up,OPAQUE,ANOTHER,09,08  #   9 ||< kit_ceiling:roof
*surf,kit_floor,FLOR,-,-,-,Floorti_low,OPAQUE,ANOTHER,10,08  #  10 ||< kit_floor:crawl
*surf,head,VERT,-,WALL,-,Extwall_ti,OPAQUE,EXTERIOR,0,0  #  11 ||< external
*surf,frame,VERT,-,F-FRAME,CLOSED,Window_frme,OPAQUE,EXTERIOR,0,0  #  12 ||< external
*surf,glazing,VERT,frame,C-WINDOW,CLOSED,Window_ext,DC_8074_04nb,EXTERIOR,0,0  #  13 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,10,     6.00 0  # zone base list
