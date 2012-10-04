*Geometry 1.1,GEN,low_kitchen # tag version, format, zone name
*date Thu Apr 19 07:57:56 2012  # latest file modification 
low_kitchen describes kitchen for lower flat in the corner
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,6.50000,0.30000,0.20000  #   1
*vertex,7.80000,0.30000,0.20000  #   2
*vertex,8.40000,0.30000,0.20000  #   3
*vertex,9.60000,0.30000,0.20000  #   4
*vertex,9.60000,2.60000,0.20000  #   5
*vertex,8.60000,2.60000,0.20000  #   6
*vertex,7.40000,2.60000,0.20000  #   7
*vertex,6.50000,2.60000,0.20000  #   8
*vertex,6.50000,0.30000,2.40000  #   9
*vertex,7.80000,0.30000,2.40000  #  10
*vertex,8.40000,0.30000,2.40000  #  11
*vertex,9.60000,0.30000,2.40000  #  12
*vertex,9.60000,2.60000,2.40000  #  13
*vertex,8.60000,2.60000,2.40000  #  14
*vertex,7.40000,2.60000,2.40000  #  15
*vertex,6.50000,2.60000,2.40000  #  16
*vertex,7.80000,0.30000,2.10000  #  17
*vertex,8.40000,0.30000,2.10000  #  18
*vertex,7.80000,0.30000,0.90000  #  19
*vertex,8.40000,0.30000,0.90000  #  20
*vertex,7.84000,0.30000,0.94000  #  21
*vertex,8.36000,0.30000,0.94000  #  22
*vertex,8.36000,0.30000,2.06000  #  23
*vertex,7.84000,0.30000,2.06000  #  24
# 
# tag, number of vertices followed by list of associated vert
*edges,6,1,2,19,17,10,9  #  1
*edges,4,17,18,11,10  #  2
*edges,6,3,4,12,11,18,20  #  3
*edges,4,4,5,13,12  #  4
*edges,4,5,6,14,13  #  5
*edges,4,6,7,15,14  #  6
*edges,4,7,8,16,15  #  7
*edges,4,8,1,9,16  #  8
*edges,8,9,10,11,12,13,14,15,16  #  9
*edges,8,1,8,7,6,5,4,3,2  # 10
*edges,10,19,20,18,17,19,21,24,23,22,21  # 11
*edges,4,2,3,20,19  # 12
*edges,4,21,22,23,24  # 13
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,fr_wall_a,VERT,-,WALL,-,Extwall_ti,OPAQUE,EXTERIOR,0,0  #   1 ||< external
*surf,head,VERT,-,WALL,-,Extwall_ti,OPAQUE,EXTERIOR,0,0  #   2 ||< external
*surf,fr_wall_b,VERT,-,WALL,-,Extwall_ti,OPAQUE,EXTERIOR,0,0  #   3 ||< external
*surf,right_wall,VERT,-,WALL,-,Partywall_ti,OPAQUE,SIMILAR,00,00  #   4 ||< identical environment
*surf,ptn_liv_a,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,07,01  #   5 ||< ptn_kitch_a:lowliving
*surf,door_liv,VERT,-,-,-,Doorint_sol,OPAQUE,ANOTHER,07,10  #   6 ||< door_kit:lowliving
*surf,ptn_liv_b,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,07,09  #   7 ||< ptn_b:lowliving
*surf,ptn_bath,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,03,04  #   8 ||< ptn_bath:bathroom
*surf,kitch_ceil,CEIL,-,-,-,Ceilti_up,OPAQUE,ANOTHER,08,03  #   9 ||< ovr_kitchen:roof
*surf,kitch_floor,FLOR,-,-,-,Floorti_low,OPAQUE,ANOTHER,15,04  #  10 ||< kitch_floor:crawl
*surf,frame,VERT,-,F-FRAME,CLOSED,Window_frme,OPAQUE,EXTERIOR,0,0  #  11 ||< external
*surf,sill,VERT,-,WALL,-,Extwall_ti,OPAQUE,EXTERIOR,0,0  #  12 ||< external
*surf,glazing,VERT,frame,C-WINDOW,CLOSED,Window_ext,DC_8074_04nb,EXTERIOR,0,0  #  13 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,10,     7.13 0  # zone base list
