*Geometry 1.1,GEN,bathroom # tag version, format, zone name
*date Wed Mar 21 11:14:53 2012  # latest file modification 
bathroom describes lower bathroom between store2 & kitchen
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,4.50000,0.30000,0.20000  #   1
*vertex,4.70000,0.30000,0.20000  #   2
*vertex,5.40000,0.30000,0.20000  #   3
*vertex,6.50000,0.30000,0.20000  #   4
*vertex,6.50000,2.60000,0.20000  #   5
*vertex,6.10000,2.60000,0.20000  #   6
*vertex,5.50000,2.60000,0.20000  #   7
*vertex,4.70000,2.60000,0.20000  #   8
*vertex,4.50000,2.60000,0.20000  #   9
*vertex,4.50000,0.30000,2.40000  #  10
*vertex,4.70000,0.30000,2.40000  #  11
*vertex,5.40000,0.30000,2.40000  #  12
*vertex,6.50000,0.30000,2.40000  #  13
*vertex,6.50000,2.60000,2.40000  #  14
*vertex,6.10000,2.60000,2.40000  #  15
*vertex,5.50000,2.60000,2.40000  #  16
*vertex,4.70000,2.60000,2.40000  #  17
*vertex,4.50000,2.60000,2.40000  #  18
*vertex,4.70000,0.30000,2.10000  #  19
*vertex,5.40000,0.30000,2.10000  #  20
*vertex,4.70000,0.30000,1.30000  #  21
*vertex,5.40000,0.30000,1.30000  #  22
*vertex,4.74000,0.30000,1.34000  #  23
*vertex,5.36000,0.30000,1.34000  #  24
*vertex,5.36000,0.30000,2.06000  #  25
*vertex,4.74000,0.30000,2.06000  #  26
# 
# tag, number of vertices followed by list of associated vert
*edges,6,1,2,21,19,11,10  #  1
*edges,4,19,20,12,11  #  2
*edges,6,3,4,13,12,20,22  #  3
*edges,4,4,5,14,13  #  4
*edges,4,5,6,15,14  #  5
*edges,4,6,7,16,15  #  6
*edges,4,7,8,17,16  #  7
*edges,4,8,9,18,17  #  8
*edges,4,9,1,10,18  #  9
*edges,9,10,11,12,13,14,15,16,17,18  # 10
*edges,9,1,9,8,7,6,5,4,3,2  # 11
*edges,10,21,22,20,19,21,23,26,25,24,23  # 12
*edges,4,2,3,22,21  # 13
*edges,4,23,24,25,26  # 14
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,fr_wall_a,VERT,-,WALL,-,Extwall_ti,OPAQUE,EXTERIOR,0,0  #   1 ||< external
*surf,head,VERT,-,WALL,-,Extwall_ti,OPAQUE,EXTERIOR,0,0  #   2 ||< external
*surf,fr_wall_b,VERT,-,WALL,-,Extwall_ti,OPAQUE,EXTERIOR,0,0  #   3 ||< external
*surf,ptn_bath,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,04,08  #   4 ||< ptn_bath:low_kitchen
*surf,ptn_hall_a,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,07,08  #   5 ||< ptn_a:lowliving
*surf,door_hall,VERT,-,-,-,Doorint_sol,OPAQUE,ANOTHER,05,01  #   6 ||< door_bath:low_hall
*surf,ptn_hall_b,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,05,15  #   7 ||< ptn_bath_b:low_hall
*surf,ptn_hall_c,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,05,14  #   8 ||< ptn_bath_a:low_hall
*surf,ptn_store,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,02,02  #   9 ||< ptn_bath:store2
*surf,bath_ceil,CEIL,-,-,-,Ceilti_up,OPAQUE,ANOTHER,08,02  #  10 ||< ovr_bath:roof
*surf,bath_floor,FLOR,-,-,-,Floorti_low,OPAQUE,ANOTHER,15,03  #  11 ||< bath_floor:crawl
*surf,frame,VERT,-,F-FRAME,CLOSED,Window_frme,OPAQUE,EXTERIOR,0,0  #  12 ||< external
*surf,sill,VERT,-,WALL,-,Extwall_ti,OPAQUE,EXTERIOR,0,0  #  13 ||< external
*surf,glazing,VERT,frame,C-WINDOW,CLOSED,Window_ext,DC_8074_04nb,EXTERIOR,0,0  #  14 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,11,     4.60 0  # zone base list
