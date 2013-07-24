*Geometry 1.1,GEN,bathup # tag version, format, zone name
*date Mon Mar 12 10:11:26 2012  # latest file modification 
bathup describes the bathroom in the upper flat
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,4.50000,0.30000,2.60000  #   1
*vertex,4.70000,0.30000,2.60000  #   2
*vertex,5.40000,0.30000,2.60000  #   3
*vertex,6.50000,0.30000,2.60000  #   4
*vertex,6.50000,2.60000,2.60000  #   5
*vertex,6.10000,2.60000,2.60000  #   6
*vertex,5.50000,2.60000,2.60000  #   7
*vertex,4.70000,2.60000,2.60000  #   8
*vertex,4.50000,2.60000,2.60000  #   9
*vertex,4.50000,0.30000,4.80000  #  10
*vertex,4.70000,0.30000,4.80000  #  11
*vertex,5.40000,0.30000,4.80000  #  12
*vertex,6.50000,0.30000,4.80000  #  13
*vertex,6.50000,2.60000,4.80000  #  14
*vertex,6.10000,2.60000,4.80000  #  15
*vertex,5.50000,2.60000,4.80000  #  16
*vertex,4.70000,2.60000,4.80000  #  17
*vertex,4.50000,2.60000,4.80000  #  18
*vertex,4.70000,0.30000,4.50000  #  19
*vertex,5.40000,0.30000,4.50000  #  20
*vertex,4.70000,0.30000,3.70000  #  21
*vertex,5.40000,0.30000,3.70000  #  22
*vertex,4.74000,0.30000,3.74000  #  23
*vertex,5.36000,0.30000,3.74000  #  24
*vertex,5.36000,0.30000,4.46000  #  25
*vertex,4.74000,0.30000,4.46000  #  26
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
*surf,fr_facade_a,VERT,-,WALL,-,Extwall_ti,OPAQUE,EXTERIOR,0,0  #   1 ||< external
*surf,head,VERT,-,WALL,-,Extwall_ti,OPAQUE,EXTERIOR,0,0  #   2 ||< external
*surf,fr_facade_b,VERT,-,WALL,-,Extwall_ti,OPAQUE,EXTERIOR,0,0  #   3 ||< external
*surf,ptn_kitch,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,10,08  #   4 ||< ptn_bath:kitchenup
*surf,ptn_liv_a,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,13,08  #   5 ||< ptn_bath_a:living_up
*surf,ptn_hall_b,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,11,01  #   6 ||< ptn_bath_a:hall_up
*surf,door_hall,VERT,-,-,-,Doorint_sol,OPAQUE,ANOTHER,11,15  #   7 ||< door_bath:hall_up
*surf,ptn_hall_c,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,11,14  #   8 ||< ptn_bath_b:hall_up
*surf,ptn_store,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,08,02  #   9 ||< ptn_bath:storeup
*surf,bath_ceil,CEIL,-,-,-,Ceilti_up,OPAQUE,ANOTHER,14,02  #  10 ||< ovr_bath:roof
*surf,bath_floor,FLOR,-,-,-,Floorti_up,OPAQUE,ANOTHER,03,10  #  11 ||< bath_ceil:bathroom
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
