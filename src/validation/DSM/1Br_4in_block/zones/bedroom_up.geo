*Geometry 1.1,GEN,bedroom_up # tag version, format, zone name
*date Mon Mar 12 10:24:05 2012  # latest file modification 
bedroom_up describes upper flat bedroom
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,5.50000,4.40000,2.60000  #   1
*vertex,6.10000,4.40000,2.60000  #   2
*vertex,6.10000,7.30000,2.60000  #   3
*vertex,5.40000,7.30000,2.60000  #   4
*vertex,4.70000,7.30000,2.60000  #   5
*vertex,2.80000,7.30000,2.60000  #   6
*vertex,2.80000,5.70000,2.60000  #   7
*vertex,2.60000,5.70000,2.60000  #   8
*vertex,2.60000,4.40000,2.60000  #   9
*vertex,2.60000,3.50000,2.60000  #  10
*vertex,4.10000,3.50000,2.60000  #  11
*vertex,4.10000,4.40000,2.60000  #  12
*vertex,4.60000,4.40000,2.60000  #  13
*vertex,5.50000,4.40000,4.80000  #  14
*vertex,6.10000,4.40000,4.80000  #  15
*vertex,6.10000,7.30000,4.80000  #  16
*vertex,5.40000,7.30000,4.80000  #  17
*vertex,4.70000,7.30000,4.80000  #  18
*vertex,2.80000,7.30000,4.80000  #  19
*vertex,2.80000,5.70000,4.80000  #  20
*vertex,2.60000,5.70000,4.80000  #  21
*vertex,2.60000,4.40000,4.80000  #  22
*vertex,2.60000,3.50000,4.80000  #  23
*vertex,4.10000,3.50000,4.80000  #  24
*vertex,4.10000,4.40000,4.80000  #  25
*vertex,4.60000,4.40000,4.80000  #  26
*vertex,5.40000,7.30000,4.50000  #  27
*vertex,4.70000,7.30000,4.50000  #  28
*vertex,5.40000,7.30000,3.15000  #  29
*vertex,4.70000,7.30000,3.15000  #  30
*vertex,5.36000,7.30000,3.19000  #  31
*vertex,4.74000,7.30000,3.19000  #  32
*vertex,4.74000,7.30000,4.46000  #  33
*vertex,5.36000,7.30000,4.46000  #  34
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,15,14  #  1
*edges,4,2,3,16,15  #  2
*edges,6,3,4,29,27,17,16  #  3
*edges,4,27,28,18,17  #  4
*edges,6,5,6,19,18,28,30  #  5
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
*edges,10,29,30,28,27,29,31,34,33,32,31  # 16
*edges,4,4,5,30,29  # 17
*edges,4,31,32,33,34  # 18
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,ptn_hall_a,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,11,03  #   1 ||< pth_br_a:hall_up
*surf,ptn_liv,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,13,06  #   2 ||< ptn_br:living_up
*surf,bk_fac_a,VERT,-,WALL,-,Extwall_ti,OPAQUE,EXTERIOR,0,0  #   3 ||< external
*surf,head,VERT,-,WALL,-,Extwall_ti,OPAQUE,EXTERIOR,0,0  #   4 ||< external
*surf,bk_fac_b,VERT,-,WALL,-,Extwall_ti,OPAQUE,EXTERIOR,0,0  #   5 ||< external
*surf,lft_fac_a,VERT,-,WALL,-,Extwall_ti,OPAQUE,EXTERIOR,0,0  #   6 ||< external
*surf,lft_fac_b,VERT,-,WALL,-,Extwall_ti,OPAQUE,EXTERIOR,0,0  #   7 ||< external
*surf,ptn_stair_c,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,01,07  #   8 ||< ptn_upbrb:stairs
*surf,ptn_stair_d,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,01,06  #   9 ||< ptn_upbrward:stairs
*surf,ptn_hall_e,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,11,07  #  10 ||< ptn_br_d:hall_up
*surf,ptn_hall_d,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,11,06  #  11 ||< ptn_br_c:hall_up
*surf,ptn_hall_c,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,11,05  #  12 ||< ptn_br_b:hall_up
*surf,door_hall,VERT,-,-,-,Doorint_sol,OPAQUE,ANOTHER,11,04  #  13 ||< door_br:hall_up
*surf,br_ceiling,CEIL,-,-,-,Ceilti_up,OPAQUE,ANOTHER,14,05  #  14 ||< ovr_bedroom:roof
*surf,br_floor,FLOR,-,-,-,Floorti_upc,OPAQUE,ANOTHER,06,14  #  15 ||< br_ceiling:low_bedroom
*surf,frame,VERT,-,F-FRAME,CLOSED,Window_frme,OPAQUE,EXTERIOR,0,0  #  16 ||< external
*surf,sill,VERT,-,WALL,-,Extwall_ti,OPAQUE,EXTERIOR,0,0  #  17 ||< external
*surf,glazing,VERT,frame,C-WINDOW,CLOSED,Window_ext,DC_8074_04nb,EXTERIOR,0,0  #  18 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,15,    11.18 0  # zone base list
