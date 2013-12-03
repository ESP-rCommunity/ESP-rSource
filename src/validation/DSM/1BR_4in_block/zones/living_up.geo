*Geometry 1.1,GEN,living_up # tag version, format, zone name
*date Mon Mar 12 10:24:31 2012  # latest file modification 
living_up describes upper flat living and dining
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,8.60000,2.60000,2.60000  #   1
*vertex,9.60000,2.60000,2.60000  #   2
*vertex,9.60000,7.30000,2.60000  #   3
*vertex,8.60000,7.30000,2.60000  #   4
*vertex,7.00000,7.30000,2.60000  #   5
*vertex,6.10000,7.30000,2.60000  #   6
*vertex,6.10000,4.40000,2.60000  #   7
*vertex,6.10000,2.60000,2.60000  #   8
*vertex,6.50000,2.60000,2.60000  #   9
*vertex,7.40000,2.60000,2.60000  #  10
*vertex,8.60000,2.60000,4.80000  #  11
*vertex,9.60000,2.60000,4.80000  #  12
*vertex,9.60000,7.30000,4.80000  #  13
*vertex,8.60000,7.30000,4.80000  #  14
*vertex,7.00000,7.30000,4.80000  #  15
*vertex,6.10000,7.30000,4.80000  #  16
*vertex,6.10000,4.40000,4.80000  #  17
*vertex,6.10000,2.60000,4.80000  #  18
*vertex,6.50000,2.60000,4.80000  #  19
*vertex,7.40000,2.60000,4.80000  #  20
*vertex,8.60000,7.30000,4.50000  #  21
*vertex,7.00000,7.30000,4.50000  #  22
*vertex,8.60000,7.30000,3.00000  #  23
*vertex,7.00000,7.30000,3.00000  #  24
*vertex,8.56000,7.30000,3.04000  #  25
*vertex,7.04000,7.30000,3.04000  #  26
*vertex,7.04000,7.30000,4.46000  #  27
*vertex,8.56000,7.30000,4.46000  #  28
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,12,11  #  1
*edges,4,2,3,13,12  #  2
*edges,6,3,4,23,21,14,13  #  3
*edges,4,21,22,15,14  #  4
*edges,6,5,6,16,15,22,24  #  5
*edges,4,6,7,17,16  #  6
*edges,4,7,8,18,17  #  7
*edges,4,8,9,19,18  #  8
*edges,4,9,10,20,19  #  9
*edges,4,10,1,11,20  # 10
*edges,10,11,12,13,14,15,16,17,18,19,20  # 11
*edges,10,1,10,9,8,7,6,5,4,3,2  # 12
*edges,10,23,24,22,21,23,25,28,27,26,25  # 13
*edges,4,4,5,24,23  # 14
*edges,4,25,26,27,28  # 15
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,ptn_kitch_a,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,10,05  #   1 ||< ptn_liv_a:kitchenup
*surf,right_other,VERT,-,WALL,-,Partywall_ti,OPAQUE,EXTERIOR,0,0  #   2 ||< external
*surf,bk_fac_a,VERT,-,WALL,-,Extwall_ti,OPAQUE,EXTERIOR,0,0  #   3 ||< external
*surf,head,VERT,-,WALL,-,Extwall_ti,OPAQUE,EXTERIOR,0,0  #   4 ||< external
*surf,bk_fac_b,VERT,-,WALL,-,Extwall_ti,OPAQUE,EXTERIOR,0,0  #   5 ||< external
*surf,ptn_br,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,12,02  #   6 ||< ptn_liv:bedroom_up
*surf,ptn_hall,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,11,02  #   7 ||< ptn_liv:hall_up
*surf,ptn_bath_a,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,09,05  #   8 ||< ptn_liv_a:bathup
*surf,ptn_kit_b,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,10,07  #   9 ||< ptn_liv_b:kitchenup
*surf,door_kit,VERT,-,-,-,Doorint_sol,OPAQUE,ANOTHER,10,06  #  10 ||< door_liv:kitchenup
*surf,liv_ceiling,CEIL,-,-,-,Ceilti_up,OPAQUE,ANOTHER,14,06  #  11 ||< ovr_living:roof
*surf,liv_floor,FLOR,-,-,-,Floorti_upc,OPAQUE,ANOTHER,07,11  #  12 ||< living_ceil:lowliving
*surf,frame,VERT,-,F-FRAME,CLOSED,Window_frme,OPAQUE,EXTERIOR,0,0  #  13 ||< external
*surf,sill,VERT,-,WALL,-,Extwall_ti,OPAQUE,EXTERIOR,0,0  #  14 ||< external
*surf,glazing,VERT,frame,C-WINDOW,CLOSED,Window_ext,DC_8074_04nb,EXTERIOR,0,0  #  15 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,12,    16.45 0  # zone base list
