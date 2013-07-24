*Geometry 1.1,GEN,BR_one # tag version, format, zone name
*date Mon Mar 12 15:36:06 2012  # latest file modification 
BR_one describes the larger corner bedroom
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,0.00000,3.80000,0.20000  #   1
*vertex,2.00000,3.80000,0.20000  #   2
*vertex,2.40000,3.80000,0.20000  #   3
*vertex,3.20000,3.80000,0.20000  #   4
*vertex,3.40000,3.80000,0.20000  #   5
*vertex,3.40000,5.10000,0.20000  #   6
*vertex,3.40000,6.60000,0.20000  #   7
*vertex,3.20000,6.60000,0.20000  #   8
*vertex,2.50000,6.60000,0.20000  #   9
*vertex,0.00000,6.60000,0.20000  #  10
*vertex,0.00000,4.70000,0.20000  #  11
*vertex,0.00000,3.80000,2.40000  #  12
*vertex,2.00000,3.80000,2.40000  #  13
*vertex,2.40000,3.80000,2.40000  #  14
*vertex,3.20000,3.80000,2.40000  #  15
*vertex,3.40000,3.80000,2.40000  #  16
*vertex,3.40000,5.10000,2.40000  #  17
*vertex,3.40000,6.60000,2.40000  #  18
*vertex,3.20000,6.60000,2.40000  #  19
*vertex,2.50000,6.60000,2.40000  #  20
*vertex,0.00000,6.60000,2.40000  #  21
*vertex,0.00000,4.70000,2.40000  #  22
*vertex,3.20000,6.60000,0.75000  #  23
*vertex,2.50000,6.60000,0.75000  #  24
*vertex,3.20000,6.60000,2.10000  #  25
*vertex,2.50000,6.60000,2.10000  #  26
*vertex,3.16000,6.60000,0.79000  #  27
*vertex,2.54000,6.60000,0.79000  #  28
*vertex,2.54000,6.60000,2.06000  #  29
*vertex,3.16000,6.60000,2.06000  #  30
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,13,12  #  1
*edges,4,2,3,14,13  #  2
*edges,4,3,4,15,14  #  3
*edges,4,4,5,16,15  #  4
*edges,4,5,6,17,16  #  5
*edges,4,6,7,18,17  #  6
*edges,6,7,8,23,25,19,18  #  7
*edges,4,8,9,24,23  #  8
*edges,6,9,10,21,20,26,24  #  9
*edges,4,10,11,22,21  # 10
*edges,4,11,1,12,22  # 11
*edges,11,12,13,14,15,16,17,18,19,20,21,22  # 12
*edges,11,1,11,10,9,8,7,6,5,4,3,2  # 13
*edges,4,25,26,20,19  # 14
*edges,10,23,24,26,25,23,27,30,29,28,27  # 15
*edges,4,27,28,29,30  # 16
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,ptn_vest,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,03,05  #   1 ||< ptn_br1:lobby_stor
*surf,ptn_hall_a,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,02,10  #   2 ||< ptn_br1_b:hall
*surf,door_hall,VERT,-,-,-,Doorint_sol,OPAQUE,ANOTHER,02,09  #   3 ||< door_br1:hall
*surf,ptn_hall_b,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,02,08  #   4 ||< ptn_br1_a:hall
*surf,ptn_store_a,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,05,06  #   5 ||< ptn_br1_b:storage
*surf,ptn_store_b,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,05,05  #   6 ||< ptn_br1_a:storage
*surf,bk_fac_a,VERT,-,WALL,-,Extwall_ti,OPAQUE,EXTERIOR,0,0  #   7 ||< external
*surf,sill_v,VERT,-,WALL,-,Extwall_ti,OPAQUE,EXTERIOR,0,0  #   8 ||< external
*surf,bk_fac_b,VERT,-,WALL,-,Extwall_ti,OPAQUE,EXTERIOR,0,0  #   9 ||< external
*surf,left_fac_a,VERT,-,WALL,-,Extwall_ti,OPAQUE,EXTERIOR,0,0  #  10 ||< external
*surf,left_fac_b,VERT,-,WALL,-,Extwall_ti,OPAQUE,EXTERIOR,0,0  #  11 ||< external
*surf,br1_ceiling,CEIL,-,-,-,Ceilti_up,OPAQUE,ANOTHER,09,04  #  12 ||< br1_ceiling:roof
*surf,br1_floor,FLOR,-,-,-,Floorti_lowc,OPAQUE,ANOTHER,10,04  #  13 ||< br1_floor:crawl
*surf,head,VERT,-,WALL,-,Extwall_ti,OPAQUE,EXTERIOR,0,0  #  14 ||< external
*surf,frame,VERT,-,F-FRAME,CLOSED,Window_frme,OPAQUE,EXTERIOR,0,0  #  15 ||< external
*surf,glazing,VERT,frame,C-WINDOW,CLOSED,Window_ext,DC_8074_04nb,EXTERIOR,0,0  #  16 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,13,     9.52 0  # zone base list
# 
# block entities:
#  *obs = obstructions
*block_start, 20 20 # geometric blocks
*obs,-1.200,4.700,0.300,1.000,0.100,2.100,0.000,1.00,ref_wall,door  # block   1
*obs,-1.200,2.700,2.400,1.000,2.100,0.100,0.000,1.00,ref_ovh,door  # block   2
*obs,-1.200,2.700,0.300,1.000,2.100,0.100,0.000,1.00,ref_base,door  # block   3
*end_block
