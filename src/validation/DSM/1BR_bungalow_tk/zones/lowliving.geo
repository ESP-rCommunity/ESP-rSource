*Geometry 1.1,GEN,lowliving # tag version, format, zone name
*date Mon Apr 16 19:33:28 2012  # latest file modification 
lowliving describes living dining of lower flat
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,8.60000,2.60000,0.20000  #   1
*vertex,9.60000,2.60000,0.20000  #   2
*vertex,9.60000,7.30000,0.20000  #   3
*vertex,8.60000,7.30000,0.20000  #   4
*vertex,7.00000,7.30000,0.20000  #   5
*vertex,6.10000,7.30000,0.20000  #   6
*vertex,6.10000,4.40000,0.20000  #   7
*vertex,6.10000,2.60000,0.20000  #   8
*vertex,6.50000,2.60000,0.20000  #   9
*vertex,7.40000,2.60000,0.20000  #  10
*vertex,8.60000,2.60000,2.40000  #  11
*vertex,9.60000,2.60000,2.40000  #  12
*vertex,9.60000,7.30000,2.40000  #  13
*vertex,8.60000,7.30000,2.40000  #  14
*vertex,7.00000,7.30000,2.40000  #  15
*vertex,6.10000,7.30000,2.40000  #  16
*vertex,6.10000,4.40000,2.40000  #  17
*vertex,6.10000,2.60000,2.40000  #  18
*vertex,6.50000,2.60000,2.40000  #  19
*vertex,7.40000,2.60000,2.40000  #  20
*vertex,8.60000,7.30000,2.10000  #  21
*vertex,7.00000,7.30000,2.10000  #  22
*vertex,8.60000,7.30000,0.52500  #  23
*vertex,7.00000,7.30000,0.52500  #  24
*vertex,8.56000,7.30000,0.56500  #  25
*vertex,7.04000,7.30000,0.56500  #  26
*vertex,7.04000,7.30000,2.06000  #  27
*vertex,8.56000,7.30000,2.06000  #  28
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
*surf,ptn_kitch_a,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,04,05  #   1 ||< ptn_liv_a:low_kitchen
*surf,right_other,VERT,-,WALL,-,Partywall_ti,OPAQUE,SIMILAR,00,00  #   2 ||< identical environment
*surf,bk_fac_a,VERT,-,WALL,-,tk_block_rn,OPAQUE,EXTERIOR,0,0  #   3 ||< external
*surf,head,VERT,-,WALL,-,tk_block_rn,OPAQUE,EXTERIOR,0,0  #   4 ||< external
*surf,bk_fac_b,VERT,-,WALL,-,tk_block_rn,OPAQUE,EXTERIOR,0,0  #   5 ||< external
*surf,ptn_br,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,06,02  #   6 ||< ptn_living:low_bedroom
*surf,ptn_hall,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,05,02  #   7 ||< ptn_liv:low_hall
*surf,ptn_a,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,03,05  #   8 ||< ptn_hall_a:bathroom
*surf,ptn_b,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,04,07  #   9 ||< ptn_liv_b:low_kitchen
*surf,door_kit,VERT,-,-,-,Doorint_sol,OPAQUE,ANOTHER,04,06  #  10 ||< door_liv:low_kitchen
*surf,living_ceil,CEIL,-,-,-,Ceilti_up,OPAQUE,ANOTHER,08,06  #  11 ||< ovr_living:roof
*surf,liv_floor,FLOR,-,-,-,Floorti_lowc,OPAQUE,ANOTHER,15,07  #  12 ||< liv_floor:crawl
*surf,frame,VERT,-,F-FRAME,CLOSED,Window_frme,OPAQUE,EXTERIOR,0,0  #  13 ||< external
*surf,sill,VERT,-,WALL,-,tk_block_rn,OPAQUE,EXTERIOR,0,0  #  14 ||< external
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
