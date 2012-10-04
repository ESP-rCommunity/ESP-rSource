*Geometry 1.1,GEN,BR_two # tag version, format, zone name
*date Wed Apr 18 18:08:33 2012  # latest file modification 
BR_two describes small bedroom adj to bath and hall
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,0.00000,2.00000,0.20000  #   1
*vertex,0.00000,0.00000,0.20000  #   2
*vertex,2.10000,0.00000,0.20000  #   3
*vertex,2.90000,0.00000,0.20000  #   4
*vertex,3.10000,0.00000,0.20000  #   5
*vertex,3.10000,2.00000,0.20000  #   6
*vertex,2.90000,2.00000,0.20000  #   7
*vertex,2.00000,2.00000,0.20000  #   8
*vertex,0.90000,2.00000,0.20000  #   9
*vertex,0.00000,2.00000,2.40000  #  10
*vertex,0.00000,0.00000,2.40000  #  11
*vertex,2.10000,0.00000,2.40000  #  12
*vertex,2.90000,0.00000,2.40000  #  13
*vertex,3.10000,0.00000,2.40000  #  14
*vertex,3.10000,2.00000,2.40000  #  15
*vertex,2.90000,2.00000,2.40000  #  16
*vertex,2.00000,2.00000,2.40000  #  17
*vertex,0.90000,2.00000,2.40000  #  18
*vertex,2.10000,0.00000,0.75000  #  19
*vertex,2.90000,0.00000,0.75000  #  20
*vertex,2.10000,0.00000,2.10000  #  21
*vertex,2.90000,0.00000,2.10000  #  22
*vertex,2.14000,0.00000,0.79000  #  23
*vertex,2.86000,0.00000,0.79000  #  24
*vertex,2.86000,0.00000,2.06000  #  25
*vertex,2.14000,0.00000,2.06000  #  26
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,11,10  #  1
*edges,6,2,3,19,21,12,11  #  2
*edges,4,3,4,20,19  #  3
*edges,6,4,5,14,13,22,20  #  4
*edges,4,5,6,15,14  #  5
*edges,4,6,7,16,15  #  6
*edges,4,7,8,17,16  #  7
*edges,4,8,9,18,17  #  8
*edges,4,9,1,10,18  #  9
*edges,9,10,11,12,13,14,15,16,17,18  # 10
*edges,9,1,9,8,7,6,5,4,3,2  # 11
*edges,4,21,22,13,12  # 12
*edges,10,19,20,22,21,19,23,26,25,24,23  # 13
*edges,4,23,24,25,26  # 14
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,left_fac,VERT,-,WALL,-,tk_block_rn,OPAQUE,EXTERIOR,0,0  #   1 ||< external
*surf,fr_fac_a,VERT,-,WALL,-,tk_block_rn,OPAQUE,EXTERIOR,0,0  #   2 ||< external
*surf,sill_v,VERT,-,WALL,-,tk_block_rn,OPAQUE,EXTERIOR,0,0  #   3 ||< external
*surf,fr_fac_b,VERT,-,WALL,-,tk_block_rn,OPAQUE,EXTERIOR,0,0  #   4 ||< external
*surf,ptn_bath,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,07,07  #   5 ||< ptn_br2:bath
*surf,ptn_hall_a,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,02,02  #   6 ||< ptn_br2:hall
*surf,door_hall,VERT,-,-,-,Doorint_sol,OPAQUE,ANOTHER,02,01  #   7 ||< door_br2:hall
*surf,ptn_vest_a,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,03,01  #   8 ||< ptn_br2_a:lobby_stor
*surf,ptn_vest_b,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,03,10  #   9 ||< ptn_br2_b:lobby_stor
*surf,br2_ceiling,CEIL,-,-,-,Ceiltk_up,OPAQUE,ANOTHER,09,01  #  10 ||< br2_ceiling:roof
*surf,br2_floor,FLOR,-,-,-,Floorti_lowc,OPAQUE,ANOTHER,10,01  #  11 ||< br2_floor:crawl
*surf,head,VERT,-,WALL,-,tk_block_rn,OPAQUE,EXTERIOR,0,0  #  12 ||< external
*surf,frame,VERT,-,F-FRAME,CLOSED,Window_frme,OPAQUE,EXTERIOR,0,0  #  13 ||< external
*surf,glazing,VERT,frame,C-WINDOW,CLOSED,Window_ext,DC_8074_04nb,EXTERIOR,0,0  #  14 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,11,     6.20 0  # zone base list
