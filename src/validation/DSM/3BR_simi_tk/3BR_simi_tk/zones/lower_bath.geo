*Geometry 1.1,GEN,lower_bath # tag version, format, zone name
*date Wed Apr 18 07:35:32 2012  # latest file modification 
lower_bath describes main bathroom of house adj entrance
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,4.50000,0.20000,0.20000  #   1
*vertex,6.50000,0.20000,0.20000  #   2
*vertex,6.50000,1.20000,0.20000  #   3
*vertex,6.50000,1.80000,0.20000  #   4
*vertex,6.50000,2.40000,0.20000  #   5
*vertex,5.50000,2.40000,0.20000  #   6
*vertex,4.70000,2.40000,0.20000  #   7
*vertex,4.50000,2.40000,0.20000  #   8
*vertex,4.50000,0.20000,2.60000  #   9
*vertex,6.50000,0.20000,2.60000  #  10
*vertex,6.50000,1.20000,2.60000  #  11
*vertex,6.50000,1.80000,2.60000  #  12
*vertex,6.50000,2.40000,2.60000  #  13
*vertex,5.50000,2.40000,2.60000  #  14
*vertex,4.70000,2.40000,2.60000  #  15
*vertex,4.50000,2.40000,2.60000  #  16
*vertex,6.50000,1.20000,1.30000  #  17
*vertex,6.50000,1.80000,1.30000  #  18
*vertex,6.50000,1.20000,2.30000  #  19
*vertex,6.50000,1.80000,2.30000  #  20
*vertex,6.50000,1.24000,1.34000  #  21
*vertex,6.50000,1.76000,1.34000  #  22
*vertex,6.50000,1.76000,2.26000  #  23
*vertex,6.50000,1.24000,2.26000  #  24
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,10,9  #  1
*edges,6,2,3,17,19,11,10  #  2
*edges,4,3,4,18,17  #  3
*edges,6,4,5,13,12,20,18  #  4
*edges,4,5,6,14,13  #  5
*edges,4,6,7,15,14  #  6
*edges,4,7,8,16,15  #  7
*edges,4,8,1,9,16  #  8
*edges,8,9,10,11,12,13,14,15,16  #  9
*edges,8,1,8,7,6,5,4,3,2  # 10
*edges,4,19,20,12,11  # 11
*edges,10,17,18,20,19,17,21,24,23,22,21  # 12
*edges,4,21,22,23,24  # 13
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,ptn_other,VERT,-,-,-,Partywall_ti,OPAQUE,EXTERIOR,0,0  #   1 ||< external
*surf,fac_a,VERT,-,WALL,-,tk_block_rn,OPAQUE,EXTERIOR,0,0  #   2 ||< external
*surf,sill_v,VERT,-,WALL,-,tk_block_rn,OPAQUE,EXTERIOR,0,0  #   3 ||< external
*surf,fac_b,VERT,-,WALL,-,tk_block_rn,OPAQUE,EXTERIOR,0,0  #   4 ||< external
*surf,ptn_ent_a,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,02,03  #   5 ||< ptn_bath_a:entry
*surf,door_ent,VERT,-,-,-,Doorint_sol,OPAQUE,ANOTHER,02,02  #   6 ||< door_bath:entry
*surf,ptn_ent_b,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,02,01  #   7 ||< ptn_bath_b:entry
*surf,ptn_liv,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,03,13  #   8 ||< ptn_bath:living
*surf,lbath_ceil,CEIL,-,-,-,Ceilti_lowc,OPAQUE,ANOTHER,07,06  #   9 ||< br1_floor:up_br_one
*surf,lbath_floor,FLOR,-,-,-,Floorti_low,OPAQUE,ANOTHER,11,01  #  10 ||< lbath_floor:crawl
*surf,head,VERT,-,WALL,-,tk_block_rn,OPAQUE,EXTERIOR,0,0  #  11 ||< external
*surf,frame,VERT,-,F-FRAME,CLOSED,Window_frme,OPAQUE,EXTERIOR,0,0  #  12 ||< external
*surf,glazing,VERT,frame,-,-,Window_ext,DC_8074_04nb,EXTERIOR,0,0  #  13 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,10,     4.40 0  # zone base list
