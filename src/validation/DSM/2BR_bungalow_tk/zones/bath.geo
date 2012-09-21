*Geometry 1.1,GEN,bath # tag version, format, zone name
*date Wed Apr 18 18:08:57 2012  # latest file modification 
bath describes the bathroom between BR2 and kithen
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,3.10000,0.00000,0.20000  #   1
*vertex,3.30000,0.00000,0.20000  #   2
*vertex,4.00000,0.00000,0.20000  #   3
*vertex,5.10000,0.00000,0.20000  #   4
*vertex,5.10000,2.00000,0.20000  #   5
*vertex,4.10000,2.00000,0.20000  #   6
*vertex,3.10000,2.00000,0.20000  #   7
*vertex,3.10000,0.00000,2.40000  #   8
*vertex,3.30000,0.00000,2.40000  #   9
*vertex,4.00000,0.00000,2.40000  #  10
*vertex,5.10000,0.00000,2.40000  #  11
*vertex,5.10000,2.00000,2.40000  #  12
*vertex,4.10000,2.00000,2.40000  #  13
*vertex,3.10000,2.00000,2.40000  #  14
*vertex,3.30000,0.00000,1.30000  #  15
*vertex,4.00000,0.00000,1.30000  #  16
*vertex,3.30000,0.00000,2.10000  #  17
*vertex,4.00000,0.00000,2.10000  #  18
*vertex,3.34000,0.00000,1.34000  #  19
*vertex,3.96000,0.00000,1.34000  #  20
*vertex,3.96000,0.00000,2.06000  #  21
*vertex,3.34000,0.00000,2.06000  #  22
# 
# tag, number of vertices followed by list of associated vert
*edges,6,1,2,15,17,9,8  #  1
*edges,4,2,3,16,15  #  2
*edges,6,3,4,11,10,18,16  #  3
*edges,4,4,5,12,11  #  4
*edges,4,5,6,13,12  #  5
*edges,4,6,7,14,13  #  6
*edges,4,7,1,8,14  #  7
*edges,7,8,9,10,11,12,13,14  #  8
*edges,7,1,7,6,5,4,3,2  #  9
*edges,4,17,18,10,9  # 10
*edges,10,15,16,18,17,15,19,22,21,20,19  # 11
*edges,4,19,20,21,22  # 12
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,facade_a,VERT,-,WALL,-,tk_block_rn,OPAQUE,EXTERIOR,0,0  #   1 ||< external
*surf,sill_v,VERT,-,WALL,-,tk_block_rn,OPAQUE,EXTERIOR,0,0  #   2 ||< external
*surf,facade_b,VERT,-,WALL,-,tk_block_rn,OPAQUE,EXTERIOR,0,0  #   3 ||< external
*surf,ptn_kit,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,08,08  #   4 ||< ptn_bath:kitchen
*surf,ptn_liv,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,06,01  #   5 ||< ptn_bath:livingdin
*surf,ptn_hall,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,02,03  #   6 ||< ptn_bath:hall
*surf,ptn_br2,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,01,05  #   7 ||< ptn_bath:BR_two
*surf,bath_ceil,CEIL,-,-,-,Ceiltk_up,OPAQUE,ANOTHER,09,07  #   8 ||< bath_ceil:roof
*surf,bath_floor,FLOR,-,-,-,Floorti_low,OPAQUE,ANOTHER,10,07  #   9 ||< bath_floor:crawl
*surf,head,VERT,-,WALL,-,tk_block_rn,OPAQUE,EXTERIOR,0,0  #  10 ||< external
*surf,frame,VERT,-,F-FRAME,CLOSED,Window_frme,OPAQUE,EXTERIOR,0,0  #  11 ||< external
*surf,glazing,VERT,frame,C-WINDOW,CLOSED,Window_ext,DC_8074_04nb,EXTERIOR,0,0  #  12 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,9,     4.00 0  # zone base list
