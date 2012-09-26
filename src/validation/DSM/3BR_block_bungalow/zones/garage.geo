*Geometry 1.1,GEN,garage # tag version, format, zone name
*date Sun Apr  8 09:33:30 2012  # latest file modification 
garage describes a small single vehicle garage
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,-4.00000,0.20000,0.20000  #   1
*vertex,-1.60000,0.20000,0.20000  #   2
*vertex,-4.00000,5.40000,0.20000  #   3
*vertex,-4.00000,0.20000,2.30000  #   4
*vertex,-1.60000,0.20000,2.60000  #   5
*vertex,-4.00000,5.40000,2.30000  #   6
*vertex,-1.60000,5.40000,0.20000  #   7
*vertex,-1.60000,2.80000,0.20000  #   8
*vertex,-1.60000,2.80000,2.60000  #   9
*vertex,-1.60000,3.90000,2.60000  #  10
*vertex,-1.60000,5.40000,2.60000  #  11
*vertex,-3.80000,0.20000,0.20000  #  12
*vertex,-1.80000,0.20000,0.20000  #  13
*vertex,-1.80000,0.20000,2.20000  #  14
*vertex,-3.80000,0.20000,2.20000  #  15
*vertex,-4.00000,4.60000,1.10000  #  16
*vertex,-4.00000,3.60000,1.10000  #  17
*vertex,-4.00000,3.60000,2.10000  #  18
*vertex,-4.00000,4.60000,2.10000  #  19
*vertex,-4.00000,4.52000,1.18000  #  20
*vertex,-4.00000,3.68000,1.18000  #  21
*vertex,-4.00000,3.68000,2.02000  #  22
*vertex,-4.00000,4.52000,2.02000  #  23
# 
# tag, number of vertices followed by list of associated vert
*edges,8,1,12,15,14,13,2,5,4  #  1
*edges,4,2,8,9,5  #  2
*edges,4,7,3,6,11  #  3
*edges,10,3,1,4,6,3,16,19,18,17,16  #  4
*edges,6,4,5,9,10,11,6  #  5
*edges,7,1,3,7,8,2,13,12  #  6
*edges,5,8,7,11,10,9  #  7
*edges,4,12,13,14,15  #  8
*edges,10,16,17,18,19,16,20,23,22,21,20  #  9
*edges,4,20,21,22,23  # 10
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,front,VERT,-,-,-,conc_blck_rn,OPAQUE,EXTERIOR,0,0  #   1 ||< external
*surf,ptn_kit,VERT,-,-,-,intcnc_block,OPAQUE,ANOTHER,12,01  #   2 ||< ptn_other_c:kit_dining
*surf,back,VERT,-,-,-,conc_blck_rn,OPAQUE,EXTERIOR,0,0  #   3 ||< external
*surf,left,VERT,-,-,-,conc_blck_rn,OPAQUE,EXTERIOR,0,0  #   4 ||< external
*surf,roof,SLOP,-,-,-,Roof,OPAQUE,EXTERIOR,0,0  #   5 ||< external
*surf,base,FLOR,-,-,-,ground,OPAQUE,GROUND,01,0  #   6 ||< ground profile  1
*surf,ptn_other_b,VERT,-,-,-,intcnc_block,OPAQUE,ANOTHER,03,05  #   7 ||< ptn_other_b:living
*surf,door,VERT,front,V-DOOR,CLOSED,Door_fire,OPAQUE,EXTERIOR,0,0  #   8 ||< external
*surf,frame,VERT,left,FRAME,CLOSED,Window_frme,OPAQUE,EXTERIOR,0,0  #   9 ||< external
*surf,glazing,VERT,frame,-,-,Window_ext,TRAN,EXTERIOR,0,0  #  10 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,0,12.48,0  # zone base
