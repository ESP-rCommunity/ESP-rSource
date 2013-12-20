*Geometry 1.1,GEN,office-e # tag version, format, zone name
*date Sun Dec  8 15:36:45 2013  # latest file modification 
office-e describes a...
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,14.50000,5.50000,30.00000  #   1
*vertex,14.49999,10.00000,30.00000  #   2
*vertex,10.00000,10.00000,30.00000  #   3
*vertex,10.00000,5.50000,30.00000  #   4
*vertex,14.50000,5.50000,33.00000  #   5
*vertex,14.49999,10.00000,33.00000  #   6
*vertex,10.00000,10.00000,33.00000  #   7
*vertex,10.00000,5.50000,33.00000  #   8
*vertex,14.50000,5.50000,31.00000  #   9
*vertex,14.49999,10.00000,31.00000  #  10
*vertex,14.50000,5.50000,31.10000  #  11
*vertex,14.50000,5.50000,32.90000  #  12
*vertex,14.50000,10.00000,31.10000  #  13
*vertex,14.50000,10.00000,32.90000  #  14
*vertex,14.50000,5.60000,31.10000  #  15
*vertex,14.50000,6.60000,31.10000  #  16
*vertex,14.50000,5.60000,32.90000  #  17
*vertex,14.50000,6.60000,32.90000  #  18
*vertex,14.50000,6.70000,31.10000  #  19
*vertex,14.50000,7.70000,31.10000  #  20
*vertex,14.50000,6.70000,32.90000  #  21
*vertex,14.50000,7.70000,32.90000  #  22
*vertex,14.50000,7.80000,31.10000  #  23
*vertex,14.50000,8.80000,31.10000  #  24
*vertex,14.50000,7.80000,32.90000  #  25
*vertex,14.50000,8.80000,32.90000  #  26
*vertex,14.50000,8.90000,31.10000  #  27
*vertex,14.50000,8.90000,32.90000  #  28
*vertex,14.50000,9.90000,31.10000  #  29
*vertex,14.50000,9.90000,32.90000  #  30
# 
# tag, number of vertices followed by list of associated vert
*edges,7,2,3,7,6,14,13,10  #  1
*edges,4,3,4,8,7  #  2
*edges,7,4,1,9,11,12,5,8  #  3
*edges,4,1,4,3,2  #  4
*edges,4,5,6,7,8  #  5
*edges,4,1,2,10,9  #  6
*edges,12,9,10,13,29,27,24,23,20,19,16,15,11  #  7
*edges,12,12,17,18,21,22,25,26,28,30,14,6,5  #  8
*edges,4,11,15,17,12  #  9
*edges,4,16,19,21,18  # 10
*edges,4,20,23,25,22  # 11
*edges,4,24,27,28,26  # 12
*edges,4,29,13,14,30  # 13
*edges,4,15,16,18,17  # 14
*edges,4,19,20,22,21  # 15
*edges,4,23,24,26,25  # 16
*edges,4,27,29,30,28  # 17
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,Right_wall,VERT,-,-,-,Int_wall,OPAQUE,SIMILAR,00,00  #   1 ||< identical environment
*surf,Rear_wall,VERT,-,-,-,Int_wall,OPAQUE,SIMILAR,00,00  #   2 ||< identical environment
*surf,Left_wall,VERT,-,-,-,Int_wall,OPAQUE,SIMILAR,00,00  #   3 ||< identical environment
*surf,Floor,FLOR,-,-,-,Floor,OPAQUE,SIMILAR,00,00  #   4 ||< identical environment
*surf,Ceiling,CEIL,-,-,-,Ceiling,OPAQUE,SIMILAR,00,00  #   5 ||< identical environment
*surf,Parapet,VERT,-,-,-,Sparel-pan,OPAQUE,EXTERIOR,0,0  #   6 ||< external
*surf,Frame-bot,VERT,-,-,-,Frame,OPAQUE,EXTERIOR,0,0  #   7 ||< external
*surf,Frame-top,VERT,-,-,-,Frame,OPAQUE,EXTERIOR,0,0  #   8 ||< external
*surf,Frame-1,VERT,-,-,-,Frame,OPAQUE,EXTERIOR,0,0  #   9 ||< external
*surf,Frame-2,VERT,-,-,-,Frame,OPAQUE,EXTERIOR,0,0  #  10 ||< external
*surf,Frame-3,VERT,-,-,-,Frame,OPAQUE,EXTERIOR,0,0  #  11 ||< external
*surf,Frame-4,VERT,-,-,-,Frame,OPAQUE,EXTERIOR,0,0  #  12 ||< external
*surf,Frame-5,VERT,-,-,-,Frame,OPAQUE,EXTERIOR,0,0  #  13 ||< external
*surf,Window-1,VERT,-,-,-,Glazing,EC_50/40-clr,EXTERIOR,0,0  #  14 ||< external
*surf,Window-2,VERT,-,-,-,Glazing,EC_50/40-clr,EXTERIOR,0,0  #  15 ||< external
*surf,Window-3,VERT,-,-,-,Glazing,EC_50/40-clr,EXTERIOR,0,0  #  16 ||< external
*surf,Window-4,VERT,-,-,-,Glazing,EC_50/40-clr,EXTERIOR,0,0  #  17 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,2,4,12,    20.43 0  # zone base list
