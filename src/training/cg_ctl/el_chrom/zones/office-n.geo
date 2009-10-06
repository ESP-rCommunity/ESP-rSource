*Geometry 1.1,GEN,office-n # tag version, format, zone name
*date Mon Jul 20 15:18:17 2009  # latest file modification 
office-n describes a
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,5.50000,14.50000,30.00000  #   1
*vertex,10.00000,14.49999,30.00000  #   2
*vertex,10.00000,10.00000,30.00000  #   3
*vertex,5.50000,10.00000,30.00000  #   4
*vertex,5.50000,14.50000,33.00000  #   5
*vertex,10.00000,14.49999,33.00000  #   6
*vertex,10.00000,10.00000,33.00000  #   7
*vertex,5.50000,10.00000,33.00000  #   8
*vertex,5.50000,14.50000,31.00000  #   9
*vertex,10.00000,14.49999,31.00000  #  10
*vertex,5.50000,14.50000,31.10000  #  11
*vertex,5.50000,14.50000,32.90000  #  12
*vertex,10.00000,14.50000,31.10000  #  13
*vertex,10.00000,14.50000,32.90000  #  14
*vertex,5.60000,14.50000,31.10000  #  15
*vertex,6.60000,14.50000,31.10000  #  16
*vertex,5.60000,14.50000,32.90000  #  17
*vertex,6.60000,14.50000,32.90000  #  18
*vertex,6.70000,14.50000,31.10000  #  19
*vertex,7.70000,14.50000,31.10000  #  20
*vertex,6.70000,14.50000,32.90000  #  21
*vertex,7.70000,14.50000,32.90000  #  22
*vertex,7.80000,14.50000,31.10000  #  23
*vertex,8.80000,14.50000,31.10000  #  24
*vertex,7.80000,14.50000,32.90000  #  25
*vertex,8.80000,14.50000,32.90000  #  26
*vertex,8.90000,14.50000,31.10000  #  27
*vertex,8.90000,14.50000,32.90000  #  28
*vertex,9.90000,14.50000,31.10000  #  29
*vertex,9.90000,14.50000,32.90000  #  30
# 
# tag, number of vertices followed by list of associated vert
*edges,7,3,2,10,13,14,6,7  #  1
*edges,4,4,3,7,8  #  2
*edges,7,1,4,8,5,12,11,9  #  3
*edges,4,4,1,2,3  #  4
*edges,4,6,5,8,7  #  5
*edges,4,2,1,9,10  #  6
*edges,12,10,9,11,15,16,19,20,23,24,27,29,13  #  7
*edges,12,17,12,5,6,14,30,28,26,25,22,21,18  #  8
*edges,4,15,11,12,17  #  9
*edges,4,19,16,18,21  # 10
*edges,4,23,20,22,25  # 11
*edges,4,27,24,26,28  # 12
*edges,4,13,29,30,14  # 13
*edges,4,16,15,17,18  # 14
*edges,4,20,19,21,22  # 15
*edges,4,24,23,25,26  # 16
*edges,4,29,27,28,30  # 17
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
*base_list,2,4,12,    20.25 0  # zone base list
