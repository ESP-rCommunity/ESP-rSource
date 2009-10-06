*Geometry 1.1,GEN,entrance # tag version, format, zone name
*date Tue Jul 21 18:22:40 2009  # latest file modification 
entrance describes a
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,0.00000,0.00000,0.00000  #   1
*vertex,20.00000,0.00000,0.00000  #   2
*vertex,20.00000,10.00000,0.00000  #   3
*vertex,10.00000,10.00000,0.00000  #   4
*vertex,10.00000,20.00000,0.00000  #   5
*vertex,0.00000,20.00000,0.00000  #   6
*vertex,0.00000,0.00000,3.00000  #   7
*vertex,20.00000,0.00000,3.00000  #   8
*vertex,20.00000,10.00000,5.00000  #   9
*vertex,10.00000,10.00000,5.00000  #  10
*vertex,10.00000,20.00000,5.00000  #  11
*vertex,0.00000,20.00000,3.00000  #  12
*vertex,0.00000,19.00000,0.00000  #  13
*vertex,0.00000,18.00000,0.00000  #  14
*vertex,0.00000,18.00000,2.00000  #  15
*vertex,0.00000,19.00000,2.00000  #  16
*vertex,0.00000,13.00000,0.00000  #  17
*vertex,0.00000,12.00000,0.00000  #  18
*vertex,0.00000,12.00000,2.00000  #  19
*vertex,0.00000,13.00000,2.00000  #  20
*vertex,0.00000,8.00000,0.00000  #  21
*vertex,0.00000,7.00000,0.00000  #  22
*vertex,0.00000,7.00000,2.00000  #  23
*vertex,0.00000,8.00000,2.00000  #  24
*vertex,7.00000,0.00000,0.00000  #  25
*vertex,8.00000,0.00000,0.00000  #  26
*vertex,8.00000,0.00000,2.50000  #  27
*vertex,7.00000,0.00000,2.50000  #  28
*vertex,12.00000,0.00000,0.00000  #  29
*vertex,13.00000,0.00000,0.00000  #  30
*vertex,13.00000,0.00000,2.50000  #  31
*vertex,12.00000,0.00000,2.50000  #  32
# 
# tag, number of vertices followed by list of associated vert
*edges,12,1,25,28,27,26,29,32,31,30,2,8,7  #  1
*edges,4,2,3,9,8  #  2
*edges,4,3,4,10,9  #  3
*edges,4,4,5,11,10  #  4
*edges,4,5,6,12,11  #  5
*edges,16,6,13,16,15,14,17,20,19,18,21,24,23,22,1,7,12  #  6
*edges,4,7,8,9,10  #  7
*edges,16,6,5,4,3,2,30,29,26,25,1,22,21,18,17,14,13  #  8
*edges,4,12,7,10,11  #  9
*edges,4,13,14,15,16  # 10
*edges,4,17,18,19,20  # 11
*edges,4,21,22,23,24  # 12
*edges,4,25,26,27,28  # 13
*edges,4,29,30,31,32  # 14
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,Surf-1,VERT,-,-,-,extern_wall,OPAQUE,EXTERIOR,0,0  #   1 ||< external
*surf,Surf-2,VERT,-,-,-,extern_wall,OPAQUE,EXTERIOR,0,0  #   2 ||< external
*surf,Surf-3,VERT,-,-,-,intern_wall,OPAQUE,ANOTHER,03,12  #   3 ||< xSurf-3:main-hall
*surf,Surf-4,VERT,-,-,-,intern_wall,OPAQUE,ANOTHER,03,11  #   4 ||< xSurf-4:main-hall
*surf,Surf-5,VERT,-,-,-,intern_wall,OPAQUE,ANOTHER,01,07  #   5 ||< recep-r:changing
*surf,Surf-6,VERT,-,-,-,extern_wall,OPAQUE,EXTERIOR,0,0  #   6 ||< external
*surf,Surf-7,CEIL,-,-,-,roof,OPAQUE,EXTERIOR,0,0  #   7 ||< external
*surf,Surf-8,FLOR,-,-,-,g_flr_entry,OPAQUE,GROUND,01,00  #   8 ||< ground profile  1
*surf,Surf-9,SLOP,-,-,-,roof,OPAQUE,EXTERIOR,0,0  #   9 ||< external
*surf,glaz-1,VERT,-,-,-,d_glz,DCF7671_06nb,EXTERIOR,0,0  #  10 ||< external
*surf,glaz-2,VERT,-,-,-,d_glz,DCF7671_06nb,EXTERIOR,0,0  #  11 ||< external
*surf,glaz-3,VERT,-,-,-,d_glz,DCF7671_06nb,EXTERIOR,0,0  #  12 ||< external
*surf,glaz-4,VERT,-,-,-,d_glz,DCF7671_06nb,EXTERIOR,0,0  #  13 ||< external
*surf,glaz-5,VERT,-,-,-,d_glz,DCF7671_06nb,EXTERIOR,0,0  #  14 ||< external
# 
*insol,1,8,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,8,   300.00 0  # zone base list
