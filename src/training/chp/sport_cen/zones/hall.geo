*Geometry 1.1,GEN,main-hall # tag version, format, zone name
*date Tue Jul 21 18:31:39 2009  # latest file modification 
main-hall describes a
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,10.00000,10.00000,0.00000  #   1
*vertex,40.00000,10.00000,0.00000  #   2
*vertex,40.00000,60.00000,0.00000  #   3
*vertex,10.00000,60.00000,0.00000  #   4
*vertex,10.00000,10.00000,10.00000  #   5
*vertex,40.00000,10.00000,10.00000  #   6
*vertex,40.00000,60.00000,10.00000  #   7
*vertex,10.00000,60.00000,10.00000  #   8
*vertex,20.00000,20.00000,15.00000  #   9
*vertex,30.00000,20.00000,15.00000  #  10
*vertex,20.00000,50.00000,15.00000  #  11
*vertex,30.00000,50.00000,15.00000  #  12
*vertex,10.00000,20.00000,0.00000  #  13
*vertex,10.00000,20.00000,5.00000  #  14
*vertex,10.00000,10.00000,5.00000  #  15
*vertex,20.00000,10.00000,0.00000  #  16
*vertex,20.00000,10.00000,5.00000  #  17
*vertex,10.00000,40.00000,0.00000  #  18
*vertex,10.00000,40.00000,10.00000  #  19
*vertex,10.00000,20.00000,10.00000  #  20
*vertex,25.00000,60.00000,0.00000  #  21
*vertex,25.00000,60.00000,5.00000  #  22
*vertex,10.00000,60.00000,5.00000  #  23
*vertex,40.00000,13.00000,0.00000  #  24
*vertex,40.00000,16.00000,0.00000  #  25
*vertex,40.00000,16.00000,8.00000  #  26
*vertex,40.00000,13.00000,8.00000  #  27
*vertex,40.00000,31.00000,0.00000  #  28
*vertex,40.00000,34.00000,0.00000  #  29
*vertex,40.00000,34.00000,8.00000  #  30
*vertex,40.00000,31.00000,8.00000  #  31
*vertex,40.00000,49.00000,0.00000  #  32
*vertex,40.00000,52.00000,0.00000  #  33
*vertex,40.00000,52.00000,8.00000  #  34
*vertex,40.00000,49.00000,8.00000  #  35
*vertex,10.00000,21.00000,4.00000  #  36
*vertex,10.00000,39.00000,4.00000  #  37
*vertex,10.00000,39.00000,5.80000  #  38
*vertex,10.00000,21.00000,5.80000  #  39
*vertex,10.00000,41.00000,4.00000  #  40
*vertex,10.00000,59.00000,4.00000  #  41
*vertex,10.00000,59.00000,5.80000  #  42
*vertex,10.00000,41.00000,5.80000  #  43
# 
# tag, number of vertices followed by list of associated vert
*edges,6,16,2,6,5,15,17  #  1
*edges,16,2,24,27,26,25,28,31,30,29,32,35,34,33,3,7,6  #  2
*edges,6,3,21,22,23,8,7  #  3
*edges,4,14,15,5,20  #  4
*edges,6,5,9,11,8,19,20  #  5
*edges,14,4,21,3,33,32,29,28,25,24,2,16,1,13,18  #  6
*edges,4,5,6,10,9  #  7
*edges,4,6,7,12,10  #  8
*edges,4,7,8,11,12  #  9
*edges,4,11,9,10,12  # 10
*edges,4,13,1,15,14  # 11
*edges,4,1,16,17,15  # 12
*edges,11,18,37,38,39,36,37,18,13,14,20,19  # 13
*edges,11,4,41,42,43,40,41,4,18,19,8,23  # 14
*edges,4,21,4,23,22  # 15
*edges,4,24,25,26,27  # 16
*edges,4,28,29,30,31  # 17
*edges,4,32,33,34,35  # 18
*edges,4,37,36,39,38  # 19
*edges,4,41,40,43,42  # 20
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,Surf-1,VERT,-,-,-,extern_wall,OPAQUE,EXTERIOR,0,0  #   1 ||< external
*surf,Surf-2,VERT,-,-,-,extern_wall,OPAQUE,EXTERIOR,0,0  #   2 ||< external
*surf,Surf-3,VERT,-,-,-,extern_wall,OPAQUE,EXTERIOR,0,0  #   3 ||< external
*surf,Surf-4,VERT,-,-,-,extern_wall,OPAQUE,EXTERIOR,0,0  #   4 ||< external
*surf,Surf-5,SLOP,-,-,-,roof,OPAQUE,EXTERIOR,0,0         #   5 ||< external
*surf,Surf-6,FLOR,-,-,-,hall_floor,OPAQUE,EXTERIOR,0,0   #   6 ||< external
*surf,Surf-7,SLOP,-,-,-,roof,OPAQUE,EXTERIOR,0,0     #   7 ||< external
*surf,Surf-8,ROOF,PITCHED,-,-,roof,OPAQUE,EXTERIOR,0,0  #   8 ||< external
*surf,Surf-9,ROOF,PITCHED,-,-,roof,OPAQUE,EXTERIOR,0,0  #   9 ||< external
*surf,Surf-10,CEIL,-,-,-,roof,OPAQUE,EXTERIOR,0,0        #  10 ||< external
*surf,xSurf-4,VERT,-,-,-,intern_wall,OPAQUE,ANOTHER,04,04  #  11 ||< Surf-4:entrance
*surf,xSurf-3,VERT,-,-,-,intern_wall,OPAQUE,ANOTHER,04,03  #  12 ||< Surf-3:entrance
*surf,change-r,VERT,-,-,-,intern_wall,OPAQUE,ANOTHER,01,02  #  13 ||< Surf-2:changing
*surf,gym-r,VERT,-,-,-,intern_wall,OPAQUE,ANOTHER,02,02  #  14 ||< Surf-2:gym
*surf,plant-r,VERT,-,-,-,intern_wall,OPAQUE,ANOTHER,05,01  #  15 ||< Surf-1:plant
*surf,glaz-1,VERT,Surf-2,-,-,d_glz,DCF7671_06nb,EXTERIOR,0,0  #  16 ||< external
*surf,glaz-2,VERT,Surf-2,-,-,d_glz,DCF7671_06nb,EXTERIOR,0,0  #  17 ||< external
*surf,glaz-3,VERT,Surf-2,-,-,d_glz,DCF7671_06nb,EXTERIOR,0,0  #  18 ||< external
*surf,xglaz_hall2,VERT,change-r,-,-,d_glz,DCF7671_06nb,ANOTHER,01,13  #  19 ||< glaz_hall2:changing
*surf,xglaz_hall,VERT,gym-r,-,-,d_glz,DCF7671_06nb,ANOTHER,02,12  #  20 ||< glaz_hall:gym
# 
*insol,1,6,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,6,  1500.00 0  # zone base list
