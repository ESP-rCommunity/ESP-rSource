*Geometry 1.1,GEN,changing # tag version, format, zone name
*date Tue Jul 21 18:26:52 2009  # latest file modification 
changing describes a
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,0.00000,20.00000,0.00000  #   1
*vertex,10.00000,20.00000,0.00000  #   2
*vertex,10.00000,40.00000,0.00000  #   3
*vertex,0.00000,40.00000,0.00000  #   4
*vertex,0.00000,20.00000,6.00000  #   5
*vertex,10.00000,20.00000,10.00000  #   6
*vertex,10.00000,40.00000,10.00000  #   7
*vertex,0.00000,40.00000,6.00000  #   8
*vertex,0.00000,20.00000,3.00000  #   9
*vertex,10.00000,20.00000,5.00000  #  10
*vertex,2.00000,40.00000,3.00000  #  11
*vertex,10.00000,40.00000,3.00000  #  12
*vertex,10.00000,20.00000,3.00000  #  13
*vertex,2.00000,20.00000,3.00000  #  14
*vertex,0.00000,38.00000,0.00000  #  15
*vertex,0.00000,36.00000,0.00000  #  16
*vertex,0.00000,36.00000,5.00000  #  17
*vertex,0.00000,38.00000,5.00000  #  18
*vertex,0.00000,32.00000,0.00000  #  19
*vertex,0.00000,30.00000,0.00000  #  20
*vertex,0.00000,30.00000,5.00000  #  21
*vertex,0.00000,32.00000,5.00000  #  22
*vertex,0.00000,26.00000,0.00000  #  23
*vertex,0.00000,24.00000,0.00000  #  24
*vertex,0.00000,24.00000,5.00000  #  25
*vertex,0.00000,26.00000,5.00000  #  26
*vertex,10.00000,21.00000,4.00000  #  27
*vertex,10.00000,39.00000,4.00000  #  28
*vertex,10.00000,39.00000,5.80000  #  29
*vertex,10.00000,21.00000,5.80000  #  30
# 
# tag, number of vertices followed by list of associated vert
*edges,4,9,10,6,5  #  1
*edges,11,2,3,7,6,10,2,27,30,29,28,27  #  2
*edges,4,3,4,8,7  #  3
*edges,17,4,15,18,17,16,19,22,21,20,23,26,25,24,1,9,5,8  #  4
*edges,4,5,6,7,8  #  5
*edges,10,4,3,2,1,24,23,20,19,16,15  #  6
*edges,4,1,2,10,9  #  7
*edges,4,14,13,12,11  #  8
*edges,4,11,12,13,14  #  9
*edges,4,15,16,17,18  # 10
*edges,4,19,20,21,22  # 11
*edges,4,23,24,25,26  # 12
*edges,4,27,28,29,30  # 13
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,Surf-1,VERT,-,-,-,intern_wall,OPAQUE,EXTERIOR,0,0  #   1 ||< external
*surf,Surf-2,VERT,-,-,-,intern_wall,OPAQUE,ANOTHER,03,13  #   2 ||< change-r:main-hall
*surf,Surf-3,VERT,-,-,-,intern_wall,OPAQUE,ANOTHER,02,01  #   3 ||< Surf-1:gym
*surf,Surf-4,VERT,-,-,-,extern_wall,OPAQUE,EXTERIOR,0,0  #   4 ||< external
*surf,Surf-5,SLOP,-,-,-,roof,OPAQUE,EXTERIOR,0,0  #   5 ||< external
*surf,Surf-6,FLOR,-,-,-,oth_ground,OPAQUE,GROUND,01,00  #   6 ||< ground profile  1
*surf,recep-r,VERT,-,-,-,intern_wall,OPAQUE,ANOTHER,04,05  #   7 ||< Surf-5:entrance
*surf,xSurf-6,CEIL,-,-,-,susp_floor,OPAQUE,ADIABATIC,0,0  #   8 ||< adiabatic
*surf,Surf-9,FLOR,-,-,-,susp_flr_re,OPAQUE,ADIABATIC,0,0  #   9 ||< adiabatic
*surf,glaz-1,VERT,Surf-4,-,-,d_glz,DCF7671_06nb,EXTERIOR,0,0  #  10 ||< external
*surf,glaz-2,VERT,Surf-4,-,-,d_glz,DCF7671_06nb,EXTERIOR,0,0  #  11 ||< external
*surf,glaz-3,VERT,Surf-4,-,-,d_glz,DCF7671_06nb,EXTERIOR,0,0  #  12 ||< external
*surf,glaz_hall2,VERT,Surf-2,-,-,d_glz,DCF7671_06nb,ANOTHER,03,19  #  13 ||< xglaz_hall2:main-hall
# 
*insol,1,6,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,2,6,9,   360.00 0  # zone base list
