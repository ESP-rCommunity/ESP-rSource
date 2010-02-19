*Geometry 1.1,GEN,gym # tag version, format, zone name
*date Tue Jul 21 18:27:10 2009  # latest file modification 
gym describes a
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,0.00000,40.00000,0.00000  #   1
*vertex,10.00000,40.00000,0.00000  #   2
*vertex,10.00000,60.00000,0.00000  #   3
*vertex,0.00000,60.00000,0.00000  #   4
*vertex,0.00000,40.00000,6.00000  #   5
*vertex,10.00000,40.00000,10.00000  #   6
*vertex,10.00000,60.00000,10.00000  #   7
*vertex,0.00000,60.00000,6.00000  #   8
*vertex,2.00000,60.00000,3.00000  #   9
*vertex,10.00000,60.00000,3.00000  #  10
*vertex,10.00000,40.00000,3.00000  #  11
*vertex,2.00000,40.00000,3.00000  #  12
*vertex,0.00000,58.00000,0.00000  #  13
*vertex,0.00000,56.00000,0.00000  #  14
*vertex,0.00000,56.00000,5.00000  #  15
*vertex,0.00000,58.00000,5.00000  #  16
*vertex,0.00000,52.00000,0.00000  #  17
*vertex,0.00000,50.00000,0.00000  #  18
*vertex,0.00000,50.00000,5.00000  #  19
*vertex,0.00000,52.00000,5.00000  #  20
*vertex,0.00000,46.00000,0.00000  #  21
*vertex,0.00000,44.00000,0.00000  #  22
*vertex,0.00000,44.00000,5.00000  #  23
*vertex,0.00000,46.00000,5.00000  #  24
*vertex,10.00000,41.00000,4.00000  #  25
*vertex,10.00000,59.00000,4.00000  #  26
*vertex,10.00000,59.00000,5.80000  #  27
*vertex,10.00000,41.00000,5.80000  #  28
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,6,5  #  1
*edges,10,2,3,7,6,2,25,28,27,26,25  #  2
*edges,4,3,4,8,7  #  3
*edges,16,4,13,16,15,14,17,20,19,18,21,24,23,22,1,5,8  #  4
*edges,4,5,6,7,8  #  5
*edges,10,4,3,2,1,22,21,18,17,14,13  #  6
*edges,4,10,9,12,11  #  7
*edges,4,11,12,9,10  #  8
*edges,4,13,14,15,16  #  9
*edges,4,17,18,19,20  # 10
*edges,4,21,22,23,24  # 11
*edges,4,25,26,27,28  # 12
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,Surf-1,VERT,-,-,-,intern_wall,OPAQUE,ANOTHER,01,03  #   1 ||< Surf-3:changing
*surf,Surf-2,VERT,-,-,-,intern_wall,OPAQUE,ANOTHER,03,14  #   2 ||< gym-r:main-hall
*surf,Surf-3,VERT,-,-,-,extern_wall,OPAQUE,EXTERIOR,0,0  #   3 ||< external
*surf,Surf-4,VERT,-,-,-,extern_wall,OPAQUE,EXTERIOR,0,0  #   4 ||< external
*surf,Surf-5,SLOP,-,-,-,roof,OPAQUE,EXTERIOR,0,0  #   5 ||< external
*surf,Surf-6,FLOR,-,-,-,oth_ground,OPAQUE,GROUND,01,00  #   6 ||< ground profile  1
*surf,xSurf-6,CEIL,-,-,-,susp_floor,OPAQUE,ADIABATIC,0,0  #   7 ||< adiabatic
*surf,Surf-8,FLOR,-,-,-,susp_flr_re,OPAQUE,ADIABATIC,0,0  #   8 ||< adiabatic
*surf,glaz-1,VERT,Surf-4,-,-,d_glz,DCF7671_06nb,EXTERIOR,0,0  #   9 ||< external
*surf,glaz-2,VERT,Surf-4,-,-,d_glz,DCF7671_06nb,EXTERIOR,0,0  #  10 ||< external
*surf,glaz-3,VERT,Surf-4,-,-,d_glz,DCF7671_06nb,EXTERIOR,0,0  #  11 ||< external
*surf,glaz_hall,VERT,Surf-2,-,-,d_glz,DCF7671_06nb,ANOTHER,03,20  #  12 ||< xglaz_hall:main-hall
# 
*insol,1,6,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,2,6,8,   360.00 0  # zone base list
