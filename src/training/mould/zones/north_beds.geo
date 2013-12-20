*Geometry 1.1,GEN,north_beds # tag version, format, zone name
*date Sun Dec  8 16:04:12 2013  # latest file modification 
north_beds describes a
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,0.00000,4.00000,2.20000  #   1
*vertex,3.00000,4.00000,2.20000  #   2
*vertex,3.00000,3.00000,2.20000  #   3
*vertex,6.00000,3.00000,2.20000  #   4
*vertex,6.00000,7.00000,2.20000  #   5
*vertex,3.00000,7.00000,2.20000  #   6
*vertex,3.00000,4.50000,2.20000  #   7
*vertex,3.00000,7.00000,2.20000  #   8
*vertex,0.00000,7.00000,2.20000  #   9
*vertex,0.00000,4.00000,4.40000  #  10
*vertex,3.00000,4.00000,4.40000  #  11
*vertex,3.00000,3.00000,4.40000  #  12
*vertex,6.00000,3.00000,4.40000  #  13
*vertex,6.00000,7.00000,4.40000  #  14
*vertex,3.00000,7.00000,4.40000  #  15
*vertex,3.00000,4.50000,4.40000  #  16
*vertex,3.00000,7.00000,4.40000  #  17
*vertex,0.00000,7.00000,4.40000  #  18
*vertex,6.00000,4.00000,2.20000  #  19
*vertex,5.00000,7.00000,3.20000  #  20
*vertex,4.00000,7.00000,3.20000  #  21
*vertex,4.00000,7.00000,4.20000  #  22
*vertex,5.00000,7.00000,4.20000  #  23
*vertex,2.00000,7.00000,3.20000  #  24
*vertex,1.00000,7.00000,3.20000  #  25
*vertex,1.00000,7.00000,4.20000  #  26
*vertex,2.00000,7.00000,4.20000  #  27
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,11,10  #  1
*edges,4,2,3,12,11  #  2
*edges,4,3,4,13,12  #  3
*edges,5,4,19,5,14,13  #  4
*edges,10,5,6,15,14,5,20,23,22,21,20  #  5
*edges,4,6,7,16,15  #  6
*edges,4,7,8,17,16  #  7
*edges,10,8,9,18,17,8,24,27,26,25,24  #  8
*edges,4,9,1,10,18  #  9
*edges,9,10,11,12,13,14,15,16,17,18  # 10
*edges,8,9,8,7,6,5,19,2,1  # 11
*edges,4,2,19,4,3  # 12
*edges,4,20,21,22,23  # 13
*edges,4,24,25,26,27  # 14
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,hall2,VERT,-,-,-,int_part,OPAQUE,ANOTHER,09,03  #   1 ||< north_beds:hall_2
*surf,hall_2_2,VERT,-,-,-,int_part,OPAQUE,ANOTHER,09,09  #   2 ||< north_beds:hall_2
*surf,bed_s,VERT,-,-,-,int_part,OPAQUE,ANOTHER,07,03  #   3 ||< north_bed:bed_south
*surf,east,VERT,-,-,-,ext_part,OPAQUE,SIMILAR,00,00  #   4 ||< identical environment
*surf,north1,VERT,-,-,-,ext_wall,OPAQUE,EXTERIOR,0,0  #   5 ||< external
*surf,Surf-6,VERT,-,-,-,int_part,OPAQUE,ADIABATIC,0,0  #   6 ||< adiabatic
*surf,Surf-7,VERT,-,-,-,int_part,OPAQUE,ADIABATIC,0,0  #   7 ||< adiabatic
*surf,north2,VERT,-,-,-,ext_wall,OPAQUE,EXTERIOR,0,0  #   8 ||< external
*surf,east,VERT,-,-,-,ext_part,OPAQUE,SIMILAR,00,00  #   9 ||< identical environment
*surf,Surf-10,CEIL,-,-,-,loft_ceil,OPAQUE,ANOTHER,11,06  #  10 ||< Loft_nbed:loft
*surf,Surf-11,FLOR,-,-,-,upper_flor,OPAQUE,ANOTHER,05,05  #  11 ||< north_beds:liv_room
*surf,kitch,FLOR,-,-,-,upper_flor,OPAQUE,ANOTHER,04,07  #  12 ||< north_beds:kitchen
*surf,window1,VERT,-,-,-,single_glaz,SC_8985_04nb,EXTERIOR,0,0  #  13 ||< external
*surf,window2,VERT,-,-,-,single_glaz,SC_8985_04nb,EXTERIOR,0,0  #  14 ||< external
# 
*insol,1,11,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,2,11,12,    21.00 0  # zone base list
