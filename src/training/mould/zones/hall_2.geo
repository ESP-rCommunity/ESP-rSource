*Geometry 1.1,GEN,hall_2 # tag version, format, zone name
*date Sun Dec  8 16:04:12 2013  # latest file modification 
hall_2 describes a
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,1.50000,2.00000,2.20000  #   1
*vertex,3.00000,2.00000,2.20000  #   2
*vertex,3.00000,4.00000,2.20000  #   3
*vertex,0.00000,4.00000,2.20000  #   4
*vertex,0.00000,3.00000,2.20000  #   5
*vertex,1.50000,3.00000,2.20000  #   6
*vertex,1.50000,2.00000,4.40000  #   7
*vertex,3.00000,2.00000,4.40000  #   8
*vertex,3.00000,4.00000,4.40000  #   9
*vertex,0.00000,4.00000,4.40000  #  10
*vertex,0.00000,3.00000,4.40000  #  11
*vertex,1.50000,3.00000,4.40000  #  12
*vertex,3.00000,3.00000,2.20000  #  13
*vertex,3.00000,3.00000,4.40000  #  14
*vertex,1.50000,4.00000,2.20000  #  15
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,8,7  #  1
*edges,4,2,13,14,8  #  2
*edges,5,3,15,4,10,9  #  3
*edges,4,4,5,11,10  #  4
*edges,4,5,6,12,11  #  5
*edges,4,6,1,7,12  #  6
*edges,7,7,8,14,9,10,11,12  #  7
*edges,6,6,15,3,13,2,1  #  8
*edges,4,13,3,9,14  #  9
*edges,4,6,5,4,15  # 10
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,bathrm,VERT,-,-,-,int_doors,OPAQUE,ANOTHER,06,10  #   1 ||< hall_2:bathrm
*surf,south_beds,VERT,-,-,-,int_part,OPAQUE,ANOTHER,07,07  #   2 ||< hall_2:bed_south
*surf,north_beds,VERT,-,-,-,int_part,OPAQUE,ANOTHER,10,01  #   3 ||< hall2:north_beds
*surf,west,VERT,-,-,-,ext_part,OPAQUE,SIMILAR,00,00  #   4 ||< identical environment
*surf,whc,VERT,-,-,-,int_part,OPAQUE,ANOTHER,08,02  #   5 ||< hall2_2:w_heat_c
*surf,whc2,VERT,-,-,-,int_doors,OPAQUE,ANOTHER,08,01  #   6 ||< hall2:w_heat_c
*surf,ceiling,CEIL,-,-,-,loft_ceil,OPAQUE,ANOTHER,11,08  #   7 ||< hall:loft
*surf,hall1,FLOR,-,-,-,upper_flor,OPAQUE,ANOTHER,03,05  #   8 ||< hall_2:hall_1
*surf,north_beds,VERT,-,-,-,int_part,OPAQUE,ANOTHER,10,02  #   9 ||< hall_2_2:north_beds
*surf,stairs,FLOR,-,-,-,upper_flor,OPAQUE,ANOTHER,02,08  #  10 ||< north_beds:stairs
# 
*insol,1,8,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,2,8,10,     4.50 0  # zone base list
