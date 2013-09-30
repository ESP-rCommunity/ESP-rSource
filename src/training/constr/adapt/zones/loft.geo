*Geometry 1.1,GEN,loft # tag version, format, zone name
*date Fri Jul 17 13:33:43 2009  # latest file modification 
loft is the roof space
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,6.00000,7.00000,4.40000  #   1
*vertex,0.00000,7.00000,4.40000  #   2
*vertex,0.00000,0.00000,4.40000  #   3
*vertex,6.00000,0.00000,4.40000  #   4
*vertex,6.00000,3.50000,6.20000  #   5
*vertex,0.00000,3.50000,6.20000  #   6
*vertex,6.00000,5.00000,4.40000  #   7
*vertex,3.00000,5.00000,4.40000  #   8
*vertex,3.00000,7.00000,4.40000  #   9
*vertex,0.00000,4.00000,4.40000  #  10
*vertex,3.00000,4.00000,4.40000  #  11
*vertex,3.00000,0.00000,4.40000  #  12
*vertex,6.00000,3.00000,4.40000  #  13
*vertex,3.00000,3.00000,4.40000  #  14
*vertex,6.00000,4.00000,4.40000  #  15
*vertex,4.50000,4.00000,4.40000  #  16
*vertex,4.50000,5.00000,4.40000  #  17
# 
# tag, number of vertices followed by list of associated vert
*edges,5,7,17,8,9,1  #  1
*edges,6,4,13,15,7,1,5  #  2
*edges,4,3,6,2,10  #  3
*edges,5,1,9,2,6,5  #  4
*edges,5,4,5,6,3,12  #  5
*edges,5,3,10,11,14,12  #  6
*edges,5,9,8,11,10,2  #  7
*edges,7,13,14,11,8,17,16,15  #  8
*edges,4,7,15,16,17  #  9
*edges,4,12,14,13,4  # 10
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,bathrm,FLOR,-,-,-,loft_floor,OPAQUE,ANOTHER,06,05  #   1 ||< loft:bathrm
*surf,east,VERT,-,-,-,ext_part,OPAQUE,SIMILAR,00,00  #   2 ||< identical environment
*surf,west,VERT,-,-,-,ext_part,OPAQUE,SIMILAR,00,00  #   3 ||< identical environment
*surf,north,SLOP,-,-,-,roof,OPAQUE,EXTERIOR,0,0  #   4 ||< external
*surf,south,SLOP,-,-,-,roof,OPAQUE,EXTERIOR,0,0  #   5 ||< external
*surf,bed2,FLOR,-,-,-,loft_floor,OPAQUE,ANOTHER,10,06  #   6 ||< loft:bed2
*surf,bed1,FLOR,-,-,-,loft_floor,OPAQUE,ANOTHER,07,05  #   7 ||< loft:bed1
*surf,hall2,FLOR,-,-,-,loft_floor,OPAQUE,ANOTHER,09,07  #   8 ||< ceiling:hall_2
*surf,w_heat_c,FLOR,-,-,-,loft_floor,OPAQUE,ANOTHER,08,05  #   9 ||< loft:w_heat_c
*surf,bed3,FLOR,-,-,-,loft_floor,OPAQUE,ANOTHER,11,05  #  10 ||< loft:bed3
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,4,1,6,7,8,    31.50 0  # zone base list
