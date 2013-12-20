*Geometry 1.1,GEN,loft # tag version, format, zone name
*date Sun Dec  8 16:04:12 2013  # latest file modification 
loft describes a
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,0.00000,0.00000,4.40000  #   1
*vertex,6.00000,0.00000,4.40000  #   2
*vertex,6.00000,7.00000,4.40000  #   3
*vertex,0.00000,7.00000,4.40000  #   4
*vertex,0.00000,3.50000,6.20000  #   5
*vertex,6.00000,3.50000,6.20000  #   6
*vertex,0.00000,2.00000,4.40000  #   7
*vertex,3.00000,2.00000,4.40000  #   8
*vertex,3.00000,0.00000,4.40000  #   9
*vertex,6.00000,3.00000,4.40000  #  10
*vertex,3.00000,3.00000,4.40000  #  11
*vertex,3.00000,7.00000,4.40000  #  12
*vertex,0.00000,4.00000,4.40000  #  13
*vertex,3.00000,4.00000,4.40000  #  14
*vertex,0.00000,3.00000,4.40000  #  15
*vertex,1.50000,3.00000,4.40000  #  16
*vertex,1.50000,2.00000,4.40000  #  17
# 
# tag, number of vertices followed by list of associated vert
*edges,5,7,17,8,9,1  #  1
*edges,6,4,13,15,7,1,5  #  2
*edges,4,3,6,2,10  #  3
*edges,5,1,9,2,6,5  #  4
*edges,5,4,5,6,3,12  #  5
*edges,7,13,4,12,3,10,11,14  #  6
*edges,5,9,8,11,10,2  #  7
*edges,7,13,14,11,8,17,16,15  #  8
*edges,4,7,15,16,17  #  9
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,first,FLOR,-,-,-,loft_floor,OPAQUE,ANOTHER,06,05  #   1 ||< roof:bathrm
*surf,West_ext,VERT,-,-,-,ext_part,OPAQUE,SIMILAR,00,00  #   2 ||< identical environment
*surf,East_ext,VERT,-,-,-,ext_part,OPAQUE,SIMILAR,00,00  #   3 ||< identical environment
*surf,South_roof,UNKN,-,-,-,roof,OPAQUE,EXTERIOR,0,0  #   4 ||< external
*surf,North_roof,VERT,-,-,-,roof,OPAQUE,EXTERIOR,0,0  #   5 ||< external
*surf,Loft_nbed,FLOR,-,-,-,loft_floor,OPAQUE,ANOTHER,10,10  #   6 ||< Surf-10:north_beds
*surf,Loft_sbed,FLOR,-,-,-,loft_floor,OPAQUE,ANOTHER,07,05  #   7 ||< roof:bed_south
*surf,hall,FLOR,-,-,-,loft_floor,OPAQUE,ANOTHER,09,07  #   8 ||< ceiling:hall_2
*surf,wh_cup,FLOR,-,-,-,loft_floor,OPAQUE,ANOTHER,08,05  #   9 ||< ceiling:w_heat_c
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,5,1,6,7,8,9,    42.00 0  # zone base list
