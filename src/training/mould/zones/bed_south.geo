*Geometry 1.1,GEN,bed_south # tag version, format, zone name
*date Sun Dec  8 16:04:12 2013  # latest file modification 
bed_south describes a
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,3.00000,0.00000,2.20000  #   1
*vertex,6.00000,0.00000,2.20000  #   2
*vertex,6.00000,3.00000,2.20000  #   3
*vertex,3.00000,3.00000,2.20000  #   4
*vertex,3.00000,0.00000,4.40000  #   5
*vertex,6.00000,0.00000,4.40000  #   6
*vertex,6.00000,3.00000,4.40000  #   7
*vertex,3.00000,3.00000,4.40000  #   8
*vertex,3.00000,2.00000,2.20000  #   9
*vertex,3.00000,2.00000,4.40000  #  10
*vertex,4.00000,0.00000,3.00000  #  11
*vertex,5.20000,0.00000,3.00000  #  12
*vertex,5.20000,0.00000,4.00000  #  13
*vertex,4.00000,0.00000,4.00000  #  14
# 
# tag, number of vertices followed by list of associated vert
*edges,10,1,2,6,5,1,11,14,13,12,11  #  1
*edges,4,2,3,7,6  #  2
*edges,4,3,4,8,7  #  3
*edges,4,9,1,5,10  #  4
*edges,5,5,6,7,8,10  #  5
*edges,5,4,3,2,1,9  #  6
*edges,4,4,9,10,8  #  7
*edges,4,11,12,13,14  #  8
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,south,VERT,-,-,-,ext_wall,OPAQUE,EXTERIOR,0,0  #   1 ||< external
*surf,east,VERT,-,-,-,ext_part,OPAQUE,SIMILAR,00,00  #   2 ||< identical environment
*surf,north_bed,VERT,-,-,-,int_part,OPAQUE,ANOTHER,10,03  #   3 ||< bed_s:north_beds
*surf,bathrm,VERT,-,-,-,int_part,OPAQUE,ANOTHER,06,02  #   4 ||< s_bed:bathrm
*surf,roof,CEIL,-,-,-,loft_ceil,OPAQUE,ANOTHER,11,07  #   5 ||< Loft_sbed:loft
*surf,floor,FLOR,-,-,-,upper_flor,OPAQUE,ANOTHER,04,05  #   6 ||< bed_1:kitchen
*surf,hall_2,VERT,-,-,-,int_part,OPAQUE,ANOTHER,09,02  #   7 ||< south_beds:hall_2
*surf,window,VERT,-,-,-,single_glaz,SC_8985_04nb,EXTERIOR,0,0  #   8 ||< external
# 
*insol,1,6,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,6,     9.00 0  # zone base list
