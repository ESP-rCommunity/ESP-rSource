*Geometry 1.1,GEN,bedroom_2 # tag version, format, zone name
*date Thu Aug 23 18:13:58 2007  # latest file modification 
bedroom_2 describes a...
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,10.58800,8.29400,2.40000  #   1
*vertex,8.27000,7.67300,2.40000  #   2
*vertex,7.25400,7.40100,2.40000  #   3
*vertex,8.06900,4.35800,2.40000  #   4
*vertex,11.40300,5.25100,2.40000  #   5
*vertex,10.58800,8.29400,4.80000  #   6
*vertex,8.27000,7.67300,4.80000  #   7
*vertex,7.25400,7.40100,4.80000  #   8
*vertex,8.06900,4.35800,4.80000  #   9
*vertex,11.40300,5.25100,4.80000  #  10
*vertex,8.06900,4.35800,2.70000  #  11
*vertex,11.40300,5.25100,2.70000  #  12
*vertex,9.04700,4.62000,2.80000  #  13
*vertex,10.88200,5.11100,2.80000  #  14
*vertex,10.88200,5.11100,4.60000  #  15
*vertex,9.04700,4.62000,4.60000  #  16
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,7,6  #  1
*edges,4,2,3,8,7  #  2
*edges,5,3,4,11,9,8  #  3
*edges,10,11,12,10,9,11,13,16,15,14,13  #  4
*edges,4,4,5,12,11  #  5
*edges,5,1,6,10,12,5  #  6
*edges,5,9,10,6,7,8  #  7
*edges,5,3,2,1,5,4  #  8
*edges,4,13,14,15,16  #  9
# 
# surf attributes:
#  surf name, surf position VERT/CIIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,p_bed3,VERT,-,-,-,int_part_1,OPAQUE,ANOTHER,08,03  #   1 ||< p_bed2:bedroom_3
*surf,p_hall,VERT,-,-,-,int_part_1,OPAQUE,ANOTHER,10,06  #   2 ||< prt_b2:upper_hall
*surf,p_bed4,VERT,-,-,-,int_part_2,OPAQUE,ANOTHER,09,06  #   3 ||< prt_b4_b2:bedroom_4
*surf,s_ext,VERT,-,-,-,ext_wall,OPAQUE,EXTERIOR,0,0  #   4 ||< external
*surf,p_conserv,VERT,-,-,-,ext_wall,OPAQUE,ANOTHER,15,12  #   5 ||< b2_con:conservatory
*surf,e_ext,VERT,-,-,-,ext_wall,OPAQUE,EXTERIOR,0,0  #   6 ||< external
*surf,ceil,CEIL,-,-,-,1st_ceiling,OPAQUE,ANOTHER,13,10  #   7 ||< ceil_b2:loft
*surf,flr_liv,FLOR,-,-,-,grd_ceil_rev,OPAQUE,ANOTHER,01,06  #   8 ||< ceil_b2:living_room
*surf,window,VERT,-,-,-,double_glaz,DCF7672_06nb,EXTERIOR,0,0  #   9 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,8,    10.87 0  # zone base list
