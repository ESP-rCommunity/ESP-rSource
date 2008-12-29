*Geometry 1.1,GEN,bedroom_4 # tag version, format, zone name
*date Thu Aug 23 18:13:58 2007  # latest file modification 
bedroom_4 describes a...
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,7.25400,7.40100,2.40000  #   1
*vertex,5.27600,6.87100,2.40000  #   2
*vertex,5.83200,4.79400,2.40000  #   3
*vertex,5.34900,4.66500,2.40000  #   4
*vertex,5.60800,3.69900,2.40000  #   5
*vertex,6.09100,3.82800,2.40000  #   6
*vertex,8.06900,4.35800,2.40000  #   7
*vertex,7.25400,7.40100,4.80000  #   8
*vertex,5.27600,6.87100,4.80000  #   9
*vertex,5.83200,4.79400,4.80000  #  10
*vertex,5.34900,4.66500,4.80000  #  11
*vertex,5.60800,3.69900,4.80000  #  12
*vertex,8.06900,4.35800,4.80000  #  13
*vertex,8.06900,4.35800,2.70000  #  14
*vertex,6.09100,3.82800,2.70000  #  15
*vertex,5.99400,3.80200,2.80000  #  16
*vertex,7.83000,4.29300,2.80000  #  17
*vertex,7.83000,4.29300,4.60000  #  18
*vertex,5.99400,3.80200,4.60000  #  19
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,9,8  #  1
*edges,4,2,3,10,9  #  2
*edges,4,3,4,11,10  #  3
*edges,4,4,5,12,11  #  4
*edges,12,15,14,13,12,5,6,15,16,19,18,17,16  #  5
*edges,5,7,1,8,13,14  #  6
*edges,6,8,9,10,11,12,13  #  7
*edges,5,7,6,3,2,1  #  8
*edges,4,6,5,4,3  #  9
*edges,4,6,7,14,15  # 10
*edges,4,16,17,18,19  # 11
# 
# surf attributes:
#  surf name, surf position VERT/CIIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,p_hall,VERT,-,-,-,int_part_1,OPAQUE,ANOTHER,10,05  #   1 ||< prt_b4:upper_hall
*surf,prt_b1_b4,VERT,-,-,-,int_part_1,OPAQUE,ANOTHER,06,07  #   2 ||< p_bed4:bedroom_1
*surf,prt_b1_b4a,VERT,-,-,-,int_part_1,OPAQUE,ANOTHER,06,06  #   3 ||< p_bed4a:bedroom_1
*surf,prt_b1_b4b,VERT,-,-,-,int_part_1,OPAQUE,ANOTHER,06,05  #   4 ||< p_bed4b:bedroom_1
*surf,s_ext,VERT,-,-,-,ext_wall,OPAQUE,EXTERIOR,0,0  #   5 ||< external
*surf,prt_b4_b2,VERT,-,-,-,int_part_2,OPAQUE,ANOTHER,07,03  #   6 ||< p_bed4:bedroom_2
*surf,ceil,CEIL,-,-,-,1st_ceiling,OPAQUE,ANOTHER,13,09  #   7 ||< ceil_b4:loft
*surf,flr_liv,FLOR,-,-,-,grd_ceil_rev,OPAQUE,ANOTHER,01,07  #   8 ||< ceil_b4:living_room
*surf,flr_din,FLOR,-,-,-,grd_ceil_rev,OPAQUE,ANOTHER,02,06  #   9 ||< ceil_b4:dining_room
*surf,Surf-10,VERT,-,-,-,ext_wall,OPAQUE,ANOTHER,15,13  #  10 ||< b4_con:conservatory
*surf,window,VERT,-,-,-,double_glaz,DCF7672_06nb,EXTERIOR,0,0  #  11 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,0,6.95,1  # zone base
