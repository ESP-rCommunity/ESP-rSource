*Geometry 1.1,GEN,hall # tag version, format, zone name
*date Thu Aug 23 18:09:28 2007  # latest file modification 
hall describes a...
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,10.58600,9.56900,0.00000  #   1
*vertex,11.43600,11.04100,0.00000  #   2
*vertex,7.97200,13.04200,0.00000  #   3
*vertex,7.12300,11.56900,0.00000  #   4
*vertex,10.58600,9.56900,2.40000  #   5
*vertex,11.43600,11.04100,2.40000  #   6
*vertex,7.97200,13.04200,2.40000  #   7
*vertex,7.12300,11.56900,2.40000  #   8
*vertex,10.93600,10.17500,0.00000  #   9
*vertex,11.33600,10.86800,0.00000  #  10
*vertex,11.33600,10.86800,2.10000  #  11
*vertex,10.93600,10.17500,2.10000  #  12
*vertex,8.75100,12.59100,0.00000  #  13
*vertex,8.75100,12.59100,2.40000  #  14
# 
# tag, number of vertices followed by list of associated vert
*edges,8,1,9,12,11,10,2,6,5  #  1
*edges,4,2,13,14,6  #  2
*edges,4,3,4,8,7  #  3
*edges,4,4,1,5,8  #  4
*edges,5,5,6,14,7,8  #  5
*edges,7,1,4,3,13,2,10,9  #  6
*edges,4,9,10,11,12  #  7
*edges,4,13,3,7,14  #  8
# 
# surf attributes:
#  surf name, surf position VERT/CIIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,s_buf,VERT,-,-,-,int_buf,OPAQUE,ANOTHER,08,07  #   1 ||< s_hall:buf_2
*surf,s_bed1,VERT,-,-,-,int_buf,OPAQUE,ANOTHER,05,04  #   2 ||< s_hall:bed1
*surf,s_bath,VERT,-,-,-,int_buf,OPAQUE,ANOTHER,04,01  #   3 ||< s_hall:bath
*surf,s_liv,VERT,-,-,-,int_buf,OPAQUE,ANOTHER,01,02  #   4 ||< s_hall:kitliv
*surf,ceil,CEIL,-,-,-,int_roof,OPAQUE,ANOTHER,10,03  #   5 ||< hall:roof
*surf,floor,FLOR,-,-,-,house_floor,OPAQUE,GROUND,01,00  #   6 ||< ground profile  1
*surf,door,VERT,-,-,-,door,OPAQUE,ANOTHER,08,08  #   7 ||< door:buf_2
*surf,s_bed2,VERT,-,-,-,door,OPAQUE,ANOTHER,06,08  #   8 ||< s_hall:bed2
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,6,     6.80 0  # zone base list
