*Geometry 1.1,GEN,roof # tag version, format, zone name
*date Thu Aug 23 18:09:28 2007  # latest file modification 
roof describes a...
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,8.28600,5.58600,2.40000  #   1
*vertex,12.98600,13.72700,2.40000  #   2
*vertex,7.61600,16.82700,2.40000  #   3
*vertex,2.91600,8.68600,2.40000  #   4
*vertex,5.36600,12.93000,5.20000  #   5
*vertex,10.58600,9.57000,2.40000  #   6
*vertex,11.43600,11.04200,2.40000  #   7
*vertex,7.12100,11.56900,2.40000  #   8
*vertex,7.97100,13.04100,2.40000  #   9
*vertex,8.75100,12.59200,2.40000  #  10
*vertex,10.30100,15.27700,2.40000  #  11
*vertex,5.21700,12.67000,2.40000  #  12
*vertex,6.06600,14.14200,2.40000  #  13
*vertex,8.48600,5.93200,2.40000  #  14
*vertex,3.11600,9.03200,2.40000  #  15
*vertex,10.73600,9.83000,5.20000  #  16
# 
# tag, number of vertices followed by list of associated vert
*edges,6,3,13,12,15,4,5  #  1
*edges,5,14,15,12,8,6  #  2
*edges,5,6,8,9,10,7  #  3
*edges,4,8,12,13,9  #  4
*edges,4,7,10,11,2  #  5
*edges,5,10,9,13,3,11  #  6
*edges,4,1,4,15,14  #  7
*edges,6,1,14,6,7,2,16  #  8
*edges,4,4,1,16,5  #  9
*edges,5,2,11,3,5,16  # 10
# 
# surf attributes:
#  surf name, surf position VERT/CIIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,roof_n,VERT,-,-,-,ext_west,OPAQUE,EXTERIOR,0,0  #   1 ||< external
*surf,liv,FLOR,-,-,-,int_roofrev,OPAQUE,ANOTHER,01,05  #   2 ||< ceil:kitliv
*surf,hall,FLOR,-,-,-,int_roofrev,OPAQUE,ANOTHER,03,05  #   3 ||< ceil:hall
*surf,bath,FLOR,-,-,-,int_roofrev,OPAQUE,ANOTHER,04,05  #   4 ||< ceil:bath
*surf,bed1,FLOR,-,-,-,int_roofrev,OPAQUE,ANOTHER,05,05  #   5 ||< ceil:bed1
*surf,bed2,FLOR,-,-,-,int_roofrev,OPAQUE,ANOTHER,06,05  #   6 ||< ceil:bed2
*surf,west_sp,FLOR,-,-,-,int_roofrev,OPAQUE,ANOTHER,02,05  #   7 ||< ceil:west_space
*surf,roof_buf,VERT,-,-,-,int_buf,OPAQUE,ANOTHER,09,08  #   8 ||< roof_buf:buf_roof
*surf,roof_w,SLOP,-,-,-,ni_roof,OPAQUE,EXTERIOR,0,0  #   9 ||< external
*surf,roof_e,SLOP,-,-,-,ni_roof,OPAQUE,EXTERIOR,0,0  #  10 ||< external
# 
*insol,2,6,1,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,6,2,3,4,5,6,7,    58.28 0  # zone base list
