*Geometry 1.1,GEN,roof # tag version, format, zone name
*date Tue Dec 13 17:03:37 2011  # latest file modification 
roof describes a...
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,8.28600,5.58600,2.40000  #   1
*vertex,12.98895,13.72530,2.40000  #   2
*vertex,7.61600,16.82700,2.40000  #   3
*vertex,2.91600,8.68600,2.40000  #   4
*vertex,5.36600,12.93000,5.20000  #   5
*vertex,11.43648,11.04172,2.40000  #   6
*vertex,7.12100,11.56900,2.40000  #   7
*vertex,7.97100,13.04100,2.40000  #   8
*vertex,8.75100,12.59200,2.40000  #   9
*vertex,10.30100,15.27700,2.40000  #  10
*vertex,5.21700,12.67000,2.40000  #  11
*vertex,6.06600,14.14200,2.40000  #  12
*vertex,8.48600,5.93200,2.40000  #  13
*vertex,3.11600,9.03200,2.40000  #  14
*vertex,10.73612,9.82993,5.20000  #  15
*vertex,10.58627,9.57037,2.40000  #  16
*vertex,9.15423,7.08989,3.39222  #  17
# 
# tag, number of vertices followed by list of associated vert
*edges,6,3,12,11,14,4,5  #  1
*edges,5,13,14,11,7,16  #  2
*edges,5,16,7,8,9,6  #  3
*edges,4,7,11,12,8  #  4
*edges,4,6,9,10,2  #  5
*edges,5,9,8,12,3,10  #  6
*edges,4,1,4,14,13  #  7
*edges,7,6,2,15,17,1,13,16  #  8
*edges,5,4,1,17,15,5  #  9
*edges,5,2,10,3,5,15  # 10
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
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
*base_list,6,2,3,4,5,6,7,    58.29 0  # zone base list
