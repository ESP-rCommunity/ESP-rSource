*Geometry 1.1,GEN,buf_roof # tag version, format, zone name
*date Tue Dec 13 16:44:23 2011  # latest file modification 
buf_roof describes a...
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,10.79800,4.13600,2.40000  #   1
*vertex,15.49800,12.27600,2.40000  #   2
*vertex,13.24800,8.37900,5.20000  #   3
*vertex,8.28600,5.58600,2.40000  #   4
*vertex,10.58600,9.56900,2.40000  #   5
*vertex,11.43600,11.04100,2.40000  #   6
*vertex,12.98600,13.72600,2.40000  #   7
*vertex,10.15200,8.01800,4.13600  #   8
*vertex,11.92800,6.99200,4.13600  #   9
*vertex,12.27400,7.59300,4.53300  #  10
*vertex,10.50000,8.61900,4.53300  #  11
*vertex,13.57900,9.95300,4.24900  #  12
*vertex,11.84700,10.95200,4.24900  #  13
*vertex,11.50700,10.36400,4.67200  #  14
*vertex,13.23900,9.36500,4.67200  #  15
*vertex,8.48600,5.93100,2.40000  #  16
*vertex,13.09800,8.12000,2.40000  #  17
*vertex,10.73600,9.82900,5.20000  #  18
# 
# tag, number of vertices followed by list of associated vert
*edges,5,1,4,16,5,17  #  1
*edges,10,4,1,3,18,4,8,11,10,9,8  #  2
*edges,10,2,7,18,3,2,12,15,14,13,12  #  3
*edges,4,1,17,2,3  #  4
*edges,4,8,9,10,11  #  5
*edges,4,12,13,14,15  #  6
*edges,5,17,5,6,7,2  #  7
*edges,6,7,6,5,16,4,18  #  8
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,buf_1,FLOR,-,-,-,fictitious,SC_fictit,ANOTHER,07,03  #   1 ||< fict:buf_1
*surf,roof_e,SLOP,-,-,-,roof,OPAQUE,EXTERIOR,0,0  #   2 ||< external
*surf,roof_w,SLOP,-,-,-,roof,OPAQUE,EXTERIOR,0,0  #   3 ||< external
*surf,roof_s,VERT,-,-,-,ext_buf,OPAQUE,EXTERIOR,0,0  #   4 ||< external
*surf,win2,SLOP,roof_e,-,-,doub_glaz,DC_8074_04nb,EXTERIOR,0,0  #   5 ||< external
*surf,glz_w,SLOP,roof_w,-,-,doub_glaz,DC_8074_04nb,EXTERIOR,0,0  #   6 ||< external
*surf,buf_2,FLOR,-,-,-,fictitious,SC_fictit,ANOTHER,08,04  #   7 ||< fict:buf_2
*surf,roof_buf,VERT,-,-,-,int_buf,OPAQUE,ANOTHER,10,08  #   8 ||< roof_buf:roof
# 
*insol,2,6,1,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,2,1,7,    27.26 0  # zone base list
