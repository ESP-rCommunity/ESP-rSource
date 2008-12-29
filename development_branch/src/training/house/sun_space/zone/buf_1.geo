*Geometry 1.1,GEN,buf_1 # tag version, format, zone name
*date Thu Aug 23 18:09:28 2007  # latest file modification 
buf_1 describes a...
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,10.79800,4.13600,0.00000  #   1
*vertex,8.28600,5.58600,0.00000  #   2
*vertex,10.79800,4.13600,2.40000  #   3
*vertex,8.28600,5.58600,2.40000  #   4
*vertex,8.46000,5.48500,0.30000  #   5
*vertex,10.53800,4.28500,0.30000  #   6
*vertex,10.53800,4.28500,2.10000  #   7
*vertex,8.46000,5.48500,2.10000  #   8
*vertex,8.48600,5.93100,0.00000  #   9
*vertex,10.58600,9.56900,0.00000  #  10
*vertex,10.58600,9.56900,2.40000  #  11
*vertex,8.48600,5.93100,2.40000  #  12
*vertex,10.08600,8.70300,0.60000  #  13
*vertex,9.28600,7.31800,0.60000  #  14
*vertex,9.28600,7.31800,2.10000  #  15
*vertex,10.08600,8.70300,2.10000  #  16
*vertex,13.09800,8.12000,0.00000  #  17
*vertex,13.09800,8.12000,2.40000  #  18
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,17,18,3  #  1
*edges,10,2,1,3,4,2,5,8,7,6,5  #  2
*edges,5,3,18,11,12,4  #  3
*edges,5,1,2,9,10,17  #  4
*edges,4,5,6,7,8  #  5
*edges,10,10,9,12,11,10,13,16,15,14,13  #  6
*edges,4,9,2,4,12  #  7
*edges,4,13,14,15,16  #  8
*edges,4,17,10,11,18  #  9
# 
# surf attributes:
#  surf name, surf position VERT/CIIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,ext_wall,VERT,-,-,-,ext_buf,OPAQUE,EXTERIOR,0,0  #   1 ||< external
*surf,west,VERT,-,-,-,ext_west,OPAQUE,EXTERIOR,0,0  #   2 ||< external
*surf,fict,CEIL,-,-,-,fictitious,SC_fictit,ANOTHER,09,01  #   3 ||< buf_1:buf_roof
*surf,floor,FLOR,-,-,-,buf_floor,OPAQUE,GROUND,01,00  #   4 ||< ground profile  1
*surf,westwin,VERT,-,-,-,doub_glaz,DC_8074_04nb,EXTERIOR,0,0  #   5 ||< external
*surf,s_liv,VERT,-,-,-,int_buf,OPAQUE,ANOTHER,01,01  #   6 ||< s_buf:kitliv
*surf,s_west,VERT,-,-,-,int_buf,OPAQUE,ANOTHER,02,01  #   7 ||< s_buf:west_space
*surf,int_win,VERT,-,-,-,doub_glaz,DC_8074_04nb,ANOTHER,01,08  #   8 ||< win3:kitliv
*surf,fictv,VERT,-,-,-,fictitious,SC_fictit,ANOTHER,08,09  #   9 ||< fictv:buf_2
# 
*insol,1,6,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,4,    13.34 0  # zone base list
