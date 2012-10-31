*Geometry 1.1,GEN,buf_2 # tag version, format, zone name
*date Thu Aug 23 18:09:28 2007  # latest file modification 
buf_2 describes a...
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,15.49800,12.27600,0.00000  #   1
*vertex,12.98600,13.72700,0.00000  #   2
*vertex,15.49800,12.27600,2.40000  #   3
*vertex,12.98600,13.72700,2.40000  #   4
*vertex,15.32500,12.37600,0.30000  #   5
*vertex,13.24700,13.57600,0.30000  #   6
*vertex,13.24700,13.57600,2.10000  #   7
*vertex,15.32500,12.37600,2.10000  #   8
*vertex,10.58600,9.57000,0.00000  #   9
*vertex,11.43600,11.04200,0.00000  #  10
*vertex,11.43600,11.04200,2.40000  #  11
*vertex,10.58600,9.57000,2.40000  #  12
*vertex,11.33600,10.86800,0.00000  #  13
*vertex,10.93600,10.17500,0.00000  #  14
*vertex,10.93600,10.17500,2.10000  #  15
*vertex,11.33600,10.86800,2.10000  #  16
*vertex,13.09800,8.12000,0.00000  #  17
*vertex,13.09800,8.12000,2.40000  #  18
# 
# tag, number of vertices followed by list of associated vert
*edges,4,17,1,3,18  #  1
*edges,10,1,2,4,3,1,5,8,7,6,5  #  2
*edges,4,2,10,11,4  #  3
*edges,5,18,3,4,11,12  #  4
*edges,7,17,9,14,13,10,2,1  #  5
*edges,4,5,6,7,8  #  6
*edges,8,10,13,16,15,14,9,12,11  #  7
*edges,4,13,14,15,16  #  8
*edges,4,9,17,18,12  #  9
# 
# surf attributes:
#  surf name, surf position VERT/CIIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,ext_wall,VERT,-,-,-,ext_buf,OPAQUE,EXTERIOR,0,0  #   1 ||< external
*surf,east,VERT,-,-,-,ext_west,OPAQUE,EXTERIOR,0,0  #   2 ||< external
*surf,s_bed,VERT,-,-,-,int_buf,OPAQUE,ANOTHER,05,01  #   3 ||< s_buf:bed1
*surf,fict,CEIL,-,-,-,fictitious,SC_fictit,ANOTHER,09,07  #   4 ||< buf_2:buf_roof
*surf,floor,FLOR,-,-,-,buf_floor,OPAQUE,GROUND,01,00  #   5 ||< ground profile  1
*surf,eastwin,VERT,-,-,-,doub_glaz,DC_8074_04nb,EXTERIOR,0,0  #   6 ||< external
*surf,s_hall,VERT,-,-,-,int_buf,OPAQUE,ANOTHER,03,01  #   7 ||< s_buf:hall
*surf,door,VERT,-,-,-,door,OPAQUE,ANOTHER,03,07  #   8 ||< door:hall
*surf,fictv,VERT,-,-,-,fictitious,SC_fictit,ANOTHER,07,09  #   9 ||< fictv:buf_1
# 
*insol,1,6,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,5,    13.92 0  # zone base list
