*Geometry 1.1,GEN,bath # tag version, format, zone name
*date Thu Aug 23 18:09:28 2007  # latest file modification 
bath describes a...
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,7.12200,11.57000,0.00000  #   1
*vertex,7.97200,13.04200,0.00000  #   2
*vertex,6.06600,14.14200,0.00000  #   3
*vertex,5.21700,12.67000,0.00000  #   4
*vertex,7.12200,11.57000,2.40000  #   5
*vertex,7.97200,13.04200,2.40000  #   6
*vertex,6.06600,14.14200,2.40000  #   7
*vertex,5.21700,12.67000,2.40000  #   8
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,6,5  #  1
*edges,4,2,3,7,6  #  2
*edges,4,3,4,8,7  #  3
*edges,4,4,1,5,8  #  4
*edges,4,5,6,7,8  #  5
*edges,4,1,4,3,2  #  6
# 
# surf attributes:
#  surf name, surf position VERT/CIIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,s_hall,VERT,-,-,-,int_buf,OPAQUE,ANOTHER,03,03  #   1 ||< s_bath:hall
*surf,s_bed2,VERT,-,-,-,int_buf,OPAQUE,ANOTHER,06,04  #   2 ||< s_bath:bed2
*surf,s_adjhouse,VERT,-,-,-,ext_west,OPAQUE,SIMILAR,00,00  #   3 ||< identical environment
*surf,s_liv,VERT,-,-,-,int_buf,OPAQUE,ANOTHER,01,10  #   4 ||< s_bath:kitliv
*surf,ceil,CEIL,-,-,-,int_roof,OPAQUE,ANOTHER,10,04  #   5 ||< bath:roof
*surf,floor,FLOR,-,-,-,house_floor,OPAQUE,GROUND,01,00  #   6 ||< ground profile  1
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,6,     3.74 0  # zone base list
