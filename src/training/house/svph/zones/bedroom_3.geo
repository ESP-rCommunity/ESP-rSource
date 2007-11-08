*Geometry 1.1,GEN,bedroom_3 # tag version, format, zone name
*date Thu Aug 23 18:13:58 2007  # latest file modification 
bedroom_3 describes a...
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,9.77300,11.33700,2.40000  #   1
*vertex,8.85500,11.09100,2.40000  #   2
*vertex,7.45500,10.71600,2.40000  #   3
*vertex,7.77800,9.50800,2.40000  #   4
*vertex,8.27000,7.67300,2.40000  #   5
*vertex,10.58800,8.29400,2.40000  #   6
*vertex,9.17900,9.88400,2.40000  #   7
*vertex,9.77300,11.33700,4.80000  #   8
*vertex,7.45500,10.71600,4.80000  #   9
*vertex,8.27000,7.67300,4.80000  #  10
*vertex,10.58800,8.29400,4.80000  #  11
*vertex,8.73500,11.05700,3.60000  #  12
*vertex,7.57600,10.74700,3.60000  #  13
*vertex,7.57600,10.74800,4.50000  #  14
*vertex,8.73500,11.05800,4.50000  #  15
# 
# tag, number of vertices followed by list of associated vert
*edges,11,1,2,3,9,8,1,12,15,14,13,12  #  1
*edges,5,3,4,5,10,9  #  2
*edges,4,5,6,11,10  #  3
*edges,4,6,1,8,11  #  4
*edges,4,8,9,10,11  #  5
*edges,6,6,5,4,7,2,1  #  6
*edges,4,7,4,3,2  #  7
*edges,4,12,13,14,15  #  8
# 
# surf attributes:
#  surf name, surf position VERT/CIIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,n_ext,VERT,-,-,-,ext_wall,OPAQUE,EXTERIOR,0,0  #   1 ||< external
*surf,p_hall,VERT,-,-,-,int_part_1,OPAQUE,ANOTHER,10,07  #   2 ||< prt_h_b3:upper_hall
*surf,p_bed2,VERT,-,-,-,int_part_1,OPAQUE,ANOTHER,07,01  #   3 ||< p_bed3:bedroom_2
*surf,e_ext,VERT,-,-,-,ext_wall,OPAQUE,EXTERIOR,0,0  #   4 ||< external
*surf,ceil,CEIL,-,-,-,1st_ceiling,OPAQUE,ANOTHER,13,05  #   5 ||< ceil_b3:loft
*surf,flr_hall,FLOR,-,-,-,grd_ceil_rev,OPAQUE,ANOTHER,03,09  #   6 ||< ceil_b3:hall
*surf,flr_lobby,FLOR,-,-,-,grd_ceil_rev,OPAQUE,ANOTHER,12,05  #   7 ||< ceil:int_lobby
*surf,window,VERT,-,-,-,double_glaz,DCF7672_06nb,EXTERIOR,0,0  #   8 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,0,7.56,1  # zone base
