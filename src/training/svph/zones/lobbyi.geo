*Geometry 1.1,GEN,int_lobby # tag version, format, zone name
*date Thu Aug 23 18:13:58 2007  # latest file modification 
int_lobby describes a...
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,8.85500,11.09100,0.00000  #   1
*vertex,7.45400,10.71600,0.00000  #   2
*vertex,7.77800,9.50800,0.00000  #   3
*vertex,9.17900,9.88400,0.00000  #   4
*vertex,8.85500,11.09100,2.40000  #   5
*vertex,7.45400,10.71600,2.40000  #   6
*vertex,7.77800,9.50800,2.40000  #   7
*vertex,9.17900,9.88400,2.40000  #   8
*vertex,8.54100,11.00700,0.00000  #   9
*vertex,7.67200,10.77400,0.00000  #  10
*vertex,7.67200,10.77400,2.10000  #  11
*vertex,8.54100,11.00700,2.10000  #  12
# 
# tag, number of vertices followed by list of associated vert
*edges,8,1,9,12,11,10,2,6,5  #  1
*edges,4,2,3,7,6  #  2
*edges,4,3,4,8,7  #  3
*edges,4,4,1,5,8  #  4
*edges,4,5,6,7,8  #  5
*edges,6,1,4,3,2,10,9  #  6
*edges,4,9,10,11,12  #  7
# 
# surf attributes:
#  surf name, surf position VERT/CIIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,n_ext,VERT,-,-,-,ext_wall,OPAQUE,EXTERIOR,0,0  #   1 ||< external
*surf,inl_hal,VERT,-,-,-,int_part_1,OPAQUE,ANOTHER,03,04  #   2 ||< p_lobbyc:hall
*surf,inl_halb,VERT,-,-,-,int_part_1,OPAQUE,ANOTHER,03,03  #   3 ||< p_lobbyb:hall
*surf,inl_halc,VERT,-,-,-,int_part_1,OPAQUE,ANOTHER,03,02  #   4 ||< p_lobbya:hall
*surf,ceil,CEIL,-,-,-,grnd_ceiling,OPAQUE,ANOTHER,08,07  #   5 ||< flr_lobby:bedroom_3
*surf,floor,FLOR,-,-,-,grnd_floor,OPAQUE,GROUND,00,01  #   6 ||< user def grnd profile  1
*surf,door,VERT,-,-,-,door,OPAQUE,EXTERIOR,0,0  #   7 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,6,     1.81 0  # zone base list
