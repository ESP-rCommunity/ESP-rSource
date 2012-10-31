*Geometry 1.1,GEN,air_col_mid # tag version, format, zone name
*date Tue Jul 21 13:27:29 2009  # latest file modification 
air_col_mid describes a...
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,13.20000,1.73600,3.37700  #   1
*vertex,15.40000,1.73600,3.37700  #   2
*vertex,15.40000,2.84000,4.39300  #   3
*vertex,13.20000,2.84000,4.39300  #   4
*vertex,13.20000,1.63600,3.47700  #   5
*vertex,15.40000,1.63600,3.47700  #   6
*vertex,15.40000,2.74000,4.49300  #   7
*vertex,13.20000,2.74000,4.49300  #   8
# 
# tag, number of vertices followed by list of associated vert
*edges,4,2,1,4,3  #  1
*edges,4,5,6,7,8  #  2
*edges,4,4,1,5,8  #  3
*edges,4,1,2,6,5  #  4
*edges,4,2,3,7,6  #  5
*edges,4,3,4,8,7  #  6
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,base,SLOP,-,-,-,colct_frame,OPAQUE,ANOTHER,06,13  #   1 ||< air_m:roof
*surf,cover,SLOP,-,-,-,dbl_glz,DCF7671_06nb,EXTERIOR,0,0  #   2 ||< external
*surf,left,VERT,-,-,-,colct_frame,OPAQUE,ANOTHER,07,05  #   3 ||< right:air_col_left
*surf,bot_frame,SLOP,-,-,-,colct_frame,OPAQUE,EXTERIOR,0,0  #   4 ||< external
*surf,right,VERT,-,-,-,colct_frame,OPAQUE,ANOTHER,09,03  #   5 ||< left:air_col_righ
*surf,top_frame,SLOP,-,-,-,colct_frame,OPAQUE,EXTERIOR,0,0  #   6 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,0,0.00,0  # zone base
