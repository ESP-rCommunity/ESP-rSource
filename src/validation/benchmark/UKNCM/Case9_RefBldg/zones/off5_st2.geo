*Geometry 1.1,GEN,off5_st2 # tag version, format, zone name
*date Wed Dec 10 10:24:26 2008  # latest file modification 
off5_st2 is 2nd level open plan office in core
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,10.50000,6.00000,3.50000  #   1
*vertex,29.50000,6.00000,3.50000  #   2
*vertex,29.50000,12.00000,3.50000  #   3
*vertex,10.50000,12.00000,3.50000  #   4
*vertex,10.50000,6.00000,7.00000  #   5
*vertex,29.50000,6.00000,7.00000  #   6
*vertex,29.50000,12.00000,7.00000  #   7
*vertex,10.50000,12.00000,7.00000  #   8
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
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,Wall-1,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,09,07  #   1 ||< Wall-3b:off2_st2
*surf,Wall-2,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,13,04  #   2 ||< Wall-4:circ2_st2
*surf,Wall-3,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,11,07  #   3 ||< Wall-1b:off4_st2
*surf,Wall-4,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,12,02  #   4 ||< Wall-2:circ1_st2
*surf,ceiling,CEIL,-,PARTN,-,inv_pa_fl_no,OPAQUE,ANOTHER,21,06  #   5 ||< floor:off5_st3
*surf,floor,FLOR,-,PARTN,-,pa_fl_notion,OPAQUE,ANOTHER,07,05  #   6 ||< ceiling:office_5
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,6,   114.00 0  # zone base list
