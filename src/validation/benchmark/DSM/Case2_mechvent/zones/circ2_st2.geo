*Geometry 1.1,GEN,circ2_st2 # tag version, format, zone name
*date Fri May  8 11:24:41 2009  # latest file modification 
circ2_st2 is 2nd level circulation adjacent to office_3
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,29.50000,6.00000,3.50000  #   1
*vertex,34.00000,6.00000,3.50000  #   2
*vertex,34.00000,12.00000,3.50000  #   3
*vertex,29.50000,12.00000,3.50000  #   4
*vertex,29.50000,6.00000,7.00000  #   5
*vertex,34.00000,6.00000,7.00000  #   6
*vertex,34.00000,12.00000,7.00000  #   7
*vertex,29.50000,12.00000,7.00000  #   8
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
*surf,Wall-1,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,09,08  #   1 ||< Wall-3c:off2_st2
*surf,Wall-2,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,10,07  #   2 ||< Wall-4b:off3_st2
*surf,Wall-3,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,11,08  #   3 ||< Wall-1c:off4_st2
*surf,Wall-4,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,14,02  #   4 ||< Wall-2:off5_st2
*surf,ceiling,CEIL,-,PARTN,-,inv_pa_fl_no,OPAQUE,ANOTHER,20,06  #   5 ||< floor:circ2_st3
*surf,floor,FLOR,-,PARTN,-,pa_fl_notion,OPAQUE,ANOTHER,06,05  #   6 ||< ceiling:circ_area2
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,6,    27.00 2  # zone base list
