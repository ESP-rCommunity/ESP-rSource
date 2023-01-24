*Geometry 1.1,GEN,basement # tag version, format, zone name
*date Sun May  2 07:55:42 2021  # latest file modification 
simplified basement elevation
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,0.00000,0.00000,0.00000  #   1
*vertex,12.00000,0.00000,0.00000  #   2
*vertex,0.00000,12.20000,0.00000  #   3
*vertex,0.00000,0.00000,2.60000  #   4
*vertex,12.00000,0.00000,2.60000  #   5
*vertex,0.00000,12.20000,2.60000  #   6
*vertex,4.97000,12.20000,2.60000  #   7
*vertex,4.97000,6.79000,2.60000  #   8
*vertex,12.00000,6.79000,2.60000  #   9
*vertex,4.97000,6.79000,0.00000  #  10
*vertex,12.00000,6.79000,0.00000  #  11
*vertex,4.97000,12.20000,0.00000  #  12
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,5,4  #   1
*edges,4,2,11,9,5  #   2
*edges,4,3,6,7,12  #   3
*edges,4,3,1,4,6  #   4
*edges,6,4,5,9,8,7,6  #   5
*edges,6,1,3,12,10,11,2  #   6
*edges,4,12,7,8,10  #   7
*edges,4,11,10,8,9  #   8
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,Surf-1,VERT,-,-,-,foundation_,OPAQUE,GROUND,001,000  #   1 ||< ground profile  1
*surf,Surf-2,VERT,-,-,-,foundation_,OPAQUE,GROUND,001,000  #   2 ||< ground profile  1
*surf,Surf-5,VERT,-,-,-,foundation_,OPAQUE,GROUND,001,000  #   3 ||< ground profile  1
*surf,Surf-6,VERT,-,-,-,foundation_,OPAQUE,GROUND,001,000  #   4 ||< ground profile  1
*surf,to_upstairs,CEIL,-,-,-,floors_r,OPAQUE,ANOTHER,002,006  #   5 ||< floor:main_second
*surf,floor_slab,FLOR,-,-,-,slab_floor,OPAQUE,GROUND,001,000  #   6 ||< ground profile  1
*surf,Surf-7,VERT,-,-,-,foundation_,OPAQUE,GROUND,001,000  #   7 ||< ground profile  1
*surf,Surf-8,VERT,-,-,-,foundation_,OPAQUE,GROUND,001,000  #   8 ||< ground profile  1
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,6,   108.37 0  # zone base list
