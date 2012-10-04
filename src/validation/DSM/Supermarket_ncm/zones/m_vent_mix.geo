*Geometry 1.1,GEN,m_vent_mix # tag version, format, zone name
*date Mon Feb 13 20:04:05 2012  # latest file modification 
m_vent_mix describes a mixing box for ventilation preheat
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,100.00000,69.00000,3.20000  #   1
*vertex,100.00000,78.50000,3.20000  #   2
*vertex,100.00000,69.00000,5.20000  #   3
*vertex,100.00000,78.50000,5.20000  #   4
*vertex,92.00000,69.00000,3.20000  #   5
*vertex,92.00000,69.00000,5.20000  #   6
*vertex,92.00000,78.50000,5.20000  #   7
*vertex,92.00000,78.50000,3.20000  #   8
# 
# tag, number of vertices followed by list of associated vert
*edges,4,5,1,3,6  #  1
*edges,4,1,2,4,3  #  2
*edges,4,2,8,7,4  #  3
*edges,4,6,3,4,7  #  4
*edges,4,8,2,1,5  #  5
*edges,4,8,5,6,7  #  6
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,ptn_refr,VERT,-,WALL,-,SMexwal2010,OPAQUE,EXTERIOR,0,0  #   1 ||< external
*surf,right_fac,VERT,-,WALL,-,SMexwal2010,OPAQUE,EXTERIOR,0,0  #   2 ||< external
*surf,back_fac,VERT,-,WALL,-,SMexwal2010,OPAQUE,EXTERIOR,0,0  #   3 ||< external
*surf,ceiling,CEIL,-,ROOF,-,SMexwal2010,OPAQUE,EXTERIOR,0,0  #   4 ||< external
*surf,floor,FLOR,-,FLOOR,EXTGRND,SMexwal2010,OPAQUE,EXTERIOR,0,0  #   5 ||< external
*surf,ptn_storage,VERT,-,WALL,-,SMexwal2010,OPAQUE,EXTERIOR,0,0  #   6 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,5,    76.00 2  # zone base list
