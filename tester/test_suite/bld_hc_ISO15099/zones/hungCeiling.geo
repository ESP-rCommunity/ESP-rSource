*Geometry 1.1,GEN,hungCeiling # tag version, format, zone name
*date Wed Jan 15 08:34:35 2014  # latest file modification 
hungCeiling describes the void of the hung ceiling
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,0.00000,0.00000,3.55000  #   1
*vertex,4.50000,0.00000,3.55000  #   2
*vertex,4.50000,6.00000,3.55000  #   3
*vertex,0.00000,6.00000,3.55000  #   4
*vertex,0.00000,0.00000,4.25000  #   5
*vertex,4.50000,0.00000,4.25000  #   6
*vertex,4.50000,6.00000,4.25000  #   7
*vertex,0.00000,6.00000,4.25000  #   8
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
*surf,panelCW,VERT,-,-,-,dummy_pnls,OPAQUE,ANOTHER,03,07  #   1 ||< panelCW:TheChannel
*surf,Wall-2,VERT,-,-,-,mass_part,OPAQUE,SIMILAR,00,00  #   2 ||< identical environment
*surf,Wall-3,VERT,-,-,-,mass_part,OPAQUE,SIMILAR,00,00  #   3 ||< identical environment
*surf,Wall-4,VERT,-,-,-,mass_part,OPAQUE,SIMILAR,00,00  #   4 ||< identical environment
*surf,Ceil,CEIL,-,-,-,susp_floor,OPAQUE,ANOTHER,01,06  #   5 ||< Base-6:TheSpace
*surf,hungCeil,FLOR,-,-,-,susp_ceil,OPAQUE,ANOTHER,01,05  #   6 ||< hungCeil:TheSpace
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,6,    27.00 0  # zone base list
