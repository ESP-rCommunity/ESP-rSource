

This file explains how to prepare the trnsys input data file to couple trnsys components.
This file contains the following four sections:

Section 1: General
Section 2: Contents of the trnsys input data file
Section 3: Records syntax and comments
Section 4: List of implemented unit code





*****************************************************************

        Section 1: General

*****************************************************************

1.  The trnsys input data file is named as "trnsysInput?.txt", where ? indicates the serial number
    of this trnsys component in the plant network. For example, if the trnsys component is the
    3rd one in the plant network, its input data file name is trnsysInput3.txt
2.  The trnsys input data file should be located at ../trnsys/
3.  The file has the same formats as other esp-r input text files. Data fields can be separated
    by spaces, tabs, or commas. However, if a field is a phrase with blanks, it must be followed
    by a comma.
4.  Keywords are case sensitive.
5.  Refer to the subroutine TRNPMXT located at esrucom/readTrnsys.F for details.








*****************************************************************

        Section 2: Contents of the trnsys input data file

*****************************************************************


Record         Description of fields
--------------------------------------------------------------
1              trnsys type number
2              number of nodes
3              node connections
4              variable types (ISV values)
5              number of trnsys parameters
6              1st parameter definition
7-->K          repeat record 6 for each trnsys parameter
K+1            number of trnsys inputs
K+2            1st trnsys input definition
K+3-->L        repeat record K+2 for each trnsys input
L+1            number of trnsys derivatives and the initialization method
L+2            first initial value if the initialization is set by user
L+3-->M        repeat L+2 for each initial value if the initialization is set by user
M+1            number of outputs (equal to the number of nodes)
M+2            1st output definition
M+3-->N        repeat M+2 for each output
N+1            number of additional outputs
N+2            1st additional output definition
N+3-->P        repeat N+2 for each additional output






*****************************************************************

        Section 3: Records syntax and comments

*****************************************************************


==========
Record 1:
==========
Syntax:
 TRNSYS-Type,   ?[I]

Comments:
"TRNSYS-Type" is a keyword and the integer represents the trnsys type number, between 1 and 999.


==========
Record 2:
==========
Syntax:
 Nodes-Number,   ?[I]

Comments:
"Nodes-Number" is a keyword and the integer indicates the number of nodes for this trnsys component.
The number of nodes is in the range between 1 and MNODEC (defined in plant.h). Usually, the number of
nodes is set as the same number of external connections. Therefore, the number of nodes does not need
to be equal to the number of nodes used by the trnsys type itself. For example, for trnsys type 60, up
to 100 nodes can be used to model a stratified storage water tank. However, for the purpose of coupling
trnsys type 60 with an esp-r network, the number of the nodes can be indicated as equal to the total
number of water inlets and immersed heat exchangers.

==========
Record 3:
==========
Syntax:
 Node-Connection,   ?[I],  ?[I],  ... ,?[I]    #Nodes-Number integers

Comments:
"Node-Connection" is a keyword and it is followed by Nodes-Number integers. Each integer lies in the
range between -1 and MPCONC (defined in plant.h). The integers should be provided as:
     >0 : gives expected number of receiving connections
     =0 : no entering or leaving external connections allowed
     -1 : only leaving connections allowed

==========
Record 4:
==========
Syntax:
 Variable-Type,     ?[I],  ?[I],  ... ,?[I]   #Nodes-Number integers

Comments:
"Variable-Type" is a keyword and it is followed by Nodes-Number integers. Each integer represents an
ISV value which defines nodal fluid type & coefficient generator model capabilities:
     ISV=0,10,20 node represents water + ....
     ISV=1,11,21 node represents dry air + ....
     ISV=9,19,29 node represents some solid material only
     0 <=ISV<10  model suitable for energy balance only
     10<=ISV<20  model suitable for energy + single phase mass balance
     20<=ISV<30  model suitable for energy + two phase mass balances

==========
Record 5:
==========
Syntax:
 Parameters,     ?[I]

Comments:
"Parameters" is a keyword. The integer defined the number of parameters which has a range between 0
and MADATA (defined in plant.h).

=============
Record 6 to K:
=============
Syntax:
 ID,      description,    type(= Y or N),    type-dependent-fields

Comments:
type=Y implies that the parameter does not change within esp-r. A user provided value is transferred
       to trnsys. In this case, the syntax is:

       ID,      description,    Y,    ?[R]

       For example, for the first trnsys parameter, if it takes a constant value as 56.78, then it can
       be defined as:

       1,   1st parameter description, Y,  56.78

type=N implies that the parameter changes within esp-r. A changeable parameter may be the surrounding
       temperature, the thermal physical properties, or the additional output of another component.
       In this case, the syntax is:

       ID,   description, N, ?[I], (?[I], ?[I]), ?[C*3]

       The first integer indicates the changeable parameter type, defined as:
       1 : specific heat of dry air
       2 : specific heat of water vapour
       3 : specific heat of water
       4 : density of dry air
       5 : density of water vapour
       6 : density of water
       7 : environment temperature
       8 : additional output of another component

       If the changeable parameter type (the 1st integer) lies in the range between 1 and 6, the second integer
       but not the third integer is needed. The second integer can be either positive or negative.
      >0 : the parameter indicates a thermal physical property that varies with temperature and the
           temperature of specified receiving fluid is applied. In esp-r, the receiving fluid's temperature
           can be easily traced with the node and coupling. The linked variable position is employed to track
           the temperature of the receiving fluid. This is possible because receiving fluids must be reflected 
           in trnsys inputs. Thus, the positive integer indicates the linked variable position.
      <0 : the parameter indicates a thermal physical property that varies with temperature and the
           temperature somewhere in the trnsys component is applied. The specified temperature of the trnsys
           component needs to be tracked with its additional output. In other words, if a trnsys parameter changes
           with the temperature in the trnsys component, this temperature has to be available in the component's
           additional ouputs. Thus, the negative number indicates the position of the additional output.

       If the changeable parameter type (the 1st integer) is equal to 7, neither the second nor the third integer
       is needed. 

       If the changeable parameter type (the 1st integer) is equal to 8, both the second and the the third integers
       are needed. The second integer indicates the component no while the third integer indicates the index of the
       additional output. 

       The 3-character string defines the unit code of that parameter in trnsys. The unit code is required to
       make the conversion between esp-r and trnsys if they have different units. Implemented unit codes are
       listed at the end of this file.

       Four examples are provided here to illustrate the above points.

       1st example: if the 1st parameter indicates the environment temperature, the syntax is:
       1,   1st parameter description,    N,  7,  TE1

       2nd example: if the 1st paramter indicates the density of receiving water at its 1st coupling of the 2nd
       node. Meanwhile, the 1st coupling of the 2nd node is reflected in the trnsys component's 5th input. In
       this case, the syntax is:
       1,   1st parameter description,    N,   6,  5, DN1

       3rd example: if the 1st parameter indicates the specific heat of water at the specified temperature in
       this trnsys component. Meanwhile, the specified temperature is reflected in its 4th additional output.
       In this case, the syntax is:
       1,   1st parameter description,    N,   3,  -4,  CP1

      4th example: if the 1st parameter indicates the 2nd additional output of the 6th component in a plant network.
       In this case, the syntax is:
       1,   1st parameter description,    N,   8,   6,   2,  DM1


===========
Record K+1:
===========
Syntax:
 Inputs,     ?[I]

Comments:
"Inputs" is a keyword. The integer defines the number of trnsys inputs which has a range between 0 and MTRNXIN
(defined in trnsys.h).

================
Record K+2 to L:
================
Syntax:
 ID,      description,    type,    type-dependent-fields

Comments:
type is an integer between -3 and 4, its impications are as follows.
  -3 : input is the outdoor temperature. In this case, the type-dependent fields contain only the unit code
       of temperature. For example, if the 1st input is outdoor temperature measured in oC, the syntax is:
       1,    1st input description,  -3,   TE1
  -2 : input is a control variable. In this case, the type-dependent fileds contain the serial number of that
       control and its unit code. For example, if the 1st input is the second control variable for this trnsys
       component and it is dimensionless, the syntax is:
       1,    1st input description,  -2,  2,  DM1
  -1 : input is the environment temperature. In this case, the type-dependent fileds contain only the unit code
       of temperature. For example, if the 1st input indicates the environment temperature measured in oC, the
       syntax is:
       1,    1st input description,  -1,   TE1
   0 : the input is a user-input value. In this case, the type-dependent fields contain only a real number, which
       is directly used as the input. IMPORTANT: make sure the real value is measured with the proper trnsys unit.
       For example, if the 1st input is equal to 35.8, the syntax is:
       1,    1st input description,  0,   35.8
   1 : the input is the temperature of an associated external connection. In this case, the type-dependent fields
       contain the linked node, coupling, and the unit code of temperature. For example, if the 1st input represents
       the temperature (oC) of the first coupling of the second node, the syntax is:
       1,    1st input description,  1,   2,  1,   TE1
   2 : the input is the 1st phase mass flow of an associated external connection. In this case, the type-dependent
       fields contain the linked node, coupling, and the unit code of mass flow rate. For example, if the 1st input
       represents the 1st phase mass flow rate (kg/hr) of the first coupling of the second node, the syntax is:
       1,    1st input description,   2,   2,  1,   MF1
   3 : the input is the 2nd phase mass flow of an associated external connection. In this case, the type-dependent
       fields contain the linked node, coupling, and the unit code of mass flow rate. For example, if the 1st input
       represents the 2nd phase mass flow rate (kg/hr) of the first coupling of the second node, the syntax is:
       1,    1st input description,   3,   2,  1,   MF1
   4 : the input is the sum of the 1st and 2nd phase mass flow of an associated external connection. In this case,
       the type-dependent fields contain the linked node, coupling, and the unit code of mass flow rate. For example,
       if the 1st input represents the sum of the 1st and 2nd phase mass flow rate (kg/hr) of the first coupling of
       the second node, the syntax is:
       1,    1st input description,   4,   2,  1,   MF1
   5 : the input is the additional output of a specified component. In this case, the type-dependent fields contain
       the component no, the serial number of the additional output of that component, and the unit code. For example,
       if the 1st input represents the 3rd additional output of the seond component in the plant network and its 
       dimension is kJ, the syntax is:
       1,    1st input description,   4,   2,  3,    EN1

===========
Record L+1:
===========
Syntax:
 Derivatives,  ?[I],  ?[I]

Comments:
"Derivatives" is a keyword. The first integer defines the number of derivatives, which has a range between 0 and
MTRNDER (defined in trnsys.h) The second integer is a switch which defines whether the initial values are provided
by the user or set by esp-r. The second integer has a value of 0 or 1:
     0  : initial values set by esp-r
     1  : initial values provided by the user

================
Records L+2 to M:
================
Syntax:
  ID,   ?[R]

Comments:
Records L+2 to M are needed only if the initial values are set by the user. Note that in the current stage,
the initial values are assumed to represent temperatures in oC only.

===============
Record M+1:
===============
Syntax:
 Outputs,   ?[I]

Comments:
"Outputs" is a keyword. The integer defines the number of outputs, which has the same value as the number
of nodes.

================
Records M+2 to N:
================
Syntax:
  ID,  description, ?[I], ?[C*3],     ?[I], ?[C*3],      ?[I], [C*3]

Comments:
For each output, the user needs to provide the following information:
    the position corresponding to the node temperature and the unit code of temperature 
    the position corresponding to the first phase mass flow and the unit code of 1st phase mass flow rate
    the position corresponding to the second phas mass flow and the unit code of 2nd phase mass flow rate
Because some trnsys types do not report all required mass flow in the ouputs, the position corresponding to
mass flows makes the following extensions:
    If position > 0, map to trnsys outputs
    If position < 0, map to trnsys inputs
    If position = 0, map to zero
For example, for the second node, if its temperature (oC) is the 5th trnsys output; its fist phase mass flow
rate (kg/hr) is not a trnsys output, but can be traced to the 10th trnsys input; its 2nd phase mass flow is zero,
the syntax is:
   2,  2nd node,  5,   TE1,    -10,   MF1,    0,  MF1

===============
Record N+1:
===============
Syntax:
 Additional-Outputs,   ?[I]

Comments:
"Additional-Outputs" is a keyword. The integer defines the number of additional outputs. This integer has a
range between 0 and MPCRES (defined in plant.h)

================
Records N+2 to P:
================
Syntax:
  ID,      description,    ?[I],    ?[C*3]

Comments:
The integer indicates the corresponding output position for this additional output. For example, if the 2nd
additional output corresponds to the 15th trnsys output and its dimension is kJ/hr, the syntax is:
   2,   2nd additional output description,   15,   PW1




*****************************************************************

        Section 4: List of implemented unit code

*****************************************************************

Temperature
   TE1   oC
   TE2   F
   TE3   K
Density
   DN1   kg/m^3
   DN2   kg/L
Energy
   EN1   kJ
   EN2   kWh
Power
   PW1   kJ/hr
   PW2   W
   PW3   kW
Specific heat
   CP1   kJ/kg-K
Flow rate
   MF1   kg/hr
   MF2   kg/s
 Dimensionless
   DM1   -

Comments:
Not all possible unit codes have been implemented. If necessary, the code can be easily expanded to incorporate
other unimplemented cases. Refer to the subroutine getConversionFactors at cetc/trnsys_wrapper.F
