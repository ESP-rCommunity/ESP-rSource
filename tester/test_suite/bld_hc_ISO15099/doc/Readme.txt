ISO 15099:2003 Convective Heat Transfer

ISO 15099 covers the calculation of the thermal performance of multi layered translucent components of buildings. Among these, glass double facades can be viewed as a prominent example for the application of this standard.

Among other algorithms, this standard gives one for the calculation of the convective heat transfer in vertical ventilated channels. This algorithm combines buoyant and "forced" components.

By defining a heat transfer coefficients file in ESP-r - typically a file with ending .htc - one can invoke this algorithm in ESP-r. As it is necessary to define from which air flow network zone the air enters the zone under consideration and this asignment is fixed, one should define three intervals in the .htc file as given in the example. The "from zone" is then typically correct, if it is below the zone under consideration.

This exemplar uses the feature described. The eqns. used are taken from version 2003, sec. 5.3 and eqn. 111.
Relevant parameters are CWIDTH - the Channel width and CHEIGHT - the channel height. Hereby,
it is generally assumed the the ventilated vertical channel is split into 
more than one zone to better model stack effects in an afn (in the exemplar, this is not done for simplicity of model reasons).
Air properties are fixed to dry air values at 300 K and 101325 Pa.

When the air is entering from outdoors, i.e. the "lowermost" zone of the vertical
channel, "forced" velocity is assumed to be zero. This assumption is based on the approach of most 
buoyancy heat transfer algorithms according to literature. These do not take a superimposed "forced"
flow into account, which seems plausible for the "first" or "lowermost" zone
of a multi-zone stacked channel.

If the air is entering from another (lower) building zone, the
flow rate through the channel from the air flow network is used to calculate 
the average velocity in eqn. 111 from ISO 15099:2003

