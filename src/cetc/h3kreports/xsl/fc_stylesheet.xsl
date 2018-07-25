
<xsl:stylesheet
		xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
		xmlns:months="Lookup table for month names"
		version="1.0">

	<months:name index="1">January</months:name>
	<months:name index="2">February</months:name>
	<months:name index="3">March</months:name>
	<months:name index="4">April</months:name>
	<months:name index="5">May</months:name>
	<months:name index="6">June</months:name>
	<months:name index="7">July</months:name>
	<months:name index="8">August</months:name>
	<months:name index="9">September</months:name>
	<months:name index="10">October</months:name>
	<months:name index="11">November</months:name>
	<months:name index="12">December</months:name>


	<xsl:output method="html"/>


	<xsl:template match="/">
		<html>
		<head>
<title>Fuel Cell Report</title>
<meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1" />

			<style>
	  body {
	  	font: 10px/1.5 Verdana, Arial, sans-serif;
		}
		h1 {
		    text-align: right; text-transform: uppercase;
		    font-style: italic; letter-spacing: 0.1em;
		    font-size: 200%; line-height: 1.0em;
			border-top: black 2px;
			border-bottom: black 2px;
		    
		}
		hr {margin-top 140px;
		page-break-before:always}
		b {
		text-transform: capitalize;
		}
		

		h3 {
		text-transform: capitalize;
		margin-left:30px;
		margin-bottom:0px;
		margin-top:45px;
		}
		
		table, td, tr {
		    border-spacing:0px;
		}
		
		table.chart { 
			margin-left:30px;
			border: 2px solid black;
		}
		
			
	  </style>
	  </head>
			<body>
				<h1>Hot3000 Fuel Cell Report </h1>

				<xsl:apply-templates />
			</body>
		</html>

	</xsl:template>

	<xsl:template match="building">
	<h2>Building</h2>
    <h3>Infiltration</h3>
	<p style="page-break-after:always;font-size:0pt;margin-bottom:0pt;margin-top:0pt"></p>
	<xsl:apply-templates select="./infiltration/air_changes_per_hour" mode="Average"/>
	<xsl:apply-templates select="./infiltration/air_changes_per_hour" mode="MinMax"/>
	<p style="margin-top:50px;"></p>
	<xsl:for-each select="./*">
	<xsl:if test="name() != 'infiltration'">
	<h3><xsl:value-of select="name()"/></h3>
	<xsl:apply-templates select="./air_point/temperature" mode="Average"/>
	<xsl:apply-templates select="./air_point/temperature" mode="MinMax"/>
	<xsl:apply-templates select="./casual_gains/*" mode="YearAndMJBin"/>
	<xsl:apply-templates select="./heat_injection" mode="YearAndMJBin"/>
	<xsl:apply-templates select="./solar_gains/total_net_absorbed" mode="YearAndMJBin"/>
	<xsl:apply-templates select="./solar_gains/total_net_gain" mode="YearAndMJBin"/>
	</xsl:if>
	</xsl:for-each>
	</xsl:template>

	<xsl:template match="electrical_net">
	<p style="page-break-after:always;font-size:0pt;margin-bottom:0pt;margin-top:0pt"></p>
	<p style="margin-top:130px;"></p>
    <hr />
	<h2>Electrical Net</h2>
    <xsl:apply-templates select="./generation/total_generation" mode="YearAndMJBin"/>
	  <xsl:apply-templates select="./loads/HVAC_load" mode="YearAndMJBin"/>
		<xsl:apply-templates select="./loads/occupant_load" mode="YearAndMJBin"/>
		<xsl:apply-templates select="./loads/total_load" mode="YearAndMJBin"/>
		</xsl:template>

	<xsl:template match="plant">
	<p style="page-break-after:always;font-size:0pt;margin-bottom:0pt;margin-top:0pt"></p>
	<p style="margin-top:130px;"></p>
	<hr />	
	<h2>Plant Data</h2>
		<xsl:apply-templates select="./CETC_HVAC/HRV/elec_load" mode="YearAndMJBin"/>

		<xsl:apply-templates select="./fuel_cell/Cogeneration_efficiency" mode="Average"/>
		<xsl:apply-templates select="./fuel_cell/Electric_efficiency_BOP" mode="Average"/>
		<xsl:apply-templates select="./fuel_cell/Electric_efficiency_FC" mode="Average"/>
		<xsl:apply-templates select="./fuel_cell/Fuel_HHV" mode="YearAndMJBin"/>
		<xsl:apply-templates select="./fuel_cell/Q_electric_net" mode="YearAndMJBin"/>
		<xsl:apply-templates select="./fuel_cell/Q_electric_parasitic" mode="YearAndMJBin"/>
		<xsl:apply-templates select="./fuel_cell/Q_thermal_net" mode="YearAndMJBin"/>
		<xsl:apply-templates select="./fuel_cell/fuel_flow" mode="YearAndMJBin"/>
    

		<xsl:apply-templates select="./water_tank_fuel/Temperature" mode="Average"/>
		<xsl:apply-templates select="./water_tank_fuel/burner_heat_transfer" mode="YearAndMJBin"/>
		<xsl:apply-templates select="./water_tank_fuel/domestic_hot_water/make_up_temperature" mode="Average"/>
		<xsl:apply-templates select="./water_tank_fuel/domestic_hot_water/thermal_load" mode="YearAndMJBin"/>
		<xsl:apply-templates select="./water_tank_fuel/domestic_hot_water/volumetric_draw" mode="Average"/>

		<xsl:apply-templates select="./water_tank_fuel/fuel_flow" mode="Average"/>

	</xsl:template>

	<xsl:template match="*" mode="YearAndMJBin">
		<p></p>
		<h3>
			<xsl:value-of select="name(.)"/>
		</h3>
		<table class="chart" cellspacing="0" cellpadding="3">
	
			<tr style="background:black; color:white;">
				<td>Month</td>
				<td>GJ</td>
				<td></td>
			</tr>
	  
	
			<xsl:apply-templates select="WattsToGJData"/>
		</table>
		<table style="margin-left:30px;">
			<tr>
				<td>
					<b>
						Annual Average: </b>
				</td>
				<td>
					<xsl:value-of select="format-number(binned_data[@type='annual']/total_average, '#.00')"/> 
	  (<xsl:value-of select="units"/>)</td>
			</tr>
		</table>
	</xsl:template>
	
	<xsl:template match="*" mode="MinMax">
		<table style="margin-left:30px">
			<tr>
				<td>
					<b>
						Annual Maximum: </b>
				</td>
				<td>
					<xsl:value-of select="format-number(binned_data[@type='annual']/max, '#.00')"/> 
	  (<xsl:value-of select="units"/>)</td>
	  <td>
	  <b style="margin-left:30px">Minimum: </b>
	  </td>
	  <td>
					<xsl:value-of select="format-number(binned_data[@type='annual']/min, '#.00')"/> 
	  (<xsl:value-of select="units"/>)</td>
			</tr>
		</table>
	</xsl:template>

	<xsl:template match="*" mode="Average">
		<p></p>
		<h3 style="margin-left:30px;">
		<xsl:value-of select="name(.)"/>
		</h3>
		<table class="chart"  cellspacing="0" cellpadding="3">
			<tr style="background:black; color:white;">
				<td>Month</td>
				<td>
					<xsl:value-of select="./units"/>
				</td>
				<td></td>
			</tr>
	
			<xsl:apply-templates select="./binned_data[@type='monthly']/total_average"/>
	  <!--<xsl:apply-templates select="binned_data[@type='annual']/total_average"/>-->
		</table>
	</xsl:template>

	<xsl:template match="WattsToGJData/bin">
	</xsl:template>
	<xsl:template match="WattsToGJData/bin[@type='monthly'] | WattsToGJData/bin[@type='annual']">
		<xsl:variable name="this_index">
			<xsl:value-of select="@number + 1"/>
		</xsl:variable>
		<tr>
			<xsl:attribute name="bgcolor">
				<xsl:choose>
					<xsl:when test="not(@number)">
						<xsl:text>#99CCFF</xsl:text>
					</xsl:when>
					<xsl:when test="position() mod 2">
						<xsl:text>white</xsl:text>
					</xsl:when>
					<xsl:otherwise>
						<xsl:text>#EEEEEE</xsl:text>
					</xsl:otherwise>
				</xsl:choose>
			</xsl:attribute>
			<td>
				<xsl:if test="@number">
					<b>
						<xsl:value-of select="document('')/*/months:name[@index=$this_index]"/>
					</b>
				</xsl:if>
				<xsl:if test="not(@number)">
					<b>Annual</b>
				</xsl:if>
			</td>
			<td>
				<xsl:value-of select="format-number(., '#.00')"/>
			</td>
			<td>
				<xsl:variable name="seg_length"><xsl:value-of select="300 div ../bin[@type='max']"/></xsl:variable>
				<xsl:variable name="image_length">
					<xsl:value-of select=". * $seg_length"/>px</xsl:variable>
				<img src="barb8.gif" width="{$image_length}" height="8px" />
			</td>
		</tr>
	</xsl:template>



	<xsl:template match="total_average">
		<xsl:variable name="this_index">
			<xsl:value-of select="../index + 1"/>
		</xsl:variable>
		
		<tr>
			<xsl:attribute name="bgcolor">
				<xsl:choose>
					<xsl:when test="position() mod 2">
						<xsl:text>white</xsl:text>
					</xsl:when>
					<xsl:otherwise>
						<xsl:text>#EEEEEE</xsl:text>
					</xsl:otherwise>
				</xsl:choose>
			</xsl:attribute>
		
			<td>
				<xsl:if test="../index">
					<b>
						<xsl:value-of select="document('')/*/months:name[@index=$this_index]"/>
					</b>
				</xsl:if>
		
				<xsl:if test="not(../index)">
					<b>Annual</b>
				</xsl:if>
			</td>
			<td>
				<xsl:value-of select="format-number(., '#.00')"/>
			</td>
			<td>

				<xsl:variable name="seg_length">
					<xsl:value-of select="300 div ../max"/>
				</xsl:variable>
				<xsl:variable name="image_length">
					<xsl:value-of select=". * $seg_length"/>px</xsl:variable>
				<img src="barb8.gif" width="{$image_length}" height="8px" />
			</td>
		</tr>
	</xsl:template>

</xsl:stylesheet>

