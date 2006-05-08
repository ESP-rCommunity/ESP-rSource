<xsl:stylesheet
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:months="http://months.name"
  version="1.0">
  
  <!-- Lookup table for months name -->
  <months:name index="1">Jan</months:name>
  <months:name index="2">Feb</months:name>
  <months:name index="3">Mar</months:name>
  <months:name index="4">Apr</months:name>
  <months:name index="5">May</months:name>
  <months:name index="6">Jun</months:name>
  <months:name index="7">Jul</months:name>
  <months:name index="8">Aug</months:name>
  <months:name index="9">Sep</months:name>
  <months:name index="10">Oct</months:name>
  <months:name index="11">Nov</months:name>
  <months:name index="12">Dec</months:name>
  
  <xsl:output method="html"/>
  
  <!-- New line character-->
  <xsl:variable name="newline"><xsl:text>&#xa;</xsl:text>
</xsl:variable>



<xsl:template  match="/">
  <html>
    <body>
      <!-- Energy flows section -->
      <h2>Hot3000 Beta Output</h2>
      <table border="1">
        <tr bgcolor="#9acd32">
          <th>Metrics</th>
          <th>Units</th>
          <th></th>
          <xsl:apply-templates select="//building/month" />
          <th>Annual Total</th>
        </tr>
    
        <tr>
          <th>Plant Data</th>
        </tr>
        
        <tr>
          <th>Primary System</th>
        </tr>
        <xsl:apply-templates select="//plant/CETC_HVAC/Primary_System/*/Cooling" mode="HTMLOutput" />   
        <xsl:apply-templates select="//plant/CETC_HVAC/Primary_System/*/Heating" mode="HTMLOutput" /> 
        <tr>
          <th>Secondary System</th>
        </tr>
        <xsl:apply-templates select="//plant/CETC_HVAC/Secondary_System/*/Cooling" mode="HTMLOutput" />   
        <xsl:apply-templates select="//plant/CETC_HVAC/Secondary_System/*/Heating" mode="HTMLOutput" /> 
        
        
        <tr>
          <th>Climate Data</th>
        </tr>
	
	 <tr>
          <td>Dry Bulb Temperature</td>
        <xsl:apply-templates select="//climate/dry_bulb_temperature" mode="HTMLOutput_Custom_Name" />
        </tr>
	
	 <tr>
          <td>Relative Humidity</td>
        <xsl:apply-templates select="//climate/relative_humidity" mode="HTMLOutput_Custom_Name" />
        </tr>	
	
	 <tr>
          <td>Diffuse Horiz. Radiation</td>
        <xsl:apply-templates select="//climate/solar/Diffuse_horizontal_radiation" mode="HTMLOutput_Custom_Name" />
        </tr>	
     
	 <tr>
          <td>Direct Norm. Radiation</td>
         <xsl:apply-templates select="//climate/solar/direct_normal_radiation" mode="HTMLOutput_Custom_Name" /> 
        </tr>	
	   
        <tr>
          <td>Wind Direction (w.r.t. North)</td>
          <xsl:apply-templates select="//climate/wind/direction" mode="HTMLOutput_Custom_Name" />
        </tr>
	
        <tr>
          <td>Wind Velocity</td>
          <xsl:apply-templates select="//climate/wind/velocity" mode="HTMLOutput_Custom_Name" />
        </tr>
        
        
        <xsl:if test = "//zone_1">
          <tr>
            <th>Above Grade</th>
          </tr>
          <tr>
            <td>Temperature</td>
            <xsl:apply-templates select="//building/zone_1/air_point/temperature" mode="HTMLOutput_Custom_Name" />
          </tr>
          
          <tr>
            <td>Internal Gains</td>
            <xsl:apply-templates select="//building/zone_1/casual_gains/total" mode="HTMLOutput_Custom_Name" />
          </tr>
          <tr>
            <td>Cooling Demand</td>
            <xsl:apply-templates select="//building/zone_1/flux/heat_extraction" mode="HTMLOutput_Custom_Name" />
          </tr>
          <tr>
            <td>Heating Demand</td>
            <xsl:apply-templates select="//building/zone_1/flux/heat_injection" mode="HTMLOutput_Custom_Name" />
          </tr>	
          
          <tr>
            <td>Net Solar Gain</td>
            <xsl:apply-templates select="//building/zone_1/solar_gains/total_net_gain" mode="HTMLOutput_Custom_Name" />
          </tr>		
        </xsl:if>
      
        <xsl:if test = "//*/zone_2/*">
          <tr>
            <th>Below Grade</th>
          </tr>
          <tr>
            <td>Temperature</td>
            <xsl:apply-templates select="//building/zone_2/air_point/temperature" mode="HTMLOutput_Custom_Name" />
          </tr>
          <tr>
            <td>Internal Gains</td>
            <xsl:apply-templates select="//building/zone_2/casual_gains/total" mode="HTMLOutput_Custom_Name" />
          </tr>
          <tr>
            <td>Cooling Demand</td>
            <xsl:apply-templates select="//building/zone_2/flux/heat_extraction" mode="HTMLOutput_Custom_Name" />
          </tr>
          <tr>
            <td>Heating Demand</td>
            <xsl:apply-templates select="//building/zone_2/flux/heat_injection" mode="HTMLOutput_Custom_Name" />
          </tr>	
          <tr>
            <td>Net Solar Gain</td>
            <xsl:apply-templates select="//building/zone_2/solar_gains/total_net_gain" mode="HTMLOutput_Custom_Name" />
          </tr>	
        </xsl:if>
      
        <xsl:if test = "//*/zone_3/*">
          <tr>
            <th>Attic</th>
          </tr>
          <tr>
            <td>Temperature</td>
            <xsl:apply-templates select="//building/zone_3/air_point/temperature" mode="HTMLOutput_Custom_Name" />
          </tr>
          <tr>
            <td>Internal Gains</td>
            <xsl:apply-templates select="//building/zone_3/casual_gains/total" mode="HTMLOutput_Custom_Name" />
          </tr>
          <tr>
            <td>Cooling Demand</td>
            <xsl:apply-templates select="//building/zone_3/flux/heat_extraction" mode="HTMLOutput_Custom_Name" />
          </tr>
          <tr>
            <td>Heating Demand</td>
            <xsl:apply-templates select="//building/zone_3/flux/heat_injection" mode="HTMLOutput_Custom_Name" />
          </tr>	
          <tr>
            <td>Net Solar Gain</td>
            <xsl:apply-templates select="//building/zone_3/solar_gains/total_net_gain" mode="HTMLOutput_Custom_Name" />
          </tr>	
        </xsl:if>
        
    </table>
    
  </body></html>
</xsl:template>




<!-- Collect name & monthly binned data for energy flows -->
<xsl:template match="//*[name]" mode="HTMLOutput" >
  
    <!--output Min Max and Average-->
    <tr>
      <td><xsl:value-of select="./name" /></td> 
      <td><xsl:value-of select="./units" /></td>
      <td> (Min.)</td>
      <xsl:for-each select="./binned_data[@type='monthly']/min">
        <td style='text-align: right'><xsl:value-of select="format-number(.,'0.000')" /></td>
      </xsl:for-each>
      <xsl:for-each select="./binned_data[@type='annual']/min">
        <td style='text-align: right'><xsl:value-of select="format-number(.,'0.000')" /></td>
      </xsl:for-each>
    </tr>
    
    <tr>
      <td><!--<xsl:value-of select="./name" />--></td>
      
      <td><xsl:value-of select="./units" /></td>
      <td> (Max.)</td>
      <xsl:for-each select="./binned_data[@type='monthly']/max">
        <td style='text-align: right'><xsl:value-of select="format-number(.,'0.000')" /></td>
      </xsl:for-each>
      <xsl:for-each select="./binned_data[@type='annual']/max">
        <td style='text-align: right'><xsl:value-of select="format-number(.,'0.000')" /></td>
      </xsl:for-each>
    </tr>
    
    <tr>
      <td><!--<xsl:value-of select="./name" />--></td>
      <td><xsl:value-of select="./units" /></td>
      <td> (Avg.)</td>
      <xsl:for-each select="./binned_data[@type='monthly']/total_average">
        <td style='text-align: right'><xsl:value-of select="format-number(.,'0.000')" /></td>
      </xsl:for-each>
      <xsl:for-each select="./binned_data[@type='annual']/total_average">
        <td style='text-align: right'><xsl:value-of select="format-number(.,'0.000')" /></td>
      </xsl:for-each>
    </tr>
    
    
    <!--Gigjoule section -->
    <xsl:if test = "./WattsToGJData/bin[@type='monthly']">
      <tr>
        <td><!--<xsl:value-of select="./name" />--></td>
        <td>GJ</td>
        <td></td>
        <xsl:for-each select="./WattsToGJData/bin[@type='monthly']">
          <td style='text-align: right'><xsl:value-of select="format-number(.,'0.000')" /></td>
        </xsl:for-each>
        <td style='text-align: right'><xsl:value-of select="format-number(./WattsToGJData/bin[@type='annual'],'0.000')" /></td>
      </tr>
    </xsl:if>
  </xsl:template>
  
  <!-- Month header -->
  <xsl:template match="//building/month" >
     <xsl:for-each select="./binned_data[@type='monthly']/total_average">  
      <xsl:variable name="this_index">
        <th><xsl:value-of select="format-number(.,'0.000')"/></th>
      </xsl:variable>
      
      <th><xsl:value-of select="document('')/*/months:name[@index=round($this_index)]" />
    </th>
    
  </xsl:for-each>
</xsl:template>



<!-- Collect name & monthly binned data for energy flows -->
<xsl:template match="//*[name]" mode="HTMLOutput_Custom_Name" >
  
    <!--output Min Max and Average-->
    
       
      <td><xsl:value-of select="./units" /></td>
      <td> (Min.)</td>
      <xsl:for-each select="./binned_data[@type='monthly']/min">
        <td style='text-align: right'><xsl:value-of select="format-number(.,'0.000')" /></td>
      </xsl:for-each>
      <xsl:for-each select="./binned_data[@type='annual']/min">
        <td style='text-align: right'><xsl:value-of select="format-number(.,'0.000')" /></td>
      </xsl:for-each>
    
    
    <tr>
      <td><!--<xsl:value-of select="./name" />--></td>
      
      <td><xsl:value-of select="./units" /></td>
      <td> (Max.)</td>
      <xsl:for-each select="./binned_data[@type='monthly']/max">
        <td style='text-align: right'><xsl:value-of select="format-number(.,'0.000')" /></td>
      </xsl:for-each>
      <xsl:for-each select="./binned_data[@type='annual']/max">
        <td style='text-align: right'><xsl:value-of select="format-number(.,'0.000')" /></td>
      </xsl:for-each>
    </tr>
    
    <tr>
      <td><!--<xsl:value-of select="./name" />--></td>
      <td><xsl:value-of select="./units" /></td>
      <td> (Avg.)</td>
      <xsl:for-each select="./binned_data[@type='monthly']/total_average">
        <td style='text-align: right'><xsl:value-of select="format-number(.,'0.000')" /></td>
      </xsl:for-each>
      <xsl:for-each select="./binned_data[@type='annual']/total_average">
        <td style='text-align: right'><xsl:value-of select="format-number(.,'0.000')" /></td>
      </xsl:for-each>
    </tr>
    
    
    <!--Gigjoule section -->
    <xsl:if test = "./WattsToGJData/bin[@type='monthly']">
      <tr>
        <td><!--<xsl:value-of select="./name" />--></td>
        <td>GJ</td>
        <td></td>
        <xsl:for-each select="./WattsToGJData/bin[@type='monthly']">
          <td style='text-align: right'><xsl:value-of select="format-number(.,'0.000')" /></td>
        </xsl:for-each>
        <td style='text-align: right'><xsl:value-of select="format-number(./WattsToGJData/bin[@type='annual'],'0.000')" /></td>
      </tr>
    </xsl:if>
  </xsl:template>
  
  <!-- Month header -->
  <xsl:template match="//building/month" >
     <xsl:for-each select="./binned_data[@type='monthly']/total_average">  
      <xsl:variable name="this_index">
        <th><xsl:value-of select="format-number(.,'0.000')"/></th>
      </xsl:variable>
      
      <th><xsl:value-of select="document('')/*/months:name[@index=round($this_index)]" />
    </th>
    
  </xsl:for-each>
</xsl:template>


<!-- Collect name & monthly binned data for minimum temperature -->

</xsl:stylesheet>

