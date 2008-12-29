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
  <html><body>
  <!-- Energy flows section -->
  <h2>Hot3000 Beta Output</h2>
  <table border="1">
    <tr bgcolor="#9acd32">
      <th>Metrics</th>
      <th>Units</th>
      <th></th>
      <xsl:apply-templates select="//*[name='building/month']" />
      <th>Annual Total</th>
    </tr>
    <xsl:apply-templates select="//*[name]" mode="HTMLOutput" /> 
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
        <td><xsl:value-of select="./WattsToGJData/bin[@type='annual']" /></td>
      </tr>
    </xsl:if>
  </xsl:template>
  
  <!-- Month header -->
  <xsl:template match="//*[name='building/month']" >
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

