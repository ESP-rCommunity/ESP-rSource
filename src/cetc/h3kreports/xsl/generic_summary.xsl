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

  <xsl:output method="text" />

  <!-- New line character-->
  <xsl:variable name="newline"><xsl:text>&#xa;</xsl:text> 
  </xsl:variable>


  
  <xsl:template  match="/">
  
<!-- Energy flows section -->
    <xsl:text>INTEGRATED ENERGY FLOW DATA</xsl:text>
    <xsl:value-of select="$newline"/>

    <!-- Header row-->
    <xsl:text>Metric, Units,</xsl:text>
    <!-- Months -->
    <xsl:apply-templates select="//*[name='building/month']" />

    <!-- Annual Total-->
    <xsl:text>Annual total,</xsl:text>
    
    <xsl:value-of select="$newline"/>
    
    <!-- Output energy flows section-->
    <xsl:apply-templates select="//*[integrated_data[@units='GJ']]" mode="monthly_watts_bin" />

    <xsl:value-of select="$newline"/>
    <xsl:value-of select="$newline"/>

    <!-- Fuel flow section -->
    <xsl:text>SYSTEM FUEL USE </xsl:text>
    <xsl:value-of select="$newline"/>

    <!-- Header row-->
    <xsl:text>Metric, units, </xsl:text>

    <!-- Months -->
    <xsl:apply-templates select="//*[name='building/month']" />

    <!-- Annual Total-->
    <xsl:text>Annual,</xsl:text>
    <xsl:value-of select="$newline"/>

    <!-- Output fuel flows section -->
    <xsl:apply-templates select="//*[contains(name,'fuel_use')]"
                         mode="fuel_use_data" />

    <xsl:value-of select="$newline"/>
    <xsl:value-of select="$newline"/>


<!-- Temperatures -->
    <xsl:text>AVERAGED TEMPERATURE DATA (oC)</xsl:text>
    <xsl:value-of select="$newline"/>

    <!-- Header row-->
    <xsl:text>Metric, , </xsl:text>

    <!-- Months -->
    <xsl:apply-templates select="//*[name='building/month']" />

    <!-- Annual Total-->
    <xsl:text>Annual,</xsl:text>
    
    <xsl:value-of select="$newline"/>
    
    <!-- Output energy flows section-->
    <xsl:apply-templates select="//*[units='(oC)']" mode="monthly_temps" />

    <xsl:value-of select="$newline"/>
    <xsl:value-of select="$newline"/>


    <!-- System efficiencies -->
    <xsl:text>SYSTEM EFFICIENCIES </xsl:text>
    <xsl:value-of select="$newline"/>

    <!-- Header row-->
    <xsl:text>Metric, , </xsl:text>

    <!-- Months -->
    <xsl:apply-templates select="//*[name='building/month']" />

    <!-- Annual Total-->
    <xsl:text>Annual,</xsl:text>
    
    <xsl:value-of select="$newline"/>
    
    <!-- Output energy flows section-->
    <xsl:apply-templates select="//*[contains(name,'efficiency')]" mode="monthly_efficiencies" />

    <xsl:value-of select="$newline"/>
    <xsl:value-of select="$newline"/>
    
    <!-- System efficiencies -->
    <xsl:text>AVERAGED MISCELLANEOUS DATA </xsl:text>
    <xsl:value-of select="$newline"/>

    <!-- Header row-->
    <xsl:text>Metric, Units, </xsl:text>

    <!-- Months -->
    <xsl:apply-templates select="//*[name='building/month']" />

    <!-- Annual Total-->
    <xsl:text>Annual,</xsl:text>
    
    <xsl:value-of select="$newline"/>

    <!-- Collect Miscellaneous data (ie data not already outputted) -->
    <xsl:apply-templates select="//*[name!='building/month' and
                                     units!='(oC)' and
                                     units!='(kg/s)' and
                                     units!='(W)' and
                                     not(contains(name,'efficiency')) and 
                                     not(contains(name,'fuel_use'))]"
                         mode="misc_data" />

        
  </xsl:template>




  
  <!-- Collect name & monthly binned data for energy flows -->
  <xsl:template match="*" mode="monthly_watts_bin" >

      <xsl:value-of select="./name" />
      <xsl:text>, </xsl:text>
      <xsl:apply-templates select="./integrated_data/@units" />
      <xsl:text>, </xsl:text>

      <xsl:apply-templates select="./integrated_data[@units='GJ']/bin[@type='monthly']" />
      <xsl:apply-templates select="./integrated_data[@units='GJ']/bin[@type='annual']" />

      <xsl:value-of select="$newline"/>
      
  </xsl:template>


  <!-- Collect name & monthly data for fuel use  -->
  <xsl:template match="*" mode="fuel_use_data" >

      <xsl:value-of select="./name" />
      <xsl:text>, </xsl:text>
      <xsl:apply-templates select="./integrated_data/@units" />
      <xsl:text>, </xsl:text>

      <xsl:apply-templates select="./integrated_data/bin[@type='monthly']" />
      <xsl:apply-templates select="./integrated_data/bin[@type='annual']" />

      <xsl:value-of select="$newline"/>

  </xsl:template>


  <!-- Spit out integrated data -->
  <xsl:template match="integrated_data/bin[@type='monthly'] | integrated_data/bin[@type='annual'] " >
    <xsl:value-of select="." /><xsl:text>, </xsl:text>
  </xsl:template>

  <xsl:template match="integrated_data/@units " >
    <xsl:value-of select="." />
  </xsl:template>


  <!-- Collect name & monthly binned data for minimum temperature -->
  <xsl:template match="*" mode="monthly_temps" >
      <xsl:value-of select="./name" />
      <xsl:text>, (min), </xsl:text>
      <xsl:apply-templates select="./binned_data[@type='monthly']/min" />
      <xsl:apply-templates select="./binned_data[@type='annual']/min" />
      <xsl:value-of select="$newline"/>
      
      <xsl:value-of select="./name" />
      <xsl:text>, (max), </xsl:text>
      <xsl:apply-templates select="./binned_data[@type='monthly']/max" />
      <xsl:apply-templates select="./binned_data[@type='annual']/max" />
      <xsl:value-of select="$newline"/>

      <xsl:value-of select="./name" />
      <xsl:text>, (avg), </xsl:text>
      <xsl:apply-templates select="./binned_data[@type='monthly']/total_average" />
      <xsl:apply-templates select="./binned_data[@type='annual']/total_average" />
      <xsl:value-of select="$newline"/>
      
      <!--<xsl:apply-templates select="./binned_data/bin[@type='annual']" />-->
      
  </xsl:template>

  
  <!-- Collect name & monthly binned data for monthly efficiencies -->
  <xsl:template match="*" mode="monthly_efficiencies" >
      <xsl:value-of select="./name" />
      <xsl:text>, </xsl:text>
      <xsl:value-of select="./units" />
      <xsl:text>, </xsl:text>

      <xsl:apply-templates select="./binned_data[@type='monthly']/active_average" />
      <xsl:apply-templates select="./binned_data[@type='annual']/active_average" />

      <xsl:value-of select="$newline"/>
      
  </xsl:template>


  <!-- Collect name & monthly data for miscellaneous parameters -->
  <xsl:template match="*" mode="misc_data" >

      <xsl:value-of select="./name" />
      <xsl:text>, </xsl:text>
      <xsl:value-of select="./units" />
      <xsl:text>, </xsl:text>
      
      <xsl:apply-templates select="./binned_data[@type='monthly']/total_average"/>
      <xsl:apply-templates select="./binned_data[@type='annual']/total_average" />

      <xsl:value-of select="$newline"/>
      
  </xsl:template>



  <!-- Spit out max/min/average data -->
  <xsl:template match="binned_data/max | binned_data/min | binned_data/active_average | binned_data/total_average " >
    <xsl:value-of select="." /><xsl:text>, </xsl:text>
  </xsl:template>


  <!-- Month header -->
  <xsl:template match="//*[name='building/month']" >
    <xsl:for-each select="./binned_data[@type='monthly']/total_average">
      
      <xsl:variable name="this_index">
        <xsl:value-of select="."/>
      </xsl:variable>
      
      <xsl:value-of select="document('')/*/months:name[@index=round($this_index)]" />
      <xsl:text>, </xsl:text>
      
    </xsl:for-each>
  
  </xsl:template>

  
  
  
  <xsl:template match="*"></xsl:template>
</xsl:stylesheet>

