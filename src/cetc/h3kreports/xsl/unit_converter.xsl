<xsl:stylesheet version="1.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

 <xsl:output method="xml" />

<xsl:template match="node()|@*">
<xsl:copy>
<xsl:apply-templates select="node()|@*"/>
</xsl:copy>
</xsl:template>

<xsl:template match="units[.='(W)']">
<units>(KW)</units>
</xsl:template>

<xsl:template match="//min[parent::*/preceding-sibling::units='(W)']">
<min><xsl:value-of select="format-number(. div 1000,'0.000')"/></min>
</xsl:template>

<xsl:template match="//max[parent::*/preceding-sibling::units='(W)']">
<max><xsl:value-of select="format-number(. div 1000,'0.000')"/></max>
</xsl:template>

<xsl:template match="//sum[parent::*/preceding-sibling::units='(W)']">
<sum><xsl:value-of select="format-number(. div 1000,'0.000')"/></sum>
</xsl:template>

<xsl:template match="//active_average[parent::*/preceding-sibling::units='(W)']">
<active_average><xsl:value-of select="format-number(. div 1000,'0.000')"/></active_average>
</xsl:template>

<xsl:template match="//total_average[parent::*/preceding-sibling::units='(W)']">
<total_average><xsl:value-of select="format-number(. div 1000,'0.000')"/></total_average>
</xsl:template>



<!--</xsl:stylesheet>

<xsl:template match="//*[units[.='(W)']]">
	<xsl:apply-templates select="./units" mode="W_to_KW"/>
	<xsl:apply-templates select="./*/max" mode="W_to_KW"/> 
</xsl:template>

<xsl:template match="//units" mode="W_to_KW">
	<units>(kW)</units>	
</xsl:template>
<xsl:template match="/*/max" mode="W_to_KW">
	<max>should be smaller</max>	
</xsl:template>-->
</xsl:stylesheet>