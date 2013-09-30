
<xsl:stylesheet
		xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
		version="2.0">

	<xsl:output method="text"/>
	<xsl:variable name="newline">
		<xsl:text>
</xsl:text> <!--note: this closing tag has to be flush against the edge to 
		prevent tabs from sneaking in 	-->
</xsl:variable>

	<xsl:template match="/">
		<!-- Template for node name -->
		<xsl:apply-templates select="//name[../binned_data[@type='step']]" />
		<xsl:value-of select="$newline"/>
		<!-- Template for node units -->
		<xsl:apply-templates select="//units[../binned_data[@type='step']]" />
		<xsl:value-of select="$newline"/>
		<!-- Template for CSV values 
		     Note: //binned_data[...][1] will select all instances of the binned_data
		     that are the first child of their parent node. Use verbose structure
		     to get only the first instance in the entire tree -->
		<xsl:apply-templates select="self::node()/descendant::binned_data[@type='step'][1]" />


	</xsl:template>

	<xsl:template match="name">
	   <!-- print out node name and ancestors in XPath format
	        This implementation could be improved to determine how many
		ancestors exist between '/' and the current node -->
	   <xsl:value-of select="name(../../../../..)" /><xsl:text>/</xsl:text>
	   <xsl:value-of select="name(../../../..)" /><xsl:text>/</xsl:text>
 	   <xsl:value-of select="name(../../..)" /><xsl:text>/</xsl:text>
	   <xsl:value-of select="name(../..)" /><xsl:text>/</xsl:text>
	   <xsl:value-of select="." /><xsl:text>, </xsl:text>
	</xsl:template>

	<xsl:template match="units">
	   <!-- write out units -->
	   <xsl:value-of select="." /><xsl:text>, </xsl:text>
	</xsl:template>

	<xsl:template match="binned_data">
		<!-- Dump CSV data:
             use xslt 2.0 grouping facilities for SPEEDY transformations -->
        <xsl:for-each-group select="//binned_data[@type='step']/value" group-by="@ts">
            <xsl:value-of select="current-group()" separator=", " />
            <xsl:value-of select="$newline"/>
        </xsl:for-each-group>
	</xsl:template>

	<!-- suppress everything else -->
	<xsl:template match="*"></xsl:template>
</xsl:stylesheet>


		

		

	


