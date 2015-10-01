<?xml version='1.0' encoding='utf-8' ?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method="xml" indent="yes" encoding="utf-8"/>
	
	<xsl:template match="/">
		<xsl:apply-templates/>
	</xsl:template>
	
	<xsl:template match="VICAR_LABEL">
	<PDS_LABEL>	
		<xsl:apply-templates select="SYSTEM"/>	
		<xsl:apply-templates select="OBJECT"/>				
		<xsl:apply-templates select="CLASS"/>
		<xsl:apply-templates select="PROPERTY"/>
		<xsl:apply-templates select="GROUP"/>
	
	</PDS_LABEL>
	</xsl:template>
	<!-- COMMENTED OUT
<xsl:for-each select="*">
</xsl:for-each>
currenlty nothing mapped to a GROUP instead of an OBJECT
	-->

	<xsl:template match="SYSTEM">
		<!-- DO NOTHING IF SYSTEM IS MATCHED -->
		<VICAR_SYSTEM>
			<xsl:apply-templates select="SYSTEM"/>
			<xsl:apply-templates/>
		</VICAR_SYSTEM>	
	</xsl:template>	

	<xsl:template match="OBJECT">
	<xsl:choose>
		<xsl:when test="@name='IDENTIFICATION'">
		<CLASS>
		<comment>/* IDENTIFICATION DATA ELEMENTS */</comment>
		<xsl:attribute name="name"><xsl:value-of select="@name"/></xsl:attribute>
			<xsl:apply-templates select="."/>
			<xsl:apply-templates/>
		</CLASS>
		</xsl:when>
		<xsl:when test="@name='TELEMETRY'">
		<CLASS>
		<comment>/* TELEMETRY DATA ELEMENTS */</comment>
		<xsl:attribute name="name"><xsl:value-of select="@name"/></xsl:attribute>
			<xsl:apply-templates select="OBJECT"/>
			<xsl:apply-templates/>
		</CLASS>
		</xsl:when>
		<xsl:when test="@name='PDS_HISTORY'">
		<CLASS>
		<comment>/* HISTORY DATA ELEMENTS */</comment>
		<xsl:attribute name="name"><xsl:value-of select="@name"/></xsl:attribute>
			<xsl:apply-templates select="OBJECT"/>
			<xsl:apply-templates/>
		</CLASS>
		</xsl:when>
		<xsl:when test="@name='COMPRESSION'">
		<CLASS>
		<comment>/* COMPRESSION RESULTS */</comment>
		<xsl:attribute name="name"><xsl:value-of select="@name"/></xsl:attribute>
			<xsl:apply-templates select="OBJECT"/>
			<xsl:apply-templates/>
		</CLASS>
		</xsl:when>
		<xsl:otherwise>
		<OBJECT>
		<xsl:attribute name="name"><xsl:value-of select="@name"/></xsl:attribute>
			<xsl:apply-templates select="OBJECT"/>
			<xsl:apply-templates/>
		</OBJECT>
		</xsl:otherwise>
	</xsl:choose>		
	</xsl:template>	
	<xsl:template match="PROPERTY">
	<xsl:choose>
		<xsl:when test="@name='IDENTIFICATION'">
		<CLASS>
		<comment>/* IDENTIFICATION DATA ELEMENTS */</comment>
		<xsl:attribute name="name"><xsl:value-of select="@name"/></xsl:attribute>
			<xsl:apply-templates select="PROPERTY"/>
			<xsl:apply-templates/>
		</CLASS>
		</xsl:when>
		<xsl:when test="@name='TELEMETRY'">
		<CLASS>
		<comment>/* TELEMETRY DATA ELEMENTS */</comment>
		<xsl:attribute name="name"><xsl:value-of select="@name"/></xsl:attribute>
			<xsl:apply-templates select="PROPERTY"/>
			<xsl:apply-templates/>
		</CLASS>
		</xsl:when>
		<xsl:when test="@name='PDS_HISTORY'">
		<CLASS>
		<comment>/* HISTORY DATA ELEMENTS */</comment>
		<xsl:attribute name="name"><xsl:value-of select="@name"/></xsl:attribute>
			<xsl:apply-templates select="PROPERTY"/>
			<xsl:apply-templates/>
		</CLASS>
		</xsl:when>
		<xsl:when test="@name='COMPRESSION'">
		<CLASS>
		<comment>/* COMPRESSION RESULTS */</comment>
		<xsl:attribute name="name"><xsl:value-of select="@name"/></xsl:attribute>
			<xsl:apply-templates select="PROPERTY"/>
			<xsl:apply-templates/>
		</CLASS>
		</xsl:when>
		<xsl:otherwise>
		<OBJECT>
		<xsl:attribute name="name"><xsl:value-of select="@name"/></xsl:attribute>
		<xsl:if test="@instance">
		<item key="INSTANCE" quoted="false"><xsl:value-of select="@instance"/></item>
		</xsl:if>
			<xsl:apply-templates select="PROPERTY"/>
			<xsl:apply-templates/>
		</OBJECT>
		</xsl:otherwise>
	</xsl:choose>

	</xsl:template>
<xsl:template match="item">   
<item>
<xsl:attribute name="name"><xsl:value-of select="@name"/></xsl:attribute>
<xsl:if test="@unit!=''">
   <xsl:attribute name="unit"><xsl:value-of select="@unit"/></xsl:attribute>
</xsl:if>
<xsl:if test="count(subitem)>0">
<xsl:apply-templates/>
</xsl:if>
<xsl:if test="count(subitem)=0">
<xsl:choose>
    <xsl:when test="@quoted">
        <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute>
	<xsl:value-of select="."/>
    </xsl:when>
    <xsl:when test="contains(., &quot;'&quot;)">
        <xsl:attribute name="quoted">true</xsl:attribute>
        <xsl:value-of select="substring(., 2, string-length(.)-2 )"/> 
    </xsl:when>
    <xsl:otherwise>
        <xsl:attribute name="quoted">false</xsl:attribute>
        <xsl:value-of select="."/>
    </xsl:otherwise>
</xsl:choose>
</xsl:if>
</item>
</xsl:template>  

<xsl:template match="subitem">
<xsl:element name="subitem">
<xsl:if test="@unit!=''">
   <xsl:attribute name="unit"><xsl:value-of select="@unit"/></xsl:attribute>
</xsl:if>
<xsl:choose>
    <xsl:when test="@quoted">
        <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute>
	<xsl:value-of select="."/>
    </xsl:when>
    <xsl:when test="contains(., &quot;'&quot;)">
        <xsl:attribute name="quoted">true</xsl:attribute>
        <xsl:value-of select="substring(., 2, string-length(.)-2)"/>
    </xsl:when>
    <xsl:otherwise>
        <xsl:attribute name="quoted">false</xsl:attribute>
        <xsl:value-of select="."/>
    </xsl:otherwise>
</xsl:choose> 
</xsl:element>
</xsl:template>


	


</xsl:stylesheet>


<!-- Stylus Studio meta-information - (c)1998-2002 eXcelon Corp.
<metaInformation>
<scenarios ><scenario default="yes" name="VicarToPDSg_scenario" userelativepaths="yes" externalpreview="no" url="aa_pds.pds.1.1.xml" htmlbaseurl="" processortype="internal" commandline="" additionalpath="" additionalclasspath="" postprocessortype="none" postprocesscommandline="" postprocessadditionalpath="" postprocessgeneratedext=""/></scenarios><MapperInfo srcSchemaPath="" srcSchemaRoot="" srcSchemaPathIsRelative="no" srcSchemaInterpretAsXML="no" destSchemaPath="" destSchemaRoot="" destSchemaPathIsRelative="no" destSchemaInterpretAsXML="no"/>
</metaInformation>
-->
