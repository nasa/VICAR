<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="2.0" 
xmlns:xs="http://www.w3.org/2001/XMLSchema"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
xmlns:sch="http://purl.oclc.org/dsdl/schematron"
xmlns:pds="http://pds.nasa.gov/pds4/pds/v1" 
xmlns:insight="http://pds.nasa.gov/pds4/insight/v0" 
>
<xsl:output method="xml" indent="yes" encoding="utf-8"/>
	<!--  .xsd is the priomary document .sch is the secondary document 
	loop thru the xsd and add info taken from the .sch-->
	<xsl:param name="schdoc" />
	<xsl:param name="docpath" />
	<xsl:param name="ldd_name" />
	<xsl:param name="user_name" />
	<xsl:param name="namespace_id" />
	<xsl:param name="current_date_param" />
	
	<!--
	<xsl:variable name="dateNow" select="current-dateTime()"/>
	-->
	<xsl:variable name="this_xsl">xsd_sch2ldd_02.xsl</xsl:variable> 
	
	<xsl:variable name="sch_fullpath1">
        <xsl:value-of select="concat($docpath,'/',$schdoc)"/>
	</xsl:variable> 
	
	<xsl:variable name="sch_fullpath2">
        <xsl:value-of select="$docpath"/>/<xsl:value-of select="$schdoc"/>
	</xsl:variable> 
	<!-- 
	<xsl:variable name="currentdate">
        <xsl:value-of select="${current_date}"/>
	</xsl:variable> 
	
	
	<xsl:variable name="currentdate_from_param">
        <xsl:value-of select="${current_date_param}"/>
	</xsl:variable> 
	-->
	
	<xsl:template match="/">
	root of document
	<xsl:apply-templates/>
	</xsl:template>
	
	<xsl:template match="xs:schema">	
	<Ingest_LDD 
    xmlns="http://pds.nasa.gov/pds4/pds/v1"
    xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
    xsi:schemaLocation="http://pds.nasa.gov/pds4/pds/v1
                        http://pds.nasa.gov/pds4/pds/v1/PDS4_PDS_11xx.xsd">

	<name><xsl:value-of select="$ldd_name"/></name>
    <ldd_version_id>1.1.0.0</ldd_version_id>
    <full_name><xsl:value-of select="$user_name"/></full_name>  <!-- cognizant PDS node person -->
    <steward_id>img</steward_id>  <!-- curating node -->
    <namespace_id><xsl:value-of select="$namespace_id"/></namespace_id>
    <comment>This is a sample Ingest_LDD file for a local (i.e., mission level) dictionary.  
        This dictionary contains some attributes (keywords) and classes.</comment>
    <input_sch><xsl:value-of select="$sch_fullpath1"/></input_sch> 
    <this_xsl>xsd_sch2ldd_02.xsl</this_xsl>	  
    <this_xsl><xsl:value-of select="concat($docpath,'/',$this_xsl)"/></this_xsl>
    <docloc><xsl:value-of select="document-location()"/></docloc>   
    <!--
    <last_modification_date_time><xsl:value-of select="format-dateTime($dateNow, '[Y0001][M01][D01]')"/></last_modification_date_time>
	-->
	<apply_templates_area>
	<xsl:apply-templates/>
	</apply_templates_area>
	
	<xsl:call-template name="test"></xsl:call-template>
	</Ingest_LDD>	
	</xsl:template>
		
	<xsl:template match="/xs:schema/xs:element">
	<DD_Attribute>
		<xsl:variable name="element_name" select="@name" />
		<name><xsl:value-of select="@name"/></name>
		<element_name><xsl:value-of select="$element_name"/></element_name>
		<type><xsl:value-of select="@type"/></type>	
		<complexType_select><xsl:value-of select="normalize-space(//xs:complexType[@name=$element_name])"/></complexType_select>
		<xsl:apply-templates select="//xs:complexType[@name=$element_name]" />
		<!--
		find the 
		<xsl:apply-templates select="PROPERTY[@name='RSM_COORDINATE_SYSTEM']" />
		<xsl:copy-of select="document($sch_fullpath1)//sch:rule"/>
		-->
		<context>
		<xsl:value-of select="normalize-space(document($sch_fullpath1)//sch:rule[@context])"/>
		</context>
		<context2>
		<xsl:variable name="dd_type" select="@type" />
		<xsl:variable name="sch_context" select="normalize-space(document($sch_fullpath1)//sch:pattern/sch:rule[contains(@context,dd_type)])" />
	   dd_type=<xsl:value-of select="$dd_type"/>
	   sch_context=<xsl:value-of select="$sch_context"/>
		</context2>
	</DD_Attribute> 
	</xsl:template>
	
	 <xsl:template match="//xs:annotation/xs:documentation">
	 <dots>
	 grandparent is <xsl:value-of select="name(../..)"/> parent is <xsl:value-of select="name(parent::*)"/> value is 
	 <xsl:value-of select="normalize-space(.)"/>
	 </dots>
	 </xsl:template>
	 
	 <!--  call a template for the selected node -->
	 
	 <!--  create a rule for handling each xs:sequence/xs:element -->
	 <xsl:template match="complexType">
	 <name><xsl:value-of select="@name"/></name>
	 <documentation><xsl:value-of select="./xs:annotation/xs:documentation"/></documentation>	
	 </xsl:template>
	 
	 <xsl:template match="sch:pattern"> 
	 <pattern>
	 <xsl:value-of select="normalize-space(.)"/>
	 </pattern>
	 </xsl:template>
	 
	 <xsl:template name="test">
	 <test>
	 Test prints
	  xsddoc <xsl:value-of select="$schdoc"/>
	   doc 2 is  <xsl:value-of select="$docpath"/>/<xsl:value-of select="$schdoc"/>
	   xsd_fullpath1 is  <xsl:value-of select="$sch_fullpath1"/>
	   xsd_fullpath2 is  <xsl:value-of select="$sch_fullpath2"/>
	   
	   <!-- 
	   currentdate param is <xsl:value-of select="$currentdate"/>
	   current_date variable is <xsl:value-of select="${current_date}"/>
	    currentdate_from_param is <xsl:value-of select="${currentdate_from_param}"/>
	   
	 Complex Type
	name  <xsl:value-of select="normalize-space(document($schdoc)//xs:complexType[@name])"/>
	name2  <xsl:value-of select="normalize-space(document($xsd_fullpath1)//xs:complexType[@name])"/>
	<Observation_Information>
	<xsl:value-of select="normalize-space(document($schdoc)//xs:complexType[@name='Observation_Information'])"/>
	</Observation_Information>
	-->
	</test>
	</xsl:template>
	
</xsl:stylesheet>