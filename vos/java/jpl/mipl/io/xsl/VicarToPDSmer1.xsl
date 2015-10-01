<?xml version='1.0' encoding='UTF-8' ?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<!-- Author: Hyun Hee Lee, Steve Levoe -->
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
		<!--xsl:attribute name="name"><xsl:value-of select="@name"/></xsl:attribute-->
			<xsl:apply-templates select="."/>
			<xsl:apply-templates/>
		</CLASS>
		</xsl:when>
		<xsl:when test="@name='TELEMETRY'">
		<CLASS>
		<comment>/* TELEMETRY DATA ELEMENTS */</comment>
		<!--xsl:attribute name="name"><xsl:value-of select="@name"/></xsl:attribute-->
			<xsl:apply-templates select="OBJECT"/>
			<xsl:apply-templates/>
		</CLASS>
		</xsl:when>
		<xsl:when test="@name='PDS_HISTORY' or @name='HISTORY'">
		<CLASS>
		<comment>/* HISTORY DATA ELEMENTS */</comment>
		<!--xsl:attribute name="name"><xsl:value-of select="@name"/></xsl:attribute-->
			<xsl:apply-templates select="OBJECT"/>
			<xsl:apply-templates/>
		</CLASS>
		</xsl:when>
		<xsl:when test="@name='COMPRESSION_PARMS'">
		<comment>/* COMPRESSION RESULTS */</comment>
		<GROUP>
		<xsl:attribute name="name"><xsl:value-of select="@name"/></xsl:attribute>
			<xsl:apply-templates select="OBJECT"/>
			<xsl:apply-templates/>
		</GROUP>
		</xsl:when>
                <xsl:when test="@name='IMAGE_DATA'">
                <comment>/* IMAGE DATA ELEMENTS */</comment>
                <OBJECT>
                <xsl:attribute name="name">IMAGE</xsl:attribute>
			<xsl:apply-templates select="OBJECT"/>
			<xsl:apply-templates/>
                </OBJECT>
                </xsl:when>
		<xsl:otherwise>
                <xsl:choose>
                    <xsl:when test="@name='GEOMETRIC_CAMERA_MODEL'">
                    <comment>/* CAMERA_MODEL DATA ELEMENTS */</comment>
                    </xsl:when>
                    <xsl:when test="@name='ROVER_COORDINATE_SYSTEM'">
                    <comment>/* COORDINATE SYSTEM STATE: ROVER */</comment>
                    </xsl:when>
                    <xsl:when test="@name='LOCAL_LEVEL_COORDINATE_SYSTEM'">
                    <comment>/* COORDINATE SYSTEM STATE: LOCAL LEVEL */</comment>
                    </xsl:when>
                    <xsl:when test="@name='SITE_COORDINATE_SYSTEM'">
                    <comment>/* COORDINATE SYSTEM STATE: SITE */</comment>
                    </xsl:when>
                    <xsl:when test="@name='HGA_ARTICULATION_STATE'">
                    <comment>/* ARTICULATION DEVICE STATE: HIGH GAIN ANTENNA */</comment>
                    </xsl:when>
                    <xsl:when test="@name='FILTER_ARTICULATION_STATE'">
                    <comment>/* ARTICULATION DEVICE STATE: FILTER */</comment>
                    </xsl:when>
                    <xsl:when test="@name='IDD_ARTICULATION_STATE'">
                    <comment>/* ARTICULATION DEVICE STATE: INSTRUMENT DEPLOYMENT DEVICE */</comment>
                    </xsl:when>
                    <xsl:when test="@name='IDD_COORDINATE_SYSTEM'">
                    <comment>/* COORDINATE SYSTEM STATE: INSTRUMENT DEPLOYMENT DEVICE */</comment>
                    </xsl:when>
                    <xsl:when test="@name='PMA_ARTICULATION_STATE'">
                    <comment>/* ARTICULATION DEVICE STATE: PANCAM MAST ASSEMBLY */</comment>
                    </xsl:when>
                    <xsl:when test="@name='PMA_COORDINATE_SYSTEM'">
                    <comment>/* COORDINATE SYSTEM STATE: PANCAM MAST ASSEMBLY */</comment>
                    </xsl:when>
                    <xsl:when test="@name='CHASSIS_ARTICULATION_STATE'">
                    <comment>/* ARTICULATION DEVICE STATE: MOBILITY CHASSIS */</comment>
                    </xsl:when>
                    <xsl:when test="@name='OBSERVATION_REQUEST_PARMS'">
                    <comment>/* OBSERVATION REQUEST */</comment>
                    </xsl:when>
                    <xsl:when test="@name='IMAGE_REQUEST_PARMS'">
                    <comment>/* IMAGE REQUEST */</comment>
                    </xsl:when>
                    <xsl:when test="@name='REFERENCE_PIXEL_REQUEST_PARMS'">
                    <comment>/* REFERENCE PIXEL REQUEST */</comment>
                    </xsl:when>
                    <xsl:when test="@name='THUMBNAIL_REQUEST_PARMS'">
                    <comment>/* THUMBNAIL REQUEST */</comment>
                    </xsl:when>
                    <xsl:when test="@name='SUBFRAME_REQUEST_PARMS'">
                    <comment>/* SUBFRAME REQUEST */</comment>
                    </xsl:when>
                    <xsl:when test="@name='ROW_SUM_REQUEST_PARMS'">
                    <comment>/* ROW SUMMATION REQUEST */</comment>
                    </xsl:when>
                    <xsl:when test="@name='COLUMN_SUM_REQUEST_PARMS'">
                    <comment>/* COLUMN SUMMATION REQUEST */</comment>
                    </xsl:when>
                    <xsl:when test="@name='SUN_FIND_REQUEST_PARMS'">
                    <comment>/* SUN FIND REQUEST */</comment>
                    </xsl:when>
                    <xsl:when test="@name='HISTOGRAM_REQUEST_PARMS'">
                    <comment>/* HISTOGRAM REQUEST */</comment>
                    </xsl:when>
                    <xsl:when test="@name='ROVER_DERIVED_GEOMETRY_PARMS'">
                    <comment>/* DERIVED GEOMETRY DATA ELEMENTS: ROVER FRAME */</comment>
                    </xsl:when>
                    <xsl:when test="@name='SITE_DERIVED_GEOMETRY_PARMS'">
                    <comment>/* DERIVED GEOMETRY DATA ELEMENTS: SITE FRAME */</comment>
                    </xsl:when>
                    <xsl:when test="@name='INSTRUMENT_STATE_PARMS'">
                    <comment>/* INSTRUMENT STATE RESULTS */</comment>
                    </xsl:when>
                    <xsl:when test="@name='GROUND_SUPPORT_EQUIPMENT'">
                    <comment>/* GROUND SUPPORT EQUIPMENT DATA ELEMENTS */</comment>
                    </xsl:when>
                    <xsl:when test="@name='DERIVED_IMAGE_PARMS'">
                    <comment>/* DERIVED IMAGE DATA ELEMENTS */</comment>
                    </xsl:when>
                    <xsl:when test="@name='SURFACE_PROJECTION_PARMS'">
                    <comment>/* SURFACE PROJECTION DATA ELEMENTS */</comment>
                    </xsl:when>
                    <xsl:when test="@name='SURFACE_MODEL_PARMS'">
                    <comment>/* SURFACE MODEL DATA ELEMENTS */</comment>
                    </xsl:when>
                    <xsl:otherwise>    
                    <comment></comment>
                    </xsl:otherwise>
                </xsl:choose>
                <GROUP>
		<xsl:attribute name="name"><xsl:value-of select="@name"/></xsl:attribute>
			<xsl:apply-templates select="OBJECT"/>
			<xsl:apply-templates/>
		</GROUP>
		</xsl:otherwise>
	</xsl:choose>		
	</xsl:template>	
	<xsl:template match="PROPERTY">
	<xsl:choose>
		<xsl:when test="@name='IDENTIFICATION'">
		<CLASS>
        <comment>/* IDENTIFICATION DATA ELEMENTS */</comment>
		<!--xsl:attribute name="name"><xsl:value-of select="@name"/></xsl:attribute-->
			<xsl:apply-templates select="PROPERTY"/>
			<xsl:apply-templates/>
		</CLASS>
		</xsl:when>
		<xsl:when test="@name='TELEMETRY'">
		<CLASS>
		<comment>/* TELEMETRY DATA ELEMENTS */</comment>
		<!--xsl:attribute name="name"><xsl:value-of select="@name"/></xsl:attribute-->
			<xsl:apply-templates select="PROPERTY"/>
			<xsl:apply-templates/>
		</CLASS>
		</xsl:when>
		<xsl:when test="@name='PDS_HISTORY' or @name='HISTORY'">
		<CLASS>
		<comment>/* HISTORY DATA ELEMENTS */</comment>
		<!--xsl:attribute name="name"><xsl:value-of select="@name"/></xsl:attribute-->
			<xsl:apply-templates select="PROPERTY"/>
			<xsl:apply-templates/>
		</CLASS>
		</xsl:when>
		<xsl:when test="@name='COMPRESSION_PARMS'">
		<comment>/* COMPRESSION RESULTS */</comment>
		<GROUP>
		<xsl:attribute name="name"><xsl:value-of select="@name"/></xsl:attribute>
			<xsl:apply-templates select="PROPERTY"/>
			<xsl:apply-templates/>
		</GROUP>
		</xsl:when>
                <xsl:when test="@name='IMAGE_DATA'">
                <comment>/* IMAGE DATA ELEMENTS */</comment>
                <OBJECT>
                <xsl:attribute name="name">IMAGE</xsl:attribute>
			<xsl:apply-templates select="OBJECT"/>
			<xsl:apply-templates/>
                </OBJECT>
                </xsl:when>
		<xsl:otherwise>
                <xsl:choose>
                    <xsl:when test="@name='GEOMETRIC_CAMERA_MODEL'">
                    <comment>/* CAMERA_MODEL DATA ELEMENTS */</comment>
                    </xsl:when>
                    <xsl:when test="@name='ROVER_COORDINATE_SYSTEM'">
                    <comment>/* COORDINATE SYSTEM STATE: ROVER */</comment>
                    </xsl:when>
                    <xsl:when test="@name='LOCAL_LEVEL_COORDINATE_SYSTEM'">
                    <comment>/* COORDINATE SYSTEM STATE: LOCAL LEVEL */</comment>
                    </xsl:when>
                    <xsl:when test="@name='SITE_COORDINATE_SYSTEM'">
                    <comment>/* COORDINATE SYSTEM STATE: SITE */</comment>
                    </xsl:when>
                    <xsl:when test="@name='HGA_ARTICULATION_STATE'">
                    <comment>/* ARTICULATION DEVICE STATE: HIGH GAIN ANTENNA */</comment>
                    </xsl:when>
                    <xsl:when test="@name='FILTER_ARTICULATION_STATE'">
                    <comment>/* ARTICULATION DEVICE STATE: FILTER */</comment>
                    </xsl:when>
                    <xsl:when test="@name='IDD_ARTICULATION_STATE'">
                    <comment>/* ARTICULATION DEVICE STATE: INSTRUMENT DEPLOYMENT DEVICE */</comment>
                    </xsl:when>
                    <xsl:when test="@name='IDD_COORDINATE_SYSTEM'">
                    <comment>/* COORDINATE SYSTEM STATE: INSTRUMENT DEPLOYMENT DEVICE */</comment>
                    </xsl:when>
                    <xsl:when test="@name='PMA_ARTICULATION_STATE'">
                    <comment>/* ARTICULATION DEVICE STATE: PANCAM MAST ASSEMBLY */</comment>
                    </xsl:when>
                    <xsl:when test="@name='PMA_COORDINATE_SYSTEM'">
                    <comment>/* COORDINATE SYSTEM STATE: PANCAM MAST ASSEMBLY */</comment>
                    </xsl:when>
                    <xsl:when test="@name='CHASSIS_ARTICULATION_STATE'">
                    <comment>/* ARTICULATION DEVICE STATE: MOBILITY CHASSIS */</comment>
                    </xsl:when>
                    <xsl:when test="@name='OBSERVATION_REQUEST_PARMS'">
                    <comment>/* OBSERVATION REQUEST */</comment>
                    </xsl:when>
                    <xsl:when test="@name='IMAGE_REQUEST_PARMS'">
                    <comment>/* IMAGE REQUEST */</comment>
                    </xsl:when>
                    <xsl:when test="@name='REFERENCE_PIXEL_REQUEST_PARMS'">
                    <comment>/* REFERENCE PIXEL REQUEST */</comment>
                    </xsl:when>
                    <xsl:when test="@name='THUMBNAIL_REQUEST_PARMS'">
                    <comment>/* THUMBNAIL REQUEST */</comment>
                    </xsl:when>
                    <xsl:when test="@name='SUBFRAME_REQUEST_PARMS'">
                    <comment>/* SUBFRAME REQUEST */</comment>
                    </xsl:when>
                    <xsl:when test="@name='ROW_SUM_REQUEST_PARMS'">
                    <comment>/* ROW SUMMATION REQUEST */</comment>
                    </xsl:when>
                    <xsl:when test="@name='COLUMN_SUM_REQUEST_PARMS'">
                    <comment>/* COLUMN SUMMATION REQUEST */</comment>
                    </xsl:when>
                    <xsl:when test="@name='SUN_FIND_REQUEST_PARMS'">
                    <comment>/* SUN FIND REQUEST */</comment>
                    </xsl:when>
                    <xsl:when test="@name='HISTOGRAM_REQUEST_PARMS'">
                    <comment>/* HISTOGRAM REQUEST */</comment>
                    </xsl:when>
                    <xsl:when test="@name='ROVER_DERIVED_GEOMETRY_PARMS'">
                    <comment>/* DERIVED GEOMETRY DATA ELEMENTS: ROVER FRAME */</comment>
                    </xsl:when>
                    <xsl:when test="@name='SITE_DERIVED_GEOMETRY_PARMS'">
                    <comment>/* DERIVED GEOMETRY DATA ELEMENTS: SITE FRAME */</comment>
                    </xsl:when>
                    <xsl:when test="@name='INSTRUMENT_STATE_PARMS'">
                    <comment>/* INSTRUMENT STATE RESULTS */</comment>
                    </xsl:when>
                    <xsl:when test="@name='GROUND_SUPPORT_EQUIPMENT'">
                    <comment>/* GROUND SUPPORT EQUIPMENT DATA ELEMENTS */</comment>
                    </xsl:when>
                    <xsl:when test="@name='DERIVED_IMAGE_PARMS'">
                    <comment>/* DERIVED IMAGE DATA ELEMENTS */</comment>
                    </xsl:when>
                    <xsl:when test="@name='SURFACE_PROJECTION_PARMS'">
                    <comment>/* SURFACE PROJECTION DATA ELEMENTS */</comment>
                    </xsl:when>
                    <xsl:when test="@name='SURFACE_MODEL_PARMS'">
                    <comment>/* SURFACE MODEL DATA ELEMENTS */</comment>
                    </xsl:when>
                    <xsl:otherwise>    
                    <comment></comment>
                    </xsl:otherwise>
                </xsl:choose>
                <GROUP>
		<xsl:attribute name="name"><xsl:value-of select="@name"/></xsl:attribute>
			<xsl:apply-templates select="PROPERTY"/>
			<xsl:apply-templates/>
		</GROUP>
		</xsl:otherwise>
	</xsl:choose>
	</xsl:template>
 
 
<xsl:template match="item"> 
<xsl:if test="@name!='PROPERTY'">  
<xsl:if test="count(subitem)>0">
<xsl:if test="not(contains(@name, '__UNIT'))">
<item>
<xsl:attribute name="name"><xsl:value-of select="@name"/></xsl:attribute>
<xsl:apply-templates/>
</item>
</xsl:if>
</xsl:if>
<xsl:if test="count(subitem)=0">
    <xsl:if test="not(contains(@name, '__UNIT'))">
    <item>
    <xsl:choose>
        <xsl:when test="contains(@name,'__PTR')">
        <xsl:attribute name="name">^<xsl:value-of select ="substring-before(@name, '__PTR')"/></xsl:attribute>
        </xsl:when>
        <!--comment this part when __PTR implementation in place-->
        <xsl:when test="@name='MODEL_DESC'">
        <xsl:attribute name="name">^<xsl:value-of select="@name"/></xsl:attribute>
        </xsl:when>
        <xsl:otherwise>
        <xsl:attribute name="name"><xsl:value-of select="@name"/></xsl:attribute>
        </xsl:otherwise>
    </xsl:choose>
    <xsl:if test="contains(following-sibling::item/@name,'__UNIT')">
    <xsl:attribute name="name"><xsl:value-of select="@name"/></xsl:attribute>
    <xsl:attribute name="unit"><xsl:value-of select="following-sibling::item[@name]"/></xsl:attribute>
    </xsl:if>
    <xsl:choose>
        <xsl:when test="contains(., ' ') or contains(., '-') or contains(., '.TXT') or contains(.,'N/A') or contains(., 'UNK') or contains(., 'NULL') or contains(., '/')">
        <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute>
        </xsl:when>
        <xsl:otherwise>
        <xsl:attribute name="quoted">false</xsl:attribute>
        </xsl:otherwise>
    </xsl:choose>
    <xsl:value-of select="."/>
    </item>
    </xsl:if> 
</xsl:if>
</xsl:if>
</xsl:template>  


<xsl:template match="subitem">
<xsl:element name="subitem">
<xsl:choose>
    <xsl:when test="contains(., ' ') or contains(., '-') or contains(., '.TXT') or contains(.,'N/A') or contains(., 'UNK') or contains(., 'NULL') or contains(., '/')">
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute>
    </xsl:when>
    <xsl:otherwise>
    <xsl:attribute name="quoted">false</xsl:attribute> 
    </xsl:otherwise>
</xsl:choose>
<xsl:choose>
<xsl:when test="contains(../following-sibling::item/subitem/@name, '__UNIT')">
<xsl:attribute name="unit"><xsl:value-of select="../following-sibling::item/subitem[@name]"/></xsl:attribute>
</xsl:when>
</xsl:choose>
<xsl:value-of select="."/>
</xsl:element>
</xsl:template>

</xsl:stylesheet>


