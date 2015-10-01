<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
    <!-- Author: Hyun Hee Lee, Steve Levoe -->
    <!-- This stylesheet is for MER OPS (nominal) transcoder -->
    <!-- Modified on Aug 11, 2004 for unit tagging -->
    <xsl:output encoding="utf-8" indent="yes" method="xml"/>
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
                    <xsl:attribute name="name">
                        <xsl:value-of select="@name"/>
                    </xsl:attribute>
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
                    <xsl:when test="@name='PAYLOAD_COORDINATE_SYSTEM'">
                        <comment>/* COORDINATE SYSTEM STATE: PAYLOAD */</comment>
                    </xsl:when>
                    <xsl:when test="@name='LOCAL_LEVEL_COORDINATE_SYSTEM'">
                        <comment>/* COORDINATE SYSTEM STATE: LOCAL LEVEL */</comment>
                    </xsl:when>
                    <xsl:when test="@name='SITE_COORDINATE_SYSTEM'">
                        <comment>/* COORDINATE SYSTEM STATE: SITE */</comment>
                    </xsl:when>
                    <xsl:when test="@name='SSI_FILTER_ARTICULATION_STATE'">
                        <comment>/* ARTICULATION DEVICE STATE: SURFACE STEREO IMAGER FILTER */</comment>
                    </xsl:when>
                    <xsl:when test="@name='SSI_ARTICULATION_STATE'">
                        <comment>/* ARTICULATION DEVICE STATE: SURFACE STEREO IMAGER */</comment>
                    </xsl:when>
                    <xsl:when test="@name='RAC_ARTICULATION_STATE'">
                        <comment>/* ARTICULATION DEVICE STATE: ROBOTIC ARM CAMERA */</comment>
                    </xsl:when>
                    <xsl:when test="@name='OM_ARTICULATION_STATE'">
                        <comment>/* ARTICULATION DEVICE STATE: MECA - OPTICAL MICROSCOPE */</comment>
                    </xsl:when>
                    <xsl:when test="@name='RA_ARTICULATION_STATE'">
                        <comment>/* ARTICULATION DEVICE STATE: ROBOTIC ARM */</comment>
                    </xsl:when>
                    <xsl:when test="@name='SUBFRAME_PARMS'">
                        <comment>/* SUBFRAME */</comment>
                    </xsl:when>
                    <xsl:when test="@name='COMPRESSION_PARMS'">
                        <comment>/* COMPRESSION */</comment>
                    </xsl:when>
                    <xsl:when test="@name='PAYLOAD_DERIVED_GEOMETRY_PARMS'">
                        <comment>/* DERIVED GEOMETRY DATA ELEMENTS: PAYLOAD FRAME */</comment>
                    </xsl:when>
                    <xsl:when test="@name='SITE_DERIVED_GEOMETRY_PARMS'">
                        <comment>/* DERIVED GEOMETRY DATA ELEMENTS: SITE FRAME */</comment>
                    </xsl:when>
                    <xsl:when test="@name='INSTRUMENT_STATE_PARMS'">
                        <comment>/* INSTRUMENT STATE RESULTS */</comment>
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
                        <comment/>
                    </xsl:otherwise>
                </xsl:choose>
                <GROUP>
                    <xsl:attribute name="name">
                        <xsl:value-of select="@name"/>
                    </xsl:attribute>
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
                    <xsl:attribute name="name">
                        <xsl:value-of select="@name"/>
                    </xsl:attribute>
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
                    <xsl:when test="@name='PAYLOAD_COORDINATE_SYSTEM'">
                        <comment>/* COORDINATE SYSTEM STATE: PAYLOAD */</comment>
                    </xsl:when>
                    <xsl:when test="@name='LOCAL_LEVEL_COORDINATE_SYSTEM'">
                        <comment>/* COORDINATE SYSTEM STATE: LOCAL LEVEL */</comment>
                    </xsl:when>
                    <xsl:when test="@name='SITE_COORDINATE_SYSTEM'">
                        <comment>/* COORDINATE SYSTEM STATE: SITE */</comment>
                    </xsl:when>
                    <xsl:when test="@name='SSI_FILTER_ARTICULATION_STATE'">
                        <comment>/* ARTICULATION DEVICE STATE: SURFACE STEREO IMAGER FILTER */</comment>
                    </xsl:when> 
                    <xsl:when test="@name='SSI_ARTICULATION_STATE'">
                        <comment>/* ARTICULATION DEVICE STATE: SURFACE STEREO IMAGER */</comment>
                    </xsl:when>
                    <xsl:when test="@name='RAC_ARTICULATION_STATE'">
                        <comment>/* ARTICULATION DEVICE STATE: ROBOTIC ARM CAMERA */</comment>
                    </xsl:when>
                    <xsl:when test="@name='OM_ARTICULATION_STATE'">
                        <comment>/* ARTICULATION DEVICE STATE: MECA - OPTICAL MICROSCOPE */</comment>
                    </xsl:when>
                    <xsl:when test="@name='RA_ARTICULATION_STATE'">
                        <comment>/* ARTICULATION DEVICE STATE: ROBOTIC ARM */</comment>
                    </xsl:when>
                    <xsl:when test="@name='SUBFRAME_PARMS'">
                        <comment>/* SUBFRAME */</comment>
                    </xsl:when>
                    <xsl:when test="@name='COMPRESSION_PARMS'">
                        <comment>/* COMPRESSION */</comment>
                    </xsl:when>
                    <xsl:when test="@name='PAYLOAD_DERIVED_GEOMETRY_PARMS'">
                        <comment>/* DERIVED GEOMETRY DATA ELEMENTS: PAYLOAD FRAME */</comment>
                    </xsl:when>
                    <xsl:when test="@name='SITE_DERIVED_GEOMETRY_PARMS'">
                        <comment>/* DERIVED GEOMETRY DATA ELEMENTS: SITE FRAME */</comment>
                    </xsl:when>
                    <xsl:when test="@name='INSTRUMENT_STATE_PARMS'">
                        <comment>/* INSTRUMENT STATE RESULTS */</comment>
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
                        <comment/>
                    </xsl:otherwise>
                </xsl:choose>
                <GROUP>
                    <xsl:attribute name="name">
                        <xsl:value-of select="@name"/>
                    </xsl:attribute>
                    <xsl:apply-templates select="PROPERTY"/>
                    <xsl:apply-templates/>
                </GROUP>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>
    <xsl:template match="item">
        <xsl:if test="@name!='PROPERTY'">
            <xsl:if test="count(subitem)>0">
                <xsl:if test="not(contains(@name, '__UNIT')) and not(contains(@name, '__UN'))">
                    <item>
                        <xsl:attribute name="name">
                            <xsl:value-of select="@name"/>
                        </xsl:attribute>
                        <xsl:apply-templates/>
                    </item>
                </xsl:if>
            </xsl:if>
            <xsl:if test="count(subitem)=0">
                <xsl:if test="not(contains(@name, '__UNIT')) and not(contains(@name, '__UN'))">
                    <item>
                        <xsl:choose>
                            <xsl:when test="contains(@name,'__PTR')">
                                <xsl:attribute name="name">^<xsl:value-of select="substring-before(@name, '__PTR')"/>
                                </xsl:attribute>
                            </xsl:when>
                            <!--comment this part when __PTR implementation in place-->
                            <xsl:when test="@name='MODEL_DESC'">
                                <xsl:attribute name="name">^<xsl:value-of select="@name"/>
                                </xsl:attribute>
                            </xsl:when>
                            <xsl:otherwise>
                                <xsl:attribute name="name">
                                    <xsl:value-of select="@name"/>
                                </xsl:attribute>
                            </xsl:otherwise>
                        </xsl:choose>
                        <xsl:variable name="unitName">
                            <xsl:choose>
                            <xsl:when test="@name='INSTRUMENT_TEMPERATURE_COUNT'">
                               <xsl:value-of select="concat(@name,'__UN')"/>
                            </xsl:when>
                            <xsl:otherwise>
                               <xsl:value-of select="concat(@name,'__UNIT')"/>
                            </xsl:otherwise>
                            </xsl:choose>
                        </xsl:variable>
                        <xsl:for-each select="following-sibling::item | preceding-sibling::item">
                            <xsl:if test="@name=$unitName">
                                <xsl:attribute name="unit">
                                    <xsl:value-of select="."/>
                                </xsl:attribute>
                            </xsl:if>
                        </xsl:for-each>
                        <xsl:choose>
                            <xsl:when test="contains(@name, '_TIME') and not(contains(@name,'LOCAL_TRUE_SOLAR_TIME'))">
                                <xsl:attribute name="quoted">false</xsl:attribute>
                            </xsl:when>
                            <xsl:when test="contains(@name, '_ID') or contains(@name, 'SPACECRAFT_CLOCK') or contains(@name, 'LOCAL_TRUE_SOLAR_TIME') or contains(@name, 'SPICE_FILE_NAME')">
                                <xsl:attribute name="quoted">
                                    <xsl:value-of select="@quoted"/> 
                                </xsl:attribute>
                            </xsl:when>
                            <xsl:when test="contains(., ' ') or contains(., '-') or contains(., '.TXT') or contains(.,'N/A') or contains(., 'UNK') or contains(., 'NULL') or contains(., '/')">
                                <xsl:attribute name="quoted">
                                    <xsl:value-of select="@quoted"/>
                                </xsl:attribute>
                            </xsl:when>
                            <xsl:otherwise>
                                <xsl:attribute name="quoted">false</xsl:attribute>
                            </xsl:otherwise>
                        </xsl:choose>
                        <xsl:choose>
                            <xsl:when test="contains(@name, '_TIME')">
                                <xsl:value-of select="translate(., &quot;Z&quot;, &quot;&quot;)"/>
                            </xsl:when>
                            <xsl:otherwise>
                                <xsl:value-of select="."/>
                            </xsl:otherwise>
                        </xsl:choose>
                    </item>
                </xsl:if>
            </xsl:if>
        </xsl:if>
    </xsl:template>
    <xsl:template match="subitem">
        <xsl:element name="subitem">
            <xsl:choose>
                <xsl:when test="contains(@name, '_TIME') and not(contains(@name,'LOCAL_TRUE_SOLAR_TIME'))">
                    <xsl:attribute name="quoted">false</xsl:attribute>
                </xsl:when>
                <xsl:when test="contains(@name, '_ID') or contains(@name, 'SPACECRAFT_CLOCK') or contains(@name, 'LOCAL_TRUE_SOLAR_TIME')">
                    <xsl:attribute name="quoted">
                        <xsl:value-of select="@quoted"/>
                    </xsl:attribute>
                </xsl:when>
                <xsl:when test="contains(., ' ') or contains(., '-') or contains(., '.TXT') or contains(.,'N/A') or contains(., 'UNK') or contains(., 'NULL') or contains(., '/')">
                    <xsl:attribute name="quoted">
                        <xsl:value-of select="@quoted"/>
                    </xsl:attribute>
                </xsl:when>
                <xsl:otherwise>
                    <xsl:attribute name="quoted">false</xsl:attribute>
                </xsl:otherwise>
            </xsl:choose>
            <xsl:variable name="unitName">
                <xsl:choose>
                <xsl:when test="@name='INSTRUMENT_TEMPERATURE_COUNT'">
                    <xsl:value-of select="concat(@name,'__UN')"/>
                </xsl:when>
                <xsl:otherwise>
                    <xsl:value-of select="concat(@name,'__UNIT')"/>
                </xsl:otherwise>
                </xsl:choose>
            </xsl:variable>
            <xsl:for-each select="../following-sibling::item/subitem | ../preceding-sibling::item/subitem">
                <xsl:if test="@name=$unitName">
                    <xsl:attribute name="unit">
                        <xsl:value-of select="."/>
                    </xsl:attribute>
                </xsl:if>
            </xsl:for-each>
            <xsl:value-of select="."/>
        </xsl:element>
    </xsl:template>
</xsl:stylesheet>
