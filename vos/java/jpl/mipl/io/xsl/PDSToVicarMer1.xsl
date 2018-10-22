<?xml version="1.0"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

	<xsl:template match="/">
		<xsl:apply-templates/>
	</xsl:template>
	<xsl:template match="PDS_LABEL">
		<VICAR_LABEL>
			<xsl:apply-templates select="./item"/>
			<xsl:apply-templates select="COMMENT"/>
			<xsl:apply-templates select="GROUP"/>
			<xsl:apply-templates select="OBJECT"/>
		</VICAR_LABEL>
	</xsl:template>

	<xsl:template match="COMMENT">
		<xsl:choose>
			<xsl:when test=".='/* IDENTIFICATION DATA ELEMENTS */'">
			 <xsl:element name="PROPERTY">
			 <xsl:attribute name="name">IDENTIFICATION</xsl:attribute>
			 <xsl:apply-templates select="//item[@key='DATA_SET_ID']"/>
			 <xsl:apply-templates select="//item[@key='DATA_SET_NAME']"/>
			 <xsl:apply-templates select="//item[@key='COMMAND_SEQUENCE_NUMBER']"/>
			 <xsl:apply-templates select="//item[@key='FRAME_ID']"/>
			 <xsl:apply-templates select="//item[@key='FRAME_TYPE']"/>
			 <xsl:apply-templates select="//item[@key='GEOMETRY_PROJECTION_TYPE']"/>
			 <xsl:apply-templates select="//item[@key='IMAGE_ID']"/>
			 <xsl:apply-templates select="//item[@key='IMAGE_TYPE']"/>
			 <xsl:apply-templates select="//item[@key='INSTRUMENT_HOST_ID']"/>
			 <xsl:apply-templates select="//item[@key='INSTRUMENT_HOST_NAME']"/>
			 <xsl:apply-templates select="//item[@key='INSTRUMENT_ID']"/>
			 <xsl:apply-templates select="//item[@key='INSTRUMENT_NAME']"/>
			 <xsl:apply-templates select="//item[@key='INSTRUMENT_SERIAL_NUMBER']"/>
			 <xsl:apply-templates select="//item[@key='INSTRUMENT_TYPE']"/>
			 <xsl:apply-templates select="//item[@key='INSTRUMENT_VERSION_ID']"/>
			 <xsl:apply-templates select="//item[@key='LOCAL_TRUE_SOLAR_TIME']"/>
			 <xsl:apply-templates select="//item[@key='MAGNET_ID']"/>
			 <xsl:apply-templates select="//item[@key='MISSION_NAME']"/>
			 <xsl:apply-templates select="//item[@key='MISSION_PHASE_NAME']"/>
			 <xsl:apply-templates select="//item[@key='OBSERVATION_ID']"/>
			 <xsl:apply-templates select="//item[@key='PLANET_DAY_NUMBER']"/>
			 <xsl:apply-templates select="//item[@key='PRODUCER_INSTITUTION_NAME']"/>
			 <xsl:apply-templates select="//item[@key='PRODUCT_CREATION_TIME']"/>
			 <xsl:apply-templates select="//item[@key='PRODUCT_ID']"/>
			 <xsl:apply-templates select="//item[@key='PRODUCT_VERSION_ID']"/>
			 <xsl:apply-templates select="//item[@key='RELEASE_ID']"/>
			 <xsl:apply-templates select="//item[@key='ROVER_MOTION_COUNTER']"/>
			 <xsl:apply-templates select="//item[@key='ROVER_MOTION_COUNTER_NAME']"/>
			 <xsl:apply-templates select="//item[@key='SEQUENCE_ID']"/>
			 <xsl:apply-templates select="//item[@key='SEQUENCE_VERSION_ID']"/>
			 <xsl:apply-templates select="//item[@key='SOLAR_LONGITUDE']"/>
			 <xsl:apply-templates select="//item[@key='SPACECRAFT_CLOCK_CNT_PARTITION']"/>
			 <xsl:apply-templates select="//item[@key='SPACECRAFT_CLOCK_START_COUNT']"/>
			 <xsl:apply-templates select="//item[@key='SPACECRAFT_CLOCK_STOP_COUNT']"/>
			 <xsl:apply-templates select="//item[@key='START_TIME']"/>
			 <xsl:apply-templates select="//item[@key='STOP_TIME']"/>
			 <xsl:apply-templates select="//item[@key='TARGET_NAME']"/>
			 <xsl:apply-templates select="//item[@key='TARGET_TYPE']"/>
			 
			</xsl:element>
			</xsl:when>
		<xsl:when test=".='/* TELEMETRY DATA ELEMENTS */'">
			 <xsl:element name="PROPERTY">
			 <xsl:attribute name="name">TELEMETRY</xsl:attribute>
			 <xsl:apply-templates select="//item[@key='APPLICATION_PROCESS_ID']"/>
			 <xsl:apply-templates select="//item[@key='APPLICATION_PROCESS_NAME']"/>
			 <xsl:apply-templates select="//item[@key='APPLICATION_PROCESS_SUBTYPE_ID']"/>
			 <xsl:apply-templates select="//item[@key='EARTH_RECEIVED_START_TIME']"/>
			 <xsl:apply-templates select="//item[@key='EARTH_RECEIVED_STOP_TIME']"/>
			 <xsl:apply-templates select="//item[@key='EXPECTED_PACKETS']"/>
			 <xsl:apply-templates select="//item[@key='PACKET_MAP_MASK']"/>
			 <xsl:apply-templates select="//item[@key='RECEIVED_PACKETS']"/>
			 <xsl:apply-templates select="//item[@key='SPICE_FILE_NAME']"/>
			 <xsl:apply-templates select="//item[@key='TELEMETRY_PROVIDER_ID']"/>
			 <xsl:apply-templates select="//item[@key='TELEMETRY_SOURCE_NAME']"/>
			 <xsl:apply-templates select="//item[@key='TELEMETRY_SOURCE_TYPE']"/>
			 <xsl:apply-templates select="//item[@key='TLM_CMD_DISCREPANCY_FLAG']"/>			 
			 </xsl:element>
			 </xsl:when>

		<xsl:when test=".='/* HISTORY DATA ELEMENTS */'">
			 <xsl:element name="PROPERTY">
			 <xsl:attribute name="name">PDS_HISTORY</xsl:attribute>
			 <xsl:apply-templates select="//item[@key='PROCESSING_HISTORY_TEXT']"/>
			 <xsl:apply-templates select="//item[@key='SOFTWARE_NAME']"/>
			 <xsl:apply-templates select="//item[@key='SOFTWARE_VERSION_ID']"/>
			 </xsl:element>
			 </xsl:when>
		<xsl:otherwise>
		</xsl:otherwise>
		</xsl:choose>
	</xsl:template>

	<xsl:template match="OBJECT">
		<xsl:choose>
			<xsl:when test="@name='IMAGE'">
			<!-- don't copy this, -->
			</xsl:when>
			<xsl:when test="@name='IMAGE_HEADER'">
			<!-- don't copy this, -->
			</xsl:when>
			<xsl:otherwise>
				<PDS_OBJECT>
					<xsl:attribute name="name">
						<xsl:value-of select="@name"/>
					</xsl:attribute>
					<xsl:element name="item">
						<xsl:attribute name="name">OBJECT</xsl:attribute>
						<xsl:attribute name="quoted">true</xsl:attribute>
						<xsl:value-of select="@name"/>
					</xsl:element>
					<xsl:apply-templates select="OBJECT"/>
					<xsl:apply-templates/>
				</PDS_OBJECT>
			</xsl:otherwise>
		</xsl:choose>
	</xsl:template>

	<xsl:template match="GROUP">
		<PROPERTY>
			<xsl:attribute name="name">
				<xsl:value-of select="@name"/>
			</xsl:attribute>

			<xsl:element name="item">
				<xsl:attribute name="name">PROPERTY</xsl:attribute>
				<xsl:attribute name="quoted">true</xsl:attribute>
				<xsl:value-of select="@name"/>
			</xsl:element>
			<xsl:apply-templates select="GROUP"/>
			<xsl:apply-templates/>
		</PROPERTY>
	</xsl:template>

	<xsl:template match="item">
	<xsl:choose>
        <xsl:when test="@key='RECORD_TYPE'">
        <comment>/* RECORD_TYPE = <xsl:value-of select="."/> */</comment>
        </xsl:when>
		<xsl:when test="@key='RECORD_BYTES'">
        <comment>/* RECORD_BYTES = <xsl:value-of select="."/> */</comment>
        </xsl:when>
		<xsl:when test="@key='FILE_RECORDS'">
        <comment>/* FILE_RECORDS = <xsl:value-of select="."/> */</comment>
        </xsl:when>
		<xsl:when test="@key='LABEL_RECORDS'">
        <comment>/* LABEL_RECORDS = <xsl:value-of select="."/> */</comment>
        </xsl:when>
		
		<xsl:otherwise>
		<item>
		<xsl:choose>
			<xsl:when test="starts-with(@key,'^')">
			<xsl:attribute name="name"><xsl:value-of select="substring(@key, 2)"/>__PTR</xsl:attribute>
		</xsl:when>
		<xsl:otherwise>
			<xsl:attribute name="name">
				<xsl:value-of select="@key"/>
			</xsl:attribute>
		</xsl:otherwise>
		</xsl:choose>

			<xsl:if test="@unit!=''">
				<xsl:attribute name="unit">
					<xsl:value-of select="@unit"/>
				</xsl:attribute>
			</xsl:if>
			<xsl:if test="count(subitem)&gt;0">
				<xsl:apply-templates/>
			</xsl:if>
			<xsl:if test="count(subitem)=0">
				<xsl:choose>
					<xsl:when test="@quoted">
						<xsl:attribute name="quoted">
							<xsl:value-of select="@quoted"/>
						</xsl:attribute>
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
		</xsl:otherwise>
		</xsl:choose>
	</xsl:template>

	<xsl:template match="subitem">
		<xsl:element name="subitem">
			<xsl:attribute name="name">
				<xsl:value-of select="@key"/>
			</xsl:attribute>
			<xsl:if test="@units!=''">
				<xsl:attribute name="unit">
					<xsl:value-of select="@units"/>
				</xsl:attribute>
			</xsl:if>
			<xsl:choose>
				<xsl:when test="@quoted">
					<xsl:attribute name="quoted">
						<xsl:value-of select="@quoted"/>
					</xsl:attribute>
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