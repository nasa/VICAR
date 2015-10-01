<?xml version='1.0' encoding='UTF-8' ?>
<xsl:stylesheet version="2.0"
	xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
	<!-- Author: Hyun Hee Lee, Steve Levoe MSL version july 2010 JPL/MIPL - 
		Steve Levoe MSL chemCAM BLOB version feb 2011 _ Steve Levoe 
		Steve Levoe MSL archive fixes from 0-89 version mar 2013 _ Steve Levoe -->
	<xsl:output method="xml" indent="yes" encoding="utf-8" />

	<xsl:template match="/">
		<xsl:apply-templates />
	</xsl:template>

	<xsl:template match="VICAR_LABEL">
		<PDS_LABEL>
			<xsl:apply-templates select="SYSTEM" />
			<xsl:apply-templates select="CLASS" />
			<xsl:apply-templates select="PROPERTY[@name='IDENTIFICATION']" />
			<xsl:apply-templates select="PROPERTY[@name='TELEMETRY']" />
			<xsl:apply-templates select="PROPERTY[@name='PDS_HISTORY']" />
			<xsl:apply-templates select="PROPERTY[@name='GEOMETRIC_CAMERA_MODEL']" />
			<xsl:apply-templates select="PROPERTY[@name='ROVER_COORDINATE_SYSTEM']" />
			<xsl:apply-templates select="PROPERTY[@name='SITE_COORDINATE_SYSTEM']" />
			<xsl:apply-templates
				select="PROPERTY[@name='LOCAL_LEVEL_COORDINATE_SYSTEM']" />
			<xsl:apply-templates select="PROPERTY[@name='RSM_COORDINATE_SYSTEM']" />
			<xsl:apply-templates select="PROPERTY[@name='ARM_COORDINATE_SYSTEM']" />
			<xsl:apply-templates select="PROPERTY[@name='RSM_ARTICULATION_STATE']" />
			<xsl:apply-templates select="PROPERTY[@name='ARM_ARTICULATION_STATE']" />
			<xsl:apply-templates select="PROPERTY[@name='CHASSIS_ARTICULATION_STATE']" />
			<xsl:apply-templates select="PROPERTY[@name='HGA_ARTICULATION_STATE']" />
			<xsl:apply-templates select="PROPERTY[@name='OBSERVATION_REQUEST_PARMS']" />
			<xsl:apply-templates select="PROPERTY[@name='IMAGE_REQUEST_PARMS']" />
			<xsl:apply-templates select="PROPERTY[@name='SUBFRAME_REQUEST_PARMS']" />
			<xsl:apply-templates select="PROPERTY[@name='THUMBNAIL_REQUEST_PARMS']" />
			<xsl:apply-templates select="PROPERTY[@name='ROW_SUMMATION_REQUEST_PARMS']" />
			<xsl:apply-templates select="PROPERTY[@name='COLUMN_SUM_REQUEST_PARMS']" />
			<xsl:apply-templates select="PROPERTY[@name='REFERENCE_PIXEL_REQUEST_PARMS']" />
			<xsl:apply-templates select="PROPERTY[@name='HISTOGRAM_REQUEST_PARMS']" />
			<xsl:apply-templates select="PROPERTY[@name='CHEMCAM_REQUEST_PARMS']" />
			<xsl:apply-templates select="PROPERTY[@name='INSTRUMENT_STATE_PARMS']" />
			<xsl:apply-templates select="PROPERTY[@name='INITIAL_STATE_PARMS']" />
			<xsl:apply-templates select="PROPERTY[@name='COMPRESSION_PARMS']" />			
			<xsl:apply-templates select="PROPERTY[@name='ROVER_DERIVED_GEOMETRY_PARMS']" />
			<xsl:apply-templates select="PROPERTY[@name='SITE_DERIVED_GEOMETRY_PARMS']" />
			<xsl:apply-templates select="PROPERTY[@name='DERIVED_IMAGE_PARMS']" />
			<xsl:apply-templates select="PROPERTY[@name='SURFACE_PROJECTION_PARMS']" />
			<xsl:apply-templates select="PROPERTY[@name='SURFACE_MODEL_PARMS']" />
			<!-- CHEMCAM specific to handle binary label data -->
			<xsl:apply-templates select="PROPERTY[@name='CHEM_REQUEST']" />
			<xsl:apply-templates select="PROPERTY[@name='ANCILLARY_TABLE']" />
			<xsl:apply-templates select="PROPERTY[@name='CMD_REPLY_FRAME_TABLE' and @instance='1']" />			
			<xsl:apply-templates select="PROPERTY[@name='SOH_SCIDATA_COLS_TABLE' and @instance='1']" />
			<xsl:apply-templates select="PROPERTY[@name='SOH_TO_RCE_TABLE' and @instance='1']" />
			<xsl:apply-templates select="PROPERTY[@name='SOH_CHECKSUM_TABLE' and @instance='1']" />			 
			<xsl:apply-templates select="PROPERTY[@name='TAKE_IMAGE_TIME_TABLE']" />
			<xsl:apply-templates select="PROPERTY[@name='CMD_REPLY_FRAME_TABLE' and @instance='2']" />			
			<xsl:apply-templates select="PROPERTY[@name='SOH_SCIDATA_COLS_TABLE' and @instance='2']" />
			<xsl:apply-templates select="PROPERTY[@name='SOH_TO_RCE_TABLE' and @instance='2']" />
			<xsl:apply-templates select="PROPERTY[@name='SOH_CHECKSUM_TABLE' and @instance='2']" />						
			<xsl:apply-templates select="PROPERTY[@name='CMD_REPLY_FRAME_TABLE' and @instance='3']" />
			<xsl:apply-templates select="PROPERTY[@name='AUTOFOCUS_TABLE']" />
			<xsl:apply-templates select="PROPERTY[@name='MUHEADER_TABLE']" />
			<xsl:apply-templates select="PROPERTY[@name='MUFOOTER_TABLE']" />
			<xsl:apply-templates select="PROPERTY[@name='IMAGE_REPLY_TABLE']" />
			<xsl:apply-templates select="PROPERTY[@name='IMAGE_HEADER_FOOTER_TABLE']" />
			
			<!-- end of CHEMCAM specific to handle binary label data -->
			<xsl:apply-templates select="PROPERTY[@name='TABLE']" />
			<xsl:apply-templates select="PROPERTY[@name='MINI_HEADER']" />
			<xsl:apply-templates select="PROPERTY[@name='IMAGE_HEADER']" />
			<xsl:apply-templates select="PROPERTY[@name='IMAGE_DATA']" />

			<xsl:apply-templates select="PROPERTYS" />
			<xsl:apply-templates select="GROUP" />

		</PDS_LABEL>
	</xsl:template>


	<xsl:template match="SYSTEM">
		<!-- DO NOTHING IF SYSTEM IS MATCHED -->
		<VICAR_SYSTEM>
			<xsl:apply-templates select="SYSTEM" />
			<xsl:apply-templates />
		</VICAR_SYSTEM>
	</xsl:template>

	<xsl:template match="PROPERTY[@name='IDENTIFICATION']">
		<CLASS>
			<xsl:attribute name="name"><xsl:value-of select="@name" /></xsl:attribute>
			<comment>/* IDENTIFICATION DATA ELEMENTS */</comment>
			<xsl:apply-templates />
		</CLASS>
	</xsl:template>

	<xsl:template match="PROPERTY[@name='TELEMETRY']">
		<CLASS>
			<xsl:attribute name="name"><xsl:value-of select="@name" /></xsl:attribute>
			<comment>/* TELEMETRY DATA ELEMENTS */</comment>
			<xsl:apply-templates />
		</CLASS>
	</xsl:template>

	<xsl:template match="PROPERTY[@name='PDS_HISTORY']">
		<CLASS>
			<xsl:attribute name="name"><xsl:value-of select="@name" /></xsl:attribute>
			<comment>/* HISTORY DATA ELEMENTS */</comment>
			<GROUP>
				<xsl:attribute name="name"><xsl:value-of select="@name" /></xsl:attribute>
				<xsl:apply-templates />
			</GROUP>
		</CLASS>
	</xsl:template>

	<xsl:template match="PROPERTY[@name='GEOMETRIC_CAMERA_MODEL']">
		<comment>/* CAMERA_MODEL DATA ELEMENTS */</comment>
		<GROUP>
			<xsl:attribute name="name"><xsl:value-of select="@name" /></xsl:attribute>
			<xsl:apply-templates />
		</GROUP>
	</xsl:template>

	<xsl:template match="PROPERTY[@name='ROVER_COORDINATE_SYSTEM']">
		<comment>/* COORDINATE SYSTEM STATE: ROVER */</comment>
		<GROUP>
			<xsl:attribute name="name"><xsl:value-of select="@name" /></xsl:attribute>
			<xsl:apply-templates />
		</GROUP>
	</xsl:template>

	<xsl:template match="PROPERTY[@name='LOCAL_LEVEL_COORDINATE_SYSTEM']">
		<comment>/* COORDINATE SYSTEM STATE: LOCAL LEVEL */</comment>
		<GROUP>
			<xsl:attribute name="name"><xsl:value-of select="@name" /></xsl:attribute>
			<xsl:apply-templates />
		</GROUP>
	</xsl:template>

	<xsl:template match="PROPERTY[@name='SITE_COORDINATE_SYSTEM']">
		<comment>/* COORDINATE SYSTEM STATE: SITE */</comment>
		<GROUP>
			<xsl:attribute name="name"><xsl:value-of select="@name" /></xsl:attribute>
			<xsl:apply-templates />
		</GROUP>
	</xsl:template>

	<xsl:template match="PROPERTY[@name='RSM_COORDINATE_SYSTEM']">
		<comment>/* COORDINATE SYSTEM STATE: REMOTE SENSING MAST */</comment>
		<GROUP>
			<xsl:attribute name="name"><xsl:value-of select="@name" /></xsl:attribute>
			<xsl:apply-templates />
		</GROUP>
	</xsl:template>
	<xsl:template match="PROPERTY[@name='ARM_COORDINATE_SYSTEM']">
		<comment>/* COORDINATE SYSTEM STATE: ROBOTIC ARM */</comment>
		<GROUP>
			<xsl:attribute name="name"><xsl:value-of select="@name" /></xsl:attribute>
			<xsl:apply-templates />
		</GROUP>
	</xsl:template>

	<xsl:template match="PROPERTY[@name='RSM_ARTICULATION_STATE']">
		<comment>/* ARTICULATION DEVICE STATE: REMOTE SENSING MAST */</comment>
		<GROUP>
			<xsl:attribute name="name"><xsl:value-of select="@name" /></xsl:attribute>
			<xsl:apply-templates />
		</GROUP>
	</xsl:template>
	<xsl:template match="PROPERTY[@name='ARM_ARTICULATION_STATE']">
		<comment>/* ARTICULATION DEVICE STATE: ROBOTIC ARM */</comment>
		<GROUP>
			<xsl:attribute name="name"><xsl:value-of select="@name" /></xsl:attribute>
			<xsl:apply-templates />
		</GROUP>
	</xsl:template>
	<xsl:template match="PROPERTY[@name='CHASSIS_ARTICULATION_STATE']">
		<comment>/* ARTICULATION DEVICE STATE: MOBILITY CHASSIS */</comment>
		<GROUP>
			<xsl:attribute name="name"><xsl:value-of select="@name" /></xsl:attribute>
			<xsl:apply-templates />
		</GROUP>
	</xsl:template>
	<xsl:template match="PROPERTY[@name='HGA_ARTICULATION_STATE']">
		<comment>/* ARTICULATION DEVICE STATE: HIGH GAIN ANTENNA */</comment>
		<GROUP>
			<xsl:attribute name="name"><xsl:value-of select="@name" /></xsl:attribute>
			<xsl:apply-templates />
		</GROUP>
	</xsl:template>
	<xsl:template match="PROPERTY[@name='OBSERVATION_REQUEST_PARMS']">
		<comment>/* OBSERVATION REQUEST */</comment>
		<GROUP>
			<xsl:attribute name="name"><xsl:value-of select="@name" /></xsl:attribute>
			<xsl:apply-templates />
		</GROUP>
	</xsl:template>
	<xsl:template match="PROPERTY[@name='IMAGE_REQUEST_PARMS']">
		<comment>/* IMAGE REQUEST */</comment>
		<GROUP>
			<xsl:attribute name="name"><xsl:value-of select="@name" /></xsl:attribute>
			<xsl:apply-templates />
		</GROUP>
	</xsl:template>
	<xsl:template match="PROPERTY[@name='SUBFRAME_REQUEST_PARMS']">
		<comment>/* SUBFRAME REQUEST */</comment>
		<GROUP>
			<xsl:attribute name="name"><xsl:value-of select="@name" /></xsl:attribute>
			<xsl:apply-templates />
		</GROUP>
	</xsl:template>
	<xsl:template match="PROPERTY[@name='THUMBNAIL_REQUEST_PARMS']">
		<comment>/* THUMBNAIL REQUEST */</comment>
		<GROUP>
			<xsl:attribute name="name"><xsl:value-of select="@name" /></xsl:attribute>
			<xsl:apply-templates />
		</GROUP>
	</xsl:template>
	<xsl:template match="PROPERTY[@name='ROW_SUM_REQUEST_PARMS']">
		<comment>/* ROW SUMMATION REQUEST */</comment>
		<GROUP>
			<xsl:attribute name="name"><xsl:value-of select="@name" /></xsl:attribute>
			<xsl:apply-templates />
		</GROUP>
	</xsl:template>
	<xsl:template match="PROPERTY[@name='COLUMN_SUM_REQUEST_PARMS']">
		<comment>/* COLUMN SUMMATION REQUEST */</comment>
		<GROUP>
			<xsl:attribute name="name"><xsl:value-of select="@name" /></xsl:attribute>
			<xsl:apply-templates />
		</GROUP>
	</xsl:template>
	<xsl:template match="PROPERTY[@name='REFERENCE_PIXEL_REQUEST_PARMS']">
		<comment>/* REFERENCE PIXEL REQUEST */</comment>
		<GROUP>
			<xsl:attribute name="name"><xsl:value-of select="@name" /></xsl:attribute>
			<xsl:apply-templates />
		</GROUP>
	</xsl:template>
	<xsl:template match="PROPERTY[@name='HISTOGRAM_REQUEST_PARMS']">
		<comment>/* HISTOGRAM REQUEST */</comment>
		<GROUP>
			<xsl:attribute name="name"><xsl:value-of select="@name" /></xsl:attribute>
			<xsl:apply-templates />
		</GROUP>
	</xsl:template>
	<xsl:template match="PROPERTY[@name='CHEMCAM_REQUEST_PARMS']">
		<comment>/* CHEMCAM REQUEST */</comment>
		<GROUP>
			<xsl:attribute name="name"><xsl:value-of select="@name" /></xsl:attribute>
			<xsl:apply-templates />
		</GROUP>
	</xsl:template>
	<xsl:template match="PROPERTY[@name='INSTRUMENT_STATE_PARMS']">
		<comment>/* INSTRUMENT STATE RESULTS */</comment>
		<GROUP>
			<xsl:attribute name="name"><xsl:value-of select="@name" /></xsl:attribute>
			<xsl:apply-templates />
		</GROUP>
	</xsl:template>
	<xsl:template match="PROPERTY[@name='COMPRESSION_PARMS']">
		<comment>/* COMPRESSION RESULTS */</comment>
		<GROUP>
			<xsl:attribute name="name"><xsl:value-of select="@name" /></xsl:attribute>
			<xsl:apply-templates />
		</GROUP>
	</xsl:template>
	
	<xsl:template match="PROPERTY[@name='INITIAL_STATE_PARMS']">
		<comment>/* INSTRUMENT INITIAL STATE RESULTS */</comment>
		<GROUP>
			<xsl:attribute name="name"><xsl:value-of select="@name" /></xsl:attribute>
			<xsl:apply-templates />
		</GROUP>
	</xsl:template>

	<xsl:template match="PROPERTY[@name='ROVER_DERIVED_GEOMETRY_PARMS']">
		<comment>/* DERIVED GEOMETRY DATA ELEMENTS: ROVER FRAME */</comment>
		<GROUP>
			<xsl:attribute name="name"><xsl:value-of select="@name" /></xsl:attribute>
			<xsl:apply-templates />
		</GROUP>
	</xsl:template>
	<xsl:template match="PROPERTY[@name='SITE_DERIVED_GEOMETRY_PARMS']">
		<comment>/* DERIVED GEOMETRY DATA ELEMENTS: SITE FRAME */</comment>
		<GROUP>
			<xsl:attribute name="name"><xsl:value-of select="@name" /></xsl:attribute>
			<xsl:apply-templates />
		</GROUP>
	</xsl:template>
	<xsl:template match="PROPERTY[@name='DERIVED_IMAGE_PARMS']">
		<comment>/* DERIVED IMAGE DATA ELEMENTS */</comment>
		<GROUP>
			<xsl:attribute name="name"><xsl:value-of select="@name" /></xsl:attribute>
			<xsl:apply-templates />
		</GROUP>
	</xsl:template>

	<xsl:template match="PROPERTY[@name='SURFACE_PROJECTION_PARMS']">
		<comment>/* SURFACE PROJECTION DATA ELEMENTS */</comment>
		<GROUP>
			<xsl:attribute name="name"><xsl:value-of select="@name" /></xsl:attribute>
			<xsl:apply-templates />
		</GROUP>
	</xsl:template>

	<xsl:template match="PROPERTY[@name='SURFACE_MODEL_PARMS']">
		<comment>/* SURFACE MODEL DATA ELEMENTS */</comment>
		<GROUP>
			<xsl:attribute name="name"><xsl:value-of select="@name" /></xsl:attribute>
			<xsl:apply-templates />
		</GROUP>
	</xsl:template>

	<xsl:template match="PROPERTY[@name='CHEM_REQUEST']">
		<comment>/* CHEMCAM REQUEST PARAMETERS */</comment>
		<GROUP>
			<xsl:attribute name="name"><xsl:value-of select="@name" /></xsl:attribute>
			<xsl:apply-templates />
		</GROUP>
	</xsl:template>


	<xsl:template match="PROPERTY[@name='ANCILLARY_TABLE']">
		<comment>/* CHEMCAM ANCILLARY TABLE PARAMETERS */</comment>
		<OBJECT>
			<xsl:attribute name="name"><xsl:value-of select="@name" /></xsl:attribute>

			<xsl:element name="item">
				<xsl:attribute name="name">PDS_OBJECT__NAME</xsl:attribute>
				<xsl:attribute name="quoted">false</xsl:attribute>
				<xsl:value-of select="@name" />
			</xsl:element>
			<xsl:apply-templates />
		</OBJECT>
	</xsl:template>

	<xsl:template match="PROPERTY[@name='CMD_REPLY_FRAME_TABLE' and @instance='1']">
		<comment>/* COMMAND REPLY FRAME SOHB TABLE PARAMETERS */</comment>
		<OBJECT>
			<!-- <xsl:attribute name="instance"><xsl:value-of select="@instance"/></xsl:attribute> -->
			<xsl:attribute name="name">CMD_REPLY_FRAME_SOHB_TABLE</xsl:attribute>

			<xsl:element name="item">
				<xsl:attribute name="name">PDS_OBJECT__NAME</xsl:attribute>
				<xsl:attribute name="quoted">false</xsl:attribute>
				CMD_REPLY_FRAME_SOHB_TABLE
			</xsl:element>
			
			<xsl:apply-templates />
		</OBJECT>
	</xsl:template>
	
	<xsl:template match="PROPERTY[@name='CMD_REPLY_FRAME_TABLE' and @instance='2']">
		<comment>/* COMMAND REPLY FRAME SOHA TABLE PARAMETERS */</comment>
		<OBJECT>
			<!-- <xsl:attribute name="instance"><xsl:value-of select="@instance"/></xsl:attribute> -->
			<xsl:attribute name="name">CMD_REPLY_FRAME_SOHA_TABLE</xsl:attribute>

			<xsl:element name="item">
				<xsl:attribute name="name">PDS_OBJECT__NAME</xsl:attribute>
				<xsl:attribute name="quoted">false</xsl:attribute>
				CMD_REPLY_FRAME_SOHA_TABLE
			</xsl:element>
			
			<xsl:apply-templates />
		</OBJECT>
	</xsl:template>

	<xsl:template match="PROPERTY[@name='CMD_REPLY_FRAME_TABLE' and @instance='3']">
		<comment>/* COMMAND REPLY FRAME AUTOFOCUS TABLE PARAMETERS */</comment>
		<OBJECT>
			<!-- <xsl:attribute name="instance"><xsl:value-of select="@instance"/></xsl:attribute> -->
			<xsl:attribute name="name">CMD_REPLY_FRAME_AF_TABLE</xsl:attribute>

			<xsl:element name="item">
				<xsl:attribute name="name">PDS_OBJECT__NAME</xsl:attribute>
				<xsl:attribute name="quoted">false</xsl:attribute>
				CMD_REPLY_FRAME_AF_TABLE
			</xsl:element>
			
			<xsl:apply-templates />
		</OBJECT>
	</xsl:template>
	

	<xsl:template match="PROPERTY[@name='TAKE_IMAGE_TIME_TABLE']">
		<comment>/* TAKE IMAGE TIME TABLE PARAMETERS */</comment>
		<OBJECT>
			<xsl:attribute name="name"><xsl:value-of select="@name" /></xsl:attribute>

			<xsl:element name="item">
				<xsl:attribute name="name">PDS_OBJECT__NAME</xsl:attribute>
				<xsl:attribute name="quoted">false</xsl:attribute>
				<xsl:value-of select="@name" />
			</xsl:element>
			<xsl:apply-templates />
		</OBJECT>
	</xsl:template>
	

	<xsl:template match="PROPERTY[@name='AUTOFOCUS_TABLE']">
		<comment>/* CHEMCAM AUTOFOCUS TABLE PARAMETERS */</comment>
		<OBJECT>
			<xsl:attribute name="name"><xsl:value-of select="@name" /></xsl:attribute>

			<xsl:element name="item">
				<xsl:attribute name="name">PDS_OBJECT__NAME</xsl:attribute>
				<xsl:attribute name="quoted">false</xsl:attribute>
				<xsl:value-of select="@name" />
			</xsl:element>
			<xsl:apply-templates />
		</OBJECT>
	</xsl:template>

	<xsl:template match="PROPERTY[@name='IMAGE_REPLY_TABLE']">
		<comment>/* CHEMCAM IMAGE REPLY TABLE PARAMETERS */</comment>
		<OBJECT>
			<xsl:attribute name="name"><xsl:value-of select="@name" /></xsl:attribute>

			<xsl:element name="item">
				<xsl:attribute name="name">PDS_OBJECT__NAME</xsl:attribute>
				<xsl:attribute name="quoted">false</xsl:attribute>
				<xsl:value-of select="@name" />
			</xsl:element>
			<xsl:apply-templates />
		</OBJECT>
	</xsl:template>


	<xsl:template match="PROPERTY[@name='IMAGE_HEADER_FOOTER_TABLE']">
		<comment>/* CHEMCAM IMAGE HEADER FOOTER TABLE PARAMETERS */</comment>
		<OBJECT>
			<xsl:attribute name="name"><xsl:value-of select="@name" /></xsl:attribute>

			<xsl:element name="item">
				<xsl:attribute name="name">PDS_OBJECT__NAME</xsl:attribute>
				<xsl:attribute name="quoted">false</xsl:attribute>
				<xsl:value-of select="@name" />
			</xsl:element>
			<xsl:apply-templates />
		</OBJECT>
	</xsl:template>
	
	<!--  NEW CHEM CAAM LABEL properties 
	SOH_SCIDATA_COLS_TABLE
	SOH_TO_RCE_TABLE
	SOH_CHECKSUM_TABLE 
	
	7-16-2012
	MUHEADER_TABLE
	MUFOOTER_TABLE
	-->
	
	<xsl:template match="PROPERTY[@name='MUHEADER_TABLE'  and @instance='1']">
		<comment>/* CHEMCAM MU HEADER TABLE */</comment>
		<OBJECT>
			<!-- <xsl:attribute name="instance"><xsl:value-of select="@instance"/></xsl:attribute> -->
			<xsl:attribute name="name">MUHEADER_TABLE</xsl:attribute>

			<xsl:element name="item">
				<xsl:attribute name="name">PDS_OBJECT__NAME</xsl:attribute>
				<xsl:attribute name="quoted">false</xsl:attribute>
				MUHEADER_TABLE
			</xsl:element>
	
			<xsl:apply-templates />
		</OBJECT>
	</xsl:template>

	<xsl:template match="PROPERTY[@name='MUFOOTER_TABLE'  and @instance='1']">
		<comment>/* CHEMCAM MU FOOTER TABLE */</comment>
		<OBJECT>
			<!-- <xsl:attribute name="instance"><xsl:value-of select="@instance"/></xsl:attribute> -->
			<xsl:attribute name="name">MUFOOTER_TABLE</xsl:attribute>

			<xsl:element name="item">
				<xsl:attribute name="name">PDS_OBJECT__NAME</xsl:attribute>
				<xsl:attribute name="quoted">false</xsl:attribute>
				MUFOOTER_TABLE
			</xsl:element>
	
			<xsl:apply-templates />
		</OBJECT>
	</xsl:template>



	
	<xsl:template match="PROPERTY[@name='SOH_SCIDATA_COLS_TABLE'  and @instance='1']">
		<comment>/* CHEMCAM BEFORE SCIENCE DATA COLUMNS TABLE */</comment>
		<OBJECT>
			<!-- <xsl:attribute name="instance"><xsl:value-of select="@instance"/></xsl:attribute> -->
			<xsl:attribute name="name">SOH_BEFORE_SCIDATA_COLS_TABLE</xsl:attribute>

			<xsl:element name="item">
				<xsl:attribute name="name">PDS_OBJECT__NAME</xsl:attribute>
				<xsl:attribute name="quoted">false</xsl:attribute>
				SOH_BEFORE_SCIDATA_COLS_TABLE
			</xsl:element>
	
			<xsl:apply-templates />
		</OBJECT>
	</xsl:template>

	<xsl:template match="PROPERTY[@name='SOH_SCIDATA_COLS_TABLE'  and @instance='2']">
		<comment>/* CHEMCAM AFTER SCIENCE DATA COLUMNS TABLE */</comment>
		<OBJECT>
			<!-- <xsl:attribute name="instance"><xsl:value-of select="@instance"/></xsl:attribute> -->
			<xsl:attribute name="name">SOH_AFTER_SCIDATA_COLS_TABLE</xsl:attribute>

			<xsl:element name="item">
				<xsl:attribute name="name">PDS_OBJECT__NAME</xsl:attribute>
				<xsl:attribute name="quoted">false</xsl:attribute>
				SOH_AFTER_SCIDATA_COLS_TABLE
			</xsl:element>
	
			<xsl:apply-templates />
		</OBJECT>
	</xsl:template>
	
	<xsl:template match="PROPERTY[@name='SOH_TO_RCE_TABLE'  and @instance='1']">
		<comment>/* CHEMCAM BEFORE TO RCE TABLE */</comment>
		<OBJECT>
			<!-- <xsl:attribute name="instance"><xsl:value-of select="@instance"/></xsl:attribute> -->
			<xsl:attribute name="name">SOH_BEFORE_TO_RCE_TABLE</xsl:attribute>

			<xsl:element name="item">
				<xsl:attribute name="name">PDS_OBJECT__NAME</xsl:attribute>
				<xsl:attribute name="quoted">false</xsl:attribute>
				SOH_BEFORE_TO_RCE_TABLE
			</xsl:element>
			
			<xsl:apply-templates />
		</OBJECT>
	</xsl:template>

	<xsl:template match="PROPERTY[@name='SOH_TO_RCE_TABLE' and @instance='2']">
		<comment>/* CHEMCAM AFTER TO RCE TABLE */</comment>
		<OBJECT>
			<!-- <xsl:attribute name="instance"><xsl:value-of select="@instance"/></xsl:attribute> -->
			<xsl:attribute name="name">SOH_AFTER_TO_RCE_TABLE</xsl:attribute>

			<xsl:element name="item">
				<xsl:attribute name="name">PDS_OBJECT__NAME</xsl:attribute>
				<xsl:attribute name="quoted">false</xsl:attribute>
				SOH_AFTER_TO_RCE_TABLE
			</xsl:element>
			
			<xsl:apply-templates />
		</OBJECT>
	</xsl:template>
	
	<xsl:template match="PROPERTY[@name='SOH_CHECKSUM_TABLE' and @instance='1']">
		<comment>/* CHEMCAM BEFORE CHECKSUM TABLE */</comment>
		<OBJECT>
			<!-- <xsl:attribute name="instance"><xsl:value-of select="@instance"/></xsl:attribute> -->
			<xsl:attribute name="name">SOH_BEFORE_CHECKSUM_TABLE</xsl:attribute>

			<xsl:element name="item">
				<xsl:attribute name="name">PDS_OBJECT__NAME</xsl:attribute>
				<xsl:attribute name="quoted">false</xsl:attribute>
				SOH_BEFORE_CHECKSUM_TABLE
			</xsl:element>
			
			<xsl:apply-templates />
		</OBJECT>
	</xsl:template>	
	
	<xsl:template match="PROPERTY[@name='SOH_CHECKSUM_TABLE' and @instance='2']">
		<comment>/* CHEMCAM AFTER CHECKSUM TABLE */</comment>
		<OBJECT>
			<!-- <xsl:attribute name="instance"><xsl:value-of select="@instance"/></xsl:attribute> -->
			<xsl:attribute name="name">SOH_AFTER_CHECKSUM_TABLE</xsl:attribute>

			<xsl:element name="item">
				<xsl:attribute name="name">PDS_OBJECT__NAME</xsl:attribute>
				<xsl:attribute name="quoted">false</xsl:attribute>
				SOH_AFTER_CHECKSUM_TABLE
			</xsl:element>
			
			<xsl:apply-templates />
		</OBJECT>
	</xsl:template>	
	<!-- SHOULD THIS BE A GROUP ?? -->
	<xsl:template match="PROPERTY[@name='TABLE']">
		<comment>/* DATA OBJECT */</comment>
		<OBJECT>
			<xsl:attribute name="name"><xsl:value-of select="@name" /></xsl:attribute>
			<xsl:apply-templates />
		</OBJECT>
	</xsl:template>

    <xsl:template match="PROPERTY[@name='MINI_HEADER']">
		<comment>/* MINI_HEADER DATA ELEMENTS */</comment>
		<GROUP>
			<xsl:attribute name="name"><xsl:value-of select="@name" /></xsl:attribute>
			<xsl:apply-templates />
		</GROUP>
	</xsl:template>
	
	<xsl:template match="PROPERTY[@name='IMAGE_HEADER']">
		<comment>/* IMAGE_HEADER DATA ELEMENTS */</comment>
		<GROUP>
			<xsl:attribute name="name"><xsl:value-of select="@name" /></xsl:attribute>
			<xsl:apply-templates />
		</GROUP>
	</xsl:template>
	<xsl:template match="PROPERTY[@name='IMAGE_DATA']">
		<comment>/* IMAGE DATA ELEMENTS */</comment>
		<OBJECT>
			<xsl:attribute name="name">IMAGE</xsl:attribute>
			<xsl:apply-templates />
		</OBJECT>
	</xsl:template>



	<xsl:template match="item">

		<xsl:variable name="vCaps" select="'ABCDEFGHIJKLMNOPQRSTUVWXYZ'" />

		<xsl:variable name="vNoECaps" select="'ABCDFGHIJKLMNOPQRSTUVWXYZ'" />

		<xsl:variable name="nonCaps" select="'abcdefghijklmnopqrstuvwxyz'" />

		<xsl:variable name="nonCapsNoe" select="'abcdfghijklmnopqrstuvwxyz'" />

		<xsl:variable name="nonLetterNumber1" select="'!@#$%^*;,?/=+\'" />

		<xsl:variable name="nonLetterNumber2" select="'!@#$%^*;,?/=\'" />

		<xsl:variable name="nonLetterNumber" select="'!@#$%^*;:,?/=+\'" />

		<xsl:variable name="digits" select="'0123456789'" />

		<xsl:variable name="digitsColon" select="'0123456789:'" />

		<xsl:variable name="digitsPlusMinus" select="'+-0123456789'" />

		<xsl:variable name="digitsPlusMinusDot" select="'+-0123456789.'" />

		<xsl:variable name="floatingPt" select="'0123456789Ee+-.'" />

		<xsl:if test="@name!='PROPERTY'">
			<xsl:if test="count(subitem)>0">

				<xsl:if test="not(contains(@name, '__UNIT'))">
					<item>
						<xsl:attribute name="name"><xsl:value-of
							select="@name" /></xsl:attribute>
						<xsl:apply-templates />
					</item>
				</xsl:if>

			</xsl:if>

			<xsl:if test="count(subitem)=0">
				<xsl:choose>
					<!-- THESE 3 NEED TO PREVENT THE LINE 330 TEST - THESE SHOULD BE ELSE'S 
						OR A CHOOSE ' -->
					<xsl:when
						test="parent::PROPERTY[@name ='IMAGE_DATA'] and contains(@name,'LINES')">
						<HIDE>remove IMAGE_DATA LINES</HIDE>
					</xsl:when>

					<xsl:when
						test="parent::PROPERTY[@name ='IMAGE_DATA'] and contains(@name,'SAMPLE_BITS')">
						<HIDE>remove IMAGE_DATA SAMPLE_BITS</HIDE>
					</xsl:when>

					<xsl:when
						test="parent::PROPERTY[@name ='IMAGE_DATA'] and contains(@name,'SAMPLE_TYPE')">
						<HIDE>remove IMAGE_DATA SAMPLE_TYPE</HIDE>
					</xsl:when>


					<xsl:when test="not(contains(@name, '__UNIT'))">
						<xsl:element name="item">


							<xsl:choose>
								<xsl:when test="contains(@name,'__PTR')">
									<xsl:attribute name="name">^<xsl:value-of
										select="substring-before(@name, '__PTR')" /></xsl:attribute>
								</xsl:when>

								<!--comment this part when __PTR implementation in place -->

								<xsl:when test="@name='MODEL_DESC'">
									<xsl:attribute name="name">^<xsl:value-of
										select="@name" /></xsl:attribute>
								</xsl:when>

								<xsl:otherwise>
									<xsl:attribute name="name"><xsl:value-of
										select="@name" /></xsl:attribute>
								</xsl:otherwise>
							</xsl:choose>
								
							<!--  we cannot depend on the order of items in the VICAR label 
							Check to see if the item we have now has a __UNIT item associated with it
							Construct the item name to look for. If we find it, then add the value as a "unit" attribute
							-->
							<xsl:variable name="name_unit">
								<xsl:value-of select="@name" />
							</xsl:variable>
							<xsl:variable name="name_unit_parent">
								<xsl:value-of select="local-name(parent::*)" />
							</xsl:variable>
							<xsl:variable name="name_unit_parent_name">
								<xsl:value-of select="parent[@name]" />
							</xsl:variable>
							<xsl:variable name="dotdot_parent_name">
								<xsl:value-of select="../@name" />
							</xsl:variable>

							<xsl:variable name="name_units" select="concat($name_unit,'__UNIT')" />
							
							<!-- put these into the document to see what is going on
							<xsl:attribute name="item_name_units"><xsl:value-of select="$name_units"/></xsl:attribute>
							<xsl:attribute name="name_unit_parent"><xsl:value-of select="$name_unit_parent"/></xsl:attribute>
							<xsl:attribute name="name_unit_parent_name"><xsl:value-of select="$name_unit_parent_name"/></xsl:attribute>
							<xsl:attribute name="dotdot_parent_name"><xsl:value-of select="$dotdot_parent_name"/></xsl:attribute>
							-->
							<xsl:choose>			 
			 					<xsl:when test="contains(following-sibling::item[@name],'__UNIT')" >
			   						<xsl:attribute name="unit"><xsl:value-of select="following-sibling::item[@name]"/></xsl:attribute>
			 					</xsl:when>
			 					
			 					<xsl:when test="../item[@name=$name_units]">				
			    					<xsl:attribute name="unit"><xsl:value-of select="../item[@name=$name_units]"/></xsl:attribute>
			 					</xsl:when>
			

			 					<xsl:otherwise>				
			 					</xsl:otherwise>
							</xsl:choose>

 
							<xsl:choose>

                                <!-- INST_CMPRS_SEGMENT_STATUS add quotes again -->
                                
                                <xsl:when test="contains(@name,'INST_CMPRS_SEGMENT_STATUS') ">
					              <xsl:attribute name="quoted">true</xsl:attribute>
                                <xsl:value-of select="normalize-space(.)" />
				                </xsl:when>
                                
								<xsl:when test="contains(@name,'STRUCTURE__PTR')">
									<xsl:attribute name="name">^<xsl:value-of
										select="substring-before(@name, '__PTR')" /></xsl:attribute>
									<xsl:attribute name="quoted">true</xsl:attribute>
									<xsl:value-of select="." />
								</xsl:when>

								<xsl:when test="contains(.,'N/A')">
									<xsl:attribute name="quoted">true</xsl:attribute>
									<xsl:value-of select="." />
								</xsl:when>

								<xsl:when test="@name='SOFTWARE_VERSION_ID'">
									<xsl:attribute name="quoted">true</xsl:attribute>
									<xsl:value-of select="." />
								</xsl:when>

								<xsl:when test="@name='START_IMAGE_ID'">
									<xsl:attribute name="quoted">false</xsl:attribute>
									<xsl:value-of select="." />
								</xsl:when>
                                
                                <!-- LOCAL_MEAN_SOLAR_TIME -->
                                <xsl:when test="@name='LOCAL_MEAN_SOLAR_TIME'">
									<xsl:attribute name="quoted">true</xsl:attribute>
									<xsl:value-of select="." />
								</xsl:when>
								
								<xsl:when test="@key='LOCAL_TRUE_SOLAR_TIME_SOL'"> 
									<xsl:attribute name="quoted">false</xsl:attribute>
									<xsl:value-of select="." />
								</xsl:when>

								<xsl:when test="@name='APPLICATION_PROCESS_ID'">
									<xsl:attribute name="quoted">false</xsl:attribute>
									<xsl:value-of select="." />
								</xsl:when>

								<xsl:when test="@name='SPACECRAFT_CLOCK_STOP_COUNT'">
									<xsl:attribute name="quoted">true</xsl:attribute>
									<xsl:value-of select="." />
								</xsl:when>

								<xsl:when test="@name='SPACECRAFT_CLOCK_START_COUNT'">
									<xsl:attribute name="quoted">true</xsl:attribute>
									<xsl:value-of select="." />
								</xsl:when>

								<xsl:when test="@name='TELEMETRY_SOURCE_SCLK_START'">
									<xsl:attribute name="quoted">true</xsl:attribute>
									<xsl:value-of select="." />
								</xsl:when>

								<xsl:when test="@name='TELEMETRY_SOURCE_START_TIME'">
									<xsl:attribute name="quoted">false</xsl:attribute>
									<xsl:value-of select="." />
								</xsl:when>

								<xsl:when test="@name='INSTRUMENT_SERIAL_NUMBER'">
									<xsl:attribute name="quoted">true</xsl:attribute>
									<xsl:value-of select="." />
								</xsl:when>

								<xsl:when test="@name='EXPECTED_TRANSMISSION_PATH'">
									<xsl:attribute name="quoted">true</xsl:attribute>
									<xsl:value-of select="." />
								</xsl:when>

								<xsl:when test="@name='FLIGHT_SOFTWARE_MODE'">
									<xsl:attribute name="quoted">true</xsl:attribute>
									<xsl:value-of select="." />
								</xsl:when>

								<xsl:when test="@name='PRODUCT_TAG'">
									<xsl:attribute name="quoted">true</xsl:attribute>
									<xsl:value-of select="." />
								</xsl:when>

								<xsl:when test="@name='VIRTUAL_CHANNEL_ID'">
									<xsl:attribute name="quoted">true</xsl:attribute>
									<xsl:value-of select="." />
								</xsl:when>

								<xsl:when test="@name='INSTRUMENT_IDLE_TIMEOUT'">
									<xsl:attribute name="quoted">false</xsl:attribute>
									<xsl:value-of select="." />
								</xsl:when>

								<xsl:when test="@name='TRANSMISSION_PATH'">
									<xsl:attribute name="quoted">true</xsl:attribute>
									<xsl:value-of select="." />
								</xsl:when>

								<xsl:when test="@name='EXPECTED_TRANSMISSION_PATH'">
									<xsl:attribute name="quoted">true</xsl:attribute>
									<xsl:value-of select="." />
								</xsl:when>

								<xsl:when test="contains(., 'UNK') or contains(., 'NULL')">
									<xsl:attribute name="quoted">true</xsl:attribute>
									<xsl:value-of select="." />
								</xsl:when>

								<xsl:when test="contains(@name,'_ID') ">
									<xsl:attribute name="quoted">true</xsl:attribute>
									<xsl:value-of select="." />
								</xsl:when>

								<xsl:when test="contains(@name,'_NAME') ">
									<xsl:attribute name="quoted">true</xsl:attribute>
									<xsl:value-of select="." />
								</xsl:when>

								<xsl:when test="contains(@name,'_NUMBER') ">
									<xsl:attribute name="quoted">false</xsl:attribute>
									<xsl:value-of select="." />
								</xsl:when>

								<xsl:when test="contains(@name,'_COUNT') ">
									<xsl:attribute name="quoted">false</xsl:attribute>
									<xsl:value-of select="." />
								</xsl:when>

								<xsl:when test="contains(@name,'_CNT') ">
									<xsl:attribute name="quoted">false</xsl:attribute>
									<xsl:value-of select="." />
								</xsl:when>
								<!-- _COUNT ??? , _CNT integer -->
								
								
								<!--  except LOCAL_TRUE_SOLAR_TIME_SOL -->
								<xsl:when test="contains(@name,'_SOLAR_TIME') ">
									<xsl:attribute name="quoted">true</xsl:attribute>
									<xsl:value-of select="." />
								</xsl:when>

								<xsl:when test="contains(@name,'_TIME') ">
									<xsl:attribute name="quoted">false</xsl:attribute>
									<xsl:value-of select="." />
								</xsl:when>

								<xsl:when test="contains(@name,'_FLAG') ">
									<xsl:attribute name="quoted">true</xsl:attribute>
									<xsl:value-of select="." />
								</xsl:when>

								<!-- could get ridiculous and do (contains(.,'0e-') or contains(.,'1e-') 
									or -->
								<xsl:when test="contains(.,'e+') and not(contains(.,'.'))">
									<xsl:attribute name="quoted">false</xsl:attribute>
									<!-- <xsl:value-of select="."/> -->
									<xsl:call-template name="globalReplace">
										<xsl:with-param name="outputString" select="." />
										<xsl:with-param name="target" select="'e+'" />
										<xsl:with-param name="replacement" select="'.0e+'" />
									</xsl:call-template>
								</xsl:when>


								<!-- could get ridiculous and do (contains(.,'0e-') or contains(.,'1e-') 
									or -->
								<xsl:when test="contains(.,'e-') and not(contains(.,'.'))">
									<xsl:attribute name="quoted">false</xsl:attribute>
									<!-- <xsl:value-of select="."/> -->
									<xsl:call-template name="globalReplace">
										<xsl:with-param name="outputString" select="." />
										<xsl:with-param name="target" select="'e-'" />
										<xsl:with-param name="replacement" select="'.0e-'" />
									</xsl:call-template>
								</xsl:when>

								<xsl:when
									test="(contains(substring(.,11,1), 'T') and contains(substring(.,14,1), ':')  and contains(substring(.,8,1), '-'))">
									<xsl:attribute name="quoted">false</xsl:attribute>
									<xsl:value-of select="." />
								</xsl:when>

								<xsl:when
									test="(contains(substring(.,9,1), 'T') and contains(substring(.,12,1), ':')  and contains(substring(.,5,1), '-'))">
									<xsl:attribute name="quoted">false</xsl:attribute>
									<xsl:value-of select="." />
								</xsl:when>

								<xsl:when
									test="(contains(substring(.,3,1), ':') and contains(substring(.,6,1), ':') )">
									<xsl:attribute name="quoted">false</xsl:attribute>
									<xsl:value-of select="." />
								</xsl:when>

								<xsl:when
									test="(contains($digitsPlusMinus,substring(.,1,1)) and not(contains(., ' ')) )">
									<xsl:attribute name="quoted">false</xsl:attribute>
									<xsl:value-of select="." />
								</xsl:when>

								<xsl:when
									test="contains(., ' ') or contains(., '-') or contains(., '.TXT')
		       or contains($nonCaps,substring(.,1,1))">
									<xsl:attribute name="quoted">true</xsl:attribute>
									<xsl:value-of select="." />
								</xsl:when>


								<xsl:otherwise>
									<xsl:attribute name="name"><xsl:value-of
										select="@name" /></xsl:attribute>
									<xsl:attribute name="quoted">false</xsl:attribute>
									<xsl:value-of select="." />
								</xsl:otherwise>
							</xsl:choose>
						</xsl:element>
					</xsl:when>
				</xsl:choose>
			</xsl:if>
		</xsl:if>


	</xsl:template>


	<xsl:template match="subitem">
		<xsl:variable name="vCaps" select="'ABCDEFGHIJKLMNOPQRSTUVWXYZ'" />

		<xsl:variable name="nonCaps" select="'abcdefghijklmnopqrstuvwxyz'" />

		<xsl:variable name="nonLetterNumber" select="'!@#$%^*;:,.?/=+\'" />

		<xsl:variable name="digits" select="'0123456789'" />

		<xsl:variable name="digitsPlusMinus" select="'+-0123456789'" />

		<xsl:element name="subitem">
			
			<xsl:attribute name="name"><xsl:value-of select="@name" /></xsl:attribute>
			
			<!--  we cannot depend on the order of items in the VICAR label 
				Check to see if the subitem we have now has a __UNIT item associated with it
				Construct the item name to look for. If we find it, then add the value as a "unit" attribute
			-->
			<xsl:variable name="name_unit">
				<xsl:value-of select="@name" />
			</xsl:variable>
			<xsl:variable name="name_unit_parent">
				<xsl:value-of select="local-name(parent::*)" />
			</xsl:variable>
			<xsl:variable name="name_unit_parent_name">
				<xsl:value-of select="parent[@name]" />
			</xsl:variable>

			<xsl:variable name="name_units" select="concat($name_unit,'__UNIT')" />
			<xsl:attribute name="subitem_name_units"><xsl:value-of select="@name_units"/></xsl:attribute>

			<xsl:choose>			 
			 <xsl:when test="contains(following-sibling::subitem[@name],'__UNIT')" >
			   <xsl:attribute name="unit"><xsl:value-of select="following-sibling::subitem[@name]"/></xsl:attribute>
			 </xsl:when>
			 
			 <xsl:when test="../../item[@name=$name_units]/subitem[@name=$name_units]">				
			    <xsl:attribute name="unit"><xsl:value-of select="../../item[@name=$name_units]/subitem[@name=$name_units]" /></xsl:attribute>
			 </xsl:when>
			 
			 <xsl:otherwise>				
			 </xsl:otherwise>
			</xsl:choose>
			
			<xsl:choose>
            
                <!-- INST_CMPRS_SEGMENT_STATUS now we do want it quoted -->
                <xsl:when test="contains(@name,'INST_CMPRS_SEGMENT_STATUS') ">
					<xsl:attribute name="quoted">true</xsl:attribute>
					<xsl:value-of select="normalize-space(.)" />
				</xsl:when>
                
				<xsl:when
					test="contains(.,'N/A') or contains(., 'UNK') or contains(., 'NULL')">
					<xsl:attribute name="quoted">true</xsl:attribute>
					<xsl:value-of select="normalize-space(.)" />
				</xsl:when>

				<xsl:when test="contains(@name,'_NAME') ">
					<xsl:attribute name="quoted">true</xsl:attribute>
					<xsl:value-of select="normalize-space(.)" />
				</xsl:when>

				<xsl:when test="contains(@name,'_ANGLE') ">
					<xsl:attribute name="quoted">false</xsl:attribute>
                    
                    <xsl:choose>
                      <xsl:when test="contains(.,'e+') and not(contains(.,'.'))">
					  <xsl:attribute name="quoted">false</xsl:attribute>
					  <!-- <xsl:value-of select="."/> -->
					  <xsl:call-template name="globalReplace">
						<xsl:with-param name="outputString" select="." />
						<xsl:with-param name="target" select="'e+'" />
						<xsl:with-param name="replacement" select="'.0e+'" />
					  </xsl:call-template>
				      </xsl:when>

				     <xsl:when test="contains(.,'e-') and not(contains(.,'.'))">
					 <xsl:attribute name="quoted">false</xsl:attribute>
					 <!-- <xsl:value-of select="."/> -->
					 <xsl:call-template name="globalReplace">
						<xsl:with-param name="outputString" select="." />
						<xsl:with-param name="target" select="'e-'" />
						<xsl:with-param name="replacement" select="'.0e-'" />
					 </xsl:call-template>
				     </xsl:when>
                
                    <xsl:otherwise>
					 <xsl:value-of select="normalize-space(.)" />
                    </xsl:otherwise>
                  </xsl:choose>
				</xsl:when>

				<xsl:when test="contains(@name,'_ID') ">
					<xsl:attribute name="quoted">true</xsl:attribute>
					<xsl:value-of select="normalize-space(.)" />
				</xsl:when>

				<xsl:when test="contains(@name,'_NUMBER') ">
					<xsl:attribute name="quoted">false</xsl:attribute>
					<xsl:value-of select="normalize-space(.)" />
				</xsl:when>

				<xsl:when test="contains(@name,'_TIME') ">
					<xsl:attribute name="quoted">false</xsl:attribute>
					<xsl:value-of select="normalize-space(.)" />
				</xsl:when>

				<xsl:when test="contains(@name,'_FLAG') ">
					<xsl:attribute name="quoted">true</xsl:attribute>
					<xsl:value-of select="normalize-space(.)" />
				</xsl:when>

				<xsl:when test="contains(@name,'_COUNT') ">
					<xsl:attribute name="quoted">false</xsl:attribute>
					<xsl:value-of select="normalize-space(.)" />
				</xsl:when>

				<xsl:when test="contains(@name,'_CNT') ">
					<xsl:attribute name="quoted">false</xsl:attribute>
					<xsl:value-of select="normalize-space(.)" />
				</xsl:when>

				<xsl:when test="contains(.,'e+') and not(contains(.,'.'))">
					<xsl:attribute name="quoted">false</xsl:attribute>
					<!-- <xsl:value-of select="."/> -->
					<xsl:call-template name="globalReplace">
						<xsl:with-param name="outputString" select="." />
						<xsl:with-param name="target" select="'e+'" />
						<xsl:with-param name="replacement" select="'.0e+'" />
					</xsl:call-template>
				</xsl:when>

				<xsl:when test="contains(.,'e-') and not(contains(.,'.'))">
					<xsl:attribute name="quoted">false</xsl:attribute>
					<!-- <xsl:value-of select="."/> -->
					<xsl:call-template name="globalReplace">
						<xsl:with-param name="outputString" select="." />
						<xsl:with-param name="target" select="'e-'" />
						<xsl:with-param name="replacement" select="'.0e-'" />
					</xsl:call-template>
				</xsl:when>

				<xsl:when
					test="(contains(substring(.,11,1), 'T') and contains(substring(.,14,1), ':')  and contains(substring(.,8,1), '-'))">
					<xsl:attribute name="quoted">false</xsl:attribute>
					<xsl:value-of select="normalize-space(.)" />
				</xsl:when>

				<xsl:when
					test="(contains(substring(.,9,1), 'T') and contains(substring(.,12,1), ':')  and contains(substring(.,5,1), '-'))">
					<xsl:attribute name="quoted">false</xsl:attribute>
					<xsl:value-of select="normalize-space(.)" />
				</xsl:when>

				<xsl:when
					test="(contains(substring(.,3,1), ':') and contains(substring(.,6,1), ':') )">
					<xsl:attribute name="quoted">false</xsl:attribute>
					<xsl:value-of select="normalize-space(.)" />
				</xsl:when>


				<xsl:when
					test="(contains($digitsPlusMinus,substring(.,1,1)) and not(contains(., ' ')) )">
					<xsl:attribute name="quoted">false</xsl:attribute>
					<xsl:value-of select="normalize-space(.)" />
				</xsl:when>


				<xsl:when
					test="contains(., ' ') or contains(., '-') or contains(., '.TXT') 
		       or contains($nonCaps,substring(.,1,1))">
					<xsl:attribute name="quoted">true</xsl:attribute>
					<xsl:value-of select="normalize-space(.)" />
				</xsl:when>

                <xsl:when test="contains(@name,'CONTACT_SENSOR_STATE') ">
					<xsl:attribute name="quoted">true</xsl:attribute>
					<xsl:value-of select="normalize-space(.)" />
				</xsl:when>

				<xsl:otherwise>
					<xsl:attribute name="quoted">false</xsl:attribute>
					<xsl:value-of select="normalize-space(.)" />
				</xsl:otherwise>

			</xsl:choose>


			<!-- <xsl:value-of select="."/> -->
		</xsl:element>
	</xsl:template>

	<xsl:template name="globalReplace">
		<xsl:param name="outputString" />
		<xsl:param name="target" />
		<xsl:param name="replacement" />
		<xsl:choose>
			<xsl:when test="contains($outputString,$target)">

				<xsl:value-of
					select="concat(substring-before($outputString,$target),
               $replacement)" />
				<xsl:call-template name="globalReplace">
					<xsl:with-param name="outputString"
						select="substring-after($outputString,$target)" />
					<xsl:with-param name="target" select="$target" />
					<xsl:with-param name="replacement" select="$replacement" />
				</xsl:call-template>
			</xsl:when>
			<xsl:otherwise>
				<xsl:value-of select="$outputString" />
			</xsl:otherwise>
		</xsl:choose>
	</xsl:template>





</xsl:stylesheet>


