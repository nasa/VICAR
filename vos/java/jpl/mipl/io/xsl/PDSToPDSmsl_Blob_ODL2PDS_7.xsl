<?xml version='1.0' encoding='UTF-8' ?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<!-- Author: Steve Levoe 

MSL PDS detached label version March 2013 _ Steve Levoe 
modifications to meet requirements for PDS archive delivery
changes based on ERRATA from 0-89 delivery
-->
<xsl:output method="xml" indent="yes" encoding="utf-8"/>

    <xsl:template match="/">
		<xsl:apply-templates/>
	</xsl:template>
	
	<xsl:template match="PDS_LABEL">
	  <PDS_LABEL>
	   <xsl:apply-templates/>
	 </PDS_LABEL>
	</xsl:template>

 <xsl:template match="GROUP">
 
	<xsl:element name="GROUP">
	
    <xsl:choose>  
	
	<!--  new PDS archive version names translated from the vicar name -->
	
	    <xsl:when test="@name='ROVER_COORDINATE_SYSTEM'">
		  <xsl:attribute name="name">ROVER_COORD_SYSTEM_PARMS</xsl:attribute>
		  <xsl:apply-templates/>
		</xsl:when>
			
		<xsl:when test="@name='RSM_COORDINATE_SYSTEM'">		
		  <xsl:attribute name="name">RSM_COORD_SYSTEM_PARMS</xsl:attribute>
		  <xsl:apply-templates/>
		</xsl:when>
			
		<xsl:when test="@name='LOCAL_LEVEL_COORDINATE_SYSTEM'">
		  <xsl:attribute name="name">LOCAL_LEVEL_COORD_SYSTEM_PARMS</xsl:attribute>
		  <xsl:apply-templates/>
		</xsl:when>
			
		<xsl:when test="@name='CHASSIS_ARTICULATION_STATE'">
		  <xsl:attribute name="name">CHASSIS_ARTICULATION_ST_PARMS</xsl:attribute>
		  <xsl:apply-templates/>
		</xsl:when>
			
		<xsl:when test="@name='SITE_COORDINATE_SYSTEM'">
		  <xsl:attribute name="name">SITE_COORD_SYSTEM_PARMS</xsl:attribute>
		  <xsl:apply-templates/>
		</xsl:when>
			
		<xsl:when test="@name='GEOMETRIC_CAMERA_MODEL'">
		  <xsl:attribute name="name">GEOMETRIC_CAMERA_MODEL_PARMS</xsl:attribute>
		  <xsl:apply-templates/>
		</xsl:when>
			
		<xsl:when test="@name='ARM_COORDINATE_SYSTEM'">
		  <xsl:attribute name="name">ARM_COORD_SYSTEM_PARMS</xsl:attribute>
		  <xsl:apply-templates/>
		</xsl:when>
			
		<xsl:when test="@name='HGA_ARTICULATION_STATE'">
		  <xsl:attribute name="name">HGA_ARTICULATION_STATE_PARMS</xsl:attribute>
		  <xsl:apply-templates/>
		</xsl:when>
			
		<xsl:when test="@name='RSM_ARTICULATION_STATE'">
		  <xsl:attribute name="name">RSM_ARTICULATION_STATE_PARMS</xsl:attribute>
		  <xsl:apply-templates/>
		</xsl:when>
			
		<xsl:when test="@name='ARM_ARTICULATION_STATE'">
		  <xsl:attribute name="name">ARM_ARTICULATION_STATE_PARMS</xsl:attribute>
		  <xsl:apply-templates/>
		</xsl:when>
		
		<xsl:when test="@name='PDS_HISTORY'">
		  <xsl:attribute name="name">PDS_HISTORY_PARMS</xsl:attribute>
		  <xsl:apply-templates/>
		</xsl:when>
		
	<!--  end of ODL to PDS GROUP name translation -->

	
		<xsl:otherwise>
			<xsl:attribute name="name"><xsl:value-of select="@name"/></xsl:attribute>  
		    <xsl:apply-templates/>
		</xsl:otherwise>
	 
	
	</xsl:choose>
	</xsl:element>
</xsl:template>
	
 <!--  match for not group -->
 
<xsl:template match="item"> 

<xsl:if test="count(subitem)>0">

	<xsl:choose>
	
	<xsl:when test="@key='CAMERA_ROTATION_AXIS_VECTOR'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">MSL:CAMERA_ROTATION_AXIS_VECTOR</xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
    </xsl:if> 
    <xsl:apply-templates/>  
   </xsl:element> 
  </xsl:when>  

  <xsl:when test="@key='CONFIGURATION_BIT_ID'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">MSL:CONFIGURATION_BIT_ID</xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
    </xsl:if> 
    <xsl:apply-templates/>  
   </xsl:element> 
  </xsl:when>  

    <xsl:when test="@key='ERROR_MODEL_NAME'"> 
       <xsl:element name="item"> 
       <xsl:attribute name="key">MSL:ERROR_MODEL_NAME</xsl:attribute> 
       <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
       <xsl:if test="normalize-space(@units)">  
          <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
       </xsl:if> 
       <xsl:apply-templates/>	 
       </xsl:element> 
  </xsl:when> 
  		
  <xsl:when test="@key='ERROR_MODEL_PARMS_NAME'"> 
       <xsl:element name="item"> 
       <xsl:attribute name="key">MSL:ERROR_MODEL_PARMS_NAME</xsl:attribute> 
       <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
       <xsl:if test="normalize-space(@units)">  
          <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
       </xsl:if> 
       <xsl:apply-templates/>	 
       </xsl:element> 
  </xsl:when> 
  
  <xsl:when test="@key='ERROR_MODEL_PARMS'"> 
       <xsl:element name="item"> 
       <xsl:attribute name="key">MSL:ERROR_MODEL_PARMS</xsl:attribute> 
       <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
       <xsl:if test="normalize-space(@units)">  
         <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
       </xsl:if> 
       <!--  <xsl:value-of select="."/>   -->
       <xsl:apply-templates/>	
       </xsl:element> 
  </xsl:when> 

  <xsl:when test="@key='HORIZON_MASK_ELEVATION'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">MSL:HORIZON_MASK_ELEVATION</xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
    </xsl:if> 
    <xsl:apply-templates/>   
   </xsl:element> 
  </xsl:when>  
  
	<xsl:when test="@key='IMAGE_RADIANCE_FACTOR'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">MSL:IMAGE_RADIANCE_FACTOR</xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
    </xsl:if> 
    <xsl:apply-templates/>   
   </xsl:element> 
  </xsl:when>  

  <xsl:when test="@key='IMAGE_RADIANCE_OFFSET'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">MSL:IMAGE_RADIANCE_OFFSET</xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute>       
    </xsl:if> 
    <xsl:apply-templates/>
   </xsl:element> 
  </xsl:when>  
 	
	<xsl:when test="@key='INPUT_PRODUCT_ID'"> 
       <xsl:element name="item"> 
       <xsl:attribute name="key">MSL:INPUT_PRODUCT_ID</xsl:attribute> 
       <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
       <xsl:if test="normalize-space(@units)">  
        <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
       </xsl:if> 
       <!--  <xsl:value-of select="."/>   -->
       <xsl:apply-templates/>	
       </xsl:element> 
  </xsl:when>  
	
	<xsl:when test="@key='INSTRUMENT_TEMPERATURE_STATUS'"> 
   		<xsl:element name="item"> 
    	<xsl:attribute name="key">MSL:INSTRUMENT_TEMPERATURE_STATUS</xsl:attribute> 
    	<xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    	<xsl:if test="normalize-space(@units)">  
       	<xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
    	</xsl:if> 
    	<xsl:apply-templates/>	
   	</xsl:element> 
  	</xsl:when>
  	
  	<xsl:when test="@key='MASK_DESC_FILE_NAME'"> 
       <xsl:element name="item"> 
       <xsl:attribute name="key">MSL:MASK_DESC_FILE_NAME</xsl:attribute> 
       <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
       <xsl:if test="normalize-space(@units)">  
        <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
       </xsl:if>
       <xsl:apply-templates/>	
       </xsl:element> 
    </xsl:when>  
  
    <xsl:when test="@key='MODEL_TRANSFORM_VECTOR'"> 
       <xsl:element name="item"> 
       <xsl:attribute name="key">MSL:MODEL_TRANSFORM_VECTOR</xsl:attribute> 
       <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
       <xsl:if test="normalize-space(@units)">  
        <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
       </xsl:if>
       <xsl:apply-templates/>	
       </xsl:element> 
    </xsl:when>  
    
      	<xsl:when test="@key='MODEL_TRANSFORM_QUATERNION'"> 
       <xsl:element name="item"> 
       <xsl:attribute name="key">MSL:MODEL_TRANSFORM_QUATERNION</xsl:attribute> 
       <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
       <xsl:if test="normalize-space(@units)">  
        <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
       </xsl:if>
       <xsl:apply-templates/>	
       </xsl:element> 
    </xsl:when>  	
    
    <!--  these were deleted before 
  	<xsl:when test="@key='MODEL_TRANSFORM_VECTOR'"> 
	</xsl:when> 
	
	<xsl:when test="@key='MODEL_TRANSFORM_QUATERNION'"> 
	</xsl:when> 
    -->
   <!-- was commented out before   
    <xsl:when test="@key='INST_CMPRS_SEGMENT_STATUS'"> 
   		<xsl:element name="item"> 
    	<xsl:attribute name="key">MSL:INST_CMPRS_SEGMENT_STATUS</xsl:attribute> 
    	<xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    	<xsl:if test="normalize-space(@units)">  
       	<xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
    	</xsl:if> 
    	<xsl:apply-templates/>	
   	</xsl:element> 
  	</xsl:when>
  	-->
  
  <xsl:when test="@key='MAP_RESOLUTION'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">MAP_RESOLUTION</xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
    <!--  
       <xsl:attribute name="units">pixel/degree</xsl:attribute> 
       -->
        <xsl:attribute name="units">
          <xsl:call-template name="fixUnits">
  		    <xsl:with-param name="unitString" select="@units"/>
  		  </xsl:call-template>
  		</xsl:attribute> 
    </xsl:if> 
    <xsl:apply-templates/> 
   </xsl:element> 
  </xsl:when>  
  
  <xsl:when test="@key='RANGE_ORIGIN_VECTOR'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">MSL:RANGE_ORIGIN_VECTOR</xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
    </xsl:if> 
    <xsl:apply-templates/>   
   </xsl:element> 
  </xsl:when>  
  	
  <xsl:when test="@key='RADIANCE_FACTOR'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">MSL:RADIANCE_FACTOR</xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
    </xsl:if> 
    <xsl:apply-templates/>   
   </xsl:element> 
  </xsl:when>  

  <xsl:when test="@key='RADIANCE_OFFSET'"> 
   <xsl:element name="item"> 
   <!-- 
    <xsl:attribute name="key">MSL:RADIANCE_OFFSET</xsl:attribute> 
    -->
    <xsl:attribute name="key">RADIANCE_OFFSET</xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)"> 
       <!--  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
       <xsl:attribute name="units">
          <xsl:call-template name="fixUnits">
  		    <xsl:with-param name="unitString" select="@units"/>
  		  </xsl:call-template>
  		</xsl:attribute> 
       -->
       <xsl:attribute name="units">W.m-2.sr-1.nm*-1</xsl:attribute>
       
    </xsl:if> 
    <xsl:apply-templates/>  
   </xsl:element> 
  </xsl:when>  
  
  <xsl:when test="@key='RADIANCE_SCALING_FACTOR'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">RADIANCE_SCALING_FACTOR</xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)"> 
       <xsl:attribute name="units">W.m-2.sr-1.nm*-1</xsl:attribute>
    </xsl:if> 
    <xsl:apply-templates/>  
   </xsl:element> 
  </xsl:when>  
  
  <xsl:when test="@key='PROJECTION_X_AXIS_VECTOR'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">MSL:PROJECTION_X_AXIS_VECTOR</xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
    </xsl:if> 
    <xsl:apply-templates/>   
   </xsl:element> 
  </xsl:when>  

  <xsl:when test="@key='PROJECTION_Y_AXIS_VECTOR'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">MSL:PROJECTION_Y_AXIS_VECTOR</xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
    </xsl:if> 
    <xsl:apply-templates/>  
   </xsl:element> 
  </xsl:when>  

  <xsl:when test="@key='PROJECTION_Z_AXIS_VECTOR'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">MSL:PROJECTION_Z_AXIS_VECTOR</xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
    </xsl:if> 
    <xsl:apply-templates/>  
   </xsl:element> 
  </xsl:when>  
   
	
	<xsl:otherwise>
 	<xsl:element name="item">
 		<xsl:attribute name="key"><xsl:value-of select="@key"/></xsl:attribute>
 		<xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute>  

		<xsl:if test="boolean(@units)">
        	<xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute>  
		</xsl:if> 
		<xsl:apply-templates/>	
 	</xsl:element>
 	</xsl:otherwise>
 </xsl:choose>
 
 </xsl:if>

 <xsl:if test="count(subitem)=0">
 	
   
    <xsl:choose>
     <xsl:when test="@key='PLACEHOLDER'"> 
     </xsl:when>
        <!-- comment - add filters to change the names of all the keywords which must be modified for MSL -->

<!-- file msl_removed_keywordsV1.0.txt --> 
   <!-- INSTRUMENT_COORDINATE_TYPE LOCAL_TRUE_SOLAR_TIME_SOL -->
   
  	<xsl:when test="@key='ICT_DIVIDER'"> 
	</xsl:when> 

    <xsl:when test="@key='INSTRUMENT_COORDINATE_TYPE'"> 
	</xsl:when> 
    
	<xsl:when test="@key='IPBC_DIVIDER'"> 
	</xsl:when> 
	
	<xsl:when test="@key='MODEL_TRANSFORM_VECTOR'"> 
	</xsl:when> 
	
	<xsl:when test="@key='MODEL_TRANSFORM_QUATERNION'"> 
	</xsl:when> 
    
    <xsl:when test="@key='N_SHOTS'"> 
	</xsl:when> 

	<xsl:when test="@key='N_SHOTS_2_AVG'"> 
	</xsl:when> 

	<xsl:when test="@key='N_SHOTS_2_IGNORE'"> 
	</xsl:when> 

	<xsl:when test="@key='OBS_FROM_LIMIT_SWITCH'"> 
	</xsl:when> 

	<xsl:when test="@key='SPEC_AD_CONVERTUV'"> 
	</xsl:when> 

	<xsl:when test="@key='SPEC_AD_CONVERTVIS'"> 
	</xsl:when> 

	<xsl:when test="@key='SPEC_AD_CONVERTVNIR'"> 
	</xsl:when> 

	<xsl:when test="@key='SPEC_IMAGE_TYPE'"> 
	</xsl:when> 

	<xsl:when test="@key='SPEC_VERT_CLK'"> 
	</xsl:when> 

	<xsl:when test="@key='SPECTROMETER_CONTROL_BYTE'"> 
	</xsl:when> 

	<xsl:when test="@key='SPECTROMETER_SELECT'"> 
	</xsl:when> 
	
	<xsl:when test="@key='SPECTROMETER_SERIAL_CLOCK'"> 
	</xsl:when> 

	<xsl:when test="@key='STACK_1_LEVEL'"> 
	</xsl:when> 

	<xsl:when test="@key='STACK_2_LEVEL'"> 
	</xsl:when> 

	<xsl:when test="@key='STACK_3_LEVEL'"> 
	</xsl:when> 

	<xsl:when test="@key='STACK_DURATION'"> 
	</xsl:when> 

	<xsl:when test="@key='START_ROW_UV'"> 
	</xsl:when> 

	<xsl:when test="@key='STOP_ROW_UV'"> 
	</xsl:when> 

	<xsl:when test="@key='START_ROW_VIS'"> 
	</xsl:when> 

	<xsl:when test="@key='STOP_ROW_VIS'"> 
	</xsl:when> 

	<xsl:when test="@key='START_ROW_VNIR'"> 
	</xsl:when> 

	<xsl:when test="@key='STOP_ROW_VNIR'"> 
	</xsl:when> 

	<xsl:when test="@key='TIME_BETWEEN_SHOTS'"> 
	</xsl:when> 


<!-- file msl_keywordsV1.2.txt --> 
<!-- 6-4-2012 added INST_CMPRS_SEGMENT_STATUS MSL:INST_CMPRS_SEGMENT_STATUS -->
  <xsl:when test="@key='ACTIVE_FLIGHT_STRING_ID'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">MSL:ACTIVE_FLIGHT_STRING_ID</xsl:attribute> 
    <!--
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    -->
    <xsl:attribute name="quoted">false</xsl:attribute>
    <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
    </xsl:if> 
    <xsl:value-of select="."/>  
   </xsl:element> 
  </xsl:when>  

  <xsl:when test="@key='AUTO_DELETE_FLAG'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">MSL:AUTO_DELETE_FLAG</xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
    </xsl:if> 
    <xsl:value-of select="."/>  
   </xsl:element> 
  </xsl:when>  

  <xsl:when test="@key='BIAS_COEFFS_FILE_NAME'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">MSL:BIAS_COEFFS_FILE_NAME</xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
    </xsl:if> 
    <xsl:value-of select="."/>  
   </xsl:element> 
  </xsl:when>  

  <xsl:when test="@key='BIAS_COEFFS_FILE_DESC'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">MSL:BIAS_COEFFS_FILE_DESC</xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
    </xsl:if> 
    <xsl:value-of select="."/>  
   </xsl:element> 
  </xsl:when>  

  <xsl:when test="@key='BRIGHTNESS_CORRECTION_FILE'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">MSL:BRIGHTNESS_CORRECTION_FILE</xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
    </xsl:if> 
    <xsl:value-of select="."/>  
   </xsl:element> 
  </xsl:when>  

  <xsl:when test="@key='BRIGHTNESS_CORRECTION_TYPE'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">MSL:BRIGHTNESS_CORRECTION_TYPE</xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
    </xsl:if> 
    <xsl:value-of select="."/>  
   </xsl:element> 
  </xsl:when>  

  <xsl:when test="@key='CAMERA_ROTATION_AXIS_VECTOR'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">MSL:CAMERA_ROTATION_AXIS_VECTOR</xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
    </xsl:if> 
    <xsl:value-of select="."/>  
   </xsl:element> 
  </xsl:when>  

  <xsl:when test="@key='COMMUNICATION_SESSION_ID'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">MSL:COMMUNICATION_SESSION_ID</xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
    </xsl:if> 
    <xsl:value-of select="."/>  
   </xsl:element> 
  </xsl:when>  

  <xsl:when test="@key='CONFIGURATION_BIT_ID'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">MSL:CONFIGURATION_BIT_ID</xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
    </xsl:if> 
    <xsl:value-of select="."/>  
   </xsl:element> 
  </xsl:when>  

  <xsl:when test="@key='DARK_SPECTRA_MODE'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">MSL:DARK_SPECTRA_MODE</xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
    </xsl:if> 
    <xsl:value-of select="."/>  
   </xsl:element> 
  </xsl:when>  
  
  <xsl:when test="@key='ERROR_MODEL_NAME'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">MSL:ERROR_MODEL_NAME</xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
    </xsl:if> 
    <xsl:value-of select="."/>  
   </xsl:element> 
  </xsl:when> 
  
  <xsl:when test="@key='ERROR_MODEL_PARMS'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">MSL:ERROR_MODEL_PARMS</xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
    </xsl:if> 
    <xsl:value-of select="."/>  
   </xsl:element> 
  </xsl:when> 
  
  <xsl:when test="@key='ERROR_MODEL_PARMS_NAME'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">MSL:ERROR_MODEL_PARMS_NAME</xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
    </xsl:if> 
    <xsl:value-of select="."/>  
   </xsl:element> 
  </xsl:when> 

  <xsl:when test="@key='EXPECTED_TRANSMISSION_PATH'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">MSL:EXPECTED_TRANSMISSION_PATH</xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
    </xsl:if> 
    <xsl:value-of select="."/>  
   </xsl:element> 
  </xsl:when>  

  <xsl:when test="@key='FLIGHT_SOFTWARE_MODE'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">MSL:FLIGHT_SOFTWARE_MODE</xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
    </xsl:if> 
    <xsl:value-of select="."/>  
   </xsl:element> 
  </xsl:when>  
  
  <xsl:when test="@key='HORIZON_MASK_ELEVATION'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">MSL:HORIZON_MASK_ELEVATION</xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
    </xsl:if> 
    <xsl:value-of select="."/>  
   </xsl:element> 
  </xsl:when>  

  <xsl:when test="@key='ICT_DIVIDER'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">MSL:ICT_DIVIDER</xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
    </xsl:if> 
    <xsl:value-of select="."/>  
   </xsl:element> 
  </xsl:when>  

  <xsl:when test="@key='IPBC_DIVIDER'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">MSL:IPBC_DIVIDER</xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
    </xsl:if> 
    <xsl:value-of select="."/>  
   </xsl:element> 
  </xsl:when>  

  <xsl:when test="@key='IMAGE_ACQUIRE_MODE'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">MSL:IMAGE_ACQUIRE_MODE</xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
    </xsl:if> 
    <xsl:value-of select="."/>  
   </xsl:element> 
  </xsl:when>  

  <xsl:when test="@key='IMAGE_RADIANCE_FACTOR'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">MSL:IMAGE_RADIANCE_FACTOR</xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
    </xsl:if> 
    <xsl:value-of select="."/>  
   </xsl:element> 
  </xsl:when>  
  
  <xsl:when test="@key='IMAGE_RADIANCE_OFFSET'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">MSL:IMAGE_RADIANCE_OFFSET</xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
    </xsl:if> 
    <xsl:value-of select="."/>  
   </xsl:element> 
  </xsl:when>  

  <xsl:when test="@key='INPUT_PRODUCT_ID'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">MSL:INPUT_PRODUCT_ID</xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
    </xsl:if> 
    <xsl:value-of select="."/>  
   </xsl:element> 
  </xsl:when>  

  <xsl:when test="@key='INSTRUMENT_COORD_FRAME_ID'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">MSL:INSTRUMENT_COORD_FRAME_ID</xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
    </xsl:if> 
    <xsl:value-of select="."/>  
   </xsl:element> 
  </xsl:when>  

  <xsl:when test="@key='INSTRUMENT_COORD_FRAME_INDEX'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">MSL:INSTRUMENT_COORD_FRAME_INDEX</xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
    </xsl:if> 
    <xsl:value-of select="."/>  
   </xsl:element> 
  </xsl:when>  

  <xsl:when test="@key='INSTRUMENT_FOCUS_DISTANCE'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">MSL:INSTRUMENT_FOCUS_DISTANCE</xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
    </xsl:if> 
    <xsl:value-of select="."/>  
   </xsl:element> 
  </xsl:when>  

  <xsl:when test="@key='INSTRUMENT_FOCUS_INIT_FLAG'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">MSL:INSTRUMENT_FOCUS_INIT_FLAG</xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
    </xsl:if> 
    <xsl:value-of select="."/>  
   </xsl:element> 
  </xsl:when>  

  <xsl:when test="@key='INSTRUMENT_FOCUS_MODE'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">MSL:INSTRUMENT_FOCUS_MODE</xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
    </xsl:if> 
    <xsl:value-of select="."/>  
   </xsl:element> 
  </xsl:when>  

  <xsl:when test="@key='INSTRUMENT_FOCUS_POSITION_CNT'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">MSL:INSTRUMENT_FOCUS_POSITION_CNT</xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
    </xsl:if> 
    <xsl:value-of select="."/>  
   </xsl:element> 
  </xsl:when>  

  <xsl:when test="@key='INSTRUMENT_FOCUS_STEP_SIZE'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">MSL:INSTRUMENT_FOCUS_STEP_SIZE</xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
    </xsl:if> 
    <xsl:value-of select="."/>  
   </xsl:element> 
  </xsl:when>  

  <xsl:when test="@key='INSTRUMENT_FOCUS_STEPS'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">MSL:INSTRUMENT_FOCUS_STEPS</xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
    </xsl:if> 
    <xsl:value-of select="."/>  
   </xsl:element> 
  </xsl:when>  

  <xsl:when test="@key='INSTRUMENT_TEMPERATURE_STATUS'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">MSL:INSTRUMENT_TEMPERATURE_STATUS</xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
    </xsl:if> 
    <xsl:value-of select="."/>  
   </xsl:element> 
  </xsl:when>  
  
  <!-- INST_CMPRS_SEGMENT_STATUS --> 
  <xsl:when test="@key='INST_CMPRS_SEGMENT_STATUS'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">MSL:INST_CMPRS_SEGMENT_STATUS</xsl:attribute> 
    <xsl:attribute name="quoted">true</xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
    </xsl:if> 
    <xsl:value-of select="."/>  
   </xsl:element> 
  </xsl:when>  

  <xsl:when test="@key='INVERSE_LUT_FILE_NAME'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">MSL:INVERSE_LUT_FILE_NAME</xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
    </xsl:if> 
    <xsl:value-of select="."/>  
   </xsl:element> 
  </xsl:when>  

  <xsl:when test="@key='LASER_MODE'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">MSL:LASER_MODE</xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
    </xsl:if> 
    <xsl:value-of select="."/>  
   </xsl:element> 
  </xsl:when>  

  <xsl:when test="@key='LINEARIZATION_MODE'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">MSL:LINEARIZATION_MODE</xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
    </xsl:if> 
    <xsl:value-of select="."/>  
   </xsl:element> 
  </xsl:when>  
  
  <!-- LOCAL_MEAN_SOLAR_TIME -->
  <xsl:when test="@key='LOCAL_MEAN_SOLAR_TIME'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">MSL:LOCAL_MEAN_SOLAR_TIME</xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
    </xsl:if> 
    <xsl:value-of select="."/>  
   </xsl:element> 
  </xsl:when>  

<!--  removed before. now it becomes MSL: 
  <xsl:when test="@key='LOCAL_TRUE_SOLAR_TIME_SOL'"> 
	</xsl:when>  -->
  <xsl:when test="@key='LOCAL_TRUE_SOLAR_TIME_SOL'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">MSL:LOCAL_TRUE_SOLAR_TIME_SOL</xsl:attribute> 
    <!--  
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    --> 
    <xsl:attribute name="quoted">false</xsl:attribute>  
    <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
    </xsl:if> 
    <xsl:value-of select="."/>  
   </xsl:element> 
  </xsl:when>  
  
  <xsl:when test="@key='MAP_RESOLUTION'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">MAP_RESOLUTION</xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
    <!--  
       <xsl:attribute name="units">pixel/degree</xsl:attribute> 
       -->
        <xsl:attribute name="units">
          <xsl:call-template name="fixUnits">
  		    <xsl:with-param name="unitString" select="@units"/>
  		  </xsl:call-template>
  		</xsl:attribute> 
    </xsl:if> 
    <xsl:value-of select="."/>  
   </xsl:element> 
  </xsl:when>  
  
  <xsl:when test="@key='MASK_DESC_FILE_NAME'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">MSL:MASK_DESC_FILE_NAME</xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
    </xsl:if> 
    <xsl:value-of select="."/>  
   </xsl:element> 
  </xsl:when>  
  
  <xsl:when test="@key='MAXIMUM_ELEVATION'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">MSL:MAXIMUM_ELEVATION</xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
    <!--  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
       -->
        <xsl:attribute name="units">
          <xsl:call-template name="fixUnits">
  		    <xsl:with-param name="unitString" select="@units"/>
  		  </xsl:call-template>
  		</xsl:attribute> 
    </xsl:if> 
    <xsl:value-of select="."/>  
   </xsl:element> 
  </xsl:when>  

  <xsl:when test="@key='MINIMUM_ELEVATION'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">MSL:MINIMUM_ELEVATION</xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
    <!-- 
      <xsl:attribute name="units">deg</xsl:attribute> 
      -->
      <xsl:attribute name="units">
          <xsl:call-template name="fixUnits">
  		    <xsl:with-param name="unitString" select="@units"/>
  		  </xsl:call-template>
  		</xsl:attribute> 
    </xsl:if> 
    <xsl:value-of select="."/>  
   </xsl:element> 
  </xsl:when>  
  
  <xsl:when test="@key='MODEL_TRANSFORM_QUATERNION'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">MSL:MODEL_TRANSFORM_QUATERNION</xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
    </xsl:if> 
    <xsl:value-of select="."/>  
   </xsl:element> 
  </xsl:when>  
  
   <xsl:when test="@key='MODEL_TRANSFORM_VECTOR'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">MSL:MODEL_TRANSFORM_VECTOR</xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
    </xsl:if> 
    <xsl:value-of select="."/>  
   </xsl:element> 
  </xsl:when>  
   
  <xsl:when test="@key='N_SHOTS_S_AVG'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">MSL:N_SHOTS_A_AVG</xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
    </xsl:if> 
    <xsl:value-of select="."/>  
   </xsl:element> 
  </xsl:when>  

  <xsl:when test="@key='NUM_SOFTWARE_KEYWORDS'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">MSL:NUM_SOFTWARE_KEYWORDS</xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
    </xsl:if> 
    <xsl:value-of select="."/>  
   </xsl:element> 
  </xsl:when>  

  <xsl:when test="@key='NUM_SOFTWARE_PARAMETERS'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">MSL:NUM_SOFTWARE_PARAMETERS</xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
    </xsl:if> 
    <xsl:value-of select="."/>  
   </xsl:element> 
  </xsl:when>  

  <xsl:when test="@key='POINTING_CORRECTION_FILE_NAME'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">MSL:POINTING_CORRECTION_FILE_NAME</xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
    </xsl:if> 
    <xsl:value-of select="."/>  
   </xsl:element> 
  </xsl:when>  

  <xsl:when test="@key='POINTING_MODEL_NAME'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">MSL:POINTING_MODEL_NAME</xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
    </xsl:if> 
    <xsl:value-of select="."/>  
   </xsl:element> 
  </xsl:when>  

  <xsl:when test="@key='POINTING_MODEL_PARAMS'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">MSL:POINTING_MODEL_PARAMS</xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
    </xsl:if> 
    <xsl:value-of select="."/>  
   </xsl:element> 
  </xsl:when>  

  <xsl:when test="@key='POINTING_MODEL_PARAMS_NAME'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">MSL:POINTING_MODEL_PARAMS_NAME</xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
    </xsl:if> 
    <xsl:value-of select="."/>  
   </xsl:element> 
  </xsl:when>  

  <xsl:when test="@key='PRODUCT_COMPLETION_STATUS'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">MSL:PRODUCT_COMPLETION_STATUS</xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
    </xsl:if> 
    <xsl:value-of select="."/>  
   </xsl:element> 
  </xsl:when>  

  <xsl:when test="@key='PRODUCT_TAG'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">MSL:PRODUCT_TAG</xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
    </xsl:if> 
    <xsl:value-of select="."/>  
   </xsl:element> 
  </xsl:when>  

  <xsl:when test="@key='PROJECTION_AXIS_OFFSET'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">MSL:PROJECTION_AXIS_OFFSET</xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units">
          <xsl:call-template name="fixUnits">
  		    <xsl:with-param name="unitString" select="@units"/>
  		  </xsl:call-template>
  		</xsl:attribute> 
    </xsl:if> 
    <xsl:value-of select="."/>  
   </xsl:element> 
  </xsl:when>  
  
  <xsl:when test="@key='PROJECTION_AZIMUTH'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key"><xsl:value-of select="@key"/></xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
      <xsl:attribute name="units">
          <xsl:call-template name="fixUnits">
  		    <xsl:with-param name="unitString" select="@units"/>
  		  </xsl:call-template>
  		</xsl:attribute> 
    </xsl:if> 
    <xsl:value-of select="."/>  
   </xsl:element> 
  </xsl:when>  
  
  <xsl:when test="@key='PROJECTION_ELEVATION'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key"><xsl:value-of select="@key"/></xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
      <xsl:attribute name="units">
          <xsl:call-template name="fixUnits">
  		    <xsl:with-param name="unitString" select="@units"/>
  		  </xsl:call-template>
  		</xsl:attribute> 
    </xsl:if> 
    <xsl:value-of select="."/>  
   </xsl:element> 
  </xsl:when>  

  <xsl:when test="@key='PROJECTION_X_AXIS_VECTOR'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">MSL:PROJECTION_X_AXIS_VECTOR</xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
    </xsl:if> 
    <xsl:value-of select="."/>  
   </xsl:element> 
  </xsl:when>  

  <xsl:when test="@key='PROJECTION_Y_AXIS_VECTOR'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">MSL:PROJECTION_Y_AXIS_VECTOR</xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
    </xsl:if> 
    <xsl:value-of select="."/>  
   </xsl:element> 
  </xsl:when>  

  <xsl:when test="@key='PROJECTION_Z_AXIS_VECTOR'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">MSL:PROJECTION_Z_AXIS_VECTOR</xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
    </xsl:if> 
    <xsl:value-of select="."/>  
   </xsl:element> 
  </xsl:when>  


  <xsl:when test="@key='RADIANCE_FACTOR'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">MSL:RADIANCE_FACTOR</xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
    </xsl:if> 
    <xsl:value-of select="."/>  
   </xsl:element> 
  </xsl:when>  
  
  <xsl:when test="@key='RADIANCE_OFFSET'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">MSL:RADIANCE_OFFSET</xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)"> 
  		<xsl:attribute name="units">W.m-2.sr-1.nm*-1</xsl:attribute>
    </xsl:if> 
     <xsl:value-of select="."/>  
   </xsl:element> 
  </xsl:when>  
  
  <xsl:when test="@key='RADIANCE_SCALING_FACTOR'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">RADIANCE_SCALING_FACTOR</xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)"> 
       <xsl:attribute name="units">W.m-2.sr-1.nm*-1</xsl:attribute>
    </xsl:if> 
    <xsl:value-of select="."/>
   </xsl:element> 
  </xsl:when>  
  
  <xsl:when test="@key='RANGE_ORIGIN_VECTOR'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">MSL:RANGE_ORIGIN_VECTOR</xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
    </xsl:if> 
    <xsl:value-of select="."/>  
   </xsl:element> 
  </xsl:when>  

  <xsl:when test="@key='REFERENCE_COORD_SYSTEM_SOLN_ID'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">MSL:REFERENCE_COORD_SYSTEM_SOLN_ID</xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
    </xsl:if> 
    <xsl:value-of select="."/>  
   </xsl:element> 
  </xsl:when>  

  <xsl:when test="@key='REFERENCE_PIXEL_IMAGE_ID'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">MSL:REFERENCE_PIXEL_IMAGE_ID</xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
    </xsl:if> 
    <xsl:value-of select="."/>  
   </xsl:element> 
  </xsl:when>  

  <xsl:when test="@key='REQUEST_ID'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">MSL:REQUEST_ID</xsl:attribute> 
    <xsl:attribute name="quoted">true</xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
    </xsl:if> 
    <xsl:value-of select="."/>  
   </xsl:element> 
  </xsl:when>  

  <xsl:when test="@key='RESPONSIVITY_CONSTANTS'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">MSL:RESPONSIVITY_CONSTANTS</xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
    </xsl:if> 
    <xsl:value-of select="."/>  
   </xsl:element> 
  </xsl:when>  

  <xsl:when test="@key='RESPONSIVITY_CONSTANTS_FILE'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">MSL:RESPONSIVITY_CONSTANTS_FILE</xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
    </xsl:if> 
    <xsl:value-of select="."/>  
   </xsl:element> 
  </xsl:when>  

  <xsl:when test="@key='SEQUENCE_EXECUTION_COUNT'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">MSL:SEQUENCE_EXECUTION_COUNT</xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
    </xsl:if> 
    <xsl:value-of select="."/>  
   </xsl:element> 
  </xsl:when>  

  <xsl:when test="@key='SOFTWARE_KEYWORD_NAME'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">MSL:SOFTWARE_KEYWORD_NAME</xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
    </xsl:if> 
    <xsl:value-of select="."/>  
   </xsl:element> 
  </xsl:when>  

  <xsl:when test="@key='SOFTWARE_KEYWORD_TYPE'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">MSL:SOFTWARE_KEYWORD_TYPE</xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
    </xsl:if> 
    <xsl:value-of select="."/>  
   </xsl:element> 
  </xsl:when>  

  <xsl:when test="@key='SOFTWARE_KEYWORD_VALUE'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">MSL:SOFTWARE_KEYWORD_VALUE</xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
    </xsl:if> 
    <xsl:value-of select="."/>  
   </xsl:element> 
  </xsl:when>  

  <xsl:when test="@key='SOFTWARE_LANGUAGE'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">MSL:SOFTWARE_LANGUAGE</xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
    </xsl:if> 
    <xsl:value-of select="."/>  
   </xsl:element> 
  </xsl:when>  

  <xsl:when test="@key='SOFTWARE_MODULE_NAME'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">MSL:SOFTWARE_MODULE_NAME</xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
    </xsl:if> 
    <xsl:value-of select="."/>  
   </xsl:element> 
  </xsl:when>  

  <xsl:when test="@key='SOFTWARE_MODULE_TYPE'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">MSL:SOFTWARE_MODULE_TYPE</xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
    </xsl:if> 
    <xsl:value-of select="."/>  
   </xsl:element> 
  </xsl:when>  

  <xsl:when test="@key='SOFTWARE_PARAMETER_NAME'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">MSL:SOFTWARE_PARAMETER_NAME</xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
    </xsl:if> 
    <xsl:value-of select="."/>  
   </xsl:element> 
  </xsl:when>  

  <xsl:when test="@key='SOFTWARE_PARAMETER_TYPE'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">MSL:SOFTWARE_PARAMETER_TYPE</xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
    </xsl:if> 
    <xsl:value-of select="."/>  
   </xsl:element> 
  </xsl:when>  

  <xsl:when test="@key='SOFTWARE_PARAMETER_VALUE'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">MSL:SOFTWARE_PARAMETER_VALUE</xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
    </xsl:if> 
    <xsl:value-of select="."/>  
   </xsl:element> 
  </xsl:when>  

  <xsl:when test="@key='SOLUTION_ID'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">MSL:SOLUTION_ID</xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
    </xsl:if> 
    <xsl:value-of select="."/>  
   </xsl:element> 
  </xsl:when>  

  <xsl:when test="@key='SOURCE_PRODUCT_WAVELENGTH'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">MSL:SOURCE_PRODUCT_WAVELENGTH</xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
    </xsl:if> 
    <xsl:value-of select="."/>  
   </xsl:element> 
  </xsl:when>  

  <xsl:when test="@key='SPECTROMETER_SELECT'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">MSL:SPECTROMETER_SELECT</xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
    </xsl:if> 
    <xsl:value-of select="."/>  
   </xsl:element> 
  </xsl:when>  

  <xsl:when test="@key='START_AZIMUTH'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">START_AZIMUTH</xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
      <xsl:attribute name="units">
          <xsl:call-template name="fixUnits">
  		    <xsl:with-param name="unitString" select="@units"/>
  		  </xsl:call-template>
  		</xsl:attribute> 
    </xsl:if> 
    <xsl:value-of select="."/>  
   </xsl:element> 
  </xsl:when>  
  
  <xsl:when test="@key='START_IMAGE_ID'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">MSL:START_IMAGE_ID</xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
    </xsl:if> 
    <xsl:value-of select="."/>  
   </xsl:element> 
  </xsl:when>  
  
  <xsl:when test="@key='START_TIME'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">START_TIME</xsl:attribute> 
    <xsl:attribute name="quoted">false</xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
    </xsl:if> 
    <xsl:value-of select="."/>  
   </xsl:element> 
  </xsl:when>  

  <xsl:when test="@key='STEREO_PRODUCT_ID'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">MSL:STEREO_PRODUCT_ID</xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
    </xsl:if> 
    <xsl:value-of select="."/>  
   </xsl:element> 
  </xsl:when>  

  <xsl:when test="@key='STOP_AZIMUTH'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">STOP_AZIMUTH</xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
      <xsl:attribute name="units">
          <xsl:call-template name="fixUnits">
  		    <xsl:with-param name="unitString" select="@units"/>
  		  </xsl:call-template>
  		</xsl:attribute> 
    </xsl:if> 
    <xsl:value-of select="."/>  
   </xsl:element> 
  </xsl:when>  
  
  <xsl:when test="@key='STOP_TIME'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">STOP_TIME</xsl:attribute> 
    <xsl:attribute name="quoted">false</xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
    </xsl:if> 
    <xsl:value-of select="."/>  
   </xsl:element> 
  </xsl:when>  
  
  <xsl:when test="@key='STRIPING_COUNT'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">MSL:STRIPING_COUNT</xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
    </xsl:if> 
    <xsl:value-of select="."/>  
   </xsl:element> 
  </xsl:when>  

  <xsl:when test="@key='STRIPING_OVERLAP_ROWS'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">MSL:STRIPING_OVERLAP_ROWS</xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
    </xsl:if> 
    <xsl:value-of select="."/>  
   </xsl:element> 
  </xsl:when>  

  <xsl:when test="@key='SURFACE_MODEL_FILE_NAME'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">MSL:SURFACE_MODEL_FILE_NAME</xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
    </xsl:if> 
    <xsl:value-of select="."/>  
   </xsl:element> 
  </xsl:when>  

  <xsl:when test="@key='TELEMETRY_SOURCE_CHECKSUM'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">MSL:TELEMETRY_SOURCE_CHECKSUM</xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
    </xsl:if> 
    <xsl:value-of select="."/>  
   </xsl:element> 
  </xsl:when>  

  <xsl:when test="@key='TELEMETRY_SOURCE_HOST_NAME'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">MSL:TELEMETRY_SOURCE_HOST_NAME</xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
    </xsl:if> 
    <xsl:value-of select="."/>  
   </xsl:element> 
  </xsl:when>  

  <xsl:when test="@key='TELEMETRY_SOURCE_SCLK_START'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">MSL:TELEMETRY_SOURCE_SCLK_START</xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
    </xsl:if> 
    <xsl:value-of select="."/>  
   </xsl:element> 
  </xsl:when>  

  <xsl:when test="@key='TELEMETRY_SOURCE_SIZE'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">MSL:TELEMETRY_SOURCE_SIZE</xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
    </xsl:if> 
    <xsl:value-of select="."/>  
   </xsl:element> 
  </xsl:when>  

  <xsl:when test="@key='TELEMETRY_SOURCE_START_TIME'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">MSL:TELEMETRY_SOURCE_START_TIME</xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
    </xsl:if> 
    <xsl:value-of select="."/>  
   </xsl:element> 
  </xsl:when>  

  <xsl:when test="@key='TELEMETRY_SOURCE_TYPE'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">MSL:TELEMETRY_SOURCE_TYPE</xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
    </xsl:if> 
    <xsl:value-of select="."/>  
   </xsl:element> 
  </xsl:when>  

  <xsl:when test="@key='TRANSMISSION_PATH'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">MSL:TRANSMISSION_PATH</xsl:attribute> 
    <xsl:attribute name="quoted">true</xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
    </xsl:if> 
    <xsl:value-of select="."/>  
   </xsl:element> 
  </xsl:when>  

  <xsl:when test="@key='VALID_MAXIMUM_PIXEL'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">MSL:VALID_MAXIMUM_PIXEL</xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
    </xsl:if> 
    <xsl:value-of select="."/>  
   </xsl:element> 
  </xsl:when>  

  <xsl:when test="@key='VALID_MINIMUM_PIXEL'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">MSL:VALID_MINIMUM_PIXEL</xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
    </xsl:if> 
    <xsl:value-of select="."/>  
   </xsl:element> 
  </xsl:when>  

  <xsl:when test="@key='VIRTUAL_CHANNEL_ID'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">MSL:VIRTUAL_CHANNEL_ID</xsl:attribute> 
    <xsl:attribute name="quoted">true</xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
    </xsl:if> 
    <xsl:value-of select="."/>  
   </xsl:element> 
  </xsl:when>  
  
  <xsl:when test="@key='X_AXIS_MAXIMUM'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key"><xsl:value-of select="@key"/></xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units">
          <xsl:call-template name="fixUnits">
  		    <xsl:with-param name="unitString" select="@units"/>
  		  </xsl:call-template>
  		</xsl:attribute> 
    </xsl:if> 
    <xsl:value-of select="."/>  
   </xsl:element> 
  </xsl:when>  

 <xsl:when test="@key='X_AXIS_MINIMUM'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key"><xsl:value-of select="@key"/></xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units">
          <xsl:call-template name="fixUnits">
  		    <xsl:with-param name="unitString" select="@units"/>
  		  </xsl:call-template>
  		</xsl:attribute> 
    </xsl:if> 
    <xsl:value-of select="."/>  
   </xsl:element> 
  </xsl:when>  
  
    
  <xsl:when test="@key='Y_AXIS_MAXIMUM'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key"><xsl:value-of select="@key"/></xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units">
          <xsl:call-template name="fixUnits">
  		    <xsl:with-param name="unitString" select="@units"/>
  		  </xsl:call-template>
  		</xsl:attribute> 
    </xsl:if> 
    <xsl:value-of select="."/>  
   </xsl:element> 
  </xsl:when>  

  <xsl:when test="@key='Y_AXIS_MINIMUM'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key"><xsl:value-of select="@key"/></xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units">
          <xsl:call-template name="fixUnits">
  		    <xsl:with-param name="unitString" select="@units"/>
  		  </xsl:call-template>
  		</xsl:attribute> 
    </xsl:if> 
    <xsl:value-of select="."/>  
   </xsl:element> 
  </xsl:when>  
  
  
  <xsl:when test="@key='ZERO_EXPOSURE_IMAGE_ID'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key">MSL:ZERO_EXPOSURE_IMAGE_ID</xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
    </xsl:if> 
    <xsl:value-of select="."/>  
   </xsl:element> 
  </xsl:when>  


<!--  not an MSL: - change the value to remove space between word and comma -->
	<xsl:when test="@key='PRODUCER_INSTITUTION_NAME'"> 
   <xsl:element name="item"> 
    <xsl:attribute name="key"><xsl:value-of select="@key"/></xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
    </xsl:if> 
    <!-- <xsl:value-of select="."/> -->
        	<xsl:call-template name="globalReplace">
  				<xsl:with-param name="outputString" select="."/>
  				<xsl:with-param name="target" select="'LAB ,'"/>
  				<xsl:with-param name="replacement" select="'LAB,'"/>
  			</xsl:call-template>  
   </xsl:element> 
  </xsl:when>  




    <xsl:otherwise>   
	 <xsl:element name="item">   
      <xsl:attribute name="key"><xsl:value-of select="@key"/></xsl:attribute>    
	  <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute>  
	  <xsl:if test="normalize-space(@units)">
             <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute>  
	  </xsl:if>
      <xsl:value-of select="."/>
     </xsl:element>
	</xsl:otherwise>
	</xsl:choose>
    </xsl:if>
 
</xsl:template>  

<!--  
<xsl:template match="COMMENT">
<xsl:copy><xsl:value-of select="."/></xsl:copy>
</xsl:template>
-->

<xsl:template match="OBJECT">
<xsl:element name="OBJECT">
 	<xsl:attribute name="name"><xsl:value-of select="@name"/></xsl:attribute>  
	<xsl:apply-templates/>	
	</xsl:element>
</xsl:template>

<xsl:template match="COMMENT">
<COMMENT><xsl:value-of select="."/></COMMENT>
</xsl:template>

<xsl:template match="ODL3">
 <xsl:copy><xsl:apply-templates select="@*|node()"/></xsl:copy>
</xsl:template>

<xsl:template match="PDS3">
<xsl:copy><xsl:apply-templates select="@*|node()"/></xsl:copy>
</xsl:template>

<!--  copy key and units -->
<xsl:template match="key">
<xsl:attribute name="key222"><xsl:value-of select="@key"/></xsl:attribute>
</xsl:template>

<xsl:template match="quoted">
<xsl:attribute name="quoted222"><xsl:value-of select="@quoted"/></xsl:attribute>
</xsl:template>

<xsl:template match="units">
<xsl:attribute name="units222"><xsl:value-of select="@units"/></xsl:attribute>
</xsl:template>


<!--  probably need units and quoted added too -->
<xsl:template match="subitem">
	<xsl:choose>
	
  <xsl:when test="@key='CONFIGURATION_BIT_ID'"> 
    <xsl:element name="subitem"> 
      <xsl:attribute name="key">MSL:CONFIGURATION_BIT_ID</xsl:attribute> 
      <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
      <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
      </xsl:if> 
      <xsl:value-of select="normalize-space(.)"/>  
   </xsl:element> 
  </xsl:when>
	
  <xsl:when test="@key='ERROR_MODEL_NAME'"> 
    <xsl:element name="subitem"> 
      <xsl:attribute name="key">MSL:ERROR_MODEL_NAME</xsl:attribute> 
      <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
      <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
      </xsl:if> 
      <xsl:value-of select="normalize-space(.)"/>  
   </xsl:element> 
  </xsl:when>
  
   <xsl:when test="@key='ERROR_MODEL_PARMS'"> 
    <xsl:element name="subitem"> 
      <xsl:attribute name="key">MSL:ERROR_MODEL_PARMS</xsl:attribute> 
      <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
      <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
      </xsl:if> 
      <xsl:value-of select="normalize-space(.)"/>  
   </xsl:element> 
  </xsl:when>
 
   <xsl:when test="@key='ERROR_MODEL_PARMS_NAME'"> 
    <xsl:element name="subitem"> 
      <xsl:attribute name="key">MSL:ERROR_MODEL_PARMS_NAME</xsl:attribute> 
      <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
      <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
      </xsl:if> 
      <xsl:value-of select="normalize-space(.)"/>  
   </xsl:element> 
  </xsl:when>
 
   <xsl:when test="@key='HORIZON_MASK_ELEVATION'"> 
    <xsl:element name="subitem"> 
      <xsl:attribute name="key">MSL:HORIZON_MASK_ELEVATION</xsl:attribute> 
      <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
      <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
      </xsl:if> 
      <xsl:value-of select="normalize-space(.)"/>  
   </xsl:element> 
  </xsl:when>

   <xsl:when test="@key='INPUT_PRODUCT_ID'"> 
    <xsl:element name="subitem"> 
      <xsl:attribute name="key">MSL:INPUT_PRODUCT_ID</xsl:attribute> 
      <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
      <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
      </xsl:if> 
      <xsl:value-of select="normalize-space(.)"/>  
   </xsl:element> 
  </xsl:when>
    
   <xsl:when test="@key='INPUT_PRODUCT_ID'"> 
    <xsl:element name="subitem"> 
      <xsl:attribute name="key">MSL:INPUT_PRODUCT_ID</xsl:attribute> 
      <xsl:attribute name="quoted">false</xsl:attribute> 
      <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
      </xsl:if> 
      <xsl:value-of select="normalize-space(.)"/>  
   </xsl:element> 
  </xsl:when>
  
  
  <xsl:when test="@key='INST_CMPRS_SEGMENT_STATUS'"> 
   	<xsl:element name="subitem"> 
    	<xsl:attribute name="key">MSL:INST_CMPRS_SEGMENT_STATUS</xsl:attribute> 
    	<xsl:attribute name="quoted">true</xsl:attribute> 
    	<xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
    	</xsl:if> 
    	<xsl:value-of select="normalize-space(.)"/>  
   	</xsl:element> 
  	</xsl:when>
    
   <!-- this one was in 6 -->
<xsl:when test="@key='INSTRUMENT_TEMPERATURE_STATUS'"> 
   	<xsl:element name="subitem"> 
    	<xsl:attribute name="key">MSL:INSTRUMENT_TEMPERATURE_STATUS</xsl:attribute> 
    	<xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    	<xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
    	</xsl:if> 
    	<xsl:value-of select="normalize-space(.)"/>  
   	</xsl:element> 
  	</xsl:when>

   <xsl:when test="@key='MASK_DESC_FILE_NAME'"> 
    <xsl:element name="subitem"> 
      <xsl:attribute name="key">MSL:MASK_DESC_FILE_NAME</xsl:attribute> 
      <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
      <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
      </xsl:if> 
      <xsl:value-of select="normalize-space(.)"/>  
   </xsl:element> 
  </xsl:when>
    	
   <xsl:when test="@key='MAP_RESOLUTION'"> 
   <xsl:element name="subitem"> 
    <xsl:attribute name="key">MAP_RESOLUTION</xsl:attribute> 
    <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
    <xsl:if test="normalize-space(@units)">  
    <!--  
       <xsl:attribute name="units">pixel/degree</xsl:attribute> 
       -->
        <xsl:attribute name="units">
          <xsl:call-template name="fixUnits">
  		    <xsl:with-param name="unitString" select="@units"/>
  		  </xsl:call-template>
  		</xsl:attribute> 
    </xsl:if> 
    <xsl:apply-templates/> 
   </xsl:element> 
  </xsl:when>  

  <xsl:when test="@key='MODEL_TRANSFORM_QUATERNION'"> 
    <xsl:element name="subitem"> 
      <xsl:attribute name="key">MSL:MODEL_TRANSFORM_QUATERNION</xsl:attribute> 
      <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
      <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
      </xsl:if> 
      <xsl:value-of select="normalize-space(.)"/>  
   </xsl:element> 
  </xsl:when>
  
     <xsl:when test="@key='MODEL_TRANSFORM_VECTOR'"> 
    <xsl:element name="subitem"> 
      <xsl:attribute name="key">MSL:MODEL_TRANSFORM_VECTOR</xsl:attribute> 
      <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
      <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
      </xsl:if> 
      <xsl:value-of select="normalize-space(.)"/>  
   </xsl:element> 
  </xsl:when>

  <xsl:when test="@key='RANGE_ORIGIN_VECTOR'"> 
    <xsl:element name="subitem"> 
      <xsl:attribute name="key">MSL:RANGE_ORIGIN_VECTOR</xsl:attribute> 
      <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute> 
      <xsl:if test="normalize-space(@units)">  
       <xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute> 
      </xsl:if> 
      <xsl:value-of select="normalize-space(.)"/>  
   </xsl:element> 
  </xsl:when>
    
	<xsl:otherwise>  
	<xsl:element name="subitem">
		<xsl:attribute name="key"><xsl:value-of select="@key"/></xsl:attribute>  
		<xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute>  
	
	 	<xsl:if test="boolean(@units)">    
        	<xsl:attribute name="units"><xsl:value-of select="@units"/></xsl:attribute>      
     	</xsl:if>
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
		<xsl:value-of select="normalize-space(.)"/>
		</xsl:otherwise>
		</xsl:choose>
	</xsl:element>
	</xsl:otherwise>  
	
 	</xsl:choose>
</xsl:template>


<xsl:template name="globalReplace">
  <xsl:param name="outputString"/>
  <xsl:param name="target"/>
  <xsl:param name="replacement"/>
  <xsl:choose>
    <xsl:when test="contains($outputString,$target)">
   
      <xsl:value-of select=
        "concat(substring-before($outputString,$target),
               $replacement)"/>
      <xsl:call-template name="globalReplace">
        <xsl:with-param name="outputString" 
             select="substring-after($outputString,$target)"/>
        <xsl:with-param name="target" select="$target"/>
        <xsl:with-param name="replacement" 
             select="$replacement"/>
      </xsl:call-template>
    </xsl:when>
    <xsl:otherwise>
      <xsl:value-of select="$outputString"/>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template name="fixUnits">
  <xsl:param name="unitString"/>
  <xsl:choose>
  <!-- 
  <DEGREES>" to "<deg>" or "<degree>"
  "<M>" to "<m>"
  "WATT*M*-2*SR-1*NM-1" -> "W.m-2.sr-1.nm*-1"
  <PIXELS/DEGREE> -> <pixel/degree> 
  
  <xsl:when test="$unitString='<DEGREES>'">   
   -->
    
    <xsl:when test="$unitString='DEGREES'">   
       <xsl:call-template name="globalReplace">
  		<xsl:with-param name="outputString" select="$unitString"/>
  		<xsl:with-param name="target" select="'DEGREES'"/>
  		<xsl:with-param name="replacement" select="'deg'"/>
  	   </xsl:call-template>   
    </xsl:when>
        
    <xsl:when test="contains($unitString,'M')"> 
      <xsl:call-template name="globalReplace">
  		<xsl:with-param name="outputString" select="$unitString"/>
  		<xsl:with-param name="target" select="'M'"/>
  		<xsl:with-param name="replacement" select="'m'"/>
  	   </xsl:call-template>   
    </xsl:when>
    
     <xsl:when test="contains($unitString,'PIXELS/DEGREE')"> 
       <xsl:call-template name="globalReplace">
  		<xsl:with-param name="outputString" select="$unitString"/>
  		<xsl:with-param name="target" select="'PIXELS/DEGREE'"/>
  		<xsl:with-param name="replacement" select="'pixel/degree'"/>
  	   </xsl:call-template>    
    </xsl:when>
        
        
    <xsl:when test="contains($unitString,'WATT*M**-2*SR**-1*NM**-1')"> 
      <xsl:call-template name="globalReplace">
  		<xsl:with-param name="outputString" select="$unitString"/>
  		<xsl:with-param name="target" select="'WATT*M**-2*SR**-1*NM**-1'"/>
  		<xsl:with-param name="replacement" select="'W.m-2.sr-1.nm*-1'"/>
  	   </xsl:call-template>       
     </xsl:when>
     
    <xsl:when test="contains($unitString,'WATT*M*-2*SR-1*NM-1')"> 
      <xsl:call-template name="globalReplace">
  		<xsl:with-param name="outputString" select="$unitString"/>
  		<xsl:with-param name="target" select="'WATT*M*-2*SR-1*NM-1'"/>
  		<xsl:with-param name="replacement" select="'W.m-2.sr-1.nm*-1'"/>
  	   </xsl:call-template>       
     </xsl:when>
    <xsl:otherwise>
      <xsl:value-of select="$unitString"/>
    </xsl:otherwise>
    </xsl:choose>
 </xsl:template>


</xsl:stylesheet>


