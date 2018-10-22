	include 'VICMAIN_FOR'

C**********************************************************************
C
C	Test Program TMP_ROUTINES
C
C	Program calls mpGetPar which calls mpInit to allocate memory 
C	for a map projection data object and then sets values in the	
C	data object based on values passed by the application programs
C	parameter list. Then mpll2xy and mpxy2ll are called to perform
C	point transformations.
C
C	Author:			Justin McNeill
C	Cognizant Engineer:	Justin McNeill
C	Date Written:		May 1994	
C	Revision history:	Original
C	
C**********************************************************************

	subroutine main44
	implicit integer (a-y)
	implicit real*8  (z)
	
	include 'mp_for_defs'

	integer i
	integer count
	integer status
	integer number_keywords
    	integer types(mp_number_of_keywords)
	integer classes(mp_number_of_keywords)
	integer ll_type
	integer lat_count, lon_count

	real*8 mp

	character*20 lat_lon_type
	character*100 msg

	real*8 double_value
	real*8 lines(10)
	real*8 samples(10)
	real*4 latitudes(10)
	real*4 longitudes(10)

	real*8 latitude
	real*8 longitude
	real*8 new_lat
	real*8 new_lon

	character*(mp_max_keywd_length) keys(mp_number_of_keywords)
	character*(mp_max_keywd_length) pdf_parms(mp_number_of_keywords)
	character*(mp_max_keywd_length) pds_keys(mp_number_of_keywords)

	character*200 pck_file
	
	character*40 string_value

	call xvmessage(' ',' ')
	call xvmessage('*********************************************'
     .		,' ')
	call xvmessage(' ',' ')
	call xvmessage('     FORTRAN Test of MP Routines',' ')
	call xvmessage(' ',' ')
	call xvmessage('*********************************************'
     .		,' ')
	call xvmessage(' ',' ')

C
C	Set PDF parms to PDS keywords mappings
C	

	pdf_parms(1) = 'TARGET'
	pds_keys(1) = 'TARGET_NAME'

	pdf_parms(2) = 'PROJ'
	pds_keys(2) = 'MAP_PROJECTION_TYPE'

	pdf_parms(3) = 'A_AXIS'
	pds_keys(3) = 'A_AXIS_RADIUS'

	pdf_parms(4) = 'B_AXIS'
	pds_keys(4) = 'B_AXIS_RADIUS'

	pdf_parms(5) = 'C_AXIS'
	pds_keys(5) = 'C_AXIS_RADIUS'

	pdf_parms(6) = 'SCALE'
	pds_keys(6) = 'MAP_SCALE'

	pdf_parms(7) = 'RESOLUTION'
	pds_keys(7) = 'MAP_RESOLUTION'

	pdf_parms(8) = 'POS_LON_DIR'
	pds_keys(8) = 'POSITIVE_LONGITUDE_DIRECTION'

	pdf_parms(9) = 'CTR_LAT'
	pds_keys(9) = 'CENTER_LATITUDE'

	pdf_parms(10) = 'CTR_LON'
	pds_keys(10) = 'CENTER_LONGITUDE'

	pdf_parms(11) = 'SPHERICAL_AZ'
	pds_keys(11) = 'SPHERICAL_AZIMUTH'

	pdf_parms(12) = 'CARTESIAN_AZ'
	pds_keys(12) = 'CARTESIAN_AZIMUTH'

	pdf_parms(13) = 'LINE_OFFSET'
	pds_keys(13) = 'LINE_PROJECTION_OFFSET'

	pdf_parms(14) = 'SAMPLE_OFFSET'
	pds_keys(14) = 'SAMPLE_PROJECTION_OFFSET'

	pdf_parms(15) = 'PARALLEL_ONE'
	pds_keys(15) = 'FIRST_STANDARD_PARALLEL'

	pdf_parms(16) = 'PARALLEL_TWO'
	pds_keys(16) = 'SECOND_STANDARD_PARALLEL'

	pdf_parms(17) = 'XYZ'
	pds_keys(17) = 'FOCAL_LENGTH'

	pdf_parms(18) = 'TGT_DIST'
	pds_keys(18) = 'TARGET_CENTER_DISTANCE'

	pdf_parms(19) = 'AXIS_LINE'
	pds_keys(19) = 'OPT_AXIS_INTERCEPT_LINE'

	pdf_parms(20) = 'AXIS_SAMPLE'
	pds_keys(20) = 'OPT_AXIS_INTERCEPT_SAMPLE'

	pdf_parms(21) = 'FOCAL_SCALE'
	pds_keys(21) = 'FOCAL_PLANE_SCALE'

	pdf_parms(22) = 'SUB_SPACE_LAT'
	pds_keys(22) = 'SUB_SPACECRAFT_LATITUDE'

	pdf_parms(23) = 'SUB_SPACE_LON'
	pds_keys(23) = 'SUB_SPACECRAFT_LONGITUDE'

	pdf_parms(24) = 'CENT_LINE'
	pds_keys(24) = 'PLANET_CENTER_LINE'

	pdf_parms(25) = 'CENT_SAMPLE'
	pds_keys(25) = 'PLANET_CENTER_SAMPLE'

	pdf_parms(26) = 'N_ANGLE'
	pds_keys(26) = 'NORTH_ANGLE'

	pdf_parms(27) = 'LONG_AXIS'
	pds_keys(27) = 'BODY_LONG_AXIS'

	pdf_parms(28) = 'LL_TYPE'
	pds_keys(28) = 'COORDINATE_SYSTEM_NAME'

	! test acceptance of old keword:
	pdf_parms(29) = 'TGTBOD'
	pds_keys(29) = 'TARGET_BODY'

	pdf_parms(30) = '\0'
	pds_keys(30) = '\0'

C
C	Retrieve NAIF planetary constants kernel path name
C

	call xvp('PCK_PATH',pck_file,count)

C
C	Set up MP data object	
C

	call xvfilename( pck_file, pck_file, status )
	if ( status.lt.0 ) call abend

	call mp_get_par( mp,pdf_parms,pds_keys,pck_file )

	flag = 0

	call mp_set_debug_flag( flag,status )
	if ( status.lt.mp_success ) call abend

C
C	Verify keywords set
C

	call mp_get_keywords( mp,keys,number_keywords,types,classes,
     .		status )
	if ( status.lt.mp_success ) then
  		call xvmessage( 'Error in mp_get_keywords call',' ')
		call xvmessage( 'Test failed', ' ')
		call abend
	endif

C
C	Retrieve and print MP data object values
C

	do i=1,number_keywords
		
	  if( types(i).eq.mp_char ) then
	  	
	     call mp_get_value_str( mp,keys(i),string_value,status )
	     if ( status.lt.mp_success ) then
		call abend 
	     endif

	     write( msg, 1000 ) keys(i), string_value
	     call xvmessage( msg, ' ' )

	  else 
           
           if( types(i).eq.mp_dble ) then

	     call mp_get_value( mp,keys(i),double_value,status )
	     if ( status.lt.mp_success ) then
		call abend 
	     endif

	     write( msg, 1010 ) keys(i), double_value
	     call xvmessage( msg, ' ' )

	   else

	     call xvmessage( 'PDS KEY of unacceptable data type', ' ' )

 	   endif

	  endif	     
	
	enddo
		
C
C	Determine input latitude and longitude type specified as input
C

	call xvp('LL_TYPE',lat_lon_type,count)

	if ( lat_lon_type.eq.'PLANETOCENTRIC' ) ll_type = 1
	if ( lat_lon_type.eq.'PLANETOGRAPHIC' .or.
     &       lat_lon_type.eq.'PLANETODETIC' ) ll_type = 2

	if ( lat_lon_type.eq.'PLANETOGRAPHIC' ) ll_type = 2
	if ( lat_lon_type.eq.'SNYDER_DEFINED' ) ll_type = 3

C
C	Retrieve latitudes and longitudes from the PDS file
C

	call xvp('LATITUDES',latitudes,lat_count)
	call xvp('LONGITUDES',longitudes,lon_count)
	if ( lat_count.lt.lon_count ) then
		count = lat_count
	else
		count = lon_count
	endif

C
C       Perform the forward transformation
C
	call xvmessage(' ',' ')
	call xvmessage('     Table of Point Transformations',' ')
	call xvmessage(' ',' ')
	call xvmessage(' (Lat,Lon) -> (Line,Sample) -> (Lat",Lon")',
     .  		' ')
	call xvmessage(' ',' ')

	do i=1,count
	
	  latitude = latitudes(i)
	  longitude = longitudes(i)

	  call mp_ll2xy(mp,lines(i),samples(i),latitude,
     .   	longitude,ll_type,status)
	  if( status.lt.mp_success ) call abend

	  call mp_xy2ll(mp,lines(i),samples(i),new_lat,
     .   	new_lon,ll_type,status)
	  if( status.lt.mp_success ) call abend

	  write ( msg, 1020 ) latitude,longitude,
     .		lines(i),samples(i),new_lat,new_lon
	  call xvmessage( msg,' ' )

	enddo
	
	call xvmessage(' ',' ')
	call xvmessage('*********************************************'
     .		,' ')
	call xvmessage(' ',' ')
	call xvmessage('     End FORTRAN test of MP routines', ' ')
	call xvmessage(' ',' ')
	call xvmessage('*********************************************'
     .		,' ')
	call xvmessage(' ',' ')

 1000   format( '     ',a, ' = ', a)
 1010	format( '     ',a, ' = ', d10.3)
 1020   format (1X,'(',F8.3,',',F8.3,') -> (',F8.3,',',F8.3,') -> (',
     .          F8.3,',',F8.3,') ')
	end
