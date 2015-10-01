/*

	Include file for Tables/Indices of the
	Mars Observer Camera (MOC) Systematic

	Purpose

	This include file contains the C structure that is used
	to describe and store the row entries of the MOC Systematic
	Volume Indices, Master Indices and Production Tables, 
	see MOC Systematic Software Requirements Document and 
	Software Design Document.

	Date		August 1993
	
	Author		Justin McNeill, Jr.

*/

#include 	"mokeywords.h"

#define	PRODUCTION_TABLE_COLUMNS	8
#define	VOLUME_INDEX_TABLE_COLUMNS	76
#define	MASTER_INDEX_TABLE_COLUMNS	81


struct 	TABLE_ENTRIES	{

	char	keyword[40];
	char	value[40];
	char	data_type[40];
	int	item_number;
	int	value_length;	};

struct 	PRODUCTION_TABLE_ENTRY	{

	char	file_specification_name[40];
	int	lines;
	int	lines_samples;
	int	start_line;
	int	start_sample;
	char	frame_id[10];
	char 	tile_id[10];
	char	mask_type[10];	};


struct 	VOLUME_INDEX_ENTRY	{

	char	file_specification_name[40];
	int	lines;
	int 	lines_samples;
	char	spacecraft_name[20];
	char	mission_phase_name[20];
	char	target_name[20];
	char	instrument_name[20];
	char	instrument_id[5];
	char	producer_id[20];
	char	product_creation_time[30];
	char	software_name[30];
	char	upload_id[30];
	char	data_set_id[30];
	char	volume_id[10];
	char	volume_version_id[10];
	char	product_id[10];
	char	associated_product_id[2][10];
	char	start_time[30];
	char	image_time[30];
	char	stop_time[30];
	char	spacecraft_clock_start_count[20];
	float	focal_plane_temperature;
	char	gain_mode_id[20];
	int	offset_mode_id;
	float	line_exposure_duration;
	int	downtrack_summing;
	int	crosstrack_summing;
	char	edit_mode_id[10];
	char	filter_name[10];
	float	exposure_duration;
	char	rationale_desc[90];
	float	target_center_distance;
	float	central_body_distance;
	float	sub_spacecraft_latitude;
	float	sub_spacecraft_longitude;
	float	twist_angle;
	float	right_ascension;
	float	declination;
	float	north_azimuth;
	float	sub_spacecraft_azimuth;
	float	horizontal_pixel_scale;
	float	vertical_pixel_scale;
	float	slant_distance;
	float	center_latitude;
	float	center_longitude;
	float	upper_left_latitude;
	float	upper_left_longitude;
	float	upper_right_latitude;
	float	upper_right_longitude;
	float	lower_left_latitude;
	float	lower_left_longitude;
	float	lower_right_latitude;
	float	lower_right_longitude;
	float	solar_distance;
	float	sub_solar_latitude;
	float	sub_solar_longitude;
	float	sub_solar_azimuth;
	float	incidence_angle;
	float	emission_angle;
	float	phase_angle;
	float	local_hour_angle;
	char	calibration_file_name[30];
	char	source_product_id[5][20];
	char	processing_history_text[170];
	int	sample_bits;
	int	checksum;
	char	stretched_flag[10];
	int	stretch_minimum[2];
	int	stretch_maximum[2];	};

struct MASTER_INDEX_ENTRY	{


	int	mrps_id;
	char	frame_id[10];
	char	tile_id[10];
	char	file_specification_name[40];
	int	lines;
	int 	lines_samples;
	int	start_line;
	int	start_sample;
	char	spacecraft_name[20];
	char	mission_phase_name[20];
	char	target_name[20];
	char	instrument_name[20];
	char	instrument_id[5];
	char	producer_id[20];
	char	product_creation_time[30];
	char	software_name[30];
	char	upload_id[30];
	char	data_set_id[30];
	char	volume_id[10];
	char	volume_version_id[10];
	char	product_id[10];
	char	associated_product_id[2][10];
	char	start_time[30];
	char	image_time[30];
	char	stop_time[30];
	char	spacecraft_clock_start_count[20];
	float	focal_plane_temperature;
	char	gain_mode_id[20];
	int	offset_mode_id;
	float	line_exposure_duration;
	int	downtrack_summing;
	int	crosstrack_summing;
	char	edit_mode_id[10];
	char	filter_name[10];
	float	exposure_duration;
	char	rationale_desc[90];
	float	target_center_distance;
	float	central_body_distance;
	float	sub_spacecraft_latitude;
	float	sub_spacecraft_longitude;
	float	twist_angle;
	float	right_ascension;
	float	declination;
	float	north_azimuth;
	float	sub_spacecraft_azimuth;
	float	horizontal_pixel_scale;
	float	vertical_pixel_scale;
	float	slant_distance;
	float	center_latitude;
	float	center_longitude;
	float	upper_left_latitude;
	float	upper_left_longitude;
	float	upper_right_latitude;
	float	upper_right_longitude;
	float	lower_left_latitude;
	float	lower_left_longitude;
	float	lower_right_latitude;
	float	lower_right_longitude;
	float	solar_distance;
	float	sub_solar_latitude;
	float	sub_solar_longitude;
	float	sub_solar_azimuth;
	float	incidence_angle;
	float	emission_angle;
	float	phase_angle;
	float	local_hour_angle;
	char	calibration_file_name[30];
	char	source_product_id[5][20];
	char	processing_history_text[170];
	int	sample_bits;
	int	checksum;
	char	stretched_flag[10];
	int	stretch_minimum[2];
	int	stretch_maximum[2];	};

