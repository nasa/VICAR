				/*					*/
				/* INCLUDE FILES NEEDED FOR ROUTINES 	*/
				/*					*/
#include <stdio.h>		
#include "pdsdef.h"		/* PDS Label Library Include Files	*/
#include "label.h"		
#include "gpds.h"		/* MIPS/PDS Include File		*/
#include <string.h>

#define  PDS_MAX_STRING_LENGTH		257

/*
VICAR ROUTINE GPDS_GET_HISTOGRAM

PURPOSE

GPDS_GET_HISTOGRAM is a MIPS PDS utility to retrieve the histogram object in
a PDS labeled file as a pointer to a one dimensional histogram array.

CALLING SEQUENCE

	#include "gpdsroutines.h"

	main ()
	{

	AGGREGATE *label_ptr;
	unsigned integer *histogram;
	char *object_name;
	char *input_file_name;
	int status_flag; 
	int number_elements, element_size;

	....
	
	label_ptr = gpds_open_file( input_file_name, &status_flag );
	
	....
	
	histogram = gpds_get_histogram( label_ptr, input_file_name, 
		object_name, &number_elements, &element_size, &status_flag );
	....
	
	gpds_close_file( label_ptr );

	....

	}

INPUT

	label_pointer	(AGGREGATE type)

	This is a pointer to the root of the PDS label. It is returned by the
	routine GPDS_OPEN_FILE.

	input_file_name (pointer to character string)

	This is a complete pathname for the PDS standard, PDS labeled input
	file which contains a histogram object.

	object_name  	(pointer to character string)

	This is the PDS object name (exact spelling including underscores)
	in which the histogram is stored in the input file. This name is 
	searched for in the PDS label of the input file. The default 
	object_name is RAW_IMAGE_HISTOGRAM (e.g. the software searches
	the PDS label of the input file for the following occurence
	'OBJECT = RAW_IMAGE_HISTOGRAM' in the default case).

RETURN

	histogram 	(unsigned integer array)

	This is the pointer to a histogram of data type unsigned integer.
	The number of elements in the histogram is returned by the output
	parameter number_elements. The maximum dimension of this array is
	65536, to accomodate images of up to two bytes per pixel.

OUTPUT

	number_elements	(integer)

	This is the number of elements returned in the array that is 
	pointed by "histogram". The maximum valid value returned by
	this argument is 65536.

	elements_size 	(integer)

	This is the number of bytes in each element returned in the 
	histogram array.

	status_flag 	(integer)

	This is an indicator of the success or failure of the returning the
	value of a PDS keyword. The following are the meanings of the 
	returned values:

	0	successful returning of pointer to histogram array

	-1	failure in finding necessary label information to proceed
		with retrieval of histogram values from input file

	-2	error in opening input file (argument input_file_name)

	-3	invalid data type (DATA_TYPE PDS keyword) found in label of 
		input file; gpds_get_histogram only supports unsigned integer 
		data types.

	-4	invalid bytes per item (ITEM_BYTES PDS keyword) found in 
		label of input file.

	-5	invalid number of elements (ITEMS PDS keyword) found
		in label of input file.

	-6	invalid PDS record type (RECORD_TYPE PDS keyword) found
		in label of input file. Record type must be FIXED_LENGTH.

	-7	memory allocation error for buffer to hold record read from
		input file.

BACKGROUND

This routine is based on the PDS Label Library of the PDS Toolbox, version 5.2
dated September 27, 1991. Refer to PDS Label Library User's Guide, D-8922
for reference to software called in this source.

HISTORY

Programmer: 		Justin McNeill, Jr.
Cognizant programmer:	Justin McNeill, Jr.
Written:		August 1993
Revisions:

Aug. 10, 98  ...RRP... Made it ANSI compatiable so it can be compiled on
                       HP.
	
*/
/*************************************************************************

C Callable Version

*************************************************************************/

unsigned int *gpds_get_histogram( label_ptr, input_file_name, object_name, 
	number_elements, element_size, status )
AGGREGATE 	label_ptr;
char	 	*input_file_name;
char	 	*object_name;
int  		*number_elements;
int  		*element_size;
int  		*status;
{
FILE		*file_ptr;
int 		label_status;

int		i, j, k;
int		byte_offset;
int		histogram_record;
int		integers_per_record, items;
int		item_bytes;
int		item_offset;
int		record_bytes;
int		records_required;
int		total_records;

static unsigned int	histogram[65536];
unsigned int	*record;

char		data_type[40];
char		string[PDS_MAX_STRING_LENGTH];
char		*value;

/*

Initialize values.

*/

*status = 0;

/*

Check that FILE_RECORDS keyword is found.

*/

value = lab_get_value(label_ptr, "ROOT", 0, 1, "FILE_RECORDS", 0, 1,
		FALSE, &label_status);

if ( 	label_status == PDS_ERROR ||
	label_status == PDS_MULTIPLE_OBJECTS ||
	label_status == PDS_MULTIPLE_PARMS )
	{
	*status = -1;
	return NULL;
	}

total_records = atoi(value);

/*

Check that RECORD_TYPE keyword is found.

*/

value = lab_get_value(label_ptr, "ROOT", 0, 1, "RECORD_TYPE", 0, 1,
		FALSE, &label_status);

if ( strcmp(value,"FIXED_LENGTH") != 0 )
	{
	*status = -6;
	return NULL;
	}

/*

Check that RECORD_BYTES keyword is found.

*/

value = lab_get_value(label_ptr, "ROOT", 0, 1, "RECORD_BYTES", 0, 1,
		FALSE, &label_status);

if ( 	label_status == PDS_ERROR ||
	label_status == PDS_MULTIPLE_OBJECTS ||
	label_status == PDS_MULTIPLE_PARMS )
	{
	*status = -1;
	return NULL;
	}

record_bytes = atoi(value);

/*

Get HISTOGRAM record. 

*/

zccase(object_name,1,-1);

value = lab_get_value(label_ptr, "ROOT", 0, 1, object_name,
		0, 1, FALSE, &label_status);

if ( 	label_status == PDS_ERROR ||
	label_status == PDS_MULTIPLE_OBJECTS ||
	label_status == PDS_MULTIPLE_PARMS )
	{
	*status = -1;
	return NULL;
	}

histogram_record = atoi(value);

/*

Get DATA_TYPE and test against acceptable types.

*/

value = lab_get_value(label_ptr, object_name, 0, 1, 
	"DATA_TYPE", 0, 1, 0, &label_status);

if ( 	label_status == PDS_ERROR ||
	label_status == PDS_MULTIPLE_OBJECTS ||
	label_status == PDS_MULTIPLE_PARMS )
	{
	*status = -1;
	return NULL;
	}
else
	{
	strcpy(data_type,value);
	if ( 	strcmp(data_type,"MSB_UNSIGNED_INTEGER") != 0 &&
		strcmp(data_type,"LSB_UNSIGNED_INTEGER") != 0 &&
		strcmp(data_type,"SUN_UNSIGNED_INTEGER") != 0 &&
		strcmp(data_type,"MAC_UNSIGNED_INTEGER") != 0 &&
		strcmp(data_type,"UNSIGNED_INTEGER") != 0 )
		{
		*status = -3;
		return NULL;
		}
	}

/*

Get ITEM_BYTES and test against unsigned integer byte size.

*/

value = lab_get_value(label_ptr, object_name, 0, 1, 
	"ITEM_BYTES", 0, 1, 0, &label_status);

if ( 	label_status == PDS_ERROR ||
	label_status == PDS_MULTIPLE_OBJECTS ||
	label_status == PDS_MULTIPLE_PARMS )
	{
	*status = -1;
	return NULL;
	}
else
	{
	*element_size = atoi(value);
	if ( *element_size != 4 )
		{
		*status = -4;
		return NULL;
		}
	}

value = lab_get_value(label_ptr, object_name, 0, 1, 
	"ITEMS", 0, 1, 0, &label_status);

if ( 	label_status == PDS_ERROR ||
	label_status == PDS_MULTIPLE_OBJECTS ||
	label_status == PDS_MULTIPLE_PARMS )
	{
	*status = -1;
	return NULL;
	}
else
	{
	*number_elements = atoi(value);
	if ( *number_elements > 65536 || *number_elements < 0 )
		{
		*status = -5;
		return NULL;
		}
	}

/* 

Determine records in histogram object and other values needed for
histogram reading.

*/

integers_per_record = record_bytes / *element_size;
records_required = *number_elements / integers_per_record;
if ( *number_elements % integers_per_record != 0 )
	records_required += 1;

/*

Allocate memory for record of unsigned integers and histogram.

*/

record = (unsigned int *)calloc(integers_per_record,sizeof(unsigned int));
if ( record == NULL )
	{
	*status = -7;
	return NULL;
	}

/*

Open input file containing histogram object.

*/

file_ptr = fopen( input_file_name, "r" );
if ( file_ptr == NULL )
	{
	*status = -2;
	return NULL;
	}

/*

Advance to record just before histogram object begins.

*/

for ( i=0; i<histogram_record-1; i++ )
	fread(record,sizeof(unsigned int),integers_per_record,file_ptr);

/*

Read histogram.

*/

for( i=0, k=0; i<records_required; i++ )
	{
	fread(record,sizeof(unsigned int),integers_per_record,file_ptr);
	for( j=0; j<integers_per_record && k<*number_elements; j++, k++ )
		histogram[k] = record[j];
	}
	
fclose(file_ptr);			/* Close input file	*/

free(record);				/* Free memory		*/

return histogram;
}
