				/*					*/
				/* INCLUDE FILES NEEDED FOR ROUTINES 	*/
				/*					*/
#include <stdio.h>		
#include "pdsdef.h"		/* PDS Label Library Include Files	*/
#include "label.h"		
#include "gpds.h"		/* VICAR/PDS Include File		*/
#include <string.h>

#define  PDS_MAX_STRING_LENGTH		257
#define  ASCII_0			48			
#define  ASCII_9			57
#define  ASCII_e			85
#define  ASCII_E			53
#define  ASCII_minus			45
#define  ASCII_period			46
#define  ASCII_plus			43

/*

VICAR ROUTINE GPDS_REPLACE_COLUMN_VALUE

PURPOSE

GPDS_REPLACE_COLUMN_VALUE is a MIPS PDS utility to replace a value in a 
specific column and row of a standard PDS table file of fixed record length.

CALLING SEQUENCE

	#include "gpdsroutines.h"

	main44 ()
	{

	AGGREGATE 	*label_ptr;
	char 	*table_file_name;
	char 	*label_file_name;
	int 	record_number;
	char 	*keyword_name;
	int 	item_number;
	char	formatted_value[];
	int 	status_flag;

	...
	
	label_ptr = gpds_open_file( label_file_name, &status_flag );

	... 

	gpds_replace_column_value( label_ptr, table_file_name, 
			record_number, keyword_name, item_number,
			formatted_value, &status_flag );
	...

	gpds_close_file( label_ptr );
	
	...
	
	}

INPUT

	label_ptr 	(AGGREGATE data type)

	This is the pointer to the PDS specific ODL tree which represents
	the label in the file. This value is passed on to other GPDS routines
	that read or write PDS label values.

	table_file_name	(pointer to character)

	This is a complete pathname for the PDS standard, PDS labeled table
	file that is to be read or searched.

	record_number	(integer)
	
	This is the number of the table record (row) that is to be referenced.

	keyword_name	(pointer to character string)
	
	This is the name of the PDS keyword that identifies the COLUMN
	of the PDS table in which a value is to be replaced.

	item_number	(integer)

	This is the number which references which value element of a set
	is desired to be returned. (e.g. FORMAT = {'BYTE','CHAR','REAL'}
	where 'BYTE' is item number 1 of the FORMAT values. 'CHAR' is item
	number 2 of the FORMAT values, and 'REAL' is item number 3.)

	formatted_value	(character string)

	This is the formatted character string of the value to replace the
	existing value in a column in the PDS table. Maximum length of
	character string is 257, in keeping with the PDS standard. This
	should be a null terminated character string value.

OUTPUT

	status_flag 	(integer)

	This is an indicator of the success or failure of the opening of the
	file for reading or writing of the PDS label. The following are the
	meanings of the returned values:

	0	successful writing of column value

	-1	failure in finding necessary label information to proceed
		with writing of column value to table file
	
	-2	error in opening table file (argument table_file_name)

	-3	invalid record number passed (argument record). Either
		zero, negative or greater than the maximum number of records
		in table file

	-4	invalid item number passed (argument item_number). Either
		zero, negative or greater than the maximum number of items
		in column object

	-5	invalid formatted value passed (argument formatted_value),
		the formatted value is checked against the column data type
		and if there is a type conflict, the status flag is set to -5.
	
	-6	memory allocation error for buffer to hold table file's
		row values
	
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

void gpds_replace_column_value( label_ptr, table_file_name, record, keyword, 
	item_number_in_set, formatted_value, status )
AGGREGATE 	label_ptr;
char	 	*table_file_name;
int  		record;
char 		*keyword;
int  		item_number_in_set;
char 		*formatted_value;
int  		*status;
{
PARAMETER 	parameter_ptr;

FILE		*table_file_ptr;
int 		label_status;

int		byte_offset;
int		items;
int		item_bytes;
int		item_offset;
int		maximum_characters_in_value;
int		record_bytes;
int		record_position_in_file;
int		seek_offset;
int		start_byte_in_table_row;
int		total_table_records;
int		value_length;

int		i, j, k;

char		*value;
char		string[PDS_MAX_STRING_LENGTH];
char		*row_string;
char		data_type[20];

/*

Initialize values.

*/
*status = 0;

value = lab_get_value(label_ptr, "ROOT", 0, 1, "FILE_RECORDS", 0, 1,
		FALSE, &label_status);

/*

Check that FILE_RECORDS keyword is found.

*/

if ( 	label_status == PDS_ERROR ||
	label_status == PDS_MULTIPLE_OBJECTS ||
	label_status == PDS_MULTIPLE_PARMS )
	{
	*status = -1;
	return;
	}

total_table_records = atoi(value);

/* 

Check that a valid record number was passed by application 	

*/
if ( record < 1 || record > total_table_records ) 
	{
	*status = -3;
	return;
	}

/*

Ensure keyword is in uppercase.

*/

zccase(keyword,1,-1);

value = lab_get_value(label_ptr, "COLUMN", keyword, 1, 
	"START_BYTE", 0, 1, 0, &label_status);

if ( 	label_status == PDS_ERROR ||
	label_status == PDS_MULTIPLE_OBJECTS ||
	label_status == PDS_MULTIPLE_PARMS )
	{
	*status = -1;
	return;
	}
else
	start_byte_in_table_row = atoi(value);	

value = lab_get_value(label_ptr, "COLUMN", keyword, 1, 
	"BYTES", 0, 1, 0, &label_status);

if ( 	label_status == PDS_ERROR ||
	label_status == PDS_MULTIPLE_OBJECTS ||
	label_status == PDS_MULTIPLE_PARMS )
	{
	*status = -1;
	return;
	}
else
	maximum_characters_in_value = atoi(value);

value = lab_get_value(label_ptr, "COLUMN", keyword, 1, 
	"DATA_TYPE", 0, 1, 0, &label_status);

if ( 	label_status == PDS_ERROR ||
	label_status == PDS_MULTIPLE_OBJECTS ||
	label_status == PDS_MULTIPLE_PARMS )
	{
	*status = -1;
	return;
	}
else
	{
	strcpy(data_type,value);

	/*

	Check user's input (formatted_value) against valid data types
	and verify that content of formatted_value agrees.

	*/

	if ( 	strcmp(data_type,"ASCII_INTEGER") == 0 ||
		strcmp(data_type,"INTEGER") == 0 ||
		strcmp(data_type,"MSB_INTEGER") == 0 ||
		strcmp(data_type,"LSB_INTEGER") == 0 ||
		strcmp(data_type,"MSB_UNSIGNED_INTEGER") == 0 ||
		strcmp(data_type,"LSB_UNSIGNED_INTEGER") == 0 ||
		strcmp(data_type,"SUN_INTEGER") == 0 ||
		strcmp(data_type,"VAX_INTEGER") == 0 ||
		strcmp(data_type,"SUN_UNSIGNED_INTEGER") == 0 ||
		strcmp(data_type,"VAX_UNSIGNED_INTEGER") == 0 	)
		for ( i=0; i<strlen(formatted_value); i++ )
			if ( (	formatted_value[i] < ASCII_0 ||
				formatted_value[i] > ASCII_9 ) &&
				formatted_value[i] != ASCII_minus &&
				formatted_value[i] != ASCII_plus )
				{
				*status = -5;
				return;
				}

	if ( 	strcmp(data_type,"ASCII_REAL") == 0 ||
		strcmp(data_type,"FLOAT") == 0 ||
		strcmp(data_type,"REAL") == 0 ||
		strcmp(data_type,"IEEE_REAL") == 0 ||
		strcmp(data_type,"SUN_REAL") == 0 ||
		strcmp(data_type,"VAX_REAL") == 0 	)
		for ( i=0; i<strlen(formatted_value); i++ )
			if ( (	formatted_value[i] < ASCII_0 ||
				formatted_value[i] > ASCII_9 ) &&
				formatted_value[i] != ASCII_minus &&
				formatted_value[i] != ASCII_plus &&
				formatted_value[i] != ASCII_period &&
				formatted_value[i] != ASCII_e &&
				formatted_value[i] != ASCII_E )
				{
				*status = -5;
				return;
				}

	}				

/*

Determine if keyword value has multiple elements.

*/

parameter_ptr = lab_find_parameter(label_ptr, "COLUMN", keyword, 1, 
	"ITEMS", 0, &label_status);

if ( 	label_status != PDS_ERROR &&
	label_status != PDS_MULTIPLE_OBJECTS &&
	label_status != PDS_MULTIPLE_PARMS )
	{
	value = lab_get_value(label_ptr, "COLUMN", keyword, 1, 
		"ITEMS", 0, 1, 0, &label_status);

	if ( 	label_status == PDS_ERROR ||
		label_status == PDS_MULTIPLE_OBJECTS ||
		label_status == PDS_MULTIPLE_PARMS )
		{
		*status = -1;
		return;
		}
	items = atoi(value);

	/*

	Verify that the user's input of item number is within bounds.

	*/
	if ( item_number_in_set < 1 || item_number_in_set > items )
		{
		*status = -4;
		return;
		}

	value = lab_get_value(label_ptr, "COLUMN", keyword, 1, 
		"ITEM_BYTES", 0, 1, 0, &label_status);

	if ( 	label_status == PDS_ERROR ||
		label_status == PDS_MULTIPLE_OBJECTS ||
		label_status == PDS_MULTIPLE_PARMS )
		{
		*status = -1;
		return;
		}
	item_bytes = atoi(value);

	value = lab_get_value(label_ptr, "COLUMN", keyword, 1, 
		"ITEM_OFFSET", 0, 1, 0, &label_status);

	if ( 	label_status == PDS_ERROR ||
		label_status == PDS_MULTIPLE_OBJECTS ||
		label_status == PDS_MULTIPLE_PARMS )
		{
		*status = -1;
		return;
		}
	item_offset = atoi(value);

	start_byte_in_table_row += item_offset * (item_number_in_set - 1);

	maximum_characters_in_value = item_bytes;
	}

/* 

Get number of bytes per record.

*/

value = lab_get_value(label_ptr, "ROOT", 0, 1, "RECORD_BYTES", 
		0, 1, 0, &label_status);

if ( 	label_status == PDS_ERROR ||
	label_status == PDS_MULTIPLE_OBJECTS ||
	label_status == PDS_MULTIPLE_PARMS )
	{
	*status = -1;
	return;
	}
record_bytes = atoi(value);

/* 

Determine starting byte of string value from table.

*/

byte_offset = start_byte_in_table_row - 1;

/* 

Open table file and advance to starting byte of string value (fseek) and 
get string value (fgets).

*/

table_file_ptr = fopen( table_file_name, "r+" );
if ( table_file_ptr == NULL )
	{
	*status = -2;
	return;
	}

row_string = (char *)calloc(record_bytes + 2,sizeof(char));
if ( row_string == NULL )
	{
	*status = -6;
	return;
	}

for ( i=0; i<record; i++ )
	fgets(row_string,record_bytes+1,table_file_ptr);

record_position_in_file = ftell(table_file_ptr);

fclose(table_file_ptr);

/* 

Calculate starting byte of last record.

*/
seek_offset = (record_position_in_file/record) * (record-1);


value_length = strlen(formatted_value);

if ( value_length < maximum_characters_in_value )
	{
	strncpy(string,formatted_value,value_length);
	string[value_length] = '\0';
	}
else
	{
	strncpy(string,formatted_value,maximum_characters_in_value);
	string[maximum_characters_in_value] = '\0';
	}

if( strcmp(data_type,"CHARACTER") == 0 )
	{
	strcat(string,"\"");
	if ( start_byte_in_table_row+maximum_characters_in_value+2 < record_bytes )
		strcat(string,",");
	for( i=0; i<maximum_characters_in_value-value_length; i++ )
		strcat(string," ");
	}
else
	{
	if ( start_byte_in_table_row+maximum_characters_in_value+2 < record_bytes )
		strcat(string,",");
	for( i=0; i<maximum_characters_in_value-value_length; i++ )
		strcat(string," ");
	}

for ( i=0; i<strlen(string); i++ )
	row_string[byte_offset+i] = string[i];

table_file_ptr = fopen( table_file_name, "r+" );
if ( table_file_ptr == NULL )
	{
	*status = -2;
	return;
	}

fseek(table_file_ptr,seek_offset,0);

fputs(row_string,table_file_ptr);

fclose(table_file_ptr);

free (row_string);
}
  
