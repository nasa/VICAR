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

VICAR ROUTINE GPDS_GET_COLUMN_VALUE

PURPOSE

GPDS_GET_COLUMN_VALUE is a MIPS PDS utility to retrieve a value from a specific
column and row of a standard PDS table file of fixed record length.

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
	char 	*data_type;
	int 	*value_length
	int 	status_flag;
	char	*value;

	...

	label_ptr = gpds_open_file( label_file_name, &status_flag );

	...

	value = gpds_get_column_value( label_ptr, table_file_name, 
			record_number, keyword_name, item_number,
			data_type, value_length, status_flag );
	...

	gpds_close_file( label_ptr );

	...
	
	}

INPUT

	label_ptr 	(AGGREGATE data type)

	This is the pointer to the PDS specific ODL tree which represents
	the label in the file. This value is passed on to other GPDS routines
	that read or write PDS label values.

	table_file_name	(pointer to character string)

	This is a complete pathname for the PDS standard, PDS labeled table
	file that is to be read or searched.

	record_number	(integer)
	
	This is the number of the table record (row) that is to read.

	keyword_name	(pointer to character string)
	
	This is the name of the PDS keyword that identifies the COLUMN
	of the PDS table from which a value is desired.

	item_number	(integer)

	This is the number which references which value element of a set
	is desired to be returned. (e.g. FORMAT = {'BYTE','CHAR','REAL'}
	where 'BYTE' is item number 1 of the FORMAT values. 'CHAR' is item
	number 2 of the FORMAT values, and 'REAL' is item number 3.)

RETURN

	value		(pointer to character string)

	This is the character string of the desired value from a column
	in the PDS table. Maximum character string length returned is 
	257, based on the PDS standards. Column values of greater length
	may cause memory addressing problems.

OUTPUT
	data_type	(pointer to character string)

	This is the PDS defined data type of the value returned in the 
	character string.

	value_length	(integer)

	This is the length of the character string returned. 

	status_flag 	(integer)

	This is an indicator of the success or failure of the opening of the
	file for reading or writing of the PDS label. The following are the
	meanings of the returned values:

	0	successful opening for reading of PDS labeled file

	-1	failure in finding necessary label information to proceed
		with retrieval of column value for table file

	-2	error in opening table file (argument table_file_name)

	-3	invalid record number passed (argument record). Either
		zero, negative or greater than the maximum number of records
		in table file

	-4	invalid item number passed (argument item_number). Either
		zero, negative or greather than the maximum number of items
		in column object

	-5	memory allocation error for buffer to hold table file's 
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

char 	*gpds_get_column_value( label_ptr, table_file_name, record, 
	keyword, item_number_in_set, data_type, value_length, status )
AGGREGATE 	label_ptr;
char	 	*table_file_name;
int  		record;
char 		*keyword;
int  		item_number_in_set;
char 		*data_type;
int 		*value_length;
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
int		start_byte_in_table_row;
int		total_table_records;

int		i, j, k;

char		*value;
static char		string[PDS_MAX_STRING_LENGTH];
char		*row_string;
static char		blanks[2];

/*

Initialize values.

*/
*status = 0;
string[0] = '\0';
strcpy(blanks," ");

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
	return blanks;
	}

total_table_records = atoi(value);

/* 

Check that a valid record number was passed by application 	

*/
if ( record < 1 || record > total_table_records ) 
	{
	*status = -3;
	return blanks;
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
	return blanks;
	}
else
	start_byte_in_table_row = atoi(value);	

value = lab_get_value(label_ptr, "COLUMN", keyword, 1, 
	"DATA_TYPE", 0, 1, 0, &label_status);

if ( 	label_status == PDS_ERROR ||
	label_status == PDS_MULTIPLE_OBJECTS ||
	label_status == PDS_MULTIPLE_PARMS )
	{
	*status = -1;
	return blanks;
	}
else
	strcpy(data_type,value);

value = lab_get_value(label_ptr, "COLUMN", keyword, 1, 
	"BYTES", 0, 1, 0, &label_status);

if ( 	label_status == PDS_ERROR ||
	label_status == PDS_MULTIPLE_OBJECTS ||
	label_status == PDS_MULTIPLE_PARMS )
	{
	*status = -1;
	return blanks;
	}
else
	maximum_characters_in_value = atoi(value);

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
		return blanks;
		}
	items = atoi(value);

	/*

	Verify that the user's input of item number is within bounds.

	*/
	if ( item_number_in_set < 1 || item_number_in_set > items )
		{
		*status = -4;
		return blanks;
		}

	value = lab_get_value(label_ptr, "COLUMN", keyword, 1, 
		"ITEM_BYTES", 0, 1, 0, &label_status);

	if ( 	label_status == PDS_ERROR ||
		label_status == PDS_MULTIPLE_OBJECTS ||
		label_status == PDS_MULTIPLE_PARMS )
		{
		*status = -1;
		return blanks;
		}
	item_bytes = atoi(value);

	value = lab_get_value(label_ptr, "COLUMN", keyword, 1, 
		"ITEM_OFFSET", 0, 1, 0, &label_status);

	if ( 	label_status == PDS_ERROR ||
		label_status == PDS_MULTIPLE_OBJECTS ||
		label_status == PDS_MULTIPLE_PARMS )
		{
		*status = -1;
		return blanks;
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
	return blanks;
	}
record_bytes = atoi(value);

/* 

Determine starting byte of string value from table.

*/

byte_offset = start_byte_in_table_row - 1;

/* 

Advance in table file to starting byte of string value (fseek) and 
get string value (fgets).

*/

table_file_ptr = fopen( table_file_name, "r" );
if ( table_file_ptr == NULL )
	{
	*status = -2;
	return blanks;
	}

row_string = (char *)calloc(record_bytes + 2,sizeof(char));
if ( row_string == NULL )
	{
	*status = -5;
	return blanks;
	}

for ( i=0; i<record; i++ )
	fgets(row_string,record_bytes+1,table_file_ptr);

strncpy(string,&row_string[byte_offset],maximum_characters_in_value);
string[maximum_characters_in_value] = '\0';

fclose(table_file_ptr);			/* Close table file.	*/

free (row_string);			/* Free memory		*/

/*

Find first non-blank character in string from its end.

*/

i = strlen(string) - 1;
while( i > 0 && (string[i] == ' ' || string[i] == '\0') )
	i--;

/*

If a comma is found then

	if data type is CHARACTER then

		search for closing ' or " of character string and
		null terminate the string.

	else
		
		search for first non-blank character and
		null terminate the string.

else

	if some other character is found, simply null terminate the string.

*/

if ( string[i] == ',' )
	if ( strcmp(data_type,"CHARACTER") == 0 )
		{
		i--;
		while ( i > 0 &&
		      ( string[i] == '\"' || 
			string[i] == '\'' || string[i] == ' ' ) )
			i--;
		string[++i] = '\0';	/* End desired string with NULL	 */
		}
	else
		{
		i--;
		while ( i > 0 && string[i] == ' ' )
			i--;
		string[++i] = '\0';	/* End desired string with NULL	 */
		}
else
	if ( strcmp(data_type,"CHARACTER") == 0 )
		{
		while ( i > 0 &&
		      ( string[i] == '\"' || 
			string[i] == '\'' || string[i] == ' ' ) )
			i--;
		string[++i] = '\0';	/* End desired string with NULL	 */
		}
	else
		string[++i] = '\0';	/* Replace comma with NULL	 */

*value_length = strlen(string);		/* Return length of string value */

return string;				/* Return address of string	 */
}
