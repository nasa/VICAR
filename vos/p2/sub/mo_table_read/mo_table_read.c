

				/*					*/
				/* INCLUDE FILES NEEDED FOR ROUTINES 	*/
				/*					*/
#include <stdio.h>		
#include "xvmaininc.h"		/* Standard VICAR Include File		*/
#include "mo_include.h"		/* PDS Label Library Include Files	*/
#include "pdsdef.h"		/* VICAR/PDS Include File		*/
#include "label.h"		/* VICAR/PDS Include File		*/
#include "gpds.h"		/* VICAR/PDS Include File		*/

#define  PDS_MAX_STRING_LENGTH		257

/*

VICAR ROUTINE MO_TABLE_READ

PURPOSE

MO_TABLE_READ is a MIPS PDS utility to open a PDS standard Mars Observer 
table and read one row of information into a predefined data structure. 
Arguments includes the pathname of the detached PDS label describing the table,
pathname of the respective table to be read, pointer to structure to hold 
values returned from the routine, row number of table to be read, table search 
key and value, and status flag.

CALLING SEQUENCE

	{

	...

	#include MO_INCLUDE.H

	char 	*label_pathname;
	char	*table_pathname;
	int	*row_number;
	char	*key;
	char	*key_value;
	int	item_number;
	int	status_flag;
	
	struct PRODUCTION_TABLE_ENTRY	*structure_ptr;

	...

	structure_ptr = (struct PRODUCTION_TABLE_ENTRY *)malloc
				(sizeof(struct PRODUCTION_TABLE_ENTRY));
	...

	mo_table_read( label_pathname, table_pathname, structure_ptr, 
		row_number, key, key_value, item_number, &status_flag );
	...

 	}

INPUT	

	label_pathname	(character string pointer)
	
	This is the complete pathname to the PDS label of the table to be read.	
	This is a required input.

	table_pathname 	(character string pointer)
	
	This is the complete pathname to the PDS table to be read, described by
	the PDS label given by the pathname of label_pathname variable.
	This is a required input.

	key		(character string pointer)

	This is a table search key. Key is the keyword name which has a value 
	specified by key_value which determines whether or not a row is to be
	returned to the structure. Key corresponds to any valid PDS keyword
	that is the name of a COLUMN object in the table's PDS label. For
	example, if the user want to return the values from the next row in
	the specified PDS table file which has its FRAME_ID column value
	equal to the string "R003T034", then the key would be "FRAME_ID" as
	a NULL terminated string and key_value would be "R003T034" as a 
	NULL terminated string. If no search is desired and a row number is
	to be as primary input for a table read, then pass a NULL (zero)
	for the key and the key value ( key = NULL ). 
	****	NOTE: this routine is NOT CASE SENSITIVE.   ****

	key_value	(character string pointer)

	For a specified PDS column name referenced by the key argument, this
	is the value of the key on which a search of a PDS table is based. 
	Key_value is a character string representation of the desired string
	in the table, thus numerical values must first be properly formatted
	into a string. For example, if the user wants to return the values 
	from the next row in the specified PDS table file which has its 
	FRAME_ID column value equal to the string "R003T034", then the key_value
	would be "R003T034" as a NULL terminated string and key would be 
	"FRAME_ID" as a NULL terminated string. If no search is desired and 
	a row number is to be as primary input for a table read, then pass a 
	NULL (zero) for the key and the key value ( key = NULL ).
	****	NOTE: this routine is NOT CASE SENSITIVE.   ****
	
	item_number	(integer)

	This is the number which references which value element of a set 
	is desired to be returned. (e.g. FORMAT = {'BYTE','CHAR','REAL'}
	where 'BYTE' is item number 1 of the FORMAT values. 'CHAR' is item
	number 2 of the FORMAT values, and 'REAL' is item number 3.)


OUTPUT

	row_number	(integer ponter)
	
	This is the table row number that is to be read and/or row at which 
	key with specified key_value is found.

	structure_ptr 	(struct pointer)
	
	This is a pointer to a MO table structures specified in the 
	user's include file, "mo_include.h". The values of the row are
	written to this structure.

	status_flag	(integer pointer)
	
	This is a indicator of the success or failure of the table read.
	The following are the returned values meanings:

	0	successful read from table

	-1	error in opening PDS label file 

	-2	error in retrieving value from a column of the PDS table file

	-3	invalid RECORD TYPE or RECORD TYPE not found in label file

	-4 	invalid row number argument

	-5 	key search failed (EOF reached)

	-6	unsupported table type

HISTORY

	Current cognizant programmer:	Justin McNeill
	Date:				August 1993
	Revisions:			Original

*/
/*************************************************************************

C Callable Version

*************************************************************************/

void	mo_table_read( label_file_name, table_file_name, t_ptr, 
	row_number, key, key_value, item_number, status )
char	*label_file_name;
char	*table_file_name;
void 	*t_ptr;
int	*row_number;
char	*key;
char	*key_value;
int	item_number;
int	*status;

{

AGGREGATE 	label_ptr;
int		i,j,k;
int		KEY_FOUND;
int		record;
int  		status_flag;
int  		total_columns;
int  		total_records;
int		value_length;

char		temp_value[PDS_MAX_STRING_LENGTH];
char		data_type[20];
char		string[200];
char		*table_name;
char		*value;

struct 	PRODUCTION_TABLE_ENTRY *table_ptr;
struct	TABLE_ENTRIES	**p_table_entries;

/*

Initialize values.

*/

KEY_FOUND = FALSE;
*status = 0;

/*

Open label file and return label pointer.

*/
label_ptr = gpds_open_file(label_file_name,&status_flag);
if ( status_flag < 0 )
	{
	*status = -1;
	return;
	}

/*

Get the number of records in table file.

*/
value = gpds_get_label_value(label_ptr, "ROOT", moFILE_RECORDS, 
		1, &status_flag);
if ( status_flag < 0 )
	{
	*status = -1;
	return;
	}
total_records = atoi(value);

/* 

Check that a valid row number was passed by application 	

*/
if ( *row_number < 1 || *row_number > total_records ) 
	{
	*status = -4;
	return;
	}

/*

Verify that record type of file is FIXED_LENGTH.

*/

value = gpds_get_label_value(label_ptr, "ROOT", moRECORD_TYPE, 1, &status_flag);
if ( status_flag < 0 )
	{
	*status = -3;
	return;
	}
else
	if ( strcmp(value,"FIXED_LENGTH") != 0 )
		{
		*status = -3;
		return;
		}
/*

If specified by user, perform table row search with key and its desired value.

*/

record = *row_number;

if ( key != NULL )
	{
	while (	KEY_FOUND == FALSE && record <= total_records )
		{

		value = gpds_get_column_value ( label_ptr, table_file_name, 
				record, key, item_number, data_type, 
				&value_length, &status_flag );

		if ( status_flag < 0 )
			{
			*status = -2;
			return;
			}

		/*

		Complete string comparison in lowercase.

		*/

		strcpy(temp_value,key_value);
		zccase(temp_value,-1,-1);	
		zccase(value,-1,-1);			

		if ( strcmp(temp_value,value) == 0 )
			{
			KEY_FOUND = TRUE;
			*row_number = record;
			}
		else
			record++;
		}

	if ( KEY_FOUND == FALSE )
		{
		*status = -5;
		return;
		}
	}

/*

Get number of columns to determine the table type.

*/

value = gpds_get_label_value(label_ptr, moTABLE, "COLUMNS", 1, &status_flag);
if ( status_flag < 0 )
	{
	*status = -1;
	return;
	}
total_columns = atoi(value);

/*

Based on the type of PDS table, retrieve the appropriate values from 
the table and load appropriate table structure.

*/

switch( total_columns )		{

case	PRODUCTION_TABLE_COLUMNS:

	/*
	
	Load keywords into structure (dynamically allocated)

	*/

	p_table_entries = (struct TABLE_ENTRIES **)calloc
		(PRODUCTION_TABLE_COLUMNS,sizeof(struct TABLE_ENTRIES *));
	if ( p_table_entries == NULL )
		{
		*status = -7;
		return;
		}
	for ( i=0; i<PRODUCTION_TABLE_COLUMNS; i++ )
		p_table_entries[i] = (struct TABLE_ENTRIES *)malloc
					(sizeof(struct TABLE_ENTRIES));

	strcpy(p_table_entries[0]->keyword,moFILE_SPECIFICATION_NAME);
	strcpy(p_table_entries[1]->keyword,moLINES);
	strcpy(p_table_entries[2]->keyword,moLINES_SAMPLES);
	strcpy(p_table_entries[3]->keyword,moSTART_LINE);
	strcpy(p_table_entries[4]->keyword,moSTART_SAMPLE);
	strcpy(p_table_entries[5]->keyword,moFRAME_ID);
	strcpy(p_table_entries[6]->keyword,moTILE_ID);	
	strcpy(p_table_entries[7]->keyword,moMASK_TYPE);
	
	/*

	Retrieve values from row in production table.
	
	*/
	
	for ( i=0; i<PRODUCTION_TABLE_COLUMNS; i++ )
		{
		p_table_entries[i]->item_number = 1;

		value = gpds_get_column_value ( label_ptr, table_file_name, 
				*row_number,p_table_entries[i]->keyword,
				p_table_entries[i]->item_number,
				p_table_entries[i]->data_type,
				&p_table_entries[i]->value_length,
				&status_flag );

		strcpy(p_table_entries[i]->value,value);

		if ( status_flag < 0 )
			{
			*status = -2;
			return;
			}
		}
			
	/*

	Load production table referenced by table_ptr.

	*/

	table_ptr = t_ptr;

	strcpy(table_ptr->file_specification_name,p_table_entries[0]->value);
	table_ptr->lines = atoi(p_table_entries[1]->value);
	table_ptr->lines_samples = atoi(p_table_entries[2]->value);
	table_ptr->start_line = atoi(p_table_entries[3]->value);
	table_ptr->start_sample = atoi(p_table_entries[4]->value);
	strcpy(table_ptr->frame_id,p_table_entries[5]->value);
	strcpy(table_ptr->tile_id,p_table_entries[6]->value);
	strcpy(table_ptr->mask_type,p_table_entries[7]->value);

	for ( i=0; i<PRODUCTION_TABLE_COLUMNS; i++ )
		free ( p_table_entries[i] );
	free ( p_table_entries );

	break;

case	MASTER_INDEX_TABLE_COLUMNS:

	break;

case	VOLUME_INDEX_TABLE_COLUMNS:

	break;

default:

	*status = -6;
	break;		};

/*

Close the label_ptr file.

*/

gpds_close_file(label_ptr);

return;
}
