#include   <stdio.h>
#include   "vicmain_c"
#include   "gpdsroutines.h"
#include   "mo_include.h"

#define TEST_ARRAY_LENGTH	10

/*

GPDS_GET_COLUMN_VALUE 	acceptance and unit test

Author: Justin McNeill
Date:	August 1993

*/

main44()
{
int		i,j,k;
int		status;
int		label_status;
int		count;
int 		total_records;
int		lengths[10];

char		string[120];
char		label_file_name[80],
		table_file_name[80];

AGGREGATE	label_ptr;

char		*value;

int		row_number;
int		item_number[TEST_ARRAY_LENGTH];
char		keywords[TEST_ARRAY_LENGTH][40], 
		data_types[TEST_ARRAY_LENGTH][20], 
		object[TEST_ARRAY_LENGTH][20], 
		values[TEST_ARRAY_LENGTH][40];

zvmessage("************************************************"," ");
zvmessage(" "," ");
zvmessage("Acceptance test for GPDS_GET_COLUMN_VALUE"," ");
zvmessage(" "," ");
zvmessage("************************************************"," ");

status = zvp("LABEL",label_file_name,&count);
if ( status < 0 ) 
	{
	zvmessage("*** Error in ZVP call for LABEL"," ");
	zabend();
	}

status = zvp("TABLE",table_file_name,&count);
if ( status < 0 ) 
	{
	zvmessage("*** Error in ZVP call for TABLE"," ");
	zabend();
	}

zvmessage(" "," ");
zvmessage("\tOpen label file"," ");
zvmessage(" "," ");

label_ptr = gpds_open_file(label_file_name,&status);
if ( status < 0 )
	{
	zvmessage("*** Error in GPDS_OPEN_FILE call"," ");
	zabend();
	}
else
	zvmessage("\tSuccessful GPDS_OPEN_FILE call"," ");

zvmessage(" "," ");
zvmessage("************************************************"," ");
zvmessage(" "," ");
zvmessage(" C Test of gpds_get_column_value"," ");
zvmessage(" "," ");
zvmessage("************************************************"," ");
zvmessage(" "," ");

strcpy(keywords[0],moFILE_SPECIFICATION_NAME);
strcpy(keywords[1],"DIMENSIONS");
strcpy(keywords[2],"DIMENSIONS");
strcpy(keywords[3],moSTART_LINE);
strcpy(keywords[4],moSTART_SAMPLE);
strcpy(keywords[5],moFRAME_ID);
strcpy(keywords[6],moTILE_ID);
strcpy(keywords[7],moMASK_TYPE);
strcpy(keywords[8],"DIMENSIONS");
strcpy(keywords[9],"MASKTYPE");		/* Test for invalid keyword name   */

for ( i=0; i<TEST_ARRAY_LENGTH; i++ )
	item_number[i] = 1;
item_number[2] = 2;			/* Get second dimension item value */
item_number[8] = -2;			/* Test for invalid item number    */

/*

Get the total number of records in table file.

*/

value = gpds_get_label_value(label_ptr,"ROOT","FILE_RECORDS",1,&status);

total_records = atoi(value);

/*

For all the records in the table file, retrieve all values for valid arguments
passed.

*/

for ( i=1; i<=total_records; i++ )
	{
	zvmessage(" "," ");
	sprintf(string,"\t*** RECORD %d OF FILE %s ***",i,table_file_name);
	zvmessage(string," ");
	zvmessage(" "," ");

	for ( j=0; j<TEST_ARRAY_LENGTH-2; j++ )
		{
		value = gpds_get_column_value(	label_ptr, table_file_name, i,
						keywords[j], item_number[j],
						data_types[j], &lengths[j], 
						&status ); 

		if ( status < 0 )
			{
			zvmessage("\t*** Error in GPDS_GET_COLUMN_VALUE call"
				," ");
			sprintf(string,"\t*** Status flag value is %d",status);
			zvmessage(string," ");
			zvmessage(" "," ");
			}
		else
			{
			strcpy(values[j],value);
			sprintf(string,"\tKEYWORD %s = %s",
				keywords[j],values[j]);
			zvmessage(string," ");
			sprintf(string,"\tof data type %s and item number %d",
				data_types[j],item_number[j]);
			zvmessage(string," ");
			zvmessage(" "," ");
			}
		}
	}

zvmessage("\tSuccessful GPDS_GET_COLUMN_VALUE calls"," ");
zvmessage(" "," ");
zvmessage("\t************************************************"," ");
zvmessage(" "," ");
zvmessage("\tMake call to GPDS_GET_COLUMN_VALUE with "," ");
zvmessage("\titem number argument out of range."," ");
zvmessage("\tExpect a -4 status flag value returned."," ");
zvmessage(" "," ");

value = gpds_get_column_value(	label_ptr, table_file_name, 1, keywords[8], 
		item_number[8], data_types[0], &lengths[0], &status ); 

if ( status < 0 )
	{
	zvmessage("\t*** Error in GPDS_GET_COLUMN_VALUE call"," ");
	sprintf(string,"\t*** Status flag value is %d",status);
	zvmessage(string," ");
	zvmessage(" "," ");
	}

zvmessage(" "," ");
zvmessage("\t************************************************"," ");
zvmessage(" "," ");
zvmessage("\tMake call to GPDS_GET_COLUMN_VALUE with "," ");
zvmessage("\trecord number argument out of range."," ");
zvmessage("\tExpect a -3 status flag value returned."," ");
zvmessage(" "," ");

value = gpds_get_column_value(	label_ptr, table_file_name, -2, keywords[0], 
		item_number[0], data_types[0], &lengths[0], &status ); 

if ( status < 0 )
	{
	zvmessage("\t*** Error in GPDS_GET_COLUMN_VALUE call"," ");
	sprintf(string,"\t*** Status flag value is %d",status);
	zvmessage(string," ");
	zvmessage(" "," ");
	}

zvmessage(" "," ");
zvmessage("\t************************************************"," ");
zvmessage(" "," ");
zvmessage("\tMake call to GPDS_GET_COLUMN_VALUE with "," ");
zvmessage("\tinvalid table file."," ");
zvmessage("\tExpect a -2 status flag value returned."," ");
zvmessage(" "," ");

value = gpds_get_column_value(	label_ptr, "NONEXISTENT.TAB", 1, keywords[0], 
		item_number[0], data_types[0], &lengths[0], &status ); 

if ( status < 0 )
	{
	zvmessage("\t*** Error in GPDS_GET_COLUMN_VALUE call"," ");
	sprintf(string,"\t*** Status flag value is %d",status);
	zvmessage(string," ");
	zvmessage(" "," ");
	}

zvmessage(" "," ");
zvmessage("\t************************************************"," ");
zvmessage(" "," ");
zvmessage("\tMake call to GPDS_GET_COLUMN_VALUE with "," ");
zvmessage("\tinvalid keyword name."," ");
zvmessage("\tExpect a -1 status flag value returned."," ");
zvmessage(" "," ");

value = gpds_get_column_value(	label_ptr, table_file_name, 1, keywords[9], 
		item_number[9], data_types[9], &lengths[9], &status ); 

if ( status < 0 )
	{
	zvmessage("\t*** Error in GPDS_GET_COLUMN_VALUE call"," ");
	sprintf(string,"\t*** Status flag value is %d",status);
	zvmessage(string," ");
	zvmessage(" "," ");
	}

zvmessage("\t************************************************"," ");
zvmessage(" "," ");

zvmessage("\tClose label file"," ");
zvmessage(" "," ");

gpds_close_file(label_ptr);

zvmessage("\tSuccessful GPDS_CLOSE_FILE call"," ");
zvmessage(" "," ");
zvmessage("************************************************"," ");
zvmessage(" "," ");
zvmessage("\tEnd of ACCEPTANCE and UNIT test"," ");
zvmessage(" "," ");
zvmessage("************************************************"," ");
}
