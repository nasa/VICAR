#include   <stdio.h>
#include   "vicmain_c"
#include   "gpdsroutines.h"
#include   "mo_include.h"

#define  TEST_ARRAY_LENGTH	20

/*

MO_TABLE_READ acceptance and unit test

Author:	Justin McNeill
Date:	August 1993

*/

main44()
{
AGGREGATE	label_ptr;

int		i,j,k;
int		status;
int		label_status;
int		count;
int 		total_records;
int		lengths[10];
int		item_number[10];
int		row_number;

char		string[120];
char		invalid_file_name[80];
char		label_file_name[80], table_file_name[80];
char		*value;
char		keywords[TEST_ARRAY_LENGTH][40], 
		object[TEST_ARRAY_LENGTH][20],
		values[TEST_ARRAY_LENGTH][40];

struct	PRODUCTION_TABLE_ENTRY	*table_ptr;

/*

Get production table label and table file names.

*/

status = zvp("P_LABEL",label_file_name,&count);
if ( status != 1 ) 
	{
	zvmessage("*** Error in ZVP call for P_LABEL"," ");
	zabend();
	}


status = zvp("P_TABLE",table_file_name,&count);
if ( status != 1 ) 
	{
	zvmessage("*** Error in ZVP call for P_TABLE"," ");
	zabend();
	}


status = zvp("INVALID",invalid_file_name,&count);
if ( status != 1 ) 
	{
	zvmessage("*** Error in ZVP call for INVALID"," ");
	zabend();
	}

/*

Get production table total number of records.

*/

label_ptr = gpds_open_file(label_file_name, &status);
if ( status < 0 )
	{
	zvmessage("*** Error in GPDS_OPEN_FILE call"," ");
	zabend();
	}

value = gpds_get_label_value(label_ptr, "ROOT", moFILE_RECORDS, 1, &status);
if ( status < 0 )
	{
	zvmessage("*** Error in GPDS_GET_LABEL_VALUE call"," ");
	zabend();
	}
total_records = atoi( value );

gpds_close_file(label_ptr);


zvmessage(" "," ");
zvmessage("********************************************************"," ");
zvmessage(" "," ");
zvmessage(" C Test of mo_table_read"," ");
zvmessage(" "," ");
zvmessage("********************************************************"," ");
zvmessage(" "," ");

/*

Read consecutive rows from production table.

*/

table_ptr = (struct PRODUCTION_TABLE_ENTRY *)malloc
		(sizeof(struct PRODUCTION_TABLE_ENTRY));

for ( i=0; i<total_records; i++ )
	{
	row_number = i + 1;

	mo_table_read(label_file_name,table_file_name,table_ptr,
		&row_number, 0, 0, 0, &status);
	if ( status < 0 ) 
		{
		zvmessage("*** Error in MO_TABLE_READ call"," ");
		sprintf(string,"\tstatus flag is %d",status);
		zvmessage(string," ");
		zabend();
		}

	zvmessage(" "," ");
	zvmessage("********************************************************",
		" ");
	sprintf(string,"\nRecord %d of \'%s\':\n",
		row_number,table_file_name);
	zvmessage(string," ");
	sprintf(string,"\tFILE_SPECIFICATION_NAME is \'%s\'",
		table_ptr->file_specification_name);
	zvmessage(string," ");
	sprintf(string,"\t(LINES,LINES_SAMPLES) = (%d,%d)",
		table_ptr->lines,table_ptr->lines_samples);
	zvmessage(string," ");
	sprintf(string,"\t(START_LINE,START_SAMPLE) = (%d,%d)",
		table_ptr->start_line,table_ptr->start_sample);
	zvmessage(string," ");
	sprintf(string,"\tFRAME_ID is \'%s\' and TILE_ID is \'%s\'",
		table_ptr->frame_id,table_ptr->tile_id);
	zvmessage(string," ");
	sprintf(string,"\tMASK_TYPE is \'%s\'",table_ptr->mask_type);
	zvmessage(string," ");

	}

/*

Search table for first row with 'MASK_TYPE' of 'GENERAL'.

*/

row_number = 1;

mo_table_read(label_file_name,table_file_name,table_ptr,
	&row_number, "MASK_TYPE", "GENERAL", 1, &status);
if ( status < 0 ) 
	{
	zvmessage("*** Error in MO_TABLE_READ call"," ");
	sprintf(string,"\tstatus flag is %d",status);
	zvmessage(string," ");
	zabend();
	}
else	
	{
	zvmessage(" "," ");
	zvmessage("********************************************************",
		" ");
	sprintf(string,"\nRecord %d of \'%s\':\n",
		row_number,table_file_name);
	zvmessage(string," ");
	sprintf(string,"\tFILE_SPECIFICATION_NAME is \'%s\'",
		table_ptr->file_specification_name);
	zvmessage(string," ");
	sprintf(string,"\t(LINES,LINES_SAMPLES) = (%d,%d)",
		table_ptr->lines,table_ptr->lines_samples);
	zvmessage(string," ");
	sprintf(string,"\t(START_LINE,START_SAMPLE) = (%d,%d)",
		table_ptr->start_line,table_ptr->start_sample);
	zvmessage(string," ");
	sprintf(string,"\tFRAME_ID is \'%s\' and TILE_ID is \'%s\'",
		table_ptr->frame_id,table_ptr->tile_id);
	zvmessage(string," ");
	sprintf(string,"\tMASK_TYPE is \'%s\'",table_ptr->mask_type);
	zvmessage(string," ");
	}
/*

Search table for first row with 'TILE_ID' of '02 of 02'.

*/

row_number = 1;

mo_table_read(label_file_name,table_file_name,table_ptr,
	&row_number, "TILE_ID", "02 OF 02", 1, &status);
if ( status < 0 ) 
	{
	zvmessage("*** Error in MO_TABLE_READ call"," ");
	sprintf(string,"\tstatus flag is %d",status);
	zvmessage(string," ");
	zabend();
	}
else	
	{
	zvmessage(" "," ");
	zvmessage("********************************************************",
		" ");
	sprintf(string,"\nRecord %d of \'%s\'\n",
		row_number,table_file_name);
	zvmessage(string," ");
	sprintf(string,"\tFILE_SPECIFICATION_NAME is \'%s\'",
		table_ptr->file_specification_name);
	zvmessage(string," ");
	sprintf(string,"\t(LINES,LINES_SAMPLES) = (%d,%d)",
		table_ptr->lines,table_ptr->lines_samples);
	zvmessage(string," ");
	sprintf(string,"\t(START_LINE,START_SAMPLE) = (%d,%d)",
		table_ptr->start_line,table_ptr->start_sample);
	zvmessage(string," ");
	sprintf(string,"\tFRAME_ID is \'%s\' and TILE_ID is \'%s\'",
		table_ptr->frame_id,table_ptr->tile_id);
	zvmessage(string," ");
	sprintf(string,"\tMASK_TYPE is \'%s\'",table_ptr->mask_type);
	zvmessage(string," ");
	}

/*

Search table for next row with 'TILE_ID' of '02 of 02' and
test for case insensitivity.

*/

row_number++;

mo_table_read(label_file_name,table_file_name,table_ptr,
	&row_number, "TILE_iD", "02 oF 02", 1, &status);
if ( status < 0 ) 
	{
	zvmessage("*** Error in MO_TABLE_READ call"," ");
	sprintf(string,"\tstatus flag is %d",status);
	zvmessage(string," ");
	zabend();
	}
else	
	{
	zvmessage(" "," ");
	zvmessage("********************************************************",
		" ");
	sprintf(string,"\nRecord %d of \'%s\'\n",
		row_number,table_file_name);
	zvmessage(string," ");
	sprintf(string,"\tFILE_SPECIFICATION_NAME is \'%s\'",
		table_ptr->file_specification_name);
	zvmessage(string," ");
	sprintf(string,"\t(LINES,LINES_SAMPLES) = (%d,%d)",
		table_ptr->lines,table_ptr->lines_samples);
	zvmessage(string," ");
	sprintf(string,"\t(START_LINE,START_SAMPLE) = (%d,%d)",
		table_ptr->start_line,table_ptr->start_sample);
	zvmessage(string," ");
	sprintf(string,"\tFRAME_ID is \'%s\' and TILE_ID is \'%s\'",
		table_ptr->frame_id,table_ptr->tile_id);
	zvmessage(string," ");
	sprintf(string,"\tMASK_TYPE is \'%s\'",table_ptr->mask_type);
	zvmessage(string," ");
	}

zvmessage(" "," ");
zvmessage(" "," ");
zvmessage("********************************************************"," ");
zvmessage(" "," ");
zvmessage("\tTest of error handling of MO table read function"," ");
zvmessage(" "," ");
zvmessage("********************************************************"," ");
zvmessage(" "," ");
zvmessage("\tMake call to table read function with "," ");
zvmessage("\ta nonexistent label file. Expect -1 status flag"," ");
zvmessage("\tindicating an error in opening the file."," ");
zvmessage(" "," ");

row_number = 1;

mo_table_read("NONEXISTENT.LBL",table_file_name,table_ptr,
	&row_number, "LINES", "002050", 1, &status);
if ( status < 0 ) 
	{
	zvmessage("\t*** Error in MO table read function."," ");
	sprintf(string,"\t\tstatus flag is %d",status);
	zvmessage(string," ");
	}

zvmessage("\n********************************************************"," ");
zvmessage(" "," ");
zvmessage("\tMake call to table read function with "," ");
zvmessage("\titem number argument out of range."," ");
zvmessage("\tExpect succesful read because there are"," ");
zvmessage("\tno keywords with multiple values."," ");
zvmessage(" "," ");

row_number = 1;

mo_table_read(label_file_name,table_file_name,table_ptr,
	&row_number, "LINES", "002050", -2, &status);
if ( status < 0 ) 
	{
	zvmessage("\t*** Error in MO table read function."," ");
	sprintf(string,"\t\tstatus flag is %d",status);
	zvmessage(string," ");
	}
else
	zvmessage("\tSuccessful MO table read "," ");

zvmessage("\n********************************************************"," ");
zvmessage(" "," ");
zvmessage("\tMake call to table read function with "," ");
zvmessage("\tinput of invalid record type."," ");
zvmessage("\tExpect a -3 status flag value returned."," ");
zvmessage(" "," ");

row_number = 1;

mo_table_read(invalid_file_name,table_file_name,table_ptr,
	&row_number, "LINES", "002050", 1, &status);
if ( status < 0 ) 
	{
	zvmessage("\t*** Error in MO table read function."," ");
	sprintf(string,"\t\tstatus flag is %d",status);
	zvmessage(string," ");
	}

zvmessage("\n********************************************************"," ");
zvmessage(" "," ");
zvmessage("\tMake call to table read function with "," ");
zvmessage("\trow_number argument that is negative."," ");
zvmessage("\tExpect a -4 status flag value returned"," ");
zvmessage("\tindicating failure in search (EOF reached)."," ");
zvmessage(" "," ");

row_number = -1;

mo_table_read(label_file_name,table_file_name,table_ptr,
	&row_number, "LINES", "002050", 1, &status);
if ( status < 0 ) 
	{
	zvmessage("\t*** Error in MO table read function."," ");
	sprintf(string,"\t\tstatus flag is %d",status);
	zvmessage(string," ");
	}

zvmessage("\n********************************************************"," ");
zvmessage(" "," ");
zvmessage("\tMake call to table read function with "," ");
zvmessage("\tkey_value argument that is non-existent.."," ");
zvmessage("\tExpect a -5 status flag value returned"," ");
zvmessage("\tindicating failure in search (EOF reached)."," ");
zvmessage(" "," ");

row_number = 1;

mo_table_read(label_file_name,table_file_name,table_ptr,
	&row_number, "LINES", "002340", 1, &status);
if ( status < 0 ) 
	{
	zvmessage("\t*** Error in MO table read function."," ");
	sprintf(string,"\t\tstatus flag is %d",status);
	zvmessage(string," ");
	}

zvmessage("\n***************************************************\n"," ");
zvmessage("\tEnd of ACCEPTANCE and UNIT test"," ");
zvmessage("\n***************************************************\n"," ");
}
