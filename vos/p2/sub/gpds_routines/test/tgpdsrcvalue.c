#include   <stdio.h>
#include   "vicmain_c"
#include   "gpdsroutines.h"
#include   "mo_include.h"

#define TEST_ARRAY_LENGTH	10

/*

GPDS_REPLACE_COLUMN_VALUE 	acceptance and unit test

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
zvmessage("Acceptance test for GPDS_REPLACE_COLUMN_VALUE"," ");
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
zvmessage(" C Test of gpds_replace_column_value"," ");
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


strcpy(values[0],"/mo1/test/images/03r00231.img");
strcpy(values[1],"512");
strcpy(values[2],"512");
strcpy(values[3],"1");
strcpy(values[4],"12345676");
strcpy(values[5],"frame_id");
strcpy(values[6],"tile_id");
strcpy(values[7],"mast_type");
strcpy(values[8],"1000");
strcpy(values[9],"MASKTYPE");		/* Test for invalid keyword name   */

/*

Print instructive messages to tester.

*/

zvmessage("\tThe following string values are loaded in the"," ");
zvmessage("\tGPDS_REPLACE_COLUMN_VALUE to replace the values"," ");
zvmessage("\tin consecutive columns of the test table in"," ");
zvmessage("\trecords 1 and 5.\n"," ");

for ( i=0; i<TEST_ARRAY_LENGTH-2; i++ )
	{
	j = i + 1;
	sprintf(string,"\t The value \'%s\' in column %d.",values[i],j);
	zvmessage(string," ");
	}

zvmessage("\n\tThe values for columns 5 and 8 should be truncated."," ");
zvmessage("\tCompare table contents after this test program is"," ");
zvmessage("\trun to the table contents prior to the test program"," ");
zvmessage("\texecution. Only records 1 and 5 should be altered.\n"," ");

/*

For records 1 and 5 in the table file, 
replace all values for valid arguments passed.

*/

for ( i=1; i<total_records; i+=4 )
	{
	zvmessage(" "," ");
	sprintf(string,"\t*** RECORD %d OF FILE %s WRITTEN ***",
		i,table_file_name);
	zvmessage(string," ");

	for ( j=0; j<TEST_ARRAY_LENGTH-2; j++ )
		{
		gpds_replace_column_value( label_ptr, table_file_name, i,
			keywords[j], item_number[j], values[j], &status ); 

		if ( status < 0 )
		   {
		   zvmessage("*** Error in GPDS_REPLACE_COLUMN_VALUE call"," ");
	 	   sprintf(string,"*** Status flag value is %d",status);
		   zvmessage(string," ");
		   zvmessage(" "," ");
		   }
		}
	}

zvmessage("\n\tSuccessful GPDS_REPLACE_COLUMN_VALUE calls"," ");
zvmessage(" "," ");
zvmessage("\t************************************************"," ");
zvmessage(" "," ");
zvmessage("\tMake call to GPDS_REPLACE_COLUMN_VALUE with "," ");
zvmessage("\timproper formatted value argument (characters"," ");
zvmessage("\tpassed were only integers are expected.)"," ");
sprintf(string,"\n\t\tKEYWORD \'%s\' to be filled with \'%s\'.",
	keywords[4],values[6]);
zvmessage(string," ");
zvmessage("\n\tExpect a -5 status flag value returned."," ");
zvmessage(" "," ");

gpds_replace_column_value( label_ptr, table_file_name, 1, keywords[4], 
			item_number[6], values[6], &status ); 
if ( status < 0 )
	{
	zvmessage("\t*** Error in GPDS_REPLACE_COLUMN_VALUE call"," ");
	sprintf(string,"\t*** Status flag value is %d",status);
	zvmessage(string," ");
	zvmessage(" "," ");
	}

zvmessage(" "," ");
zvmessage("\t************************************************"," ");
zvmessage(" "," ");
zvmessage("\tMake call to GPDS_REPLACE_COLUMN_VALUE with "," ");
zvmessage("\titem number argument out of range."," ");
zvmessage("\tExpect a -4 status flag value returned."," ");
zvmessage(" "," ");

gpds_replace_column_value( label_ptr, table_file_name, 1, keywords[8], 
			item_number[8], values[8], &status ); 
if ( status < 0 )
	{
	zvmessage("\t*** Error in GPDS_REPLACE_COLUMN_VALUE call"," ");
	sprintf(string,"\t*** Status flag value is %d",status);
	zvmessage(string," ");
	zvmessage(" "," ");
	}

zvmessage(" "," ");
zvmessage("\t************************************************"," ");
zvmessage(" "," ");
zvmessage("\tMake call to GPDS_REPLACE_COLUMN_VALUE with "," ");
zvmessage("\trecord number argument out of range."," ");
zvmessage("\tExpect a -3 status flag value returned."," ");
zvmessage(" "," ");

gpds_replace_column_value( label_ptr, table_file_name, -2, keywords[0], 
		item_number[0], values[0], &status ); 
if ( status < 0 )
	{
	zvmessage("\t*** Error in GPDS_REPLACE_COLUMN_VALUE call"," ");
	sprintf(string,"\t*** Status flag value is %d",status);
	zvmessage(string," ");
	zvmessage(" "," ");
	}

zvmessage(" "," ");
zvmessage("\t************************************************"," ");
zvmessage(" "," ");
zvmessage("\tMake call to GPDS_REPLACE_COLUMN_VALUE with "," ");
zvmessage("\tinvalid table file."," ");
zvmessage("\tExpect a -2 status flag value returned."," ");
zvmessage(" "," ");

gpds_replace_column_value( label_ptr, "NONEXISTENT.TAB", 1, keywords[0], 
		item_number[0], values[0], &status ); 
if ( status < 0 )
	{
	zvmessage("\t*** Error in GPDS_REPLACE_COLUMN_VALUE call"," ");
	sprintf(string,"\t*** Status flag value is %d",status);
	zvmessage(string," ");
	zvmessage(" "," ");
	}

zvmessage(" "," ");
zvmessage("\t************************************************"," ");
zvmessage(" "," ");
zvmessage("\tMake call to GPDS_REPLACE_COLUMN_VALUE with "," ");
zvmessage("\tinvalid keyword name."," ");
zvmessage("\tExpect a -1 status flag value returned."," ");
zvmessage(" "," ");

gpds_replace_column_value( label_ptr, table_file_name, 1, keywords[9], 
		item_number[9], values[9], &status ); 
if ( status < 0 )
	{
	zvmessage("\t*** Error in GPDS_REPLACE_COLUMN_VALUE call"," ");
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
