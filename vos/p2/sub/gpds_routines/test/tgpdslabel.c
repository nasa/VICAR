#include   <stdio.h>
#include   "vicmain_c"
#include   "gpdsroutines.h"
#include   "mo_include.h"

#define  TEST_ARRAY_LENGTH	20

/*

GPDS_GET_LABEL_VALUE	acceptance and unit test

Author:	Justin McNeill
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
char		input_file_name[80], table_file_name[80];

AGGREGATE	label_ptr;

char		*value;
int		item_number[10];
char		keywords[TEST_ARRAY_LENGTH][40], 
		object[TEST_ARRAY_LENGTH][20],
		values[TEST_ARRAY_LENGTH][40];

status = zvp("INP",input_file_name,&count);
if ( status != 1 ) 
	{
	zvmessage("*** Error in ZVP call for INP"," ");
	zabend();
	}

zvmessage(" "," ");
zvmessage("************************************************"," ");
zvmessage(" "," ");
zvmessage(" C Test of gpds_open_file"," ");
zvmessage(" "," ");
zvmessage("************************************************"," ");
zvmessage(" "," ");

label_ptr = gpds_open_file(input_file_name,&status);
if ( status < 0 )
	{
	zvmessage("*** Error in GPDS_OPEN_FILE call"," ");
	zabend();
	}
else
	zvmessage("Successful GPDS_OPEN_FILE call"," ");

zvmessage(" "," ");
zvmessage("************************************************"," ");
zvmessage(" "," ");
zvmessage(" C Test of gpds_get_label_value"," ");
zvmessage(" "," ");
zvmessage("************************************************"," ");
zvmessage(" "," ");

/* Obtain the following keywords from the PDS label 			*/

for( i=0; i<TEST_ARRAY_LENGTH; i++ )
	strcpy(object[i],"ROOT");
strcpy(object[8],"IMAGE");
strcpy(object[9],"IMAGE");
	
strcpy(keywords[0],moPDS_VERSION_ID);
strcpy(keywords[1],moRECORD_TYPE);
strcpy(keywords[2],moRECORD_BYTES);
strcpy(keywords[3],moFILE_RECORDS);
strcpy(keywords[4],moSPACECRAFT_NAME);
strcpy(keywords[5],moLABEL_RECORDS);
strcpy(keywords[6],moMISSION_PHASE_NAME);
strcpy(keywords[7],moTARGET_NAME);
strcpy(keywords[8],moLINES);
strcpy(keywords[9],moLINES_SAMPLES);
strcpy(keywords[10],moPRODUCER_ID);
strcpy(keywords[11],moPRODUCT_CREATION_TIME);
strcpy(keywords[12],moSOFTWARE_NAME);
strcpy(keywords[13],moUPLOAD_ID);
strcpy(keywords[14],moDATA_SET_ID);
strcpy(keywords[15],moVOLUME_ID);
strcpy(keywords[16],moVOLUME_VERSION_ID);
strcpy(keywords[17],moPRODUCT_ID);
strcpy(keywords[18],moASSOCIATED_PRODUCT_ID);
strcpy(keywords[19],"INVALID_KEYWORD");

for( i=0; i<TEST_ARRAY_LENGTH; i++ )
	{
	value = gpds_get_label_value(label_ptr,object[i],keywords[i],1,&status);
	strcpy(values[i],value);
	if ( status < 0 )
		{
		zvmessage("*** Error in GPDS_GET_LABEL_VALUE call"," ");
		sprintf(string,"*** Keyword \'%s\' has not been found.",
			keywords[i]);
		zvmessage(string," ");
		zvmessage(" "," ");
		}
	else
		{
		sprintf(string,"KEYWORD %s = %s",keywords[i],values[i]);
		zvmessage(string," ");
		zvmessage(" "," ");
		}
	}

value = gpds_get_label_value(label_ptr,object[18],keywords[18],2,&status);
strcpy(values[i],value);
if ( status < 0 )
	{
	zvmessage("*** Error in GPDS_GET_LABEL_VALUE call"," ");
	sprintf(string,"*** Keyword \'%s\' has not been found."," ");
	zvmessage(string," ");
	zvmessage(" "," ");
	}
sprintf(string,"KEYWORD %s (item 2) = %s",keywords[i],values[i]);
zvmessage(string," ");
zvmessage(" "," ");

zvmessage("Successful GPDS_GET_LABEL_VALUE calls"," ");

zvmessage(" "," ");
zvmessage("************************************************"," ");
zvmessage(" "," ");
zvmessage(" C Test of gpds_close_file"," ");
zvmessage(" "," ");
zvmessage("************************************************"," ");

gpds_close_file(label_ptr);

zvmessage(" "," ");
zvmessage("Successful GPDS_CLOSE_FILE call"," ");
zvmessage(" "," ");
}
