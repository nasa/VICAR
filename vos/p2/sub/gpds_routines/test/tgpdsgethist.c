#include   <stdio.h>
#include   "vicmain_c"
#include   "gpdsroutines.h"
#include   "mo_include.h"

#define  NUMBER_OF_INVALID_FILES 4

/*

GPDS_GET_HISTOGRAM	acceptance and unit test

Author:	Justin McNeill
Date:	August 1993

*/

main44()
{
AGGREGATE	label_ptr;

int		i,j,k;
int		count;
int		elements,
		element_size;
int		label_status;
int		status;

unsigned int 	*histogram;

char		input_file_name[80];
char		invalid_files[4][80];
char		object_name[40];
char		string[120];

/*

Initialize input argument

*/

strcpy(object_name,"RAW_IMAGE_HISTOGRAM");

/*

Get value of user parameter INP

*/

status = zvp("INP",input_file_name,&count);
if ( status != 1 ) 
	{
	zvmessage("*** Error in ZVP call for INP"," ");
	zabend();
	}


status = zvp("INVALID1",invalid_files[0],&count);
if ( status != 1 ) 
	{
	zvmessage("*** Error in ZVP call for INVALID1"," ");
	zabend();
	}


status = zvp("INVALID2",invalid_files[1],&count);
if ( status != 1 ) 
	{
	zvmessage("*** Error in ZVP call for INVALID2"," ");
	zabend();
	}


status = zvp("INVALID3",invalid_files[2],&count);
if ( status != 1 ) 
	{
	zvmessage("*** Error in ZVP call for INVALID3"," ");
	zabend();
	}

status = zvp("INVALID4",invalid_files[3],&count);
if ( status != 1 ) 
	{
	zvmessage("*** Error in ZVP call for INVALID4"," ");
	zabend();
	}

zvmessage(" "," ");
zvmessage("************************************************"," ");
zvmessage(" "," ");
zvmessage(" C Test of gpds_get_histogram"," ");
zvmessage(" "," ");
zvmessage("************************************************"," ");
zvmessage(" "," ");

/*

Open input file

*/

label_ptr = gpds_open_file(input_file_name,&status);
if ( status < 0 )
	{
	zvmessage("\t*** Error in GPDS_OPEN_FILE call\n"," ");
	zabend();
	}
else
	zvmessage("\tSuccessful GPDS_OPEN_FILE call\n"," ");

/*

Retrieve histogram object from input file
and print histogram values in listing if successful.

*/

histogram = gpds_get_histogram(label_ptr, input_file_name, object_name,
		&elements, &element_size, &status );
if ( status < 0 )
	zvmessage("\t*** Error in GPDS_GET_HISTOGRAM call\n"," ");
else
	{
	zvmessage("\tSuccessful GPDS_GET_HISTOGRAM call\n"," ");
	sprintf(string,"\tHISTOGRAM LISTING from file %s\n",input_file_name);
	zvmessage(string," ");
	sprintf(string,"\t\tElements in histogram = %d",elements);
	zvmessage(string," ");
	sprintf(string,"\t\tElements size = %d\n",element_size);
	zvmessage(string," ");
	zvmessage("\tOnly non-zero histogram values are listed.\n"," ");
	
	for ( i=0; i<elements; i++ )
	    if ( histogram[i] != 0 )
		{
		sprintf(string,"\t\tHISTOGRAM[%d] = %d",i,histogram[i]);
		zvmessage(string," ");
		}

	}

/*

Close input file

*/

gpds_close_file( label_ptr );

zvmessage("\n\tSuccessful GPDS_CLOSE_FILE call\n"," ");

zvmessage(" "," ");
zvmessage("************************************************"," ");
zvmessage(" "," ");
zvmessage(" Test of gpds_get_histogram with invalid input"," ");
zvmessage(" file name or invalid object name."," ");
zvmessage(" "," ");
zvmessage("************************************************"," ");
zvmessage(" "," ");

/*

Open invalid input file.

*/

label_ptr = gpds_open_file(input_file_name,&status);
if ( status < 0 )
	{
	zvmessage("\t*** Error in GPDS_OPEN_FILE call\n"," ");
	zabend();
	}
else
	{
	zvmessage("\tSuccessful GPDS_OPEN_FILE call"," ");
	sprintf(string,"\tfor file %s\n",input_file_name);
	zvmessage(string," ");
	}

zvmessage("\n\tInput file name loaded into GPDS_GET_HISTOGRAM"," ");
zvmessage("\tis invalid; expect error with status flag of -2."," ");

histogram = gpds_get_histogram(label_ptr, "NONEXISTENT.FILE", object_name,
		&elements, &element_size, &status );
if ( status < 0 )
	{
	zvmessage("\n\t*** Error in GPDS_GET_HISTOGRAM call"," ");
	sprintf(string,"\t\tstatus flag is %d.\n",status);
	zvmessage(string," ");
	}

zvmessage("\n\tObject name loaded into GPDS_GET_HISTOGRAM"," ");
zvmessage("\tis invalid; expect error with status flag of -1."," ");

histogram = gpds_get_histogram(label_ptr, input_file_name, "INVALID_OBJECT",
		&elements, &element_size, &status );
if ( status < 0 )
	{
	zvmessage("\n\t*** Error in GPDS_GET_HISTOGRAM call"," ");
	sprintf(string,"\t\tstatus flag is %d.\n",status);
	zvmessage(string," ");
	}

/*

Close input file

*/

gpds_close_file( label_ptr );

zvmessage("\n\tSuccessful GPDS_CLOSE_FILE call\n"," ");

zvmessage(" "," ");
zvmessage("************************************************"," ");
zvmessage(" "," ");
zvmessage(" Test of gpds_get_histogram with invalid"," ");
zvmessage(" histogram object keyword values and invalid"," ");
zvmessage(" record type keyword."," ");
zvmessage(" "," ");
zvmessage("************************************************"," ");
zvmessage(" "," ");

/*

Open first invalid input file.

*/

label_ptr = gpds_open_file(invalid_files[0],&status);
if ( status < 0 )
	{
	zvmessage("\t*** Error in GPDS_OPEN_FILE call\n"," ");
	zabend();
	}
else
	{
	zvmessage(" "," ");
	zvmessage("\t******************************************\n"," ");
	zvmessage("\tSuccessful GPDS_OPEN_FILE call"," ");
	sprintf(string,"\tfor file %s\n",invalid_files[0]);
	zvmessage(string," ");
	}

zvmessage("\n\tInput file name loaded into GPDS_GET_HISTOGRAM"," ");
zvmessage("\thas invalid DATA_TYPE (not of type unsigned integer)."," ");
zvmessage("\tExpect an error with status flag of -3."," ");

histogram = gpds_get_histogram(label_ptr, invalid_files[0], object_name,
		&elements, &element_size, &status );
if ( status < 0 )
	{
	zvmessage("\n\t*** Error in GPDS_GET_HISTOGRAM call"," ");
	sprintf(string,"\t\tstatus flag is %d.\n",status);
	zvmessage(string," ");
	}

/*

Close file

*/

gpds_close_file( label_ptr );


/*

Open second invalid input file.

*/

label_ptr = gpds_open_file(invalid_files[1],&status);
if ( status < 0 )
	{
	zvmessage("\t*** Error in GPDS_OPEN_FILE call\n"," ");
	zabend();
	}
else
	{
	zvmessage(" "," ");
	zvmessage("\t******************************************\n"," ");
	zvmessage("\tSuccessful GPDS_OPEN_FILE call"," ");
	sprintf(string,"\tfor file %s\n",invalid_files[1]);
	zvmessage(string," ");
	}


zvmessage("\n\tInput file name loaded into GPDS_GET_HISTOGRAM"," ");
zvmessage("\thas invalid ITEM_BYTES (not equal to the value 4)."," ");
zvmessage("\tExpect an error with status flag of -4."," ");

histogram = gpds_get_histogram(label_ptr, invalid_files[1], object_name,
		&elements, &element_size, &status );
if ( status < 0 )
	{
	zvmessage("\n\t*** Error in GPDS_GET_HISTOGRAM call"," ");
	sprintf(string,"\t\tstatus flag is %d.\n",status);
	zvmessage(string," ");
	}

/*

Close file.

*/

gpds_close_file( label_ptr );

zvmessage("\n\tSuccessful GPDS_CLOSE_FILE call\n"," ");


/*

Open third invalid input file.

*/

label_ptr = gpds_open_file(invalid_files[2],&status);
if ( status < 0 )
	{
	zvmessage("\t*** Error in GPDS_OPEN_FILE call"," ");
	zabend();
	}
else
	{
	zvmessage(" "," ");
	zvmessage("\t******************************************\n"," ");
	zvmessage("\tSuccessful GPDS_OPEN_FILE call"," ");
	sprintf(string,"\tfor file %s\n",invalid_files[2]);
	zvmessage(string," ");
	}

zvmessage("\n\tInput file name loaded into GPDS_GET_HISTOGRAM"," ");
zvmessage("\thas invalid ITEMS (less than 0 or greater than 65536)."," ");
zvmessage("\tExpect an error with status flag of -5."," ");

histogram = gpds_get_histogram(label_ptr, invalid_files[2], object_name,
		&elements, &element_size, &status );
if ( status < 0 )
	{
	zvmessage("\n\t*** Error in GPDS_GET_HISTOGRAM call"," ");
	sprintf(string,"\t\tstatus flag is %d.\n",status);
	zvmessage(string," ");
	}

/*

Close file.

*/

gpds_close_file( label_ptr );

zvmessage("\n\tSuccessful GPDS_CLOSE_FILE call\n"," ");


/*

Open fourth invalid input file.

*/

label_ptr = gpds_open_file(invalid_files[3],&status);
if ( status < 0 )
	{
	zvmessage("\t*** Error in GPDS_OPEN_FILE call"," ");
	zabend();
	}
else
	{
	zvmessage(" "," ");
	zvmessage("\t******************************************\n"," ");
	zvmessage("\tSuccessful GPDS_OPEN_FILE call"," ");
	sprintf(string,"\tfor file %s\n",invalid_files[3]);
	zvmessage(string," ");
	}

zvmessage("\n\tInput file name loaded into GPDS_GET_HISTOGRAM"," ");
zvmessage("\thas invalid RECORD_TYPE (not FIXED_LENGTH)."," ");
zvmessage("\tExpect an error with status flag of -6."," ");

histogram = gpds_get_histogram(label_ptr, invalid_files[3], object_name,
		&elements, &element_size, &status );
if ( status < 0 )
	{
	zvmessage("\n\t*** Error in GPDS_GET_HISTOGRAM call"," ");
	sprintf(string,"\t\tstatus flag is %d.\n",status);
	zvmessage(string," ");
	}

/*

Close file.

*/

gpds_close_file( label_ptr );

zvmessage("\n\tSuccessful GPDS_CLOSE_FILE call\n"," ");

zvmessage(" "," ");
zvmessage("**********************************************"," ");
zvmessage(" "," ");
zvmessage("\tEnd of ACCEPTANCE and UNIT TEST"," ");
zvmessage(" "," ");
zvmessage("**********************************************"," ");
}
