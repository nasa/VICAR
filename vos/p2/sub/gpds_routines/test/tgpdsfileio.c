#include   <stdio.h>
#include   "vicmain_c"
#include   "gpdsroutines.h"
#include   "mo_include.h"

/*

GPDS_OPEN_FILE and GPDS_CLOSE_FILE	acceptance and unit test

Author:	Justin McNeill
Date:	August 1993

*/

main44()
{
int		i,j,k;
int		count;
int		status;
int		label_status;

char		string[120];
char		input_file_name[80];
char		invalid_file[80];

AGGREGATE	label_ptr;

status = zvp("INP",input_file_name,&count);
if ( status != 1 ) 
	{
	zvmessage("*** Error in ZVP call for INP"," ");
	zabend();
	}

status = zvp("INVALID",invalid_file,&count);
if ( status != 1 ) 
	{
	zvmessage("*** Error in ZVP call for INVALID"," ");
	zabend();
	}

zvmessage(" "," ");
zvmessage("************************************************"," ");
zvmessage(" "," ");
zvmessage(" C Test of gpds_open_file and gpds_close_file"," ");
zvmessage(" "," ");
zvmessage("************************************************"," ");
zvmessage(" "," ");

/*

Test of opening and reopening of existent file.
Success is expected.

*/

sprintf(string,"\tTest opening existent file, \'%s\'",input_file_name);
zvmessage(string," ");
zvmessage("\tExpect message of successful GPDS_OPEN_FILE call.\n"," ");

label_ptr = gpds_open_file(input_file_name,&status);
if ( status < 0 )
	zvmessage("\t*** Error in GPDS_OPEN_FILE call\n"," ");
else
	zvmessage("\tSuccessful GPDS_OPEN_FILE call\n"," ");

zvmessage("\t**********************************************\n"," ");
sprintf(string,"\tTest reopening existent file, \'%s\'",input_file_name);
zvmessage(string," ");
zvmessage("\tExpect message of successful GPDS_OPEN_FILE call.\n"," ");

label_ptr = gpds_open_file(input_file_name,&status);
if ( status < 0 )
	zvmessage("\t*** Error in GPDS_OPEN_FILE call\n"," ");
else
	zvmessage("\tSuccessful GPDS_OPEN_FILE call\n"," ");

/*

Test of closing existent file.
Success is expected.

*/

zvmessage("\t**********************************************\n"," ");
sprintf(string,"\tTest closing existent file, \'%s\'",input_file_name);
zvmessage(string," ");
zvmessage("\tExpect message of successful GPDS_CLOSE_FILE call.\n"," ");

gpds_close_file(label_ptr);
zvmessage("\tSuccessful GPDS_CLOSE_FILE call\n"," ");

/*

Test of opening of invalid file.
Failure is expected.

*/

zvmessage("\t**********************************************\n"," ");
sprintf(string,"\tTest opening invalid file, \'%s\'",invalid_file);
zvmessage(string," ");
zvmessage("\tExpect message of error in GPDS_OPEN_FILE call.\n"," ");

label_ptr = gpds_open_file(invalid_file,&status);
if ( status < 0 )
	zvmessage("\t*** Error in GPDS_OPEN_FILE call\n"," ");
else
	zvmessage("\tSuccessful GPDS_OPEN_FILE call\n"," ");

/*

Test of opening and closing of nonexistent file.
Graceful failure and error messages are expected.

*/

zvmessage("\t**********************************************\n"," ");
zvmessage("\tTest opening nonexistent file, \'NONEXISTENT.FILE\'"," ");
zvmessage("\tExpect error message for GPDS_OPEN_FILE call.\n"," ");

label_ptr = gpds_open_file("NONEXISTENT.FILE",&status);
if ( status < 0 )
	zvmessage("\t*** Error in GPDS_OPEN_FILE call\n"," ");
else
	zvmessage("\tSuccessful GPDS_OPEN_FILE call\n"," ");

zvmessage("\tTest closing nonexistent file, \'NONEXISTENT.FILE\'"," ");
zvmessage("\tNo abends or address errors expected.\n"," ");

gpds_close_file(label_ptr);
zvmessage("\tSuccessful GPDS_CLOSE_FILE call\n"," ");

zvmessage(" "," ");
zvmessage("************************************************"," ");
zvmessage(" "," ");
zvmessage(" End of ACCEPTANCE and UNIT test."," ");
zvmessage(" "," ");
zvmessage("************************************************"," ");
}
