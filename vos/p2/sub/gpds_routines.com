$!****************************************************************************
$!
$! Build proc for MIPL module gpds_routines
$! VPACK Version 1.9, Monday, August 10, 1998, 12:08:44
$!
$! Execute by entering:		$ @gpds_routines
$!
$! The primary option controls how much is to be built.  It must be in
$! the first parameter.  Only the capitalized letters below are necessary.
$!
$! Primary options are:
$!   COMPile     Compile the program modules
$!   ALL         Build a private version, and unpack the PDF and DOC files.
$!   STD         Build a private version, and unpack the PDF file(s).
$!   SYStem      Build the system version with the CLEAN option, and
$!               unpack the PDF and DOC files.
$!   CLEAN       Clean (delete/purge) parts of the code, see secondary options
$!   UNPACK      All files are created.
$!   REPACK      Only the repack file is created.
$!   SOURCE      Only the source files are created.
$!   SORC        Only the source files are created.
$!               (This parameter is left in for backward compatibility).
$!   TEST        Only the test files are created.
$!   IMAKE       Only the IMAKE file (used with the VIMAKE program) is created.
$!   OTHER       Only the "other" files are created.
$!
$!   The default is to use the STD parameter if none is provided.
$!
$!****************************************************************************
$!
$! The secondary options modify how the primary option is performed.
$! Note that secondary options apply to particular primary options,
$! listed below.  If more than one secondary is desired, separate them by
$! commas so the entire list is in a single parameter.
$!
$! Secondary options are:
$! COMPile,ALL:
$!   DEBug      Compile for debug               (/debug/noopt)
$!   PROfile    Compile for PCA                 (/debug)
$!   LISt       Generate a list file            (/list)
$!   LISTALL    Generate a full list            (/show=all)   (implies LIST)
$! CLEAN:
$!   OBJ        Delete object and list files, and purge executable (default)
$!   SRC        Delete source and make files
$!
$!****************************************************************************
$!
$ write sys$output "*** module gpds_routines ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_Test = ""
$ Create_Imake = ""
$ Create_Other = ""
$ Do_Make = ""
$!
$! Parse the primary option, which must be in p1.
$ primary = f$edit(p1,"UPCASE,TRIM")
$ if (primary.eqs."") then primary = " "
$ secondary = f$edit(p2,"UPCASE,TRIM")
$!
$ if primary .eqs. "UNPACK" then gosub Set_Unpack_Options
$ if (f$locate("COMP", primary) .eqs. 0) then gosub Set_Exe_Options
$ if (f$locate("ALL", primary) .eqs. 0) then gosub Set_All_Options
$ if (f$locate("STD", primary) .eqs. 0) then gosub Set_Default_Options
$ if (f$locate("SYS", primary) .eqs. 0) then gosub Set_Sys_Options
$ if primary .eqs. " " then gosub Set_Default_Options
$ if primary .eqs. "REPACK" then Create_Repack = "Y"
$ if primary .eqs. "SORC" .or. primary .eqs. "SOURCE" then Create_Source = "Y"
$ if primary .eqs. "TEST" then Create_Test = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if primary .eqs. "OTHER" then Create_Other = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_Test .or. Create_Imake .or -
        Create_Other .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to gpds_routines.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Create_Other then gosub Other_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_Test = "Y"
$   Create_Imake = "Y"
$   Create_Other = "Y"
$ Return
$!
$ Set_EXE_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Default_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("gpds_routines.imake") .nes. ""
$   then
$      vimake gpds_routines
$      purge gpds_routines.bld
$   else
$      if F$SEARCH("gpds_routines.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake gpds_routines
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @gpds_routines.bld "STD"
$   else
$      @gpds_routines.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create gpds_routines.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack gpds_routines.com -
	-s gpds_open_file.c gpds_close_file.c gpds_get_label_value.c -
	   gpds_get_column_value.c gpds_replace_column_value.c -
	   gpds_get_histogram.c -
	-i gpds_routines.imake -
	-t tgpdslabel.c tgpdslabel.imake tgpdslabel.pdf tstgpdslabel.pdf -
	   tgpdsgcvalue.c tgpdsgcvalue.imake tgpdsgcvalue.pdf -
	   tstgpdsgcvalue.pdf tgpdsrcvalue.c tgpdsrcvalue.imake -
	   tgpdsrcvalue.pdf tstgpdsrcvalue.pdf tgpdsfileio.c -
	   tgpdsfileio.imake tgpdsfileio.pdf tstgpdsfileio.pdf tgpdsgethist.c -
	   tgpdsgethist.imake tgpdsgethist.pdf tstgpdsgethist.pdf -
	-o gpds_open_file.hlp gpds_close_file.hlp gpds_get_label_value.hlp -
	   gpds_get_column_value.hlp gpds_replace_column_value.hlp -
	   gpds_get_histogram.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create gpds_open_file.c
$ DECK/DOLLARS="$ VOKAGLEVE"
				/*					*/
				/* INCLUDE FILES NEEDED FOR ROUTINES 	*/
				/*					*/
#include "pdsdef.h"		/* PDS Label Library Include Files	*/
#include "label.h"		
#include "gpds.h"		/* VICAR/PDS Include File		*/

/*

VICAR ROUTINE GPDS_OPEN_FILE

PURPOSE

GPDS_OPEN_FILE is a MIPS PDS utility to open a PDS standard file.

CALLING SEQUENCE

	#include "gpdsroutines.h"

	main44 ()
	{

	AGGREGATE *label_ptr;
	char file_name[40];
	int status_flag;
	...

	label_ptr = gpds_open_file( file_name, &status_flag );

	...
	
	}

INPUT

	file_name  	(character string)

	This is a complete pathname for the PDS standard, PDS labeled file 
	that is to be read or searched.

RETURN

	label_ptr 	(AGGREGATE data type)

	This is the pointer to the PDS specific ODL tree which represents
	the label in the file. This value is passed on to other GPDS routines
	that read or write PDS label values. If status flag is negative, there
	is a failure with opening the designated file and a NULL is returned
	for label_ptr.

OUTPUT

	status_flag 	(integer)

	This is an indicator of the success or failure of the opening of the
	file for reading or writing of the PDS label. The following are the
	meanings of the returned values:

	0	successful opening for reading of PDS labeled file
	-1	failure in returning label pointer
	
BACKGROUND

This routine is based on the PDS Label Library of the PDS Toolbox, version 5.2
dated September 27, 1991. Refer to PDS Label Library User's Guide, D-8922
for reference to software called in this source.

HISTORY

Programmer: 		Justin McNeill, Jr.
Cognizant programmer:	Justin McNeill, Jr.
Written:		August, 1993
Revisions:

Aug. 10, 98  ...RRP... Made it ANSI compatiable so it can be compiled on
                       HP.

*/
/*************************************************************************

C Callable Version

*************************************************************************/

AGGREGATE gpds_open_file( file_name, status )
char *file_name;
int  *status;
{
AGGREGATE label_ptr;
int label_status;

/* Initialize status flag 		*/
label_status = PDS_SUCCESS;

/* Perform PDS label library setups 	*/
lab_setup();

/* Read label of file and return ODL tree label pointer 	*/
label_ptr = lab_read_label_or_template( file_name );

/* Perform error checking */
if ( 	label_ptr == NULL ||
	label_status == PDS_ERROR ||
	label_status == PDS_MULTIPLE_OBJECTS ||
	label_status == PDS_MULTIPLE_PARMS )
	*status = -1;
else
	*status = 0;

return label_ptr;
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create gpds_close_file.c
$ DECK/DOLLARS="$ VOKAGLEVE"
				/*					*/
				/* INCLUDE FILES NEEDED FOR ROUTINES 	*/
				/*					*/
#include "pdsdef.h"		/* PDS Label Library Include Files	*/
#include "label.h"		
#include "gpds.h"		/* VICAR/PDS Include File		*/


/*

VICAR ROUTINE GPDS_CLOSE_FILE

PURPOSE

GPDS_CLOSE_FILE is a MIPS PDS utility to close a PDS standard file for
label reading and writing.

CALLING SEQUENCE


	#include "gpdsroutines.h"

	main44()
	{
	AGGREGATE label_ptr;
	char file_name[40];
	int status_flag;
	...

	label_ptr = gpds_open_file( file_name, &status_flag );

	...

	gpds_close_file( label_ptr );

	...
	
	}

INPUT

	label_pointer 	(AGGREGATE data type)

	This is the pointer to the PDS specific ODL tree which represents
	the label in the file. This value is passed on to other GPDS routines
	that read or write PDS label values.

BACKGROUND

This routine is based on the PDS Label Library of the PDS Toolbox, version 5.2
dated September 27, 1991. Refer to PDS Label Library User's Guide, D-8922
for reference to software called in this source.

HISTORY

Programmer: 		Justin McNeill, Jr.
Cognizant programmer:	Justin McNeill, Jr.
Written:		August 1993
Revisions:		None

*/
/*************************************************************************

C Callable Version

*************************************************************************/
void gpds_close_file( label_ptr )
AGGREGATE label_ptr;
{
/* 

Deallocate memory of ODL tree	

*/

if ( label_ptr != NULL )
	label_ptr = lab_remove_label_or_template( label_ptr );

lab_exit();
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create gpds_get_label_value.c
$ DECK/DOLLARS="$ VOKAGLEVE"
				/*					*/
				/* INCLUDE FILES NEEDED FOR ROUTINES 	*/
				/*					*/
#include "pdsdef.h"		/* PDS Label Library Include Files	*/
#include "label.h"		
#include "gpds.h"		/* VICAR/PDS Include File		*/
#include <string.h>

/*
VICAR ROUTINE GPDS_GET_LABEL_VALUE

PURPOSE

GPDS_GET_LABEL_VALUE is a MIPS PDS utility to retrieve a value of a keyword
in a PDS labeled file.

CALLING SEQUENCE

	#include "gpdsroutines.h"

	main44()
	{

	AGGREGATE *label_ptr;
	char object_name[40];
	char keyword_name[40];
	char *value;
	int element_of_value;
	int status_flag;
	...

	value = gpds_get_label_value( label_ptr, object_name, keyword_name, 
			element_of_value, &status_flag );

	...
	
	}

INPUT

	label_ptr		(AGGREGATE type)

	This is a pointer to the root of the PDS label. It is returned by the
	routine GPDS_OPEN_FILE.

	object_name  		(character string)

	This is the PDS label object name (exact spelling including underscores)
	in which the PDS keyword value is to be found.

	keyword_name  		(character string)

	This is the PDS keyword name (exact spelling including underscores)
	for which a value is to be returned.

	element_of_value	(integer)

	This is the index or position of a value in a multivalued keyword
	in the PDS label (e.g. ASSOCIATED_PRODUCT_ID = { 03R00321, 03B00322 })
	In this example, the value of 03B00322 of the keyword would be 
	referenced by element_of_value set equal to "2", corresponding to the
	second element of the multivalued keyword. 

RETURN

	value 		(character string) 

	This is the pointer to a character string list containing the value
	of a PDS keyword in the PDS label.

OUTPUT

	status_flag 	(integer)

	This is an indicator of the success or failure of the returning the
	value of a PDS keyword. The following are the meanings of the 
	returned values:

	0	successful returning of pointer to value
	-1	failure in returning pointer to value, failure in 
		finding object or parameter;

	
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
char *gpds_get_label_value( label_ptr, object, keyword, element, status )
AGGREGATE label_ptr;
char *object;
char *keyword;
int element;
int *status;
{
int 	label_status;
char 	*value;
static char	blank[2];

strcpy(blank," ");

/* Get a single value from PDS label	*/
value = lab_get_value( label_ptr, object, 0, 1, keyword, 0, element,
	FALSE, &label_status );

/* Perform error checking		*/
if ( 	label_status == PDS_ERROR ||
	label_status == PDS_MULTIPLE_OBJECTS ||
	label_status == PDS_MULTIPLE_PARMS )
	{
	*status = -1;
	return blank;
	}
else
	{
	*status = 0;
	return value;
	}
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create gpds_get_column_value.c
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create gpds_replace_column_value.c
$ DECK/DOLLARS="$ VOKAGLEVE"
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
  
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create gpds_get_histogram.c
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create gpds_routines.imake

/* Imake file for MIPS subroutines GPDS_ROUTINES   */

#define SUBROUTINE  	gpds_routines

#define MODULE_LIST  	gpds_open_file.c \
			gpds_close_file.c \
			gpds_get_column_value.c \
			gpds_get_histogram.c\
			gpds_get_label_value.c \
			gpds_replace_column_value.c

#define P2_SUBLIB
#define LIB_PDS_LABEL

#define USES_ANSI_C

#if UNIX_OS
#define C_OPTIONS -D PDS_TOOLBOX
#endif
$ Return
$!#############################################################################
$Test_File:
$ create tgpdslabel.c
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
$!-----------------------------------------------------------------------------
$ create tgpdslabel.imake
/* 

IMAKE file for Test of VICAR subroutine GPDS_GET_LABEL_VALUE 

*/

#define PROGRAM tgpdslabel

#define MODULE_LIST tgpdslabel.c

#define MAIN_LANG_C
#define TEST

#define USES_C

#if UNIX_OS
#define C_OPTIONS -D SUN_UNIX -D PDS_TOOLBOX
#endif

#define   LIB_RTL
#define   LIB_TAE
#define   LIB_P2SUB 
#define   LIB_PDS_LABEL
$!-----------------------------------------------------------------------------
$ create tgpdslabel.pdf
Process
PARM INP 	STRING	
End-Proc
$!-----------------------------------------------------------------------------
$ create tstgpdslabel.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"

write " "
write " TEST PROGRAM FOR GPDS_GET_LABEL_VALUE"
write " "

write " Be sure that the file listed as a parameter value of"
write " the program is in your local directory before executing"
write " this proc. The file is available on arsia workstation in"
write " the directory /mo1/test/images"

tgpdslabel	INP="03r00231.img"

end-proc
$!-----------------------------------------------------------------------------
$ create tgpdsgcvalue.c
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
$!-----------------------------------------------------------------------------
$ create tgpdsgcvalue.imake
/* 

IMAKE file for Test of VICAR subroutine GPDS_GET_COLUMN_VALUE 

*/

#define PROGRAM tgpdsgcvalue

#define MODULE_LIST tgpdsgcvalue.c

#define MAIN_LANG_C
#define TEST

#define USES_C

#if UNIX_OS
#define C_OPTIONS -D SUN_UNIX -D PDS_TOOLBOX
#endif

#define   LIB_RTL
#define   LIB_TAE
#define   LIB_P2SUB     
#define   LIB_PDS_LABEL
$!-----------------------------------------------------------------------------
$ create tgpdsgcvalue.pdf
Process
PARM LABEL 	STRING	
PARM TABLE	STRING	
End-Proc
$!-----------------------------------------------------------------------------
$ create tstgpdsgcvalue.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"

write " "
write " TEST PROGRAM FOR GPDS_GET_COLUMN_VALUE"
write " "

write " Be sure that files listed as parameter values of program"
write " are in your local directory before executing this proc."
write " Files available on arsia workstation, directory /mo1/test/tables"

tgpdsgcvalue 	LABEL="items_test.lbl" +
		TABLE="items_test.tab"
end-proc
$!-----------------------------------------------------------------------------
$ create tgpdsrcvalue.c
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
$!-----------------------------------------------------------------------------
$ create tgpdsrcvalue.imake
/* 

IMAKE file for Test of VICAR subroutine GPDS_REPLACE_COLUMN_VALUE 		

*/

#define PROGRAM tgpdsrcvalue

#define MODULE_LIST tgpdsrcvalue.c

#define MAIN_LANG_C
#define TEST

#define USES_C

#if UNIX_OS
#define C_OPTIONS -D SUN_UNIX -D PDS_TOOLBOX
#endif

#define   LIB_RTL
#define   LIB_TAE
#define   LIB_P2SUB
#define   LIB_PDS_LABEL
$!-----------------------------------------------------------------------------
$ create tgpdsrcvalue.pdf
Process
PARM LABEL 	STRING	
PARM TABLE	STRING	
End-Proc
$!-----------------------------------------------------------------------------
$ create tstgpdsrcvalue.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
tgpdsrcvalue 	label="items_test.lbl" +
		table="test.tab"
end-proc
$!-----------------------------------------------------------------------------
$ create tgpdsfileio.c
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
$!-----------------------------------------------------------------------------
$ create tgpdsfileio.imake
/* 

IMAKE file for Test of MIPS subroutines 
GPDS_OPEN_FILE and GPDS_CLOSE_FILE.

*/

#define PROGRAM tgpdsfileio

#define MODULE_LIST tgpdsfileio.c

#define MAIN_LANG_C
#define TEST

#define USES_C

#if UNIX_OS
#define C_OPTIONS -D PDS_TOOLBOX
#endif

#define   LIB_RTL
#define   LIB_TAE
#define   LIB_P2SUB
#define   LIB_PDS_LABEL
$!-----------------------------------------------------------------------------
$ create tgpdsfileio.pdf
Process
PARM INP 	STRING	
PARM INVALID 	STRING	
End-Proc
$!-----------------------------------------------------------------------------
$ create tstgpdsfileio.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"

write " "
write " TEST PROGRAM FOR GPDS_OPEN_FILE AND GPDS_CLOSE_FILE"
write " "

write " Be sure that files listed as parameter values of program"
write " are in your local directory before executing this proc."
write " Files available on arsia workstation, directories"
write " /mo1/test/tables and /mo1/test/images."

tgpdsfileio 	INP="03r00231.img" +
		INVALID="gproduction.tab"
end-proc
$!-----------------------------------------------------------------------------
$ create tgpdsgethist.c
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
$!-----------------------------------------------------------------------------
$ create tgpdsgethist.imake
/* 

IMAKE file for Test of MIPS subroutines 
GPDS_GET_HISTOGRAM

*/

#define PROGRAM tgpdsgethist
#define MODULE_LIST tgpdsgethist.c

#define MAIN_LANG_C
#define TEST

#define USES_C

#if UNIX_OS
#define C_OPTIONS -D SUN_UNIX -D PDS_TOOLBOX
#endif

#define   LIB_RTL
#define   LIB_TAE
#define   LIB_P2SUB    
#define   LIB_PDS_LABEL
$!-----------------------------------------------------------------------------
$ create tgpdsgethist.pdf
Process
PARM INP 	STRING	
PARM INVALID1 	STRING	
PARM INVALID2	STRING	
PARM INVALID3	STRING	
PARM INVALID4	STRING	
End-Proc
$!-----------------------------------------------------------------------------
$ create tstgpdsgethist.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"

write "	"
write " TEST PROGRAM FOR GPDS_GET_HISTOGRAM"
write " "

write " Be sure that the input images listed as parameters"
write " are in your local directory before executing"
write " this proc."
write " Files are available on arsia workstation,"
write " directory /mo1/test/images"

tgpdsgethist 	INP="03r00231.img" +
		INVALID1="03r00231.iv1" +
		INVALID2="03r00231.iv2" +
		INVALID3="03r00231.iv3" +
		INVALID4="03r00231.iv4" 
end-proc
$ Return
$!#############################################################################
$Other_File:
$ create gpds_open_file.hlp

1 VICAR ROUTINE GPDS_OPEN_FILE

PURPOSE

GPDS_OPEN_FILE is a MIPS PDS utility to open a PDS standard file.

CALLING SEQUENCE

	#include "gpdsroutines.h"

	main44 ()
	{

	AGGREGATE *label_ptr;
	char file_name[40];
	int status_flag;
	...

	label_ptr = gpds_open_file( file_name, &status_flag );

	...
	
	}

INPUT

	file_name  	(character string)

	This is a complete pathname for the PDS standard, PDS labeled file 
	that is to be read or searched.

RETURN

	label_ptr 	(AGGREGATE data type)

	This is the pointer to the PDS specific ODL tree which represents
	the label in the file. This value is passed on to other GPDS routines
	that read or write PDS label values.

OUTPUT

	status_flag 	(integer)

	This is an indicator of the success or failure of the opening of the
	file for reading or writing of the PDS label. The following are the
	meanings of the returned values:

	0	successful opening for reading of PDS labeled file
	-1	failure in returning label pointer
	
2 BACKGROUND

This routine is based on the PDS Label Library of the PDS Toolbox, version 5.2
dated September 27, 1991. Refer to PDS Label Library User's Guide, D-8922
for reference to software called in this source.

2 HISTORY

Programmer: 		Justin McNeill, Jr.
Cognizant programmer:	Justin McNeill, Jr.
Written:		August 1993
Revisions:

Aug. 10, 98  ...RRP... Made it ANSI compatiable so it can be compiled on
                       HP.
$!-----------------------------------------------------------------------------
$ create gpds_close_file.hlp

1 VICAR ROUTINE GPDS_CLOSE_FILE

PURPOSE

GPDS_CLOSE_FILE is a MIPS PDS utility to close a PDS standard file for
label reading and writing.

CALLING SEQUENCE


	#include "gpdsroutines.h"

	main44()
	{
	AGGREGATE label_ptr;
	char file_name[40];
	int status_flag;
	...

	label_ptr = gpds_open_file( file_name, &status_flag );

	...

	gpds_close_file( label_ptr );

	...
	
	}

INPUT

	label_pointer 	(AGGREGATE data type)

	This is the pointer to the PDS specific ODL tree which represents
	the label in the file. This value is passed on to other GPDS routines
	that read or write PDS label values.

2 BACKGROUND

This routine is based on the PDS Label Library of the PDS Toolbox, version 5.2
dated September 27, 1991. Refer to PDS Label Library User's Guide, D-8922
for reference to software called in this source.

2 HISTORY

Programmer: 		Justin McNeill, Jr.
Cognizant programmer:	Justin McNeill, Jr.
Written:		August 1993
Revisions:

Aug. 10, 98  ...RRP... Made it ANSI compatiable so it can be compiled on
                       HP.
$!-----------------------------------------------------------------------------
$ create gpds_get_label_value.hlp

1 VICAR ROUTINE GPDS_GET_LABEL_VALUE

PURPOSE

GPDS_GET_LABEL_VALUE is a MIPS PDS utility to retrieve a value of a keyword
in a PDS labeled file.

CALLING SEQUENCE

	#include "gpdsroutines.h"

	main44()
	{

	AGGREGATE *label_ptr;
	char object_name[40];
	char keyword_name[40];
	char *value;
	int element_of_value;
	int status_flag;
	...

	value = gpds_get_label_value( label_ptr, object_name, keyword_name, 
			element_of_value, &status_flag );

	...
	
	}

INPUT

	label_ptr		(AGGREGATE type)

	This is a pointer to the root of the PDS label. It is returned by the
	routine GPDS_OPEN_FILE.

	object_name  		(character string)

	This is the PDS label object name (exact spelling including underscores)
	in which the PDS keyword value is to be found.

	keyword_name  		(character string)

	This is the PDS keyword name (exact spelling including underscores)
	for which a value is to be returned.

	element_of_value	(integer)

	This is the index or position of a value in a multivalued keyword
	in the PDS label (e.g. ASSOCIATED_PRODUCT_ID = { 03R00321, 03B00322 })
	In this example, the value of 03B00322 of the keyword would be 
	referenced by element_of_value set equal to "2", corresponding to the
	second element of the multivalued keyword. 

RETURN

	value 		(character string) 

	This is the pointer to a character string list containing the value
	of a PDS keyword in the PDS label.

OUTPUT

	status_flag 	(integer)

	This is an indicator of the success or failure of the returning the
	value of a PDS keyword. The following are the meanings of the 
	returned values:

	0	successful returning of pointer to value
	-1	failure in returning pointer to value, failure in 
		finding object or parameter;

	
2 BACKGROUND

This routine is based on the PDS Label Library of the PDS Toolbox, version 5.2
dated September 27, 1991. Refer to PDS Label Library User's Guide, D-8922
for reference to software called in this source.

2 HISTORY

Programmer: 		Justin McNeill, Jr.
Cognizant programmer:	Justin McNeill, Jr.
Written:		August 1993
Revisions:

Aug. 10, 98  ...RRP... Made it ANSI compatiable so it can be compiled on
                       HP.
$!-----------------------------------------------------------------------------
$ create gpds_get_column_value.hlp

1 VICAR ROUTINE GPDS_GET_COLUMN_VALUE

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
	
2 BACKGROUND

This routine is based on the PDS Label Library of the PDS Toolbox, version 5.2
dated September 27, 1991. Refer to PDS Label Library User's Guide, D-8922
for reference to software called in this source.

2 HISTORY

Programmer: 		Justin McNeill, Jr.
Cognizant programmer:	Justin McNeill, Jr.
Written:		August 1993
Revisions:

Aug. 10, 98  ...RRP... Made it ANSI compatiable so it can be compiled on
                       HP.

$!-----------------------------------------------------------------------------
$ create gpds_replace_column_value.hlp

1 VICAR ROUTINE GPDS_REPLACE_COLUMN_VALUE

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

	-5	memory allocation error for buffer to hold table file's
		row values
	
2 BACKGROUND

This routine is based on the PDS Label Library of the PDS Toolbox, version 5.2
dated September 27, 1991. Refer to PDS Label Library User's Guide, D-8922
for reference to software called in this source.

2 HISTORY

Programmer: 		Justin McNeill, Jr.
Cognizant programmer:	Justin McNeill, Jr.
Written:		August 1993
Revisions:

Aug. 10, 98  ...RRP... Made it ANSI compatiable so it can be compiled on
                       HP.
$!-----------------------------------------------------------------------------
$ create gpds_get_histogram.hlp

1 VICAR ROUTINE GPDS_GET_HISTOGRAM

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

	-5	invalid number of elements in histogram object found in 
		label of input file.

	-6	memory allocation error for buffer to hold record read from
		input file.

2 BACKGROUND

This routine is based on the PDS Label Library of the PDS Toolbox, version 5.2
dated September 27, 1991. Refer to PDS Label Library User's Guide, D-8922
for reference to software called in this source.

2 HISTORY

Programmer: 		Justin McNeill, Jr.
Cognizant programmer:	Justin McNeill, Jr.
Written:		August 1993
Revisions:

Aug. 10, 98  ...RRP... Made it ANSI compatiable so it can be compiled on
                       HP.
$ Return
$!#############################################################################
