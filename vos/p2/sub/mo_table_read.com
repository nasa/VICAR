$!****************************************************************************
$!
$! Build proc for MIPL module mo_table_read
$! VPACK Version 1.7, Wednesday, September 22, 1993, 09:01:04
$!
$! Execute by entering:		$ @mo_table_read
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
$ write sys$output "*** module mo_table_read ***"
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
$ write sys$output "Invalid argument given to mo_table_read.com file -- ", primary
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
$   if F$SEARCH("mo_table_read.imake") .nes. ""
$   then
$      vimake mo_table_read
$      purge mo_table_read.bld
$   else
$      if F$SEARCH("mo_table_read.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake mo_table_read
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @mo_table_read.bld "STD"
$   else
$      @mo_table_read.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create mo_table_read.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack mo_table_read.com -
	-s mo_table_read.c -
	-i mo_table_read.imake -
	-t tmo_table_read.c tmo_table_read.imake tmo_table_read.pdf -
	   tstmotableread.pdf -
	-o mo_table_read.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create mo_table_read.c
$ DECK/DOLLARS="$ VOKAGLEVE"


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
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create mo_table_read.imake
/* Imake file for MIPS subroutine MO_TABLE_READ   */

#define SUBROUTINE  	mo_table_read

#define MODULE_LIST  	mo_table_read.c 

#define P2_SUBLIB
#define LIB_PDS_LABEL

#define USES_C

#if UNIX_OS
#define C_OPTIONS -D SUN_UNIX -D PDS_TOOLBOX
#endif
$ Return
$!#############################################################################
$Test_File:
$ create tmo_table_read.c
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
$!-----------------------------------------------------------------------------
$ create tmo_table_read.imake
/* 

IMAKE file for Test of MIPS routine MO_TABLE_READ

*/

#define PROGRAM tmo_table_read

#define MODULE_LIST tmo_table_read.c

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
$ create tmo_table_read.pdf
Process
PARM P_TABLE 	STRING	
PARM P_LABEL 	STRING	
PARM INVALID 	STRING	
End-Proc
$!-----------------------------------------------------------------------------
$ create tstmotableread.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"

write " "
write " TEST PROGRAM FOR MO_TABLE_READ"
write " "

write " Be sure that files listed as parameter values of program"
write " are in your local directory before executing this proc."
write " Files available on arsia workstation, directories"
write " /mo1/test/tables."

tmo_table_read 	P_TABLE="gproduction.tab" +
		P_LABEL="gproduction.lbl" +
		INVALID="invalid.lbl"
end-proc
$ Return
$!#############################################################################
$Other_File:
$ create mo_table_read.hlp

1 VICAR ROUTINE MO_TABLE_READ

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

2 INPUT	

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


2 OUTPUT

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

2 HISTORY

	Current cognizant programmer:	Justin McNeill
	Date:				August 1993
	Revisions:			Original
$ Return
$!#############################################################################
