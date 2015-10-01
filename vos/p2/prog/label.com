$!****************************************************************************
$!
$! Build proc for MIPL module label
$! VPACK Version 1.9, Saturday, November 13, 2010, 15:52:36
$!
$! Execute by entering:		$ @label
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
$!   PDF         Only the PDF file is created.
$!   TEST        Only the test files are created.
$!   IMAKE       Only the IMAKE file (used with the VIMAKE program) is created.
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
$ write sys$output "*** module label ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_PDF = ""
$ Create_Test = ""
$ Create_Imake = ""
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
$ if primary .eqs. "PDF" then Create_PDF = "Y"
$ if primary .eqs. "TEST" then Create_Test = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_PDF .or. Create_Test .or -
        Create_Imake .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to label.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_PDF then gosub PDF_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_PDF = "Y"
$   Create_Test = "Y"
$   Create_Imake = "Y"
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
$   Create_PDF = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$   Create_PDF = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Create_PDF = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("label.imake") .nes. ""
$   then
$      vimake label
$      purge label.bld
$   else
$      if F$SEARCH("label.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake label
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @label.bld "STD"
$   else
$      @label.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create label.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack label.com -mixed -
	-s label.c -
	-i label.imake -
	-p label.pdf -
	-t tstlabel.pdf tstlabel.log_solos tstlabel.log_linux
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create label.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/*								*/
/*	LABEL							*/
/*								*/
/*	VICAR label processing program				*/
/*								*/
/*	For help on operation, see help in PDF file		*/
/*								*/
/*	Initial release:  March 1, 1984				*/
/*			  Dan Stanfill (DFS)			*/
/*								*/

#include <ctype.h>
#include "vicmain_c"
#include "defines.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define TRUE 1
#define FALSE 0

#define REPLACE 0
#define ADD 1

#define NOCOPY 0
#define COPY 1

#define MAXSTRINGSIZE	4000

#define WIDTH	80		/* width of the printout on the screen */

#define DEL_COUNT	10	/* Max count of parameters to -delete */

#if MAX_TASKS > MAX_PROPS	/* Get maximum label set size */
#define MAX_SETS MAX_TASKS
#else
#define MAX_SETS MAX_PROPS
#endif

/* Error handling macros */

#define return_on_error(A)   zvsignal(A,status,0); if (status <= 0) zabend()
#define continue_on_error(A) zvsignal(A,status,0); if (status <= 0) continue
#define abort_on_error(A)    zvsignal(A,status,1)
#define break_on_error(A)    zvsignal(A,status,0); if (status <= 0) break

/* Globally used variables */
    char msg[MAXSTRINGSIZE];	/* Message buffer for printing */
    int status;			/* Return status indicator */

/* Buffer for print_key_value_pairs() so it can pack items */
    char printbuf[MAXSTRINGSIZE+1];

/* Boolean value of PACK keyword */
    int pack_listed_items;

/* Multivalued label item structure */
    struct multival
    {
	int nelements;
	int maxlength;
	char *data;		/* ptr to array[nelements][maxlength] */
	int allocsize;
    };

/* Prototypes */

void add_items(int action);
void concat_labels(void);
void add_system_label(void);
void delete_items(void);
void edit_label(void);
void list_label(void);
void remove_label(void);
void switch_labels(void);
int count_sig_digits (char *str);
void concat_set(int source_unit, int out_unit, char *type, char *set_opt, char *set_name, int instance);
int string_on_list(char *string, char list[][MAX_LABEL_KEY_SIZE+1], int list_len);
int int_on_list(int intval, int list[], int list_len);
void revise_instance_list(char names[MAX_SETS][MAX_LABEL_KEY_SIZE+1],
	int subset, int new_list[], int list_len);
void dump_all_items(int unit);
void list_system_info(int unit);
void list_property_items(int unit);
void list_history_items(int unit);
void list_task_headers(int unit);
void print_key_value_pair(char key[], struct multival *value, char *format);
void flush_key_value_pair(void);

void my_abort3(char *str, char *p1, char *p2);
void my_abort2(char *str, char *p1);
void my_abort1(char *str);

void main44(void)
{   
    int count,def;
    char command[7];
    
    zvmessage("LABEL version 15-Nov-2010", "");

    zvparm("_SUBCMD",command,&count,&def,0,0);
    switch (command[0])
    {   case 'A' : add_items(ADD);
		   break;
	case 'C' : if (command[1] == 'O')
		       concat_labels();
		   else
		       add_system_label();
		   break;
	case 'D' : delete_items();
		   break;
	case 'E' : edit_label();
		   break;
	case 'L' : list_label();
		   break;
	case 'R' : if (command[2] == 'P')
		       add_items(REPLACE);
		   else
		       remove_label();
		   break;
	case 'S' : switch_labels();
    }
}

void add_items(int action)
{
/*	Local variables:					*/
/*								*/
/*  in_unit	-- Input file unit number			*/
/*  out_unit	-- Output file unit number			*/
/*  count	-- Count of parameter from zvparm		*/
/*  instance	-- instance of TASK or PROPERTY			*/
/*  key		-- name of key to be inserted			*/
/*  value	-- multival struct describing item to be added	*/
/*  form	-- INT, REAL, DOUB, or STRING.  The format of	*/
/*		   VALUE.					*/
/*  set		-- the name of the task or property subset in	*/
/*		    which the item is to be inserted		*/
/*  mode	-- The mode to use when calling zladd		*/
/*  data_copy   -- Flag indicating whether or not to copy data  */
/*  		   to an output file.				*/
/*  p		-- Pointer to the current char in string being	*/
/*		   parse.					*/
/*  element_number -- Starting elem given in () after key name	*/
/*								*/

    int in_unit,out_unit,count,instance;
    int data_copy;
    char form[12],set[33],message[80],type[12];
    char key[33], mode[12], set_opt[12];
    char *p;
    int len;
    int maxlen, nelements;		/* from original label item */
    int element_number, element;
    int change_format;		/* TRUE iff we can change the format */
    int nlb, nbb;		/* BINARY header and prefix lengths */
    struct multival value;
    char *parse_value();
    int task_cnt, prop_cnt;
    int update;

/* Check for update mode */
    update = zvptst("UPDATE");

/* Find out whether data is to be copied */
    zvpcnt("OUT",&count);
    data_copy = (count == 1) ? TRUE : FALSE;

/* Get the file unit numbers */
    if (data_copy) 
    {
	status = zvunit(&in_unit,"INP",1, NULL);
	abort_on_error(in_unit);

	status = zvunit(&out_unit,"OUT",1, NULL);
	abort_on_error(out_unit);

	status = zvopen(in_unit,"OPEN_ACT","SA","IO_ACT","","COND","BINARY", NULL);
	status = zvget(in_unit,"NLB",&nlb,"NBB",&nbb, NULL);
	if (status != 1)
	    nlb = nbb = 0;
	status = zvopen(out_unit,"OP","WRITE","OPEN_ACT","SA","COND","BINARY",
			"U_NLB",nlb,"U_NBB",nbb, NULL);
    }
    else	/* open for update */
    {
	status = zvunit(&out_unit,"INP",1, NULL);
	abort_on_error(out_unit);

	status = zvopen(out_unit,"OP","UPDATE","OPEN_ACT","SA", NULL);
    }
/* Get list of items to be added */
    zvp("ITEMS",msg,&count);

/* Get type of labels to deal with (HISTORY, PROPERTY, or SYSTEM) */

    zvpcnt("TASK", &task_cnt);
    zvpcnt("PROPERTY", &prop_cnt);

    if (task_cnt != 0 && prop_cnt != 0)
	my_abort1("Cannot specify both TASK and PROPERTY");

    zvp("TYPE",type,&count);
    if (count == 0)			/* No type specified */
    {
	if (prop_cnt != 0)
	    strcpy(type,"PROPERTY");
	else				/* TASK given or default case */
	    strcpy(type,"HISTORY");
    }
    else				/* Type given, check for consistency */
    {
	if (strcmp(type,"HISTORY") == 0 && prop_cnt != 0)
	    my_abort1("Cannot use PROPERTY parameter if TYPE=HISTORY");
	if (strcmp(type,"PROPERTY") == 0 && task_cnt != 0)
	    my_abort1("Cannot use TASK parameter if TYPE=PROPERTY");
	if (strcmp(type,"PROPERTY") == 0 && prop_cnt == 0)
	    my_abort1("Must specify PROPERTY parameter if TYPE=PROPERTY");
	if (strcmp(type,"SYSTEM") == 0 && (prop_cnt != 0 || task_cnt != 0))
	    my_abort1("Cannot use TASK or PROPERTY parameters if TYPE=SYSTEM");
    }

    if (strcmp(type,"PROPERTY") == 0)
	strcpy(set_opt,"PROPERTY");	/* optional arg for zl routines */
    else
	strcpy(set_opt,"HIST");

/* Initialize to beginning of buffer */
    p = msg;

/* Loop through items, adding each in turn */
    while (*p != '\0')
    {
	form[0] = '\0';
	change_format = TRUE;
	nelements = 0;

/* Skip over white space */
	p += strspn(p, " \t");
	if (*p == '\0')
	    break;

/* Find key word */
	len = strcspn(p, "=( \t");
	strncpy(key, p, len);
	key[len] = '\0';
	p += len;

	p += strspn(p, " \t");		/* skip white space */

	if (*p == '(')			/* element number */
	{
	    p++;
	    sscanf(p, "%d", &element_number);
	    if (element_number == 0 || element_number < -1)
	       my_abort3("Illegal starting element number of %d for keyword %s",
			(void *)element_number, key);
	    p += strspn(p, "1234567890+");	/* find closing parenthesis */
	    p += strspn(p, " \t");		/* skip white space */
	    if (*p != ')')
		my_abort2("Missing ')' for starting element of keyword %s",
			key);
	    p++;			/* skip the ) */
	    p += strspn(p, " \t");		/* skip white space */

	/* Get the type of the currently existing label so we can match it */

	    zvp("INSTANCE",&instance,&count);
	    if (strcmp(type,"HISTORY")==0)
		zvp("TASK",set,&count);
	    else if (strcmp(type,"PROPERTY")==0)
		zvp("PROPERTY",set,&count);
	    else				/* SYSTEM */
		count = 0;
	    if (count == 0)
		status = zlinfo(out_unit, type, key, form, &maxlen,
				&nelements, NULL);
	    else
		status = zlinfo(out_unit, type, key, form, &maxlen,
			&nelements, set_opt, set, "INSTANCE", instance, NULL);
	    change_format = FALSE;
	    if (status == CANNOT_FIND_KEY)
	    {
		form[0] = '\0';
		nelements = 0;
		change_format = TRUE;
	    }
	    else
		abort_on_error(out_unit);

	}
	else
	    element_number = 0;	     /* Element number subscript not present */

	if (*p != '=')
	    my_abort2("Missing '=' for keyword %s", key);
	p++;

	p += strspn(p, " \t");		/* skip white space after the = */

	if (*p == '\0')
	    my_abort2("Null value for keyword %s", key);

/* We're now at the beginning of value part of buffer */
	value.data = NULL;
	value.nelements = 0;
	value.maxlength = 0;

/* First time through parse_value.... just get maxlen, nelem, and format */

	parse_value(&value, p, form, key, change_format);

	if (strcmp(form, "INT") == 0)
	    value.maxlength = sizeof(int);
	if (strcmp(form, "REAL") == 0)
	    value.maxlength = sizeof(float);
	if (strcmp(form, "DOUB") == 0)
	    value.maxlength = sizeof(double);

	value.data = malloc(value.nelements * value.maxlength);
	if (value.data == NULL)
	    my_abort2("Out of memory processing keyword %s!!", key);

/* Second time... fill the array.  All errors should have been caught the */
/* first time through, so there is no need to free up value.data.	  */

	p = parse_value(&value, p, form, key, FALSE);

/* If TASK was not given and type is HISTORY, then default it */
/* If SYSTEM label, do that here too (since no TASK/PROP sets to worry about) */

	element = element_number;

	if (strcmp(type,"SYSTEM") == 0 ||
	    (strcmp(type,"HISTORY") == 0 && task_cnt == 0))
	{
	    if (action == REPLACE)
	    {
		if (element_number == 0)		/* not given */
		{
		    status = zldel(out_unit,type,key, NULL);
		    if (!update) {		/* ignore errors in update */
		        if (status <= 0)
			    free(value.data);
		        abort_on_error(out_unit);
		    }
		    element = 1;
		}
		strcpy(mode, "REPLACE");
	    }
	    else if (element_number == 0) {		/* not given */
		if (update) {
		    status = zldel(out_unit,type,key,NULL);  /* ignore errors */
		    element = 1;
		    strcpy(mode, "REPLACE");
		}
		else
		    strcpy(mode, "ADD");
	    }
	    else
		strcpy(mode, "INSERT");

	    status = zladd(out_unit,type,key,value.data,"FORMAT",form,
			"ELEMENT", element,
			"NELEMENT", value.nelements, "ULEN", value.maxlength,
			"MODE", mode, NULL);
	}
	else
	{
/* PROPERTY or HISTORY with specified set name */

	    zvp("INSTANCE",&instance,&count);
	    if (strcmp(type,"PROPERTY")==0)
		zvp("PROPERTY",set,&count);
	    else				/* HISTORY */
		zvp("TASK",set,&count);

	    if (action == REPLACE)
	    {
		if (element_number == 0)		/* not given */
		{
		    status = zldel(out_unit,type,key,set_opt,set,
			"INSTANCE",instance, NULL);
		    if (!update) {		/* ignore errors in update */
		        if (status <= 0)
			    free(value.data);
		        abort_on_error(out_unit);
		    }
		    element = 1;
		}
		strcpy(mode, "REPLACE");
	    }
	    else if (element_number == 0) {		/* not given */
		if (update) {
		    status = zldel(out_unit,type,key,set_opt,set,
			"INSTANCE",instance, NULL);	/* ignore errors */
		    element = 1;
		    strcpy(mode, "REPLACE");
		}
		else
		    strcpy(mode, "ADD");
	    }
	    else
		strcpy(mode, "INSERT");

	    status = zladd(out_unit,type,key,value.data,"FORMAT",form,
			set_opt, set, "INSTANCE", instance,
			"ELEMENT", element,
			"NELEMENT", value.nelements, "ULEN", value.maxlength,
			"MODE", mode, NULL);
	}
	free(value.data);

	abort_on_error(out_unit);

	if (element_number == -1 || element_number > nelements)
	    element_number = nelements+1;
	if (strcmp(mode, "ADD") == 0)
	    sprintf(message, "Keyword %s added", key);
	else if (strcmp(mode, "REPLACE") == 0)
	{
	    if (element_number == 0)
		sprintf(message, "Keyword %s replaced", key);
	    else
		if (value.nelements == 1)
		    sprintf(message, "Element %d replaced in keyword %s",
			element_number, key);
		else
		    sprintf(message,
			"Elements %d through %d replaced in keyword %s",
			element_number, element_number+value.nelements-1, key);
	}
	else
	{
	    if (value.nelements == 1)
		sprintf(message, "Element %d added to keyword %s",
			element_number, key);
	    else
		sprintf(message, "Elements %d through %d added to keyword %s",
			element_number, element_number+value.nelements-1, key);
	}
	zvmessage(message, "");

	p += strspn(p, " \t");
	if (*p == ',')	     /* skip over any (optional) commas between items */
	{
	    p++;
	    p += strspn(p, " \t");
	}
    }	/* End of while loop */


/* Copy the data over if requested */
    if (data_copy)
    {
	char *buf = NULL;
	int rec_size;
	status = zvget(in_unit, "RECSIZE", &rec_size, NULL);
	abort_on_error(in_unit);
	buf = (char *)malloc(rec_size);
	if (buf == NULL) {
	    zvmessage("Out of memory!!!", "");
	    zabend();
	}

	while (TRUE)
	{   status = zvread(in_unit,buf, NULL);
	    if (status == END_OF_FILE) break;
	    abort_on_error(in_unit);
	    status = zvwrit(out_unit,buf, NULL);
	    abort_on_error(out_unit);
	}
	free(buf);

/* Close the files and exit */
	status = zvclose(in_unit, NULL);
    }

    status = zvclose(out_unit, NULL);

}

char *parse_value(value, p, format, key, change_format)

/* Parse out the value string pointed at by *p.  If value.data is NULL,	*/
/* then the string is only scanned, with format, value.nelements, and	*/
/* value.maxlength being updated.  If value.data is not NULL, then the	*/
/* values are actually stored in value.data, and value.maxlength must	*/
/* already be set.  If change_format == TRUE, then we can change the fmt*/
/* based on user input.  If it's FALSE, the label already exists, so	*/
/* don't change the format.						*/

struct multival *value;
char *p;
char *format;
char *key;
int change_format;
{
    int paren, done, quote;
    int len, nelem, sig_digits;
    char savechar;
    char *start;

    paren = FALSE;
    done = FALSE;
    nelem = 0;

    if (*p == '(')			/* start of multival entry */
    {
	paren = TRUE;
	p++;
	p += strspn(p, " \t");		/* skip white space */
    }

    while (!done)
    {
	/* This item is a string if the rest of them are strings, if it	*/
	/* starts with a quote, or if the first char is not a number	*/

	if (strcmp(format, "STRING") == 0 ||
	    *p == '\'' || strchr("1234567890+-.Ee", *p) == 0)	   /* string */
	{
	    quote = FALSE;
	    if (*p == '\'')
	    {
		p++;
		quote = TRUE;
	    }

	    if (*format != '\0' && strcmp(format, "STRING") != 0 &&
			!change_format)
	       my_abort3("Can't change format from %s to STRING for keyword %s",
			 format, key);
	    strcpy(format, "STRING");		/* change format to STRING */

	    start = p;

	    if (quote)
		len = strcspn(p, "\'");	/* find end of string */
	    else
		len = strcspn(p, ",)\' \t");
	    p += len;
	    if ((*p != '\'' && quote) || (*p == '\'' && !quote))
		my_abort3(
		     "String for keyword %s, element %d improperly terminated",
		     key, (void *)(nelem+1));
	    if (!quote && len == 0)
		my_abort3("Invalid value for keyword %s, element %d",
			key, (void *)(nelem+1));

	    if (value->data != NULL)
	    {
		strncpy(value->data + (nelem * value->maxlength), start, len);
		*(value->data + (nelem * value->maxlength) + len) = '\0';
	    }
	    else
	    {
		value->maxlength = MAX(value->maxlength, len+1); /*+1 for term*/
		value->nelements = nelem+1;
	    }
	    if (*p == '\'')
		p++;			/* skip closing quote */
	}
	else						/* INT, REAL or DOUB */
	{
	    start = p;
	    len = strspn(p, "1234567890+-.Ee");		/* find end of number */
	    if (len == 0)
		my_abort3("Invalid value for keyword %s, element %d",
			key, (void *)(nelem+1));
	    p += len;
	    savechar = *p;
	    *p = '\0';				/* make the number a string */

	    /* It's a real or a double if the rest of them are reals	*/
	    /* or doubles, or it has a decimal point or an 'E' or 'e'	*/ 
	    /* in it.							*/

	    if (strcmp(format, "REAL") == 0 || strcmp(format, "DOUB") == 0 ||
		strchr(start, '.') != 0 || strchr(start, 'E') != 0 || 
		strchr(start, 'e') != 0)
	    {						/* REAL or DOUB */
		sig_digits = count_sig_digits (start);

		/* If we've already found DOUB's, don't go back to REAL */
		/* despite what sig_digits might say.			*/
		if (sig_digits <= 6 && strcmp(format, "DOUB") != 0)
		{
		    /* Can't use REAL format if we're currently at STRING or*/
		    /* or we're not REAL and we can't change it.	    */

		    if ((*format != '\0' && strcmp(format, "REAL") != 0 &&
		         !change_format)  ||  (strcmp(format, "STRING") == 0))
		        my_abort3(
			    "Can't change format from %s to REAL for keyword %s",
			    format, key);
		    strcpy(format, "REAL");
		}
		else
		{
		    /* Can't use DOUB format if we're currently at STRING or*/
		    /* we're not DOUB and we can't change it.  		    */

		    if ((*format != '\0' && strcmp(format, "DOUB") != 0 &&
		    	 !change_format)  ||  (strcmp(format, "STRING") == 0))
		        my_abort3(
			    "Can't change format from %s to DOUB for keyword %s",
			    format, key);
		    strcpy(format, "DOUB");
		}

		if (value->data != NULL && *format == 'R')
		    sscanf(start, "%f", value->data+(nelem*value->maxlength));
		else if (value->data != NULL && *format == 'D')
		    sscanf(start, "%lf", value->data+(nelem*value->maxlength));
		else
		{	/* not maxlen of 4 in case we change to string later */
		    value->maxlength = MAX(value->maxlength, len+1);
		    value->nelements = nelem+1;
		}
	    }
	    else
	    {							/* INT */
		/* Can't change format to INT from anything else */

		if (*format != '\0' && strcmp(format, "INT") != 0)
		    my_abort3(
			"Can't change format from %s to INT for keyword %s",
			format, key);
		strcpy(format, "INT");

		if (value->data != NULL)
		    sscanf(start, "%d", value->data+(nelem*value->maxlength));
		else
		{	/* not maxlen of 4 in case we change to string later */
		    value->maxlength = MAX(value->maxlength, len+1);
		    value->nelements = nelem+1;
		}
	    }
	    *p = savechar;
	}

/* Done with one element, now check for others */

	nelem++;

	p += strspn(p, " \t");		/* skip white space */

	if (paren)			/* look for multi-values */
	{
	    if (*p == ')')
	    {
		done = TRUE;
		p++;
	    }
	    else if (*p == ',')		/* skip commas */
		*p++;
	    else if (*p == '\0')
		my_abort2("No closing parenthesis for multi-valued keyword %s",
			key);
	}
	else
	    done = TRUE;

	p += strspn(p, " \t");		/* skip white space */
    }

    if (!paren && *p == ')')
	my_abort2("Extra closing parenthesis found for keyword %s", key);

    return p;
}

/* Prints an error message and aborts */
void my_abort3(char *str, char *p1, char *p2)
{
    char message[80];

    sprintf(message, str, p1, p2);
    zvmessage(message,"");
    zabend();
}
void my_abort2(char *str, char *p1)
{
    char message[80];

    sprintf(message, str, p1);
    zvmessage(message,"");
    zabend();
}
void my_abort1(char *str)
{
    char message[80];

    sprintf(message, str);
    zvmessage(message,"");
    zabend();
}





int count_sig_digits (char *str)
{
	char *fsd;
	int len;

	fsd = strpbrk (str, "123456789");
        if (fsd == NULL)		/* Only 0 and . are present */
		return 1;
	len = strspn (fsd, "0123456789.");

	if (len != strspn (fsd, "0123456789"))
		return len - 1;
	else return len;
}


int isdouble(str)

/* Determines if string has more than single precision detail */

char *str;
{
    double n1, n2;
    float nf;

    sscanf (str, "%lf", &n1);
    nf = (float) n1;
    n2 = (double) nf;

    return (n2 == n1);
}


void add_system_label(void)
{
/* add_system_label 						*/
/*								*/
/* 	Take an unlabeled file and copy with a label		*/
/*								*/
/*	Local variables:					*/
/*								*/
/*  in_unit	-- Input file unit number			*/
/*  out_unit	-- Output file unit number			*/
/*  count	-- Count of parameter from zvparm		*/
/*  defaulted	-- Obsolete flag from zvparm (unused)		*/
/*  comment	-- User comment label to add (always STRING)	*/
/*  nl		-- Number of lines in image			*/
/*  ns		-- Number of samples per line			*/
/*  format	-- format of image (see pdf)			*/
/*  nbb         -- Number of binary bytes to precede each line  */
/*  nlb         -- Number of binary header lines                */
/*  nrecs       -- Number of records or total lines in image    */
/*  line	-- line number counter				*/
/*  host	-- Host type name for input file		*/
/*  intfmt,realfmt -- Integer and real formats for input file	*/
/*  bhost,bintfmt,brealfmt -- Binary label versions		*/
/*  bltype	-- Type of binary label				*/
/*  recsize	-- Size of a record, needed for binary labels	*/
/*  pixsize	-- Size of a pixel, needed for binary labels	*/

    int in_unit,out_unit,count,defaulted,nl,ns,nb,nbb,nlb,nrecs;
    int recsize, pixsize;
    int line,samp,band;
    char comment[81],format[8],org[8];
    char host[33], intfmt[33], realfmt[33];
    char bhost[33], bintfmt[33], brealfmt[33], bltype[33];

    struct		/* Window into the input		*/
    {
	int sl;		/* Starting line of input		*/
	int ss;		/* Starting sample of input		*/
	int nl;		/* Number of lines			*/
	int ns;		/* Number of samples			*/
    } window;

    struct		/* Window into the input		*/
    {
	int sb;		/* Starting band of input		*/
	int nb;		/* Number of bands			*/
    } bands;

/* Get unit numbers */
    status = zvunit(&in_unit,"INP",1, NULL);
    abort_on_error(in_unit);
    status = zvunit(&out_unit,"OUT",1, NULL);
    abort_on_error(out_unit);

/* Get file type info. */
    zvp("ORG",org,&count);

    zvp("FORMAT",format,&count);

/* Get the host and data formats */
    zvp("HOST", host, &count);
    zveaction("sa","");			/* Abort if invalid host */
    zvhost(host, intfmt, realfmt);
    zveaction("", "");

    zvpcnt("INTFMT",&count);
    if (count != 0)
	zvp("INTFMT", intfmt, &count);		/* override zvhost setting */
    zvpcnt("REALFMT",&count);
    if (count != 0)
	zvp("REALFMT", realfmt, &count);	/* override zvhost setting */

    if (zvptst("BINARY"))
    {

/* Get file size info and open accordingly.  Note that we must treat	*/
/* the input in terms of bytes in case there's a weird binary prefix.	*/
	zvp("NBB",&nbb,&count);
	if (nbb) zvadd(in_unit, "U_NBB",nbb, NULL);

	zvp("NLB",&nlb,&count);
	if (nlb) zvadd(in_unit, "U_NLB",nlb, NULL);

	zvp("NL",&nl,&count);
	if (count != 0) status = zvadd(in_unit,"U_NL",nl, NULL);

	zvp("NS",&ns,&count);
	if (count != 0)
	{
	    if (strcmp(org,"BIP") == 0)		/* No change for BIP */
		zvadd(in_unit, "U_NS",ns, NULL);
	    else
	    {
		zvpixsize(&pixsize, format, intfmt, realfmt);
		recsize = ns * pixsize + nbb;
		zvadd(in_unit, "U_NS",recsize, NULL);
	    }
	}

	zvp("NB",&nb,&count);
	if (strcmp(org,"BIP") != 0)		/* No change unless BIP */
	    zvadd(in_unit, "U_NB",nb, NULL);
	else
	{
	    zvpixsize(&pixsize, format, intfmt, realfmt);
	    recsize = nb * pixsize + nbb;
	    zvadd(in_unit, "U_NB",recsize, NULL);
	}

	status = zvopen(in_unit,"COND","NOLABELS","I_FORMAT","BYTE",
		"U_FORMAT","BYTE","U_ORG",org,"OPEN_ACT","SA","IO_ACT","SA",
		"CONVERT","OFF", "HOST",host, "INTFMT",intfmt,
		"REALFMT",realfmt, NULL);

	if (strncmp(org, "BIP", 3) == 0)
	    nrecs = nl * ns + nlb;
	else
	    nrecs = nl * nb + nlb;

/* Get binary host and data formats */
	zvpcnt("BHOST",&count);
	if (count != 0)
	    zvp("BHOST", bhost, &count);
	else
	    strcpy(bhost, host);
	zveaction("sa","");			/* Abort if invalid host */
	zvhost(bhost, bintfmt, brealfmt);
	zveaction("", "");

	zvpcnt("BINTFMT",&count);
	if (count != 0)
	    zvp("BINTFMT", bintfmt, &count);	/* override zvhost setting */
	zvpcnt("BREALFMT",&count);
	if (count != 0)
	    zvp("BREALFMT", brealfmt, &count);	/* override zvhost setting */

	zvp("BLTYPE", bltype, &count);

	/* open the output file */
	zvopen(out_unit, "OP","WRITE", "OPEN_ACT","SA", "IO_ACT","SA",
			"COND","BINARY", "U_NLB",nlb, "U_NBB",nbb,
			"U_ORG",org, "U_NL",nl, "U_NS",ns, "U_NB",nb,
			"CONVERT","OFF", "HOST",host, "INTFMT",intfmt,
			"REALFMT",realfmt, "BHOST",bhost, "BINTFMT",bintfmt,
			"BREALFMT",brealfmt, "BLTYPE",bltype,
			"U_FORMAT",format, "O_FORMAT",format, NULL);

	{
	    char *buf = NULL;
	    int rec_size;
	    status = zvget(in_unit, "RECSIZE", &rec_size, NULL);
	    abort_on_error(in_unit);
	    buf = (char *)malloc(rec_size);
	    if (buf == NULL) {
	        zvmessage("Out of memory!!!", "");
	        zabend();
	    }

	    for (line=0; line<nrecs; line++)
	    {
	        zvread(in_unit, buf, NULL);
	        zvwrit(out_unit, buf, NULL);
	    }
	    free(buf);
	}
    }
    else		/* Not binary */
    {
	zvp("NL",&nl,&count);
	if (count != 0)
	    zvadd(in_unit, "U_NL",nl, NULL);

	zvp("NS",&ns,&count);
	if (count != 0)
	    zvadd(in_unit, "U_NS",ns, NULL);

	zvp("NB",&nb,&count);
	zvadd(in_unit, "U_NB",nb, NULL);

/* Open the input file	*/
	status = zvopen(in_unit,"COND","NOLABELS","I_FORMAT",format,
		"U_FORMAT",format,"U_ORG",org,"OPEN_ACT","SA","IO_ACT","SA",
		"CONVERT","OFF", "HOST",host, "INTFMT",intfmt,
		"REALFMT",realfmt, NULL);

	zvparm("WINDOW",&window,&count,&defaulted,0,0);
	if (count==0)
	{
	    window.sl = window.ss = 1;
	    status = zvget(in_unit,"NL",&window.nl,"NS",&window.ns, NULL);
	}

	zvparm("BANDS",&bands,&count,&defaulted,0,0);
	if (count==0)
	{
	    bands.sb =  1;
	    bands.nb =  nb;
	}

	/* open the output file */
	status = zvopen(out_unit,"OP","WRITE","OPEN_ACT","SA","IO_ACT","SA",
			"U_ORG",org,"U_NL",window.nl,"U_NS",window.ns,
			"U_NB",bands.nb, "CONVERT","OFF", "HOST",host,
			"INTFMT",intfmt, "REALFMT",realfmt,
			"U_FORMAT",format, "O_FORMAT",format, NULL);

	/* Now copy over the data */

	{
	    char *buf = NULL;
	    int rec_size;
	    status = zvget(out_unit, "RECSIZE", &rec_size, NULL);
	    abort_on_error(out_unit);
	    buf = (char *)malloc(rec_size);
	    if (buf == NULL) {
	        zvmessage("Out of memory!!!", "");
	        zabend();
	    }

	    if ( strncmp( org, "BSQ",3 ) == 0 )
	        for (band = bands.sb; band <= (bands.nb + bands.sb - 1); band++)
		    for (line=window.sl; line<= (window.nl+window.sl-1); line++)
		    {
		        status = zvread(in_unit,buf,"SAMP",window.ss,
				"LINE",line, "BAND",band,"NSAMPS",window.ns, NULL);
		        status = zvwrit(out_unit,buf, NULL);
		    }
	    else if ( strncmp( org, "BIL",3 ) == 0 )
	        for (line=window.sl; line <= (window.nl + window.sl-1); line++)
		    for (band=bands.sb; band <= (bands.nb + bands.sb-1); band++)
		    {
		        status =zvread(in_unit,buf,"SAMP",window.ss,"LINE",line,
				   "BAND",band,"NSAMPS",window.ns, NULL);
		        status = zvwrit(out_unit,buf, NULL);
		    }
	    else				/* BIP */
	        for (line = window.sl; line <= (window.nl+window.sl-1); line++)
		    for (samp=window.ss; samp<=(window.ns+window.ss-1); samp++)
		    {
		        status = zvread(in_unit,buf,"SAMP",samp,"LINE",line,
				   "BAND",bands.sb,"NBANDS",bands.nb, NULL);
		        status = zvwrit(out_unit,buf, NULL);
		    }
	    free(buf);
	}
    }

/* See if comment was given and if so add it in */
    zvparm("COMMENT",comment,&count,&defaulted,0,0);
    if (count == 1)
    {
	status = zladd(out_unit,"HISTORY","COMMENT",comment,
			"FORMAT","STRING", NULL);
	abort_on_error(out_unit);
    }

/* Close files and exit */
    status = zvclose(in_unit, NULL);
    status = zvclose(out_unit, NULL);

}


void concat_labels(void)
{
/*	concat_labels						*/
/*								*/
/*	Concatenates (merges) all history or property labels,	*/
/*	or a single label set, from input 1 into input 2.	*/
/*								*/
/*	Local variables:					*/
/*								*/
/*  source_unit	-- Unit number for label source (inp #1)	*/
/*  in_unit	-- Input file unit number (inp #2)		*/
/*  out_unit	-- Output file unit number			*/
/*  task_cnt	-- Flags if TASK parameter given		*/
/*  prop_cnt	-- Flags if PROPERTY parameter given		*/
/*  count	-- Count of parameter from zvparm		*/
/*  data_copy   -- Flag indicating whether or not to copy data  */
/*  		   to an output file.				*/
/*  single_set	-- True if only one label set is to be xferred	*/
/*  type	-- Label type, from TYPE parameter		*/
/*  set_opt	-- Optional arg for zladd			*/
/*  set_name	-- Value of TASK or PROPERTY parameter		*/
/*  set_names	-- List of all source set names from zlp/hinfo	*/
/*  instance	-- instance of TASK or PROPERTY			*/
/*  instances	-- List of all src set instances from zlp/hinfo	*/
/*  nsets_label	-- # of sets in source				*/

    int source_unit, in_unit, out_unit;
    int task_cnt, prop_cnt;
    int i, count, status;
    int data_copy, single_set;
    int nlb, nbb;		/* BINARY header and prefix lengths */
    char type[12], set_opt[12];
    char set_name[MAX_LABEL_KEY_SIZE+1];
    char set_names[MAX_SETS][MAX_LABEL_KEY_SIZE+1];
    int instance;
    int instances[MAX_SETS];
    int nsets_label;
    char *p;

/* Open input 1 (the source of the labels */
    status = zvunit(&source_unit, "INP", 1, NULL);
    abort_on_error(source_unit);
    status = zvopen(source_unit, "OPEN_ACT","SA", "IO_ACT","SA", NULL);
    abort_on_error(source_unit);

/* Find out whether data is to be copied */
    zvpcnt("OUT",&count);
    data_copy = (count == 1) ? TRUE : FALSE;

/* Get the file unit numbers */
    if (data_copy) 
    {
	zvselpi(2);		/* inp #2 is the primary */

	status = zvunit(&in_unit,"INP",2, NULL); /* inp2 supplies data, orig lbl */
	abort_on_error(in_unit);

	status = zvunit(&out_unit,"OUT",1, NULL);
	abort_on_error(out_unit);

	status = zvopen(in_unit,"OPEN_ACT","SA","IO_ACT","","COND","BINARY", NULL);
	status = zvget(in_unit,"NLB",&nlb,"NBB",&nbb, NULL);
	if (status != 1)
	    nlb = nbb = 0;
	status = zvopen(out_unit,"OP","WRITE","OPEN_ACT","SA","COND","BINARY",
			"U_NLB",nlb,"U_NBB",nbb, NULL);
    }
    else	/* open for update */
    {
	status = zvunit(&out_unit,"INP",2, NULL);
	abort_on_error(out_unit);

	status = zvopen(out_unit,"OP","UPDATE","OPEN_ACT","SA", NULL);
    }

/* Get type of labels to deal with (HISTORY, PROPERTY, or BOTH) */

    zvpcnt("TASK", &task_cnt);
    zvpcnt("PROPERTY", &prop_cnt);

    if (task_cnt != 0 && prop_cnt != 0)
	my_abort1("Cannot specify both TASK and PROPERTY");

    zvp("TYPE",type,&count);
    if (strcmp(type, "BOTH") == 0)	/* TASK/PROPERTY will override */
    {
	if (prop_cnt != 0)
	    strcpy(type,"PROPERTY");
	else if (task_cnt != 0)
	    strcpy(type,"HISTORY");
    }
    else				/* Type given, check for consistency */
    {
	if (strcmp(type,"HISTORY") == 0 && prop_cnt != 0)
	    my_abort1("Cannot use PROPERTY parameter if TYPE=HISTORY");
	if (strcmp(type,"PROPERTY") == 0 && task_cnt != 0)
	    my_abort1("Cannot use TASK parameter if TYPE=PROPERTY");
    }
    single_set = (prop_cnt != 0 || task_cnt != 0);
    if (single_set)
    {
	if (strcmp(type,"PROPERTY") == 0)
	{
	    strcpy(set_opt,"PROPERTY");	/* optional arg for zl routines */
	    zvp("PROPERTY", set_name, &count);
	}
	else
	{
	    strcpy(set_opt,"HIST");
	    zvp("TASK", set_name, &count);
	}
	zvp("INSTANCE", &instance, &count);

	/* Convert set name to upper case for comparing strings */
	for (p=set_name; *p != '\0'; p++)
	{
	    if (islower(*p))
		*p = toupper(*p);
	}
    }
    else
	strcpy(set_name, "");

    if (single_set)
    {
	concat_set(source_unit, out_unit, type, set_opt, set_name, instance);
    }
    else
    {
	if (strcmp(type, "BOTH") == 0 || strcmp(type, "PROPERTY") == 0)
	{
	    nsets_label = MAX_SETS;
	    status = zlpinfo(source_unit, (char *)set_names, &nsets_label,
		"INST_NUM", instances, "ULEN",MAX_LABEL_KEY_SIZE+1, NULL);
	    abort_on_error(out_unit);

	    for (i=0; i < nsets_label; i++)
	    {
		concat_set(source_unit, out_unit, "PROPERTY", "PROPERTY",
			set_names[i], instances[i]);
	    }
	}
	if (strcmp(type, "BOTH") == 0 || strcmp(type, "HISTORY") == 0)
	{
	    nsets_label = MAX_SETS;
	    status = zlhinfo(source_unit, (char *)set_names, instances,
			 &nsets_label, "ULEN",MAX_LABEL_KEY_SIZE+1, NULL);
	    abort_on_error(out_unit);

	    for (i=0; i < nsets_label; i++)
	    {
		concat_set(source_unit, out_unit, "HISTORY", "HIST",
			set_names[i], instances[i]);
	    }
	}
    }

/* Copy the data over if requested */
    if (data_copy)
    {
	char *buf = NULL;
	int rec_size;
	status = zvget(in_unit, "RECSIZE", &rec_size, NULL);
	abort_on_error(in_unit);
	buf = (char *)malloc(rec_size);
	if (buf == NULL) {
	    zvmessage("Out of memory!!!", "");
	    zabend();
	}

	while (TRUE)
	{   status = zvread(in_unit,buf, NULL);
	    if (status == END_OF_FILE) break;
	    abort_on_error(in_unit);
	    status = zvwrit(out_unit,buf, NULL);
	    abort_on_error(out_unit);
	}
	free(buf);

/* Close the files and exit */
	status = zvclose(in_unit, NULL);
    }

    status = zvclose(out_unit, NULL);

    status = zvclose(source_unit, NULL);
}


void concat_set(
    int source_unit,			/* label source and destination units */
    int out_unit,
    char *type,				/* HISTORY or PROPERTY */
    char *set_opt,			/* HIST or PROPERTY, for zladd opts */
    char *set_name,			/* Name of set to transfer */
    int instance			/* Instance number to transfer */
)
{
/*	concat_set						*/
/*								*/
/*	Concatenates (merges) a single history or property set	*/
/*	from source_unit to out_unit.				*/
/*								*/
/*	Local variables:					*/
/*								*/
/*  header_name	-- Name of set header keyword "TASK","PROPERTY"	*/
/*  key		-- Keyword of each item				*/
/*  format	-- Format of each item				*/
/*  set_names	-- List of all output set names from zlp/hinfo	*/
/*  instances	-- List of all out set instances from zlp/hinfo	*/
/*  nsets_label	-- # of sets in source				*/
/*  out_inst	-- Instance number for set in the output	*/
/*  value	-- buffer for holding label value		*/

    int i, status;
    char header_name[MAX_LABEL_KEY_SIZE+1];
    char key[33], format[12];
    char set_names[MAX_SETS][MAX_LABEL_KEY_SIZE+1];
    int instances[MAX_SETS];
    int nsets_label;
    int out_inst;
    struct multival value;

    value.data = NULL;
    value.nelements = 0;
    value.maxlength = 0;
    value.allocsize = 0;

    if (strcmp(type, "HISTORY") == 0)
	strcpy(header_name, "TASK");
    else
	strcpy(header_name, "PROPERTY");

/* Set current key to set name */
    status = zlinfo(source_unit, type, header_name, format,
		&value.maxlength, &value.nelements,
		set_opt, set_name,
		"INSTANCE",instance,
		"STRLEN", &value.maxlength, NULL);
    abort_on_error(source_unit);

/* Determine the instance number for the output */

    nsets_label = MAX_SETS;
    if (strcmp(type, "PROPERTY") == 0)
    {
	status = zlpinfo(out_unit, (char *)set_names, &nsets_label,
			"INST_NUM", instances, "ULEN",MAX_LABEL_KEY_SIZE+1, NULL);
    }
    else
    {
	status = zlhinfo(out_unit, (char *)set_names, instances, &nsets_label,
			"ULEN",MAX_LABEL_KEY_SIZE+1, NULL);
    }
    abort_on_error(out_unit);

    out_inst = 1;

    for (i=nsets_label-1; i >= 0; i--)
    {
	if (strcmp(set_names[i], set_name) == 0)
	{
	    out_inst = instances[i] + 1;
	    break;
	}
    }

/* Create the history set in the output if necessary.  This is a trick;	*/
/* we add a TASK label to the end of the last existing task, which is	*/
/* all the RTL really needs to see, then let the loop below add USER	*/
/* and DAT_TIM.								*/
/* Note:  Property label sets are automatically created.		*/

    if (strcmp(type, "HISTORY") == 0)
    {
	status = zladd(out_unit, type, "TASK", set_name, "FORMAT", "STRING",
			"MODE", "ADD", NULL);/*"HIST", set_names[nsets_label-1],
			"INSTANCE", instances[nsets_label-1], 0); !!!!*/
	/* Ignore errors in the above; it will return NO_SUCH_KEY */
    }

/* Cycle through each key in the subset */
    while (TRUE)
    {

/* Get next keyword */
	status = zlninfo(source_unit,key,format,&value.maxlength,
		&value.nelements, NULL);
	if ((status == END_OF_LABEL) || (strcmp(key,"TASK") == 0) ||
		(strcmp(key,"PROPERTY") == 0)) break;
	break_on_error(source_unit);
	if (strcmp(format, "STRING") == 0)
	    value.maxlength++;	/* leave room for null string terminator */

/* Get next value */
	if (value.maxlength * value.nelements > value.allocsize)
	{
	    if (value.data != NULL)
		free(value.data);
	    value.data = malloc(value.maxlength * value.nelements);
	    value.allocsize = value.maxlength * value.nelements;
	    if (value.data == NULL)
	    {
		zvmessage("Out of memory!!!", "");
		zabend();
	    }
	}
	status = zlget(source_unit,type,key,value.data,
		set_opt,set_name, "INSTANCE",instance,
		"FORMAT",format, "ULEN", value.maxlength,
		"NELEMENT", value.nelements, NULL);
	continue_on_error(source_unit);

/* Add value to output label */

	status = zladd(out_unit, type, key, value.data,
		set_opt,set_name, "INSTANCE", out_inst,
		"FORMAT", format, "ULEN", value.maxlength,
		"NELEMENT", value.nelements, "MODE", "ADD", NULL);

    }	 	/* End of while loop */

    if (value.data != NULL)
	free(value.data);
}


void delete_items(void)
{
/*	delete_items						*/
/*								*/
/*	Delete all or part of the history or property label	*/
/*								*/
/*	Local variables:					*/
/*								*/
/* count -- count returned from zvparm				*/
/* defaulted -- flag indicating whether parameter was defaulted	*/
/* format -- format of keyword returned from zlinfo and zlninfo	*/
/* i -- increment variable					*/
/* in_unit -- input file unit number				*/
/* inst_to_delete -- list of instances for sets_to_delete	*/
/* instances -- task/prop instances corresponding to set_names	*/
/* j -- increment variable					*/
/* key -- current key name from zlninfo				*/
/* keys_to_delete -- list of keys to delete			*/
/* maxlen -- maxlength parameter for zlinfo and zlninfo		*/
/* n_inst -- count of INSTNCES parameter			*/
/* nelem -- nelements parameter for zlinfo and zlninfo		*/
/* nsets_label -- number of history or property subsets		*/
/* nkeys -- count of KEYS parameter				*/
/* nsets -- count of TASKS or PROPERTY parameter		*/
/* out_unit -- output file unit number				*/
/* revised_instances -- revised list of instances for passing	*/
/*			to zldel				*/
/* subset -- index variable for looping through subsets		*/
/* set_names -- list of all history or property subset names	*/
/* sets_to_delete -- list of tasks or properties to delete	*/
/* element_to_delete -- ELEMENT param, starting el for deletion	*/
/* nelement_to_delete -- NELEMENT param, num of el's to delete	*/
/* n_elem, n_nelem -- count of ELEMENT and NELEMENT parameter	*/
/* nret -- # of elements actually deleted by zldel call		*/

    int count,in_unit,out_unit,nsets_label,subset,maxlen,nelem,nsets,nkeys;
    int instances[MAX_SETS],revised_instances[MAX_SETS];
    int inst_to_delete[DEL_COUNT],n_inst,i,j,data_copy;
    char keys_to_delete[DEL_COUNT][MAX_LABEL_KEY_SIZE+1];
    char sets_to_delete[DEL_COUNT][MAX_LABEL_KEY_SIZE+1];
    int element_to_delete, nelement_to_delete;
    int n_elem, n_nelem, nret;
    int nlb, nbb;
    int task_cnt, prop_cnt;
    char type[12], set_opt[12];

    char format[12],key[33],*p;
    char set_names[MAX_SETS][MAX_LABEL_KEY_SIZE+1];

/* Boolean variables */
    int defaulted,use_all_sets,delete_whole_subset,del_all_keys_in_set;
    int care_about_inst,set_header;

/* Find out whether data is to be copied */
    zvpcnt("OUT",&count);
    data_copy = (count == 1) ? TRUE : FALSE;

/* Get the file unit numbers */
    if (data_copy) 
    {
	status = zvunit(&in_unit,"INP",1, NULL);
	abort_on_error(in_unit);

	status = zvunit(&out_unit,"OUT",1, NULL);
	abort_on_error(out_unit);

	status = zvopen(in_unit,"OPEN_ACT","SA","IO_ACT","","COND","BINARY", NULL);
	status = zvget(in_unit,"NLB",&nlb,"NBB",&nbb, NULL);
	if (status != 1)
	    nlb = nbb = 0;
	status = zvopen(out_unit,"OP","WRITE","OPEN_ACT","SA","COND","BINARY",
			"U_NLB",nlb,"U_NBB",nbb, NULL);
    }
    else	/* open for update */
    {
	status = zvunit(&out_unit,"INP",1, NULL);
	abort_on_error(out_unit);

	status = zvopen(out_unit,"OP","UPDATE","OPEN_ACT","SA", NULL);
    }

/* Get type of labels to deal with (HISTORY or PROPERTY) */

    zvpcnt("TASKS", &task_cnt);
    zvpcnt("PROPERTY", &prop_cnt);

    if (task_cnt != 0 && prop_cnt != 0)
	my_abort1("Cannot specify both TASKS and PROPERTY");

    zvp("TYPE",type,&count);
    if (count == 0)			/* No type specified */
    {
	if (prop_cnt != 0)
	    strcpy(type,"PROPERTY");
	else				/* TASK given or default case */
	    strcpy(type,"HISTORY");
    }
    else				/* Type given, check for consistency */
    {
	if (strcmp(type,"HISTORY") == 0 && prop_cnt != 0)
	    my_abort1("Cannot use PROPERTY parameter if TYPE=HISTORY");
	if (strcmp(type,"PROPERTY") == 0 && task_cnt != 0)
	    my_abort1("Cannot use TASK parameter if TYPE=PROPERTY");
    }

    if (strcmp(type,"PROPERTY") == 0)
	strcpy(set_opt,"PROPERTY");	/* optional arg for zl routines */
    else
	strcpy(set_opt,"HIST");

/* Get history or property label set information */
    nsets_label = MAX_SETS;

    if (strcmp(type,"PROPERTY") == 0)
    {
	status = zlpinfo(out_unit, (char *)set_names, &nsets_label,
			"INST_NUM", instances, "ULEN",MAX_LABEL_KEY_SIZE+1, NULL);
    }
    else
	status = zlhinfo(out_unit, (char *)set_names, instances, &nsets_label,
			"ulen",MAX_LABEL_KEY_SIZE+1, NULL);
    abort_on_error(out_unit);

/* Make revised instance list the same as the original list */
    for (j = 0; j < nsets_label; j++) revised_instances[j] = instances[j];

/* Initialize boolean flags */
    use_all_sets = FALSE;
    delete_whole_subset = FALSE;
    del_all_keys_in_set = FALSE;
    care_about_inst = FALSE;

/* Get tasks for scope of deletion */
    if (strcmp(type,"PROPERTY")==0)
	zvparm("PROPERTY", sets_to_delete, &nsets, &defaulted,
		DEL_COUNT, MAX_LABEL_KEY_SIZE+1);
    else
	zvparm("TASKS", sets_to_delete, &nsets, &defaulted,
		DEL_COUNT, MAX_LABEL_KEY_SIZE+1);

/* Convert all set names to upper case for comparing strings */
    for (i=0; i<nsets; i++)
    {
	for (p=sets_to_delete[i]; *p != '\0'; p++)
	    if (islower(*p))
		*p = toupper(*p);
    }

/* If no set names were given, then use all sets for scope */
    use_all_sets = (nsets == 0);

/* Get keys to delete */
    zvparm("KEYS", keys_to_delete, &nkeys, &defaulted,
		DEL_COUNT, MAX_LABEL_KEY_SIZE+1);

    if (nkeys == 0)
    {
	delete_whole_subset = TRUE;
	del_all_keys_in_set = TRUE;
    }
    else
    {
/* Convert all keywords to upper case for comparing strings */
	for (i=0; i<nkeys; i++)
	{
	    for (p=keys_to_delete[i]; *p != '\0'; p++)
		if (islower(*p))
		    *p = toupper(*p);
	}

/* See if user gave keyword /ALL */
        if (strcmp(keys_to_delete[0],"/ALL") == 0)
	{
	    del_all_keys_in_set = TRUE;
	}
	else 
	{
	    if (   string_on_list("TASK",keys_to_delete,nkeys)
		|| string_on_list("USER",keys_to_delete,nkeys)
		|| string_on_list("DAT_TIM",keys_to_delete,nkeys)
		|| string_on_list("PROPERTY",keys_to_delete,nkeys))
	    {
		zvmessage("KEYS contained illegal keyword", "");
		zvmessage("Keywords TASK, USER, DAT_TIM, and PROPERTY not allowed", "");
		zabend();
	    }
	}	/* end else */
     }		/* end else */

/* Get any instances for narrowing scope */
    zvparm("INSTNCES",inst_to_delete,&n_inst,&defaulted,DEL_COUNT,0);
    care_about_inst = (n_inst > 0);

/* Get starting element to delete for multi-valued items */
    zvp("ELEMENT", &element_to_delete, &n_elem);
    if (n_elem == 0)
	element_to_delete = 1;		/* defaulted... start at beginning */
    else
    {						/* ELEMENT given */
	if (delete_whole_subset)
	    my_abort1("KEYS must be specified if ELEMENT is used");
    }

/* Get number of elements to delete for multi-valued items */
    zvp("NELEMENT", &nelement_to_delete, &n_nelem);
    if (n_nelem == 0)
	    nelement_to_delete = -1;	/* defaulted... delete all elements */
    else
    {						/* NELEMENT given */
	if (delete_whole_subset)
	    my_abort1("KEYS must be specified if NELEMENT is used");
    }

/* Now cycle through each subset and check deletion scope */
    for (subset = 0; subset < nsets_label; subset++)
    {   if (((! use_all_sets) && 
	        (! string_on_list(set_names[subset],sets_to_delete,nsets)))
	      || (care_about_inst && 
	        (! int_on_list(instances[subset],inst_to_delete,n_inst))))
	    continue;

/* set label pointer to this subset */
	if (strcmp(type,"PROPERTY")==0)
	    strcpy(key,"PROPERTY");
	else
	    strcpy(key,"TASK");
	status = zlinfo(out_unit, type, key, format, &maxlen, &nelem,
	    set_opt,set_names[subset],"INSTANCE",revised_instances[subset], NULL);
	abort_on_error(out_unit);

/* Now cycle through all keywords in the subset */
	do
	{   set_header = (strcmp(key,"TASK") == 0)
			  || (strcmp(key,"USER") == 0)
			  || (strcmp(key,"DAT_TIM") == 0)
			  || (strcmp(key,"PROPERTY") == 0);
	    if (   (del_all_keys_in_set && (! set_header))
		|| (string_on_list(key,keys_to_delete,nkeys)))
	    {
		status = zldel(out_unit, type, key, set_opt,set_names[subset],
		    "INSTANCE",revised_instances[subset],
		    "ELEMENT", element_to_delete,
		    "NELEMENT", nelement_to_delete, "NRET", &nret, NULL);
		abort_on_error(out_unit);
		if (nret != 0 &&
		    (element_to_delete == 1 && element_to_delete+nret > nelem))
		{
		    sprintf(msg,"Keyword %s deleted",key);
		    zvmessage(msg, "");
		}
		else if (nret == 1)
		{
		    sprintf(msg, "Element %d deleted from keyword %s",
				element_to_delete, key);
		    zvmessage(msg, "");
		}
		else if (nret != 0)
		{
		    sprintf(msg,
			    "Elements %d through %d deleted from keyword %s",
			    element_to_delete, element_to_delete+nret-1, key);
		    zvmessage(msg, "");
		}
	    }

/* Increment to next keyword */
	    status = zlninfo(out_unit,key,format,&maxlen,&nelem, NULL);

	} while ((strcmp(key,"TASK") != 0) && (strcmp(key,"PROPERTY") != 0) &&
		 (status == 1));

/* Delete the set header if the whole set is to be deleted */
	if (delete_whole_subset)
	{
	    if (strcmp(type,"PROPERTY") == 0)
	    {
		status = zldel(out_unit, "PROPERTY", "PROPERTY",
			"PROPERTY",set_names[subset],
			"INSTANCE",revised_instances[subset], NULL);
		abort_on_error(out_unit);
		sprintf(msg,"Property subset %s deleted",set_names[subset]);
	    }
	    else
	    {
		status = zldel(out_unit, "HISTORY", "USER",
			"HIST", set_names[subset],
			"INSTANCE",revised_instances[subset], NULL);
		abort_on_error(out_unit);

		status = zldel(out_unit, "HISTORY", "DAT_TIM",
			"HIST", set_names[subset],
			"INSTANCE",revised_instances[subset], NULL);
		abort_on_error(out_unit);

		status = zldel(out_unit, "HISTORY", "TASK",
			"HIST", set_names[subset],
			"INSTANCE",revised_instances[subset], NULL);
		abort_on_error(out_unit);
		sprintf(msg,"History subset %s deleted",set_names[subset]);
	    }
	    revise_instance_list(set_names,subset,revised_instances,
				nsets_label);

	    zvmessage(msg, "");

	}	/* End if block */
    }		/* End for loop */

/* Copy the data over if requested */
    if (data_copy)
    {
	char *buf = NULL;
	int rec_size;
	status = zvget(in_unit, "RECSIZE", &rec_size, NULL);
	abort_on_error(in_unit);
	buf = (char *)malloc(rec_size);
	if (buf == NULL) {
	    zvmessage("Out of memory!!!", "");
	    zabend();
	}

	while (TRUE)
	{   status = zvread(in_unit,buf, NULL);
	    if (status == END_OF_FILE) break;
	    abort_on_error(in_unit);
	    status = zvwrit(out_unit,buf, NULL);
	    abort_on_error(out_unit);
	}
	free(buf);

/* Close the files and exit */
	status = zvclose(in_unit, NULL);
    }

    status = zvclose(out_unit, NULL);
}

void revise_instance_list(char names[MAX_SETS][MAX_LABEL_KEY_SIZE+1],
	int subset, int new_list[], int list_len)
{
    int i;

    for (i = subset + 1; i < list_len; i++)
    {
	if (strcmp(names[subset],names[i]) == 0)
	    new_list[i] = new_list[i] - 1;
    }
}

int string_on_list(char *string, char list[][MAX_LABEL_KEY_SIZE+1], int list_len)

/*								*/
/*	string_on_list						*/
/*								*/
/*	Boolean function 					*/
/*		Returns true if string is on list		*/
/*		False otherwise					*/
/*								*/
/*	Passed variables:					*/
/*								*/
/* list -- List of C strings 					*/
/* list_len -- number of strings in list			*/
/* string -- string to find on list				*/
/*								*/
/* NOTE:  The inner dimension of list must be MAX_LABEL_KEY_SIZE+1 */
/*								*/
{
    int i;

    for (i = 0; i < list_len; i++)
    {
	if (strcmp(string,list[i]) == 0) return(TRUE);
    }
    return(FALSE);
}


int int_on_list(int intval, int list[], int list_len)
{
    int i;

    for (i = 0; i < list_len; i++)
    {
	if (list[i] == intval) return(TRUE);
    }
    return(FALSE);
}

void edit_label(void){}

void list_label(void)
{
/*	List out an image label					*/
/*								*/
/*	Local variables:					*/
/*								*/
/* asterisks -- row of asterisks for delimiter line		*/
/* count -- count returned from zvparm				*/
/* current_input -- Input file number currently being listed	*/
/* defaulted -- Flag indicating whether parameter was defaulted	*/
/* extent -- Value of keyword EXTENT.  See PDF for description	*/
/* header -- file header message buffer				*/
/* input_file_name -- String with name of current input file	*/
/* number_of_inputs -- Total number of input files given	*/
/* packstr -- string value of PACK keyword.			*/

    int count,current_input,defaulted,number_of_inputs;
    int unit;
    char input_file_name[256],extent[8],packstr[8],header[256];
    static char asterisks[] =
      "************************************************************";

/* Begin execution */

    zvpcnt("INP",&number_of_inputs);

/* Get extent parameter for later use */
    zvparm("EXTENT",extent,&count,&defaulted,0,0);

/* Get PACK parameter and save it in a logical variable for later use */
    zvparm("PACK",packstr,&count,&defaulted,0,0);
    if (strcmp(packstr, "PACK") == 0)
	pack_listed_items = TRUE;
    else
	pack_listed_items = FALSE;

/* Loop through listing each input file */
    for (current_input = 1; current_input <= number_of_inputs; current_input++)
    {

/* Get input file name */
	zvpone("INP",input_file_name,current_input, 0);

/* Capitalization removed 5/92 RGD since it's significant for Unix */
/* Capitalize first letter of file name */
/****	if (input_file_name[0] > 96)	****/
/****	    input_file_name[0] &= 0xDF;	****/

/* Limit length of filename to 256-33 (33 chars is the size of the */
/* "*** File ***" message) so it will fit in the "header" buffer. */
	input_file_name[256-33] = '\0';

/* Print out header for current image */
        zvmessage(asterisks, "");
        sprintf(header,"        ************  File %s ************",
		input_file_name);
	zvmessage(" ", "");			/* newline */
	zvmessage(header, "");

/* Get file unit number */
	status = zvunit(&unit,"INP",current_input, NULL);
	continue_on_error(unit);

/* Open current input */
/* 11/10 lwk:  change action to "SA" when only 1 input, since that is
   the VICAR standard;  but keep "S" when >1 input because this was
   apparently the original intent of the multiple input-file option */
	if (number_of_inputs==1)
	  status = zvopen(unit,"OPEN_ACT","SA", NULL);
	else
	  status = zvopen(unit,"OPEN_ACT","S", NULL);
	if (status != 1) continue;

/* Find extent of listing to be done, and act accordingly */
	switch (extent[0])
	{   case 'D' : dump_all_items(unit);		/* DUMP		*/
	    	       break;
	    case 'S' : list_system_info(unit);		/* SYSTEM	*/
	    	       break;
	    case 'A' : list_system_info(unit);		/* ALL		*/
		       list_property_items(unit);
	    case 'H' : list_history_items(unit);	/* HISTORY	*/
	    	       break;
	    case 'T' : list_task_headers(unit);		/* TASKS	*/
	    	       break;
            case 'P' : list_property_items(unit);
		       break;
	}

/* Output a row of asterisks as a delimiter			*/
        zvmessage(" ", "");
        zvmessage(asterisks, "");

/* Close current input and go on to next if there is one	*/
	status = zvclose(unit, NULL);
    }	/* End of for loop 	*/
}	/* End of list_label()	*/

void list_system_info(int unit)
{
/*	list_system_info					*/
/*								*/
/*	subroutine to list out specific system label 		*/
/*	information to standard output				*/
/*								*/
/*	passed variables:					*/
/*								*/
/* unit -- unit number of file whose label is being looked at	*/
/*								*/
/*								*/
/*	Local variables:					*/
/*								*/
/* dim 		-- number of dimensions in image		*/
/* format	-- pixel format of image			*/
/* message	-- single message line				*/
/* nb		-- Number of bands in image			*/
/* nl		-- Number of lines (per band) in image		*/
/* ns		-- Number of samples per line in image		*/
/* org		-- File organization				*/
/* type		-- Type of image (usually  =IMAGE)		*/
/* bltype       -- Type of binary label if specified		*/
/*								*/

    int dim,nb,nl,ns,nlb,nbb;
    char format[33],message[200],org[33],type[33],host[33],bltype[33];

/* Get the pertinent information from zvget */
    status = zvget(unit,"FORMAT",format,"NL",&nl,"NS",&ns,"NB",&nb,
          "NLB",&nlb,"NBB",&nbb,"DIM",&dim,"ORG",org,"TYPE",type,"HOST",host,
          "BLTYPE",bltype, NULL);
    return_on_error(unit);

/* Start printing out the info */
    sprintf(message,"                %d dimensional %s file",dim,type);
    zvmessage(message, "");

    sprintf(message,"                File organization is %s",org);
    zvmessage(message, "");

    sprintf(message,"                Pixels are in %s format from a %s host",
		format,host);
    zvmessage(message, "");

/* Decide on whether to use nl,ns,nb or n1-n4 */
    if (dim <=3)
    {
	if (dim == 2)
	    sprintf(message,"                %d lines",nl);
	else
	{
	    sprintf(message,"                %d bands",nb);
	    zvmessage(message, "");
	    sprintf(message,"                %d lines per band",nl);
	}
	zvmessage(message, "");

	sprintf(message,"                %d samples per line",ns);
	zvmessage(message, "");

	if (strlen(bltype) != 0)
	    sprintf(message,"                %d lines of binary header of type %s",
			nlb,bltype);
	else
	    sprintf(message,"                %d lines of binary header",nlb);
	zvmessage(message, "");

	sprintf(message,"                %d bytes of binary prefix per line",
			nbb);
	zvmessage(message, "");
    }
/*  else use n1-n4; */

}

void dump_all_items(int unit)
{
/*	dump_all_items						*/
/*								*/
/*	List out every keyword value pair starting from the 	*/
/*	beginning of the system label all the way to the 	*/
/*	end of the label					*/
/*								*/
/*	Local variables:					*/
/*								*/
/* key -- Keyword returned from zlninfo				*/
/* format -- format of label item from zlninfo			*/
/* value -- Structure that describes the value for the item	*/
/* instances -- array of instances corresp. to tasks		*/
/* tasks -- array of task names of history subsets		*/
/* nhist -- number of history subsets				*/
/* subset -- number of current history subset			*/
/* dummy -- holds LENGTH from zlinfo... needed since we need the*/
/* 	    len of the string (via STRLEN optional) instead of  */
/*	    the len of the int or real, since everything is	*/
/*	    treated as a string by the zlget calls.		*/
/*								*/

    int instances[MAX_TASKS],nhist,nprop,subset,length,dummy;
    char key[MAX_LABEL_KEY_SIZE+1],format[12];
    struct multival value;
    char tasks[MAX_TASKS][MAX_LABEL_KEY_SIZE+1];
    char props[MAX_PROPS][MAX_LABEL_KEY_SIZE+1];

    printbuf[0] = '\0';			/* empty the string buffer */
    value.allocsize = 0;
    value.data = NULL;

/***********************************************************************/
/* Loop through system label */

    while (TRUE)
    {

/* Next key */
	status = zlninfo(unit,key,format,&dummy,&value.nelements,
		"STRLEN", &value.maxlength, NULL);
	value.maxlength++;	/* leave room for null string terminator */
	if (status == END_OF_LABEL) return;
	if (status <= 0 && value.data != NULL)
	    free(value.data);
	return_on_error(unit);

/* If end of system label, break out of loop */
        if ((strcmp(key,"TASK") == 0) || (strcmp(key,"PROPERTY") == 0))
	    break;

	if (value.maxlength * value.nelements > value.allocsize)
	{
	    value.data = malloc(value.maxlength * value.nelements);
	    value.allocsize = value.maxlength * value.nelements;
	    if (value.data == NULL)
	    {
		zvmessage("Out of memory!", "");
		zabend();
	    }
	}
/* Get value of current item */
	status = zlget(unit,"SYSTEM",key,value.data,"FORMAT","STRING",
		"NELEMENT", value.nelements, "ULEN", value.maxlength, NULL);
	if (status <= 0 && value.data != NULL)
	    free(value.data);
	return_on_error(unit);

/* Output depending on data format */
	print_key_value_pair(key,&value,format);
    }

    flush_key_value_pair();

/***********************************************************************/
/* Now cycle through property items */

    nprop = MAX_PROPS;
    status = zlpinfo(unit,(char *)props,&nprop, "inst_num", instances,
			"ulen",MAX_LABEL_KEY_SIZE+1, NULL);
    if (status <= 0 && value.data != NULL)
	free(value.data);
    return_on_error(unit);

    if (nprop > 0) {			/* Only if we have something to do */
/* Write out current property name */
	sprintf(msg,"PROPERTY='%s'",props[0]);
	zvmessage(msg, "");

	subset = 0;
	while (TRUE)
	{
/* Next key */
	    status = zlninfo(unit,(char *)key,format,&dummy,&value.nelements,
			"STRLEN", &value.maxlength, NULL);
	    if (status == END_OF_LABEL) break;
	    if (strcmp(key,"TASK") == 0) break;		/* end of props */
	    if (status <= 0 && value.data != NULL)
		free(value.data);
	    return_on_error(unit);
	    value.maxlength++;	/* leave room for null string terminator */

/* Check for property header */
            if (strcmp(key,"PROPERTY") == 0) subset = subset + 1;

/* Get value of current item */
	    if (value.maxlength * value.nelements > value.allocsize)
	    {
		if (value.data != NULL)
		    free(value.data);
		value.data = malloc(value.maxlength * value.nelements);
		value.allocsize = value.maxlength * value.nelements;
		if (value.data == NULL)
		{
		    zvmessage("Out of memory!!", "");
		    zabend();
		}
	    }
	    status = zlget(unit,"PROPERTY",key,value.data,
		"PROPERTY",props[subset], "INSTANCE",instances[subset],
		"FORMAT","STRING",
		"ULEN",value.maxlength, "NELEMENT",value.nelements, NULL);
	    if (status <= 0 && value.data != NULL)
		free(value.data);
	    return_on_error(unit);

/* Output depending on data format */
	    print_key_value_pair(key,&value,format);
	}

	flush_key_value_pair();
    }

/***********************************************************************/
/* Now cycle through history items */

    nhist = MAX_TASKS;
    status = zlhinfo(unit,(char *)tasks,instances,&nhist,"ulen",MAX_LABEL_KEY_SIZE+1, NULL);
    if (status <= 0 && value.data != NULL)
	free(value.data);
    return_on_error(unit);

    if (nhist > 0) {			/* Only if we have something to do */
/* Write out current task */
	sprintf(msg,"TASK='%s'",tasks[0]);
	zvmessage(msg, "");

	subset = 0;
	while (TRUE)
	{
/* Next key */
	    status = zlninfo(unit,key,format,&dummy,&value.nelements,
			"STRLEN", &value.maxlength, NULL);
	    if (status == END_OF_LABEL) break;
	    if (status <= 0 && value.data != NULL)
		free(value.data);
	    return_on_error(unit);
	    value.maxlength++;	/* leave room for null string terminator */

/* Check for task header */
            if (strcmp(key,"TASK") == 0) subset = subset + 1;

/* Get value of current item */
	    if (value.maxlength * value.nelements > value.allocsize)
	    {
		if (value.data != NULL)
		    free(value.data);
		value.data = malloc(value.maxlength * value.nelements);
		value.allocsize = value.maxlength * value.nelements;
		if (value.data == NULL)
		{
		    zvmessage("Out of memory!!", "");
		    zabend();
		}
	    }
	    status = zlget(unit,"HISTORY",key,value.data,"HIST",&tasks[subset],
		"INSTANCE",instances[subset],"FORMAT","STRING",
		"ULEN", value.maxlength, "NELEMENT", value.nelements, NULL);
	    if (status <= 0 && value.data != NULL)
		free(value.data);
	    return_on_error(unit);

/* Output depending on data format */
	    print_key_value_pair(key,&value,format);
	}

	flush_key_value_pair();

    }

    if (value.data != NULL)
	free(value.data);

}

void list_history_items(int unit)
{
/*	list_history_items()					*/
/*								*/
/*	List out all items in the history label			*/
/*								*/
/*	Passed variables:					*/
/*								*/
/* unit -- input file unit number				*/
/*								*/
/*	Local variables:					*/
/*								*/
/* format -- The format of a given label item			*/
/* i -- temporary increment variable				*/
/* instances -- Array containing task instances of " "		*/
/* key -- Name of a label item keyword				*/
/* number_of_tasks -- Number of history subsets in label	*/
/* subset -- Increment variable for subsets			*/
/* task_names -- Array containing task names of history subsets	*/
/* time -- Time returned by zlget				*/
/* username -- User field of a given task			*/
/* value -- Structure that describes the value for the item	*/
/* dummy -- holds LENGTH from zlinfo... needed since we need the*/
/* 	    len of the string (via STRLEN optional) instead of  */
/*	    the len of the int or real, since everything is	*/
/*	    treated as a string by the zlget calls.		*/
/*								*/

    int instances[MAX_TASKS],number_of_tasks,subset,i,dummy;
    char task_names[MAX_TASKS][MAX_LABEL_KEY_SIZE+1];
    char username[132],time[28],key[MAX_LABEL_KEY_SIZE+1],format[32];
    struct multival value;

    printbuf[0] = '\0';			/* empty the string buffer */
    value.allocsize = 0;
    value.data = NULL;

/* Get task names of history subsets */
    number_of_tasks = MAX_TASKS;	/* No more than MAX_TASKS allowed */
    status = zlhinfo(unit,(char *)task_names,instances,&number_of_tasks,
		"ulen",MAX_LABEL_KEY_SIZE+1, NULL);
    return_on_error(unit);

/* Cycle through each subset, listing out all labels */
    for (subset = 0; subset < number_of_tasks; subset++)
    {
	flush_key_value_pair();

/* Get the user and the time (standard task info) for task header */
        status = zlget(unit,"HISTORY","USER",username,
	    "HIST",task_names[subset],"INSTANCE",instances[subset], NULL);
	if (status <= 0 && value.data != NULL)
	    free(value.data);
        return_on_error(unit);
	status = zlget(unit,"HISTORY","DAT_TIM",time,
	      "HIST",task_names[subset],"INSTANCE",instances[subset], NULL);
	if (status <= 0 && value.data != NULL)
	    free(value.data);
	return_on_error(unit);

/* Print out the header for the task */
        sprintf(msg,"---- Task: %s -- User: %s -- %s ----",
	        task_names[subset],username,time);
	zvmessage(msg, "");

/* Set current key to task name */
        status = zlinfo(unit,"HISTORY","TASK",format,&dummy,&value.nelements,
	    "HIST",task_names[subset],"INSTANCE",instances[subset],
	    "STRLEN", &value.maxlength, NULL);
	continue_on_error(unit);

/* Cycle through each key in the subset */
        while (TRUE)
	{

/* Get next keyword */
    	    status = zlninfo(unit,key,format,&dummy,&value.nelements,
		"STRLEN", &value.maxlength, NULL);
	    if ((status == END_OF_LABEL) || (strcmp(key,"TASK") == 0)) break;
	    break_on_error(unit);
	    value.maxlength++;	/* leave room for null string terminator */

/* Don't print out DAT_TIM or USER again */
            if ((strcmp(key,"DAT_TIM") ==0) || (strcmp(key,"USER") == 0))
	        continue;

/* Get next value */
	    if (value.maxlength * value.nelements > value.allocsize)
	    {
		if (value.data != NULL)
		    free(value.data);
		value.data = malloc(value.maxlength * value.nelements);
		value.allocsize = value.maxlength * value.nelements;
		if (value.data == NULL)
		{
		    zvmessage("Out of memory!!!", "");
		    zabend();
		}
	    }
	    status = zlget(unit,"HISTORY",key,value.data,
		"HIST",task_names[subset], "INSTANCE",instances[subset],
		"FORMAT","STRING", "ULEN", value.maxlength,
		"NELEMENT", value.nelements, NULL);
	    continue_on_error(unit);

/* Print out key and value pair */
	    print_key_value_pair(key,&value,format);
	}	/* End of while loop */
    }		/* End of for loop */

    if (value.data != NULL)
	free(value.data);

    flush_key_value_pair();
}		/* End of list_history_items */


void list_property_items(int unit)
{
/*	list_property_items()					*/
/*								*/
/*	List out all items in the property label		*/
/*								*/
/*	Passed variables:					*/
/*								*/
/* unit -- input file unit number				*/
/*								*/
/*	Local variables:					*/
/*								*/
/* format -- The format of a given label item			*/
/* i -- temporary increment variable				*/
/* instances -- Array containing property instances		*/
/* key -- Name of a label item keyword				*/
/* number_of_props -- Number of property subsets in label	*/
/* subset -- Increment variable for subsets			*/
/* prop_names -- Array containing names of property subsets	*/
/* value -- Structure that describes the value for the item	*/
/* dummy -- holds LENGTH from zlinfo... needed since we need the*/
/* 	    len of the string (via STRLEN optional) instead of  */
/*	    the len of the int or real, since everything is	*/
/*	    treated as a string by the zlget calls.		*/
/*								*/

    int instances[MAX_PROPS],number_of_props,subset,i,dummy;
    char prop_names[MAX_PROPS][MAX_LABEL_KEY_SIZE+1];
    char key[MAX_LABEL_KEY_SIZE+1],format[32];
    struct multival value;

    printbuf[0] = '\0';			/* empty the string buffer */
    value.allocsize = 0;
    value.data = NULL;

/* Get property names of property subsets */
    number_of_props = MAX_PROPS;	/* No more than MAX_PROPS allowed */
    status = zlpinfo(unit,(char *)prop_names,&number_of_props,
		"inst_num", instances, "ulen",MAX_LABEL_KEY_SIZE+1, NULL);
    return_on_error(unit);

/* Cycle through each subset, listing out all labels */
    for (subset = 0; subset < number_of_props; subset++)
    {

	flush_key_value_pair();

/* Print out the header for the property */
	if (instances[subset] <= 1)
            sprintf(msg,"---- Property: %s ----", prop_names[subset]);
	else
            sprintf(msg,"---- Property: %s (#%d) ----", prop_names[subset],
						instances[subset]);
	zvmessage(msg, "");

/* Set current key to task name */
        status = zlinfo(unit,"PROPERTY","PROPERTY",format,&dummy,
		&value.nelements,"PROPERTY",prop_names[subset],
		"INSTANCE",instances[subset],
		"STRLEN", &value.maxlength, NULL);
	continue_on_error(unit);

/* Cycle through each key in the subset */
        while (TRUE)
	{

/* Get next keyword */
	    status = zlninfo(unit,key,format,&dummy,&value.nelements,
		"STRLEN", &value.maxlength, NULL);
	    if ((status == END_OF_LABEL) || (strcmp(key,"TASK") == 0) ||
		(strcmp(key,"PROPERTY") == 0)) break;
	    break_on_error(unit);
	    value.maxlength++;	/* leave room for null string terminator */

/* Get next value */
	    if (value.maxlength * value.nelements > value.allocsize)
	    {
		if (value.data != NULL)
		    free(value.data);
		value.data = malloc(value.maxlength * value.nelements);
		value.allocsize = value.maxlength * value.nelements;
		if (value.data == NULL)
		{
		    zvmessage("Out of memory!!!", "");
		    zabend();
		}
	    }
	    status = zlget(unit,"PROPERTY",key,value.data,
		"PROPERTY",prop_names[subset], "INSTANCE",instances[subset],
		"FORMAT","STRING", "ULEN", value.maxlength,
		"NELEMENT", value.nelements, NULL);
	    continue_on_error(unit);

/* Print out key and value pair */
	    print_key_value_pair(key,&value,format);
	}	/* End of while loop */
    }		/* End of for loop */

    if (value.data != NULL)
	free(value.data);

    flush_key_value_pair();
}		/* End of list_property_items */


void print_key_value_pair(char key[], struct multival *value, char *format)
{
/*	subroutine to print out a key-value pair			*/
/*	Note: You must call flush_key_value_pair() before printing	*/
/*	      anything other than key-value pairs so the last line can	*/
/*	      come out (nothing is printed until the line fills if the	*/
/*	      pack option is set).					*/
/*									*/

    int i,length;

 /* If packing, make sure key and at least one element will fit on the line */
    length = strlen(key) + strlen(value->data) + 8;   /* len of key + 1 elem */
    if ((strlen(printbuf)!=0) && (strlen(printbuf)+length >= WIDTH))
    {
	zvmessage(printbuf, "");		/* flush old buffer */
	printbuf[0] = '\0';
    }

    if (strlen(printbuf) != 0)
	strcat(printbuf, "  ");		/* two spaces between items */
    strcat(printbuf, key);
    strcat(printbuf, "=");

    if (value->nelements > 1)		/* multivalued */
	strcat(printbuf, "(");

    for (i=0; i<value->nelements; i++)
    {
        length = strlen(value->data+(i*value->maxlength)) + 4;
	if ((strlen(printbuf)!=0) && (strlen(printbuf)+length >= WIDTH))
	{
	    zvmessage(printbuf, "");	/* flush old buffer */
	    printbuf[0] = '\0';
	}

	if (*format == 'S')
	    strcat(printbuf, "'");
	strcat(printbuf, value->data+(i*value->maxlength));
	if (*format == 'S')
	    strcat(printbuf, "'");
	if (i != value->nelements-1)
	    strcat(printbuf, ", ");
    }

    if (value->nelements > 1)
	strcat(printbuf, ")");

    if (!pack_listed_items && strlen(printbuf) != 0)	/* flush buffer */
    {
	zvmessage(printbuf, "");
	printbuf[0]='\0';
    }
}

void flush_key_value_pair(void)
/*									*/
/*	Flushes print_key_value_pair()'s buffer.  Must be called before	*/
/*	printing anything else to the screen after calling		*/
/*	print_key_value_pair().						*/

{
    if (strlen(printbuf) != 0)
    {
	zvmessage(printbuf, "");
	printbuf[0]='\0';
    }
}

void list_task_headers(int unit)
{
/*	list_task_headers()					*/
/*								*/
/*	List out standard history subset info only		*/
/*								*/
/*	Passed variables:					*/
/*								*/
/* unit -- input file unit number				*/
/*								*/
/*	Local variables:					*/
/*								*/
/* instances -- Array containing task instances of " "		*/
/* number_of_tasks -- Number of history subsets in label	*/
/* subset -- Increment variable for subsets			*/
/* task_names -- Array containing task names of history subsets	*/
/* time -- Time returned by zlget				*/
/* username -- User field of a given task			*/

    int instances[MAX_TASKS],number_of_tasks,subset;
    char task_names[MAX_TASKS][MAX_LABEL_KEY_SIZE+1];
    char time[28],username[132];

/* Get task names of history subsets */
    number_of_tasks = MAX_TASKS;	/* No more than MAX_TASKS allowed */
    status = zlhinfo(unit,(char *)task_names,instances,&number_of_tasks,
			"ulen",MAX_LABEL_KEY_SIZE+1, NULL);
    return_on_error(unit);

/* Cycle through each subset, listing out standard task info */
    for (subset = 0; subset < number_of_tasks; subset++)
    {

/* Get the user and the time (standard task info) for task header */
        status = zlget(unit,"HISTORY","USER",username,
	      "HIST",task_names[subset],"INSTANCE",instances[subset], NULL);
        return_on_error(unit);
	status = zlget(unit,"HISTORY","DAT_TIM",time,
	      "HIST",task_names[subset],"INSTANCE",instances[subset], NULL);
	return_on_error(unit);

/* Print out the header for the task */
        sprintf(msg,"---- Task: %s -- User: %s -- %s ----",
	        task_names[subset],username,time);
	zvmessage(msg, "");
    }
}

void remove_label(void)
{
/* 	remove entire label from image file			*/
/*								*/
/*	Local variables:					*/
/*								*/

    int count,defaulted;
    int in_unit;		/* Input file unit number	*/
    int out_unit;               /* Output file unit number	*/
    int sl,ss,nl,ns;		/* Vicar size field		*/
    int nli,nsi;		/* Dummies for zvsize		*/
    int nlb,nsb;		/* # of binary lines & samps	*/
    int nrecs;			/* Number of records to copy	*/
    int line;			/* image line increment variable*/
    int samp;			/* image sample increment variable*/
    int band;			/* image band increment variable*/

    int lblsize;		/* vicar label size		*/
    int recsize;		/* record size of image file 	*/
    int numll;			/* number of lines of vicar label */
    char org[32];

    struct       		/* Window into the input        */
    {   
	int sb;	        	/* Starting band of input	*/
	int nb;		        /* Number of bands		*/
    } bands;

    char *buf = NULL;
    int rec_size;

/* Get unit numbers */
    status = zvunit(&in_unit,"INP",1, NULL);
    abort_on_error(in_unit);
    status = zvunit(&out_unit,"OUT",1, NULL);
    abort_on_error(out_unit);

/* Check for no blocking on output */
    if (zvptst("NOBLOCK"))
        status = zvadd(out_unit,"COND","NOBLOCK", NULL);

/* Open files	*/
    if (zvptst("BINARY") || zvptst("NOBINHEAD") || zvptst("NOBINPREF"))
	zvopen(in_unit, "OPEN_ACT","SA", "IO_ACT","SA", "COND","BINARY", NULL);
    else
	zvopen(in_unit,"OPEN_ACT","SA","IO_ACT","SA", NULL);

    zvsize(&sl,&ss,&nl,&ns,&nli,&nsi);
    if ((ns < 14) && zvptst("NOBLOCK"))
    {
	zvmessage("Output record must be greater than 14 samples", "");
	zvmessage("for unblocked tapes.", "");
	zabend();
    }

    zvparm("BANDS",&bands,&count,&defaulted,0,0);
    if (defaulted)
    {
	bands.sb =  1;
	status = zvget(in_unit,"NB",&bands.nb, NULL);
    }

    memset(org, 0, 8); 
    status = zvget(in_unit,"ORG",org, NULL);

    status = zvget(in_unit, "RECSIZE", &rec_size, NULL);
    abort_on_error(in_unit);
    buf = (char *)malloc(rec_size);
    if (buf == NULL) {
	zvmessage("Out of memory!!!", "");
	zabend();
    }

    if (zvptst("BINARY"))
    {
	zvget(in_unit, "NLB",&nlb, "NBB",&nsb,
			"NL",&nl, "NS",&ns, "NB",&bands.nb, NULL);
	zvopen(out_unit, "OP","WRITE", "COND","NOLABELS BINARY",
		"OPEN_ACT","SA", "IO_ACT","SA", "U_ORG",org,
		"U_NBB",nsb, "U_NLB",nlb, "U_NL",nl, "U_NS",ns,
		"U_NB",bands.nb, NULL);

	if (strncmp(org, "BIP", 3) == 0)
	    nrecs = nlb + nl*ns;
	else
	    nrecs = nlb + nl*bands.nb;

	for (line=0; line<nrecs; line++)
	{
	    zvread(in_unit, buf, NULL);
	    zvwrit(out_unit, buf, NULL);
	}
    }
    else if (zvptst("NOBINHEAD")) /* Binary header is removed */
    {
	zvget(in_unit, "NLB",&nlb, "NBB",&nsb,
			"NL",&nl, "NS",&ns, "NB",&bands.nb, NULL); 

	zvopen(out_unit, "OP","WRITE", "COND","BINARY",
		"OPEN_ACT","SA", "IO_ACT","SA", "U_ORG",org,
		"U_NBB",nsb, "U_NL",nl, "U_NS",ns,
		"U_NB",bands.nb, NULL);

	/* numll=lblsize/recsize; 
	if (lblsize%recsize != 0)   numll++; NO NEED FOR THIS */

	if (strncmp(org, "BIP", 3) == 0)
	    nrecs = nlb + nl*ns;
	else
	    nrecs = nlb + nl*bands.nb;

	/* nrecs += (numll-1); NO NEED FOR THIS */


	for (line=0; line<nrecs; line++)
	{

	    zvread(in_unit, buf, NULL);
	    if ((line+1)>=nlb) 
	    	zvwrit(out_unit, buf, NULL);
	}
    }
    else if (zvptst("NOBINPREF"))  /* Binary prefix is removed */ 
    {
	zvget(in_unit, "NLB",&nlb, "NBB",&nsb,
			"NL",&nl, "NS",&ns, "NB",&bands.nb, NULL);

	zvopen(out_unit, "OP","WRITE", "COND","BINARY",
		"OPEN_ACT","SA", "IO_ACT","SA", "U_ORG",org,
		"U_NLB",nlb, "U_NL",nl, "U_NS",ns,
		"U_NB",bands.nb, NULL);

	if (strncmp(org, "BIP", 3) == 0)
	    nrecs = nlb + nl*ns;
	else
	    nrecs = nlb + nl*bands.nb;

	for (line=0; line<nrecs; line++)
	{
	    zvread(in_unit, buf, NULL);
	    zvwrit(out_unit, buf, NULL);
	}
    }
    else		/* Not binary */
    {
	status = zvopen(out_unit,"OP","WRITE","COND","NOLABELS","OPEN_ACT","SA",
	      "IO_ACT","SA","U_ORG",org,"U_NL",nl,"U_NS",ns,"U_NB",bands.nb, NULL);

	/*  Copy Data	*/
	if ( strncmp( org,"BSQ",3 ) == 0 )
	    for (band = bands.sb; band <= (bands.nb + bands.sb - 1); band++)
		for (line = sl; line <= nl + sl - 1; line++)
		{
		    status = zvread(in_unit,buf,"SAMP",ss,"LINE",line,
				"BAND",band, "NSAMPS",ns, NULL);
		    status = zvwrit(out_unit,buf, NULL);
		}
	else if ( strncmp( org, "BIL",3 ) == 0 )
	    for (line = sl; line <= nl + sl - 1; line++)
		for (band = bands.sb; band <= (bands.nb + bands.sb - 1); band++)
		{
		    status = zvread(in_unit,buf,"SAMP",ss,"LINE",line,
				"BAND",band, "NSAMPS",ns, NULL);
		    status = zvwrit(out_unit,buf, NULL);
		}
	else
	   for (line = sl; line <= nl + sl - 1; line++)
		for (samp = ss; samp <= ns + ss - 1; samp++)
		{
		    status = zvread(in_unit,buf,"SAMP",samp,"LINE",line,
				   "BAND",bands.sb,"NBANDS",bands.nb, NULL);
		    status = zvwrit(out_unit,buf, NULL);
		}
    }

    free(buf);

/* Close files and exit */
    status = zvclose(in_unit, NULL);
    status = zvclose(out_unit, NULL);
}

void switch_labels(void)
{
/*	switch_labels						*/
/* 								*/
/* Create an output containing the data and system label	*/
/* of the second input and the history and property labels of	*/
/* the first.							*/
/* 								*/
    int input_1,input_2,output;		/* Unit numbers		*/
    int nl,ns,nb,nlb,nbb;		/* Size of output	*/
    int i;				/* temp variable	*/
    char format[12];			/* format of output	*/
    char type[32];			/* type of output	*/
    char org[8];			/* ORG of output	*/

    status = zvunit(&input_1,"INP",1, NULL);
    status = zvunit(&input_2,"INP",2, NULL);

    status = zvopen(input_1,"OPEN_ACT","SA","IO_ACT","SA", NULL);
    status = zvopen(input_2,"OPEN_ACT","SA","IO_ACT","","COND","BINARY", NULL);
    status = zvget(input_2,"NLB",&nlb,"NBB",&nbb, NULL);
    if (status != 1)
	nlb = nbb = 0;

/* Get size of output */
    status = zvget(input_2,"FORMAT",format,"NL",&nl,"NS",&ns,"NB",&nb,
	  "TYPE", type, "ORG", org, NULL);

/* Open output */
    status = zvunit(&output,"OUT",1, NULL);
    status = zvopen(output,"OP","WRITE","U_FORMAT",format,
	"O_FORMAT",format,"U_NL",nl,"U_NS",ns,"U_NB",nb,
	"U_NLB",nlb,"U_NBB",nbb,"COND","BINARY",
	"OPEN_ACT","SA","IO_ACT","SA","TYPE",type,"U_ORG",org, NULL);

/* Copy the data */

    {
	char *buf = NULL;
	int rec_size;
	status = zvget(input_2, "RECSIZE", &rec_size, NULL);
	abort_on_error(input_2);
	buf = (char *)malloc(rec_size);
	if (buf == NULL) {
	    zvmessage("Out of memory!!!", "");
	    zabend();
	}

	while (TRUE)
	{   status = zvread(input_2,buf, NULL);
	    if (status == END_OF_FILE) break;
	    abort_on_error(input_2);
	    status = zvwrit(output,buf, NULL);
	    abort_on_error(output);
	}
	free(buf);
    }

/* Close the data sets */
    status = zvclose(input_1, NULL);
    status = zvclose(input_2, NULL);
    status = zvclose(output, NULL);

}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create label.imake
#define PROGRAM label

#define MODULE_LIST label.c

#define MAIN_LANG_C
#define R2LIB

#define USES_ANSI_C

#define LIB_RTL
#define LIB_TAE

$ Return
$!#############################################################################
$PDF_File:
$ create label.pdf
PROCESS HELP=*

SUBCMD ADD	! ADD A LABEL ITEM
    PARM INP TYPE=STRING COUNT=1
    PARM OUT TYPE=STRING COUNT=0:1 DEFAULT=--
    PARM ITEMS TYPE=STRING COUNT=1
    PARM TASK TYPE=(STRING,32) COUNT=0:1 DEFAULT=--
    PARM INSTANCE TYPE=INTEGER COUNT=1 DEFAULT=1
    PARM PROPERTY TYPE=(STRING,32) COUNT=0:1 DEFAULT=--
    PARM TYPE TYPE=KEYWORD VALID=(HISTORY,PROPERTY) COUNT=0:1 DEFAULT=--
    PARM UPDATE TYPE=KEYWORD VALID=UPDATE COUNT=0:1 DEFAULT=--
END-SUBCMD

SUBCMD CONCAT
    PARM INP TYPE=STRING COUNT=2
    PARM OUT TYPE=STRING COUNT=0:1 DEFAULT=--
    PARM TYPE TYPE=KEYWORD VALID=(HISTORY,PROPERTY,BOTH) COUNT=1 DEFAULT=BOTH
    PARM TASK TYPE=STRING COUNT=0:1 DEFAULT=--
    PARM PROPERTY TYPE=STRING COUNT=0:1 DEFAULT=--
    PARM INSTANCE TYPE=INTEGER COUNT=1 DEFAULT=1
END-SUBCMD

SUBCMD CREATE	! CREATE A SYSTEM LABEL
    PARM INP TYPE=STRING
    PARM OUT TYPE=STRING
    PARM NL TYPE=INTEGER COUNT=0:1 DEFAULT=--
    PARM NS TYPE=INTEGER COUNT=0:1 DEFAULT=--
    PARM NB TYPE=INTEGER COUNT=1   DEFAULT=1
    PARM NBB TYPE=INTEGER COUNT=1  DEFAULT=0
    PARM NLB TYPE=INTEGER COUNT=1  DEFAULT=0
    PARM FORMAT TYPE=KEYWORD VALID=(BYTE,HALF,FULL,REAL,DOUB,+
         COMP) DEFAULT=BYTE
    PARM ORG    TYPE=KEYWORD VALID=(BSQ,BIL,BIP) DEFAULT=BSQ
    PARM BINARY TYPE=KEYWORD VALID=(BINARY,NOBINARY) DEFAULT=NOBINARY
    PARM WINDOW TYPE=INTEGER COUNT=(0,4) DEFAULT=--
    PARM BANDS   TYPE=INTEGER COUNT=(0,2) DEFAULT=--
    PARM COMMENT TYPE=(STRING,132) COUNT=0:1 DEFAULT=--
    PARM HOST     TYPE=(STRING,32) COUNT=0:1 DEFAULT="NATIVE"
    PARM INTFMT   TYPE=(STRING,32) COUNT=0:1 DEFAULT=--
    PARM REALFMT  TYPE=(STRING,32) COUNT=0:1 DEFAULT=--
    PARM BHOST    TYPE=(STRING,32) COUNT=0:1 DEFAULT=--
    PARM BINTFMT  TYPE=(STRING,32) COUNT=0:1 DEFAULT=--
    PARM BREALFMT TYPE=(STRING,32) COUNT=0:1 DEFAULT=--
    PARM BLTYPE   TYPE=(STRING,32) COUNT=1   DEFAULT=""
END-SUBCMD

SUBCMD DELETE
    PARM INP TYPE=STRING COUNT=1
    PARM OUT TYPE=STRING COUNT=0:1 DEFAULT=--
    PARM KEYS  TYPE=(STRING,32) COUNT=0:10 DEFAULT=--
    PARM TASKS TYPE=(STRING,32) COUNT=0:10 DEFAULT=--
    PARM INSTNCES TYPE=INTEGER COUNT=0:10 DEFAULT=--
    PARM PROPERTY TYPE=(STRING,32) COUNT=0:10 DEFAULT=--
    PARM ELEMENT TYPE=INTEGER COUNT=0:1 DEFAULT=--
    PARM NELEMENT TYPE=INTEGER COUNT=0:1 DEFAULT=--
    PARM TYPE TYPE=KEYWORD VALID=(HISTORY,PROPERTY) COUNT=0:1 DEFAULT=--
END-SUBCMD

SUBCMD LIST
    PARM INP TYPE=STRING COUNT=1:10
    PARM EXTENT TYPE=KEYWORD+
                VALID=(ALL,DUMP,HISTORY,SYSTEM,TASKS,PROPERTY) DEFAULT=ALL
    PARM PACK TYPE=KEYWORD VALID=(PACK,NOPACK) DEFAULT=NOPACK
END-SUBCMD

SUBCMD REMOVE
    PARM INP TYPE=STRING COUNT=1
    PARM OUT TYPE=STRING COUNT=1
    PARM SIZE TYPE=INTEGER COUNT=(0,4) DEFAULT=--
    PARM BANDS TYPE=INTEGER COUNT=(0,2) DEFAULT=--
    PARM BLOCKING TYPE=KEYWORD VALID=(BLOCK,NOBLOCK) DEFAULT=BLOCK
    PARM BINARY TYPE=KEYWORD VALID=(BINARY,NOBINARY,NOBINHEAD,NOBINPREF) DEFAULT=NOBINARY
END-SUBCMD

SUBCMD REPLACE
    PARM INP TYPE=STRING COUNT=1
    PARM OUT TYPE=STRING COUNT=0:1 DEFAULT=--
    PARM ITEMS TYPE=STRING COUNT=1
    PARM TASK TYPE=(STRING,32) COUNT=0:1 DEFAULT=--
    PARM INSTANCE TYPE=INTEGER COUNT=1 DEFAULT=1
    PARM PROPERTY TYPE=(STRING,32) COUNT=0:1 DEFAULT=--
    PARM TYPE TYPE=KEYWORD VALID=(SYSTEM,HISTORY,PROPERTY) COUNT=0:1 DEFAULT=--
END-SUBCMD

SUBCMD SWITCH
    PARM INP TYPE=STRING COUNT=2
    PARM OUT TYPE=STRING COUNT=1
END-SUBCMD

! inp and inp{1-2} are only default until subcmds get straightened out
!# parm inp hints=default
!# parm inp(1-10) hints=default

END-PROC
!
!=======================================================================
!
.TITLE
VICAR Program LABEL
.HELP
LABEL is a VICAR program, designed for both interactive and batch
use, which can be used to create, remove, list, and modify in
various ways the labels in a VICAR file.  It is designed to replace
all old IBM VICARlabel processing programs (LABCAT, VLABEL etc.) and
add additional capabilities.

When LABEL is invoked on the command line, the mode of operation
is determined by the subcommand which is used.  For help on the
subcommands and the general operation of LABEL, type TUTOR LABEL.
.page
Limitations:  The VICAR label items are in the form of "keyword = value".
If the value is of the string type, there is a limitation on the maximum
string length depending on the operating systems.  At current time, the
maximum string length is limited to 132 characters, including white spaces 
(the Solaris OS constraint).  Users need to be aware the program does not 
warn users if the maximum string length is exceeded.    
.page
   Written by:            D. F. Stanfill 1984
   Cognizant Programmer:  R. G. Deen 1992
   Latest revision:       R. G. Deen, 2010
                          Added UPDATE keyword to ADD, and ANSI-fied prototypes.
                          A. C. Chen 8-2002
                          Added max string length limitation in the PDF help
                          file.
                          R. G. Deen 3-1999
                          Added property instances and LABEL-CONCAT subcommand.
                          R. R. Patel Jun 17, 1998.
                          Updated add option to correctly add a label with
                          double precision value (AR-9034). Also updated
                          remove to correctly remove binary header.
                          R. G. Deen 5-92
                          Added support for Property labels, and added host
                          types and fixed binary labels in LABEL-CREATE.
                          J. F. McNeill 12-21-91
                          BINARY header and prefixes handled in all cases.
                          Header and prefix sizes added to LABEL-LIST info.
                          R. G. Deen 2-91
                          Converted to Unix-compatible executive
                          J. F. McNeill 8-16-90
                          Addition of BINARY keyword for -CREATE
.LEVEL1
!
!=======================================================================
!
.SUBCMD ADD
Add label items to an image
label
.VAR INP -ADD
Input file name
.VAR OUT -ADD
Output file name
(Optional)
.VAR ITEMS -ADD
List of items to be
added 
.VAR TASK -ADD
Task name of label item
.VAR INSTANCE -ADD
Instance or occurrence of named
task/property
.VAR PROPERTY -ADD
Property name of label item
.VAR TYPE -ADD
Add HISTORY or PROPERTY label
.VAR UPDATE -ADD
Update existing label if it
already exists
!
!=======================================================================
!
.SUBCMD CONCAT
Add label sets from input 1
to input 2.
.VAR INP -CONCAT
Input file names.  Items
from 1 are added to 2
.VAR OUT -CONCAT
Output file name
.VAR TYPE -CONCAT
HISTORY, PROPERTY, or
BOTH
.VAR TASK -CONCAT
History task to transfer,
if only one
.VAR PROPERTY -CONCAT
Property set to transfer,
if only one
.VAR INSTANCE -CONCAT
Instance number, goes
with SET_NAME
!
!=======================================================================
!
.SUBCMD CREATE
Create a system label for an
unlabeled file.
.VAR INP -CREATE
Input file name
.VAR OUT -CREATE
Output file name
.VAR NL -CREATE
Number of lines in image
in input image
.VAR NS -CREATE
Number of samples in image
.VAR NB -CREATE
Number of bands in input image
.VAR NBB -CREATE
Number of binary prefix bytes
per line
.VAR NLB -CREATE
Number of binary header lines
.VAR FORMAT -CREATE
Pixel format
.VAR ORG -CREATE
File organization
.VAR BINARY -CREATE
Image has binary label?
.VAR WINDOW -CREATE
Window to be taken from input
(independent of NL & NS)
.VAR BANDS -CREATE
Bands to be taken from input
(independent of NB)
.VAR COMMENT -CREATE
Optional comment for label
.VAR HOST -CREATE
Machine type of input file
.VAR INTFMT -CREATE
Integer format of input file
(default given by HOST)
.VAR REALFMT -CREATE
Real format of input file
(default given by HOST)
.VAR BHOST -CREATE
Machine type of input binary
label (default to HOST)
.VAR BINTFMT -CREATE
Integer format of input binary
label (default given by BHOST)
.VAR BREALFMT -CREATE
Real format of input binary
label (default given by BHOST)
.VAR BLTYPE -CREATE
Type of binary label
!
!=======================================================================
!
.SUBCMD DELETE
Delete all or part of the
history or property labels
.VAR INP -DELETE
Input file name
.VAR OUT -DELETE
Output file name
(Optional)
.VAR KEYS -DELETE
Keywords of items
to be deleted
(or /ALL)
.VAR TASKS -DELETE
Task names of KEYS or
tasks to be deleted
.VAR INSTNCES -DELETE
Instances of TASKS or
PROPERTY
.VAR PROPERTY -DELETE
Property names of KEYS
or properties to be
deleted
.VAR ELEMENT -DELETE
Starting element
number to delete
.VAR NELEMENT -DELETE
Number of elements
to delete
.VAR TYPE -DELETE
Delete HISTORY or PROPERTY
labels
!
!=======================================================================
!
.SUBCMD LIST
List out label information
.VAR INP -LIST
Input file name(s)
(up to 10)
.VAR EXTENT -LIST
Extent of listing to be done
(type help for valid values)
.VAR PACK -LIST
Specifies packing of items
on a line
!
!=======================================================================
!
.SUBCMD REMOVE
Remove the label from an image
.VAR INP -REMOVE
Input file name
.VAR OUT -REMOVE
Output file name
.VAR SIZE -REMOVE
VICAR size field
(sl,ss,nl,ns)
.VAR BANDS -REMOVE
Window to be taken from input
(independent of NB)
.VAR BLOCKING -REMOVE
Block the output image?
(BLOCK,NOBLOCK)
.VAR BINARY -REMOVE
Binary label, header or prefix before image?
(BINARY,NOBINHEAD,NOBINPREF,NOBINARY)
!
!=======================================================================
!
.SUBCMD REPLACE
Replace a list of items
.VAR INP -REPLACE
Input file name
.VAR OUT -REPLACE
Output file name
(Optional)
.VAR ITEMS -REPLACE
List of items to be
replaced
.VAR TASK -REPLACE
Task name of label item
.VAR INSTANCE -REPLACE
Instance or occurrence of named
task/property
.VAR PROPERTY -REPLACE
Property name of label item
.VAR TYPE -REPLACE
TYPE of label item
(SYSTEM, HISTORY, or PROPERTY)
!
!=======================================================================
!
.SUBCMD SWITCH
Switch history labels
.VAR INP -SWITCH
Input file names
(2 required)
.VAR OUT -SWITCH
Output file name
!
!=======================================================================
!
.LEVEL2
!
!=======================================================================
!
.SUBCMD ADD
LABEL-ADD is used to add one or more label items to an image's history
or property label, or to add one or more label elements to a currently
existing label item.  The items are given as a list in a quoted string,
separated by either spaces or commas.  Each item must be given in the
form "keyword(element) = value-list", where "keyword" is the keyword
of the label, "(element)" is the OPTIONAL starting element number, and
"value-list" is a single value or a list of values enclosed in parentheses.
If a value in "value-list" is a string, it should be contained in single
quotes.  If the value is an integer, real or double-precision value,
it is given as is.

If both an input and an output file are given, then the output file
label is modified and the data is copied to it.  If only an input file
is given, then the input label is modified and no output file is created.

Normally, attempting to add a keyword that already exists causes an error.
However, if the 'UPDATE keyword is turned on, the keyword will be replaced
if it already exists.
.page
Examples:

LABEL-ADD infile outfile "SLOPE = 3, COMMENT='Hello' COORD=(32.5,59.96)"

would add the integer keyword SLOPE with a value of 3, the string
keyword COMMENT with the value contained in the single quotes,
"Hello", and the real keyword COORD with the two values 32.5 and
59.96 to the current task label, the LABEL history subset.
The input file is not modified.

LABEL-ADD infile outfile "COUNT=2, TARGET='NEPTUNE'" PROPERTY=MYPROP

would add the integer keyword COUNT with a value of 2 and the string
keyword TARGET with the value "NEPTUNE" to the property label set
named "MYPROP" (first instance).  The input file is not modified.
.page
Examples (cont.):

LABEL-ADD infile +
          ITEMS="DN = 255 COMMENT = 'All lines are black' LIST(2)=(37,42)" +
          TASK=INSERT INSTANCE=2

would add the integer keyword DN with a value of 255, and the 
string keyword COMMENT with the value 'All lines are black'
to the second occurence of the INSERT history subset in the
input file.  If the keyword LIST existed previously in the label,
then elements 2 and 3 would be added, with the values 37 and 42,
respectively.  The old element 2 would become element 4, i.e. the
new elements are inserted into the item.  If LIST did not exist
previously, it would be created with two elements with values 37
and 42.  Since no output file is given, the input file is modified,
and no output file is produced.
.VAR INP -ADD
INP specifies the name of the input file.  The input must have a valid
VICAR label.

Note:  If no output file name is given, the input file's label will 
be modified directly.
.VAR OUT -ADD
OUT is the name of the output file to which the data and the modified
label are to be written.

If OUT is omitted from the command line, the label items are added to
the input file directly.
.VAR ITEMS -ADD
The label items which are to be added. The items are given as a
list in a single string parameter, in the form

key(element)=value-list

where "key" is a valid (0 - 8 character) label keyword, "element"
is an optional starting element number (an integer), and "value-list"
is the list of values to be associated with that keyword.

The operation differs slightly depending on whether or not the
starting element number is given.

If the element is not given, i.e. the item is of the form
"key=value-list", then the entire label item is affected.  The
indicated keyword is added to the label, with one or more values
specified by "value-list".  If the keyword already exists, an
error message is produced (unless UPDATE is turned on, in which
case the keyword is replaced).
.page
If the element is given, then only parts of the label item are
affected.  If the keyword currently exists in the file, then the
indicated values are inserted into it, starting at element number
"element".  For instance, if the label item "LIST" had three values
(1,2,3), and the following was in the label-add ITEM string:
"LIST(2)=(10,11)", then values 10 and 11 would be inserted into
LIST starting at position 2.  The new value of LIST would be
(1,10,11,2,3).

If the element is given, but the keyword does not currently exist
in the file, the element number is ignored and a new label item
is created containing the values in "value-list".
.page
A value of -1 for the element number indicates the end of the label
item, i.e. all elements will be appended to the end of the item.

If the same keyword appears twice in "value-list", each is treated
independently, i.e. the first one is used, then the second one.  If
the first one causes the number of elements or the relative position
of an element to change, the second one uses the NEW positions, values,
and number of elements.
.page
"Value-list" is a list of values, enclosed in parentheses, and
separated by commas.  If there is only one value, the parentheses
are not necessary.

All of the values in "value-list" should be of the same type:
integer, real, doub(le), or string.  The type should also match the type
of the existing label, if "element" is given.  The LABEL program will
try to convert to the appropriate type, but if it can't it will give
an error.  It can convert integers to real, doub(le) or string, and 
reals to doub(le) or string.  Any other conversion is an error.

The four types are differentiated by the way in which they are
specified. Strings should be contained in single quotes.  The quotes
are not strictly necessary if there are no commas, spaces, or parentheses
in the string, but they should normally be included anyway.  Standard
FORTRAN notation for integers, reals and doub(le)s is understood. Doub(le)s
are real values with more than 6 significant digits.              

.PAGE
Examples of valid "value-list" specifications:

Specification			Meaning
-------------			-------
787				single integer
(21)				single integer
(6,-9,42)			list of three integers
+3e2				single real
-2.000001			single doub(le)
(3.14159, 2)			list of two reals.  The integer 2 is
				  converted to a real: (3.14159, 2.0)
'a string item'			single string
('another string item')		single string
('string 1','string 2')		list of two strings
('string 1', 123, 4.5, wow)	list of four strings.  The values get converted
				  to strings: ('string 1', '123', '4.5', 'wow')
.page
Using this input format, the entire list of items is input
as a single string, such as

"slope=-45.8, planet='Jupiter' avgdn = 128 coord=(86.3, 44.8)"

This string stores the real value "slope", the string value
"planet", the integer value "avgdn", and the two element real
value "coord".
.VAR TASK -ADD
The name of the task heading or history subset under which the label
item is to be stored.

The presence of TASK implies that the history label will be modified,
so TYPE is unnecessary.  If you wish to add to a property label, use
PROPERTY instead.  If neither TASK nor PROPERTY are given, and TYPE is
omitted, all labels will be placed in the history label under the
heading of the current task (LABEL if an output file is given, the
last task otherwise).

An error occurs if both TASK and PROPERTY are given, or if TASK is
given with TYPE=PROPERTY.
.VAR INSTANCE -ADD
Since the same program may be run on an image several times, it is
possible to have multiple history task headings with the same name.
Property sets may also have more than one instance (e.g. different
versions of a camera model).  INSTANCE=n is used to refer to the
n-th occurence of the named history task or property set.  If INSTANCE
is not specified, 1 is assumed.

For PROPERTY labels only, setting INSTANCE=0 will create a new instance
of the given property.  New instances may also be created by giving
an INSTANCE higher than any currently existing in the label (only one at
a time will be added), but INSTANCE=0 avoids having to determine the highest
existing instance number first.  However, each keyword in the ITEMS list
is treated independently, so if you have more than one, you'll get multiple
new property instances added if you specify INSTANCE=0!  Use with care.

If TASK or PROPERTY is not specified, then INSTANCE is ignored.
.VAR PROPERTY -ADD
The name of the property subset under which the label item is to be
stored.

The presence of PROPERTY implies that the property label will be
modified, so TYPE is unnecessary.  If you wish to add to a history
label, use TASK instead.  PROPERTY is mandatory for property labels,
so even if you say TYPE=PROPERTY you must still specify a property name.

An error occurs if both PROPERTY and TASK are given, or if PROPERTY is
given with TYPE=HISTORY.
.VAR TYPE -ADD
Specifies whether the items will be added to the HISTORY or PROPERTY
label.  TYPE is not normally needed, since the presence of the TASK
parameter implies history labels, while the presence of the PROPERTY
parameter implies property labels.  TYPE may be used for error-checking,
however, as TASK, PROPERTY, and TYPE must all agree on the type of
labels being added.

.VAR UPDATE -ADD
Normally, if elements were not specified and the keyword already exists
in the label, an error is produced.  The -REPLACE subcommand can be used
in this case, but that causes an error if the keyword does not already
exist.

The 'UPDATE keyword to -ADD mixes both subcommands.  If specified, the
keyword will be added to the label if it does not already exist, or
replaced (updated) it if it does.
!
!=======================================================================
!
.SUBCMD CONCAT
LABEL-CONCAT is used to concatenate property and/or history label sets
from input 1 into input 2.  The resultant image has the data and labels
of input 2, with selected property sets from label 1 added in.  If OUT
is specified, a new file is written, otherwise input 2 is modified in-place.

The result is similar to LABEL-SWITCH except that history/property labels
from input 2 survive the process.

Either the entire history and/or property label from input 1, or a single
property set or history task, may be transferred.  Specifying TASK or
PROPERTY will cause a single set to be transferred (in which case TYPE=BOTH
is ignored); omitting both TASK and PROPERTY will transfer the entire
history and/or property label, controlled by TYPE.

In all cases, new history tasks or property sets are created; existing
history/property sets in input 2 are not modified.

The new label sets are always added after all existing ones; there is no
facility to add sets into the middle or the beginning.  To simulate adding
sets to the beginning, add them in the other order, then use LABEL-SWITCH.
(e.g. instead of adding A before B, add B's labels after A, then switch the
data back).
.page
Examples:

LABEL-CONCAT (file_1,file_2)

would add all history and property sets from file_1 to the end of the
history and property sections of file_2.

LABEL-CONCAT (file_1,file_2) property="CAMERA_MODEL"

would transfer the CAMERA_MODEL property (instance 1) to file_2.

LABEL-CONCAT (file_1,file_2) outfile task="GEN" instance=1

would add history task GEN, instance 1, from file_1 to file_2's labels,
writing the result in outfile.  file_2 is not modified.
.VAR INP -CONCAT
INP specifies the name of the two input files.  The result has the data
and complete label of input 2, with selected label sets from input 1.

Note:  if no output file name is given, input 2's label will be modified
directly.
.VAR OUT -CONCAT
OUT is the name of the output file to which the data and the modified
label are to be written.

If OUT is omitted from the command line, the label sets are added to
input 2 directly.
.VAR TYPE -CONCAT
TYPE specifies the type of label to transfer.

BOTH: The entire label, both history and property, is transferred, if
    TASK and PROPERTY are omitted.  If either TASK or PROPERTY is specified,
    only that given single set is transferred, and TYPE=BOTH is quietly
    ignored.  BOTH is the default.
HISTORY: The history label is transferred.  If TASK is given, only one
    history task is transferred, otherwise the entire history label is.
    If TASK is specified, TYPE may be defaulted; you don't have to set both.
PROPERTY: The property label is transferred.  If PROPERTY is given, only one
    property set is transferred, otherwise the entire property label is.
    If PROPERTY is specified, TYPE may be defaulted; you don't have to set
    both.
.VAR TASK -CONCAT
TASK specifies the name of the history task to transfer.  If neither TASK
nor PROPERTY is given, the entire history or property label is transferred
(according to TYPE).

TASK and PROPERTY may not both be specified.
.VAR PROPERTY -CONCAT
PROPERTY specifies the name of the history task to transfer.  If neither
PROPERTY nor TASK is given, the entire history or property label is
transferred (according to TYPE).

TASK and PROPERTY may not both be specified.
.VAR INSTANCE -CONCAT
INSTANCE specifies the instance number of the history task or property set
to transfer (relative to input 1).  It is only used if TASK or PROPERTY is
active.

The default is 1.
!
!=======================================================================
!
.SUBCMD CREATE
LABEL-CREATE takes a file with no label and, using the
information given by the user, generates a system label for it.

VICAR in some cases can attempt to determining the image size
for you on a VMS machine if you don't specify it, but to avoid
potential problems it is recommended that you give the input size
with NL, NS, and NB when it is known.  The image size cannot be
automatically determined on a Unix machine.

In addition, if the pixel format is not specified, byte format
is assumed.  If the file organization is not specified, BSQ
(band sequential) is assumed.

Unless you specify otherwise via the HOST label, the input file
is assumed to be in the data format of the machine you are running
on.  If this is not the case (i.e. the file was created on a VAX
and you are running on a Sun), then you need to specify HOST.
.VAR INP -CREATE
The name of a single input file which does not have a valid 
VICAR label.  The input is assumed unconditionally to have
no label, and the new label is simply prepended to the output.
.VAR OUT -CREATE
The name of a single output file to receive the new label and
the data from the input.
.VAR NL -CREATE
The number of lines in the input image.  Note that this number is not
a window but the actual number of image lines (not including any
binary headers), and is unconditionally assumed to be correct.
.VAR NS -CREATE
The number of samples in the input image.  Note that this number is
not a window but the actual number of samples (not including any
binary headers), and is unconditionally assumed to be correct.
.VAR NB -CREATE
The number of bands in the input image.  Note that this number is not
a window but the actual number of bands (not including any binary
headers), and is unconditionally assumed to be correct.
Default is a one band image.
.VAR NBB -CREATE
The number of bytes of binary prefix which precede each image
record in the output file.  This number is added to NS (for BSQ or
BIL files) or NB (for BIP files) to get the total size of an
image record (i.e. the values for NS and NB should not include NBB).

The BINARY keyword must be specified in order for NBB to take effect.
The default for NBB is 0.
.VAR NLB -CREATE
The number of lines of binary header that precede the image data.
This number is added to the number of image records to get the total
number of record in the image (i.e. the values for NL, NS, and NB should
not include NLB).

The BINARY keyword must be specified in order for NLB to take effect.
The default for NLB is 0.
.VAR FORMAT -CREATE
The format of the pixels in the file.  Defaults to 
BYTE.  The valid values are:

	BYTE	Single byte unsigned binary integer
	HALF	Half-precision (usually two bytes) signed integer
	FULL	Full-precision (usually four bytes) signed integer
	REAL 	Single-precision floating point number
	DOUB	Double-precision floating point number
	COMP	Complex pairs of REAL numbers in the order (real,imaginary)
.VAR ORG -CREATE
ORG provides a means of specifying the file organization.  Organizations
may be : BSQ (band sequential), BIP (band interleaved by pixel), or BIL
(band inerleaved by line). Default is BSQ.
.VAR BINARY -CREATE
If BINARY is specified as a keyword, the new file is created with a binary
label specified by NLB and NBB.  The data in the binary labels comes from
the input file, i.e. if NLB is 2 then the first two lines of the input file
will make up the binary header.  See also NLB and NBB.
.VAR WINDOW -CREATE
WINDOW provides a means of extracting a subset of the input
for writing to the output.  Its format is the same as that 
of the VICAR size field,

	WINDOW = (sl,ss,nl,ns)

where 		sl	is the starting line to read,
		ss	is the starting sample,
		nl	is the number of lines to read,
		ns	is the number of samples to be
			extracted from each line.

Note that the window is independent of the parameters NL and NS.
NL and NS specify the physical size of the input, and window 
provides a subset of the input for output.  To create a 3-dimensional
window, the BANDS parameter must also be used.

If BINARY is set, then WINDOW and BANDS are ignored.
.VAR BANDS -CREATE
BANDS provides a means of extracting a subset of the input
for writing to the output.  Its format is as follows:

	BANDS = (sb,nb)

where 		sb	is the starting band to read,
		nb	is the number of bands to be
			extracted.

Note that the BANDS parameter is independent of the NB parameter.
NB specifies the physical size of the input, and BANDS
provides a subset of the input for output.  See also WINDOW.

If BINARY is set, then WINDOW and BANDS are ignored.
.VAR COMMENT -CREATE
COMMENT is an optional string label item for the user to insert
into the label.  It is stored under the keyword COMMENT in
the current task.  It may be up to 132 characters in length.
.VAR HOST -CREATE
HOST provides a means to specify the type of machine the data came from.
If the image was created on a machine type other than the one you are
running on, then the image data will likely be in a different data format,
e.g. VAX floating point instead of IEEE floating point.  Setting the proper
value for HOST allows other VICAR programs to access the image data
properly.

The default for HOST is "NATIVE", meaning the data format for the machine
that LABEL-CREATE is running on.
.page
The valid values may expand as VICAR is ported to other machines, but as
of this writing they are:

   NATIVE    The machine LABEL-CREATE is currently running on (default).
   LOCAL     Same as NATIVE.
   ALLIANT   Alliant FX series computer.
   AXP-LINUX Alpha running Linux.
   AXP-UNIX  Alpha running Digital Unix.
   AXP-VMS   Alpha running VMS.
   CRAY      Cray (port is incomplete).
   DECSTATN  DECstation (any DEC MIPS-based RISC machine) running Ultrix.
   HP-700    HP 9000 Series 700 workstation.
   MAC-AUX   Macintosh running A/UX.
   MAC-MPW   Macintosh running native mode with Mac Programmers Workbench.
   SGI       Silicon Graphics workstation.
   SUN-3     Sun 3, any model.
   SUN-4     Sun 4 or SPARCstation, or clone such as Solbourne, running SunOS 4.
   SUN-SOLR  Sun (or compatible) running Solaris.
   TEK       Tektronix workstation.
   VAX-VMS   VAX running VMS.
   X86-LINUX Intel processor running Linux.
   X86-SOLR  Intel processor running Solaris.
.VAR INTFMT -CREATE
INTFMT specifies the format used to represent integers in the image.
It should rarely be used, as it defaults to the integer format for the
machine specified in HOST.

The valid values may expand as VICAR is ported to other machines, but as
of this writing they are:

   NATIVE    The format for the machine LABEL-CREATE is currently running on.
   LOCAL     Same as NATIVE.
   HIGH      High byte first, "big endian", used with most hosts.
   LOW       Low byte first, "little endian", used with VAX-VMS, DECSTATN,
             and all AXP and X86 machines.
.VAR REALFMT -CREATE
REALFMT specifies the format used to represent floating-point data in the
image.  It should rarely be used, as it defaults to the floating-point
format for the machine specified in HOST.

The valid values may expand as VICAR is ported to other machines, but as
of this writing they are:

   NATIVE    The format for the machine LABEL-CREATE is currently running on.
   LOCAL     Same as NATIVE.
   IEEE      IEEE 754 format, with high-order bytes firt, used with most hosts.
   RIEEE     Reverse IEEE format, like IEEE but with bytes reversed.  Used
             on DECSTATN, all X86, and all AXP except AXP-VMS.
   VAX       VAX format, single precision is VAX F, double is VAX D, used on
             VAX-VMS and AXP-VMS.
.VAR BHOST -CREATE
BHOST provides a means to specify the type of machine the data in the
binary label came from.  If the binary label was created on a machine
type other than the one you are running on, then the binary label data
will likely be in a different data format, e.g. VAX floating point instead
of IEEE floating point.  Setting the proper value for BHOST allows other
VICAR programs to access the image data properly.

BHOST defaults to the value given for HOST (which in turn defaults to the
native format), so BHOST should rarely be needed.  BHOST is ignored if
BINARY is not set.
.page
The valid values may expand as VICAR is ported to other machines, but as
of this writing they are:

   NATIVE    The machine LABEL-CREATE is currently running on (default).
   LOCAL     Same as NATIVE.
   ALLIANT   Alliant FX series computer.
   AXP-LINUX Alpha running Linux.
   AXP-UNIX  Alpha running Digital Unix.
   AXP-VMS   Alpha running VMS.
   CRAY      Cray (port is incomplete).
   DECSTATN  DECstation (any DEC MIPS-based RISC machine) running Ultrix.
   HP-700    HP 9000 Series 700 workstation.
   MAC-AUX   Macintosh running A/UX.
   MAC-MPW   Macintosh running native mode with Mac Programmers Workbench.
   SGI       Silicon Graphics workstation.
   SUN-3     Sun 3, any model.
   SUN-4     Sun 4 or SPARCstation, or clone such as Solbourne, running SunOS 4.
   SUN-SOLR  Sun (or compatible) running Solaris.
   TEK       Tektronix workstation.
   VAX-VMS   VAX running VMS.
   X86-LINUX Intel processor running Linux.
   X86-SOLR  Intel processor running Solaris.
.VAR BINTFMT -CREATE
BINTFMT specifies the format used to represent integers in the image.
It should rarely be used, as it defaults to the integer format for the
machine specified in BHOST.  BINTFMT is ignored if BINARY is not set.

The valid values may expand as VICAR is ported to other machines, but as
of this writing they are:

   NATIVE    The format for the machine LABEL-CREATE is currently running on.
   LOCAL     Same as NATIVE.
   HIGH      High byte first, "big endian", used with most hosts.
   LOW       Low byte first, "little endian", used with VAX-VMS, DECSTATN,
             and all AXP and X86 machines.
.VAR BREALFMT -CREATE
BREALFMT specifies the format used to represent floating-point data in the
image.  It should rarely be used, as it defaults to the floating-point
format for the machine specified in BHOST.  BREALFMT is ignored if BINARY
is not set.

The valid values may expand as VICAR is ported to other machines, but as
of this writing they are:

   NATIVE    The format for the machine LABEL-CREATE is currently running on.
   LOCAL     Same as NATIVE.
   IEEE      IEEE 754 format, with high-order bytes firt, used with most hosts.
   RIEEE     Reverse IEEE format, like IEEE but with bytes reversed.  Used
             on DECSTATN, all X86, and all AXP except AXP-VMS.
   VAX       VAX format, single precision is VAX F, double is VAX D, used on
             VAX-VMS and AXP-VMS.
.VAR BLTYPE -CREATE
BLTYPE is a string specifying the type of the binary label, if present.
It is optional, but is highly recommended to document what kind of binary
label is present.  No checking is performed on the value for BLTYPE, but
it should correspond to one of the registered binary label type names.
Consult the cognizant programmer for the program that created the image if
you do not know the binary label type name.
!
!=======================================================================
!
.SUBCMD DELETE
LABEL-DELETE can be used to delete all or part of the history or
property label information in a given VICAR image file.  The extent
of deletion is controlled by the different parameters.  For help
on the operation of each parameter, type HELP parameter-name
from tutor mode.

If only an input file is given, the input file label is modified,
and no output is produced.  If an output file is given, then
the input file is not modified and the output file is copied 
from the input.

NOTE:  The addition of Property instances is backwards compatible except
for LABEL-DELETE.  If INSTNCES is not specified, LABEL-DELETE will
operate on *all* instances of the given properties (just like it works
with history/tasks), rather than just the first property (which would
be the backwards-compatible way).
.PAGE
Examples:

LABEL-DELETE A B

The entire history label is deleted from A, and the result
is written to B.  Only the system and property labels remain.

LABEL-DELETE A 'PROPERTY

The entire property label is deleted from A.  The system and
history labels remain.

LABEL-DELETE A KEYS=COMMENT

Every occurrence of the item COMMENT is deleted from the history label
of A, leaving the rest of the label intact.  No output file is produced.
.page
Examples (cont.)

LABEL-DELETE A KEYS=/ALL 'HISTORY

Every keyword is deleted from each history task, leaving the history
subsets themselves intact. 

LABEL-DELETE A B TASKS=(GEOMA,LABEL)

Deletes every occurence of the history subsets created
by the tasks GEOMA and LABEL (including the current task).

LABEL-DELETE A B PROPERTY=MAP

Deletes the entire MAP property (all instances), including the property header.

LABEL-DELETE A B PROPERTY=MAP INSTNCES=1

Deletes the entire MAP property (first instance only), including the
property header.
.page
Examples (cont.)

LABEL-DELETE A B KEYS=(NAH,COMMENT) TASKS=(LABEL,GEOMA) INSTNCES=(1,2)

Here let's assume that GEOMA was run on A once, and it wrote the
item NAH.  LABEL was run on the image twice previously (making 
the current task instance 3), writing the keyword COMMENT only
once.  The item NAH will then be deleted from the first (only)
instance of GEOMA, and COMMENT will be deleted from the one
instance of LABEL in which it is found.

LABEL-DELETE A B KEYS=MANYVAL ELEMENT=3 NELEMENT=2

Elements 3 and 4 will be deleted from all occurrences of the
item MANYVAL in the history label.  If any occurrence of MANYVAL
has 2 or less elements, nothing will be changed for that occurrence.
If any occurrence of MANYVAL has 3 elements, only the third will
be deleted.
.page
Examples (cont.)

LABEL-DELETE A B KEYS=YOW TASK=FUNSTUF INSTNCES=2 ELEMENT=5 NELEMENT=1

Element 5 will be deleted from the item YOW found in the
second instance of the task FUNSTUF.

LABEL-DELETE A B KEYS=COORD PROPERTY=MINE ELEMENT=2 NELEMENT=2

Elements 2 and 3 will be deleted from the item COORD found in the
property subset MINE.

LABEL-DELETE A B KEYS=/ALL ELEMENT=2 NELEMENT=-1

Elements 2 through the end of the item will be deleted for every
item in the history label.  The result will be that all history label
items in the file become single-valued.
!
.VAR INP -DELETE
Name of a single input file.  It must have a valid VICAR label.

If no output file is given, the input file label is modified.
.VAR OUT -DELETE
Name of an optional output file.  If an output file name is 
given, the modified label and the image data are copied to the
output file, and the input file remains unchanged.

If no output file name is given, the input file label is
modified directly.
.VAR KEYS -DELETE
Indicates the names, or keywords, of specific items to be deleted.
If nothing is given under TASKS or PROPERTY, then every occurrence
of the given keywords will be deleted from the appropriate (history
or property) label section.  A list of specific subsets can be given
with the TASKS or PROPERTY and INSTNCES parameters to narrow the
scope of the deletion.  ELEMENT and NELEMENT may also be used to
limit what is deleted.

In addition, the value /ALL may be given for this parameter,
specifying that every key is to be deleted provided it does not
form part of a subset header.  The effect is to empty the subset
without deleting the subset itself.
.VAR TASKS -DELETE
TASKS can be used to limit the deletion process to certain
history subsets or task labels within the history label.

If items were given under the parameter KEYS, then the task
names given limit the deletion of those items to the named tasks.
Otherwise, the subsets named by TASKS are deleted.

The parameter INSTNCES can be used to limit which tasks are looked in
for items (if KEYS is given) or which tasks are deleted (if KEYS not given).

The presence of TASKS implies that the history label will be modified,
so TYPE is unnecessary.  If you wish to delete a property label, use
PROPERTY instead.  If neither TASKS nor PROPERTY are given, and TYPE is
omitted, then the HISTORY label is assumed.

An error occurs if both TASKS and PROPERTY are given, or if TASKS is
given with TYPE=PROPERTY.
.VAR INSTNCES -DELETE
The parameter INSTNCES can be used to limit the history tasks or property
sets in which items are deleted (if KEYS is given), or the tasks or
properties which are deleted (if KEYS is not given).

If specific task/property names were given in TASKS/PROPERTY, then each
value given for INSTNCES will be applied to every subset task name.  If
nothing was specified in TASKS/PROPERTY (and TYPE is set), then every task
or property with the named instances will be deleted.

 For example, TASKS=(STRETCH,LGEOM) INSTNCES=(1,4)

would refer to the first and fourth instances of both the STRETCH
task and the LGEOM task.  If one or more of the tasks/properties does not
exist, an informational message is issued, but no fatal error occurs.

.VAR PROPERTY -DELETE
PROPERTY can be used to limit the deletion process to certain
property subsets within the property label.

If items were given under the parameter KEYS, then the property
names given limit the deletion of those items to the named property.
Otherwise, the subsets named by PROPERTY are deleted.

The parameter INSTNCES can be used to limit which properties are looked
in for items (if KEYS is given) or which properties are deleted (if KEYS
not given).

The presence of PROPERTY implies that the property label will be modified,
so TYPE is unnecessary.  If you wish to delete a history label, use
TASKS instead, or specify TYPE=HISTORY.  If neither TASKS nor PROPERTY
are given, and TYPE is omitted, then the HISTORY label is assumed.

An error occurs if both TASKS and PROPERTY are given, or if PROPERTY is
given with TYPE=HISTORY.

NOTE:  The addition of Property instances is backwards compatible except
for LABEL-DELETE.  If INSTNCES is not specified, LABEL-DELETE will
operate on *all* instances of the given properties (just like it works
with history/tasks), rather than just the first property (which would
be the backwards-compatible way).
.VAR ELEMENT -DELETE
Specifies the starting element to delete in each item for multi-valued
label items.  This parameter limits the action of the other parameters.
Without ELEMENT, each selected item is deleted in its entirety.  With
ELEMENT, only part of each selected item is normally deleted.

If the element specified does not exist in a particular item, then
no action is taken on that item, and no warning is issued.

If ELEMENT is given as 1 (the default value), then the entire
item may be deleted, depending on the value of NELEMENT.

This parameter is normally used in conjunction with NELEMENT.
.VAR NELEMENT -DELETE
Specifies the number of elements to delete in each item for multi-valued
label items.  This parameter limits the action of the other parameters.
NELEMENT defaults to -1.

If NELEMENT is given as -1 (or defaulted), then all the remaining
elements in the item are deleted.  If ELEMENT is greater than 1, it
deletes all elements from ELEMENT to the end of the item.  If ELEMENT
is 1, the entire item is deleted.

If there are not enough elements in a particular item to satisfy
the request, then the number of elements deleted is reduced to fit.
No warning is issued.  For example, if an item has 5 elements, and you
request ELEMENT 4, NELEMENT 3, that would mean you want to delete
elements 4, 5, and 6.  Since there are only 5 elements, only 4 and 5
are deleted.

This parameter is normally used in conjunction with ELEMENT.
.VAR TYPE -DELETE
Specifies whether the items will be deleted from the HISTORY or PROPERTY
label.  If TYPE is not specified, the type is assumed from the presence
of the TASKS or PROPERTY parameters.  If neither TASKS, PROPERTY, or
TYPE is specified, then HISTORY labels are assumed.

If both the TASKS and PROPERTY labels are specified, or if they disagree
with a given TYPE, then an error occurs.
!
!=======================================================================
!
.SUBCMD LIST
LABEL-LIST is used to list out the contents of the label in 
various formats (specified by EXTENT).  Up to 10 input
files may be given.  No processing is done on any of the
files.

Multi-valued items are listed enclosed in parentheses and
separated by commas.
.VAR INP -LIST
The names of from one to ten input files.  The input files
must have valid labels.
.VAR EXTENT -LIST
EXTENT specifies how and how much of the label is 
to be listed.  The valid values are:

      ALL       SYSTEM, PROPERTY, and HISTORY together.
      SYSTEM    Lists out the important system label information
                in a readable format.
      PROPERTY  Lists out a header for each property subset,
                followed by all keyword-value pairs in that subset.
      TASKS     Lists out a header for each history subset task,
                containing the task name, name of the user who ran the
                task, and the date and time of the task.
      HISTORY   Lists out a header for each history subset task,
                followed by all keyword-value pairs in that subset.
      DUMP      Lists out every keyword-value pair in the label.
                No attempt is made to format beyond the most basic level.
.VAR PACK -LIST
PACK specifies whether or not to pack more than one label item onto
one line in the output.  Valid values are PACK and NOPACK.  NOPACK
is the default.

If NOPACK is in effect, every label item is listed on a line by
itself.  If there is more than one element in an item, as many
elements as will fit are listed on the same line.

If PACK is in effect, then label items are packed together as much
as possible on the output line, separated by spaces.  If there is
not room for the label key and at least one of the values, a new
line is started.  Only label items from one property or history
subset are packed together.

PACK and NOPACK apply when EXTENT is PROPERTY, HISTORY, or DUMP.
It also applies when EXTENT is ALL, but only to the property and
history portions.
!
!=======================================================================
!
.SUBCMD REMOVE
LABEL-REMOVE can be used to remove the the VICAR label, binary header,
binary prefix, or all three (the default) from the input, and write the
results with the data in an output file. If the output is a tape, the
NOBLOCK keyword may be specified to prevent the tape from being blocked
(useful for sending to external sites).
.VAR INP -REMOVE
VICAR labeled input file whose label is to be removed.  The input
file data will be copied to the output, and the input file will
remain unchanged.
.VAR OUT -REMOVE
The required output file.  By default, the output file will consist only
of image data and will have no label (but see the BINARY parameter).

Take care to record the image size of the output file, since VICAR
may not be able to determine the proper size if needed on a future
date.
.VAR SIZE -REMOVE
SIZE can be used to copy only a portion of the input to the output
file.  It is in standard VICAR size field format

	SIZE=(sl,ss,nl,ns)	,

where 		sl	is the starting line to read,
		ss	is the starting sample,
		nl	is the number of lines to read,
		ns	is the number of samples to be
			extracted from each line.

Note that SIZE works only in the line and sample directions--to provide
a complete subsection specification, you must also use the BANDS parameter.

SIZE is ignored if BINARY is set.
.VAR BANDS -REMOVE
BANDS can be used to copy only a portion of the input to the output
file.  It has the following format:

	BANDS= (sb,nb)

where 		sb	is the starting band to read,
		nb	is the number of bands to read.

The output file size will be nl x ns x nb, where nl and ns are
specified with the SIZE parameter.

BANDS is ignored if BINARY is in effect.
.VAR BLOCKING -REMOVE
If NOBLOCK is specified, then if the output is tape the tape will
not be blocked, that is, there will be one record per block on the
tape.  If NOBLOCK is not specified, then the output tape will be in
blocks of in the neighborhood of 20,000 bytes.

If the output is disk, this parameter has no effect.
.VAR BINARY -REMOVE
This keyword controls whether to include binary headers and/or prefixes
in the output.  Note that the NOBINHEAD and NOBINPREF options RETAIN THE
VICAR LABEL!

BINARY causes LABEL-REMOVE to remove only the VICAR label; the binary
header and prefix are both retained.
 
NOBINARY causes LABEL-REMOVE to remove the binary label (both header and
prefix) along with the VICAR label.

NOBINHEAD causes LABEL-REMOVE to remove only the binary header.  The
VICAR label is retained.

NOBINPREF causes LABEL-REMOVE to remove only the binary prefix of
image lines.  The VICAR label is retained.

The default is NOBINARY.

If anything other than NOBINARY is specified, then the SIZE and BANDS
parameters are disabled, so the entire file will be copied.
!
!=======================================================================
!
.SUBCMD REPLACE
LABEL-REPLACE is used in the same fashion as LABEL-ADD, except

1)  The item must already exist, and
2)  The item may be in the system label as well as the history and
    property labels.

If TASK is specified, TYPE=HISTORY is assumed and an error occurs if
TYPE is different.  Likewise, if PROPERTY is specified, TYPE=PROPERTY
is assumed.

To replace an item in the system label, the keyword SYSTEM must be
given, and TASK and PROPERTY may not be specified.  Great care should
be given to replacing any system items, however, as it may affect how
the data is read by the executive.  In particular, NS, RECSIZE, BUFSIZE,
and LBLSIZE should never be changed.  The results would be unpredictable.
.VAR INP -REPLACE
INP specifies the name of the input file.  The input must have a valid
VICAR label.

Note:  If no output file name is given, the input file's label will 
be modified directly.
.VAR OUT -REPLACE
OUT is the name of the output file to which the data and the modified
label are to be written.

If OUT is omitted from the command line, the label items are replaced
in the input file directly.
.VAR ITEMS -REPLACE
The label items which are to be replaced. The items are given as a
list in a single string parameter, in the form

key(element)=value-list

where "key" is a valid (0 - 8 character) label keyword, "element"
is an optional starting element number (an integer), and "value-list"
is the list of values to be associated with that keyword.

The operation differs slightly depending on whether or not the
starting element number is given.

If the element is not given, i.e. the item is of the form "key=value-list",
then the entire label item is replaced.  The indicated keyword is
first deleted from the label in its entirety, then the new item is
added with values specified in "value-list".  The keyword must
previously exist in the label, or an error message will be issued.
.page
If the element is given, then only parts of the label item are
affected.  If the keyword currently exists in the file, then the
indicated values replace existing elements in it, starting at element
number "element".  For instance, if the label item "LIST" had four
values (1,2,3,4), and the following was in the label-replace ITEM
string: "LIST(2)=(10,11)", then values 10 and 11 would be put into
LIST starting at position 2, replacing the old values at those
positions.  The new value of LIST would be (1,10,11,4).

If the element is given, but the keyword does not currently exist
in the file, the element number is ignored and a new label item
is created containing the values in "value-list".  This is slightly
different from the action when "element" is not given, as the item
does not have to previously exist.
.page
A value of -1 for the element number indicates the end of the label
item, i.e. all elements will be appended to the end of the item.

If the same keyword appears twice in "value-list", each is treated
independently, i.e. the first one is used, then the second one.  If
the first one causes the number of elements or the relative position
of an element to change, the second one uses the NEW positions, values,
and number of elements.
.page
"Value-list" is a list of values, enclosed in parentheses, and
separated by commas.  If there is only one value, the parentheses
are not necessary.

All of the values in "value-list" should be of the same type:
integer, real, doub(le) or string.  The type should also match the type
of the existing label, if "element" is given.  The LABEL program will
try to convert to the appropriate type, but if it can't it will give
an error.  It can convert integers to real, doub(le) or string, and 
reals to doub(le) or string.  Any other conversion is an error.

The four types are differentiated by the way in which they are
specified. Strings should be contained in single quotes.  The quotes
are not strictly necessary if there are no commas, spaces, or parentheses
in the string, but they should normally be included anyway.  Standard
FORTRAN notation for integers and reals is understood. Doub(le)s
are real values with more than 6 significant digits.              

.PAGE
Examples of valid "value-list" specifications:

Specification			Meaning
-------------			-------
787				single integer
(21)				single integer
(6,-9,42)			list of three integers
+3e2				single real
-2.000001			single doub(le)
(3.14159, 2)			list of two reals.  The integer 2 is
				  converted to a real: (3.14159, 2.0)
'a string item'			single string
('another string item')		single string
('string 1','string 2')		list of two strings
('string 1', 123, 4.5, wow)	list of four strings.  The values get converted
				  to strings: ('string 1', '123', '4.5', 'wow')
.page
Using this input format, the entire list of items is input
as a single string, such as

"slope=-45.8, planet='Jupiter' avgdn = 128 coord=(86.3, 44.8)"

This string replaces the real value "slope", the string value
"planet", the integer value "avgdn", and the two element real
value "coord".

.VARI TASK -REPLACE
The name of the task heading or history subset under which the label
item is to be stored.

The presence of TASK implies that the history label will be modified,
so TYPE is unnecessary.  If you wish to replace a property label, use
PROPERTY instead.  If neither TASK nor PROPERTY are given, and TYPE is
omitted, all labels will be placed in the history label under the
heading of the current task (LABEL if an output file is given, the
last task otherwise).

An error occurs if both TASK and PROPERTY are given, or if TASK is
given with TYPE=PROPERTY or TYPE=SYSTEM.
.VAR INSTANCE -REPLACE
Since the same program may be run on an image several times, it is
possible to have multiple history task headings with the same name.
Property sets may also have more than one instance (e.g. different
versions of a camera model).  INSTANCE=n is used to refer to the
n-th occurence of the named history task or property set.  If INSTANCE
is not specified, 1 is assumed.

If TASK or PROPERTY is not specified, then INSTANCE is ignored.
.VAR PROPERTY -REPLACE
The name of the property subset under which the label item is to be
stored.

The presence of PROPERTY implies that the property label will be
modified, so TYPE is unnecessary.  If you wish to replace a history
label, use TASK instead.  PROPERTY is mandatory for property labels,
so even if you say TYPE=PROPERTY you must still specify a property name.

An error occurs if both PROPERTY and TASK are given, or if PROPERTY is
given with TYPE=HISTORY or TYPE=SYSTEM.
.VARI TYPE -REPLACE
TASK specifies whether the items will be replaced in the SYSTEM,
HISTORY, or PROPERTY label.  TYPE is not normally needed for history
and property labels, since the presence of the TASK parameter implies
history labels, while the presence of the PROPERTY parameter implies
property labels.  TYPE may be used for error-checking, however, as
TASK, PROPERTY, and TYPE must all agree on the type of labels being added.

To replace an item in the system label, the keyword SYSTEM must be given.
Great care should be given to replacing any system items, however, as 
it may affect how the data is read be the executive.  In particular,
NS, RECSIZE, BUFSIZE, and LBLSIZE should never be changed.  The results
would be unpredictable.  If TYPE=SYSTEM is given, then TASK and PROPERTY
may not be used.
!
!=======================================================================
!
.SUBCMD SWITCH
LABEL-SWITCH creates an output file containing the following:
		the history and property labels of the first input file 
		the system label of the second input 
		the data of the second input file 

INVOCATION:

	LABEL-SWITCH (A,B) C

where A, B, and C are VICAR labeled files.
.VAR INP -SWITCH
The two input files whose labels are to be switched.  See help
on LABEL-SWITCH for all the details.
.VAR OUT -SWITCH
A single output file containing the data of the second input and
the history and property labels of the first input.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstlabel.pdf
procedure
refgbl $echo
refgbl $autousage
refgbl $syschar
!"WMS_TEST_WORK:[TESTDATA.GLL]s0061000300.1"
body
let _onfail="continue"
let $echo="yes"
let $autousage="none"
local testfile type=string init="/project/test_work/testdata/gll/s0061000300.1"
if ($syschar(1) = "VAX_VMS")
  let testfile = "WMS_TEST_WORK:[TESTDATA.GLL]s0061000300.1"
end-if
write "		Test for program LABEL"
write ""
write "Make sure file &testfile is available"
if ($syschar(1) = "VMS")
  write "Make sure you have specified a VMS-style pathname (default is Unix)"
end-if
!
write ""
write "Generate a file with some labels first"
gen t1 10 10  
insert t1 t2
stretch t2 t3
insert t3 t4
write "Now list out the labels"
label-list t4
!
write "	-ADD -- add some label items to the existing file (nocopy)"
label-add t4 items="slope=5.6, time=45 name='No name' mv=(1.2,3.5, 4.6,7.8,9)"
label-add t4 items="dslope=5.00000012" 
label-list t4 'hist
label-add t4 items="comment='straight data copy' comment(2)='second comment'" task=insert inst=1
label-list t4
label-add t4 items="proj=mercator center=(45,12.7) line=5 samp=5" property=tstmap
label-add t4 items="scale=10.0" prop=tstmap
label-list t4 'prop
label-add t4 items="red=(1,2,3) green=(4,5,6) blue=(7,8,9)" prop=tstlut
label-list t4
label-add t4 items="red=(11,12,13) green=(14,15,16) blue=(17,18,19)" prop=tstlut instance=2
label-list t4
write " Update existing label"
label-add t4 items="proj=cylindrical samp=10 longitude=10.5" property=tstmap 'prop 'update
label-list t4
write " Now add items using data copy"
label-add t4 t5 items="action=('LABEL-ADD', '-- copy option used') long=(1,4,5,6,7,8,9,10)"
label-list t5 'dump
label-add t5 items="long(2)=(2,3)"
label-add t5 items="red(4)=4 green(4)=7 blue(4)=10" prop=tstlut
label-list t5 'dump 'pack
!
write " -CONCAT -- Add label sets to other images"
gen t6
label-concat (t5,t6)
label-list t6
gen t6
label-concat (t5,t6) 'prop
label-list t6
gen t6
label-concat (t5,t6) task=stretch
label-list t6
gen t6
label-concat (t5,t6) t7 prop=tstlut instance=2
label-list t7
!
write " -DELETE -- Delete different items"
label-delete t5 task=insert key=mv element=3 nelement=2
label-list t5 'hist 'pack
label-delete t5 task=insert
label-list t5 'hist
label-delete t5 task=gen key=/ALL
label-list t5
label-delete t5 key=(red,green) prop=tstlut element=1 nelement=1 instnces=2
label-list t5 'prop
label-delete t5 key=(red,green,blue) prop=tstlut element=1 nelement=1
label-list t5 'prop
label-delete t5 t6 key=/all prop=tstmap
label-list t6 'prop
label-delete t6 'property
label-list t6
!
write " -REPLACE -- Replace specific label items"
label-repla t4 item="nl=5" 'sys
label-list t4
label-repla t3 t4 "ival=9999" task=gen inst=1
label-list t4
label-repla t5 item="long(5)=(50,60)"
label-repla t5 item="red(2)=30 green(2)=60" prop=tstlut
label-repla t5 item="blue(2)=(90,100)" prop=tstlut instance=2
label-list t5 'pack
!
write " Now make everything single-valued by deleting elements > 1"
label-delete t5 t6 key=/ALL element=2 nelement=-1 'hist
label-delete t6 key=/ALL element=2 nelement=-1 'prop
label-list t6
copy t4 t7
!label-create t7 t4 NL=10 NB=10 NLB=5 NBB=64 'BINARY
label-list t4
write " -REMOVE -- Remove the binary header" 
label-remove t4 t1 'NOBINHEAD
write "	attempt to list should be successful" 
label-list t1
write " -REMOVE -- Remove the binary prefix" 
label-remove t4 t1 'NOBINPREF
write "	attempt to list should be successful" 
label-list t1
write " -REMOVE -- Remove all the header" 
label-remove t4 t1
write "	attempt to list should cause an error"
label-list t1
write " -CREATE -- add a label then list it out"
label-create t1 t6 nl=10 ns=10 comment="What a slick image"
label-list t6
!
write " -SWITCH -- gen a different size file then switch the labels"
gen t1 5 5 linc=10 
list t1
label-switch (t5,t1) t2
list t2
label-list t2
!
write ""
write "Generate a 3-D file with some labels first"
gen t1 10 10 3 'BIL
copy t1 t7
!label-create t7 t1 NL=10 NS=10 NB=3 NLB=5 NBB=64 'BINARY
label-list t1
!write " -REMOVE -- Remove the binary header" 
!label-remove t1 t2 'NOBINHEAD
write "	attempt to list should be successful" 
label-list t2
write " -REMOVE -- Remove the binary prefix" 
label-remove t1 t2 'NOBINPREF
write "	attempt to list should be successful" 
label-list t2
write " -REMOVE -- Remove the binary header and prefix" 
label-remove t1 t2
write "	attempt to list should cause an error"
label-list t2
write " -CREATE -- add a label"
label-create t2 t3 ns=10 nl=10 nb=3 org=BIL
label-list t3
write " -CREATE -- different host formats"
label-create t2 t3 ns=10 nl=10 nb=3 org=BIL host=VAX-VMS
label-list t3 'sys
label-list t3 'dump
label-create t2 t3 ns=10 nl=10 nb=3 org=BIL host=SUN-4
label-list t3 'sys
label-list t3 'dump
write " test handling long records. " 
gen t1 10 200000 
label-add t1 t4 items="slope=5.6"
label-list t4
!
write "multi-input option and abend with -LIST"
label-list (t1 t2 t3 t4)
! make sure xxxx doesn't exist
ush rm xxxx
write "this should generate an error msg but no abend:"
label-list (t1 xxxx t3 t4)
write "this should generate an abend:"
label-list xxxx 
!
if ($syschar(1) = "UNIX")
  ush rm t1 t2 t3 t4 t5 t6 t7
else
  dcl delete t1.z*;
  dcl delete t2.z*;
  dcl delete t3.z*;
  dcl delete t4.z*;
  dcl delete t5.z*;
  dcl delete t6.z*;
  dcl delete t7.z*;
end-if
!
label-list "&testfile"
label-remove "&testfile" test.nolabel 'BINARY
write " attempt to list should cause error "
label-list test.nolabel
label-create test.nolabel test.vpds NL=800 NS=800 NLB=2 NBB=200 'BINARY
label-list test.vpds
difpic ("&testfile",test.vpds)
label-create test.nolabel test.binary nl=800 ns=800 nlb=2 nbb=200 'bin +
	bhost=vax-vms bltype="tstpds"
label-list test.binary 'sys
label-list test.binary 'dump
label-create test.nolabel test.binary nl=800 ns=800 nlb=2 nbb=200 'bin +
	bhost="vax-vms" brealfmt="vax" bltype="tstpds"
label-list test.binary
label-list test.binary 'dump
write "The following commands test the new binary features of LABEL"
label-add test.vpds test.added items="LIST=(1,2)"
label-add test.added items="test=binary" prop=tstprop
label-list test.added
difpic (test.vpds,test.added)
label-del test.added test.deleted keys="LIST" tasks="LABEL"
label-del test.deleted keys="TEST" prop=tstprop
label-list test.deleted
difpic (test.vpds,test.deleted)
label-switch (test.added,test.deleted) test.switch
label-list test.switch
difpic (test.vpds,test.switch)
!
if ($syschar(1) = "UNIX")
  ush rm test.nolabel test.vpds test.binary test.added test.deleted test.switch
else
  dcl delete test.nolabel;
  dcl delete test.vpds;
  dcl delete test.binary;
  dcl delete test.added;
  dcl delete test.deleted;
  dcl delete test.switch;
end-if
!
end-proc
$!-----------------------------------------------------------------------------
$ create tstlabel.log_solos
tstlabel
let _onfail="continue"
let $echo="yes"
let $autousage="none"
local testfile type=string init="/project/test_work/testdata/gll/s0061000300.1"
if ($syschar(1) = "VAX_VMS")
end-if
write "		Test for program LABEL"
		Test for program LABEL
write ""

write "Make sure file /project/test_work/testdata/gll/s0061000300.1 is available"
Make sure file /project/test_work/testdata/gll/s0061000300.1 is available
if ($syschar(1) = "VMS")
end-if
write ""

write "Generate a file with some labels first"
Generate a file with some labels first
gen t1 10 10
Beginning VICAR task gen
GEN Version 6
GEN task completed
insert t1 t2
Beginning VICAR task insert
INSERT version 02-MAY-94
stretch t2 t3
Beginning VICAR task stretch
STRETCH version Oct 17 2002
DN values      0 and    255 excluded

Histogram after exclusion: Mean=     9.0909 Sigma=     3.9800
*** AUTO-STRETCH OPTION ***
Percent saturation at low end=  1.00 at high end=  1.00
AUTO-STRETCH:      1 to      0 and     18 to    255
insert t3 t4
Beginning VICAR task insert
INSERT version 02-MAY-94
write "Now list out the labels"
Now list out the labels
label-list t4
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File t4 ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in BYTE format from a SUN-SOLR host
                1 bands
                10 lines per band
                10 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: GEN -- User: lwk -- Sat Nov 13 10:55:57 2010 ----
IVAL=0.0
SINC=1.0
LINC=1.0
BINC=1.0
MODULO=0.0
---- Task: INSERT -- User: lwk -- Sat Nov 13 10:55:57 2010 ----
---- Task: STRETCH -- User: lwk -- Sat Nov 13 10:55:58 2010 ----
PARMS='AUTO-STRETCH:      1 to      0 and     18 to    255'
---- Task: INSERT -- User: lwk -- Sat Nov 13 10:55:58 2010 ----
 
************************************************************
write "	-ADD -- add some label items to the existing file (nocopy)"
	-ADD -- add some label items to the existing file (nocopy)
label-add t4 items="slope=5.6, time=45 name='No name' mv=(1.2,3.5, 4.6,7.8,9)"
Beginning VICAR task label
LABEL version 15-Nov-2010
Keyword slope added
Keyword time added
Keyword name added
Keyword mv added
label-add t4 items="dslope=5.00000012"
Beginning VICAR task label
LABEL version 15-Nov-2010
Keyword dslope added
label-list t4 'hist
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File t4 ************
---- Task: GEN -- User: lwk -- Sat Nov 13 10:55:57 2010 ----
IVAL=0.0
SINC=1.0
LINC=1.0
BINC=1.0
MODULO=0.0
---- Task: INSERT -- User: lwk -- Sat Nov 13 10:55:57 2010 ----
---- Task: STRETCH -- User: lwk -- Sat Nov 13 10:55:58 2010 ----
PARMS='AUTO-STRETCH:      1 to      0 and     18 to    255'
---- Task: INSERT -- User: lwk -- Sat Nov 13 10:55:58 2010 ----
SLOPE=5.6
TIME=45
NAME='No name'
MV=(1.2, 3.5, 4.6, 7.8, 9.0)
DSLOPE=5.00000012
 
************************************************************
label-add t4 items="comment='straight data copy' comment(2)='second comment'" task=insert inst=1
Beginning VICAR task label
LABEL version 15-Nov-2010
Keyword comment added
Element 2 added to keyword comment
label-list t4
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File t4 ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in BYTE format from a SUN-SOLR host
                1 bands
                10 lines per band
                10 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: GEN -- User: lwk -- Sat Nov 13 10:55:57 2010 ----
IVAL=0.0
SINC=1.0
LINC=1.0
BINC=1.0
MODULO=0.0
---- Task: INSERT -- User: lwk -- Sat Nov 13 10:55:57 2010 ----
COMMENT=('straight data copy', 'second comment')
---- Task: STRETCH -- User: lwk -- Sat Nov 13 10:55:58 2010 ----
PARMS='AUTO-STRETCH:      1 to      0 and     18 to    255'
---- Task: INSERT -- User: lwk -- Sat Nov 13 10:55:58 2010 ----
SLOPE=5.6
TIME=45
NAME='No name'
MV=(1.2, 3.5, 4.6, 7.8, 9.0)
DSLOPE=5.00000012
 
************************************************************
label-add t4 items="proj=mercator center=(45,12.7) line=5 samp=5" property=tstmap
Beginning VICAR task label
LABEL version 15-Nov-2010
Keyword proj added
Keyword center added
Keyword line added
Keyword samp added
label-add t4 items="scale=10.0" prop=tstmap
Beginning VICAR task label
LABEL version 15-Nov-2010
Keyword scale added
label-list t4 'prop
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File t4 ************
---- Property: TSTMAP ----
PROJ='mercator'
CENTER=(45.0, 12.7)
LINE=5
SAMP=5
SCALE=10.0
 
************************************************************
label-add t4 items="red=(1,2,3) green=(4,5,6) blue=(7,8,9)" prop=tstlut
Beginning VICAR task label
LABEL version 15-Nov-2010
Keyword red added
Keyword green added
Keyword blue added
label-list t4
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File t4 ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in BYTE format from a SUN-SOLR host
                1 bands
                10 lines per band
                10 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Property: TSTMAP ----
PROJ='mercator'
CENTER=(45.0, 12.7)
LINE=5
SAMP=5
SCALE=10.0
---- Property: TSTLUT ----
RED=(1, 2, 3)
GREEN=(4, 5, 6)
BLUE=(7, 8, 9)
---- Task: GEN -- User: lwk -- Sat Nov 13 10:55:57 2010 ----
IVAL=0.0
SINC=1.0
LINC=1.0
BINC=1.0
MODULO=0.0
---- Task: INSERT -- User: lwk -- Sat Nov 13 10:55:57 2010 ----
COMMENT=('straight data copy', 'second comment')
---- Task: STRETCH -- User: lwk -- Sat Nov 13 10:55:58 2010 ----
PARMS='AUTO-STRETCH:      1 to      0 and     18 to    255'
---- Task: INSERT -- User: lwk -- Sat Nov 13 10:55:58 2010 ----
SLOPE=5.6
TIME=45
NAME='No name'
MV=(1.2, 3.5, 4.6, 7.8, 9.0)
DSLOPE=5.00000012
 
************************************************************
label-add t4 items="red=(11,12,13) green=(14,15,16) blue=(17,18,19)" prop=tstlut instance=2
Beginning VICAR task label
LABEL version 15-Nov-2010
Keyword red added
Keyword green added
Keyword blue added
label-list t4
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File t4 ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in BYTE format from a SUN-SOLR host
                1 bands
                10 lines per band
                10 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Property: TSTMAP ----
PROJ='mercator'
CENTER=(45.0, 12.7)
LINE=5
SAMP=5
SCALE=10.0
---- Property: TSTLUT ----
RED=(1, 2, 3)
GREEN=(4, 5, 6)
BLUE=(7, 8, 9)
---- Property: TSTLUT (#2) ----
RED=(11, 12, 13)
GREEN=(14, 15, 16)
BLUE=(17, 18, 19)
---- Task: GEN -- User: lwk -- Sat Nov 13 10:55:57 2010 ----
IVAL=0.0
SINC=1.0
LINC=1.0
BINC=1.0
MODULO=0.0
---- Task: INSERT -- User: lwk -- Sat Nov 13 10:55:57 2010 ----
COMMENT=('straight data copy', 'second comment')
---- Task: STRETCH -- User: lwk -- Sat Nov 13 10:55:58 2010 ----
PARMS='AUTO-STRETCH:      1 to      0 and     18 to    255'
---- Task: INSERT -- User: lwk -- Sat Nov 13 10:55:58 2010 ----
SLOPE=5.6
TIME=45
NAME='No name'
MV=(1.2, 3.5, 4.6, 7.8, 9.0)
DSLOPE=5.00000012
 
************************************************************
write " Update existing label"
 Update existing label
label-add t4 items="proj=cylindrical samp=10 longitude=10.5" property=tstmap 'prop 'update
Beginning VICAR task label
LABEL version 15-Nov-2010
Keyword proj replaced
Keyword samp replaced
Keyword longitude replaced
label-list t4
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File t4 ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in BYTE format from a SUN-SOLR host
                1 bands
                10 lines per band
                10 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Property: TSTMAP ----
CENTER=(45.0, 12.7)
LINE=5
SCALE=10.0
PROJ='cylindrical'
SAMP=10
LONGITUDE=10.5
---- Property: TSTLUT ----
RED=(1, 2, 3)
GREEN=(4, 5, 6)
BLUE=(7, 8, 9)
---- Property: TSTLUT (#2) ----
RED=(11, 12, 13)
GREEN=(14, 15, 16)
BLUE=(17, 18, 19)
---- Task: GEN -- User: lwk -- Sat Nov 13 10:55:57 2010 ----
IVAL=0.0
SINC=1.0
LINC=1.0
BINC=1.0
MODULO=0.0
---- Task: INSERT -- User: lwk -- Sat Nov 13 10:55:57 2010 ----
COMMENT=('straight data copy', 'second comment')
---- Task: STRETCH -- User: lwk -- Sat Nov 13 10:55:58 2010 ----
PARMS='AUTO-STRETCH:      1 to      0 and     18 to    255'
---- Task: INSERT -- User: lwk -- Sat Nov 13 10:55:58 2010 ----
SLOPE=5.6
TIME=45
NAME='No name'
MV=(1.2, 3.5, 4.6, 7.8, 9.0)
DSLOPE=5.00000012
 
************************************************************
write " Now add items using data copy"
 Now add items using data copy
label-add t4 t5 items="action=('LABEL-ADD', '-- copy option used') long=(1,4,5,6,7,8,9,10)"
Beginning VICAR task label
LABEL version 15-Nov-2010
Keyword action added
Keyword long added
label-list t5 'dump
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File t5 ************
LBLSIZE=1120
FORMAT='BYTE'
TYPE='IMAGE'
BUFSIZ=24576
DIM=3
EOL=1
RECSIZE=10
ORG='BSQ'
NL=10
NS=10
NB=1
N1=10
N2=10
N3=1
N4=0
NBB=0
NLB=0
HOST='SUN-SOLR'
INTFMT='HIGH'
REALFMT='IEEE'
BHOST='VAX-VMS'
BINTFMT='LOW'
BREALFMT='VAX'
BLTYPE=''
COMPRESS='NONE'
EOCI1=0
EOCI2=0
PROPERTY='TSTMAP'
CENTER=(45.0, 12.7)
LINE=5
SCALE=10.0
PROJ='cylindrical'
SAMP=10
LONGITUDE=10.5
PROPERTY='TSTLUT'
RED=(1, 2, 3)
GREEN=(4, 5, 6)
BLUE=(7, 8, 9)
PROPERTY='TSTLUT'
RED=(11, 12, 13)
GREEN=(14, 15, 16)
BLUE=(17, 18, 19)
TASK='GEN'
USER='lwk'
DAT_TIM='Sat Nov 13 10:55:57 2010'
IVAL=0.0
SINC=1.0
LINC=1.0
BINC=1.0
MODULO=0.0
TASK='INSERT'
USER='lwk'
DAT_TIM='Sat Nov 13 10:55:57 2010'
COMMENT=('straight data copy', 'second comment')
TASK='STRETCH'
USER='lwk'
DAT_TIM='Sat Nov 13 10:55:58 2010'
PARMS='AUTO-STRETCH:      1 to      0 and     18 to    255'
TASK='INSERT'
USER='lwk'
DAT_TIM='Sat Nov 13 10:55:58 2010'
SLOPE=5.6
TIME=45
NAME='No name'
MV=(1.2, 3.5, 4.6, 7.8, 9.0)
DSLOPE=5.00000012
TASK='LABEL'
USER='lwk'
DAT_TIM='Sat Nov 13 10:55:59 2010'
ACTION=('LABEL-ADD', '-- copy option used')
LONG=(1, 4, 5, 6, 7, 8, 9, 10)
 
************************************************************
label-add t5 items="long(2)=(2,3)"
Beginning VICAR task label
LABEL version 15-Nov-2010
Elements 2 through 3 added to keyword long
label-add t5 items="red(4)=4 green(4)=7 blue(4)=10" prop=tstlut
Beginning VICAR task label
LABEL version 15-Nov-2010
Element 4 added to keyword red
Element 4 added to keyword green
Element 4 added to keyword blue
label-list t5 'dump 'pack
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File t5 ************
LBLSIZE=1120  FORMAT='BYTE'  TYPE='IMAGE'  BUFSIZ=24576  DIM=3  EOL=1
RECSIZE=10  ORG='BSQ'  NL=10  NS=10  NB=1  N1=10  N2=10  N3=1  N4=0  NBB=0
NLB=0  HOST='SUN-SOLR'  INTFMT='HIGH'  REALFMT='IEEE'  BHOST='VAX-VMS'
BINTFMT='LOW'  BREALFMT='VAX'  BLTYPE=''  COMPRESS='NONE'  EOCI1=0
EOCI2=0
PROPERTY='TSTMAP'
CENTER=(45.0, 12.7)  LINE=5  SCALE=10.0  PROJ='cylindrical'  SAMP=10
LONGITUDE=10.5  PROPERTY='TSTLUT'  RED=(1, 2, 3, 4)  GREEN=(4, 5, 6, 7)
BLUE=(7, 8, 9, 10)  PROPERTY='TSTLUT'  RED=(11, 12, 13)  GREEN=(14, 15, 16)
BLUE=(17, 18, 19)
TASK='GEN'
USER='lwk'  DAT_TIM='Sat Nov 13 10:55:57 2010'  IVAL=0.0  SINC=1.0
LINC=1.0  BINC=1.0  MODULO=0.0  TASK='INSERT'  USER='lwk'
DAT_TIM='Sat Nov 13 10:55:57 2010'  COMMENT=('straight data copy', 
'second comment')  TASK='STRETCH'  USER='lwk'
DAT_TIM='Sat Nov 13 10:55:58 2010'
PARMS='AUTO-STRETCH:      1 to      0 and     18 to    255'  TASK='INSERT'
USER='lwk'  DAT_TIM='Sat Nov 13 10:55:58 2010'  SLOPE=5.6  TIME=45
NAME='No name'  MV=(1.2, 3.5, 4.6, 7.8, 9.0)  DSLOPE=5.00000012
TASK='LABEL'  USER='lwk'  DAT_TIM='Sat Nov 13 10:55:59 2010'
ACTION=('LABEL-ADD', '-- copy option used')  LONG=(1, 2, 3, 4, 5, 6, 7, 8, 
9, 10)
 
************************************************************
write " -CONCAT -- Add label sets to other images"
 -CONCAT -- Add label sets to other images
gen t6
Beginning VICAR task gen
GEN Version 6
GEN task completed
label-concat (t5,t6)
Beginning VICAR task label
LABEL version 15-Nov-2010
label-list t6
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File t6 ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in BYTE format from a SUN-SOLR host
                1 bands
                10 lines per band
                10 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Property: TSTMAP ----
CENTER=(45.0, 12.7)
LINE=5
SCALE=10.0
PROJ='cylindrical'
SAMP=10
LONGITUDE=10.5
---- Property: TSTLUT ----
RED=(1, 2, 3, 4)
GREEN=(4, 5, 6, 7)
BLUE=(7, 8, 9, 10)
---- Property: TSTLUT (#2) ----
RED=(11, 12, 13)
GREEN=(14, 15, 16)
BLUE=(17, 18, 19)
---- Task: GEN -- User: lwk -- Sat Nov 13 10:55:59 2010 ----
IVAL=0.0
SINC=1.0
LINC=1.0
BINC=1.0
MODULO=0.0
---- Task: GEN -- User: lwk -- Sat Nov 13 10:55:57 2010 ----
IVAL=0.0
SINC=1.0
LINC=1.0
BINC=1.0
MODULO=0.0
---- Task: INSERT -- User: lwk -- Sat Nov 13 10:55:57 2010 ----
COMMENT=('straight data copy', 'second comment')
---- Task: STRETCH -- User: lwk -- Sat Nov 13 10:55:58 2010 ----
PARMS='AUTO-STRETCH:      1 to      0 and     18 to    255'
---- Task: INSERT -- User: lwk -- Sat Nov 13 10:55:58 2010 ----
SLOPE=5.6
TIME=45
NAME='No name'
MV=(1.2, 3.5, 4.6, 7.8, 9.0)
DSLOPE=5.0
---- Task: LABEL -- User: lwk -- Sat Nov 13 10:55:59 2010 ----
ACTION=('LABEL-ADD', '-- copy option used')
LONG=(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
 
************************************************************
gen t6
Beginning VICAR task gen
GEN Version 6
GEN task completed
label-concat (t5,t6) 'prop
Beginning VICAR task label
LABEL version 15-Nov-2010
label-list t6
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File t6 ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in BYTE format from a SUN-SOLR host
                1 bands
                10 lines per band
                10 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Property: TSTMAP ----
CENTER=(45.0, 12.7)
LINE=5
SCALE=10.0
PROJ='cylindrical'
SAMP=10
LONGITUDE=10.5
---- Property: TSTLUT ----
RED=(1, 2, 3, 4)
GREEN=(4, 5, 6, 7)
BLUE=(7, 8, 9, 10)
---- Property: TSTLUT (#2) ----
RED=(11, 12, 13)
GREEN=(14, 15, 16)
BLUE=(17, 18, 19)
---- Task: GEN -- User: lwk -- Sat Nov 13 10:55:59 2010 ----
IVAL=0.0
SINC=1.0
LINC=1.0
BINC=1.0
MODULO=0.0
 
************************************************************
gen t6
Beginning VICAR task gen
GEN Version 6
GEN task completed
label-concat (t5,t6) task=stretch
Beginning VICAR task label
LABEL version 15-Nov-2010
label-list t6
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File t6 ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in BYTE format from a SUN-SOLR host
                1 bands
                10 lines per band
                10 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: GEN -- User: lwk -- Sat Nov 13 10:55:59 2010 ----
IVAL=0.0
SINC=1.0
LINC=1.0
BINC=1.0
MODULO=0.0
---- Task: STRETCH -- User: lwk -- Sat Nov 13 10:55:58 2010 ----
PARMS='AUTO-STRETCH:      1 to      0 and     18 to    255'
 
************************************************************
gen t6
Beginning VICAR task gen
GEN Version 6
GEN task completed
label-concat (t5,t6) t7 prop=tstlut instance=2
Beginning VICAR task label
LABEL version 15-Nov-2010
label-list t7
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File t7 ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in BYTE format from a SUN-SOLR host
                1 bands
                10 lines per band
                10 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Property: TSTLUT ----
RED=(11, 12, 13)
GREEN=(14, 15, 16)
BLUE=(17, 18, 19)
---- Task: GEN -- User: lwk -- Sat Nov 13 10:55:59 2010 ----
IVAL=0.0
SINC=1.0
LINC=1.0
BINC=1.0
MODULO=0.0
---- Task: LABEL -- User: lwk -- Sat Nov 13 10:55:59 2010 ----
 
************************************************************
write " -DELETE -- Delete different items"
 -DELETE -- Delete different items
label-delete t5 task=insert key=mv element=3 nelement=2
Beginning VICAR task label
LABEL version 15-Nov-2010
Elements 3 through 4 deleted from keyword MV
label-list t5 'hist 'pack
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File t5 ************
---- Task: GEN -- User: lwk -- Sat Nov 13 10:55:57 2010 ----
IVAL=0.0  SINC=1.0  LINC=1.0  BINC=1.0  MODULO=0.0
---- Task: INSERT -- User: lwk -- Sat Nov 13 10:55:57 2010 ----
COMMENT=('straight data copy', 'second comment')
---- Task: STRETCH -- User: lwk -- Sat Nov 13 10:55:58 2010 ----
PARMS='AUTO-STRETCH:      1 to      0 and     18 to    255'
---- Task: INSERT -- User: lwk -- Sat Nov 13 10:55:58 2010 ----
SLOPE=5.6  TIME=45  NAME='No name'  MV=(1.2, 3.5, 9.0)  DSLOPE=5.00000012
---- Task: LABEL -- User: lwk -- Sat Nov 13 10:55:59 2010 ----
ACTION=('LABEL-ADD', '-- copy option used')  LONG=(1, 2, 3, 4, 5, 6, 7, 8, 
9, 10)
 
************************************************************
label-delete t5 task=insert
Beginning VICAR task label
LABEL version 15-Nov-2010
Keyword COMMENT deleted
History subset INSERT deleted
Keyword SLOPE deleted
Keyword TIME deleted
Keyword NAME deleted
Keyword MV deleted
Keyword DSLOPE deleted
History subset INSERT deleted
label-list t5 'hist
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File t5 ************
---- Task: GEN -- User: lwk -- Sat Nov 13 10:55:57 2010 ----
IVAL=0.0
SINC=1.0
LINC=1.0
BINC=1.0
MODULO=0.0
---- Task: STRETCH -- User: lwk -- Sat Nov 13 10:55:58 2010 ----
PARMS='AUTO-STRETCH:      1 to      0 and     18 to    255'
---- Task: LABEL -- User: lwk -- Sat Nov 13 10:55:59 2010 ----
ACTION=('LABEL-ADD', '-- copy option used')
LONG=(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
 
************************************************************
label-delete t5 task=gen key=/ALL
Beginning VICAR task label
LABEL version 15-Nov-2010
Keyword IVAL deleted
Keyword SINC deleted
Keyword LINC deleted
Keyword BINC deleted
Keyword MODULO deleted
label-list t5
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File t5 ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in BYTE format from a SUN-SOLR host
                1 bands
                10 lines per band
                10 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Property: TSTMAP ----
CENTER=(45.0, 12.7)
LINE=5
SCALE=10.0
PROJ='cylindrical'
SAMP=10
LONGITUDE=10.5
---- Property: TSTLUT ----
RED=(1, 2, 3, 4)
GREEN=(4, 5, 6, 7)
BLUE=(7, 8, 9, 10)
---- Property: TSTLUT (#2) ----
RED=(11, 12, 13)
GREEN=(14, 15, 16)
BLUE=(17, 18, 19)
---- Task: GEN -- User: lwk -- Sat Nov 13 10:55:57 2010 ----
---- Task: STRETCH -- User: lwk -- Sat Nov 13 10:55:58 2010 ----
PARMS='AUTO-STRETCH:      1 to      0 and     18 to    255'
---- Task: LABEL -- User: lwk -- Sat Nov 13 10:55:59 2010 ----
ACTION=('LABEL-ADD', '-- copy option used')
LONG=(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
 
************************************************************
label-delete t5 key=(red,green) prop=tstlut element=1 nelement=1 instnces=2
Beginning VICAR task label
LABEL version 15-Nov-2010
Element 1 deleted from keyword RED
Element 1 deleted from keyword GREEN
label-list t5 'prop
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File t5 ************
---- Property: TSTMAP ----
CENTER=(45.0, 12.7)
LINE=5
SCALE=10.0
PROJ='cylindrical'
SAMP=10
LONGITUDE=10.5
---- Property: TSTLUT ----
RED=(1, 2, 3, 4)
GREEN=(4, 5, 6, 7)
BLUE=(7, 8, 9, 10)
---- Property: TSTLUT (#2) ----
RED=(12, 13)
GREEN=(15, 16)
BLUE=(17, 18, 19)
 
************************************************************
label-delete t5 key=(red,green,blue) prop=tstlut element=1 nelement=1
Beginning VICAR task label
LABEL version 15-Nov-2010
Element 1 deleted from keyword RED
Element 1 deleted from keyword GREEN
Element 1 deleted from keyword BLUE
Element 1 deleted from keyword RED
Element 1 deleted from keyword GREEN
Element 1 deleted from keyword BLUE
label-list t5 'prop
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File t5 ************
---- Property: TSTMAP ----
CENTER=(45.0, 12.7)
LINE=5
SCALE=10.0
PROJ='cylindrical'
SAMP=10
LONGITUDE=10.5
---- Property: TSTLUT ----
RED=(2, 3, 4)
GREEN=(5, 6, 7)
BLUE=(8, 9, 10)
---- Property: TSTLUT (#2) ----
RED=13
GREEN=16
BLUE=(18, 19)
 
************************************************************
label-delete t5 t6 key=/all prop=tstmap
Beginning VICAR task label
LABEL version 15-Nov-2010
Keyword CENTER deleted
Keyword LINE deleted
Keyword SCALE deleted
Keyword PROJ deleted
Keyword SAMP deleted
Keyword LONGITUDE deleted
label-list t6 'prop
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File t6 ************
---- Property: TSTMAP ----
---- Property: TSTLUT ----
RED=(2, 3, 4)
GREEN=(5, 6, 7)
BLUE=(8, 9, 10)
---- Property: TSTLUT (#2) ----
RED=13
GREEN=16
BLUE=(18, 19)
 
************************************************************
label-delete t6 'property
Beginning VICAR task label
LABEL version 15-Nov-2010
Property subset TSTMAP deleted
Keyword RED deleted
Keyword GREEN deleted
Keyword BLUE deleted
Property subset TSTLUT deleted
Keyword RED deleted
Keyword GREEN deleted
Keyword BLUE deleted
Property subset TSTLUT deleted
label-list t6
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File t6 ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in BYTE format from a SUN-SOLR host
                1 bands
                10 lines per band
                10 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: GEN -- User: lwk -- Sat Nov 13 10:55:57 2010 ----
---- Task: STRETCH -- User: lwk -- Sat Nov 13 10:55:58 2010 ----
PARMS='AUTO-STRETCH:      1 to      0 and     18 to    255'
---- Task: LABEL -- User: lwk -- Sat Nov 13 10:55:59 2010 ----
ACTION=('LABEL-ADD', '-- copy option used')
LONG=(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
---- Task: LABEL -- User: lwk -- Sat Nov 13 10:56:00 2010 ----
 
************************************************************
write " -REPLACE -- Replace specific label items"
 -REPLACE -- Replace specific label items
label-repla t4 item="nl=5" 'sys
Beginning VICAR task label
LABEL version 15-Nov-2010
Keyword nl replaced
label-list t4
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File t4 ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in BYTE format from a SUN-SOLR host
                1 bands
                5 lines per band
                10 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Property: TSTMAP ----
CENTER=(45.0, 12.7)
LINE=5
SCALE=10.0
PROJ='cylindrical'
SAMP=10
LONGITUDE=10.5
---- Property: TSTLUT ----
RED=(1, 2, 3)
GREEN=(4, 5, 6)
BLUE=(7, 8, 9)
---- Property: TSTLUT (#2) ----
RED=(11, 12, 13)
GREEN=(14, 15, 16)
BLUE=(17, 18, 19)
---- Task: GEN -- User: lwk -- Sat Nov 13 10:55:57 2010 ----
IVAL=0.0
SINC=1.0
LINC=1.0
BINC=1.0
MODULO=0.0
---- Task: INSERT -- User: lwk -- Sat Nov 13 10:55:57 2010 ----
COMMENT=('straight data copy', 'second comment')
---- Task: STRETCH -- User: lwk -- Sat Nov 13 10:55:58 2010 ----
PARMS='AUTO-STRETCH:      1 to      0 and     18 to    255'
---- Task: INSERT -- User: lwk -- Sat Nov 13 10:55:58 2010 ----
SLOPE=5.6
TIME=45
NAME='No name'
MV=(1.2, 3.5, 4.6, 7.8, 9.0)
DSLOPE=5.00000012
 
************************************************************
label-repla t3 t4 "ival=9999" task=gen inst=1
Beginning VICAR task label
LABEL version 15-Nov-2010
Keyword ival replaced
label-list t4
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File t4 ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in BYTE format from a SUN-SOLR host
                1 bands
                10 lines per band
                10 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: GEN -- User: lwk -- Sat Nov 13 10:55:57 2010 ----
SINC=1.0
LINC=1.0
BINC=1.0
MODULO=0.0
IVAL=9999
---- Task: INSERT -- User: lwk -- Sat Nov 13 10:55:57 2010 ----
---- Task: STRETCH -- User: lwk -- Sat Nov 13 10:55:58 2010 ----
PARMS='AUTO-STRETCH:      1 to      0 and     18 to    255'
---- Task: LABEL -- User: lwk -- Sat Nov 13 10:56:00 2010 ----
 
************************************************************
label-repla t5 item="long(5)=(50,60)"
Beginning VICAR task label
LABEL version 15-Nov-2010
Elements 5 through 6 replaced in keyword long
label-repla t5 item="red(2)=30 green(2)=60" prop=tstlut
Beginning VICAR task label
LABEL version 15-Nov-2010
Element 2 replaced in keyword red
Element 2 replaced in keyword green
label-repla t5 item="blue(2)=(90,100)" prop=tstlut instance=2
Beginning VICAR task label
LABEL version 15-Nov-2010
Elements 2 through 3 replaced in keyword blue
label-list t5 'pack
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File t5 ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in BYTE format from a SUN-SOLR host
                1 bands
                10 lines per band
                10 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Property: TSTMAP ----
CENTER=(45.0, 12.7)  LINE=5  SCALE=10.0  PROJ='cylindrical'  SAMP=10
LONGITUDE=10.5
---- Property: TSTLUT ----
RED=(2, 30, 4)  GREEN=(5, 60, 7)  BLUE=(8, 9, 10)
---- Property: TSTLUT (#2) ----
RED=13  GREEN=16  BLUE=(18, 90, 100)
---- Task: GEN -- User: lwk -- Sat Nov 13 10:55:57 2010 ----
---- Task: STRETCH -- User: lwk -- Sat Nov 13 10:55:58 2010 ----
PARMS='AUTO-STRETCH:      1 to      0 and     18 to    255'
---- Task: LABEL -- User: lwk -- Sat Nov 13 10:55:59 2010 ----
ACTION=('LABEL-ADD', '-- copy option used')  LONG=(1, 2, 3, 4, 50, 60, 7, 8, 
9, 10)
 
************************************************************
write " Now make everything single-valued by deleting elements > 1"
 Now make everything single-valued by deleting elements > 1
label-delete t5 t6 key=/ALL element=2 nelement=-1 'hist
Beginning VICAR task label
LABEL version 15-Nov-2010
Element 2 deleted from keyword ACTION
Elements 2 through 10 deleted from keyword LONG
label-delete t6 key=/ALL element=2 nelement=-1 'prop
Beginning VICAR task label
LABEL version 15-Nov-2010
Element 2 deleted from keyword CENTER
Elements 2 through 3 deleted from keyword RED
Elements 2 through 3 deleted from keyword GREEN
Elements 2 through 3 deleted from keyword BLUE
Elements 2 through 3 deleted from keyword BLUE
label-list t6
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File t6 ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in BYTE format from a SUN-SOLR host
                1 bands
                10 lines per band
                10 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Property: TSTMAP ----
CENTER=45.0
LINE=5
SCALE=10.0
PROJ='cylindrical'
SAMP=10
LONGITUDE=10.5
---- Property: TSTLUT ----
RED=2
GREEN=5
BLUE=8
---- Property: TSTLUT (#2) ----
RED=13
GREEN=16
BLUE=18
---- Task: GEN -- User: lwk -- Sat Nov 13 10:55:57 2010 ----
---- Task: STRETCH -- User: lwk -- Sat Nov 13 10:55:58 2010 ----
PARMS='AUTO-STRETCH:      1 to      0 and     18 to    255'
---- Task: LABEL -- User: lwk -- Sat Nov 13 10:55:59 2010 ----
ACTION='LABEL-ADD'
LONG=1
---- Task: LABEL -- User: lwk -- Sat Nov 13 10:56:00 2010 ----
 
************************************************************
copy t4 t7
Beginning VICAR task copy
 COPY VERSION 12-JUL-1993
label-list t4
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File t4 ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in BYTE format from a SUN-SOLR host
                1 bands
                10 lines per band
                10 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: GEN -- User: lwk -- Sat Nov 13 10:55:57 2010 ----
SINC=1.0
LINC=1.0
BINC=1.0
MODULO=0.0
IVAL=9999
---- Task: INSERT -- User: lwk -- Sat Nov 13 10:55:57 2010 ----
---- Task: STRETCH -- User: lwk -- Sat Nov 13 10:55:58 2010 ----
PARMS='AUTO-STRETCH:      1 to      0 and     18 to    255'
---- Task: LABEL -- User: lwk -- Sat Nov 13 10:56:00 2010 ----
 
************************************************************
write " -REMOVE -- Remove the binary header"
 -REMOVE -- Remove the binary header
label-remove t4 t1 'NOBINHEAD
Beginning VICAR task label
LABEL version 15-Nov-2010
write "	attempt to list should be successful"
	attempt to list should be successful
label-list t1
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File t1 ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in BYTE format from a SUN-SOLR host
                1 bands
                10 lines per band
                10 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: GEN -- User: lwk -- Sat Nov 13 10:55:57 2010 ----
SINC=1.0
LINC=1.0
BINC=1.0
MODULO=0.0
IVAL=9999
---- Task: INSERT -- User: lwk -- Sat Nov 13 10:55:57 2010 ----
---- Task: STRETCH -- User: lwk -- Sat Nov 13 10:55:58 2010 ----
PARMS='AUTO-STRETCH:      1 to      0 and     18 to    255'
---- Task: LABEL -- User: lwk -- Sat Nov 13 10:56:00 2010 ----
---- Task: LABEL -- User: lwk -- Sat Nov 13 10:56:01 2010 ----
 
************************************************************
write " -REMOVE -- Remove the binary prefix"
 -REMOVE -- Remove the binary prefix
label-remove t4 t1 'NOBINPREF
Beginning VICAR task label
LABEL version 15-Nov-2010
write "	attempt to list should be successful"
	attempt to list should be successful
label-list t1
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File t1 ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in BYTE format from a SUN-SOLR host
                1 bands
                10 lines per band
                10 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: GEN -- User: lwk -- Sat Nov 13 10:55:57 2010 ----
SINC=1.0
LINC=1.0
BINC=1.0
MODULO=0.0
IVAL=9999
---- Task: INSERT -- User: lwk -- Sat Nov 13 10:55:57 2010 ----
---- Task: STRETCH -- User: lwk -- Sat Nov 13 10:55:58 2010 ----
PARMS='AUTO-STRETCH:      1 to      0 and     18 to    255'
---- Task: LABEL -- User: lwk -- Sat Nov 13 10:56:00 2010 ----
---- Task: LABEL -- User: lwk -- Sat Nov 13 10:56:01 2010 ----
 
************************************************************
write " -REMOVE -- Remove all the header"
 -REMOVE -- Remove all the header
label-remove t4 t1
Beginning VICAR task label
LABEL version 15-Nov-2010
write "	attempt to list should cause an error"
	attempt to list should cause an error
label-list t1
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File t1 ************
[VIC2-GENERR] Exception in XVOPEN, processing file: t1
[VIC2-BADLBL] Bad input label; check file contents
 ** ABEND called **
continue
write " -CREATE -- add a label then list it out"
 -CREATE -- add a label then list it out
label-create t1 t6 nl=10 ns=10 comment="What a slick image"
Beginning VICAR task label
LABEL version 15-Nov-2010
label-list t6
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File t6 ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in BYTE format from a SUN-SOLR host
                1 bands
                10 lines per band
                10 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: LABEL -- User: lwk -- Sat Nov 13 10:56:01 2010 ----
COMMENT='What a slick image'
 
************************************************************
write " -SWITCH -- gen a different size file then switch the labels"
 -SWITCH -- gen a different size file then switch the labels
gen t1 5 5 linc=10
Beginning VICAR task gen
GEN Version 6
GEN task completed
list t1
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sat Nov 13 10:56:01 2010
     Samp     1       3       5
   Line
      1       0   1   2   3   4
      2      10  11  12  13  14
      3      20  21  22  23  24
      4      30  31  32  33  34
      5      40  41  42  43  44
label-switch (t5,t1) t2
Beginning VICAR task label
LABEL version 15-Nov-2010
list t2
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sat Nov 13 10:55:57 2010
 Task:LABEL     User:lwk       Date_Time:Sat Nov 13 10:56:01 2010
     Samp     1       3       5
   Line
      1       0   1   2   3   4
      2      10  11  12  13  14
      3      20  21  22  23  24
      4      30  31  32  33  34
      5      40  41  42  43  44
label-list t2
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File t2 ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in BYTE format from a SUN-SOLR host
                1 bands
                5 lines per band
                5 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Property: TSTMAP ----
CENTER=(45.0, 12.7)
LINE=5
SCALE=10.0
PROJ='cylindrical'
SAMP=10
LONGITUDE=10.5
---- Property: TSTLUT ----
RED=(2, 30, 4)
GREEN=(5, 60, 7)
BLUE=(8, 9, 10)
---- Property: TSTLUT (#2) ----
RED=13
GREEN=16
BLUE=(18, 90, 100)
---- Task: GEN -- User: lwk -- Sat Nov 13 10:55:57 2010 ----
---- Task: STRETCH -- User: lwk -- Sat Nov 13 10:55:58 2010 ----
PARMS='AUTO-STRETCH:      1 to      0 and     18 to    255'
---- Task: LABEL -- User: lwk -- Sat Nov 13 10:55:59 2010 ----
ACTION=('LABEL-ADD', '-- copy option used')
LONG=(1, 2, 3, 4, 50, 60, 7, 8, 9, 10)
---- Task: LABEL -- User: lwk -- Sat Nov 13 10:56:01 2010 ----
 
************************************************************
write ""

write "Generate a 3-D file with some labels first"
Generate a 3-D file with some labels first
gen t1 10 10 3 'BIL
Beginning VICAR task gen
GEN Version 6
GEN task completed
copy t1 t7
Beginning VICAR task copy
 COPY VERSION 12-JUL-1993
label-list t1
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File t1 ************
                3 dimensional IMAGE file
                File organization is BIL
                Pixels are in BYTE format from a SUN-SOLR host
                3 bands
                10 lines per band
                10 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: GEN -- User: lwk -- Sat Nov 13 10:56:01 2010 ----
IVAL=0.0
SINC=1.0
LINC=1.0
BINC=1.0
MODULO=0.0
 
************************************************************
write "	attempt to list should be successful"
	attempt to list should be successful
label-list t2
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File t2 ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in BYTE format from a SUN-SOLR host
                1 bands
                5 lines per band
                5 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Property: TSTMAP ----
CENTER=(45.0, 12.7)
LINE=5
SCALE=10.0
PROJ='cylindrical'
SAMP=10
LONGITUDE=10.5
---- Property: TSTLUT ----
RED=(2, 30, 4)
GREEN=(5, 60, 7)
BLUE=(8, 9, 10)
---- Property: TSTLUT (#2) ----
RED=13
GREEN=16
BLUE=(18, 90, 100)
---- Task: GEN -- User: lwk -- Sat Nov 13 10:55:57 2010 ----
---- Task: STRETCH -- User: lwk -- Sat Nov 13 10:55:58 2010 ----
PARMS='AUTO-STRETCH:      1 to      0 and     18 to    255'
---- Task: LABEL -- User: lwk -- Sat Nov 13 10:55:59 2010 ----
ACTION=('LABEL-ADD', '-- copy option used')
LONG=(1, 2, 3, 4, 50, 60, 7, 8, 9, 10)
---- Task: LABEL -- User: lwk -- Sat Nov 13 10:56:01 2010 ----
 
************************************************************
write " -REMOVE -- Remove the binary prefix"
 -REMOVE -- Remove the binary prefix
label-remove t1 t2 'NOBINPREF
Beginning VICAR task label
LABEL version 15-Nov-2010
write "	attempt to list should be successful"
	attempt to list should be successful
label-list t2
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File t2 ************
                3 dimensional IMAGE file
                File organization is BIL
                Pixels are in BYTE format from a SUN-SOLR host
                3 bands
                10 lines per band
                10 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: GEN -- User: lwk -- Sat Nov 13 10:56:01 2010 ----
IVAL=0.0
SINC=1.0
LINC=1.0
BINC=1.0
MODULO=0.0
---- Task: LABEL -- User: lwk -- Sat Nov 13 10:56:02 2010 ----
 
************************************************************
write " -REMOVE -- Remove the binary header and prefix"
 -REMOVE -- Remove the binary header and prefix
label-remove t1 t2
Beginning VICAR task label
LABEL version 15-Nov-2010
write "	attempt to list should cause an error"
	attempt to list should cause an error
label-list t2
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File t2 ************
[VIC2-GENERR] Exception in XVOPEN, processing file: t2
[VIC2-BADLBL] Bad input label; check file contents
 ** ABEND called **
continue
write " -CREATE -- add a label"
 -CREATE -- add a label
label-create t2 t3 ns=10 nl=10 nb=3 org=BIL
Beginning VICAR task label
LABEL version 15-Nov-2010
label-list t3
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File t3 ************
                3 dimensional IMAGE file
                File organization is BIL
                Pixels are in BYTE format from a SUN-SOLR host
                3 bands
                10 lines per band
                10 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: LABEL -- User: lwk -- Sat Nov 13 10:56:02 2010 ----
 
************************************************************
write " -CREATE -- different host formats"
 -CREATE -- different host formats
label-create t2 t3 ns=10 nl=10 nb=3 org=BIL host=VAX-VMS
Beginning VICAR task label
LABEL version 15-Nov-2010
label-list t3 'sys
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File t3 ************
                3 dimensional IMAGE file
                File organization is BIL
                Pixels are in BYTE format from a VAX-VMS host
                3 bands
                10 lines per band
                10 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
 
************************************************************
label-list t3 'dump
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File t3 ************
LBLSIZE=370
FORMAT='BYTE'
TYPE='IMAGE'
BUFSIZ=24576
DIM=3
EOL=0
RECSIZE=10
ORG='BIL'
NL=10
NS=10
NB=3
N1=10
N2=3
N3=10
N4=0
NBB=0
NLB=0
HOST='VAX-VMS'
INTFMT='LOW'
REALFMT='VAX'
BHOST='VAX-VMS'
BINTFMT='LOW'
BREALFMT='VAX'
BLTYPE=''
COMPRESS='NONE'
EOCI1=0
EOCI2=0
TASK='LABEL'
USER='lwk'
DAT_TIM='Sat Nov 13 10:56:02 2010'
 
************************************************************
label-create t2 t3 ns=10 nl=10 nb=3 org=BIL host=SUN-4
Beginning VICAR task label
LABEL version 15-Nov-2010
label-list t3 'sys
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File t3 ************
                3 dimensional IMAGE file
                File organization is BIL
                Pixels are in BYTE format from a SUN-4 host
                3 bands
                10 lines per band
                10 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
 
************************************************************
label-list t3 'dump
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File t3 ************
LBLSIZE=370
FORMAT='BYTE'
TYPE='IMAGE'
BUFSIZ=24576
DIM=3
EOL=0
RECSIZE=10
ORG='BIL'
NL=10
NS=10
NB=3
N1=10
N2=3
N3=10
N4=0
NBB=0
NLB=0
HOST='SUN-4'
INTFMT='HIGH'
REALFMT='IEEE'
BHOST='VAX-VMS'
BINTFMT='LOW'
BREALFMT='VAX'
BLTYPE=''
COMPRESS='NONE'
EOCI1=0
EOCI2=0
TASK='LABEL'
USER='lwk'
DAT_TIM='Sat Nov 13 10:56:02 2010'
 
************************************************************
write " test handling long records. "
 test handling long records. 
gen t1 10 200000
Beginning VICAR task gen
GEN Version 6
GEN task completed
label-add t1 t4 items="slope=5.6"
Beginning VICAR task label
LABEL version 15-Nov-2010
Keyword slope added
label-list t4
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File t4 ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in BYTE format from a SUN-SOLR host
                1 bands
                10 lines per band
                200000 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: GEN -- User: lwk -- Sat Nov 13 10:56:02 2010 ----
IVAL=0.0
SINC=1.0
LINC=1.0
BINC=1.0
MODULO=0.0
---- Task: LABEL -- User: lwk -- Sat Nov 13 10:56:03 2010 ----
SLOPE=5.6
 
************************************************************
write "multi-input option and abend with -LIST"
multi-input option and abend with -LIST
label-list (t1 t2 t3 t4)
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File t1 ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in BYTE format from a SUN-SOLR host
                1 bands
                10 lines per band
                200000 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: GEN -- User: lwk -- Sat Nov 13 10:56:02 2010 ----
IVAL=0.0
SINC=1.0
LINC=1.0
BINC=1.0
MODULO=0.0
 
************************************************************
************************************************************
 
        ************  File t2 ************
[VIC2-GENERR] Exception in XVOPEN, processing file: t2
[VIC2-BADLBL] Bad input label; check file contents
************************************************************
 
        ************  File t3 ************
                3 dimensional IMAGE file
                File organization is BIL
                Pixels are in BYTE format from a SUN-4 host
                3 bands
                10 lines per band
                10 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: LABEL -- User: lwk -- Sat Nov 13 10:56:02 2010 ----
 
************************************************************
************************************************************
 
        ************  File t4 ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in BYTE format from a SUN-SOLR host
                1 bands
                10 lines per band
                200000 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: GEN -- User: lwk -- Sat Nov 13 10:56:02 2010 ----
IVAL=0.0
SINC=1.0
LINC=1.0
BINC=1.0
MODULO=0.0
---- Task: LABEL -- User: lwk -- Sat Nov 13 10:56:03 2010 ----
SLOPE=5.6
 
************************************************************
ush rm xxxx
[TAE-ABNSHELL] Abnormal shell termination.;
 proc 'tstlabel', line 159
continue
write "this should generate an error msg but no abend:"
this should generate an error msg but no abend:
label-list (t1 xxxx t3 t4)
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File t1 ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in BYTE format from a SUN-SOLR host
                1 bands
                10 lines per band
                200000 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: GEN -- User: lwk -- Sat Nov 13 10:56:02 2010 ----
IVAL=0.0
SINC=1.0
LINC=1.0
BINC=1.0
MODULO=0.0
 
************************************************************
************************************************************
 
        ************  File xxxx ************
[VIC2-GENERR] Exception in XVOPEN, processing file: xxxx
[VIC2-HOSTMSG]  No such file or directory
************************************************************
 
        ************  File t3 ************
                3 dimensional IMAGE file
                File organization is BIL
                Pixels are in BYTE format from a SUN-4 host
                3 bands
                10 lines per band
                10 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: LABEL -- User: lwk -- Sat Nov 13 10:56:02 2010 ----
 
************************************************************
************************************************************
 
        ************  File t4 ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in BYTE format from a SUN-SOLR host
                1 bands
                10 lines per band
                200000 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: GEN -- User: lwk -- Sat Nov 13 10:56:02 2010 ----
IVAL=0.0
SINC=1.0
LINC=1.0
BINC=1.0
MODULO=0.0
---- Task: LABEL -- User: lwk -- Sat Nov 13 10:56:03 2010 ----
SLOPE=5.6
 
************************************************************
write "this should generate an abend:"
this should generate an abend:
label-list xxxx
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File xxxx ************
[VIC2-GENERR] Exception in XVOPEN, processing file: xxxx
[VIC2-HOSTMSG]  No such file or directory
 ** ABEND called **
continue
if ($syschar(1) = "UNIX")
  ush rm t1 t2 t3 t4 t5 t6 t7
else
end-if
label-list "/project/test_work/testdata/gll/s0061000300.1"
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File /project/test_work/testdata/gll/s0061000300.1 ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in BYTE format from a VAX-VMS host
                1 bands
                800 lines per band
                800 samples per line
                2 lines of binary header
                200 bytes of binary prefix per line
---- Task: TASK -- User: RTO320 -- Wed Jan 16 10:37:39 1991 ----
MISSION='GALILEO'
SENSOR='SSI'
PICNO='?'
PA='E1LNLTIN_-05B___LCOP'
RIM=610003
MOD91=0
MOD10=1
MOD8=0
TCA='CLOSEST TIME'
TARGET='MOON'
SCETYEAR=1990
SCETDAY=342
SCETHOUR=16
SCETMIN=59
SCETSEC=48
SCETMSEC=133
FILTER=0
EXP=4.167
GAIN=3
RATE=3
TLMFMT='HCJ'
BOOM='P'
FIBE='1000'
BARC='RC'
ENTROPY=0.0
TBPPXL=0.0
TPPLNE=0.0
INA=-999.0
EMA=-999.0
PHA=-999.0
HRA=-999.0
TWIST=0.0
CONE=0.0
RA=0.0
DEC=0.0
SUNAZ=0.0
NORAZ=0.0
SCAZ=0.0
SMRAZ=0.0
SMEAR=-999.0
HSCL=0.0
VSCL=0.0
LAT=0.0
LON=0.0
RAD=0.0
PLRANGE=0.0
SLRANGE=0.0
SOLRANGE=7.779091e+08
 
************************************************************
label-remove "/project/test_work/testdata/gll/s0061000300.1" test.nolabel 'BINARY
Beginning VICAR task label
LABEL version 15-Nov-2010
write " attempt to list should cause error "
 attempt to list should cause error 
label-list test.nolabel
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File test.nolabel ************
[VIC2-GENERR] Exception in XVOPEN, processing file: test.nolabel
[VIC2-BADLBL] Bad input label; check file contents
 ** ABEND called **
continue
label-create test.nolabel test.vpds NL=800 NS=800 NLB=2 NBB=200 'BINARY
Beginning VICAR task label
LABEL version 15-Nov-2010
label-list test.vpds
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File test.vpds ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in BYTE format from a SUN-SOLR host
                1 bands
                800 lines per band
                800 samples per line
                2 lines of binary header
                200 bytes of binary prefix per line
---- Task: LABEL -- User: lwk -- Sat Nov 13 10:56:14 2010 ----
 
************************************************************
difpic ("/project/test_work/testdata/gll/s0061000300.1",test.vpds)
Beginning VICAR task difpic
DIFPIC version 05jul10
 NUMBER OF DIFFERENCES =   0
label-create test.nolabel test.binary nl=800 ns=800 nlb=2 nbb=200 'bin  +
	bhost=vax-vms bltype="tstpds"
Beginning VICAR task label
LABEL version 15-Nov-2010
label-list test.binary 'sys
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File test.binary ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in BYTE format from a SUN-SOLR host
                1 bands
                800 lines per band
                800 samples per line
                2 lines of binary header of type TSTPDS
                200 bytes of binary prefix per line
 
************************************************************
label-list test.binary 'dump
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File test.binary ************
LBLSIZE=1000
FORMAT='BYTE'
TYPE='IMAGE'
BUFSIZ=24576
DIM=3
EOL=0
RECSIZE=1000
ORG='BSQ'
NL=800
NS=800
NB=1
N1=800
N2=800
N3=1
N4=0
NBB=200
NLB=2
HOST='SUN-SOLR'
INTFMT='HIGH'
REALFMT='IEEE'
BHOST='VAX-VMS'
BINTFMT='LOW'
BREALFMT='VAX'
BLTYPE='TSTPDS'
COMPRESS='NONE'
EOCI1=0
EOCI2=0
TASK='LABEL'
USER='lwk'
DAT_TIM='Sat Nov 13 10:56:15 2010'
 
************************************************************
label-create test.nolabel test.binary nl=800 ns=800 nlb=2 nbb=200 'bin  +
	bhost="vax-vms" brealfmt="vax" bltype="tstpds"
Beginning VICAR task label
LABEL version 15-Nov-2010
label-list test.binary
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File test.binary ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in BYTE format from a SUN-SOLR host
                1 bands
                800 lines per band
                800 samples per line
                2 lines of binary header of type TSTPDS
                200 bytes of binary prefix per line
---- Task: LABEL -- User: lwk -- Sat Nov 13 10:56:15 2010 ----
 
************************************************************
label-list test.binary 'dump
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File test.binary ************
LBLSIZE=1000
FORMAT='BYTE'
TYPE='IMAGE'
BUFSIZ=24576
DIM=3
EOL=0
RECSIZE=1000
ORG='BSQ'
NL=800
NS=800
NB=1
N1=800
N2=800
N3=1
N4=0
NBB=200
NLB=2
HOST='SUN-SOLR'
INTFMT='HIGH'
REALFMT='IEEE'
BHOST='VAX-VMS'
BINTFMT='LOW'
BREALFMT='VAX'
BLTYPE='TSTPDS'
COMPRESS='NONE'
EOCI1=0
EOCI2=0
TASK='LABEL'
USER='lwk'
DAT_TIM='Sat Nov 13 10:56:15 2010'
 
************************************************************
write "The following commands test the new binary features of LABEL"
The following commands test the new binary features of LABEL
label-add test.vpds test.added items="LIST=(1,2)"
Beginning VICAR task label
LABEL version 15-Nov-2010
Keyword LIST added
label-add test.added items="test=binary" prop=tstprop
Beginning VICAR task label
LABEL version 15-Nov-2010
Keyword test added
label-list test.added
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File test.added ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in BYTE format from a SUN-SOLR host
                1 bands
                800 lines per band
                800 samples per line
                2 lines of binary header
                200 bytes of binary prefix per line
---- Property: TSTPROP ----
TEST='binary'
---- Task: LABEL -- User: lwk -- Sat Nov 13 10:56:14 2010 ----
---- Task: LABEL -- User: lwk -- Sat Nov 13 10:56:15 2010 ----
LIST=(1, 2)
 
************************************************************
difpic (test.vpds,test.added)
Beginning VICAR task difpic
DIFPIC version 05jul10
 NUMBER OF DIFFERENCES =   0
label-del test.added test.deleted keys="LIST" tasks="LABEL"
Beginning VICAR task label
LABEL version 15-Nov-2010
Keyword LIST deleted
label-del test.deleted keys="TEST" prop=tstprop
Beginning VICAR task label
LABEL version 15-Nov-2010
Keyword TEST deleted
label-list test.deleted
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File test.deleted ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in BYTE format from a SUN-SOLR host
                1 bands
                800 lines per band
                800 samples per line
                2 lines of binary header
                200 bytes of binary prefix per line
---- Property: TSTPROP ----
---- Task: LABEL -- User: lwk -- Sat Nov 13 10:56:14 2010 ----
---- Task: LABEL -- User: lwk -- Sat Nov 13 10:56:15 2010 ----
---- Task: LABEL -- User: lwk -- Sat Nov 13 10:56:16 2010 ----
 
************************************************************
difpic (test.vpds,test.deleted)
Beginning VICAR task difpic
DIFPIC version 05jul10
 NUMBER OF DIFFERENCES =   0
label-switch (test.added,test.deleted) test.switch
Beginning VICAR task label
LABEL version 15-Nov-2010
label-list test.switch
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File test.switch ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in BYTE format from a SUN-SOLR host
                1 bands
                800 lines per band
                800 samples per line
                2 lines of binary header
                200 bytes of binary prefix per line
---- Property: TSTPROP ----
TEST='binary'
---- Task: LABEL -- User: lwk -- Sat Nov 13 10:56:14 2010 ----
---- Task: LABEL -- User: lwk -- Sat Nov 13 10:56:15 2010 ----
LIST=(1, 2)
---- Task: LABEL -- User: lwk -- Sat Nov 13 10:56:16 2010 ----
 
************************************************************
difpic (test.vpds,test.switch)
Beginning VICAR task difpic
DIFPIC version 05jul10
 NUMBER OF DIFFERENCES =   0
if ($syschar(1) = "UNIX")
  ush rm test.nolabel test.vpds test.binary test.added test.deleted test.switch
else
end-if
end-proc
disable-log
$!-----------------------------------------------------------------------------
$ create tstlabel.log_linux
tstlabel
let $autousage="none"
local testfile type=string init="/project/test_work/testdata/gll/s0061000300.1"
if ($syschar(1) = "VAX_VMS")
end-if
write "		Test for program LABEL"
		Test for program LABEL
write ""

write "Make sure file /project/test_work/testdata/gll/s0061000300.1 is available"
Make sure file /project/test_work/testdata/gll/s0061000300.1 is available
if ($syschar(1) = "VMS")
end-if
write ""

write "Generate a file with some labels first"
Generate a file with some labels first
gen t1 10 10
Beginning VICAR task gen
GEN Version 6
GEN task completed
insert t1 t2
Beginning VICAR task insert
INSERT version 02-MAY-94
stretch t2 t3
Beginning VICAR task stretch
STRETCH version Oct 17 2002
DN values      0 and    255 excluded

Histogram after exclusion: Mean=     9.0909 Sigma=     3.9800
*** AUTO-STRETCH OPTION ***
Percent saturation at low end=  1.00 at high end=  1.00
AUTO-STRETCH:      1 to      0 and     18 to    255
insert t3 t4
Beginning VICAR task insert
INSERT version 02-MAY-94
write "Now list out the labels"
Now list out the labels
label-list t4
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File t4 ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in BYTE format from a X86-LINUX host
                1 bands
                10 lines per band
                10 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: GEN -- User: lwk -- Sat Nov 13 15:49:49 2010 ----
IVAL=0.0
SINC=1.0
LINC=1.0
BINC=1.0
MODULO=0.0
---- Task: INSERT -- User: lwk -- Sat Nov 13 15:49:49 2010 ----
---- Task: STRETCH -- User: lwk -- Sat Nov 13 15:49:49 2010 ----
PARMS='AUTO-STRETCH:      1 to      0 and     18 to    255'
PARMS2='task stretch'
---- Task: INSERT -- User: lwk -- Sat Nov 13 15:49:49 2010 ----
 
************************************************************
write "	-ADD -- add some label items to the existing file (nocopy)"
	-ADD -- add some label items to the existing file (nocopy)
label-add t4 items="slope=5.6, time=45 name='No name' mv=(1.2,3.5, 4.6,7.8,9)"
Beginning VICAR task label
LABEL version 15-Nov-2010
Keyword slope added
Keyword time added
Keyword name added
Keyword mv added
label-add t4 items="dslope=5.00000012"
Beginning VICAR task label
LABEL version 15-Nov-2010
Keyword dslope added
label-list t4 'hist
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File t4 ************
---- Task: GEN -- User: lwk -- Sat Nov 13 15:49:49 2010 ----
IVAL=0.0
SINC=1.0
LINC=1.0
BINC=1.0
MODULO=0.0
---- Task: INSERT -- User: lwk -- Sat Nov 13 15:49:49 2010 ----
---- Task: STRETCH -- User: lwk -- Sat Nov 13 15:49:49 2010 ----
PARMS='AUTO-STRETCH:      1 to      0 and     18 to    255'
PARMS2='task stretch'
---- Task: INSERT -- User: lwk -- Sat Nov 13 15:49:49 2010 ----
SLOPE=5.6
TIME=45
NAME='No name'
MV=(1.2, 3.5, 4.6, 7.8, 9.0)
DSLOPE=5.00000012
 
************************************************************
label-add t4 items="comment='straight data copy' comment(2)='second comment'" task=insert inst=1
Beginning VICAR task label
LABEL version 15-Nov-2010
Keyword comment added
Element 2 added to keyword comment
label-list t4
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File t4 ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in BYTE format from a X86-LINUX host
                1 bands
                10 lines per band
                10 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: GEN -- User: lwk -- Sat Nov 13 15:49:49 2010 ----
IVAL=0.0
SINC=1.0
LINC=1.0
BINC=1.0
MODULO=0.0
---- Task: INSERT -- User: lwk -- Sat Nov 13 15:49:49 2010 ----
COMMENT=('straight data copy', 'second comment')
---- Task: STRETCH -- User: lwk -- Sat Nov 13 15:49:49 2010 ----
PARMS='AUTO-STRETCH:      1 to      0 and     18 to    255'
PARMS2='task stretch'
---- Task: INSERT -- User: lwk -- Sat Nov 13 15:49:49 2010 ----
SLOPE=5.6
TIME=45
NAME='No name'
MV=(1.2, 3.5, 4.6, 7.8, 9.0)
DSLOPE=5.00000012
 
************************************************************
label-add t4 items="proj=mercator center=(45,12.7) line=5 samp=5" property=tstmap
Beginning VICAR task label
LABEL version 15-Nov-2010
Keyword proj added
Keyword center added
Keyword line added
Keyword samp added
label-add t4 items="scale=10.0" prop=tstmap
Beginning VICAR task label
LABEL version 15-Nov-2010
Keyword scale added
label-list t4 'prop
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File t4 ************
---- Property: TSTMAP ----
PROJ='mercator'
CENTER=(45.0, 12.7)
LINE=5
SAMP=5
SCALE=10.0
 
************************************************************
label-add t4 items="red=(1,2,3) green=(4,5,6) blue=(7,8,9)" prop=tstlut
Beginning VICAR task label
LABEL version 15-Nov-2010
Keyword red added
Keyword green added
Keyword blue added
label-list t4
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File t4 ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in BYTE format from a X86-LINUX host
                1 bands
                10 lines per band
                10 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Property: TSTMAP ----
PROJ='mercator'
CENTER=(45.0, 12.7)
LINE=5
SAMP=5
SCALE=10.0
---- Property: TSTLUT ----
RED=(1, 2, 3)
GREEN=(4, 5, 6)
BLUE=(7, 8, 9)
---- Task: GEN -- User: lwk -- Sat Nov 13 15:49:49 2010 ----
IVAL=0.0
SINC=1.0
LINC=1.0
BINC=1.0
MODULO=0.0
---- Task: INSERT -- User: lwk -- Sat Nov 13 15:49:49 2010 ----
COMMENT=('straight data copy', 'second comment')
---- Task: STRETCH -- User: lwk -- Sat Nov 13 15:49:49 2010 ----
PARMS='AUTO-STRETCH:      1 to      0 and     18 to    255'
PARMS2='task stretch'
---- Task: INSERT -- User: lwk -- Sat Nov 13 15:49:49 2010 ----
SLOPE=5.6
TIME=45
NAME='No name'
MV=(1.2, 3.5, 4.6, 7.8, 9.0)
DSLOPE=5.00000012
 
************************************************************
label-add t4 items="red=(11,12,13) green=(14,15,16) blue=(17,18,19)" prop=tstlut instance=2
Beginning VICAR task label
LABEL version 15-Nov-2010
Keyword red added
Keyword green added
Keyword blue added
label-list t4
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File t4 ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in BYTE format from a X86-LINUX host
                1 bands
                10 lines per band
                10 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Property: TSTMAP ----
PROJ='mercator'
CENTER=(45.0, 12.7)
LINE=5
SAMP=5
SCALE=10.0
---- Property: TSTLUT ----
RED=(1, 2, 3)
GREEN=(4, 5, 6)
BLUE=(7, 8, 9)
---- Property: TSTLUT (#2) ----
RED=(11, 12, 13)
GREEN=(14, 15, 16)
BLUE=(17, 18, 19)
---- Task: GEN -- User: lwk -- Sat Nov 13 15:49:49 2010 ----
IVAL=0.0
SINC=1.0
LINC=1.0
BINC=1.0
MODULO=0.0
---- Task: INSERT -- User: lwk -- Sat Nov 13 15:49:49 2010 ----
COMMENT=('straight data copy', 'second comment')
---- Task: STRETCH -- User: lwk -- Sat Nov 13 15:49:49 2010 ----
PARMS='AUTO-STRETCH:      1 to      0 and     18 to    255'
PARMS2='task stretch'
---- Task: INSERT -- User: lwk -- Sat Nov 13 15:49:49 2010 ----
SLOPE=5.6
TIME=45
NAME='No name'
MV=(1.2, 3.5, 4.6, 7.8, 9.0)
DSLOPE=5.00000012
 
************************************************************
write " Update existing label"
 Update existing label
label-add t4 items="proj=cylindrical samp=10 longitude=10.5" property=tstmap 'prop 'update
Beginning VICAR task label
LABEL version 15-Nov-2010
Keyword proj replaced
Keyword samp replaced
Keyword longitude replaced
label-list t4
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File t4 ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in BYTE format from a X86-LINUX host
                1 bands
                10 lines per band
                10 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Property: TSTMAP ----
CENTER=(45.0, 12.7)
LINE=5
SCALE=10.0
PROJ='cylindrical'
SAMP=10
LONGITUDE=10.5
---- Property: TSTLUT ----
RED=(1, 2, 3)
GREEN=(4, 5, 6)
BLUE=(7, 8, 9)
---- Property: TSTLUT (#2) ----
RED=(11, 12, 13)
GREEN=(14, 15, 16)
BLUE=(17, 18, 19)
---- Task: GEN -- User: lwk -- Sat Nov 13 15:49:49 2010 ----
IVAL=0.0
SINC=1.0
LINC=1.0
BINC=1.0
MODULO=0.0
---- Task: INSERT -- User: lwk -- Sat Nov 13 15:49:49 2010 ----
COMMENT=('straight data copy', 'second comment')
---- Task: STRETCH -- User: lwk -- Sat Nov 13 15:49:49 2010 ----
PARMS='AUTO-STRETCH:      1 to      0 and     18 to    255'
PARMS2='task stretch'
---- Task: INSERT -- User: lwk -- Sat Nov 13 15:49:49 2010 ----
SLOPE=5.6
TIME=45
NAME='No name'
MV=(1.2, 3.5, 4.6, 7.8, 9.0)
DSLOPE=5.00000012
 
************************************************************
write " Now add items using data copy"
 Now add items using data copy
label-add t4 t5 items="action=('LABEL-ADD', '-- copy option used') long=(1,4,5,6,7,8,9,10)"
Beginning VICAR task label
LABEL version 15-Nov-2010
Keyword action added
Keyword long added
label-list t5 'dump
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File t5 ************
LBLSIZE=1150
FORMAT='BYTE'
TYPE='IMAGE'
BUFSIZ=20480
DIM=3
EOL=1
RECSIZE=10
ORG='BSQ'
NL=10
NS=10
NB=1
N1=10
N2=10
N3=1
N4=0
NBB=0
NLB=0
HOST='X86-LINUX'
INTFMT='LOW'
REALFMT='RIEEE'
BHOST='VAX-VMS'
BINTFMT='LOW'
BREALFMT='VAX'
BLTYPE=''
COMPRESS='NONE'
EOCI1=0
EOCI2=0
PROPERTY='TSTMAP'
CENTER=(45.0, 12.7)
LINE=5
SCALE=10.0
PROJ='cylindrical'
SAMP=10
LONGITUDE=10.5
PROPERTY='TSTLUT'
RED=(1, 2, 3)
GREEN=(4, 5, 6)
BLUE=(7, 8, 9)
PROPERTY='TSTLUT'
RED=(11, 12, 13)
GREEN=(14, 15, 16)
BLUE=(17, 18, 19)
TASK='GEN'
USER='lwk'
DAT_TIM='Sat Nov 13 15:49:49 2010'
IVAL=0.0
SINC=1.0
LINC=1.0
BINC=1.0
MODULO=0.0
TASK='INSERT'
USER='lwk'
DAT_TIM='Sat Nov 13 15:49:49 2010'
COMMENT=('straight data copy', 'second comment')
TASK='STRETCH'
USER='lwk'
DAT_TIM='Sat Nov 13 15:49:49 2010'
PARMS='AUTO-STRETCH:      1 to      0 and     18 to    255'
PARMS2='task stretch'
TASK='INSERT'
USER='lwk'
DAT_TIM='Sat Nov 13 15:49:49 2010'
SLOPE=5.6
TIME=45
NAME='No name'
MV=(1.2, 3.5, 4.6, 7.8, 9.0)
DSLOPE=5.00000012
TASK='LABEL'
USER='lwk'
DAT_TIM='Sat Nov 13 15:49:49 2010'
ACTION=('LABEL-ADD', '-- copy option used')
LONG=(1, 4, 5, 6, 7, 8, 9, 10)
 
************************************************************
label-add t5 items="long(2)=(2,3)"
Beginning VICAR task label
LABEL version 15-Nov-2010
Elements 2 through 3 added to keyword long
label-add t5 items="red(4)=4 green(4)=7 blue(4)=10" prop=tstlut
Beginning VICAR task label
LABEL version 15-Nov-2010
Element 4 added to keyword red
Element 4 added to keyword green
Element 4 added to keyword blue
label-list t5 'dump 'pack
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File t5 ************
LBLSIZE=1150  FORMAT='BYTE'  TYPE='IMAGE'  BUFSIZ=20480  DIM=3  EOL=1
RECSIZE=10  ORG='BSQ'  NL=10  NS=10  NB=1  N1=10  N2=10  N3=1  N4=0  NBB=0
NLB=0  HOST='X86-LINUX'  INTFMT='LOW'  REALFMT='RIEEE'  BHOST='VAX-VMS'
BINTFMT='LOW'  BREALFMT='VAX'  BLTYPE=''  COMPRESS='NONE'  EOCI1=0
EOCI2=0
PROPERTY='TSTMAP'
CENTER=(45.0, 12.7)  LINE=5  SCALE=10.0  PROJ='cylindrical'  SAMP=10
LONGITUDE=10.5  PROPERTY='TSTLUT'  RED=(1, 2, 3, 4)  GREEN=(4, 5, 6, 7)
BLUE=(7, 8, 9, 10)  PROPERTY='TSTLUT'  RED=(11, 12, 13)  GREEN=(14, 15, 16)
BLUE=(17, 18, 19)
TASK='GEN'
USER='lwk'  DAT_TIM='Sat Nov 13 15:49:49 2010'  IVAL=0.0  SINC=1.0
LINC=1.0  BINC=1.0  MODULO=0.0  TASK='INSERT'  USER='lwk'
DAT_TIM='Sat Nov 13 15:49:49 2010'  COMMENT=('straight data copy', 
'second comment')  TASK='STRETCH'  USER='lwk'
DAT_TIM='Sat Nov 13 15:49:49 2010'
PARMS='AUTO-STRETCH:      1 to      0 and     18 to    255'
PARMS2='task stretch'  TASK='INSERT'  USER='lwk'
DAT_TIM='Sat Nov 13 15:49:49 2010'  SLOPE=5.6  TIME=45  NAME='No name'
MV=(1.2, 3.5, 4.6, 7.8, 9.0)  DSLOPE=5.00000012  TASK='LABEL'  USER='lwk'
DAT_TIM='Sat Nov 13 15:49:49 2010'  ACTION=('LABEL-ADD', 
'-- copy option used')  LONG=(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
 
************************************************************
write " -CONCAT -- Add label sets to other images"
 -CONCAT -- Add label sets to other images
gen t6
Beginning VICAR task gen
GEN Version 6
GEN task completed
label-concat (t5,t6)
Beginning VICAR task label
LABEL version 15-Nov-2010
label-list t6
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File t6 ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in BYTE format from a X86-LINUX host
                1 bands
                10 lines per band
                10 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Property: TSTMAP ----
CENTER=(45.0, 12.7)
LINE=5
SCALE=10.0
PROJ='cylindrical'
SAMP=10
LONGITUDE=10.5
---- Property: TSTLUT ----
RED=(1, 2, 3, 4)
GREEN=(4, 5, 6, 7)
BLUE=(7, 8, 9, 10)
---- Property: TSTLUT (#2) ----
RED=(11, 12, 13)
GREEN=(14, 15, 16)
BLUE=(17, 18, 19)
---- Task: GEN -- User: lwk -- Sat Nov 13 15:49:49 2010 ----
IVAL=0.0
SINC=1.0
LINC=1.0
BINC=1.0
MODULO=0.0
---- Task: GEN -- User: lwk -- Sat Nov 13 15:49:49 2010 ----
IVAL=0.0
SINC=1.0
LINC=1.0
BINC=1.0
MODULO=0.0
---- Task: INSERT -- User: lwk -- Sat Nov 13 15:49:49 2010 ----
COMMENT=('straight data copy', 'second comment')
---- Task: STRETCH -- User: lwk -- Sat Nov 13 15:49:49 2010 ----
PARMS='AUTO-STRETCH:      1 to      0 and     18 to    255'
PARMS2='task stretch'
---- Task: INSERT -- User: lwk -- Sat Nov 13 15:49:49 2010 ----
SLOPE=5.6
TIME=45
NAME='No name'
MV=(1.2, 3.5, 4.6, 7.8, 9.0)
DSLOPE=5.0
---- Task: LABEL -- User: lwk -- Sat Nov 13 15:49:49 2010 ----
ACTION=('LABEL-ADD', '-- copy option used')
LONG=(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
 
************************************************************
gen t6
Beginning VICAR task gen
GEN Version 6
GEN task completed
label-concat (t5,t6) 'prop
Beginning VICAR task label
LABEL version 15-Nov-2010
label-list t6
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File t6 ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in BYTE format from a X86-LINUX host
                1 bands
                10 lines per band
                10 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Property: TSTMAP ----
CENTER=(45.0, 12.7)
LINE=5
SCALE=10.0
PROJ='cylindrical'
SAMP=10
LONGITUDE=10.5
---- Property: TSTLUT ----
RED=(1, 2, 3, 4)
GREEN=(4, 5, 6, 7)
BLUE=(7, 8, 9, 10)
---- Property: TSTLUT (#2) ----
RED=(11, 12, 13)
GREEN=(14, 15, 16)
BLUE=(17, 18, 19)
---- Task: GEN -- User: lwk -- Sat Nov 13 15:49:49 2010 ----
IVAL=0.0
SINC=1.0
LINC=1.0
BINC=1.0
MODULO=0.0
 
************************************************************
gen t6
Beginning VICAR task gen
GEN Version 6
GEN task completed
label-concat (t5,t6) task=stretch
Beginning VICAR task label
LABEL version 15-Nov-2010
label-list t6
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File t6 ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in BYTE format from a X86-LINUX host
                1 bands
                10 lines per band
                10 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: GEN -- User: lwk -- Sat Nov 13 15:49:49 2010 ----
IVAL=0.0
SINC=1.0
LINC=1.0
BINC=1.0
MODULO=0.0
---- Task: STRETCH -- User: lwk -- Sat Nov 13 15:49:49 2010 ----
PARMS='AUTO-STRETCH:      1 to      0 and     18 to    255'
PARMS2='task stretch'
 
************************************************************
gen t6
Beginning VICAR task gen
GEN Version 6
GEN task completed
label-concat (t5,t6) t7 prop=tstlut instance=2
Beginning VICAR task label
LABEL version 15-Nov-2010
label-list t7
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File t7 ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in BYTE format from a X86-LINUX host
                1 bands
                10 lines per band
                10 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Property: TSTLUT ----
RED=(11, 12, 13)
GREEN=(14, 15, 16)
BLUE=(17, 18, 19)
---- Task: GEN -- User: lwk -- Sat Nov 13 15:49:49 2010 ----
IVAL=0.0
SINC=1.0
LINC=1.0
BINC=1.0
MODULO=0.0
---- Task: LABEL -- User: lwk -- Sat Nov 13 15:49:49 2010 ----
 
************************************************************
write " -DELETE -- Delete different items"
 -DELETE -- Delete different items
label-delete t5 task=insert key=mv element=3 nelement=2
Beginning VICAR task label
LABEL version 15-Nov-2010
Elements 3 through 4 deleted from keyword MV
label-list t5 'hist 'pack
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File t5 ************
---- Task: GEN -- User: lwk -- Sat Nov 13 15:49:49 2010 ----
IVAL=0.0  SINC=1.0  LINC=1.0  BINC=1.0  MODULO=0.0
---- Task: INSERT -- User: lwk -- Sat Nov 13 15:49:49 2010 ----
COMMENT=('straight data copy', 'second comment')
---- Task: STRETCH -- User: lwk -- Sat Nov 13 15:49:49 2010 ----
PARMS='AUTO-STRETCH:      1 to      0 and     18 to    255'
PARMS2='task stretch'
---- Task: INSERT -- User: lwk -- Sat Nov 13 15:49:49 2010 ----
SLOPE=5.6  TIME=45  NAME='No name'  MV=(1.2, 3.5, 9.0)  DSLOPE=5.00000012
---- Task: LABEL -- User: lwk -- Sat Nov 13 15:49:49 2010 ----
ACTION=('LABEL-ADD', '-- copy option used')  LONG=(1, 2, 3, 4, 5, 6, 7, 8, 
9, 10)
 
************************************************************
label-delete t5 task=insert
Beginning VICAR task label
LABEL version 15-Nov-2010
Keyword COMMENT deleted
History subset INSERT deleted
Keyword SLOPE deleted
Keyword TIME deleted
Keyword NAME deleted
Keyword MV deleted
Keyword DSLOPE deleted
History subset INSERT deleted
label-list t5 'hist
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File t5 ************
---- Task: GEN -- User: lwk -- Sat Nov 13 15:49:49 2010 ----
IVAL=0.0
SINC=1.0
LINC=1.0
BINC=1.0
MODULO=0.0
---- Task: STRETCH -- User: lwk -- Sat Nov 13 15:49:49 2010 ----
PARMS='AUTO-STRETCH:      1 to      0 and     18 to    255'
PARMS2='task stretch'
---- Task: LABEL -- User: lwk -- Sat Nov 13 15:49:49 2010 ----
ACTION=('LABEL-ADD', '-- copy option used')
LONG=(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
 
************************************************************
label-delete t5 task=gen key=/ALL
Beginning VICAR task label
LABEL version 15-Nov-2010
Keyword IVAL deleted
Keyword SINC deleted
Keyword LINC deleted
Keyword BINC deleted
Keyword MODULO deleted
label-list t5
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File t5 ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in BYTE format from a X86-LINUX host
                1 bands
                10 lines per band
                10 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Property: TSTMAP ----
CENTER=(45.0, 12.7)
LINE=5
SCALE=10.0
PROJ='cylindrical'
SAMP=10
LONGITUDE=10.5
---- Property: TSTLUT ----
RED=(1, 2, 3, 4)
GREEN=(4, 5, 6, 7)
BLUE=(7, 8, 9, 10)
---- Property: TSTLUT (#2) ----
RED=(11, 12, 13)
GREEN=(14, 15, 16)
BLUE=(17, 18, 19)
---- Task: GEN -- User: lwk -- Sat Nov 13 15:49:49 2010 ----
---- Task: STRETCH -- User: lwk -- Sat Nov 13 15:49:49 2010 ----
PARMS='AUTO-STRETCH:      1 to      0 and     18 to    255'
PARMS2='task stretch'
---- Task: LABEL -- User: lwk -- Sat Nov 13 15:49:49 2010 ----
ACTION=('LABEL-ADD', '-- copy option used')
LONG=(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
 
************************************************************
label-delete t5 key=(red,green) prop=tstlut element=1 nelement=1 instnces=2
Beginning VICAR task label
LABEL version 15-Nov-2010
Element 1 deleted from keyword RED
Element 1 deleted from keyword GREEN
label-list t5 'prop
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File t5 ************
---- Property: TSTMAP ----
CENTER=(45.0, 12.7)
LINE=5
SCALE=10.0
PROJ='cylindrical'
SAMP=10
LONGITUDE=10.5
---- Property: TSTLUT ----
RED=(1, 2, 3, 4)
GREEN=(4, 5, 6, 7)
BLUE=(7, 8, 9, 10)
---- Property: TSTLUT (#2) ----
RED=(12, 13)
GREEN=(15, 16)
BLUE=(17, 18, 19)
 
************************************************************
label-delete t5 key=(red,green,blue) prop=tstlut element=1 nelement=1
Beginning VICAR task label
LABEL version 15-Nov-2010
Element 1 deleted from keyword RED
Element 1 deleted from keyword GREEN
Element 1 deleted from keyword BLUE
Element 1 deleted from keyword RED
Element 1 deleted from keyword GREEN
Element 1 deleted from keyword BLUE
label-list t5 'prop
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File t5 ************
---- Property: TSTMAP ----
CENTER=(45.0, 12.7)
LINE=5
SCALE=10.0
PROJ='cylindrical'
SAMP=10
LONGITUDE=10.5
---- Property: TSTLUT ----
RED=(2, 3, 4)
GREEN=(5, 6, 7)
BLUE=(8, 9, 10)
---- Property: TSTLUT (#2) ----
RED=13
GREEN=16
BLUE=(18, 19)
 
************************************************************
label-delete t5 t6 key=/all prop=tstmap
Beginning VICAR task label
LABEL version 15-Nov-2010
Keyword CENTER deleted
Keyword LINE deleted
Keyword SCALE deleted
Keyword PROJ deleted
Keyword SAMP deleted
Keyword LONGITUDE deleted
label-list t6 'prop
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File t6 ************
---- Property: TSTMAP ----
---- Property: TSTLUT ----
RED=(2, 3, 4)
GREEN=(5, 6, 7)
BLUE=(8, 9, 10)
---- Property: TSTLUT (#2) ----
RED=13
GREEN=16
BLUE=(18, 19)
 
************************************************************
label-delete t6 'property
Beginning VICAR task label
LABEL version 15-Nov-2010
Property subset TSTMAP deleted
Keyword RED deleted
Keyword GREEN deleted
Keyword BLUE deleted
Property subset TSTLUT deleted
Keyword RED deleted
Keyword GREEN deleted
Keyword BLUE deleted
Property subset TSTLUT deleted
label-list t6
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File t6 ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in BYTE format from a X86-LINUX host
                1 bands
                10 lines per band
                10 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: GEN -- User: lwk -- Sat Nov 13 15:49:49 2010 ----
---- Task: STRETCH -- User: lwk -- Sat Nov 13 15:49:49 2010 ----
PARMS='AUTO-STRETCH:      1 to      0 and     18 to    255'
PARMS2='task stretch'
---- Task: LABEL -- User: lwk -- Sat Nov 13 15:49:49 2010 ----
ACTION=('LABEL-ADD', '-- copy option used')
LONG=(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
---- Task: LABEL -- User: lwk -- Sat Nov 13 15:49:50 2010 ----
 
************************************************************
write " -REPLACE -- Replace specific label items"
 -REPLACE -- Replace specific label items
label-repla t4 item="nl=5" 'sys
Beginning VICAR task label
LABEL version 15-Nov-2010
Keyword nl replaced
label-list t4
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File t4 ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in BYTE format from a X86-LINUX host
                1 bands
                5 lines per band
                10 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Property: TSTMAP ----
CENTER=(45.0, 12.7)
LINE=5
SCALE=10.0
PROJ='cylindrical'
SAMP=10
LONGITUDE=10.5
---- Property: TSTLUT ----
RED=(1, 2, 3)
GREEN=(4, 5, 6)
BLUE=(7, 8, 9)
---- Property: TSTLUT (#2) ----
RED=(11, 12, 13)
GREEN=(14, 15, 16)
BLUE=(17, 18, 19)
---- Task: GEN -- User: lwk -- Sat Nov 13 15:49:49 2010 ----
IVAL=0.0
SINC=1.0
LINC=1.0
BINC=1.0
MODULO=0.0
---- Task: INSERT -- User: lwk -- Sat Nov 13 15:49:49 2010 ----
COMMENT=('straight data copy', 'second comment')
---- Task: STRETCH -- User: lwk -- Sat Nov 13 15:49:49 2010 ----
PARMS='AUTO-STRETCH:      1 to      0 and     18 to    255'
PARMS2='task stretch'
---- Task: INSERT -- User: lwk -- Sat Nov 13 15:49:49 2010 ----
SLOPE=5.6
TIME=45
NAME='No name'
MV=(1.2, 3.5, 4.6, 7.8, 9.0)
DSLOPE=5.00000012
 
************************************************************
label-repla t3 t4 "ival=9999" task=gen inst=1
Beginning VICAR task label
LABEL version 15-Nov-2010
Keyword ival replaced
label-list t4
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File t4 ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in BYTE format from a X86-LINUX host
                1 bands
                10 lines per band
                10 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: GEN -- User: lwk -- Sat Nov 13 15:49:49 2010 ----
SINC=1.0
LINC=1.0
BINC=1.0
MODULO=0.0
IVAL=9999
---- Task: INSERT -- User: lwk -- Sat Nov 13 15:49:49 2010 ----
---- Task: STRETCH -- User: lwk -- Sat Nov 13 15:49:49 2010 ----
PARMS='AUTO-STRETCH:      1 to      0 and     18 to    255'
PARMS2='task stretch'
---- Task: LABEL -- User: lwk -- Sat Nov 13 15:49:50 2010 ----
 
************************************************************
label-repla t5 item="long(5)=(50,60)"
Beginning VICAR task label
LABEL version 15-Nov-2010
Elements 5 through 6 replaced in keyword long
label-repla t5 item="red(2)=30 green(2)=60" prop=tstlut
Beginning VICAR task label
LABEL version 15-Nov-2010
Element 2 replaced in keyword red
Element 2 replaced in keyword green
label-repla t5 item="blue(2)=(90,100)" prop=tstlut instance=2
Beginning VICAR task label
LABEL version 15-Nov-2010
Elements 2 through 3 replaced in keyword blue
label-list t5 'pack
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File t5 ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in BYTE format from a X86-LINUX host
                1 bands
                10 lines per band
                10 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Property: TSTMAP ----
CENTER=(45.0, 12.7)  LINE=5  SCALE=10.0  PROJ='cylindrical'  SAMP=10
LONGITUDE=10.5
---- Property: TSTLUT ----
RED=(2, 30, 4)  GREEN=(5, 60, 7)  BLUE=(8, 9, 10)
---- Property: TSTLUT (#2) ----
RED=13  GREEN=16  BLUE=(18, 90, 100)
---- Task: GEN -- User: lwk -- Sat Nov 13 15:49:49 2010 ----
---- Task: STRETCH -- User: lwk -- Sat Nov 13 15:49:49 2010 ----
PARMS='AUTO-STRETCH:      1 to      0 and     18 to    255'
PARMS2='task stretch'
---- Task: LABEL -- User: lwk -- Sat Nov 13 15:49:49 2010 ----
ACTION=('LABEL-ADD', '-- copy option used')  LONG=(1, 2, 3, 4, 50, 60, 7, 8, 
9, 10)
 
************************************************************
write " Now make everything single-valued by deleting elements > 1"
 Now make everything single-valued by deleting elements > 1
label-delete t5 t6 key=/ALL element=2 nelement=-1 'hist
Beginning VICAR task label
LABEL version 15-Nov-2010
Element 2 deleted from keyword ACTION
Elements 2 through 10 deleted from keyword LONG
label-delete t6 key=/ALL element=2 nelement=-1 'prop
Beginning VICAR task label
LABEL version 15-Nov-2010
Element 2 deleted from keyword CENTER
Elements 2 through 3 deleted from keyword RED
Elements 2 through 3 deleted from keyword GREEN
Elements 2 through 3 deleted from keyword BLUE
Elements 2 through 3 deleted from keyword BLUE
label-list t6
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File t6 ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in BYTE format from a X86-LINUX host
                1 bands
                10 lines per band
                10 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Property: TSTMAP ----
CENTER=45.0
LINE=5
SCALE=10.0
PROJ='cylindrical'
SAMP=10
LONGITUDE=10.5
---- Property: TSTLUT ----
RED=2
GREEN=5
BLUE=8
---- Property: TSTLUT (#2) ----
RED=13
GREEN=16
BLUE=18
---- Task: GEN -- User: lwk -- Sat Nov 13 15:49:49 2010 ----
---- Task: STRETCH -- User: lwk -- Sat Nov 13 15:49:49 2010 ----
PARMS='AUTO-STRETCH:      1 to      0 and     18 to    255'
PARMS2='task stretch'
---- Task: LABEL -- User: lwk -- Sat Nov 13 15:49:49 2010 ----
ACTION='LABEL-ADD'
LONG=1
---- Task: LABEL -- User: lwk -- Sat Nov 13 15:49:50 2010 ----
 
************************************************************
copy t4 t7
Beginning VICAR task copy
 COPY VERSION 12-JUL-1993
label-list t4
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File t4 ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in BYTE format from a X86-LINUX host
                1 bands
                10 lines per band
                10 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: GEN -- User: lwk -- Sat Nov 13 15:49:49 2010 ----
SINC=1.0
LINC=1.0
BINC=1.0
MODULO=0.0
IVAL=9999
---- Task: INSERT -- User: lwk -- Sat Nov 13 15:49:49 2010 ----
---- Task: STRETCH -- User: lwk -- Sat Nov 13 15:49:49 2010 ----
PARMS='AUTO-STRETCH:      1 to      0 and     18 to    255'
PARMS2='task stretch'
---- Task: LABEL -- User: lwk -- Sat Nov 13 15:49:50 2010 ----
 
************************************************************
write " -REMOVE -- Remove the binary header"
 -REMOVE -- Remove the binary header
label-remove t4 t1 'NOBINHEAD
Beginning VICAR task label
LABEL version 15-Nov-2010
write "	attempt to list should be successful"
	attempt to list should be successful
label-list t1
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File t1 ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in BYTE format from a X86-LINUX host
                1 bands
                10 lines per band
                10 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: GEN -- User: lwk -- Sat Nov 13 15:49:49 2010 ----
SINC=1.0
LINC=1.0
BINC=1.0
MODULO=0.0
IVAL=9999
---- Task: INSERT -- User: lwk -- Sat Nov 13 15:49:49 2010 ----
---- Task: STRETCH -- User: lwk -- Sat Nov 13 15:49:49 2010 ----
PARMS='AUTO-STRETCH:      1 to      0 and     18 to    255'
PARMS2='task stretch'
---- Task: LABEL -- User: lwk -- Sat Nov 13 15:49:50 2010 ----
---- Task: LABEL -- User: lwk -- Sat Nov 13 15:49:50 2010 ----
 
************************************************************
write " -REMOVE -- Remove the binary prefix"
 -REMOVE -- Remove the binary prefix
label-remove t4 t1 'NOBINPREF
Beginning VICAR task label
LABEL version 15-Nov-2010
write "	attempt to list should be successful"
	attempt to list should be successful
label-list t1
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File t1 ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in BYTE format from a X86-LINUX host
                1 bands
                10 lines per band
                10 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: GEN -- User: lwk -- Sat Nov 13 15:49:49 2010 ----
SINC=1.0
LINC=1.0
BINC=1.0
MODULO=0.0
IVAL=9999
---- Task: INSERT -- User: lwk -- Sat Nov 13 15:49:49 2010 ----
---- Task: STRETCH -- User: lwk -- Sat Nov 13 15:49:49 2010 ----
PARMS='AUTO-STRETCH:      1 to      0 and     18 to    255'
PARMS2='task stretch'
---- Task: LABEL -- User: lwk -- Sat Nov 13 15:49:50 2010 ----
---- Task: LABEL -- User: lwk -- Sat Nov 13 15:49:50 2010 ----
 
************************************************************
write " -REMOVE -- Remove all the header"
 -REMOVE -- Remove all the header
label-remove t4 t1
Beginning VICAR task label
LABEL version 15-Nov-2010
write "	attempt to list should cause an error"
	attempt to list should cause an error
label-list t1
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File t1 ************
[VIC2-GENERR] Exception in XVOPEN, processing file: t1
[VIC2-BADLBL] Bad input label; check file contents
 ** ABEND called **
continue
write " -CREATE -- add a label then list it out"
 -CREATE -- add a label then list it out
label-create t1 t6 nl=10 ns=10 comment="What a slick image"
Beginning VICAR task label
LABEL version 15-Nov-2010
label-list t6
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File t6 ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in BYTE format from a X86-LINUX host
                1 bands
                10 lines per band
                10 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: LABEL -- User: lwk -- Sat Nov 13 15:49:50 2010 ----
COMMENT='What a slick image'
 
************************************************************
write " -SWITCH -- gen a different size file then switch the labels"
 -SWITCH -- gen a different size file then switch the labels
gen t1 5 5 linc=10
Beginning VICAR task gen
GEN Version 6
GEN task completed
list t1
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sat Nov 13 15:49:50 2010
     Samp     1       3       5
   Line
      1       0   1   2   3   4
      2      10  11  12  13  14
      3      20  21  22  23  24
      4      30  31  32  33  34
      5      40  41  42  43  44
label-switch (t5,t1) t2
Beginning VICAR task label
LABEL version 15-Nov-2010
list t2
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sat Nov 13 15:49:49 2010
 Task:LABEL     User:lwk       Date_Time:Sat Nov 13 15:49:50 2010
     Samp     1       3       5
   Line
      1       0   1   2   3   4
      2      10  11  12  13  14
      3      20  21  22  23  24
      4      30  31  32  33  34
      5      40  41  42  43  44
label-list t2
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File t2 ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in BYTE format from a X86-LINUX host
                1 bands
                5 lines per band
                5 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Property: TSTMAP ----
CENTER=(45.0, 12.7)
LINE=5
SCALE=10.0
PROJ='cylindrical'
SAMP=10
LONGITUDE=10.5
---- Property: TSTLUT ----
RED=(2, 30, 4)
GREEN=(5, 60, 7)
BLUE=(8, 9, 10)
---- Property: TSTLUT (#2) ----
RED=13
GREEN=16
BLUE=(18, 90, 100)
---- Task: GEN -- User: lwk -- Sat Nov 13 15:49:49 2010 ----
---- Task: STRETCH -- User: lwk -- Sat Nov 13 15:49:49 2010 ----
PARMS='AUTO-STRETCH:      1 to      0 and     18 to    255'
PARMS2='task stretch'
---- Task: LABEL -- User: lwk -- Sat Nov 13 15:49:49 2010 ----
ACTION=('LABEL-ADD', '-- copy option used')
LONG=(1, 2, 3, 4, 50, 60, 7, 8, 9, 10)
---- Task: LABEL -- User: lwk -- Sat Nov 13 15:49:50 2010 ----
 
************************************************************
write ""

write "Generate a 3-D file with some labels first"
Generate a 3-D file with some labels first
gen t1 10 10 3 'BIL
Beginning VICAR task gen
GEN Version 6
GEN task completed
copy t1 t7
Beginning VICAR task copy
 COPY VERSION 12-JUL-1993
label-list t1
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File t1 ************
                3 dimensional IMAGE file
                File organization is BIL
                Pixels are in BYTE format from a X86-LINUX host
                3 bands
                10 lines per band
                10 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: GEN -- User: lwk -- Sat Nov 13 15:49:50 2010 ----
IVAL=0.0
SINC=1.0
LINC=1.0
BINC=1.0
MODULO=0.0
 
************************************************************
write "	attempt to list should be successful"
	attempt to list should be successful
label-list t2
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File t2 ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in BYTE format from a X86-LINUX host
                1 bands
                5 lines per band
                5 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Property: TSTMAP ----
CENTER=(45.0, 12.7)
LINE=5
SCALE=10.0
PROJ='cylindrical'
SAMP=10
LONGITUDE=10.5
---- Property: TSTLUT ----
RED=(2, 30, 4)
GREEN=(5, 60, 7)
BLUE=(8, 9, 10)
---- Property: TSTLUT (#2) ----
RED=13
GREEN=16
BLUE=(18, 90, 100)
---- Task: GEN -- User: lwk -- Sat Nov 13 15:49:49 2010 ----
---- Task: STRETCH -- User: lwk -- Sat Nov 13 15:49:49 2010 ----
PARMS='AUTO-STRETCH:      1 to      0 and     18 to    255'
PARMS2='task stretch'
---- Task: LABEL -- User: lwk -- Sat Nov 13 15:49:49 2010 ----
ACTION=('LABEL-ADD', '-- copy option used')
LONG=(1, 2, 3, 4, 50, 60, 7, 8, 9, 10)
---- Task: LABEL -- User: lwk -- Sat Nov 13 15:49:50 2010 ----
 
************************************************************
write " -REMOVE -- Remove the binary prefix"
 -REMOVE -- Remove the binary prefix
label-remove t1 t2 'NOBINPREF
Beginning VICAR task label
LABEL version 15-Nov-2010
write "	attempt to list should be successful"
	attempt to list should be successful
label-list t2
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File t2 ************
                3 dimensional IMAGE file
                File organization is BIL
                Pixels are in BYTE format from a X86-LINUX host
                3 bands
                10 lines per band
                10 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: GEN -- User: lwk -- Sat Nov 13 15:49:50 2010 ----
IVAL=0.0
SINC=1.0
LINC=1.0
BINC=1.0
MODULO=0.0
---- Task: LABEL -- User: lwk -- Sat Nov 13 15:49:50 2010 ----
 
************************************************************
write " -REMOVE -- Remove the binary header and prefix"
 -REMOVE -- Remove the binary header and prefix
label-remove t1 t2
Beginning VICAR task label
LABEL version 15-Nov-2010
write "	attempt to list should cause an error"
	attempt to list should cause an error
label-list t2
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File t2 ************
[VIC2-GENERR] Exception in XVOPEN, processing file: t2
[VIC2-BADLBL] Bad input label; check file contents
 ** ABEND called **
continue
write " -CREATE -- add a label"
 -CREATE -- add a label
label-create t2 t3 ns=10 nl=10 nb=3 org=BIL
Beginning VICAR task label
LABEL version 15-Nov-2010
label-list t3
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File t3 ************
                3 dimensional IMAGE file
                File organization is BIL
                Pixels are in BYTE format from a X86-LINUX host
                3 bands
                10 lines per band
                10 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: LABEL -- User: lwk -- Sat Nov 13 15:49:50 2010 ----
 
************************************************************
write " -CREATE -- different host formats"
 -CREATE -- different host formats
label-create t2 t3 ns=10 nl=10 nb=3 org=BIL host=VAX-VMS
Beginning VICAR task label
LABEL version 15-Nov-2010
label-list t3 'sys
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File t3 ************
                3 dimensional IMAGE file
                File organization is BIL
                Pixels are in BYTE format from a VAX-VMS host
                3 bands
                10 lines per band
                10 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
 
************************************************************
label-list t3 'dump
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File t3 ************
LBLSIZE=370
FORMAT='BYTE'
TYPE='IMAGE'
BUFSIZ=20480
DIM=3
EOL=0
RECSIZE=10
ORG='BIL'
NL=10
NS=10
NB=3
N1=10
N2=3
N3=10
N4=0
NBB=0
NLB=0
HOST='VAX-VMS'
INTFMT='LOW'
REALFMT='VAX'
BHOST='VAX-VMS'
BINTFMT='LOW'
BREALFMT='VAX'
BLTYPE=''
COMPRESS='NONE'
EOCI1=0
EOCI2=0
TASK='LABEL'
USER='lwk'
DAT_TIM='Sat Nov 13 15:49:50 2010'
 
************************************************************
label-create t2 t3 ns=10 nl=10 nb=3 org=BIL host=SUN-4
Beginning VICAR task label
LABEL version 15-Nov-2010
label-list t3 'sys
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File t3 ************
                3 dimensional IMAGE file
                File organization is BIL
                Pixels are in BYTE format from a SUN-4 host
                3 bands
                10 lines per band
                10 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
 
************************************************************
label-list t3 'dump
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File t3 ************
LBLSIZE=370
FORMAT='BYTE'
TYPE='IMAGE'
BUFSIZ=20480
DIM=3
EOL=0
RECSIZE=10
ORG='BIL'
NL=10
NS=10
NB=3
N1=10
N2=3
N3=10
N4=0
NBB=0
NLB=0
HOST='SUN-4'
INTFMT='HIGH'
REALFMT='IEEE'
BHOST='VAX-VMS'
BINTFMT='LOW'
BREALFMT='VAX'
BLTYPE=''
COMPRESS='NONE'
EOCI1=0
EOCI2=0
TASK='LABEL'
USER='lwk'
DAT_TIM='Sat Nov 13 15:49:50 2010'
 
************************************************************
write " test handling long records. "
 test handling long records. 
gen t1 10 200000
Beginning VICAR task gen
GEN Version 6
GEN task completed
label-add t1 t4 items="slope=5.6"
Beginning VICAR task label
LABEL version 15-Nov-2010
Keyword slope added
label-list t4
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File t4 ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in BYTE format from a X86-LINUX host
                1 bands
                10 lines per band
                200000 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: GEN -- User: lwk -- Sat Nov 13 15:49:50 2010 ----
IVAL=0.0
SINC=1.0
LINC=1.0
BINC=1.0
MODULO=0.0
---- Task: LABEL -- User: lwk -- Sat Nov 13 15:49:50 2010 ----
SLOPE=5.6
 
************************************************************
write "multi-input option and abend with -LIST"
multi-input option and abend with -LIST
label-list (t1 t2 t3 t4)
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File t1 ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in BYTE format from a X86-LINUX host
                1 bands
                10 lines per band
                200000 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: GEN -- User: lwk -- Sat Nov 13 15:49:50 2010 ----
IVAL=0.0
SINC=1.0
LINC=1.0
BINC=1.0
MODULO=0.0
 
************************************************************
************************************************************
 
        ************  File t2 ************
[VIC2-GENERR] Exception in XVOPEN, processing file: t2
[VIC2-BADLBL] Bad input label; check file contents
************************************************************
 
        ************  File t3 ************
                3 dimensional IMAGE file
                File organization is BIL
                Pixels are in BYTE format from a SUN-4 host
                3 bands
                10 lines per band
                10 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: LABEL -- User: lwk -- Sat Nov 13 15:49:50 2010 ----
 
************************************************************
************************************************************
 
        ************  File t4 ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in BYTE format from a X86-LINUX host
                1 bands
                10 lines per band
                200000 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: GEN -- User: lwk -- Sat Nov 13 15:49:50 2010 ----
IVAL=0.0
SINC=1.0
LINC=1.0
BINC=1.0
MODULO=0.0
---- Task: LABEL -- User: lwk -- Sat Nov 13 15:49:50 2010 ----
SLOPE=5.6
 
************************************************************
ush rm xxxx
[TAE-ABNSHELL] Abnormal shell termination.;
 proc 'tstlabel', line 159
continue
write "this should generate an error msg but no abend:"
this should generate an error msg but no abend:
label-list (t1 xxxx t3 t4)
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File t1 ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in BYTE format from a X86-LINUX host
                1 bands
                10 lines per band
                200000 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: GEN -- User: lwk -- Sat Nov 13 15:49:50 2010 ----
IVAL=0.0
SINC=1.0
LINC=1.0
BINC=1.0
MODULO=0.0
 
************************************************************
************************************************************
 
        ************  File xxxx ************
[VIC2-GENERR] Exception in XVOPEN, processing file: xxxx
[VIC2-HOSTMSG]  No such file or directory
************************************************************
 
        ************  File t3 ************
                3 dimensional IMAGE file
                File organization is BIL
                Pixels are in BYTE format from a SUN-4 host
                3 bands
                10 lines per band
                10 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: LABEL -- User: lwk -- Sat Nov 13 15:49:50 2010 ----
 
************************************************************
************************************************************
 
        ************  File t4 ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in BYTE format from a X86-LINUX host
                1 bands
                10 lines per band
                200000 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: GEN -- User: lwk -- Sat Nov 13 15:49:50 2010 ----
IVAL=0.0
SINC=1.0
LINC=1.0
BINC=1.0
MODULO=0.0
---- Task: LABEL -- User: lwk -- Sat Nov 13 15:49:50 2010 ----
SLOPE=5.6
 
************************************************************
write "this should generate an abend:"
this should generate an abend:
label-list xxxx
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File xxxx ************
[VIC2-GENERR] Exception in XVOPEN, processing file: xxxx
[VIC2-HOSTMSG]  No such file or directory
 ** ABEND called **
continue
if ($syschar(1) = "UNIX")
  ush rm t1 t2 t3 t4 t5 t6 t7
else
end-if
label-list "/project/test_work/testdata/gll/s0061000300.1"
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File /project/test_work/testdata/gll/s0061000300.1 ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in BYTE format from a VAX-VMS host
                1 bands
                800 lines per band
                800 samples per line
                2 lines of binary header
                200 bytes of binary prefix per line
---- Task: TASK -- User: RTO320 -- Wed Jan 16 10:37:39 1991 ----
MISSION='GALILEO'
SENSOR='SSI'
PICNO='?'
PA='E1LNLTIN_-05B___LCOP'
RIM=610003
MOD91=0
MOD10=1
MOD8=0
TCA='CLOSEST TIME'
TARGET='MOON'
SCETYEAR=1990
SCETDAY=342
SCETHOUR=16
SCETMIN=59
SCETSEC=48
SCETMSEC=133
FILTER=0
EXP=4.167
GAIN=3
RATE=3
TLMFMT='HCJ'
BOOM='P'
FIBE='1000'
BARC='RC'
ENTROPY=0.0
TBPPXL=0.0
TPPLNE=0.0
INA=-999.0
EMA=-999.0
PHA=-999.0
HRA=-999.0
TWIST=0.0
CONE=0.0
RA=0.0
DEC=0.0
SUNAZ=0.0
NORAZ=0.0
SCAZ=0.0
SMRAZ=0.0
SMEAR=-999.0
HSCL=0.0
VSCL=0.0
LAT=0.0
LON=0.0
RAD=0.0
PLRANGE=0.0
SLRANGE=0.0
SOLRANGE=7.779091e+08
 
************************************************************
label-remove "/project/test_work/testdata/gll/s0061000300.1" test.nolabel 'BINARY
Beginning VICAR task label
LABEL version 15-Nov-2010
write " attempt to list should cause error "
 attempt to list should cause error 
label-list test.nolabel
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File test.nolabel ************
[VIC2-GENERR] Exception in XVOPEN, processing file: test.nolabel
[VIC2-BADLBL] Bad input label; check file contents
 ** ABEND called **
continue
label-create test.nolabel test.vpds NL=800 NS=800 NLB=2 NBB=200 'BINARY
Beginning VICAR task label
LABEL version 15-Nov-2010
label-list test.vpds
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File test.vpds ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in BYTE format from a X86-LINUX host
                1 bands
                800 lines per band
                800 samples per line
                2 lines of binary header
                200 bytes of binary prefix per line
---- Task: LABEL -- User: lwk -- Sat Nov 13 15:49:56 2010 ----
 
************************************************************
difpic ("/project/test_work/testdata/gll/s0061000300.1",test.vpds)
Beginning VICAR task difpic
DIFPIC version 05jul10
 NUMBER OF DIFFERENCES =   0
label-create test.nolabel test.binary nl=800 ns=800 nlb=2 nbb=200 'bin  +
	bhost=vax-vms bltype="tstpds"
Beginning VICAR task label
LABEL version 15-Nov-2010
label-list test.binary 'sys
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File test.binary ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in BYTE format from a X86-LINUX host
                1 bands
                800 lines per band
                800 samples per line
                2 lines of binary header of type TSTPDS
                200 bytes of binary prefix per line
 
************************************************************
label-list test.binary 'dump
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File test.binary ************
LBLSIZE=1000
FORMAT='BYTE'
TYPE='IMAGE'
BUFSIZ=20480
DIM=3
EOL=0
RECSIZE=1000
ORG='BSQ'
NL=800
NS=800
NB=1
N1=800
N2=800
N3=1
N4=0
NBB=200
NLB=2
HOST='X86-LINUX'
INTFMT='LOW'
REALFMT='RIEEE'
BHOST='VAX-VMS'
BINTFMT='LOW'
BREALFMT='VAX'
BLTYPE='TSTPDS'
COMPRESS='NONE'
EOCI1=0
EOCI2=0
TASK='LABEL'
USER='lwk'
DAT_TIM='Sat Nov 13 15:49:56 2010'
 
************************************************************
label-create test.nolabel test.binary nl=800 ns=800 nlb=2 nbb=200 'bin  +
	bhost="vax-vms" brealfmt="vax" bltype="tstpds"
Beginning VICAR task label
LABEL version 15-Nov-2010
label-list test.binary
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File test.binary ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in BYTE format from a X86-LINUX host
                1 bands
                800 lines per band
                800 samples per line
                2 lines of binary header of type TSTPDS
                200 bytes of binary prefix per line
---- Task: LABEL -- User: lwk -- Sat Nov 13 15:49:56 2010 ----
 
************************************************************
label-list test.binary 'dump
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File test.binary ************
LBLSIZE=1000
FORMAT='BYTE'
TYPE='IMAGE'
BUFSIZ=20480
DIM=3
EOL=0
RECSIZE=1000
ORG='BSQ'
NL=800
NS=800
NB=1
N1=800
N2=800
N3=1
N4=0
NBB=200
NLB=2
HOST='X86-LINUX'
INTFMT='LOW'
REALFMT='RIEEE'
BHOST='VAX-VMS'
BINTFMT='LOW'
BREALFMT='VAX'
BLTYPE='TSTPDS'
COMPRESS='NONE'
EOCI1=0
EOCI2=0
TASK='LABEL'
USER='lwk'
DAT_TIM='Sat Nov 13 15:49:56 2010'
 
************************************************************
write "The following commands test the new binary features of LABEL"
The following commands test the new binary features of LABEL
label-add test.vpds test.added items="LIST=(1,2)"
Beginning VICAR task label
LABEL version 15-Nov-2010
Keyword LIST added
label-add test.added items="test=binary" prop=tstprop
Beginning VICAR task label
LABEL version 15-Nov-2010
Keyword test added
label-list test.added
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File test.added ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in BYTE format from a X86-LINUX host
                1 bands
                800 lines per band
                800 samples per line
                2 lines of binary header
                200 bytes of binary prefix per line
---- Property: TSTPROP ----
TEST='binary'
---- Task: LABEL -- User: lwk -- Sat Nov 13 15:49:56 2010 ----
---- Task: LABEL -- User: lwk -- Sat Nov 13 15:49:56 2010 ----
LIST=(1, 2)
 
************************************************************
difpic (test.vpds,test.added)
Beginning VICAR task difpic
DIFPIC version 05jul10
 NUMBER OF DIFFERENCES =   0
label-del test.added test.deleted keys="LIST" tasks="LABEL"
Beginning VICAR task label
LABEL version 15-Nov-2010
Keyword LIST deleted
label-del test.deleted keys="TEST" prop=tstprop
Beginning VICAR task label
LABEL version 15-Nov-2010
Keyword TEST deleted
label-list test.deleted
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File test.deleted ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in BYTE format from a X86-LINUX host
                1 bands
                800 lines per band
                800 samples per line
                2 lines of binary header
                200 bytes of binary prefix per line
---- Property: TSTPROP ----
---- Task: LABEL -- User: lwk -- Sat Nov 13 15:49:56 2010 ----
---- Task: LABEL -- User: lwk -- Sat Nov 13 15:49:56 2010 ----
---- Task: LABEL -- User: lwk -- Sat Nov 13 15:49:56 2010 ----
 
************************************************************
difpic (test.vpds,test.deleted)
Beginning VICAR task difpic
DIFPIC version 05jul10
 NUMBER OF DIFFERENCES =   0
label-switch (test.added,test.deleted) test.switch
Beginning VICAR task label
LABEL version 15-Nov-2010
label-list test.switch
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File test.switch ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in BYTE format from a X86-LINUX host
                1 bands
                800 lines per band
                800 samples per line
                2 lines of binary header
                200 bytes of binary prefix per line
---- Property: TSTPROP ----
TEST='binary'
---- Task: LABEL -- User: lwk -- Sat Nov 13 15:49:56 2010 ----
---- Task: LABEL -- User: lwk -- Sat Nov 13 15:49:56 2010 ----
LIST=(1, 2)
---- Task: LABEL -- User: lwk -- Sat Nov 13 15:49:56 2010 ----
 
************************************************************
difpic (test.vpds,test.switch)
Beginning VICAR task difpic
DIFPIC version 05jul10
 NUMBER OF DIFFERENCES =   0
if ($syschar(1) = "UNIX")
  ush rm test.nolabel test.vpds test.binary test.added test.deleted test.switch
else
end-if
end-proc
exit
slogoff
if ($RUNTYPE = "INTERACTIVE")
  if ($syschar(1) = "VAX_VMS")
  end-if
else
  if ($syschar(1) = "VAX_VMS")
  end-if
end-if
ulogoff
END-PROC
END-PROC
$ Return
$!#############################################################################
