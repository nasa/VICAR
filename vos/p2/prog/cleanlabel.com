$!****************************************************************************
$!
$! Build proc for MIPL module cleanlabel
$! VPACK Version 1.9, Monday, December 07, 2009, 16:02:18
$!
$! Execute by entering:		$ @cleanlabel
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
$ write sys$output "*** module cleanlabel ***"
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
$ write sys$output "Invalid argument given to cleanlabel.com file -- ", primary
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
$   if F$SEARCH("cleanlabel.imake") .nes. ""
$   then
$      vimake cleanlabel
$      purge cleanlabel.bld
$   else
$      if F$SEARCH("cleanlabel.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake cleanlabel
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @cleanlabel.bld "STD"
$   else
$      @cleanlabel.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create cleanlabel.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack cleanlabel.com -mixed -
	-s cleanlabel.c -
	-i cleanlabel.imake -
	-p cleanlabel.pdf -
	-t tstcleanlabel.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create cleanlabel.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/******************************************************************************
*	CLEANLABEL
*
*	This program performs cleaning of duplicated task names in VICAR
*	image labels.  
*
*	Author :  M. O'Shaughnessy, 9-13-1989
*       25-5-94 CRI MSTP S/W Conversion (VICAR Porting)
*           "close_files" proc changed to "cclose_files" to avoid 
*           improper linking on "SUN".   
******************************************************************************/
/*  Include files                                                            */
#include "vicmain_c"
#include "zvproto.h"
#include "defines.h"  /* for definition of MAX_TASKS (=200) */
#include "errdefs.h"  /* for definitions of VICAR runtime errors */
#include <string.h>

/*  Constant & macro definitions
*  To change the maximum specifiable number of task names, all you need to do 
*  is to change the max size of TASK_NAMES in the PDF.  
*
*  The flags are defined so that the default-value flag is always 1.
*/
#define VERSION_DATE "01-JUL-94"  /* date of current version of cleanlabel */
#define TRUE 	1
#define FALSE   0
#define SUCCESS 1

/* flags */
#define ALL     1                 /* flag: clean all the keywords */
#define SOME    0                 /* flag: clean only the specified keywords */
#define LAST    1                 /* flag: keep last instance of keyword */
#define FIRST   0                 /* flag: keep first instance of keyword */
#define OUT_SAME_AS_INP  1        /* flag: output file is same as input file */
#define NEW_OUT_FILE     0        /* flag: create new output file */
#define TASK_FLAG "TASK"          /* keyword marking start of history task */
#define LABELLENGTH 132             /* maximum label length */

/* buffer sizes */
#define MAXLINESIZE 200000         /* size of buffer for image data transfer */
#define MAX_KEYS 2500		  /* Max # of keys that can be deleted */

#define abort_on_error(A)         zvsignal(A,status,1)

/*  Type definitions							     */
typedef int BOOL ;
typedef char char132 [LABELLENGTH] ;


/*  Structure definition	       				            
*
*   Variables within the structure GLOBALS:
*
*   input_file	   name of input file
*   output_file    name of output file
*   inp_unit 	   unit number of the input file
*   out_unit       unit number of the output file
*   task_names []  list of tasks which contain the keyword to be cleaned
*   num_tasks      total number of tasks
*   instances []   list of specific instances of each task in task_names
*   keys []        list of keywords to be cleaned
*   num_keys       total number of keywords, if known
*   write_method   flag controlling whether new output file is created
*   keys_to_clean  flag controlling how many keywords are cleaned
*   key_to_keep    flag controlling whether first or last instance of keyword 
*                       is saved
*/
struct INVARIANT {
	char132 input_file ;	  
	char132 output_file ;	  
	int inp_unit ;		  
	int out_unit ;  
	char task_names [MAX_TASKS][MAX_LABEL_KEY_SIZE+1] ; 
        int num_tasks ;  
	int instances [MAX_TASKS] ;      
	char keys [MAX_KEYS][MAX_LABEL_KEY_SIZE+1];
	int num_keys ;
	BOOL write_method ;	  
	BOOL keys_to_clean ;
	BOOL key_to_keep ;
} ;
char msg[10];
/******************************************************************************
*		Description of some inputs to 
*		VICAR runtime routines
*
*  KEY		:Keyword of desired label item.
*  (invar->keys[item] contains the name of one keyword.
*  FORMAT 	:Format of the labelitem's value. Defaults to intrinsic type.
*  LENGTH 	:Length of returned values.  For strings, includes '\0'.
*  HIST
*  (invar->task_names) [task] is a name of a processing task.  Each 
*  taskname identifies one history subset (i.e. a taskname and its associated 
*  keywords).  The tasknames are listed in order of occurence, and any one 
*  taskname might be duplicated within the list.
*  INSTANCES
*  invar->instances [task] is a number which represents one instance of a single
*  taskname within invar.task_names.  (ie first reference, second reference, 
*  etc. first = earliest).  invar.task_names and .instance are indexed in the 
*  same way. 
******************************************************************************/


void message (text, key, stop)
   char *text, *key; 
   int stop;
/*****************************************************************************
*  Print a message which can be recorded in the log file.  Also, if the
*  message is an error message (indicated by stop=TRUE), zabend. 
******************************************************************************/
{
   zvmessage (text, key) ;
   if (stop) zabend () ;
   return ;
}

void print_program_banner ()
/******************************************************************************
*  Zvmessage is used rather than printf so that the version date will be 
*  recorded in the logfile of CLEANLABEL.
******************************************************************************/
{
  char aline[80] ;

  zvmessage
    ("**************************************************************","") ;
  (void) sprintf 
      (aline, "VICAR2 program CLEANLABEL, version %s", VERSION_DATE) ;
  message (aline, "", FALSE) ;
  zvmessage
    ("**************************************************************","") ;
  return ;
}

void copy_image (image_unit, hist_unit)
   int image_unit, /* inp file containing image data */
       hist_unit;  /* on inp: contains cleaned history label */
                    /* on output: contains cleaned history label and image */
/******************************************************************************
*  This routine copies image data from the input file into the file containing
*  the cleaned history label.  (The system label of the input file was 
*  transferred automatically.)
*
*  The two files were opened as follows:
*  if NEW_OUT_FILE
*     file with image_unit called zvunit with INP, opened as READ
*     file with hist_unit called zvunit OUT, opened as WRITE
*  if INP_SAME_AS_OUT
*     file with image_unit and file with hist_unit are the same
*     -------*For this case, copy_image does not need to be called.*-------- 
******************************************************************************/
{
    char buf [MAXLINESIZE] ;      /* buffer used for image copying */
    int status ;                
    
    message ("Copying image to cleaned header...","", FALSE) ;

/* Copy the data */
    while (TRUE) {   
        status = zvread (image_unit, buf, NULL) ;
        
	if (status == END_OF_FILE) break ;
    	else if (status != SUCCESS) {
            (void) sprintf(msg,"%d\0",status);
            message ("Error reading image", msg, FALSE) ;
            abort_on_error (image_unit) ; }
    }  /* end WHILE statement */

/* Write the data to the output file */
    status = zvwrit (hist_unit, buf, NULL) ;
    if (status != SUCCESS) { 
        (void) sprintf(msg,"%d\0",status);
        message ("Error writing image", msg, FALSE) ;
	abort_on_error (hist_unit) ;
    }
    return ;
  }


void delete_instances (invar, keyword)
   struct INVARIANT *invar;
   char *keyword;    /* keyword to be deleted */
/******************************************************************************
*  Delete all instances of the given task except for the first or last instance
*  (determined by the value of invar.search_method.)
******************************************************************************/
{
   int j, status, nelem, maxlen, test, instance_cnt ;
   BOOL found_once ;
   char aline[80], format[12] ;

   zveaction ("", "") ;
   found_once = FALSE ;
   instance_cnt = -1 ;

/*****************************************************************************/
   if (invar->key_to_keep == FIRST) {
       for (j = 0; j < invar->num_tasks ; j++) { 
/* 
*  Make sure that one keyword is not deleted:
*  Call ZLINFO to see if there is an instance of the keyword in the current
*  task.  (Defaulting HIST makes the task the current task.)
*  If there is an instance, check to see if it should be saved, otherwise 
*  delete it.  If there isn't an instance, go to the next task.
*/
	   status = zlinfo (invar->inp_unit, "HISTORY", keyword,
                   format, &maxlen, &nelem, 
		   "HIST", invar->task_names [j], 
		   "INSTANCE", invar->instances [j], NULL) ;
	   if (status == SUCCESS) {
	       instance_cnt++ ;
	       if (found_once == FALSE) {
	           found_once = TRUE ;
		   continue ;
	       }
	       status = zldel (invar->out_unit, "HISTORY", keyword,
		   "HIST", invar->task_names [j], 
		   "INSTANCE", invar->instances [j], NULL) ;
	       abort_on_error (invar->out_unit) ;          
	   }
	   else {
	       if (status == CANNOT_FIND_KEY) continue ;
	       else abort_on_error (invar->out_unit) ;          
	   } /* end if SUCCESS */
        } /* end of for-loop */
    } /* if */
/*****************************************************************************/
   else if (invar->key_to_keep == LAST) {
       test = invar->num_tasks - 1 ;
       for (j = test; j >= 0 ; j--) { 
	   status = zlinfo (invar->inp_unit, "HISTORY", keyword, format,
                   &maxlen, &nelem, 
		   "HIST", invar->task_names [j], 
		   "INSTANCE", invar->instances [j], NULL) ;
	   if (status == SUCCESS) {
	       instance_cnt++ ;
	       if (found_once == FALSE) {
	           found_once = TRUE ;
		   continue ;
	       }
	       status = zldel (invar->out_unit, "HISTORY", keyword,
 		   "HIST", invar->task_names [j], 
		   "INSTANCE", invar->instances [j], NULL) ;
	       abort_on_error (invar->out_unit) ;          
	   }
	   else {
	       if (status == CANNOT_FIND_KEY) continue ;
	       else abort_on_error (invar->out_unit) ;          
	   } /* end if SUCCESS */
        } /* end of for-loop */
    } /* else */
/***************************************************************************/

   if (found_once == FALSE) {
       (void) sprintf (aline, "Could not find keyword %s", keyword) ;
       message (aline, "CLLBL-I-KNF", FALSE) ;
   }
   else if (instance_cnt > 0) {
       if (instance_cnt == 1) {
	  (void) sprintf 
               (aline, " 1 instance  of keyword %8s was  cleaned",keyword) ;
       }
       else { 
	   (void) sprintf 
                  (aline, "%2d instances of keyword %8s were cleaned", 
		    instance_cnt, keyword) ;
       }
       message (aline, "", FALSE) ;
   }
   return ;
}


void open_files (invar)
   struct INVARIANT *invar;
/******************************************************************************
*  This routine does the following:
*  - calls ZVUNIT AND ZVOPEN on the input and output files
*  - calls ZVGET on the input file to get information about the VICAR image 
*****************************************************************************/ 
{
   int status ;
   if (invar->write_method == NEW_OUT_FILE) {
       status = zvunit (&invar->inp_unit, "INP", 1, NULL) ;
       abort_on_error (invar->inp_unit) ;
       status = zvunit (&invar->out_unit, "UPDATE", 1, 
               "U_NAME", invar->output_file, NULL) ;

       abort_on_error (invar->out_unit) ;
       status = zvopen(invar->inp_unit, "U_ORG", "BSQ", "TYPE", "IMAGE", NULL) ;
       status = zvopen(invar->out_unit, "U_ORG", "BSQ", "OP", "WRITE", 
              "TYPE", "IMAGE", NULL) ;
   }
   else if (invar->write_method == OUT_SAME_AS_INP) {
       status = zvunit (&invar->out_unit, "INP", 1, NULL) ;
       abort_on_error (invar->out_unit) ;
       status = zvopen(invar->out_unit, "U_ORG", "BSQ", "OP", "UPDATE", 
              "TYPE", "IMAGE", NULL) ;
       invar->inp_unit = invar->out_unit ;
   }
   if (status != SUCCESS)
       message ("Illegal file type", "SF-F-BADFTYPE", TRUE) ;

   return ;
}


void cclose_file (unit_number)
int unit_number;
/******************************************************************************
*  This routine closes a file from I/O processing. Upon closing, the unit
*  number is freed for future use.
******************************************************************************/
{
   (void) zvclose (unit_number, "CLOS_ACT", "FREE", NULL) ;
   return ;
}  
   
void get_parameters (invar)
   struct INVARIANT *invar;
/******************************************************************************
*  This routine returns the parameter values specified in the VICAR procedure
*  definition file CLEANLABEL.PDF.  The routine also determines which modes to
*  use, from the values of the output filename OUT, the keyword MODE and the
*  parameter TASK_NAMES.
******************************************************************************/
{
   int count, def ;
   char mode[10] ;

   (void) zvp ("INP", invar->input_file, &count) ;
   (void) zvp ("MODE", mode, &count) ;
   if ((strcmp(mode, "keeplast")==0) || (strcmp(mode,"KEEPLAST")==0)) { 
       invar->key_to_keep = LAST ; 
   }
   else invar->key_to_keep = FIRST ;
   (void) zvparm ("OUT", invar->output_file, &count, &def, 1, 0) ;
   if (count == 0) {
       invar->write_method = OUT_SAME_AS_INP ;
/***************************************************************************** 
* Note that the input filename is assigned to the output filename.  In 
* open_files, the input and output files' unit number is **out_unit**.
* This is done so that (in process_header) the file associated with out_unit
* is always closed.  
* Beware: inp_unit gets assigned to out_unit to make calls to zlninfo simpler.
******************************************************************************/
       strcpy(invar->output_file, invar->input_file) ;
   }
   else invar->write_method = NEW_OUT_FILE ;
   (void) zvpcnt("KEYWORDS", &invar->num_keys);  
   if (invar->num_keys == 0) invar->keys_to_clean = ALL ;
   else {	
       invar->keys_to_clean = SOME ;
       /* Obtain the keywords. */
       (void) zvparm("KEYWORDS", invar->keys, &invar->num_keys, &def,
			MAX_KEYS, MAX_LABEL_KEY_SIZE+1);
   }

   return;
} 


void make_list (invar)
   struct INVARIANT *invar;
/****************************************************************************
*  Find the keywords and place them in the array invar->keys
****************************************************************************/
{
    int keycnt, i, len, nelem, status ;
    BOOL task_header, in_array ;
    char one_key[MAX_LABEL_KEY_SIZE+1];
    char format[12];

    zveaction ("", "") ;
    keycnt = 0 ;

/* 
*  Set label pointer to the beginning of the history label. 
*/
    strcpy (one_key, "TASK") ;
    status = zlinfo (invar->inp_unit, "HISTORY", one_key,
	    format, &len, &nelem,
	    "HIST", invar->task_names[0],
	    "INSTANCE", invar->instances[0], NULL ) ;
    if (status != SUCCESS) {
        (void) sprintf(msg,"%d\0",status);
        message ("Error status number", msg, FALSE) ;
        abort_on_error (invar->inp_unit) ;
    }

/*
*  Get the next label item.  Check that the label item is not part of a task 
*  header.  If it isn't part of the header, check that the label item is not 
*  already in the array invar->keys.  If the label item is not already in array
*  then put it there. 
*/
    do {
        status = zlninfo (invar->inp_unit, one_key, format, &len, &nelem, NULL) ;
	if ((status != END_OF_LABEL) && (status != SUCCESS)) 
	    abort_on_error (invar->inp_unit) ;

	task_header = (strcmp(one_key, "TASK") == 0) ||
	              (strcmp(one_key, "USER") == 0) ||
	              (strcmp(one_key, "DAT_TIM") == 0) ;
        if (task_header == TRUE) continue ;
        in_array = FALSE;
        for (i = 0; i < keycnt; i++) {
            in_array = (strcmp(one_key, invar->keys[i]) == 0) ;
	    if (in_array) break ;
	}

	if (in_array == TRUE) continue ;
	else (void) strcpy (invar->keys [keycnt++], one_key) ;
     } while (status != END_OF_LABEL) ;

    invar->num_keys = keycnt - 1 ;
    return ;
}


void process_header (invar)
   struct INVARIANT *invar;
/******************************************************************************
*  Open the label and clean duplicated label items.
******************************************************************************/
{
    int status, item ;
    char aline [80] ;

    open_files (invar) ;

/* 
*  Call ZLHINFO to fill the arrays task_names & instances and get the total 
*  number of tasks.  Trap to make sure that at least one task name is found.
*/
    invar->num_tasks = MAX_TASKS ; 
    status = zlhinfo (invar->inp_unit, (char *)invar->task_names,
	     invar->instances,
             &invar->num_tasks, "ULEN", MAX_LABEL_KEY_SIZE+1, NULL) ;
    abort_on_error (invar->inp_unit) ;
    if (invar->num_tasks < 1)  message ("Error: NO_TASKS_IN_LABEL", "", TRUE) ;
    (void) sprintf (aline, "Beginning to clean %s...", invar->input_file) ;
    message (aline, "", FALSE) ;
/* 
*  Clean the instances of keywords:
*  If ALL the keywords are to be cleaned, 
*     make an array of keywords to search through.
*  If SOME keywords to be cleaned are specified in the PDF,
*     nothing special
*  loop through the array of keywords and clean the instances of each.
*/

    if (invar->keys_to_clean == ALL) make_list (invar) ;
    for (item = 0; item < invar->num_keys; item++) {
        delete_instances (invar, invar->keys [item]) ;
    }       
    message ("Cleaning completed successfully!!!", "", FALSE) ;
/* 
*  If needed, copy the input image into the file that contains the cleaned
*  history label. invar.out_unit will contain the image and updated label.
*/
    if (invar->write_method == NEW_OUT_FILE) {
        copy_image (invar->inp_unit, invar->out_unit) ;
    }

/* Close the files.  
*  If there is no new output file, the input file is given the unit number 
*  'out_unit,' so out_unit is always closed.
*
*  If there is a new output file, the input file is given the unit number 
*  'inp_unit' and the output file gets 'out_unit,' so both units need to be
*  closed.
*/
    cclose_file (invar->out_unit) ;
    if (invar->write_method == NEW_OUT_FILE) cclose_file (invar->inp_unit) ;

    return ;
}


void main44()
{
    struct INVARIANT invar ;
    zveaction ("SA", "") ;
    print_program_banner () ;
    get_parameters (&invar) ;
    process_header (&invar) ;
    return;
}


$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create cleanlabel.imake
#define PROGRAM  cleanlabel

#define MODULE_LIST cleanlabel.c

#define MAIN_LANG_C
#define R2LIB 

#define USES_ANSI_C

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
$ Return
$!#############################################################################
$PDF_File:
$ create cleanlabel.pdf
process help=*
!------------------------------------------------------------------------------
! CLEANLABEL.pdf
!------------------------------------------------------------------------------
! Name of VICAR image file whose label is to be cleaned of multiple label items.
parm inp (string,132)

! Name of output VICAR image file.  Default is to replace the label of
! INP with the cleaned label.
parm out (string,132) count=0:1 default=--

! Names of the keywords which are to be cleaned.  Default is to clean all
! keywords (label items).
parm keywords (string,32) count=0:50 default=--

! Keyword indicating whether to purge all tasknames but the most recent 
! instance, or all tasknames but the oldest instance.
parm mode keyword valid=(keeplast,keepfrst) default=keeplast

!# annot function="VICAR Utilities"
!# annot keywords=("duplicate label","history label","keyword","label item")
end-proc
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
.title
Remove duplicate label items from an image's history label
.help
PURPOSE

   This program can be used to remove duplicated label items from the history 
label of any VICAR image.
.page
EXECUTION

The following operational options are available.

Parameters in capitalized letters and words in quotes are to be entered 
literally, while words in braces represent values to be entered by the user.  
Default options are indicated.  The terms "keyword" and "label item" are used 
interchangeably.

.page
Output file options:

   OUT = (null value)  --->default<---
	This option will cause the input file's history label to be cleaned.

   OUT = <filename>
	Create a new file (named by the user) which contains (a) the cleaned 
	history label and (b) a copy of the image in the input file.  The 
	input file label is not changed.

	CLEANLABEL assumes that the filename specification is complete.  The 
	output file will put in your default directory on magnetic disk unless 
	you specify the destination disk and directory.
.page
Keyword options:

   KEYWORDS = (null value) --->default<---
	This option will result in the cleaning of all history label items.

   KEYWORDS = <list of label items>
	Clean only the label items listed.  Up to 50 items can be specified.
	Only the label items should be entered -- do not enter values.  
	E.G. You list an image label and decide to remove the line
	
	SLAT=40.0

	To do this, specify KEYWORDS = SLAT.  The value of 40.0 is removed 
	automatically.

	CLEANLABEL will not clean the task header keywords TASK, USER, and 
	DAT_TIM.  It also will not clean any system header keywords.
.page
Cleaning options:

   MODE = "keeplast" --->default<---
	This option will cause LABELCLEAN to delete all instances it finds
	of each keyword, except the instance which is most recent, in terms
	of the history of the image.

   MODE = "keepfrst"
	In this mode, the oldest instance of each task is kept and all others
	are deleted.
.page
OPERATION

This program assumes that the input Vicar image is in band sequential (BSQ)
format.

.page
HISTORY

	Author : Megan O'Shaughnessy  9-21-1989
	Revised :

	MOS, 10-11-1991
	Fixed FR 70991, dereferenced unit number in calls to XVSIGNAL, in 
	keeping with changes to that subroutine.
        Made portable for UNIX   RNR(CRI)   5-25-94
            For SUN "close_files" proc changed to "cclose_files" to avoid 
            improper linking. 
!------------------------------------------------------------------------------
.level1
.variable inp
Name of input VICAR image.

(If OUT is nulled, INP is
also the output file.)
.variable out
Name of output VICAR image.
.variable keywords
Label items to be cleaned.  
Default is to clean all items.
.variable mode
Indicates mode of deletion:

keeplast 
  Delete all label items except 
  for the most recent instance 
  of each label item.

keepfrst
  Delete all label items except
  for the oldest instance of 
  each label item.
!------------------------------------------------------------------------------
.level2
.variable inp
Contains the filename of the VICAR image whose label you would like to clean.  
If the image is not in your default directory, INP should contain the complete 
file specification.
.variable out
Contains the filename of the VICAR image which will contain the cleaned label.
(Also see general help.)

Defaulting OUT will cause LABELCLEAN to keep the cleaned label in the input
file.

Specifying a filename for OUT has the following results:
	a) The label of the input image will remain unchanged.
	b) The output image file will contain the cleaned label and a copy of
           the input image.
	c) The file will be written to your default directory unless another
	   directory is specified.
.variable keywords
Contains the names of the label items which are to be cleaned.  Default is to
clean all label items.  (Also see general help.)
.variable mode
Keyword indicating how the label items will be cleaned.  (Also see general 
help.)

MODE = KEEPLAST (default)
This option will cause all occurences of label items to be deleted except for 
the most recent occurence of each label item.  

MODE = KEEPFRST
This option will cause all occurences of label items to be deleted except for 
the oldest occurence of each label item.  

The next pages show an example of how each mode works.
.page
---- Task: A --
SLAT = 0.0		For example, assume that the list on the left 
SLON = 30.5		represents an image label.
NL = 1024
NS = 1024		Cleaning this label will affect only SLAT, SLON, and 
---- Task: B --		FORMAT, since these are the only label items which
FORMAT = HALF		are duplicated.
IPL_NA = 140.		
SLAT = 46.		The next page shows the results of cleaning with
SLON = 80.		each mode.
---- Task: C --
FORMAT = HALF
FUNC = "IN1-IN2"
.page
RESULT OF MODE = KEEPLAST	RESULT OF MODE = KEEPFRST

---- Task: A --	 		---- Task: A --
NL = 1024			SLAT = 0.0
NS = 1024			SLON = 30.5
---- Task: B --			NL = 1024
IPL_NA = 140.			NS = 1024
SLAT = 46.			---- Task: B --
SLON = 80.			FORMAT = HALF
---- Task: C --			IPL_NA = 140.
FORMAT = HALF			---- Task: C --
FUNC = "IN1-IN2"		FUNC = "IN1-IN2"
.end


$ Return
$!#############################################################################
$Test_File:
$ create tstcleanlabel.pdf
procedure 
refgbl $autousage
body
let $autousage="none"
!------------------------------------------------------------------------------
!generate test images
!------------------------------------------------------------------------------
gen out=source.img nl=10 ns=10 nb=1 format=byte
label-add inp=source.img task=GEN +
    items = "PLANET='jupiter', SLAT=40., SLON=50."
label-add inp=source.img out=source.img +
    items = "PLANET='saturn', SLAT=10., SLON=20., FARENC='farenc'"
label-add inp=source.img out=source.img +
    items = "PLANET='neptune', FUNC='in1*0', SLAT=45., SLON=70., FORMAT='byte'"

label-l source.img
copy source.img prod5.img
copy source.img prod6.img

!------------------------------------------------------------------------------
! test the program
!------------------------------------------------------------------------------
cleanlabel inp=source.img out=prod1.img keywords=-- mode=keeplast
label-l prod1.img

cleanlabel inp=source.img out=prod2.img keywords=-- mode=keepfrst
label-l prod2.img

cleanlabel inp=source.img out=prod3.img keywords=(PLANET,SLON) mode=keeplast
label-l prod3.img

cleanlabel inp=source.img out=prod4.img keywords=(PLANET,SLON) mode=keepfrst
label-l prod4.img

cleanlabel inp=prod5.img out=-- keywords=-- mode=keeplast
label-l prod5.img

cleanlabel inp=prod6.img out=-- keywords=(PLANET,SLON) mode=keepfrst
label-l prod6.img

!------------------------------------------------------------------------------
! test the keyword error logic
!------------------------------------------------------------------------------
cleanlabel inp=prod6.img out=-- keywords=(FRED, GEORGE) mode=keepfrst
label-list prod6.img
end-proc
$ Return
$!#############################################################################
