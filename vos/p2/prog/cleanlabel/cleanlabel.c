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


