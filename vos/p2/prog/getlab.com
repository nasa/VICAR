$!****************************************************************************
$!
$! Build proc for MIPL module getlab
$! VPACK Version 1.9, Monday, December 07, 2009, 16:21:59
$!
$! Execute by entering:		$ @getlab
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
$ write sys$output "*** module getlab ***"
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
$ write sys$output "Invalid argument given to getlab.com file -- ", primary
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
$   if F$SEARCH("getlab.imake") .nes. ""
$   then
$      vimake getlab
$      purge getlab.bld
$   else
$      if F$SEARCH("getlab.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake getlab
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @getlab.bld "STD"
$   else
$      @getlab.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create getlab.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack getlab.com -mixed -
	-s getlab.c -
	-i getlab.imake -
	-p getlab.pdf -
	-t tstgetlab.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create getlab.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "vicmain_c"
#include "taeconf.inp"
#include "parblk.inc"
#include "pgminc.inc"
#include "defines.h"            /* for MAX_TASKS, MAX_LABEL_KEY_SIZE */
#include <string.h>
#include <ctype.h>
 
#define SUCCESS     1
#define PARSIZE     1000
#define MAX_OUT     1000
#define KEY_SIZE    MAX_LABEL_KEY_SIZE

void get_parameters (char *, char *, char *, char *, int *, int *);
void upper_case (char *);
void get_keyword_value (char *, char *, char *, char *, int, char *, int);
void write_parblock (char *, char *);
  
void main44(void) {
   char key[KEY_SIZE+1], task[KEY_SIZE+1];
   char location[15], type[15];
   char output[MAX_OUT];
   int instance;
   int element;
 
   /* retrive user supplied parameters */ 
   get_parameters(key,location,type,task,&instance,&element);

   /* process user input keyword value */
   get_keyword_value(key,location,type,task,instance,output, element);

   /* write retrived value to the output parameter based on the data type */
   write_parblock(type,output);
 
   return;
}


/* this subroutine will process user inputs, by retriving input values
   and convert string values into upper case characters.  */
void get_parameters (char *key, char *loc, char *type, char *task,
                     int *instance, int *element) {
 
   int status;
   int count;

   zvp("ELEMENT", element, &count);

   /* retrive label item, convert to upper case and store into 'key' */ 
   zvp("LAB_ITEM",key,&count);
   upper_case(key);

   /* retrive item type, conver to upper case and store into 'type' */
   zvp("ITM_TYPE",type,&count);
   upper_case(type);

   /* test for label type input, ans store the keyword into 'loc' */ 
   if (zvptst("SYSTEM")) {
      strcpy(loc,"SYSTEM");
      return;
   }
   else if (zvptst("PROPERTY")) {
      strcpy(loc,"PROPERTY");
      zvp("ITM_TASK",task,&count);
      upper_case(task);
      return;
   }
   else
      strcpy(loc,"HISTORY");

   /* test for which mode of retrival, (i.e. INSTANCE, LATEST, or EARLIEST), 
      and store the value into 'task' */ 
   if (zvptst("INSTANCE")) {
      strcpy(task,"INSTANCE");
      zvp("ITM_INST",instance,&count);
      zvp("ITM_TASK",task,&count);
      upper_case(task);
   }
   else if (zvptst("LATEST")) 
      strcpy(task,"LATEST");
   else
      strcpy(task,"EARLIEST");
 
   return;
}


/* this subroutine convers input string into upper case characters. */ 
void upper_case (char *s) {
   int i;
 
   for (i = 0; i <= strlen(s); i++)
      s[i] = islower(s[i]) ? toupper(s[i]) : s[i];
   return;
}


/* this subroutine process keyword input, by determining which keyword and 
   retrive the proper value from the label */ 
void get_keyword_value (char *key, char *location, char *type, char *task, 
                        int instance, char *out, int element) {

   int   instances[MAX_TASKS], count, j;
   int   unit, length, form;
   int   status;
   char  tasks[MAX_TASKS][KEY_SIZE+1];
 
   zvunit(&unit,"INP",1, NULL);
   zvopen(unit, "OPEN_ACT", "SA", "IO_ACT", "SA", NULL);
 
   if (!strcmp(location,"SYSTEM"))
   /* retrive value from system label */
      status = zlget(unit,"SYSTEM",key,out, "FORMAT",type, "LENGTH",&length,
		     "ELEMENT", element, "NELEMENT", 1, NULL);

   else if (!strcmp(location,"PROPERTY")) {      
   /* retrive value from property label */
      status = zlget(unit, "PROPERTY", key, out, "FORMAT",type,
                     "LENGTH",&length, "PROPERTY",task,
		     "ELEMENT", element, "NELEMENT", 1, NULL);

   }

   else {
      count = MAX_TASKS;
      zlhinfo(unit, (char *)tasks, instances, &count, "ULEN", KEY_SIZE+1, NULL);
      if (!strcmp(task,"LATEST")) {
      /* retrive value from the latest history label */
         for (j = count-1;  j >= 0; j--) {
            status = zlget(unit, "HISTORY", key, out, "ERR_ACT","",
                           "HIST",tasks[j], "INSTANCE",instances[j],
                           "FORMAT",type, "LENGTH",&length,
			   "ELEMENT", element, "NELEMENT", 1, NULL);
            if (status == SUCCESS) break;
            else if (status == CANNOT_FIND_KEY) continue;
            else zvsignal(unit, status, 1);     /* abort */
         }
      }
      else if (!strcmp(task,"EARLIEST")) {
      /* retrive value from the earliest history label */
         for (j = 0;  j <= count-1; j++) {
            status = zlget(unit, "HISTORY", key, out, "ERR_ACT","",
                           "HIST",tasks[j], "INSTANCE",instances[j],
                           "FORMAT",type, "LENGTH",&length,
			   "ELEMENT", element, "NELEMENT", 1, NULL);
            if (status == SUCCESS) break;
            else if (status == CANNOT_FIND_KEY) continue;
            else zvsignal(unit, status, 1);     /* abort */
         }
      }
      else    /* INSTANCE mode */ {
      /* retrive value from history label based on the user supplied instance */
         status = zlget(unit, "HISTORY", key, out, "FORMAT",type,
                        "INSTANCE",instance, "LENGTH",&length, "HIST",task,
			"ELEMENT", element, "NELEMENT", 1, NULL);
      }
   }
   if (status != SUCCESS) zvsignal(unit, status, 1);    /* abort */
   if (!strcmp(type,"STRING")) out[length]='\0';
 
   return;
}


/* This subroutine assigns the retrived value to the user supplied output 
   parameter based on the supplied data type. */ 
void write_parblock (char *type, char *output) {

   struct PARBLK par_block;
   static char name[9] = "ITM_NAME";
   double doubleout;
 
   if (!strcmp(type,"REAL"))
      doubleout = *(float *)output;
 
   q_init(&par_block, P_BYTES, P_ABORT);
 
   if (!strcmp(type,"STRING")) q_string(&par_block,name,1,&output,    P_ADD);
   if (!strcmp(type,"INT"))    q_intg  (&par_block,name,1, output,    P_ADD);
   if (!strcmp(type,"REAL"))   q_real  (&par_block,name,1,&doubleout, P_ADD);
 
   zvq_out(&par_block);
 
   return;
}         

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create getlab.imake
#define PROGRAM getlab

#define MODULE_LIST getlab.c

#define MAIN_LANG_C
#define USES_C
#define R2LIB

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB

$ Return
$!#############################################################################
$PDF_File:
$ create getlab.pdf
PROCESS HELP=*         !process GETLAB
local dummy string
PARM INP STRING
PARM LAB_ITEM (STRING,32)
PARM ITM_NAME NAME default=dummy
PARM ITM_TYPE (STRING,8)  VALID=("STRING","string","INT","int","REAL","real")
PARM LAB_TYPE KEYWORD     VALID=(SYSTEM,HISTORY,PROPERTY)    DEFAULT=HISTORY
PARM ITM_TASK (STRING,32)                                    DEFAULT=LATEST
PARM ITM_INST INTEGER                                        DEFAULT=200
PARM MODE     KEYWORD     VALID=(EARLIEST, LATEST, INSTANCE) DEFAULT=LATEST
PARM ELEMENT INTEGER DEFAULT=1

!# annot function="VICAR Procedure Generation"
!# annot keywords=("label item","single-valued",intrinsic,"Output parameter",+
!# "TAE variable")
END-PROC
.TITLE
Copies of VICAR label item to a TAE variable
.HELP
PURPOSE: This PDF will get a requested label item from any part
         of a VICAR label and assign it to a local variable. This local
         variable must be previously defined using the intrinsic command:
 
                     VICAR>LOCAL variable_name variable_type
 
          where variable_type must match ITM_TYPE.

        Note: GETLAB only works on single-valued variables. Multi-valued
        variables are not yet supported.  However, the ELEMENT keyword can
        be used to select one element from a multi-valued keyword.
.PAGE
OPERATION:
          There is one mode of "system label" operation:
                SYSTEM
SYSTEM:   This mode retrieves the label item matching LAB_ITEM in the system
          section of the VICAR label
 
          There are three modes of "history label" operation:
                EARLIEST, LATEST, INSTANCE.
 
EARLIEST: This mode retrieves the earliest history label item matching LAB_ITEM.
LATEST:   This mode retrieves the latest history label item matching LAB_ITEM.
INSTANCE: This mode retrieves a specific instance of a history label item.
          When using the INSTANCE mode, the ITM_TASK and ITM_INST parameters
          must be supplied. ITM_TASK specifies the name of the task to
          be searched for the label item, and ITM_INST specifies which
          instance of ITM_TASK should be searched, since the task ITM_TASK
          may appear multiple times in the history label.
.PAGE
EXAMPLES:
 
The following examples use the label listed below.
        ************  File A ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in BYTE format
                1 bands
                10 lines per band
                10 samples per line
---- Task: TASK1 -- User: XXX000 -- Mon Jan 0 00:00:00 0000 ----
IVAL=0.0
SINC=1.0
LINC=1.0
BINC=1.0
MODULO=0.0
TEST1='FIRST TEST STRING'
TEST2=123
TEST3=4.56
---- Task: TASK2 -- User: XXX000 -- Mon Jan 1 00:00:00 0000 ----
TEST1='MIDDLE TEST STRING'
---- Task: TASK3 -- User: XXX000 -- Mon Jan 2 00:00:00 0000 ----
TEST1='LAST TEST STRING'
 
In order to retrieve label values, local variables of the appropriate type
must be declared first:
        LOCAL T1 STRING
        LOCAL T2 INT
        LOCAL T3 REAL
 
To receive SYSTEM label information:
GETLAB A FORMAT T1 STRING 'SYSTEM       !'T1' would equal 'BYTE'
GETLAB A NL T2 INT 'SYSTEM              !'T2' would equal '10'
 
To receive lastest HISTORY label information:
GETLAB A TEST1 T1 STRING                !'T1' should equal 'LAST TEST STRING'
GETLAB A TEST2 T2 INT                   !'T2' should equal '123'
GETLAB A TEST3 T3 REAL                  !'T3' should equal '4.56'
 
To receive earliest HISTORY label information:
GETLAB A TEST1 T1 STRING 'EARLIEST      !'T1' should equal 'FIRST TEST STRING'
 
To receive specific HISTORY label information:
GETLAB A TEST1 T1 STRING TASK2 1 'INSTANCE
                                        !'T1' would equal 'MIDDLE TEST STRING'
 
.PAGE
AUTHOR:   C. C. Meisl
COGNIZANT PROGRAMMER:  C. C. Meisl
Unix port: Bob Deen, 4-92
REVISION:  2
REVISION HISTORY:
 
   18 March 97 ...T.Huang... Ported from VAX to UNIX.  This version has
                             an added keyword parameter "PROPERTY" to handle
                             retrieval of property label info.
      June 2008   rgd        Added ELEMENT keyword

.LEVEL1
.VARIABLE INP
Name of file containing
label to be accessed
.VARIABLE LAB_ITEM
Name of label item to
be accessed
.VARIABLE ITM_NAME
Name of local variable
in which to store value
of label item LAB_ITEM
.VARIABLE ITM_TYPE
Type of label item
to be accessed. ITM_NAME
must be of same type
.VARIABLE LAB_TYPE 
This parameter specifices 
the type of vicar label to 
be retrive from.
.VARIABLE ITM_TASK
Task name for task
specific access of label,
i.e INSTANCE access
.VARIABLE ITM_INST
Instance number of task
for task specific access
of label, i.e. INSTANCE
access
.VARIABLE MODE
Type of label access
.VARIABLE ELEMENT
Which element to pick.
.LEVEL2
.VARIABLE INP
This parameter specifies the name of the file containing the label items
to be retrieved. If the directory is not the default, the file name must
include the directory specification.
.VARIABLE LAB_ITEM
This parameter specifies the name of the label item to be retrieved. The
label will be searched for label items that match this parameter.
.VARIABLE ITM_NAME
This parameter specifies the name of the local variable that will contain
the value(s) of the searched label item LAB_ITEM. The type of the local variable
must be equivalent to the type of the label item. The local variable must
also have a count that is either equal to the count of the returned label
item value, or have a count range that will include the count of the label
item value.
.VARIABLE LAB_TYPE
This parameter specifies the type of the value to be retrieved.  The valid 
values are STRING, REAL, or INT.
.VARIABLE ITM_TYPE
This parameter specifies the type of the label item value to be retrieved. This
type much match the type of the label item value and the type of the local
variable in which the label item value will be stored.
.VARIABLE ITM_TASK
This parameter specifies the name of the task that is to be searched for
a label item. This parameter is only used in INSTANCE mode. See examples.
.VARIABLE ITM_INST
This parameter specifies the instance number of the task to be searched
for a label item. This parameter is only used in INSTANCE mode. See examples.
.VARIABLE MODE
This parameter specifies the mode of label item access. If the first version 
of a label item is desired, then MODE is EARLIEST. If the last version of a 
label item is desired (i.e. its most recent value), then MODE is LATEST. If a 
specific version of a label item is desired, then MODE is INSTANCE. If the 
INSTANCE mode is selected, then parameters ITM_TASK and ITM_INST must be 
specified.
.VARIABLE ELEMENT
Specifies which element of a multi-valued label item to pick.  GETLAB
supports retrieving only one value at a time, but that value can be any
element of the keyword.  ELEMENT starts counting at 1.
.END

$ Return
$!#############################################################################
$Test_File:
$ create tstgetlab.pdf
PROCEDURE HELP=*
refgbl $autousage
refgbl $echo
refgbl $syschar
 
BODY

let $autousage="none"
let _onfail="continue"
let $echo="yes"
 
LOCAL T1 STRING
LOCAL T2 INT
LOCAL T3 REAL
LOCAL EXP REAL 
local dir string
 
!create test data (i.e. create test label items)
gen a.img 10 10
label-add a.img ITEMS="TEST1='FIRST TEST STRING' TEST2=123 TEST3=4.56"
f2 a.img a.img func="in1"
label-add a.img ITEMS="TEST1='MIDDLE TEST STRING'"
f2 a.img a.img func="in1"
label-add a.img ITEMS="TEST1='LAST TEST STRING'"
 
!list test label
label-list a.img
 
!test label item retrieval with GETLAB
!test LAB_ITEM=TEST1 ITM_TYPE=STRING MODE=LATEST
getlab inp=a.img lab_item=TEST1 itm_name=T1 itm_type=STRING +
   lab_type=HISTORY mode=LATEST
disp T1
!'T1' should equal 'LAST TEST STRING'
 
!test LAB_ITEM=TEST2 ITM_TYPE=INT MODE=LATEST
getlab inp=a.img lab_item=TEST2 itm_name=T2 itm_type=INT +
   lab_type=HISTORY mode=LATEST
disp T2
!'T2' should equal '123'
!test LAB_ITEM=TEST3 ITM_TYPE=REAL MODE=LATEST
getlab inp=a.img lab_item=TEST3 itm_name=T3 itm_type=REAL +
   lab_type=HISTORY mode=LATEST
disp T3
!'T3' should equal '4.56'
 
!test LAB_ITEM=FORMAT ITM_TYPE=STRING MODE=SYSTEM
getlab inp=a.img lab_item=FORMAT itm_name=T1 itm_type=STRING +
   lab_type=SYSTEM mode=LATEST
disp T1
!'T1' should equal 'BYTE'
 
!test LAB_ITEM=NL ITM_TYPE=INT MODE=SYSTEM
getlab inp=a.img lab_item=NL itm_name=T2 itm_type=INT +
   lab_type=SYSTEM mode=LATEST
disp T2
!'T2' should equal '10'
 
!test LAB_ITEM=TEST1 ITM_TYPE=STRING MODE=EARLIEST
getlab inp=a.img lab_item=TEST1 itm_name=T1 itm_type=STRING +
   lab_type=HISTORY mode=EARLIEST
disp T1
!'T1' should equal 'FIRST TEST STRING'
 
!test LAB_ITEM=TEST1 ITM_TYPE=STRING ITM_TASK=F2 ITM_INST=1 MODE=INSTANCE
getlab inp=a.img lab_item=TEST1 itm_name=T1 itm_type=STRING +
   itm_task=F2 itm_inst=1 lab_type=HISTORY mode=INSTANCE
disp T1
!'T1' should equal 'MIDDLE TEST STRING'

write "Test correct handling of nonexistent label item."  
write "This case should generate and ABEND." 
getlab inp=a.img lab_item=BADTEST itm_name=T1 itm_type=STRING +
   lab_type=HISTORY mode=LATEST
 
!use GETLAB successfully to reset SFI and SKEY flags so previous ABEND does
!not cause test job to report failure.
getlab inp=a.img lab_item=TEST1 itm_name=T1 itm_type=STRING +
   lab_type=HISTORY mode=LATEST
disp T1

! test case for retriving property label
if ($syschar(1)="UNIX")
   let dir = "/project/test_work/testdata/cassini/iss/"
else
   let dir = "wms_test_work:[testdata.cassini.iss]"
end-if

!Set dns to 10 and replicate - set exposure to 0
f2 inp=&"dir"sum2.1 out=l1.a func=10
label-rep l1.a 'PROP property="CASSINI-ISS" item="EXPOSURE_DURATION=0"
label-list l1.a

getlab inp=l1.a lab_item="EXPOSURE_DURATION" itm_name=EXP lab_type=PROPERTY +
       itm_type=REAL itm_task="CASSINI-ISS"

disp EXP

! Test multivalued items.  Should report BB
label-add l1.a 'prop property="CASSINI-ISS" item="MULTIVAL=(AA,BB,CC)"
label-list l1.a

getlab inp=l1.a lab_item="MULTIVAL" itm_name=T1 lab_type=PROPERTY itm_type=STRING itm_task="CASSINI-ISS" element=2
disp T1

if ($syschar(1)="UNIX")
!   ush rm l1.*
   ush rm a.img
else
   dcl del l1.*;*
   dcl del a.img;*
end-if
 
WRITE "TEST OF GETLAB COMPLETE."
 
END-PROC
.title
TSTGETLAB -- Test PDF for GETLAB
.help
 
        Test Procedure:
                1) create test-label
                2) test label retrieval possibilities in GETLAB
                3) test error handling (non-existent label item)
.page

                Validation
 
        After each execution of GETLAB, the returned label value
        is displayed. This value should be compared with the
        expected value. Not all possible combinations of parameters
        to GETLAB are tested, but most expected uses of the PDF are.
.end

$ Return
$!#############################################################################
