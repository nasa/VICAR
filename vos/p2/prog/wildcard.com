$!****************************************************************************
$!
$! Build proc for MIPL module wildcard
$! VPACK Version 1.9, Wednesday, May 23, 2012, 17:03:51
$!
$! Execute by entering:		$ @wildcard
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
$ write sys$output "*** module wildcard ***"
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
$ write sys$output "Invalid argument given to wildcard.com file -- ", primary
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
$   if F$SEARCH("wildcard.imake") .nes. ""
$   then
$      vimake wildcard
$      purge wildcard.bld
$   else
$      if F$SEARCH("wildcard.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake wildcard
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @wildcard.bld "STD"
$   else
$      @wildcard.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create wildcard.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack wildcard.com -mixed -
	-s wildcard_vms.c wildcard_unix.c wildcard.ush -
	-i wildcard.imake -
	-p wildcard.pdf wildcard_vms.pdf wildcard_unix.pdf -
	-t tstwildcard.pdf tstwildcard.log_solos
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create wildcard_vms.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/*********************************  Wildcard_vms   ****************************
 * WILDCARD_VMS  (Search for file names and add to local variable)
 *
 *  REVISION HISTORY
 *    9-94 CRI MSTP S/W Conversion (VICAR Porting) 
 ******************************************************************************/
#include "vicmain_c"
#include rms
#include "taeconf.inp"
#include "pgminc.inc"
#include "parblk.inc"
#include "defines.h"
#include <descrip.h>
#include <string.h>

/*** Routine definitions      ***/

static void collect_file_names ();
static void list_names ();
static void write_parblock ();
static void * get_space ();
static void find_default_extension ();
static void search_for_names ();
static void update_string ();

/*** Define constants        ***/ 

#define SUCCESS 1
#define MAX_NAMES 512
#define PARSIZE 5000
#define MAX_CHAR 4096

main44()
{
    char *name_list[MAX_NAMES] ;
    int count;

    count = 0 ;

    zvmessage ("WILDCARD version 05-SEP-94",""); 

    collect_file_names (name_list, &count) ;            /* Collect names */

/*
    list_names (name_list, count) ;  
*/
 
    write_parblock ( name_list, count );

    return ;
}

static void collect_file_names (name_list, count)
  char *name_list[] ;
  int *count;
{                                    
    int limit, input_cnt, input, def, nbytes, i, status ;
    char input_string[4096], *current, *newf ;
    char ls_string[4099] = "ls ";

    for (i=0; i<4096; i++) input_string[i] = 0 ;

    zvparm ("NAME_LMT", &limit, &input_cnt, &def, 1, 0) ;
    if (limit > MAX_NAMES) limit = MAX_NAMES ;

    current = input_string ;
                                                               
    zvparm ("INP", input_string, &input_cnt, &def, 1, 0) ;
    if (!input_cnt) input_cnt = 1 ;                /* value is PDF default*/

    for (input=0; input < input_cnt && *count < limit; input++) 
      {
        search_for_names (current, name_list, count) ;
        current += strlen (current) + 1 ;
      }

    if (*count > limit) *count = limit ;
 
    return ;                                                                 
}


 /* create default file extension based on  */
static void find_default_extension (name) 
  char *name ;        /* process id, i.e. .Zxx where xx are the  */
{                              /* last two hex chars of the process id.   */
    int i, size ;
    static $DESCRIPTOR(log,PROCESS_ID_LOGICAL) ;
    static $DESCRIPTOR(out,"                ") ;

    strcpy (name, ".z") ;

    i = sys$trnlog(&log,&size,&out,  /* Obtain the last two hex chars  */
                   0,0,0) ;          /* of the process id              */
                                     /* from a logical defined outside */
    if (i != SUCCESS) return ;       /* of vicar2;                     */

    size = size & 0x0000ffff ;
    strncat (name,                   /* Use the process id chars to    */
            out.dsc$a_pointer,       /* make the default file name     */
            size) ;                  /* unique.                        */

    return ;
}
 /* will return in name_list[] */
static void search_for_names (name, name_list, count)
  char *name, *name_list[] ;         /* all files based on name, a */
  int *count ; 				      /* wild-carded file spec.     */
{
    struct FAB fab = cc$rms_fab ;   /* initialize file access block */
    struct NAM nam = cc$rms_nam ;   /* initialize name block */
    int length, rms_stat ;
    char def_file_spec[8] ;

    find_default_extension (def_file_spec) ;

    /* file access block */
    fab.fab$l_dna = def_file_spec ;          /* default file spec string & */
    fab.fab$b_dns = strlen (def_file_spec) ; /* def. file spec string size */
    fab.fab$l_fna = name ;                   /* file spec string &         */
    fab.fab$b_fns = strlen (name) ;          /* file spec string size      */
#if 0
    fab.fab$l_fop = FAB$M_OFP ;              /* file processing options    */
#endif
    fab.fab$l_nam = &nam ;                   /* name block &               */

    /* name block */
    nam.nam$l_esa = get_space (NAM$C_MAXRSS);/* expanded string area &     */
    nam.nam$b_ess = NAM$C_MAXRSS ;           /* expanded string area size  */
    nam.nam$l_rsa = get_space (NAM$C_MAXRSS);/* resultant string area &    */
    nam.nam$b_rss = NAM$C_MAXRSS ;           /* resultant string area size */

    rms_stat = sys$parse (&fab) ;            /* initialize for searching   */
    if (rms_stat != RMS$_NORMAL) lib$signal (rms_stat) ;
                                                                             
    /* directory search for files matching NAM specs */
    while (rms_stat == RMS$_NORMAL && *count < MAX_NAMES)
      {    
        rms_stat = sys$search (&fab) ;
        if (rms_stat != RMS$_NORMAL) break ;

        length = nam.nam$b_rsl - nam.nam$b_node ;
        name_list[*count] = get_space (length + 1) ;
        strncpy (name_list[*count], nam.nam$l_dev, length) ;
        name_list[(*count)++][length] = 0 ;
      }

    if (rms_stat == RMS$_FNF)
        zvmessage ("No files found","") ;
    else if (rms_stat != RMS$_NMF && rms_stat != RMS$_NORMAL) 
        lib$signal (rms_stat) ;

    return ;
}

  /* lists file names in name_list array */
static void list_names (name_list, count)
  char *name_list[] ;
  int count ;
{
    int i ;
    char mess[80] ;

    for (i=0; i<count; i++)
      {
        sprintf (mess, "File %3d: {%s}", i, name_list[i]) ;
        zvmessage (mess,"") ;
      }

    return ;
}
                                       
static void write_parblock ( output, count )
   char *output[];
   int count;
{       
   struct PARBLK par_block;
   char name_keyword[] = "LOCALVAR";

   q_init ( &par_block, P_BYTES, P_ABORT );         
   update_string  (output, count, &par_block, name_keyword);
   zvq_out ( &par_block );

   return;
}         

static void update_string ( string_var, count, par_block, name )
   char *string_var[], *name;
   int  *par_block;
   int  count;
{
   int status;

   status = q_string ( par_block, name, count, string_var, P_ADD );
   return;
}

static void * get_space (size)
int size;
{                                   /* Allocate the requested space from */
    char *buffer;                   /* virtual memory and zero it out.   */

    buffer = 0;
    if (!size) return buffer ;
    buffer = calloc (1,size) ;
    if (!buffer) 
      {
        perror ("Unable to allocate working space") ;
      }

    return buffer ;
}            
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create wildcard_unix.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/*********************************  Wildcard_unix   ***************************
 * WILDCARD_UNIX  (Add file names to local variable)
 *
 *  REVISION HISTORY
 *    9-94 CRI MSTP S/W Conversion (VICAR Porting) 
 ******************************************************************************/
#include <stdio.h>
#include "vicmain_c"
#include "taeconf.inp"
#include "parblk.inc"
#include <string.h>

/*** Routine definitions      ***/

static void collect_file_names ();
static void list_names ();
static void write_parblock ();
static void * get_space ();
static void update_string ();

/*** Define constants        ***/ 

#define SUCCESS 1
#define MAX_NAMES 512
#define PARSIZE 5000
#define LOCVAR "LOCALVAR"

void main44(void )
{
    char *name_list[MAX_NAMES] ;
    int count;

    count = 0 ;

    zvmessage ("WILDCARD version 05-SEP-94",""); 

    collect_file_names (name_list, &count) ;            /* Collect names */

/*
    list_names (name_list, count) ;  
*/

    if (count == 0) {
      zvmessage("Wildcard -- No Files Found","");
      return;
    }
 
    write_parblock ( name_list, count );

    return ;
}

static void collect_file_names (name_list, count)
  char *name_list[] ;
  int *count;
{                                    
    int input_cnt, def, status, length ;
    char filename[133] ;
    char  mess[80];
    FILE *fileptr;
    char file_in[4096];

    zvparm ("INP", filename, &input_cnt, &def, 1, 0) ;
/*
    sprintf (mess,"file %s, name_lim=%d, in_cnt=%d", filename,limit,input_cnt);
        zvmessage (mess,"");
*/
    fileptr = fopen (filename, "r");
    if (!fileptr) {
       sprintf (mess,"Error opening file %s", filename);
       zvmessage (mess,"");
       zabend();
    }

    file_in[0] = 0;
    while ( !feof (fileptr)) {
       fgets (file_in,4095,fileptr);
       if (file_in[0] == 0) continue;
       length = strlen (file_in);
       name_list[*count] = (char *) get_space(length + 1);
       strncpy (name_list[*count],file_in, length); 
       name_list[(*count)++][length - 1] = 0;
       file_in[0] = 0;
    }
    fclose (fileptr);
                                                   
    return ;                                                                 
}

  /* lists file names in name_list array */
static void list_names (name_list, count)
  char *name_list[] ;
  int count ;
{
    int i ;
    char mess[80] ;
    for (i=0; i<count; i++)
      {
        (void) sprintf (mess, "File %3d: {%s}", i, name_list[i]) ;
        zvmessage (mess,"") ;
      }

    return ;
}
                                       
static void write_parblock ( output, count )
   char *output[];
   int count;
{       
   struct PARBLK par_block;
   char name_keyword[9];
   strcpy (name_keyword,LOCVAR); 
   q_init (&par_block, P_BYTES, P_ABORT);
   update_string (output, count, (int *)&par_block, name_keyword);
   zvq_out ( &par_block );

   return;
}         

static void update_string ( string_var, count, par_block, name )
   char *string_var[], *name;
   int  *par_block;
   int  count;
{
   int status;
   status = q_string (par_block, name, count, string_var, P_ADD );
   return;
}

static void * get_space (size)
int size;
{                                   /* Allocate the requested space from */
    char *buffer;                   /* virtual memory and zero it out.   */

    buffer = NULL;
    if (!size) return buffer ;
    buffer = (char *) calloc (1,size) ;
    if (!buffer) 
      {
        perror ("Unable to allocate working space") ;
      }
    return buffer ;
}            
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create wildcard.ush
$ DECK/DOLLARS="$ VOKAGLEVE"
:
WILDCARDED_NAME=$1
TEMP_FILE_NAME=$2
MAX_FILE_NAMES_TO_FIND=$3
SEARCH_DIR=$4
#echo "1= "$1"  2= " $2"  3 = " $3" 4 = "$4"  0= " $0 "  arg" $# 

#check number of parameters input - UNIX file name expansion adds parameters
if [ $# != 4 ]
  then echo "Usage: parameters must be inp='xxx' localvar=yyy [name_lim=n] [directory='dd']" 1>&2
  exit 0
fi

#get this user
pwd > pwdf

#use "find" to search the specified directories and all subordinates for
#the wildcarded name and put those found in a temp file

find $SEARCH_DIR -name "$WILDCARDED_NAME" -type f -print > tmpfile.nawk

gawk ' BEGIN { own = "./" ; parent = "../" ;
       #print "own= "own "  parent= "parent;
       COUNT_OF_FILES = ARGV[2] ;
       if ( COUNT_OF_FILES == "" ) COUNT_OF_FILES = 512 ;
       getline pwdx < "pwdf" ; pwdx = pwdx "/" ; ARGV[2] = "" ; 
       numc = split (pwdx, pwd_array, "/");
       for (i = 2; i < numc - 1; i++) { 
         pwd_parent = pwd_parent pwd_array[i] "/" ;
       # print pwd_parent;
       }
       pwd_parent = "/" pwd_parent ;
       #print "pwdx= "pwdx  "  [2]=  "pwd_array[2] " count el= "numc ;
       }
       {
         if (index ($0,own) == 1)
            sub (own, pwdx) 
         else
            if (index ($0,parent) == 1)
                sub (parent, pwd_parent);
         if (NR <= COUNT_OF_FILES ) 
            print $1
         else
            exit}
        ' tmpfile.nawk > $TEMP_FILE_NAME $MAX_FILE_NAMES_TO_FIND 
rm tmpfile.nawk
exit 0

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create wildcard.imake
#if VMS_OS
#define PROGRAM wildcard_vms
#define MODULE_LIST wildcard_vms.c 
#define CLEAN_OTHER_LIST wildcard_unix.c wildcard.ush
#else
#define PROGRAM wildcard_unix
#define MODULE_LIST wildcard_unix.c
#define CLEAN_OTHER_LIST wildcard_vms.c
#endif

#define MAIN_LANG_C
#define R2LIB

#define USES_C

#define LIB_RTL
#define LIB_TAE

$ Return
$!#############################################################################
$PDF_File:
$ create wildcard.pdf
PROCEDURE HELP=*
PARM INP STRING
PARM LOCALVAR NAME
PARM NAME_LMT INTEGER DEFAULT=512
PARM DIRECTORY STRING DEFAULT="."
refgbl $syschar
refgbl $echo
refgbl $autousage
body
let $echo="no"
let $autousage="none"
if ($syschar(1) = "VAX_VMS")
  wildcard_vms inp=&INP localvar=@LOCALVAR name_lmt=&NAME_LMT
else
  let _ONFAIL="GOTO ERROR"
  ush cp $R2LIB/wildcard.ush ./.
  ush chmod +x wildcard.ush
  ush wildcard.ush &INP fout.wildcard &NAME_LMT &DIRECTORY 
  wildcard_unix inp=fout.wildcard localvar=@LOCALVAR 
  ush rm fout.wildcard
  GOTO DONE
  ERROR>WRITE "Wildcard -- Re-enter command line"
  DONE>
  ush rm wildcard.ush
end-if

!# annot function="VICAR Procedure Generation"
!# annot keywords=(INP,default,string,LOCALVAR)

END-PROC
.TITLE
Find all files matching a wildcarded string
.HELP
PURPOSE

This program will find all files in the specified directory (default is current
directory) that match the wildcarded string INP. INP can have multiple
wildcards (each specified by an *). A limit on the returned files can be given
with NAME_LMT (it defaults to 512). The files that match INP will be returned
in the local variable specified by LOCALVAR. LOCALVAR must be previously
defined with a count sufficient to hold all the files. 
.if UNIX
The directory specified and all subordinate directories will be queried.
.ifend
 
.page
EXECUTION

.if VAX_VMS
VICAR>WILDCARD wildcard_input_string local_variable maximum_#_files
.elseif UNIX
VICAR>WILDCARD wildcard_input_string local_variable maximum_#_files directory 
.ifend
EXAMPLES
        If the default directory contains the files,
		TEST01.IMG
		TEST02.IMG
		TEST03.IMG

	Then the following use of WILDCARD will return the files into local
	storage:

		VICAR>local FILES STRING COUNT=0:3
.if VAX_VMS
		VICAR>wildcard INP=TEST*.IMG LOCALVAR=FILES NAM=3
.elseif UNIX
                VICAR>wildcard inp="TEST*.IMG" loc=FILES nam=3 dir="."

                NOTE: if more than one file can be found the -  inp="xx"
                      must be specified because of the UNIX name expansion.     
.ifend

	Of course, any wildcard specification can be used that will uniquely
	find only those files desired.

.page
HISTORY

author: K. K. Anderson & C. C. Meisl, 15-FEB-1988
updated: C. C. Meisl, 15-MAR-1988      added wildcard version number
         RNR (CRI) 5-Sept-1994 MSTP S/W Conversion (VICAR Porting)
            Added proceedure for UNIX FIND, NAWK, and "C" to queue
            to a local variable.
cognizant programmer:  C. C. Meisl

.level1
.variable inp
Wildcarded string to
find files. 
.variable localvar
Local variable to hold
files found.
.variable name_lmt
Maximum number of files
to find.
.variable directory
Directory to start search
from.
.if UNIX
.elseif VAX_VMS
(This parameter not suported
under VMS.)
.ifend
.level2
.variable inp
Wildcarded string, where the wildcard is one or more asterics. The form
.if VAX_VMS
is WILD* *WILD* *WILD*.* *WILD*.*;*.
.elseif UNIX
is inp="WILD*" inp="*WILD*" inp="*WILD*.*".  If more than one file could be
found, the inp="---" form must be used.
.ifend
.variable localvar
Localvar must be a previously defined local variable capable of containing 
all of the files found by the wildcarded string. 
.variable name_lmt
Name_lmt will limit the files found.  The default is 512.
.variable directory
.if UNIX
Directory defaults to the current directory ".".  to specifiy another 
directory to start the search from specify dir=/tmp/home/said.  In this case 
the said directory will be the starting directory for the search and all
subordinate directories of /tmp/home/said will be searched. 
.elseif VAX_VMS
(This parameter not supported
under VMS.)
.ifend
.end
$!-----------------------------------------------------------------------------
$ create wildcard_vms.pdf
PROCESS 
PARM INP STRING
PARM LOCALVAR NAME
PARM NAME_LMT INTEGER DEFAULT=512
END-PROC
$!-----------------------------------------------------------------------------
$ create wildcard_unix.pdf
PROCESS 
PARM INP STRING
PARM LOCALVAR NAME
END-PROC
$ Return
$!#############################################################################
$Test_File:
$ create tstwildcard.pdf
procedure help=*

PARM PCA     KEYWORD VALID=(PCA,NOPCA) DEFAULT=NOPCA

refgbl $syschar
refgbl $echo
refgbl $autousage
body      
let _onfail="continue"
let $echo="yes"
let $autousage="none"

local FILES STRING COUNT=0:100

!make sure debug is off
flag-del debug

!create test files
gen TEST01.IMG 5 5

if ($syschar(1) = "VAX_VMS")
  dcl copy TEST01.IMG TEST02.IMG
  dcl copy TEST01.IMG TEST03.IMG 
  dcl copy TEST01.IMG TEST03.IMG
  dcl create/dir [.tstwildcard]
  dcl copy TEST01.IMG [.tstwildcard]TEST01.IMG
  dcl copy TEST01.IMG [.tstwildcard]TEST02.IMG
else
  ush cp TEST01.IMG TEST02.IMG
  ush cp TEST01.IMG TEST03.IMG
  ush mkdir tstwildcard
  ush cp TEST01.IMG tstwildcard
  ush cp TEST02.IMG tstwildcard 
end-if

if (PCA="PCA") flag-add debug

if ($syschar(1) = "VAX_VMS")
  wildcard TEST*.IMG FILES
  disp FILES
!'FILES' should include:
!      [dir]TEST01.IMG;1, [dir]TEST02.IMG;1, [dir]TEST03.IMG;2
!where [dir] is the current default directory

  wildcard TEST03.* FILES
  disp FILES
!'FILES' should include:
!   [dir]TEST03.IMG;2 
!where [dir] is the current default directory

  wildcard TEST03.*;* FILES
  disp FILES
!'FILES' should include:
!   [dir]TEST03.IMG;2, [dir]TEST03.IMG;1
!where [dir] is the current default directory

  wildcard [.*wildcard]*.*;* FILES
  disp FILES
!'FILES' should include:
!   [dir.tstwildcard]TEST01.IMG;1, [dir.tstwildcard]TEST02.IMG;1
!where [dir] is the current default directory

else
  wildcard inp='TEST*.IMG' loc=FILES n=50
!  wildcard TEST*.IMG FILES
  disp FILES
!'FILES' should include:
!      [dir]TEST01.IMG, [dir]TEST02.IMG, [dir]TEST03.IMG
!where [dir] is the current default directory

  wildcard inp='TEST03.*' loc=FILES
  disp FILES
!'FILES' should include:
!   [dir]TEST03.IMG 
!where [dir] is the current default directory

  wildcard inp='TEST02.*' loc=FILES na=5 di='./tstwildcard'
  disp FILES
!'FILES' should include:
!   [dir]TEST03.IMG
!where [dir] is the current default directory

  wildcard inp='*.*' loc=FILES na=5 di='./*wildcard'
  disp FILES
!'FILES' should include:
!   [dir/tstwildcard/TEST01.IMG, [dir/tstwildcard/TEST02.IMG
!where [dir] is the current default directory
end-if
!***************************
! clean-up
!***************************
flag-del debug

if ($syschar(1) = "VAX_VMS")
  dcl del/log TEST01.IMG;*
  dcl del/log TEST02.IMG;*
  dcl del/log TEST03.IMG;*
  dcl del/log [.tstwildcard]TEST01.IMG;*
  dcl del/log [.tstwildcard]TEST02.IMG;*
  dcl set prot=O:RWED tstwildcard.dir
  dcl del/log tstwildcard.dir;*
else
  ush rm TEST01.IMG
  ush rm TEST02.IMG
  ush rm TEST03.IMG
  ush rm tstwildcard/TEST01.IMG
  ush rm tstwildcard/TEST02.IMG
  ush rmdir tstwildcard
end-if


END-PROC
.title
tstwildcard -- Test PDF for wildcard
.help

	Test procedure:
		1) create test files
		2) use wildcard to find files

	wildcard is used four times to test the use of the wildcard asterisk
	in the four possible fields (or parts of fields):
		directory:*.extension;version
		directory:filename.*;version
		directory:filename.extension;*
		*:*.*;version
        The durectory defaults to the current directory.
	Note that when version is not wildcarded, the most recent version
	(i.e. highest version number) should be returned.
.page

		Validation of Test

	After each execution of wildcard, the contents of variable FILES
	is displayed. Assuming no files existed in the current directory
	with the same names as the test files, FILES should contain those
	filenames specified after each test part.
.page

		PCA option

	This test can be run with PCA assuming wildcard was compiled and
	linked with the PCA option. To run the test with PCA use:
  		VICAR>tstwildcard 'PCA
.level1
.variable pca
Keyword specifying whether
or not to exercise PCA
data collection during test
.end
$!-----------------------------------------------------------------------------
$ create tstwildcard.log_solos
tstwildcard
let $autousage="none"
local FILES STRING COUNT=0:100
flag-del debug
gen TEST01.IMG 5 5
Beginning VICAR task gen
GEN Version 6
GEN task completed
if ($syschar(1) = "VAX_VMS")
else
  ush cp TEST01.IMG TEST02.IMG
  ush cp TEST01.IMG TEST03.IMG
  ush mkdir tstwildcard
  ush cp TEST01.IMG tstwildcard
  ush cp TEST02.IMG tstwildcard
end-if
if (PCA="PCA") flag-add debug
if ($syschar(1) = "VAX_VMS")
else
  wildcard inp='TEST*.IMG' loc=FILES n=50
let $echo="no"
Beginning VICAR task wildcard_unix
WILDCARD version 05-SEP-94

FILES=("/home/lwk/test_solos/TEST01.IMG","/home/lwk/test_solos/TEST02.IMG",
"/home/lwk/test_solos/TEST03.IMG","/home/lwk/test_solos/tstwildcard/TEST01.IMG",
"/home/lwk/test_solos/tstwildcard/TEST02.IMG")

Beginning VICAR task wildcard_unix
WILDCARD version 05-SEP-94

FILES="/home/lwk/test_solos/TEST03.IMG"

Beginning VICAR task wildcard_unix
WILDCARD version 05-SEP-94

FILES="/home/lwk/test_solos/tstwildcard/TEST02.IMG"

Beginning VICAR task wildcard_unix
WILDCARD version 05-SEP-94

FILES=("/home/lwk/test_solos/tstwildcard/TEST01.IMG",
"/home/lwk/test_solos/tstwildcard/TEST02.IMG")

exit
slogoff
$ Return
$!#############################################################################
