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
