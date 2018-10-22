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
