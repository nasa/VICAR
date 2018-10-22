#include "vicmain_c"
#include <zvproto.h>
#include <stdlib.h>

#define FLAG int
void open_the_files (int* in1, int* in2, int* out, int* nl, int *ns, int* nb, 
		     FLAG overwrite, FLAG update);
void close_the_files (int in1, int in2, int out, FLAG update );
void error (char* msg);
void copy_bands (int in,int out,int sb,int eb,int tob,int nl,int ns );
void insert_the_band (int inunit1, int inunit2, int outunit, int band, 
		      int nl, int ns, int nb, FLAG overwrite, FLAG update );
void process_params(int* band, FLAG* overwrite, FLAG* update );

void main44 (void )
{


int inunit1,
    inunit2,
    outunit,
    nl,
    ns,
    nb,
    band,
    overwrite,
    update;


   /* Display start-up message */
   zifmessage ("INSERT3D version 2-May-1994");

   /* Initialize all variable declarations to 0 */ 
   inunit1   = 0;
   inunit2   = 0;
   outunit   = 0;
   nl        = 0;
   ns        = 0;
   nb        = 0;
   band      = 0;
   update    = 0;
   overwrite = 0;

   /* Get the band, update and overwrite parameters. Overwrite means 
   overwrite the band instead of inserting behind it. */ 

   process_params( &band, &overwrite, &update ) ;

   /* Open the two input files and the one output file, retrieving 
   the file units as well as the dimensions of the first file.*/ 

   open_the_files ( &inunit1, &inunit2, &outunit, &nl, &ns, &nb, 
                    overwrite, update ) ;

   /* Insert or overwrite the band from the second input into the first with
   result into the output. */ 

   insert_the_band ( inunit1, inunit2, outunit, band, 
                     nl, ns, nb, overwrite, update ) ;

   close_the_files ( inunit1, inunit2, outunit, update ) ;

   return;   
}

void close_the_files (int in1, int in2, int out, FLAG update )
{

   zvclose ( in1 , NULL) ;
   zvclose ( in2 , NULL) ;
   if ( !update) zvclose ( out, NULL ) ;

   return ;
}


void copy_bands (int in,int out,int sb,int eb,int tob,int nl,int ns )
{

   int band, line, NS;
   /* jaw - changed from inline to in_line for linux port */
   char *in_line ;
  
   if ( sb > eb ) 
       return ;

   NS = ns;
   in_line = (char *) malloc (NS * 4) ;

   for ( band = sb; band <= eb; ++band, ++tob )

      for ( line=1 ; line<=nl; ++line )
      {
         zvread ( in, in_line, "BAND", band, "LINE", line , NULL) ;
         zvwrit ( out,in_line, "BAND", tob, "LINE", line , NULL) ;
      }

   free ( in_line ) ;

   return ;                               
}


void error (char* msg)
{
    
   zvmessage ( msg , "") ;
   zabend() ;
   return ;
}


void insert_the_band (int inunit1, int inunit2, int outunit, int band, 
                 int nl, int ns, int nb, FLAG overwrite, FLAG update ) 
{
   int last_band_before_insert ;

   last_band_before_insert = ( overwrite || update) ? band-1 : band ;

   if ( last_band_before_insert > nb ) last_band_before_insert = nb ;

   if ( update ) {
      copy_bands (inunit2, inunit1, 1 /* Start Band */, 
                  1 /* End Band */, band /* To band */, nl, ns ) ;
   } else {
      if ( last_band_before_insert > 0 ) {
          copy_bands (inunit1, outunit, 1  /* Start Band */, 
                      last_band_before_insert /* End Band */ ,
                      1 /* To Band */, nl, ns ) ;
      }
      copy_bands (inunit2, outunit, 1, 1, last_band_before_insert+1, nl, ns ) ;
                                                            
      if ( band+1 <= nb ) {
         copy_bands (inunit1, outunit, band+1, nb, last_band_before_insert+2,
                     nl, ns ) ;
      }
   }

   return ;
}                            



void open_the_files (int* in1, int* in2, int* out, int* nl, int *ns, int* nb, 
		     FLAG overwrite, FLAG update)
{
                             
   int nl2, ns2, nb2, nbands ;

   zvunit ( in1, "INP", 1, NULL) ;
   zvunit ( in2, "INP", 2, NULL) ;

   if ( !update ) {
      zvunit ( out, "OUT", 1, NULL) ;
   }

   if ( update ) {
      zvopen ( *in1, "OPEN_ACT", "SAU", "U_ORG", "BSQ", "IO_ACT", "SAU",
               "OP", "UPDATE" , NULL) ;
   } else {
      zvopen ( *in1, "OPEN_ACT", "SA", "U_ORG", "BSQ", "IO_ACT", "SA" , NULL) ;
   }

   zvopen ( *in2, "OPEN_ACT", "SA", "U_ORG", "BSQ", "IO_ACT", "SA" , NULL) ;

   zvget ( *in1, "NL", nl, "NS", ns, "NB", nb , NULL) ;   
   zvget ( *in2, "NL", &nl2, "NS", &ns2, "NB", &nb2 , NULL) ;   

   if ( ( *nl != nl2 ) || ( *ns != ns2 ) )
      error ( "The second input must be the same size as the output.");

   if ( nb2 != 1 ) 
      error ( "The second input must have depth equal to 1");

   nbands = overwrite ? *nb : *nb + 1 ;

   if ( !update ) {
       zvopen ( *out, "OP", "WRITE", "U_ORG", "BSQ",
                "OPEN_ACT", "SA", "IO_ACT", "SA", "U_NB", nbands, NULL);
   }

   return ;
}


void process_params(int* band, FLAG* overwrite, FLAG* update ) 
{
   int cnt;

   zvp ( "BAND", band, &cnt ) ;

   *overwrite = zvptst ( "OVER" ) ;

   *update = zvptst ( "UPDATE" ) ;
   
   return ;
}

