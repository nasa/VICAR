$!****************************************************************************
$!
$! Build proc for MIPL module insert3d
$! VPACK Version 1.9, Monday, December 07, 2009, 16:32:44
$!
$! Execute by entering:		$ @insert3d
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
$ write sys$output "*** module insert3d ***"
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
$ write sys$output "Invalid argument given to insert3d.com file -- ", primary
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
$   if F$SEARCH("insert3d.imake") .nes. ""
$   then
$      vimake insert3d
$      purge insert3d.bld
$   else
$      if F$SEARCH("insert3d.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake insert3d
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @insert3d.bld "STD"
$   else
$      @insert3d.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create insert3d.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack insert3d.com -mixed -
	-s insert3d.c -
	-i insert3d.imake -
	-p insert3d.pdf -
	-t tstinsert3d.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create insert3d.c
$ DECK/DOLLARS="$ VOKAGLEVE"
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

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create insert3d.imake
#define PROGRAM  insert3d

#define MODULE_LIST insert3d.c

#define MAIN_LANG_C
#define R2LIB 

#define USES_C

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
$ Return
$!#############################################################################
$PDF_File:
$ create insert3d.pdf
process help=*
parm inp string count=2
parm out string count=0:1 default=--
parm band integer count=1
parm overwrit keyword valid=(over,nover) default=nover
parm update keyword valid=(noupdate,update) default=noupdate

!# annot function="VICAR Utilities"
!# annot keywords=("2-d band","3-d file",OVER UPDATE)
end-proc
.title
Insert a band into a 3-d multispectral file
.help
PURPOSE:

INSERT3D will place a 2-d band (also called a depth) into 
a 3d file. The band may be inserted between 2 existing 
bands or written over an existing band. The keyword 
'OVER causes overwriting. Inserting is the default.
The first input file is the 3-D file that will receive
the band and the second input is the band to be inserted. 
The output receives the final product unless 'UPDATE is
specified in which case the band is written into the
primary input, and the output file, if present, is ignored.
.page

Example:    INSERT3D  (A,B) C BAND=23 'OVER

Writes B as band 23 over band 23 in A with C receiving the updated
3-D file. A and B are left unchanged.

The file, A, must be a 3-D file in band-sequential organization and B 
a 2-D band (or depth) with the same cross-section (number of lines
and samples) as A.

The program, TRAN, may be used to change the organization of a file.

Example:    INSERT3D (A,B) BAND=116 'UPDATE

Here, the band in B is written directly into A in update mode.

'UPDATE implies 'OVER. The band count of A can not increse in 'UPDATE
mode.

Note that INSERT3D can insert a band into a file after the previous
last band. The BAND value must be one greater than the last band number.
.page
History

Programmer:	Mike Girard
Written:	February 1990

Revisions:	
                Made portable for UNIX.  Jim Turner (CRI) 13-April-1994
                
.level1
.vari inp
The inserted and the insertee.
.vari out
The final product.
.vari band
Where to insert.
.vari overwrit
Write over instead of inserting between.
.vari update
Write into the primary input.
.level2
.vari inp
The first input is the 3-D file. The second input is 
the 2-D file containing the band to be inserted.
.vari out
Contains the result of the second input being written
into the first. The inputs are always left unchanged 
by this operation unless 'UPDATE is specified in which
case the primary input is written into and the output,
if present, is ignored.
.vari band
The band number of the band to be overwritten or inserted 
behind, depending on the keyword, 'OVER.
.vari overwrit
Will cause the second input to be overwritten on the selected
band of the first input. Inserting the second file behind the
indicated band of the first file is the default. In this case, 
the output file will have a band count greater by one than 
primary input.
.vari update
Will cause the primary input to recieve the band rather than
the output. 'UPDATE implies 'OVER, that is, in 'UPDATE mode
the new band must overwrite its existing counterpart.
.end                                                 
$ Return
$!#############################################################################
$Test_File:
$ create tstinsert3d.pdf
procedure
refgbl $echo
refgbl $autousage
body
let $autousage="none"
let _onfail="continue"
let $echo="yes"
!
! This unit test checks the following insert3d capabilities:
!
!    1. Insert after
!    2. Add to end
!    3. Insert over
!    4. Update in place
!
! Gen a 3-D file, a, and a 2-D file, b.
!
gen a 3 3 3
!
gen b 3 3 ival=33 sinc=0 linc=0
!
! Now use insert3d to insert a band after band 2 in 'a'.
! The result will go in 'c'.
!
insert3d (a,b) c band=2
!
! Use label-list to show that the number of bands
! in 'c' has increased by 1.
! Also, the program, list, will show the result.
!
label-l c
!
list c
!
!
! Now add a band at the end and list label and contents
! to verify.
!
insert3d (a,b) c band=4
label-l c
list c
!
! Now overwrite band 2.
!
insert3d (a,b) c band=2 'over
label-l c
list c
!
! Verify the update in place mode.
!
insert3d (a,b) band=1 'update
label-l a
list a
!
end-proc
$ Return
$!#############################################################################
