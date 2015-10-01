$!****************************************************************************
$!
$! Build proc for MIPL module clabel
$! VPACK Version 1.9, Monday, December 07, 2009, 16:02:05
$!
$! Execute by entering:		$ @clabel
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
$ write sys$output "*** module clabel ***"
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
$ write sys$output "Invalid argument given to clabel.com file -- ", primary
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
$   if F$SEARCH("clabel.imake") .nes. ""
$   then
$      vimake clabel
$      purge clabel.bld
$   else
$      if F$SEARCH("clabel.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake clabel
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @clabel.bld "STD"
$   else
$      @clabel.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create clabel.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack clabel.com -mixed -
	-s clabel.c -
	-i clabel.imake -
	-p clabel.pdf -
	-t tstclabel.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create clabel.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/* 02 JAN 1995 ... CRI ... MSTP S/W CONVERSION (VICAR PORTING) */
#include "vicmain_c"
#include <math.h>
#include <string.h>

#define	TRUE		1
#define	FALSE		0
#define	PI		3.14159265359
#define	ROUTINE
#define	SUCCESS		1

#define	NFONTS		1000
#define	CONTOUR_LENGTH	50000 
#define	BIG_NUMBER	(0x7fff)

#define	CURRENT_SEGMENT	((*segments).current_triplet) 
#define	LAST_SEGMENT	((*segments).end_triplet) 
#define	DIRECTION	((*segments).direction)
#define	FORWARD		1
#define	BACKWARD	-1

#define	TOLERANCE	.0009

typedef struct
{
  float		line ;
  float		samp ;
  float		dn ;
} TRIPLET ;

typedef struct
{
  int		nsamps ;
  int		nlines ;
  TRIPLET	*start_triplet ;
  TRIPLET	*end_triplet ;
  TRIPLET	*current_triplet ;
  TRIPLET	last_nonzero_triplet ;
  TRIPLET	first_nonzero_triplet ;
  int		direction ;
} GRAPHICS1 ;

typedef struct
{
  int		nvals ;
  int		dnval ;
  float		l[ CONTOUR_LENGTH ] ;
  float		s[ CONTOUR_LENGTH ] ;
} CONTOUR ;

typedef struct
{
  int		nfonts ;
  int		font ;
  int		tall ;
  float		wide ;
  int		thick ;
  int		position[ 2*NFONTS ][ 1 ] ;
  char		text[ NFONTS*100 ][ 9 ] ;
  int		dn[ NFONTS ] ;
  int		loc[ NFONTS ] ;
} FONT ;

int	font_height, font_type, ico_long, ico_line, ico_samp, ii, img_nl ;
int	img_ns, jj, min_len, posi_taken[ 449 ][ 3 ] ;

/*globaldef int	unit ;*/
int unit;

	 
void main44(void)
{
  float		trans[ 2 ] ;

  int		ii=0, jj=0, count=0, line=0, samp=0 ;

  GRAPHICS1	segments ;

  CONTOUR	contour ;

  FONT		font ;

  zifmessage ("CLABEL version 02-Jan-1995");
  process_parameters ( trans ) ;

  initialize ( &segments, &contour, &font ) ;

  while ( ( contour.dnval = more_contours ( &segments ) ))
  {
    find_entire_contour ( &contour, &segments ) ;

    generate_font_params ( &contour, &font, trans ) ;

    contour.nvals = 0 ;
  }

  finish ( &font ) ;

  return ;
}



ROUTINE check ( unit, status, message )
                      
  int	unit ;
  int	status ;
  char	*message ;
{
  if (status == SUCCESS) return ;
    
  zvmessage ( message," " ) ;

  zvsignal ( unit, status, TRUE ) ; 

  zabend() ;
}



int ROUTINE eq_triplet ( t1, t2 )
/*****
  Returns TRUE if the two triplets are equal and FALSE otherwise.
 *****/

  TRIPLET t1, t2 ;
{
  int		decision ;
  double	x, y, z ;

  decision =  (((x = fabs( t1.line - t2.line )) < TOLERANCE) &&
               ((y = fabs( t1.samp - t2.samp )) < TOLERANCE) &&
               ((z = fabs( t1.dn - t2.dn ))     < TOLERANCE)) ;

  return decision ;
}



ROUTINE find_entire_contour ( contour, segments )
/*****
  Will follow along the contour until the entire contour has been found.  The
  contour values will be returned in 'contour'.
 *****/

  CONTOUR	*contour ;
  GRAPHICS1	*segments ;
{
  int		i ;
  float		con_line, con_samp ;
  float		max_samp = 0 ;

  TRIPLET	values ;

  (*contour).l[ (*contour).nvals ] = CURRENT_SEGMENT -> line ;
  (*contour).s[ (*contour).nvals ] = CURRENT_SEGMENT -> samp ;

  CURRENT_SEGMENT -> line = 0 ;
  CURRENT_SEGMENT -> samp = 0 ;

  con_line = 0 ;
  con_samp = 0 ;

  while ( still_on_contour ( segments, &values ) )
  {
    (*contour).l[ ++(*contour).nvals ] = values.line ;
    (*contour).s[ (*contour).nvals ] =  values.samp ;

    if ( con_line == 0 )
      con_line = values.line ;
    else if ( con_line > values.line )
      con_line = values.line ;

    if ( max_samp == 0 )
      max_samp = values.samp ;
    else if ( max_samp < values.samp )
      max_samp = values.samp ;
  }

  max_samp /= 2 ;

  for ( i = 0; i < (*contour).nvals; i++ )
  {
    if ( (*contour).l[ i ] == con_line )
    {
      if ( con_samp == 0 )
        con_samp = (*contour).s[ i ] ;
      else if ( fabs( max_samp - con_samp ) >
                fabs( max_samp - (*contour).s[ i ] ) )
        con_samp = (*contour).s[ i ] ;
    }
  }

  ico_long = ceil( (*contour).nvals ) ;
  ico_line = ceil( con_line ) ;
  ico_samp = ceil( con_samp ) ;

  return ;
}



ROUTINE finish ( font )

  FONT	*font ;
{
/*  globalref int	unit ;*/
  int		status ;
  int		nparms ;
  int		i, k, kk=0, lng, tmp1, tmp2 ;
  char		ss[ 100 ] ;

  tmp1 = (*font).tall * 2 ;
  while ( kk < jj )
  {

    for ( i = kk+1 ; i < jj ; i++ )
    {
      sprintf ( ss, "%d", posi_taken[ kk ][ 2 ] ) ;
      lng = strlen( ss ) ;
      sprintf ( ss, "%d", posi_taken[ i ][ 2 ] ) ;
      if ( lng < strlen( ss ) )
        lng = strlen( ss ) ;
      tmp2 = (*font).tall * (*font).wide * lng * 1.25 ;

      if (( fabs( posi_taken[ kk ][ 0 ] - posi_taken[ i ][ 0 ] ) < tmp1 ) &&
          ( fabs( posi_taken[ kk ][ 1 ] - posi_taken[ i ][ 1 ] ) < tmp2 ))
      {
        if ( posi_taken[ kk ][ 2 ] < posi_taken[ i ][ 2 ] )
        {
          for ( k = 0 ; k < 3 ; k++ )
            posi_taken[ kk ][ k ] = 0 ;
        }
        else {
          for ( k = 0 ; k < 3 ; k++ )
            posi_taken[ i ][ k ] = 0 ;
        }
      }
    }

    kk++ ;
  }

  for ( i = 0 ; i < jj ; i++ )
  {
    (*font).position[ i*2 ][ 0 ] = posi_taken[ i ][ 0 ] ;
    (*font).position[ i*2+1 ][ 0 ] = posi_taken[ i ][ 1 ] ;
    sprintf ( ss, "%d", posi_taken[ i ][ 2 ] ) ;
    strcpy ( (*font).text[ i ], ss ) ;
  }

  status = zvpopen( "grlabel.parm", "SA", &unit ) ;

  status = zvpout( "FONT", &(*font).font, "INT", 1, 0 ) ;
  status = zvpout( "TALL", &(*font).tall, "INT", 1, 0 ) ;
  status = zvpout( "WIDE", &(*font).wide, "REAL", 1, 0 ) ;
  status = zvpout( "THICK", &(*font).thick, "INT", 1, 0 ) ;
  status = zvpout( "POSITION", (*font).position, "INT", ((*font).nfonts)*2, 9 );
  status = zvpout( "TEXT", (*font).text, "STRING", ((*font).nfonts), 9 ) ;

  status = zvpclose() ;

  status = zvclose( unit, NULL ) ;

  return ;
}



void ROUTINE gen_the_font_window ( len, tall, wide, dnarg ) 

  int	len, tall, dnarg ;
  float	wide ;
{
  int	window_nl, window_ns, wndow_ns ;

  window_ns = len * tall * wide ;
  wndow_ns = len * wide ;
  window_nl = tall ;

/*****
  this is the fix for the line
 *****/
  if ( ico_line < window_nl )
    ico_line = window_nl ;
  else if ( ico_line > (img_nl - window_nl) )
    ico_line = img_nl - window_nl ;

/*****
  this is the fix for the sample
 *****/
  if ( ico_samp < wndow_ns )
    ico_samp = wndow_ns ;
  else if ( ico_samp > (img_ns - window_ns) )
    ico_samp = img_ns - window_ns ;

  ico_line += ( window_nl/2 ) + .5 ;
  ico_samp += .5 ;

  ico_line += .5 ;
  ico_samp -= ( window_ns/2 ) + .5 ;

  if ( ico_samp < 1 )
    ico_samp = ceil( wide + 1. ) ;

  posi_taken[ jj ][ 0 ] = ico_line ;
  posi_taken[ jj ][ 1 ] = ico_samp ;
  posi_taken[ jj ][ 2 ] = dnarg ;
  jj++ ;

  return ;
}



ROUTINE generate_font_params ( contour, font, trans )
/*****
  Generate a set of parameters for the program FONT from 'contour'.  Exclude
  contours less than 'min_len' in length.
 *****/

  CONTOUR	*contour ;
  FONT		*font ;
  float		trans[ 2 ] ;
{
  char	ss[ 100 ] ;
  float	wide ;
  int	tall, len, dnarg ;

  if ( (*contour).nvals < min_len ) return ;

  (*font).font = font_type ;
  tall = (*font).tall = font_height ;
  wide = (*font).wide = .8 ;
  (*font).thick = 1 ;

  dnarg = (*contour).dnval = trans[ 0 ] * (*contour).dnval + trans[ 1 ] ;

  sprintf ( ss, "%d", (*contour).dnval ) ;
  len = strlen( ss ) ;
  if ( ii > 0 )
    (*font).text[ ii ][ len+1 ] = 0 ;

  (*font).dn[ (*font).nfonts ] = 255 ;
  (*font).loc[ ((*font).nfonts) ] = 1 ;

  gen_the_font_window ( len, tall, wide, dnarg ) ;

  ++(*font).nfonts ;
  ii++ ;

  return ;
}



 ROUTINE initialize ( segments, contour, font ) 

  GRAPHICS1	*segments ;
  CONTOUR	*contour ;
  FONT		*font ;
{
/*  globaldef int	unit ;*/
  int		status ;
  double	r ;

  TRIPLET	*t,*tp ;

  (*contour).nvals = 0 ;
  (*font).nfonts = 0 ;

  status = zvunit( &unit, "INP", 1, NULL ) ;
  check ( unit, status, " Primary input unit assignment failed...") ;

  status = zvopen( unit, "ADDRESS", &((*segments).start_triplet), "OP",
                  "UPDATE", NULL ) ;
  check ( unit, status, " Primary input open failed...") ;

  status = zvget( unit, "NL", &((*segments).nlines), "NS",
              &((*segments).nsamps), NULL ) ;
  check ( unit, status, " Could not acquire gr1 file size..." ) ;

  CURRENT_SEGMENT = (*segments).start_triplet ;

  r = ( (double)((*segments).nlines ) * (double)((*segments).nsamps)) ;
  r = ceil( r/12. ) ;
      
  (*segments).end_triplet =( (*segments).start_triplet + (int)r - 1 ) ;

  while ( CURRENT_SEGMENT++ < (*segments).end_triplet)
  {
    if ((( CURRENT_SEGMENT -> line ) == 0 ) &&
        ( (CURRENT_SEGMENT+1) -> line ) == 0 )
    {
	  (*segments).end_triplet = CURRENT_SEGMENT ;
	  break ;
    }
  }

  for ( t = (*segments).current_triplet; t <= (*segments).end_triplet; ++t )
  {
    if ( (*t).line == 0 )
    {
      tp = t + 1 ;
      if ( (*tp).line == 0 ) (*segments).end_triplet = t ;
    }
  }

  (*segments).last_nonzero_triplet = *CURRENT_SEGMENT ;
  DIRECTION = FORWARD ;

  return ;
}     



int ROUTINE more_contours ( segments)
/*****
  Finds the next contour in the segments starting from beginning.
 *****/

  GRAPHICS1	*segments ;
{

  (*segments).current_triplet = (*segments).start_triplet ;

  do
  {
    if ( (((*segments).current_triplet) -> line) != 0. )
    {
      (*segments).first_nonzero_triplet = *((*segments).current_triplet) ;
      return ( ((*segments).current_triplet) -> dn ) ;
    }
  } while ( ++((*segments).current_triplet) < ((*segments).end_triplet) ) ;

  return 0 ;
}



int ROUTINE  more_triplets ( segments, triplet ) 
/*****
  Will search the 'segments' for the next nonzero triplet, which will be
  returned.
 *****/

  GRAPHICS1	*segments ;
  TRIPLET	*triplet ;
{
  while (( (++CURRENT_SEGMENT) -> line == 0 ) &&
            ( CURRENT_SEGMENT <= LAST_SEGMENT )) ;

  if ( CURRENT_SEGMENT >= LAST_SEGMENT ) return FALSE ;
  else
  {
    *triplet = *CURRENT_SEGMENT ;
    return ( TRUE ) ;
  }
}



ROUTINE process_parameters ( trans ) 

  float	trans[ 2 ] ;
{
  int	count ;

  zvp( "MINLEN", &min_len, &count ) ;

  zvp( "FONTHT", &font_height, &count ) ;

  zvp( "TRANS", trans, &count ) ;

  zvp( "FONT", &font_type, &count ) ;

  zvp( "IMG_NL", &img_nl, &count ) ;

  zvp( "IMG_NS", &img_ns, &count ) ;

  return ;
}



int ROUTINE search_for_contour ( segments )
/*****
  Will search through the 'segments' to find the next piece of the current
  contour.
 *****/

  GRAPHICS1	*segments ;
{
  TRIPLET	triplet ;

  (*segments).current_triplet = (*segments).start_triplet ;

  while ( more_triplets ( segments, &triplet ) )
  {
    if ( eq_triplet ( triplet,((*segments).last_nonzero_triplet)) )
    {
      (*segments).last_nonzero_triplet = *CURRENT_SEGMENT ;
      CURRENT_SEGMENT -> line = 0 ;
      CURRENT_SEGMENT -> samp = 0 ;
      return TRUE ;
    }
  }
  (*segments).current_triplet = (*segments).start_triplet ;

  while ( more_triplets ( segments, &triplet ) )
  {
    if ( eq_triplet ( triplet,((*segments).first_nonzero_triplet)) )
    {
      (*segments).last_nonzero_triplet = *CURRENT_SEGMENT ;
      CURRENT_SEGMENT -> line = 0 ;
      CURRENT_SEGMENT -> samp = 0 ;
      return TRUE ;
    }
  }

  return FALSE ;
}	  



int ROUTINE still_on_contour ( segments, values ) 
/*****
  Will return 'TRUE' if still on the current contour as described in 'segments'.
  The value of the contour point is returned in 'values'.
 *****/

  GRAPHICS1	*segments ;
  TRIPLET	*values ;
{
  do
  {
    if ( (CURRENT_SEGMENT += DIRECTION) > (*segments).end_triplet )
      return FALSE ;

    if ( (((*segments).current_triplet) -> line ) > 0. )
    {
      ( *values ) = *CURRENT_SEGMENT ;
      (*segments).last_nonzero_triplet = *CURRENT_SEGMENT ;
      CURRENT_SEGMENT -> line = 0 ;
      CURRENT_SEGMENT -> samp = 0 ;
      return TRUE ;
    }

    if ((CURRENT_SEGMENT -= 2*DIRECTION) < (*segments).start_triplet )
      return FALSE ;

    if ( (CURRENT_SEGMENT -> line ) > 0. )
    {
      ( *values ) = *CURRENT_SEGMENT ;
      (*segments).last_nonzero_triplet = *CURRENT_SEGMENT ;
      CURRENT_SEGMENT -> line = 0 ;
      CURRENT_SEGMENT -> samp = 0 ;
      return TRUE ;
    }
  } while ( search_for_contour ( segments ) ) ;

  return FALSE ;
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create clabel.imake
#define PROGRAM clabel

#define MODULE_LIST clabel.c

#define MAIN_LANG_C
#define R2LIB

#define USES_C

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
$ Return
$!#############################################################################
$PDF_File:
$ create clabel.pdf
process help=*
parm	inp		(string,100)
parm	minlen		integer		default=100
parm	trans		real	count=2	default=(1.,0.)
parm	fontht		integer	count=1	default=10
parm	font		integer	count=1	default=1
parm	img_nl		integer	count=1
parm	img_ns		integer	count=1
end-proc
.title
VICAR Program "clabel"---
       Adds contour label information to a file that has been generated
       by "polyscrb" from a file generated by "contour".
.help
PURPOSE:

"clabel" will read the contour information generated by the program,
"contour", after "contour's" output graphics file has been expanded by
the program, "polyscrb". "clabel" will generate a "font" parameter file,
and "font" may be used to add labels to the contour file.
.page
OPERATION:


"clabel" should be used in the following sequence:

	contour A B
	polyscrb B C
	copy C D
	clabel D
	font C E

Note that "clabel" will destroy the contents of its input file, and
that it will create a parameter file that the program, "font", will accept.
The labeled contours will appear in the output of "font".
.page
WRITTEN BY: Mike Girard 12/3/90

COGNIZANT PROGRAMMER: nghia

REVISION: New
          Made portable for UNIX ... V. Unruh ... (CRI) (Jan.  2, 1995)

.level1
.vari inp
"polyscrb'd" output contour file.
.vari minlen
Minimum contour length to be labeled.
.vari trans
Label = trans[0]*dn + trans[1]
.vari fontht
Height in pixels of font.
.vari font
Integer representing font type.
.level2
.vari inp
The single input file to "clabel" which must be an expanded contour
file such as would be produced if the program, "polyscrb", operating 
on a graphics file produced by the program, "contour".
.vari minlen
A contour in the input contour file with length less than 'minlen'
will not be labeled.
.vari trans
The label, which is taken from the dn in the image corresponding
to the contour, will be transformed by LABEL=TRANS[0]*DN+TRANS[1].
.vari fontht
The height of the text in the label in pixels.
.vari font
The type of font to use in the labels. The font type is
taken from those available in the program, "font".
.end
$ Return
$!#############################################################################
$Test_File:
$ create tstclabel.pdf
procedure help=*

refgbl $echo
refgbl $aecho
refgbl $becho

refgbl $autousage
body
let $autousage="none"

let $echo="yes"
let $aecho="yes"
let $becho="yes"

!	generate the input image
!
! gen +
  f2 +
  out = frac.img +
  nl  = 512 +
  ns  = 512  fun="(line-4)**2 + (samp-4)**2"

!	find the contour
!
contour +
  inp     = frac.img +
  out     = frac.con +
  zstart  = 0 +
  zend    = 255 +
  contint = 50 +
  numpts  = 5 +
  dim     = 3

!	generate the contour
!
polyscrb +
  inp = frac.con +
  out = frac.pol +
  dim = 3 +
  nl  = 512 +
  ns  = 512

!	find the contour and generate an output file for the font program
!
label-list frac.pol
list frac.pol
!
clabel +
  inp    = frac.pol +
  minlen = 100 +
  fontht = 5 +
  trans  = ( .5, 1000. ) +
  img_nl = 512 +
  img_ns = 512
!
label-list frac.pol
list frac.pol

let $echo = "no"
let $aecho = "no"
let $becho = "no"

end-proc
$ Return
$!#############################################################################
