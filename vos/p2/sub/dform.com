$!****************************************************************************
$!
$! Build proc for MIPL module dform
$! VPACK Version 1.9, Monday, December 07, 2009, 16:11:06
$!
$! Execute by entering:		$ @dform
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
$!   TEST        Only the test files are created.
$!   IMAKE       Only the IMAKE file (used with the VIMAKE program) is created.
$!   OTHER       Only the "other" files are created.
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
$ write sys$output "*** module dform ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_Test = ""
$ Create_Imake = ""
$ Create_Other = ""
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
$ if primary .eqs. "TEST" then Create_Test = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if primary .eqs. "OTHER" then Create_Other = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_Test .or. Create_Imake .or -
        Create_Other .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to dform.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Create_Other then gosub Other_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_Test = "Y"
$   Create_Imake = "Y"
$   Create_Other = "Y"
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
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("dform.imake") .nes. ""
$   then
$      vimake dform
$      purge dform.bld
$   else
$      if F$SEARCH("dform.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake dform
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @dform.bld "STD"
$   else
$      @dform.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create dform.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack dform.com -mixed -
	-s dform.c -
	-i dform.imake -
	-t tdform.f tzdform.c tdform.imake tdform.pdf tstdform.pdf -
	-o dform.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create dform.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/*

       dform

       the purpose of dform is to manipulate data into a
       byte form suitable for display 


       calling sequence:
       call dform ( in_buf, out_buf, ns, hisbuf, isw, itype, cmode, ib1, ib2, 
                    r1, r2, coll )

       where    in_buf    	input array

		out_buf 	output byte array

                ns     		no. of elements in array in_buf

                hist_buf 	histogram buffer

                isw    		process switch
                             0 = convert data and acquire histogram
                             1 = initialize for later calls

                itype  		data type	(in C language)
                             0 = byte		(0 = signed char)
                             1 = integer * 2	(1 = short int)
                             2 = integer * 4	(2 = int)
                             3 = real * 4	(3 = float)

                cmode  		conversion mode
                             0 = no compression                      
                             1 = bit range selected
                             2 = scaled

                ib1    		low order bit selected
                ib2    		high order bit selected

                r1     		low scale value
                r2     		high scale value  

		coll   		histogram collection flag
		      	     0 = no histogram required
			     1 = histogram required

---------------------------------------------------------------------
 
       Revision history . . .

       bam/ram/hbd/jhr 2/86    fixed.....?????

       bam   9/86    
       	Replaced missing card from byte histogram collect loop
        that screwed up the histograms for byte data.....

       jfm   June 22, 1993
	Made portable by Justin McNeill. Additional
	argument added for output array. COLL argument
	is no longer optional. Test programs expanded
	and corrected. Bit extraction for INTEGER*4 
	data corrected.

	jfm   Dec. 13, 1993	
 	Corrected byte-to-byte transfer and simplified
	handling of data conversion (removed unnecesary	
	allocation of holding arrays). Expanded test
	script. (FR 76815) (JFM059)

*/

#define	NO_COMPRESSION	0
#define BIT_EXTRACTION  1
#define SCALING		2

#include "xvmaininc.h"
#include "ftnbridge.h"

/************************************************************************/
/* C-Callable Version                                                   */
/************************************************************************/

void zdform(in_buf, out_buf, ns, hist_buf, isw, itype, cmode,
	ib1, ib2, r1, r2, coll )
void 	*in_buf;
char 	*out_buf;
int 	ns, hist_buf[], isw, itype, cmode, ib1, ib2, coll;
float	r1, r2;
{
/* 
	Save information from initialization.
*/
static int	data_type, conversion_mode, 
		low_order_bit, high_order_bit;
static float	low_scale_value, high_scale_value, scalar;

int 		x;			/* Loop control variables	   */
float 		value_difference;	/* Scalar calculation variable	   */

char		csamp;
int 		ihb1,ihb2,number_bits;
int 		intval,intsamp;
short int 	isamp,itemp;
float 		rtemp;

char		*byte_buf;
int		*int_buf;
short int	*short_buf;
float		*float_buf;

/* 
	Check ISW for appropriate route
	
	This is the initialization mode. Save all the pertinent
	information for later use.
*/
if ( isw == 1 )
	{
	for ( x=0;x<256;x++ )
		hist_buf[x] = 0;

	data_type 	= itype;
	conversion_mode = cmode;	
	low_order_bit	= ib1;
	high_order_bit	= ib2;

	if ( conversion_mode == SCALING )	/* Compute data scalar 	*/	
		{
		low_scale_value		= r1;
		high_scale_value 	= r2;
		value_difference	= high_scale_value - low_scale_value;
		if ( value_difference == 0.0 ) 
			value_difference = 255.0;
		scalar 			= 255.0 / value_difference;
		}
	}
else
	/*	

	Switch on data type of input.

	*/
	switch ( data_type ) 	{

	case 0:	/* BYTE to BYTE data transfer */

		byte_buf = in_buf;

		switch( conversion_mode ) 	{
		
		case NO_COMPRESSION:

			for ( x=0;x<ns;x++ )
				{
			  	out_buf[x] 	= *(byte_buf+x);
				intval 		= *(byte_buf+x);
				if ( intval<0 )
					intval += 256;
				if ( coll == 1 ) /* Acquire histogram	*/
					hist_buf[intval] += 1;
				}
			break;

		case BIT_EXTRACTION:

			ihb1 = low_order_bit;
			ihb2 = high_order_bit;
			number_bits = ihb2 - ihb1 + 1;
			for ( x=0;x<ns;x++ )
			    {
			    csamp = *(byte_buf+x);
			    out_buf[x] = (csamp>>ihb1)&~(~0<<number_bits);
			    if ( coll == 1 )
			      hist_buf[(int) out_buf[x]] += 1;
			    }
			break;

		case SCALING:

			for ( x=0;x<ns;x++ )
				{
				if ( *(byte_buf+x) < low_scale_value )
					*(byte_buf+x) = (char)low_scale_value;
				if ( *(byte_buf+x) > high_scale_value )
					*(byte_buf+x) = (char)high_scale_value;
				itemp = ((float)(*(byte_buf+x))-low_scale_value)
						* scalar;
				out_buf[x] = (char)itemp;

				if ( coll == 1 )
					hist_buf[itemp] += 1;
				}
			break;	}

		break;

	case 1: /* HALFWORD array conversion */

		short_buf = in_buf;

		switch( conversion_mode ) 	{
		
		case NO_COMPRESSION:

			for ( x=0;x<ns;x++ )
				{
			  	out_buf[x] 	= (char)*(short_buf+x);
				intval 		= (char)*(short_buf+x);
				if ( intval<0 )
					intval += 256;
				if ( coll == 1 ) /* Acquire histogram	*/
					hist_buf[intval] += 1;
				}
			break;

		case BIT_EXTRACTION:

			ihb1 = low_order_bit;
			ihb2 = high_order_bit;
			number_bits = ihb2 - ihb1 + 1;
			for ( x=0;x<ns;x++ )
			    {
			    isamp = *(short_buf+x);
			    itemp = (isamp>>ihb1)&~(~0<<number_bits);
			    out_buf[x] = (char)itemp;
			    if ( coll == 1 )
				hist_buf[itemp] += 1;
			    }
			break;

		case SCALING:

			for ( x=0;x<ns;x++ )
				{
				if ( *(short_buf+x) < low_scale_value )
					*(short_buf+x) = (short)low_scale_value;
				if ( *(short_buf+x) > high_scale_value )
					*(short_buf+x) = (short)high_scale_value;
				itemp = ((float)*(short_buf+x)-low_scale_value)
						* scalar;
				out_buf[x] = (char)itemp;
				if ( coll == 1 )
					hist_buf[itemp] += 1;
				}
			break;	}

		break;

	case 2: /* INT array conversion  */

		int_buf = in_buf;

		switch( conversion_mode ) 	{
		
		case NO_COMPRESSION:

			for ( x=0;x<ns;x++ )
				{
			  	out_buf[x] 	= (char)*(int_buf+x);
				intval 		= (char)*(int_buf+x);
				if ( intval<0 )
					intval += 256;
				if ( coll == 1 ) /* Acquire histogram	*/
					hist_buf[intval] += 1;
				}
			break;

		case BIT_EXTRACTION:

			ihb1 = low_order_bit;
			ihb2 = high_order_bit;
			number_bits = ihb2 - ihb1 + 1;
			for ( x=0;x<ns;x++ )
		    	    {
			    intsamp = *(int_buf+x);
			    intval = (intsamp>>ihb1)&~(~0<<number_bits);
			    out_buf[x] =  (char)intval;
			    if ( coll == 1 )
				hist_buf[intval] += 1;
			    }
			break;

		case SCALING:

			for ( x=0;x<ns;x++ )
				{
				if ( *(int_buf+x) < low_scale_value )
					*(int_buf+x) = low_scale_value;
				if ( *(int_buf+x) > high_scale_value )
					*(int_buf+x) = high_scale_value;
				intval = (*(int_buf+x)-low_scale_value)*scalar;
				out_buf[x] = (char)intval;
				if ( coll == 1 )
					hist_buf[intval] += 1;
				}
			break;	}

		break;

	case 3: /* FLOAT array conversion */

		float_buf = in_buf;

		switch( conversion_mode ) 	{
		
		case NO_COMPRESSION:

			for ( x=0;x<ns;x++ )
				{
			  	out_buf[x] 	= (char)*(float_buf+x);
				intval 		= (char)*(float_buf+x);
				if ( intval<0 )
					intval += 256;
				if ( coll == 1 ) /* Acquire histogram	*/
					hist_buf[intval] += 1;
				}
			break;

		case BIT_EXTRACTION:

			break;

		case SCALING:

			for ( x=0;x<ns;x++ )
				{
				if ( *(float_buf+x) < low_scale_value )
					*(float_buf+x) = low_scale_value;
				if ( *(float_buf+x) > high_scale_value )
					*(float_buf+x) = high_scale_value;
				rtemp = (*(float_buf+x)-low_scale_value)*scalar;
				out_buf[x] = (char)rtemp;
				intval = (int)rtemp;
				if ( coll == 1 )
					hist_buf[intval] += 1;
				}
			break;	}

		break;
	}
}

/************************************************************************/
/* Fortran-Callable Version                                             */
/************************************************************************/


void FTN_NAME2(dform, DFORM) (in_buf, out_buf, ns, hist_buf, isw, itype, cmode,
	ib1, ib2, r1, r2, coll )
void 	*in_buf;
char 	*out_buf;
int 	*ns, *hist_buf, *isw, *itype, *cmode, *ib1, *ib2, *coll;
float	*r1, *r2;
{
   zdform( in_buf, out_buf, *ns, hist_buf, *isw, *itype, *cmode,
	*ib1, *ib2, *r1, *r2, *coll );
}

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create dform.imake
/***********************************************************************

                     IMAKE FILE FOR SUBROUTINE LIBRARY dform

   To Create the build file give the command:

	$ vimake dform                     (VMS)
   or
	% vimake dform                     (Unix)


*************************************************************************/

#define SUBROUTINE dform

#define MODULE_LIST dform.c

#define P2_SUBLIB

#define USES_C
$ Return
$!#############################################################################
$Test_File:
$ create tdform.f
C
C	Test program for DFORM subroutine
C
C	Author: Justin McNeill
C	Date:	November 1993
C	
C
      include 'VICMAIN_FOR'

      subroutine main44

      character*80 buffer
      integer hisbuf(256)
      integer coll

      byte obuf(10)
      byte try(10)
      integer*2 try1(10)
      integer*4 try2(10)
      data try/25,26,27,28,29,30,31,32,33,34/
      data try1/1125,1126,1127,1128,1129,1130,1131,1132,1133,1134/
      data try2/1000,1050,1100,1150,1200,1250,1300,1350,1400,1450/
!
!     test 1 checks the collect flag and byte-to-byte transfer
!
      isw = 1
      coll = 0
      call xvmessage(' Test of byte, halfword and fullword',' ')
      call xvmessage(' conversions and scaling',' ')
      call xvmessage(' with FORTRAN interface ****',' ')
      call xvmessage(' ',' ')
      call xvmessage(' Byte data input ',' ')
      call xvmessage(' ',' ')
     
      write(buffer,100) try(1),try(2),try(3),try(4),try(5)
      call xvmessage(buffer,' ')
      write(buffer,100) try(6),try(7),try(8),try(9),try(10)
      call xvmessage(buffer,' ')

      call dform( try, obuf, 10, hisbuf, isw, 0, 2, 0, 7, 
     - 			0., 100., coll)

      isw = 0
      coll = 0
      call xvmessage(' ',' ')
      call xvmessage(' Entries scaled by a factor of 2.55',' ')
      call xvmessage(' ',' ')
     
      call dform( try, obuf, 10, hisbuf, isw, 0, 0, 0, 0, 
     - 			0., 0., coll )

      write(buffer,100) obuf(1),obuf(2),obuf(3),obuf(4),obuf(5)
      call xvmessage(buffer,' ')
      write(buffer,100) obuf(6),obuf(7),obuf(8),obuf(9),obuf(10)
      call xvmessage(buffer,' ')
      call xvmessage(' ',' ')
!
!     test 2 checks the halfword-to-byte transfer
!
      isw = 1
      coll = 0
      call xvmessage(' ',' ')
      call xvmessage(' Halfword data input ',' ')
      call xvmessage(' ',' ')
     
      write(buffer,100) try1(1),try1(2),try1(3),try1(4),try1(5)
      call xvmessage(buffer,' ')
      write(buffer,100) try1(6),try1(7),try1(8),try1(9),try1(10)
      call xvmessage(buffer,' ')

      call dform( try1, obuf, 10, hisbuf, isw, 1, 2, 0, 7, 
     - 			1125., 1135., coll)

      isw = 0
      coll = 0
      call xvmessage(' ',' ')
      call xvmessage(' Entries scaled by a factor of 25.5',' ')
      call xvmessage(' on a range between 1125 and 1135.',' ')
      call xvmessage(' ',' ')
     
      call dform( try1, obuf, 10, hisbuf, isw, 0, 0, 0, 0, 
     - 			0., 0., coll )

      write(buffer,100) obuf(1),obuf(2),obuf(3),obuf(4),obuf(5)
      call xvmessage(buffer,' ')
      write(buffer,100) obuf(6),obuf(7),obuf(8),obuf(9),obuf(10)
      call xvmessage(buffer,' ')
      call xvmessage(' ',' ')

      if(obuf(7).gt.127) then
	call xvmessage('BYTE on SIG is greater than +127',' ')
      endif
!
!     test 3 checks the fullword-to-byte transfer
!
      isw = 1
      coll = 0
      call xvmessage(' ',' ')
      call xvmessage(' Fullword data input ',' ')
      call xvmessage(' ',' ')
     
      write(buffer,100) try2(1),try2(2),try2(3),try2(4),try2(5)
      call xvmessage(buffer,' ')
      write(buffer,100) try2(6),try2(7),try2(8),try2(9),try2(10)
      call xvmessage(buffer,' ')

      call dform( try2, obuf, 10, hisbuf, isw, 2, 2, 0, 7, 
     - 			1000., 1450., coll)

      isw = 0
      coll = 0
      call xvmessage(' ',' ')
      call xvmessage(' Entries scaled by a factor of 0.56',' ')
      call xvmessage(' on a range between 1000 and 1450.',' ')
      call xvmessage(' ',' ')
     
      call dform( try2, obuf, 10, hisbuf, isw, 0, 0, 0, 0, 
     - 			0., 0., coll )

      write(buffer,100) obuf(1),obuf(2),obuf(3),obuf(4),obuf(5)
      call xvmessage(buffer,' ')
      write(buffer,100) obuf(6),obuf(7),obuf(8),obuf(9),obuf(10)
      call xvmessage(buffer,' ')
      call xvmessage(' ',' ')

      call xvmessage(' ',' ')
      call xvmessage(' TEST C INTERFACE ****',' ')
      call xvmessage(' ',' ')

      call tzdform
      
      return

 100  format(' ',I4,' ',I4,' ',I4,' ',I4,' ',I4,' ')
      end
$!-----------------------------------------------------------------------------
$ create tzdform.c
/*

	Test subroutine of DFORM from C programming language

	Author:	Justin McNeill
	Date:	June 1993

	Revisions:

	December 13, 1993 	Byte-to-byte data transfer test added.
				(FR ) (JFM059)
*/

#include "xvmaininc.h"
#include "ftnbridge.h"

FTN_NAME(tzdform)()
{
       	int hisbuf[256],coll,isw;
	int h,i,j,k;
       	char substring[10],string[200];
       	char try[40],try1[20];
	short int try2[10];
	int try3[10];
	float try4[10];
	float r1, r2;

	/* initialize values of 8-bit array TRY */
	for(i=25,j=0;i<35;i++,j++)
		try[j] = i;

	/*
	test 0 checks the collect flag - byte-to-byte data transfer	
	*/
        isw  = 1;			/* initialize and do not acquire */
	coll = 0;			/* histogram 			 */
     	zvmessage("\n Byte data input."," ");
	
	sprintf(string,"  %d",try[0]);
	for(k=1;k<j;k++)
		{
		sprintf(substring,"  %d",try[k]);
		strcat(string,substring);
		}
	zvmessage(string," ");
 
	zdform( try, try1, j, hisbuf, isw, 0, 2, 0, 7, 0., 100., coll );

	isw  = 0;
	coll = 0;
	zdform( try, try1, j, hisbuf, isw, 0, 0, 0, 0, 0., 0., coll );

	zvmessage(" "," ");
	zvmessage(" Entries scaled by a factor of 2.55"," ");
	zvmessage(" "," ");
	
	sprintf(string,"  %d",try1[0]);
	for(k=1;k<j;k++)
		{
		sprintf(substring,"  %d",try1[k]);
		strcat(string,substring);
		}
	zvmessage(string," ");

	/* initialize values of array TRY1 	*/
	for(i=1,j=0;i<11;i++,j++)
		try1[j] = i;
	for(i=(-127);i<-122;i++,j++)
		try1[j] = i;
	for(i=(-5);i<0;i++,j++)
		try1[j] = i;

	/* initialize values of array TRY2 	*/
	for(i=125,j=0;i<135;i++,j++)
		try2[j] = i;

	/* initialize values of array TRY3 	*/
	for(i=1001,j=0;i<1011;i++,j++)
		try3[j] = i;

	/* initialize values of array TRY4 	*/
	for(i=10001,j=0;i<10011;i++,j++)
		try4[j] = (float)i;

	
	/*
	test 1 checks the collect flag - use byte data for simplicity
	*/
        isw  = 1;			/* initialize and do not acquire */
	coll = 0;			/* histogram 			 */
     	zvmessage("\n Byte data input."," ");
	
	sprintf(string,"  %d",try1[0]);
	for(k=1;k<20;k++)
		{
		sprintf(substring,"  %d",try1[k]);
		strcat(string,substring);
		}
	zvmessage(string," ");
 
	zdform( try1, try, 20, hisbuf, isw, 0, 0, 0, 7, 0., 255., coll );
 	zvmessage("\n No histogram produced."," ");

	for(h=0,k=0;k<32;k++)		/* print histogram		*/
		{
		sprintf(string,"      %d",hisbuf[h++]);
		for(j=1;j<8;j++)
			{
			sprintf(substring,"      %d",hisbuf[h++]);
			strcat(string,substring);
			}
		zvmessage(string," ");
		}

	isw  = 0;			/* acquire histogram		*/
	coll = 1;
	zdform( try1, try, 20, hisbuf, isw, 0, 0, 0, 0,	0., 0., coll );
	zvmessage("\n Histogram produced."," ");

	for(h=0,k=0;k<32;k++)		/* print histogram		*/
		{
		sprintf(string,"      %d",hisbuf[h++]);
		for(j=1;j<8;j++)
			{
			sprintf(substring,"      %d",hisbuf[h++]);
			strcat(string,substring);
			}
		zvmessage(string," ");
		}

	/*
	test 2 - work with integer*2 data 
	first, extract the first 3 bits then collect histogram and
	finally scale.
	*/

	isw  = 1;
	zvmessage("\n Half word data input."," ");

	sprintf(string,"  %d",try2[0]);
	for(k=1;k<10;k++)
		{
		sprintf(substring,"  %d",try2[k]);
		strcat(string,substring);
		}
	zvmessage(string," ");

	zdform ( try2, try, 10, hisbuf, isw, 1, 1, 0, 2, 0., 255., coll );
	
	isw = 0;			/* acquire histogram		*/
	coll = 1;	
	zdform ( try2, try, 10, hisbuf, isw, 0, 0, 0, 0, 0., 0., coll );
	zvmessage("\n Bits 0 thru 2 extracted "," ");

	sprintf(string,"  %d",try[0]);
	for(k=1;k<10;k++)
		{
		sprintf(substring,"  %d",try[k]);
		strcat(string,substring);
		}
	zvmessage(string," ");

	zvmessage("\n Histogram produced."," ");
	for(h=0,k=0;k<32;k++)
		{
		sprintf(string,"      %d",hisbuf[h++]);
		for(j=1;j<8;j++)
			{
			sprintf(substring,"      %d",hisbuf[h++]);
			strcat(string,substring);
			}
		zvmessage(string," ");
		}

	isw  = 1;			/* initialize with scale values */
	r1   = 120.0;
	r2   = 150.0;
	sprintf( string,"\n Data scaled r1 = %f  r2 = %f", r1, r2 );
 	zvmessage( string," " );
	zdform ( try2, try, 10, hisbuf, isw, 1, 2, 0, 7, r1, r2, coll );

	isw = 0;			/* scale values			*/
	coll = 0;	
	zdform( try2, try, 10, hisbuf, isw, 0, 0, 0, 0, 0., 0., coll);
	sprintf(string,"  %d",try[0]);
	for(k=1;k<10;k++)
		{
		sprintf(substring,"  %d",try[k]);
		strcat(string,substring);
		}
	zvmessage(string," ");

	/*
	test 3 - integer data 
	*/

	isw  = 1;
	zvmessage("\n Integer data input."," ");
	
	sprintf(string,"  %d",try3[0]);
	for(k=1;k<10;k++)
		{
		sprintf(substring,"  %d",try3[k]);
		strcat(string,substring);
		}
	zvmessage(string," ");

	zdform ( try3, try, 10, hisbuf, isw, 2, 1, 0, 7, 0., 255., coll );

	isw = 0;			/* acquire histogram and do	*/
	coll = 1;			/* bit extraction.		*/
	zdform( try3, try, 10, hisbuf, isw, 0, 0, 0, 0, 0., 0., coll);
	zvmessage("\n Bits 0 thru 7 extracted "," ");

	sprintf(string,"  %d",try[0]);
	for(k=1;k<10;k++)
		{
		sprintf(substring,"  %d",try[k]);
		strcat(string,substring);
		}
	zvmessage(string," ");

	zvmessage("\n Histogram produced."," ");
	for(h=0,k=0;k<32;k++)
		{
		sprintf(string,"      %d",hisbuf[h++]);
		for(j=1;j<8;j++)
			{
			sprintf(substring,"      %d",hisbuf[h++]);
			strcat(string,substring);
			}
		zvmessage(string," ");
		}

	isw  = 1;
	coll = 0;
      	r1   = 1000.0;
	r2   = 1010.0;
	sprintf( string,"\n Data scaled r1 = %f  r2 = %f", r1, r2 );
       	zvmessage( string," " );
       	zdform ( try3, try, 10, hisbuf, isw, 2, 2, 0, 7, r1, r2, coll );

	isw = 0;			/* perform bit extraction	*/

       	zdform ( try3, try, 10, hisbuf, isw, 0, 0, 0, 0, 0., 0., coll );

	sprintf(string,"  %d",try[0]);
	for(k=1;k<10;k++)
		{
		sprintf(substring,"  %d",try[k]);
		strcat(string,substring);
		}
	zvmessage(string," ");

	/*
	test 4 - real data 
	*/

 	zvmessage("\n Real data input."," ");

	sprintf(string,"  %7.2f",try4[0]);
	for(k=1;k<10;k++)
		{
		sprintf(substring,"  %7.2f",try4[k]);
		strcat(string,substring);
		}
	zvmessage(string," ");

	isw  = 1;
	r1   = 10000.0;
	r2   = 10010.0;

	zdform ( try4,try, 10, hisbuf, isw, 3, 2, 0, 7, r1, r2, coll );

	isw = 0;			/* Acquire histogram and perform */
	coll = 1;			/* scaling of data.		 */

	zdform ( try4,try, 10, hisbuf, isw, 0, 0, 0, 0, 0., 0., coll );
	sprintf( string,"\n Data scaled r1 = %f  r2 = %f", r1, r2 );
	zvmessage( string, " " );

	sprintf(string,"  %d",try[0]);
	for(k=1;k<10;k++)
		{
		sprintf(substring,"  %d",try[k]);
		strcat(string,substring);
		}
	zvmessage(string," ");

 	zvmessage("\n Histogram produced."," ");
	for(h=0,k=0;k<32;k++)
		{
		sprintf(string,"      %d",hisbuf[h++]);
		for(j=1;j<8;j++)
			{
			sprintf(substring,"      %d",hisbuf[h++]);
			strcat(string,substring);
			}
		zvmessage(string," ");
		}
	zvmessage("\n\n TEST PROGRAM COMPLETED"," ");
}
$!-----------------------------------------------------------------------------
$ create tdform.imake
#define PROGRAM tdform

#define MODULE_LIST tdform.f tzdform.c

#define TEST

#define MAIN_LANG_FORTRAN
#define USES_C
#define USES_FORTRAN

#define LIB_P2SUB
#define LIB_RTL
#define LIB_TAE
$!-----------------------------------------------------------------------------
$ create tdform.pdf
process help=*
END-PROC
$!-----------------------------------------------------------------------------
$ create tstdform.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
tdform
end-proc
$ Return
$!#############################################################################
$Other_File:
$ create dform.hlp
1 DFORM
  DFORM converts integer*2 (short), integer*4 (int) or real*4 (float) data 
  into signed byte data and optionally collects histograms, performs bit
  extraction or scaling of array values.

  Calling Sequence:
	
  from FORTRAN program ...
	CALL DFORM ( IN_BUF, OUT_BUF, NS, HISBUF, ISW, ITYPE, CMODE, 
			IB1, IB2, R1, R2, COLL )

  from C program ...
  	zdform ( in_buf, out_buf, ns, hisbuf, isw, itype, cmode, 
			ib1, ib2, r1, r2, coll );

  where        	in_buf   	input array           		( VOID )

		out_buf		output byte array		( BYTE )

    		ns     		no. of elements in array buf   	( INTEGER )

       		hisbuf 		histogram buffer               	( INTEGER )

   		isw    		process switch                	( INTEGER )
   			0 = convert data and acquire histogram
                        1 = initialize for later calls

               	itype  		data type                     	( INTEGER )
                        0 = byte 	(signed char)
                        1 = integer * 2	(short int)
                        2 = integer * 4	(int)
                        3 = real * 4	(float)

  		cmode  		conversion mode              	( INTEGER )
                        0 = no compression                      
                        1 = bit range selected
                        2 = scaled

		ib1    		low order bit selected        	( INTEGER )
            	ib2    		high order bit selected         ( INTEGER )

		r1     		low scale value			( REAL )
               	r2    		high scale value   		( REAL )
 
      		coll   		histogram collection flag 	( INTEGER )
 			0 = no histogram to be collected
			1 = histogram collected

2 Operation - Bit Extraction

One note regarding bit extraction mode -  This function is implemented to do
explicit bit extraction without modification to compensate for the handling of
negative numbers. Thus, a value like +1001, when undergoing bit extraction for
its lowest seven bits, results in a value of NEGATIVE 23 (-23), instead of
a positive 233 (+233). The reason for this is that the highest order bit in
the byte is used for sign with two's complement arithmetic for the remaining
bits. However, in extracting the lowest three bits, a POSITIVE 1 (+1) would
result.

2 History

  Original Programmer: Barbara A. McGuffie
  Current Cognizant Programmer: Justin McNeill
  Source Language: C

  Revision history:

	June 22, 1993 	Made portable by Justin McNeill. Additional
			argument added for output array. COLL argument
			is no longer optional. Test programs expanded
			and corrected. Bit extraction for INTEGER*4 
			data corrected.

	Dec. 13, 1993	Corrected byte-to-byte transfer and simplified
			handling of data conversion (removed unnecesary
			allocation of holding arrays). Expanded test
			script. (FR 76815) (JFM059)
$ Return
$!#############################################################################
