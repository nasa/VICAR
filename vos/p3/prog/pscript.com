$!****************************************************************************
$!
$! Build proc for MIPL module pscript
$! VPACK Version 1.9, Wednesday, March 10, 2010, 12:27:41
$!
$! Execute by entering:		$ @pscript
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
$ write sys$output "*** module pscript ***"
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
$ write sys$output "Invalid argument given to pscript.com file -- ", primary
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
$   if F$SEARCH("pscript.imake") .nes. ""
$   then
$      vimake pscript
$      purge pscript.bld
$   else
$      if F$SEARCH("pscript.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake pscript
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @pscript.bld "STD"
$   else
$      @pscript.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create pscript.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack pscript.com -mixed -
	-s pscript.c -
	-i pscript.imake -
	-t tstpscript.pdf bw.tst h.tst rgb.tst -
	-p pscript.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create pscript.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/**
 **	pscript.c:
 **
 **	Purpose:
 **
 **		To convert Vicar images to PostScript text file format,
 **		downloadable to a PostScript-compatible device.
 **
 **	Revision History:
 **
 **		18 MAR 1993: NDR: Converted from FORTRAN to C (Unix Port).
 **		09 JUN 1993: NDR: Fixed bugs in scaling images,
 **                               Incremental subsampling, and Titles.
 **/

#include "vicmain_c"
#include <string.h>	/* for malloc */
#include <stdlib.h>
#include <stdio.h>

void main44(void)
{
	float range[2],offset,slope,availheight,availwidth,aspect,margin;
	int count,def,defpos,defwidth,defheight;
	int scale, numinp;
	int dotitle,numtitles;
	int inunit[3],nl,ns;
	int nlo,nso;
	int nbits,nsb,bitshift;
	int increment;
	int fontpoint;
	int hexcount;
	int i,line,samp,band;
	int pixsize;
	unsigned char *lbuff,*ptr,*endptr;
	float pagesize[2],pagepos[2];
	float width,height,freq;
	char *hexformat, command_line[133], command_part[20];
	char *format, filename[133];
	char outname[133],title[5][132];
	char fontname[133];
	char timestr[80];
	int psize;
	long tim;
	long time();
	char *ctime();
	FILE *outfile;  /* FILE is not very portable... */
	
	zveaction("sa","");

 	/* Find out if the input image isn't BYTE; if so, scale */

	zvunit( inunit, "inp", 1, NULL);
	zvopen( inunit[0], NULL);
	zvget( inunit[0],"pix_size",&psize,NULL);
        zvclose( inunit[0],NULL);

	zvparm( "dnrange", range, &count, &def, 0,0);
	scale = (def == 0) || (psize>1);
	if (scale)
	{
		format = "real";
		offset = range[0];
		slope = 255. / (range[1]+1e-38 - offset);
	}
	else format = "byte";

	zvpcnt( "inp", &numinp );
	for (i=0;i<numinp; i++)
	{
		zvunit( inunit+i, "inp", i+1,NULL);
		zvopen( inunit[i], "u_format", format, NULL);
	}

	zvget( inunit[0], "nl", &nl, "ns", &ns, NULL);
	zvp( "inc", &increment,&count);
	zvpone( "inp", filename, 1, 132 ); 
	zvparm( "title", title, &numtitles, &def, 5,132);
	dotitle = (def == 0) || ! zvptst( "notitle" );

	/** allocate storage buffer **/

	zvpixsizeu(  &pixsize, format, inunit[0] );
	lbuff = (unsigned char *)malloc( (long)pixsize*ns );
	if (!lbuff)
	{
		zvmessage( "unable to allocate line buffer","");
		zabend();
	}
	
	zvpone( "font", fontname, 1, 132 );
	zvp( "point", &fontpoint, &count );
	
	tim = time(0);			/* Get date and time */
	strcpy(timestr, ctime(&tim) );

	nso = ns/increment;
	nlo = nl/increment;
	if (zvptst( "8bits" ) )
	{
		nbits = 8;
		hexcount = 30;
		hexformat="%02x";
	}
	else
	{
		nbits = 4;
		hexcount = 60;
		hexformat="%01x";
	}
	nsb = nso/hexcount;
	bitshift = 8-nbits;
	
	zvparm( "pagesize", pagesize, &count, &def, 0,0);
	
	if (zvptst("wide"))
	{
		float ptmp;
		
		/* landscape option -- switch values */
		ptmp = pagesize[0];
		pagesize[0] = pagesize[1];
		pagesize[1] = ptmp;
	}
	
	zvp( "margin", &margin, &count );
	zvpone( "out", outname, 1, 132 );
	zvparm( "pagepos", pagepos, &count,&defpos, 0,0);
	zvparm( "width", &width, &count, &defwidth, 0,0);
	zvparm( "height", &height, &count, &defheight, 0,0);

	availwidth = pagesize[0] - pagepos[0] - 0.5;
	availheight = pagesize[1] - pagepos[1] - margin -
                       (float) ((numtitles+1) * fontpoint)/72.0 - 0.5;
	aspect = (float) nl / (float) ns;

	if (defwidth == 1)
		if (defheight == 1)
		{					    /* both defaulted */
			width = availwidth;
			height = availheight;
			if (height/width > aspect)	/*force correct aspect*/
				height = width * aspect;
			else
				width = height / aspect;
		}
		else				      /* only width defaulted */
			width = height / aspect;
	else
		if (defheight == 1)		     /* only height defaulted */
			height = width * aspect;
		else					/* neither defaulted  */
			if (height/width > aspect)	/*force correct aspect*/
				height = width * aspect;
			else
				width = height / aspect;
							/*force to fit on page*/
	if (width > availwidth)
	{
		width = availwidth;
		height = width * aspect;
	}
	if (height > availheight)
	{
		height = availheight;
		width = height / aspect;
	}

	if (defpos==1)
	{
		pagepos[0] = (pagesize[0] - width) /2.;
		pagepos[1] = pagesize[1] - (availheight + height + 0.5)/2.0 ;
	}

	outfile = fopen( outname, "w" );
	
	if (!outfile)
	{
		zvmessage( "Error opening output file"," ");
		zabend();
	}

/*
 * Set up the ADOBE POSTSCRIPT (R)  Page description Comments
 */
	fprintf( outfile, "%s\n",	"%!PS-Adobe-2.0");
	fprintf( outfile, "%s\n",	"%%Creator: VICAR Program PSCRIPT");
	fprintf( outfile, "%s%s\n",	"%%Title: ", filename);
	fprintf( outfile, "%s%s",	"%%CreationDate: ", timestr);
	fprintf( outfile, "%s\n",	"%%Pages: 1");
	fprintf( outfile, "%s%s\n",	"%%DocumentFonts: ", fontname);
	fprintf( outfile, "%s\n\n",	"%%EndComments");

/*
 * Set up the PostScript Command definitions
 */

	fprintf( outfile, "%s\n",	" /inch { 72 mul} def");
	fprintf( outfile, "%s%9.3f%s\n"," /height ", height, " def");
	fprintf( outfile, "%s%9.3f%s\n"," /width ", width, " def");
	fprintf( outfile, "%s%9.3f%s\n"," /xPos ", pagepos[0], " def");
	fprintf( outfile, "%s%9.3f%s\n"," /yPos ", pagepos[1], " def");
	fprintf( outfile, "%s%d%s\n",	" /fontpt ", fontpoint, " def");

/*
 *  Postscript buffer for image line
 */

	if (numinp == 1)  /* grayscale image */
	{
		fprintf( outfile, "%s%d%s\n",	" /picstr ", nso, " string def");
	}
	else if (numinp==3)
	{
		fprintf( outfile, "%s%d%s\n",	" /Rstr ", nso, " string def");
		fprintf( outfile, "%s%d%s\n",	" /Gstr ", nso, " string def");
		fprintf( outfile, "%s%d%s\n",	" /Bstr ", nso, " string def");
	}
	else
	{
		zvmessage( "*** Specify one or 3 images, only ***", "");
		zabend();
	}

/*
 *  Define PostScript procedure to read in hex image data
 */

	fprintf( outfile, "%s\n",	" /vicarimage");
	fprintf( outfile, " { %d %d %d [ %d 0 0 %d 0 %d ] \n",
				nso,nlo,nbits,nso,-nlo,nlo );
	if (numinp == 1)
	{
		fprintf( outfile, "     {currentfile picstr readhexstring pop}\n");
		fprintf( outfile, "     image\n");
		strcpy(command_part, "lpr ");
	}
	else
	{
		fprintf( outfile, "     {currentfile Rstr readhexstring pop}\n");
		fprintf( outfile, "     {currentfile Gstr readhexstring pop}\n");
		fprintf( outfile, "     {currentfile Bstr readhexstring pop}\n");
		fprintf( outfile, "     true 3\n");
		fprintf( outfile, "     colorimage\n");
		strcpy(command_part, "lp -dphaser183-518 ");
	}
	fprintf( outfile, "     }  def\n");

/*
 * Save old POSTSCRIPT Parameters and change scaling,position:
 */

	fprintf( outfile, " gsave\n");
	zvparm( "freq", &freq, &count, &def, 0,0);
	if (def==0) fprintf( outfile, " %9.2f %s\n", freq,
		"45 {dup mul exch  dup mul add 1 exch sub} setscreen" );
	
	if (zvptst( "wide" )) 
		fprintf( outfile, "  0 %6.2f inch  translate -90 rotate\n", 
					pagesize[0] );
		
	fprintf( outfile, " xPos inch yPos inch translate\n");

	if (zvptst( "wide" )) 
		fprintf( outfile, "%s\n", " gsave %Save for text");

	fprintf( outfile, " width inch height inch scale\n");

 
	fprintf( outfile, " vicarimage\n");	/* Call image reading  procedure */

/*
 * Read in VICAR image and convert to Hexadecimal text output
 */

	for (line=0; line<nlo; line++)
	{
	   for (band=0; band<numinp; band++)
	   {
		ptr=lbuff;
		zvread( inunit[band], lbuff, 
			"line", line*increment + 1, NULL );
		if (scale)
			scale_image( lbuff, slope,offset,ns,increment);
		else if (increment>1)
			subsample_image(lbuff, ns, increment);
		
		/* write out in blocks of <hexcount> values */
		
		for (samp=0; samp<nsb; samp++)
		{
		   for (i=0; i<hexcount; i++)
		   fprintf( outfile, hexformat, (*ptr++)>>bitshift );
		   fprintf( outfile, "\n" );
		}
		
		/* write out the rest of line */
		
		endptr = lbuff+nso;
		while (ptr < endptr)
		fprintf( outfile, hexformat, (*ptr++)>>bitshift );
		fprintf( outfile, "\n" );
		
	    } /* band loop */
	} /* line  loop */
	
	/*** end of image-dump ***/
	
	fprintf( outfile, " grestore\n" );  /* restores page parameters */
	
	if (dotitle)
	{
	    /* font name and scale */
	   
	    fprintf( outfile, "  /%s findfont fontpt scalefont\n", fontname );
	    fprintf( outfile, "  setfont\n");
	    
	    /***
	     *** Next, position the TITLE centered beneath the image
	     *** by use of the formulas, translated from the Reverse Polish:
	     ***           X_position = POS(1) + (WIDTH - STR_WIDTH)/2
	     ***           Y_position = POS(2) - (MARGIN + STR_HEIGHT)
	     ***/

	    fprintf( outfile, "  %% The following centers title below image:\n");

            if (numtitles == 0)
            {
                strcpy(title[0],filename);
		numtitles = 1;
	    }
	    
	    if (zvptst( "wide" )) 
	    {
	        for (i=0; i<numtitles; i++)
	        {
	            fprintf( outfile, "  (%s)\n", title[i] );
		    fprintf( outfile, " dup stringwidth pop\n");
		    fprintf( outfile, " width inch exch sub 2 div\n");
		    fprintf( outfile, " %d fontpt mul -1 mul\n", i+1);
		    fprintf( outfile, " %6.3f inch sub moveto show\n", margin);
		}
	    }
	    else
	    {
	        for (i=0; i<numtitles; i++)
	        {
	            fprintf( outfile, "  (%s)\n", title[i] );
		    fprintf( outfile,
		      " dup stringwidth pop %d fontpt mul %9.3f inch add\n",
				   i+1,  margin);
		    fprintf( outfile,
		      " yPos inch exch sub exch %s\n", "% Y-Position on page");
		    fprintf( outfile,
		     " width inch sub 2 div %s\n", "% Width of image in inches");
		    fprintf( outfile,
	 		" xPos inch exch sub exch moveto show %s\n",
			"% X-position");
		}
	    }

	    fprintf( outfile, " grestore  %s\n", "% Restore former parameters");

	} /* title */
	
	fprintf( outfile, " showpage %s\n", " % Command to print out page.");
	
	fclose( outfile );
	for (i=0; i<numinp; i++)
	   zvclose( inunit[i], NULL);
                                                     /* added by rea 8/5/93  */
        if (!zvptst("NOPRINT"))
        {
           strcpy(command_line,command_part);
           strcat(command_line,outname);
           system(command_line);
        }

        if (zvptst("PRINT"))
        {
	   system("lpq");
           strcpy(command_line,"rm -f ");
           strcat(command_line,outname);
           system(command_line);
        }
                                                    /* end of 8/5/93 addition */
}



/* subsample a byte array (no scaling) */

subsample_image(lbuf, ns, increment)
unsigned char *lbuf;	/* input/output -- the values to scale	*/
int ns;			/* input - number of values		*/
int increment;		/* input - subsampling			*/
{
	register unsigned char *buf = lbuf;
	register int i;
	
	for (i=0; i<ns; i+=increment)
		(*lbuf++) = buf[i];
}



/* scale and convert a real buffer to byte, and sub-sample */

scale_image( lbuf, slope,offset,ns,increment)
unsigned char *lbuf;	/* input/output -- the values to scale	*/
float slope;		/* input - scale factor 		*/
float offset;		/* input - offset			*/
int ns;			/* input - number of values		*/
int increment;		/* input - subsampling			*/
{
	float *rbuf = (float *)lbuf;
	int value,i;
	
	for (i=0; i<ns; i+=increment)
	{
		/* scale and clip to byte range */

		value = (rbuf[i] - offset) * slope;
		value = value <   0 ?   0 : value;
		(*lbuf++) =  value > 255 ? 255 : value;
	}
}

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create pscript.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM pscript

   To Create the build file give the command:

		$ vimake pscript			(VMS)
   or
		% vimake pscript			(Unix)


************************************************************************/


#define PROGRAM	pscript
#define R3LIB

#define MODULE_LIST pscript.c

#define MAIN_LANG_C
#define USES_C

#define LIB_RTL
#define LIB_TAE

/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$Test_File:
$ create tstpscript.pdf
procedure
refgbl $autousage
body

  let $autousage="none"

  gen bw nl=10 ns=10
  gen r nl=10 ns=10 linc=25 sinc=25
  gen g nl=10 ns=10 linc=0 sinc=25
  gen b nl=10 ns=10 linc=25 sinc=0

  gen h nl=20 ns=20 'half

  write " --- This procedure generates 3 PostScript text files, called"
  write " --- bw.pst h.pst, & rgb.pst, which may be downloaded to appropriate"
  write " --- postscript printers, or compared with the supplied"
  write " --- correct-postscript files, called bw.tst, h.tst, & rgb.tst;"
  write " --- The files should only differ in the Creation Date line."

  pscript bw bw.pst title="Black & White Test Image"
  pscript h h.pst title=("Subsampled","Halfword","with","Titles") inc=2
  pscript (r,g,b) rgb.pst title="Color Test Image"

end-proc
$!-----------------------------------------------------------------------------
$ create bw.tst
%!PS-Adobe-2.0
%%Creator: VICAR Program PSCRIPT
%%Title: bw
%%CreationDate: Mon Mar 22 11:21:47 1993
%%Pages: 1
%%DocumentFonts: Times-Roman
%%EndComments

 /inch { 72 mul} def
 /height     6.000 def
 /width     6.000 def
 /xPos     1.250 def
 /yPos     2.500 def
 /fontpt 15 def
 /picstr 10 string def
 /vicarimage
 { 10 10 8 [ 10 0 0 -10 0 10 ] 
     {currentfile picstr readhexstring pop}
     image
     }  def
 gsave
 xPos inch yPos inch translate
 width inch height inch scale
 vicarimage
00010203040506070809
0102030405060708090a
02030405060708090a0b
030405060708090a0b0c
0405060708090a0b0c0d
05060708090a0b0c0d0e
060708090a0b0c0d0e0f
0708090a0b0c0d0e0f10
08090a0b0c0d0e0f1011
090a0b0c0d0e0f101112
 grestore
  /Times-Roman findfont fontpt scalefont
  setfont
  % The following centers title below image:
  (Black & White Test Image)
 dup stringwidth pop 1 fontpt mul     0.000 inch add
 yPos inch exch sub exch % Y-Position on page
 width inch sub 2 div % Width of image in inches
 xPos inch exch sub exch moveto show % X-position
 grestore  % Restore former parameters
 showpage  % Command to print out page.
$!-----------------------------------------------------------------------------
$ create h.tst
%!PS-Adobe-2.0
%%Creator: VICAR Program PSCRIPT
%%Title: h
%%CreationDate: Wed Jun  9 14:58:30 1993
%%Pages: 1
%%DocumentFonts: Times-Roman
%%EndComments

 /inch { 72 mul} def
 /height     6.000 def
 /width     6.000 def
 /xPos     1.250 def
 /yPos     2.500 def
 /fontpt 15 def
 /picstr 10 string def
 /vicarimage
 { 10 10 8 [ 10 0 0 -10 0 10 ] 
     {currentfile picstr readhexstring pop}
     image
     }  def
 gsave
 xPos inch yPos inch translate
 width inch height inch scale
 vicarimage
00020406080a0c0e1012
020406080a0c0e101214
0406080a0c0e10121416
06080a0c0e1012141618
080a0c0e10121416181a
0a0c0e10121416181a1c
0c0e10121416181a1c1e
0e10121416181a1c1e20
10121416181a1c1e2022
121416181a1c1e202224
 grestore
  /Times-Roman findfont fontpt scalefont
  setfont
  % The following centers title below image:
  (Subsampled)
 dup stringwidth pop 1 fontpt mul     0.000 inch add
 yPos inch exch sub exch % Y-Position on page
 width inch sub 2 div % Width of image in inches
 xPos inch exch sub exch moveto show % X-position
  (Halfword)
 dup stringwidth pop 2 fontpt mul     0.000 inch add
 yPos inch exch sub exch % Y-Position on page
 width inch sub 2 div % Width of image in inches
 xPos inch exch sub exch moveto show % X-position
  (with)
 dup stringwidth pop 3 fontpt mul     0.000 inch add
 yPos inch exch sub exch % Y-Position on page
 width inch sub 2 div % Width of image in inches
 xPos inch exch sub exch moveto show % X-position
  (Titles)
 dup stringwidth pop 4 fontpt mul     0.000 inch add
 yPos inch exch sub exch % Y-Position on page
 width inch sub 2 div % Width of image in inches
 xPos inch exch sub exch moveto show % X-position
 grestore  % Restore former parameters
 showpage  % Command to print out page.
$!-----------------------------------------------------------------------------
$ create rgb.tst
%!PS-Adobe-2.0
%%Creator: VICAR Program PSCRIPT
%%Title: r
%%CreationDate: Mon Mar 22 11:21:47 1993
%%Pages: 1
%%DocumentFonts: Times-Roman
%%EndComments

 /inch { 72 mul} def
 /height     6.000 def
 /width     6.000 def
 /xPos     1.250 def
 /yPos     2.500 def
 /fontpt 15 def
 /Rstr 10 string def
 /Gstr 10 string def
 /Bstr 10 string def
 /vicarimage
 { 10 10 8 [ 10 0 0 -10 0 10 ] 
     {currentfile Rstr readhexstring pop}
     {currentfile Gstr readhexstring pop}
     {currentfile Bstr readhexstring pop}
     true 3
     colorimage
     }  def
 gsave
 xPos inch yPos inch translate
 width inch height inch scale
 vicarimage
0019324b647d96afc8e1
0019324b647d96afc8e1
00000000000000000000
19324b647d96afc8e1fa
0019324b647d96afc8e1
19191919191919191919
324b647d96afc8e1fa13
0019324b647d96afc8e1
32323232323232323232
4b647d96afc8e1fa132c
0019324b647d96afc8e1
4b4b4b4b4b4b4b4b4b4b
647d96afc8e1fa132c45
0019324b647d96afc8e1
64646464646464646464
7d96afc8e1fa132c455e
0019324b647d96afc8e1
7d7d7d7d7d7d7d7d7d7d
96afc8e1fa132c455e77
0019324b647d96afc8e1
96969696969696969696
afc8e1fa132c455e7790
0019324b647d96afc8e1
afafafafafafafafafaf
c8e1fa132c455e7790a9
0019324b647d96afc8e1
c8c8c8c8c8c8c8c8c8c8
e1fa132c455e7790a9c2
0019324b647d96afc8e1
e1e1e1e1e1e1e1e1e1e1
 grestore
  /Times-Roman findfont fontpt scalefont
  setfont
  % The following centers title below image:
  (Color Test Image)
 dup stringwidth pop 1 fontpt mul     0.000 inch add
 yPos inch exch sub exch % Y-Position on page
 width inch sub 2 div % Width of image in inches
 xPos inch exch sub exch moveto show % X-position
 grestore  % Restore former parameters
 showpage  % Command to print out page.
$ Return
$!#############################################################################
$PDF_File:
$ create pscript.pdf
PROCESS HELP=*
PARM INP    TYPE=(STRING,72) COUNT=1:3
PARM OUT    TYPE=(STRING,72) 
PARM WIDTH  REAL             DEFAULT=0.0
PARM HEIGHT REAL             DEFAULT=0.0
PARM INC    INTEGER          DEFAULT=1
PARM DNRANGE REAL    COUNT=2 DEFAULT=(0.,255.)
PARM NUMBITS KEYWORD VALID=(8BITS,4BITS) DEFAULT=8BITS
PARM PAGEPOS REAL    COUNT=2 DEFAULT=(0.,0.)
PARM PAGESIZE REAL   COUNT=2 DEFAULT=(8.5,11.)
PARM ORIENT KEYWORD  VALID=(TALL,WIDE) DEFAULT=TALL
PARM PRINT  KEYWORD  VALID=(TITLE,NOTITLE) DEFAULT=NOTITLE
PARM TITLE  (STRING,132) COUNT=(0:5) DEFAULT=--
PARM FONT   (STRING,60)  DEFAULT="Courier-Bold"
PARM POINT  INTEGER  DEFAULT=18
PARM MARGIN REAL     DEFAULT=0.
PARM FREQ   REAL     DEFAULT=60.
PARM DISPOSE KEYWORD VALID=(NOPRINT,PRINT,SAVE) DEF=PRINT
END-PROC
.TITLE
VICAR/IBIS Program PSCRIPT
.HELP
PURPOSE

      PSCRIPT converts a standard VICAR image into a text file of
POSTSCRIPT (R) commands, downloadable to a laserprinter.  The
parameters allow the choice of a 4-bit or 8-bit gray level output for a
single file, and full color for an R,G,B color triplet. PSCRIPT also
provides incremental subsampling of the image and appending a title in
any supported font. The textfile may then be edited to achieve any
additional special effects in the output, or inserted in another
POSTSCRIPT document. 

.page
EXECUTION

PSCRIPT IN.IMG OUT.PS  <PARMS> 
PSCRIPT (R,G,B) OUT.PS  <PARMS> 

 All of the parameters may be defaulted;  the resulting textfile, upon
downloading to the laserprinter, will produce an 8-bit grayscale image
for a single image, or color for an RGB triplet. The default image is
sized to be 6 inches wide, centered on the page and oriented with the
long edge of the page vertical.  By default there is no title. 
.page
OPERATION

PSCRIPT converts the input image to a standard textfile of POSTSCRIPT
commands for downloading to a laserprinter.

For Macintosh users, use the "LaserWriter Font Utility" program,
provided with the system installation disks, to directly download the
output textfile to a POSTSCRIPT laserprinter. For most "quick-look"
purposes the 4BIT mode will suffice for grayscale images; indeed, many
laser printers have only 5 bits of true grayscale, but this addtional
bit of shade requires an 8-bit coding, and so takes up twice the space.

The use of Color PostScript requires an Adobe PostScript 2.0-compatible
color printer. Check your printer manual for details.

.page

REVISION HISTORY:


Original Programmer:   Niles Ritter 	May 1988

Cognizant Programmer:  Niles Ritter	March 1993

REVISION:		1.0 May 1988	Orginal Version
			2.0 Sept 1992	Upgraded to support Color.
			3.0 Mar. 1993	Ported to Unix; Rewritten in C.
			4.0 Jun. 1993	Fixed Scaling, INC bugs & Titles.
.LEVEL1
.VARIABLE INP
The input image files.
.VARIABLE OUT
The output postscript
text file
.VARIABLE INC
Subsampling increment
.VARIABLE DNRANGE
Range of dn values.
.VARIABLE WIDTH
The image width in inches
(default = fill page)
.VARIABLE HEIGHT
The image height
(default = fill page)
.VARIABLE PAGEPOS
The x-y position on page
(default = page centered)
.VARIABLE ORIENT
Orientation of image on page.
.VARIABLE NUMBITS
Number of bits of greylevel
.VARIABLE PRINT
Keyword to print a title
.VARIABLE TITLE
Title (def = File-Name)
.VARIABLE FONT
Font for Title
.VARIABLE POINT
Pointsize for font
.VARIABLE MARGIN
Additional vertical margin
between title and image.
.VARIABLE FREQ
Number of half-tone cells
per inch for output device.
.VARIABLE PAGESIZE
Physical size of page.
.VARIABLE DISPOSE
PRINT -> print and delete output
SAVE  -> print and save output
NOPRINT-> no print, save output
.LEVEL2
.VARIABLE FONT
The name of the printer font to print title.
Many printers use the following font-styles; the printer
will only take the fontnames below EXACTLY as written:

	Courier		   Times-Roman		Helvetica
	Courier-Bold	   Times-Italic		Helvetica-Bold
	Courier-Oblique	   Times-BoldItalic	Helvetica-Oblique
	Courier-BoldOblique 			Helvetica-BoldOblique

	Helvetica-Narrow		AvantGarde-Book
	Helvetica-Narrow-Bold		AvantGarde-BookOblique
	Helvetica-Narrow-Oblique	AvantGarde-Demi
	Helvetica-Narrow-BoldOblique	AvantGarde-DemiOblique

		ZaphChancery-MediumItalic

Consult your laserwriter manual for further available fonts.

.VARIABLE DISPOSE
     Three options are available for disposition of the output.  If PRINT
(the default) is specified, the output dataset is submitted to the printer,
then deleted.  If NOPRINT is specified, the output dataset not printed.  If
SAVE is specified, the output dataset is submitted to the printer, but not 
deleted.
.END
$ Return
$!#############################################################################
