$!****************************************************************************
$!
$! Build proc for MIPL module vgrcdcopy
$! VPACK Version 1.9, Tuesday, June 04, 2013, 17:13:27
$!
$! Execute by entering:		$ @vgrcdcopy
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
$ write sys$output "*** module vgrcdcopy ***"
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
$ write sys$output "Invalid argument given to vgrcdcopy.com file -- ", primary
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
$   if F$SEARCH("vgrcdcopy.imake") .nes. ""
$   then
$      vimake vgrcdcopy
$      purge vgrcdcopy.bld
$   else
$      if F$SEARCH("vgrcdcopy.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake vgrcdcopy
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @vgrcdcopy.bld "STD"
$   else
$      @vgrcdcopy.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create vgrcdcopy.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack vgrcdcopy.com -mixed -
	-s vgrcdcopy.c decomp.c trans_eng_hdr.c vgrcdlabgen.c engrhdr.h -
	   gcfdata.h linesfx.h vgrsubcom.h uengrhdr.h ugcfdata.h uvgrsubcom.h -
	-i vgrcdcopy.imake -
	-t tstvgrcdcopy.pdf tstvgrcdcopy.log_solos -
	-p vgrcdcopy.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create vgrcdcopy.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include <sys/types.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "zvproto.h"
#include "linesfx.h"

/* Feb.2013 -lwk- changed __unix flag to SUN_SOLARIS_ARCH for selection
                  between Solaris and Linux;  had to add xvmaininc.h to
                  vgrcdlabgen.c to make that flag visible;  changed all
                  instances of "long int" to "int" for 64-bit linux. 8/

/*#ifdef __unix	*/
#if SUN_SOLARIS_ARCH
#include "uengrhdr.h"
#else
#include "engrhdr.h"
#endif

/*									*/
/*  Forward declarations for:						*/
/*									*/
/*    Huffman First-Difference decompression routines as		*/
/*      found on the PDS CD-ROM.					*/
/*									*/
void decmpinit ( int* );
void decompress ( char*, char*, int*, int*);

int trans_eng_hdr ( struct edrhdr *ptr, struct edrhdr *transed,
		int byte_size, int half_size, int byte_trans[12], int half_trans[12] );



/*									*/
/*  OK...Lets begin.							*/
/*									*/
int main ( int argc, char *argv[])  {
  char *halfbuf, *longbuf;
  char intfmt[20], realfmt[20];
  char target_name[40], current_item[40], next_item[40], last_item[40];
  int byte_trans[12], half_trans[12], full_trans[12];
  int byte_size, half_size, full_size, status;
  short int halftemp, t1, t2, t3, t4, t5, t6, t9, t10;
  unsigned short fds16, fds60;
  char t7, t8;
  int longtemp;
  short int engbuf, labellines;
  int outunit, reading_label;
  int linesize=836;
  char strbuff[836], in_line[836], out_line[836], label[80][80], sync;
  int i, j, k, numl1, numl2, vaxhist[256], hist[256], enc_hist[511], total;
  FILE *fp;
#ifdef DEBUG
  FILE *fptemp1, *fptemp2, *fptemp3;
#endif
  struct edrhdr *edrptr, *nedrptr;
  struct linesuffix *lsfxptr;
/*  struct edrlin *vline;  */
  struct vgrlineout  { char prefix[224]; char linedata[800]; } *vline;
  struct uncompline  { char linedata[800];  struct linesuffix ls; } *uline;


/*									*/
/*  Now check and make sure that the right number of parameters have	*/
/*  been entered.  Open file pointers to the files, exiting if problems	*/
/*  Also, go ahead and calloc() the needed buffers since you've		*/
/*  determined that everything is a go so far.				*/
/*  Additionally, establish the translation buffers to handle the RTL	*/
/*  way of doing VAX->UNIX translations.  This is better than relying	*/
/*  on just byte-swapping.						*/
/*									*/
if (argc < 3)  {
  printf ("VGRcdcopy decompresses a Huffman First-Difference compressed\n");
  printf (" VGR image, translating it from a PDS labeled image to a\n");
  printf (" Voyager formatted VICAR image.  The binary headers and line\n");
  printf (" prefixes are KEPT IN VAX-VMS FORMAT.  This should cause\n");
  printf (" no problems since the next program run on this image is\n");
  printf (" VGRFILLIN, which is advertised as being ported.\n");
  printf ("Usage:  VGRcdcopy <input file> <output file>\n");
  printf ("     :  <input file> is a Huffman First-Difference compressed VGR image\n");
  printf ("     :  <output file> is a VGR formatted VICAR image\n");
  exit ( 0);
}

#if VMS_OS
   if ( (fp = fopen ( argv[1], "r","ctx=stm","rfm=udf")) == NULL)  
#else
   if  ( (fp = fopen ( argv[1], "r")) == NULL)
#endif
{
  printf ("Cannot open input compressed VGR file\n");
  exit ( 0);
}

status = zvhost ( "VAX-VMS", intfmt, realfmt);

status = zvtrans_in ( byte_trans, "BYTE", "BYTE", intfmt, realfmt);
status = zvtrans_in ( half_trans, "HALF", "HALF", intfmt, realfmt);
status = zvtrans_in ( full_trans, "FULL", "FULL", intfmt, realfmt);

zvpixsize ( &byte_size, "BYTE", intfmt, realfmt);
zvpixsize ( &half_size, "HALF", intfmt, realfmt);
zvpixsize ( &full_size, "FULL", intfmt, realfmt);

status = zvunit ( &outunit, "FOO", 1, "U_NAME", argv[2], NULL);
status = zvopen ( outunit, "U_FORMAT", "BYTE", "U_NL", 800, "U_NS", 800,
                  "U_NLB", 2, "U_NBB", 224, "O_FORMAT", "BYTE",
                  "U_NB", 1, "U_ORG", "BSQ", "OP", "WRITE", NULL );

if ( !status)  {
  printf ("Cannot open output VGR formatted VICAR file\n");
  exit ( 0);
}

if ( ( edrptr = (struct edrhdr *) calloc ( 1, 1024)) == NULL)  {
  printf ("Cannot calloc memory for Eng. Data buffer\n");
  exit ( 0);
}

if ( ( nedrptr = (struct edrhdr *) calloc ( 1, 1024)) == NULL)  {
  printf ("Cannot calloc memory for Native Format Eng. Data buffer\n");
  exit ( 0);
}

if ( ( uline = (struct uncompline *)calloc ( 1, sizeof ( struct uncompline))) == NULL)  {
  printf ("Cannot calloc memory for Image line buffer\n");
  exit ( 0);
}

if ( ( vline = (struct vgrlineout *) calloc ( 1, sizeof ( struct vgrlineout))) == NULL)  {
  printf ("Cannot calloc memory for VGR Output Image line buffer\n");
  exit ( 0);
}

if ( ( halfbuf = calloc ( 1, sizeof ( short int))) == NULL)  {
  printf ("Cannot calloc memory for short int buffer\n");
  exit ( 0);
}

if ( ( longbuf = calloc ( 1, sizeof ( int))) == NULL)  {
  printf ("Cannot calloc memory for int buffer\n");
  exit ( 0);
}



/*									*/
/*  Read in label contents.  Prepare to parse out the info needed.	*/
/*  Remember to stay in sync by slurping up the single			*/
/*  byte pad that is added to the end of odd length records.		*/
/*  I have been given examples where the PDS file do NOT have variable-	*/
/*  length record sizes as the first two bytes read in (short int).	*/
/*  This leads to the software crashing as it get 'CC' (from CCSD...)	*/
/*  as the record-length count, and goes zipping off and getting con-	*/
/*  fused.  "CC" = 17219, so that's a ridiculous record length.		*/
/*  This should NEVER happen on a distributed CD-ROM, but I'll check 	*/
/*  for it just in case. 						*/
/*									*/
labellines = 0;
reading_label = 1;
while ( reading_label)  {
  fread ( halfbuf, 2, 1, fp);
  if ( strncmp ( halfbuf, "CC", 2) == 0)  {
    printf ("--->Looks like this file isn't properly formatted with variable-length\n");
    printf ("    record size delimiters.  Exiting processing\n");
    exit ( 0);
  }
  zvtrans ( half_trans, halfbuf, &halftemp, 1);
  fread ( strbuff, 1, halftemp, fp);
  strbuff[halftemp] = '\0';
  strcpy ( label[labellines], strbuff);
  if ( (float) halftemp/2 != halftemp/2)  { fread ( &sync, 1, 1, fp); }  /* syncing */
  sscanf ( label[labellines], "%s", current_item);
  if ( strcmp ( current_item, "TARGET_NAME") == 0)  {
    sscanf ( label[labellines], "%s %s %s", current_item, next_item, last_item);
    strcpy ( target_name, last_item);
  }
  labellines++;
  if ( strcmp ( strbuff, "END") == 0)  reading_label = 0;
}


/*									*/
/*  Read in image histogram.  Remember from Charlie:  they did not	*/
/*  include the 'missing lines' pixels in the 0 DN histogram count.	*/
/*  This can lead to histograms that don't sum to 640000 (800 x 800)	*/
/*									*/
/*  Remember:  I am going to write the histogram out as VAX formatted	*/
/*	ints.  Any conversions are for my own use/debugging	*/
/*									*/
for ( i = 0, numl1 = 0; i < 2; i++)  {
  fread ( halfbuf, 2, 1, fp);
  zvtrans ( half_trans, halfbuf, &halftemp, 1);
  numl2 = halftemp/4;
  for ( j = numl1; j < numl2 + numl1; j++)  {
    fread ( &vaxhist[j], 4, 1, fp);
  }
  numl1 += numl2;
}


/*									*/
/*  Read in the Huffman First-Difference histogram.			*/
/*  Remember:  DONT SWAP THE BYTES LIKE YOU WOULD EXPECT!  The PDS 	*/
/*  software supplied on the CD-ROM seems to expect VAX short ints.	*/
/*  This works even though the comments in the code state that it will	*/
/*  "automatically" swap the elements.					*/
/*									*/
/*  After populating the Huffman histogram, call the initialization	*/
/*  routine, decmpinit(), which will build the Huffman tree.		*/
/*									*/
for ( i = 0, numl1 = 0; i < 3; i++)  {
  fread ( halfbuf, 2, 1, fp);
  zvtrans ( half_trans, halfbuf, &halftemp, 1);
  numl2 = halftemp/4;
  for ( j = numl1; j < numl2 + numl1; j++)  {
    fread ( &enc_hist[j], 4, 1, fp);
/*    zvtrans ( full_trans, longbuf, &enc_hist[j], 1); */  	/* DONT SWAP THE BYTES LIKE YOU WOULD EXPECT! */
  }
  numl1 += numl2;
}
decmpinit ( enc_hist);


/*									*/
/*  Read in Engineering data record (may have odd number of bytes?)	*/
/*  Translate the Engineering data record to native format so that	*/
/*  I can properly call vgrlabgen().					*/
/*  Remember:  I will write the Eng. Data Rec. as VAX formatted stuff	*/
/*									*/
for ( i = 0; i < 1; i++)  {
  fread ( halfbuf, 2, 1, fp);
  zvtrans ( half_trans, halfbuf, &engbuf, 1);
  fread ( edrptr, 1, engbuf, fp);
  if ( (float) halftemp/2 != halftemp/2)  { fread ( &sync, 1, 1, fp); }  /* stay in sync  */
  status = zvwrit ( outunit, edrptr, "NSAMPS", 1024, NULL);
  trans_eng_hdr ( edrptr, nedrptr, byte_size, half_size, byte_trans, half_trans);
}


/*									*/
/*  NOW....we are at the start of the image.				*/
/*  Read each variable length line from the input, run it through the	*/
/*  the decompression, and write the results to the output file.	*/
/*  Remember:  each line decompresses to a buffer of 836 bytes.  The	*/
/*  extra 36 bytes are the 'line suffix'.  This info is described on	*/
/*  the CD-ROM in the file /CDROM/label/linesufx.lbl			*/
/*  Remember that the binary prefix for each line is 224 bytes long	*/
/*  This will give a total line length of 1024 bytes.			*/
/*									*/
/*  REMEMBER: I am writing out the binary headers and prefixes in	*/
/*	VAX VAX VAX VAX VAX VAX VAX VAX VAX VAX VAX VAX VAX		*/
/*  format.  This should allow VGRFILLIN to run succesfully on any	*/
/*  platform, since it is advertised as being ported.			*/
/*  This is accomplished by simply moving the bytes to the correct	*/
/*  location in the prefix.  I do not need to go through the 		*/
/*  significant pain of running all this through a structure.		*/
/*									*/
status = zvwrit ( outunit, vaxhist, "NSAMPS", 1024, NULL);
for ( i = 0; i < 800; i++)  {
  fread ( halfbuf, 2, 1, fp);
  zvtrans ( half_trans, halfbuf, &halftemp, 1);
  fread ( in_line, 1, halftemp, fp);
  if ( (float) halftemp/2 != halftemp/2)  { fread ( &sync, 1, 1, fp); }  /* stay in sync  */
  longtemp = (int) halftemp;
  decompress ( in_line, (char *)uline, &longtemp, &linesize);


  memcpy ( vline->linedata, uline->linedata, 800);
  memcpy ( &vline->prefix[22],(char *) &(uline->ls).fds_mod16_number, 2);
  memcpy ( &vline->prefix[24],(char *) &(uline->ls).fds_mod60_number, 2);
  memcpy ( &vline->prefix[26],(char *) &(uline->ls).fds_line_number, 2);
  memcpy ( &vline->prefix[120],(char *) &(uline->ls).mtis_line_number, 2);
  memcpy ( &vline->prefix[152],(char *) &(uline->ls).missing_frames, 2);
  memcpy ( &vline->prefix[174],(char *) &(uline->ls).retained_frame_bits, 20);
  memcpy ( &vline->prefix[194],(char *) &(uline->ls).input_type, 1);
  memcpy ( &vline->prefix[195],(char *) &(uline->ls).input_source, 1);
  memcpy ( &vline->prefix[220],(char *) &(uline->ls).first_sample_number, 2);
  memcpy ( &vline->prefix[222],(char *) &(uline->ls).last_sample_number, 2);

  if ( i == 0)  {
    zvtrans ( half_trans, &(uline->ls).fds_mod16_number, &fds16, 1);
    zvtrans ( half_trans, &(uline->ls).fds_mod60_number, &fds60, 1);
  }

#ifdef DEBUG
  zvtrans ( half_trans, &(uline->ls).fds_line_number, &t3, 1);
  zvtrans ( half_trans, &(uline->ls).mtis_line_number, &t4, 1);
  zvtrans ( half_trans, &(uline->ls).missing_frames, &t5, 1);
  zvtrans ( byte_trans, &(uline->ls).input_type, &t7, 1);
  zvtrans ( byte_trans, &(uline->ls).input_source, &t8, 1);
  zvtrans ( half_trans, &(uline->ls).first_sample_number, &t9, 1);
  zvtrans ( half_trans, &(uline->ls).last_sample_number, &t10, 1);
  printf ("Line: %03d has %d bytes | 1st,end samp: %d, %d | fds: %05d:%02d | input type,source %d,%d\n",
           t4, halftemp, t9, t10, fds16, fds60, t7, t8);
#endif

  status = zvwrit ( outunit, vline, "NSAMPS", 1024, NULL);
}

fclose ( fp);

vgrcdlabgen ( outunit, nedrptr, fds16, fds60, target_name, 0, 1, -1.0);
status = zvclose ( outunit, NULL);


/*									*/
/*  Here's the basic printf() stuff I need for debugging.  To turn	*/
/*  this off, just take the -DDEBUG flag out of the makefile.		*/
/*  The information printed is: the label; the histogram; the Huffman	*/
/*  First-Difference histogram; and some information from the Eng. Data	*/
/*  header.  Also some statistics are computed showing the missing	*/
/*  lines problem described above.					*/
/*									*/
#ifdef DEBUG
printf ("\n\n");
printf ("The size of edr is: %d\n\n", sizeof ( struct edrhdr));
printf ("The size of linesuffix is: %d\n\n", sizeof ( struct linesuffix));
/* printf ("The size of edrlin is: %d\n\n", sizeof ( struct edrlin));  */
printf ("The size of vgrlineout is: %d\n\n", sizeof ( struct vgrlineout));

if ( (fptemp1 = fopen ( "./edrhdr.temp", "wb")) == NULL)  {
  printf ("Cannot open temp edrhdr file\n");
  exit ( 0);
}
  
if ( (fptemp2 = fopen ( "./edrhdr.trans.temp", "wb")) == NULL)  {
  printf ("Cannot open temp edrhdr file\n");
  exit ( 0);
}
  
if ( (fptemp3 = fopen ( "./linesfx.temp", "wb")) == NULL)  {
  printf ("Cannot open temp line suffix file\n");
  exit ( 0);
}

printf ("------Writing Engineering Header (VAX fmt) edrhdr.temp----------\n");
fwrite ( edrptr, sizeof ( struct edrhdr), 1, fptemp1);
printf ("---Writing Engineering Header (translated) edrhdr.temp.trans----\n");
fwrite ( nedrptr, sizeof ( struct edrhdr), 1, fptemp2);
printf ("------Writing Line Suffix bytes (VAX fmt) linesfx.temp-------\n");
fwrite ( &(uline->ls), 36, 1, fptemp3);

fclose ( fptemp1);
fclose ( fptemp2);
fclose ( fptemp3);

printf ("------------Printing Image Histogram---------\n");
for ( i = 0, total = 0; i < 256; i++)  { 
  zvtrans ( full_trans, &vaxhist[i], &hist[i], 1);
  printf ("hist[%d]: %d\n", i, hist[i]);
  total += hist[i];
}
printf ("----------Done Printing Image Histogram---------\n");
printf ("-------Things to Note About Image Histogram-----\n");
printf ("Total: %d\n", total);
if ( total != 640000)  {
  printf ("--> Histogram count is off by %d, look for missing lines\n", 640000-total);  
}
printf ("------------Printing Huffman Encoding Histogram---------\n");
for ( i = 0, total = 0; i < 511; i++)  {
  zvtrans ( full_trans, &enc_hist[i], &longtemp, 1) ;
  printf ("enc_hist[%d]: %d\n", i, longtemp);
}
printf ("----------Done Printing Huffman Encoding Histogram---------\n");

printf ("Read %d bytes for Eng. Rec...\n", engbuf);
printf ("ENG REC->lines:  %d\n", nedrptr->sds.nlines);
printf ("ENG REC->full_lines:  %d\n", nedrptr->sds.nfull);
printf ("ENG REC->partial_lines:  %d\n", nedrptr->sds.npartial);
printf ("ENG REC->input_type:  %d\n", nedrptr->sds.input.src.type);
printf ("ENG REC->input_src:  %d\n", nedrptr->sds.input.src.unused);
printf ("ENG REC->first_fds16_count:  %d\n", nedrptr->fds_mod16);
printf ("ENG REC->first_fds60_count:  %d\n", nedrptr->fds_mod60);
printf ("ENG REC->first_fds_line_count:  %d\n", nedrptr->fds_line);
printf ("ENG REC->last_fds16_count:  %d\n", nedrptr->lfds_mod16);
printf ("ENG REC->last_fds60_count:  %d\n", nedrptr->lfds_mod60);
printf ("ENG REC->last_fds_line_count:  %d\n", nedrptr->lfds_line);
printf ("ENG REC->sds.fid.imcode:  %d\n", nedrptr->sds.fid.imcode);

printf ("------------Printing PDS Label Information--------------\n");
for ( i = 0; i < labellines; i++)  {printf ("%s\n", label[i]); }
printf ("----------Done Printing PDS Label Information--------------\n");

printf ("TARGET_NAME:  %s\n\n", target_name);

#endif

free ( edrptr);
free ( nedrptr);
free ( uline);
free ( vline);
return 0;
}  /*  end of main program	*/
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create decomp.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include <stdio.h>
#include <stdlib.h>

/****************************************************************************
*_TITLE node_def - definition of NODE type                                  *
*_ARGS NONE                                                                 *

*_DESCR node_def defines the basic element of a node used to build the      *
*       Huffman tree for data decompression.  The code declares a user      *
*       defined type that consists of an integer field and a left and right *
*       pointer field.  The *right and *left pointers point to another      *
*       NODE structure, a recursive reference of itself.  The dn field will *
*       contain a -1 if the node is not a leaf (or end node) otherwise it   *
*       will contain the pixel difference value.  This value is then        *
*       subtracted from the preceding pixel and 256 is added to normalize   *
*       the actual first difference value.  The left and right pointers are *
*       undefined (NULL) if the node is a leaf, otherwise they will point   *
*       to the next adjacent node in the tree.                              *

*_LIMS  This definition has been tested on VAX 750 (VMS 4.7), DEC MicroVAX  *
*       (ULTRIX 2.2), SUN Workstation (UNIX 4.2, release 3.4), and an       *
*       IBM PC (MICROSOFT 4.0, 3.0 compilers).  When converting to other    *
*       systems, check for portability conflicts.                           *

*_HIST  17-FEB-88  Kris Becker  USGS, Flagstaff Original Version            *
*_END                                                                       *
*****************************************************************************/

  typedef struct leaf 
              {
                struct leaf *right;
                short int dn;
                struct leaf *left;
               } NODE;


/*************************************************************************
 Declare the tree pointer. This pointer will hold the root of the tree
 once the tree is created by the accompanying routine huff_tree.
**************************************************************************/
  NODE *tree;


 void decompress(ibuf,obuf,nin,nout)
/****************************************************************************
*_TITLE decompress - decompresses image lines stored in compressed format   *
*_ARGS  TYPE       NAME      I/O        DESCRIPTION                         */
        char       *ibuf;  /* I         Compressed data buffer              */
        char       *obuf;  /* O         Decompressed image line             */
        int   *nin;   /* I         Number of bytes on input buffer     */
        int   *nout;  /* I         Number of bytes in output buffer    */

/*
*_DESCR This routine decompresses Huffman encoded compressed data.  Huffman *
*       coding uses a variable number of bits to encode different values of *
*       the original data.  The compression results because the most        *
*       frequently occurring data value is represented in the smallest      *
*       number of bits.  This routine is called by a main program to        *
*       decompress an image line contained in ibuf.  The first byte of      *
*       ibuf contains the actual first pixel value of the line.  All        *
*       other bytes in the input line contain the compressed data.  The     *
*       output buffer, obuf, will contain the decompressed image line       *
*       after the call to the "dcmprs" routine.                             *

*_LIMS  This routine has been tested on VAX 750 (VMS 4.6), DEC MicroVAX     *
*       (ULTRIX 2.2), SUN Workstation (UNIX 4.2, release 3.4), and an       *
*       IBM PC (MICROSOFT 4.0, 3.0 compilers).  When converting to other    *
*       systems, check for portability conflicts.                           *

*       Please note that the calling convention used for this routine       *
*       follow VAX/VMS FORTRAN pass-by-reference standards to allow linking *
*       to FORTRAN calling routines.  C programmers must be aware of this   *
*       when coding calling routines.                                       *

*_HIST  17-FEB-88 Kris Becker USGS, Flagstaff Original C Version            *
*_END                                                                       *
*****************************************************************************/


  {
 /* The external root pointer to tree */
    extern NODE *tree;

 /* Declare functions called from this routine */
    void dcmprs();


/*************************************************************************
  This routine is fairly simple as it's only function is to call the 
  routine dcmprs. 
**************************************************************************/

    dcmprs(ibuf,obuf,nin,nout,tree);
  
    return;
  }



void decmpinit(hist)
/***************************************************************************
*_TITLE decmpinit - initializes the Huffman tree                           *
*_ARGS  TYPE       NAME      I/O        DESCRIPTION                        */
        int   *hist;  /* I         First-difference histogram.  This  *
*                                       array MUST be dimensioned to at    *
*                                       least 511 elements.  There are a   *
*                                       total of 511 first-differerence    *
*                                       values.  The least first-          *
*                                       difference value is 0-255 or -255, *
*                                       while the largest first-difference *
*                                       is 255-0 or 255.  The first-       *
*                                       difference values are normalized   *
*                                       for table use to the range 1 to    *
*                                       511 by adding 256 to each          *
*                                       difference.                        */

/*
*_DESCR This routine is relatively simple; it is responsible for creating  *
*       the Huffman tree from the first difference histogram of the image. *
*       In a first difference histogram, the first byte in each image line *
*       is the actual pixel value.  All other pixels are obtained by       *
*       subtracting the first difference value at the current pixel from   *
*       the actual value of the preceding pixel and adding 256 to provide  *
*       a positive number.  The I-th element of the array "hist" should be *
*       the frequency of occurances for the value I.  Note the declaration *
*       of the pointer tree.  This external variable is defined by this    *
*       routine.  It returns the pointer to the root of the Huffman tree   *
*       created by "huff_tree". The huff_tree routine will automatically   *
*       swap the low and high order bytes of the 32-bit elements in the    *
*       of the first difference histogram for the computer systems which   *
*       store integers in "most significant byte first" order. For computer*
*       systems which store 32-bit words in "least significant byte first  *
*       order, no swapping of the bytes occurs.


*_LIMS  This routine has been tested on VAX 750 (VMS 4.6), DEC MicroVAX    *
*       (ULTRIX 2.2), SUN Workstation (UNIX 4.2, release 3.4), and an      *
*       IBM PC (MICROSOFT 4.0, 3.0 compilers).  When converting to other   *
*       systems, check for portability conflicts.                          *

*_HIST  17-FEB-88  Kris Becker  USGS, Flagstaff Original C Version         *
*_END                                                                      *
****************************************************************************/

{
  extern NODE *tree;          /* Huffman tree root pointer */

  /* Specify the calling function to initialize the tree */
  NODE *huff_tree(int *);

/****************************************************************************
  Simply call the huff_tree routine and return.
*****************************************************************************/

  tree = huff_tree(hist);

  return;
 }





NODE *huff_tree(hist)
/****************************************************************************
*_TITLE huff_tree - constructs the Huffman tree; returns pointer to root    *
*_ARGS  TYPE          NAME        I/O   DESCRIPTION                         */
        int     *hist;     /* I    First difference histogram          */
/*
*_DESC  huff_tree constructs a binary Huffman tree and returns the pointer  *
*       to the root of the tree.  The implementation here may not be the    *
*       most efficient, but conditions of the algorithm used to compress    *
*       the data governed the design of this algorithm.  Other              *
*       implementations are available in FORTRAN and VAX MACRO Assembler.   *
*       This routine allocates memory as needed to construct the tree.      *
*       The tree is implemented as a user defined structure described       *
*       above.  The algorithm uses an array of node pointers allocated      *
*       for all possible values.  This array is then initialized by         *
*       assigning all leafs to the array.  Each leaf has a cooresponding    *
*       frequency assigned to it and the frequencies are sorted in ascending*
*       order.  All zero frequencies are ignored and tree construction      *
*       begins.  The tree is built by combining the two least occuring      *
*       frequencies into one node.  This new node is treated as one by      *
*       adding together the two frequencies forming a cummulative frequency *
*       of the combining nodes.  The second smallest node now contains the  *
*       newly combined node and the smallest node is deleted from the list. *
*       The frequency list is then resorted to determine the next two node  *
*       combinations until one node is left.  This node will be the root of *
*       the tree.  This pointer is then returned to the calling routine.    *


*_LIMS  This routine has been tested on VAX 750 (VMS 4.6), DEC MicroVAX     *
*       (ULTRIX 2.2), SUN Workstation (UNIX 4.2, release 3.4), and an       *
*       IBM PC (MICROSOFT 4.0, 3.0 compilers).  When converting to other    *
*       systems, check for portability conflicts.                           *

*       This routine uses the memory allocation routine "malloc".  Check    *
*       for call specifications and casting portability of these features   *
*       for the compiler in used.                                           *

*_HIST  17-FEB-88  Kris Becker  USGS, Flagstaff Original C Version          *
*_END                                                                       *
*****************************************************************************/

  {
  /*  Local variables used */
    int freq_list[512];      /* Histogram frequency list */
    NODE **node_list;             /* DN pointer array list */
    
    register int *fp;        /* Frequency list pointer */
    register NODE **np;           /* Node list pointer */

    register int num_freq;   /* Number non-zero frequencies in histogram */
    int sum;                 /* Sum of all frequencies */

    register short int num_nodes; /* Counter for DN initialization */
    register short int cnt;       /* Miscellaneous counter */

    short int znull = -1;         /* Null node value */
    
    register NODE *temp;          /* Temporary node pointer */

  /* Functions called */
    void sort_freq();
    NODE *new_node();

/***************************************************************************
  Allocate the array of nodes from memory and initialize these with numbers
  corresponding with the frequency list.  There are only 511 possible 
  permutations of first difference histograms.  There are 512 allocated 
  here to adhere to the FORTRAN version.
****************************************************************************/

   fp = freq_list;
   node_list = (NODE **) malloc(sizeof(temp)*512);
   if (node_list == NULL)
    {
      printf("\nOut of memory in huff_tree!\n");
      exit(1);
    }
   np = node_list;

   for (num_nodes=1, cnt=512 ; cnt-- ; num_nodes++)
     {
/**************************************************************************
    The following code has been added to standardize the VAX byte order
    for the "int" type.  This code is intended to make the routine
    as machine independant as possible.
***************************************************************************/
        unsigned char *cp = (unsigned char *) hist++;
        unsigned int j;
        short int i;
        for (i=4 ; --i >= 0 ; j = (j << 8) | *(cp+i));

/* Now make the assignment */
        *fp++ = j;
        temp = new_node(num_nodes);
        *np++ = temp;
     }

     (*--fp) = 0;         /* Ensure the last element is zeroed out.  */

/***************************************************************************
  Now, sort the frequency list and eliminate all frequencies of zero.
****************************************************************************/

  num_freq = 512;
  sort_freq(freq_list,node_list,num_freq);

  fp = freq_list;
  np = node_list;

  for (num_freq=512 ; (*fp) == 0 && (num_freq) ; fp++, np++, num_freq--);


/***************************************************************************
  Now create the tree.  Note that if there is only one difference value,
  it is returned as the root.  On each interation, a new node is created
  and the least frequently occurring difference is assigned to the right
  pointer and the next least frequency to the left pointer.  The node 
  assigned to the left pointer now becomes the combination of the two
  nodes and it's frequency is the sum of the two combining nodes.
****************************************************************************/

  for (temp=(*np) ; (num_freq--) > 1 ; )
    {
        temp = new_node(znull);
        temp->right = (*np++);
        temp->left = (*np);
        *np = temp;
        *(fp+1) = *(fp+1) + *fp;
        *fp++ = 0;
        sort_freq(fp,np,num_freq);
    }
  
  return temp;
 }




NODE *new_node(value)
/****************************************************************************
*_TITLE new_node - allocates a NODE structure and returns a pointer to it   *
*_ARGS  TYPE        NAME        I/O     DESCRIPTION                         */
        short int   value;    /* I      Value to assign to DN field         */

/*
*_DESC  new_node allocates virtual memory for a new NODE structure.  It     *
*       initializes the right and left pointers to NULL and the dn field    *
*       is initialized to the passed parameter 'value'.                     *


*_LIMS  This routine has been tested on VAX 750 (VMS 4.6), DEC MicroVAX     *
*       (ULTRIX 2.2), SUN Workstation (UNIX 4.2, release 3.4), and an       *
*       IBM PC (MICROSOFT 4.0, 3.0 compilers).  When converting to other    *
*       systems, check for portability conflicts.                           *

*       This routine uses the malloc routine that requests virtual memory   *
*       from the system.  Most C libraries have some version of this        *
*       function.  Check the reference manuals for compatibility.           *

*_HIST  17-FEB-88  Kris Becker  USGS, Flagstaff Original Version            *
*_END                                                                       *
*****************************************************************************/

  {
    NODE *temp;         /* Pointer to the memory block */


/***************************************************************************
  Allocate the memory and intialize the fields.
****************************************************************************/

  temp = (NODE *) malloc(sizeof(NODE));

  if (temp != NULL) 
    {
      temp->right = NULL;
      temp->dn = value;
      temp->left = NULL;
    }
  else
    {
       printf("\nOut of memory in new_node!\n");
       exit(1);
    }

   return temp;
  }



 void sort_freq(freq_list,node_list,num_freq)
/****************************************************************************
*_TITLE sort_freq - sorts frequency and node lists in increasing freq. order*
*_ARGS  TYPE       NAME            I/O  DESCRIPTION                         */
        int   *freq_list;   /* I   Pointer to frequency list           */
        NODE       **node_list;  /* I   Pointer to array of node pointers   */
        int   num_freq;     /* I   Number of values in freq list       */

/*
*_DESCR This routine uses an insertion sort to reorder a frequency list     *
*       in order of increasing frequency.  The corresponding elements       *
*       of the node list are reordered to maintain correspondence.  The     *
*       node list is actually a pointer to an array of pointers to tree     *
*       nodes.

*_LIMS  This routine has been tested on VAX 750 (VMS 4.6), DEC MicroVAX     *
*       (ULTRIX 2.2), SUN Workstation (UNIX 4.2, release 3.4), and an       *
*       IBM PC (MICROSOFT 4.0, 3.0 compilers).  When converting to other    *
*       systems, check for portability conflicts.                           *

*_HIST  17-FEB-88 Kris Becker USGS, Flagstaff Original C Version            *
*_END                                                                       *
*****************************************************************************/
  {
    /* Local Variables */
    register int *i;       /* primary pointer into freq_list */
    register int *j;       /* secondary pointer into freq_list */
    
    register NODE **k;          /* primary pointer to node_list */
    register NODE **l;          /* secondary pointer into node_list */

    int temp1;             /* temporary storage for freq_list */
    NODE *temp2;                /* temporary storage for node_list */

    register int cnt;      /* count of list elements */


/************************************************************************
  Save the current element - starting with the second - in temporary
  storage.  Compare with all elements in first part of list moving 
  each up one element until the element is larger.  Insert current 
  element at this point in list.
*************************************************************************/

   if (num_freq <= 0) return;      /* If no elements or invalid, return */

   for (i=freq_list, k=node_list, cnt=num_freq ; --cnt ; *j=temp1, *l=temp2)
     {
        temp1 = *(++i);
        temp2 = *(++k);

        for (j = i, l = k ;  *(j-1) > temp1 ; )
          {
            *j = *(j-1);
            *l = *(l-1);
            j--;
            l--;
            if ( j <= freq_list) break;
          }
 
     }
  return;
  }




 void dcmprs(ibuf,obuf,nin,nout,root)
/****************************************************************************
*_TITLE dcmprs - decompresses Huffman coded compressed image lines          *
*_ARGS  TYPE       NAME       I/O       DESCRIPTION                         */
        char       *ibuf;   /* I        Compressed data buffer              */
        char       *obuf;   /* O        Decompressed image line             */
        int   *nin;    /* I        Number of bytes on input buffer     */
        int   *nout;   /* I        Number of bytes in output buffer    */
        NODE       *root;   /* I        Huffman coded tree                  */

/*
*_DESCR This routine follows a path from the root of the Huffman tree to    *
*       one of it's leaves.  The choice at each branch is decided by the    *
*       successive bits of the compressed input stream.  Left for 1, right  *
*       for 0.  Only leaf nodes have a value other than -1.  The routine    *
*       traces a path through the tree until it finds a node with a value   *
*       not equal to -1 (a leaf node).  The value at the leaf node is       *
*       subtracted from the preceeding pixel value plus 256 to restore      *
*       the uncompressed pixel.  This algorithm is then repeated until the  *
*       entire line has been processed.                                     *


*_LIMS  This routine has been tested on VAX 750 (VMS 4.6), DEC MicroVAX     *
*       (ULTRIX 2.2), SUN Workstation (UNIX 4.2, release 3.4), and an       *
*       IBM PC (MICROSOFT 4.0, 3.0 compilers).  When converting to other    *
*       systems, check for portability conflicts.                           *

*       Please note that the calling convention used for these routines     *
*       follow VAX/VMS FORTRAN pass-by-reference standards to allow         *
*       linking to FORTRAN calling routines.  C programmers must be aware   *
*       of this when coding calling routines.                               *

*_HIST  17-FEB-88 Kris Becker USGS, Flagstaff Original C Version            *
*_END                                                                       *
*****************************************************************************/
  {
    /* Local Variables */
    register NODE *ptr = root;        /* pointer to position in tree */
    register unsigned char test;      /* test byte for bit set */
    register unsigned char idn;       /* input compressed byte */

    register char odn;                /* last dn value decompressed */

    char *ilim = ibuf + *nin;         /* end of compressed bytes */
    char *olim = obuf + *nout;        /* end of output buffer */



/**************************************************************************
  Check for valid input values for nin, nout and make initial assignments.
***************************************************************************/

    if (ilim > ibuf && olim > obuf)
       odn = *obuf++ = *ibuf++;
    else
       {
           printf("\nInvalid byte count in dcmprs!\n");
           exit(1);
       }

/**************************************************************************
  Decompress the input buffer.  Assign the first byte to the working 
  variable, idn.  An arithmatic and (&) is performed using the variable
  'test' that is bit shifted to the right.  If the result is 0, then
  go to right else go to left.
***************************************************************************/

    for (idn=(*ibuf) ; ibuf < ilim  ; idn =(*++ibuf))
     {
        for (test=0x80 ; test ; test >>= 1)
           {
            ptr = (test & idn) ? ptr->left : ptr->right;

            if (ptr->dn != -1) 
              {
                if (obuf >= olim) return;
                odn -= ptr->dn + 256;
                *obuf++ = odn;
                ptr = root;
              }
          }
     }
   return;
  }

void free_tree(nfreed)
/****************************************************************************
*_TITLE free_tree - free memory of all allocated nodes                      *
*_ARGS  TYPE       NAME       I/O        DESCRIPTION                        */
        int   *nfreed;  /* O        Return of total count of nodes     *
*                                        freed.                             */

/*
*_DESCR This routine is supplied to the programmer to free up all the       *
*       allocated memory required to build the huffman tree.  The count     *
*       of the nodes freed is returned in the parameter 'nfreed'.  The      *
*       purpose of the routine is so if the user wishes to decompress more  *
*       than one file per run, the program will not keep allocating new     *
*       memory without first deallocating all previous nodes associated     *
*       with the previous file decompression.                               *

*_HIST  16-AUG-89 Kris Becker   USGS, Flagstaff Original Version            *
*_END                                                                       *
****************************************************************************/

{
	int total_free = 0;

	extern NODE *tree;      /* Huffman tree root pointer */

/* Specify the function to free the tree */
	int free_node();
/****************************************************************************
  Simply call the free_node routine and return the result.
*****************************************************************************/

	*nfreed = free_node(tree,total_free);

	return;
}

int free_node(pnode,total_free)
/***************************************************************************
*_TITLE free_node - deallocates an allocated NODE pointer
*_ARGS  TYPE     NAME          I/O   DESCRIPTION                           */
        NODE     *pnode;       /* I  Pointer to node to free               */
        int total_free;   /* I  Total number of freed nodes           */

/*
*_DESCR  free_node will check both right and left pointers of a node       *
*        and then free the current node using the free() C utility.        *
*        Note that all nodes attached to the node via right or left        *
*        pointers area also freed, so be sure that this is the desired     *
*        result when calling this routine.                                 *

*        This routine is supplied to allow successive calls to the         *
*        decmpinit routine.  It will free up the memory allocated          *
*        by previous calls to the decmpinit routine.  The call to free     *
*        a previous huffman tree is:  total = free_node(tree,(long) 0);    *
*        This call must be done by the programmer application routine      *
*        and is not done by any of these routines.                         *
*_HIST   16-AUG-89  Kris Becker U.S.G.S  Flagstaff Original Version        */
{
	if (pnode == (NODE *) NULL) return(total_free);
	
	if (pnode->right != (NODE *) NULL)
		total_free = free_node(pnode->right,total_free);
	if (pnode->left != (NODE *) NULL)
		total_free = free_node(pnode->left,total_free);

	free((char *) pnode);
	return(total_free + 1);
}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create trans_eng_hdr.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/*  This routine will translate the Engr. Data Binary Hdr (found in the	*/
/*  Voyager PDS CD-ROMs) and convert it to s structure that is in the 	*/
/*  native machine format.  This is necessary so that vgrcdlabgen()	*/
/*  will correctly interpret the information to create the VGR VICAR 	*/
/*  label.  								*/
/*  Myche McAuley  6/95							*/
/*									*/
#include <sys/types.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "zvproto.h"

/*#ifdef __unix		wrong flag! */
#if SUN_SOLARIS_ARCH
#include "uengrhdr.h"
#else
#include "engrhdr.h"
#endif


int trans_eng_hdr ( struct edrhdr* ptr, struct edrhdr* transed,
		    int byte_size, int half_size, int byte_trans[12], int half_trans[12])  {

  unsigned char *curloc;

  curloc = (unsigned char *) ptr;

  zvtrans ( byte_trans, curloc, &(transed->recid), 1); curloc += byte_size;
  zvtrans ( byte_trans, curloc, &(transed->fileno), 1); curloc += byte_size;
  zvtrans ( half_trans, curloc, &(transed->phys_seq_no), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->log_seq_no), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->ert), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->ert_min), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->ert_msec), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->lert), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->lert_min), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->lert_msec), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->fds_mod16), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->fds_mod60), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->fds_line), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->lfds_mod16), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->lfds_mod60), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->lfds_line), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->scet), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->scet_min), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->scet_msec), 1); curloc += half_size;
  zvtrans ( byte_trans, curloc, &(transed->system_version), 32); curloc += byte_size * 32;
  zvtrans ( half_trans, curloc, &(transed->gcf[1].sync_code_msb), 1); curloc += half_size;
  zvtrans ( byte_trans, curloc, &(transed->gcf[1].source_station), 1); curloc += byte_size;
  zvtrans ( byte_trans, curloc, &(transed->gcf[1].sync_code_lsb), 1); curloc += byte_size;
  zvtrans ( byte_trans, curloc, &(transed->gcf[1].block_format), 1); curloc += byte_size;
  zvtrans ( byte_trans, curloc, &(transed->gcf[1].destination_code), 1); curloc += byte_size;
  zvtrans ( half_trans, curloc, &(transed->gcf[1].gddudt.word), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->gcf[1].s1), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->gcf[1].time_lsb), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->gcf[1].s2), 1); curloc += half_size;
  zvtrans ( byte_trans, curloc, &(transed->gcf[1].msec_clock), 1); curloc += byte_size;
  zvtrans ( byte_trans, curloc, &(transed->gcf[1].serial_number), 1); curloc += byte_size;
  zvtrans ( half_trans, curloc, &(transed->gcf[1].dsn.word), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->gcf[1].esc), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->gcf[2].sync_code_msb), 1); curloc += half_size;
  zvtrans ( byte_trans, curloc, &(transed->gcf[2].source_station), 1); curloc += byte_size;
  zvtrans ( byte_trans, curloc, &(transed->gcf[2].sync_code_lsb), 1); curloc += byte_size;
  zvtrans ( byte_trans, curloc, &(transed->gcf[2].block_format), 1); curloc += byte_size;
  zvtrans ( byte_trans, curloc, &(transed->gcf[2].destination_code), 1); curloc += byte_size;
  zvtrans ( half_trans, curloc, &(transed->gcf[2].gddudt.word), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->gcf[2].s1), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->gcf[2].time_lsb), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->gcf[2].s2), 1); curloc += half_size;
  zvtrans ( byte_trans, curloc, &(transed->gcf[2].msec_clock), 1); curloc += byte_size;
  zvtrans ( byte_trans, curloc, &(transed->gcf[2].serial_number), 1); curloc += byte_size;
  zvtrans ( half_trans, curloc, &(transed->gcf[2].dsn.word), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->gcf[2].esc), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->irt), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->irt_min), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->irt_msec), 1); curloc += half_size;
  zvtrans ( byte_trans, curloc, &(transed->tlm_mode), 1); curloc += byte_size;
  zvtrans ( byte_trans, curloc, &(transed->unused), 1); curloc += byte_size;
  zvtrans ( half_trans, curloc, &(transed->sds.unused1), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->sds.fid), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->sds.system_noise_temp_min), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->sds.system_noise_temp_max), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->sds.symbol_snr_min), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->sds.symbol_snr_max), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->sds.agc_min), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->sds.agc_max), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->sds.pn_errs), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->sds.fds_count_errs), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->sds.sync_pars), 3); curloc += half_size * 3;
  zvtrans ( half_trans, curloc, &(transed->sds.nlines), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->sds.nfull), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->sds.npartial), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->sds.nbadrec), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->sds.nlog_seq_breaks), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->sds.sort_par), 4); curloc += half_size * 4;
  zvtrans ( half_trans, curloc, &(transed->sds.nmf_from_idr), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->sds.nmf_from_wbdl), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->sds.nmf_from_sdr), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->sds.nmf_missing), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->sds.unused2), 1); curloc += half_size;
  zvtrans ( byte_trans, curloc, &(transed->sds.picno), 10); curloc += byte_size * 10;
  zvtrans ( byte_trans, curloc, &(transed->sds.target_body), 10); curloc += byte_size * 10;
  zvtrans ( half_trans, curloc, &(transed->sds.input.source), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->subcom.subword1), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->subcom.subword2), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->subcom.subword3), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->subcom.picture_count), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->subcom.parword_a), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->subcom.parword_a_ind), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->subcom.parword_a_ptr), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->subcom.parword_b), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->subcom.parword_b_ptr), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->subcom.parword_c), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->subcom.parword_c_ptr), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->subcom.parword_d), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->subcom.parword_d_ind), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->subcom.parword_d_ptr), 1); curloc += half_size;
  zvtrans ( byte_trans, curloc, &(transed->subcom.na_sample1), 10); curloc += byte_size * 10;
  zvtrans ( half_trans, curloc, &(transed->subcom.word20.word), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->iss_engineering), 5); curloc += half_size * 5;
  zvtrans ( byte_trans, curloc, &(transed->nept_byte), 1); curloc += byte_size;
  zvtrans ( byte_trans, curloc, &(transed->unused5), 5); curloc += byte_size * 5;

  return 1;

}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create vgrcdlabgen.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/*										*/
/*  vgrcdlabgen.c								*/
/*										*/
/*  This routine takes a (native format) Engineering Data Record from the 	*/
/*  compressed PDS image CD-ROM and parses out the necessary information to	*/
/*  create the Voyager VICAR label.  This binary header information is stored	*/
/*  on the PDS CD's in VAX format.  For this routine to work correctly, 	*/
/*  this header MUST be translated into a format understandable by the native	*/
/*  machine.  This translation is performed by the trans_eng_hdr() routine.	*/
/*										*/
/*  The original code for vgrcdlabgen() was derived from the VICAR subroutine	*/
/*  vgrlabgen() which worked under VMS only.  This package is designed to allow	*/
/*  for the creation of VGR VICAR images from the PDS CD-ROMs on other 		*/
/*  platforms: namely UN*X and VMS.  Additionally, I should note that all of 	*/
/*  the 'unnecessary' routines were replaced by sprintf() and strncpy().  These	*/
/*  routines were: AUTCON, OUTCON, MVL, BSWAP, ITLA, HEXCON, and TIMEOUT.	*/
/*  The above routines simply formatted numbers for textual representation 	*/
/*  now done by sprintf) and copied bytes around (now done by strncpy and 	*/
/*  memcpy).									*/
/*  										*/
/*  As a last note:  since this is designed to get its input data from the PDS	*/
/*  CD-ROMs, I took advantage of the fact that the fds counts and target name	*/
/*  dont have to be found using sources other than the binary header.  In the	*/
/*  'old' days, the fds counts were retrieved from the binary *prefix* and the	*/
/*  target name was derived from the binary *header*.  Since the target name	*/
/*  is available in ASCII in the PDS label, I just get it and pass it into this	*/
/*  routine as 'targname'.  This should be fine assuming that the PDS label is 	*/
/*  correct.  The advantage in doing this is that I do not have to use zpbname()*/
/*  to lookup the target name.  As for the fds counts, I would have to grok a 	*/
/*  VGR binary line prefix.  I dont want to bother with passing a pointer to a 	*/
/*  structure that is mostly empty anyway.  I have the two numbers from earlier	*/
/*  processing, so I'll just pass them in here.					*/
/*										*/
/*  I guess it should be obvious that this is not you fathers' Oldsmobile ;-)	*/
/*										*/
#include <stdio.h>
#include "vgrimcod.h"   	/* telemery codes */
#include <string.h>
#include "xvmaininc.h"	/* for SUN_SOLARIS_ARCH */

/*#ifdef __unix		/* wrong flag */
#if SUN_SOLARIS_ARCH
#include "uengrhdr.h"
#else
#include "engrhdr.h"
#endif


int check_ind ( int location, int ind) {
char alpha_buf[80];
  if ( ind != 1) {
    sprintf ( alpha_buf," VGRCDLABGEN -- ERROR %d AT LOCATION %d \n",ind,location);
    printf  ( alpha_buf);
    return 1;			/*  keep the compilers happy with a return value*/
  }
}

vgrcdlabgen ( ounit, edr, mod16, mod60, targname, sedrflg, print, exposure)

  int ounit;			/*  VICAR file unit number			*/
  struct edrhdr *edr;		/*  pointer to native format binary header	*/
  unsigned short mod16, mod60;	/*  FDS counts found in previous processing	*/
  char *targname;		/*  obtained from the PDS label on the CD	*/
  int sedrflg, print;		/*  flags to use SEDR (usually NOT) and prints	*/
  float exposure;		/*  usually set to -1.0 to force a table lookup	*/

{
/*                       1         2         3         4         5         6         7
               0123456789012345678901234567890123456789012345678901234567890123456789012 */ 
char v01[73]= "                     800     800 800 800 L 1                          SC";
char v02[73]= "VGR-*   FDS *****.**   PICNO **********   SCET **.*** **:**:**         C";
char v03[73]= "xA CAMERA  EXP *******.* MSEC FILT x(xxxxxx)  xx GAIN  SCAN RATE **:1  C";
char v04[73]= "ERT **.*** **:**:**   */** ******* RES   VIDICON TEMP ****.** DEG C    C";
char v05[73]= "IN/xxxxxx/xx OUT/xxxxxx/xx     GANYMEDE    DSS #**   BIT SNR ****.***  C";
char v06[73]= " xxxxx A/xxxxxxxx B/xxxx C/xxxx D/xxxxxxxx ETLM/xxxxxxxxxxxxxxxxxxxxS AC";
char v07[73]= "NA OPCAL xx(*******.*MSEC)PIXAVG ***/* OPERATIONAL MODE *(******)     AC";
char v08[73]= "CAM ECAL CYCLE BEAM  RESET OPEN  CLOSE FLOOD AEXPM  FIL G1 SHUT MODE  AC";
char v09[73]= "NA   xxx  xxxx  xxx   xxx   xxx   xxx   xxx   xxx   x x  x xxxxxxxxxx AC";
char v10[73]= "WA   xxx  xxxx  xxx   xxx   xxx   xxx   xxx   xxx   x x  x xxxxxxxxxx AC";
char v11[73]= "LSB_TRUNC=XXX  TLM_MODE=XXXX  COMPRESSION=XXX                          C";
char v12[73]= "INCIDENCE ANGLE ***.*     EMISSION ANGLE ***.*    PHASE ANGLE ***.*    C";
char v13[73]= "NORTH AZIMUTH ANGLE ***.*     KM/LINE *****.**      KM/SAMP *****.**   C";
char v14[73]= "ALT ******** KM SL.RANGE ******** KM VFOV ******* KM HFOV ******* KM   C";
char v15[73]= "LAT  ***(UL) ***(UR) ***(LL) ***(LR) ***(C) ***(SUB S/C) ***(SUBSOL)   C";
char v16[73]= "LONG ***(UL) ***(UR) ***(LL) ***(LR) ***(C) ***(SUB S/C) ***(SUBSOL)   C";
/*                       1         2         3         4         5         6         7
               0123456789012345678901234567890123456789012345678901234567890123456789012 */ 
/*******************************************************************/
/* NOTE: The last label record output should have an "L" in cc 72  */
/*******************************************************************/

float expo[24] = {    5.0,    7.5,   12.5,   15.0,   22.5,   30.0,
		     45.0,   60.0,   90.0,  120.0,  180.0,  240.0,
		    360.0,  480.0,  720.0,  960.0, 1440.0, 1920.0,
		   2880.0, 3840.0, 5760.0, 7680.0,11520.0,15360.0};

float expo_new[31] =   {      5.0,    15.0,    90.0,   120.0,   180.0,
			    240.0,   360.0,   480.0,   720.0,   960.0,
			   1440.0,  1920.0,  2880.0,  3840.0,  5760.0,
			   7680.0, 11520.0, 15360.0,   960.0,  1440.0,
			   1920.0,  2880.0,  3840.0,  5760.0,  7680.0,
			  11520.0, 15360.0, 23040.0, 30720.0, 46080.0,
			  61440.0};

char fpname[2][8][7] = {
	"CH4/JS ","BLUE   ","CLEAR  ","VIOLET ",	/* WA FILTERS */
	"SODIUM ","GREEN  ","CH4/U  ","ORANGE ",

	"CLEAR  ","VIOLET ","BLUE   ","ORANGE ",	/* NA FILTERS */
	"CLEAR  ","GREEN  ","GREEN  ","UV     " };

char mtbl[8][6] = {
	"NOSHUT","NOSHUT","NAONLY","WAONLY",		/* CAMERA MODES */
	"BOTALT","BSIMAN","BODARK","BOTSIM" };

char shut_mode[4][11] = {"NORMAL    ","LONG START",
			"LONG END  ","LONG OPEN "};

char yesno[2][3]={ "YES","NO " };

char ascii[16] = {'0','1','2','3','4','5','6','7',
		  '8','9','0','A','B','C','D','F'};

short edit[32] = {					/* CAMERA EDIT & RESOLUTION*/
  	101,0,0,0,101,0,305,0,101,101,0,203,101,0,0,101,0,101,
   /*   ------ JUN 7 94   IM7 & 9 replaces GS4B & 4C      */
   /*   101,0,0,0,101,0,305,0,101,101,0,101,101,0,0,101,0,101,  */
	-110,0,102,101,-105,0,103,0,102,101,304,101,101,101 };
		/* e.g. -105 is 1/5 EDIT PARTIAL RES */

union parword_ind parword_ind;

short date[6];
short ss[10],ns[10];
int nlab;
int imcode,icamera;
int expcd,filter,gain,scan_rate,res;
int year,day,minute,msec;
int source,mrk3_id,mrk4_id;
int i,l,n,ind,mode,pixel_average,g1_voltage;
char station_name[13];
float exp,noise;
float vidicon_temp = -80.0;	/* Dummy value for vidicon temp */
char format[6],buf[8], temp[80];
int lsb_trunc;                          /* lsb truncation flag */

  /* Spacecraft ID: VGR-1 or VGR-2     */
  if ( edr->sds.fid.sc_id ) v02[4]='1';
    else v02[4]='2';

  /* Spacecraft Clock		     */
  sprintf ( temp, "%d.%02d", mod16, mod60);
  strncpy ( &v02[12], temp, 8);

  /* Picture No,e.g."1234U-234" */
  if (edr->sds.picno[0] != 0) strncpy ( &v02[29], &edr->sds.picno[0], 10);

  /* Spacecraft Event Time	     */
  if ((sedrflg) || (edr->scet.day_year.year != 0) || (edr->scet.day_year.day != 0) || (edr->scet_min != 0)  || (edr->scet_msec != 0))  {
    date[0] = edr->scet.day_year.year;
    date[1] = edr->scet.day_year.day;
    minute = edr->scet_min;
    date[2] = minute/60;
    date[3] = minute%60;
    msec = edr->scet_msec;
    date[4] = msec/1000;
    date[5] = msec%1000;
  sprintf ( temp, "%02d.%03d %02d:%02d:%02d", date[0], date[1], date[2], date[3], date[4]);
  strncpy ( &v02[47], temp, 15);
  }

  /* Camera ID */
  imcode = edr->sds.fid.imcode;
  icamera = edr->subcom.subword1.camera_id;
  if ( imcode == GS4 )  strncpy ( &v03[0], "PWS      ", 9);
  else if ( imcode == GS2 )  strncpy ( &v03[0], "PRA      ", 9);
  else if ( icamera == 0 )  strncpy ( &v03[0], "WA CAMERA", 9);
  else strncpy ( &v03[0], "NA CAMERA", 9);

  expcd = edr->subcom.subword3.exposure;	/* Exposure time */
  if ( exposure < 0.0 )  {			/*  Force a table lookup  */
    if ( edr->subcom.subword3.exposure_table == 1 )  {
      exp = expo_new[expcd-1];			/* in milliseconds */
      if ( expcd > 0 && expcd < 32 )  {
        sprintf ( temp, "%9.1f", exp);
        strncpy ( &v03[15], temp, 9);
      }
    }
    else  {
      exp = expo[expcd-1];			/* in milliseconds */
      if ( expcd > 0 && expcd < 25 )  {
        sprintf ( temp, "%9.1f", exp);
        strncpy ( &v03[15], temp, 9);
      }
    }
  }
  else  {					/*  Use given exposure value  */
    sprintf ( temp, "%9.1f", exposure);
    strncpy ( &v03[15], temp, 9);
  }

  filter = edr->subcom.subword3.filter;		/* Filter position */
  v03[35] = ascii[filter];
  strncpy ( &v03[37], &fpname[icamera][filter][0], 6);

  gain = edr->subcom.word20.bits2.gain_state;	/* Gain state */
  if (gain == 1)  strncpy ( &v03[46], "HI", 2);
  else  strncpy ( &v03[46], "LO", 2);

  zvgrimfmt ( imcode, format, &scan_rate, ss, ns);     /* Scan rate */
  if (imcode == IM26) scan_rate = 2;
  else if (imcode == OC3) scan_rate = 3;
  sprintf ( temp, "%2d", scan_rate);
  strncpy ( &v03[65], temp, 2);

  date[0] = edr->ert.day_year.year;		/* Earth Received Time */
  date[1] = edr->ert.day_year.day;
  minute = edr->ert_min;
  date[2] = minute/60;
  date[3] = minute%60;
  msec = edr->ert_msec;
  date[4] = msec/1000;
  date[5] = msec%1000;
  sprintf ( temp, "%02d.%03d %02d:%02d:%02d", date[0], date[1], date[2], date[3], date[4]);
  strncpy ( &v04[4], temp, 15);

  res = edit[imcode];		/* Pixel editing and resolution */
  if (res < 0)  {
    strncpy    ( &v04[27], "PARTIAL", 7);
    res = -res;
  }
  else strncpy ( &v04[27], "FULL   ", 7);
  if (imcode == IMS)  strncpy ( &v04[22], ".843", 4);
  else  {
    sprintf ( temp, "%c/%2d", ascii[res/100], res%100);
    strncpy ( &v04[22], temp, 4);
  }

  sprintf ( temp, "%7.2f", vidicon_temp);	/* Vidicon temperature */
  strncpy ( &v04[54], temp, 7);	

  if (edr->output_volume[0] != 0)  {		/* EDR tape id & file number */
    strncpy ( &v05[3], &edr->output_volume[0], 6);
    sprintf ( temp, "%2d", edr->fileno);
    strncpy ( &v05[10], temp, 2);
  }

  if ( targname)  sprintf ( temp, "%-10s", targname);      /* Target body obtained from PDS label on CD */
  else  sprintf ( temp, "%10s", "          ");
  strncpy ( &v05[31], temp, 10);

  source = edr->gcf[1].source_station;		/* DSN Source Station ID */
  ind = zdsnid ( &source, &mrk3_id, &mrk4_id, station_name);
  if ( ind == 0)                              sprintf ( temp, "**");
  else if ( edr->gcf[1].s2.dsn_mark_no == 1)  sprintf ( temp, "%02d", mrk4_id);
  else                                        sprintf ( temp, "%02d", mrk3_id);
  strncpy ( &v05[48], temp, 2);

  noise = (float)edr->sds.system_noise_temp_max/128.0; /* System Noise Temperature */
  sprintf ( temp, "%8.3f", noise);
  strncpy ( &v05[61], temp, 8);

/*	This code from VGRLOG not fully understood    */
/*	Currently, I'm not even using it
  i = 2*edr->subcom.subword3.na_elect_cal + edr->subcom.subword3.wa_elect_cal + edr->subcom.word20.word;
  bswap(&i,1);
  hexcon(&i,buf,4);
  mvl(&buf[3],&v06[1],5);

  bswap(&edr->subcom.parword_a,15);
  hexcon(&edr->subcom.parword_a,&v06[9],4);
  hexcon(&edr->subcom.parword_b,&v06[20],2);
  hexcon(&edr->subcom.parword_c,&v06[27],2);
  hexcon(&edr->subcom.parword_d,&v06[34],4);
  hexcon(&edr->subcom.na_sample1,&v06[48],10);
  bswap(&edr->subcom.parword_a,15);
*/

  sprintf ( temp, "%08.1f", exp);
  strncpy ( &v07[12], temp, 8);

					     /* Pixel average */
  pixel_average = 8 * edr->subcom.word20.bits2.pixel_average;
  sprintf ( temp, "%03d", pixel_average);
  strncpy ( &v07[33], temp, 3);
  ind = edr->subcom.word20.bits2.pixel_avg_ind;
  if (ind == 0) v07[37] = '0';
  else          v07[37] = '1';

  mode = edr->subcom.parword_a.mode;	     /* Camera mode */
  v07[56] = ascii[mode];
  strncpy ( &v07[58], &mtbl[mode][0], 6);

  parword_ind.word = edr->subcom.parword_a_ind | edr->subcom.parword_d_ind;
  sprintf ( temp, "%s", yesno[1 - edr->subcom.subword3.na_elect_cal]);
  strncpy ( &v09[5], temp, 3);
  if ( !parword_ind.bits.na_cycle_ind)  strncpy ( &v09[10], "PREP", 4);
  else                                  strncpy ( &v09[10], "READ", 4);
  sprintf ( temp, "%s", yesno[parword_ind.bits.na_beam_ind]);
  strncpy ( &v09[16], temp, 3);
  sprintf ( temp, "%s", yesno[parword_ind.bits.na_shutter_reset]);
  strncpy ( &v09[22], temp, 3);
  sprintf ( temp, "%s", yesno[parword_ind.bits.na_shutter_open]);
  strncpy ( &v09[28], temp, 3);
  sprintf ( temp, "%s", yesno[parword_ind.bits.na_shutter_close]);
  strncpy ( &v09[34], temp, 3);
  sprintf ( temp, "%s", yesno[parword_ind.bits.na_light_flood]);
  strncpy ( &v09[40], temp, 3);
  if ( edr->subcom.parword_b.na_exposure == 0)  strncpy ( &v09[46], "YES", 3);
  else                                          strncpy ( &v09[46], "NO ", 3);
  v09[52] = ascii[edr->subcom.parword_b.na_filt];
  if ( edr->subcom.parword_b.na_filt_step_mode == 0) v09[54] = 'P';
  else                                               v09[54] = 'S';
  strncpy ( &v09[59], shut_mode[edr->subcom.parword_d.na_shutter_select], 10);

  sprintf ( temp, "%s", yesno[1 - edr->subcom.subword3.wa_elect_cal]);
  strncpy ( &v10[5], temp, 3);
  if ( !parword_ind.bits.wa_cycle_ind)  strncpy ( &v10[10], "PREP", 4);
  else                                  strncpy ( &v10[10], "READ", 4);
  sprintf ( temp, "%s", yesno[parword_ind.bits.wa_beam_ind]);
  strncpy ( &v10[16], temp, 3);
  sprintf ( temp, "%s", yesno[parword_ind.bits.wa_shutter_reset]);
  strncpy ( &v10[22], temp, 3);
  sprintf ( temp, "%s", yesno[parword_ind.bits.wa_shutter_open]);
  strncpy ( &v10[28], temp, 3);
  sprintf ( temp, "%s", yesno[parword_ind.bits.wa_shutter_close]);
  strncpy ( &v10[34], temp, 3);
  sprintf ( temp, "%s", yesno[parword_ind.bits.wa_light_flood]);
  strncpy ( &v10[40], temp, 3);
  if ( edr->subcom.parword_c.wa_exposure == 0)  strncpy ( &v10[46], "YES", 3);
  else                                          strncpy ( &v10[46], "NO ", 3);
  v10[52] = ascii[edr->subcom.parword_c.wa_filt];
  if ( edr->subcom.parword_c.wa_filt_step_mode == 0) v10[54] = 'P';
  else                                               v10[54] = 'S';
  strncpy ( &v10[59], shut_mode[edr->subcom.parword_d.wa_shutter_select], 10);

  i = edr->subcom.word20.bits2.fds_code;
  g1_voltage = edr->subcom.word20.bits2.g1_voltage;
  v09[57] = '*';
  v10[57] = '*';
  if (i == 5) v09[57] = ascii[g1_voltage];	
  else v10[57] = ascii[g1_voltage];	

  strncpy ( &v11[24], format, 5);		/* TLM FORMAT	*/

  /* process truncation code */
  if (imcode == GS2 || imcode == GS4)   {	/* restored Jun 10 1994   */
    strncpy ( &v11[10], "N/A", 3);
    strncpy ( &v11[42], "N/A", 3);
  }
  else  {
    if ( icamera == 0) lsb_trunc = edr->subcom.word20.bits.wa_lsb_trunc;
    else               lsb_trunc = edr->subcom.word20.bits.na_lsb_trunc;
    if ((edr->nept_byte.compression == 1) && (lsb_trunc == 0))  
      strncpy ( &v11[10], "ON ", 3);
    else  
      strncpy ( &v11[10], "OFF", 3);
  }

  if ( edr->nept_byte.compression == 1)
    strncpy ( &v11[42], "ON ", 3);
  else  
    strncpy ( &v11[42], "OFF", 3);

  if ( !sedrflg)  {		        /*If SEDR not present*/
    v11[71]='L';  	    		/* Put an 'L' in cc 72 of last rec*/
    nlab = 11;
  }

  if ( print == 1)  {
    printf ( "%s\n", v01);
    printf ( "%s\n", v02);
    printf ( "%s\n", v03);
    printf ( "%s\n", v04);
    printf ( "%s\n", v05);
    printf ( "%s\n", v06);
    printf ( "%s\n", v07);
    printf ( "%s\n", v08);
    printf ( "%s\n", v09);
    printf ( "%s\n", v10);
    printf ( "%s\n", v11);
  }

  ind = zladd ( ounit, "HISTORY", "LAB01", v01, "FORMAT", "STRING", "ULEN", 72, 0);
  check_ind(1, ind);
  ind = zladd ( ounit, "HISTORY", "LAB02", v02, "FORMAT", "STRING", "ULEN", 72, 0);
  check_ind(2, ind);
  ind = zladd ( ounit, "HISTORY", "LAB03", v03, "FORMAT", "STRING", "ULEN", 72, 0);
  check_ind(3, ind);
  ind = zladd ( ounit, "HISTORY", "LAB04", v04, "FORMAT", "STRING", "ULEN", 72, 0);
  check_ind(4, ind);
  ind = zladd ( ounit, "HISTORY", "LAB05", v05, "FORMAT", "STRING", "ULEN", 72, 0);
  check_ind(5, ind);
  ind = zladd ( ounit, "HISTORY", "LAB06", v06, "FORMAT", "STRING", "ULEN", 72, 0);
  check_ind(6, ind);
  ind = zladd ( ounit, "HISTORY", "LAB07", v07, "FORMAT", "STRING", "ULEN", 72, 0);
  check_ind(7, ind);
  ind = zladd ( ounit, "HISTORY", "LAB08", v08, "FORMAT", "STRING", "ULEN", 72, 0);
  check_ind(8, ind);
  ind = zladd ( ounit, "HISTORY", "LAB09", v09, "FORMAT", "STRING", "ULEN", 72, 0);
  check_ind(9, ind);
  ind = zladd ( ounit, "HISTORY", "LAB10", v10, "FORMAT", "STRING", "ULEN", 72, 0);
  check_ind(10, ind);
  ind = zladd ( ounit, "HISTORY", "LAB11", v11, "FORMAT", "STRING", "ULEN", 72, 0);
  check_ind(11, ind);

  if ( sedrflg)  {			    	/*If SEDR present*/
    v16[71] = 'L';         			/* Put an 'L' in cc 72 of last rec*/
    nlab    = 16;

    if ( print == 1)  {
      printf ( "%s\n", v12);
      printf ( "%s\n", v13);
      printf ( "%s\n", v14);
      printf ( "%s\n", v15);
      printf ( "%s\n", v16);
    }

    ind = zladd ( ounit, "HISTORY", "LAB12", v12, "FORMAT", "STRING", "ULEN", 72, 0);
    check_ind(12, ind);
    ind = zladd ( ounit, "HISTORY", "LAB13", v13, "FORMAT", "STRING", "ULEN", 72, 0);
    check_ind(13, ind);
    ind = zladd ( ounit, "HISTORY", "LAB14", v14, "FORMAT", "STRING", "ULEN", 72, 0);
    check_ind(14, ind);
    ind = zladd ( ounit, "HISTORY", "LAB15", v15, "FORMAT", "STRING", "ULEN", 72, 0);
    check_ind(15, ind);
    ind = zladd ( ounit, "HISTORY", "LAB16", v16, "FORMAT", "STRING", "ULEN", 72, 0);
    check_ind(16, ind);
  }

  ind = zladd ( ounit, "HISTORY", "NLABS", &nlab, "FORMAT", "INT", 0);
  check_ind(17, ind);

}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create engrhdr.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/*  I kept the order of all the packed bit-fields so that VAX formats   */
/*  are retained.  I had to reverse these fields for the UNIX platforms	*/
/*                                                                      */
/*  --> This is the VAX VERSION OF THE Eng. Header Data Structure <--   */
/*  Myche McAuley  6/95							*/


#include "gcfdata.h"			/* GCF block data definitions	     */
#include "vgrsubcom.h"			/* Voyager ISS Subcom definitions    */

union input_id {
  unsigned short source;
    struct {
      unsigned type      :8;	/* 0=VGR1, 1=VGR2, 4=VGR1 SIM, ...   */
      unsigned unused    :1;	/* Unused			     */
      unsigned rt        :1;	/* 1=data from R/T (via DACS)        */
      unsigned sdr       :1;	/* 1=data from SDR tape		     */
      unsigned idr       :1;	/* 1=data from IDR		     */
      unsigned edr       :1;	/* 1=data from EDR		     */
      unsigned sfdu      :1;	/* 1=data from SFDU tape 	     */
      unsigned unused2   :1;	/* 				     */
      unsigned fill_data :1;	/* 1=fill data			     */
    } src;
};

union eventbuf  {
  unsigned short word;
  struct  {
    unsigned day        :9;
    unsigned year       :7;
  }  day_year;
};

struct edrhdr
  {
  char recid;			/* Header record id = 0 	     */
  unsigned char fileno;		/* EDR tape file number = 0,1,2,...  */
  unsigned short phys_seq_no;	/* EDR tape physical sequence number */
  unsigned short log_seq_no;	/* EDR tape logical sequence number  */
  union eventbuf ert;		/* ERT day & year, first valid line  */
  unsigned short ert_min;	/* ERT minute of day,first valid line*/
  unsigned short ert_msec;	/* ERT msec of minute,first valid lin*/
  union eventbuf lert;		/* ERT day & year, last valid line   */
  unsigned short lert_min;	/* ERT minute of day,last valid line */
  unsigned short lert_msec;	/* ERT msec of minute,last valid line*/
  unsigned short fds_mod16;	/* FDS mod16 count, first valid line */
  unsigned short fds_mod60;	/* FDS mod60 count, first valid line */
  unsigned short fds_line;	/* FDS line count, first valid line  */
  unsigned short lfds_mod16;	/* FDS mod16 count, last valid line  */
  unsigned short lfds_mod60;	/* FDS mod60 count, last valid line  */
  unsigned short lfds_line;	/* FDS line count, last valid line   */
  union eventbuf scet;		/* SCET day & year		     */
  unsigned short scet_min;	/* SCET minute of day 		     */
  unsigned short scet_msec;	/* SCET msec of minute  	     */
  char system_version[6];	/* "VMSx.x" */
  char input_device[3];
  char input_fileid[6];
  char output_device[3];  	/* "MT3" */
  char output_volume[6];	/* "EDR002" */
  char cpu;			/* "1", "2", or "3" */
  char creation_day[3];		/* julian day of record creation     */
  char creation_year[2];	/* year of record creation 	     */
  short rver_number;            /* catalog raw version number        */
  struct gcfdata gcf[2];	/* GCF data, first & last valid line */
  union eventbuf irt;		/* IRT day & year                    */
  unsigned short irt_min;	/* IRT minute of day  		     */
  unsigned short irt_msec;	/* IRT msec of minute  		     */
  unsigned char  tlm_mode;	/* telemetry mode		     */
  unsigned char unused;

  struct {		/* Source Data Summary */

    unsigned short unused1;
    struct
    {
      unsigned sc_id		  :1;	/* 0=VGR-2, 1=VGR-1  */
      unsigned imcode		  :5;	/* Image format code */
      unsigned major_data_type    :2;	/* 2=Imaging Data    */
      unsigned unused		  :8;	/* Unused */
    } fid;

    unsigned short system_noise_temp_min;
    unsigned short system_noise_temp_max;
             short symbol_snr_min;
             short symbol_snr_max;
    unsigned short agc_min;
    unsigned short agc_max;
    unsigned short pn_errs;		/* # bit-errs in 32-bit PN code */
    unsigned short fds_count_errs;	/* # bit-errs in 24-bit FDSC */
    unsigned short sync_pars[3];
    unsigned short nlines;		/* Total # of valid lines    */
    unsigned short nfull;		/* number of full lines      */
    unsigned short npartial;		/* number of partial lines   */
    unsigned short nbadrec;		/* # of unreadable records   */
    unsigned short nlog_seq_breaks;	/* # of logical seq breaks   */
    unsigned short sort_par[4];		/* Note: [2]=station 2       */
    unsigned short nmf_from_idr;	/* # of minor frms from IDR  */
    unsigned short nmf_from_wbdl;	/* # of minor frms from DACS */
    unsigned short nmf_from_sdr;	/* # of minor frms from SDR  */
    unsigned short nmf_missing;		/* # of missing minor frames */
    unsigned short unused2;
    char picno[10];	        	/* Picture No,e.g."1234U-234"*/
    char target_body[10];		/* Target body, "GANYMEDE"   */
    union input_id input;		/* Input source & type       */
  } sds;

    struct vgrsubcom subcom;
  unsigned short iss_engineering[5];
  struct  {
  unsigned int	im2wp_flag : 1;		/* if set, data from PWS part>*/
					/*>of the crazy IM2W format   */
  unsigned int	compression: 1;		/* 0=OFF, 1=ON		      */
  unsigned int	unused4    : 6;
  }  nept_byte;
  unsigned char	unused5[5];

};
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create gcfdata.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/*===========================================================================*
 | GCFDATA.H  -- Ground Control Facility Data Block Definition		     |
 |									     |
 |	Total length = 20 bytes						     |
 |									     |
 |	Reference: E. Kelly,"DSN MKIV Changes Affecting the VGR PRJ",	     |
 |		   IOM Voyager-GDSE-84-029, 19 July 1984.		     |
 *===========================================================================*/

union gdd_udt
	{
	short word;
	struct
		{
		unsigned ddt : 7;    /* DSN data dependent type */
		unsigned udt : 6;    /* DSN user dependent type */
		unsigned gdd : 3;    /* DSN gross data description */
		} bits;
	};

union dsn_status
	{
	short word;
	struct
		{
		unsigned config       : 8;    /* DSN configuration */
		unsigned lock_status  : 6;    /* DSN lock status */
		unsigned cu_data      : 2;    /* DSN coded/uncoded data */
		} bits;
	};

struct gcfdata
	{
	unsigned short sync_code_msb;	/* 16 msb of frame-sync code	     */
	unsigned char source_station;	/* DSN source station		     */
	unsigned char sync_code_lsb;	/*  8 lsb of frame_sync code	     */
	unsigned char block_format;	/* Format code for this GCF block    */
	unsigned char destination_code; /* DSN destination codes 	     */
	union gdd_udt gddudt;		/* DSN GDD, UDT, DDT data            */
        struct  {
  	  unsigned time_msb	:8;
	  unsigned sc_number	:7;	/* 31=VGR1,32=VGR2,41=VGR1 SIM,42=VGR2 SIM */
	  unsigned ddt		:1;
	}  s1;
	unsigned short time_lsb;
	struct  {
	  unsigned block	:4;
	  unsigned day_of_year	:10;
	  unsigned dsn_mark_no	:2;	/* 0=Mark III, 1=Mark IV             */
	}  s2;
	unsigned char msec_clock;
	unsigned char serial_number;
	union dsn_status dsn;		/* DSN configuration and lock status */
	unsigned short esc;
	};
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create linesfx.h
$ DECK/DOLLARS="$ VOKAGLEVE"
struct linesuffix  {
  short int fds_mod16_number;		/*  1  */
  short int fds_mod60_number;		/*  3  */
  short int fds_line_number;		/*  5  */
  short int mtis_line_number;		/*  7  */
  short int missing_frames;		/*  9  */
  short int retained_frame_bits[10];	/*  11  */
  char input_type;			/*  31  */
  char input_source;			/*  32  */
  short int first_sample_number;	/*  33  */
  short int last_sample_number;		/*  35  */
}; 
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create vgrsubcom.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/*===========================================================================*
 |  VGRSUBCOM.H  -- Voyager ISS Subcom Data Structure definition	     |
 | 									     |
 |  Reference: 618-236, Rev A, Section 3				     |
 *===========================================================================*/

struct par_ind {
	unsigned wa_light_flood   :1;	/* 0=flood pulse sent, 1=not sent    */
	unsigned wa_shutter_close :1;	/* 0=close pulse sent, 1=not sent    */
	unsigned wa_shutter_open  :1;	/* 0=open pulse sent, 1=not sent     */
	unsigned wa_shutter_reset :1;	/* 0=reset pulse sent, 1=not sent    */
	unsigned wa_beam_ind      :1;	/* 0=beam on, 1=beam off	     */
	unsigned wa_cycle_ind	  :1;	/* 0=actual/pseudo prepare, 1=read   */
	unsigned na_light_flood   :1;	/* 0=flood pulse sent, 1=not sent    */
	unsigned na_shutter_close :1;	/* 0=close pulse sent, 1=not sent    */
	unsigned na_shutter_open  :1;	/* 0=open pulse sent, 1=not sent     */
	unsigned na_shutter_reset :1;	/* 0=reset pulse sent, 1=not sent    */
	unsigned na_beam_ind      :1;	/* 0=beam on, 1=beam off	     */
	unsigned na_cycle_ind	  :1;	/* 0=actual/pseudo prepare, 1=read   */
	unsigned spare		  :4;	/* Spare bits			     */
	};

union parword_ind {
	unsigned short word;
	struct par_ind bits;
       };

union parword20 {
	unsigned short word;
	struct {	
	  unsigned fds_code	   :4;	/* FDS destination code: 5=WA, 6=NA  */
	  unsigned gain_state	   :1;	/* 0=low gain, 1=high gain	     */
	  unsigned g1_voltage	   :3;	/* G1 Voltage (0-7)		     */
	  unsigned memory_readout  :1;	/* 1=secondry memory readout,0=normal*/
	  unsigned na_lsb_trunc    :1;  /* 0=truncated, 1=no truncation      */
	  unsigned wa_lsb_trunc    :1;  /* 0=truncated, 1=no truncation      */
	  unsigned unused	   :5; 
	  } bits;
	struct {	
	  unsigned fds_code	   :4;	/* FDS destination code: 5=WA, 6=NA  */
	  unsigned gain_state	   :1;	/* 0=low gain, 1=high gain	     */
	  unsigned g1_voltage	   :3;	/* G1 Voltage (0-7)		     */
	  unsigned pixel_average   :5;  /* average of pixels above thresh    */
	  unsigned pixel_avg_ind   :3;  /* 0=.LT.min_count, 7=.GE.min_count  */
	  } bits2;
	};

struct vgrsubcom {
	struct  {
	unsigned shuttered_pic  :15;	/* 0=unshuttered, all ones=shuttered */
	unsigned camera_id	:1 ;	/* Camera ID: 0=WA, 1=NA 	     */
	}  subword1;

	struct  {			/* Subcom word #2 */
	unsigned segment_number :6 ;	/* Line segment number=0,1...,sr-1   */
	unsigned line_number    :10;	/* Image line number		     */
	}  subword2;

	struct  {			/* Subcom word #3 */
	unsigned odd_parity_bit :1 ;	/* Filter odd parity bit	     */
	unsigned filter		:3 ;	/* Filter position 		     */
	unsigned exposure	:5 ;	/* Exposure time code 		     */
	unsigned wa_elect_cal	:1 ;	/* WA electronics cal status: 1=on   */
	unsigned na_elect_cal	:1 ;	/* NA electronics cal status: 1=on   */
	unsigned exposure_table :1 ;	/* exposure table, 0:old, 1:new	     */
	unsigned spare		:4 ;	/* Spare bits = 0		     */
	}  subword3;

	unsigned short picture_count;	/* Subcom word #4: Shuttered pic cnt */

	struct {			/* Subcom word #5: Par word A */
	  unsigned next_use       :5;  	/* Next use count		     */
	  unsigned next_mode	  :3;	/* Next camera mode		     */
	  unsigned use	          :5;	/* Use count			     */
	  unsigned mode    	  :3;	/* Camera mode			     */
	  } parword_a;

	unsigned short parword_a_ind;	/* Subcom word #6: Par word A ind    */
	unsigned short parword_a_ptr;	/* Subcom word #7: Par word A pointer*/

	struct {			/* Subcom word #8: Par word B        */
	  unsigned use         	   :7;	/* Use count			     */
	  unsigned na_filt         :3;	/* Filter position or step count     */
	  unsigned na_filt_step_mode :1;/* 0=Position mode, 1=Step mode      */
	  unsigned na_exposure	   :5;	/* 0=Auto Exposure, >0 Exposure #    */
	  } parword_b;

	unsigned short parword_b_ptr;	/* Subcom word #9: Par word B pointer*/

	struct {			/* Subcom word #10: Par word C       */
	  unsigned use    	   :7;	/* Use count			     */
	  unsigned wa_filt         :3;	/* Filter position or step count     */
	  unsigned wa_filt_step_mode :1;/* 0=Position mode, 1=Step mode      */
	  unsigned wa_exposure	   :5;	/* 0=Auto Exposure, >0 Exposure #    */
	  } parword_c;

	unsigned short parword_c_ptr;	/* Subcom word #11: Par word C pointe*/

	struct {			/* Subcom word #12: Par word D	     */
	  unsigned use    	   :7;	/* Use count			     */
	  unsigned wa_shutter_select :2;/* 0=normal, 1=start long exp,       */
					/* 2=end long exp, 3=open	     */
	  unsigned na_shutter_select :2;/* 0=normal, 1=start long exp,       */
					/* 2=end long exp, 3=open	     */
	  unsigned na_optics_cal   :5;	/* =0 off, >0 Exposurce #	     */
	  } parword_d;

	unsigned short parword_d_ind;	/* Subcom word #13: Par word D ind   */
	unsigned short parword_d_ptr;	/* Subcom word #14: Par word D pointe*/

					/* Subcom words #15-19 */
	unsigned char na_sample1;	/* First NA sample from previous frm */
	unsigned char wa_sample1;	/* First WA sample from previous frm */
	unsigned char na_sample2;	/* Secnd NA sample from previous frm */
	unsigned char wa_sample2;	/* Secnd WA sample from previous frm */
	unsigned char na_sample3;	/* Third NA sample from previous frm */
	unsigned char wa_sample3;	/* Third WA sample from previous frm */
	unsigned char na_sample4;	/* Forth NA sample from previous frm */
	unsigned char wa_sample4;	/* Forth WA sample from previous frm */
	unsigned char na_sample5;	/* Fifth NA sample from previous frm */
	unsigned char wa_sample5;	/* Fifth WA sample from previous frm */

	union parword20 word20;		/* Subcom word #20 */
	};
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create uengrhdr.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/*  I flipped the order of all the packed bit-fields so that 		*/
/*  (hopefully) the numbers would be correct upon running zvtrans 	*/
/*  on the structures below.  This was required for correct operation	*/
/*  on: SGI so far.							*/
/*  (SGI docs state "Bits in a bitfield are allocated 			*/
/*    most-significant-bit-first within a unit) 			*/
/*  However, I have no idea (well I do have some idea) how this 	*/
/*  might affect the code if it were running on a VAX (the original 	*/
/*  format).  I bet (*know*) this is going to cause problems.  I will 	*/
/*  try to use ifdef flags so that the correct includes are used 	*/
/*  depending upon the system where the application is being built.	*/
/*									*/
/*  --> This is the UN*X VERSION OF THE Eng. Header Data Structure <--  */

#include "ugcfdata.h"	/* UN*X version of GCF block data definitions	*/
#include "uvgrsubcom.h"	/* UN*X version of VGR ISS Subcom definitions	*/

union input_id {
  unsigned short source;
    struct {
      unsigned fill_data :1;  /* 1=fill data				*/
      unsigned unused2   :1;  /* 					*/
      unsigned sfdu      :1;  /* 1=data from SFDU tape 	     		*/
      unsigned edr       :1;  /* 1=data from EDR		     	*/
      unsigned idr       :1;  /* 1=data from IDR		     	*/
      unsigned sdr       :1;  /* 1=data from SDR tape		     	*/
      unsigned rt        :1;  /* 1=data from R/T (via DACS)        	*/
      unsigned unused    :1;  /* Unused			     		*/
      unsigned type      :8;  /* 0=VGR1, 1=VGR2, 4=VGR1 SIM, ...   	*/
    } src;
};

union eventbuf  {
  unsigned short word;
  struct  {
    unsigned year	:7;
    unsigned day	:9;
  }  day_year;
};

struct edrhdr
  {
  char recid;			/* Header record id = 0 	     */
  unsigned char fileno;		/* EDR tape file number = 0,1,2,...  */
  unsigned short phys_seq_no;	/* EDR tape physical sequence number */
  unsigned short log_seq_no;	/* EDR tape logical sequence number  */
  union eventbuf ert;		/* ERT day & year, first valid line  */
  unsigned short ert_min;	/* ERT minute of day,first valid line*/
  unsigned short ert_msec;	/* ERT msec of minute,first valid lin*/
  union eventbuf lert;		/* ERT day & year, last valid line   */
  unsigned short lert_min;	/* ERT minute of day,last valid line */
  unsigned short lert_msec;	/* ERT msec of minute,last valid line*/
  unsigned short fds_mod16;	/* FDS mod16 count, first valid line */
  unsigned short fds_mod60;	/* FDS mod60 count, first valid line */
  unsigned short fds_line;	/* FDS line count, first valid line  */
  unsigned short lfds_mod16;	/* FDS mod16 count, last valid line  */
  unsigned short lfds_mod60;	/* FDS mod60 count, last valid line  */
  unsigned short lfds_line;	/* FDS line count, last valid line   */
  union eventbuf scet;		/* SCET day & year		     */
  unsigned short scet_min;	/* SCET minute of day 		     */
  unsigned short scet_msec;	/* SCET msec of minute  	     */
  char system_version[6];	/* "VMSx.x" */
  char input_device[3];
  char input_fileid[6];
  char output_device[3];  	/* "MT3" */
  char output_volume[6];	/* "EDR002" */
  char cpu;			/* "1", "2", or "3" */
  char creation_day[3];		/* julian day of record creation     */
  char creation_year[2];	/* year of record creation 	     */
  short rver_number;            /* catalog raw version number        */
  struct gcfdata gcf[2];	/* GCF data, first & last valid line */
  union eventbuf irt;		/* IRT day & year                    */
  unsigned short irt_min;	/* IRT minute of day  		     */
  unsigned short irt_msec;	/* IRT msec of minute  		     */
  unsigned char  tlm_mode;	/* telemetry mode		     */
  unsigned char unused;
  
  struct {		/* Source Data Summary */
  
    unsigned short unused1;
    struct
    {
      unsigned unused		  :8;	/* Unused */
      unsigned major_data_type    :2;	/* 2=Imaging Data    */
      unsigned imcode		  :5;	/* Image format code */
      unsigned sc_id		  :1;	/* 0=VGR-2, 1=VGR-1  */
    } fid;
  
    unsigned short system_noise_temp_min;
    unsigned short system_noise_temp_max;
             short symbol_snr_min;
             short symbol_snr_max;
    unsigned short agc_min;
    unsigned short agc_max;
    unsigned short pn_errs;		/* # bit-errs in 32-bit PN code */
    unsigned short fds_count_errs;	/* # bit-errs in 24-bit FDSC */
    unsigned short sync_pars[3];
    unsigned short nlines;		/* Total # of valid lines    */
    unsigned short nfull;		/* number of full lines      */
    unsigned short npartial;		/* number of partial lines   */
    unsigned short nbadrec;		/* # of unreadable records   */
    unsigned short nlog_seq_breaks;	/* # of logical seq breaks   */
    unsigned short sort_par[4];		/* Note: [2]=station 2       */
    unsigned short nmf_from_idr;	/* # of minor frms from IDR  */
    unsigned short nmf_from_wbdl;	/* # of minor frms from DACS */
    unsigned short nmf_from_sdr;	/* # of minor frms from SDR  */
    unsigned short nmf_missing;		/* # of missing minor frames */
    unsigned short unused2;
    char picno[10];	        	/* Picture No,e.g."1234U-234"*/
    char target_body[10];		/* Target body, "GANYMEDE"   */
    union input_id input;		/* Input source & type       */
  } sds;
  
  struct vgrsubcom subcom;
  unsigned short iss_engineering[5];
  struct  {
  unsigned int	unused4    : 6;
  unsigned int	compression: 1;		/* 0=OFF, 1=ON		      */
  unsigned int	im2wp_flag : 1;		/* if set, data from PWS part>*/
  					/*>of the crazy IM2W format   */
  }  nept_byte;
  unsigned char	unused5[5];
  
};
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ugcfdata.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/*===========================================================================*
 | UGCFDATA.H  -- Ground Control Facility Data Block Definition		     |
 |                For UN*X machines.                                         |
 |                ---> i.e. bit-fields are in reversed order                 |
 |									     |
 |	Total length = <machine dependent due to packing...20 bytes on VAX   |
 |									     |
 |	Reference: E. Kelly,"DSN MKIV Changes Affecting the VGR PRJ",	     |
 |		   IOM Voyager-GDSE-84-029, 19 July 1984.		     |
 *===========================================================================*/

union gdd_udt  {
  short word;
  struct  {
    unsigned gdd : 3;    /* DSN gross data description */
    unsigned udt : 6;    /* DSN user dependent type */
    unsigned ddt : 7;    /* DSN data dependent type */
  } bits;
};

union dsn_status  {
  short word;
  struct  {
    unsigned cu_data      : 2;    /* DSN coded/uncoded data */
    unsigned lock_status  : 6;    /* DSN lock status */
    unsigned config       : 8;    /* DSN configuration */
  } bits;
};

struct gcfdata  {
  unsigned short sync_code_msb;		/* 16 msb of frame-sync code	     */
  unsigned char source_station;		/* DSN source station		     */
  unsigned char sync_code_lsb;		/*  8 lsb of frame_sync code	     */
  unsigned char block_format;		/* Format code for this GCF block    */
  unsigned char destination_code; 	/* DSN destination codes 	     */
  union gdd_udt gddudt;			/* DSN GDD, UDT, DDT data            */
  struct  {
    unsigned ddt		:1;
    unsigned sc_number	:7;		/* 31=VGR1,32=VGR2		*/
					/* 41=VGR1 SIM,42=VGR2 SIM 	*/
    unsigned time_msb		:8;
  }  s1;
  unsigned short time_lsb;
  struct  {
    unsigned dsn_mark_no	:2;	/* 0=Mark III, 1=Mark IV             */
    unsigned day_of_year	:10;
    unsigned block		:4;
  }  s2;
  unsigned char msec_clock;
  unsigned char serial_number;
  union dsn_status dsn;		/* DSN configuration and lock status */
  unsigned short esc;
};
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create uvgrsubcom.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/*===========================================================================*
 |  VGRSUBCOM.H  -- Voyager ISS Subcom Data Structure definition	     |
 | 									     |
 |  Reference: 618-236, Rev A, Section 3				     |
 *===========================================================================*/

struct par_ind {
	unsigned spare		  :4;	/* Spare bits			     */
	unsigned na_cycle_ind	  :1;	/* 0=actual/pseudo prepare, 1=read   */
	unsigned na_beam_ind      :1;	/* 0=beam on, 1=beam off	     */
	unsigned na_shutter_reset :1;	/* 0=reset pulse sent, 1=not sent    */
	unsigned na_shutter_open  :1;	/* 0=open pulse sent, 1=not sent     */
	unsigned na_shutter_close :1;	/* 0=close pulse sent, 1=not sent    */
	unsigned na_light_flood   :1;	/* 0=flood pulse sent, 1=not sent    */
	unsigned wa_cycle_ind	  :1;	/* 0=actual/pseudo prepare, 1=read   */
	unsigned wa_beam_ind      :1;	/* 0=beam on, 1=beam off	     */
	unsigned wa_shutter_reset :1;	/* 0=reset pulse sent, 1=not sent    */
	unsigned wa_shutter_open  :1;	/* 0=open pulse sent, 1=not sent     */
	unsigned wa_shutter_close :1;	/* 0=close pulse sent, 1=not sent    */
	unsigned wa_light_flood   :1;	/* 0=flood pulse sent, 1=not sent    */
	};

union parword_ind {
	unsigned short word;
	struct par_ind bits;
       };

union parword20 {
	unsigned short word;
	struct {	
	  unsigned unused	   :5; 
	  unsigned wa_lsb_trunc    :1;  /* 0=truncated, 1=no truncation      */
	  unsigned na_lsb_trunc    :1;  /* 0=truncated, 1=no truncation      */
	  unsigned memory_readout  :1;	/* 1=secondry memory readout,0=normal*/
	  unsigned g1_voltage	   :3;	/* G1 Voltage (0-7)		     */
	  unsigned gain_state	   :1;	/* 0=low gain, 1=high gain	     */
	  unsigned fds_code	   :4;	/* FDS destination code: 5=WA, 6=NA  */
	  } bits;
	struct {	
	  unsigned pixel_avg_ind   :3;  /* 0=.LT.min_count, 7=.GE.min_count  */
	  unsigned pixel_average   :5;  /* average of pixels above thresh    */
	  unsigned g1_voltage	   :3;	/* G1 Voltage (0-7)		     */
	  unsigned gain_state	   :1;	/* 0=low gain, 1=high gain	     */
	  unsigned fds_code	   :4;	/* FDS destination code: 5=WA, 6=NA  */
	  } bits2;
	};

struct vgrsubcom {
	struct  {
	unsigned camera_id	:1 ;	/* Camera ID: 0=WA, 1=NA 	     */
	unsigned shuttered_pic  :15;	/* 0=unshuttered, all ones=shuttered */
	}  subword1;

	struct  {			/* Subcom word #2 */
	unsigned line_number    :10;	/* Image line number		     */
	unsigned segment_number :6 ;	/* Line segment number=0,1...,sr-1   */
	}  subword2;

	struct  {			/* Subcom word #3 */
	unsigned spare		:4 ;	/* Spare bits = 0		     */
	unsigned exposure_table :1 ;	/* exposure table, 0:old, 1:new	     */
	unsigned na_elect_cal	:1 ;	/* NA electronics cal status: 1=on   */
	unsigned wa_elect_cal	:1 ;	/* WA electronics cal status: 1=on   */
	unsigned exposure	:5 ;	/* Exposure time code 		     */
	unsigned filter		:3 ;	/* Filter position 		     */
	unsigned odd_parity_bit :1 ;	/* Filter odd parity bit	     */
	}  subword3;

	unsigned short picture_count;	/* Subcom word #4: Shuttered pic cnt */

	struct {			/* Subcom word #5: Par word A */
	  unsigned mode    	  :3;	/* Camera mode			     */
	  unsigned use	          :5;	/* Use count			     */
	  unsigned next_mode	  :3;	/* Next camera mode		     */
	  unsigned next_use       :5;  	/* Next use count		     */
	  } parword_a;

	unsigned short parword_a_ind;	/* Subcom word #6: Par word A ind    */
	unsigned short parword_a_ptr;	/* Subcom word #7: Par word A pointer*/

	struct {			/* Subcom word #8: Par word B        */
	  unsigned na_exposure	   :5;	/* 0=Auto Exposure, >0 Exposure #    */
	  unsigned na_filt_step_mode :1;/* 0=Position mode, 1=Step mode      */
	  unsigned na_filt         :3;	/* Filter position or step count     */
	  unsigned use         	   :7;	/* Use count			     */
	  } parword_b;

	unsigned short parword_b_ptr;	/* Subcom word #9: Par word B pointer*/

	struct {			/* Subcom word #10: Par word C       */
	  unsigned wa_exposure	   :5;	/* 0=Auto Exposure, >0 Exposure #    */
	  unsigned wa_filt_step_mode :1;/* 0=Position mode, 1=Step mode      */
	  unsigned wa_filt         :3;	/* Filter position or step count     */
	  unsigned use    	   :7;	/* Use count			     */
	  } parword_c;

	unsigned short parword_c_ptr;	/* Subcom word #11: Par word C pointe*/

	struct {			/* Subcom word #12: Par word D	     */
	  unsigned na_optics_cal   :5;	/* =0 off, >0 Exposurce #	     */
	  unsigned na_shutter_select :2;/* 0=normal, 1=start long exp,       */
					/* 2=end long exp, 3=open	     */
	  unsigned wa_shutter_select :2;/* 0=normal, 1=start long exp,       */
					/* 2=end long exp, 3=open	     */
	  unsigned use    	   :7;	/* Use count			     */
	  } parword_d;

	unsigned short parword_d_ind;	/* Subcom word #13: Par word D ind   */
	unsigned short parword_d_ptr;	/* Subcom word #14: Par word D pointe*/

					/* Subcom words #15-19 */
	unsigned char na_sample1;	/* First NA sample from previous frm */
	unsigned char wa_sample1;	/* First WA sample from previous frm */
	unsigned char na_sample2;	/* Secnd NA sample from previous frm */
	unsigned char wa_sample2;	/* Secnd WA sample from previous frm */
	unsigned char na_sample3;	/* Third NA sample from previous frm */
	unsigned char wa_sample3;	/* Third WA sample from previous frm */
	unsigned char na_sample4;	/* Forth NA sample from previous frm */
	unsigned char wa_sample4;	/* Forth WA sample from previous frm */
	unsigned char na_sample5;	/* Fifth NA sample from previous frm */
	unsigned char wa_sample5;	/* Fifth WA sample from previous frm */

	union parword20 word20;		/* Subcom word #20 */
	};
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create vgrcdcopy.imake
#define PROGRAM vgrcdcopy
#define MODULE_LIST vgrcdcopy.c decomp.c vgrcdlabgen.c trans_eng_hdr.c

#define INCLUDE_LIST  engrhdr.h gcfdata.h linesfx.h vgrsubcom.h \
 uengrhdr.h ugcfdata.h uvgrsubcom.h

#define MAIN_LANG_C
#define USES_ANSI_C

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_FORTRAN
/*#define DEBUG	/* remove on delivery */
$ Return
$!#############################################################################
$Test_File:
$ create tstvgrcdcopy.pdf
procedure help=*
refgbl $syschar
refgbl $echo

body
let _ONFAIL="continue"
let $echo="yes"

! the input file c2684533.imq was downloaded from the PDS website

vgrcdcopy /project/test_work/testdata/vgr/c2684533.imq c2684533.vic

label-l c2684533.vic

! check contents, including binary header and prefix:
label-remove c2684533.vic c2684533.nolab 'bin
label-create c2684533.nolab c2684533.bin nl=802 ns=1024
list c2684533.bin lin=60 sinc=60

end-proc  
$!-----------------------------------------------------------------------------
$ create tstvgrcdcopy.log_solos
tstvgrcdcopy
vgrcdcopy /project/test_work/testdata/vgr/c2684533.imq c2684533.vic
let $echo="yes"
   if ($syschar(1)= "UNIX")
       ush $R2LIB/vgrcdcopy /project/test_work/testdata/vgr/c2684533.imq c2684533.vic
    else
   end-if
end-proc
label-l c2684533.vic
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File c2684533.vic ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in BYTE format from a SUN-SOLR host
                1 bands
                800 lines per band
                800 samples per line
                2 lines of binary header
                224 bytes of binary prefix per line
---- Task: TASK -- User: lwk -- Thu Feb 21 21:42:36 2013 ----
LAB01=
'                     800     800 800 800 L 1                          SC'
LAB02=
'VGR-2   FDS 26845.33   PICNO 1661U2-001   SCET 86.024 16:08:45         C'
LAB03=
'NA CAMERA  EXP    1920.0 MSEC FILT 0(CLEAR )  LO GAIN  SCAN RATE  1:1  C'
LAB04=
'ERT 86.025 20:30:29   1/ 1 FULL    RES   VIDICON TEMP  -80.00 DEG C    C'
LAB05=
'IN/ISU603/14 OUT/xxxxxx/xx     ARIEL       DSS #45   BIT SNR   18.523  C'
LAB06=
' xxxxx A/xxxxxxxx B/xxxx C/xxxx D/xxxxxxxx ETLM/xxxxxxxxxxxxxxxxxxxxS AC'
LAB07=
'NA OPCAL xx(001920.0*MSEC)PIXAVG 048/0 OPERATIONAL MODE 2(NAONLY)     AC'
LAB08=
'CAM ECAL CYCLE BEAM  RESET OPEN  CLOSE FLOOD AEXPM  FIL G1 SHUT MODE  AC'
LAB09=
'NA   NO   READ  YES   NO    NO    NO    NO    NO    0 P  7 NORMAL     AC'
LAB10=
'WA   NO   PREP  NO    YES   NO    NO    NO    NO    2 P  * NORMAL     AC'
LAB11=
'LSB_TRUNC=OFF  TLM_MODE=IM-2  COMPRESSION=OFF                          L'
NLABS=11
 
************************************************************
label-remove c2684533.vic c2684533.nolab 'bin
Beginning VICAR task label
LABEL version 15-Nov-2010
label-create c2684533.nolab c2684533.bin nl=802 ns=1024
Beginning VICAR task label
LABEL version 15-Nov-2010
list c2684533.bin lin=60 sinc=60
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:LABEL     User:lwk       Date_Time:Thu Feb 21 21:42:37 2013
     Samp     1     121     241     361     481     601     721     841     961
   Line
      1       0   0 244  65   0   0   0   0   0   0   0   0   0   0   0   0   0   0
     61       0   0  59   0  14  15  12  12  11  11  10  11  11  12  12  12  12  15
    121       0   0 119   0  15  13  12  11  11  10  11  12  11  11  11  13  13  14
    181       0   0 179   0  12  12  12  12  12  12  11  12  11  11  13  12  14  13
    241       0   0 239   0  15  12  11  11  11  12  12  12  12  12  13  43  34  15
    301       0   0  43   0  14  12  14  12  14  14  13  12  14  51  59  39  33  18
    361       0   0 103   0  15  14  12  12  13  13  13  13  68  67  45  18  26  26
    421       0   0 163   0  15  14  13  13  14  13  13  62  50  49  47  40  39  28
    481       0   0 223   0  15  15  13  14  14  15  66  59  53  20  53  44  61  32
    541       0   0  27   0  17  15  15  14  15  14  63  54  55  55  49  44  37  35
    601       0   0  87   0  17  16  15  15  15  69  63  59  56  46  51  45  30  37
    661       0   0 147   0  19  17  15  15  15  65  65  68  55  57  46  49  39  39
    721       0   0 207   0  19  19  18  16  15  67  64  66  66  52  54  42  40   6
    781       0   0  11   0  18  18  17  16  16  76  65  65  60  58  66  53  54  44
end-proc
exit
slogoff
if ($RUNTYPE = "INTERACTIVE")
  if ($syschar(1) = "VAX_VMS")
  end-if
else
  if ($syschar(1) = "VAX_VMS")
  end-if
end-if
ulogoff
END-PROC
END-PROC
$ Return
$!#############################################################################
$PDF_File:
$ create vgrcdcopy.pdf
procedure help=*
refgbl $syschar
refgbl $echo
PARM INP TYPE=STRING count=0:1 default=--
PARM OUT TYPE=STRING count=0:1 default=--

body
let $echo="yes"
   if ($syschar(1)= "UNIX")
       ush $R2LIB/vgrcdcopy &INP &OUT
! to run a local copy, uncomment the following line and comment out the
! preceding one (or just run the program from the command line):
!      ush ./vgrcdcopy &INP &OUT
    else
      dcl $vgrcdcp:=="$R2LIB:vgrcdcopy.exe"
      dcl vgrcdcp &INP &OUT
   end-if
end-proc

!special comments:
!#annot function=data/compression
!#annot project=voyager
!#annot keywords=(data,compression,decompresion,voyager)

.Title
    vgrcdcopy.- A procedure for running the program vgrcdcopy
    within VICAR.  vgrcdcopy decompresses a Voyager image from
    the PDS cd-roms into a VICAR labeled image.

.Help
PURPOSE
    vgrcdcopy decompresses a Huffman First-Difference compressed
    VGR image, translating it from labeled image to a Voyager
    formatted VICAR image.

EXECUTION
    vgrcdcopy INP  OUT

NOTE:  VGRCDCOPY is not a VICAR program, but has a .pdf that is hard-coded
to run the application in $R2LIB.  To run the program from your directory,
simply run it from the command line:

   % ./vgrcdcopy INP OUT

EXAMPLE
    vgrcdcpy vg_0020:[000000.jupiter.c1940XXX]c1940944.imq   XXX.out
.PAGE
OUTPUT FILES
    a decompressed Voyager formatted Vicar Image.

OPERATION

  vgrcdcopy decompresses a Huffman First-Difference compressed
  VGR image, translating it from a PDS labeled image to a
  Voyager formatted VICAR image.  The binary headers and line
  prefixes are KEPT IN VAX-VMS FORMAT.  This should cause
  no problems since the next program run on this image is
  VGRFILLIN, which is advertised as being ported.

  The program verifies that the right number of parameters have	
  been entered.  Open file pointers to the files, exiting if problems.	

  The PDS label is parsed for information.  The Image histogram is read
  in from the file.  The 'missing lines' pixels in the 0 DN histogram 
  count are not included.  This can lead to histograms that don't sum to 
  640000 (800 x 800).	
									
  The histogram is written out as VAX formatted long ints.  
					
  Each variable length line is read in, decompressed , and written 
  to the output file.  Remember:  each line decompresses to a buffer 
  of 836 bytes.  The extra 36 bytes are the 'line suffix'.  This info 
  is described on the CD-ROM in the file /CDROM/label/linesufx.lbl			 Remember that the binary prefix for each line is 224 bytes long.	
  This will give a total line length of 1024 bytes.			
									
  The binary headers and prefixes are written in	
	VAX VAX VAX VAX VAX VAX VAX VAX VAX VAX VAX VAX VAX		
  format.  This should allow VGRFILLIN to run succesfully on any	
  platform, since it is advertised as being ported.			
  This is accomplished by simply moving the bytes to the correct	
  location in the prefix.  
		
PROGRAM HISTORY

    Original Programmer :  Myche McAuley  June 1995
    Cognizant Programmer:  Myche McAuley

REVISION HISTORY

    26 Mar 96  OAM  ...Revised to run on VMS. First Release. 
    
    
.LEVEL1
.VARIABLE INP	
input filename.
.VARIABLE OUT
output filename.

.LEVEL2
.VARIABLE INP
Location of the compressed VGR Image on the PDS cdrom.
.VARIABLE OUT
A Voyager formatted VICAR Image.
.End





$ Return
$!#############################################################################
