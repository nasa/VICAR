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
