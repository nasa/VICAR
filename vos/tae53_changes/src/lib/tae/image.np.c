/****************************************************************************
 *      Copyright (c) 1993, 1994
 *      Century Computing, Inc.
 *      ALL RIGHTS RESERVED
 *
 *      The software (programs, data bases and/or documentation) on or in
 *      any media can not be reproduced, disclosed, or used except under
 *      the terms of the TAE Plus Software License Agreement.
 *
 ***************************************************************************/




/*>>UNIX<<								*/

/*
 * 	TAE/UNIX image file i/o function package.
 *	The names of the externally callable functions all begin with
 *	"i_".  Calling sequences are described
 *	in the "C Programmer's Reference Manual for TAE".
 *
 *	FORTRAN programmers use XI calls to bridge routines, which in
 *	turn call this package.
 */


/* Change Log:
 *
 *	20-jun-85	New release, created from the xic.c package for
 *			compatibility with the VAX/VMS version...dm
 *	26-jul-85	Return  zero-filled data for non-existent lines
 *			within max blocks, in i_read (PR# 7128)...dm
 *	12-dec-86	Fix lint warnings...peb
 */

#include 	"taeconf.inp"		/* TAE standard, configuration defs */
#include 	"pgminc.inc"		/* C programmer constants & structs */
#include 	<errno.h>               /* error code                       */
#include "taeintproto.h"
#include <unistd.h>
#include <fcntl.h>
FUNCTION static COUNT move_spec
(
 TEXT		source[],		/* in:  source file spec	*/
 TEXT		dest[]			/* out: destination file spec	*/
);
FUNCTION static BOOL chk_senti 
(
    TEXT	senti[4]		/* image file sentinal characters	*/
 );

#define GOOD  0				/* Good status from UNIX system calls */


FUNCTION  static CODE  open_image
(
 FUNINT	*filedes,		/* out: descriptor of open file */
 TEXT	hostname[],		/* in/out: name of opened file  */
 FUNINT	open_mode		/* in: file access mode, 0,1,2  */
);
FUNCTION  static CODE  calc_bytes
(
 struct IFCB	*ifcb,		/* in: image file control block		*/
 FUNINT		channel,	/* in:  channel number of line		*/
 FUNINT		line,		/* in:  line number within channel	*/
 FUNINT		lines,		/* in:  number of lines to read/write	*/
 int		*bytes,		/* out: numer of bytes to read/write	*/
 int		*vblock	        /* out: starting block number 		*/
);
FUNCTION  static  CODE read_image
(
 TAEINT      filedes,                /* in: descriptor of file to be read */
 TEXT	     buffer[],               /* out: buffer to read into          */
 TAEINT      bytes,                  /* in: number of bytes to read       */
 TAEINT      start_blk               /* in: block number to start I/O from*/
 );
FUNCTION  static CODE  write_image
(
 FUNINT      filedes,                /* in: descriptor of file to write   */
 TEXT	     buffer[],               /* in: input data buffer             */
 FUNINT      bytes,                  /* in: number of bytes to write      */
 FUNINT	     max_blks,		     /* in: max number of blocks in file  */
 FUNINT      start_blk               /* in: block number to start I/O from
					starts with 1		     */
 );



/*
 *	chk_senti - Check image file sentinal for correctness.
 *
 *	Function returns:
 *
 *		TRUE	if sentinal is good.
 *		FALSE	if sentinal is bad.
 */

FUNCTION static BOOL chk_senti 
(
    TEXT	senti[4]		/* image file sentinal characters	*/
)
    {
    BOOL	status;

    status = TRUE;
    if (senti[0] != 'I'  ||
	senti[1] != 'M'  ||
	senti[2] != 'A'  ||
	senti[3] != 'G')
	status = FALSE;
    return(status);
    }

/*
 *	i_clse - Close image file.
 */

FUNCTION VOID i_clse
(
 struct IFCB		*ifcb,		/* in/out: image file control block	*/
 FUNINT		disp		/* in:  I_DEL or I_SAVE			*/
)
    {
    CODE        code;

    errno = 0;
    code = close ((*ifcb).i_filds);            	/* -1 means error            */
    if ((code > -1) && (disp == I_DEL))	    	/* file to be deleted        */
        unlink((*ifcb).i_filnm);                /* mark for deletion         */
    return/*(errno)*/;
    }

/*
 *	i_herr - Get host dependent error code.
 *
 *	Function return:  host dependent error code associated with
 *			  the last operation using the specified IFCB.
 */

FUNCTION CODE i_herr 
(
 struct IFCB		*ifcb		/* in: image file control block	*/
)
    {
    return((*ifcb).i_hcode);
    }

/*
 *	i_opin - Open existing file for input or input/output.
 *
 *	Return codes:  SUCCESS or P_FAIL.
 */

FUNCTION CODE i_opin
(
 struct IFCB	*ifcb,		/* out:	image file control block	*/
 FUNINT		lun,		/* in:  log. unit no. (ignored for UNIX)	*/
 TEXT		hostname[],	/* in:  host filespec of file to open	*/
 FUNINT		type		/* in:  open type (I_INPUT or I_INOUT)	*/
 )
    {
    CODE		open_mode;	/* file open mode: read/update	    */
    int			filedes;	/* descriptor for open file	    */
    TEXT		filename[I_FSSIZ+1];
    COUNT		hostlen;
    int			status;
    int			hdr_block;

    open_mode = (type == I_INPUT) ? 0 : 2;		/* read or update   */
    hostlen = move_spec(hostname, filename);
    status = open_image(&filedes, filename, open_mode);
    if (status != GOOD)
	{
	(*ifcb).i_hcode = status;
	return (P_FAIL);	
	}

/* Read image header block (VB #1) into user supplied IFCB.	*/

    hdr_block = 1;				/* position of header block */
    status = read_image(filedes, (GENPTR) ifcb,
	sizeof (struct IFCB), hdr_block);
    if (status != GOOD)
        goto fail_cleanup;

/*  Move pertinent information to user ifcb	*/

    hostlen = move_spec(filename, (*ifcb).i_filnm);	
    (*ifcb).i_filsz = hostlen;
    (*ifcb).i_filds = filedes;			/* new file descriptor  */
    if (!chk_senti((*ifcb).i_senti))		/* check sentinel 	*/
        goto fail_cleanup;
    (*ifcb).i_hcode = 0;			/* error number = 0     */
    return(SUCCESS);


fail_cleanup:
    i_clse (ifcb, I_SAVE);	
    (*ifcb).i_hcode = status;
    return (P_FAIL);
    }

/*
 *	i_opou - open file for output.
 *
 *	Return codes:  SUCCESS or P_FAIL.
 */

FUNCTION CODE i_opou 
(
 struct IFCB	*ifcb,		/* out: image file control block	*/
 FUNINT		lun,		/* in:  log. unit no. (ignored for VMS)	*/
 TEXT		hostname[],	/* in:  host filespec of file to open	*/
 FUNINT		org,		/* in:  file organization I_CI or I_CS	*/
 FUNINT		chans,		/* in:  number of channels		*/
 FUNINT		lines,		/* in:  number of lines per channel	*/
 FUNINT		linsiz,		/* in:  bytes per line			*/
 FUNINT		labels,		/* in:  number of label records		*/
 FUNINT		labsiz		/* in:  bytes per label record		*/
 )
    {
    CODE		open_mode;	/* file open mode: create	    */
    int			filedes;	/* descriptor for open file	    */
    TEXT		filename[I_FSSIZ+1];
    COUNT		hostlen;
    int			status;
    int			blocks, hdr_block;

    blocks = chans * lines * ((linsiz-1)/I_SECT + 1)	/* data lines	*/
	     + 1					/* header block	*/
	     + labels * ((labsiz-1)/I_SECT + 1);	/* label records*/

/* build IFCB block	*/

    (*ifcb).i_senti[0] = 'I';
    (*ifcb).i_senti[1] = 'M';
    (*ifcb).i_senti[2] = 'A';
    (*ifcb).i_senti[3] = 'G';
    (*ifcb).i_lines = lines;
    (*ifcb).i_chans = chans;
    (*ifcb).i_linsz = linsiz;
    (*ifcb).i_labs  = labels;
    (*ifcb).i_labsz = labsiz;
    (*ifcb).i_org   = org;
    (*ifcb).i_blocs = blocks;
    (*ifcb).i_start = 2
		+ labels * ((labsiz-1)/I_SECT + 1);	/* vbn of 1st line    */
							/* vbn starts with 1  */

    hostlen = s_bcopy(hostname, (*ifcb).i_filnm, 103);	/* copy & get length  */
    (*ifcb).i_filsz = hostlen;

/*	open the file through system call			*/

    open_mode = 1;					/* file create mode   */
    hostlen = move_spec(hostname, filename);
    status = open_image(&filedes, filename, open_mode);
    if (status != GOOD)
	{
	(*ifcb).i_hcode = status;
	return (P_FAIL);	
	}
    (*ifcb).i_filds = filedes;

/* Write header block (the IFCB).	*/

    hdr_block = 1;				/* position of header block */
    status = write_image(filedes, (GENPTR) ifcb,
	sizeof (struct IFCB), blocks, hdr_block);
    if (status != GOOD)
        goto fail_cleanup;
    (*ifcb).i_hcode = 0;			/* error number = 0     */
    return(SUCCESS);


fail_cleanup:

    i_clse (ifcb, I_DEL);		/* delete dynamic storage */
    (*ifcb).i_hcode = status;		/* deliver proper code	  */
    return(P_FAIL);
    }

/* 	open_image. Open an image file for read/write.
 *
 *	NOTE: If no file type is specified in the file name, ".dat" is
 *	assumed.
 */

FUNCTION  static CODE  open_image
(
 FUNINT	*filedes,		/* out: descriptor of open file */
 TEXT	hostname[],		/* in/out: name of opened file  */
 FUNINT	open_mode		/* in: file access mode, 0,1,2  */
 )
    {
    TEXT        filespec[STRINGSIZ+1];
    COUNT       i, len;
    BOOL        no_ftype;   		/* true if no file type            */

    f_subst(hostname, filespec);        /* substite for $ symbols, if any  */

    no_ftype = TRUE;
    len = s_length(filespec);
    for (i=len-1; i >= 0 && filespec[i] != '/'; i--)    /* libr name if '/'*/
        {
        if (filespec[i] == '.')
            {
            no_ftype = FALSE;                     /* file type present       */
            break;
            }
        }
    if (no_ftype)
        s_bcopy(".dat", &filespec[len], I_FSSIZ-len);  /* give def type */

    errno = 0;
    if (open_mode == 1)                         /* open for write       */
        *filedes = creat(filespec, 0755);       /* create a new file    */
    else
        *filedes = open(filespec, open_mode);   /* open the file        */
    move_spec(filespec, hostname);		/* return name to caller*/
    return (errno);
    }

/*
 *	i_rdlb - Read label record.
 *
 *	Function returns:  SUCCESS, P_FAIL, or I_NOLAB.
 */

FUNCTION CODE i_rdlb
(
 struct IFCB	*ifcb,		/* in/out: image file control block	*/
 GENPTR		buffer,		/* out: user's receive data buffer	*/
 FUNINT		labnum		/* in:  label record number (1-n)...	*/
    				/* where n is number of label records	*/
 )
    {
    COUNT		blklab;		/* blocks per label			*/
    int			status, vblock;


    if (labnum <= 0  ||  labnum > (*ifcb).i_labs)
	{
	(*ifcb).i_hcode = ERANGE;		/* block # not in range		*/
	return(I_NOLAB);
	}
    blklab = ((*ifcb).i_labsz-1)/I_SECT + 1;	/* blocks per label		*/
    vblock = 2 + (labnum-1) * blklab;		/* starting VBN for label	*/

    status = read_image((*ifcb).i_filds, buffer,
		(*ifcb).i_labsz, vblock);
    (*ifcb).i_hcode = status;
    if (status != GOOD)
	return (P_FAIL);
    return (SUCCESS);
    }

/*
 *	i_read - Initiate read to an image file.
 *
 *	Function returns:  SUCCESS, P_FAIL, or I_IVLIN
 *
 *	If user requested to read a line (less than max-line) 
 *	that was not written before a zero-filled line 
 *	is returned.
 */

FUNCTION CODE i_read
(
 struct IFCB    *ifcb,		/* in/out: image file control block	*/
 GENPTR		buffer,		/* out: user's data receive buffer	*/
 FUNINT		channel,	/* in:  channel number of line		*/
 FUNINT		line,		/* in:  line number within channel	*/
 FUNINT		lines		/* in:  number of lines to read		*/
 )
    {
    CODE		code;
    int			status;
    int			bytes;		/* numer of bytes to read/write		*/
    int			vblock;		/* starting block number 		*/


/*  compute the number of bytes to be read and the starting block number */

    code = calc_bytes(ifcb, channel, line, lines, &bytes, &vblock);
    if (code != SUCCESS)
	{
	(*ifcb).i_hcode = ERANGE;	/* data not within range	 */
	return (I_IVLIN);
	}
    status = read_image((*ifcb).i_filds, buffer,
		bytes, vblock);
    if (status == ERANGE)		/* outside physically written area */
	status = GOOD;			/* okay status			   */
    (*ifcb).i_hcode = status;
    if (status != GOOD)
	return (P_FAIL);
    return (SUCCESS);
    }


/*
 *	i_wait - Wait for I/O (read or write) completion on IFCB.
 *
 *	Function returns:  SUCCESS or P_FAIL.
 *
 *	NOTE: Under UNIX, image file I/O function is synchronous.
 *		Therefore, i_wait simply returns the host code of
 *		the already completed I/O operation.
 */

FUNCTION CODE i_wait
(
 struct IFCB	*ifcb		/* in/out: image file control block	*/
 )
    {
    int 		status;

    status = (*ifcb).i_hcode;
    if (status != GOOD)
	return(P_FAIL);
    return(SUCCESS);
    }

/*
 *	i_write - Initiate write to an image file.
 *
 *	Function returns:  SUCCESS, P_FAIL, or I_IVLIN
 *
 */

FUNCTION CODE i_write
(
 struct IFCB	*ifcb,		/* in/out: image file control block	*/
 GENPTR		buffer,		/* out: user's data buffer		*/
 FUNINT		channel,	/* in:  channel number of line		*/
 FUNINT		line,		/* in:  line number within channel	*/
 FUNINT		lines		/* in:  number of lines to write	*/
)
    {
    CODE		code;
    int			status;
    int			bytes;		/* numer of bytes to read/write		*/
    int			vblock;		/* starting block number 		*/


/*  compute the number of bytes to write and the starting block number */

    code = calc_bytes(ifcb, channel, line, lines, &bytes, &vblock);
    if (code != SUCCESS)
	{
	(*ifcb).i_hcode = ERANGE;	/* data not within range	 */
	return (I_IVLIN);
	}
    status = write_image((*ifcb).i_filds, buffer,
		bytes, (*ifcb).i_blocs, vblock);
    (*ifcb).i_hcode = status;
    if (status != GOOD)
	return (P_FAIL);
    return (SUCCESS);
    }

/*
 *	i_xtnd - Extend image file.
 *
 *	Return codes:  SUCCESS or P_FAIL.
 */

FUNCTION CODE i_xtnd
(
 struct IFCB	*ifcb,		/* in/out: image file control block	*/
 FUNINT		channels,	/* in:  number of channels to add	*/
 FUNINT		lines		/* in:  number of lines to add per channel*/
 )
    {
    int			tot_chans;	/* new number of channels		*/
    int			tot_lines;	/* new number of lines			*/
    int			tot_blocks;
    int			hdr_block;
    int			status;

    if (channels == 0 && lines == 0)		/* do nothing?		*/
	{
	(*ifcb).i_hcode = 0;
	return (SUCCESS);
	}	
    tot_chans  = (*ifcb).i_chans + channels;
    tot_lines  = (*ifcb).i_lines + lines;
    tot_blocks = tot_chans * tot_lines * (((*ifcb).i_linsz-1)/I_SECT + 1)
		+ 1		/* data lines + header block			*/
		+ (*ifcb).i_labs * (((*ifcb).i_labsz-1)/I_SECT + 1);  /* label records*/


/* Update the header block (the IFCB) and write back to disk	*/

    (*ifcb).i_lines = tot_lines;
    (*ifcb).i_chans = tot_chans;
    (*ifcb).i_blocs = tot_blocks;
    hdr_block = 1;				/* position of header block */
    status = write_image((*ifcb).i_filds, (GENPTR) ifcb,
	sizeof (struct IFCB), tot_blocks, hdr_block);
    (*ifcb).i_hcode = status;
    if (status != GOOD)
	return(P_FAIL);
    return(SUCCESS);
    }


/*
 *	calc_bytes. Calculate the number of bytes for image data read/write.
 */

FUNCTION  static CODE  calc_bytes
(
 struct IFCB	*ifcb,		/* in: image file control block		*/
 FUNINT		channel,	/* in:  channel number of line		*/
 FUNINT		line,		/* in:  line number within channel	*/
 FUNINT		lines,		/* in:  number of lines to read/write	*/
 int		*bytes,		/* out: numer of bytes to read/write	*/
 int		*vblock	        /* out: starting block number 		*/
 )
    {
    COUNT		blklin;		/* blocks per line			*/
    COUNT		blkchan;	/* blocks for 1 line group		*/
    COUNT		mod_linsiz;
    int			last_block;


    blklin = ((*ifcb).i_linsz-1)/I_SECT + 1;	/* blocks per line		*/
    if ((*ifcb).i_org == I_CI)			/* if channel interleaved	*/
	{
	blkchan = (*ifcb).i_chans * blklin;
	*vblock  = (*ifcb).i_start		/* start of data lines		*/
		  + (line-1) * blkchan		/* start of line group		*/
		  + (channel-1) * blklin;	/* line within group		*/
	}
    else					/* channel sequential		*/
	{
	blkchan = (*ifcb).i_lines * blklin;
	*vblock  = (*ifcb).i_start		/* start of data lines		*/
		  + (channel-1) * blkchan	/* start of selected channel	*/
		  + (line-1) * blklin;		/* line within channel		*/
	}
/*
 *	Calculate byte count. Note that caller receives multiple
 *	lines on a I_SECT byte boundary.  The wasted space at the end
 *	of the last line is subtracted out (the 'mod' term) so the
 *	dummy data does not have to be read.  This is nice for when
 *	lines = 1 and the caller dimensions for one line only.
 */

    mod_linsiz = (*ifcb).i_linsz % I_SECT;	/* over block boundary		*/
    if (mod_linsiz == 0)
	*bytes = lines * blklin * I_SECT;	/* no wasted data		*/
    else
	*bytes = lines * blklin * I_SECT -
		(I_SECT-mod_linsiz); 		/* subtract the waste		*/
    last_block = *vblock + (*bytes-1)/I_SECT;
    if (last_block > (*ifcb).i_blocs)
        return (I_IVLIN);
    return (SUCCESS);
    }

/*
 *	i_wrlb - Write label record.
 *
 *	Function returns:  SUCCESS, P_FAIL, or I_NOLAB.
 */

FUNCTION CODE i_wrlb
(
 struct IFCB	*ifcb,		/* in/out: image file control block	*/
 GENPTR		buffer,		/* out: user's data buffer		*/
 FUNINT		labnum		/* in:  label record number (1-n)...	*/
				/* where n is number of label records	*/
 )
    {
    COUNT		blklab;		/* blocks per label			*/
    int			status, vblock;


    if (labnum <= 0  ||  labnum > (*ifcb).i_labs)
	{
	(*ifcb).i_hcode = ERANGE;		/* block # not in range		*/
	return(I_NOLAB);
	}
    blklab = ((*ifcb).i_labsz-1)/I_SECT + 1;	/* blocks per label		*/
    vblock = 2 + (labnum-1) * blklab;		/* starting VBN for label	*/

    status = write_image((*ifcb).i_filds, buffer,
		(*ifcb).i_labsz, (*ifcb).i_blocs, vblock);
    (*ifcb).i_hcode = status;
    if (status != GOOD)
	return (P_FAIL);
    return (SUCCESS);
    }

/*
 *	move_spec - move a file spec.  Internal to i_ package.
 *
 *	NOTES:
 *
 *		- Stops on blank in file spec.
 *		- Cuts output string off at max length I_FSSIZ.
 */

FUNCTION static COUNT move_spec
(
 TEXT		source[],		/* in:  source file spec	*/
 TEXT		dest[]			/* out: destination file spec	*/
 )
    {
    COUNT		i;
    COUNT		source_len;

    source_len = s_length(source);		/* get length of source string	*/
    for (i = 0; i < source_len; i++)
	{
	if (source[i] == ' '  ||  i >= I_FSSIZ)
	    {
	    dest[i] = EOS;
	    return(i);
	    }
	dest[i] = source[i];
	}
    dest[i] = EOS;
    return(i);
    }

/* 	read_image. Read data from an image file.
 *
 * 	NOTE: If start block is beyond the physical end-of-file, 
 *            no data is actually read.
 */

FUNCTION  static  CODE read_image
(
 TAEINT      filedes,                /* in: descriptor of file to be read */
 TEXT	     buffer[],               /* out: buffer to read into          */
 TAEINT      bytes,                  /* in: number of bytes to read       */
 TAEINT      start_blk               /* in: block number to start I/O from*/
 )
    {

    TAEINT      num_bytes;              /* number of bytes really read       */
    LONG        offset;                 /* byte offset of start block        */
    LONG        count;

    errno = 0;
    num_bytes = 0;                      /* initialize                        */
    offset = (start_blk-1)*I_SECT;
    count = lseek(filedes, offset, 0);          /* position to start block   */
    if (count == -1)                            /* improper  seek 	     */
	return (errno);				/* return error code	     */
    num_bytes = read(filedes, buffer, bytes);  	/* read the data 	     */
    if (errno == 0)				/* no i/o error		     */
	{
	if (num_bytes < bytes)			/* not enough data in file   */
       	    zero_block(&buffer[num_bytes], 
		(bytes - num_bytes));  		/* zero pad    	     	     */
	if (num_bytes == 0) 
	    return (ERANGE);			/* inform caller 	     */
	}
    return(errno);
    }

/*
 *	write_image. Write data to an image file.
 */

FUNCTION  static CODE  write_image
(
 FUNINT      filedes,                /* in: descriptor of file to write   */
 TEXT	     buffer[],               /* in: input data buffer             */
 FUNINT      bytes,                  /* in: number of bytes to write      */
 FUNINT	     max_blks,		     /* in: max number of blocks in file  */
 FUNINT      start_blk               /* in: block number to start I/O from
					starts with 1		     */
 )
    {
    int		num_bytes;              /* number of bytes really written    */
    TAEINT      nbytes, max_ext;
    LONG        offset;                 /* byte offset of start block        */
    LONG        count;

    errno = 0;
    num_bytes = 0;                      /* initialize                        */
    if (start_blk > max_blks)
	return (ERANGE);		/* block number out of range	     */

    max_ext = (max_blks - start_blk + 1)*I_SECT;       /* maximum extension  */
    nbytes = (bytes < max_ext) ?                       /* if too much data   */
        bytes : max_ext;                               /* then truncate      */
    offset = (start_blk-1)*I_SECT;
    count = lseek(filedes, offset, 0);         	/* position to start block   */
    if (count > -1)                                      /* successful       */
        num_bytes = write(filedes, buffer, nbytes);      /* write the data   */
    return(errno);
    }

