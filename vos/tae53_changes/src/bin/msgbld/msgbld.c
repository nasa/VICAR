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



/*
 * 	MSGBLD. Utility program to build a index file corresponding to the
 *	help message file supplied by the user.
 *
 *	The help message file is assumed to be a text file, consisting of
 *	message keys and the associated help text in the following form:
 *	
 *	.key	key-x
 *	textline-1
 *	.
 *	.			(for key x)
 *	textline-n
 *
 *	The output message index file consists of a number of records, the
 * 	nth one	cosisting of key-n in the help message file and the 
 *	corresponding record position context. Any time the message file is 
 *	updated, the MSGBLD utility should be invoked to create a new index 
 *	file corresponding to it.
 *
 *	The Help message file must have the file type of HLP-TYPE. The file is
 *	located in the library specified in theinput filespec.
 * 	The message index file has the same name as the text file, with the 
 *	type INX_TYPE. It is created in the same library where the message file
 *	is located.
 */

/*
 *	CHANGE LOG:
 *
 *	16-may-83	Change d_init() calling sequence...dm
 *	21-mar-84	Update d_init() calling sequence...dm
 *	05-sep-84	Update f_crack() calling sequence...lia
 *	12-may-87	Cleanup up extraneous stuff...ljn
 *	30-jan-90	Don't print out version...ljn
 *	31-jul-91	Braces around static string initializer...ljn
 *      09-dec-93       Solaris Port: Deleted GLOBAL version variable...ws
 *
 */


#include	"stdh.inp"
#include	"taeconf.inp"
#include	"parblk.inc"
#include	"fileinc.inp"
#include 	"tmhost.inp"
#include	"dirinc.inc"
#include	"terminc.inc"

#include "taeintproto.h"



FUNCTION  static VOID  init_msg(void);
FUNCTION  static  CODE open_msg(

    struct  PARBLK	*parblk,		/* in: parameter block 	*/
    struct  SFILE	*sfile, 		/* out: message file SFILE 	*/
    struct  FSBLOCK	*fsblock,		/* out: FSBLOCK for message file */
    FUNINT		lun			/* in: lun to use for file i/o */
);
FUNCTION  static CODE  index_bld(

    struct  SFILE	*tsfile,	/* in: SFILE for message text file */
    struct  FSBLOCK	*tfsblock,	/* in: FSBLOCK for text file       */
    struct  SFILE	*isfile,	/* out: SFILE for index file       */
    COUNT		ilun		/* in: lun to use for index file   */
);



    FUNCTION  int main(void)		

    {
    struct  PARBLK	parblk;		/* block to receive parameters form tm */
    struct  FSBLOCK	txt_fsblk;	/* fsblock for message text file  */
    struct  SFILE	txt_sfile;	/* sfile for message text file	  */
    struct  SFILE	inx_sfile;
    COUNT	txt_lun, inx_lun;	/* lun's to use		  	  */
    CODE		code;
 

    init_msg();				/* send initiation message	*/

    txt_lun = 1;
    inx_lun = 2;
    p_inim(&parblk, sizeof(struct PARBLK), P_ABORT);	/* initialise in abort mode 	*/
    						/* open the message file  */
    code = open_msg(&parblk, &txt_sfile, &txt_fsblk, txt_lun);
    if (code == SUCCESS)			
	code = index_bld(&txt_sfile, &txt_fsblk, &inx_sfile, 
		inx_lun);			/* build the index file   */

    if (code == SUCCESS)
	{
	parm_err("Index file '%s.%s' created successfully .", "TAE-MSGBSUCC", 
		(uintptr_t)txt_fsblk.name, (uintptr_t)INX_TYPE, 0);
	z_exit(1, "TAE-MSGBSUCC");		/* $sfi, $skey indicate success */
	}
    else
	z_exit(-1, "TAE-MSGBFAIL");		/* $sfi, $skey indicate fail */
    return(0);
    }

/*
 *	init_msg. Write initiation message on the terminal.
 */

    FUNCTION  static VOID  init_msg(void)

    {
    
    CODE	termtype;
    COUNT	termlines, termcols;		
    TEXT	buffer[80]; 

    t_init(&termtype, &termlines, &termcols);
    s_copy("	TAE  utility process MSGBLD", buffer);
    t_write(buffer, T_DOUBLE);		/* write msg to terminal */
    t_write("", T_STDCC);
    return;
    }

/*
 *	open_msg. Open the help-message file.
 */
   
    FUNCTION  static  CODE open_msg(

    struct  PARBLK	*parblk,		/* in: parameter block 	*/
    struct  SFILE	*sfile, 		/* out: message file SFILE 	*/
    struct  FSBLOCK	*fsblock,		/* out: FSBLOCK for message file */
    FUNINT		lun			/* in: lun to use for file i/o */
    )

    {
    TEXT		file_spec[FSPECSIZ+1];	/* input file spec	*/
    COUNT		count;
    CODE		code;
    TEXT		**fvector;		/* pointer to string vector OM*/
    TEXT		errstr[STRINGSIZ+1];
    static TEXT		null[] = "";

    code = p_string(parblk, "FILE", &fvector, &count);	/* get file spec from parblk */
    s_copy(fvector[0], file_spec);
    f_crack(file_spec, null, null, MSG_TYPE, fsblock, errstr);	/* break to components */
    if (!s_equal((*fsblock).type, MSG_TYPE))		
	{
	parm_err("File type should be '%s'.", "TAE-BADTYPE", (uintptr_t)MSG_TYPE,0,0);	
	return(FAIL);
	}
    
    code = f_opnblk(sfile, lun, fsblock, F_READ);	/* open file directly */
    if (code == F_FILERR)
	parm_err("Could not open file '%s.%s' in library '%s'.", "TAE-MSGOPEN",
		(uintptr_t)(*fsblock).name, (uintptr_t)(*fsblock).type, (uintptr_t)(*fsblock).libr);
    else if (code == F_NOFILE)
	parm_err("Could not find file '%s.%s' in library '%s'.", "TAE-NOFILE", 
		(uintptr_t)(*fsblock).name, (uintptr_t)(*fsblock).type, (uintptr_t)(*fsblock).libr);
    return(code);
    }

/*
 * 	index_bld. Build an index file corresponding to the message file.
 */

    FUNCTION  static CODE  index_bld(

    struct  SFILE	*tsfile,	/* in: SFILE for message text file */
    struct  FSBLOCK	*tfsblock,	/* in: FSBLOCK for text file       */
    struct  SFILE	*isfile,	/* out: SFILE for index file       */
    COUNT		ilun		/* in: lun to use for index file   */
    )

    {
    struct  DIRBLK	dirblk;		/* directive block for text file   */
    struct  INXREC	inxrec;		/* output record 		   */
    TEXT		key[STRINGSIZ+1];
    TEXT		dirctv[STRINGSIZ+1];
    CODE 		code;
    static TEXT		*sys_char[1] = {""};		/* all values okay */


    /*	 open index file with same name and library 		*/
    code = f_open(isfile, ilun, (*tfsblock).libr, (*tfsblock).name, 
		INX_TYPE, F_WRITE);
    if (code != SUCCESS)
	{
	parm_err("Could not open index file in library '%s'.", "TAE-INXOPEN",
		(uintptr_t)(*tfsblock).libr,0,0);
	return(FAIL);
	}
    d_init(&dirblk, tsfile, (*tfsblock).libr, sys_char, 1);	
						/* initialize for d_ calls */
    do
	{
	code = d_dirctv(&dirblk, dirctv, key);	/* read next directive record */
	if (s_equal(dirctv, ".end") || code != SUCCESS )
	    break;				/* end of file		*/
    	if (!s_equal(dirctv, ".key"))		/* not key specification */
	    {
	    parm_err("Invalid directive '%s' encountered .", "TAE-INVDIR",
		(uintptr_t)dirctv,0,0);
	    return(FAIL);
	    }
	bytmov((GENPTR) key, (GENPTR) inxrec.key, KEYSIZ+1);  /* copy key */
	MOVE_STRUCT((*tsfile).posctx, inxrec.posctx);	      /* copy position context */
	code = f_bwrite(isfile, (GENPTR) &inxrec, sizeof(inxrec));   /* write to file  */
 	}
    while (code == SUCCESS);

    if (code == D_EOF)				/* end of message text file */
	{
	code = SUCCESS;			
	f_close(isfile, F_KEEP);		/* close index file	*/
	}
    f_close(tsfile, F_KEEP);			/* close text file	*/
    return(code);
    }
