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



/* TPL CHECKOUT FILE_TIME=27-DEC-1987 13:49 DUA1:[TAEV2.OLB.GENLIB]DYNAMICPDF.C;4 */
/* TPL CHECKOUT FILE_TIME=14-AUG-1987 13:50 DUA1:[TAEV2.OLB.GENLIB]DYNAMICPDF.C;3 */
/* TPL CHECKOUT FILE_TIME= 7-AUG-1987 10:37 DUA1:[TAEV2.OLB.GENLIB]DYNAMICPDF.C;2 */
/* TPL CHECKOUT FILE_TIME=15-JUL-1987 19:13 DUA1:[TAEV2.OLB.GENLIB]DYNAMICPDF.C;1 */
/*
 *	Dp_ functions to build and manipulate a dynamic pdf
 *
 *	CHANGE LOG:
 *      2-JUNE-87	Created...tpl
 *      6-AUG-87	Initialize link in Dp_ExtParms...tpl
 *      14-aug-87	Check linenum before setting page mark...tpl
 *      18-dec-87	Corrected casting stuffs...tpl
 *      04-apr-88	Added Dp_GetDynParms...tpl
 *      24-jun-88	set header version as "DynamicPDF"...tpl
 *	02-aug-89	Do some inits to avoid possible compiler warns...ljn
 *	22-oct-92	Prototyping tae_alloc is unnecessary and Ultrix 4.3
 *			does not like it...rt
 *
 */
#include	"taeconf.inp"	/* TAE configuration (REQUIRED)		*/
#include        "tminc.inc"
#include	"parblk.inc"	/* parameter block definitions		*/
#include	"symtab.inc"	/* TM symbol table			*/
#include        "compiled.inc"     /* compiled pdf definition              */
#include        "vminc.inc"         /* handle parblk definition             */
#include        "fileinc.inp"
#include "taeintproto.h"
#include <string.h>
/*
    local declaration
*/

    struct    HELP_LINK {
                        struct TXTSTOR strings;
                        TEXT    parmname[9];
                        struct HELP_LINK *link;
                        };

    struct    DP_STRUCT {
                        TEXT      type[STRINGSIZ+1]; 
                                           /* pdf type(process-compiled,etc)*/
                        TEXT      execute[FSPECSIZ+1]; 
                        TEXT      name[STRINGSIZ+1]; 
                        struct TXTSTOR   title;
                        struct TXTSTOR   body;    /* text strings of the body 
                                                     of pdf */
                        struct HELP_LINK help;
                        struct HELP_LINK level1;  /* head of level 1 help         */
                        struct HELP_LINK level2;  /* head of level 2 help         */
                        struct VM_STRUCT *vm;     /* virtual parblk pointer     */
                        };


FUNCTION static struct HELP_LINK * dp_FindParm
(
 struct HELP_LINK *help,
 TEXT      parmname[]
 );
FUNCTION static VOID dp_FreeText
(
 struct TXTSTOR   *t                 /* IN: text storage pointer */
 );
FUNCTION static CODE dp_ReadParms
(
 struct VM_STRUCT	*vm,		/* in/out: dynamic PDF handle	*/
 struct SFILE       *file          /* in/out: file block           */
 );
FUNCTION static VOID dp_InitHeader
(
 struct DP_STRUCT    *dp,            /* dynamic PDF handle               */
 struct CP_HEADER	*header	/* out: skel compiled PDF header    */
 );
FUNCTION static CODE dp_OutParblk
(
 struct SFILE	*file,		/* in/out: compiled PDF file		*/
 struct VM_STRUCT	*vm,		/* in:  symbol table to build from	*/
 struct POSCTX	*first_pos	/* out: file posit of 1st record in grp	*/
 );
FUNCTION static CODE dp_DeleteTemp
(
 TEXT		filespec[FSPECSIZ+1]	/* pdf file spec	*/
 );



/*
 *	Dp_New.  Create a dynamic pdf structure and return handle
 */

    FUNCTION GENPTR Dp_New(void)

    {
    GENPTR h;           /* dynamic pdf handle                */

    /*
        allocate memory space for the header 
    */
    h = tae_alloc ( 1, sizeof(struct DP_STRUCT) ); 
    zero_block ( h, sizeof (struct DP_STRUCT) );       

    return( h );
    }

/*
 *        Dp_SetType - set the type of the pdf
 *
 */
    FUNCTION CODE Dp_SetType 
(
 GENPTR   h,
 CODE     type
)
    {
    struct DP_STRUCT *dp;

    dp = (struct DP_STRUCT *)h;

    if ( !NULLSTR( (*dp).type ) )
        return(FAIL);

    if ( type == P_PROCESS )
        s_copy ( "PROCESS-COMPILED", (*dp).type );
    else if ( type == P_PROCEDURE)
        s_copy ( "PROCEDURE-COMPILED", (*dp).type );
    else if ( type == P_PARMSET)
        s_copy ( "PARMSET-COMPILED", (*dp).type );
    else if ( type == P_GLOBAL)
        s_copy ( "GLOBAL-COMPILED", (*dp).type );
    else
        return(P_BADTYPE);

    return(SUCCESS);
    }

/*
 *        Dp_ReadPDF - read in an existing pdf 
 *
 *
 */
FUNCTION CODE Dp_ReadPDF
(
 GENPTR     h,                         /* handle created by dp_new */
 TEXT       filespec[]                /* name of a compiled pdf   */
 )
    {
    TAEINT     lun = 0;                       /* dummy lun */
    struct CP_HEADER	header;		/* buffer for compiled files header rec	*/
    CODE		code;
    COUNT		recsize, i, linecount;
    struct SFILE	file;		
    TEXT                library [FLIBRSIZ+1];
    TEXT                stmt[STRINGSIZ+1];
    struct DP_STRUCT    *dp;
    BOOL                firsthelp;
    struct HELP_LINK    *curhelp;

    getulib ( library );
    code = f_opnspc ( &file, lun, filespec, library, NULL, CPD_TYPE, F_READ );
   if ( code != SUCCESS)
        return(code);

    dp = (struct DP_STRUCT *)h;

    (*dp).vm = (struct VM_STRUCT *)Vm_New(P_CONT);
/*
    read first record to get the pdf type 
*/
    code = f_bread(&file, (GENPTR)(*dp).type, STRINGSIZ, &recsize);
    if (code != SUCCESS)
	{
        f_close(&file, F_KEEP);
        return(code);
        }
/* 
    Read & process the header record 
*/

    code = f_bread(&file, (GENPTR)&header, sizeof(struct CP_HEADER), &recsize);
    if (code != SUCCESS)
	{
        f_close(&file, F_KEEP);
        return(code);
        }
    if (header.type != C_HEADER)
	{
        f_close(&file, F_KEEP);
        return(P_NOTCOMP);
        }

    s_copy ( header.execute, (*dp).execute );

/* Now the PARBLKs	*/

    if (header.before.possav)			/* if "before" PARBLK exists	*/
	{
	f_setpos(&file, &header.before);		/* pos to 1st "before" PARBLK	*/
	code = dp_ReadParms((*dp).vm, &file);               /* read the before PARBLKs	*/
	if (code != SUCCESS)
	    {
            f_close(&file, F_KEEP);
            return(code);
            }
	}

/* Now position for normal PDF procedure processing.	*/

    (*dp).body.numline = 0;

    if ( s_equal( "PROCEDURE-COMPILED", (*dp).type ) )

      {
/*
      read the body (process,parmset, and global don't have body )
*/
      if (header.body.possav)			/* if "before" PARBLK exists	*/
        {
        f_setpos(&file, &header.body);/* position for procedure body */
/*
        read the body till END-PROC to find out how many lines
*/
        linecount = 0;
        while ( FOREVER )
            {
            code = f_read ( &file, stmt );
            if ( code != SUCCESS )
                {
                f_close ( &file, F_KEEP);
                return(code);
                }

            if ( s_lseq ( "END-PROC", stmt ) )
                break;
            linecount++;
            }


        linecount = linecount-1;              /* subtract BODY */

        f_setpos(&file, &header.body);/* position for procedure body */

        code = f_read ( &file, stmt );        /* skip BODY line */
        if ( code != SUCCESS )
            return(FAIL);

        (*dp).body.tp = (TEXT **)tae_alloc ( 1, linecount*sizeof(TEXT *) );

        for ( i = 0; i < linecount; i++)
            {
            code = f_read ( &file, stmt );
            if ( code != SUCCESS )
                return(code);

            (*dp).body.tp[i] = tae_alloc(1, strlen(stmt)+1 );
            s_copy ( stmt, (*dp).body.tp[i] );
            }
        (*dp).body.numline = i;
        }
      }    
    if (header.title.possav)			/* if title exists	*/
        {
/*
        read title
*/
        f_setpos(&file, &header.title);	/* position for title */
/*
        read the title till next directive or EOF to find out how many lines
*/
        linecount = 0;
        while ( FOREVER )
            {
            code = f_read ( &file, stmt );

            if ( code == F_EOF )
                break;

            if ( code != SUCCESS )
                {
                f_close ( &file, F_KEEP);
                return(code);
                }

            if (      s_lseq ( ".HELP", stmt )  ||
                      s_lseq ( ".LEVEL1", stmt )  ||
                      s_lseq ( ".LEVEL2", stmt )  ||
                      s_lseq ( ".END", stmt )  )
                break;
            linecount++;
            }


        linecount = linecount-1;              /* subtract .TITLE */

        f_setpos(&file, &header.title);/* position for procedure body */

        code = f_read ( &file, stmt );        /* skip .TITLE line */
        if ( code != SUCCESS )
            return(FAIL);

        (*dp).title.tp = (TEXT **)tae_alloc ( 1, linecount*sizeof(TEXT *) );
        for ( i = 0; i < linecount; i++)
            {
            code = f_read ( &file, stmt );
            if ( code == F_EOF )
                break;
            if ( code != SUCCESS )
                return(FAIL);
            (*dp).title.tp[i] = tae_alloc(1, strlen(stmt)+1 );
            s_copy ( stmt, (*dp).title.tp[i] );
            }
        (*dp).title.numline = i;
        }
            
/*
    read general help
*/
    if ( header.help.possav)
        {
/*
        read the help till next directive or EOF to find out how many lines
*/
        linecount = 0;
        while ( FOREVER )
            {
            code = f_read ( &file, stmt );

            if ( code == F_EOF )
                break;

            if ( code != SUCCESS )
                {
                f_close ( &file, F_KEEP);
                return(code);
                }

            if (      s_lseq ( ".LEVEL1", stmt )  ||
                      s_lseq ( ".LEVEL2", stmt )  ||
                      s_lseq ( ".END", stmt )  )
                break;
            linecount++;
            }


        linecount = linecount-1;              /* subtract .HELP */

        f_setpos(&file, &header.help);/* position for procedure body */

        code = f_read ( &file, stmt );        /* skip .HELP line */
        if ( code != SUCCESS )
            return(FAIL);

        (*dp).help.strings.tp = (TEXT **)tae_alloc ( 1, linecount*sizeof(TEXT *) );
        for ( i = 0; i < linecount; i++)
            {
            code = f_read ( &file, stmt );
            if ( code == F_EOF )
                break;
            if ( code != SUCCESS )
                return(FAIL);
            (*dp).help.strings.tp[i] = tae_alloc(1, strlen(stmt)+1 );
            s_copy ( stmt, (*dp).help.strings.tp[i] );
            }
        (*dp).help.strings.numline = i;
        }
            
/*
    read level 1 help
*/
    if ( header.level1.possav )
        {
/*
        read the level 1 help till next directive or EOF 
        to find out how many lines for each variables
*/
        f_setpos(&file, &header.level1);/* position for level 1 help */

        code = f_read ( &file, stmt );        /* skip .LEVEL1 line */
        if ( code != SUCCESS )
            return(FAIL);

        firsthelp = TRUE;
        curhelp = &(*dp).level1;

        while ( FOREVER )
            {
            code = f_read ( &file, stmt );

            if ( code == F_EOF )
                break;

            if ( code != SUCCESS )
                {
                f_close ( &file, F_KEEP);
                return(code);
                }

            if (      s_lseq ( ".HELP", stmt )  ||
                      s_lseq ( ".LEVEL2", stmt )  ||
                      s_lseq ( ".END", stmt )  )
                break;
            if ( s_lseq ( ".VAR", stmt ) &&  !firsthelp )
                {
                /* find end of chain */
                for ( curhelp = &(*dp).level1; (*curhelp).link != NULL;
                      curhelp = (struct HELP_LINK *)(*curhelp).link )
                    ;
                (*curhelp).link =  (struct HELP_LINK *)tae_alloc ( 1, sizeof(struct HELP_LINK ) );
                curhelp = (struct HELP_LINK *)(*curhelp).link ;
                s_copy ( stmt+5, (*curhelp).parmname );
                (*curhelp).strings.numline = 0;
                continue;
                }
            else
                {
                if ( s_lseq ( ".VAR", stmt ) )
                    {
                    firsthelp = FALSE;
                    (*curhelp).strings.numline = 0;
                    s_copy ( stmt+5, (*curhelp).parmname );
                    continue;
                    }
                }

            (*curhelp).strings.numline++;

            }

        f_setpos(&file, &header.level1);/* position for level 1 */

        code = f_read ( &file, stmt );        /* skip .LEVEL1 line */
        if ( code != SUCCESS )
            return(FAIL);

        for ( curhelp = &(*dp).level1; curhelp != NULL; 
              curhelp = (struct HELP_LINK *)(*curhelp).link )
          {
          (*curhelp).strings.tp = 
                (TEXT **)tae_alloc ( 1, 
                         (*curhelp).strings.numline*sizeof(TEXT *) );
          code = f_read ( &file, stmt );
          if ( code != SUCCESS )        /* skip .VAR */
                return(FAIL);
          for ( i = 0; i < (*curhelp).strings.numline; i++)
            {
            code = f_read ( &file, stmt );
            if ( code == F_EOF )
                break;
            if ( code != SUCCESS )
                return(FAIL);
            
            (*curhelp).strings.tp[i] = tae_alloc(1, strlen(stmt)+1 );
            s_copy ( stmt, (*curhelp).strings.tp[i] );
            }
          } 
        }
            
/*
    read level 2 help
*/
    if ( header.level2.possav )
        {
/*
        read the level 2 help till next directive or EOF 
        to find out how many lines for each variables
*/
        f_setpos(&file, &header.level2);/* position for level 2 help */

        code = f_read ( &file, stmt );        /* skip .level2 line */
        if ( code != SUCCESS )
            return(FAIL);

        firsthelp = TRUE;
        curhelp = &(*dp).level2;

        while ( FOREVER )
            {
            code = f_read ( &file, stmt );

            if ( code == F_EOF )
                break;

            if ( code != SUCCESS )
                {
                f_close ( &file, F_KEEP);
                return(code);
                }

            if (      s_lseq ( ".HELP", stmt )  ||
                      s_lseq ( ".LEVEL2", stmt )  ||
                      s_lseq ( ".END", stmt )  )
                break;
            if ( s_lseq ( ".VAR", stmt ) &&  !firsthelp )
                {
                /* find end of chain */
                for ( curhelp = &(*dp).level2; (*curhelp).link != NULL;
                      curhelp = (struct HELP_LINK *)(*curhelp).link )
                    ;
                (*curhelp).link = (struct HELP_LINK *)tae_alloc ( 1, sizeof(struct HELP_LINK ) );
                curhelp = (struct HELP_LINK *)(*curhelp).link ;
                s_copy ( stmt+5, (*curhelp).parmname );
                (*curhelp).strings.numline = 0;
                continue;
                }
            else
                {
                if ( s_lseq ( ".VAR", stmt ) )
                    {
                    firsthelp = FALSE;
                    (*curhelp).strings.numline = 0;
                    s_copy ( stmt+5, (*curhelp).parmname );
                    continue;
                    }
                }

            (*curhelp).strings.numline++;

            }

        f_setpos(&file, &header.level2);/* position for procedure body */

        code = f_read ( &file, stmt );        /* skip .level2 line */
        if ( code != SUCCESS )
            return(FAIL);

          code = f_read ( &file, stmt );
          if ( code != SUCCESS )        /* skip .VAR */
                return(FAIL);

        for ( curhelp = &(*dp).level2; curhelp != NULL; 
              curhelp = (struct HELP_LINK *)(*curhelp).link )
          {
          (*curhelp).strings.tp = 
                (TEXT **)tae_alloc ( 1, 
                         (*curhelp).strings.numline*sizeof(TEXT *) );
          for ( i = 0; i < (*curhelp).strings.numline; i++)
            {
            code = f_read ( &file, stmt );
            if ( code == F_EOF )
                break;
            if ( code != SUCCESS )
                return(FAIL);
            
            (*curhelp).strings.tp[i] = tae_alloc(1, strlen(stmt)+1 );
            s_copy ( stmt, (*curhelp).strings.tp[i] );
            }
          }
        }
            
/*
    close the compiled pdf
*/
    f_close( &file, F_KEEP );

    return(SUCCESS);


    }

/*
 *        Dp_SetTitle - set/replace the title text of a pdf
 *
 *
 */
FUNCTION CODE Dp_SetTitle 
(
 GENPTR    h,
 COUNT     count,
 TEXT      **title
 )
    {
    struct DP_STRUCT *dp;
    COUNT     i;

    if ( count <= 0 )
        return(FAIL);

    dp = (struct DP_STRUCT *)h;

    if ( (*dp).title.numline > 0 )
        dp_FreeText( &(*dp).title );

    (*dp).title.numline = count;
    (*dp).title.tp = (TEXT **)tae_alloc(1, count*sizeof(TEXT *) );

    for ( i=0; i < count; i++ )        /* copy the strings */
        {
        (*dp).title.tp[i] = tae_alloc(1, STRINGSIZ+1 );
        s_copy ( title[i], (*dp).title.tp[i] );
        }
    return ( SUCCESS );
    }

/*
 *        Dp_SetHelp - set/replace general pdf help
 *
 *
 */
FUNCTION CODE Dp_SetHelp 
(
 GENPTR    h,
 COUNT     count,
 TEXT      **help
 )
    {
    struct DP_STRUCT *dp;
    COUNT     i;

    if ( count <= 0 )
        return(FAIL);

    dp = (struct DP_STRUCT *)h;

    if ( (*dp).help.strings.numline > 0 )
        dp_FreeText ( &(*dp).help.strings );

    (*dp).help.strings.numline = count;
    (*dp).help.strings.tp = (TEXT **)tae_alloc(1, count*sizeof(TEXT *) );

    for ( i=0; i < count; i++ )        /* copy the strings */
        {
        (*dp).help.strings.tp[i] = tae_alloc(1, STRINGSIZ+1 );
        s_copy ( help[i], (*dp).help.strings.tp[i] );
        }
    return ( SUCCESS );
    }

/*
 *        Dp_SetBody - set/replace the body of a pdf
 *
 *
 */
FUNCTION CODE Dp_SetBody 
(
 GENPTR    h,
 COUNT     count,
 TEXT      **body
 )

    {
    struct DP_STRUCT *dp;
    COUNT     i;

    if ( count <= 0 )
        return(FAIL);

    dp = (struct DP_STRUCT *)h;

    if ( (*dp).body.numline > 0 )
        dp_FreeText ( &(*dp).body );

    (*dp).body.numline = count;
    (*dp).body.tp = (TEXT **)tae_alloc(1, count*sizeof(TEXT *) );

    for ( i=0; i < count; i++ )        /* copy the strings */
        {
        (*dp).body.tp[i] = tae_alloc(1, STRINGSIZ+1 );
        s_copy ( body[i], (*dp).body.tp[i] );
        }
    return ( SUCCESS );
    }

/*
 *        Dp_SetParms - set/replace the parameter blocks
 *
 *
 */
FUNCTION CODE Dp_SetParms 
(
 GENPTR h,
 GENPTR vmh
 )
    {
    struct DP_STRUCT *dp;
    struct VM_STRUCT *vm, *vmin;
    struct VARIABLE  *v, *vp, *last_var;
    CODE             code;

    dp = (struct DP_STRUCT *)h;
    if ( (*dp).vm != NULL )
      Vm_Free ((*dp).vm );
    vmin = (struct VM_STRUCT *)vmh;
    vm = (*dp).vm = (struct VM_STRUCT *)Vm_New( (*vmin).npblk.mode );
    
    (*vm).npblk.msgtyp = (*vmin).npblk.msgtyp;
    (*vm).npblk.last = (*vmin).npblk.last;
    (*vm).npblk.actmod = (*vmin).npblk.actmod;
    (*vm).npblk.vers = (*vmin).npblk.vers;

    for ( vp=(*vmin).npblk.symtab.link; vp != NULL; vp = (*vp).v_link )
        {
        v = Vm_AllocVar (&(*vm).npblk.symtab);
        code = Vm_SpCopyVar(vp, v);  	/* copy the variable	    */
        if (code != SUCCESS)
            return(code);
                                                /* find last var            */	
        for (last_var=(struct VARIABLE *)&(*vm).npblk.symtab;
                      (*last_var).v_link != NULL; 
                      last_var = (*last_var).v_link )
                ;
        (*last_var).v_link = v;
        }


    return(SUCCESS);
    }

/*
 *        Dp_SetLevel1 - set/replace the level1 help of a parameter
 *
 *
 */
FUNCTION CODE Dp_SetLevel1
(
 GENPTR    h,
 TEXT      parmname[],
 COUNT     count,
 TEXT      **help
 )
    {
    struct DP_STRUCT *dp;
    COUNT     i;
    struct HELP_LINK *hlink;

    if ( count <= 0 )
        return(FAIL);

    dp = (struct DP_STRUCT *)h;

    hlink = dp_FindParm ( &(*dp).level1, parmname );
    if ( hlink == NULL ) 
        return(FAIL);

    if ( (*hlink).strings.tp != NULL)
        dp_FreeText ( &(*hlink).strings );

    (*hlink).strings.numline = count;
    (*hlink).strings.tp = (TEXT **)tae_alloc(1, count*sizeof(TEXT *) );

    for ( i=0; i < count; i++ )        /* copy the strings */
        {
        (*hlink).strings.tp[i] = tae_alloc(1, STRINGSIZ+1 );
        s_copy ( help[i], (*hlink).strings.tp[i] );
        }
    return ( SUCCESS );
    }

/*
 *        Dp_SetLevel2 - set/replace the level2 help of a parameter
 *
 *
 */
FUNCTION CODE Dp_SetLevel2
(
 GENPTR    h,
 TEXT      parmname[],
 COUNT     count,
 TEXT      **help
 )
    {
    struct DP_STRUCT *dp;
    COUNT     i;
    struct HELP_LINK *hlink;

    if ( count <= 0 )
        return(FAIL);

    dp = (struct DP_STRUCT *)h;

    hlink = dp_FindParm ( &(*dp).level2, parmname );
    if ( hlink == NULL ) 
        return(FAIL);

    if ( (*hlink).strings.tp != NULL)
        dp_FreeText ( &(*hlink).strings );

    (*hlink).strings.numline = count;
    (*hlink).strings.tp = (TEXT **)tae_alloc(1, count*sizeof(TEXT *) );

    for ( i=0; i < count; i++ )        /* copy the strings */
        {
        (*hlink).strings.tp[i] = tae_alloc(1, STRINGSIZ+1 );
        s_copy ( help[i], (*hlink).strings.tp[i] );
        }
    return ( SUCCESS );
    }

/*
 *        Dp_ExtParms - extract parameters from a dynamic pdf instance
 *
 *
 */
FUNCTION CODE Dp_ExtParms
(
 GENPTR h,
 GENPTR vmh
 )
    {

    struct DP_STRUCT *dp;
    struct VM_STRUCT *vm, *vmout;
    struct VARIABLE  *v, *vp, *last_var;
    CODE             code;

    dp = (struct DP_STRUCT *)h;
    vm = (struct VM_STRUCT *)vmh;
    vmout = (struct VM_STRUCT *)(*dp).vm;
    
    (*vm).npblk.symtab.link = NULL;
    (*vm).npblk.msgtyp = (*vmout).npblk.msgtyp;
    (*vm).npblk.last = (*vmout).npblk.last;
    (*vm).npblk.actmod = (*vmout).npblk.actmod;
    (*vm).npblk.vers = (*vmout).npblk.vers;

    for ( vp=(*vmout).npblk.symtab.link; vp != NULL; vp = (*vp).v_link )
        {
        v = Vm_AllocVar (&(*vm).npblk.symtab);
        code = Vm_SpCopyVar(vp, v);  	/* copy the variable	    */
        if (code != SUCCESS)
            return(code);
                                                /* find last var            */	
        for (last_var=(struct VARIABLE *)&(*vm).npblk.symtab;
                      (*last_var).v_link != NULL; 
                      last_var = (*last_var).v_link )
                ;
        (*last_var).v_link = v;
        }


    return(SUCCESS);
    }

/*
 *        Dp_Save - save the pdf
 *
 *
 */
FUNCTION CODE Dp_Save 
(
 GENPTR    h,
 TEXT      filespec[]
 )
    {
    struct COMP_HOUSEKEEP housekeep;		/* local copy of housekeeping info*/
    struct CP_HEADER	header;			/* local copy of file header	*/
    struct POSCTX	pos;
    TEXT                library [FLIBRSIZ+1];
    struct SFILE        file;
    struct DP_STRUCT    *dp;
    COUNT               i, body_line;
    CODE                code;
    struct CP_SUBCMD	sub_rec;
    struct HELP_LINK    *helplink;
    TEXT                tempstring[STRINGSIZ+1];
    TAEINT    lun = 0;                      /* dummy lun to use for the file  */


   dp = (struct DP_STRUCT *)h;

   if ( NULLSTR ( filespec ) )
      s_copy ( (*dp).name, tempstring );
   else
      s_copy ( filespec, tempstring );

   getulib ( library );

   code = f_opnspc ( &file, lun, tempstring, library, NULL, CPD_TYPE, F_WRITE );
   if ( code != SUCCESS)
        return(code);


    body_line = 0;               		/* BODY line # in uncompiled PDF*/

    code = f_write(&file, (*dp).type);		/* write type as first record
                                                   in compiled file*/
    if (code != SUCCESS)
       {
       f_close ( &file, F_DELETE );
       return(code);
       }

    dp_InitHeader( dp, &header );	        /* initialize header rec*/
    code = f_bwrite(&file, 			/* skeleton header rec to compiled*/
		(GENPTR)&header, sizeof(struct CP_HEADER));
    if (code != SUCCESS)
       {
       f_close ( &file, F_DELETE );
       return(code);
       }
    MOVE_STRUCT( file.posctx, housekeep.pos_header);	/* save header pos*/

/*
    write empty subcommand block
*/
    sub_rec.type = C_SUBCMD;
    sub_rec.subcmd[0] = EOS;	/* build & write termination...		*/
    code = f_bwrite(&file,	/* CP_SUBCMD record to compiled file	*/
	    (GENPTR)&sub_rec, sizeof(struct CP_SUBCMD));
    if (code != SUCCESS)
	return(code);

/*
    write parblk(s)
*/

    code = dp_OutParblk (&file, (*dp).vm, &pos);/* "before" PARBLK(s)	*/
    if (code != SUCCESS)
	return(FAIL);
    MOVE_STRUCT(pos, header.before);			/* save "before" pos	*/


/* now the body info	*/

    if ( (*dp).body.numline > 0 )
    {
      code = f_write(&file, "BODY");		/* BODY record to compiled file*/
      if ( code != SUCCESS )
        {
        f_close ( &file, F_DELETE );
        return(code);
        }
      MOVE_STRUCT( file.posctx, header.body);/* save BODY pos		*/
      for ( i=0; i < (*dp).body.numline; i++ )
        {
        code = f_write ( &file, (*dp).body.tp[i] );
        if ( code != SUCCESS )
            {
            f_close ( &file, F_DELETE );
            return(code);
            }
        }
    
      code = f_write(&file, "END-PROC");		/* BODY record to compiled file*/
      if ( code != SUCCESS )
        {
        f_close ( &file, F_DELETE );
        return(code);
        }
      header.body_line = i+1;
    }

/* now do help info             */

   if ( (*dp).title.numline != 0 )
        {
        code = f_write(&file, ".TITLE");
        if ( code != SUCCESS )
            return(FAIL);
        MOVE_STRUCT( file.posctx, header.title);/* save BODY pos		*/
         
        for ( i=0; i < (*dp).title.numline; i++ )
            {
            code = f_write ( &file, (*dp).title.tp[i] );
            if ( code != SUCCESS )
                {
                f_close ( &file, F_DELETE );
                return(code);
                }
            }
        }
        
   if ( (*dp).help.strings.numline != 0 )
        {
        code = f_write(&file, ".HELP");
        if ( code != SUCCESS )
            return(FAIL);
        MOVE_STRUCT( file.posctx, header.help);/* save help pos		*/
         
        for ( i=0; i < (*dp).help.strings.numline; i++ )
            {
            code = f_write ( &file, (*dp).help.strings.tp[i] );
            if ( code != SUCCESS )
                {
                f_close ( &file, F_DELETE );
                return(code);
                }
            }
        }
        
   if ( (*dp).level1.strings.numline != 0 )
        {
        code = f_write(&file, ".LEVEL1");
        if ( code != SUCCESS )
            return(FAIL);
        MOVE_STRUCT( file.posctx, header.level1);/* save level 1 pos		*/
         
        for ( helplink = &(*dp).level1; helplink != NULL; 
              helplink=(*helplink).link)
            {
            s_copy (".VAR ", tempstring );
            s_append ( (*helplink).parmname, tempstring );
            i = strlen ( (*helplink).parmname ) + 5;
            tempstring[i] = EOS;
            code = f_write ( &file, tempstring );
            if ( code != SUCCESS )
                {
                f_close ( &file, F_DELETE );
                return(code);
                }

            for ( i=0; i < (*helplink).strings.numline; i++ )
                {
                code = f_write ( &file, (*helplink).strings.tp[i] );
                if ( code != SUCCESS )
                    {
                    f_close ( &file, F_DELETE );
                    return(code);
                    }
                }
            }
        }
        
   if ( (*dp).level2.strings.numline != 0 )
        {
        code = f_write(&file, ".LEVEL2");
        if ( code != SUCCESS )
            return(FAIL);
        MOVE_STRUCT( file.posctx, header.level2);/* save level 2 pos	*/
         
        for ( helplink = &(*dp).level2; helplink!=NULL; 
              helplink=(*helplink).link)
            {
            s_copy (".VAR ", tempstring );
            s_append ( (*helplink).parmname, tempstring );
            i = strlen ( (*helplink).parmname ) + 5;
            tempstring[i] = EOS;
            code = f_write ( &file, tempstring );
            if ( code != SUCCESS )
                {
                f_close ( &file, F_DELETE );
                return(code);
                }

            for ( i=0; i < (*helplink).strings.numline; i++ )
                {
                code = f_write ( &file, (*helplink).strings.tp[i] );
                if ( code != SUCCESS )
                    {
                    f_close ( &file, F_DELETE );
                    return(code);
                    }
                }
            }
        }
        
/* Now re-write the header with the pointers filled in.
 * Note that for UNIX systems we do not read the record before 
 * writing as read and write to the same stream does not work properly.
 * So we position ourselves to the right record and rewrite the record
 * (i.e. under UNIX f_brewrite is the same as f_bwrite).
 */

    f_setpos(&file, &housekeep.pos_header);
#ifdef  VAX_VMS
    code = f_bread( &file, (GENPTR)&tmp_header, sizeof(struct CP_HEADER),
			&recsize);		/* dummy read to lock position	*/
    if (code != SUCCESS)
        {
        f_close ( &file, F_DELETE );
	return(code);
        }
    if (recsize != sizeof(struct CP_HEADER))
	{
	code = F_BTOOSMALL;			/* another wrong rec possibility*/
        f_close ( &file, F_DELETE );
	return(code);
	}
#endif
    code = f_brewrite(&file, 	(GENPTR)&header, 
		sizeof(struct CP_HEADER));	/* CP_HEADER record to compiled file*/ 
    if (code != SUCCESS)
        {
        f_close ( &file, F_DELETE );
	return(code);
        }
    code = f_close ( &file, F_KEEP );
    return ( code );
    }

/*
 *        dp_FindParm
 *
 *
 */
FUNCTION static struct HELP_LINK * dp_FindParm
(
 struct HELP_LINK *help,
 TEXT      parmname[]
 )

    {
    struct HELP_LINK *helpblock;

    for ( helpblock=help; helpblock != NULL; helpblock=(*helpblock).link)
        {
        if ( s_equal( (*helpblock).parmname, parmname ) )
            return( helpblock );
        }
/*  no such variable; must be new */
    if ( (*help).strings.numline == 0 && (*help).link == NULL )
        {
        s_copy ( parmname, (*help).parmname );
        return ( help );
        }
    else
        {
        /* find the end of the chain */
        for ( helpblock = help; (*helpblock).link != NULL; 
              helpblock = (*helpblock).link  )
            ;

        (*helpblock).link = (struct HELP_LINK *)
                    tae_alloc ( 1, sizeof ( struct HELP_LINK ) );

        helpblock = (*helpblock).link;

        s_copy ( parmname, (*helpblock).parmname );
        return ( helpblock );
        }
    
    }

/*
 *        dp_FreeText - free the text strings stored.
 *
 *
 */
FUNCTION static VOID dp_FreeText
(
 struct TXTSTOR   *t                 /* IN: text storage pointer */
 )
    {
    COUNT i;

    for ( i=0; i<(*t).numline; i++ )
        tae_free ( (*t).tp[i] );

    tae_free ( (*t).tp );                            /* free everything else  */

    return;
    }

/*
 *        Dp_Free - free the dynamic pdf structure
 *
 *
 */
FUNCTION VOID Dp_Free
(
    GENPTR      h              /* IN: dynamic pdf handle       */
 )
    {
    struct DP_STRUCT *dp;
    struct HELP_LINK *hlink;

    dp = (struct DP_STRUCT *)h;

    if ( (*dp).title.numline > 0 )
         dp_FreeText ( &(*dp).title );

    if ( (*dp).body.numline > 0 )
         dp_FreeText ( &(*dp).body );

/*
    free help strings entries
*/
    if ( (*dp).help.strings.numline > 0 )
         dp_FreeText ( &(*dp).help.strings );

/*
    free all level 1 entries
*/

    for (hlink = &(*dp).level1;
         hlink != NULL;
         hlink = (struct HELP_LINK *)(*hlink).link )
        {
                
        dp_FreeText( &(*hlink).strings);
        if ( hlink != &(*dp).level1 )
            {
            tae_free (hlink);
            }
        }

/*
    free all level 2 entries
*/

    for (hlink = &(*dp).level2;
         hlink != NULL;
         hlink = (struct HELP_LINK *)(*hlink).link )
        {
                
        dp_FreeText( &(*hlink).strings);
        if ( hlink != &(*dp).level2 )
            {
            tae_free (hlink);
            }
        }

    if ( (*dp).vm != NULL )
      Vm_Free ((*dp).vm );

    tae_free ( h );                            /* free everything else  */
    return;
    }

/*
 *	dp_ReadParms - input variables from PARBLK records in a compiled PDF.
 *
 *	Assumes the compiled PDF is open and positioned to the first PARBLK
 *	record to be read.
 *
 *	This function assumes that all variables in the PARBLK are to be
 *	added rather than updated.
 *
 */

FUNCTION static CODE dp_ReadParms
(
 struct VM_STRUCT	*vm,		/* in/out: dynamic PDF handle	*/
 struct SFILE       *file          /* in/out: file block           */
 )
    {
    struct LARGE_PARBLK	p;		/* PARBLK buffer for file reads	*/
    struct VARIABLE	*v, *vp, *last_var;
    CODE		code;
    COUNT		recsize;

    while (FOREVER)
	{
	code = f_bread(file, (GENPTR)&p, sizeof(p), &recsize);
	if (code == F_EOF)
	    {
            f_close(file, F_KEEP);
            return(code);
            }
	if (code != SUCCESS)
	    {
            f_close(file, F_KEEP);
            return(code);
            }
	if (p.msgtyp != C_BEFORE)
	    {
            f_close(file, F_KEEP);
            return(FAIL);
            }
	makeabs(&p.symtab, p.pool);	/* make pointers absolute		*/
	if (chk_parblk((struct PARBLK*) &p) != SUCCESS)	/* check internal integrity		*/
	    {
            f_close(file, F_KEEP);
            return(code);
            }
        for (vp=p.symtab.link; vp != NULL; vp = (*vp).v_link)
	    {
            v = Vm_AllocVar (&(*vm).npblk.symtab);
            code = Vm_SpCopyVar(vp, v);  	/* copy the variable	    */
            if (code != SUCCESS)
	            {
                    f_close(file, F_KEEP);
                    return(code);
                    }
                                                /* find last var            */	
            for (last_var=(struct VARIABLE *)&(*vm).npblk.symtab;
                          (*last_var).v_link != NULL; 
                          last_var = (*last_var).v_link )
                ;
            (*last_var).v_link = v;
	    }
	if (p.last)			/* if this is the last PARBLK in group	*/
	    break;
	}
    return(SUCCESS);
    }

/*
 *	dp_InitHeader - build skeleton compiled PDF header record.
 */

FUNCTION static VOID dp_InitHeader
(
 struct DP_STRUCT    *dp,            /* dynamic PDF handle               */
 struct CP_HEADER	*header	/* out: skel compiled PDF header    */
 )
    {
    TEXT		time_buf[STRINGSIZ+1];
    COUNT               body_line = 0;

    (*header).type       = C_HEADER;
    s_bcopy("DynamicPDF", (*header).version, CP_TIMESIZ);	/* TM executable version*/
    get_time(time_buf);
    s_bcopy(time_buf, (*header).timedate, CP_TIMESIZ);	/* save time of compil	*/
    (*header).body_line = body_line;
    s_copy( (*dp).execute, (*header).execute);		/* EXECUTE= file spec	*/
    (*header).interrupt = TRUE;
    (*header).body.possav       =			/* no file posits known	*/
	(*header).title.possav  =
	(*header).help.possav   =
	(*header).level1.possav =
	(*header).level2.possav =
	(*header).before.possav =
	(*header).after.possav  = FALSE;
    return;
    }

/*
 *	dp_OutParblk - build PARBLK record group from a symbol table
 *	and output to the compiled PDF file.
 */

FUNCTION static CODE dp_OutParblk
(
 struct SFILE	*file,		/* in/out: compiled PDF file		*/
 struct VM_STRUCT	*vm,		/* in:  symbol table to build from	*/
 struct POSCTX	*first_pos	/* out: file posit of 1st record in grp	*/
 )
    {
    struct LARGE_PARBLK	p;
    CODE		code;
    COUNT		num_var;	/* sent so far				*/
    struct VARIABLE	*first_p_var;	/* first var in "p"			*/
    struct  SYMTAB	*st;
    struct  VARIABLE	*v;




    q_init((struct PARBLK*) &p, LARGE_P_BYTES, P_ABORT);               /* init storage space */

    (*first_pos).possav = FALSE;
    p.msgtyp = C_BEFORE;
    r_init(p.pool, sizeof p.pool);	/* init PARBLK storage area		*/
    p.symtab.link = NULL;		/* in case no variables			*/
    p.last        = FALSE;
    num_var       = 0;

/* Pack all the variables in 'symtab' into as many parblks as necessary.  */

   st = &(*vm).npblk.symtab;

/* Pack all the variables in each 'symtab' into as many parblks as necessary.  */

    for (v = (*st).link; v != NULL; v = (*v).v_link)
	{
	num_var++;
	code = Vm_ParblkOut(file, &p, v);		/* add variable to file */
	if (code != SUCCESS)
	    return(code);
	first_p_var = p.symtab.link;
	if (num_var > 1  &&
	    (*first_p_var).v_link == NULL  &&	/* if we just wrote to disk...*/
	    !(*first_pos).possav)		/* for the first time	*/
	    MOVE_STRUCT((*file).posctx, *first_pos);	/* save pos of 1st rec	*/
	}
    if (p.symtab.link != NULL)			/* write last block 	*/
    	{
	makerel(&p.symtab, p.pool);		/* make ptrs relative	*/
	p.last = TRUE;
	p.blksiz = r_top(p.pool) - (GENPTR) &p;
	code = f_bwrite(file, (GENPTR) &p, p.blksiz);
	if (code != SUCCESS)
    	    return (code);
	if (!(*first_pos).possav)			/* if was 1st rec...	*/
	    MOVE_STRUCT((*file).posctx, *first_pos);	/* save its position	*/
    	}
    return(SUCCESS);
    }

/*
 *        Dp_SetExecute - set/replace name of executable
 *
 *
 */
FUNCTION VOID Dp_SetExecute
(
 GENPTR    h,
 TEXT      *execute
 )

    {
    struct DP_STRUCT *dp;

    dp = (struct DP_STRUCT *)h;

    s_copy ( execute, (*dp).execute );

    return ;
    }

/*
 *        Dp_SetName - set/replace name of the PDF object
 *
 *
 */
FUNCTION VOID Dp_SetName
(
 GENPTR    h,
 TEXT      *objname
 )

    {
    struct DP_STRUCT *dp;

    dp = (struct DP_STRUCT *)h;

    s_copy ( objname, (*dp).name );

    return ;
    }

/*
 *        Dp_SetLevel2Page - set .page in the level2 help of a parameter
 *
 *
 */
FUNCTION CODE Dp_SetLevel2Page 
(
 GENPTR    h,
 TEXT      parmname[],
 COUNT     linenum
 )
    {
    struct DP_STRUCT *dp;
    COUNT     i;
    struct HELP_LINK *hlink, *templink, *previous;

    if ( linenum <= 0 )
        return(FAIL);

    dp = (struct DP_STRUCT *)h;


    for ( hlink = &(*dp).level2; hlink != NULL; hlink=(*hlink).link)
        {
        if ( s_equal( (*hlink).parmname, parmname ) )
            break;
        }

    if ( hlink == NULL )
        return ( FAIL );

    if ( linenum > (*hlink).strings.numline )
        return (FAIL);
   
    templink = (struct HELP_LINK *)tae_alloc(1,sizeof(struct HELP_LINK ) );
    (*templink).strings.numline = (*hlink).strings.numline + 1;
    (*templink).strings.tp = (TEXT **)tae_alloc(1, 
                              (*templink).strings.numline*sizeof(TEXT *) );

    for ( i=0; i < (*templink).strings.numline; i++ )       /* copy the strings */
        {
        (*templink).strings.tp[i] = tae_alloc(1, STRINGSIZ+1 );
        if ( i <= linenum - 1 )
            {
            s_copy ( (*hlink).strings.tp[i], (*templink).strings.tp[i] );
            continue;
            }
        else if ( i == linenum )
            {
            s_copy (".page", (*templink).strings.tp[i]);
            continue;
            }
        else
            {
            s_copy ( (*hlink).strings.tp[i-1], (*templink).strings.tp[i] );
            }
        }


    if ( hlink != &(*dp).level2 )
        {
        /* find previous link */
        for ( previous = &(*dp).level2; (*previous).link != hlink ; 
              previous = (*previous).link )
            ;
        (*previous).link = templink;
        (*templink).link = (*hlink).link;
        s_copy ( (*hlink).parmname, (*templink).parmname );
        dp_FreeText ( &(*hlink).strings );
        tae_free ( (GENPTR)hlink );
        }
    else
        {
        Dp_SetLevel2 ( h, (*hlink).parmname, (*templink).strings.numline,
                       (*templink).strings.tp );
        dp_FreeText ( &(*templink).strings );
        tae_free ( (GENPTR)templink );
        }
        
            
    return ( SUCCESS );
    }

/*
 *        Dp_SetHelpPage - set .page in the help block of a parameter
 *
 *
 */
FUNCTION CODE Dp_SetHelpPage
(
 GENPTR    h,
 COUNT     linenum
 )
    {
    struct DP_STRUCT *dp;
    struct HELP_LINK *templink, *hlink;
    COUNT     i;

    if ( linenum <= 0 )
        return(FAIL);

    dp = (struct DP_STRUCT *)h;

    hlink = &(*dp).help;

    if ( linenum > (*hlink).strings.numline ) 
        return (FAIL);
   
    templink = (struct HELP_LINK *)tae_alloc(1,sizeof(struct HELP_LINK ) );
    (*templink).strings.numline = (*hlink).strings.numline + 1;
    (*templink).strings.tp = (TEXT **)tae_alloc(1, 
                              (*templink).strings.numline*sizeof(TEXT *) );

    for ( i=0; i < (*templink).strings.numline; i++ )       /* copy the strings */
        {
        (*templink).strings.tp[i] = tae_alloc(1, STRINGSIZ+1 );
        if ( i <= linenum - 1 )
            {
            s_copy ( (*hlink).strings.tp[i], (*templink).strings.tp[i] );
            continue;
            }
        else if ( i == linenum )
            {
            s_copy (".page", (*templink).strings.tp[i]);
            continue;
            }
        else
            {
            s_copy ( (*hlink).strings.tp[i-1], (*templink).strings.tp[i] );
            }
        }

    Dp_SetHelp ( h, (*templink).strings.numline, (*templink).strings.tp );    

    dp_FreeText ( &(*templink).strings );
    tae_free ( (GENPTR)templink );
            
    return ( SUCCESS );
    }

/*
 *	Dp_GetDynParms.  Send V-block to TM for dynamic parameters
 *                    and return acquired parameter values.
 */

FUNCTION CODE Dp_GetDynParms 
(
 Id    dp,
 Id    vmobj
 )
    {
    struct VM_STRUCT*   h;
    CODE		code;
    struct DP_STRUCT    *dpstr;
    struct VM_STRUCT    *vmstr;
    TEXT		pdfspec[FSPECSIZ+1];	/* pdf file spec	*/
    struct LARGE_PARBLK p;
    TEXT   session [STRINGSIZ+1];

    dpstr = (struct DP_STRUCT *)dp;
    vmstr = (struct VM_STRUCT *)vmobj;
      
    if ( NULLSTR ( (*dpstr).name ) )
        {
        /* create a dp file name using session id*/
        getsession ( session );
        s_copy ( session, pdfspec );
        s_append ( ".cpd", pdfspec );
        }
    else
        s_copy ( (*dpstr).name, pdfspec );


    if ( (*dpstr).vm == NULL )
        return ( P_FAIL );

    h = Vm_New ( (*vmstr).npblk.mode );

    code = Dp_ExtParms ( dp, (Id) h );
    if ( code != SUCCESS )
        {
        Vm_Free ( h );
        return(code);
        }
    
    code = Dp_Save ( dp, pdfspec );
    if ( code != SUCCESS )
        {
        Vm_Free ( h );
        return(code);
        }

    Vm_st2blk ((GENPTR)h, &p, LARGE_P_BYTES );
    code =   q_dynp ((struct PARBLK*) &p, pdfspec, M_FULLPDF ) ;
    if ( code != SUCCESS)
        {
        Vm_Free ( h );
        dp_DeleteTemp ( pdfspec );
        return(code);
        }

    code = Vm_ReadFromTM ( vmobj );
    if ( code != SUCCESS)
        {
        Vm_Free ( h );
        dp_DeleteTemp ( pdfspec );
        return(code);
        }

    /* delete the temp file 
     */
    dp_DeleteTemp ( pdfspec );

    Vm_Free ( h );
    return ( SUCCESS );
    }

/*
 *	dp_DeleteTemp - delete the temp file
 */

FUNCTION static CODE dp_DeleteTemp
(
 TEXT		filespec[FSPECSIZ+1]	/* pdf file spec	*/
 )

    {
    CODE		code;
    struct  SFILE       file;
    TAEINT              lun = 0;            /* dummy lun to use for the file  */
    TEXT                library [FLIBRSIZ+1];


    /* delete the temp file 
     */
    getulib ( library );
    code = f_opnspc ( &file, lun, filespec, library, NULL, CPD_TYPE, F_READ );
    if ( code != SUCCESS)
        return(code);
    
    code = f_close ( &file, F_DELETE );
    if ( code != SUCCESS)
        return(code);
    return SUCCESS;
    }
