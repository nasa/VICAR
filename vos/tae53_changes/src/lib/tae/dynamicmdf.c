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
 *	Dm_ functions to build and manipulate a dynamic mdf
 *
 *	CHANGE LOG:
 *      12-JUNE-87	Created...tpl
 *	02-aug-89	Some compilers complained about un-inited vars...ln
 *	22-oct-92	Prototyping tae_alloc is unnecessary and Ultrix 4.3
 *			does not like it...rt
 *
 */

#include	"taeconf.inp"	/* TAE configuration (REQUIRED)		*/
#include        "tminc.inc"
#include        "fileinc.inp"
#include        "pgminc.inc"
#include "taeintproto.h"
#include <string.h>

#define DM_TITLE    1
#define DM_ENTRY    2
#define DM_HELP     3

    struct    ENTRY_LINK {
                         struct TXTSTOR entrytext;
                         GENPTR link;
                         };

    struct    DM_STRUCT {
                        struct TXTSTOR    title;
                        struct ENTRY_LINK menuentry;    /* text strings of 
                                                       the body of a pdf */
                        struct TXTSTOR    help;
                        };

FUNCTION VOID dm_FreeText 
(
 struct TXTSTOR   *t                 /* IN: text storage pointer */
);
FUNCTION static BOOL dm_GetDirective
(
 TEXT    statement[],
    TEXT    directive[]
 );


/*
 *	Dm_New.  Create a dynamic mdf structure and return handle
 */

    FUNCTION GENPTR Dm_New(void)

    {
    GENPTR    h;
    /*
        allocate memory space for the handle
    */
    h = tae_alloc ( 1, sizeof(struct DM_STRUCT) ); 
    zero_block ( h, sizeof (struct DM_STRUCT) );       

    return( h );
    }

/*
 *        Dm_ReadMDF - read in an existing mdf 
 *
 *
 */
FUNCTION CODE Dm_ReadMDF 
(
 GENPTR     h,                         /* handle created by dm_new */
 TEXT       filespec[]                /* name of a compiled mdf   */
 )
    {
    CODE		code;
    COUNT		i;
    struct SFILE	file;		
    TEXT                library [FLIBRSIZ+1];
    TEXT                stmt[STRINGSIZ+1];
    TEXT                directive[10];
    struct DM_STRUCT    *dm;
    BOOL                firstdir;
    BOOL                firstentry;
    struct ENTRY_LINK   *menuentry;
    struct ENTRY_LINK   *curentry = 0;
    CODE                mode = 0;
    TAEINT              lun = 0;                       /* dummy lun */

    getulib ( library );
    code = f_opnspc ( &file, lun, filespec, library, NULL, MDF_TYPE, F_READ );
    if ( code != SUCCESS)
        return(FAIL);

    dm = (struct DM_STRUCT *)h;


    firstdir = TRUE;
    firstentry = TRUE;
/*
    read till end of file or .end
*/

    while (  (code = f_read ( &file, stmt ))  != F_EOF )
        {
        if ( dm_GetDirective ( stmt, directive ) )
            {
            if ( s_equal ( ".title", directive ) )
                    {
                    if ( firstdir )
                        {
                        firstdir = FALSE;
                        mode = DM_TITLE;
                       (*dm).title.tp = (TEXT **)tae_alloc 
                                                ( 1, 20*sizeof(TEXT *) );
                        }
                    else                /* wrong .title location */
                        {
                        f_close ( &file, F_KEEP );
                        return ( FAIL );
                        }
                    }
            else if ( s_equal ( ".MENU", directive ) ||
                      s_equal ( ".PROC", directive ) ||
                      s_equal ( ".COMMAND", directive ) )
                    {
                    firstdir = FALSE;
                    mode = DM_ENTRY;
                    /*
                        create a new entry  and link it in
                                                            */
                    if ( !firstentry )
                        {                            /* find the end */
                        for ( menuentry= &(*dm).menuentry; 
                             (*menuentry).link!=NULL;
                             menuentry = (struct ENTRY_LINK *)(*menuentry).link) ;
                        (*menuentry).link = tae_alloc ( 1, 
                                sizeof(struct ENTRY_LINK ) );
                        curentry = (struct ENTRY_LINK *)(*menuentry).link;
                        }
                    else
                        {
                        firstentry = FALSE;
                        curentry = &(*dm).menuentry;
                        }
                    (*curentry).entrytext.tp = 
                            (TEXT **)tae_alloc ( 1, 100*sizeof(TEXT *) );
                    i = (*curentry).entrytext.numline;
                    (*curentry).entrytext.tp[i] = 
                                tae_alloc(1, strlen(stmt)+1 );
                    s_copy ( stmt, (*curentry).entrytext.tp[i] );
                    (*curentry).entrytext.numline++;
                    }
            else if ( s_equal ( ".HELP", directive ) )
                    {
                    firstdir = FALSE;
                    mode = DM_HELP;
                    (*dm).help.tp = 
                        (TEXT **)tae_alloc ( 1, 1000*sizeof(TEXT *) );
                    }
            else if ( s_equal (".END", directive ) )
                    {
                    f_close(&file, F_KEEP );
                    return(SUCCESS);
                    }
            else
                    return(FAIL);            /* should never get here*/
            }
        else
            {
            if ( mode == DM_TITLE )
                {
                i = (*dm).title.numline;
                (*dm).title.tp[i] = tae_alloc(1, strlen(stmt)+1 );
                s_copy ( stmt, (*dm).title.tp[i] );
                (*dm).title.numline++;
                }
            else if ( mode == DM_HELP )
                {
                i = (*dm).help.numline;
                (*dm).help.tp[i] = tae_alloc(1, strlen(stmt)+1 );
                s_copy ( stmt, (*dm).help.tp[i] );
                (*dm).help.numline++;
                }
            else if ( mode == DM_ENTRY )
                {
                i = (*curentry).entrytext.numline;
                (*curentry).entrytext.tp[i] = 
                            tae_alloc(1, strlen(stmt)+1 );
                s_copy ( stmt, (*curentry).entrytext.tp[i] );
                (*curentry).entrytext.numline++;
                
                }
            else                        /* skip leading comment lines */
                ;
            }
        }
    f_close ( &file, F_KEEP );
    return(SUCCESS);
    }            

/*
 *        Dm_SetTitle - set/replace the title text of a mdf
 *
 *
 */
FUNCTION CODE Dm_SetTitle
(
 GENPTR    h,
 COUNT     count,
 TEXT      **title
 )
    {
    struct DM_STRUCT *dm;
    COUNT     i;

    if ( count <= 0 )
        return( P_BADCOUNT );

    dm = (struct DM_STRUCT *)h;

    if ( (*dm).title.numline > 0 )
        dm_FreeText( &(*dm).title );

    (*dm).title.numline = count;
    (*dm).title.tp = (TEXT **)tae_alloc(1, count*sizeof(TEXT *) );

    for ( i=0; i < count; i++ )        /* copy the strings */
        {
        (*dm).title.tp[i] = tae_alloc(1, strlen(title[i])+1 );
        s_copy ( title[i], (*dm).title.tp[i] );
        }
    return ( SUCCESS );
    }

/*
 *        Dm_SetHelp - set/replace general mdf help
 *
 *
 */
FUNCTION CODE Dm_SetHelp
(
 GENPTR    h,
 COUNT     count,
 TEXT      **help
 )

    {
    struct DM_STRUCT *dm;
    COUNT     i;

    if ( count <= 0 )
        return( P_BADCOUNT );

    dm = (struct DM_STRUCT *)h;

    if ( (*dm).help.numline > 0 )
        dm_FreeText ( &(*dm).help );

    (*dm).help.numline = count;
    (*dm).help.tp = (TEXT **)tae_alloc(1, count*sizeof(TEXT *) );

    for ( i=0; i < count; i++ )        /* copy the strings */
        {
        (*dm).help.tp[i] = tae_alloc(1, strlen(help[i])+1 );
        s_copy ( help[i], (*dm).help.tp[i] );
        }
    return ( SUCCESS );
    }

/*
 *        Dm_AddItem - add an menu item
 *
 *
 */
FUNCTION CODE Dm_AddItem
(
 GENPTR    h,
 COUNT     ev_count, 
 TEXT      *entry_vec[],
 COUNT     hv_count,                /* applicable to .command only*/
 TEXT      *help_vec[],             /* applicable to .command only*/
 CODE      mode
 )
    {
    struct DM_STRUCT *dm;
    COUNT     i, j, count;
    struct ENTRY_LINK *menuentry;
    struct ENTRY_LINK *newentry;
    TEXT   directive[10];

    if ( ev_count <= 0 )
        return(P_BADCOUNT);

    dm = (struct DM_STRUCT *)h;

    if ( mode != P_BEGIN && mode != P_END )
            return(FAIL);

    if ( !dm_GetDirective (entry_vec[0], directive)) /* first line must have a 
                                                     directive */
        {
        return(P_NOTMDIR);
        }

    count = ev_count;
    if ( s_equal (".command", directive ) && hv_count > 0 )
        count = ev_count + hv_count + 1;
        
    if ( (*dm).menuentry.entrytext.numline == 0 )          /* first entry */
        {
        (*dm).menuentry.entrytext.tp = (TEXT **) tae_alloc ( 1,
                                    count * sizeof(TEXT *));
        for ( i=0; i < ev_count; i++ )        /* copy the strings */
              {
              (*dm).menuentry.entrytext.tp[i] = 
                        tae_alloc(1,strlen(entry_vec[i])+1);
              s_copy ( entry_vec[i], (*dm).menuentry.entrytext.tp[i] );
              }
        if ( count > ev_count )
            {
            (*dm).menuentry.entrytext.tp[ev_count] = tae_alloc(1,7);
            s_copy ( ".EHELP", (*dm).menuentry.entrytext.tp[ev_count] );
            
            for ( i=0; i < hv_count; i++ )        /* copy the help strings */
              {
              j = ev_count + i + 1;
              (*dm).menuentry.entrytext.tp[j] = 
                            tae_alloc(1,strlen(help_vec[i])+1);
              s_copy ( help_vec[i], (*dm).menuentry.entrytext.tp[j] );
              }
            }
        (*dm).menuentry.entrytext.numline = count;
        return(SUCCESS);   
        }        
    if ( mode == P_BEGIN )
        {
        if ( (*dm).menuentry.entrytext.numline != 0 )
          {                                    /* save current first item */
          newentry = (struct ENTRY_LINK *)tae_alloc(1,
                                     sizeof(struct ENTRY_LINK) );
          zero_block ( (GENPTR)newentry, sizeof(struct ENTRY_LINK) );
          (*newentry).entrytext.tp = (TEXT **) tae_alloc ( 1,
                (*dm).menuentry.entrytext.numline * sizeof(TEXT *) );

          (*newentry).entrytext.numline = (*dm).menuentry.entrytext.numline;

          for ( i=0; i < (*dm).menuentry.entrytext.numline; i++ )
            {
            (*newentry).entrytext.tp[i] = 
                        tae_alloc(1,STRINGSIZ+1);
            s_copy ( (*dm).menuentry.entrytext.tp[i], 
                        (*newentry).entrytext.tp[i] );
            }
          (*newentry).link = (*dm).menuentry.link;

          (*dm).menuentry.link = (GENPTR)newentry;
          dm_FreeText( &(*dm).menuentry.entrytext );
          }


        (*dm).menuentry.entrytext.tp = (TEXT **) tae_alloc ( 1,
                                    count * sizeof(TEXT *));
        for ( i=0; i < ev_count; i++ )        /* copy the strings */
              {
              (*dm).menuentry.entrytext.tp[i] = 
                        tae_alloc(1,strlen(entry_vec[i])+1);
              s_copy ( entry_vec[i], (*dm).menuentry.entrytext.tp[i] );
              }
        if ( count > ev_count )
            {
            (*dm).menuentry.entrytext.tp[i] = tae_alloc(1,7);
            s_copy ( ".EHELP", (*dm).menuentry.entrytext.tp[i] );
            
            for ( i=0; i < hv_count; i++ )        /* copy the help strings */
              {
              j = ev_count + i + 1;
              (*dm).menuentry.entrytext.tp[j] = tae_alloc(1,strlen(help_vec[i])+1);
              s_copy ( help_vec[i], (*dm).menuentry.entrytext.tp[j] );
              }
            }
        (*dm).menuentry.entrytext.numline = count;
           
        }
    else
        {
        for ( menuentry= &(*dm).menuentry; (*menuentry).link != NULL; 
                  menuentry = (struct ENTRY_LINK *)(*menuentry).link )
             ;
        newentry = (struct ENTRY_LINK *)tae_alloc(1,
                                     sizeof(struct ENTRY_LINK) );
        zero_block ( (GENPTR)newentry, sizeof(struct ENTRY_LINK) );
        (*menuentry).link = (GENPTR)newentry ;
        (*newentry).entrytext.tp = (TEXT **) tae_alloc ( 1,
                                    count * sizeof(TEXT *) );
        for ( i=0; i < ev_count; i++ )        /* copy the strings */
              {
              (*newentry).entrytext.tp[i] = 
                        tae_alloc(1,strlen(entry_vec[i])+1);
              s_copy ( entry_vec[i], (*newentry).entrytext.tp[i] );
              }
        if ( count > ev_count )
            {
            (*newentry).entrytext.tp[i] = tae_alloc(1,7);
            s_copy ( ".EHELP", (*newentry).entrytext.tp[i] );
            
            for ( i=0; i < hv_count; i++ )        /* copy the help strings */
              {
              j = ev_count + i + 1;
              (*newentry).entrytext.tp[j] = tae_alloc(1,strlen(help_vec[i])+1);
              s_copy ( help_vec[i], (*newentry).entrytext.tp[j] );
              }
            }
        (*newentry).entrytext.numline = count;
        }
    return ( SUCCESS );
    }

/*
 *        Dm_Save - save the mdf
 *
 *
 */
FUNCTION CODE Dm_Save
(
 GENPTR    h,
 TEXT      filespec[]
 )
    {
    TAEINT              lun = 0;                       /* dummy lun */
    TEXT                library [FLIBRSIZ+1];
    struct SFILE       file;
    struct ENTRY_LINK   *menuentry;
    CODE   code;
    COUNT  i;
    struct DM_STRUCT    *dm;




   dm = (struct DM_STRUCT *)h;

   getulib ( library );

   code = f_opnspc ( &file, lun, filespec, library, NULL, MDF_TYPE, F_WRITE );
   if ( code != SUCCESS)
        return(code);




/* now the title info	*/

    code = f_write(&file, ".TITLE");		/* title directive */
    if ( code != SUCCESS )
        {
        f_close ( &file, F_DELETE );
        return(code);
        }

    for ( i=0; i < (*dm).title.numline; i++ )
        {
        code = f_write ( &file, (*dm).title.tp[i] );
        if ( code != SUCCESS )
            {
            f_close ( &file, F_DELETE );
            return(code);
            }
        }
/*
    the entry info
*/

    for ( menuentry= &(*dm).menuentry;
          menuentry != NULL; 
          menuentry = (struct ENTRY_LINK *)(*menuentry).link )
        {
        for ( i=0; i < (*menuentry).entrytext.numline; i++ )
            {
            f_write ( &file, (*menuentry).entrytext.tp[i] );
            if ( code != SUCCESS )
                {
                f_close ( &file, F_DELETE );
                return(code);
                }
            }
        }

/* the help info	*/

    code = f_write(&file, ".HELP");		/* help directive */
    if ( code != SUCCESS )
        {
        f_close ( &file, F_DELETE );
        return(code);
        }
    for ( i=0; i < (*dm).help.numline; i++ )
        {
        code = f_write ( &file, (*dm).help.tp[i] );
        if ( code != SUCCESS )
            {
            f_close ( &file, F_DELETE );
            return(code);
            }
        }
    code = f_write(&file, ".END");		/* the end directive */
    if ( code != SUCCESS )
        {
        f_close ( &file, F_DELETE );
        return(code);
        }

    f_close ( &file, F_KEEP );
    return(SUCCESS);
    }

/*
 *        Dm_FreeText - free the text strings stored.
 *
 *
 */
FUNCTION VOID dm_FreeText
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
 *        Dm_Free - free the dynamic mdf structure
 *
 *
 */
FUNCTION VOID Dm_Free
(
 GENPTR      h              /* IN: dynamic mdf handle       */
)
    {
    struct ENTRY_LINK *menuentry;
    
    struct DM_STRUCT *dm;

    dm = (struct DM_STRUCT *)h;

    if ( (*dm).title.numline > 0 )
         dm_FreeText ( &(*dm).title );

    if ( (*dm).help.numline > 0 )
         dm_FreeText ( &(*dm).help );

/*
    free entries
*/
    for (menuentry = &(*dm).menuentry;
         menuentry != NULL;
         menuentry = (struct ENTRY_LINK *)(*menuentry).link)
        {
                
        dm_FreeText( &(*menuentry).entrytext );
        if ( menuentry != &(*dm).menuentry )
            {
            tae_free (menuentry);
            }
        }

    tae_free ( h );                            /* free everything else  */
    return;
    }

/*
 *    dm_GetDirective - get the directive of a statement
 */
FUNCTION static BOOL dm_GetDirective
(
 TEXT    statement[],
    TEXT    directive[]
 )
    {
    if ( NULLSTR(statement) )
        return(FAIL);
    else
        {
        if ( s_lseq ( ".COMMAND", statement ) )
            {
            s_copy ( ".COMMAND", directive );
            return (TRUE);
            }
        else if ( s_lseq ( ".MENU", statement ) )
            {
            s_copy ( ".MENU", directive );
            return (TRUE);
            }
        else if ( s_lseq ( ".PROC", statement ) )
            {
            s_copy ( ".PROC", directive );
            return (TRUE);
            }
        else if ( s_lseq ( ".END", statement ) )
            {
            s_copy ( ".END", directive );
            return (TRUE);
            }
        else if ( s_lseq ( ".TITLE", statement ) )
            {
            s_copy ( ".TITLE", directive );
            return (TRUE);
            }
        else if ( s_lseq ( ".HELP", statement ) )
            {
            s_copy ( ".HELP", directive );
            return (TRUE);
            }
        else
            return(FAIL);
        }
    }

