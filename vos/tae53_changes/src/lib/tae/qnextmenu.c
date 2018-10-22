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
 *    This module contains the function q_setnextmenu
 *  CHANGE LOG:
 *	24-Jun-87	Created...tpl
 *	09-aug-87	Changed count of $MENUS from MAXVAL to 
 *			(*menu_gbl).v_maxc...palm
 *      30-sep-87       Checked for undefined $MENU
 */
#include     "taeconf.inp"
#include     "tminc.inc"
#include     "parblk.inc"
#include     "symtab.inc"
#include     "menuinc.inc"
#include "taeintproto.h"
/*
 *  q_setnextmenu - set the menu to be executed upon exit
 *
 */

FUNCTION  CODE  q_setnextmenu 
(
 struct PARBLK  *parblk,                /* IN/OUT: parblk            */
 TEXT		mdfname[]		/* IN: mdf name 	     */
)
    {
    struct VARIABLE     *old_menu_gbl;		/* ptr to variable $MENUS    */

    CODE		code;
    TEXT		errstr[STRINGSIZ+1];
    TEXT                library[STRINGSIZ+1];
    TEXT                *locvector[MAXVAL];
    TAEINT              i, count;
    TEXT                menu_spec[FSPECSIZ+1];
    struct  FSBLOCK     mn_fsblk;		/* FSBLOCK for opened mdf    */

    getulib ( library );

    menu_spec[0] = EOS;

    if  ( NULLSTR(mdfname) )			/*  menu name given          */
         return( P_BADNAME );

    old_menu_gbl = p_find ( parblk, "$MENUS" );  /* get the global $menu      */
    if ( old_menu_gbl == NULL )
        return ( FAIL );
    

    if (NESTING(old_menu_gbl) == (*old_menu_gbl).v_maxc)   /* at max count?  */
      	return (M_NESTERR);		        /* error return		     */

    s_copy(mdfname, menu_spec);
    code = f_crack(mdfname, library, "", MDF_TYPE, &mn_fsblk, errstr);
    f_spec(&mn_fsblk, menu_spec);		/* get full file spec	     */
    count = NESTING(old_menu_gbl);	        /* increment nesting level   */

    for ( i=0; i< count; i++ )
        locvector[i]=(TEXT *) SVAL(*old_menu_gbl,i);
      
    locvector[ count ] = (TEXT *)menu_spec;

    count++;


    code = q_string ( parblk, "$MENUS", count, locvector, P_UPDATE);

    if ( code != SUCCESS )
        return (code );
 
    code = q_out ( parblk );
    if ( code != SUCCESS )
        return (code );
    
    return(SUCCESS);
    }
