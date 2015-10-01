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



/* TPL CHECKOUT FILE_TIME= 9-AUG-1987 15:52 DUA1:[TAEV2.OLB.GENLIB]VMNEXTMENU.C;2 */
/*
 *    This module contains the function Vm_SetNextMenu
 *  CHANGE LOG:
 *	24-Jun-87	Created...tpl
 *	09-aug-87	Changed from MAXVAL to v_maxc for 
 *			$MENUS max count...palm
 *      13-aug-87       Corrected vm_putstring to Vm_SetString...tpl
 *      22-jul-92	PR1519: Label Vm_SetNextMenu as CLASSIC_FUNCTION...kbs
 */
#include     "taeconf.inp"
#include     "tminc.inc"
#include     "parblk.inc"
#include     "symtab.inc"
#include     "menuinc.inc"
#include     "vminc.inc"
#include "taeintproto.h"

/*
 *  Vm_SetNextMenu - set the menu to be executed upon exit
 *
 */

CLASSIC_FUNCTION  CODE  Vm_SetNextMenu 
(
    GENPTR              h,                      /* IN: parblk handle         */
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

    old_menu_gbl = Vm_Find ( h, "$MENUS" );      /* get the global $menu      */
    if ( old_menu_gbl == NULL )
        return ( FAIL );

    if (NESTING(old_menu_gbl) == (*old_menu_gbl).v_maxc)   /* if full	     */
      	return (M_NESTERR);		        /* error return		     */

    s_copy(mdfname, menu_spec);
    code = f_crack(mdfname, library, "", MDF_TYPE, &mn_fsblk, errstr);
    f_spec(&mn_fsblk, menu_spec);		/* get full file spec	     */
    count = NESTING(old_menu_gbl);	        /* increment nesting level   */

    for ( i=0; i< count; i++ )
        locvector[i]=(TEXT *) SVAL(*old_menu_gbl,i);
      
    locvector[ count ] = (TEXT *)menu_spec;

    count++;


    code = Vm_SetString ( h, "$MENUS", count, locvector, P_UPDATE);
    if ( code != SUCCESS )
        return (code );

    Vm_SetTCLVar ( h );
    if ( code != SUCCESS )
        return (code );
    
    return(SUCCESS);
    }
