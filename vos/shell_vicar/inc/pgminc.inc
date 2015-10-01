/*
Generation time:  6-NOV-1990 17:56:15.72
*/
/*

        PGMINC.TXT.  Definitions for "native" (i.e. as defined in the
        Version 1 PGM document)  TAE subroutines. Native routines 
        use FORTRAN-77 character strings.

        CAUTION:  this include file is generated from PGMINC.TXT using
        the GENERATE.COM DCL procedure.  Only PGMINC.TXT should be
        edited!!

        CAUTION:  DO NOT add other "INCLUDE" statements without
        consulting INSTALLTAE on UNIX.

        NOTE: UNIX users BEWARE! The PGMINC.FIN file generated from
        PGMINC.TXT isn't quite ready for porting. Two further changes
        are required: (1) Substitute the 'INCLUDE' statement below 
	with the actual file, xiinc.fin, and (2) check for any
        TABS and remove them as required by Apollo Fortran.
        

        CHANGE LOG:
        31-mar-83       Delete xsyefn...palm
        05-aug-83       Fix misspellings (conflicts with PGM)...peb
        25-OCT-83       Add new P_  and x.. codes for xr package...dm
        31-oct-83       Fix xlabsz misspelling...palm
        05-nov-83       Set xprdim to P_BYTES/4...palm
        10-nov-83       New xfilnm, xfilsz, xfilsp, xifdim...palm
        18-nov-83       Add xover and xerror...palm
        06-dec-83       XI package defs moved to XIINC.TXT and included...peb
        23-feb-84       New 
        26-jul-84       New "generation language"...palm
        27-sep-84       Add I_INPUT, I_OUTPUT for C-callable image pkg...peb
        25-oct-84       Add M_HOLD...nhe
        04-nov-84       Put M_HOLD in correct slot...nhe
        27-nov-84       Add P_NOPARM, P_OLDPARM...peb
        13-aug-86       On 26-Aug-85, dab added M_DYNEXIT for exit from 
                        dynamic tutor directly to PGMINC.INC.  I migrated
                        the change into here...nci
        14-sep-86       Add application language type definitions...dm
        16-jul-87       Add dynamic pdf,mdf codes...tpl
        10-mar-87       Change XIINC to xiinc.inp; add warning at top of 
                        this file...ljn
        28-dec-88       Added P_BADSIZE...tpl
	03-may-89	M_COMMAND...ljn
        12-apr-90	Added xbmnmx, xnster...krw
	06-nov-90	P_BADSTATE...ljn

        Avoid re-definitions when multiply included in C:

*/
#ifndef I_PGMINC
#define I_PGMINC 0
/*

        error and success code definitions for parameter block 
        manipulation.

*/
#define	PGM_BASE	1800	/* base of arbitrary constants */
#define	P_BADNAME	1800	/* bad parameter name */
#define	P_BADTYPE	1801	/* parameter type is wrong */
#define	P_BADCOUNT	1802	/* caller-supplied array to small */
#define	P_ABORT	1803	/* abort on error */
#define	P_CONT	1804	/* return with error code on error */
#define	P_DUPNAME	1805	/* duplicate name attempt */
#define	P_KILLED	1806	/* tutor user killed process */
#define	P_UPDATE	1807	/* update mode for v-block */
#define	P_ADD	1808	/* add mode for v-block */
#define	P_NOROOM	1809	/* no room in v-block */
#define	P_OVER	1810	/* overflow for FOR-77 string */
#define	P_ERROR	1811	/* general error indicator */
#define	P_NOPARM	1812	/* adding qual to non-exist parm */
#define	P_OLDPARM	1813	/* parm created before quals existed */
#define	P_FAIL	2	/* fail */
#define	P_PROCEDURE	1870	/* pdf file type is procedure */
#define	P_PROCESS	1871	/* pdf file type is process   */
#define	P_PARMSET	1872	/* pdf file type is parmset   */
#define	P_GLOBAL	1873	/*  pdf file type is global   */
#define	P_BEGIN	1874	/* add menu entry to the front */
#define	P_END	1875	/* add menu entry to the end */
#define	P_BMINMAX	1814	/* bad min and max combination */
#define	P_NOTCOMP	1815	/* not a compiled PDF */
#define	P_NOTMDIR	1816	/* first line is not a menu dir */
#define	P_BADSIZE	1817	/* bad string size specified */
#define	P_BADSTATE	1818	/* bad panel state specified */
#define	FP_EOP	2101	/* end of parameter list */
#define	FP_NULL	2102	/* no parameter returned */
#define	P_NONE	0	/* not file or nocheck mode */
#define	I_INPUT	1	/* input file mode */
#define	I_INOUT	3	/* update file mode */
#define	P_FILE	4	/* file parameter */
/*

        Codes for parblk.msgtyp field.  Note that for dynamic parameters, 
        we do not have a header block transmitted to TM as we did in the
        prototype; the PDF file spec for dynamic parameters is obtained 
        by TM from the string value of a variable named "_PROC" in the PARBLK.

*/
#define	M_FULLPDF	1850	/* dynamic on all PDF parms, (process-to-TM) */
#define	M_SUBPDF	1851	/* dynamic on PDF subset  ,(process-to-TM) */
#define	M_KILLED	1852	/* TM is killing process  ,(TM-to-process) */
#define	M_INIPAR	1853	/* TM is sending initial PARBLK ,(TM-to-process) */
#define	M_CONTINUE	1854	/* TM is continuing process , (TM-to-process) */
#define	M_OUTPUT	1855	/* process supplying output values, (process-to-TM) */
#define	M_LOGMSG	1856	/* process is sending log message */
/*

        New V1.2 "handshake" versions 

*/
#define	M_HOUTPUT	1857	/* M_OUTPUT with handshake */
#define	M_HLOGMSG	1858	/* log message */
/*

        Message for hold async jobs

*/
#define	M_HOLD	1859	/* used to hold up an async reply */
/*

        Message to inform process of exit from dynamic tutor

*/
#define	M_DYNEXIT	1860	/* exit dyn tutor (TM-to-process) */
#define	M_COMMAND	1861	/* dyn cmd from process */
/*

        Definitions for application program (language) type 
        NOTE: Not sure if they belong here (or in TAECONF)

*/
#define	UNKNOWN_TYPE	-1	/* Language type unknown */
#define	C_TYPE	1	/* Application in 'c'    */
#define	FORTRAN_TYPE	2	/* Application in FORTRAN */
/*

        The XI definitions are in a separate file:

*/
#include       "xiinc.inp"
#endif
