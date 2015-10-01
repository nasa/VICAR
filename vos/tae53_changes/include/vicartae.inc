/* VICARTAE - Definitions associated with VICAR extensions to TAE.	*/
/* Note that most of these must be kept in parallel with the VICAR RTL	*/
/* definitions (in XVMAININC and defines.h).				*/

#ifndef I_VICARTAE		/* prevent double include		*/
#define I_VICARTAE	0

#ifdef vms
#define UNIX_OS		0
#define VMS_OS		1
#else
#define UNIX_OS		1
#define VMS_OS		0
#endif

/* Definitions for selecting/deselecting individual VICAR extensions	*/
/* Note: AUTOUSAGE should be enabled even if RM isn't so PDFs can reference */
/* the (non-functional) global variable even if the RM isn't enabled        */
#define INS_USE_TAPE	1	/* Set up matape global vars and mount commands */
#define INS_USE_PRSTYLE	1	/* Use VICAR prompt-style dynamic tutor	      */
#define INS_USE_FLAG	1	/* Enable FLAG command			      */
#define INS_USE_LISTPROC 1	/* Enable LISTPROC command		      */
#define INS_USE_AUTOUSAGE 1	/* Enable $AUTOUSAGE gbl var (needed for RM)  */
#if VMS_OS
#define INS_USE_EMACS	1	/* Enable EMACS command (VMS only)	      */
#define INS_USE_PNAME	1	/* Enable setting process name in batch (VMS) */
#define INS_USE_RM	1	/* Enable Resource Monitor package (VMS only) */
#define INS_USE_VRDI	1	/* Enable intrinsic USEDISP/FREEDISP (VMS)    */
#define INS_USE_MAL	1	/* Enable MAL/DAL commands (VMS only)	      */
#define INS_USE_BATCH	1	/* Enable additions to batch job file (VMS)   */
#define INS_USE_ATTACH	1	/* Enable ATTACH command (VMS only)	      */
#define INS_USE_DEFINE	1	/* Enable DEFLOG and DEFSYM commands (VMS)    */
#else
#define INS_USE_EMACS	0	/* Enable EMACS command (VMS only)	      */
#define INS_USE_PNAME	0	/* Enable setting process name in batch (VMS) */
#define INS_USE_RM	1	/* Enable Resource Monitor package */
#define INS_USE_VRDI	0	/* Enable intrinsic USEDISP/FREEDISP (VMS)    */
#define INS_USE_MAL	0	/* Enable MAL/DAL commands (VMS only)	      */
#define INS_USE_BATCH	0	/* Enable additions to batch job file (VMS)   */
#define INS_USE_ATTACH	0	/* Enable ATTACH command (VMS only)	      */
#define INS_USE_DEFINE	0	/* Enable DEFLOG and DEFSYM commands (VMS)    */
#endif

/* Miscellaneous definitions */

#if INS_USE_VRDI

#define VRDIDEVSIZE	16	/* Max size of VRDI device name (with pad) */

#endif

/* The GETRUSAGE_AVAIL define is used by the USAGE command, found in */
/* $TAEVICAR/rm_unix.np.c to determine how it implements USAGE.  Note */
/* that this only needs to be on the UNIX system. */
#ifdef UNIX
#if defined(sgi) || (defined(sun) && OSMajorVersion<5) || defined(__linux__)
#define GETRUSAGE_AVAIL	1
#else
#define GETRUSAGE_AVAIL	0
#endif
#endif

#if INS_USE_TAPE

#define MAXTAPES	15	/* Size of tape table			*/
#define TAPEDEVSIZE	64	/* Max chars in tape device spec	*/
#define TAPENAMESIZE	15	/* Max chars in tape symbolic name	*/
#define MAXTAPEDEVS	50	/* Max # of tape drives available	*/

#define I_TAPE		0	/* codes returned from i_analyze()	*/
#define I_DISK		1
#define I_SYNERR	2

#if VMS_OS
struct itmlst		/* Item list for sys$mount, sys$getdvi, etc. */
{
    short length;
    short code;
    long *buffer;
    long retlen;
};
#endif

#endif

#endif
