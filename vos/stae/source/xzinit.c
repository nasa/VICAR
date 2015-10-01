/******************************************************************************
 *	Copyright (c) 1990, 1991, 1992,
 *	National Aeronautics and Space Administration
 *	ALL RIGHTS RESERVED
 *
 *	The software (programs, data bases and/or documentation) on or in
 *	any media can not be reproduced, disclosed or used except under
 *	terms of the license between COSMIC and your organization.
 *****************************************************************************/

#ifndef FORTRAN
#else
C ***** DO NOT USE TABS OF FORMFEEDS IN ANY UNIX FORTRAN FILES *****
C
C       XZINIT. FORTRAN CALLABLE ROUTINE .
C       WRITTEN IN FORTRAN TO AVOID PROBLEM OF CONVERTING A C STRING
C       TO A FORTRAN-77 STRING (FOR THE STANDARD OUTPUT FILE NAME ).
C
C
C       CHANGE LOG:
C       
C       29-MAR-83       HARDCODE PGMINC.FIN...DM
C       30-SEP-83       STORE TERMINAL TYPE, GETTERM -> XUGETT...peb
C       15-NOV-83       LIMIT VARIABLE NAMES TO 6 CHARS FOR PORTABILITY...dm
C       17-NOV-83       Add call to xzcall...palm
C       30-NOV-83       MODIFY FOR UNIX/F77 COMPATIBILITY
C       02-DEC-83       Fix bug in XRSTR calling sequence...dm
C       06-DEC-83       Save XZSAVL common block explicitely...dm
C       08-DEC-83       Do not open terminal as stdout using LUN 6...dm
C       10-MAY-84       Position file at the end in APPEND mode...dm
C       22-MAY-84       Convert file name to lower case...dm
C       21-AUG-84       Make xzinit portable, Isolate non-portable 
C                       routines to different source module...dm
C       01-APR-88       - APOLLO conversion requires all Fortran files to have
C                       ** NO TABS ** and ** NO FORMFEEDS **!!!
C                       - Fortran files now go through C pre-processor...ljn
C	23-JUL-91	PR325: Change STDLUN to LUN in PREOPN...ljn
C
C
        SUBROUTINE XZINIT(BLOCK, DIM, STDLUN, MODE, STATUS)

#include  "pgminc.fin"

        INTEGER BLOCK(1)                
        INTEGER DIM                     
        INTEGER STDLUN                  
        INTEGER MODE                    
        INTEGER STATUS                  


C Return FAIL always
        STATUS = 0
        RETURN
        END
#endif

