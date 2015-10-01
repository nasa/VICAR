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
static int dummy;	/* prevent ranlib from whining */
#else
C ***** DO NOT USE TABS OR FORMFEEDS IN ANY UNIX FORTRAN FILES *****
C
C       CHANGE LOG:
C
C       10-dec-86       Allow carriage control in FORMAT statement...peb
C       01-apr-88       Conversion for Apollo requires in all Fortran files
C                       ** NO TABS **  and ** NO FORMFEEDS ** !!!...ljn
C

        SUBROUTINE PREOPN(LUN, STDREC, OPENED)

        INTEGER         LUN
        CHARACTER*(*)   STDREC
        LOGICAL         OPENED
        INTEGER         L, CH   

C  NOTE:  Under UNIX, LUN 6 is already pre-connected to the standard output
C       when the application program got control.  When invoked by TM the
C       standard output, initially, is the terminal. So before reopening the
C       standard output, we must check if the user also uses LUN 6 for
C       standrd output and wants terminal as the standard output.
C       In that case we do not reopen the file to avoid
C       conflict with the FORTRAN I/O system.
C       

        L = LEN(STDREC)                 
        DO 20 I=1,L
        CH = ICHAR(STDREC(I:I))
        IF (CH .GE. ICHAR('A') .AND. CH .LE. ICHAR('Z')) THEN
            CH = CH + ICHAR('a') - ICHAR('A')
            STDREC(I:I) = CHAR(CH)
        ENDIF
20      CONTINUE 

        OPENED = .FALSE. 
        IF (LUN .EQ. 6) OPENED = .TRUE.  
        RETURN
        END
C
C       WRTSTD.  Write to standard output device if different from terminal.
C
        SUBROUTINE  WRTSTD(MSG)

        CHARACTER*(*)   MSG                     

        INTEGER         LENGTH                  
        INTEGER         LUN                     
        LOGICAL         TERMNL                  
        
        CALL GETLUN_TAE(LUN)
        CALL GETSTD(TERMNL)
        LENGTH = LEN(MSG)
        
        IF (.NOT. TERMNL) THEN
            WRITE(LUN, 100) (MSG(I:I),I=1,LENGTH)
        ENDIF
100     FORMAT(1X, 131(:,A1))
        RETURN
        END
#endif
