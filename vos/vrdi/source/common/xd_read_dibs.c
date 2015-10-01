/* This same code is used in both the VRDI and in the USEDISP/FREEDISP	*/
/* commands in TAE.  The code must be repeated since TAE cannot link to	*/
/* the VRDI, and USEDISP/FREEDISP must be TAE intrinsics under VMS so	*/
/* the device will be allocated in the parent process.			*/
/*									*/
/* The VRDI version is named ".c", and the TAE version is named ".cnp",	*/
/* as per their respective naming standards.	                        */
/*									*/
/* The symbol VRDI must be defined for the VRDI version, and TAE must   */
/* be defined for the TAE version.                                      */

#ifndef TAE
#include "xvmaininc.h"
#else
#define VMS_OS 1		/* TAE only used for VMS */
#define UNIX_OS 0
#endif

/*	XD_Read_DIBs - description
 *
 *	Purpose:
 *
 *	Written by:
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = XD_Read_DIBs( )
 *
 *	Parameter List:
 *
 *
 *	Possible Error Codes:
 *
 */

#include "xdexterns.h"
#include "xderrors.h"
#include "xdfuncs.h"

#ifdef VRDI
#include "xdroutines.h"
#include "xdsupport.h"
#endif /* VRDI */

#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>


#define	COMMENT_CHARACTER	'!'

#define	AVAIL_POS	1
#define	DEVNAME_POS	3
#define DEVCODE_POS	8
#define SUBTYPE_POS	11
#define	TERMINAL_POS	13
#define	SYSTEM_POS	18
#define	DEVMAKE_POS	22
#define	RESOLUTION_POS	38
#define	NIMPS_POS	40
#define	NLUTS_POS	43
#define	NLINES_POS	46
#define	NSAMPS_POS	52
#define	NCURSORS_POS	58
#define	DEF_CFG_POS	61
#define	PROCESSOR_POS	66
#define	OVERLAY_POS	68
#define	ALPHA_POS	70
#define AUTOT_POS       72
#define SYSALLOC_POS	74
#define LNAME_POS	76

#define	MAX_LENGTH	82

#define	REMOVE_TRAILING_BLANKS(str)			     \
   {								\
   int	i;							\
   for ( i = strlen(str)-1; str[i] == ' '; str[i--] = 0 );	\
   }

#if VMS_OS
#define MAKEUPC(c)  (islower(c) ? _toupper(c) : (c))
#define	AFFIRMATIVE(x)	(MAKEUPC(x) == 'Y')
#endif /* VMS_OS */

#if UNIX_OS
#define MAKEUPC(c)  (islower(c) ? toupper(c) : (c))
#define	AFFIRMATIVE(x)	(MAKEUPC(x) == 'Y')
#endif /* UNIX_OS */

FUNCTION int XD_Read_DIBs()

   {
   FILE		*DIB_File;
   int		Device_Number, i;
   char		DIB_Line[MAX_LENGTH+1], temp1, temp2, temp3, temp4;

#if VMS_OS
   DIB_File = fopen( "VRDI$XDDEVICE_DIB", "r" );
#endif /* VMS_OS */

#if UNIX_OS
   DIB_File = fopen( getenv("VRDIDIB"), "r" );
#endif /* UNIX_OS */

   if (DIB_File == NULL) {
      return (DIB_OPEN_ERROR);
      }

   Device_Number = 0;
   while ( fgets( DIB_Line, MAX_LENGTH, DIB_File ) != NULL ) {
      if ((DIB_Line[0] != COMMENT_CHARACTER) && (strlen(DIB_Line) >0)) {
	 DIB[Device_Number] = (struct DIB_STRUCTURE *)
                              malloc( sizeof(struct DIB_STRUCTURE) );
	 if (DIB[Device_Number] == NULL) {
	    fclose( DIB_File );
	    return (MEMORY_ERROR);
	    }

	 DIB[Device_Number]->Available = AFFIRMATIVE(DIB_Line[AVAIL_POS]);
#ifdef VRDI
	 DIB[Device_Number]->Processor = AFFIRMATIVE(DIB_Line[PROCESSOR_POS]);
	 DIB[Device_Number]->Overlay = AFFIRMATIVE(DIB_Line[OVERLAY_POS]);
	 DIB[Device_Number]->AlphaNumerics = AFFIRMATIVE(DIB_Line[ALPHA_POS]);
	 DIB[Device_Number]->AutoTracking = AFFIRMATIVE(DIB_Line[AUTOT_POS]);
	 DIB[Device_Number]->Resolution = MAKEUPC(DIB_Line[RESOLUTION_POS]);
#endif /* VRDI */
         DIB[Device_Number]->SystemAllocatable =
			 MAKEUPC((DIB_Line[SYSALLOC_POS])); /* Y, N, or X */

	 strncpy( DIB[Device_Number]->DeviceName,
		  &DIB_Line[DEVNAME_POS], DEVNAME_SIZE );
	 DIB[Device_Number]->DeviceName[DEVNAME_SIZE] = 0;
	 REMOVE_TRAILING_BLANKS(DIB[Device_Number]->DeviceName);

	 strncpy( DIB[Device_Number]->DeviceCode,
		  &DIB_Line[DEVCODE_POS], DEVCODE_SIZE );
	 DIB[Device_Number]->DeviceCode[DEVCODE_SIZE] = 0;
	 REMOVE_TRAILING_BLANKS(DIB[Device_Number]->DeviceCode);

#ifdef VRDI
	 strncpy( DIB[Device_Number]->Make,
		  &DIB_Line[DEVMAKE_POS], DEVMAKE_SIZE );
	 DIB[Device_Number]->Make[DEVMAKE_SIZE] = 0;
	 REMOVE_TRAILING_BLANKS(DIB[Device_Number]->Make);
#endif

	 strncpy( DIB[Device_Number]->Terminal,
		  &DIB_Line[TERMINAL_POS], TERMINAL_SIZE );
	 DIB[Device_Number]->Terminal[TERMINAL_SIZE] = 0;
	 REMOVE_TRAILING_BLANKS(DIB[Device_Number]->Terminal);
	 
	 strncpy( DIB[Device_Number]->LogicalName,
		  &DIB_Line[LNAME_POS], LOGNAME_SIZE );
	 DIB[Device_Number]->LogicalName[LOGNAME_SIZE] = 0;
	 REMOVE_TRAILING_BLANKS(DIB[Device_Number]->LogicalName);

	 sscanf( &DIB_Line[SUBTYPE_POS], " %c",
		 &DIB[Device_Number]->SubType );

	 sscanf( &DIB_Line[SYSTEM_POS], " %d",	/* -1 matches any machine */
		 &DIB[Device_Number]->SystemNumber );

	 sscanf( &DIB_Line[NIMPS_POS], " %d %d",
		 &DIB[Device_Number]->nImps,    &DIB[Device_Number]->nLuts );
	 sscanf( &DIB_Line[NLINES_POS], " %d %d", 
		 &DIB[Device_Number]->nLines,   &DIB[Device_Number]->nSamps );
	 sscanf( &DIB_Line[NCURSORS_POS], " %d",
		 &DIB[Device_Number]->nCursors );
#ifdef VRDI
	 sscanf( &DIB_Line[DEF_CFG_POS], " %c%c%c%c", &temp1, &temp2, &temp3,
                 &temp4);
	 DIB[Device_Number]->DefaultConfig[0] = temp1 - '0';
	 DIB[Device_Number]->DefaultConfig[1] = temp2 - '0';
	 DIB[Device_Number]->DefaultConfig[2] = temp3 - '0';
	 DIB[Device_Number]->DefaultConfig[3] = temp4 - '0';

	 for ( i = 0; i < TOTAL_SUPPORTED; i++ ) {

            /*  If the device name and subtype match, then exit the loop  */

	    if (strncmp(DIB[Device_Number]->DeviceCode,
			Supported_Devices[i].Prefix, DEVCODE_SIZE) == 0) {
	
               if ((DIB[Device_Number]->SubType==Supported_Devices[i].SubType) ||
                   (Supported_Devices[i].SubType == '*')) {
                  DIB[Device_Number]->DeviceType = Supported_Devices[i].Type;
                  break;
                  }
	       }
	    }
	 if ( i == TOTAL_SUPPORTED ) {
	    fclose( DIB_File );
	    return (DIB_FILE_ERROR);
	    }
#endif /* VRDI */

	 Device_Number ++;
	 }
      }
   TotalDevices = Device_Number;
   
   fclose( DIB_File );
   
   return (SUCCESS);
   }


/* dib_substr(string, sub)						*/
/* Returns TRUE if the string "sub" appears anywhere in the string	*/
/* "string".  FALSE otherwise.  Named "dib_substr" to avoid potential	*/
/* conflicts with the name "substr" on other compilers.			*/

int dib_substr(s, sub)
char *s, *sub;
{
int i;

for (i = 0; i < strlen(s)-strlen(sub)+1; i++)
   if (strncmp(sub, s+i, strlen(sub)) == 0)
      return TRUE;

return FALSE;
}

