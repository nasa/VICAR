#ifndef  FILE_NAME_INCLUDED
#define  FILE_NAME_INCLUDED	1

/**  Copyright (c) 1995, 1999 California Institute of Technology	**/
/**  U. S. Government sponsorship under NASA contract is acknowledged	**/

/**************************************************************************
 *				FILE_NAME.H
 *
 *	This include defines the prototype functions for the routines in
 *  the module FILE_NAME.  Those routines convert the file specifications
 *  from the UNIX syntax to the VMS, and vice versa.
 *************************************************************************/

char	*expand_unix_path( char * );
char	*filename_for_catalog( char * );
char	*filename_for_host ( char * );
char	*vms_path( char * );
char	*unix_path( char * );
char	*vms_file_spec( char * );
char	*unix_file_spec( char * );
char	*unix_basename( char * );
char	*unix_dirname( char * );

#endif
