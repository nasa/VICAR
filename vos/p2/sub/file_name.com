$!****************************************************************************
$!
$! Build proc for MIPL module file_name
$! VPACK Version 1.9, Monday, December 07, 2009, 16:17:43
$!
$! Execute by entering:		$ @file_name
$!
$! The primary option controls how much is to be built.  It must be in
$! the first parameter.  Only the capitalized letters below are necessary.
$!
$! Primary options are:
$!   COMPile     Compile the program modules
$!   ALL         Build a private version, and unpack the PDF and DOC files.
$!   STD         Build a private version, and unpack the PDF file(s).
$!   SYStem      Build the system version with the CLEAN option, and
$!               unpack the PDF and DOC files.
$!   CLEAN       Clean (delete/purge) parts of the code, see secondary options
$!   UNPACK      All files are created.
$!   REPACK      Only the repack file is created.
$!   SOURCE      Only the source files are created.
$!   SORC        Only the source files are created.
$!               (This parameter is left in for backward compatibility).
$!   TEST        Only the test files are created.
$!   IMAKE       Only the IMAKE file (used with the VIMAKE program) is created.
$!   OTHER       Only the "other" files are created.
$!
$!   The default is to use the STD parameter if none is provided.
$!
$!****************************************************************************
$!
$! The secondary options modify how the primary option is performed.
$! Note that secondary options apply to particular primary options,
$! listed below.  If more than one secondary is desired, separate them by
$! commas so the entire list is in a single parameter.
$!
$! Secondary options are:
$! COMPile,ALL:
$!   DEBug      Compile for debug               (/debug/noopt)
$!   PROfile    Compile for PCA                 (/debug)
$!   LISt       Generate a list file            (/list)
$!   LISTALL    Generate a full list            (/show=all)   (implies LIST)
$! CLEAN:
$!   OBJ        Delete object and list files, and purge executable (default)
$!   SRC        Delete source and make files
$!
$!****************************************************************************
$!
$ write sys$output "*** module file_name ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_Test = ""
$ Create_Imake = ""
$ Create_Other = ""
$ Do_Make = ""
$!
$! Parse the primary option, which must be in p1.
$ primary = f$edit(p1,"UPCASE,TRIM")
$ if (primary.eqs."") then primary = " "
$ secondary = f$edit(p2,"UPCASE,TRIM")
$!
$ if primary .eqs. "UNPACK" then gosub Set_Unpack_Options
$ if (f$locate("COMP", primary) .eqs. 0) then gosub Set_Exe_Options
$ if (f$locate("ALL", primary) .eqs. 0) then gosub Set_All_Options
$ if (f$locate("STD", primary) .eqs. 0) then gosub Set_Default_Options
$ if (f$locate("SYS", primary) .eqs. 0) then gosub Set_Sys_Options
$ if primary .eqs. " " then gosub Set_Default_Options
$ if primary .eqs. "REPACK" then Create_Repack = "Y"
$ if primary .eqs. "SORC" .or. primary .eqs. "SOURCE" then Create_Source = "Y"
$ if primary .eqs. "TEST" then Create_Test = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if primary .eqs. "OTHER" then Create_Other = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_Test .or. Create_Imake .or -
        Create_Other .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to file_name.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Create_Other then gosub Other_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_Test = "Y"
$   Create_Imake = "Y"
$   Create_Other = "Y"
$ Return
$!
$ Set_EXE_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Default_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("file_name.imake") .nes. ""
$   then
$      vimake file_name
$      purge file_name.bld
$   else
$      if F$SEARCH("file_name.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake file_name
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @file_name.bld "STD"
$   else
$      @file_name.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create file_name.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack file_name.com -mixed -
	-s file_name.c -
	-i file_name.imake -
	-t tstfile_name.c tstfile_name.imake tstfile_name.pdf -
	-o file_name.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create file_name.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/**  Copyright (c) 1995, California Institute of Technology		**/
/**  U. S. Government sponsorship under NASA contract is acknowledged	**/

#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "xvmaininc.h"
#include "ftnbridge.h"

#define  UNIX_WMS_PATH		"/project/"
#define  VMS_WMS_PATH_LOWER	"/wms_"
#define  VMS_WMS_PATH_UPPER	"/WMS_"
#define  MAX_FILENAME_LTH	256

/**************************************************************************
 *				UNIX_BASENAME
 *
 *	Finds the basename of a unix file specification.  This is the
 *  function as the UNIX shell commmand 'basename'.  This routine just
 *  points to the base portion of the supplied file spec.  If a NULL
 *  pointer is passed in, an empty string is returned.
 *************************************************************************/
char	*unix_basename(
  char	*FileSpec)
{ int	Lth,
	Idx = 0;

  if (FileSpec == 0) return "";

  if (*FileSpec == '/') Idx++;
  while ((Lth = strcspn(&FileSpec[Idx],"/")) && FileSpec[Idx+Lth])
        Idx += Lth + 1;

  return (&FileSpec[Idx]);
}

/**************************************************************************
 *				UNIX_DIRNAME
 *
 *	Returns a pointer to the pathname portion of a file specification.
 *  This is a destructive function and operates on the string passed to it
 *  as a parameter.  If the file specification is empty or NULL, a constant
 *  value of "." will be returned.
 *************************************************************************/
char	*unix_dirname(
  char	*FileSpec)
{ 
  char	*Ptr;

  if (FileSpec == 0) return ".";

  Ptr = unix_basename(FileSpec);

  if (FileSpec == Ptr) return ".";
  if ((Ptr-FileSpec) == 1 && *(--Ptr) == '/') return "/";
  if (*(--Ptr) == '/') *Ptr = 0;

  return FileSpec;
}

/**************************************************************************
 *				VMS_PATH
 *
 *	Converts a string repesenting a UNIX directory into the equivalent
 *  VMS style pathname.  This routine recognizes the MIPS WMS name
 *  convention differences between VMS and UNIX and compensates for it.
 *  If there is a directory path that can not be converted, a NULL pointer
 *  will be returned.  This routine has a weak check to see if the input
 *  pathname is already a VMS direcotry, if it is, no conversion is
 *  performed.
 *  VALID INPUT PATHS:			INVALID INPUT PATHS:
 *	/Path					/
 *	/Path/					~user
 *	subdirectory				~user/pathname
 *	subdirectory/
 *************************************************************************/
char	*vms_path(
  char	*unix_path)
{ static char	path[MAX_FILENAME_LTH] = {""};
  char	*up = unix_path,
	*vp = path,
	*tp,
	wms_path[MAX_FILENAME_LTH];
  int	lth;

  if (strlen(unix_path) == 0) return ("");
  if (strlen(unix_path) == 1 && *up == '/') return (NULL);
  if (*up == '~') return (NULL);	/* ~usr is invalid */
  if (strpbrk(unix_path,":["))		/* Already a VMS path */
  { strcpy(path,unix_path);
    return (path);
  }

  /***  Check for current working directory  ***/
  if (strcmp(unix_path,"./") == 0)
  { strcpy(path,"[]");
    return (path);
  }

  /***  Check for WMS pathname ... need to convert it  ***/
  if (strncmp(unix_path,UNIX_WMS_PATH,strlen(UNIX_WMS_PATH)) == 0)
  { strcpy(wms_path,VMS_WMS_PATH_UPPER);
    strcat(wms_path,(up+strlen(UNIX_WMS_PATH)));
    up = wms_path;
  }

  if (*up == '/')		/*  Check for root level pathname */
  { lth = strcspn(up+1, "/");
    memcpy(vp, (up+1), lth);
    vp[lth] = 0;
    strcat(vp, ":");
    strcat(vp, up+(lth+1));

    vp += strlen(vp) - 1;
    if ((tp = strpbrk(path, "/")))
    { if (vp == tp)
      { *vp = 0;
        return path;
      } else *tp = '[';
      if (*vp == '/') *vp =']';	/* Allow '/' or NULL terminated pathname */
      else strcat(vp,"]");
    } else return path;		/* Only disk portion specified */
  } else
  { strcpy(vp, "[.");		/* Path starts from current directory */
    strcat(vp, up);
    
    vp += strlen(vp) - 1;
    if (*vp == '/') *vp = ']';	/* Allow '/' or NULL terminated pathname */
    else strcat(vp,"]");
  }

  while ((vp = strpbrk(path, "/"))) *vp = '.';

  return path;
}

/**************************************************************************
 *				UNIX_PATH
 *
 *	Converts a string repesenting a VMS directory into the equivalent
 *  UNIX style pathname.  This routine recognizes the MIPS WMS name
 *  convention differences between VMS and UNIX and compensates for it.
 *  Only full pathnames or subdirectories can be converted correctly.
 *  If there is a directory path that can not be converted, a NULL pointer
 *  will be returned.  The returned pathname will be terminated by a "/"
 *  unless the current directory is specified  (i.e., "[]" or "").  This
 *  routine does a simple check to see if the input pathname is already a
 *  UNIX pathname.  If it is, no conversion is performed, and the input
 *  string contents is returned untouched.
 *  VALID INPUT PATHS:			INVALID INPUT PATHS:
 *	DISK:					DISK:[.PATH]
 *	[.PATH]					[PATH]
 *	DISK:[PATH]				[PATH]FILENAME
 *	DISK					DISK:[PATH]FILENAME
 *						DISK:[]
 *************************************************************************/
char	*unix_path(
  char	*vms_path)
{ static char	path[MAX_FILENAME_LTH] = {""},
		wms_path[MAX_FILENAME_LTH] = {""};
  char	*vp = vms_path,
    *up = path;
  int	idx,
	lth;

  if (strlen(vms_path) == 0) return ("");
  path[0] = 0;
  if (strpbrk(vms_path,"/"))		/* Already a unix path */
  { if (*vp == '~') return (NULL);	/* No "~usr" paths allowed */
    strcpy(path,vms_path);
    if (path[strlen(path)-1] != '/')	/* Terminated it with a '/' */
       strcat(path,"/");

    /*** Convert everything to lower if path is "/project/" ***/
    if (strncmp(path,"/project/",9) == 0)
       for (idx=0; path[idx]; idx++)
           path[idx] = tolower(path[idx]);

    return path;
  }

  if (*vp != '[')			/* Check for root level pathname */
  { strcpy(up,"/");
    lth = strcspn(vms_path, ":");
    strncat(up, vp, lth);
    up[lth+1] = 0;
#ifdef STUPID_SGI_COMIPLER_WARNINGS
     Just a thought to translate logical names ...
     ... it is a bit too involved for this routine 
   if (getenv(up+1))
   { strcpy(logical_path,getenv(up+1));
     /***  Check for "concealed" logical names  ***/
     if (logical_path[strlen(logical_path)-1] == ']')
     { /* Concealed logical name */
     } else if (logical_path[0] == '_')
     { /* Too far */
     } else strcpy(up+1,logical_path);
   }

#endif

    strcat(up, "/");
    vp += lth;
    if (*vp == ':') vp++;		/* Look for directory part */
    if (strlen(vp) == 0) return (up);
    if (*vp != '[') return (NULL);
    if (strncmp(vp,"[.",2) == 0) return (NULL);
    if ((int)strlen(vp) < 3) return (NULL);
    vp++;
  } else
  { if ((int)strlen(vp) < 3)
    { if (strcmp(vp,"[]") == 0)
      { strcpy(path,"./");
        return (path);
      } else return (NULL);
    }
    if (strncmp(vp,"[.",2) != 0) return (NULL);
    else vp += 2;			/* skip over the "[." */
  }

  strcat(up,vp);
  while ((up = strpbrk(path, "."))) *up = '/';
  lth = strlen(path);
  if (path[strlen(path)-1] == ']')
     path[strlen(path)-1] = '/';
  else if (strpbrk(path,"]")) return (NULL);
       else strcat(path,"/");
  up = path;

  /***  Check for WMS pathname ... need to convert it  ***/
  if (strncmp(path,VMS_WMS_PATH_LOWER,strlen(VMS_WMS_PATH_LOWER)) == 0 ||
      strncmp(path,VMS_WMS_PATH_UPPER,strlen(VMS_WMS_PATH_UPPER)) == 0)
  { strcpy(wms_path,UNIX_WMS_PATH);
    strcat(wms_path,(up+strlen(VMS_WMS_PATH_UPPER)));
    up = wms_path;
  }

  /*** Convert everything to lower if path is "/project/" ***/
  if (strncmp(up,"/project/",9) == 0)
     for (idx=0; up[idx]; idx++)
         up[idx] = tolower(up[idx]);

  return up;
}

/**************************************************************************
 *				VMS_FILE_SPEC
 *
 *	Converts a UNIX file specification, including the pathname, into
 *  a VMS style file specification.  This routine also checks to see if it
 *  is already a VMS file specification, or at least does not contain a
 *  pathname portion.  This routine will return a NULL pointer if the
 *  file name can not be converted.
 *************************************************************************/
char	*vms_file_spec(
  char	*file_name)
{ static char	file_spec[MAX_FILENAME_LTH] = {""};
  char	*path,
	temp_name[MAX_FILENAME_LTH];
  int	lth;

  lth = strlen(file_name);
  if (lth > sizeof(file_name)) lth = sizeof(file_spec) - 1;

  if (strchr(file_name, '/') == NULL )
  { memcpy(file_spec,file_name,lth);
    file_spec[lth] = 0;
    return (file_spec);				/* Already a VMS file spec */
  }

  strcpy(temp_name, file_name);
  lth = strlen(temp_name) - 1;
  while (temp_name[lth] != '/') lth--;
  temp_name[lth] = 0;
  /* Check if only a path was given, not a full file spec */
  if (strlen(&temp_name[lth+1]) == 0) return (NULL);
  if ((path = vms_path(temp_name)))
     strcpy(file_spec,path);
  else return (NULL);
  strcat(file_spec,&temp_name[lth+1]);

  return file_spec;
}

/**************************************************************************
 *				UNIX_FILE_SPEC
 *
 *	Converts a VMS file specification, including the pathname, into
 *  a UNIX style file specification.  This routine also checks to see if it
 *  is already a UNIX file specification, or at least does not contain a
 *  pathname portion.  This routine will return a NULL pointer if the
 *  file name can not be converted.
 *************************************************************************/
char	*unix_file_spec(
  char	*file_name)
{ static char	file_spec[MAX_FILENAME_LTH] = {""};
  char	*fn,
	*path,
	temp_name[MAX_FILENAME_LTH];
  int	idx,
	lth;

  lth = strlen(file_name);
  if (lth > sizeof(file_spec)) lth = sizeof(file_spec) - 1;

  if ((fn = strchr(file_name, '/')) != NULL )
  { if (file_name[0] == '~') return (NULL);
    memcpy(file_spec,file_name,lth);
    file_spec[lth] = 0;
    if (file_spec[lth-1] == '/') return (NULL);	/* Only a Pathname */

    /*** Convert everything to lower if first path is "project" ***/
    if (strncmp(file_spec,"/project/",9) == 0)
       for (idx=0; file_spec[idx]; idx++)
           file_spec[idx] = tolower(file_spec[idx]);

    return (file_spec);			/* Already a UNIX file spec */
  }

  strncpy(temp_name, file_name, sizeof(temp_name));
  temp_name[sizeof(temp_name)-1] = 0;

  if ((fn = strchr(temp_name, ']')) == NULL)
  { if (strchr(file_name, ':'))
    { /***  Error, can not have a disk designation only  ***/
      return (NULL);
     }
     return (file_name);		/* Only a filename */
  }
  *fn = 0;
  fn++;
  if (strlen(fn) == 0) return (NULL);	/* Only a Pathname */

  if ((path = unix_path(temp_name)))
     strcpy(file_spec,path);
  else return (NULL);
  strcat(file_spec,fn);

  /*** Convert everything to lower if first path is "project" ***/
  if (strncmp(file_spec,"/project/",9) == 0)
     for (idx=0; file_spec[idx]; idx++)
         file_spec[idx] = tolower(file_spec[idx]);

  return file_spec;
}

/**************************************************************************
 *				EXPAND_UNIX_PATH
 *
 *	Expands a UNIX path or filename for environment or user home
 *  locations.  It does not deal with the home locations for a user that
 *  is not the current user.
 *************************************************************************/
char	*expand_unix_path(
  char	*InPath)
{ int   LthEnv;
  static char	Buffer[256];
  char	TempBuf[256],
	*Pin = InPath;

  memset(Buffer,0,sizeof(Buffer));

  while (Pin && *Pin != 0)
  { if (*Pin == '$')
    { Pin++;
      strcpy(TempBuf,Pin);
      LthEnv = strcspn(Pin," ./,?<>';:`~!@#$%^&*()=+[]{}\"\\");
      TempBuf[LthEnv] = 0;
      if (getenv(TempBuf))
         strcat(Buffer,getenv(TempBuf));
      else strncat(Buffer,Pin-1,LthEnv+1);
      Pin += LthEnv;
    } else if (Pin == InPath && *Pin == '~')
    { if (*(Pin+1) == '/' && getenv("HOME"))
      { strcat(Buffer,getenv("HOME"));
        Pin++;
      } else strncat(Buffer,Pin++,1);
    } else strncat(Buffer,Pin++,1);
  }

  return Buffer;
}

/**************************************************************************
 *				FILENAME_FOR_CATALOG
 *
 *	This routine is geared for the MIPS WMS catalog system which uses
 *  a UNIX style file specification.  This routine basically is the same
 *  as UNIX_FILE_SPEC routine, except it verifies that the whole path is
 *  specified.  If it can not generate the file specification, a NULL
 *  pointer is returned.
 *************************************************************************/
char	*filename_for_catalog(
  char	*file_spec)
{ char	*cat_spec;

  cat_spec = unix_file_spec(file_spec);
  if (cat_spec && *cat_spec != '/') return (NULL);
  else return cat_spec;
}

/**************************************************************************
 *				FILENAME_FOR_HOST
 *
 *	This routine will convert the filename from either a UNIX or VMS
 *  style specification into the current host's style (VMS or UNIX).  If
 *  it can not convert the filename, a NULL pointer is returned.
 *************************************************************************/
char	*filename_for_host(
  char	*file_spec)
{
#if VMS_OS
  return (vms_file_spec(file_spec));
#else
  return (unix_file_spec(file_spec));
#endif
}

/*************************************************************************
 *	FTN_NAME		Filename_For_Catalog
 *
 *	This is the fortran bridge routine for the filename_for_catalog
 *  routine.  There is a slight differnce in the routine calls because
 *  of the way the bridging works.  The calling software must supply the
 *  memory for the returned string as a second parameter, and there is
 * an additional third parameter returning the status (0 - good, !0 - bad)
 ************************************************************************/
void	FTN_NAME2_(file_for_catalog, FILE_FOR_CATALOG) ( char *file_spec_in,
		char *file_spec_out, int *status, ZFORSTR_PARAM)
{
  ZFORSTR_BLOCK
  char Cstrg[256],
	*temp;

  *status = 0;
  zsfor2c(Cstrg,sizeof(Cstrg),file_spec_in,&file_spec_in,3,1,1, status);
  temp = filename_for_catalog(Cstrg);
  if (temp) {
     zsc2for(temp,0,file_spec_out,&file_spec_in,3,2,2, status);
  }
  else *status = 1;

  return;
}

/*************************************************************************
 *	FTN_NAME		Filename_For_Host
 *
 *	This is the fortran bridge routine for the filename_for_host
 *  routine.  There is a slight differnce in the routine calls because
 *  of the way the bridging works.  The calling software must supply the
 *  memory for the returned string as a second parameter, and there is
 *  an additional third parameter returning the status (0 - good, !0 - bad)
 ************************************************************************/
void FTN_NAME2_(file_for_host, FILE_FOR_HOST) ( char *file_spec_in,
			char *file_spec_out, int *status,ZFORSTR_PARAM)
{
  ZFORSTR_BLOCK
  char	Cstrg[256],
	*temp;

  *status = 0;
  zsfor2c(Cstrg,sizeof(Cstrg),file_spec_in,&file_spec_in,3,1,1, status);
  temp = filename_for_host(Cstrg);
  if (temp) {
     zsc2for(temp,0,file_spec_out,&file_spec_in,3,2,2, status);
  }
  else *status = 1;

  return;
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create file_name.imake
/******************************************************************************
/*
/*                     IMAKE FILE FOR MODULE file_name
/*
/*   To Create the build file give the command:
/*
/*		$ vimake file_name			(VMS)
/*   or
/*		% vimake file_name			(Unix)
/*
/*****************************************************************************/

/***  Define for whom this file exisits  ***/
#define SUBROUTINE file_name		/* Only one of these */
/*#define PROGRAM file_name		/* Only one of these */

/***  List all modules which are used by locally by this module  ***/
#define MODULE_LIST file_name.c

#define MAIN_LANG_C

#define USES_ANSI_C

/***  Specify  Program or Subroutine specific DEFINES  ***/
#ifdef PROGRAM
#define R2LIB
#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#endif

#ifdef SUBROUTINE
#define P2_SUBLIB
#endif

/***  Local library definitions ...
/***  ... must be commented out when delivered
/***
#ifdef PROGRAM
#define LIB_LOCAL
#endif

#if VMS_OS
#define LOCAL_LIBRARY test_lib.olb
#else
#define LOCAL_INCLUDE -I$(IncludePath)
#define LOCAL_LIBRARY $(ObjectPath)/librts.a
#endif
/***  End of local library definitions  ***/
/**********  End of file_name imake file  **********/
$ Return
$!#############################################################################
$Test_File:
$ create tstfile_name.c
/**  Copyright (c) 1995, California Institute of Technology		**/
/**  U. S. Government sponsorship under NASA contract is acknowledged	**/

#include <stdio.h>
#include "file_name.h"
char	*file_names[] = {	"wms_gll:[ssi]whatta.img",
				"WMS_GLL:[ssi]whatta.img",
				"WMS_GLL_WORK:[ssi]boo.img",
				"WMS_VMS:[TEST.LOWER.CASE]CONVERSION.DAT",
				"/project/gll/blah/blah/blah/stuff.dat",
				"/project/LOWER/CASE/CONVERSION.EXP",
				"error:[.directory]file.dat",
				"[more.errors]file.dat",
				"/not/wms/file.dat",
				"[.invalid.catalog]valid.host",
				"CatalogNeeds.path",
				"~user/file/invalid",
				"/error/no/file/spec/",
				"error:[no.file.spec]",
				0 };
char	*paths_vms[] = {	"Disk_name:",		"[.subdirectory.list]",
				"full:[path.name]",	"just_disk",
				"[.partial",		"more:[partial",
				"bad:[.listing]",	"[need.disk.too]",
				"no:[files]allowed",	"[]",
				"/no/conversion/needed","WMS_GLL:[Case.test]",
				"WMS_MPF:[IMP]",	"/project/Change/Case",
				0 };
char	*paths_unix[] = {	"/root_level",		"/terminated/",
				"sub.directory.path",	"terminated.sub.path/",
				"/",			"~no/users/allowed",
				"no:[conversion.needed]","./",
				"/project/Change/WMS",
				0 };
main()
{ char	*input_name,
	*catalog_name,
	dummy_buf[1024],
	*host_name,
	*ptr;
  int	idx;

  for (idx=0; file_names[idx]; idx++)
  { input_name = file_names[idx];
    ptr = catalog_name = filename_for_catalog(input_name);
    if (catalog_name == NULL) catalog_name = "Error in file spec";
    host_name = filename_for_host(input_name);
    if (host_name == NULL) host_name = "Error in file spec";

    printf("\nInput: %s\n  Catalog: %s\n  Host: %s\n",
           input_name, catalog_name, host_name);
    if (ptr)
    { strcpy(dummy_buf,ptr);
      printf("  Basename: %s\n  Dirname: %s\n",
             unix_basename(dummy_buf),unix_dirname(ptr));
    }
  }

  printf("\n\nVMS to UNIX path test:\n");
  for (idx=0; paths_vms[idx]; idx++)
  { printf("   %s --> ",paths_vms[idx]);
    ptr = unix_path(paths_vms[idx]);
    if (ptr) printf("%s\n",ptr);
    else printf("Bad pathname\n");
  }

  printf("\n\nUNIX to VMS path test:\n");
  for (idx=0; paths_unix[idx]; idx++)
  { printf("   %s --> ",paths_unix[idx]);
    ptr = vms_path(paths_unix[idx]);
    if (ptr) printf("%s\n",ptr);
    else printf("Bad pathname\n");
  }
    
  printf("\n\n Vanna says \"Bye Bye\",  and so do I\n");
  exit(0);
}
$!-----------------------------------------------------------------------------
$ create tstfile_name.imake
/******************************************************************************
/*
/*                     IMAKE FILE FOR MODULE tstfile_name
/*
/*   To Create the build file give the command:
/*
/*		$ vimake tstfile_name			(VMS)
/*   or
/*		% vimake tstfile_name			(Unix)
/*
/*****************************************************************************/

/***  Define for whom this file exisits  ***/
/*#define SUBROUTINE tstfile_name		/* Only one of these */
#define PROGRAM tstfile_name		/* Only one of these */

/***  List all modules which are used by locally by this module  ***/
#define MODULE_LIST tstfile_name.c

#define MAIN_LANG_C

#define USES_ANSI_C

/***  Specify  Program or Subroutine specific DEFINES  ***/
#ifdef PROGRAM
#define R2LIB
#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#endif

#ifdef SUBROUTINE
#define P2_SUBLIB
#endif

/***  Local library definitions ...
/***  ... must be commented out when delivered
/***
#ifdef PROGRAM
#define LIB_LOCAL
#endif

#if VMS_OS
#define LOCAL_LIBRARY test_lib.olb
#else
#define LOCAL_INCLUDE -I$(IncludePath)
#define LOCAL_LIBRARY $(ObjectPath)/librts.a
#endif
/***  End of local library definitions  ***/
/**********  End of tstfile_name imake file  **********/
$!-----------------------------------------------------------------------------
$ create tstfile_name.pdf
process help=*
end-proc
.title
TEST Program TSTFILE_NAME
.help
This is a test program for the subroutines in the module FILE_NAME.  It
requires no parameters and can be run outside of VICAR.
.end
$ Return
$!#############################################################################
$Other_File:
$ create file_name.hlp
FILE_NAME

	This module contains a set of C anf fortran callable routines whichs
are used to convert valid file specfication between the UNIX and VMS syntax.
There are a total of 9 routines in this package, two convert the pathname or
directory, two convert the entire file specification and the remaining are
used to convert a filespec for use with the MIPS catalog and Working Mission
Storage, and to convert a file spec into the current host's syntax.

	All of the routines are case sensitive and return a pointer to the
converted specification.  All of the C routines have one input parameter which
is a pointer to specification which needs to be converted.  The input data is
not modified.  The maximum length of the returned string is 256 bytes.  If
there is an error in converting the specification, a NULL pointer is returned.

	The following text shows the routine protoypes.  These prototype
declarations are defined in the file: "file_name.h"

	char	*filename_for_catalog( char * );
	char	*filename_for_host ( char * );
	char	*vms_path( char * );
	char	*unix_path( char * );
	char	*vms_file_spec( char * );
	char	*unix_file_spec( char * );
	char	*expand_unix_path( char * );

	Additionally, the two fortran routines programs mimic the C versions,
but only convert full filenames, the capability to convert just the path is
not available.  The routines, and calling sequences are:

	file_for_catalog( character, character, integer)
	file_for_host( character, character, integer)

The first parameter is the input file specification, the second is the
output file specification, the third parameter is the return status.  A value
of 0 means the conversion was accomplished correctly, a non-zero value
indicates a failure.  The same character variable can be used for both
parameters.  The routines are truely subroutines so the "call" statement
must be used.

	There are some restricitions to the specifications which can be
correctly translated.  These routines do not perform a complete syntax
check for full complience, but are fairly simple translaters.  Be advised
that some valid UNIX file specifications are invalid in the VMS realm
(i.e., a space or multiple periods in the filename).

	There are additional capabilities to correct for the VMS and UNIX
system use of the WMS.  The WMS is a UNIX based file server and the directory
structure could not be correctly mounted from a VMS client.  To compensate
for this, a naming convention has been implement to convert between UNIX
and VMS path names.  A UNIX host would reference a file in the path:
		/project/gll/ssi/
while a VMS host would reference the file in the directory:
		WMS_GLL:[SSI]
Whenever a UNIX path begins with "/project/" or a VMS path begins with "WMS_",
these routines will make the correct conversion.  To further complicate
matters,  VMS is not a case-sensitive system.  The WMS has been set-up, by
convention, to ony contain lowercase file specification.  Anytime a conversion
is run through the filename or path conversion routines, the string will
be converted to lowercase if the first level directory is "project".  This
will occur for UNIX to UNIX conversions as well as VMS to UNIX conversions.


	The following lists typical valid syntax and the invalid syntax
which is trapped for the 6 routines.

filename_for_catalog
	Input File Specification	Converted File Specification
	DISK:[FULL.NAME]FILE.EXT	/DISK/FULL/NAME/FILE.EXT
	DISK_ONLY:FILE.NAME		/DISK_ONLY/FILE.NAME
	/unix/specs/are.ok		/unix/specs/are.ok
	/PROJECT/not/WMS		/PROJECT/not/WMS
	/project/WMS/CaseConverted	/project/wms/caseconverted

	Invalid File specifications:
	[NEED.DISK]FILE.EXT		NULL Pointer
	[.SUBDIRECTORY]NOT_OKEY.DOKEY	NULL Pointer
	Just_FileName.NOT_Okay		NULL Pointer

filename_for_host	Calls vms_file_spec or unix_file_spec depending upon
			the host type.

vms_path
	Input File Specification	Converted File Specification
	/full/paths/are/okay/		full:[paths.are.okay]
	/missing/slash/okay		missing:[slash.okay]
	sub/directory/okay/		[.sub.directory.okay]
	/disk_only/			disk_only:
	/disk_only			disk_only:
	subdirectory			[.subdirectory]
	VMS:[SYNTAX.OKAY]		VMS:[SYNTAX.OKAY]
	NO:[VMS.CHECKING		NO:[VMS.CHECKING

	Invalid File specifications:
	~no/usr/path/allowed		NULL Pointer
	/project/			wms_:

unix_path
	Input Path Specification	Converted Path Specification
	Disk:[Full.Path]		/Disk/Full/Path/
	[.Sub.Directory.only]		sub/directory/only/
	[.missing.bracket.okay		missing/bracket/okay/
	DiskOnly:			/DiskOnly/
	No_colon_okay			/No_colon_okay/
	unix/okay/too			unix/okay/too/

	Invalid Path specifications:
	Need:[.Full Path]		NULL Pointer
	~no/usr/paths/though		NULL Pointer
	WMS_:				/project/

vms_file_spec
	Input Path Specification	Converted Path Specification
	/DISK_NAME/PATH/NAME/FILE.DAT	DISK_NAME:[PATH.NAME]FILE.DAT
	SUBDIRECTORY/FILE.NAME		[.SUBDIRECTORY]FILE.NAME
	[VMS.NAME]OKAY.TOO		[VMS.NAME]OKAY.TOO
	DISK:VMS_NAME.GOOD		DISK:VMS_NAME.GOOD

	Invalid Path specifications:
	~no/user/paths/allowed.period	NULL Pointer

unix_file_spec
	Input Path Specification	Converted Path Specification
	DISK:[FULL.NAME]FILE.EXT	/DISK/FULL/NAME/FILE.EXT
	[.SUBDIRECTORY]OKEY.DOKEY	SUBDIRECTORY/OKEY.DOKEY
	DISK_ONLY:FILE.NAME		/DISK_ONLY/FILE.NAME
	/unix/specs/are.ok		/unix/specs/are.ok
	Just_FileName.Okay		Just_FileName.Okay

	Invalid Path specifications:
	[NEED.DISK]FILE.EXT		NULL Pointer
	~no/usr/paths/allowed.got_it	NULL Pointer
$ Return
$!#############################################################################
