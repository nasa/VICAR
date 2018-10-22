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
