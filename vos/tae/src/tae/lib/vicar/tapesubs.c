/* This same code is used in both the VICAR RTL and in the tape mount	*/
/* commands in TAE.  The code must be repeated to keep the RTL separate	*/
/* from TAE as much as possible.					*/
/*									*/
/* The following #includes must be changed to reflect which place the	*/
/* file is in.  They must be the only differences between the versions.	*/
/* For the RTL version:							*/
/*   #include "XVMAININC"						*/
/*   #include defines							*/
/* For the TAE version:							*/
/*   #include "vicartae.inc"						*/

#include "vicartae.inc"

#include <ctype.h>

/* Tape support functions that deal with the $TAPES name table.	*/

void i_crack();

#ifndef NULL
#define NULL 0
#endif

/************************************************************************/
/* Find symbolic name in tape table.  Returns index into the table.	*/
/************************************************************************/

int i_search_name(tape, count, name)
char *tape[];			/* In: pointers to "name=tape" strings	*/
int count;			/* In: size of tape table		*/
char *name;			/* In: name to find			*/
{
char e_name[TAPENAMESIZE+1], e_device[TAPEDEVSIZE+1];
char u_name[TAPENAMESIZE+1];
int i;

make_upper_case(u_name, name);
for (i=0; i < count; i++) {
   i_crack (tape[i], e_name, e_device);		/* decode entry syntax */
   if (strcmp(e_name, u_name)==0)
      return i;
}

return -1;					/* not found */
}

/************************************************************************/
/* Find device name in tape table.  Returns index into the table.	*/
/************************************************************************/

int i_search_device (tape, count, device) 
char *tape[];			/* In: pointers to "name=tape" strings	*/
int count;			/* In: size of tape table		*/
char *device;			/* In: device string to find		*/
{
char e_name[TAPENAMESIZE+1], e_device[TAPEDEVSIZE+1];
int i;

for (i=0; i < count; i++) {
   i_crack (tape[i], e_name, e_device);		/* decode entry syntax */
   if (strcmp(e_device, device)==0)
      return i;
}

return -1;				/* not found */
}

/************************************************************************/
/* Break tape table entry into component name and device.		*/
/************************************************************************/

void i_crack (entry, name, device)
char *entry;		/* In: entry of form "name=device"	*/
char *name;		/* Out: symbolic name			*/
char *device;		/* Out: device name in entry		*/
{
char *p;
char *strchr();

p = strchr(entry, '=');

if (p == NULL) {
   strcpy(name, entry);			/* equals missing */
   strcpy(device, "");
}
else {
   strncpy(name, entry, p - entry);	/* copy part before '=' */
   name[p-entry] = '\0';
   strcpy(device, p+1);			/* copy part after '=' */
}

make_upper_case(name, name);

return;
}

/************************************************************************/
/* Study file specification.   Return codes:				*/
/*		I_TAPE -- tape file spec				*/
/*		I_DISK -- disk file spec				*/
/*		I_SYNERR -- syntax error (tape only)			*/
/* It gets complicated under Unix to distinguish between tape names	*/
/* (of the form "tape/n") and subdirectories.  So, if it's not		*/
/* definitely a tape under Unix, return I_DISK.  I_SYNERR is still	*/
/* returned under VMS, which should never have slashes in the filename.	*/
/* To be a tape the name must either have no slashes and be in the	*/
/* tape table, or it must have a single slash with the part before the	*/
/* slash in the tape table and the part after composed only of digits.	*/
/************************************************************************/

int i_analyze (filespec, tape_table, count, index, filenr)
char *filespec;			/* In: file spec			*/
char *tape_table[];		/* In: $TAPES type vector		*/
int count;			/* In: size of tape_table		*/
int *index;			/* Out: for tape, index in tape_table	*/
int *filenr;			/* Out: for tape, file number		*/
{
char name[255], spec[255];
long int long_int;
int code;
int i,j;
char *slash, *pnum;
int not_tape;
char *strchr();

*filenr = 0;
*index = -1;
for (i=0, j=0; filespec[i] != '\0'; i++)	/* remove blanks */
   if (!isspace(filespec[i]))
      spec[j++] = islower(filespec[i]) ? toupper(filespec[i]) : filespec[i];
spec[j] = '\0';

slash = strchr(spec, '/');	/* find slash in spec */

if (slash == NULL) {		/* No slash found, check tape table for name */
   *index = i_search_name (tape_table, count, spec);
   if (*index < 0)
      return I_DISK;		/* not in table, so must be disk */
   else
      return I_TAPE;
}

/* Slash present */

#if UNIX_OS
not_tape = I_DISK;
#else				/* VMS */
not_tape = I_SYNERR;
#endif

if (strchr(slash+1, '/') != NULL)	/* more than one slash, not a tape */
   return not_tape;

strncpy(name, spec, slash - spec);	/* dig out symbolic tape name */
name[slash-spec] = '\0';

*index = i_search_name (tape_table, count, name);
if (*index < 0)
   return not_tape;			/* not in the tape table */

pnum = slash+1;				/* pointer at number */
if (strlen(pnum) == 0)
   return not_tape;			/* no number present */

for (i=0; i<strlen(pnum); i++)
   if (!isdigit(pnum[i]))
      return not_tape;

*filenr = atoi(pnum);			/* get tape number */

return I_TAPE;				/* it's a tape! */
}
