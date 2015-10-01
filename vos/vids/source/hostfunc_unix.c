/* hostfunc.c -- host dependent functions.  All modules in this file	*/
/* are system dependent and must be converted for each new host.	*/

/*!!!!! NOTE:  These are all kludges.  Must be done right in the future!!!!!*/

#include "VIDSdefs.h"

/************************************************************************/
/* GetHostFileName returns a full host file spec given a user specified file
 * name.  If the host library function fails, it returns the user
 * specified name and a -2 (fail) status.  It uses the .Zxx default file
 * extension of the VICAR RTL.
 */
int GetHostFileName(myName, hostName)

  char		*myName;	/* in: a file name			*/
  char		*hostName;	/* out: a complete host file name	*/
{
  strcpy(hostName, myName);
  return SUCCESS;
}

/************************************************************************/
/* PeekChar -- If a keyboard character has been hit, PeekChar will
 * return that character; otherwise it returns NULL.  If the "terminate"
 * argument is True, then all i/o's will be canceled and PeekChar reset.
 */
char PeekChar(terminate)
  int terminate;			/* terminate pending i/o's?	*/
{
  return '\0';
}

/************************************************************************/
/* SetExitHandler -- Defines an exit handler with some arguments.
 */
int SetExitHandler(func, nargs, arg1, arg2, arg3)
  int (*func)();
  int nargs;
  int arg1, arg2, arg3;
{
  return SUCCESS;

}

/************************************************************************/
/* GetTermSize returns a the height and width, in characters, of the
 * terminal screen.
 */
int GetTermSize(height, width)

  int		*height;	/* out: # of lines in terminal		*/
  int		*width;		/* out: # of columns in terminal	*/
{
  *height = 24;
  *width = 80;

  return SUCCESS;
}

/************************************************************************/
/* BigMalloc allocates a large section of memory by setting up an alternate
 * paging file.  This is to get around the PAGFIL quota limit in VMS.
 * It is needed mainly for large displays, such as 2K X 2K image planes.
 * It is called exactly like malloc().  BigFree frees up the allocated memory.
 * If the requested memory size is one megabyte or less, the normal malloc()
 * is used.
 */
char *BigMalloc(size)
  int size;			/* in: # of bytes to allocate		*/
{
  return (char *)malloc(size);
}

/************************************************************************/
/* BigFree frees a large section of memory allocated by BigMalloc.
 * It is called exactly like free().
 */
void BigFree(addr)
  char *addr;			/* in: address to free */
{
  free(addr);
}
