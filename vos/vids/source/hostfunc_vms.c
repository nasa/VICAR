/* hostfunc.c -- host dependent functions.  All modules in this file	*/
/* are system dependent and must be converted for each new host.	*/

#include "VIDSdefs.h"
#include <rms.h>			/* for GetHostFileName(),BigMalloc()*/
#include <ssdef.h>			/* for PeekChar()		*/
#include <iodef.h>			/* for PeekChar()		*/
#include <dvidef.h>			/* for GetTermSize()		*/
#include <secdef.h>			/* for BigMalloc()		*/

static struct bigmem_ctrl {
  struct bigmem_ctrl *next;
  char *start;
  char *end;
  int channel;
} *bigmem_head = NULL;

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
  static char	localName[256];
  $DESCRIPTOR(host_desc, localName);
  struct dsc$descriptor_s myName_desc;
  static char defaultName[20] = "";
  $DESCRIPTOR(default_desc, defaultName);
  static $DESCRIPTOR(log_desc,"V2$PIDCODE");	/* For the ".Zxx" extension */
  static char out[20];
  $DESCRIPTOR(out_desc, out);
  int i,stat,size;
  long context;				/* for lib$find_file		*/

  if (*defaultName == '\0') {		/* no default name yet */
    out_desc.dsc$w_length = 20;
    stat = sys$trnlog(&log_desc, &size, &out_desc, 0,0,0);
    if (stat == SUCCESS) {
      size &= 0xFFFF;			/* build ".Zxx" extension */
      memset(defaultName, 0, sizeof(defaultName));
      strcpy(defaultName, ".z");
      strncat(defaultName, out, size);
    }
  }

  default_desc.dsc$w_length = strlen(defaultName);
  myName_desc.dsc$w_length = strlen(myName);
  myName_desc.dsc$a_pointer = myName;
  myName_desc.dsc$b_class = DSC$K_CLASS_S;
  myName_desc.dsc$b_dtype = DSC$K_DTYPE_T;
  context = 0;
  stat = 1;
  stat = lib$find_file(&myName_desc, &host_desc, &context, &default_desc,0,0,0);
  if (stat != RMS$_NORMAL)
  {
    strcpy(hostName, myName);
    stat = -2;
  }
  else
  {
    for (i = 0; i < 256; i++) if (localName[i] == ' ') break;
    localName[i] = '\0';
    strcpy(hostName, localName);
  }
  lib$find_file_end(&context);
  return stat;
}
/************************************************************************/
/* PeekChar -- If a keyboard character has been hit, PeekChar will
 * return that character; otherwise it returns NULL.  If the "terminate"
 * argument is True, then all i/o's will be canceled and PeekChar reset.
 */
char PeekChar(terminate)
  int terminate;			/* terminate pending i/o's?	*/
{
  int	stat;				/* status holder		*/
  static char	theChar;		/* character read from terminal	*/
  char		saveChar;		/* saved version of theChar	*/
  static int firstCall=1;		/* True if need to initialize	*/
  static int channel;			/* i/o channel in use		*/
  static long eventFlag;		/* flag indicating i/o complete	*/
  static struct
  {
    unsigned short status;	/* i/o completion status	*/
    unsigned short count;	/* i/o transfer count		*/
    long info;			/* i/o specific info		*/
  } iosb;
  $DESCRIPTOR(name,"sys$output");	/* name of terminal;TT no good	*/
  					/* under TAE			*/

  if (!terminate)
  {
    if (firstCall)	/* Assign a channel for the terminal; get an	*/
    {			/* event flag+clear it; start 1st i/o.		*/
      if (sys$assign(&name, &channel, 0, 0) != SS$_NORMAL) return '\0';
      if (lib$get_ef(&eventFlag) != SS$_NORMAL) return '\0';
      stat = sys$clref(eventFlag);
      if ((stat != SS$_WASCLR) && (stat != SS$_WASSET)) return '\0';
      stat = sys$qio(eventFlag, channel,
                     IO$_READVBLK | IO$M_NOECHO | IO$M_NOFILTR,
                     &iosb,0,0,&theChar,1,0,0,0,0);
      if (stat != SS$_NORMAL) return '\0';
      firstCall = 0;
    }
/* Now check to see if i/o complete (key was struck).  If so, initiate	*/
/* another i/o and return the key which was struck.			*/
    if (sys$readef(eventFlag,&stat) != SS$_WASSET)     /* no key struck	*/
      return '\0';
    if (iosb.status != SS$_NORMAL) return '\0';	/* if i/o failed...	*/

    saveChar = theChar;		/* save key struck to prevent race cond	*/
    stat = sys$qio(eventFlag, channel,
                   IO$_READVBLK | IO$M_NOECHO | IO$M_NOFILTR,
                   &iosb,0,0,&theChar,1,0,0,0,0);
    if (stat != SS$_NORMAL)	/* if queueing fails, try to re-init	*/
      firstCall = 0;
    return saveChar;
  }
  else		/* if terminating, clear up everything and return NULL	*/
  {
    if (!firstCall)	/* if firstCall (not inited), do nothing	*/
    {
      sys$cancel(channel);	/* cancel any outstanding requests	*/
      sys$dassgn(channel);	/* deassign the i/o channel		*/
      lib$free_ef(&eventFlag);	/* Free the eventflag			*/
      firstCall = 1;		/* Must re-init next time		*/
    }
  }
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

  struct EX_CB				/* Exit handler control block	*/
  {
    struct EX_CB *flink; 	/* used by VMS			*/
    long	hndlrpt;	/* pointer to my handler	*/
    char	numarg;		/* number of arguments to follow*/
    char	fill[3];	/* fill out the longword	*/
    long	*statargpt;	/* pointer to mandatory first arg */
    long	farg1,farg2,farg3;	/* other optional arguments */
  };

  static struct EX_CB	excb;
  static int code;
  int status;

  excb.fill[0] = excb.fill[1] = excb.fill[2] = 0;  /* required */
  excb.hndlrpt = func;
  excb.numarg = nargs+1;
  excb.statargpt = &code;
  if (nargs >= 1)
    excb.farg1 = arg1;
  if (nargs >= 2)
    excb.farg2 = arg2;
  if (nargs >= 3)
    excb.farg3 = arg3;

  status = sys$dclexh (&excb);		/* declare the hander */
  if (status != SS$_NORMAL)
    return FAIL;

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
  static long retlen;
  static struct itmlst
  {
    short length;
    short code;
    long *address;
    long *retlen;
  } itemlist[] = {4, DVI$_TT_PAGE, 0, &retlen,		/* height */
		  4, DVI$_DEVBUFSIZ, 0, &retlen,	/* width */
		  0, 0, 0, 0 };
  $DESCRIPTOR(name,"sys$output");	/* name of terminal;TT no good	*/
  					/* under TAE			*/

  itemlist[0].address = height;
  itemlist[1].address = width;

  sys$getdviw(0,0, &name, itemlist, 0,0,0,0);

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
  int status;
  int blocks;
  char *s[2], *r[2];
  int flags;
  struct bigmem_ctrl *mem;
  struct FAB fab;

  if (size <= 1024*1024)	/* smaller than a megabyte, so use malloc() */
    return malloc(size);

  mem = malloc(sizeof(struct bigmem_ctrl));
  if (mem == NULL)
    return malloc(size);		/* not likely */

  blocks = (size+511) / 512;

  fab = cc$rms_fab;
  fab.fab$l_fna = "v2$scratch:vidstemp.pgfl";
  fab.fab$b_fns = strlen(fab.fab$l_fna);
  fab.fab$w_mrs = 512;
  fab.fab$b_rfm = FAB$C_FIX;
  fab.fab$b_fac = FAB$M_BIO | FAB$M_GET | FAB$M_PUT;
  fab.fab$l_fop = FAB$M_UFO | FAB$M_MXV | FAB$M_TMD;	/* user-file-open, */
			/* maximize version #,temporary file delete-on-close */
  fab.fab$l_alq = blocks;

  status = sys$create(&fab);

  if (status != RMS$_NORMAL && status != RMS$_CREATED) {
    free(mem);
    return malloc(size);		/* try a normal alloc */
  }

  mem->channel = fab.fab$l_stv;		/* save channel */

  r[0] = r[1] = 10;	/* Having these addresses the same causes the	*/
			/* program region to be expanded (10 is just an	*/
			/* arbitrary number < p1 region			*/

  flags = SEC$M_EXPREG | SEC$M_WRT;   /* expand program region, allow writes */

  status = sys$crmpsc(r,s,0,flags,0,0,0,mem->channel,blocks,0,0,0); /* map it */

  mem->start = s[0];
  mem->end = s[1];

  if ((status != SS$_NORMAL) && (status != SS$_CREATED)) {
    status = sys$dassgn(mem->channel);
    free(mem);
    return malloc(size);		/* try a normal malloc */
  }

  mem->next = bigmem_head;
  bigmem_head = mem;

  return mem->start;

}

/************************************************************************/
/* BigFree frees a large section of memory allocated by BigMalloc.
 * It is called exactly like free().
 */
void BigFree(addr)
  char *addr;			/* in: address to free */
{
  int status;
  char *r[2];
  struct bigmem_ctrl *mem, *oldmem;

  oldmem = NULL;
  for (mem = bigmem_head; mem != NULL; mem = mem->next) {
    if (mem->start == addr)
      break;
    oldmem = mem;
  }

  if (mem == NULL) {		/* whoops, address not allocated */
    free(addr);			/* maybe it was allocated normally */
    return;			
  }

  if (oldmem == NULL)		/* remove this struct from linked list */
    bigmem_head = mem->next;
  else
    oldmem->next = mem->next;

  r[0] = mem->start;
  r[1] = mem->end;

  status = sys$deltva(r, 0, 0);		/* delete the mapped section */

  status = sys$dassgn(mem->channel);	/* close the file (which deletes it) */

  free(mem);

}
