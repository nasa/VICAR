$!****************************************************************************
$!
$! Build proc for MIPL module chkspace
$! VPACK Version 1.9, Thursday, February 17, 2011, 16:28:52
$!
$! Execute by entering:		$ @chkspace
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
$!   PDF         Only the PDF file is created.
$!   TEST        Only the test files are created.
$!   IMAKE       Only the IMAKE file (used with the VIMAKE program) is created.
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
$ write sys$output "*** module chkspace ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_PDF = ""
$ Create_Test = ""
$ Create_Imake = ""
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
$ if primary .eqs. "PDF" then Create_PDF = "Y"
$ if primary .eqs. "TEST" then Create_Test = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_PDF .or. Create_Test .or -
        Create_Imake .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to chkspace.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_PDF then gosub PDF_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_PDF = "Y"
$   Create_Test = "Y"
$   Create_Imake = "Y"
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
$   Create_PDF = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$   Create_PDF = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Create_PDF = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("chkspace.imake") .nes. ""
$   then
$      vimake chkspace
$      purge chkspace.bld
$   else
$      if F$SEARCH("chkspace.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake chkspace
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @chkspace.bld "STD"
$   else
$      @chkspace.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create chkspace.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack chkspace.com -mixed -
	-s chkspace_vms.c chkspace_unix.c -
	-i chkspace.imake -
	-p chkspace.pdf chkspace_vms.pdf chkspace_unix.pdf -
	-t tstchkspace.pdf tstchkspace.log_solos
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create chkspace_vms.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/*********************************  chkspace_vms   ****************************
 * CHKSPACE_VMS  (Check for available space on a disk)
 *
 *  REVISION HISTORY
 *   26-FEB-90  TCG  First version
 *   02-JAN-95  CRI(RNR) MSTP S/W Conversion (VICAR Porting) 
 ******************************************************************************/
#include "vicmain_c"		/* TAE main program shell */
#include rms			/* VMS RMS data structures */
#include "pgminc.inc"
#include "defines.h"
#include <descrip.h>		/* VMS descriptors */
#include <string.h>
#include ssdef			/* Success code definitions */
#include iodef			/* IO function code definitions */
#include dvidef			/* GETDVI device information definitions */
#include dcdef			/* Device class definitions */
#include fibdef			/* File information block definitions */
#include jpidef			/* GETJPI job/process information definitions */

#include "TAECONF.inp"		/* TAE configuration */
#include "PARBLK.inc"		/* TAE parameter block */

/*** Define constants        ***/ 
#undef fib$w_exctl
/*#define fib$rw_exctl fib$r_exctl_overlay.fib$w_exctl
*//*
*/
#define CHKS_QFNOTACT -32767	/* local code for quota file not active */
#define CHKS_NODISKQUOTA -32766	/* local code for no quota for this user */

#define SUCCESS 1
#define FAILURE 0

/*** Routine definitions      ***/

static int get_parameters();
static int valid_disk();
static int get_freespace();
static int get_quota();
static int write_to_taevbl();
static int create_desc();

void main44(void)
/*
 * CHKSPACE
 */
{
    char message[512];		/* User message buffer */
    int free_blocks;		/* Number of blocks free on structure */
    int quota_blocks;		/* Number of blocks of quota availble */
    int result_blocks;		/* Number of blocks to tell user */
    char disk_name[128];	/* Disk name input provided by user */
    char outdisk[128];		/* Parsed device name */
    int outlen;			/* Length of parsed device name */
    struct fibdef fib;
    zifmessage ("CHKSPACE version 02-JAN-95"); 

    /* Get the disk name to look at */
    get_parameters(&disk_name);

    /* Parse the disk name - reject non disk devices */
    if (valid_disk(&disk_name, &outdisk, &outlen)) {
	if (outdisk[0] == 'N' && outdisk[1] == 'L') {
	    /* This is the null device, it has infinite blocks */
	    result_blocks = 10000000;
	    qprint("CHKSPACE: This is the null device.");
	    sprintf(message, "CHKSPACE: %d blocks available.", result_blocks);
	    zvmessage(message, "");
	    write_to_taevbl(&result_blocks);
	} else {
	    /* This is a real disk, so: */
	    /*  get the space available */
	    get_freespace(&outdisk, &outlen, &free_blocks);

	    /*  see if quotas are on, user has quota, and how much */
	    get_quota(&outdisk, &outlen, &quota_blocks);

	    /*  decide what to report to the user */
	    if (quota_blocks == CHKS_QFNOTACT) {
		/* quota file is turned off or non-existent */
		result_blocks = free_blocks;
		qprint("CHKSPACE: Space constrained by disk space.");
		sprintf(message, "CHKSPACE: %d blocks available.", result_blocks);
		zvmessage(message, "");
	    } else if (quota_blocks == CHKS_NODISKQUOTA) {
		/* user does not have a quota on this disk */
		result_blocks = 0;
		qprint("CHKSPACE: You have no disk quota on this disk.");
		sprintf(message, "CHKSPACE: 0 blocks available.");
		zvmessage(message, "");
	    } else {
		/* quota file is turned on */
		if (free_blocks < quota_blocks) {
		    /* # of free blocks is less than the quota the user has */
		    result_blocks = free_blocks;
 		    qprint("CHKSPACE: Space constrained by disk space.");
		    sprintf(message, "CHKSPACE: %d blocks available.",
			    result_blocks);
		    zvmessage(message, "");
		} else {
		    /* user has less quota than free space on disk */
		    result_blocks = quota_blocks;
		    qprint("CHKSPACE: Space constrained by your disk quota.");
		    sprintf(message, "CHKSPACE: %d blocks available.",
			    result_blocks);
		    zvmessage(message, "");
		}
	    }
	    /* return the results to a TAE variable */
	    write_to_taevbl(&result_blocks);
	}
    } else {
	/* The user did not specify a disk device */
	result_blocks = 0;
	write_to_taevbl(&result_blocks);
	qprint("CHKSPACE: DISK name is not valid.");
	sprintf(message, "CHKSPACE: %d blocks available.", result_blocks);
	zvmessage(message, "");
	zabend();
    }
    return;
}

int get_parameters(char *dname)
/*
 *  Returns the value of TCL parameter disk in 'dname'
 */
{
    int status;
    int count;

    zvp("INP", dname, &count);

    return SUCCESS;
}

int valid_disk(char *dname, char *odevice, int *olen)
/*
 *  Makes sure that the user provided a valid disk name, not their display
 *  device or other nonsensical request.  Ignore any directory or file
 *  names that they provide.  We only need to know about the device.
 */
{
    struct FAB fab;		/* RMS file access block */
    struct NAM nam;		/* RMS name block */
    char *default_file_name = "SYS$DISK:";	/* default file name */
    char resultant_file_spec[NAM$C_MAXRSS];	/* output buffer for parse */
    char expanded_storage_area[NAM$C_MAXRSS];	/* output buffer for parse */
    int status;			/* system service status */
    int dvi_item;		/* GETDVI item code */
    int devclass;		/* device class */

    /* init file access block and name block */
    fab = cc$rms_fab;
    nam = cc$rms_nam;

    /* set up file name block with intputs */
    fab.fab$l_fna = dname;
    fab.fab$b_fns = strlen(dname);
    fab.fab$l_dna = default_file_name;
    fab.fab$b_dns = strlen(default_file_name);
    fab.fab$l_fop = FAB$M_OFP;
    fab.fab$l_nam = &nam;

    /* setup name block for output */
    nam.nam$b_nop = NAM$M_SYNCHK;
    nam.nam$l_rsa = resultant_file_spec;
    nam.nam$b_rss = NAM$C_MAXRSS;
    nam.nam$l_esa = expanded_storage_area;
    nam.nam$b_ess = NAM$C_MAXRSS;

    /* parse the device name */
    status = sys$parse(&fab);

    /* copy results to global variables for later use */
    *olen = nam.nam$b_dev;
    strncpy(odevice, nam.nam$l_dev, nam.nam$b_dev);

    /* call GETDVI to determine device class (disk or mailbox) */
    dvi_item = DVI$_DEVCLASS;
    status = LIB$GETDVI(&dvi_item,
			0,
			create_desc(0, odevice, olen),
			&devclass,
			0,
			0);

    /* If disk or null device mailbox, then success otherwise failure */
    if (devclass == DC$_DISK)
	return SUCCESS;
    else if (devclass == DC$_MAILBOX) {
	if (odevice[0] == 'N' && odevice[1] == 'L')
	    return SUCCESS;
	else
	    return FAILURE;
    } else
	return FAILURE;
}


int get_freespace(char *dname, int *dsize, int *size)
/*
 *  Get_freespace will determine if the disk is a volume set or a single
 *  volume.  It will then return the number of blocks free on the entire
 *  structure.
 */
{
    int dvi_item;		/* GETDVI item code */
    int status;			/* status return */
    int this_free;		/* space free on this volume */
    int total_free;		/* space free on volume set */
    int vset;			/* volume set flag */
    int vcnt;			/* count of volumes in a volume set */
    int i;			/* loop counter */
    char root_name[128];	/* root volume name of a volume set */
    short int root_len;		/* length of the root name */
    char sub_name[128];		/* subsequent volume name */
    short int sub_len;		/* length of sub name */
    int len128 = 128;		/* constant 128 */

    /* See if this device is part of a volume set */
    dvi_item = DVI$_VOLSETMEM;
    status = LIB$GETDVI(&dvi_item,
			0,
			create_desc(0, dname, dsize),
			&vset,
			0,
			0);
    if (vset == 1) {
	/* This is a volume set */
	/* get the number of volumes */
	dvi_item = DVI$_VOLCOUNT;
	status = LIB$GETDVI(&dvi_item,
			    0,
			    create_desc(0, dname, dsize),
			    &vcnt,
			    0,
			    0);
	/* get the root volume name */
	dvi_item = DVI$_ROOTDEVNAM;
	status = LIB$GETDVI(&dvi_item,
			    0,
			    create_desc(0, dname, dsize),
			    0,
			    create_desc(1, root_name, &len128),
			    &root_len);
	/* get free space on the root volume */
	dvi_item = DVI$_FREEBLOCKS;
	status = LIB$GETDVI(&dvi_item,
			    0,
			    create_desc(0, root_name, &root_len),
			    &this_free,
			    0,
			    0);
	total_free = this_free;

	/* Loop through remaining disks in volume set */
	i = 2;
	while (i <= vcnt) {
	    /* get name of next volume */
	    dvi_item = DVI$_NEXTDEVNAM;
	    status = LIB$GETDVI(&dvi_item,
				0,
				create_desc(0, root_name, &root_len),
				0,
				create_desc(1, sub_name, &len128),
				&sub_len);
	    /* get space on next volume */
	    dvi_item = DVI$_FREEBLOCKS;
	    status = LIB$GETDVI(&dvi_item,
				0,
				create_desc(0, sub_name, &sub_len),
				&this_free,
				0,
				0);
	    total_free = total_free + this_free;
	    /* set up for next trip through the loop */
	    strncpy(root_name, sub_name, sub_len);
	    root_len = sub_len;
	    i = i + 1;
	}			/* end while */
    } else {
	/* not a volume set */
	dvi_item = DVI$_FREEBLOCKS;
	status = LIB$GETDVI(&dvi_item,
			    0,
			    create_desc(0, dname, dsize),
			    &this_free,
			    0,
			    0);
	total_free = this_free;
    }
    *size = total_free;
    return;
}

int get_quota(char *dname, int *dsize, int *size)
/*
 *  Gets the quota value for this user on disk 'dname' (name length 'dsize')
 *  into 'size'.  If the disk has no quotas CHKS_NOQUOTAS is returned.  If
 *  this user has no quota CHKS_NOUSER is returned.
 */
{
    int iosb[2];		/* I/O status block */
    int len;			/* return length */
    int status;			/* system service status */
    int quoval;			/* Return value */
    int item_code;		/* JPI item code */
    int uic;			/* User Identification Code */
    int channel;		/* channel number */
    struct fibdef fib;		/* RMS file information block */
    struct DQF {		/* structure of disk quota request block */
	int dqf$l_flags;	/* flags unused */
	int dqf$l_uic;		/* UIC of the user being checked */
	int dqf$l_usage;	/* blocks in use */
	int dqf$l_permquota;	/* allowed blocks */
	int dqf$l_overdraft;	/* overdraft over permquota */
	int temp[3];		/* reserved by DIGITAL */
    } quota;

    struct mydesc {		/* simple descriptor block */
	int size;
	int address;
    } dqfdesc, fibdesc;

/* get UIC */
    item_code = JPI$_UIC;
    status = LIB$GETJPI(&item_code, 0, 0, &uic, 0, 0);

/* load the quota request block */
    quota.dqf$l_flags = 0;
    quota.dqf$l_uic = uic;
    dqfdesc.size = 32;
    dqfdesc.address = &quota;

/* load the file information block */
    fib.fib$l_wcc = 0;
    fib.fib$l_exsz = 0;
#if ALPHA_ARCH
    fib.fib$w_exctl = FIB$C_EXA_QUOTA;
#else
    fib.fib$r_exctl_overlay.fib$w_exctl = FIB$C_EXA_QUOTA;
#endif
    fibdesc.size = 48;
    fibdesc.address = &fib;

/* assign channel to device */
    status = sys$assign(create_desc(0, dname, dsize), &channel, 0, 0);

/* read quota file */
    status = sys$qiow(1, channel, IO$_ACPCONTROL, &iosb, 0, 0,
		      &fibdesc, &dqfdesc, &len, &dqfdesc, 0, 0);
/* check the status */
    if (iosb[0] == SS$_NORMAL)
	/* quota file turned on, return user information */
	quoval = quota.dqf$l_permquota - quota.dqf$l_usage;
    else if (iosb[0] == SS$_QFNOTACT || iosb[0] == SS$_ILLIOFUNC)
	/* quota file turned off, or no quota file */
	quoval = CHKS_QFNOTACT;
    else
	/* user does not have quota on this disk */
	quoval = CHKS_NODISKQUOTA;

/* deassign channel */
    status = sys$dassgn(channel);

    *size = quoval;
    return;
}


int write_to_taevbl(int *size)
/*
 *  Writes the value size into the TAE varaiable VAR_NAME
 */
{
    struct PARBLK parblk;	/* TAE parameter block */
    char name[9] = "LOCALVAR";	/* TCL name - where to put results */
    int tsize[2] =		/* Temp size, requires array (!?) */
    {0, 0};

/* Load a temp variable with the results */
    tsize[0] = *size;

/* Init parameter block */
    q_init(&parblk, P_BYTES, P_ABORT);

/* Write an integer */
    q_intg(&parblk, name, 1, &tsize, P_ADD);

/* Return parameter block to TAE */
    zvq_out(&parblk);

    return;
}

int create_desc(int i, char *cp, int *slen)
/*
 * CREATE_DESC gets a descriptor #'i' for the string 'cp' of length 'slen'
 */
{
/* Define the descriptor block. */
    static struct dsc$descriptor_s desc[10];

/* Load the descriptor block. */
    desc[i].dsc$w_length = (short) *slen;
    desc[i].dsc$b_dtype = DSC$K_DTYPE_T;
    desc[i].dsc$b_class = DSC$K_CLASS_S;
    desc[i].dsc$a_pointer = cp;
    return (&desc[i]);
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create chkspace_unix.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/*********************************  chkspace_unix  ****************************
 * CHKSPACE_UNIX  (Check for available space on a disk)
 *
 *  REVISION HISTORY
 *   26-FEB-90  TCG  First version
 *   02-JAN-95  CRI(RNR) MSTP S/W Conversion (VICAR Porting) 
 *   21-AUG-95  As per FR85841 local chkspace.ush used and -a removed from df
 ******************************************************************************/
#include <stdio.h>
#include "vicmain_c"
#include "taeconf.inp"
#include "parblk.inc"
#include <math.h>
#include <string.h>

/*** Routine definitions      ***/

static int get_parameters();
static int write_to_taevbl();
static int get_file_data();
static int convert_ascii();

/*** Define constants        ***/ 

#define SUCCESS 1
#define FAILURE 0
#define PARSIZE 5000

void main44(void)
{
    char message[512];		/* User message buffer */
    int result_blocks = 0;	/* Number of blocks to tell user */
    char file_name[128];	/* Temporary file name input */
    char ascii_space[20];	/* available space in ascii */
    int space_length;		/* Length of available space string */
    int status;                 /* Return status */

    zifmessage ("CHKSPACE version 02-JAN-95"); 

    /* Get the temporary file name */
    get_parameters(file_name);

    /* Open the file and read in the available space from UNIX DF */
    status = get_file_data (file_name, ascii_space, &space_length) ;  
    if (status == SUCCESS) {

      if (space_length > 0)
          convert_ascii (ascii_space, space_length, &result_blocks) ;  
      else result_blocks = 0;

      zvmessage("CHKSPACE: Space constrained by disk space.","");
    }
    else
      zvmessage("CHKSPACE: DISK name not valid.","");

    sprintf(message, "CHKSPACE: %d blocks available.", result_blocks);
    zvmessage(message, "");

    /* return the results to a TAE variable */
    write_to_taevbl(&result_blocks);

    return;
}

static int get_file_data (char *filename, char *space, int *length)
{                                    
    FILE *fileptr;
    char errfile[7] = "errors";	/* error file name */

    /* see if error file was created */
    fileptr = fopen (errfile, "r");
    if (fileptr) {
       fclose (fileptr);
       return FAILURE;
    }
    /* open the temporary file */
    fileptr = fopen (filename, "r");
    if (!fileptr) {
       return FAILURE;
    }

    space[0] = 0;

    /* get the ascii number */
    fgets (space,20,fileptr);

    /* get the string length */
    *length = strlen (space);

    /* close the temporary file */
    fclose (fileptr);
                                                   
    return SUCCESS;                                                                 
}

static int get_parameters(char *fname)
/*
 *  Returns the value of TCL parameter file in 'fname'
 */
{
    int count;

    /* get the temporary file name */
    zvp("INP", fname, &count);

    return SUCCESS;
}

static int convert_ascii(char *aspace, int length, int *blocks)
/*
 *  Returns the converted value of available 512 byte blocks'
 */
{
    int i, tspace;

    *blocks = 0;
    for (i=0; i<length-1; i++)
      {
        tspace = (unsigned char) *(aspace + i) - 48;
        tspace *= (int)pow ((double)10,(double)((length - 2) - i));
        *blocks += tspace;
      }
      *blocks *= 2;  /* convert kilo-bytes to 512 byte blocks */

    return SUCCESS;
}

static int write_to_taevbl(int *size)
/*
 *  Writes the value size into the TAE varaiable VAR_NAME
 */
{
    struct PARBLK par_block;

    char name[9] = "LOCALVAR";	/* TCL name - where to put results */
    int tsize[2] =		/* Temp size, requires array (!?) */
    {0, 0};

/* Load a temp variable with the results */
    tsize[0] = *size;

/* Init parameter block */
    q_init(&par_block, P_BYTES, P_ABORT);

/* Write an integer */
    q_intg(&par_block, name, 1, tsize, P_ADD);


/* Return parameter block to TAE */
    zvq_out(&par_block);

    return SUCCESS;
}
/*****************************************************************************/
/*                              End of Module                                */
/*****************************************************************************/
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create chkspace.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM chkspace

   To Create the build file give the command:

	$ vimake chkspace                     (VMS)
   or
	% vimake chkspace                     (Unix)


*************************************************************************/

#if VMS_OS
#define PROGRAM chkspace_vms
#define MODULE_LIST chkspace_vms.c 
#define CLEAN_OTHER_LIST chkspace_unix.c
#else
#define PROGRAM chkspace_unix
#define MODULE_LIST chkspace_unix.c
#define CLEAN_OTHER_LIST chkspace_vms.c
#endif

#define MAIN_LANG_C
#define USES_C

#define R2LIB
#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
$ Return
$!#############################################################################
$PDF_File:
$ create chkspace.pdf
procedure help=*
    local $space$ int
   
    parm DISK		type=(string,80) default="SYS$DISK:"
    parm VAR_NAME	type=name default=$space$

refgbl $syschar
refgbl $echo
body
!let $echo="no"

if ($syschar(1) = "VAX_VMS")

  chkspace_vms inp=&DISK localvar=@VAR_NAME

else

  if (DISK = "SYS$DISK:")
    let DISK = "."
  end-if
  let _ONFAIL="CONTINUE"

#  ush ./chkspace_scr &DISK fout.chkspace  
# on delivery, comment out above line and replace with the one below:
  ush $R2LIB/chkspace_scr &DISK fout.chkspace  

  chkspace_unix inp=fout.chkspace localvar=@VAR_NAME 

#  ush rm -f fout.chkspace
#  ush rm -f errors

end-if

!# annot function="VICAR Utilities"
end-proc
!# annot keywords=("free blocks","parameter DISK",quotas)
.TITLE
Returns amount of available space on specified disk

.HELP
PURPOSE

   CHKSPACE returns the number of free blocks (512 bytes) available to this
user on a specific disk drive.  The amount returned is the amount left in
the user's quota if quotas are enabled, or the amount left on the disk, if  
no quotas are enabled.  If the parameter DISK is not a disk name, the
program will issue an error message and return 0 blocks.  If the parameter
DISK is not supplied, the current default disk is used. 
.if1 UNIX
Disk quotas are not used on UNIX devices.

   Important note:  CHKSPACE does not reserve the space or in anyway 
   assure that the same amount of space will be available at any time
   in the near future.  It is an advisory only.  

.page
EXECUTION

   CHKSPACE 
       - will return a message containing space left available on
       the current default disk.

   CHKSPACE disk 
       - will return a message containing the space available on
       the specified disk.

   CHKSPACE disk variable
       - will return a message containing the space available on
       the specified disk and will put the value in the specified
       TCL variable.  The variable must have been declared as an
       integer. (e.g. local vari int)
         
RESTRICTIONS

       None.

.page
MESSAGES
   
    CHKSPACE: Space constrained by disk space.
       Issued when the free space available is limited only be the
       number of blocks left on the disk.

.if VAX_VMS
    CHKSPACE: Space constrained by your disk quota.
       Issued when the free space available is limited by your 
       remaining disk quota.

    CHKSPACE: You have no disk quota on this disk.
       Issued when a disk has active quotas, but you do not have
       an authorized quota on the disk.
    
    CHKSPACE: This is the null device.
       Issued when the disk name provided is the null (infinite) device.
.ifend

    CHKSPACE: DISK name not valid.
       Issued when the disk name provided is not a disk device.

    CHKSPACE: nnn blocks available.
       Always issued to indicate the number of blocks available at
       this time.

WRITTEN BY:		T. C. Greer  26-FEB-1990
COGNIZANT PROGRAMMER:	Lucas Kamp
REVISION: 1  (26-FEB-1990)
          Made portable for UNIX   Richardson(CRI) 2-JAN-95
          As per FR85841, -a option on df removed because doesn't exist on
          Andes. chkspace.ush now works in any directory, not just root. 8/95
16-Feb-2011 -lwk- renamed chkspace.ush to chkspace_scr.sh and wrapped it in a 
	separate .com file, in order to ensure that it gets created with the
	proper protections upon system build;  added "-k" to df in chkspace_scr.ush, 
	since df has changed;  changed "nawk" to "awk" because Linux doesn't know 
	nawk.

.LEVEL1
.VARIABLE DISK
Name of the disk to check.
.VARIABLE VAR_NAME
Name of TCL variable to fill.
.LEVEL2
.VARIABLE DISK

Name of the disk on which to check the number of available blocks.

.if VAX_VMS
This name can be a device name, a logical name or a complete file
specification.  If a complete file specification is supplied, only
the device name is used from it.  
.elseif UNIX
This name can be a device name, a logical name, or "~", or ".".
.ifend
If the device name turns out to be a device other than a disk, CHKSPACE
returns an error message and sets the VAR_NAME (if supplied) to 0.

.VARIABLE VAR_NAME

Name of TCL variable to contain the number of available blocks. 

This variable must previously have been declared in the following 
manner:

       LOCAL vname INT

Where vname is the name of the variable.
.END
$!-----------------------------------------------------------------------------
$ create chkspace_vms.pdf
PROCESS 
PARM INP STRING
PARM LOCALVAR NAME
END-PROC
$!-----------------------------------------------------------------------------
$ create chkspace_unix.pdf
PROCESS 
PARM INP STRING
PARM LOCALVAR NAME
END-PROC
$ Return
$!#############################################################################
$Test_File:
$ create tstchkspace.pdf
PROCEDURE HELP=*
refgbl $syschar
refgbl $echo
refgbl $autousage
BODY
let $echo="no"
let $autousage="none"
let _ONFAIL="CONTINUE"
WRITE " "
WRITE "Due to the dynamic nature of testers and disk space,"
WRITE "the program CHKSPACE must be tested interactively."
WRITE "Please see the CHKSPACE Interactive Test Plan for"
WRITE "instructions.  The following tests are to validate"
WRITE "the basics of chkspace, but not the accuracy."
WRITE " "
write "Test the basic invocation without parameters"
write " "
chkspace
write " "
write "Test with a legal disk name"
if ($syschar(1) = "VAX_VMS")
chkspace DEV:
else
chkspace /usr
end-if
write " "
write "Test with an illegal disk name"
if ($syschar(1) = "VAX_VMS")
chkspace DEVIL:
else
chkspace /devil
end-if
write " "
write "Define a local variable"
local space int
write " "
write "Test with default disk and a local variable"
chkspace ,space
write " "
disp space
write " "
write "Test with defined disk and a local variable"
if ($syschar(1) = "VAX_VMS")
chkspace VTMP: space
else
chkspace $VTMP space
end-if
write " "
disp space
write " "
write "Test with an illegal disk name and a local variable"
if ($syschar(1) = "VAX_VMS")
chkspace DEVIL: space
else
chkspace /devil space
end-if
write " "
disp space
if ($syschar(1) = "VAX_VMS")
else
write "Test with the '~'"
chkspace ~
write "Test with the '.'"
chkspace .
let $echo="no"
end-if

END-PROC
.title
TSTCHKSPACE -- Test PDF for CHKSPACE
.help

	Test Procedure:                                                       
 		Follow instructions in test plan document.
.end

$!-----------------------------------------------------------------------------
$ create tstchkspace.log_solos
tstchkspace
 
Due to the dynamic nature of testers and disk space,
the program CHKSPACE must be tested interactively.
Please see the CHKSPACE Interactive Test Plan for
instructions.  The following tests are to validate
the basics of chkspace, but not the accuracy.
 
Test the basic invocation without parameters
 
Beginning VICAR task chkspace_unix
CHKSPACE version 02-JAN-95
CHKSPACE: Space constrained by disk space.
CHKSPACE: 324324760 blocks available.
 
Test with a legal disk name
Beginning VICAR task chkspace_unix
CHKSPACE version 02-JAN-95
CHKSPACE: Space constrained by disk space.
CHKSPACE: 43416346 blocks available.
 
Test with an illegal disk name
Beginning VICAR task chkspace_unix
CHKSPACE version 02-JAN-95
CHKSPACE: DISK name not valid.
CHKSPACE: 0 blocks available.
 
Define a local variable
 
Test with default disk and a local variable
Beginning VICAR task chkspace_unix
CHKSPACE version 02-JAN-95
CHKSPACE: Space constrained by disk space.
CHKSPACE: 324324760 blocks available.
 

space=324324760

 
Test with defined disk and a local variable
Beginning VICAR task chkspace_unix
CHKSPACE version 02-JAN-95
CHKSPACE: DISK name not valid.
CHKSPACE: 0 blocks available.
 

space=0

 
Test with an illegal disk name and a local variable
Beginning VICAR task chkspace_unix
CHKSPACE version 02-JAN-95
CHKSPACE: DISK name not valid.
CHKSPACE: 0 blocks available.
 

space=0

Test with the '~'
Beginning VICAR task chkspace_unix
CHKSPACE version 02-JAN-95
CHKSPACE: Space constrained by disk space.
CHKSPACE: 324324832 blocks available.
Test with the '.'
Beginning VICAR task chkspace_unix
CHKSPACE version 02-JAN-95
CHKSPACE: Space constrained by disk space.
CHKSPACE: 324324832 blocks available.
exit
slogoff
$ Return
$!#############################################################################
