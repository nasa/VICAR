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
