/******************************************************************************
 *	Copyright (c) 1990, 1991, 1992,
 *	National Aeronautics and Space Administration
 *	ALL RIGHTS RESERVED
 *
 *	The software (programs, data bases and/or documentation) on or in
 *	any media can not be reproduced, disclosed or used except under
 *	terms of the license between COSMIC and your organization.
 *****************************************************************************/



/* <<BSD/UNIX>>	
 *
 *	Batch Processing Routines
 *	-------------------------
 *
 * CHANGE LOG:
 *
 *	28-jan-84	Implement nice...dm
 *	29-jan-84	Make batch job file name unique...dm
 *	10-may-84	Add RUNTYPE=BATCH to environment,
 *			Move get_runtype() here...dm
 *	14-jun-85	Update for TAE 1.3 implementation...dm
 *	18-jun-85	Do not assume tm running from $TSYSLIB...dm
 *	01-jul-85	Fix UNIX compilation errors...dm
 *	11-dec-86	Fix lint warnings...peb
 *	13-dec-90	removed reference to TMIMAGE...ljn
 *	13-dec-90	use Bourne shell nice...ljn
 *      10-jan-91       Fix possible race condition between closing of
 *               	the job file and submittion of the job...krw
 *	15-mar-91	Make log file name unique (to match job file)
 *			so we don't overwrite log for subsequent runs...krw
 *	14-jun-91	PR1097:Change taetm to $TAEBIN/$TAEPLAT/taetm...ljn
 *	26-nov-91	PR1256:New file naming scheme for batch .job files...ljn
 *
 *****************************************************************************
 */


#include	"stdh.inp"	/* standard C definitions		    */
#include	"taeconf.inp"	/* TAE configuration definitions	    */
#include	"tminc.inc"	/* TM-only host-independent definitions	    */
#include	"fileinc.inp"	/* File handling structures and definitions */
#include 	"tmhost.inp"	
#include 	"parblk.inc"

    GLOBAL	v10bch = 0;	/* source version			    */






#define GOOD(code) ((code & 1) == 1)

    static COUNT job_number = 0;	/* number for job file   */

/*
 * assign_log: Assigns file names for batch job context
 *
 * NOTE: this name is used for compatibity with the VMS version.
 *
 */

    FUNCTION  CODE  assign_log (proc_name, cmdctx, que_name, log_name)

    TEXT	proc_name[];		/* IN: Batch job proc name	     */
    struct	CONTXT	*cmdctx;	/* IN: Context block of Batch command*/
    TEXT	que_name[6];		/* OUT: batch type, nice/nohup	     */
    TEXT	log_name[];		/* OUT: log file name		     */

    {
    FAST	CODE	code;
    struct	FSBLOCK	out_file;
    struct	VARIABLE 	*v, *v1;
    struct 	VARIABLE	*lookex();
    TEXT	errstr[STRINGSIZ+1];

    v = lookex(&(*cmdctx).qualst, "STDOUT");	/* Get the STDOUT qualifier */
    v1 = lookex(&(*cmdctx).qualst, "RUNTYPE");	/* and RUNTYPE qualifier    */
    if (v == NULL || v1 == NULL)
	{
	tmierr (1001);				/* Can't find qualifiers    */
	return (FAIL);
	}

    s_copy("nohup", que_name);			/* default queue type	     */
    if ((*v1).v_count > 1)			/* if queue specified        */
	{
	s_bcopy(SVAL(*v1, 1), que_name, 5);  	/* copy queue name	     */
	s_lower(que_name);
        if ( !s_equal(que_name, "nice") && !s_equal(que_name, "nohup")
		&& !s_equal(que_name, "norun"))
	    {
	    tmmsg(PROCFAIL, "Invalid batch queue name '%s' specified.",
		"TAE-INVBQNAM", SVAL(*v1, 1));
	    return (FAIL);
	    }
	}
    if ((*v).v_count != 0)			/* User specify log filename? */
	code = f_crack (SVAL (*v, 0), "",
		"", BLOG_TYPE, &out_file, errstr);   /* Yes, put name in msg  */
    else
	{
        TEXT	proc_job[FSPECSIZ+1];		    /* 1 up log file name */
	sprintf (proc_job,"%s%d", proc_name, job_number);
	code = f_crack ("", "", proc_job,
		BLOG_TYPE, &out_file, errstr);	    /* No, use job file name */
	}
    f_spec (&out_file, log_name);		    /* build log file name   */

    return (SUCCESS);
    }

/*    run_batch - Processes a tcl command when the runtype is BATCH.
 *
 *	Function return codes:
 *		
 *		DO_SUCCESS
 *		DO_CHECK
 *
 */
    FUNCTION CODE run_batch (procctx,cmdctx)

    struct	CONTXT	*procctx;	/* IN: Invoking proc context	  */
    struct	CONTXT	*cmdctx;	/* IN: proc with batch qualifier  */

    {
    struct	SFILE	bch_file;
    struct	FSBLOCK	bch_block;
    FAST	CODE	code;
    CODE		disp;			/* job file disposition      */
    COUNT		spec_len;
    TEXT		que_name[6];		/* nice or nohup	     */
    TEXT		job_name[FSPECSIZ+1];
    TEXT		log_name[FSPECSIZ+1];
    TAEINT		lun1 = 2;
    TAEINT		pid;


    bch_block.libr[0] = EOS;			/* file created in . dir     */
    job_number++;				/* increment job number	     */
    if (job_number > 999)
	job_number = 1;				/* roll over		     */
    get_pid (&pid);				/* process id of TM	     */
    sprintf(bch_block.name, "job%d_%d", pid, job_number);  /* file name	     */
    s_copy(JOB_TYPE, bch_block.type);		   /* type is .job	     */
    bch_block.attr[0] = EOS;
    spec_len = f_spec (&bch_block, job_name);	/* job file spec	     */
    code = f_opnblk (&bch_file, lun1, &bch_block,
    		     F_WRITE);			/* Open job file 	     */
    if (code != SUCCESS) goto openerr;

    code = assign_log ((*cmdctx).pdf.name,
	   cmdctx, que_name, log_name);/* Generate assignment for TAE */
    if (code != SUCCESS)
	return (DO_CHECK);


    code = job_ins(&bch_file, cmdctx);
    if (code != SUCCESS)
	{
	f_close(&bch_file, F_DELETE);
	return (DO_CHECK);
	}
    code = bld_tcl (cmdctx, &bch_file, BATCH);	    /* build TCL cmd      */
    if (code != SUCCESS)
	{
	f_close(&bch_file, F_DELETE);
	return (DO_CHECK);
	}
    f_close (&bch_file, F_KEEP);
    code = submit (que_name, job_name, log_name);   /* Now submit the job    */
    if (code == FAIL)				    /* job failed?           */
       {
       code = f_opnblk (&bch_file, lun1, &bch_block, F_READ);/* Open job file*/
       if (code != SUCCESS) goto openerr;
       f_close (&bch_file, F_DELETE);               /* Delete it             */
       }
    return (SUCCESS);

openerr:
    tmmsg (PROCFAIL, "Unable to create batch job file (%s). %s",
    	   "TAE-BCHOPN", job_name, bch_file.errmsg);
    return (DO_CHECK);
    }

/*    submit -  Submit the batch job using the function system().
 *
 *	Function return codes:
 *		
 *		SUCCESS
 *		FAIL
 */
    FUNCTION CODE submit (que_name, job_name, log_name)

    TEXT	que_name[];		/* in: batch queue name (nice/nohup) */
    TEXT	job_name[];		/* in: batch input file name	*/
    TEXT	log_name[];		/* in: batch output file name 	*/

    {
    TEXT	string[5*FSPECSIZ+1];
    TEXT	message[STRINGSIZ+1];
    TAEINT	status;
    char	*getenv();

    static  TEXT runtype[]  = "RUNTYPE=BATCH; export RUNTYPE; " ;
    static  TEXT act_mode[] = "TAE_ACTMODE=LOCAL; export TAE_ACTMODE; ";

    if (s_equal(que_name, "norun"))	/* do not submit the job	*/
	{
	sprintf (message, "Batch job file '%s' created successfully.",
	    job_name);
	put_stdout(message);
	return (SUCCESS);
	}

/* 	build the command string  for submitting the job		    */

    s_copy("(", string);
    s_append(runtype, string);			/* add runtype to environ   */
    s_append(act_mode, string);			/* local vs. remote activation */
    s_append("CHILD_CMD_INPUT=", string);	/* add job file name too    */
    s_append(job_name, string);			
    s_append("; export CHILD_CMD_INPUT; ", string);
    s_append("CHILD_JOB_LOG=", string);		/* add log file name too    */
    s_append(log_name, string);			
    s_append("; export CHILD_JOB_LOG; ", string);

    if (s_equal(que_name, "nice"))	/* use Bourne Shell nice	    */
	s_append("/usr/bin/nice -15", string);
    else
        s_append(que_name, string);	/* put queue name		    */
				/* default tae monitor image   	    */	
    s_append("  $TAEBIN/$TAEPLAT/taetm", string);  	
    s_append(" >", string);		/* standard output same as	    */
    s_append(log_name, string);		/* log file (Append to it)	    */
#ifdef VICAR_BATCH_LOG
/* Make the stdout log file name different from the CHILD_JOB_LOG       */
/* log file name to keep them from interfering with each other.  The    */
/* CHILD_JOB_LOG file acts pretty much like the session log does, while */
/* the stdout file still gets jumbled due to its being opened twice in  */
/* the subprocess.  This is temporary until Unix batch logs can be done */
/* right!                                                               */
    s_append(".stdout", string);
#endif
    s_append(" & )", string);
    status = system(string);		/* submit to shell for background run */
    if (status == 127)
        {
	tmmsg(PROCFAIL,"Could not issue batch run command.", "TAE-BATCHRUN");
	return (FAIL);
	}
    else if (status == 0)
	{
	sprintf (message, "Batch job file '%s' submitted successfully.",
	    job_name);
	put_stdout(message);
	return (SUCCESS);
	}
    else
	{
	tmmsg (PROCFAIL, "Batch job submission failure. Status code = %d.",
	    "TAE-NOBATCH", status);
	return (FAIL);
	}
    }
