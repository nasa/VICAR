#include "xvmaininc.h"

#include <stdio.h>
#if VMS_OS
#else
#include <sys/types.h>
#include <sys/wait.h>
#endif

#include "ms_defines.h"
#include "cltsub.h"

main (int argc, char **argv)
{
 int sd;
 msUserRequestStruct	req;
 msCkStruct		ckdata;
 msSpkStruct		spkdata;

 const char *cm = "tstMsClient";
 printf ("***** ************************* ******\n");
 printf ("***** DOING REMOTE GLL_GETSPICE ******\n");
 printf ("***** ************************* ******\n");
 memset ((void*) &req, '\0', sizeof(req));
 req.sc_id	= -77;
 req.system	= REF_B1950;
 req.scet[0]	= 1990;
 req.scet[1]	= 44;
 req.scet[2]	= 5;
 req.scet[3]	= 58;
 req.scet[4]	= 16;
 req.scet[5]	= 962;
 
 strcpy (req.instrument_name, "SSI");
 strcpy (req.target_name, "VENUS");
 strcpy (req.ck_source, "NEAR");
 strcpy (req.provInfo.seg_id, "NONENONE*NONE*NONENONE000000000000*NONE*");

 memset ((void*) &ckdata, '\0', sizeof(ckdata));
 memset ((void*) &spkdata, '\0', sizeof(spkdata));
 if (msclt_gllgetspice(&req, &ckdata, &spkdata)) {
    fprintf (stderr, "%s:%s\n", cm,
	"ERROR getting GLL SPICE");
	exit (0);
    }

 printf ("***** ************************* ******\n");
 printf ("***** DOING REMOTE GLL_PUTSPICE ******\n");
 printf ("***** ************************* ******\n");
 if (msclt_gllputspice(&ckdata)) {
    fprintf (stderr, "%s:%s\n", cm,
	"ERROR writing to remote kernel");
    exit (0);
    }
 else fprintf (stderr, "GLL Kernel write SUCCESSFULL !!!\n");

/*
 printf ("***** ************************** ******\n");
 printf ("***** DOING REMOTE VGR1_GETSPICE ******\n");
 printf ("***** ************************** ******\n");
 memset ((void*) &req, '\0', sizeof(req));
 req.sc_id      = VGR_1_SC_ID;
 req.system     = REF_B1950;
 req.scet[0]    = 1979;
 req.scet[1]    = 63;
 req.scet[2]    = 19;
 req.scet[3]    = 23;
 req.scet[4]    = 0;
 req.scet[5]    = 0;
 strcpy (req.provInfo.seg_id, "NONENONE*NONE*NONENONE000000000000*NONE*");

 strcpy (req.instrument_name, "ISSN");
 strcpy (req.target_name, "IO");
 strcpy (req.ck_source, "NEAR");

 memset ((void*) &ckdata, '\0', sizeof(ckdata));
 memset ((void*) &spkdata, '\0', sizeof(spkdata));
 if (msclt_vgr1getspice(&req, &ckdata, &spkdata)) {
    fprintf (stderr, "%s:%s\n", cm, "ERROR getting VGR1 SPICE");
    }

 printf ("***** ************************** ******\n");
 printf ("***** DOING REMOTE VGR1_PUTSPICE ******\n");
 printf ("***** ************************** ******\n");
 if (msclt_vgr1putspice(&ckdata)) {
    fprintf (stderr, "%s:%s\n", cm,
        "ERROR writing to remote kernel");
    exit (0);
    }
 else fprintf (stderr, "VGR1 Kernel write SUCCESSFULL !!!\n");


 printf ("***** ************************** ******\n");
 printf ("***** DOING REMOTE VGR2_GETSPICE ******\n");
 printf ("***** ************************** ******\n");
 memset ((void*) &req, '\0', sizeof(req));
 req.sc_id	= VGR_2_SC_ID;
 req.system     = REF_B1950;
 req.scet[0]    = 1979;
 req.scet[1]    = 160;
 req.scet[2]    = 0;
 req.scet[3]    = 0;
 req.scet[4]    = 0;
 req.scet[5]    = 0;
 strcpy (req.provInfo.seg_id, "NONENONE*NONE*NONENONE000000000000*NONE*");

 strcpy (req.instrument_name, "ISSN");
 strcpy (req.target_name, "JUPITER");
 strcpy (req.ck_source, "NEAR");

 memset ((void*) &ckdata, '\0', sizeof(ckdata));
 memset ((void*) &spkdata, '\0', sizeof(spkdata));
 if (msclt_vgr2getspice(&req, &ckdata, &spkdata)) {
    fprintf (stderr, "%s:%s\n", cm, "ERROR getting VGR2 SPICE");
    }

 printf ("***** ************************** ******\n");
 printf ("***** DOING REMOTE VGR2_PUTSPICE ******\n");
 printf ("***** ************************** ******\n");
 if (msclt_vgr2putspice(&ckdata)) {
    fprintf (stderr, "%s:%s\n", cm,
        "ERROR writing to remote kernel");
    exit (0);
    }
 else fprintf (stderr, "VGR2 Kernel write SUCCESSFULL !!!\n");
*/

 printf ("***** ************************* ******\n");
 printf ("***** DOING REMOTE CAS_GETSPICE ******\n");
 printf ("***** ************************* ******\n");
 memset ((void*) &req, '\0', sizeof(req));
 req.sc_id	= -82;
 req.system	= REF_B1950;
 req.scet[0]	= 2000;
 req.scet[1]	= 350;
 req.scet[2]	= 0;
 req.scet[3]	= 0;
 req.scet[4]	= 0;
 req.scet[5]	= 0;
 
 strcpy (req.instrument_name, "ISSN");
 strcpy (req.target_name, "JUPITER");
 strcpy (req.ck_source, "NEAR");
 strcpy (req.provInfo.seg_id, "NONENONE*NONE*NONENONE000000000000*NONE*");

 memset ((void*) &ckdata, '\0', sizeof(ckdata));
 memset ((void*) &spkdata, '\0', sizeof(spkdata));
 if (msclt_casgetspice(&req, &ckdata, &spkdata)) {
    fprintf (stderr, "%s:%s\n", cm,
	"ERROR getting CAS SPICE");
	exit (0);
    }

 printf ("***** ************************* ******\n");
 printf ("***** DOING REMOTE CAS_PUTSPICE ******\n");
 printf ("***** ************************* ******\n");

 strcpy (ckdata.ck_id, "M902");
 ckdata.instrument = CASISSNA_NAIF_ID;
 if (msclt_casputspice(&ckdata)) {
    fprintf (stderr, "%s:%s\n", cm,
	"ERROR writing to remote kernel");
    exit (0);
    }
 else fprintf (stderr, "CAS Kernel write SUCCESSFULL !!!\n");

/****************************
 printf ("\n***** REQUEST ******\n");
 msclt_printuserrequeststruct(req);
 printf ("\n***** CKDATA ******\n");
 msclt_printckstruct(ckdata);
 printf ("\n***** SPKDATA ******\n");
 msclt_printspkstruct(spkdata);

 printf ("***** **************** ******\n");
 printf ("***** DOING GLL_GET_CK ******\n");
 printf ("***** **************** ******\n");
 if (msclt_gllgetck("mips_nav2.ck", "/home/sle")) {
    fprintf (stderr, "%s:%s\n", cm,
	"ERROR writing to remote kernel");
    exit (0);
    }
 if (msclt_gllgetspk ("gll_long_2.bsp", "/home/sle")) {
    fprintf (stderr, "%s:%s\n", cm,
	"ERROR reading SPK from server");
    exit(0);
    }
***************************/
}
