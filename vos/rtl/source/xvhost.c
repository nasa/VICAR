#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"
#include "ftnbridge.h"

/* This routine returns the integer and real data representations of a	*/
/* machine given its name.  The input string is the host name as given	*/
/* in the HOST label of a file.  Note that this is the name of the kind	*/
/* of machine, not the name for a particular machine.  The output	*/
/* strings are the integer and real data representations as used in the	*/
/* INTFMT and REALFMT label items.  A host string of "NATIVE" or "LOCAL"*/
/* returns the types for the machine currently running.  From C, the	*/
/* same information can be obtained via the NATIVE_INTFMT and		*/
/* NATIVE_REALFMT macros in xvmaininc.h, but that is not available from	*/
/* Fortran.  Note that "NATIVE" or "LOCAL" can be passed in directly to	*/
/* the translation routines, so you don't have to call this routine.	*/
/*									*/
/* The main use of this routine is to allow the user to specify a	*/
/* machine type in order to generate a file of that type.  Although	*/
/* this is not the way things are usually done (files are usually output*/
/* in the native host format), it is occasionally required to write a	*/
/* file in another format.						*/
/*									*/
/* If the host is not valid, INTFMT and REALFMT are returned as empty	*/
/* strings, and an error is returned.					*/
/*									*/
/* If the special host name "HOSTNAME" is given, then the host type	*/
/* name for the native machine (the value that would go in the HOST	*/
/* label in the image, or be passed in to another call to xvhost) is	*/
/* returned in INTFMT (note: *NOT* the INTFMT for the machine!).	*/
/* REALFMT will be undefined in this case.  Using "HOSTNAME" in this way*/
/* is intended for use from Fortran only, for the rare cases it might	*/
/* be needed.  If you are in C, then use the NATIVE_HOST_LABEL macro	*/
/* from xvmaininc.h instead.						*/

/* This table must match xvmaininc.h at all times! */

#define N_HOSTS	26

static char *host_table[N_HOSTS][3] = {
		{"VAX-VMS",  "LOW",         "VAX"          },
		{"AXP-VMS",  "LOW",         "VAX"          },
		{"SUN-3",    "HIGH",        "IEEE"         },
		{"SUN-4",    "HIGH",        "IEEE"         },
		{"SUN-SOLR", "HIGH",        "IEEE"         },
		{"X86-SOLR", "LOW",         "RIEEE"        },
		{"ALLIANT",  "HIGH",        "IEEE"         },
		{"DECSTATN", "LOW",         "RIEEE"        },
		{"CRAY",     "???",         "???"          },
		{"MAC-AUX",  "HIGH",        "IEEE"         },
		{"MAC-MPW",  "HIGH",        "IEEE"         },
		{"SGI",	     "HIGH",        "IEEE"         },
		{"TEK",      "HIGH",        "IEEE"         },
		{"HP-700",   "HIGH",        "IEEE"         },
		{"AXP-UNIX", "LOW",         "RIEEE"        },
		{"AXP-LINUX","LOW",         "RIEEE"        },
		{"X86-LINUX","LOW",         "RIEEE"        },
		{"X86-64-LINX","LOW",       "RIEEE"        },
		{"PPC-LINUX","HIGH",        "IEEE"         },
		{"MAC-OSX",  "HIGH",        "IEEE"         },
		{"X86-MACOSX", "LOW",       "RIEEE"        },
		{"MAC64-OSX", "LOW",        "RIEEE"        },
		{"X86-NT",   "LOW",         "RIEEE"        },
		{"NATIVE",   NATIVE_INTFMT, NATIVE_REALFMT },
		{"LOCAL",    NATIVE_INTFMT, NATIVE_REALFMT },
		{"HOSTNAME", NATIVE_HOST_LABEL, ""         }	/* SPECIAL! */
	};

/************************************************************************/
/* Fortran-Callable Version						*/
/************************************************************************/

void FTN_NAME2(xvhost, XVHOST) (char *host, char *intfmt, char *realfmt,
				int *status, ZFORSTR_PARAM)
/* char *host;			In: Host type name */
/* char *intfmt, *realfmt;	Out: Int and real data representations */
/* int *status;			Out: status return */
{
   ZFORSTR_BLOCK
   char c_host[30];
   char c_intfmt[30];
   char c_realfmt[30];

   zsfor2c(c_host, 29, host, &host, 4, 1, 1, status);

   *status = zvhost(c_host, c_intfmt, c_realfmt);

   zsc2for(c_intfmt, 0, intfmt, &host, 4, 2, 2, status);
   zsc2for(c_realfmt,0, realfmt,&host, 4, 3, 3, status);

   return;
}

/************************************************************************/
/* C-Callable Version							*/
/************************************************************************/

int zvhost(host, intfmt, realfmt)
char *host;			/* In: Host type name */
char *intfmt, *realfmt;		/* Out: Int and real data representations */
{
   int i;
   char c_host[30];

   current_call = VHOST;

   v2_make_upper_case_max(c_host, host, 29);

   for (i=0; i<N_HOSTS; i++) {
      if (strcmp(c_host, host_table[i][0]) == 0) {
         strcpy(intfmt, host_table[i][1]);
         strcpy(realfmt, host_table[i][2]);
         return SUCCESS;
      }
   }

   strcpy(intfmt, "");
   strcpy(realfmt, "");
   v2_error_handler(NO_UNIT, INVALID_HOST);	/* Host not found */
   return INVALID_HOST;
}

