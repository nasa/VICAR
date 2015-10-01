/******************************************************************************
 *	Copyright (c) 1990, 1991, 1992,
 *	National Aeronautics and Space Administration
 *	ALL RIGHTS RESERVED
 *
 *	The software (programs, data bases and/or documentation) on or in
 *	any media can not be reproduced, disclosed or used except under
 *	terms of the license between COSMIC and your organization.
 *****************************************************************************/

/*
 * Change Log:
 * 19-mar-91	Modified for TAE Plus V5.1...krw
 * 17-apr-91	VMS port: prog names, decterm calling sequence...ljn
 * 18-may-91	Don't include `$' in DECterm commands...ljn
 * 31-jul-91	ddo_palette.res name shortened to 14 chars...ljn
 * 27-sep-91	Too many %s's in sprintf() in generate_ok()...ljn
 * 05-oct-92	IBM RS/6000 needs time.h, too...rt
 * 07-oct-92	List of demos changed, so now dynamically using File 
 *         	Selection; Basic Items now executes presdemo; taecg code
 *              added for C++ (Fortran removed from panel)...kbs,cew
 * 01-jul-93	Sun needs time.h, too...rt
 */
#include 	"taeconf.inp"  
#include 	"wptinc.inp"  
#include	"symtab.inc"  
#include	"parblk.inc" 
#include	"fileinc.inp"
#include	"chartype.inc"
#if defined (hpux) || defined(VMS) || defined(AIX) || defined(sun) || defined(sgi)
#include	"time.h"
#endif
  
#ifdef VMS
#define HOMEROOM_RES "$TAEDEMORES:homeroom.res"
#define PRESDEMO_RES "$TAEDEMORES:presdemo.res"
#define DDODEMO_RES  "$TAEDEMORES:ddodemo.res"
#define DDO_PALETTE_RES "$TAEDEMORES:ddopalette.res"
#else
#define HOMEROOM_RES "$TAEDEMORES/homeroom.res"
#define PRESDEMO_RES "$TAEDEMORES/presdemo.res"
#define DDODEMO_RES  "$TAEDEMORES/ddodemo.res"
#define DDO_PALETTE_RES "$TAEDEMORES/ddopalette.res"
#endif

#define EVENT_HANDLER 		/* a flag for documentation */  
 
struct VARIABLE *Vm_Find ();  
  
#define StringParm(vmId, name)    (SVAL(*Vm_Find(vmId, name),0))  
#define IntParm(vmId, name)       (IVAL(*Vm_Find(vmId, name), 0))  
#define RealParm(vmId, name)      (RVAL(*Vm_Find(vmId, name), 0))  
  
/*	Vm objects and panel Ids.   */ 
 
Id vmCollection ;
static Id homeroomTarget, homeroomView, homeroomId; 
static Id demosTarget, demosView, demosId; 
static Id HomeiconTarget, HomeiconView, HomeiconId; 
static Id getnameTarget, getnameView, getnameId; 
static Id objectsTarget, objectsView, objectsId;
static Id ItemsTarget, ItemsView, ItemsId;
static Id dcficonsTarget, dcficonsView, dcficonsId;
static Id misciconsTarget, misciconsView, misciconsId;
static Id hmrmiconsTarget, hmrmiconsView, hmrmiconsId;
static Id threedTarget, threedView, threedId;
static Id iconsTarget, iconsView, iconsId;
static Id pdCollection, ddoCollection, ddoCollection2;
static Id ddoTarget, ddoView, ddoId;
static Id ddoTarget2, ddoView2, ddo2Id;
static Id generateTarget, generateView, generateId;
static Id ftpTarget, ftpView, ftpId;

EVENT_HANDLER homeroom_timeout (); 
EVENT_HANDLER homeroom_exit (); 
EVENT_HANDLER homeroom_workbnch (); 
EVENT_HANDLER homeroom_demo (); 
EVENT_HANDLER homeroom_ftp (); 
EVENT_HANDLER homeroom_codegen (); 
EVENT_HANDLER homeroom_idraw ();
EVENT_HANDLER homeroom_objbank ();
EVENT_HANDLER homeroom_host ();
EVENT_HANDLER homeroom_tae ();
EVENT_HANDLER demos_exit (); 
EVENT_HANDLER demos_demolist (); 
EVENT_HANDLER Homeicon_icon (); 
EVENT_HANDLER getname_name (); 
EVENT_HANDLER objects_objlist ();
EVENT_HANDLER objects_exit ();
EVENT_HANDLER Items_exit ();
EVENT_HANDLER dcficons_exit ();
EVENT_HANDLER miscicons_exit ();
EVENT_HANDLER threed_quit ();
EVENT_HANDLER hmrmicons_exit ();
EVENT_HANDLER icons_iconlist ();
EVENT_HANDLER icons_exit ();
EVENT_HANDLER ddo_quit ();
EVENT_HANDLER ddo2_quit ();
EVENT_HANDLER generate_ok ();
EVENT_HANDLER generate_close ();
EVENT_HANDLER ftp_close ();

/*	Display Id for use by direct Xlib calls: */
Display   *theDisplay;

/*	Dispatch Table: one per panel 	*/ 
typedef VOID (*FUNCTION_PTR) (); 
static struct DISPATCH  
    { 
    TEXT		*parmName; 
    FUNCTION_PTR 	eventFunction; 
    } ;
 
struct DISPATCH homeroomDispatch[] = {
  {"exit", homeroom_exit}, 
  {"workbnch", homeroom_workbnch}, 
  {"idraw", homeroom_idraw},
  {"demo", homeroom_demo}, 
  {"host", homeroom_host},
  {"ftp", homeroom_ftp}, 
  {"codegen", homeroom_codegen}, 
  {"objbank", homeroom_objbank},
  {"tae", homeroom_tae},
  {NULL, NULL}		/* terminator entry */ 
  }; 
 
struct DISPATCH demosDispatch[] = {
  {"exit", demos_exit}, 
  {"demolist", demos_demolist}, 
  {NULL, NULL}		/* terminator entry */ 
    }; 
 
struct DISPATCH HomeiconDispatch[] = {
  {"icon", Homeicon_icon}, 
  {NULL, NULL}		/* terminator entry */ 
    }; 
 
struct DISPATCH getnameDispatch[] = {
  {"name", getname_name}, 
  {NULL, NULL}		/* terminator entry */ 
    }; 

struct DISPATCH objectsDispatch[] = {
  {"objlist", objects_objlist},
  {"exit", objects_exit},
  {NULL, NULL}          /* terminator entry */
    };

struct DISPATCH ItemsDispatch[] = {
  {"quit", Items_exit},
  {NULL, NULL}          /* terminator entry */
    };

struct DISPATCH misciconsDispatch[] = {
  {"exit", miscicons_exit},
  {NULL, NULL}          /* terminator entry */
    };

struct DISPATCH dcficonsDispatch[] = {
  {"exit", dcficons_exit},
  {NULL, NULL}          /* terminator entry */
    };

struct DISPATCH hmrmiconsDispatch[] = {
  {"exit", hmrmicons_exit},
  {NULL, NULL}          /* terminator entry */
    };

struct DISPATCH threedDispatch[] = {
  {"quit", threed_quit},
  {NULL, NULL}          /* terminator entry */
    };

struct DISPATCH iconsDispatch[] = {
  {"iconlist", icons_iconlist},
  {"exit", icons_exit},
  {NULL, NULL}                /* terminator entry */
    };

struct DISPATCH ddoDispatch[] = {
  {"quit", ddo_quit},
  {NULL, NULL}          /* terminator entry */
    };

struct DISPATCH ddoDispatch2[] = {
  {"quit", ddo2_quit},
  {NULL, NULL}          /* terminator entry */
    };

struct DISPATCH generateDispatch[] = {
  {"ok", generate_ok},
  {"close", generate_close},
  {NULL, NULL}          /* terminator entry */
    };

struct DISPATCH ftpDispatch[] = {
  {"close", ftp_close},
  {NULL, NULL}          /* terminator entry */
    };

/* Other Global storage */
static int demosup=FALSE, objbankup=FALSE;
static int itemsup=FALSE, dcficonsup=FALSE, misciconsup=FALSE;
static int hmrmiconsup=FALSE, threedup=FALSE, iconsup=FALSE;
static int ddoup=FALSE, ddo2up=FALSE, generateup=FALSE, ftpup=FALSE;

void HidePanels();

/* Macro for hiding a panel from the display, without erasing it */
/* The "up" flag stays on because we want to restore "up" panels later */
#define HIDEPANEL(id, up) \
	if (id && up) Wpt_SetPanelState (id, WPT_INVISIBLE); 

/* Macro for redisplaying an "up" panel */
#define SHOWPANEL(id, up) \
	if (id && up) Wpt_SetPanelState (id, WPT_VISIBLE);

#define HIDEPANEL(id, up) \
	if (id && up) Wpt_SetPanelState (id, WPT_INVISIBLE); 

/* A variation on HIDEPANEL that also clear the "up" flag */
#define REMOVE(id, up) if(id) { \
	Wpt_SetPanelState(id, WPT_INVISIBLE); \
	up = FALSE; }


 
main (argc, argv) 
FUNINT	argc; 
TEXT	*argv[]; 
{ 
struct DISPATCH          *dp;           /* working dispatch pointer  */ 
IMPORT struct VARIABLE   *Vm_Find(); 
struct VARIABLE          *parmv;	/* pointer to event VARIABLE */ 
Id                       Vm_New(); 
Id                       Wpt_NewPanel (); 
WptPanelEvent            wptEvent;     /* event data		     */
CODE                     code; 
void            	 t_pinit ();
int             	 tlines, tcols, ttype;
 
    t_pinit (&tlines, &tcols, &ttype);
    f_force_lower (FALSE);	/* permit upper/lowercase file names */
    theDisplay  = Wpt_Init (NULL); 
    initializePanels (); 
 
    /*	main event loop */ 
    Wpt_SetTimeOut(5000);			/* 5 second timeout for clock */
    while (FOREVER) {
    	code = Wpt_NextEvent (&wptEvent);	/* get next panel event  */ 
    	if (code < 0)  
    	    continue; 
	else if (code == WPT_TIMEOUT_EVENT)
	    homeroom_timeout();
        else if (code == WPT_PARM_EVENT)
	    {
	    dp = (struct DISPATCH *) wptEvent.p_userContext;  
	    for (;  (*dp).parmName != NULL;   dp++)
	        if (strcmp ((*dp).parmName, wptEvent.parmName) == 0)  
		    { 
		    parmv = Vm_Find (wptEvent.p_dataVm, wptEvent.parmName); 
		    (*(*dp).eventFunction)  
			((*parmv).v_cvp, (*parmv).v_count); 
		    break; 
		    } 
	    }
        } 
} 

FUNCTION VOID initializePanels ()
{
Id  Co_Find ();
Id  Co_New ();
CODE Vm_GetIntg();
CODE Vm_SetIntg();
TAEINT size[2], origin[2];
COUNT count;

  vmCollection = Co_New (P_ABORT);
  Co_ReadFile (vmCollection, HOMEROOM_RES);
  homeroomView = Co_Find (vmCollection, "homeroom_v");
  homeroomTarget = Co_Find (vmCollection, "homeroom_t");
  demosView = Co_Find (vmCollection, "demos_v");
  demosTarget = Co_Find (vmCollection, "demos_t");
  HomeiconView = Co_Find (vmCollection, "Homeicon_v");
  HomeiconTarget = Co_Find (vmCollection, "Homeicon_t");
  getnameView = Co_Find (vmCollection, "getname_v");
  getnameTarget = Co_Find (vmCollection, "getname_t");
  objectsView = Co_Find (vmCollection, "objects_v");
  objectsTarget = Co_Find (vmCollection, "objects_t");
  ItemsView = Co_Find (vmCollection, "Items_vu");
  ItemsTarget = Co_Find (vmCollection, "Items_tar");
  dcficonsView = Co_Find (vmCollection, "dcficons_v");
  dcficonsTarget = Co_Find (vmCollection, "dcficons_t");
  misciconsView = Co_Find (vmCollection, "msicons_v");
  misciconsTarget = Co_Find (vmCollection, "msicons_t");
  hmrmiconsView = Co_Find (vmCollection, "hricons_v");
  hmrmiconsTarget = Co_Find (vmCollection, "hricons_t");
  threedView = Co_Find (vmCollection, "threed_v");
  threedTarget = Co_Find (vmCollection, "threed_t");
  iconsView = Co_Find (vmCollection, "icons_v");
  iconsTarget = Co_Find (vmCollection, "icons_t");
  generateView = Co_Find (vmCollection, "generate_v");
  generateTarget = Co_Find (vmCollection, "generate_t");
  ftpView = Co_Find (vmCollection, "ftp_v");
  ftpTarget = Co_Find (vmCollection, "ftp_t");

  /* Center the login panel on the display then show it */
  Vm_GetIntg (getnameView, "_panel.size", 2, size, &count);
					/* assume screen 0 for simplicity */
  origin[0] = (DisplayWidth(theDisplay, 0) - size[0])/2; 
  origin[1] = (DisplayHeight(theDisplay, 0) - size[1])/2;
  Vm_SetIntg(getnameView, "_panel.origin", 2, origin, P_UPDATE);
  getnameId = Wpt_NewPanel("",getnameTarget,getnameView,NULL,getnameDispatch,
	WPT_PREFERRED);
}
 
/*	event handlers */ 

/* Update the clock on the main homeroom panel */
EVENT_HANDLER homeroom_timeout ()
{
long now;
TAEINT hours, minutes;
struct tm *time_ptr;

  if (homeroomId)
    {
    time(&now);					/* current system time */
    time_ptr = (struct tm *)localtime(&now);
    hours = time_ptr->tm_hour;
    minutes = time_ptr->tm_min;
    Wpt_SetIntg(homeroomId, "hours", hours);
    Wpt_SetIntg(homeroomId, "minutes", minutes);
    }
}

EVENT_HANDLER homeroom_exit (value, count)
TEXT	*value[];	/* string pointers */ 
FUNINT	count;		/* number of strings */ 
{
  Wpt_PanelErase(homeroomId);
  XCloseDisplay (theDisplay);
  exit(0);
}

EVENT_HANDLER homeroom_workbnch (value, count)
TEXT	*value[];	/* string pointers */ 
FUNINT	count;		/* number of strings */ 
{
  HidePanels();
  Wpt_BeginWait (HomeiconId);
#ifdef VMS
  /* spawn/nowait doesn't work- why? */
  system("taewb");		/* System call to invoke workbench */
#else
  system("taewb&");		/* System call to invoke workbench */
#endif
  Wpt_EndWait (HomeiconId);
}

EVENT_HANDLER homeroom_codegen (value, count)
TEXT	*value[];	/* string pointers */ 
FUNINT	count;		/* number of strings */ 
{
  Wpt_BeginWait(homeroomId);
  if (!generateId)
    generateId = Wpt_NewPanel("", generateTarget, generateView, NULL,
			    generateDispatch, 0); 
  else
    Wpt_SetPanelState (generateId, WPT_VISIBLE);
  generateup = TRUE;
  Wpt_EndWait(homeroomId);
}

EVENT_HANDLER homeroom_demo (value, count)
TEXT	*value[];	/* string pointers */ 
FUNINT	count;		/* number of strings */ 
{
  TEXT *stringvec[1];
  BOOL statevec[1];

  Wpt_BeginWait(homeroomId);
  if (!demosId)
    {
    demosId = Wpt_NewPanel("", demosTarget, demosView, NULL, demosDispatch, 0); 
    /* Dim the Filter (Apply) button of file selection to prevent
     * directory traversal (since demos are in only one directory).
     */
    stringvec[0] = "Apply";
    statevec[0] = FALSE ; 
    Wpt_SetConstraintSensitivity (demosId, "demolist", 1, stringvec, statevec);
    }
  else
    Wpt_SetPanelState (demosId, WPT_VISIBLE);
  demosup = TRUE;
  Wpt_EndWait(homeroomId);
}

EVENT_HANDLER homeroom_ftp (value, count)
TAEINT	value[];	/* integer vector	*/
FUNINT	count;		/* number of integers	*/
{
  Wpt_BeginWait (homeroomId);
  if (!ftpId)
    ftpId = Wpt_NewPanel("", ftpTarget, ftpView, NULL, ftpDispatch, 0); 
  else
    Wpt_SetPanelState (ftpId, WPT_VISIBLE);
  ftpup = TRUE;
  Wpt_EndWait (homeroomId);
}

EVENT_HANDLER homeroom_idraw (value, count)
TEXT    *value[];       /* string pointers */
FUNINT  count;          /* number of strings */
{
  Wpt_BeginWait(homeroomId);
#ifdef VMS
  system("taeidraw");
#else
  system("taeidraw &");	/* System call */
#endif
  Wpt_EndWait(homeroomId);
}

EVENT_HANDLER homeroom_objbank (value, count)
TEXT    *value[];       /* string pointers */
FUNINT  count;          /* number of strings */
{
  Wpt_BeginWait(homeroomId);
  if (!objectsId)
   objectsId=Wpt_NewPanel("",objectsTarget,objectsView,NULL,objectsDispatch,0); 
  else
    Wpt_SetPanelState (objectsId, WPT_VISIBLE);
  objbankup = TRUE;
  Wpt_EndWait(homeroomId);
}

EVENT_HANDLER homeroom_host (value, count)
TEXT    *value[];       /* string pointers */
FUNINT  count;          /* number of strings */
{
  Wpt_BeginWait(homeroomId);
#ifdef VMS
  decterm("", ""); 		/* async decterm (no command, no term opts) */
#else
  system("xterm -sb&");
#endif
  Wpt_EndWait(homeroomId);
}

EVENT_HANDLER homeroom_tae (value, count)
TEXT    *value[];       /* string pointers */
FUNINT  count;          /* number of strings */
{
  Wpt_BeginWait(homeroomId);
#ifdef VMS
  decterm("taetm",
    "/window=(title=\"taetm\",icon=\"taetm\")"); /* DECterm running TM */
#else
  system("xterm -sb -e taetm&");
#endif
  Wpt_EndWait(homeroomId);
}

EVENT_HANDLER demos_exit (value, count)
TEXT	*value[];	/* string pointers */ 
FUNINT	count;		/* number of strings */ 
{
  Wpt_SetPanelState (demosId, WPT_INVISIBLE);
  demosup = FALSE;
}

EVENT_HANDLER demos_demolist (value, count)
TEXT	*value[];	/* string pointers */ 
FUNINT	count;		/* number of strings */ 
{
TEXT	startupCmd[STRINGSIZ+1];

	/* The selection value is the name of the proper startup file */
	/* Using File Selection dialog for the demo name, so 2 values */

    if ( count && s_equal (value[0], "Cancel") )
	{
        Wpt_SetPanelState (demosId, WPT_INVISIBLE);
        demosup = FALSE;
	}

    if ( count == 2 && s_equal (value[0], "OK") )
        {
        HidePanels();
        Wpt_BeginWait (HomeiconId);
#ifdef VMS
	Wpt_PanelMessage (homeroomId, "Not implemented for VMS.");
/* TBD: This needs to be ported.
 * FSB returns full pathname and previous code expected
 * only base filename. Need to parse value[1] to find the filename.
 * -kbs
 */
  	/* spawn/nowait doesn't work- why? */
    	/** sprintf(startupCmd, "run $TAEDEMOBIN:%s", value[1]); **/
#else
    	sprintf(startupCmd, "%s &", value[1]);
#endif
    	system(startupCmd);
    	Wpt_EndWait (HomeiconId);
        }
}

EVENT_HANDLER Homeicon_icon (value, count)
TEXT	*value[];	/* string pointers */ 
FUNINT	count;		/* number of strings */ 
{
  Wpt_SetPanelState (homeroomId, WPT_VISIBLE);

  Wpt_SetPanelState (HomeiconId, WPT_INVISIBLE);

  SHOWPANEL (demosId, demosup);
    
  SHOWPANEL (objectsId, objbankup);

  SHOWPANEL (ItemsId, itemsup);

  SHOWPANEL (dcficonsId, dcficonsup);

  SHOWPANEL (misciconsId, misciconsup);

  SHOWPANEL (hmrmiconsId, hmrmiconsup);

  SHOWPANEL (threedId, threedup);

  SHOWPANEL (iconsId, iconsup);

  SHOWPANEL (ddoId, ddoup);

  SHOWPANEL (ddo2Id, ddo2up);

  SHOWPANEL (generateId, generateup);

  SHOWPANEL (ftpId, ftpup);
}

EVENT_HANDLER getname_name (value, count)
TEXT	*value[];	/* string pointers */ 
FUNINT	count;		/* number of strings */ 
{
TEXT title[STRINGSIZ+1];
TEXT *title_ptr[1];
long now;
TAEINT hours[1], minutes[1];
struct tm *time_ptr;

  /* extract the current system time, update the target then display */
  /*   the homeroom main panel */
  Wpt_BeginWait (getnameId);
  time(&now);
  time_ptr = (struct tm *)localtime(&now);
  hours[0] = time_ptr->tm_hour;
  minutes[0] = time_ptr->tm_min;
  Vm_SetIntg(homeroomTarget, "hours", 1, hours, P_UPDATE);
  Vm_SetIntg(homeroomTarget, "minutes", 1, minutes, P_UPDATE);

  sprintf(title, "%s's Homeroom", value[0]);
  title_ptr[0] = title;
  Vm_SetString(homeroomView, "_panel", 1, title_ptr, P_UPDATE);
  homeroomId = Wpt_NewPanel("", homeroomTarget, homeroomView, NULL,
			    homeroomDispatch, 0);
  Wpt_PanelErase (getnameId);	
  getnameId = 0;	
}

EVENT_HANDLER objects_objlist (value, count)
TEXT    *value[];       /* string pointers */
FUNINT  count;          /* number of strings */
{
  Id  Co_Find ();
  Id  Co_New ();

  Wpt_BeginWait (objectsId);
  if (s_equal (value[0], "Basic TAE Items")) {
#ifdef VMS
        /* spawn/nowait doesn't work- why? */
        system("presdemo");		/* System call to invoke presdemo */
#else
        system("presdemo&");		/* System call to invoke presdemo */
#endif
        itemsup = TRUE;
   }
  else if (s_equal (value[0], "Icons")) {
    if (!iconsId)
      iconsId = Wpt_NewPanel("",iconsTarget,iconsView,NULL,iconsDispatch,0);  
    else
      Wpt_SetPanelState (iconsId, WPT_VISIBLE);
    iconsup = TRUE;
   }
  else if (s_equal (value[0], "Data Driven Objects")) {
	/* Load ddodemo's resource file */
   if (!ddoCollection)
	{
   	ddoCollection = Co_New (P_ABORT);
   	Co_ReadFile (ddoCollection, DDODEMO_RES);
   	ddoView = Co_Find (ddoCollection, "ddodemo_v");
   	ddoTarget = Co_Find (ddoCollection, "ddodemo_t");
	}
   if (!ddoId)
      ddoId = Wpt_NewPanel("",ddoTarget,ddoView,NULL,ddoDispatch,0);
   else
      Wpt_SetPanelState (ddoId, WPT_VISIBLE);	/* Turn it back On */
   ddoup = TRUE;
   }
  else if (s_equal (value[0], "Data Driven Objects 2")) {
	/* Load the secondary ddo example resource file */
   if (!ddoCollection2)
	{
   	ddoCollection2 = Co_New (P_ABORT);
   	Co_ReadFile (ddoCollection2, DDO_PALETTE_RES);
   	ddoView2 = Co_Find (ddoCollection2, "palette_v");
   	ddoTarget2 = Co_Find (ddoCollection2, "palette_t");
	}
   if (!ddo2Id)
      ddo2Id = Wpt_NewPanel("",ddoTarget2,ddoView2,NULL,ddoDispatch2,0);
   else
      Wpt_SetPanelState (ddo2Id, WPT_VISIBLE);	/* Turn it back On */
   ddo2up = TRUE;
   }
  Wpt_EndWait (objectsId);
}

/* Removes any panels brought up through the object bank */
EVENT_HANDLER objects_exit (value, count)
TAEINT  value[];       /* string pointers */
FUNINT  count;          /* number of strings */
{
  REMOVE (ItemsId, itemsup);
  REMOVE (dcficonsId, dcficonsup);
  REMOVE (misciconsId, misciconsup);
  REMOVE (hmrmiconsId, hmrmiconsup);
  REMOVE (threedId, threedup);
  REMOVE (iconsId, iconsup);
  REMOVE (objectsId, objbankup);
  REMOVE (ddoId, ddoup);
  REMOVE (ddo2Id, ddo2up);
  REMOVE (generateId, generateup);
}

EVENT_HANDLER Items_exit (value, count)
TAEINT  value[];        /* integer vector       */
FUNINT  count;          /* number of integers   */
{
  Wpt_SetPanelState (ItemsId, WPT_INVISIBLE);
  itemsup = FALSE;
}

EVENT_HANDLER dcficons_exit (value, count)
TAEINT  value[];        /* integer vector       */
FUNINT  count;          /* number of integers   */
{
  Wpt_SetPanelState (dcficonsId, WPT_INVISIBLE);
  dcficonsup = FALSE;
}

EVENT_HANDLER miscicons_exit (value, count)
TAEINT  value[];        /* integer vector       */
FUNINT  count;          /* number of integers   */
{
  Wpt_SetPanelState (misciconsId, WPT_INVISIBLE);
  misciconsup = FALSE;
}

EVENT_HANDLER threed_quit (value, count)
TAEINT  value[];        /* integer vector       */
FUNINT  count;          /* number of integers   */
{
  Wpt_SetPanelState (threedId, WPT_INVISIBLE);
  threedup = FALSE;
}

EVENT_HANDLER ddo_quit (value, count)
TAEINT  value[];        /* integer vector       */
FUNINT  count;          /* number of integers   */
{
  Wpt_SetPanelState (ddoId, WPT_INVISIBLE);       /* just hide it, takes too long to build */
  ddoup = FALSE;
}

EVENT_HANDLER ddo2_quit (value, count)
TAEINT  value[];        /* integer vector       */
FUNINT  count;          /* number of integers   */
{
  Wpt_SetPanelState (ddo2Id, WPT_INVISIBLE);       /* just hide it, takes too long to build */
  ddo2up = FALSE;
}

EVENT_HANDLER hmrmicons_exit (value, count)
TAEINT  value[];        /* integer vector       */
FUNINT  count;          /* number of integers   */
{
  Wpt_SetPanelState (hmrmiconsId, WPT_INVISIBLE);
  hmrmiconsup = FALSE;
}

EVENT_HANDLER icons_iconlist (value, count)
TEXT    *value[];       /* string pointers */
FUNINT  count;          /* number of strings */
{
  Wpt_BeginWait (iconsId);
  if (s_equal (value[0], "3D")) {  
    if (!threedId)
      threedId = Wpt_NewPanel("",threedTarget,threedView,NULL,threedDispatch,0);
    else if (!threedup)
      Wpt_SetPanelState (threedId, WPT_VISIBLE);
    threedup = TRUE;
   }
  else if (s_equal (value[0], "Homeroom")) {
    if (!hmrmiconsId)
      hmrmiconsId = Wpt_NewPanel ("", hmrmiconsTarget,
			hmrmiconsView, NULL, hmrmiconsDispatch, 0);
    else if (!hmrmiconsup)
      Wpt_SetPanelState (hmrmiconsId, WPT_VISIBLE);
    hmrmiconsup = TRUE;
   }
  else if (s_equal (value[0], "Pacor/EFECS")) {
    if (!dcficonsId)
      dcficonsId = Wpt_NewPanel ("", dcficonsTarget,
			dcficonsView, NULL, dcficonsDispatch, 0);
    else if (!dcficonsup)
      Wpt_SetPanelState (dcficonsId, WPT_VISIBLE);
    dcficonsup = TRUE;
   }
  else if (s_equal (value[0], "Miscellaneous")) {
    if (!misciconsId)
      misciconsId = Wpt_NewPanel ("", misciconsTarget,
			misciconsView, NULL, misciconsDispatch, 0);
    else if (!misciconsup)
      Wpt_SetPanelState (misciconsId, WPT_VISIBLE);
    misciconsup = TRUE;
   }
  Wpt_EndWait (iconsId);
}

EVENT_HANDLER icons_exit (value, count)
TAEINT  value[];        /* integer vector       */
FUNINT  count;          /* number of integers   */
{
  Wpt_SetPanelState (iconsId, WPT_INVISIBLE);
  iconsup = FALSE;
}

/* Build and execute a code generation command (taecg) */
EVENT_HANDLER generate_ok (value, count)
TAEINT	value[];	/* integer vector */
FUNINT	count;		/* number of integers */
{
  TEXT genCommand[5*STRINGSIZ+1];
  TEXT genOptions[STRINGSIZ];
  TEXT *language	= StringParm (generateTarget, "language");
  TEXT *codeStyle	= StringParm (generateTarget, "codestyle");
  TEXT *resfile		= StringParm (generateTarget, "resfilespec");
  TEXT *appfile		= StringParm (generateTarget, "appfilespec");
  TEXT *multiple	= "Multiple Files";
  TEXT *single		= "Single File";


/* make log file spec.  Same directory as application.
   Same name as resource file.  Extension = .?log, where
   where ? = c (C), a (ada), f (fortran), t (TCL).
*/
  TEXT logfile[FSPECSIZ];
  TEXT logfilename[FNAMESIZ];
  TEXT logfiledir[FLIBRSIZ];
  TEXT logfiletype[FTYPESIZ];
  TEXT errMsg[STRINGSIZ];
  struct FSBLOCK f;

  Wpt_BeginWait (generateId);
  if (NULLSTR(resfile) || NULLSTR(appfile))
    {
    Wpt_PanelMessage (generateId,
	"To correctly generate code, you must supply both a \n\
resource file name and an application specification.");
    Wpt_EndWait (generateId);
    return;
    }

/* remove the path and the extension to get the name of the logfile */
  f_name (resfile, logfilename);
  f_libr (appfile, logfiledir);
  s_copy ("?log", logfiletype);
  logfiletype[0] = tolower(language[0]);    /* .clog, .alog, .flog, .tlog */
  f_crack ("", logfiledir, logfilename, logfiletype, &f, errMsg);
  f_spec (&f, logfile);

  sprintf (genOptions, "\"-l %s -f %s",
             language, appfile);

  if (s_equal(language, "Ada") ||
      s_equal(language, "C++") ||
      s_equal(language, "C"))
      {
      if (s_equal(codeStyle, single))
          s_append (" -s single", genOptions);
      else if (s_equal(codeStyle, multiple))
          {
          s_append (" -s multiple", genOptions);
          }
      }
  else
      s_append (" -s single", genOptions);

  s_append ("\"", genOptions);

#ifdef VMS
  sprintf (genCommand, "@$taebin:wbgenerate %s %s %s",
    logfile, resfile, genOptions);
  decterm (genCommand,
    "/window=(title=\"wbgenerate\",icon=\"wbgenerate\",x_pos=1,y_pos=1,col=80)/little_font");
#else
  sprintf (genCommand,
#if defined(vax) || defined(MIPSEL)
    "dxterm -sb -sl 255 -name twb_generate -title taecg -e wbgenerate %s %s %s &",
#else
    "xterm -sb -sl 255 -name twb_generate -title taecg -e wbgenerate %s %s %s &",
#endif
    logfile, resfile, genOptions);
  system (genCommand);
#endif
  Wpt_EndWait(generateId);
/*  Wpt_PanelMessage (generateId, "Code Generation completed."); */
}

EVENT_HANDLER generate_close (value, count)
TAEINT	value[];	/* integer vector */
FUNINT	count;		/* number of integers */
{
  Wpt_SetPanelState (generateId, WPT_INVISIBLE);
  generateup = FALSE; 
}

EVENT_HANDLER ftp_close (value, count)
TAEINT	value[];	/* integer vector */
FUNINT	count;		/* number of integers */
{
  Wpt_SetPanelState (ftpId, WPT_INVISIBLE);
  ftpup = FALSE; 
}



FUNCTION void HidePanels()
{
  Wpt_SetPanelState (homeroomId, WPT_INVISIBLE);

  HIDEPANEL (demosId, demosup);

  HIDEPANEL (objectsId, objbankup);

  HIDEPANEL (ItemsId, itemsup);

  HIDEPANEL (dcficonsId, dcficonsup);

  HIDEPANEL (misciconsId, misciconsup);

  HIDEPANEL (hmrmiconsId, hmrmiconsup);

  HIDEPANEL (threedId, threedup);

  HIDEPANEL (iconsId, iconsup);

  HIDEPANEL (ddoId, ddoup);
 
  HIDEPANEL (ddo2Id, ddo2up);

  HIDEPANEL (generateId, generateup);

  HIDEPANEL (ftpId, ftpup);

  if (!HomeiconId)
      HomeiconId = Wpt_NewPanel("", HomeiconTarget, HomeiconView, NULL,
			    HomeiconDispatch, 0);
  else
      Wpt_SetPanelState (HomeiconId, WPT_VISIBLE);
}
