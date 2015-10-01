////////////////////////////////////////////////////////
// HelpBrowser.cc: Actually run the help browser.
// If the browser supports Mosaic- or Netscape-style
// remote-control, and we've already started one, then
// don't start another but instead re-use the existing
// window.  If remote-control is not supported,
// we always start a new browser. This is a global object,
// accessed via theHelpBrowser.
////////////////////////////////////////////////////////

#include "HelpBrowser.h"
#include "Application.h"
#include <iostream>
#include <Xm/RepType.h>

#ifdef __VMS	// VMS-specific code
extern "C" {
#include <descrip.h>
#include <clidef.h>
extern int lib$spawn(struct dsc$descriptor_s *, int, int, int *);
}
#else		// Unix-specific code
#include <stdlib.h>
#include <sys/types.h>
#include <signal.h>
#include <unistd.h>
#include <sys/wait.h>
#include <stdio.h>
#endif

///////////////////////////////////////////////////////
// Initialize the init variable
///////////////////////////////////////////////////////

int HelpBrowser::_helpBrowserInit = 0;

////////////////////////////////////////////////////////
// Declare a global object: theHelpBrowser
////////////////////////////////////////////////////////

HelpBrowser *theHelpBrowser = new HelpBrowser();

////////////////////////////////////////////////////////
// Resources for this class.  They apply to any and all widgets that have help
////////////////////////////////////////////////////////

XtResource HelpBrowser::_resources[] = {
  {
    (char *)"helpBrowserCommand",
    (char *)"HelpBrowserCommand",
    XmRString,
    sizeof(String),
    XtOffsetOf(HelpBrowser, _helpBrowserCommand),
    XmRImmediate,
    (XtPointer) NULL,
  },
  {
    (char *)"helpBrowserHasRemoteControl",
    (char *)"HelpBrowserHasRemoteControl",
    XmRBoolean,
    sizeof(Boolean),
    XtOffsetOf(HelpBrowser, _helpBrowserHasRemoteControl),
    XmRImmediate,
    (XtPointer) True,
  },
  {
    (char *)"helpBrowserControlStyle",
    (char *)"HelpBrowserControlStyle",
    (char *)"HelpBrowserControlStyleType",
    sizeof(HelpBrowserControlStyleType),
    XtOffsetOf(HelpBrowser, _helpBrowserControlStyle),
    XmRImmediate,
    (XtPointer) (int)DEFAULT_BROWSER_HELP,
  }, 
  {
    (char *)"helpBaseLocation",
    (char *)"HelpBaseLocation",
    XmRString,
    sizeof(String),
    XtOffsetOf(HelpBrowser, _helpBaseLocation),
    XmRImmediate,
    (XtPointer) NULL,
  },
  {
    (char *)"helpLocation",
    (char *)"HelpLocation",
    XmRString,
    sizeof(String),
    XtOffsetOf(HelpBrowser, _helpLocation),
    XmRImmediate,
    (XtPointer) NULL,
  },
};

////////////////////////////////////////////////////////
// Constructor
////////////////////////////////////////////////////////

HelpBrowser::HelpBrowser()
{
    if (!_helpBrowserInit) {   // one-time initialization
        XmRepTypeId id;
        static String ControlStyleNames[] = { (char *)"netscape",
				(char *)"mosaic", (char *)"default" };

        static HelpBrowserControlStyleType ControlStyleValues[] =
	                                              { NETSCAPE_HELP,
							MOSAIC_HELP,
							DEFAULT_BROWSER_HELP };

        id = XmRepTypeRegister((char *)"HelpBrowserControlStyleType",
				ControlStyleNames, 
			        ControlStyleValues,
				XtNumber(ControlStyleNames) );
        XmRepTypeAddReverse(id);
        _helpBrowserInit = 1;
  } 

    _helpBrowserCommand = NULL;
    _helpBaseLocation = NULL;
    _helpLocation = NULL;
    _helpBrowserHasRemoteControl = True;
    _helpBrowserControlStyle = DEFAULT_BROWSER_HELP;

    _browserPid = 0;
    _currentBrowserCmd = NULL;
    
}

////////////////////////////////////////////////////////
// Given a widget, get its help resources and start the browser.
////////////////////////////////////////////////////////
void HelpBrowser::run(Widget w)
{
    // Get the URL of the help, and the browser to use, from the
    // resource database.

    XtGetApplicationResources(w, (XtPointer)this,
			_resources, XtNumber(_resources), NULL, 0);

    if (_helpBrowserCommand == NULL || strlen(_helpBrowserCommand) == 0) {
        std::cerr << "No help browser available!\n";
        return;
    }
    if (_helpBaseLocation == NULL || strlen(_helpBaseLocation) == 0)
        return;
    if (_helpLocation == NULL || strlen(_helpLocation) == 0)
        return;

    run(_helpBrowserCommand, _helpBaseLocation, _helpLocation,
	_helpBrowserHasRemoteControl, _helpBrowserControlStyle);
}

////////////////////////////////////////////////////////
// Given a cmd, base, loc, and controlStyle, start Browser
////////////////////////////////////////////////////////

void HelpBrowser::run(const char *cmd, const char *base, const char *loc,
		      Boolean hasRemote, 
		      HelpBrowserControlStyleType controlStyle)
		      // hasRemote = True; controlStyle = DEFAULT_BROWSER_HELP
{
    char browseString[256];

    strcpy(browseString, base);
    strcat(browseString, loc);

    run(cmd, browseString, hasRemote, controlStyle);
}

////////////////////////////////////////////////////////
// Given a URL, start Browser
////////////////////////////////////////////////////////

void HelpBrowser::run(const char *url)
{
    Widget w = theApplication->baseWidget();

    // Get the browser to use, from the
    // resource database.

    XtGetApplicationResources(w, (XtPointer)this,
			_resources, XtNumber(_resources), NULL, 0);

    if (_helpBrowserCommand == NULL || strlen(_helpBrowserCommand) == 0) {
        std::cerr << "No help browser available!\n";
        return;
    }

    run(_helpBrowserCommand, url, _helpBrowserHasRemoteControl,
	_helpBrowserControlStyle);
}

////////////////////////////////////////////////////////
// Given the help resources, actually start the browser.  This is
// system-specific code.
////////////////////////////////////////////////////////

#ifdef __VMS
////////////////////////////////////////////////////////
// VMS version
////////////////////////////////////////////////////////
void HelpBrowser::run(const char *cmd, const char *url, Boolean hasRemote,
		      HelpBrowserControlStyleType controlStyle)
  // hasRemote = True; controlStyle = DEFAULT_BROWSER_HELP
  // controlStyle is not actually used in the VMS version
{
    char cmdString[256];
    strcpy(cmdString, cmd);
    strcat(cmdString, " \"");
    strcat(cmdString, url);
    strcat(cmdString, "\"");

    $DESCRIPTOR(cmd_desc, cmdString);
    cmd_desc.dsc$w_length = strlen(cmdString);

    int flags = CLI$M_NOWAIT;

    // This version does not support killBrowser()
    lib$spawn(&cmd_desc, 0, 0, &flags);

    // This one may (see below)
    // lib$spawn(&cmd_desc, 0, 0, &flags, , &_browserPID);
}

////////////////////////////////////////////////////////

void HelpBrowser::killBrowser()
{
    // NOT YET IMPLEMENTED
    // Note: under VMS the browser is implemented as a subprocess.
    // Subprocesses automatically terminate when the spawning process
    // goes away... so there is no need for this code in the normal
    // case (cleaning up on program termination).  However, it might be
    // useful if one wanted to kill the browser early for some reason.
 
    // The following needs to be done to implement this as a VAX
    // function:
    // 1)  Swap the lib$spawn commands above
    //     i.e make active:
    //         lib$spawn(&cmd_desc, 0, 0, &flags,,&_browserPID);
    // 2)  Build the stop command and issue it via lib$spawn
    //      - the following commented code might do the job but is
    //        untested.
    //   char tmp_buf[80];
    //   sprintf (tmp_buf, "STOP /ID=%i", _browserPID);
    //   $DESCRIPTOR(cmd_desc, tmp_buf);
    //   cmd_desc.dsc$w_length = strlen(tmp_buf);
    //   int flags = CLI$M_NOWAIT;
    //   lib$spawn(&cmd_desc, 0, 0, &flags);
    //
    // There's probably also a way to directly kill a process given its PID
    // which would be easier than spawning off a STOP command.
}

#else
////////////////////////////////////////////////////////
// Unix version 
////////////////////////////////////////////////////////

void HelpBrowser::run(const char *cmd, const char *url, Boolean hasRemote,
		      HelpBrowserControlStyleType controlStyle)
		// hasRemote = True; controlStyle = DEFAULT_BROWSER_HELP 
{
    char filename[100];
  
    if (controlStyle == DEFAULT_BROWSER_HELP) {

	if (strstr(cmd, "netscape"))	// "netscape" is in the command
	    controlStyle = NETSCAPE_HELP;

	else				// assume it's mosaic
	    controlStyle = MOSAIC_HELP;

    } 
  
    if (controlStyle == NETSCAPE_HELP) {	// netscape browser

        char *netscapeCmd = new char[strlen(cmd) + strlen(url) + 25];
    
        if (!hasRemote) {	// if no remote ctrl, start a new netscape
            sprintf(netscapeCmd, "%s %s &", cmd, url);
            system(netscapeCmd);
        }
        else {
            sprintf(netscapeCmd, "%s -remote 'openURL(%s)'", cmd, url);
            int netscapeStatus;
            netscapeStatus = system(netscapeCmd);
      
            if ( netscapeStatus != 0 ) {  // no netscape, so start a new one
	        sprintf(netscapeCmd, "%s %s &", cmd, url);
	        system(netscapeCmd);
            }
        }
	delete[] netscapeCmd;
    }		// end netscape code

    else {		// mosaic browser
        if (_browserPid) {              // Check to see if it's still running
            if (waitpid(_browserPid, NULL, WNOHANG) == _browserPid) {
                _browserPid = 0;          // it's dead
            }
        }

    // Start a new browser if we don't currently have one running, or if
    // we don't support remote-control, or if the name of the browser command
    // has changed.

	if (!_browserPid || !hasRemote ||
		(_currentBrowserCmd && strcmp(_currentBrowserCmd, cmd) != 0)) {
	    if (!(_browserPid = (int)fork())) {
		execlp(cmd, cmd, url, NULL); // Spawn the browser process
		_exit(1);
            }
        }
        else {				// Remote-control the existing one
	    sprintf(filename, "/tmp/Mosaic.%d", _browserPid);
            FILE *file = fopen(filename, "w");
            if (file == NULL) {
	        std::cerr << "Error opening Mosaic remote-control file: '"
			  << url << "'\n";
            }
            else {
	        fprintf(file, "goto\n");
                fprintf(file, "%s\n", url);
                fclose(file);
                kill(_browserPid, SIGUSR1);
            }
        }

        // Save the name of the browser in case it changes

        if (_currentBrowserCmd == NULL ||
	                       (strcmp(_currentBrowserCmd, cmd) != 0)) {
            if (_currentBrowserCmd)
	        delete []_currentBrowserCmd;
            _currentBrowserCmd = new char[strlen(cmd)+1];
            strcpy(_currentBrowserCmd, cmd);
        }
    }
}

////////////////////////////////////////////////////////
void HelpBrowser::killBrowser()
{
    if (_browserPid) {			// Check to see if it's still running
        if (waitpid(_browserPid, NULL, WNOHANG) == _browserPid) {
            _browserPid = 0;		// it's dead
        }
        else {
            kill(_browserPid, SIGKILL);
            _browserPid = 0;		// dead now!
        }
    }
}

#endif /* VMS/Unix */

