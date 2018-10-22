////////////////////////////////////////////////////////
// HelpSelfCmd.cc: Command used in a Help menu to refer to context-
// sensitive help on widget named in the constructor (usually itself).
////////////////////////////////////////////////////////
#include "HelpSelfCmd.h"
#include "HelpBrowser.h"

HelpSelfCmd::HelpSelfCmd(const char *name, int active, Widget base,
							const char *nameForHelp)
		: NoUndoCmd(name, active)
{
    _baseWidget = base;
    _nameForHelp = nameForHelp;		// Caller must not free this
}

void HelpSelfCmd::doit()
{

    // Find out what widget the user wants help on

    Widget w = XtNameToWidget(_baseWidget, (char *)_nameForHelp);

    // Now run the browser on this wiget.

    theHelpBrowser->run(w);
}

