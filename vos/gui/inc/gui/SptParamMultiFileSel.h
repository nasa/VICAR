////////////////////////////////////////////////////////////////////////
// SptParamMultiFileSel - Parameter class for a multiple file selection
// dialog (like on xvd).  Filename may come from user or from dataflow.
// When a new filename arrives, the given command is executed if it is
// non-NULL.  This command receives a dynamically-allocated filename string.
////////////////////////////////////////////////////////////////////////
#ifndef SPTPARAMMULTIFILESEL_H
#define SPTPARAMMULTIFILESEL_H

#include "SptInputParamBase.h"

class MainWindow;

class SptParamMultiFileSel : public SptInputParamBase {
 private:

 protected:

   MainWindow *_mainWindow;		// So FileSelBox can pop down window

   virtual void doLayout(Widget parent, char *name);

   virtual void createDirectView(Widget parent);
   virtual void createShowValueCmd();
   virtual void createModeMenu(Widget parent, char *name);

   SptParamMultiFileSel(char *name, Cmd *cmd=NULL, DA_ParamID param=NULL_ID,
			MainWindow *mainWindow=NULL)
	: SptInputParamBase(name, FALSE, cmd, param)
		{ _mainWindow = mainWindow; }
 public:

   static SptParamMultiFileSel *create(Widget parent, char *name,
		Cmd *cmd = NULL, DA_ParamID param = NULL_ID,
		MainWindow *mainWindow = NULL)
	{  SptParamMultiFileSel *ptr=new SptParamMultiFileSel(name, cmd, param,
								mainWindow);
	   if (ptr) ptr->initialize(parent, ParamModeDirect);
	   return ptr;
	}
};

#endif
