////////////////////////////////////////////////////////
// SgKeyinCmdInterface:  Command interface to get string value for cmd.
// Initial values come from Cmd object.  This class is not specific
// to the sage client library and can be reused if needed.
////////////////////////////////////////////////////////
#ifndef SGKEYINCMDINTERFACE_H
#define SGKEYINCMDINTERFACE_H
#include "CmdInterface.h"

class KeyinView;

class SgKeyinCmdInterface : public CmdInterface {

 protected: 

   static String _defaults[];

   KeyinView *_keyinView;

 public:

   SgKeyinCmdInterface(Widget, Cmd *);
   virtual ~SgKeyinCmdInterface();

   virtual void executeCmd(XtPointer);

   virtual void setValue(CmdValue);
};
#endif
