/////////////////////////////////////////////////////////////
// SiHistSpikeCmd.h: 
/////////////////////////////////////////////////////////////
#ifndef SiHistSPIKECMD_H
#define SiHistSPIKECMD_H
#include "Cmd.h"

class SiHistBox;

class SiHistSpikeCmd : public Cmd {

  protected:

    int _oldValue;
    SiHistBox *_box;

    virtual void doit();
    virtual void undoit();

  public:

    SiHistSpikeCmd ( const char *, int, SiHistBox * );
    ~SiHistSpikeCmd() { }

    virtual const char *const className () { return "SiHistSpikeCmd"; }
};
#endif
