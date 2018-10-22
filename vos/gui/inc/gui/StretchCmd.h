//////////////////////////////////////////////////////////////
// StretchCmd.h: This command applies LUT(s) to the image
//////////////////////////////////////////////////////////////
#ifndef STRETCHCMD
#define STRETCHCMD
#include "Cmd.h"

class Lut;
class SiHistogram;
class StretchValue;

class StretchCmd : public Cmd {

  protected:

    int _created;

    Lut *_lutR, *_lutG, *_lutB;
    SiHistogram *_histR, *_histG, *_histB;

    StretchValue *_currentValue;
    StretchValue *_undoValue;
    StretchValue *_redValue, *_grnValue, *_bluValue, *_allValue;

    void stretchOneBand(Lut *, StretchValue *, SiHistogram *,
                        double &, double &);

  public:

    StretchCmd(const char *, int, Lut *, SiHistogram *, 
	Lut *, SiHistogram *, Lut *, SiHistogram * ); // for three bands
    StretchCmd(const char *, int, Lut *, SiHistogram *);  // for one band only
    ~StretchCmd();

    virtual void doit();
    virtual void undoit();

    virtual void freeValue(CmdValue);

    Lut *getLutRed() { return _lutR; }
    Lut *getLutGrn() { return _lutG; }
    Lut *getLutBlu() { return _lutB; }
};
#endif
