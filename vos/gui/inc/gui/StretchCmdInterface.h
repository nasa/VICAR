//////////////////////////////////////////////////////////////
// StretchCmdInterface.h: Command interface subclass that fills 
// StretchValue structure end calls execution on the command with 
// filled-in value.
//////////////////////////////////////////////////////////////
#ifndef STRETCHCMDINTERFACE
#define STRETCHCMDINTERFACE
#include "CmdInterface.h"
#include "StretchValue.h"
#include "Cmd.h"

class StretchParmInpView;
class StretchRadioBtn;
class StretchCheckBox;
class StretchParmListDialog;

const int Stretch_MAX_RADIOS = 13;
const int Stretch_MAX_CHECKS = 4;
const int Stretch_MAX_INP_VIEWS = 22;

class StretchCmdInterface : public CmdInterface {

  private:

    static XtResource _resources[];
    
  protected: 

    StretchValue *_stretchValue;

    StretchRadioBtn *_radioBtnList[Stretch_MAX_RADIOS];
    int _numRadios;

    StretchCheckBox *_checkBoxList[Stretch_MAX_CHECKS];
    int _numChecks;

    StretchParmInpView *_inpViewList[Stretch_MAX_INP_VIEWS];
    int _numInpViews;

    StretchParmListDialog *_itableListDialog;
    StretchParmListDialog *_tableListDialog;
    StretchParmListDialog *_alarmListDialog;

    CmdInterface *_itableValueList;

    // Obtained from resource file

    int _alarmValue;
    char *_selectedParmColor, *_unselectedParmColor;

  public:

    StretchCmdInterface(Widget, Cmd *);
    virtual ~StretchCmdInterface() { }

    void stretchIt(StretchValue *);
    void stretchIt();

    virtual void setValue(CmdValue);
    virtual void setSensitivity(StretchType);

    // Operations on saved stretch value

    void setBand(StretchBand band) 
	{ _stretchValue->band = band;}
    void setType(StretchType name) 
	{ _stretchValue->stretchName = name;}
    void setLow(double v) 
	{ _stretchValue->low = v;}
    void setHigh(double v) 
	{ _stretchValue->high = v;}
    void setMin(double v) 
	{ _stretchValue->dnmin = v;}
    void setMax(double v) 
	{ _stretchValue->dnmax = v;}
    void setDnValue(int v) 
	{ _stretchValue->dnValue = v;}
    void setNumBits(int v) 
	{ _stretchValue->nbits = v;}
    void setCurve(double v) 
	{ _stretchValue->curve = v;}
    void setInterval(int v) 
	{ _stretchValue->interval = v;}
    void setMaxNum(int v) 
	{ _stretchValue->maxnum = v;}
    void setLowPerc(double v) 
	{ _stretchValue->lPerc = v;}
    void setHighPerc(double v) 
	{ _stretchValue->hPerc = v;}
    void setStretched(Boolean v) 
	{ _stretchValue->stretched = v;}
    void setTable(int *inTable, int *outTable, int size) 
	{ _stretchValue->inTable = inTable;
	  _stretchValue->outTable = outTable;
	  _stretchValue->tableNoVals = size; }
    void setITable(int *inTable, int *outTable, int size) 
	{ _stretchValue->inITable = inTable;
          _stretchValue->outITable = outTable;
          _stretchValue->itableNoVals = size; }
    void setAlarmTable(int *alarmTable, int size) 
	{ _stretchValue->alarmValues = alarmTable;
	  _stretchValue->alarmNoVals = size; }
    void setBackground(int v) 
	{ _stretchValue->backgnd = v; }
    void setFunction(char *f) 
	{ _stretchValue->func = f; }
    void setMean(double v) 
	{ _stretchValue->mean = v; }
    void setPMean(double v) 
	{ _stretchValue->pmean = v; }
    void setAmpl(double v) 
	{ _stretchValue->ampl = v; }
    void setFreq(double v) 
	{ _stretchValue->freq = v; }
    void setPhi(double v) 
	{ _stretchValue->phi = v; }
    void setRange(int v) 
	{ _stretchValue->range = v; }
    void setFactor(double v) 
	{ _stretchValue->factor = v; }
    void setPercent(double v) 
	{ _stretchValue->percent = v; }
    void setLowPercent(double v) 
	{ _stretchValue->lpercent = v; }
    void setHighPercent(double v) 
	{ _stretchValue->hpercent = v; }
    void setGsigma(double v) 
	{ _stretchValue->gsigma = v; }
    void setGmean(double v) 
	{ _stretchValue->gmean = v; }
    void setTableOn(Boolean on) 
	{ _stretchValue->table = on; }
    void setAlarmOn(Boolean on) 
	{ _stretchValue->alarm = on; }
    void setComplOn(Boolean on) 
	{ _stretchValue->comple = on; }
    void setOffOn(Boolean on)
	{ _stretchValue->off = on; }
};
#endif
