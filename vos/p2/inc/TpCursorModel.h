////////////////////////////////////////////////////////////////
// TpCursorModel.h: Calls updates on cursor views such as position 
// view, dn view etc., when something cursor-related happens.
////////////////////////////////////////////////////////////////
#ifndef TPCURSORMODEL_H
#define TPCURSORMODEL_H
#include "CursorModel.h"

class TpCursorModel : public CursorModel {

  protected:

    int _n;
    Widget *_aiw;

  public:
  
    TpCursorModel(Boolean trackingEnabled, Widget iw, Widget *aiw, int n);
    virtual ~TpCursorModel();

    void getImageWidgets(Widget *&aiw, int &n);
};
#endif
