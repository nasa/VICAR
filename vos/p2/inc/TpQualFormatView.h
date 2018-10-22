////////////////////////////////////////////////////////////////
// TpQualFormatView.h: 
////////////////////////////////////////////////////////////////
#ifndef TPQUALFORMATVIEW_H
#define TPQUALFORMATVIEW_H
#include "UIComponent.h"
#include "TpQualifier.h"
#include "sl_lists.h"

class TpQualFormatCmdInterface;
class TpQualFormatSingleView;
class TpQualFormatValue;

class TpQualFormatView : public UIComponent {

  protected:

    TpQualFormatCmdInterface *_ci;
    TpQualFormatValue *_value;

    Widget _formatView;

    SL_List<TpQualFormatSingleView *> *_qualInfos;

  public:

    TpQualFormatView(Widget parent, const char *name, TpQualFormatCmdInterface *);
    virtual ~TpQualFormatView() { };

    void setQuals(TpQualFormatValue *);

    void setName(char *, int numQual);
    void setUnit(char *, int numQual);
    void setType(TpQualType, int numQual);

    virtual const char *const className() { return "TpQualFormatView"; }

};
#endif
