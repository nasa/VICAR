///////////////////////////////////////////////////////////////
// TpQualifier.h:
///////////////////////////////////////////////////////////////
#ifndef TPQUALIFIER_H
#define TPQUALIFIER_H
#include <iostream>

typedef enum { TpText, TpReal, TpFull } TpQualType;

union TpValue {
    int f;
    float r;
    char *t;
};

class TpQualifier {

  protected:

    const TpQualType _type;
    TpValue _value;

  friend std::ostream &operator<<(std::ostream &ostr, const TpQualifier &q);

  public:

    TpQualifier(TpQualType type);
    ~TpQualifier();

    TpQualType getType() const { return _type; }

    void setValue(char *value);

    void getValue(int &value) const;
    void getValue(float &value) const;
    void getValue(char *&value) const;

    char *valueToString() const;

};
#endif
