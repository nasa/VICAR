class TpContrastValue {

  protected:

    int _min, _max;

  public:

    TpContrastValue(int min, int max) { _min = min; _max = max; }
    TpContrastValue(TpContrastValue &value) 
	{ _min = value.getMin(); _max = value.getMax(); }

    int getMin() { return _min; }
    int getMax() { return _max; }

    void setMin(int min) { _min = min; }
    void setMax(int max) { _max = max; }

};
