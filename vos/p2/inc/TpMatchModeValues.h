/////////////////////////////////////////////////////////////////////////////
// TpMatchModeValues.h: A structure that holds match mode parameters
/////////////////////////////////////////////////////////////////////////////
#ifndef TPMATCHMODEVALUES_H
#define TPMATCHMODEVALUES_H

class TpMatchModeValues {
  public:
    int _pmk;          // Product Momentum Correlation matching patch size
    int _lsm;          // Least Squares Momentum matching patch size
    int _searchWindow; // Search Area 
    float _accuracy;   // Point Accuracy
    float _threshold;    // Threshold

    TpMatchModeValues(int pmk = 7, int lsm = 15, int sw = 20, 
		      float acc = 0.3, float threshold = 0.5) { 
	_pmk = pmk; 
	_lsm = lsm; 
	_searchWindow = sw; 
	_accuracy = acc; 
	_threshold = threshold; 
    }
};

#endif
