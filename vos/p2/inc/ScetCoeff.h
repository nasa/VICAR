/***********************************************************************
* ScetCoeff.h     
* Include file for use with sclk2scet & scet2sclk programs.
* History:
*    March 29, 2001 (Hyun H. Lee)
*       Added get_values_with_scet(...) method. 
*       Renamed get_values(...) to get_values_with_sclk(...).  
***********************************************************************/

#ifndef _SCETCOEFF_
#define _SCETCOEFF_
#include <iostream>
#include <iomanip>
#include <strings.h>
#include <fstream>
#include <errno.h>

class ScetCoeff {
private:
        double *sclk0;
        double *sclkrate;
        char **scet0;
        int num;
public:
        // constructors
        ScetCoeff() : sclk0(0), sclkrate(0), scet0(0), num(0) {}
        ScetCoeff(char *filename) { load_coeff(filename); }

        // function to load coefficient file 
        int load_coeff(char *filename);

        // function to get the values with sclk as input 
        // input is non-converted sclk0; return 0 on success -1 on failure
        int get_values_with_sclk(double insclk0, double &sclk0, double &rate, 
			  	 char *scet0);
  
        // function to get the values with scet as input
        // input is scet_in; return 0 on success -1 on failure
        int get_values_with_scet(char *scet_in, double &sclk0, double &rate,
                                 char *scet0);
	void reset();

        // destructor
        ~ScetCoeff() { reset(); }
};

#endif
