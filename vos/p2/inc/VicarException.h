/*
       Copyright 2011-Present
       California Institute of Technology. 
       ALL RIGHTS RESERVED.
       U.S. Government Sponsorship acknowledged. 09-08-2011.

       Auther : Rajesh R. Patel

History :
---------

Date              Who                        What
-------------------------------------------------------------------------------
09/08/2011        Raj       Initial Release

 */
#ifndef __VICAR_EXCEPTION_H__
#define __VICAR_EXCEPTION_H__

#include <exception>
#include <string>

//using namespace std;

namespace jpl
{
namespace mipl
{
namespace p2
{
class VicarException : public std::exception
{
private:
	std::string what_;
public:
	VicarException(std::string what) : what_(what) {}
	virtual ~VicarException() throw () {}
	const char* what() const throw () { return this->what_.c_str(); }
};
}
}
}
#endif
