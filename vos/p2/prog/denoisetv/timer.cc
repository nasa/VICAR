#ifdef _MSC_VER		// Accomodate DOS file descriptor
#include <sys/timeb.h>
#else
#include <sys/timeb.h>
#endif

// The difference in the returned times gives 
// the time of exectution of the code in milli-seconds.

double realtime(void)
{
	struct timeb tp;
	ftime(&tp);
	return((double)(tp.time)*1000+(double)(tp.millitm));
}
