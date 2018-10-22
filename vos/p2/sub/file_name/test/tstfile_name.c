/**  Copyright (c) 1995, California Institute of Technology		**/
/**  U. S. Government sponsorship under NASA contract is acknowledged	**/

#include <stdio.h>
#include "file_name.h"
char	*file_names[] = {	"wms_gll:[ssi]whatta.img",
				"WMS_GLL:[ssi]whatta.img",
				"WMS_GLL_WORK:[ssi]boo.img",
				"WMS_VMS:[TEST.LOWER.CASE]CONVERSION.DAT",
				"/project/gll/blah/blah/blah/stuff.dat",
				"/project/LOWER/CASE/CONVERSION.EXP",
				"error:[.directory]file.dat",
				"[more.errors]file.dat",
				"/not/wms/file.dat",
				"[.invalid.catalog]valid.host",
				"CatalogNeeds.path",
				"~user/file/invalid",
				"/error/no/file/spec/",
				"error:[no.file.spec]",
				0 };
char	*paths_vms[] = {	"Disk_name:",		"[.subdirectory.list]",
				"full:[path.name]",	"just_disk",
				"[.partial",		"more:[partial",
				"bad:[.listing]",	"[need.disk.too]",
				"no:[files]allowed",	"[]",
				"/no/conversion/needed","WMS_GLL:[Case.test]",
				"WMS_MPF:[IMP]",	"/project/Change/Case",
				0 };
char	*paths_unix[] = {	"/root_level",		"/terminated/",
				"sub.directory.path",	"terminated.sub.path/",
				"/",			"~no/users/allowed",
				"no:[conversion.needed]","./",
				"/project/Change/WMS",
				0 };
main()
{ char	*input_name,
	*catalog_name,
	dummy_buf[1024],
	*host_name,
	*ptr;
  int	idx;

  for (idx=0; file_names[idx]; idx++)
  { input_name = file_names[idx];
    ptr = catalog_name = filename_for_catalog(input_name);
    if (catalog_name == NULL) catalog_name = "Error in file spec";
    host_name = filename_for_host(input_name);
    if (host_name == NULL) host_name = "Error in file spec";

    printf("\nInput: %s\n  Catalog: %s\n  Host: %s\n",
           input_name, catalog_name, host_name);
    if (ptr)
    { strcpy(dummy_buf,ptr);
      printf("  Basename: %s\n  Dirname: %s\n",
             unix_basename(dummy_buf),unix_dirname(ptr));
    }
  }

  printf("\n\nVMS to UNIX path test:\n");
  for (idx=0; paths_vms[idx]; idx++)
  { printf("   %s --> ",paths_vms[idx]);
    ptr = unix_path(paths_vms[idx]);
    if (ptr) printf("%s\n",ptr);
    else printf("Bad pathname\n");
  }

  printf("\n\nUNIX to VMS path test:\n");
  for (idx=0; paths_unix[idx]; idx++)
  { printf("   %s --> ",paths_unix[idx]);
    ptr = vms_path(paths_unix[idx]);
    if (ptr) printf("%s\n",ptr);
    else printf("Bad pathname\n");
  }
    
  printf("\n\n Vanna says \"Bye Bye\",  and so do I\n");
  exit(0);
}
