#include "VIDSdefs.h"

extern struct PARBLK InParblk;

int doVIDS(env)
    
    struct VIDSEnvironment	*env;		/* the VIDS environment		*/
    
{
  struct VARIABLE *v, *p_find();
  char *cmdstr, *substr;
  int i, status;
  int jbw_do(),jcolor_do(),jdisp_do(),jerase_do(),jgrab_do(),jdrop_do();
  int jgraphics_do(),jon_do(),joff_do(),jgrdisp_do();
  int jpan_do(),jzoom_do(),jhist_do(),jprofile_do();
  int jdtf_do(),jpseudo_do(),jpsedit_do(),jpscopy_do();
  int jwedge_do(),jdraw_do(),jtext_do(),jlist_do();
  int jdef_region_do(),jdef_plane_do();
  int jset_range_do(),jset_message_do(),jset_cursor_do(),jset_size_do();
  int jset_color_do(),jset_text_do();
  int jload_image_do(),jload_stretch_do(),jload_pseudo_do();
  int jsave_image_do(),jsave_stretch_do(),jsave_pseudo_do();
  int jmovie_set_do(),jmovie_load_do(),jmovie_run_do(),jmovie_show_do();
  int jget_cursor_do(),jget_stat_do(),jget_planes_do(),jget_translate_do();
  int jget_device_do(),jget_plinfo_do(),jget_region_do(),jget_rgnlist_do();
  int jget_devlist_do();
  int jstretch_linear_do(),jstretch_percent_do(),jstretch_copy_do();
  int jstretch_gauss_do(),jstretch_comp_do(),jstretch_smooth_do();
  int jstretch_shift_do(),jstretch_period_do(),jstretch_alarm_do();
  int jstretch_table_do(),jstretch_cursor_do(),jstretch_func_do();
  int jstretch_contour_do(),jstretch_log_do();
  static struct
  {
    char		*cmd;		/* Command name -- must be upper case	*/
    char		*subcmd;	/* Subcommand name--must be upper case	*/
    int			AllocRequired;	/* Command requires device allocated	*/
    int			(*cproc)();	/* Command processing routine		*/
  } commands[] =
  {
/* Add new commands to the list below.  Make sure that command and subcommand names	*/
/* are in upper case.  If there is no subcommand, put a NULL in that slot.  Try to	*/
/* keep the commands in alphabetical order for ease of look-up.				*/

/*  Command name	subcommand name		AllocRequired	routine address	*/
    "JBW",		"",			TRUE,		jbw_do,
    "JCOLOR",		"",			TRUE,		jcolor_do,
    "JDEF",		"PLANE",		TRUE,		jdef_plane_do,
    "JDEF",		"REGION",		TRUE,		jdef_region_do,
    "JDISP",		"",			TRUE,		jdisp_do,
    "JDRAW",		"",			TRUE,		jdraw_do,
    "JDROP",		"",			FALSE,		jdrop_do,
    "JDTF",		"",			TRUE,		jdtf_do,
    "JERASE",		"",			TRUE,		jerase_do,
    "JGET",		"CURSOR",		TRUE,		jget_cursor_do,
    "JGET",		"DEVICE",		TRUE,		jget_device_do,
    "JGET",		"DEVLIST",		FALSE,		jget_devlist_do,
    "JGET",		"PLANES",		TRUE,		jget_planes_do,
    "JGET",		"PLINFO",		TRUE,		jget_plinfo_do,
    "JGET",		"REGION",		TRUE,		jget_region_do,
    "JGET",		"RGNLIST",		TRUE,		jget_rgnlist_do,
    "JGET",		"STATS",		TRUE,		jget_stat_do,
    "JGET",		"TRANSLATE",		TRUE,		jget_translate_do,
    "JGRAB",		"",			FALSE,		jgrab_do,
    "JGRAPHICS",	"",			TRUE,		jgraphics_do,
    "JGRDISP",		"",			TRUE,		jgrdisp_do,
    "JHIST",		"",			TRUE,		jhist_do,
    "JLIST",		"",			TRUE,		jlist_do,
    "JLOAD",		"IMAGE",		TRUE,		jload_image_do,
    "JLOAD",		"STRETCH",		TRUE,		jload_stretch_do,
    "JLOAD",		"PSEUDO",		TRUE,		jload_pseudo_do,
    "JMOVIE",		"FRAMELOAD",		TRUE,		jmovie_load_do,
    "JMOVIE",		"RUN",			TRUE,		jmovie_run_do,
    "JMOVIE",		"SET",			TRUE,		jmovie_set_do,
    "JMOVIE",		"SHOW",			TRUE,		jmovie_show_do,
    "JON",		"",			TRUE,		jon_do,
    "JOFF",		"",			TRUE,		joff_do,
    "JPAN",		"",			TRUE,		jpan_do,
    "JPROFILE",		"",			TRUE,		jprofile_do,
    "JPSCOPY",		"",			TRUE,		jpscopy_do,
    "JPSEDIT",		"",			TRUE,		jpsedit_do,
    "JPSEUDO",		"",			TRUE,		jpseudo_do,
    "JSAVE",		"IMAGE",		TRUE,		jsave_image_do,
    "JSAVE",		"STRETCH",		TRUE,		jsave_stretch_do,
    "JSAVE",		"PSEUDO",		TRUE,		jsave_pseudo_do,
    "JSET",		"COLOR",		TRUE,		jset_color_do,
    "JSET",		"CURSOR",		TRUE,		jset_cursor_do,
    "JSET",		"MESSAGE",		FALSE,		jset_message_do,
    "JSET",		"RANGE",		TRUE,		jset_range_do,
    "JSET",		"SIZE",			TRUE,		jset_size_do,
    "JSET",		"TEXT",			FALSE,		jset_text_do,
    "JSTRETCH",		"ALARM",		TRUE,		jstretch_alarm_do,
    "JSTRETCH",		"COMPLEMENT",		TRUE,		jstretch_comp_do,
    "JSTRETCH",		"CONTOUR",		TRUE,		jstretch_contour_do,
    "JSTRETCH",		"COPY",			TRUE,		jstretch_copy_do,
    "JSTRETCH",		"CURSOR",		TRUE,		jstretch_cursor_do,
    "JSTRETCH",		"EDIT",			TRUE,		jdtf_do,
    "JSTRETCH",		"FUNCTION",		TRUE,		jstretch_func_do,
    "JSTRETCH",		"GAUSS",		TRUE,		jstretch_gauss_do,
    "JSTRETCH",		"LINEAR",		TRUE,		jstretch_linear_do,
    "JSTRETCH",		"LOGARITHM",		TRUE,		jstretch_log_do,
    "JSTRETCH",		"PERCENT",		TRUE,		jstretch_percent_do,
    "JSTRETCH",		"PERIODIC",		TRUE,		jstretch_period_do,
    "JSTRETCH",		"SHIFT",		TRUE,		jstretch_shift_do,
    "JSTRETCH",		"SHOW",			TRUE,		jdtf_do,
    "JSTRETCH",		"SMOOTH",		TRUE,		jstretch_smooth_do,
    "JSTRETCH",		"TABLE",		TRUE,		jstretch_table_do,
    "JTEXT",		"",			TRUE,		jtext_do,
    "JWEDGE",		"",			TRUE,		jwedge_do,
    "JZOOM",		"",			TRUE,		jzoom_do,
    ""		/* Null terminator, **REQUIRED** */
  };
    
  v = p_find (&InParblk, "_PROC");
  if (v == NULL)
    ABORT (FAIL, "Message has no command; make sure _PROC variable is being sent",
	   "VIDS-NOCOMM");

  cmdstr = SVAL(*v, 0);
  UpperCase(cmdstr, cmdstr);
  
  v = p_find (&InParblk, "_SUBCMD");
  if (v != NULL)
  {
    substr = SVAL(*v, 0);
    UpperCase(substr, substr);
  }
  else
    substr = "";

  for (i = 0; *commands[i].cmd != '\0'; i++)
  {
    if (s_equal(commands[i].cmd, cmdstr) &&
         s_equal(commands[i].subcmd, substr))
    {
      if (commands[i].AllocRequired && (env->devUnit == noUnit))
        ABORT(FAIL, "You must grab a display device first with JGRAB.",
              "VIDS-NODEV");
      NotifyUser(Verbose,"","Beginning %s.", cmdstr);
      InitTempRgns();
      UpdateVRDIState(env);		/* see if device status has changed */
      status = (*commands[i].cproc) (env);	/* Execute the command */
      FreeTempRgns(env);
      return status;
    }
  }
  ABORT (FAIL, "Sorry, no such VIDS command is available", "VIDS-NOSCHCOM");
}
