struct linesuffix  {
  short int fds_mod16_number;		/*  1  */
  short int fds_mod60_number;		/*  3  */
  short int fds_line_number;		/*  5  */
  short int mtis_line_number;		/*  7  */
  short int missing_frames;		/*  9  */
  short int retained_frame_bits[10];	/*  11  */
  char input_type;			/*  31  */
  char input_source;			/*  32  */
  short int first_sample_number;	/*  33  */
  short int last_sample_number;		/*  35  */
}; 
