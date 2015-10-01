/*---------------------------------------------------------------------------*/
/*                        LABEL Function Prototypes                          */
/*---------------------------------------------------------------------------*/

#ifndef SUN_UNIX
                                              
AGGREGATE gpds_open_file (char *file_name,
                          int *status);

char *gpds_get_label_value (AGGREGATE label_ptr,
				char *object,
				char *keyword,
				int element,
                          	int *status);
char *gpds_get_column_value (AGGREGATE label_ptr,
				char *table_file_name,
				int record,
				char *keyword,
				int item_number_in_set,
				char *data_type,
				int *value_length,
                          	int *status);

void gpds_replace_column_value (AGGREGATE label_ptr,
				char *table_file_name,
				int record,
				char *keyword,
				int item_number_in_set,
				char *formatted_value,
                          	int *status);

void gpds_close_file (AGGREGATE label_ptr);

void mo_table_read( 	char *label_file_name,
			char *table_file_name,
			void *table_ptr,
			int  *row_number,
			char *key,
			char *value,
			int  item_number,
                        int  *status);

#else
                                               
AGGREGATE gpds_open_file ();
char *gpds_get_label_value ();
char *gpds_get_column_value ();
void gpds_replace_column_value ();
void gpds_close_file ();
void mo_table_read ();

#endif
