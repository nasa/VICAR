int tokenize(char *line, char *sep_char, int *n_adr, int **b_adr, int **e_adr);
int get_max_token_length(int n_tokens, int *bt, int *et);
int extract_token(char *dest, char *src, int *b, int *e, int w);
int get_token_list(char *src, int n_tokens, int *b, int *e, char ***token_list_adr);
