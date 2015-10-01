/*
  	RAMTEK specific
 */

#ifdef PRIVATE
#undef PRIVATE
#endif

#if VMS_OS
#ifdef 	RAMTEK_INITIALIZE
#define 	PRIVATE	globaldef noshare
#else
#define 	PRIVATE	globalref
#endif
#endif

#if UNIX_OS
#define PRIVATE
#endif

PRIVATE short RM_Channel_No;
PRIVATE char RM_string[4];
PRIVATE short RM_Device[4];
PRIVATE int RM_Autotrack_Device;
PRIVATE int RM_Cursor_No[2];

#define ADJUST_LUT(O,N) if (O == 1) N = 2;\
  			if (O == 2) N = 1;\
  			if (O == 3) N = 0;

#define RTEK_CLASS_S	1
#define RTEK_DTYPE_T	14

#ifdef 	RAMTEK_INITIALIZE
PRIVATE short	RM_Lut_Section = 1;
PRIVATE short	RM_Cursor_State[2] = { 0, 0 };

PRIVATE struct ERASE_STRUCTURE {
  unsigned char flags;
  unsigned char opcode;
  unsigned short op_flag_1;
  unsigned char  mask1;
  unsigned char  mask2;
  unsigned char  fg_color;
  unsigned char  filler;
  unsigned short x_min;
  unsigned short y_min;
  unsigned short x_max;
  unsigned short y_max;
} RM_Erase = { 0x12, 9, 0x0043, 0, 0, 0, 0, 0, 0, 0, 0 };

PRIVATE struct IMAGE_STRUCTURE {
  unsigned char  flags;
  unsigned char  opcode;
  unsigned short op_flag_1;
  unsigned short op_flag_2;
  unsigned char  mask1;
  unsigned char  mask2;
  unsigned short x_min;
  unsigned short y_min;
  unsigned short x_max;
  unsigned short y_max;
  unsigned short scan;
  unsigned short cop_x;
  unsigned short cop_y;
  unsigned short image_mode;
  unsigned short data_length;
} RM_Image = { 0x0F, 0, 0x80C1, 0x0020, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0 };

PRIVATE struct IMAGE2_STRUCTURE {
  unsigned char	flags;
  unsigned char	opcode;
  unsigned short data_length;
} RM_Image2 = { 0x09, 0, 0 };

PRIVATE struct SETDW_STRUCTURE {
  unsigned char	flags;
  unsigned char	opcode;
  unsigned short op_flag_1;
  unsigned short left_origin;
  unsigned short top_origin;
} RM_Set_Window = { 0x02, 0x08, 0x0020, 0, 0 };

PRIVATE struct ZOOM_STRUCTURE {
  unsigned char	flags;
  unsigned char	opcode;
  unsigned char	element;
  unsigned char	line;
} RM_Zoom = { 0, 0x26, 0, 0 };

PRIVATE struct CURSOR_STRUCTURE {
  unsigned char	device;
  unsigned char	opcode;
  unsigned short global_x;
  unsigned short global_y;
  unsigned short cursor_state;
} RM_Cursor = { 0, 0, 0, 0, 0 };

PRIVATE struct AUTO_STRUCTURE {
  unsigned char	device;
  unsigned char	opcode;
  unsigned short cursor_mask;
} RM_Auto = { 0, 0x33, 0 };

PRIVATE struct CIRCLE_STRUCTURE {
  unsigned char	flags;
  unsigned char	opcode;
  unsigned short op_flag_1;
  unsigned char  mask1;
  unsigned char  mask2;
  unsigned char  color;
  unsigned char  filler;
  unsigned short x_min;
  unsigned short y_min;
  unsigned short x_max;
  unsigned short y_max;
  unsigned short data_length;
  unsigned short center_x;
  unsigned short center_y;
  unsigned short radius;
} RM_Circle = { 0x03, 0x6D, 0x0043, 0, 0, 0, 0, 0, 0, 0, 0, 6, 0, 0, 0 };

PRIVATE struct TEXT_STRUCTURE {
  unsigned char	flags;
  unsigned char	opcode;
  unsigned short op_flag_1;
  unsigned char  mask1;
  unsigned char  mask2;
  unsigned char color;
  unsigned char filler;
  unsigned short x_min;
  unsigned short y_min;
  unsigned short x_max;
  unsigned short y_max;
  unsigned char x_size;
  unsigned char y_size;
  unsigned short cop_x;
  unsigned short cop_y;
  unsigned short data_length;
} RM_Write_Text = { 0x0B, 0x0C, 0x8443, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };

PRIVATE struct	RTEK_descriptor {
  unsigned short RTEK_length;
  unsigned char  RTEK_dtype;
  unsigned char  RTEK_class;
  char          *RTEK_pointer;
} RM_desc = {4,
              RTEK_DTYPE_T,
              RTEK_CLASS_S,
              RM_string };
#else
PRIVATE short	RM_Lut_Section;
PRIVATE short	RM_Cursor_State[2];

PRIVATE struct ERASE_STRUCTURE {
  unsigned char flags;
  unsigned char opcode;
  unsigned short op_flag_1;
  unsigned char  mask1;
  unsigned char  mask2;
  unsigned char  fg_color;
  unsigned char  filler;
  unsigned short x_min;
  unsigned short y_min;
  unsigned short x_max;
  unsigned short y_max;
} RM_Erase;

PRIVATE struct IMAGE_STRUCTURE {
  unsigned char flags;
  unsigned char opcode;
  unsigned short op_flag_1;
  unsigned short op_flag_2;
  unsigned char  mask1;
  unsigned char  mask2;
  unsigned short x_min;
  unsigned short y_min;
  unsigned short x_max;
  unsigned short y_max;
  unsigned short scan;
  unsigned short cop_x;
  unsigned short cop_y;
  unsigned short image_mode;
  unsigned short data_length;
} RM_Image;

PRIVATE struct IMAGE2_STRUCTURE {
  unsigned char	flags;
  unsigned char	opcode;
  unsigned short data_length;
} RM_Image2;

PRIVATE struct SETDW_STRUCTURE {
  unsigned char	flags;
  unsigned char	opcode;
  unsigned short op_flag_1;
  unsigned short left_origin;
  unsigned short top_origin;
} RM_Set_Window;

PRIVATE struct ZOOM_STRUCTURE {
  unsigned char	flags;
  unsigned char	opcode;
  unsigned char	element;
  unsigned char	line;
} RM_Zoom;

PRIVATE struct CURSOR_STRUCTURE {
  unsigned char	device;
  unsigned char	opcode;
  unsigned short global_x;
  unsigned short global_y;
  unsigned short cursor_state;
} RM_Cursor;

PRIVATE struct AUTO_STRUCTURE {
  unsigned char	device;
  unsigned char	opcode;
  unsigned short cursor_mask;
} RM_Auto;

PRIVATE struct CIRCLE_STRUCTURE {
  unsigned char	flags;
  unsigned char	opcode;
  unsigned short op_flag_1;
  unsigned char  mask1;
  unsigned char  mask2;
  unsigned char  color;
  unsigned char  filler;
  unsigned short x_min;
  unsigned short y_min;
  unsigned short x_max;
  unsigned short y_max;
  unsigned short data_length;
  unsigned short center_x;
  unsigned short center_y;
  unsigned short radius;
} RM_Circle;

PRIVATE struct TEXT_STRUCTURE {
  unsigned char	flags;
  unsigned char	opcode;
  unsigned short op_flag_1;
  unsigned char  mask1;
  unsigned char  mask2;
  unsigned char color;
  unsigned char filler;
  unsigned short x_min;
  unsigned short y_min;
  unsigned short x_max;
  unsigned short y_max;
  unsigned char x_size;
  unsigned char y_size;
  unsigned short cop_x;
  unsigned short cop_y;
  unsigned short data_length;
} RM_Write_Text;

PRIVATE struct	RTEK_descriptor {
  unsigned short RTEK_length;
  unsigned char  RTEK_dtype;
  unsigned char  RTEK_class;
  char          *RTEK_pointer;
} RM_desc;
#endif

#define	ERASE_WORDS	8
#define	ERASE_FG_COLOR	RM_Erase.fg_color
#define	ERASE_MASK1	RM_Erase.mask1
#define	ERASE_MASK2	RM_Erase.mask2
#define	ERASE_X_MIN	RM_Erase.x_min
#define	ERASE_Y_MIN	RM_Erase.y_min
#define	ERASE_X_MAX	RM_Erase.x_max
#define	ERASE_Y_MAX	RM_Erase.y_max

#define	IMAGE_WORDS	13
#define	IMAGE_OP	RM_Image.opcode
#define	IMAGE_MASK1	RM_Image.mask1
#define	IMAGE_MASK2	RM_Image.mask2
#define	IMAGE_X_MIN	RM_Image.x_min
#define	IMAGE_Y_MIN	RM_Image.y_min
#define	IMAGE_X_MAX	RM_Image.x_max
#define	IMAGE_Y_MAX	RM_Image.y_max
#define	IMAGE_X		RM_Image.cop_x
#define	IMAGE_Y		RM_Image.cop_y
#define	IMAGE_LENGTH	RM_Image.data_length

#define	IMAGE2_WORDS	2
#define	IMAGE2_OP	RM_Image2.opcode
#define	IMAGE2_LENGTH	RM_Image2.data_length

#define SETDW_WORDS	4
#define SETDW_LEFT	RM_Set_Window.left_origin
#define SETDW_TOP	RM_Set_Window.top_origin

#define CREAD_WORDS	1
#define CWRITE_WORDS	4
#define CURSOR_DEVICE	RM_Cursor.device
#define CURSOR_OP	RM_Cursor.opcode
#define GLOBAL_X	RM_Cursor.global_x
#define GLOBAL_Y	RM_Cursor.global_y
#define CURSOR_STATE	RM_Cursor.cursor_state
#define CURSOR_INVISIBLE	0x0800
#define CURSOR_VISIBLE		0x0600
#define SET_BLINK( C,B ) if (B == 0) RM_Cursor_State[C-1] &= CURSOR_VISIBLE;\
                         if (B == 1) RM_Cursor_State[C-1] |= CURSOR_INVISIBLE;
#define WRITE_GLOBAL	0x2D
#define READ_GLOBAL	0x2F

#define AUTO_WORDS	2
#define AUTO_DEVICE	RM_Auto.device
#define AUTO_CURSOR	RM_Auto.cursor_mask

#define ZOOM_WORDS	2
#define ZOOM_LINE	RM_Zoom.line
#define ZOOM_ELEMENT	RM_Zoom.element

#define CIRCLE_WORDS	12
#define CIRCLE_COLOR	RM_Circle.color
#define CIRCLE_MASK1	RM_Circle.mask1
#define CIRCLE_MASK2	RM_Circle.mask2
#define	CIRCLE_X_MIN	RM_Circle.x_min
#define	CIRCLE_Y_MIN	RM_Circle.y_min
#define	CIRCLE_X_MAX	RM_Circle.x_max
#define	CIRCLE_Y_MAX	RM_Circle.y_max
#define CIRCLE_X	RM_Circle.center_x
#define CIRCLE_Y	RM_Circle.center_y
#define CIRCLE_RAD	RM_Circle.radius

#define WRITE_TEXT_WORDS	12
#define TEXT_HEIGHT		9
#define TEXT_WIDTH		7
#define TEXT_CENTER		2
#define TEXT_RIGHT		3
#define TEXT_MASK1	RM_Write_Text.mask1
#define TEXT_MASK2	RM_Write_Text.mask2
#define TEXT_COLOR	RM_Write_Text.color
#define	TEXT_X_MIN	RM_Write_Text.x_min
#define	TEXT_Y_MIN	RM_Write_Text.y_min
#define	TEXT_X_MAX	RM_Write_Text.x_max
#define	TEXT_Y_MAX	RM_Write_Text.y_max
#define TEXT_X_SIZE	RM_Write_Text.x_size
#define TEXT_Y_SIZE	RM_Write_Text.y_size
#define TEXT_X		RM_Write_Text.cop_x
#define TEXT_Y		RM_Write_Text.cop_y
#define TEXT_LENGTH	RM_Write_Text.data_length

#define	LOAD_AUX_MEM	3
#define	READ_AUX_MEM	4
#define	WRITE_IMAGE	0x0A
#define	READ_IMAGE	0x0B

PRIVATE struct AM_STRUCTURE {
  char	device;
  char	opcode;
  short	address;
  short	length;
} Aux_Memory;

#define LUT_OPCODE	Aux_Memory.opcode
#define LUT_DEVICE	Aux_Memory.device
#define LUT_ADDRESS	Aux_Memory.address
#define LUT_LENGTH	Aux_Memory.length

/*    Vector definitions   */

#define MAX_VECTOR_PAIRS	60
#define HEADER_WORDS		7
#define WRITE_VECTOR_LINKED	0x0E

PRIVATE struct VECTOR_STRUCTURE {
  unsigned char	flags;
  unsigned char	opcode;
  unsigned short op_flag_1;
  unsigned char  mask1;
  unsigned char  mask2;
  unsigned short color;
  unsigned short cop_x;
  unsigned short cop_y;
  unsigned short data_length;
  union {
    unsigned int vector_data;
    struct {
      unsigned short x;
      unsigned short y;
    } vector_pairs[MAX_VECTOR_PAIRS];
  } vector_union;
} RM_Vector;

#define VECTOR_FLAGS	RM_Vector.flags
#define VECTOR_OPCODE	RM_Vector.opcode
#define VECTOR_FLAG_1	RM_Vector.op_flag_1
#define VECTOR_COLOR	RM_Vector.color
#define VECTOR_MASK1	RM_Vector.mask1
#define VECTOR_MASK2	RM_Vector.mask2
#define VECTOR_COP_X	RM_Vector.cop_x
#define VECTOR_COP_Y	RM_Vector.cop_y
#define VECTOR_LENGTH	RM_Vector.data_length
#define VECTOR_X(I)	RM_Vector.vector_union.vector_pairs[I].x
#define VECTOR_Y(I)	RM_Vector.vector_union.vector_pairs[I].y
#define VECTOR_DATA	RM_Vector.vector_union.vector_data

/*    AFG definitions   */

#define AFG_LINE_SIZE	80
#define LAST_POSITION	1920
#define SPACE		' '

struct AFG_STRUCTURE { 
  unsigned ascii_char   :  7;
  unsigned rev_video    :  1;
  unsigned blink        :  1;
  unsigned under_line   :  1;
  unsigned intensity    :  2;
  unsigned              :  4;
};

union AFG_UNION_STRUCT {
  short AFG_Word;
  struct AFG_STRUCTURE AFG_Char;
};

PRIVATE union AFG_UNION_STRUCT AFG_Line[AFG_LINE_SIZE];

PRIVATE union AFG_UNION_STRUCT *AFG_Lut;

#define ATEXT_CHAR(I)		AFG_Lut[I].AFG_Char.ascii_char
#define ATEXT_REV_VIDEO(I)	AFG_Lut[I].AFG_Char.rev_video
#define ATEXT_BLINK(I)		AFG_Lut[I].AFG_Char.blink
#define ATEXT_UNDER_LINE(I)	AFG_Lut[I].AFG_Char.under_line
#define ATEXT_INTENSITY(I)	AFG_Lut[I].AFG_Char.intensity
#define ATEXT_WORD(I)		AFG_Lut[I].AFG_Word

/*     Cursor definitions    */

#ifdef 	RAMTEK_INITIALIZE
PRIVATE short cur_shapes[2][64] = {
    0x0100, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0180, 0x0100,
    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0100, 0x0000,

    0x0100, 0x0000, 0x0100, 0x0000, 0x0100, 0x0000, 0x0100, 0x0000,
    0x0100, 0x0000, 0x0100, 0x0000, 0x0100, 0x0000, 0x0100, 0x0000,
    0x0100, 0x0000, 0x0100, 0x0000, 0x0100, 0x0000, 0x0100, 0x0000,
    0x0100, 0x0000, 0x0100, 0x0000, 0x0100, 0x0000, 0xFFFF, 0xFFFF,
    0x0100, 0x0000, 0x0100, 0x0000, 0x0100, 0x0000, 0x0100, 0x0000,
    0x0100, 0x0000, 0x0100, 0x0000, 0x0100, 0x0000, 0x0100, 0x0000,
    0x0100, 0x0000, 0x0100, 0x0000, 0x0100, 0x0000, 0x0100, 0x0000,
    0x0100, 0x0000, 0x0100, 0x0000, 0x0100, 0x0000, 0x0100, 0x0000
};
PRIVATE struct CFONT_STRUCTURE {
  unsigned char	cursor;
  unsigned char	opcode;
  unsigned short bytes;
  unsigned short pairs[64];
} RM_CFont= { 0, 0x30, 128 };

#else
PRIVATE short cur_shapes[2][64];

PRIVATE struct CFONT_STRUCTURE {
  unsigned char	cursor;
  unsigned char	opcode;
  unsigned short bytes;
  unsigned short pairs[64];
} RM_CFont;
#endif

#define CFONT_WORDS	66
#define CFONT_NPAIRS	64
#define CFONT_CURSOR	RM_CFont.cursor
#define CFONT_PAIRS(i)	RM_CFont.pairs[i]
