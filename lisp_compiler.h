// requires bitguy.h, lisp_parser.h, lisp_types

typedef struct{
  symbol name;
  type_def * type;
  void * data;
}var_def;

// Currently the compiler just contains variables.
struct _compiler_state{
  var_def * vars;
  size_t var_cnt;
};

typedef struct _compiler_state compiler_state;

typedef struct{
  compiler_state * c;
  char * buffer;
  // required functions
  //fcn_def * fcns;
  //size_t fcn_cnt;
  char ** deps;
  size_t dep_cnt;
}comp_state;

typedef struct{
  type_def result_type;
  void * fcn;
}compiled_expr;

typedef struct{
  char * name;
  i64 arg_cnt;
  void * fcn;
}cmacro_def;


typedef enum {
  COMPILE_OK,
  COMPILE_ERROR
}compile_status;


compiler_state * get_compiler();
compiler_state * compiler_make();
var_def * lisp_compile_expr(expr ex, compile_status * opt_outstatus);
void * lisp_compile_and_run_expr(expr ex, compile_status * opt_outstatus);
void lisp_load_base();
compile_status lisp_run_exprs(expr * exprs, size_t exprcnt);
compile_status lisp_run_script_string(char * code);
compile_status lisp_run_script_file(char * filepath);
// defines a variable pointer.
void compiler_define_variable_ptr(symbol sym, type_def * t, void * ptr);
void define_macro(char * name, int nargs, void * fcn);

void compiler_reg_type(compiler_state *c, symbol name, type_def * t);
void compiler_load_types(compiler_state *);

comp_state comp_state_make();

// warning: The returned variable will eventually get invalidated by defining new variables.
var_def * get_variable(symbol s);

bool is_type_compatible(type_def * call_type, type_def * arg_type, expr callexpr);

cmacro_def * get_cmacro_def(symbol s);
compiled_expr compile_expr(expr * e);
type_def compile_iexpr(expr expr1);
void compiler_set_state(compiler_state * ls);
void write_dependencies(type_def ** deps);

void with_compiler(compiler_state * c, void (* fcn)());
void pop_compiler();
void push_compiler(compiler_state * c);


char * get_c_name(symbol s);
void format_c_name(symbol s);
void check_name_vs_type(symbol name);

type_def * _type_macro(expr typexpr);
type_def * _compile_expr(c_block * block, c_value * val,  expr e );
#define COMPILE_ASSERT(expr) if(!(expr)){loge("at %s : %i:", __FILE__, __LINE__);loge("Compile error '" #expr "' at %s: %l", __FILE__, __LINE__ ); logd("\n");return &error_def;}
#define COMPILE_ERROR(fmt, ...) {loge("at %s : %i: ", __FILE__, __LINE__);loge(fmt,##__VA_ARGS__); logd("\n"); return &error_def;}
void compile_as_c(c_root_code * codes, size_t code_cnt);
type_def * compile_value(c_value * val, value_expr e);

void defun(char * sym, type_def * t, void * fcnptr);
type_def * macro_store_type();
type_def * expand_macro(c_block * block, c_value * val, expr * exprs, size_t cnt);
// symbols
void with_symbols(var_def ** vars, size_t * vars_cnt, void (*fcn)());
void push_symbols(var_def ** vars, size_t * vars_cnt);
void pop_symbols();

bool test_lisp2c();
void checktypepool();
