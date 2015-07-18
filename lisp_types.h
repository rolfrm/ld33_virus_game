// Requires bitguy.h
typedef enum {
  SIMPLE = 0,
  FUNCTION = 1,
  POINTER = 2,
  STRUCT = 3,
  UNION = 4,
  ENUM = 5,
  TYPEDEF = 6,
  OPAQUE_STRUCT,
  type_def_kind_cnt
} type_def_kind;

typedef struct{
  u64 id;
}symbol;

struct _type_def;
typedef struct _type_def type_def;
struct _decl;
typedef struct _decl decl;

struct{
  type_def * types;
  symbol * names;
  i64 cnt;
}members;

struct _type_def{
  type_def_kind type;
  union{
    struct _enum{
      symbol * names;
      i64 * values;
      i64 cnt;
      symbol name;
    }cenum;

    struct _simple{
      symbol name;
      size_t size;
    }simple;
    
    struct{
      type_def * ret;
      type_def ** args;
      i64 cnt;
    }fcn;
    
    struct{
      symbol name; // can be empty
      decl * members;
      i64 cnt;
    }cstruct;

    struct{
      symbol name; // can be empty
      decl * members;
      i64 cnt;
      i8 opaque;
    }cunion;

    struct{
      type_def * inner;
    }ptr;

    struct{
      symbol name;
      type_def * inner;
    }ctypedef;
  };
};

struct _decl{
  symbol name;
  type_def * type;
};

// Returns the number of sub-types in t.
size_t get_sub_type_cnt(type_def * t);

// Writes all the sub types into out_types.
void get_sub_types(type_def * t, type_def ** out_types);

// Calculates the size of type t.
u64 size_of(type_def * t);

// Creates a new function type.
type_def * function_type(type_def * ret,size_t cnt, type_def ** ts);

// Creates a new simple type named 'name' with size s.
type_def make_simple(char * name, size_t s);

// Creates a pointer type from def. If the same type signature already exists it returns that.
type_def make_ptr(type_def * def);

// Function to calculate type dependencies.
// writes the dependencies of a type in defs
// descending order, so least dependent comes first.
void make_dependency_graph(type_def ** deps, type_def * def);

void print_def(type_def * type);
void print_min_type(type_def * type);
void print_cdecl(decl idecl);
void print_decl(type_def * t, symbol name);

type_def * get_fcn_ptr_function(type_def * td, int * out_ptrs);

type_def * str2type(char * str);
