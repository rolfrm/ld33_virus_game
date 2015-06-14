#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <iron/types.h>
#include <iron/utils.h>
#include <iron/log.h>
#include <iron/test.h>
#include <iron/fileio.h>
#include <iron/mem.h>
#include "lisp_types.h"
#include "lisp_parser.h"
#include "lisp_compiler.h"
#include "lisp_std_types.h"
#include "type_pool.h"
size_t get_sub_type_cnt(type_def * t){
  switch(t->type){
  case OPAQUE_STRUCT:
  case ENUM:
  case SIMPLE:
    return 0;
    break;
  case POINTER:
    return 1;
    break;
  case UNION:
  case STRUCT:
    return t->cstruct.cnt;
    break;
  case TYPEDEF:
    return 1;
    break;
  case FUNCTION:
    return 1 + t->fcn.cnt;
  case type_def_kind_cnt:
    ERROR("Unsupported type");
    break;
  }
  return 0;
}
	  
void get_sub_types(type_def * t, type_def ** out_types){
  switch(t->type){
  case OPAQUE_STRUCT:
  case ENUM:
  case SIMPLE:
    break;
  case POINTER:
    *out_types = t->ptr.inner;
    break;
  case UNION:
  case STRUCT:
    for(i64 i = 0; i < t->cstruct.cnt; i++){
      out_types[i] = t->cstruct.members[i].type;
    }
    break;
  case TYPEDEF:
    *out_types = t->ctypedef.inner;
    break;
  case FUNCTION:
    *out_types++ = t->fcn.ret;
    for(int i = 0; i < t->fcn.cnt; i++){
      *out_types++ = t->fcn.args[i];
    }
  case type_def_kind_cnt:
    ERROR("Unsupported type");
    break;
  }
}


type_def make_simple(char * name){
  static type_def def;
  def.type = SIMPLE;
  def.simple.name = get_symbol(name);
  return def;
}

type_def make_ptr(type_def * def){
  type_def out;
  out.type = POINTER;
  out.ptr.inner = def;
  return out;
}

void print_min_type(type_def * type){
  switch(type->type){
  case SIMPLE:
    format("%s", get_c_name(type->simple.name));
    break;
  case UNION:  
  case STRUCT:
  case OPAQUE_STRUCT:
    format("%s", get_c_name(type->cstruct.name));
    break;
  case POINTER:
    print_min_type(type->ptr.inner);
    format(" *");
    break;
  case ENUM:
    format("%s",symbol_name(type->cenum.name));
    break;
  case TYPEDEF:
   format("%s",symbol_name(type->ctypedef.name));
   break;
  case FUNCTION:
    ERROR("Cannot print function definition, only as decleration (named) ");
    break;
  case type_def_kind_cnt:
    ERROR("not implemented %i", type->type);
  }
}

void print_cdecl(decl idecl){
  
  type_def * def = idecl.type;
  switch(def->type){
  case ENUM:
  case UNION:
  case TYPEDEF:
  case STRUCT:
  case OPAQUE_STRUCT:
  case SIMPLE:
  case POINTER:
    print_min_type(def);
    format(" %s",get_c_name(idecl.name));
    break;
  case FUNCTION:
    print_min_type(def->fcn.ret);
    format(" %s(",get_c_name(idecl.name));
    for(i64 i = 0; i < def->fcn.cnt; i++){
      print_min_type(def->fcn.args[i]);
      if(i + 1 != def->fcn.cnt)
	format(", ");
    }
    format(")");
    break;
  case type_def_kind_cnt:
    ERROR("Not supported: '%i'\n", def->type);
  }
}

void print_decl(type_def * t, symbol name){
  decl dcl;
  dcl.name = name;
  dcl.type = t;
  print_cdecl(dcl);
}

void print_def(type_def * type){
  type_def * inner;
  switch(type->type){
  case SIMPLE:
    format("%s", symbol_name(type->simple.name));
    break;
  case OPAQUE_STRUCT:
    format("struct %s", symbol_name(type->cstruct.name));
    break;
  case STRUCT:
    format("struct %s{\n", type->cstruct.name.id == 0 ? "" : symbol_name(type->cstruct.name));
    for(i64 i = 0; i < type->cstruct.cnt; i++){	
      if(type->cstruct.members[i].name.id != 0){
	print_min_type(type->cstruct.members[i].type);
	format(" %s;\n",symbol_name(type->cstruct.members[i].name));
      }else{
	print_def(type->cstruct.members[i].type);
	format("\n");
      }
    }
    format("}"); 
    break;
  case POINTER:
    print_min_type(type->ptr.inner);
    format(" *");
    break;
  case ENUM:
    format("%s",symbol_name(type->cenum.name));
    break;
  case UNION:
    format("union {\n");
    for(i64 i = 0; i < type->cunion.cnt; i++){
      print_def(type->cunion.members[i].type);
      format(" %s;\n", symbol_name(type->cunion.members[i].name));
    }
    format("};");
    break;
 case TYPEDEF:
    inner = type->ctypedef.inner;
    format("typedef ");
    print_def(inner);
    format(" %s;\n", symbol_name(type->ctypedef.name));
    break;
  case FUNCTION:
    ERROR("Cannot print function definition, only as decleration (named) ");
    break;
  case type_def_kind_cnt:
    ERROR("not implemented %i", type->type);
  }
}

// if the code depends on *def it also depends on a number of other 
// types. This is however only what needs to be forward declared.
void _make_dependency_graph(type_def ** defs, type_def * def, bool nested){
  bool check(){
    type_def ** defs_it = defs;
    for(; *defs_it != NULL; defs_it++){
      if(*defs_it == def)
	return false;
    }
    return true;
  }
  
  bool check_add(){
    type_def ** defs_it = defs;
    for(; *defs_it != NULL; defs_it++){
      if(*defs_it == def)
	return false;
    }
    *defs_it = def;
    return true;
  }
  type_def * inner;
  if(check() == false) return;
  switch(def->type){
  case UNION:
    for(i64 i = 0; i < def->cunion.cnt; i++){
      type_def * sdef = def->cunion.members[i].type;
      _make_dependency_graph(defs,sdef,true);
    }	  
    
    if(def->cunion.name.id != 0 && !nested) check_add();// *defs_it = def;
    break;
  case STRUCT:
    for(i64 i = 0; i < def->cstruct.cnt; i++){
      type_def * sdef = def->cstruct.members[i].type;
      _make_dependency_graph(defs,sdef,true);
    }
    if(!symbol_cmp(def->cstruct.name, symbol_empty) && !nested) check_add();;
    break;
  case POINTER:
    //*defs_it = def;
    //def = def->ptr.inner;
    _make_dependency_graph(defs,def->ptr.inner,true);
    break;
  case TYPEDEF:
    //if(nested) break;
    inner = def->ctypedef.inner; 
    if(inner->type == STRUCT){
      inner = get_opaque(inner);
    }
    _make_dependency_graph(defs,inner, nested);
    check_add();    
    break;
    
  case FUNCTION:
    _make_dependency_graph(defs, def->fcn.ret, nested);

    for(int i = 0; i < def->fcn.cnt; i++)
      _make_dependency_graph(defs, def->fcn.args[i],nested);
    break;
  case OPAQUE_STRUCT:
  case ENUM:
    check_add();
    break;
  case SIMPLE:
    break;
  case type_def_kind_cnt:
    ERROR("Unknown type: %i",def->type);
    break;
  }
}

// if the code depends on *def it also depends on a number of other 
// types. This is however only what needs to be forward declared.
void make_dependency_graph(type_def ** defs, type_def * def){
  _make_dependency_graph(defs,def,false);
}

void add_var_dep(symbol * vdeps, symbol newdep){
  for(;!symbol_cmp(*vdeps, symbol_empty); vdeps++){
    if(symbol_cmp(*vdeps, newdep))
      return;
  }
  *vdeps = newdep;
}

void value_dep(type_def ** deps, symbol * vdeps, c_value val){
  var_def * var;
  switch(val.type){

  case C_INLINE_VALUE:
    make_dependency_graph(deps, val.raw.type);
    break;
  case C_FUNCTION_CALL:
    add_var_dep(vdeps, val.call.name);
    make_dependency_graph(deps, val.call.type);
    for(size_t argi = 0; argi < val.call.arg_cnt; argi++){
      value_dep(deps, vdeps, val.call.args[argi]);
    }
    break;
  case C_OPERATOR:
    value_dep(deps, vdeps, *val.operator.left);
    value_dep(deps, vdeps, *val.operator.right);
    break;
  case C_SUB_EXPR:
  case C_DEREF:
    value_dep(deps, vdeps, *val.value);
    break;
  case C_SYMBOL:
    var = get_variable(val.symbol);
    if(var == NULL){
      //ERROR("Undefined symbol '%s'",val.symbol);
      // might be a local variable.
      return;
    }
    add_var_dep(vdeps, val.symbol);
    make_dependency_graph(deps, var->type);
    break;
  case C_CAST:
    make_dependency_graph(deps, val.cast.type);
    value_dep(deps, vdeps, *val.cast.value);
  }
}

void expr_dep(type_def ** deps, symbol * vdeps, c_expr expr){
  switch(expr.type){
  case C_VAR:
    make_dependency_graph(deps, expr.var.var.type);
    if(expr.var.value != NULL){
      add_var_dep(vdeps, expr.var.var.name);
      value_dep(deps, vdeps, *expr.var.value);
    }

    break;
  case C_VALUE:
  case C_RETURN:
  case C_VALUE_UNENDED:
    value_dep(deps, vdeps, expr.value);
    break;
  case C_BLOCK:
    block_dep(deps, vdeps, expr.block);
    break;
  case C_KEYWORD:
    break;
  }
}

void block_dep(type_def ** deps, symbol * vdeps, c_block blk){
  for(size_t i = 0; i < blk.expr_cnt; i++){
    expr_dep(deps, vdeps, blk.exprs[i]);
  }
}

void c_root_code_dep(type_def ** deps, symbol * vdeps, c_root_code code){
  switch(code.type){
  case C_FUNCTION_DEF:
    make_dependency_graph(deps, code.fcndef.type);
    block_dep(deps, vdeps, code.fcndef.block);
    break;
  case C_VAR_DEF:
    //add_var_dep(vdeps, code.var.var.name);
    make_dependency_graph(deps, code.var.var.type);
    if(code.var.value != NULL)
      value_dep(deps, vdeps, *code.var.value);
    break;
  case C_DECL:
    make_dependency_graph(deps, code.decl.type);
    break;
  case C_TYPE_DEF:
    make_dependency_graph(deps, code.type_def);
  default:
    break;
  }
}


void print_value(c_value val){
  switch(val.type){
  case C_DEREF:
    format("*");
  case C_SUB_EXPR:
    format("(");
    print_value(*val.value);
    format(")");
    break;
  case C_INLINE_VALUE:
    format("%s", val.raw.value);
    break;
  case C_FUNCTION_CALL:
    format("%s(", get_c_name(val.call.name));
    for(size_t i = 0; i < val.call.arg_cnt; i++){
      print_value(val.call.args[i]);

      if(i != val.call.arg_cnt -1){
	format(", ");
      }
    }
    format(")");
    break;
  case C_OPERATOR:
    format("(");
    print_value(*val.operator.left);
    format(" %s ", val.operator.operator);
    print_value(*val.operator.right);
    format(")");
    break;
  case C_SYMBOL:
    format("%s", get_c_name(val.symbol));
    break;
  case C_CAST:
    format("((");
    print_min_type(val.cast.type);
    format(")");
    print_value(*val.cast.value);
    format(")");
  }
}

void print_c_var(c_var var){
  print_cdecl(var.var);
  if(var.value != NULL){
    format(" = ");
    print_value(*var.value);
  }
  format(";\n");
}

static void print_expr2(c_expr expr){
  switch(expr.type){
  case C_VAR:
    print_c_var(expr.var);
    break;
  case C_RETURN:
    format("return ");
  case C_VALUE_UNENDED:
  case C_VALUE:
    print_value(expr.value);
    if(expr.type != C_VALUE_UNENDED)
      format(";\n");
    break;
  case C_BLOCK:
    format("{\n");
    for(size_t i = 0; i < expr.block.expr_cnt; i++){
      print_expr2(expr.block.exprs[i]);
    }
    format("}\n");
    break;
  case C_KEYWORD:
    format("%s ", get_c_name(expr.keyword));
    break;
  }
}

void print_block(c_block blk){
  size_t var_cnt = 0;
  for(size_t i = 0; i < blk.expr_cnt; i++)
    if(blk.exprs[i].type == C_VAR) var_cnt++;  
  var_def _vars[var_cnt];
  var_def * vars = _vars;
  var_cnt = 0;

  format("{\n");
  with_symbols(&vars, &var_cnt, lambda(void, (){
	for(size_t i = 0; i < blk.expr_cnt; i++){
	  if(blk.exprs[i].type == C_VAR){
	    vars[var_cnt].name = blk.exprs[i].var.var.name;
	    vars[var_cnt].type = blk.exprs[i].var.var.type;
	    vars[var_cnt].data = &blk.exprs[i].var.value;
	    var_cnt += 1;
	  }
	  print_expr2(blk.exprs[i]);
	}
      }));
  format("}\n");
}

void print_fcn_code(c_fcndef fcndef){
  type_def * typeid  = fcndef.type;
  ASSERT(typeid->type == FUNCTION);
  print_min_type(typeid->fcn.ret);
  format(" %s(",get_c_name(fcndef.name));
  // ** handle variables ** //
  
  size_t varcnt = typeid->fcn.cnt;
  var_def _vars[varcnt];
  var_def * vars = _vars;
  for(size_t i = 0; i < varcnt; i++){
    type_def * arg_type = typeid->fcn.args[i];
    vars[i].data = NULL;
    vars[i].name = fcndef.args[i];
    vars[i].type = arg_type;
    print_min_type(arg_type);
    format(" %s", get_c_name(vars[i].name));
    if(i != varcnt-1){
      format(",");
    }
  }
  format(")");
  with_symbols(&vars,&varcnt,lambda(void,(){print_block(fcndef.block);}));
}

void print_c_code(c_root_code code){
  switch(code.type){
  case C_INCLUDE:
    format("#include \"%s\"\n",code.include);
    break;
  case C_INCLUDE_LIB:
    format("#include <%s>\n",code.include);
    break;
  case C_FUNCTION_DEF:

    print_fcn_code(code.fcndef);
    break;
  case C_VAR_DEF:
    print_c_var(code.var);
    break;
  case C_TYPE_DEF:
    print_def(code.type_def);format(";\n");
    break;
  case C_DECL:
    print_cdecl(code.decl);
    format(";\n");
    break;
  }
}


void write_dependencies(type_def ** deps){
  format("#include \"cstd_header.h\"\n");
  for(; *deps != NULL; deps++){
    type_def * t = *deps;
    if(t->type == STRUCT || t->type == OPAQUE_STRUCT){
      char * name = symbol_name(t->cstruct.name);
      if(name != NULL){
	format("struct %s;\n", name);
      }
    }
    if(t->type == ENUM){
      ERROR("Should not happen");
    }
    if(t->type == TYPEDEF){
      type_def * inner = t->ctypedef.inner;
      if(inner->type == ENUM){
	format("typedef enum {\n");
	for(int j = 0; j < inner->cenum.cnt; j++){
	  char * comma = (j !=(inner->cenum.cnt-1) ? "," : "");
	  format("   %s = %i%s\n", symbol_name(inner->cenum.names[j]), inner->cenum.values[j], comma);
	}
	format("}%s;\n", get_c_name(t->ctypedef.name));
      
      }else{
	decl dcl;
	dcl.name = t->ctypedef.name;
	dcl.type = t->ctypedef.inner;
	format("typedef struct ");
	print_cdecl(dcl);
	format(";\n");
      }
    }
  }
}
type_def * function_type(type_def * ret,size_t cnt, type_def ** ts){
  type_def td;
  td.type = FUNCTION;
  td.fcn.ret = ret;
  td.fcn.cnt = cnt;
  td.fcn.args = ts;
  return type_pool_get(&td);
}

// test //
bool test_print_c_code(){
  { // Simple include
    c_root_code c1;
    c1.type = C_INCLUDE_LIB;
    c1.include = "stdio.h";
    print_c_code(c1);
  }
  
  { // Complex, function definition
    c_value cv1a1;
    cv1a1.type = C_INLINE_VALUE;
    cv1a1.raw.value = "\"hello world!\"";
    cv1a1.raw.type = &char_ptr_def;

    c_value a_sym;
    a_sym.type = C_SYMBOL;
    a_sym.symbol = get_symbol("a");

    c_value cv1;
    cv1.type = C_FUNCTION_CALL;
    cv1.call.name = get_symbol("printf");

    cv1.call.arg_cnt = 1;
    cv1.call.args = &a_sym;

    c_expr expr;
    expr.type = C_VALUE;
    expr.value = cv1;
    
    c_fcndef fundef;
    type_def ftype;
    ftype.type = FUNCTION;
    ftype.fcn.cnt = 0;
    ftype.fcn.ret = &char_ptr_def;
    
    c_expr var;
    decl v;
    v.name = get_symbol("a");
    v.type = &char_ptr_def;
    var.type = C_VAR;
    var.var.var = v;
    var.var.value = &cv1a1;
    
    c_expr ret;
    ret.type = C_RETURN;
    ret.value = a_sym;

    c_expr exprs2[] = {var, expr, ret};
    
    c_block block;
    block.exprs = exprs2;
    block.expr_cnt = array_count(exprs2);
    
    c_expr expr3;
    expr3.type = C_BLOCK;
    expr3.block = block;
      
    fundef.block.exprs = &expr3;
    fundef.block.expr_cnt = 1;

    fundef.type = type_pool_get(&ftype);
    fundef.name = get_symbol("print_test");
    
    c_root_code c2;
    c2.type = C_FUNCTION_DEF;
    c2.fcndef = fundef;
    print_c_code(c2);
  }

  { // Complex type expansion
    c_root_code c3;
    c3.type = C_TYPE_DEF;
    c3.type_def = &type_def_def;
    print_c_code(c3);
  }

  return TEST_SUCCESS;
}
