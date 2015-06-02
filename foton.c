#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <iron/full.h>
#include <stdarg.h>

#include "lisp_parser.h"
#include "lisp_types.h"
#include "lisp_compiler.h"
#include "repl.h"
const char * allowed_errors[] ={
  "Unknown touch device",
  "Invalid renderer"
};
#include <signal.h>
bool faulty = false;
void _error(const char * file, int line, const char * str, ...){
  char buffer[1000];  
  va_list arglist;
  va_start (arglist, str);
  vsprintf(buffer,str,arglist);
  va_end(arglist);
  bool noncritical = false;
  for(u32 i = 0; i < array_count(allowed_errors);i++)
    if(strstr(buffer, allowed_errors[i]) != NULL)
      noncritical = true;
  if(!noncritical)
    faulty = true;
  if(noncritical) return; //skip noncritical errors
  loge("** ERROR at %s:%i **\n",file,line);
  loge("%s", buffer);
  printf("\n");
  printf("** **\n");
  raise(SIGINT);
}

bool lisp_compiler_test();
bool test_lisp_parser();
bool test_lisp2c();

int main(int argc, char *argv[] ){
  if(argc == 1 || argc == 2 && strcmp(argv[1],"--repl") == 0){
    repl();
    return 0;
  }
  
  if(argc == 2 && strcmp(argv[1],"--test") == 0){
    log("Running tests...\n");
    TEST(test_lisp_parser);
    TEST(test_lisp2c);
    return 0;
  }

  if(argc == 2){
    compiler_state * c = compiler_make();
    lisp_run_script_file(c,argv[1]);
    return 0;
  }

  return 0;
}