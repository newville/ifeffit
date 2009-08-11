/* ifeffit command shell:

   This provides a simple command line shell interface o the  
   Ifeffit XAFS Analysis Library, using the GNU readline library.

   Copyright (c) 1998--2000 Matthew Newville, The University of Chicago
  
   This software is distributed under the terms of the GNU General 
   Public License as published by the Free Software Foundation.  It 
   is distributed in the hope that it will be useful, but WITHOUT 
   ANY WARRANTY; without even the implied warranty of 
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

   See the GNU General Public License (in the file COPYING) for 
   more details.

   This file is based on and borrows heavily from the file fileman.c
   distributed with the GNU readline library.

   Matt Newville newville@cars.uchicago.edu
 */


#define Version "1.1"
#define HIST_LINES_DEF  500
#define HIST_FILE_DEF  "/.ifeffit_hist"

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <sys/types.h>
#ifdef HAVE_SYS_FILE_H
#include <sys/file.h>
#endif
#include <sys/stat.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

#ifdef HAVE_STRING_H
#include <string.h>
#else 
#include <strings.h>
#endif

#ifdef MY_READLINE
#  include "../../readline/readline.h"
#  include "../../readline/history.h"
#else
#  include <readline/readline.h>
#  include <readline/history.h>
#endif

#if !defined (_FUNCTION_DEF)
#  define _FUNCTION_DEF
typedef int Function ();
#endif 


int   sys_exec(), sys_help();
int   com_list(), com_more();
int    com_pwd(), com_cd();
int   iff_load_file(), write_history_file();
char  *stripwhite(), *progname, *home;
static char comstr[1024], line_ex[1024], hist_file[512];
void   initialize_readline();

#include "../lib/ifeffit.h"
#include "commands.h"

/* main */
int main (int argc, char **argv) {
  
  char *line, *s, *s1, *prompt, *tmp;
  char *pr1, *pr2;
  int   hist_lines = HIST_LINES_DEF;
  int   exec = 1;
  int   ret, i, j, c,quiet;

  pr1 = "Ifeffit> ";
  pr2 = "  ...  > ";

  progname = argv[0];
  initialize_readline();	/* start readline completer. */

  /* initialize ifeffit */
  ret = iff_exec(" set &screen_echo=1");
  quiet = 0;
  if (ret != 0) {
    printf("  fatal error loading ifeffit library\n") ;
    exit(ret);
  }
  /* handle command line switches */
  for (i=1;i<argc; i++) {
    if ( ((strncmp(argv[i], "-q", 2)) == 0) ) {
      /* -q  means execute script with no screen echo */
      ret = iff_exec(" set &screen_echo=0");
      quiet = 1;
    } else if ((strncmp(argv[i], "-x", 2)) == 0) {  
      /* -x  means execute script without pausing and exit */
      ret = iff_exec(" set &pause_ignore=1");
      exec = 0;   
    } else if (((strncmp(argv[i], "-h", 2)) == 0) ||
	       ((strncmp(argv[i], "--help", 6)) == 0)) {  
      printf(" Ifeffit usage:   ifeffit [options] file1 file2 ...\n");
      printf(" options\n");
      printf("      -h  show this help message\n");
      printf("      -x  exit after running script files\n");
      printf("      -q  run quietly, with no screen output\n");
      printf("      -n  disable prompt (ie, from 'Ifeffit>' to '')\n");
      printf("      -v  show version information and exit\n");
      printf("      -i  show installation directory and exit\n");
      exit(0);
    } else if ((strncmp(argv[i], "-v", 2)) == 0) {  
      printf(" Ifeffit %s\n", iff_strval("&build"));
      printf(" Command line version %s\n", Version);
      exit(0);
    } else if ((strncmp(argv[i], "-n", 2)) == 0) {  
      pr1 = "";
      pr2 = "";
    } else if ((strncmp(argv[i], "-i", 2)) == 0) {  
      printf("%s\n", iff_strval("&install_dir"));
      exit(0);
    }
  }
    
  if (quiet == 0) {
    s1 = calloc(256,sizeof(char));
    i = iff_get_string("&build",s1);
    s  = calloc(i+1,sizeof(char));
    strncpy(s,s1,i); 
    printf(" Ifeffit  %s\n", s);
    printf("                command-line shell version %s with GNU Readline\n", Version);
  }

  /* get environmental variables associated with history file */

  tmp = getenv("IFF_HISTORY_LINES");
  if (tmp) {   hist_lines = atoi(tmp);  }   
  tmp = getenv("IFF_HISTORY_FILE");
  if (tmp) {
    strcpy(hist_file, tmp);
  } else {
    home = getenv("HOME");
    if (home) {
      strcpy(hist_file, home);
      strcat(hist_file, "/.ifeffit_hist");
    }
  }
  /* read the history file */
  if (strlen(hist_file) > 3) {ret = read_history(hist_file);}

  /* load all script files given on command line */
  for (i=1;i<argc; i++) {
    if ( ((strncmp(argv[i], "-x", 2)) != 0) &&
	 ((strncmp(argv[i], "-n", 2)) != 0) &&
	 ((strncmp(argv[i], "-q", 2)) != 0) ) {  
      ret = iff_load_file(argv[i]);	
      if (ret == 1) {exec = 0;}
    }
  }
  ret = 0;

  /* execution loop  */
  while (exec) {
    prompt = pr1;
    if (ret == -1) { prompt = pr2;} 
    if (!(line = readline(prompt))) break;
    s = stripwhite(line);
    if (( strncmp(s , "exit",4) == 0) ||
	( strncmp(s , "quit",4) == 0)) {
      ret = 0; exec = 0;
    } else if (*s) {
      add_history(s);  
      ret = execute_line(s);
      /* special case :: ret=1 means 'good exit', 
	 so set return stat to 0 and exit while(exec) loop */
      if (ret == 1) { exec = 0; ret = 0; }
      /* a truly bad exit status */
      if (ret >  1) { exec = 0;} 
    }
    free(line);
  }
  i = write_history_file(hist_file, hist_lines);
  i = iff_exec(" quit ") ;
  exit(ret);
}
/* done with main */

/* Execute a command line. */
int execute_line (char *line) {
  register int i;
  COMMAND *command;
  char *word , *cmd, *arg;
  /* Isolate the command word. */
  if (line[0]=='!') {
    arg = line + 1;
    cmd = "!";
  } else {
    i = 0;
    while (line[i] &&  (whitespace (line[i]))) i++;;
    arg = line + i;
    while (line[i] && !(whitespace (line[i]))) i++;
    if (line[i]) { line[i++] = '\0'; }
    while (whitespace (line[i]) )  i++;
    arg = line + i;
    cmd = line;
  }
  command = find_command(cmd);
  sprintf(line_ex, "%s %s\n\0", cmd, arg);
  if ((!command) ||
      (find_command(cmd)->func == find_command("plot")->func)) {
    return (iff_exec(line_ex));
  } else {
    i = ((*(command->func)) (arg)); 
    return(0); /* never exit due to system call ! */
  }
}

int iff_load_file(char *file) { /* load a file of ifeffit commands */
  FILE *fp;
  int i=1;
  fp = fopen(file,"r");
  if (fp) {
    fclose(fp);
    sprintf(line_ex, "load %s \n\0",file);
    i = iff_exec(line_ex);
    if (i==1) {  return i;}
  } else {
    printf (" No file %s!\n",file);
  }
  return i;
}

int write_history_file(char *file, int n_hist) {
  int i;
  i = write_history(file);
  i = history_truncate_file(file,n_hist);
  return i;
}


int com_list (char *arg) {
  if (!arg)  arg = "";
  sprintf (comstr, "ls %s", arg);
  return (system (comstr));
}

int com_more (char *arg) {
  sprintf (comstr, "more %s", arg);
  return (system (comstr));
}

int com_cd (char *arg) {
  if (chdir (arg) == -1) { return 1;  }
  com_pwd("");
  return 0;
}

int com_pwd (char *arg) {
  if (getcwd(comstr,1024))  {
    printf (" %s\n", comstr);
  }
  return 0;
}

int sys_exec(char *arg) {
  if ((arg)&&(strlen(arg) > 1)) { 
    sprintf (comstr, "%s", arg);
    return (system (arg));
  } else {
    printf("  starting shell.  type 'exit'  to return to ifeffit\n");
    return (system ("sh"));
  }
}

/* Print out help for ARG, or for all of the 
   commands if ARG is  not present. */
int sys_help (char *arg) {
  register int i;
  int printed = 0;
  if (!*arg ) { 
    for (i = 0; commands[i].name; i++) {
      printf (" %-12s  %s\n", commands[i].name, commands[i].desc);
      printed++;
    }
  } else {
    for (i = 0; commands[i].name; i++) {
      if ( (strcmp (arg, commands[i].name) == 0)) {
	printf (" %-12s  %s\n     %s\n", commands[i].name, 
		commands[i].desc, commands[i].doc);
	printed++;
      }
    }
  }
  if (!printed) {
    printf ("No commands match `%s'.  Possibilties are:\n", arg);
    for (i = 0; commands[i].name; i++) { /* Print in six columns. */
      if (printed == 5) {
	printed = 0;
	printf ("\n");
      }
      printf ("%-12s ", commands[i].name);
      printed++;
    }
    if (printed) printf ("\n");
  }
  return (0);
}

/* Look up NAME as the name of a command, and return a pointer to that
   command.  Return a NULL pointer if NAME isn't a command name. */
COMMAND *find_command (char *name) {
  register int i;
  for (i = 0; commands[i].name; i++) {
    if (strcmp (name, commands[i].name) == 0) return (&commands[i]);
  }
  return ((COMMAND *)NULL);
}

/* Strip whitespace from the start and end of STRING. */
char *stripwhite (char *string) {
  register char *s, *t;
  for (s = string; whitespace (*s); s++)  ;
  if (*s == 0) return (s);
  t = s + strlen (s) - 1;
  while (t > s && whitespace (*t))  t--;
  *++t = '\0';
  return s;
}

char *dupstr (char *s) {
  char *r;
  r = calloc(strlen(s)+1,sizeof(char));
  strcpy (r, s);
  return r;
}


/* **************************************************************** */
/*                                                                  */
/*                  Interface to Readline Completion                */
/*                                                                  */
/* **************************************************************** */

char *command_generator ();
char **ifeffit_completion ();

/* Tell the GNU Readline library how to complete.  We want to try to complete
   on command names if this is the first word in the line, or on filenames
   if not. */
void initialize_readline () {
  /* Allow conditional parsing of the ~/.inputrc file. */
  rl_readline_name = "Ifeffit";
  /* Tell the completer that we want a crack first. */
  rl_attempted_completion_function = (CPPFunction *)ifeffit_completion;
}

/* Attempt to complete on the contents of TEXT.  START and END bound the
   region of rl_line_buffer that contains the word to complete.  TEXT is
   the word to complete.  We can use the entire contents of rl_line_buffer
   in case we want to do some simple parsing.  Return the array of matches,
   or NULL if there aren't any. */
char **ifeffit_completion (char *text, int start, int end) {
  char **matches;
  matches = (char **)NULL;
  /* If this word is at the start of the line, then it is a command
     to complete.  Otherwise it is the name of a file in the current
     directory. */
  if (start == 0) matches = rl_completion_matches (text, command_generator);
  return (matches);
}

/* Generator function for command completion.  STATE lets us know whether
   to start from scratch; without any state (i.e. STATE == 0), then we
   start at the top of the list. */
char *command_generator (char *text, int state) {
  static int list_index, len;
  char *name;
  /* If this is a new word to complete, initialize now.  This includes
     saving the length of TEXT for efficiency, and initializing the index
     variable to 0. */
  if (!state)  {
    list_index = 0;
    len = strlen(text);
  }
  /* Return the next name which partially matches from the command list. */
  while (name = commands[list_index].name) {
    list_index++;
    /*if (strncmp (name, text, len) == 0)   return name;    */
    if (strncmp(name, text, len) == 0) return (dupstr(name));
  }  
  return ((char *)NULL);
}

