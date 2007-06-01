/* ifeffit command shell: Windows

   This provides a simple command line shell interface o the  
   Ifeffit XAFS Analysis Library, without the GNU readline library.

   Copyright (c) 1998--2000 Matthew Newville, The University of Chicago
  
   This software is distributed under the terms of the GNU General 
   Public License as published by the Free Software Foundation.  It 
   is distributed in the hope that it will be useful, but WITHOUT 
   ANY WARRANTY; without even the implied warranty of 
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

   See the GNU General Public License (in the file COPYING) for 
   more details.

   This file is ased on and borrows heavily from the file fileman.c
   distributed with the GNU readline library.

   Matt Newville newville@cars.uchicago.edu
 */

#define Version "0.9b"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

char *stripwhite();
char *progname, *home;
static char comstr[1024], line_ex[1024],histfile[512], startfile[512];
static  char line[1024];

/* extern char *xmalloc(); */
/* extern char *readline();*/
#include "ifeffit.h"

/* A structure which contains information on 
   the commands this program can understand. */

typedef  int _stdcall Func();
int _stdcall  promptline();
int _stdcall sys_exec();
int _stdcall sys_help();
int _stdcall com_list();
int _stdcall com_more();
int _stdcall com_pwd();
int _stdcall com_cd();
int _stdcall execute_line();
int _stdcall load_startup_file();
int _stdcall iff_load_file()  ;

#include "commands.h"

#define whitespace(c) (((c) ==' ')||((c) == '\t'))

/* main */
int main (int argc, char **argv) {
  char *s, *prompt;
  int  exec, ret, i;
  progname = argv[0];
  /* Loop reading and executing lines until the user quits. */
  /*  ifeffit_("") ;*/
  ret  = 1;
  exec = 1;

  /* initialize ifeffit */
  ret = iff_exec(" set &screen_echo=1");
  if (ret != 0) {
    printf("  fatal error loading ifeffit library\n") ;
    exit(ret);
  }
  s = iff_strval("&build");
  printf("  Ifeffit %s\n", s);
  printf("          command-line shell version %s for Win32\n", Version);

  /* handle command line switches */
  for (i=1;i<argc; i++) {
    if ( ((strncmp(argv[i], "-q", 2)) == 0) ) {
      /* -q  means execute script with no screen echo */
      ret = iff_exec(" set &screen_echo=0");
    } else if ((strncmp(argv[i], "-x", 2)) == 0) {  
      /* -x  means execute script without pausing and exit */
      ret = iff_exec(" set &pause_ignore=1");
      exec = 0;   
    }
  }

  /* load all script files given on command line */
  for (i=1;i<argc; i++) {
    if ( ((strncmp(argv[i], "-x", 2)) != 0) &&
	 ((strncmp(argv[i], "-q", 2)) != 0) ) {  
      ret = iff_load_file(argv[i]);	
      if (ret == 1) {exec = 0;}
    }
  }
  ret = 0;
  /* execution loop  */
  while (exec) {
    prompt = "Ifeffit> ";
    if (ret == -1) { prompt = "  ...  > ";}
    ret = promptline(prompt); 
    s   = stripwhite(line);
    if (( strncmp(s , "exit",4) == 0) ||
	( strncmp(s , "quit",4) == 0)) {
    /* ret = iff_exec(" exit ") ; */
	ret = 0; exec = 0;
    } else if (*s) {
      ret = execute_line(s);
      /* special case :: ret=1 means 'good exit', 
	 so set return stat to 0 and exit while(exec) loop */
      if (ret == 1) { exec = 0; ret = 0; }
      /* a truly bad exit status */
      if (ret >  1) { exec = 0;} 
    }
     /* free(line);*/
  }
  
  exit(ret);
}
/* done with main */


/* Execute a command line. */
int _stdcall execute_line (char *line) {
  register int i;
  COMMAND *command;
  char *word ;
  /* Isolate the command word. */
  i = 0;
  while (line[i] && whitespace (line[i]))  i++;
  word = line + i;
  while (line[i] && !whitespace (line[i])) i++;
  if (line[i]) line[i++] = '\0';
  command = find_command(word);

  /* Get argument to command, if any.   */
  while (whitespace (line[i]))  i++;
  word = line + i; 

  sprintf(line_ex, "%s %s \n\0", line, word);
  /*
     printf(  ":: %s %s \n\0", line, word);
  */
  if ((!command)||
      (find_command(line)->func ==  find_command("plot")->func )) {
    return (iff_exec (line_ex));
  } else if (find_command(line)->func == find_command("dir")->func) {
    return (sys_exec(line_ex));
  } else {
    return  ((*(command->func)) (word));
  }
}

int _stdcall load_startup_file(char *file) {
  FILE *fp;
  int i;
  fp = fopen(file,"r");
  printf ("  Startup file %s",file);   
  if (fp) {
    fclose(fp);
    i = iff_load_file(file);
    if (i==0) {
      printf ("  loaded OK\n");
    }  else {
      printf ("  loaded with an error\n");
    }
  } else {
    printf ("  not found\n");   
  }
  return 0;
}

int _stdcall iff_load_file(char *file) {
  FILE *fp;
  int i=1;
  fp = fopen(file,"r");
  if (fp) {
    fclose(fp);
    sprintf(line_ex, "load %s \n\0",file);
    i = iff_exec(line_ex);
    if (i!=0) { 
      printf (" Error loading %s!\n",file);
    }
  } else {
    printf (" No file %s!\n",file);
  }
  return i;
}


int _stdcall sys_exec(char *arg) {
  sprintf (comstr, "%s", arg);
  printf (" will send: %s", comstr);
  system (arg);
  return(0);
}

/* Print out help for ARG, or for all of the commands if ARG is
   not present. */
int _stdcall sys_help (char *arg) {
  register int i;
  int printed = 0;
  printf (" syshelp: %s\n", arg);
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

int  _stdcall com_list (char *arg) {
  if (!arg)  arg = "";
  sprintf (comstr, "DIR %s", arg);
  return (0);  /* system (comstr)); */
}

int _stdcall com_more (char *arg) {
  sprintf (comstr, "more < %s", arg);
  system (comstr);
  return (0);
}
int _cdecl getcwd(), chdir();

int _stdcall com_cd (char *arg) {
  sprintf (comstr, "cd %s", arg);
  /* system(comstr);*/
  chdir(arg);
  com_pwd("");
  return 0;
}

int _stdcall com_pwd (char *arg) {
   if (getcwd(comstr,1024))  {
    printf (" %s\n", comstr);
  } else {
    printf ("Error with pwd: %s\n", comstr);
    return 1;
  }
 
  return 0;
}
/**/

/* Strip whitespace from the start and end of STRING.  Return a pointer
   into STRING. */

char *stripwhite (char *string) {
  register char *s, *t;
  for (s = string; whitespace (*s); s++)  ;
  if (*s == 0) return (s);
  t = s + strlen (s) - 1;
  while (t > s && whitespace (*t))  t--;
  *++t = '\0';
  return s;
}

int _stdcall promptline(char *prompt) {
  int i ,c;
  printf("%s", prompt);
  for (i = 0 ; i< 1023 && (c  = getchar()) != EOF && c !='\n'; ++i) {
    line[i] = c;
  }
  line[i] = '\0';
  return i;
 }
