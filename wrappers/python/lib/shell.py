#!/usr/bin/python

import cmd
from . import ifeffit
Ifeffit = ifeffit.Ifeffit

class shell(cmd.Cmd,Ifeffit):
    __doc__= """
  IFeffit Shell in Python

  This simple Ifeffit Shell provides enough functionality for most
  use.  Command history and editing are supported (with the arrow
  keys and other GNU Readline mechanisms). Shell commands will be
  executed if preceded by a '!' (and not a whimper!).

  For a  more functional command shell see the C version

  M Newville  Univ of Chicago
"""
    ps1    = "Ifeffit>"
    ps2    = "  ...  >"
    intro  = " Ifeffit Shell in Python "
    misc_header = "Help is also available on: "
    max_save_lines = 500
    def __init__(self,completekey='tab'):
        import os
        try:
            import readline
            self.rdline = readline
        except ImportError:
            self.rdline = None

            
        cmd.Cmd.__init__(self,completekey='tab')
        Ifeffit.__init__(self)
        self.prompt    = self.ps1
        return None

    def __del__(self):
        if (self.rdline):
            self.rdline.set_history_length(1000)
            self.rdline.write_history_file('iff_shell.iff')

    def emptyline(self):
        pass

    def help_help(self):
        print( self.__doc__)
    def help_Ifeffit(self):
        print( Ifeffit.__doc__)
    def help_shell(self):
        print( """
        shell commands can be executed by preceding either by 'shell' or '!':
   Ifeffit>! ls
   Ifeffit>shell lpr ifeffit.ps""")
    def help_EOF(self):
        print( " EOF = quit = exit = Control^D ==exit the program")
    def do_shell(self, arg):
        import os
        os.system(arg)
    def do_EOF(self,arg):
        return 1
    def default(self,str1):
        str = str1.strip()
        if ((str == 'quit') or (str == 'exit')):
            return 1
        elif (str[0:1] == '!'):
            self.do_shell(str)
        else:
            j = self.ifeffit(str)
            self.prompt = self.ps1
            if (j == -1): self.prompt = self.ps2


#
# if this was invoked from a command line, execute the shell!
if (__name__ == '__main__'):
    shell().cmdloop()

