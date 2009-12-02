#!/usr/bin/python
##
##  GIFEFFIT: Graphical User Interface to the IFEFFIT XAFS Analysis Library
##
##  Copyright (c) 1997--2000  Matthew Newville, The University of Chicago
##  Copyright (c) 1992--1996  Matthew Newville, University of Washington
##
##  Permission to use and redistribute the source code or binary forms of
##  this software and its documentation, with or without modification is
##  hereby granted provided that the above notice of copyright, these
##  terms of use, and the disclaimer of warranty below appear in the
##  source code and documentation, and that none of the names of The
##  University of Chicago, The University of Washington, or the authors
##  appear in advertising or endorsement of works derived from this
##  software without specific prior written permission from all parties.
##
##  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
##  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
##  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
##  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
##  CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
##  TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
##  SOFTWARE OR THE USE OR OTHER DEALINGS IN THIS SOFTWARE.
##
from   Tkinter import *
from   string  import *
from   Ifeffit import Ifeffit
from   TkIfeffit import *
from   DataPlotter import *
import re, os, sys, types
import Pmw, ScrolledText, tkFileDialog, tkColorChooser

from tkFileDialog import askopenfilename
# from Pmw import MessageDialog


class ReadDataFile(BaseWindow,Ifeffit):
    """ ReadDataFile Class:  prompt for ASCII file name, guess array names,
    allow them to be changed, and read in file"""
    def __init__(self,  iff_com=None, master=None):
        if (iff_com == None): iff_com = self.ifeffit
        self.iff_com = iff_com
        self.master  = master
        self.main    = None
        self.input_file = ''
        self.marray  = 32
        self.narray  = 16
        self.array_disp = 7
        self.array   = []
        self.entr    = []
        self.file_type = StringVar()
        self.file_type.set('<from column labels>')
        for i in range(self.marray):
            self.array.append('')
            self.entr.append('')
        self.read()

    def btn_press(self,btn,event=None):
        if ((btn == 'ok') or (btn == 'apply')):
            self.read_cmd = self.read_final_file()
        if ((btn == 'ok') or (btn == 'cancel')): self.main.destroy()
        if (btn == 'newfile'):
            self.main.withdraw()
            self.read(self.master)

    def ftype_choice(self,event=None):
        x = event
        if ((self.input_file == None) or (self.input_file == '')): return
        if (x == '<from column labels>'):
            self.read_temp_file()
        else:
            for i in range(self.marray): self.array[i] = ' '
            if (x == 'xmu'):
                self.array[0] = 'energy'
                self.array[1] = 'xmu'
            elif (x == 'chi'):
                self.array[0] = 'k'
                self.array[1] = 'chi'
            elif (x == 'chi.dat'):
                self.array[0] = 'k'
                self.array[1] = 'chi'
                self.array[2] = 'mag'
                self.array[3] = 'phase'
            elif (x == 'feff.dat'):
                self.array[0] = 'k'
                self.array[1] = 'cphase'
                self.array[2] = 'mag'
                self.array[3] = 'phase'
                self.array[4] = 'redfactor'
                self.array[5] = 'lambda'
                self.array[6] = 'realp'
                self.array_disp = 7
            elif (x == 'rsp'):
                self.array[0] = 'r'
                self.array[1] = 'chi_re'
                self.array[2] = 'chi_im'
                self.array[3] = 'chi_mag'
                self.array[4] = 'chi_phase'
        for i in range(self.array_disp):
            self.entr[i].delete(0,END)
            self.entr[i].insert(0,self.array[i])


    def read(self, master=None, title='Read ASCII Data File'):
        self.input_file = ask_for_file(self.master,
                                       [("data files","*.dat *.chi *.xmu"),
                                        ("all files","*")]   )
        if (master == None):
            self.main = Tk()
        else:
            self.main = Toplevel(master)
        Frame.__init__(self, self.main)
        self.main.title(title)
        self.main.option_add('*font', ('Helvetica', 12))
        if ((self.input_file != None) and (self.input_file != '')):
            self.read_temp_file()
            self.display_arrays()
        else:
            self.main.destroy()


    def display_arrays(self):
        ma  = self.main
        self.balloon = Pmw.Balloon(ma)
        self.menuBar = Pmw.MenuBar(ma, hull_borderwidth=1,
                                   hull_relief = 'raised',
                                   hotkeys=1, balloon = self.balloon)
        self.menuBar.pack(fill='x')
        self.menuBar.addmenu('File', 'Read, Cancel, Exit')
        self.menuBar.addmenuitem('File', 'command',
                                 'Read this file with these arrays',
                                 label='Read',
                                 command= Command(self.btn_press, 'apply'))
        self.menuBar.addmenuitem('File', 'command',
                                 'Do not read this file, Look for a new file',
                                 label='Change File',
                                 command= Command(self.btn_press, 'newfile'))
        self.menuBar.addmenuitem('File', 'command',
                                 'Close without reading these arrays from this file',
                                 label='Cancel',
                                 command= Command(self.btn_press, 'cancel'))
        self.menuBar.addmenuitem('File', 'command',
                                 'Read this file with these arrays, and exit',
                                 label='OK',
                                 command= Command(self.btn_press, 'ok'))

        # file buffer display
        self.f_win = Pmw.ScrolledText(ma,label_text=self.input_file,
                                      borderframe = 1,usehullsize=0,
                                      text_padx = 10, text_pady = 10,
                                      labelpos=NW,
                                      hull_width=650, hull_height=320)
        self.f_win.importfile(self.input_file)
        self.f_win.pack(side='top')
        # group name
        fr_0 = Frame(ma)
        fr_0.pack(side='top',anchor='w')
        fr_1 = Frame(fr_0)
        fr_1.pack(side='left', fill='x',anchor='w')
        fr1  = Frame(fr_1)
        Label(fr1, text='Group: ').pack(side='left')
        self.g_ent   = Entry(fr1,width=20)
        self.g_ent.selection_clear()
        self.g_ent.insert(0,self.group)
        self.g_ent.pack(side='left',fill='x')
        fr1.pack(side='left',fill='x')

        fr_4 = Frame(fr_0)
        fr_4.pack(side='left', fill='x',anchor='w')
        self.file_type.set('<from column labels>')
        self.ftypes = Pmw.OptionMenu(fr_4, labelpos = 'w',
                                     label_text = '  Column File Types: ',
                                     menubutton_textvariable = self.file_type,
                                     menubutton_width = 22,
                                     items  = ['<from column labels>',
                                               'xmu','chi','rsp', 'chi.dat', 'feff.dat'],
                                     initialitem  = '<from column labels>',
                                     command = Command(self.ftype_choice),
                                     )
        #                                          menubutton_width = 18)
        self.ftypes.pack(side='left',fill='x')

        # arrays name
        fr_2 = Frame(ma)
        fr_2.pack(side='top',anchor='w')
        fr2  = Frame(fr_2)
        Label(fr2, text='Arrays: ').pack(side='left')
        for i in range(self.array_disp):
            self.entr[i]  = Entry(fr2, width=10)
            self.entr[i].selection_clear()
            self.entr[i].insert(0,self.array[i])
            self.entr[i].pack(side='left')
        fr2.pack(side='top')

        #
        fr_3 = Frame(ma)
        bbox = Pmw.ButtonBox(fr_3)
        bbox.pack(side='left',fill='both', expand=1)
        b_ok= bbox.add('OK',         command = Command(self.btn_press,'ok'))
        b_rd= bbox.add('Read',       command = Command(self.btn_press,'apply'))
        b_ca= bbox.add('Done',       command = Command(self.btn_press,'cancel'))
        b_nf=bbox.add('Change File', command = Command(self.btn_press,'newfile'))
        bbox.setdefault('Read')
        bbox.alignbuttons()
        self.balloon.bind(b_ok, '',
                    'Read these arrays from this file and exit')
        self.balloon.bind(b_rd, '',
                    'Read these arrays from this file')
        self.balloon.bind(b_ca, '',
                    'Close without reading these arrays from this file')
        self.balloon.bind(b_nf, '',
                    'Do not read these arrays, Look for a new file')
        fr_3.pack()
        self.createMsgWin(ma)

    def read_temp_file(self):
        if ((self.input_file == '') or  (self.input_file == None)): return
        self.ifeffit('read_data(group=_xx_,notitles,type=label,file=%s)'
                % self.input_file)
        col_lab = strip(self.get_string('column_label'))
        if (col_lab == '--undefined--'):
            self.ifeffit('read_data(group=_xx_,notitles,type=raw,file=%s)'
                    % self.input_file)
            col_lab = strip(self.get_string('column_label'))
            if (col_lab == '--undefined--'):
                self.f_win.clear()
                self.f_win.settext(' Invalid File: ' + self.input_file )
                self.group = ' '
                for i in range(self.marray): self.array[i] = ' '
                return
        tmp_lis = split(col_lab)
        s       = os.path.basename(self.input_file)
        x       = split(s,'.')
        self.group = x[0]

        self.narray = min(self.marray,len(tmp_lis))
        for i in range(self.marray): self.array[i] = ' '
        for i in range(self.narray): self.array[i] = tmp_lis[i]
        g = self.group
        # erase temp arrays
        for i in range(self.marray):
            a = self.array[i]
            if (a != ' '): s = self.ifeffit('erase _XX_.%s' % a)

    def read_final_file(self):
        if ((self.input_file=='') or (self.input_file == None)): return
        g = self.g_ent.get()
        c = 'read_data(file=%s, group=%s,label=(' %(self.input_file, g)
        # print '  array size   =  ' , self.narray, self.marray
        for i in range(self.narray):
            d = self.array[i]
            if (i < self.array_disp):  d = self.entr[i].get()
            c = c + d + ' '
        self.iff_com( c + '))')



###############################################################################
##
## GIFEFFIT CLASS
##
###############################################################################
class GIFeffit(BaseWindow,Ifeffit):
    progname    = "G. I. Feffit"
    version     = "0.9"
    copyright   = """ Copyright (c) 2000 Matt Newville \n The University of Chicago"""
    contact     = """ email: newville@cars.uchicago.edu \n web:  http://cars9.uchicago.edu/ifeffit/   """
    def __init__(self, load=None):
        self.root   = Tk()
        # launch Ifeffit in screen_echo = 0 mode
        Ifeffit.__init__(self, screen_echo = 0)
        self.root.option_add('*font', ('Helvetica', 12))
        self.drawsplash(self.root)
        Pmw.initialise(self.root)
        self.root.withdraw()

        self.root.title('G.I.Feffit')
        self.inp_buff  = []  # command buffer
        self.ind_      = 0    # index in inp_buff
        self.plot_opts = self.reset_plot_opts()
        self.prefs     = {}
        self.feff_paths= {}
        self.ask_exit  = 1
        self.iff_com   = self.do_ifeffit
	self.balloon   = Pmw.Balloon(self.root)
        self.inp_buff.append("") ;
        self.createMenubar(self.root)
        self.createMainWin(self.root)
        self.root.mainloop()

    def drawsplash(self,master):
	# Create about dialog.
	Pmw.aboutversion(self.version)
	Pmw.aboutcopyright(self.copyright)
	Pmw.aboutcontact(self.contact)
	self.about = Pmw.AboutDialog(master, applicationname = self.progname)
        self.about.after(2000,self.removesplash)

    def removesplash(self):
        self.root.deiconify()
        self.ifeffit("newplot") 
        self.clear_echo_buffer()
        self.about.withdraw()

    def createMainWin(self,master=None):
        pane    = Pmw.PanedWidget(master,hull_width=700,hull_height=350)
        pane.add('top',min=80,max=800)
        pane.add('bot',min=50,max=60)
        nbFrame = Pmw.NoteBook(pane.pane('top'), 
                               raisecommand= Command(self.nb_raise))

        nbFrame.pack(fill = 'both', expand= 1, padx = 1, pady = 1)

        # Add the "Appearance" page to the notebook.
        cmd_page = nbFrame.add('Command Buffer')
        sca_page = nbFrame.add('Scalars')
        arr_page = nbFrame.add('Arrays')
        str_page = nbFrame.add('Strings')
        mac_page = nbFrame.add('Macros')
        pth_page = nbFrame.add('Feff Paths')
        self.nbframe = nbFrame
        self.balloon.bind(cmd_page, '', 'Ifeffit commands and raw output')
        self.balloon.bind(sca_page, '', 'Ifeffit scalars')
        self.balloon.bind(arr_page, '', 'Ifeffit arrays')
        self.balloon.bind(str_page, '', 'Ifeffit strings')
        self.balloon.bind(mac_page, '', 'Macros')
        self.balloon.bind(pth_page, '', 'Feff Path Definitions')


        self.cmd_win  = ScrolledText.ScrolledText(cmd_page)
        self.cmd_win.configure(width=85, height=40)
        self.cmd_win.tag_configure('output',  foreground='red',font=('Helvetica', 12))
        self.cmd_win.tag_configure('input',   font=('Helvetica', 12))
        self.cmd_win.pack(side='top')

        self.sca_win  = ScrolledText.ScrolledText(sca_page)
        self.sca_win.configure(width=85, height=40, font=('Courier', 12))
        self.sca_win.insert('end', "\n\n")
        self.sca_win.pack(side='top')

        self.arr_win  = ScrolledText.ScrolledText(arr_page)
        self.arr_win.configure(width=85, height=40, font=('Courier', 12))
        self.arr_win.insert('end', "\n\n")
        self.arr_win.pack(side='top')

        self.str_win  = ScrolledText.ScrolledText(str_page)
        self.str_win.configure(width=85, height=40, font=('Courier', 12))
        self.str_win.insert('end', "\n\n")
        self.str_win.pack(side='top')

        self.mac_win  = ScrolledText.ScrolledText(mac_page)
        self.mac_win.configure(width=85, height=40)
        self.mac_win.insert('end', "\n\n")
        self.mac_win.pack(side='top')

        self.pth_win  = ScrolledText.ScrolledText(pth_page)
        self.pth_win.configure(width=85, height=40)
        self.pth_win.insert('end', "\n\n")
        self.pth_win.pack(side='top')

        nbFrame.setnaturalsize()
        self.cmdBox   = Frame(pane.pane('bot'))
        self.cmdFrame = Frame(self.cmdBox)

        Label(self.cmdFrame, text='Ifeffit>').pack(side='left')
        self.cmdBox.pack(side='top', anchor='w')
        self.cmdFrame.pack(anchor='w', side='left',fill='x')
        self.command  = StringVar()
        self.field    = Entry(self.cmdFrame,  width=64)
        self.balloon.bind(self.field, '',
                          'Enter Ifeffit commands or use arrows to scroll through commands')
        self.field['textvariable'] = self.command
        self.field.bind('<Return>',  self.process)
        self.field.bind('<Up>',      self.process)
        self.field.bind('<Down>',    self.process)
        self.field.pack(side='left')
        self.createMsgWin(pane.pane('bot'))
	pane.pack(expand = 1, fill = 'both')


    def createMenubar(self, master):
        self.menuBar = Pmw.MenuBar(master, hull_borderwidth=1,
                              hull_relief = 'raised',
                              hotkeys=1, balloon = self.balloon)
        self.menuBar.pack(fill='x')
        self.menuBar.addmenu('File', 'Read Files or Exit')
        #        self.menuBar.addcascademenu('File', 'Read')
        self.menuBar.addmenuitem('File', 'command',  'Read Data File',
                                 label='Read Data File',
                                 command=Command(self.read_datafile, self.root))
        self.menuBar.addmenuitem('File', 'command',  'Load Command File',
                                 label='Read Command File',
                                 command=Command(self.read_cmndfile, self.root))
        self.menuBar.addmenuitem('File', 'command',
                                 'Restore a saved session from a .sav file',
                                 label='Read Saved Session',
                                 command=Command(self.restore_state, self.root))

        self.menuBar.addmenuitem('File', 'separator')
#        self.menuBar.addcascademenu('File', 'Write')
#         self.menuBar.addmenuitem('Write', 'command', 'Write Data File',
#                                  label='Write Data File',
#                                  command=Command(self.write_datafile, self.root))
        self.menuBar.addmenuitem('File', 'command', 'Write Command File',
                                 label='Write Command File',
                                 command=Command(self.write_cmndfile, self.root))
        self.menuBar.addmenuitem('File', 'command', 'Save session to .sav file',
                                 label='Save session',
                                 command=Command(self.save_state, self.root))
        self.menuBar.addmenuitem('File', 'separator')
        self.menuBar.addmenuitem('File', 'command',  'Close',  label='Exit',
                                 command=Command(self.exit))
                                 
        self.menuBar.addmenu('Help', 'About', side='right')
        self.menuBar.addmenuitem('Help', 'command',  'Get information on application',
                                 label='About...', command=self.help)

#         self.menuBar.addmenu('Edit', 'Edit Text and Preferences')
#         self.menuBar.addmenuitem('Edit', 'command', 'Define and Edit Macros',
#                                  label='Macros',
#                             command=Command(self.macro_editor, self.root))
#         self.menuBar.addmenuitem('Edit', 'command', 'Equaton Editor',
#                                  label='Equation Editor',
#                                  command=Command(self.macro_editor, self.root))
#         self.menuBar.addmenuitem('Edit', 'separator')
#         self.menuBar.addmenuitem('Edit', 'command', 'Plot Preferences',
#                                  label='Plot Preferences',
#                                  command=Command(self.setplot_opts, self.root))
#
#         self.menuBar.addmenuitem('Edit', 'command', 'General Preferences',
#                                  label='General Preferences',
#                                  command=Command(self.set_prefs, self.root))


        self.menuBar.addmenu('Plotting', 'General Data Plotting')
        self.menuBar.addmenuitem('Plotting', 'command',
                                 'General Plot Interface',  label='Plotter',
                                 command=Command(self.plotter, self.root))

        self.menuBar.addmenu('XAFS', 'XAFS Data Analysis ')
        self.menuBar.addmenuitem('XAFS', 'command',
                                 'EXAFS Pre-Edge and Normalization',
                                 label='Pre-Edge',
                                 command=Command(self.pre_edge, self.root))
        self.menuBar.addmenuitem('XAFS', 'command',
                                 'EXAFS Background Subtraction',
                                 label='Background',
                                 command=Command(self.autobk, self.root))
        self.menuBar.addmenuitem('XAFS', 'command',
                                 'EXAFS Fourier Transforms',
                                 label='FFT ',
                                 command=Command(self.fft, self.root))

#         self.menuBar.addmenuitem('Analysis', 'command',
#                                  'Define FEFF Path',
#                                  label='Define FEFF Paths',
#                                  command=Command(self.def_paths, self.root))

    def process(self, event):
        if ( event.keysym == 'Return'):
            cmd = self.command.get()
            self.do_ifeffit(cmd)
        else:
            dir = 0
            if (event.keysym == 'Up'):   dir = -1
            if (event.keysym == 'Down'): dir =  1
            self.ind_ =  self.ind_ + dir
            if (self.ind_ < 1):
                self.ind_  = 0
                self.command.set("")
            elif (self.ind_ == len(self.inp_buff)):
                self.command.set("")
                self.ind_ = self.ind_ - 1
            else:
                self.command.set(self.inp_buff[self.ind_])


    def do_ifeffit(self,cmd="",do_raise=1):
        """execute Ifeffit command in GIFeffit's Notebook motif,
        updating the Command Buffer and currently raised page"""
        ret = 0
        cmd = strip(cmd)
        if (cmd != ""):
            if ((cmd == 'quit') or (cmd =='exit')):
                self.exit(prompt=self.ask_exit)
            elif (cmd == 'pwd'):
                s = os.getcwd()
                self.cmd_win.insert('end'," %s\n" % cmd, 'input')
                self.cmd_win.insert('end'," %s\n" % s,   'output')
                self.command.set("")
                self.cmd_win.see('end')
            elif ((cmd == 'cd') or (cmd[0:3] == 'cd ')):
                try:
                    os.chdir(cmd[3:])
                except OSError:
                    pass
                s = os.getcwd()
                self.cmd_win.insert('end'," %s\n" % cmd, 'input')
                self.cmd_win.insert('end'," %s\n" % s,   'output')
                self.command.set("")
                self.cmd_win.see('end')
            elif ((cmd[0:3] == 'ls ') or (cmd[0:4] == 'dir ') or
                  (cmd == 'ls') or (cmd == 'dir')):
                self.cmd_win.insert('end'," %s\n" % cmd, 'input')
                t = os.getcwd()
                self.cmd_win.insert('end'," %s:\n" % t,   'output')
                s = os.listdir(t)
                s.sort()
                out = ''
                for i in range(len(s)):
                    lsi = len(s[i])
                    if (lsi < 20):
                        out = out + s[i] + ' '*(22 - lsi)
                    else:
                        out = out + s[i] + '  '
                    if (len(out) > 71):
                        self.cmd_win.insert('end'," %s\n" % out,   'output')
                        out = ''
                if (len(out) > 1):
                    self.cmd_win.insert('end'," %s\n" % out,   'output')
                    out = ''
                self.command.set("")
                self.cmd_win.see('end')
            elif (cmd[0:1] == '!'):
                os.system(cmd[1:])
                self.command.set("")
            else:
                ret = self.ifeffit(cmd)
                self.inp_buff.append(cmd)
                self.cmd_win.insert('end'," %s\n" % cmd, 'input')
                self.command.set("")
                self.cmd_win.see('end')
                self.ind_ = len(self.inp_buff)
                self.add_echo_lines(self.cmd_win, erase=0)
            # update the currently raised window
        if (do_raise == 1):
            raised = self.nbframe.getcurselection()
            self.nb_raise(raised)
        return ret

    def nb_raise(self,page):
        if   (page == 'Arrays'):     self.arrays_update()
        elif (page == 'Scalars'):    self.scalars_update()
        elif (page == 'Strings'):    self.strings_update()
        elif (page == 'Macros'):     self.macros_update()
        elif (page == 'Feff Paths'): self.paths_update()

    def arrays_update(self):
        self.clear_echo_buffer()
        self.ifeffit("show @arrays")
        self.add_echo_lines(self.arr_win, erase=1)

    def scalars_update(self):
        self.clear_echo_buffer()
        self.ifeffit("show @scalars")
        self.add_echo_lines(self.sca_win, erase=1)

    def strings_update(self):
        self.clear_echo_buffer()
        self.ifeffit("show @strings")
        self.add_echo_lines(self.str_win, erase=1)

    def macros_update(self):
        self.clear_echo_buffer()
        self.ifeffit("show @macros")
        self.mac_win.delete(0.0, self.mac_win.index('end') )
        self.mac_win.tag_configure('desc',    foreground='darkgreen', font=('Helvetica', 12))
        self.mac_win.tag_configure('args',    foreground='red',  font=('Helvetica', 12))
        nmac    = 0
        macargs = []
        macdesc = []
        buff    = self.get_echo_buffer()
        for s in buff:
            if (s[0:5] == 'macro'):
                sx   = re.split(r',*\s+', strip(s[6:]))
                macargs.append(sx)
                macdesc.append("")
                nmac = nmac + 1
            else:
                if (nmac > 0): macdesc[nmac-1] = s

        for i in range(nmac):
            self.mac_win.insert('end', "macro %s" % macargs[i][0])
            self.mac_win.insert('end', " ")
            for j in range(1,len(macargs[i])):
                self.mac_win.insert('end', " %s" % macargs[i][j], 'args')
            self.mac_win.insert('end', "\n")
            if (macdesc[i] != ""):  self.mac_win.insert('end', "  %s\n" % macdesc[i], 'desc')
            self.ifeffit("show %s " % macargs[i][0])
            skip = 1
            if (macdesc[i] != ""): skip = 2
            buff = self.get_echo_buffer()
            i  = 0
            for s in buff:
                i = i  + 1
                if ((i > skip) and (s != 'end macro')):
                    self.mac_win.insert('end', "  %s\n" % s)
            self.mac_win.see('end')

    def paths_update(self):
        self.clear_echo_buffer()
        self.ifeffit("show @paths")
        self.add_echo_lines(self.pth_win, erase=1)

    def add_echo_lines(self, win, erase=1):
        if (erase == 1):  win.delete(0.0, win.index('end') )
        buff =  self.get_echo_buffer()
        for s in buff:
            if (win == self.cmd_win):
                win.insert('end', "# %s\n" % s, 'output')
            elif ((win == self.sca_win) and (s[0:1] == '&')):
                pass
            elif ((win == self.str_win) and (s[0:2] == '$&')):
                pass
            else:
                win.insert('end', " %s\n" % s)
        win.see('end')

    def read_datafile(self, master):
        ReadDataFile(iff_com=self.do_ifeffit, master=master)

    def read_cmndfile(self, master):
        ReadCmndFile(iff_com=self.do_ifeffit, master=master)

    def write_datafile(self, master):
        WriteDataFile(master=master)

    def write_cmndfile(self, master):
        WriteCmndFile(master=master, buffer = self.inp_buff)

    def save_state(self, master):
        xfile = tkFileDialog.asksaveasfilename(
            filetypes=[("save files","*.sav"),
                       ("all files","*")],
            initialfile = 'ifeffit.sav' , parent=master)
        xfile = trim_cwd(xfile)
        if (xfile != ''):
            self.do_ifeffit("save(file=\"%s\")" % xfile)

    def restore_state(self, master):
        xfile = ""
        xfile = tkFileDialog.askopenfilename(
            filetypes=[("save files","*.sav"),
                       ("all files","*")], parent=master)
        xfile = trim_cwd(xfile)
        if (xfile != ''):
            self.do_ifeffit("restore(file=\"%s\")" % xfile)

    def plotter(self,master):
        DataPlotter(iff_com = self.do_ifeffit,
                    master=master,  plot_opts= self.plot_opts)

    def set_prefs(self, master):
        SetPrefs(master=master, prefs = self.prefs)

    def pre_edge(self,master):
        PreEdge(iff_com = self.do_ifeffit, master=master)

    def autobk(self,master):
        Spline(iff_com = self.do_ifeffit, master=master)

    def fft(self,master):
        Ifft(iff_com = self.do_ifeffit, master=master)

    def def_paths(self,master):
        Paths(iff_com = self.do_ifeffit, master=master,paths=self.feff_paths)

    def macro_editor(self,master):
        pass

    def exit(self,prompt=1): 
        res = 'Yes'
        if (prompt == 1):
            d = Pmw.MessageDialog(self.root,
                                  title = 'Save Ifeffit Session?',
                                  defaultbutton = 0,
                                  buttons = ('Yes', 'No','Cancel'),
                                  message_text =
                                  'Save current state of Ifeffit before Exit?')
            res = d.activate()
            if (res == 'Yes'):
                u = self.save_state(self.root)
                if (u == None): res = 'Cancel'
        if (res != 'Cancel'):
            self.root.destroy()
            sys.exit(0)

    def help(self):
	self.about.show()

if (__name__ == '__main__'):
    GIFeffit(load=sys.argv[1:])
