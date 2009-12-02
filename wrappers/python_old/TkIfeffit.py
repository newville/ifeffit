#!/usr/bin/python
###!/usr/bin/python
##
##  TkIfeffit: Tk widgets for GIFEFFIT
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
import re, os, sys, types
import Pmw, ScrolledText, tkFileDialog, tkColorChooser

def trim_cwd(f):
    "trim current working directory from a file name"
    cwd = os.getcwd()
    i   = find(f, cwd)
    out = f
    if (i == 0): out = strip(f[len(cwd)+1:len(f)])
    return out

def ask_for_file(parent,defaults):
    "use file dialog to ask for file, and chdir to that dir"
    f = ' '
    f = tkFileDialog.askopenfilename(
        filetypes=defaults, parent=parent)
    if (len(f) > 0):
        os.chdir(os.path.dirname(f))
    u = trim_cwd(os.path.basename(f))
    return u

def entry_replace(ent, val):
    "replace Entry text with val"
    ent.delete(0,len(ent.get()))
    ent.insert(0,val)


def LabEnt(frame, label, value, width,labwidth=None):
    if (labwidth == None):
        Label(frame,  text=label).pack(side='left',anchor='w', fill='x')
    else:
        Label(frame, width=labwidth, anchor='w',
              text=label).pack(side='left',anchor='w', fill='x')
    wid = Entry(frame, width=width)
    entry_replace(wid,value)
    wid.pack(side='left',anchor='w', fill='x')
    return wid

class Command:
    " Generic Command execution (simpler than lambdas)"
    def __init__(self, func, *args, **kw):
        self.func = func
        self.args = args
        self.kw   = kw
    def __call__(self, *args, **kw):
        args = self.args + args
        kw.update(self.kw)
        apply(self.func,args, kw)

class BaseWindow(Frame):
    def MakeMainWindow(self,master):
        if (master == None):
            self.main = Tk()
        else:
            self.main = Toplevel(master)
        Frame.__init__(self, self.main)
        ma  = self.main
        self.main.title(self.window_title)
        self.main.withdraw()
        self.balloon = Pmw.Balloon(ma)
        self.menuBar = Pmw.MenuBar(ma, hull_borderwidth=1,
                                   hull_relief = 'raised',
                                   hotkeys=1, balloon = self.balloon)
        self.menuBar.pack(fill='x')
        self.menuBar.addmenu('File',    'Close')
        self.menuBar.addmenuitem('File', 'command',
                                 'Close This Window',
                                 label='Close',
                                 command= Command(self.btn_press, 'ok'))
        return self.main

    def btn_press(self,btn,event=None,ok_action=None,extra=None):
        ret = None
        if ((btn == 'ok') or (btn == 'apply')):
            if ((ok_action != None) and (callable(ok_action) == 1)):
                ret  = ok_action()
        if ((btn == 'ok') or (btn == 'cancel')): self.main.destroy()
        if (btn == 'calc'):
            self.do_calc()
            self.update_params(self.param_map)
            self.update_plot(None)
        return ret

    def createMsgWin(self, m,width=65):
        self.mesFrame = Frame(m)
        self.mesFrame.pack(side='bottom', fill='x')
	self.statusBar = Pmw.MessageBar(self.mesFrame, entry_width = width,
                                        entry_relief='groove',
                                        labelpos = 'w', label_text = '')
	self.statusBar.pack(side='left', fill='x')
	self.balloon.configure(statuscommand = self.statusBar.helpmessage,
                               state = 'status')

    def update_params(self,params):
        for i in range(len(params)):
            r = self.get_scalar(params[i][0])
            entry_replace(self.wid[params[i][1]], r)

    def update_array_choice(self, type, selection, get_group=1):
        # print ' BASE update_array_choice: ', type, selection
        if (get_group == 1):
            self.arrays  = self.get_group_arrays()
        self.groups  = ['None'] + self.arrays.keys()
        self.wid['group'].setlist(self.groups)
        if (type == 'group'):
            tmpx = ['','']
            ix = 0
            iy = 1
            if (selection != 'None'):
                tmpx =  self.arrays[selection]['names']
                for i in range(len(tmpx)):
                    if (tmpx[i] == self.defs['x']):  ix  = i
                for i in range(len(tmpx)):
                    if (tmpx[i] == self.defs['y']):  iy  = i
            self.wid['x'].setlist(tmpx)
            self.wid['y'].setlist(tmpx)
            self.wid['x'].selectitem(tmpx[ix])
            self.wid['y'].selectitem(tmpx[iy])
            self.update_array_choice('x',tmpx[ix], get_group=0)
        elif (type == 'x'):
            group = self.wid['group'].get()
            if ((group != 'None') and (selection != '')):
                innx  = self.arrays[group]['names'].index(selection)
                npts  = self.arrays[group]['npts'][innx]
                tmpx = []
                for i in range(len(self.arrays[group]['npts'])):
                    if (self.arrays[group]['npts'][i] == npts):
                        tmpx.append(self.arrays[group]['names'][i])
                self.wid['y'].setlist(tmpx)
                iy = 1
                for i in range(len(tmpx)):
                    if (tmpx[i] == self.defs['y']):  iy  = i
                self.wid['y'].selectitem(tmpx[iy])


class SetPrefs(BaseWindow):
    """ SetPrefs Class:  select overall preferences"""
    def __init__(self,  buffer=None,master=None):
        self.master  = master
        self.main    = None
        self.buffer  = []
        if (buffer != None):  self.buffer = buffer
        self.buflen  = len(self.buffer)
        self.comfile = 'gifeffit.iff'
        self.cwin    = 0
        if (master == None):
            self.main = Tk()
        else:
            self.main = Toplevel(master)
        Frame.__init__(self, self.main)
        self.main.title('G. I. Feffit Preferences')
        self.main.option_add('*font', ('Helvetica', 12))
        self.display()

    def display(self):
        print " Well, maybe someday"


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


class ReadCmndFile(BaseWindow,Ifeffit):
    """ ReadCmndFile Class:  prompt for IFeffit Command File name,
    allow complete loading, or stepping through execution"""
    def __init__(self,  iff_com=None, master=None):
        if (iff_com == None): iff_com = self.ifeffit
        self.iff_com = iff_com
        self.master  = master
        self.main    = None
        self.comfile = ''
        self.lineno  = 0
        self.buffer  = []
        self.buflen  = 0
        self.cwin    = 0
        self.done    = 0
        self.read()

    def read(self, master=None, title='Read IFEFFIT Command File'):
        self.comfile = ask_for_file(self.master,
                                    [("command files","*.iff *.inp"),
                                     ("all files","*")] )
        if (self.comfile != ''):
            self.lineno = 0
            self.lineno  = 0
            self.buffer  = []
            f = open (self.comfile, 'r')
            self.buffer = f.readlines()
            self.buffer.append("")
            self.buflen = len(self.buffer)
            f.close

        if (master == None):
            self.main = Tk()
        else:
            self.main = Toplevel(master)
        Frame.__init__(self, self.main)
        self.main.title(title)
        self.main.option_add('*font', ('Helvetica', 12))
        if ((self.comfile != None) and (self.comfile != '')):
            self.display_comfile()
        else:
            self.main.destroy()


    def display_comfile(self):
        ma  = self.main
        self.balloon = Pmw.Balloon(ma)
        self.menuBar = Pmw.MenuBar(ma, hull_borderwidth=1,
                                   hull_relief = 'raised',
                                   hotkeys=1, balloon = self.balloon)
        self.menuBar.pack(fill='x')
        self.menuBar.addmenu('File', 'Read, Cancel, Exit')
        self.menuBar.addmenuitem('File', 'command',
                                 'Read this file with these arrays, and exit',
                                 label='Load and Exit',
                                 command= Command(self.btn_press, 'ok'))
        self.menuBar.addmenuitem('File', 'command',
                                 'Read this file with these arrays',
                                 label='Load',
                                 command= Command(self.btn_press, 'apply'))
        self.menuBar.addmenuitem('File', 'command',
                                 'Do not read this file, Look for a new file',
                                 label='Change File',
                                 command= Command(self.btn_press, 'newfile'))
        self.menuBar.addmenuitem('File', 'command',
                                 'Close without reading these arrays from this file',
                                 label='Cancel',
                                 command= Command(self.btn_press, 'cancel'))
        # file buffer display
        self.cwin = Pmw.ScrolledText(ma,label_text=self.comfile,
                                     borderframe = 1,usehullsize=0,
                                     text_padx = 10, labelpos=NW,
                                     hull_width=600, hull_height=320)
        self.cwin.clear()
        self.cwin.tag_configure('comment',foreground='red',  font=('Helvetica', 12))
        self.cwin.tag_configure('command',foreground='black',font=('Helvetica', 12))
        self.cwin.tag_configure('active', foreground='blue', font=('Helvetica', 12))
        new = 1
        for i in range(self.buflen):
            l = re.sub(r'\r\n$', '\n', self.buffer[i])
            t  = strip(l[:-1])
            if (len(t) < 1): t = " "
            words = split(t)
            if (len(words) < 2): words = [" ", " "]
            if ((t[0:1] == '#') or (t[0:1] == '%') or (t[0:1] == ';') or
                (words[0] == 'pause')):
                self.cwin.insert('end',l,'comment')
            else:
                if ((new == 1) and (t != ' ') and (t != '')):
                    new = 0
                    self.cwin.insert('end',l,'active')
                    self.lineno = i+1
                else:
                    self.cwin.insert('end',l,'command')
        self.cwin._textbox.configure(state='disabled')
        self.cwin.pack(side='top')
        fr_3 = Frame(ma)
        bbox = Pmw.ButtonBox(fr_3)
        bbox.pack(side='left',fill='both', expand=1)
        b_ok= bbox.add('Load All',    command = Command(self.btn_press,'ok'))
        b_ss= bbox.add('Single Step', command = Command(self.btn_press,'onestep'))
        b_sk= bbox.add('Skip Line',   command = Command(self.btn_press,'skip'))
        b_ca= bbox.add('Done',        command = Command(self.btn_press,'cancel'))
        b_nf=bbox.add('Change File',  command = Command(self.btn_press,'newfile'))
        bbox.setdefault('Load All')
        bbox.alignbuttons()
        self.balloon.bind(b_ok, '', 'Load this entire file')
        self.balloon.bind(b_ss, '', 'Execute next line (highlighted)')
        self.balloon.bind(b_sk, '', 'Skip highlighted line, go to next line')
        self.balloon.bind(b_ca, '', 'Finished loading input commands - Quit now')
        self.balloon.bind(b_nf, '', 'Look for a new command file')
        fr_3.pack()
        self.createMsgWin(ma)

    def btn_press(self,btn,event=None):
        if ((btn == 'ok') or (btn == 'apply')):  self.load_all()
        if ((btn == 'ok') or (btn == 'cancel')): self.main.destroy()
        if (btn == 'newfile'):
            self.main.withdraw()
            self.read(self.master)
            self.done =0
        elif (btn == 'onestep'):
            self.exec_nextline()
        elif (btn == 'skip'):
            self.exec_nextline(skip=1)

    def exec_nextline(self,skip=0):
        if (self.done == 1): return
        result = -1
        while (result !=0):
            index1  = '%i.0' % (self.lineno)
            index2  = '%i.0' % (self.lineno+1)
            s      = self.cwin.get(index1,index2)
            t      = strip(s[:-1])
            result = 0
            if ((t != '') and (t != ' ') and (skip == 0)):
                result = self.iff_com(s[:-1],do_raise=0)
            self.cwin._textbox.configure(state='normal')
            self.cwin.delete(index1,index2)
            self.cwin.insert(index1, s, 'command')
            if (self.lineno == self.buflen-1):
                self.done = 1
            else:
                for i in range (self.lineno, self.buflen):
                    l = re.sub(r'\r\n$', '\n', self.buffer[i])
                    t  = strip(l[:-1])
                    if (len(t) < 1): t = ' '
                    words = split(t)
                    if (len(words) < 2): words = [" ", " "]
                    if ((t[0:1] == '#') or (t[0:1] == '%') or (t[0:1] == ';') 
                        or (words[0] == 'pause')):
                        pass
                    elif (i == self.buflen-1):
                        self.lineno = i
                        self.done   = 1
                    elif ((t != '') and (t[0:1] != ' ')):
                        self.lineno = i+1
                        index1  = '%i.0' % (self.lineno)
                        index2  = '%i.0' % (self.lineno+1)
                        s = self.cwin.get(index1,index2)
                        self.cwin.delete(index1,index2)
                        self.cwin.insert(index1, s, 'active')
                        self.cwin.see( '%i.0' % (self.lineno + 6 ))
                        break
            self.cwin._textbox.configure(state='disabled')
        #
        self.iff_com("",do_raise=1)

    def load_all(self):
        if (self.comfile != None):
            self.done = 0
            for i in range(self.lineno,self.buflen):
                self.exec_nextline()


class WriteCmndFile(BaseWindow):
    """ WriteCmndFile Class:  display accumulated IFeffit Commands,
    allow editing, and saving to file"""
    def __init__(self,  buffer=None,master=None):
        self.master  = master
        self.main    = None
        self.buffer  = []
        if (buffer != None):  self.buffer = buffer
        self.buflen  = len(self.buffer)
        self.comfile = 'gifeffit.iff'
        self.cwin    = 0
        if (master == None):
            self.main = Tk()
        else:
            self.main = Toplevel(master)
        Frame.__init__(self, self.main)
        self.main.title('Save Command File')
        self.main.option_add('*font', ('Helvetica', 12))
        self.display()

    def btn_press(self,btn,event=None):
        if ((btn == 'ok') or (btn == 'apply')):  self.write_file()
        if ((btn == 'ok') or (btn == 'cancel')): self.main.destroy()

    def write_file(self):
        self.comfile = self.ent.get()
        f = open(self.comfile, 'w')
        t = self.cwin.get()
        f.write(t)
        f.close()

    def display(self):
        ma  = self.main
        self.balloon = Pmw.Balloon(ma)
        self.menuBar = Pmw.MenuBar(ma, hull_borderwidth=1,
                                   hull_relief = 'raised',
                                   hotkeys=1, balloon = self.balloon)
        self.menuBar.pack(fill='x')
        self.menuBar.addmenu('File', 'Read, Cancel, Exit')
        self.menuBar.addmenuitem('File', 'command', 'Save Commands to named file',
                                 label='Save',
                                 command= Command(self.btn_press, 'apply'))
        self.menuBar.addmenuitem('File', 'command', 'Do not save to file',
                                 label='Cancel',
                                 command= Command(self.btn_press, 'cancel'))
        self.cwin = Pmw.ScrolledText(ma,label_text='Commands to Save (editable)',
                                      borderframe = 1,usehullsize=0,
                                      text_padx = 10, labelpos=NW,
                                      hull_width=600, hull_height=320)
        self.cwin.clear()
        self.cwin.tag_configure('normal', foreground='black', font=('Helvetica', 12))
        for i in range(self.buflen):
            l = strip(self.buffer[i])
            if (l != ''): self.cwin.insert('end',l+"\n",'normal')

        self.cwin.pack(side='top')
        fr1 = Frame(ma)
        Label(fr1, text='File: ').pack(side='left')
        self.ent   = Entry(fr1,width=50)
        self.ent.selection_clear()
        self.ent.insert(0,self.comfile)
        self.ent.pack(side='left',fill='x')
        self.balloon.bind(self.ent, '', 'File Name')
        fr1.pack(side='top', anchor='w')
        fr1 = Frame(ma)
        bbox = Pmw.ButtonBox(fr1)
        bbox.pack(side='top',fill='both', expand=1,anchor='w')
        b_ok= bbox.add('Save',    command = Command(self.btn_press,'ok'))
        b_ca= bbox.add('Cancel',  command = Command(self.btn_press,'cancel'))
        bbox.setdefault('Save')
        bbox.alignbuttons()
        self.balloon.bind(b_ok, '', 'Save to file')
        self.balloon.bind(b_ca, '', 'Do not save to file - Quit now')
        fr1.pack(side='top', anchor='w')
        self.createMsgWin(ma)



################################################################################




class Ifft(BaseWindow,Ifeffit):
    " FFT Interface "
    window_title = "FFT Window"
    do_btn_label = 'Calculate FFT'
    do_btn_hint  = 'Calculate chi(R)'
    xa_label     = 'k'
    ya_label     = 'chi'
    def __init__(self,  master=None, iff_com=None):
        if (iff_com   == None): iff_com = self.ifeffit
        self.iff_com = iff_com
        self.master  = master
        self.opts    = {}
        self.wid     = {'x':0, 'y':0, 'group':0}
        self.params  = {'dk':1.0, 'kmin':0, 'kmax':0,  'kw':1}
        self.defs    = {'x':'k','y':'chi'}
        self.w_types = ('Hanning', 'Hanning Fraction',
                        'Kaiser-Bessel', 'Parzen',
                        'Welch', 'Sine')
        self.w_keys  = ('hann','fhan','kais','parz','welc','sine')
        self.plot_types = ('chi(k)', 'k-weighted chi(k)',
                           'k-weighted chi(k) with window(k)',
                           '|chi(R)|', 'Im[chi(R)]', 'Re[chi(R)]',
                           '|chi(R)| + Im[chi(R)]',
                           '|chi(R)| + Re[chi(R)]' )
        self.arrays  = self.get_group_arrays()
        self.groups  = ['None'] + self.arrays.keys()
        self.param_map = (('dk1', 'dk'),  ('kweight', 'kw'),
                          ('kmin', 'kmin'),('kmax', 'kmax'))
        self.drawWindow(master)


    def do_calc(self):
        (gr,xa,ya)    = (self.wid['group'].get(),
                         self.wid['x'].get(),
                         self.wid['y'].get() )
        (dk,kw,kn,kx) = (self.wid['dk'].get(),
                         self.wid['kw'].get(),
                         self.wid['kmin'].get(),
                         self.wid['kmax'].get() )
        wtype  =  self.wid['wtype'].get()
        for i in range(len(self.w_types)):
            if (wtype == self.w_types[i]): wkey  = self.w_keys[i]


        # fe0 =  self.params['find_e0'].get()
        if ((xa != '') and (ya != '')):
            cmnd = 'fftf(real=%s.%s' % (gr,ya)
            cmnd = '%s,kweight=%s,kmin=%s,kmax=%s,dk=%s' % (cmnd,kw,kn,kx,dk)
            cmnd = '%s,kwindow="%s")' % (cmnd,wkey)
            self.iff_com(cmnd)
            self.iff_com('set %s.chi_kw = %s.%s * %s.%s^kweight'
                         % (gr,gr,ya,gr,xa))


    def update_plot(self,selection):
        if (selection == None):  selection = self.wid['plot'].get()
        (gr,xa,ya,kw)  = (self.wid['group'].get(),
                          self.wid['x'].get(),
                          self.wid['y'].get(),
                          self.wid['kw'].get() )
        if (gr != 'None'):
            kw   = float(kw)
            lab  = 'xlabel="R (\A)",ylabel="|\gx(R)| (\A\u-%s\d)"'  % (int(kw+1))
            cmnd = 'plot(%s.r,%s.chir_mag)' % (gr,gr)
            if  (selection == self.plot_types[0]):
                lab  = 'xlabel="k (\A\u-1\d)",ylabel="\gx(k)"'
                cmnd = 'newplot(%s.%s,%s.%s,%s)' % (gr,xa,gr,ya,lab)
            elif (selection == self.plot_types[1]):
                lab  = 'xlabel="k (\A\u-1\d)",ylabel="k\u%s\d\gx(k)"' % (int(kw))
                cmnd = 'newplot(%s.%s,%s.chi_kw,%s)' % (gr,xa,gr,lab)
            elif (selection == self.plot_types[2]):
                lab  = 'xlabel="k (\A\u-1\d)",ylabel="k\u%s\d\gx(k)"' % (int(kw))
                cmnd = 'newplot(%s.k,%s.chi_kw,%s)' % (gr,gr,lab)
                self.iff_com(cmnd)
                self.ifeffit("_xmax_ = ceil(abs(%s.chi_kw))" % gr)
                xmax  = self.get_scalar("_xmax_")
                xmax  = (int(xmax*100) + 2) /100.
                self.ifeffit("erase _xmax_")
                cmnd  =  'plot(%s.%s,"%s*%s.win")' % (gr,xa,xmax,gr)
            elif (selection == self.plot_types[3]):
                cmnd = 'newplot(%s.r,%s.chir_mag,%s)' % (gr,gr,lab)
            elif (selection == self.plot_types[4]):
                lab  = 'xlabel="R (\A)",ylabel="Im[\gx(R)] (\A\u-%s\d)"' % (int(kw+1))
                cmnd = 'newplot(%s.r,%s.chir_im,%s)' % (gr,gr,lab)
            elif (selection == self.plot_types[5]):
                lab  = 'xlabel="R (\A)",ylabel="Re[\gx(R)] (\A\u-%s\d)"' % (int(kw+1))
                cmnd = 'newplot(%s.r,%s.chir_re,%s)' % (gr,gr,lab)
            elif (selection == self.plot_types[6]):
                cmnd = 'newplot(%s.r,%s.chir_mag,%s)' % (gr,gr,lab)
                self.iff_com(cmnd)
                cmnd = 'plot(%s.r,%s.chir_im)' % (gr,gr)
            elif (selection == self.plot_types[7]):
                cmnd = 'newplot(%s.r,%s.chir_mag,%s)' % (gr,gr,lab)
                self.iff_com(cmnd)
                cmnd = 'plot(%s.r,%s.chir_re)' % (gr,gr)
            self.iff_com(cmnd)


    def drawWindow(self,master):
        self.main = self.MakeMainWindow(master)
        ma  = self.main
        #
        frame1  = Frame(ma,   borderwidth=3, relief='ridge')
        fr1     = Frame(frame1)
        fr2     = Frame(fr1)
        Label(fr2, foreground='red',  width=15,
              text='Data Arrays   ').pack(side='left',
                                          anchor='w',fill='both')
        fr2.pack(side='left', anchor='w',fill='x')
        fr2     = Frame(fr1)
        # Group
        Label(fr2, foreground='black', width=7,
              text='Group').pack(side='left', anchor='e',fill='both')

        self.wid['group'] = Pmw.ComboBox(fr2, scrolledlist_items= self.groups,
                                         entry_width = 12, entry_state="disabled",
                                         selectioncommand =
                                         Command(self.update_array_choice,'group',get_group=1))

        self.wid['group'].pack(side='left',anchor='e',fill='x')
        fr2.pack(side='right', anchor='w', fill='both')
        fr1.pack(side='top', anchor='w', fill='both')

        fr1     = Frame(frame1)
        fr2     = Frame(fr1)
        # X array
        Label(fr2, foreground='black',  width=7,
              text= self.xa_label).pack(side='left',anchor='w',fill='x')
        self.wid['x']  = Pmw.ComboBox(fr2, scrolledlist_items= self.groups,
                                         entry_width = 12, entry_state="disabled",
                                         selectioncommand =
                                         Command(self.update_array_choice,'x',get_group=1))
        self.wid['x'].pack(side='left',anchor='w',fill='x')
        # Y array
        Label(fr2, foreground='black',  width=7,
              text=self.ya_label).pack(side='left',anchor='w',fill='x')
        self.wid['y']  = Pmw.ComboBox(fr2, scrolledlist_items= self.groups,
                                         entry_width = 12, entry_state="disabled",
                                         selectioncommand =
                                         Command(self.update_array_choice,'y',get_group=1))
        self.wid['y'].pack(side='left',fill='x')
        fr2.pack(side='right', anchor='w',fill='both')
        fr1.pack(side='top',   anchor='w', fill='both')
        frame1.pack(side='top', anchor='w',fill='both')


        self.wid['group'].setlist(self.groups)
        self.wid['group'].selectitem(self.groups[0])
        if (len(self.groups) > 1):
            self.wid['group'].selectitem(self.groups[1])
            self.update_array_choice('group',self.groups[1],get_group=0)

        ## Analysis Params
        frame1  = Frame(ma,   borderwidth=3, relief='ridge')
        fr1     = Frame(frame1)
        Label(fr1, foreground='red',
              text='Analysis Parameters').pack(side='left', anchor='w',fill='both')
        fr1.pack(side='top', anchor='w',fill='x')

        # K min / K max
        fr1    = Frame(frame1)
        fr2    = Frame(fr1)
        self.wid['kmin'] = LabEnt(fr2,'k min ',  self.params['kmin'],  12, labwidth=8)
        self.wid['kmax'] = LabEnt(fr2,' k max',  self.params['kmax'],  12, labwidth=8)
        self.wid['kmin'].pack(side='left', anchor='w')
        self.wid['kmax'].pack(side='left')
        fr2.pack(side='left', anchor='w',fill='x')
        fr1.pack(side='top', anchor='w',fill='x')

        # dk /  kweight
        fr1    = Frame(frame1)
        fr2    = Frame(fr1)
        self.wid['kw']  = LabEnt(fr2,'k weight', self.params['kw'],12,
                                  labwidth=8)
        self.wid['kw'].pack(side='left')
        self.wid['dk']  = LabEnt(fr2,'  dk',   self.params['dk'],  12,
                                 labwidth=8)
        self.wid['dk'].pack(side='left', anchor='w')
        fr2.pack(side='left', anchor='w',fill='x')
        fr1.pack(side='top', anchor='w',fill='x')

        # window type
        fr1    = Frame(frame1)
        fr2    = Frame(fr1)
        self.wid['wtype'] = Pmw.ComboBox(fr1, scrolledlist_items= self.w_types,
                                         entry_width = 20, entry_state="disabled")
        self.wid['wtype'].pack(side='left',anchor='e',fill='x')
        self.wid['wtype'].selectitem(self.w_types[0])


        fr2.pack(side='left', anchor='w',fill='x')
        fr1.pack(side='top', anchor='w',fill='x')


        self.balloon.bind(self.wid['kmin'], '',
                          'Minimum k value for FT Window')
        self.balloon.bind(self.wid['kmax'], '',
                          'Maximum k value for FT Window')
        self.balloon.bind(self.wid['kw'], '',
                          'k-weight for FT')
        self.balloon.bind(self.wid['dk'], '',
                          'k-window parameter (window "sill" size)')
        self.balloon.bind(self.wid['wtype'], '',
                          'Functional form for FT window')
        fr2.pack(side='left', anchor='w',fill='x')
        fr1.pack(side='top', anchor='w',fill='x')

        frame1.pack(side='top',anchor='w',fill='both')

        ## Plot Frame
        frame1  = Frame(ma)
        fr1     = Frame(frame1)
        Label(fr1, foreground='red',
              text='Plot to Show').pack(side='left', anchor='w',fill='both')
        fr1.pack(side='top', anchor='w',fill='x')

        # Droplist
        fr1    = Frame(frame1)
        self.wid['plot'] = Pmw.ComboBox(fr1, scrolledlist_items= self.plot_types,
                                     entry_width = 40, entry_state="disabled",
                                     selectioncommand =
                                     Command(self.update_plot))
        self.wid['plot'].pack(side='left',anchor='e',fill='x')
        self.wid['plot'].selectitem(self.plot_types[3])
        fr1.pack(side='left',anchor='w',fill='both')
        frame1.pack(side='top',anchor='w',fill='both')


        # bottom
        fr3    = Frame(ma)
        bbox   = Pmw.ButtonBox(fr3)
        bbox.pack(side='left',fill='both', expand=1)
        b_calc = bbox.add(self.do_btn_label,
                          command= Command(self.btn_press,'calc'))
        b_quit = bbox.add('Quit', command = Command(self.btn_press,'ok'))
        bbox.setdefault(self.do_btn_label)
        bbox.alignbuttons()
        self.balloon.bind(b_calc, '',self.do_btn_hint)
        self.balloon.bind(b_quit, '', 'Close This Window')
        fr3.pack(side='top',anchor='w')
        self.createMsgWin(ma,width=50)
        self.update_params(self.param_map)
        self.main.deiconify()



class Spline(BaseWindow,Ifeffit):
    " Spline Interface "
    window_title = "Spline Window"
    do_btn_label = 'Calculate Spline'
    do_btn_hint  = 'Calculate background and chi(k)'
    xa_label     = 'energy'
    ya_label     = 'mu'
    def __init__(self,  master=None, iff_com=None):
        if (iff_com   == None): iff_com = self.ifeffit
        self.iff_com = iff_com
        self.master  = master
        self.opts    = {}
        self.wid     = {'x':0, 'y':0, 'group':0}
        self.params  = {'rbkg':1.0, 'kmin':0, 'kmax':0, 'e0':0,
                        'kw':1, 'find_e0':0}
        self.defs    = {'x':'energy','y':'xmu'}
        self.plot_types = ('xmu(E)', 'xmu with bkg(E)',
                           'chi(k)', 'k-weighted chi(k)')
        self.arrays  = self.get_group_arrays()
        self.groups  = ['None'] + self.arrays.keys()

        self.param_map = (('rbkg', 'rbkg'),  ('e0','e0'),
                          ('kweight_spl', 'kw'),
                          ('kmin_spl', 'kmin'),
                          ('kmax_spl', 'kmax'))

        self.drawWindow(master)


    def do_calc(self):
        (gr,xa,ya)       = (self.wid['group'].get(),
                            self.wid['x'].get(),
                            self.wid['y'].get() )
        (rb,ee,kw,kn,kx) = (self.wid['rbkg'].get(),
                            self.wid['e0'].get(),
                            self.wid['kw'].get(),
                            self.wid['kmin'].get(),
                            self.wid['kmax'].get() )
        fe0 =  self.params['find_e0'].get()
        if ((xa != '') and (ya != '')):
            cmnd  = 'spline(%s.%s,%s.%s' % (gr,xa,gr,ya)
            cmnd  =  '%s,rbkg=%s,kweight=%s,kmin=%s,kmax=%s' % (cmnd,rb,kw,kn,kx)
            if (fe0 == 1):
                cmnd  =  '%s,find_e0' % (cmnd)
            else:
                cmnd  =  '%s,e0=%s' % (cmnd,ee)

            self.iff_com(cmnd+')')



    def update_plot(self,selection):
        if (selection == None):  selection = self.wid['plot'].get()
        (gr,xa,ya)       = (self.wid['group'].get(),
                            self.wid['x'].get(),
                            self.wid['y'].get() )
        if (gr != 'None'):
            #        self.plot_types = ('xmu(E)', 'xmu with bkg(E)',
            #                           'chi(k)', 'k-weighted chi(k)')
            lab   =  'xlabel="E (eV)",ylabel="\gm(E)"'
            if  (selection == self.plot_types[0]):
                cmnd  = 'newplot(%s.%s,%s.%s,%s)' % (gr,xa,gr,ya,lab)
            elif (selection == self.plot_types[1]):
                cmnd  = 'newplot(%s.%s,%s.%s,%s)' % (gr,xa,gr,ya,lab)
                self.iff_com(cmnd)
                cmnd  = 'plot(%s.%s,%s.bkg)' % (gr,xa,gr)
                if (ya == 'pre'):
                    cmnd  = 'plot(%s.%s,%s.bkg_pre)' % (gr,xa,gr)
            elif (selection == self.plot_types[2]):
                lab   = 'xlabel="k (\A\u-1\d)",ylabel="\gx(k)"'
                cmnd  = 'newplot(%s.k,%s.chi,%s)' % (gr,gr,lab)
            elif (selection == self.plot_types[3]):
                cmnd  = 'set %s.chi_kw = %s.chi * %s.k^kweight_spl' % (gr,gr,gr)
                self.iff_com(cmnd)
                kw    = self.get_scalar('kweight_spl')
                lab   = 'xlabel="k (\A\u-1\d)",ylabel="k\u%s\d\gx(k)"' % (int(kw))
                cmnd  = 'newplot(%s.k,%s.chi_kw,%s)' % (gr,gr,lab)

            self.iff_com(cmnd)


    def drawWindow(self,master):
        self.main = self.MakeMainWindow(master)
        ma = self.main

        #
        frame1  = Frame(ma,   borderwidth=3, relief='ridge')
        fr1     = Frame(frame1)
        fr2     = Frame(fr1)
        Label(fr2, foreground='red',  width=15,
              text='Data Arrays   ').pack(side='left',
                                          anchor='w',fill='both')
        fr2.pack(side='left', anchor='w',fill='x')
        fr2     = Frame(fr1)
        # Group
        Label(fr2, foreground='black', width=7,
              text='Group').pack(side='left', anchor='e',fill='both')

        self.wid['group'] = Pmw.ComboBox(fr2, scrolledlist_items= self.groups,
                                     entry_width = 12, entry_state="disabled",
                                     selectioncommand =
                                     Command(self.update_array_choice,'group',get_group=1))

        self.wid['group'].pack(side='left',anchor='e',fill='x')
        fr2.pack(side='right', anchor='w', fill='both')
        fr1.pack(side='top', anchor='w', fill='both')

        fr1     = Frame(frame1)
        fr2     = Frame(fr1)
        # Energy
        Label(fr2, foreground='black',  width=7,
              text=self.xa_label).pack(side='left',anchor='w',fill='x')
        self.wid['x']  = Pmw.ComboBox(fr2, scrolledlist_items= self.groups,
                                         entry_width = 12, entry_state="disabled",
                                         selectioncommand =
                                         Command(self.update_array_choice,'x',get_group=1))
        self.wid['x'].pack(side='left',anchor='w',fill='x')
        # Mu
        Label(fr2, foreground='black',  width=7,
              text=self.ya_label).pack(side='left',anchor='w',fill='x')
        self.wid['y']  = Pmw.ComboBox(fr2, scrolledlist_items= self.groups,
                                         entry_width = 12, entry_state="disabled",
                                         selectioncommand =
                                         Command(self.update_array_choice,'y',get_group=1))
        self.wid['y'].pack(side='left',fill='x')
        fr2.pack(side='right', anchor='w',fill='both')
        fr1.pack(side='top',   anchor='w', fill='both')
        frame1.pack(side='top', anchor='w',fill='both')


        self.wid['group'].setlist(self.groups)
        self.wid['group'].selectitem(self.groups[0])
        if (len(self.groups) > 1):
            self.wid['group'].selectitem(self.groups[1])
            self.update_array_choice('group',self.groups[1],get_group=0)

        ## Analysis Params
        frame1  = Frame(ma,   borderwidth=3, relief='ridge')
        fr1     = Frame(frame1)
        Label(fr1, foreground='red',
              text='Analysis Parameters').pack(side='left', anchor='w',fill='both')
        fr1.pack(side='top', anchor='w',fill='x')

        # E0  / find E0
        fr1    = Frame(frame1)
        fr2    = Frame(fr1)
        self.params['find_e0'] = IntVar()
        self.wid['e0']     = LabEnt(fr2,'E0',   self.params['e0'],  12, labwidth=8)
        self.wid['find_e0'] = Checkbutton(fr2,text = 'Find E0', padx = 6,
                                         variable = self.params['find_e0'])
        self.wid['e0'].pack(side='left', anchor='w')
        self.wid['find_e0'].select()
        self.wid['find_e0'].pack(side='left')
        fr2.pack(side='left', anchor='w',fill='x')
        fr1.pack(side='top', anchor='w',fill='x')

        # R bkg / Kweight
        fr1    = Frame(frame1)
        fr2    = Frame(fr1)
        self.wid['rbkg'] = LabEnt(fr2,'Rbkg',      self.params['rbkg'],  12, labwidth=8)
        self.wid['kw']   = LabEnt(fr2,' K weight', self.params['kw'],12, labwidth=8)
        self.wid['rbkg'].pack(side='left', anchor='w')
        self.wid['kw'].pack(side='left')
        fr2.pack(side='left', anchor='w',fill='x')
        fr1.pack(side='top', anchor='w',fill='x')

        # K min / K max
        fr1    = Frame(frame1)
        fr2    = Frame(fr1)
        self.wid['kmin'] = LabEnt(fr2,'K min ',  self.params['kmin'],  12, labwidth=8)
        self.wid['kmax'] = LabEnt(fr2,' K max',  self.params['kmax'],  12, labwidth=8)
        self.wid['kmin'].pack(side='left', anchor='w')
        self.wid['kmax'].pack(side='left')
        fr2.pack(side='left', anchor='w',fill='x')
        fr1.pack(side='top', anchor='w',fill='x')


        self.balloon.bind(self.wid['kmin'], '',
                          'Minimum k value for background subtraction')
        self.balloon.bind(self.wid['kmax'], '',
                          'Maximum k value for background subtraction')
        self.balloon.bind(self.wid['kw'], '',
                          'K-weight for background FT (0 or 1 recommended)')
        self.balloon.bind(self.wid['rbkg'], '',
                          'Maximum R value to consider in background')
        self.balloon.bind(self.wid['e0'], '',
                          'Energy origin to define k=0')

        frame1.pack(side='top',anchor='w',fill='both')

        ## Plot Frame
        frame1  = Frame(ma)
        fr1     = Frame(frame1)
        Label(fr1, foreground='red',
              text='Plot to Show').pack(side='left', anchor='w',fill='both')
        fr1.pack(side='top', anchor='w',fill='x')

        # Droplist
        fr1    = Frame(frame1)
        self.wid['plot'] = Pmw.ComboBox(fr1, scrolledlist_items= self.plot_types,
                                     entry_width = 20, entry_state="disabled",
                                     selectioncommand =
                                     Command(self.update_plot))
        self.wid['plot'].pack(side='left',anchor='e',fill='x')
        self.wid['plot'].selectitem(self.plot_types[1])
        fr1.pack(side='left',anchor='w',fill='both')
        frame1.pack(side='top',anchor='w',fill='both')


        # bottom
        fr3   = Frame(ma)
        bbox   = Pmw.ButtonBox(fr3)
        bbox.pack(side='left',fill='both', expand=1)
        b_calc = bbox.add(self.do_btn_label,
                          command= Command(self.btn_press,'calc'))
        b_quit = bbox.add('Quit', command = Command(self.btn_press,'ok'))
        bbox.setdefault(self.do_btn_label)
        bbox.alignbuttons()
        self.balloon.bind(b_calc, '', self.do_btn_hint)

        self.balloon.bind(b_quit, '', 'Close This Window')
        fr3.pack(side='top',anchor='w')
        self.createMsgWin(ma,width=50)
        self.update_params(self.param_map)
        self.main.deiconify()




class Paths(BaseWindow,Ifeffit):
    " Define Paths "
    window_title = "Define FEFF Paths"
    do_btn_label = 'Accept Paths'
    do_btn_hint  = 'Click here to use this definition'
    xa_label     = 'energy'
    ya_label     = 'mu'
    def __init__(self,  master=None, iff_com=None,paths=None):
        if (iff_com  == None): iff_com = self.ifeffit
        if (paths    == None): paths   = {}
        self.feff_paths   = paths
        # print ' self.paths ' , self.feff_paths
        self.iff_com = iff_com
        self.master  = master
        self.opts    = {}
        self.wid     = {'x':0, 'y':0, 'group':0}
        self.params  = {'dr':0, 'vary_dr':0,
                        'feff_file':''}
        self.defs    = {'x':'energy','y':'xmu'}
        self.plot_types = ('chi(k)', 'k-weighted chi(k)')
        self.param_map = ()
        self.arrays  = self.get_group_arrays()
        self.groups  = ['None'] + self.arrays.keys()
        self.drawWindow(master)

    def update_params(self,params):
        for i in range(len(params)):
            r = self.get_scalar(params[i][0])
            entry_replace(self.wid[params[i][1]], r)

    def parse_feffdat(self):
        pass

    def btn_press(self,btn,event=None,ok_action=None):
        ret = None
        if ((btn == 'ok') or (btn == 'apply')):
            if ((ok_action != None) and (callable(ok_action) == 1)):
                ret  = ok_action()
        if ((btn == 'ok') or (btn == 'cancel')): self.main.destroy()
        if (btn == 'calc'):
            self.do_calc()
            self.update_params(self.param_map)
            self.update_plot(None)
        if (btn == 'pickfile'):
            self.feff_file = ask_for_file(self.master,
                                          [("feff files","feff*.dat"),
                                           ("all files","*")] )
            # print "feff_file = ", self.feff_file
            self.parse_feffdat()
            entry_replace(self.wid['feff_file'], self.feff_file)
        return ret

    def do_calc(self):
        (gr,xa,ya)   = ('None','','')
        dr  = self.wid['dr'].get()
        vdr = self.params['vary_dr'].get()
        if ((xa != '') and (ya != '')):
            cmnd  = 'pre_edge(%s.%s,%s.%s' % (gr,xa,gr,ya)
            if (fe0 == 1):
                cmnd  =  '%s,e0find' % (cmnd)
            else:
                cmnd  =  '%s,e0=%s'  % (cmnd,e0)
            if (fst != 1):
                cmnd  =  '%s,edge_step=%s'  % (cmnd,st)
            cmnd  = "%s,pre1=%s,pre2=%s,norm1=%s,norm2=%s)"  % (cmnd,p1,p2,n1,n2)
            self.iff_com(cmnd)


    def update_plot(self,selection):
        if (selection == None):  selection = self.wid['plot'].get()
        (gr,xa,ya)   = ('None','','')

        if (gr != 'None'):
            xlab = 'E (eV)'
            ylab = 'mu'
            lab  = 'xlabel=%s,ylabel=%s' % (xlab,ylab)
            if  (selection == self.plot_types[0]):
                lab
                cmnd = 'newplot(%s.%s,%s.%s,%s)' % (gr,xa,gr,ya,lab)
            elif (selection == self.plot_types[1]):
                cmnd  =  'newplot(%s.%s,%s.%s,%s)' % (gr,xa,gr,ya,lab)
                self.iff_com(cmnd)
                self.iff_com("set %s.preline = pre_offset+pre_slope*%s.%s"
                             % (gr,gr,xa) )
                cmnd  =  'plot(%s.%s,%s.preline)' % (gr,xa,gr)
            elif (selection == self.plot_types[2]):
                cmnd  =  'newplot(%s.%s,%s.pre,%s)' % (gr,xa,gr,lab)
            elif (selection == self.plot_types[3]):
                cmnd  =  'newplot(%s.%s,%s.norm,%s)' % (gr,xa,gr,lab)
            self.iff_com(cmnd)


    def drawWindow(self,master):
        self.main = self.MakeMainWindow(master)
        ma = self.main

        #
        frame1  = Frame(ma,   borderwidth=3, relief='ridge')
        fr1  = Frame(frame1)
        fr2  = Frame(fr1)
        Label(fr2, foreground='red',  width=15,
              text='FEFF Path:  ').pack(side='left',
                                        anchor='w',fill='both')
        fr2.pack(side='left', anchor='w',fill='x')
        fr1.pack(side='top', anchor='w', fill='both')
        fr1  = Frame(frame1)
        Button(fr1,text='Feff File',
               command = Command(self.btn_press,'pickfile')).pack(side='left')
        self.wid['feff_file'] = LabEnt(fr1,'', self.params['feff_file'],
                                       40, labwidth=2)
        fr1.pack(side='top', anchor='w', fill='both')
        frame1.pack(side='top', anchor='w',fill='both')


        ## Analysis Params
        frame1  = Frame(ma,   borderwidth=3, relief='ridge')
        fr1     = Frame(frame1)
        Label(fr1, foreground='red',
              text='Path Parameters').pack(side='left', anchor='w',fill='both')
        fr1.pack(side='top', anchor='w',fill='x')

        # R /Delta R
        fr1    = Frame(frame1)
        fr2    = Frame(fr1)
        self.params['vary_dr'] = IntVar()
        self.wid['dr']     = LabEnt(fr2,'DR', self.params['dr'], 12, labwidth=6)
        self.wid['vary_dr'] = Checkbutton(fr2,text = 'vary R', padx = 6,
                                          variable = self.params['vary_dr'])
        self.wid['dr'].pack(side='left', anchor='w')
        self.wid['vary_dr'].deselect()
        self.wid['vary_dr'].pack(side='left')
        fr2.pack(side='left', anchor='w',fill='x')
        fr1.pack(side='top', anchor='w',fill='x')

        self.balloon.bind(self.wid['dr'], '',
                          'Near Neighbor distance')
        self.balloon.bind(self.wid['vary_dr'], '',
                          'vary Near Neighbor distance in fit')

        frame1.pack(side='top',anchor='w',fill='both')

        ## Plot Frame
        frame1  = Frame(ma)
        fr1     = Frame(frame1)
        Label(fr1, foreground='red',
              text='Plot to Show').pack(side='left', anchor='w',fill='both')
        fr1.pack(side='top', anchor='w',fill='x')

        # Droplist
        fr1    = Frame(frame1)
        self.wid['plot'] = Pmw.ComboBox(fr1, scrolledlist_items= self.plot_types,
                                     entry_width = 40, entry_state="disabled",
                                     selectioncommand =
                                     Command(self.update_plot))
        self.wid['plot'].pack(side='left',anchor='e',fill='x')
        self.wid['plot'].selectitem(self.plot_types[1])
        fr1.pack(side='left',anchor='w',fill='both')
        frame1.pack(side='top',anchor='w',fill='both')


        # bottom
        fr3   = Frame(ma)
        bbox   = Pmw.ButtonBox(fr3)
        bbox.pack(side='left',fill='both', expand=1)
        b_calc = bbox.add(self.do_btn_label,
                          command= Command(self.btn_press,'calc'))
        b_quit = bbox.add('Quit', command = Command(self.btn_press,'ok'))
        bbox.setdefault(self.do_btn_label)
        bbox.alignbuttons()
        self.balloon.bind(b_calc, '', self.do_btn_hint)

        self.balloon.bind(b_quit, '', 'Close This Window')
        fr3.pack(side='top',anchor='w')
        self.createMsgWin(ma,width=50)
        self.update_params(self.param_map)
        self.main.deiconify()




class PreEdge(BaseWindow,Ifeffit):
    " Pre Edge Interface "
    window_title = "PreEdge Window"
    do_btn_label = 'Calculate Pre-Edge'
    do_btn_hint  = 'Calculate pre-edge line, e0, and normalization'
    xa_label     = 'energy'
    ya_label     = 'mu'
    def __init__(self,  master=None, iff_com=None):
        if (iff_com   == None): iff_com = self.ifeffit
        self.iff_com = iff_com
        self.master  = master
        self.opts    = {}
        self.wid     = {'x':0, 'y':0, 'group':0}
        self.params  = {'pre1':-100, 'pre2':-50, 'e0':0,'find_e0':0,
                        'norm1':100, 'norm2':300, 'step':0,'find_st':0}
        self.defs    = {'x':'energy','y':'xmu'}
        self.plot_types = ('xmu(E)', 'xmu with pre-edge line',
                           'pre-edge subtracted mu(E)',
                           'normalized mu(E)')
        self.param_map = (('pre1', 'pre1'),('pre2', 'pre2'),
                          ('norm1', 'norm1'),('norm2', 'norm2'),
                          ('e0','e0'),
                          ('edge_step', 'step') )

        self.arrays  = self.get_group_arrays()
        self.groups  = ['None'] + self.arrays.keys()
        self.drawWindow(master)

    def do_calc(self):
        (gr,xa,ya) = (self.wid['group'].get(),
                      self.wid['x'].get(),
                      self.wid['y'].get() )
        (p1,p2,n1,n2,e0,st) = (self.wid['pre1'].get(),
                            self.wid['pre2'].get(),
                            self.wid['norm1'].get(),
                            self.wid['norm2'].get(),
                            self.wid['e0'].get(),
                            self.wid['step'].get())
        fst =  self.params['find_st'].get()
        fe0 =  self.params['find_e0'].get()
        if ((xa != '') and (ya != '')):
            cmnd  = 'pre_edge(%s.%s,%s.%s' % (gr,xa,gr,ya)
            if (fe0 == 1):
                cmnd  =  '%s,e0find' % (cmnd)
            else:
                cmnd  =  '%s,e0=%s'  % (cmnd,e0)
            if (fst != 1):
                cmnd  =  '%s,edge_step=%s'  % (cmnd,st)
            cmnd  = "%s,pre1=%s,pre2=%s,norm1=%s,norm2=%s)"  % (cmnd,p1,p2,n1,n2)
            self.iff_com(cmnd)


    def update_plot(self,selection):
        if (selection == None):  selection = self.wid['plot'].get()
        (gr,xa,ya)       = (self.wid['group'].get(),
                            self.wid['x'].get(),
                            self.wid['y'].get() )
        if (gr != 'None'):
            xlab = 'E (eV)'
            ylab = 'mu'
            lab  = 'xlabel=%s,ylabel=%s' % (xlab,ylab)
            if  (selection == self.plot_types[0]):
                lab
                cmnd = 'newplot(%s.%s,%s.%s,%s)' % (gr,xa,gr,ya,lab)
            elif (selection == self.plot_types[1]):
                cmnd  =  'newplot(%s.%s,%s.%s,%s)' % (gr,xa,gr,ya,lab)
                self.iff_com(cmnd)
                self.iff_com("set %s.preline = pre_offset+pre_slope*%s.%s"
                             % (gr,gr,xa) )
                cmnd  =  'plot(%s.%s,%s.preline)' % (gr,xa,gr)
            elif (selection == self.plot_types[2]):
                cmnd  =  'newplot(%s.%s,%s.pre,%s)' % (gr,xa,gr,lab)
            elif (selection == self.plot_types[3]):
                cmnd  =  'newplot(%s.%s,%s.norm,%s)' % (gr,xa,gr,lab)
            self.iff_com(cmnd)


    def drawWindow(self,master):
        self.main = self.MakeMainWindow(master)
        ma = self.main

        #
        frame1  = Frame(ma,   borderwidth=3, relief='ridge')
        fr1     = Frame(frame1)
        fr2     = Frame(fr1)
        Label(fr2, foreground='red',  width=15,
              text='Data Arrays   ').pack(side='left',
                                          anchor='w',fill='both')
        fr2.pack(side='left', anchor='w',fill='x')
        fr2     = Frame(fr1)
        # Group
        Label(fr2, foreground='black', width=7,
              text='Group').pack(side='left', anchor='e',fill='both')

        self.wid['group'] = Pmw.ComboBox(fr2, scrolledlist_items= self.groups,
                                     entry_width = 12, entry_state="disabled",
                                     selectioncommand =
                                     Command(self.update_array_choice,'group',get_group=1))

        self.wid['group'].pack(side='left',anchor='e',fill='x')
        fr2.pack(side='right', anchor='w', fill='both')
        fr1.pack(side='top', anchor='w', fill='both')

        fr1     = Frame(frame1)
        fr2     = Frame(fr1)
        # Energy
        Label(fr2, foreground='black',  width=7,
              text=self.xa_label).pack(side='left',anchor='w',fill='x')
        self.wid['x']  = Pmw.ComboBox(fr2, scrolledlist_items= self.groups,
                                         entry_width = 12, entry_state="disabled",
                                         selectioncommand =
                                         Command(self.update_array_choice,'x',get_group=1))
        self.wid['x'].pack(side='left',anchor='w',fill='x')
        # Mu
        Label(fr2, foreground='black',  width=7,
              text=self.ya_label).pack(side='left',anchor='w',fill='x')
        self.wid['y']  = Pmw.ComboBox(fr2, scrolledlist_items= self.groups,
                                         entry_width = 12, entry_state="disabled",
                                         selectioncommand =
                                         Command(self.update_array_choice,'y',get_group=1))
        self.wid['y'].pack(side='left',fill='x')
        fr2.pack(side='right', anchor='w',fill='both')
        fr1.pack(side='top',   anchor='w', fill='both')
        frame1.pack(side='top', anchor='w',fill='both')


        self.wid['group'].setlist(self.groups)
        self.wid['group'].selectitem(self.groups[0])
        if (len(self.groups) > 1):
            self.wid['group'].selectitem(self.groups[1])
            self.update_array_choice('group',self.groups[1],get_group=0)

        ## Analysis Params
        frame1  = Frame(ma,   borderwidth=3, relief='ridge')
        fr1     = Frame(frame1)
        Label(fr1, foreground='red',
              text='Analysis Parameters').pack(side='left', anchor='w',fill='both')
        fr1.pack(side='top', anchor='w',fill='x')

        # E0  / find E0
        fr1    = Frame(frame1)
        fr2    = Frame(fr1)
        self.params['find_e0'] = IntVar()
        self.wid['e0']     = LabEnt(fr2,'E0',   self.params['e0'],  12, labwidth=18)
        self.wid['find_e0'] = Checkbutton(fr2,text = 'Find E0', padx = 6,
                                         variable = self.params['find_e0'])
        self.wid['e0'].pack(side='left', anchor='w')
        self.wid['find_e0'].select()
        self.wid['find_e0'].pack(side='left')
        fr2.pack(side='left', anchor='w',fill='x')
        fr1.pack(side='top', anchor='w',fill='x')

        # step  / find step
        fr1    = Frame(frame1)
        fr2    = Frame(fr1)
        self.params['find_st'] = IntVar()
        self.wid['step']    = LabEnt(fr2,'Edge Step',
                                      self.params['step'],  12, labwidth=18)
        self.wid['find_st'] = Checkbutton(fr2,text = 'Find Step', padx = 6,
                                         variable = self.params['find_st'])
        self.wid['step'].pack(side='left', anchor='w')
        self.wid['find_st'].select()
        self.wid['find_st'].pack(side='left')
        fr2.pack(side='left', anchor='w',fill='x')
        fr1.pack(side='top', anchor='w',fill='x')


        # K min / K max
        fr1    = Frame(frame1)
        fr2    = Frame(fr1)
        self.wid['pre1'] = LabEnt(fr2,'Pre Edge Range: ',
                                  self.params['pre1'],  12, labwidth=18)
        self.wid['pre2'] = LabEnt(fr2,' to  ',  self.params['pre2'],
                                  12, labwidth=6)
        self.wid['pre1'].pack(side='left', anchor='w')
        self.wid['pre2'].pack(side='left')
        fr2.pack(side='left', anchor='w',fill='x')
        fr1.pack(side='top', anchor='w',fill='x')

        # K min / K max
        fr1    = Frame(frame1)
        fr2    = Frame(fr1)
        self.wid['norm1'] = LabEnt(fr2,'Edge Step Range: ',
                                   self.params['norm1'],  12, labwidth=18)
        self.wid['norm2'] = LabEnt(fr2,' to  ',  self.params['norm2'],
                                   12, labwidth=6)
        self.wid['norm1'].pack(side='left', anchor='w')
        self.wid['norm2'].pack(side='left')
        fr2.pack(side='left', anchor='w',fill='x')
        fr1.pack(side='top', anchor='w',fill='x')


        self.balloon.bind(self.wid['pre1'], '',
                          'Min Energy (relative to e0) for pre-edge line')
        self.balloon.bind(self.wid['pre2'], '',
                          'Max Energy (relative to e0) for pre-edge line')
        self.balloon.bind(self.wid['norm1'], '',
                          'Min Energy (relative to e0) for finding edge step')
        self.balloon.bind(self.wid['norm2'], '',
                          'Max Energy (relative to e0) for finding edge step')
        self.balloon.bind(self.wid['e0'], '',
                          'Energy origin')
        self.balloon.bind(self.wid['step'], '',
                          'Edge step')
        self.balloon.bind(self.wid['find_st'], '',
                          'Automatic determination of edge step?')
        self.balloon.bind(self.wid['find_e0'], '',
                          'Automatic determination of e0?')

        frame1.pack(side='top',anchor='w',fill='both')

        ## Plot Frame
        frame1  = Frame(ma)
        fr1     = Frame(frame1)
        Label(fr1, foreground='red',
              text='Plot to Show').pack(side='left', anchor='w',fill='both')
        fr1.pack(side='top', anchor='w',fill='x')

        # Droplist
        fr1    = Frame(frame1)
        self.wid['plot'] = Pmw.ComboBox(fr1, scrolledlist_items= self.plot_types,
                                     entry_width = 40, entry_state="disabled",
                                     selectioncommand =
                                     Command(self.update_plot))
        self.wid['plot'].pack(side='left',anchor='e',fill='x')
        self.wid['plot'].selectitem(self.plot_types[1])
        fr1.pack(side='left',anchor='w',fill='both')
        frame1.pack(side='top',anchor='w',fill='both')


        # bottom
        fr3   = Frame(ma)
        bbox   = Pmw.ButtonBox(fr3)
        bbox.pack(side='left',fill='both', expand=1)
        b_calc = bbox.add(self.do_btn_label,
                          command= Command(self.btn_press,'calc'))
        b_quit = bbox.add('Quit', command = Command(self.btn_press,'ok'))
        bbox.setdefault(self.do_btn_label)
        bbox.alignbuttons()
        self.balloon.bind(b_calc, '', self.do_btn_hint)

        self.balloon.bind(b_quit, '', 'Close This Window')
        fr3.pack(side='top',anchor='w')
        self.createMsgWin(ma,width=50)
        self.update_params(self.param_map)
        self.main.deiconify()


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
        self.iff_com   = self.do_ifeffit
	self.balloon   = Pmw.Balloon(self.root)
        self.inp_buff.append("") ;
        self.createMenubar(self.root)
        self.createMainWin(self.root)
        self.createMsgWin(self.root)
        self.load_startup_file()
        if ((load != None) and (type(load) == types.ListType)):
            for i in range(len(load)):
                self.iff_com("load(%s)" % load[i])
        self.clear_echo_buffer()
        self.root.deiconify()
        self.root.mainloop()

    def drawsplash(self,master):
	# Create about dialog.
	Pmw.aboutversion(self.version)
	Pmw.aboutcopyright(self.copyright)
	Pmw.aboutcontact(self.contact)
	self.about = Pmw.AboutDialog(master, applicationname = self.progname)
        self.about.after(5000,self.removesplash)

    def removesplash(self):
        self.about.withdraw()

    def createMainWin(self,master=None):
        nbFrame = Pmw.NoteBook(master, raisecommand= Command(self.nb_raise))

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
        self.cmd_win.configure(width=85, height=20)
        self.cmd_win.tag_configure('output',  foreground='red',font=('Helvetica', 12))
        self.cmd_win.tag_configure('input',   font=('Helvetica', 12))
        self.cmd_win.pack(side='top')

        self.sca_win  = ScrolledText.ScrolledText(sca_page)
        self.sca_win.configure(width=85, height=20)
        self.sca_win.insert('end', "\n\n")
        self.sca_win.pack(side='top')

        self.arr_win  = ScrolledText.ScrolledText(arr_page)
        self.arr_win.configure(width=85, height=20)
        self.arr_win.insert('end', "\n\n")
        self.arr_win.pack(side='top')

        self.str_win  = ScrolledText.ScrolledText(str_page)
        self.str_win.configure(width=85, height=20)
        self.str_win.insert('end', "\n\n")
        self.str_win.pack(side='top')

        self.mac_win  = ScrolledText.ScrolledText(mac_page)
        self.mac_win.configure(width=85, height=20)
        self.mac_win.insert('end', "\n\n")
        self.mac_win.pack(side='top')

        self.pth_win  = ScrolledText.ScrolledText(pth_page)
        self.pth_win.configure(width=85, height=20)
        self.pth_win.insert('end', "\n\n")
        self.pth_win.pack(side='top')



        nbFrame.setnaturalsize()
        self.cmdBox   = Frame(master)
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
        self.menuBar.addcascademenu('File', 'Write')
#         self.menuBar.addmenuitem('Write', 'command', 'Write Data File',
#                                  label='Data File',
#                                  command=Command(self.write_datafile, self.root))
        self.menuBar.addmenuitem('Write', 'command', 'Write Command File',
                                 label='Command File',
                                 command=Command(self.write_cmndfile, self.root))
        self.menuBar.addmenuitem('Write', 'command', 'Save session to .sav file',
                                 label='Save session',
                                 command=Command(self.save_state, self.root))
        self.menuBar.addmenuitem('File', 'separator')
        self.menuBar.addmenuitem('File', 'command',  'Close',  label='Exit',
                                 command=self.exit)
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


        self.menuBar.addmenu('Process', 'General Data Processing')
        self.menuBar.addmenuitem('Process', 'command',
                                 'General Plot Interface',  label='Plotter',
                                 command=Command(self.plotter, self.root))
        self.menuBar.addmenuitem('Process', 'command',
                                 'EXAFS Pre-Edge and Normalization',
                                 label='Pre-Edge',
                                 command=Command(self.pre_edge, self.root))
        self.menuBar.addmenuitem('Process', 'command',
                                 'EXAFS Background Subtraction',
                                 label='Background',
                                 command=Command(self.autobk, self.root))
        self.menuBar.addmenuitem('Process', 'command',
                                 'EXAFS Fourier Transforms',
                                 label='FFT ',
                                 command=Command(self.fft, self.root))

#         self.menuBar.addmenu('Analysis', 'Data Analysis ')
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
                sys.exit()
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
                if ((i >= skip) and (s != 'end macro')):
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
            self.do_ifeffit("save(%s)" % xfile)

    def restore_state(self, master):
        xfile = ""
        xfile = tkFileDialog.askopenfilename(
            filetypes=[("save files","*.sav"),
                       ("all files","*")], parent=master)
        xfile = trim_cwd(xfile)
        if (xfile != ''):
            self.do_ifeffit("restore(%s)" % xfile)

    def plotter(self,master):
        DataPlotter(iff_com = self.do_ifeffit,
                    master=master,  plot_opts= self.plot_opts)

    def search_frame(self, master):
        a = 1
        # print " Searching Current Frame for a string"

#     def setplot_opts(self, master):
#         SetPlotPrefs(master=master)

    def set_prefs(self, master):
        SetPrefs(master=master, prefs = self.prefs)

    def load_startup_file(self):
        pass
#       self.prefs['startup_file'] = 'AAA'
#         print "loading startup file:"
#         print self.prefs

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

    def exit(self):
        sys.exit(0)

    def help(self):
	self.about.show()

