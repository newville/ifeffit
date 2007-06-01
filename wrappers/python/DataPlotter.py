
from   Tkinter import *
from   string  import *
from   Ifeffit import Ifeffit
from   TkIfeffit import *
import re, os, sys, types
import Pmw, ScrolledText, tkFileDialog, tkColorChooser

class DataPlotter(BaseWindow,Ifeffit):
    " DataPlotter: plot data "
    def __init__(self,  master=None, iff_com=None, plot_opts = None):
        if (iff_com   == None): iff_com = self.ifeffit
        self.iff_com  = iff_com
        self.master   = master
        if (plot_opts == None): plot_opts = self.reset_plot_opts()
        self.plot_opts = plot_opts
        self.arrays  = self.get_group_arrays()
        self.groups  = ['None'] + self.arrays.keys()
        self.ntrace  = 4
        self.opt_win = None
        self.trace   = []
        self.selects = []
        self.non_x   = ['xmu', 'bkg', 'pre', 'norm', 'chi', 'chik', 'chir_pha',
                        'chir_re', 'chir_im', 'chir_mag', 'win']
        self.operations = ("None" , "negate", "derivative", "+", "-", "*", "/")
        self.colors  = ("blue" , "red", "black", 'darkgreen', 'magenta',
                      'maroon', 'yellow', 'orange',   'purple')
        self.styles  = ("solid" , "dashed", "dotted", "points")
        self.symbols = ('None', '+','*','o', 'X', 'squares', 'triangles')
        self.tkvar   = {}
        self.tkvar['showgrid'] = IntVar()
        self.tkvar['showgrid'].set(1)
        self.show_operations = 0
        self.opts    = {'xmin': 0, 'xmax': 0,  'ymin': 0, 'ymax': 0,'titl':0,
                        'xlab':0, 'ylab':0}
        for i in range(self.ntrace):
            self.trace.append(["","","","",""])
            self.selects.append(["None", "", "", "None", ""])
        self.drawWindow(master)

    def btn_press(self,btn,event=None,ok_action=None,trace=None):
        ret = None
        if ((btn == 'ok') or (btn == 'apply')):
            if ((ok_action != None) and (callable(ok_action) == 1)):
                ret  = ok_action()
        if ((btn == 'ok') or (btn == 'cancel')): self.main.destroy()
        if ((btn == 'ok_opts') or (btn == 'cancel_opts')): self.opt_win.destroy()
        if (btn == 'operations'):
            self.show_operations = 1
            self.main.withdraw()
            self.drawWindow(self.master)
        elif (btn == 'no_operations'):
            self.show_operations = 0
            self.main.withdraw()
            self.drawWindow(self.master)

        elif (btn == 'options'):
            self.plot_options(self.main)
            
        elif (btn == 'more'):
            for i in range(self.ntrace):
                for j  in range(len(self.trace[i])):
                    self.selects[i][j] = self.trace[i][j].get()
            self.ntrace = self.ntrace + 1
            self.trace.append(["","","","",""])
            self.selects.append(["None","","","None",""])
            self.main.withdraw()
            self.drawWindow(self.master)
        elif (btn == 'ps'):
            self.save_hardcopy('/ps')
        elif (btn == 'cps'):
            self.save_hardcopy('/cps')
        elif (btn == 'gif'):
            self.save_hardcopy('/gif')
        return ret
    
    def save_hardcopy(self,device):
        initfile = 'ifeffit.ps'
        if (device == '/gif'):  initfile = 'ifeffit.gif'
        if (device == '/png'):  initfile = 'ifeffit.png'
        xfile = tkFileDialog.asksaveasfilename(
            filetypes=[("Postscript files","*.ps"),
                       ("all files","*")],
            initialfile = initfile, parent=self.master)
        xfile = trim_cwd(xfile)
        if (xfile != ''):
            self.iff_com('plot(device="%s", file=%s)'%(device, xfile))

    def style_choose(self,trace,select):
        # print " style_choose ", trace, select
        st = strip(select)
        self.plot_opts['traces'][trace][1] = st
        wsy = "symbol#%i"% trace
        sym = self.opts[wsy].get()
        # print " symbol should be = ", sym
        if ((st == "solid") and (sym != "None")):
            self.symbol_choose(trace,sym)
        self.do_plot()


    def symbol_of(self,trace):
        " return (style,symbol) from ifeffit styles "
        sty = 'solid'
        sym = 'None'
        sty = self.plot_opts['traces'][trace][1]
        if (sty[0:11] == 'linespoints'):
            sym = self.symbols[int(sty[11:])-1]
            sty = 'solid'
        elif (sty[0:6] == 'points'):
            sym = self.symbols[int(sty[6:])-1]
            sty = 'points'
        return sty, sym
        
    def symbol_choose(self,trace,select):
        style = self.plot_opts['traces'][trace][1]
        style = strip(style)
        wsy   = "symbol#%i"% trace
        sym   = self.opts[wsy].get()
        # print "symbol choose ", style, sym
        if ((style == 'solid') or (style[0:11] == 'linespoints')):
            select = strip(select)
            for i in range(len(self.symbols)):
                if (select == self.symbols[i]):
                    self.plot_opts['traces'][trace][1] = 'linespoints%s' % (i+1)
        elif (style[0:5] == 'point'):
            for i in range(len(self.symbols)):
                if (sym == self.symbols[i]):
                    self.plot_opts['traces'][trace][1] = 'points%s' % (i+1)
        self.do_plot()
        
        
    def toggle_grid(self):
        self.plot_opts['showgrid'] =  self.tkvar['showgrid'].get()
        grid_cmd = 'plot(grid=1)'
        if (self.plot_opts['showgrid'] == 0): grid_cmd = 'plot(nogrid=1)'
        self.ifeffit(grid_cmd)
        self.do_plot()

    def color_choose(self,attr):
        # print "color_choose", attr
        c = None
        i = -999
        t = ['fg','bg','gridcolor']
        try:
            j = t.index(attr)
            c = self.plot_opts[attr]
            i = - (j + 10)
        except ValueError:
            try:
                i = int(attr)
                c = self.plot_opts['traces'][i][0]
            except ValueError:
                pass
        if (c != None):
            s  = tkColorChooser.askcolor(color= c,parent=self.master)
            nc = s[1]  # nc = new color
            if ((nc == "") or (nc == None)): nc = c
        if (i != -999):
            # bg, fg, gridcolor
            if (i < 0):
                qwid = t[-(i+10)]
                self.plot_opts[qwid] = nc
            # trace#N
            else:
                qwid = 'color#%i' % (i)
                self.plot_opts['traces'][i][0] = nc
            #
            self.opts[qwid].configure(background=nc,
                                     activebackground = nc)
        self.do_plot()


    def do_zoom_in(self):
        self.clear_echo_buffer()
        self.ifeffit('zoom(show)')
        s  = self.get_echo_buffer()
        m1 = split(replace(replace(s[2],'cursor: x =',''),'y =',''),',')
        m2 = split(replace(replace(s[1],'cursor: x =',''),'y =',''),',')
        entry_replace(self.opts['xmin'], min(strip(m1[0]), strip(m2[0])))
        entry_replace(self.opts['xmax'], max(strip(m1[0]), strip(m2[0])))
        entry_replace(self.opts['ymin'], min(strip(m1[1]), strip(m2[1])))
        entry_replace(self.opts['ymax'], max(strip(m1[1]), strip(m2[1])))

    def do_zoom_out(self):
        entry_replace(self.opts['xmin'], "")
        entry_replace(self.opts['xmax'], "")
        entry_replace(self.opts['ymin'], "")
        entry_replace(self.opts['ymax'], "")
        self.do_plot()

    def do_cursor(self):
        self.ifeffit("cursor(show,crosshair)")
        s  = self.get_echo_buffer()
        m1 = split(replace(replace(s[1],'cursor: x =',''),'y =',''),',')
        entry_replace(self.opts['x_curs'], strip(m1[0]))
        entry_replace(self.opts['y_curs'], strip(m1[1]))

    def do_plot(self):
        self.set_plot_opts(self.plot_opts)
        atts = ('titl', 'xlab', 'ylab', 'xmin', 'xmax', 'ymin', 'ymax')
        dat  = {}
        for i in atts:
            try:
                dat[i] = strip(self.opts[i].get())
            except:
                dat[i] = ''
        
        new   = 1
        ntotal= 0
        nlast = 0
        for i in range(len(self.trace)):
            tr = self.trace[i]
            gr = tr[0].get()
            if (gr != 'None'):
                ntotal = ntotal + 1
                nlast  = i
        for i in range(nlast + 1):
            tr = self.trace[i]
            color = self.plot_opts['traces'][i][0]
            style = self.plot_opts['traces'][i][1]
            gr = tr[0].get()
            if (gr == 'None'):
                tr = self.trace[nlast]
                gr = tr[0].get()
            ##
            if (gr != 'None'):
                x  = tr[1].get()
                y  = tr[2].get()
                grx = "%s.%s" % (gr, x)
                gry = "%s.%s" % (gr, y)
                if (self.show_operations):
                    op = tr[3].get()
                    yy = tr[4].get()
                    if (op == "negate"):
                        gry = "-%s" % gry
                    elif (op == "derivative"):
                        gry = "deriv(%s)" % gry
                    elif ((op == "+") and (yy != "None")):
                        gry = "(%s+%s.%s)" % (gry, gr,yy)
                    elif ((op == "-") and (yy != "None")):
                        gry = "(%s-%s.%s)" % (gry, gr,yy)
                    elif ((op == "*") and (yy != "None")):
                        gry = "(%s*%s.%s)" % (gry, gr,yy)
                    elif ((op == "/") and (yy != "None")):
                        gry = "(%s/%s.%s)" % (gry, gr,yy)
                ##
                cmd = "plot(%s, %s" % (grx,gry)
                if (new == 1):
                    new = 0
                    cmd = 'new'+cmd
                    if (dat['titl'] == ''): dat['titl'] = gr
                    if (dat['xlab'] == ''): dat['xlab'] = str(x)
                    if (dat['ylab'] == ''): dat['ylab'] = str(y)
                    cmd  = "%s, xlabel='%s',ylabel='%s'" %(cmd,dat['xlab'],dat['ylab'])
                    cmd = "%s, title='%s'" %(cmd,dat['titl'])
                    for j in ('xmin','xmax','ymin','ymax'):
                        if (dat[j] != ''):   cmd = "%s, %s=%s" % (cmd, j, dat[j])

                    #
                self.iff_com(cmd + ")")


    def update_array_choice(self, index, type, selection, get_group=1):
        # print ' PLOT update_array_choices: ', index, type, selection, get_group
        if (get_group == 1):
            self.arrays  = self.get_group_arrays()
        self.groups  = ['None'] + self.arrays.keys()
        for i in range(self.ntrace):
            self.trace[i][0].setlist(self.groups)
        if (type == 'gr'):
            if (selection != 'None'):
                tmpx = []
                for i in range(len(self.arrays[selection]['names'])):
                    x = self.arrays[selection]['names'][i]
                    try:
                        n = self.non_x.index(x)
                    except ValueError:
                        tmpx.append(x)
                self.trace[index][1].setlist(tmpx)
                self.trace[index][1].selectitem(tmpx[0])
                self.update_array_choice(index,'x',tmpx[0], get_group=0)
            else:
                self.trace[index][1].setlist([""])
                self.trace[index][1].selectitem("")
                self.trace[index][2].setlist([""])
                self.trace[index][2].selectitem("")
        elif (type == 'x'):
            group = self.trace[index][0].get()
            if ((group != 'None') and (selection != '')):
                innx  = self.arrays[group]['names'].index(selection)
                npts  = self.arrays[group]['npts'][innx]
                tmpy = []
                for i in range(len(self.arrays[group]['npts'])):
                    if (self.arrays[group]['npts'][i] == npts):
                        tmpy.append(self.arrays[group]['names'][i])

                self.trace[index][2].setlist(tmpy)
                self.trace[index][2].selectitem(tmpy[min(1,len(tmpy)-1)])
                if (self.show_operations):
                    self.trace[index][3].selectitem(self.operations[0])
                    tmpy = ['None'] + tmpy
                    self.trace[index][4].setlist(tmpy)
                    self.trace[index][4].selectitem(tmpy[0])



    def plot_options(self,master):
        self.opt_win = Toplevel(self.main)
        ma = self.opt_win
        ma.title('Plot Options')
        self.balloon = Pmw.Balloon(ma)
        self.menuBar = Pmw.MenuBar(ma, hull_borderwidth=1,
                                   hull_relief = 'raised',
                                   hotkeys=1, balloon = self.balloon)
        self.menuBar.pack(fill='x')
        self.menuBar.addmenu('File',    'Close')
        self.menuBar.addmenuitem('File', 'command',
                                 'Close Plot Window',
                                 label='Close',
                                 command= Command(self.btn_press, 'ok_opts'))


        fr1   = Frame(ma,borderwidth=1)
        Label(fr1, foreground='red',
              text='Titles and Labels ').pack(side='top',anchor='w')
        fr2   = Frame(fr1)
        self.opts['titl'] = LabEnt(fr2,'Title   ',self.plot_opts['title'],50)
        self.opts['titl'].pack(side='left')
        fr2.pack(side='top', anchor='w')
        fr2   = Frame(fr1)
        self.opts['xlab'] = LabEnt(fr2,'X Label ',  self.plot_opts['xlabel'],20)
        self.opts['ylab'] = LabEnt(fr2,' Y Label ', self.plot_opts['ylabel'],20)
        self.opts['xlab'].pack(side='left',anchor='w')
        self.opts['ylab'].pack(side='left')
        fr2.pack(side='top', anchor='w')
        fr1.pack(side='top',anchor='w')

        # colors
        fr1   = Frame(ma,borderwidth=1)
        Label(fr1, foreground='red',
              text='Colors').pack(side='top',anchor='w')
        fr2   = Frame(fr1)
        Label(fr2, text='Background:').pack(side='left',anchor='w')
        self.opts['bg'] = Button(fr2,text=" ",padx=1,width=2,height=1,
                                command=Command(self.color_choose,'bg'))
        self.opts['bg'].configure(background = self.plot_opts['bg'],
                                 activebackground = self.plot_opts['bg'] )
        self.opts['bg'].pack(side='left')

        Label(fr2, text='Foreground:').pack(side='left',anchor='w')
        self.opts['fg'] = Button(fr2,text=" ",padx=1,width=2,height=1,
               command=Command(self.color_choose,'fg'))
        self.opts['fg'].configure(background = self.plot_opts['fg'],
                                 activebackground = self.plot_opts['fg'] )
        self.opts['fg'].pack(side='left')

        Label(fr2, text='Grid:').pack(side='left',anchor='w')
        self.opts['gridcolor'] = Button(fr2,text=" ",padx=1,width=2,height=1,
               command=Command(self.color_choose,'gridcolor'))
        self.opts['gridcolor'].configure(background = self.plot_opts['gridcolor'],
                                        activebackground = self.plot_opts['gridcolor'] )
        self.opts['gridcolor'].pack(side='left')

        self.opts['showgrid'] = Checkbutton(fr2,text="Show Grid",padx=2,
                                            variable=self.tkvar['showgrid'],
                                            command=Command(self.toggle_grid))
        self.opts['showgrid'].pack(side='left')
        self.opts['showgrid'].select()
        if (self.plot_opts['showgrid'] == 0): self.opts['showgrid'].deselect()

        fr2.pack(side='top',anchor='w')
        fr1.pack(side='top',anchor='w')
        ##


    def drawWindow(self,master):
        if (master == None):
            self.main = Tk()
        else:
            self.main = Toplevel(master)
        Frame.__init__(self, self.main)
        ma  = self.main
        self.main.title('Plot Data Window')
        self.main.withdraw()
        self.balloon = Pmw.Balloon(ma)
        self.menuBar = Pmw.MenuBar(ma, hull_borderwidth=1,
                                   hull_relief = 'raised',
                                   hotkeys=1, balloon = self.balloon)
        self.menuBar.pack(fill='x')
        self.menuBar.addmenu('File',    'Close')
        self.menuBar.addmenuitem('File', 'command',
                                 'Close Plot Window',
                                 label='Close',
                                 command= Command(self.btn_press, 'ok'))
        self.menuBar.addmenu('Export',    'Export Plot to Postscript file')
        self.menuBar.addmenuitem('Export', 'command',
                                 'Save current plot to Postscript (B&W)',
                                 label='Save Postscript (B&W)',
                                 command= Command(self.btn_press, 'ps'))
        self.menuBar.addmenuitem('Export', 'command',
                                 'Save current plot to Postscript (Color)',
                                 label='Save Postscript (Color)',
                                 command= Command(self.btn_press, 'cps'))
        self.menuBar.addmenuitem('Export', 'command',
                                 'Save current plot to GIF (Color)',
                                 label='Save GIF (Color)',
                                 command= Command(self.btn_press, 'gif'))

        self.menuBar.addmenu('Operations',    'Optional Operations')
        self.menuBar.addmenuitem('Operations', 'command',
                                 'Enable  simple algebraic operations',
                                 label='enable operations',
                                 command= Command(self.btn_press, 'operations'))
        self.menuBar.addmenuitem('Operations', 'command',
                                 'Disable simple algebraic operations',
                                 label='disable operations',
                                 command= Command(self.btn_press, 'no_operations'))


        self.menuBar.addmenu('Options',    'Plot Options')
        self.menuBar.addmenuitem('Options', 'command',
                                 'Set General Plot Options',
                                 label='Plot Options',
                                 command= Command(self.btn_press, 'options'))

        # ranges
        fr1   = Frame(ma,borderwidth=1)
        Label(fr1, foreground= 'red', text='Cursor and Ranges ').pack(side='top',anchor='w')
        fr2   = Frame(fr1)
        self.opts['xmin'] = LabEnt(fr2,'X: min  ', self.plot_opts['xmin'],10)
        self.opts['xmax'] = LabEnt(fr2,' max ', self.plot_opts['xmax'],10)
        self.opts['xmin'].pack(side='left',anchor='w')
        self.opts['xmax'].pack(side='left')
        self.opts['ymin'] = LabEnt(fr2,'Y: min  ', self.plot_opts['ymin'],10)
        self.opts['ymax'] = LabEnt(fr2,' max ', self.plot_opts['ymax'],10)
        self.opts['ymin'].pack(side='left',anchor='w')
        self.opts['ymax'].pack(side='left')
        fr2.pack(side='top', anchor='w')

        fr2   = Frame(fr1)
        zbbox = Pmw.ButtonBox(fr2)
        zbbox.pack(side='left',fill='both', expand=1)
        zombi = zbbox.add('Zoom In',  command = Command(self.do_zoom_in))
        zombo = zbbox.add('Zoom Out', command = Command(self.do_zoom_out))
        zombc = zbbox.add('Cursor',   command = Command(self.do_cursor))
        zbbox.alignbuttons()
        self.balloon.bind(zombi, '','Zoom In on Plot Region')
        self.balloon.bind(zombo, '','Zoom Out to Full Plot Range')
        self.balloon.bind(zombc, '','Read Cursor Position (on Mouse Click)')
        self.opts['x_curs'] = LabEnt(fr2,'X:', "",10)
        self.opts['y_curs'] = LabEnt(fr2,'Y: ',"",10)
        self.opts['x_curs'].pack(side='left',anchor='w')
        self.opts['y_curs'].pack(side='left')
        fr2.pack(side='top',anchor='w')
        fr1.pack(side='top',anchor='w')

        # traces
        fr1   = Frame(ma,borderwidth=1)
        Label(fr1, foreground= 'red', text='Traces ').pack(side='top',anchor='w')
        Label(fr1, text='# ').pack(side='left')
        Label(fr1, text=' Group            ').pack(side='left')
        Label(fr1, text=' X array          ').pack(side='left')
        Label(fr1, text=' Y array          ').pack(side='left')
        if (self.show_operations):
            Label(fr1, text=' operate        ').pack(side='left')
            Label(fr1, text=' array          ').pack(side='left')
        Label(fr1, text='  color ').pack(side='left')
        Label(fr1, text=' style    ').pack(side='left')
        Label(fr1, text=' symbol   ').pack(side='left')
        fr1.pack(side='top',anchor='w')
        fr0   = Frame(ma,borderwidth=1)
        for i in range(len(self.trace)):
            j  = i+1
            lab= " %i "     % (j)
            co = "color#%i" % (j)
            st = "style#%i" % (j)
            sy = "symbol#%i"% (j)
            fr  = Frame(fr0)
            Label(fr, text= str(i+1)).pack(side='left',anchor='w')
            self.trace[i][0] = Pmw.ComboBox(fr,  scrolledlist_items  = self.groups,
                                            entry_width= 10,entry_state="disabled",
                                            selectioncommand=
                                            Command(self.update_array_choice,i, 'gr',get_group=1))
            self.trace[i][0].pack(side='left')
            self.trace[i][1] = Pmw.ComboBox(fr, entry_width=10,entry_state="disabled",
                                            scrolledlist_items=[""],
                                            selectioncommand=
                                            Command(self.update_array_choice,i, 'x',get_group=1))

            self.trace[i][1].pack(side='left')
            self.trace[i][2] = Pmw.ComboBox(fr, entry_width=10,entry_state="disabled",
                                            scrolledlist_items=[""])
            self.trace[i][2].pack(side='left')
            self.trace[i][3] = Pmw.ComboBox(fr, entry_width=9,entry_state="disabled",
                                            scrolledlist_items=self.operations)
            self.trace[i][4] = Pmw.ComboBox(fr, entry_width=10,entry_state="disabled",
                                            scrolledlist_items=[""])
            if (self.show_operations):
                self.trace[i][3].pack(side='left')
                self.trace[i][4].pack(side='left')

            self.opts[co] = Button(fr,text=" ",padx=1,width=2,highlightthickness=1,
                                   command=Command(self.color_choose,j))
            self.opts[co].configure(background   = self.plot_opts['traces'][j][0],
                                   activebackground =self.plot_opts['traces'][j][0] )
            self.opts[co].pack(side='left')

            self.opts[st] = Pmw.ComboBox(fr,  scrolledlist_items  = self.styles,
                                        entry_width= 7,entry_state="disabled",
                                        selectioncommand=Command(self.style_choose,j))
            
            self.opts[sy] = Pmw.ComboBox(fr,  scrolledlist_items  = self.symbols,
                                        entry_width= 8,entry_state="disabled",
                                        selectioncommand=Command(self.symbol_choose,j))

            (styl_, symb_) = self.symbol_of(j)
            self.opts[st].selectitem(styl_)
            self.opts[st].pack(side='left')

            self.opts[sy].selectitem(symb_)
            self.opts[sy].pack(side='left')

            fr.pack(side='top')

        #
        fr0.pack(side='top', anchor='w')

        for i in range(self.ntrace):
            self.trace[i][0].selectitem(self.selects[i][0])
            if (self.selects[0] > 0):
                self.update_array_choice( i,'gr', self.selects[i][0],get_group=0)
                self.update_array_choice( i,'x',  self.selects[i][1],get_group=0)
                for j in range(1,5):
                    if ((self.selects[i][j] != None) and
                        (self.selects[i][j] != "None") and
                        (self.selects[i][j] != "")):
                        self.trace[i][j].selectitem(self.selects[i][j])


        # bottom
        fr3   = Frame(ma)
        bbox   = Pmw.ButtonBox(fr3)
        bbox.pack(side='left',fill='both', expand=1)
        b_new  = bbox.add('Plot', command = Command(self.do_plot))
        b_more = bbox.add('Add more traces', command = Command(self.btn_press,'more'))
        b_quit = bbox.add('Quit', command = Command(self.btn_press,'ok'))
        bbox.setdefault('Plot')
        bbox.alignbuttons()
        self.balloon.bind(b_new,  '','Update Plot')
        self.balloon.bind(b_more, '','Add More Row for Traces to Plot')
        self.balloon.bind(b_quit, '', 'Close The Plotting Window')
        fr3.pack(side='top',anchor='w')
        self.createMsgWin(ma)
        self.main.deiconify()
