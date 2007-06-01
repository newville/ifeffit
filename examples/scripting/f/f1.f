       program use_if
       integer i
       include '/usr/local/share/ifeffit/config/ifeffit.inc'
c
       i = ifeffit(' ')
       i = ifeffit('read_data(../../data/cu.xmu, group=cu, type=xmu)')
       i = ifeffit('spline(cu.energy, cu.xmu, rbkg = 1.2)')
       i = ifeffit('plot(cu.k, cu.chi)')
c
       end    
