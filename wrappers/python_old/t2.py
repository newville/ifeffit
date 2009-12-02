import Ifeffit

x = range(20)

i = Ifeffit.Ifeffit(screen_echo = 0)
y = i.ifeffit("read_data a.xmu")
d = i.get_group_arrays()
print " d = ", d

