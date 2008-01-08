PATH = %PATH%;C:\dlog-reasoner\lib
del *.exe
sicstus -f --goal "compile('src/dlog.pl'), save_program('dlog.sav'), halt."
spld --output=dlog.exe --respath=c:\dlog-reasoner\xml_reader --static dlog.sav -LD xml_reader\xerces-c_2.lib
del *.sav *.c *.exp *.ilk *.lib *.pdb *.res *.rsrc *.spresdata


