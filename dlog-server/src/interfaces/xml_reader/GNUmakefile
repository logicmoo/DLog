include ../../../GNUgen.mak

OPTFLAGS = -O2,-march=i686,-fomit-frame-pointer,
INCFLAGS = -I$(XERCES_DIR)/include,
MSCFLAGS = -DLINUX,-D_REENTRANT,-ansi,-pipe,-DHAVE_RTTI,-Wall
CXXFLAGS = $(OPTFLAGS)$(INCFLAGS)$(MSCFLAGS)
SPLFRFLAGS = --cflag=$(CXXFLAGS) -LD -L$(XERCES_DIR)/lib -lxerces-c

POFILES=sax.po

all:	sax.$(SOEXT) $(POFILES)

%.$(SOEXT):	%.pl %.cpp
	$(SPLFR) $^ $(SPLFRFLAGS)

clean:
	@$(RM) sax.$(OEXT) sax.$(SOEXT) $(POFILES)
