#
# Makefile
#

# Setting

PRG = nbdprogram
RB = R CMD build
RI = R CMD INSTALL
RC = R CMD check
RR = R CMD REMOVE
CL = rm -fr
BUILDDIR = _build
CHECKDIR = _check

BOPTS = --md5
IOPTS = --clean --debug

# Commands

make:
	@echo "Building..."
	mkdir -p $(BUILDDIR)
	$(RB) $(PRG)/ $(BOPTS)
	mv $(PRG)*.tar.gz $(BUILDDIR)
	
check:
	mkdir -p $(CHECKDIR)
	$(RC) $(BUILDDIR)/$(PRG)_*.tar.gz -o $(CHECKDIR)
	
crancheck:
	mkdir -p $(CHECKDIR)
	$(RC) $(BUILDDIR)/$(PRG)_*.tar.gz --as-cran -o $(CHECKDIR)

install:
	@echo "Installing..."
	$(RI) $(BUILDDIR)/$(PRG)_0.*.tar.gz $(IOPTS)
	
remove:
	@echo "Removing..."
	$(RR) $(PRG)

clean:
	@echo "Cleaning..."
	$(CL) $(PRG)*_*.tar.gz
	$(CL) $(BUILDDIR)
	$(CL) $(CHECKDIR)
