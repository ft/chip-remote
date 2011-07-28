all:
	@if [ -d build ]; then \
	  cd build; \
	  $(MAKE); \
	else \
	  mkdir build; \
	  cd build; \
	  cmake ..; \
	  $(MAKE); \
	fi;

apidocs:
	doxygen

tags:
	ctags *.c *.h
	ctags -e *.c *.h

install:
	@if [ -f build/cdce-remote ]; then \
	  cd build; \
	  $(MAKE) install; \
	else \
	  printf 'Project not build yet.\n'; \
	fi;

uninstall:
	@if [ -f build/cdce-remote ]; then \
	  cd build; \
	  $(MAKE) uninstall; \
	else \
	  printf 'Project not build yet.\n'; \
	fi;

test:
	@if [ -f build/cdce-remote ]; then \
	  cd build; \
	  $(MAKE) test; \
	else \
	  printf 'Project not build yet.\n'; \
	fi;

happiness:
	$(MAKE) tags
	$(MAKE) all
	(cd build; $(MAKE) inbuildexecscript;)
	$(MAKE) apidocs
	$(MAKE) test

.PHONY: all apidocs happiness install tags test uninstall
