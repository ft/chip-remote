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
	@doxygen

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

.PHONY: all apidocs tags
