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

.PHONY: all apidocs tags
