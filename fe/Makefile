PWD               =$(shell pwd)
SOURCE            =$(shell find src/ -type f -name "*.c")
INCLUDE           =$(shell find src/ -type f -name "*.h")
OBJECT            =${SOURCE:.c=.o}
ASM               =$(shell find src/ -type f -name "*.S")
ASMOBJECT         =${ASM:.S=.o}
CC                = gcc
NASM              = nasm
#SANITIZER         =-fsanitize=address,undefined
RUNTIME_DEBUG     =-D_FORTIFY_SOURCE=2 -D_GLIBCC_ASSERTIONS
PROGNAME          =cunitpp-fe
LIBNAME           =lib$(PROGNAME).a
INCNAME           =cunitpp.h

CFLAGS            =
LDFLAGS           = -lelf

# test
TEST              =$(shell find unittest/ -type f -name "*-test.c")
TESTOBJECT        =${TEST:.c=.t}

# sample
SAMPLE            =$(shell find sample/ -type f -name "*.c")
SAMPLEOBJECT      =${SAMPLE:.c=.t}

# install
INSTALL_INC_DIR   =/usr/include
INSTALL_LIB_DIR   =/usr/lib

# -------------------------------------------------------------------------------
#
# Flags for different types of build
#
# -------------------------------------------------------------------------------
RELEASE_FLAGS     = -I$(PWD) -O3
RELEASE_LIBS      =

TEST_FLAGS        =-O0 -g3 -DCONFIG_UNITTEST -Wall -Werror
TEST_LIBS         = -lcunitpp -lelf

SAMPLE_FLAGS      =-O3 -g3
SAMPLE_LIBS       =

all : release

# -------------------------------------------------------------------------------
#
# Test
#
# -------------------------------------------------------------------------------
test: CFLAGS += $(TEST_FLAGS)
test: LDFLAGS += $(TEST_LIBS)
test: $(TESTOBJECT)

unittest/%.t : unittest/%.c  $(ASMOBJECT) $(OBJECT) $(INCLUDE) $(SOURCE)
	$(CC) $(CFLAGS) $(OBJECT) $(ASMOBJECT) -o $@ $< $(LDFLAGS)

# -------------------------------------------------------------------------------
#
# Sample
#
# -------------------------------------------------------------------------------
sample/%.t : sample/%.c $(ASMOBJECT) $(OBJECT) $(INCLUDE) $(SOURCE)
	$(CC) $(CFLAGS) $(OBJECT) $(ASMOBJECT) -o $@ $< $(LDFLAGS)

sample: CFLAGS += $(SAMPLE_FLAGS)
sample: LDFLAGS += $(SAMPLE_LIBS)

sample: $(SAMPLEOBJECT)

# -------------------------------------------------------------------------------
#
#  Release
#
# -------------------------------------------------------------------------------
release: CFLAGS  += $(RELEASE_FLAGS)
release: LDFLAGS  += $(RELEASE_LIBS)

release: $(OBJECT) $(ASMOBJECT)
	ar rcs $(LIBNAME) $(OBJECT) $(ASMOBJECT)

# -------------------------------------------------------------------------------
#
#  Install
#
# -------------------------------------------------------------------------------
install : release
	cp src/$(INCNAME) $(INSTALL_INC_DIR)/$(INCNAME)
	cp $(LIBNAME)     $(INSTALL_LIB_DIR)/$(LIBNAME)

.PHONY: uninstall
uninstall :
	rm -f $(INSTALL_INC_DIR)/$(INCNAME)
	rm -f $(INSTALL_LIB_DIR)/$(LIBNAME)

# -------------------------------------------------------------------------------
#
# Objects
#
# -------------------------------------------------------------------------------
src/%.o : src/%.c src/%.h
	$(CC) -o $@ $<

src/%.o : src/%.S
	$(NASM) -f elf64 -o $@ $<

clean:
	rm -rf $(OBJECT)
	rm -rf $(ASMOBJECT)
	rm -rf $(TESTOBJECT)
	rm -rf $(SAMPLEOBJECT)
	rm -rf $(LIBNAME)
