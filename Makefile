# $Id: Makefile,v 1.3 2013/03/02 15:54:41 daniel Exp daniel $
# $Name:  $

GSC=gambitc
GSCFLAGS=

CC=gcc
CFLAGS=-g -Wall -Wno-unused -fPIC
CPPFLAGS=
LDFLAGS=
LIBS= -lgambc -L /usr/lib/gambit-c
C_SRC=stb_image.c stb_image_write.c
C_OBJ=$(patsubst %.c,%.o,$(C_SRC))
SCM_SRC=ffi.scm util.scm srfi-4-util.scm matrix.scm \
        matrix-io.scm image.scm image-colors.scm image-io.scm \
	histogram.scm

SCM_C=$(patsubst %.scm,%.c,$(SCM_SRC))

SCM_OBJ=$(patsubst %.scm,%.o,$(SCM_SRC))


.PHONY: NOTICE

all:    imptool.o1

clean:  
	rm -rf $(SCM_C) *.o

imptool.o1: $(C_OBJ) SCM
	gcc -shared -o imptool.o1 $(LIBS) $(C_OBJ) $(SCM_OBJ) imptool.o1.o

$(C_OBJ): $(C_SRC)
	gcc -c $(CFLAGS) $^ 

SCM: $(SCM_C)
	gcc  -c  -D___DYNAMIC $(CFLAGS) $^ imptool.o1.c

$(SCM_C): $(SCM_SRC)
	$(GSC) -link -flat -o imptool.o1.c   $^ > /dev/null


