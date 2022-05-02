# Murphi Makefile
#
# Author            :             Ulrich Stern
# Editor            :             Weixuan Yang 
# Email             :             endreim@outlook.com
# Last modified date:             May. 02, 2022

INCLUDEPATH = include
CXX = g++ -DCATCH_DIV #                   # for elaines

CFLAGS = -Wno-write-strings -m32

%: %.C
	${CXX} ${CFLAGS} ${OFLAGS} -o $@ $@.C -I${INCLUDEPATH} -lm

