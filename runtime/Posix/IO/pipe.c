#define _ISOC99_SOURCE
#define _BSD_SOURCE
#define _POSIX_C_SOURCE 200112L

#include "platform.h"

Int Posix_IO_pipe (Pointer fds) {
	return pipe ((int *) fds);
}
