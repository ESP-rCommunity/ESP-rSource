#ifndef _SERIAL_H_
#define _SERIAL_H_

// Open a serial port
int openport(const char *infile, char writeable, int timeout);

// Return the number of bytes available on a port
int availableport(int fd);
 
// Read from a port with a timeout
int readport(int fd, unsigned char *buffer, size_t len, unsigned int timeout);

// Close port
int closeport(int fd);

#endif
//EOF
