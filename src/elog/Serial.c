#ifdef _WIN32
	#define WIN_HANDLE

	// Defines and headers
	#define _CRT_SECURE_NO_WARNINGS
	#define _CRT_SECURE_NO_DEPRECATE
	#include <windows.h>

	#define _POSIX_
	#pragma warning( disable : 4996 )    // allow deprecated POSIX name functions
	#include <io.h>
#else
    #include <sys/ioctl.h>
	#include <termios.h>
	#include <unistd.h>
#endif
 
 
// Includes
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <errno.h>
#include <fcntl.h>
 
#include "BaxUtils.h"

// Open a serial port
int openport(const char *infile, char writeable, int timeout)
{
	int fd;

	fd = fileno(stdin);
	if (infile != NULL && infile[0] != '\0' && !(infile[0] == '-' && infile[1] == '\0'))
	{
#ifdef _WIN32
		int flags = O_BINARY;
#else
		int flags = O_NOCTTY | O_NDELAY;
#endif
		flags |= (writeable) ? O_RDWR : O_RDONLY;

#ifdef _WIN32
		{
			HANDLE hSerial;
			hSerial = CreateFileA(infile, GENERIC_READ | GENERIC_WRITE, 0, 0, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
			if (hSerial == INVALID_HANDLE_VALUE) { fd = -1; fprintf(stderr, "ERROR: CreateFile problem '%s'\n", infile); }
			else
			{
#ifdef WIN_HANDLE
				fd = (intptr_t)hSerial;
#else
				// This fails for Bluetooth serial ports... keep as a handle
				fd = _open_osfhandle((intptr_t)hSerial, 0);
				if (fd == -1) { fprintf(stderr, "ERROR: Problem converting handle %x to fd %d for '%s'\n", hSerial, fd, infile); }
#endif
			}
		}
#else
		fd = open(infile, flags);
#endif

		if (fd < 0)
		{
			fprintf(stderr, "ERROR: Problem opening input: '%s'\n", infile);
			return -1;
		}

		// Set serial port parameters (OS-specific)
#ifdef _WIN32
		{
			HANDLE hSerial;
			DCB dcbSerialParams = { 0 };
			COMMTIMEOUTS timeouts = { 0 };

#ifdef WIN_HANDLE
			hSerial = (HANDLE)fd;
#else
			hSerial = (HANDLE)_get_osfhandle(fd);
#endif
		if (hSerial == INVALID_HANDLE_VALUE)
		{
			fprintf(stderr, "ERROR: Failed to get HANDLE from file.\n");
		}
		else
		{
			dcbSerialParams.DCBlength = sizeof(dcbSerialParams);
			if (!GetCommState(hSerial, &dcbSerialParams))
			{
				fprintf(stderr, "ERROR: GetCommState() failed.\n");
			}
			else
			{
				//dcbSerialParams.BaudRate = CBR_115200;
				dcbSerialParams.ByteSize = 8;
				dcbSerialParams.StopBits = ONESTOPBIT;
				dcbSerialParams.Parity = NOPARITY;
				if (!SetCommState(hSerial, &dcbSerialParams)){
					fprintf(stderr, "ERROR: SetCommState() failed.\n");
				};
			}

			timeouts.ReadIntervalTimeout = 0;
			timeouts.ReadTotalTimeoutConstant = timeout;
			timeouts.ReadTotalTimeoutMultiplier = 0;
			timeouts.WriteTotalTimeoutConstant = 0;
			timeouts.WriteTotalTimeoutMultiplier = 0;
			if (!SetCommTimeouts(hSerial, &timeouts))
			{
				fprintf(stderr, "ERROR: SetCommTimeouts() failed.\n");
			}
		}
	}
#else
	fcntl(fd, F_SETFL, 0);    // Clear all descriptor flags
	// Set the port options
	{
		struct termios options;
		tcgetattr(fd, &options);
		options.c_cflag = (options.c_cflag | CLOCAL | CREAD | CS8) & ~(PARENB | CSTOPB | CSIZE | CRTSCTS);
		options.c_lflag &= ~(ICANON | ECHO | ISIG); // Enable data to be processed as raw input
		tcsetattr(fd, TCSANOW, &options);
	}
#endif

	}
	return fd;
}
 
// Return the number of bytes available on a port
int availableport(int fd)
{
#if defined(_WIN32) && defined(WIN_HANDLE)
	DWORD errors = 0;
	COMSTAT stat = {0};
	ClearCommError((HANDLE)fd, &errors, &stat);
	return stat.cbInQue;
#else
	int available = 0;
	ioctl(fd, FIONREAD, &available);
	return available;
#endif
}
 
// Read from a port with a timeout
int readport(int fd, unsigned char *buffer, size_t len, unsigned int timeout)
{
	unsigned char *p = (unsigned char *)buffer;
	unsigned long start = (unsigned long)MillisecondsEpoch();
	int totalRead = 0;

	if (fd < 0 || buffer == NULL) { return 0; }
	for (;;)
	{
		int numRead = -1;
		int numToRead = len - totalRead;
		unsigned int elapsed;

		if (numToRead <= 0) { return totalRead;  }

#if defined(_WIN32) && defined(WIN_HANDLE)
		{
			DWORD dwRead = 0;
			if (!ReadFile((HANDLE)fd, p, numToRead, &dwRead, 0)) { numRead = -1; }
			else { numRead = dwRead; }
		}
#else
		numRead = read(fd, p, numToRead);
#endif

		if (numRead > 0)
		{
			totalRead += numRead;
			p += numRead;
		}

		if(timeout)
		{
			elapsed = ((unsigned long)MillisecondsEpoch() - start);
			if (elapsed >= timeout) { return totalRead; }
		}
		else return numRead;
	}
}


int closeport(int fd)
{
#if defined(_WIN32) && defined(WIN_HANDLE)
	if (fd != -1 && (HANDLE)fd != INVALID_HANDLE_VALUE) { CloseHandle((HANDLE)fd); }
#else
	if (fd != -1) { close(fd); } // && fd != fileno(stdin)
#endif
return 1;
}

