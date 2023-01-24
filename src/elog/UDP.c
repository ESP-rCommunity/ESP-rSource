/* Cross-platform alternatives */
#ifdef _WIN32

#define WIN_HANDLE

    /* Defines and headers */
    #define _CRT_SECURE_NO_WARNINGS
    #define _CRT_SECURE_NO_DEPRECATE
    #include <windows.h>
    #include <io.h>
    
    /* Strings */
    #define strcasecmp _stricmp
    #define snprintf _snprintf    

    /* Files */
    #include <direct.h>
    #define mkdir(path, mode) _mkdir(path)

    /* Sleep */
    #define sleep(seconds) Sleep(seconds * 1000UL)
    #define usleep(microseconds) Sleep(microseconds / 1000UL)

    /* Time */
    #define gmtime_r(timer, result) gmtime_s(result, timer)
    #define timegm _mkgmtime

    /* Socket */
    #include <winsock.h>
    #define _POSIX_
    typedef int socklen_t;
    #pragma warning( disable : 4996 )    /* allow deprecated POSIX name functions */
    #pragma comment(lib, "wsock32")
	#define socketErrno   (WSAGetLastError())
	#define SOCKET_EWOULDBLOCK WSAEWOULDBLOCK

    /* Thread */
	#define thread_t HANDLE
    #define thread_create(thread, attr_ignored, start_routine, arg) ((*(thread) = CreateThread(attr_ignored, 0, start_routine, arg, 0, NULL)) == NULL)
    #define thread_join(thread, value_ptr_ignored) ((value_ptr_ignored), WaitForSingleObject(thread, INFINITE) != WAIT_OBJECT_0)
    #define thread_cancel(thread) (TerminateThread(thread, -1) == 0)
    #define thread_return_t DWORD WINAPI
    #define thread_return_value(value) ((unsigned int)(value))

    /* Mutex */
	#define mutex_t HANDLE
    #define mutex_init(mutex, attr_ignored) ((*(mutex) = CreateMutex(attr_ignored, FALSE, NULL)) == NULL)
    #define mutex_lock(mutex) (WaitForSingleObject(*(mutex), INFINITE) != WAIT_OBJECT_0)
    #define mutex_unlock(mutex) (ReleaseMutex(*(mutex)) == 0)
    #define mutex_destroy(mutex) (CloseHandle(*(mutex)) == 0)

    /* Device discovery */
    #include <setupapi.h>
    #ifdef _MSC_VER
        #include <cfgmgr32.h>
    #endif
    #pragma comment(lib, "setupapi.lib")
    #pragma comment(lib, "advapi32.lib")    /* For RegQueryValueEx() */

#else

    /* Sockets */
	#include <sys/ioctl.h>
    #include <unistd.h>
    #include <sys/wait.h>
    #include <sys/types.h>
    #include <sys/socket.h>
    #include <netinet/in.h>
    #include <arpa/inet.h>
    #include <netinet/tcp.h>
    #include <netdb.h>
    #include <termios.h>
    typedef int SOCKET;
    #define SOCKET_ERROR (-1)
    #define closesocket close
    #define ioctlsocket ioctl
	#define socketErrno errno
	#define socketStrerr(_e) strerror(_e)
	#define SOCKET_EWOULDBLOCK EWOULDBLOCK

    /* Thread */
    #include <pthread.h>
    #define thread_t      pthread_t
    #define thread_create pthread_create
    #define thread_join   pthread_join
    #define thread_cancel pthread_cancel
    typedef void *        thread_return_t;
    #define thread_return_value(value_ignored) ((value_ignored), NULL)

    /* Mutex */
	#define mutex_t       pthread_mutex_t
    #define mutex_init    pthread_mutex_init
    #define mutex_lock    pthread_mutex_lock
    #define mutex_unlock  pthread_mutex_unlock
    #define mutex_destroy pthread_mutex_destroy

#endif


/* Includes */
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <errno.h>
#include <fcntl.h>
#include <time.h>
#include <sys/timeb.h>
#include <sys/stat.h>

void* makeServer(void)
{
	return malloc(sizeof(struct sockaddr_in));
}

/* Returns a socket error string */
#ifdef _WIN32
const char *socketStrerr(int e)
{
    static char errorString[256];
    if (FormatMessageA(FORMAT_MESSAGE_FROM_SYSTEM, 0, e, 0, errorString, 255, NULL))
    {
        return errorString;
    }
    sprintf(errorString, "<unknown #%d 0x%x>", e, e);
    return errorString;
}
#endif


/* Open a UDP socket*/
SOCKET opensocket(const char *host, int defaultPort, struct sockaddr_in *serverAddr)
{
    SOCKET s = SOCKET_ERROR;
    char serverName[128] = "localhost"; /* "localhost"; "127.0.0.1"; */
    char *portIndex;
    int serverPort = defaultPort;
    struct hostent *hp;

	#ifdef _WIN32
    {
        WSADATA wsaData;
        WSAStartup(MAKEWORD(1, 1), &wsaData);
    }
	#endif

    /* assigned parameters */
    if (host != NULL && strlen(host) > 0)
    {
        strcpy(serverName, host);
        if ((portIndex = strstr(serverName, ":")) != NULL)
        {
            *portIndex++ = '\0';
            serverPort = atoi(portIndex);
        }
    }

    /* get server host information, name and address */
    hp = gethostbyname(serverName);
    if (hp == NULL)
    {
        serverAddr->sin_addr.s_addr = inet_addr(serverName);
        hp = gethostbyaddr((char *)&serverAddr->sin_addr.s_addr, sizeof(serverAddr->sin_addr.s_addr), AF_INET);
    }
    if (hp == NULL)
    {
        fprintf(stderr, "ERROR: Problem getting host socket information (%s)\n", socketStrerr(socketErrno));
        return SOCKET_ERROR;
    }
    memcpy(&(serverAddr->sin_addr), hp->h_addr, hp->h_length);
    serverAddr->sin_family = AF_INET; 
    serverAddr->sin_port = htons(serverPort);
    fprintf(stderr, "DEBUG: Server address: [%s] = %s : %d\n", hp->h_name, inet_ntoa(serverAddr->sin_addr), serverPort);
    s = socket(AF_INET, SOCK_DGRAM, 0); 
    if (s < 0) 
    {
        fprintf(stderr, "ERROR: Socket creation failed (%s)\n", socketStrerr(socketErrno));
        return SOCKET_ERROR;
    }

	/* Allow rapid reuse of this socket */
	{
		int option = 1;
		setsockopt(s, SOL_SOCKET, SO_REUSEADDR, (char *)&option, sizeof(option));
	}
    
	/* Bind the socket */
	if (bind(s, (struct sockaddr *) serverAddr, sizeof(struct sockaddr)) == SOCKET_ERROR)
	{
		fprintf(stderr, "UDPRECV: ERROR: Socket bind failed (%s)\n", socketStrerr(socketErrno));
		return 6;
	}

	// Set non-blocking
	{
		int value = 1;
		ioctlsocket(s, FIONBIO, &value);
	}
	
    return s;
}


/* UDP-transmit a packet */
int transmit(SOCKET s, struct sockaddr_in *serverAddr, const void *sendBuffer, size_t sendLength)
{
    int sent = 0;
    if (sendLength > 0)
    {
        fprintf(stderr, "[%d]\n", (unsigned int)sendLength);
        if (sendto(s, (const char *)sendBuffer, sendLength, 0, (struct sockaddr *)serverAddr, sizeof(*serverAddr)) == SOCKET_ERROR)
        {
            fprintf(stderr, "ERROR: Send failed (%s)\n", socketStrerr(socketErrno));
            return 0;
        }
        sent += (size_t)sendLength;
    }
    return sent;
}


/* UDP-receive a packet */
int receive(SOCKET s, struct sockaddr_in *serverAddr, void *receiveBuffer, size_t receiveLength)
{
	int len, lenServ = sizeof(struct sockaddr_in);

	len = recvfrom(s, (char *)receiveBuffer, (int)receiveLength, 0, serverAddr, &lenServ);

	if (len == 0 || len == SOCKET_ERROR)
	{ 
		if(socketErrno != SOCKET_EWOULDBLOCK)
		{
			fprintf(stderr, "UDPRECV: ERROR: Receive failed (%s)\n", socketStrerr(socketErrno)); 
			return -1;
		}
		return 0; 
	}

	fprintf(stderr, "UDPRECV: Received from %s:%d\n", inet_ntoa(serverAddr->sin_addr), ntohs(serverAddr->sin_port));

	return len;
}			

void UdpCleanup (void * server, SOCKET s)
{
	// Close socket
	if(s != 0)	closesocket(s);
	// Free server - assumes if a server exists then WSA code was called too
	if(server != NULL) 
	{
		free(server);

		#ifdef _WIN32
			WSACleanup();
		#endif
	}
}

