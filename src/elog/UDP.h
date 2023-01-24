/*
	UDP socket usage
*/
#ifndef _UDP_H_
#define _UDP_H_

/* Fake return type to maximise useability*/
#ifndef SOCKET
#ifdef _WIN32
	typedef UINT_PTR SOCKET;
#else
	typedef int SOCKET;
#endif
#endif

/* Hide server struct */
void* makeServer(void);

/* Open a UDP socket*/
SOCKET opensocket(const char *host, int defaultPort, struct sockaddr_in *serverAddr);

/* UDP-transmit a packet */
int transmit(SOCKET s, struct sockaddr_in *serverAddr, const void *sendBuffer, size_t sendLength);

/* UDP-receive a packet */
int receive(SOCKET s, struct sockaddr_in *serverAddr, void *receiveBuffer, size_t receiveLength);

void UdpCleanup (void * server, SOCKET s);

#endif
