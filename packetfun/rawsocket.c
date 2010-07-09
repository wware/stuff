/*
 * gcc -c -o rawsocket.o rawsocket.c -I/usr/include/python2.5
 * gcc -shared -o rawsocket.so rawsocket.o -lpython2.5 -L/usr/lib/python2.5
 */

#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <netdb.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <linux/if_ether.h>

#include "Python.h"

#define DEFAULT_DEVICE "eth0"

static PyObject *
rawsocket_socket(PyObject * self, PyObject * args)
{
	int sock;
	int protocol;
	if (!PyArg_ParseTuple(args, "i", &protocol))
		return NULL;
	sock = socket(AF_INET, SOCK_PACKET, htons(protocol));
	if (sock < 0) {
		PyErr_SetString(PyExc_IOError,
				"can't allocate a socket");
		return NULL;
	}
	return PyInt_FromLong(sock);
}

static PyObject *
rawsocket_sendto(PyObject * self, PyObject * args)
{
	int sock;
	char *packet;
	int packetlen;
	char *device;
	struct sockaddr sa;
	device = DEFAULT_DEVICE;
	if (!PyArg_ParseTuple(args, "is#|s",
			      &sock, &packet, &packetlen, &device))
		return NULL;
	strcpy(sa.sa_data, device);
	if (sendto(sock, packet, packetlen, 0, &sa, sizeof(sa)) < 0) {
		PyErr_SetString(PyExc_IOError,
				"socket sendto unsuccessful");
		return NULL;
	}
	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *
rawsocket_recvfrom(PyObject * self, PyObject * args)
{
	int sock;
	const int MAX_RECEIVE_PACKET = 1000;
	char packet[MAX_RECEIVE_PACKET];
	int packetlen;
	if (!PyArg_ParseTuple(args, "i", &sock))
		return NULL;
	packetlen = recvfrom(sock, packet, MAX_RECEIVE_PACKET, 0,
			     NULL, NULL);
	if (packetlen < 0) {
		PyErr_SetString(PyExc_IOError,
				"socket recvfrom unsuccessful");
		return NULL;
	}
	return PyString_FromStringAndSize(packet, packetlen);
}

static PyMethodDef rawsocket_methods[] = {
	{"socket", rawsocket_socket, 1},
	{"sendto", rawsocket_sendto, 1},
	{"recvfrom", rawsocket_recvfrom, 1},
	{NULL, NULL}		/* sentinel */
};

DL_EXPORT(void)
     initrawsocket()
{
	Py_InitModule("rawsocket", rawsocket_methods);
}
