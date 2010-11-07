CFLAGS += -I/usr/include/dbus-1.0 -I/usr/lib/dbus-1.0/include \
	-I/usr/include/glib-2.0 -I/usr/lib/glib-2.0/include
LDFLAGS += -lglib-2.0 -l dbus-glib-1

all: try-dbus dbus-example

clean:
	rm -f *~ *.o try-dbus dbus-example
