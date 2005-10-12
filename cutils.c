#include <sys/types.h>
#include <sys/socket.h>
#include <sys/time.h>

void configsock(int sock) {
  struct timeval t;
  t.tv_sec = 45;
  t.tv_usec = 0;

  setsockopt(sock, SOL_SOCKET, SO_RCVTIMEO, (void *)&t, sizeof(t));
  setsockopt(sock, SOL_SOCKET, SO_SNDTIMEO, (void *)&t, sizeof(t));

}



