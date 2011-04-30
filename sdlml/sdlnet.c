/* Hooks and glue for SDL_net. Incomplete. */
#include <SDL.h>
#include <SDL_net.h>

/*
Uint32 ml_htonl(Uint32 i) {
  return ((i >> 24) & 255) |
    (((i >> 16) & 255) << 16) |
    (((i >> 8) & 255) << 8) |
    (((i >> 0) & 255) << 24);
}
*/

inline Uint16 ml_htons(Uint16 i) {
#if SDL_BYTEORDER == SDL_LIL_ENDIAN
  return ((i >> 8) & 255) | ((i & 255) << 8);
#else
  return i;
#endif
}

int ml_resolveip(Uint32 addr, char *dest, int maxlen) {
  IPaddress ip;
  ip.host = addr;
  const char *s = SDLNet_ResolveIP(&ip);
  printf("from resolveip got: %p\n", s);
  if (!s) return -1;
  int len = strlen(s);
  if (len > maxlen) return -1;
  {
    int i;
    for (i = 0; i < len; i++) dest[i] = s[i];
  }
  return len;
}

/* return 0 on success */
int ml_resolvehost(char *name, Uint32 *addr) {
  IPaddress ip;
  if (0 == SDLNet_ResolveHost(&ip, name, 0)) {
    // XXX experienced some backwardsness here at some
    // point, possibly a mingw bug? Don't know what
    // the resolution was. -tom 25 Dec 2009
#ifdef WIN32
    // ip.host = ml_htonl(ip.host);
#else
    // XXX Apparently I gave up on other platforms,
    // but there probably is something we're supposed
    // to do here. Find and fix! -tom7  23 May 2010
    // # error sorry
#endif
    printf("resolve: %d.%d.%d.%d\n",
	   (ip.host >> 24) & 255,
	   (ip.host >> 16) & 255,
	   (ip.host >> 8) & 255,
	   (ip.host >> 0) & 255);
	   
    *addr = ip.host;
    return 0;
  } else {
    return -1;
  }
}

TCPsocket ml_tcp_open(Uint32 addr, int port) {
  IPaddress ip;
  ip.host = addr;
  ip.port = ml_htons(port);
  printf("%8x : %d\n", addr, port);
  return SDLNet_TCP_Open(&ip);
}

void ml_getpeeraddress(TCPsocket sock, Uint32 *addr, int *port) {
  // XXX probably needs to ntohs
  IPaddress *ip = SDLNet_TCP_GetPeerAddress(sock);
  if (!ip) {
    *addr = 0;
    *port =0;
  } else {
    *addr = ip->host;
    *port = ip->port;
  }
}

int ml_send_offset(TCPsocket sock, char *bytes, int offset, int len) {
  return SDLNet_TCP_Send(sock, bytes + offset, len);
}

