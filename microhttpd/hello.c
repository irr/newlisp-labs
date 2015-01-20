#include <stdarg.h>
#include <stdint.h>
#include <string.h>

#include <sys/types.h>
#include <sys/select.h>
#include <sys/socket.h>
#include <microhttpd.h>

#define PORT 8081

static int answer_to_connection (void *cls, 
                                 struct MHD_Connection *connection,
                                 const char *url, 
                                 const char *method,
                                 const char *version, 
                                 const char *upload_data,
                                 size_t *upload_data_size, 
                                 void **con_cls)
{
  if (0 != strcmp(method, "GET")) {
    return MHD_NO;
  }

  const char *page = "Hello, newLISP!";

  struct MHD_Response *response = MHD_create_response_from_data(strlen(page), 
                                                                (void *) page, 
                                                                MHD_YES, 
                                                                MHD_YES);
  int ret = MHD_queue_response(connection, MHD_HTTP_OK, response);
  MHD_destroy_response(response);

  return ret;
}

int main ()
{
  struct MHD_Daemon *daemon;

  daemon = MHD_start_daemon(MHD_USE_SELECT_INTERNALLY, PORT, 0, 0, 
                            &answer_to_connection, 0, MHD_OPTION_END);
  if (0 == daemon) {
    return 1;
  }

  getchar();

  MHD_stop_daemon(daemon);
  
  return 0;
}

/* 
gcc -g hello.c -o hello -l microhttpd
valgrind --leak-check=full ./hello

sudo apt-get install libmicrohttpd-dev
sb /usr/include/microhttpd.h
*/