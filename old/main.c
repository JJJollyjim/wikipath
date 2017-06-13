#include <expat.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <unistd.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <fcntl.h>


static bool in_title = false;
static bool in_text = false;

void start_element(void *data, const char *el, const char **attribute) {
  if (strcmp(el, "title") == 0) {
    in_title = true;
  } else if (strcmp(el, "text") == 0) {
    in_text = true;
  }
}

void end_element(void *data, const char *el) {
  if (strcmp(el, "title") == 0) {
    in_title = false;
    fputc('\0', stdout);
  } else if (strcmp(el, "text") == 0) {
    in_text = false;
    fputc('\0', stdout);
  }
}

void handle_data(void *data, const char *content, int length) {
  if (in_title) {
    fwrite(content, length, 1, stdout);
  } else if (in_text) {
    fwrite(content, length, 1, stdout);
  }
}


int main(int argc, char *argv[]) {
  if (argc < 2) return 1;
  int fd;

  fd = open(argv[1], O_RDONLY);
  if (fd == -1) return 2;

  struct stat sb;
  if (fstat(fd, &sb) == -1) return 3;

  const char* data = mmap(NULL, sb.st_size, PROT_READ, MAP_PRIVATE, fd, 0);

  XML_Parser p = XML_ParserCreate(NULL);
  XML_SetElementHandler(p, start_element, end_element);
  XML_SetCharacterDataHandler(p, handle_data);

  unsigned long long int position = 0;
  unsigned long long int len = 0;
  int perc;
  int perc1;
  while (position < sb.st_size) {
    position += len;
    len = (sb.st_size - position) > (64*1024*1024) ? 64*1024*1024 : (sb.st_size - position);

    perc = (position*100)/sb.st_size;
    if (perc != perc1)
      fprintf(stderr, "%d\%\n", perc);
    perc1 = perc;

    /* if (perc > 10) return 0; */

    if (XML_Parse(p, data + position, len, len == 0) != XML_STATUS_OK) {
      fprintf(stderr, "error: %s at %ld", XML_ErrorString(XML_GetErrorCode(p)), XML_GetErrorByteIndex(p));
      return 5;
    }
  }

}

