#if defined (__WINDOWS__)
#ifndef getline_H
#define getline_H

extern "C" {
  size_t getline (char **lineptr, size_t *n, FILE *stream);
}

#endif
#endif
