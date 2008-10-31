#include <pwd.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include "hspwd.h"

char *getHomeDir(const char *c) {
  struct passwd *p = getpwnam(c);
  if (!p) return NULL;
  p->pw_dir;
  int n = strlen(p->pw_dir);
  char *ret = malloc((n+1)*sizeof(char));
  strcpy(ret,p->pw_dir);
  return ret;
}
