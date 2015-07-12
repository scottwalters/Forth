// read in forth.dict and dump it, so that we can make sure nf.c is doing its job...

#include <stdio.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/mman.h>
  
#define ORIGIN 0x0100000  // everything starts at the one meg mark
#define MEG    0x0100000  // handy constant

int *mem = ORIGIN;

int fp;
struct stat fi;

int *addx;  // disassembly pointer into body
int *addy;  // follows link fields from end to beginning
int *addz;  // remembers start of last word so we dont disassemble too far

int *tmp;
char *tmpc;

int memzero;

int compilef;

#define NFLF(n) (n+4)
#define NFCF(n) (n+5)
#define NFPF(n) (n+6)
#define CFNF(n) (n-5)
#define IMMEDIATE 0x40

main() {
  if(-1 == (fp = open("forth.mem", 0, O_RDWR))) {
    perror("failed to open forth.mem - no you need to run nf first to initialize it?\n");
    exit(1);
  }
   
  if(-1 == fstat(fp, &fi)) {
    perror("failed to stat forth.mem - this shouldnt ever happen\n");
    exit(2);
  }
   
  // mmap(void *addr, size_t len, int prot, int flags, int fd, off_t offset);
  if((void *)ORIGIN != mmap((void *)ORIGIN, fi.st_size + 10*MEG,
    PROT_READ|PROT_WRITE|PROT_EXEC,
    MAP_FIXED|MAP_INHERIT|MAP_PRIVATE,  // private untill/unless we get some really good locking going in the dict...
    fp,
    0  
  )) {
    printf("failed to map memory at fixed address of %lx .. perhaps the exe has bloated too much?\n", ORIGIN);
    exit(3);
  }

  // addy=mem+1024;    // skip ip, csp, rsp, and the other user variables, tib, stacks, into first name field
  addy = mem[40];    // "current"/latest
  memzero = *(int *)mem[0];

  printf("current/latest was stored as being: %lx\n", addy);
  printf("DP was stored as being: %lx\n", *(mem+3));
  printf("TIB was stored as being: %lx\n", *(mem+6));

  while(addy) {
    printf("\n%s", ((char *)addy)+1); 
    if(*addy & IMMEDIATE) printf(" immediate ");
    printf("\n");
    printf("code field: %lx\n", *NFCF(addy));
    printf("link field: %lx\n", *NFLF(addy));
    addx=NFPF(addy);
    if(memzero == NFCF(addy)) printf("** take-control word: takes control on startup\n");
    else if(memzero >= addx && memzero <= addz) printf("** take-control word: take control points in here somewhere, but not to our code field! error!\n");
    while(addx<addz) {
      tmp = *addx;
      tmpc = (char *)(CFNF(tmp)); tmpc++;  // skip the length/flags byte in the name field
      if(tmp>MEG && tmp<MEG*2) {
        printf("%s ", tmpc);
        // special cases where next values in stream aren't instructions but are instead data.
        if(compilef) {
          compilef=0;
        } else if(!strcmp(tmpc, "COMPILE")) {
          compilef=1; // this tells us not to dump-data for the next word, as the next word isnt really there
        } else if(!strcmp(tmpc, "0BRANCH") || !strcmp(tmpc, "BRANCH") || !strcmp(tmpc, "LIT")) {
          addx++; printf("0x0%lx ", *addx);
        } else if(!strcmp(tmpc, "(.\")") || !strcmp(tmpc, "(COMMENT)")) {
          int len;
          char *c;
          addx++; len = *addx; addx++;
          c = (char *)addx;
          // addx += (len*sizeof(int)+sizeof(int)-1)/sizeof(int); // this appears to work
          addx += (len+sizeof(int)-1)/sizeof(int); addx--;
          printf("%c", '"');
          while(len--) printf("%c", *(c++));
          printf("%c\n", '"');
          // printf("%c%s%c ", '"', addx, '"');
          // len+=sizeof(int)-1; addx+=len/sizeof(int); // skip to word after end of string
          // addx++;
        }
      } else {
        printf("(???) %lx ", tmp);
      }
      addx++;
    }
    printf("\n");
    addz = addy;
    addy = (void *)*NFLF(addy);
  }
  printf("\nend of dictionary\n");
  exit(0);
}

