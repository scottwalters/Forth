/* Forth Construction Set, by Scott Walters */
/* Based on Portable FORTH interpreter in C, by Allan Pratt */

#define DEBUG 
#include <stdio.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/mman.h>

#define ORIGIN 0x0100000  // everything starts at the one meg mark
#define MEG    0x0100000  // handy constant

#define NFLF(n) (n+4)
#define NFCF(n) (n+5)
#define NFPF(n) (n+6)
#define CFNF(n) (n-5)
#define IMMEDIATE 0x40
    
int *mem=(void *)ORIGIN;  // general purprose pointer into memory
int *csp;                 // computation stack pointer
int *rsp;                 // return stack pointer

int *ip;	          // instruction pointer, points at:
int *w;	                  // word pointer, pointers at:
void (*p)();              // value in the code field of a word, which points at code.

int fp;                   // for reading and writting forth memory
struct stat fi;           // info about our memory image file

int tmpa, tmpb;           // intermediate computation values
double tmpc;

char *comment;            // debugging...
char *nullstr="";
char *c;
int dbga, dbgb;
char *words[100];

signed char ctov[128]={   // for converting octal/decimal/hex/beyond to binary
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   1,   2,   3,   4,   5,   6,   7,   0,   0,   0,   0,   0,   0,   0,
   0,  10,  11,  12,  13,  14,  15,  16,  17,  18,  19,  20,  21,  22,  23,  24,
  25,  26,  27,  28,  29,  30,  31,  32,  33,  34,   0,   0,   0,   0,   0,   0,
   0,  10,  11,  12,  13,  14,  15,  16,  17,  18,  19,  20,  21,  22,  23,  24,
  25,  26,  27,  28,  29,  30,  31,  32,  33,  34,   0,   0,   0,   0,   0,   0,
};

#define csp1 (csp+1)
#define csp2 (csp+2)
#define csp3 (csp+3)

/* FORTH PRIMITIVES */

/* CSP and RSP stacks are both accessed pre-deincrement and post-increment: stacks grows downwards */

/* words that use w position data: w is addr of code field of word; w+1 is addr of first param field */
docol()      { *(--rsp) = (int)ip; ip = w+1; words[++dbga]=c; } // nest: ip points to first param field entry of word
dosimis()    { ip = (int *)*(rsp++); dbga--; }         // unnest one word - runtime portion of ';'
dolit()      { *(--csp)=*(ip++); }             // value is next word in instruction stream
docon()      { *(--csp) = *(w+1); }            // fetch value in first param field
dovar()      { *(--csp) = (int)(w+1); }        // fetch address of first param field
douser()     { *(--csp) = ORIGIN+*(w+1)*sizeof(int); }     // fetch address of "user variable" at the top of memory
doexecute()  { w=(int *)*(csp++); p=(void(*)())*w; (p)(); } // stack contains cfa of a word to execute
             // execute cannot be used on LIT, (COMMENT), relative branches, or anything else dependent on
             // instruction stream position (ie, on IP)
/*
doexecute() {
  // this is flawed.. we need to set ip to the calling word for (COMMENT), not = to cfa.. 
  // this is the only instruction that allocates a local - 
  // this doesnt give everything a performace hit, does it?
  int oldip=ip;
  ip = (int *)*(csp++);  // take IP from top of computation stack
  w = (int *)*(ip++);    // go from ip to code field address
  p=(void(*)())*w;       // go from code field address to code field's value 
  (p)();                 // call that
  ip=oldip;
}
 */


/* low level - used to minipulate virtual machine state */
doipstore()  { ip = (int *)*(csp++); }         // put the instruction pointer on the stack
doipfetch()  { *(--csp) = (int)ip; }           // use the value on the stack as instruction pointer
dopbye()     { close(fp); exit(0); }           // exit
dorpstore()  { rsp = (int *)mem[2]; }
dospstore()  { csp = (int *)mem[1]; }
dorpfetch()  { *(--csp) = (int)rsp; }             // fetch return stack pointer value
dospfetch()  { *(--csp) = (int)csp; }             // fetch computation stack pointer value

/* debugging... */
dopcomment() { tmpa = *(ip++); comment = (char *)ip; ip += (tmpa+sizeof(int)-1)/sizeof(int); }
dopdump() {
  int *t1;
  int *t2;
  int *t3;
  int *t4;
  char *t5;
  printf("\n(DUMP)\n");
  // for(t1=rsp;t1<mem[5];t1++) {
  for(t1=((int *)mem[5])-1;t1>=rsp;t1--) {
    // for each stack frame... rsp is return stack pointer, mem[5] is R0, stack grows downwards
    for(t2=t1;t2<mem[5];t2++) printf("  ");  
    t3 = *t1; // go from rsp to part of the return stack
    t4 = *(t3-1); // go from part of return stack (previous word) to part of PF of a word were in, which is a CFA
    t5 = (char *)CFNF(t4); t5++; // go from being a CFA to a NFA
    printf("%s\n", t5);
  }
  exit(0);
}

/* primitives that implement branching */
docbranch()    { ip+=*(csp++); }                     // branch by the value on top of the stack
dobranch()     { ip += *ip; }                        // branch by the value in the instruction stream
dozbranch()    { if(!*(csp++)) ip+=*ip; else ip++; } // branch by value in instruction stream if stack top contains 0
dotbranch()    { if(*(csp++)) ip+=*ip; else ip++; }  // branch by value in instruction stream if stack top is true

/* primitives that add looping to branching */
doi()          { *(--csp) = *rsp; }            // index value of present loop onto stack
doleave()      { *(rsp+1)=*rsp; }              // set the index to the limit of a do
doploop()      { if(++*(rsp) < *(rsp+1))       // inc index; if index is less then max, branch (loop)
                   ip+=*ip;                    // otherwise, skip over branch immed value and drop index and max
                 else { ip++; rsp+=2; } }
dopploop()     {                               // comp stack points to value to add to index;
  *rsp+=*(csp++);                              // while index is less then limit, loop, otherwise skip
  if(*rsp < *(rsp+1))  ip+=*ip;                // branch value and drop limit and index from return stack.
  else { ip++; rsp+=2; }
}

/* storing and fetching from memory */
docstore()     { *((char *)(*csp))=*csp1&0xff; csp+=2; } // store a byte at a memory address
docfetch()     { *csp=*((unsigned char *)(*csp)); }      // fetch a byte from a memory address
docmove()      { 
for (;*csp;(*csp)--) {                    // move a regeion of memory
                 *(int *)*csp1=*(int *)*csp2;
                 (*csp1)++;
                 (*csp2)++;
                 }
                 csp+=3; 
               }
dofetch()      { *csp=*((int *)(*csp)); }           // fetch a word from a memory address
dostore()      { *((int *)(*csp))=*csp1; csp+=2; }  // store a word to a memory address
dopstore()     { *((int *)(*csp))+=*csp1; csp+=2; } // add a word to a value at an address
dofromr()      { *(--csp)=*(rsp++); }               // move a value from return stack to computation stack
dotor()        { *(--rsp)=*(csp++); }               // move a value from computation stack to return stack
dopick()       { *csp=*(csp+*csp); }                // copy datum from position /n/ on stack to top of stack

/* 
  this is coded in Forth now
dofill()       {                                    // fill a region with a byte value
    *csp+=*(csp+1);
    for(;*(csp+1)<*csp;*(csp)++) *(csp+1)=*(csp+2);
    csp+=3;
}
 */

/* i/o */
/* doprint()      { printf((char *)*csp,*csp1,*csp2,*csp3,*(csp+4),*(csp+5)); } */
/*
dotype()       { tmpa=(char *)(((int)*csp)+1+((int)*csp1));
                 tmpb=(char)*(char *)tmpa; *(char *)tmpa=(char)0; 
                 printf((char *)*csp1); *(char *)tmpa=(char)tmpb; csp+=2;  }
 */
/* dotype()       { printf("%s\n", *(csp++)); } */
/* dotype()       { while(*csp) { (*csp)--; printf("%c", *(csp+1)); (*csp+1)++; } csp-=2; } */
dotype()       { for(tmpa=*(csp+1);tmpa<*(csp+1)+*csp;tmpa++) { printf("%c", *(char *)tmpa); }; csp+=2; }
doexpect()     { fgets((char *)*csp1,*csp,stdin); csp+=2; }

/* general computational primitives */ 
doltlt()       { *csp1<<=*csp; csp++; }
dogtgt()       { *csp1>>=*csp; csp++; }
dominusminus() { (*(int *)*csp)--; csp++; } 
doplusplus()   { (*(int *)*csp)++; csp++; }
doand()        { *csp1 = *csp1 & *csp; csp++; } 
dodrop()       { csp++; }
dodup()        { csp--; *csp = *csp1; }
doequal()      { *csp1=*csp==*csp1; csp++; }
doless()       { *csp1=*csp1<*csp; csp++; }
dominus()      { *csp=-*csp; }
donoteq()      { *csp1=*csp!=*csp1; csp++; }
door()         { *csp1 = *csp1 | *csp; csp++; }
doover()       { *(csp-1)=*csp1; csp--; }
dopdo()        { *(--rsp)=*csp1; *(--rsp)=*csp; csp+=2; }
doplus()       { *csp1+=*csp; csp++; }
dor()          { *(--csp) = *rsp; }
dorot()        { tmpa=*csp2; *csp2=*csp1; *csp1=*csp; *csp=tmpa; }
dosubtract()   { *csp1-=*csp; csp++; }
doswap()       { tmpa=*csp; *csp=*csp1; *csp1=tmpa; }
dotdup()       { *(csp-1)=*csp1; *(csp-2)=*csp; csp-=2; }
dotoggle()     { *(char *)*csp1^=*csp; csp++; }
doxor()        { *csp1 = *csp1 ^ *csp; csp++; }
dozeq()        { *csp = *csp==0; }
dozless()      { *csp = *csp<0; }

dodigit()      {
    if(*csp1>0x7f || ctov[*csp1]>=*csp) *(++csp) = 0;
    else { *csp1=ctov[tmpa]; *csp=1; }
}

dostar()       { *csp1 = *csp1 * *csp; csp++; }
doslash()      { *(csp+1) = *(csp+1) / *csp; csp++; }

dodminus() {
    if(--*csp1==0xffffffff) (*csp)--;
    *csp=~*csp; *csp1=~*csp1;
}

dopsave() {
  printf("\nSaving..."); fflush(stdout);
  fp =  open("forth.mem", 0, O_RDWR);
  printf("\nWrote %d bytes.\n",
    sizeof(int)*
    write(fp, mem, mem[3]*sizeof(int))   // mem[3] is DP
  );
  if    (close(fp) == -1) perror("Couldn't close file!");
  else  printf("Saved.\n");
  exit(0);
}

/* (FIND) (defunct) */
    /* WORD TOP -- xx FLAG, where TOP is NFA to start at; WORD
                   is the word to find; xx is PFA of found word; yy is
                   actual length of the word found;
                   FLAG is 1 if found. If not found, 0 alone is stacked. */

struct {
  int addr;
  char *name;
} prims[] = {
  {(int)docol, "DOCOL"}, {(int)docon, "DOCON"}, {(int)dovar, "DOVAR"}, {(int)douser, "DOUSE"},
  {(int)dobranch, "BRANCH"}, {(int)dozbranch, "0BRANCH"}, {(int)dotbranch, "1BRANCH"},
  {(int)dopdo, "(DO)"}, {(int)doploop, "(LOOP)"}, {(int)dopploop, "(+LOOP)"}, {(int)doleave, "LEAVE"},
  {(int)doi, "I"}, {(int)dor, "R"}, {(int)dofromr, "R>"}, {(int)dotor, ">R"},
  {(int)doequal, "="}, {(int)doless, "<"}, {(int)donoteq, "!="},
  {(int)dominusminus, "--"}, {(int)doplusplus, "++"}, 
  {(int)dopstore, "+!"},
  {(int)doand, "AND"}, {(int)door, "OR"}, {(int)doxor, "XOR"}, {(int)dotoggle, "TOGGLE"},
  {(int)dostore, "!"}, {(int)dofetch, "@"}, {(int)docfetch, "C@"}, {(int)docstore, "C!"},
  {(int)dotype, "TYPE"}, {(int)docmove, "CMOVE"}, {(int)doexpect, "EXPECT"},
  {(int)dodrop, "DROP"}, {(int)dodup, "DUP"}, {(int)doover, "OVER"}, {(int)doswap, "SWAP"}, 
  {(int)dorot, "ROT"}, {(int)dopick, "PICK"},
  {(int)dotdup, "2DUP"},
  {(int)doexecute, "EXECUTE"}, {(int)dolit, "LIT"}, {(int)dosimis, ";S"},
  {(int)dominus, "MINUS"}, {(int)doplus, "+"}, {(int)dosubtract, "-"},
  {(int)dorpfetch, "RP@"}, {(int)dorpstore, "RP!"}, 
  {(int)dospfetch, "SP@"}, {(int)dospstore, "SP!"},
  {(int)doipfetch, "IP@"}, 
  {(int)dozeq, "0="}, {(int)dozless, "0<"},
  {(int)dodigit, "DIGIT"}, {(int)dostar, "*"}, {(int)doslash, "/"}, {(int)dodminus, "DMINUS"},
  {(int)dopbye, "(BYE)"}, {(int)dopsave, "(SAVE)"}, {(int)dopdump, "(DUMP)"},
  {(int)dopcomment, "(COMMENT)"},

  {(int)docbranch, "??? cbranch"},
  /* {(int)doprint, "??? print"}, */
  /* {(int)dofill, "??? fill"},   -- this is coded in Forth now */

  {0, ""} 
};

main(argc,argv)
  int argc;
  char *argv[]; {

  int *addx;
  int *addy; 
  int *addz; 

  int i;

  printf("Forth Construction Set starting up... main located at %lx, ORIGIN is %lx\n", (int)main, ORIGIN);

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

  // forth.mem doesn't suffer from things unexpectedly moving around in memory, but
  // this C interpreter does! when we get recompiled, all of our symbols move. hence,
  // we must implement a sort of dynamic linking of the primitive words to where their
  // code is present located. this assumes that the order of the primitives never changes.
  // XXX put some 'overflow' words into forth.dict, in case add more prims later, people can keep using their old images.
  
  // for each primitive, locate that primitive, find the old CF value, then replace each instantance with new CF val


  for(i=0;prims[i].addr;i++) {
    addy = (int *)*(mem+40);    // current/latest
    while(addy) {
      if(strcmp(prims[i].name, 1+(char *)addy) == 0) break;
      addy = (void *)*NFLF(addy);
    }
    if(!addy) {
      printf("Failed to locate word \"%s\" in dictionary.\n", prims[i].name);
    } else {
      printf("%d: %s (%lx):", i, prims[i].name, prims[i].addr);
      addx = (void *)*NFCF(addy);      // note the old address
      addz = (int *)*(mem+40);
      while(addz) {
        // replace each instance of the old primitive codefield address with the new address.
        // in practise, only var, const and docol get stuck into the cf's of random words, though.
        if(*NFCF(addz) == (int)addx) {
          *NFCF(addz) = prims[i].addr;
          printf("."); fflush(stdout); // debug
        }
        addz = (void *)*NFLF(addz);  
      }
    }
    printf("\n");
  }

  ip=(int *)mem[0];
  csp=(int *)mem[1];
  rsp=(int *)mem[2];
  mem[7]=sizeof(int);  // wordsize

/* this is my fuck-you sweet inner loop so bite me */
  mainloop:
    w = (int *)*(ip++);

#ifdef DEBUG
    printf("%16s ", words[dbga]?words[dbga]+1:nullstr);
    for(dbgb=0;dbgb<dbga;dbgb++) printf("  ");
    c = (char *)CFNF(w); printf("%lx: %lx: (top: %lx %lx) %s      %s\n", ip, w, *csp, *csp1, c+1, comment);  comment=nullstr; // debugging...
#endif /* DEBUG */

    p = (void (*)()) *w;
    (p)();
  goto mainloop;
}

