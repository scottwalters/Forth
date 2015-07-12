/*
 * nf.c -- this program can be run to generate a new environment for the
 * FORTH interpreter forth.c. It takes the dictionary from the standard input.
 * Normally, this dictionary is in the file "forth.dict", so 
 *	nf < forth.dict
 * will do the trick.
 */

#include "forth.lex.h"
#include <stdio.h>
#include <ctype.h>
#include <sys/mman.h>
/* #include "common.h" */

#define ORIGIN 0x0100000  // everything starts at the one meg mark
#define MEG    0x0100000  // handy constant

extern int yylineno;
char s[65536];

#define isoctal(c)	(c >= '0' && c <= '7')	/* augument ctype.h */

#define assert(c,s)	(!(c) ? failassert(s) : 1)
#define chklit()	(!prev_lit ? dictwarn("Qustionable literal") : 1)

#define LINK struct linkrec
#define CHAIN struct chainrec

struct chainrec {
    char chaintext[32];
    int defloc;				/* CFA or label loc */
    int chaintype;			/* 0=undef'd, 1=absolute, 2=relative */
    CHAIN *nextchain;
    LINK *firstlink;
};

struct linkrec {
    int loc;
    LINK *nextlink;
};

CHAIN firstchain;

#define newchain()	(CHAIN *)(calloc(1,sizeof(CHAIN)))
#define newlink()	(LINK *)(calloc(1,sizeof(LINK)))

CHAIN *find();
CHAIN *lastchain();
LINK *lastlink();

char *strcat();
char *calloc();

int dp;
int latest;

int *mem;
char *cmem;

FILE *outf, *fopen();
FILE *ffp;

main(argc, argv)
int argc;
char *argv[];
{
    // mmap(void *addr, size_t len, int prot, int flags, int fd, off_t offset);
    if(MAP_FAILED == (mem = mmap((void *)ORIGIN, MEG, PROT_READ|PROT_WRITE, MAP_ANON|MAP_PRIVATE, -1, 0))) {
      printf("failed to map a meg of memory\n");
      exit(1);
    }

    cmem = (char *)mem;
    puts("Opening output file");

    strcpy(firstchain.chaintext," ** HEADER **");
    firstchain.nextchain = NULL;
    firstchain.firstlink = NULL;

    puts("Call Buildcore");
    buildcore();
    puts("call builddict");
    builddict();
    puts("Make FORTH and COLDIP");
    mkrest();
    puts("call checkdict");
    checkdict();
    puts("call writedict");
    writedict();

    printf("%s: done.\n", argv[0]);
}

buildcore()			/* set up low core */
{
  dp=0;
  dp+=256; // skip user vars page
  mem[6] = ORIGIN+dp*sizeof(int);  // TIB (text-in buffer)
  dp+=256; 
  dp+=256; // skip over computation stack area: stack grows down into here
  mem[1] = mem[4] = ORIGIN+(dp-1)*sizeof(int);  // computation stack & s0
  dp+=256; // return stack grows down into here
  mem[2] = mem[5] = ORIGIN+(dp-1)*sizeof(int);  // return stack & r0

  // finally, we start creating words here:
  mem[3] = ORIGIN+dp*sizeof(int);  // data pointer... this gets updated with new dp value before we bail
}

builddict() {
    /* read the dictionary */
    int prev_lit = 0, lit_flag = 0;
    int temp;
    TOKEN *token;

    // extern FILE * yyin;
    // freopen("forth.dict", "r", yyin); // can we only do this if we decide input is a terminal?

    FILE * yymyin;
    yymyin = fopen("forth.dict", "r");
    if(!yymyin) {
      perror("failed to open forth.dict for read");
      exit(1);
    }
    yyrestart(yymyin);

    while ((token = yylex()) != NULL) {	/* EOF returned as a null pointer */
	printf("\ntoken: %s: %d ",token->text, token->type);
	switch (token->type) {

	case PRIM:
	    printf("primitive ");
	    if ((token = yylex()) == NULL)	/* get the next word */
		dicterr("No word following PRIM");
	    strcpy (s,token->text);
	    printf("-> %s <-",s);
	    if ((token == yylex()) == NULL)	/* get the value */
		dicterr("No value following PRIM <word>");
            printf("Prim CF 0x%x\n", mkval(token));
	    mkword(s,mkval(token));  
	    break;

	case CONST:
	    printf("constant ");
	    if ((token = yylex()) == NULL)	/* get the word */
		dicterr("No word following CONST");
	    strcpy (s,token->text);		/* s holds word */
	    printf("-> %s <-",s);
	    if (!find("DOCON"))
		dicterr ("Constant definition before DOCON: %s",s);
            /* put the CF of DOCON into this word's CF */
            // instance() returns the code field. for us, thats a small integer. 
	    mkword(s,mem[instance("DOCON")]);
	    if ((token = yylex()) == NULL)	/* get the value */
		dicterr("No value following CONST <word>");
	    temp = mkval(token);

	    /* two special-case constants */
            /* XXX not sure what to do with these or how to handle this */
	    // if (strcmp(s,"FIRST") == 0) temp = INITR0;
	    // else if (strcmp(s,"LIMIT") == 0) temp = DPBASE;

	    comma(temp);
	    break;

	case VAR:
	    printf("variable ");
	    if ((token = yylex()) == NULL)	/* get the variable name */
		dicterr("No word following VAR");
	    strcpy (s,token->text);
	    printf("-> %s <-",s);
	    if (!find("DOVAR"))
		dicterr("Variable declaration before DOVAR: %s",s);
	    mkword(s,mem[instance("DOVAR")]);
	    if ((token = yylex()) == NULL)	/* get the value */
		dicterr("No value following VAR <word>");
	    comma(mkval(token));
	    break;

	case USER:
	    printf("uservar ");
	    if ((token = yylex()) == NULL)	/* get uservar name */
		dicterr("No name following USER");
	    strcpy (s,token->text);
	    printf("-> %s <-",s);
	    if (!find("DOUSE"))
		dicterr("User variable declared before DOUSE: %s",s);
	    mkword(s,mem[instance("DOUSE")]);
	    if ((token = yylex()) == NULL)	/* get the value */
		dicterr("No value following USER <word>");
	    comma(mkval(token));
	    break;

	case COLON:
	    printf("colon def'n ");
	    if ((token = yylex()) == NULL)	/* get name of word */
		dicterr("No word following : in definition");
	    strcpy (s,token->text);
	    printf("-> %s <-",s);
	    if (!find("DOCOL"))
		dicterr("Colon definition appears before DOCOL: %s",s);

            mkword (s, mem[instance("DOCOL")]); // correct - we want CF, which is a small int
	    break;

	case SEMICOLON:
	    puts("end colon def'n");
            // this pointer is the CFA of the word being compiled (the simicolon)
	    comma (ORIGIN+instance(";S")*sizeof(int)); 
	    break;

	case SEMISTAR:
	    printf("end colon w/0x40 ");
	    comma (ORIGIN+instance(";S")*sizeof(int));	/* compile cfA of ;S, not CF */
	    mem[latest] |= 0x40;	/* make the word immediate */
	    break;

	case STRING_LIT:
	    printf("string literal ");
	    strcpy(s,token->text);
	    mkstr(s);		/* mkstr compacts the string in place */
	    printf("string=(%d) \"%s\" ",strlen(s),s);

            // ok, this is ugly and kludgely. strncat() would take care of this for us...
	    comma(strlen(s)); // the (.") (defined in forth.dict) expects the int count in a word by itself, so this is correct
            // *cmem=strlen(s); cmem++; - just another way to write the above, except for maintaining dp!
            cmem=(char *)mem+dp*sizeof(int);
	    {
		char *stemp;
                int  slen;
		stemp = s;
                slen = strlen(s);
		while(slen--) {
                  *cmem=*stemp; cmem++; stemp++;
                }
	    }
            dp+=(strlen(s)+(sizeof(int)-1))/sizeof(int);
	   break;
	
	case COMMENT:
        case WHITESPACE:
            // XXX insert comments, kinda like strings, perhaps with the word: (()) before it.
            // comments shouldnt do anything, but they should be available when the code is decompiled.
	    printf("comment\n");
#if 1
	    if(token->type == COMMENT) comma(ORIGIN+instance("(COMMENT)")*sizeof(int));	/* compile cfA of (COMMENT), not CF */
	    else comma(ORIGIN+instance("(WHITESPACE)")*sizeof(int));	/* compile cfA of (WHITESPACE), not CF */
	    strcpy(s,token->text);
	    printf("string=(%d) \"%s\" ",strlen(s),s);
	    comma(strlen(s)); // the (COMMENT) prim expects the int count in a word by itself, so this is correct
            cmem=(char *)mem+dp*sizeof(int);
	    {
		char *stemp;
                int  slen;
		stemp = s;
                slen = strlen(s);
		while(slen--) {
                  *cmem=*stemp; cmem++; stemp++;
                }
	    }
            dp+=(strlen(s)+(sizeof(int)-1))/sizeof(int);
#endif
	    break;

	case LABEL:
	    printf("label: ");
	    if ((token = yylex()) == NULL)
		dicterr("No name following LABEL");
	    printf("-> %s <-", token->text);
	    define(token->text,2);	/* place in sym. table w/o compiling
					   anything into dictionary; 2 means
					   defining a label */
	    break;

	case LIT:
	    comma (ORIGIN+instance("LIT")*sizeof(int));	/* compile cfA of LIT, not CF */
		lit_flag = 1;		/* and fall through to the rest */
break;

	default:
            // includes OTHER DECIMAL OCTAL HEX ...

	    if(prev_lit) {
              switch(token->type) {
	        case DECIMAL: comma(mkdecimal(token->text)); break;
	        case HEX: comma(mkhex(token->text)); break;
	        case OCTAL: comma(mkoctal(token->text)); break;
                // XXX these C_* cases are probably broken, as we want to C, not , (allocate a byte, not a word)
	        case C_BS: comma('\b'); break;
	        case C_FF: comma('\f'); break;
	        case C_NL: comma('\n'); break;
	        case C_CR: comma('\r'); break;
	        case C_TAB: comma('\t'); break;
	        case C_BSLASH: comma(0x5c); break;  /* ASCII backslash */
	        case C_LIT: comma(*((token->text)+1)); break;
                default: printf("LIT: not really a litteral value: %s\n", token->text);
                         goto not_really_lit;
              }
            } 
            if(prev_lit) break;

            not_really_lit: // an example of when this happends: COMPILE LIT <randomotherword> 
            if (find(token->text) != NULL) {	/* is word defined? */

                if(findtype(token->text)==2) {
		  printf("  label: %s\n",token->text);
	    	  comma(instance(token->text)); 
                } else {
		  printf("  normal: %s\n",token->text);
	    	  comma(ORIGIN+instance(token->text)*sizeof(int)); 
                }
		break;

	    } else {

		printf("forward reference: %s\n", token->text);
		comma (ORIGIN+instance (token->text)*sizeof(int));		/* create an instance, to be resolved at definition  XXX i expect that this will always be 0 */
                break;
	    }
	}
	if (lit_flag) puts("expect a literal");
	prev_lit = lit_flag;	/* to be used by chklit() next time */
	lit_flag = 0;
    }
}

comma(i) {
    /* put at mem[dp]; increment dp */
    mem[dp++] = (int)i;
}

/*
 * make a word in the dictionary.  the new word will have name *s, its CF
 * will contain v. Also, resolve any previously-unresolved references by
 * calling define()
 */

mkword(s, v)
char *s;      // name of the word
int v;        // code field to use for it
{
	int here, count = 0;
	char *olds;
        int len;
	olds = s;		/* preserve this pointer for resolving references */

	printf("%s ",s);

	here = dp;		/* hold this value to place length byte */

        cmem = (char *)(mem)+dp*sizeof(int);

        if((void *)cmem != (void *)&mem[dp]) {
          sprintf((void *)stderr, "something with your platform foiled my code - im really sorry, i tried =(\n");
        }

	*cmem = (len = strlen(s) & 0x0f) | 0x80; cmem++;
	while (len--) {		/* for each character */
          *cmem=*s;
          cmem++;
	  s++;
	}

	dp+=16/sizeof(int);         // skip 16 bytes, or 4 words - skip over the name field onto the link field 

	if(latest) mem[dp++] = ORIGIN+latest*sizeof(int);  // the link field  - latest is the last word defined or 0. this is correct.
        else mem[dp++] = 0;

	latest = here;		    /* update the link */

	mem[dp] = v;		    /* code field; leave dp pointing to CFA */

	define(olds,1);		    /* place in symbol table. 1 == "not a label" (define() uses dp) */

        dp++;                       // after define() sees dp, we can and should inc it

	/* that's all. Now dp points (once again) to the first UNallocated
           spot in mem, and everybody's happy. */
}

mkrest() {
  int temp;

  /* leave IP, DP in a state that the VM will run and boot */
  if ((mem[dp] = ORIGIN+instance("COLD")*sizeof(int)) == 0)
    dicterr("COLD must be defined to take control at startup");
  /* address of CF of magic take-control word */
  // speicicifcly, outside of any word, is just the code-field-address of the 'COLD' word, which is DOCOL.
  // we nest from a nonexistant level into our 1st level (#0).
  mem[0] = ORIGIN+dp*sizeof(int);  // instruction pointer... point us at "COLD", which should take control on 1st boot
  dp++;
  mem[3] = ORIGIN+dp*sizeof(int);  // data pointer... 
  mem[40] = ORIGIN+latest*sizeof(int); // latest. 

  /* other initializations... */
  mem[7] = ORIGIN+latest*sizeof(int);  // wordsize
}

writedict() {
  /* write memory to COREFILE and map to MAPFILE */
  int     i, temp, tempb, firstzero, nonzero;
  char    chars[9], outline[80], tstr[6];

  printf ("Writing forth.mem... dp=%d\n", dp*sizeof(int));

  if ((outf = fopen ("forth.mem", "w")) == NULL) {
    printf ("nf: can't open forth.mem for output.\n");
    exit (2);
  }

  printf("Wrote %d bytes to forth.mem\n",
    fwrite (mem, 4, dp, outf)
  );

  if (fclose (outf) == EOF) {
    fprintf (stderr, "Error closing forth.mem\n");
    exit (3);
  }
  puts("Success!\n");
  exit(0);
}

mkval(t)			/* convert t->text to integer based on type */
TOKEN *t;
{
	char *s = t->text;
	int sign = 1;

	if (*s == '-') {
		sign = -1;
		s++;
	}

	switch (t->type) {
	case DECIMAL:
		return (sign * mkdecimal(s));
	case HEX:
		return (sign * mkhex(s));
	case OCTAL:
		return (sign * mkoctal(s));
	default:
		dicterr("Bad value following PRIM, CONST, VAR, or USER");
	}
}

mkhex(s)
char *s;
{				/*  convert hex ascii to integer */
    int     temp;
    temp = 0;

    s += 2;			/* skip over '0x' */
    while (isxdigit (*s)) {	/* first non-hex char ends */
	temp <<= 4;		/* mul by 16 */
	if (isupper (*s))
	    temp += (*s - 'A') + 10;
	else
	    if (islower (*s))
		temp += (*s - 'a') + 10;
	    else
		temp += (*s - '0');
	s++;
    }
    return temp;
}

mkoctal(s)
char *s;
{				/*  convert Octal ascii to integer */
    int     temp;
    temp = 0;

    while (isoctal (*s)) {	/* first non-octal char ends */
	temp = temp * 8 + (*s - '0');
	s++;
    }
    return temp;
}

mkdecimal(s)			/* convert ascii to decimal */
char *s;
{
	return (atoi(s));	/* alias */
}

dicterr(s,p1)
char *s;
int p1;		/* might be char * -- printf uses it */
{
    fprintf(stderr,s,p1);
    fprintf(stderr,"\nError at line %d of the forth.dict file.\n", yylineno);
    if(latest) {
      fprintf(stderr,"Last word defined was "); printword(latest);
    }
/*    fprintf(stderr, "; last word read was \"%s\"", token->text); */
    fprintf(stderr,"\n");
    exit(1);
}

dictwarn(s)		/* almost like dicterr, but don't exit */
char *s;
{
    fprintf(stderr,"\nWarning: %s\nLast word read was ",s);
    printword(latest);
    putc('\n',stderr);
}
    
printword(n)
int n;
{
    int count, tmp;
    cmem = (char *)(mem)+n*sizeof(int);
    count=*cmem & 0x0f; cmem++;
    while(count--) {
      putc(*cmem, stderr); cmem++;
    }
}


mkstr(s)
char *s;
{
    /* modifies a string in place with escapes compacted. Strips leading & trailing \" */
    char *source;
    char *dest;

    source = dest = s;
    source++;			/* skip leading quote */
    while (*source != '"') {	/* string ends with unescaped \" */
	if (*source == '\\') {	/* literal next */
	    source++;
	}
	*dest++ = *source++;
    }
    *dest = '\0';
}

failassert(s)
char *s;
{
    puts(s);
    exit(1);
}

checkdict()			/* check for unresolved references */
{
    CHAIN *ch = &firstchain;

    while (ch != NULL) {
	if ((ch->firstlink) != NULL) {
	    fprintf(stderr,"Unresolved forward reference: %s\n",ch->chaintext);
	}
	ch = ch->nextchain;
    }
}

    
/********* structure-handling functions find(s), define(s,t), instance(s) **/

CHAIN *
find(s)		/* returns a pointer to the chain named s */
char *s;
{
	CHAIN *ch;
	ch = &firstchain;
	while (ch != NULL) {
		if (strcmp (s, ch->chaintext) == 0) return ch;
		else ch = ch->nextchain;
	}
	return NULL;	/* not found */
}

CHAIN *
findtype(s)		/* returns a pointer to the chain named s */
char *s;
{
	CHAIN *ch;
	ch = &firstchain;
	while (ch != NULL) {
		if (strcmp (s, ch->chaintext) == 0) return ch->chaintype;
		else ch = ch->nextchain;
	}
	return -1;	/* not found */
}

/* define must create a symbol table entry if none exists, with type t.
   if one does exist, it must have type 0 -- it is an error to redefine
   something at this stage. Change to type t, and fill in the outstanding
   instances, with the current dp if type=1, or relative if type=2. */
/* type 1 is for "non labels", like calls to other words. type 1 is absolute address */
/* type 2 is for labels. were interested in the offset to get to where we were initially defined (as type 0) */

define(s, t) 
  char *s;
  int t;  {
  CHAIN *ch;
  LINK *ln, *templn;
  
  printf("define(%s,%d)\n",s,t);
  if(t<1 || t>2) dicterr("Program error: type in define() not 1 or 2.");
  if((ch=find(s)) !=NULL) {  /* defined or instanced? */
    if(ch->chaintype != 0)   /* already defined! */
      dicterr("Word already defined!\n");
    else {
      printf("there are forwards refs: ");
      ch->chaintype=t;
      ch->defloc=dp;
    }
  } else {
    printf("no forwards refs");
    ch = ((lastchain()->nextchain) = newchain()); /* create a new, blank chain */
    /* create a new chain, link it in, leave ch pointing to it */
    strcpy(ch->chaintext, s);
    ch->chaintype = t;
    ch->defloc = dp;        /* fill in data for use of future references */
  }
  /* now ch points to the chain (possibly) containing forward refs */
  if((ln = ch->firstlink)==NULL) return;      /* no links.. */
  switch(ch->chaintype) {
    case 1: printf("chain type: absolute\n"); break;
    case 2: printf("chain type: relative\n"); break;
  }
  while(ln!=NULL) {
    printf("      Forward ref at 0x%x\n", ln->loc);
    // here, we finally know the address of the label/word, and were going back and filling in references to us
    switch(ch->chaintype) {
      case 1: mem[ln->loc] = ORIGIN+dp*sizeof(int);   /* absolute */
              break;
      case 2:  mem[ln->loc] = dp - ln->loc; // relative - diff between current position and labels location
               break;
      default: dicterr("Bad type field in define()");
    }
    /* "now skip to the next link and free this one" */
    templn = ln;
    ln=ln->nextlink;
    free(templn);
  }
  /* clean up that last pointer.. */
  ch->firstlink = NULL;
}
    
/*
   instance must return a value to be compiled into the dictionary at
   dp, consistent with the symbol 's': if s is undefined, it returns 0,
   and adds this dp to the chain for s (creating that chain if necessary).
   If s IS defined, it returns <s> (absolute) or (s-dp) (relative), where
   <s> was the dp when s was defined.
 */
instance(s)
char *s;
{
	CHAIN *ch;
	LINK *ln;

	printf("instance(%s):\n",s);
	if ((ch = find(s)) == NULL) {	/* not defined yet at all */
		puts("entirely new -- create a new chain");
		/* create a new chain, link it in, leave ch pointing to it */
		ch = ((lastchain() -> nextchain) = newchain());
		strcpy(ch->chaintext, s);
		ln = newlink();		/* make its link */
		ch->firstlink = ln;
		ln->loc = dp;		/* store this location there */
		return 0;		/* all done */
	}
	else {
		switch(ch->chaintype) {
		case 0:			/* not defined yet */
			puts("still undefined -- add a link");
			/* create a new link, point the last link to it, and
			   fill in the loc field with the current dp */
			(lastlink(ch)->nextlink = newlink()) -> loc = dp;
			return 0;
		case 1:			/* absolute */
			puts("defined absolute.");
			return ch->defloc;
			// return ch->defloc; // whomever we return this to optionally multiplies *sizeof(int) and adds ORIGIN --- XXX, highly experimental -- was botching relative backwards references, which happends when a backwards branch to a label (type 2 word) is encountered by always doing the ORIGIN+sizeof(int)* thing
                        // return ORIGIN+ch->defloc*sizeof(int); // this botches mkword(x,instance()) calls
		case 2:			/* relative */
			puts("defined relative.");
			// printf("debug: label location: %lx  current location: %lx  difference: %d %lx\n", ch->defloc, dp, ch->defloc - dp, ch->defloc - dp); 
			return ch->defloc - dp; 
		default:
			dicterr("Program error: bad type for chain");
		}
	}
}

CHAIN *lastchain() {
  CHAIN *ch=&firstchain;
  while(ch->nextchain != NULL) ch=ch->nextchain;
  return ch;
}

LINK *lastlink(ch)      /* return the last link in the chain */
CHAIN *ch;              /* CHAIN MUST HAVE AT LEAST ONE LINK */
{
        LINK *ln = ch->firstlink;
    
        while (ln->nextlink != NULL) ln = ln->nextlink;
        return ln;
}
