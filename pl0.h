#include <stdio.h>

#define norw       21             // total number of reserved words
#define txmax      100            // length of identifier table
#define nmax       14             // max. no. of digits in numbers
#define al         10             // length of identifiers
#define amax       2047           // maximum address
#define levmax     3              // maximum depth of block nesting
#define cxmax      2000           // size of code array

#define nul	   0x1
#define ident      0x2
#define number     0x4
#define plus       0x8
#define minus      0x10
#define times      0x20
#define slash      0x40
#define oddsym     0x80
#define eql        0x100
#define neq        0x200
#define lss        0x400
#define leq        0x800
#define gtr        0x1000
#define geq        0x2000
#define lparen     0x4000
#define rparen     0x8000
#define comma      0x10000
#define semicolon  0x20000
#define period     0x40000
#define becomes    0x80000
#define doAnd      0x80001
#define doOr       0x80002
#define doNot      0x80004
#define Mod        0x80008

#define beginsym   0x100000
#define endsym     0x200000
#define ifsym      0x400000
#define thensym    0x800000
#define whilesym   0x1000000
#define dosym      0x2000000
#define callsym    0x4000000
#define constsym   0x8000000
#define varsym     0x10000000
#define procsym    0x20000000
#define elsesym    0x40000000
#define readsym    0x40000001
#define writesym   0x40000002
#define exitsym    0x40000008

#define truesym    0x40000010
#define falsesym   0x40000020
#define booleansym 0x40000040

#define typesym    0x40000100
#define realsym    0x40000200
#define intersym   0x40000400

#define typeerror  0x40001000
#define voiderror  0x40002000
enum object {
    constant, variable, proc,type
};

enum fct {
    lit, opr, lod, sto, cal, Int, jmp, jpc, read, write        // functions
};

//typedef enum {
//    false,
//    true
//} boolean;

typedef struct{
    enum fct f;		// function code
    long l; 		// level
    long a; 		// displacement address
} instruction;
/*  lit 0, a : load constant a
    opr 0, a : execute operation a
    lod l, a : load variable l, a
    sto l, a : store variable l, a
    cal l, a : call procedure a at level l
    Int 0, a : increment t-register by a
    jmp 0, a : jump to a
    jpc 0, a : jump conditional to a       */

char ch;               // last character read
unsigned long sym;     // last symbol read
unsigned long lastsym;  //最后得到的词法类型

char id[al+1];         // last identifier read
long num;              // last number read
long cc;               // character count
long ll;               // line length
long kk, err;
long cx;               // code allocation index
long circleNO;  //count circle number
long pos_exit;
char line[81];
char a[al+1];
instruction code[cxmax+1];
char word[norw][al+1];
unsigned long wsym[norw];
unsigned long ssym[256];

char mnemonic[8][3+1];
unsigned long declbegsys, statbegsys, facbegsys;

struct{
    char name[al+1];
    enum object kind;
    long val;
    long level;
    long addr;
    __int64 type1;//ÀàÐÍÏî
}table[txmax+1];

char infilename[80];
FILE* infile;

// the following variables for block
long dx;		// data allocation index
long lev;		// current depth of block nesting
long tx;		// current table index

// the following array space for interpreter
#define stacksize 50000
long s[stacksize];	// datastore
