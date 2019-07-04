// pl/0 compiler with code generation
#include <stdlib.h>
#include <string.h>
#include "pl0.h"

void error(long n){
    long i;

    printf(" ****");
    for (i=1; i<=cc-1; i++){
	printf(" ");
    }
    printf("^%2d\n",n);
    err++;
}

void getch() {//滤掉空格，读取一个字符
    if(cc==ll){
	if(feof(infile)){
	    printf("************************************\n");
	    printf("      program incomplete\n");
	    printf("************************************\n");
	    exit(1);
	}
	ll=0; cc=0;
	printf("%5d ", cx);
	while((!feof(infile))&&((ch=getc(infile))!='\n')){
	    printf("%c",ch);
	    ll=ll+1; line[ll]=ch;
	}
	printf("\n");
	ll=ll+1; line[ll]=' ';
    }
    cc=cc+1; ch=line[cc];
}

void getsym(){//词法分析出一个词法单元
    long i,j,k;

    while(ch==' '||ch=='\t'){
	getch();
    }
    if(isalpha(ch)){ 	// identified or reserved
	k=0;
	do{
	    if(k<al){
		a[k]=ch; k=k+1;
	    }
	    getch();
	}while(isalpha(ch)||isdigit(ch));
	if(k>=kk){
	    kk=k;
	}else{
	    do{
		kk=kk-1; a[kk]=' ';
	    }while(k<kk);
	}
	strcpy(id,a); i=0; j=norw-1;
	do{
	    k=(i+j)/2;
	    if(strcmp(id,word[k])<=0){
		j=k-1;
	    }
	    if(strcmp(id,word[k])>=0){
		i=k+1;
	    }
	}while(i<=j);
	if(i-1>j){
	    sym=wsym[k];
	}else{
	    sym=ident;
	}
    }else if(isdigit(ch)){ // number
	k=0; num=0; sym=number;
	do{
	    num=num*10+(ch-'0');
	    k=k+1; getch();
	}while(isdigit(ch));
	if(k>nmax){
	    error(31);
	}
    }else if(ch==':'){
	getch();
	if(ch=='='){
	    sym=becomes; getch();
	}else{
	    sym=nul;
	}
    }else if(ch=='<'){
	getch();
	if(ch=='='){
	    sym=leq; getch();
	}else if(ch=='>'){
	    sym=neq; getch();
	}else{
	    sym=lss;
	}
    }else if(ch=='>'){
	getch();
	if(ch=='='){
	    sym=geq; getch();
	}else{
	    sym=gtr;
	}
    }
	else if(ch =='/')//添加注释 /*...*/ and //...
	{
		getch();
		if(ch =='*')
		{
			getch();
			while(1)
			{
				while(ch != '*')
					getch();
				getch();
				if(ch =='/')
				{break;}
			}
			getch();
			getsym();
		}
		else if(ch == '/')
		{
			cc = ll;
			ch = ' ';
			getsym();
		}else
		{
			sym = '/';
		}
	}
	else{
	sym=ssym[(unsigned char)ch]; getch();
    }
}

void gen(enum fct x, long y, long z){//将指令索引加入code数组
    if(cx>cxmax){
	printf("program too long\n");
	exit(1);
    }
    code[cx].f=x; code[cx].l=y; code[cx].a=z;
    cx=cx+1;
}

void test(unsigned long s1, unsigned long s2, long n){
    if (!(sym & s1)){
	error(n);
	s1=s1|s2;
	while(!(sym & s1)){
	    getsym();
	}
    }
}

void enter(enum object k){//判断标识符的类型，enter object into table
    tx=tx+1;
    strcpy(table[tx].name,id);
    table[tx].kind=k;
    switch(k){
	case constant:
	    if(num>amax){
		error(31);
		num = 0;
	    }
	    table[tx].val=num;
	    break;
	case variable:
	    table[tx].level=lev; table[tx].addr=dx; dx=dx+1;
	    break;
	case proc:
	    table[tx].level=lev;
	    break;
    }
}

long position(char* id){	// find identifier id in table
    long i;

    strcpy(table[0].name,id);
    i=tx;
    while(strcmp(table[i].name,id)!=0){
	i=i-1;
    }
    return i;
}

void constdeclaration(){//识别常量
    if(sym==ident){
	getsym();
	if(sym==eql||sym==becomes){
	    if(sym==becomes){
		error(1);
	    }
	    getsym();
	    if(sym==number){
		enter(constant); getsym();
	    }else{
		error(2);
	    }
	}else{
	    error(3);
	}
    }else{
	error(4);
    }
}

void vardeclaration(){//识别变量
    if(sym==ident){
	enter(variable); getsym();
    }else{
	error(4);
    }
}

void listcode(long cx0){	// list code generated for this block
    long i;

    for(i=cx0; i<=cx-1; i++){
	printf("%10d%5s%3d%5d\n", i, mnemonic[code[i].f], code[i].l, code[i].a);
    }

}

void expression(unsigned long);
void factor(unsigned long fsys){//因子语法分析
    long i;

    test(facbegsys,fsys,24);//类型测试
    while(sym & facbegsys){//字符类型判断
	if(sym==ident){//标识符
	    i=position(id);
	    if(i==0){
		error(11);
	    }else{
		switch(table[i].kind){
		    case constant://常量
			gen(lit,0,table[i].val);
			break;
		    case variable://变量
			gen(lod,lev-table[i].level,table[i].addr);
			break;
		    case proc://保留字
			error(21);
			break;
		}
	    }
	    getsym();
	}else if(sym==number){//数字
	    if(num>amax){
		error(31); num=0;
	    }
	    gen(lit,0,num);
	    getsym();
	}else if(sym==lparen){//左括号
	    getsym();
	    expression(rparen|fsys);
	    if(sym==rparen){
		getsym();
	    }else{
		error(22);
	    }
	}
	test(fsys,lparen,23);
    }
}

void term(unsigned long fsys){//乘除语法分析
    unsigned long mulop;

    factor(fsys|times|slash);
    while(sym==times||sym==slash){
	mulop=sym; getsym();
	factor(fsys|times|slash);
	if(mulop==times){
	    gen(opr,0,4);
	}else{
	    gen(opr,0,5);
	}
    }
}

void expression(unsigned long fsys){//表达式语法分析
    unsigned long addop;

    if(sym==plus||sym==minus){
	addop=sym; getsym();
	term(fsys|plus|minus);
	if(addop==minus){
	    gen(opr,0,1);
	}
    }else{
	term(fsys|plus|minus);
    }
    while(sym==plus||sym==minus){
	addop=sym; getsym();
	term(fsys|plus|minus);
	if(addop==plus){
	    gen(opr,0,2);
	}else{
	    gen(opr,0,3);
	}
    }
}

void condition(unsigned long fsys){//条件语法分析
    unsigned long relop;

    if(sym==oddsym){
	getsym(); expression(fsys);
	gen(opr,0,6);
    }else{
	expression(fsys|eql|neq|lss|gtr|leq|geq);
	if(!(sym&(eql|neq|lss|gtr|leq|geq))){
	    error(20);
	}else{
	    relop=sym; getsym();
	    expression(fsys);
	    switch(relop){
		case eql:
		    gen(opr,0,8);
		    break;
		case neq:
		    gen(opr,0,9);
		    break;
		case lss:
		    gen(opr,0,10);
		    break;
		case geq:
		    gen(opr,0,11);
		    break;
		case gtr:
		    gen(opr,0,12);
		    break;
		case leq:
		    gen(opr,0,13);
		    break;
	    }
	}
    }
}

void statement(unsigned long fsys){//语句分析
    long i,cx1,cx2,cx3;

    if(sym==ident){//标识符
	i=position(id);
	if(i==0){
	    error(11);
	}else if(table[i].kind!=variable){	// assignment to non-variable
	    error(12); i=0;
	}
	getsym();
	if(sym==becomes){//判断是否为赋值号
	    getsym();
	}else{
	    error(13);
	}
	expression(fsys);//判断表达式
	if(i!=0){
	    gen(sto,lev-table[i].level,table[i].addr);
	}
    }else if(sym==callsym){//call
	getsym();
	if(sym!=ident){
	    error(14);
	}else{
	    i=position(id);
	    if(i==0){
		error(11);
	    }else if(table[i].kind==proc){//判断是否为程序名
		gen(cal,lev-table[i].level,table[i].addr);
	    }else{
		error(15);
	    }
	    getsym();
	}
    }else if(sym==ifsym){     //if()then{} else{}
	getsym();
	condition(fsys|thensym|dosym);//判断是否符合条件语法
	if(sym==thensym){
	    getsym();
	}else{
	    error(16);
	}
	cx1=cx;//保存当前指令地址
	gen(jpc,0,0);//生成条件跳转指令
	statement(fsys);//处理then后的语句
	cx2=cx;
	gen(jmp, 0, 0); //将来会直接跳转到else语句后面
	code[cx1].a=cx;
	if(sym == elsesym)
	{
		getsym();
		statement(fsys);
		code[cx2].a=cx;//当前是else后面的语句结束位置
	}
    }
	else if(sym==beginsym){// begin-end
	getsym();
	statement(fsys|semicolon|endsym);
	while(sym==semicolon||(sym&statbegsys)){
	    if(sym==semicolon){
		getsym();
	    }else{
		error(10);
	    }
	    statement(fsys|semicolon|endsym);
	}
	if(sym==endsym){
	    getsym();
	}else{
	    error(17);
	}
    }else if(sym==whilesym){
    circleNO++;
	cx1=cx; getsym();
	condition(fsys|dosym);
	cx2=cx;	gen(jpc,0,0);
	if(sym==dosym){
	    getsym();
	}else{
	    error(18);
	}
	statement(fsys); gen(jmp,0,cx1);
	code[cx2].a=cx;
	if(pos_exit!=0){
        code[pos_exit].a=cx;
        pos_exit=0;
	}
	circleNO--;
    }
    else if(sym==exitsym){
        if(circleNO==0)
            error(26);
        else{
            pos_exit=cx;
            gen(jmp,0,0);
        }
        getsym();
    }else if(sym==readsym){//输入
        getsym();
        if(sym!=lparen)
            error(26);
        do{
            getsym();
            if(!(i=position(id))) error(11);
            else{
                gen(read,lev-table[i].level,table[i].addr);//生成读取变量的read指令
            }
            getsym();//
        }while(sym==comma);//如果变量之间仍有逗号则继续读取
        if(sym==rparen)
            getsym();
        else
            error(22);
    }else if(sym==writesym){//输出
        getsym();
        if(sym!=lparen)
            error(26);
        do{
            getsym();
            if(sym==rparen){
                gen(opr,0,14);
                break;
            }
            else{
                if(!(i=position(id))) error(11);
                if(table[i].kind==constant){
                   gen(lit,0,table[i].val);
                   gen(opr,0,15);//打印常量或数字
                }
                else if(i){
                    gen(write,lev-table[i].level,table[i].addr);//生成打印变量的write指令
                }
            }
            getsym();//
        }while(sym==comma);//如果变量之间仍有逗号则继续读取
        if(sym==rparen)
            getsym();
        else
            error(22);
    }
    test(fsys,0,19);
}

void block(unsigned long fsys){//分析程序处理过程（开始符）
    long tx0;		// initial table index
    long cx0; 		// initial code index
    long tx1;		// save current table index before processing nested procedures
    long dx1;		// save data allocation index

    dx=3; tx0=tx; table[tx].addr=cx; gen(jmp,0,0);
    if(lev>levmax){
	error(32);
    }
    do{
	if(sym==constsym){
	    getsym();
	    do{
		constdeclaration();
		while(sym==comma){
		    getsym(); constdeclaration();
		}
		if(sym==semicolon){
		    getsym();
		}else{
		    error(5);
		}
	    }while(sym==ident);
	}
	if(sym==varsym){
	    getsym();
	    do{
		vardeclaration();
		while(sym==comma){
		    getsym(); vardeclaration();
		}
		if(sym==semicolon) {
		    getsym();
		}else{
		    error(5);
		}
	    }while(sym==ident);
	}
	while(sym==procsym){
	    getsym();
	    if(sym==ident){
		enter(proc); getsym();
	    }else{
		error(4);
	    }
	    if(sym==semicolon){
		getsym();
	    }else{
		error(5);
	    }
	    lev=lev+1; tx1=tx; dx1=dx;
	    block(fsys|semicolon);
	    lev=lev-1; tx=tx1; dx=dx1;
	    if(sym==semicolon){
		getsym();
		test(statbegsys|ident|procsym,fsys,6);
	    }else{
		error(5);
	    }
	}
	test(statbegsys|ident,declbegsys,7);
    }while(sym&declbegsys);
    code[table[tx0].addr].a=cx;
    table[tx0].addr=cx;		// start addr of code
    cx0=cx; gen(Int,0,dx);
    statement(fsys|semicolon|endsym);
    gen(opr,0,0); // return
    test(fsys,0,8);
    listcode(cx0);
}

long base(long b, long l){
    long b1;

    b1=b;
    while (l>0){	// find base l levels down
	b1=s[b1]; l=l-1;
    }
    return b1;
}

void interpret(){//解释执行
    long p,b,t;		// program-, base-, topstack-registers
    instruction i;	// instruction register
    int input;

    printf("start PL/0\n");
    t=0; b=1; p=0;
    s[1]=0; s[2]=0; s[3]=0;
    do{
	i=code[p]; p=p+1;
	switch(i.f){//code[program].function
	    case lit:
		t=t+1;
		s[t]=i.a;//code[program].address
		break;
	    case opr:
		switch(i.a){ 	// operator
		    case 0:	// return
			t=b-1; p=s[t+3]; b=s[t+2];
			break;
		    case 1:
			s[t]=-s[t];
			break;
		    case 2:
			t=t-1; s[t]=s[t]+s[t+1];
			break;
		    case 3:
			t=t-1; s[t]=s[t]-s[t+1];
			break;
		    case 4:
			t=t-1; s[t]=s[t]*s[t+1];
			break;
		    case 5:
			t=t-1; s[t]=s[t]/s[t+1];
			break;
		    case 6:
			s[t]=s[t]%2;
			break;
		    case 8:
			t=t-1; s[t]=(s[t]==s[t+1]);
			break;
		    case 9:
			t=t-1; s[t]=(s[t]!=s[t+1]);
			break;
		    case 10:
			t=t-1; s[t]=(s[t]<s[t+1]);
			break;
		    case 11:
			t=t-1; s[t]=(s[t]>=s[t+1]);
			break;
		    case 12:
			t=t-1; s[t]=(s[t]>s[t+1]);
			break;
		    case 13:
			t=t-1; s[t]=(s[t]<=s[t+1]);
			break;
			case 14://打印回车
                printf("\n");
                break;
            case 15://打印常量
                printf("%d\t",s[t]);
                t=t-1;
		}
		break;
	    case lod:
		t=t+1; s[t]=s[base(b,i.l)+i.a];
		break;
	    case sto:
		s[base(b,i.l)+i.a]=s[t]; printf("%10d\n", s[t]); t=t-1;
		break;
	    case cal:		// generate new block mark
		s[t+1]=base(b,i.l); s[t+2]=b; s[t+3]=p;
		b=t+1; p=i.a;
		break;
	    case Int:
		t=t+i.a;
		break;
	    case jmp:
		p=i.a;
		break;
	    case jpc:
		if(s[t]==0){
		    p=i.a;
		}
		t=t-1;
		break;
		//case jpnc:
        case read:      //读取变量
            scanf("%d",&input);
            s[base(b,i.l)+i.a]=input;
            break;
        case write:     //打印变量
            printf("%d\t",s[base(b,i.l)+i.a]);
            //break;


	}
    }while(p!=0);
    printf("end PL/0\n");
}

main(){
    long i;
    for(i=0; i<256; i++){
	ssym[i]=nul;
    }
    strcpy(word[0],  "begin     ");
    strcpy(word[1],  "call      ");
    strcpy(word[2],  "const     ");
    strcpy(word[3],  "do        ");
	strcpy(word[4],  "else      ");
    strcpy(word[5],  "end       ");
    strcpy(word[6],  "exit      ");
    strcpy(word[7],  "if        ");
    strcpy(word[8],  "odd       ");
    strcpy(word[9],  "procedure ");
    strcpy(word[10], "read      ");
    strcpy(word[11], "then      ");
    strcpy(word[12], "var       ");
    strcpy(word[13], "while     ");
    strcpy(word[14], "write     ");
    //保留字
    wsym[0]=beginsym;
    wsym[1]=callsym;
    wsym[2]=constsym;
    wsym[3]=dosym;
	wsym[4]=elsesym;
    wsym[5]=endsym;
    wsym[6]=exitsym;
    wsym[7]=ifsym;
    wsym[8]=oddsym;
    wsym[9]=procsym;
    wsym[10]=readsym;
    wsym[11]=thensym;
    wsym[12]=varsym;
    wsym[13]=whilesym;
    wsym[14]=writesym;


    ssym['+']=plus;
    ssym['-']=minus;
    ssym['*']=times;
    ssym['/']=slash;
    ssym['(']=lparen;
    ssym[')']=rparen;
    ssym['=']=eql;
    ssym[',']=comma;
    ssym['.']=period;
    ssym[';']=semicolon;
    ssym['&']=doAnd;//与运算
    ssym['|']=doOr;//或运算
    ssym['~']=doNot;//非运算
    ssym['%']=Mod;//取模运算

    //指令类型
    strcpy(mnemonic[lit],"lit");
    strcpy(mnemonic[opr],"opr");
    strcpy(mnemonic[lod],"lod");
    strcpy(mnemonic[sto],"sto");
    strcpy(mnemonic[cal],"cal");
    strcpy(mnemonic[Int],"int");
    strcpy(mnemonic[jmp],"jmp");
    strcpy(mnemonic[jpc],"jpc");
    //strcpy(mnemonic[jpnc],"jpnc");
    strcpy(mnemonic[read],"read");//读取变量值
    strcpy(mnemonic[write],"write");//打印变量值

    declbegsys=constsym|varsym|procsym;
    statbegsys=beginsym|callsym|elsesym|exitsym|ifsym|readsym|whilesym|writesym;
    facbegsys=ident|number|lparen;

    printf("please input source program file name: ");
    scanf("%s",infilename);
    printf("\n");
    if((infile=fopen(infilename,"r"))==NULL){
	printf("File %s can't be opened.\n", infilename);
	exit(1);
    }

    err=0;
    cc=0; cx=0; ll=0; ch=' '; kk=al; getsym();
    lev=0; tx=0;
    block(declbegsys|statbegsys|period);
    if(sym!=period){
	error(9);
    }
    if(err==0){
	interpret();
    }else{
	printf("errors in PL/0 program\n");
    }
    fclose(infile);
}
