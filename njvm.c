#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <bigint.h>
#include "procedures.h"

StackSlot *stack;
ObjRef *sda;
ObjRef rg;
int stackPointer = 0;
int framepointer = 0;
int version = 8;
int debug_pc;
bool isDebug = false;


ObjRef newPrimObject(int dataSize) {
  ObjRef objRef;

  objRef = malloc(sizeof(unsigned int) +
                  dataSize * sizeof(unsigned char));
  if (objRef == NULL) {
    fatalError("newPrimObject() got no memory");
  }
  objRef->size = dataSize;
  return objRef;
}

void fatalError(char *msg) {
  printf("Fatal error: %s\n", msg);
  exit(1);
}


ObjRef newCompoundObject(int n){
	ObjRef superObj = malloc(sizeof(unsigned int) + (n * sizeof(ObjRef)) );
	superObj->size = n | MSB;
	n--;
	while(n != -1){
		GET_REFS(superObj)[n] = NULL;
		n--;
	}
	return superObj;
}


void pushObjRef(ObjRef objRef){
	stack[stackPointer].isObjRef = TRUE;
	stack[stackPointer].u.objRef = objRef;
	stackPointer++;	
}

ObjRef popObjRef(void){
	stackPointer = stackPointer - 1;
	return stack[stackPointer].u.objRef;
}

void push(int immediateValue){
	stack[stackPointer].isObjRef = FALSE;
	stack[stackPointer].u.number = immediateValue;
	stackPointer++;
}

int pop(void){
	stackPointer = stackPointer - 1;
	return stack[stackPointer].u.number;
}

void pushSDA(int adresse){
	sda[adresse] = popObjRef();
}

void runCode(unsigned int *instuctions){
	int pc = 0;
	bool stop = false;
	char readChar;
	int index;
	ObjRef ref;
	ObjRef ref2;
	if(isDebug) pc = debug_pc;
	while(!stop){
		unsigned int ir = instuctions[pc] >> 24;
		int immediateVal = SIGN_EXTEND(IMMEDIATE(instuctions[pc]));
		pc++;			
		switch(ir){			
			case HALT:
				stop = true;
				break;
			case PUSHC:
				bigFromInt(immediateVal); 
				pushObjRef(bip.res);
				break;			
			case PUSHG:
				pushObjRef(sda[immediateVal]);
				break;
			case POPG:
				pushSDA(immediateVal);
				break;
			case POPL:
				/*stack[framepointer + immediateVal].isObjRef = TRUE;*/
				stack[framepointer + immediateVal].u.objRef = popObjRef();
				/*stack[framepointer + immediateVal].u.number = pop();*/
				break;
			case PUSHL:
				pushObjRef(stack[framepointer + immediateVal].u.objRef);
				break;
			case WRINT:
				bip.op1 = popObjRef();				
				bigPrint(stdout);
				break;
			case WRCHR: 
				bip.op1 = popObjRef(); 
				printf("%c", bigToInt());
				break;
			case RDINT:
				bigRead(stdin);
				pushObjRef(bip.res);
				break;			
			case RDCHR:
				scanf("%c", &readChar);bigFromInt((int)readChar); 
				pushObjRef(bip.res);
				break;
			case ASF:
				push(framepointer);
				framepointer = stackPointer;
				stackPointer = stackPointer + immediateVal;
				break;
			case RSF:
				stackPointer = framepointer;
				framepointer = pop();
				break;
			case JMP:
				pc = immediateVal;
				break;
			case CALL:
				push(pc);
				pc = immediateVal;
				break;
			case BRF:
				bip.op1 = popObjRef(); 
				if(bigToInt() != TRUE) pc = immediateVal;
				break;
			case BRT:
				bip.op1 = popObjRef(); 
				if(bigToInt() != FALSE) pc = immediateVal; 
				break;
			case RET:
				/*pc = stack[stackPointer-1].u.number;*/
				pc = pop();
				break;
			case DROP:
				while(immediateVal != 0){
					pop();
					immediateVal--;
				}
				break;
			case PUSHR:
				pushObjRef(rg);
				break;
			case POPR:
				rg = popObjRef();
				break;
			case DUP: 
				ref = popObjRef();
				pushObjRef(ref);
				pushObjRef(ref);
				break;
			case NEW :  
				pushObjRef(newCompoundObject( immediateVal));
				break;
			case GETF :
				ref = popObjRef();
				pushObjRef(GET_REFS(ref)[immediateVal]);
				break;
			case PUTF :
				ref = popObjRef();
				ref2 = popObjRef();
				GET_REFS(ref2)[immediateVal] = ref;
				break;
			case NEWA :
				bip.op1 = popObjRef();
				ref = newCompoundObject(bigToInt());
				pushObjRef(ref);
				break;
			case GETFA :
				bip.op1 = popObjRef();
				ref = popObjRef();
				index = bigToInt();

				if (index < 0 || index > GET_SIZE(ref)) {
					printf("Index is out of bounds exception!\n");
					exit(99);
				} else {
					pushObjRef(GET_REFS(ref)[index]);
				}
				break;
			case PUTFA :
				ref2 = popObjRef();
				bip.op1 = popObjRef();
				ref = popObjRef();
				index = bigToInt();
				if (index < 0 || index > GET_SIZE(ref)) {
					printf("Index is out of bounds exception!\n");
					exit(99);
				} else {
					GET_REFS(ref)[index] = ref2;
				}
				break;
			case GETSZ :
				ref = popObjRef();
				if (IS_PRIM(ref) == TRUE) {
					bigFromInt(-1);
					pushObjRef(bip.res);
				} else {
					bigFromInt(GET_SIZE(ref));
					pushObjRef(bip.res);
				}
				break;
			case PUSHN :
				pushObjRef(NULL);
				break;
			case REFEQ :
				ref = popObjRef();
				ref2 = popObjRef();
				if (GET_REFS(ref) == GET_REFS(ref2)) {
					bigFromInt(TRUE);
					pushObjRef(bip.res);
				} else {
					bigFromInt(FALSE);
					pushObjRef(bip.res);
				}
				break;
			case REFNE :
				ref = popObjRef();
				ref2 = popObjRef();
				if (GET_REFS(ref) == GET_REFS(ref2)) {
					bigFromInt(FALSE);
					pushObjRef(bip.res);
				} else {
					bigFromInt(TRUE);
					pushObjRef(bip.res);
				}
				break;
			case ADD:
				bip.op2 = popObjRef();
				bip.op1 = popObjRef();		
				bigAdd();
				pushObjRef(bip.res);				
				break;
			case SUB:
				bip.op2 = popObjRef();
				bip.op1 = popObjRef();
				bigSub();pushObjRef(bip.res);
				break;
			case MUL:
				bip.op2 = popObjRef();
				bip.op1 = popObjRef();
				bigMul();pushObjRef(bip.res);
				break;
			case DIV:
				bip.op2 = popObjRef();
				bip.op1 = popObjRef();
				bigDiv();pushObjRef(bip.res);			
				break;
			case MOD:
				bip.op2 = popObjRef();
				bip.op1 = popObjRef();
				bigDiv();pushObjRef(bip.rem);
				break;
			case EQ:
				bip.op2 = popObjRef();
				bip.op1 = popObjRef();
				bigCmp() == 0 ? bigFromInt(1) : bigFromInt(0);
				pushObjRef(bip.res);
				break;
			case NE:
				bip.op2 = popObjRef();
				bip.op1 = popObjRef();
				bigCmp() != 0 ? bigFromInt(1) : bigFromInt(0);
				pushObjRef(bip.res);
				break;
			case LT:
				bip.op2 = popObjRef();
				bip.op1 = popObjRef();
				bigCmp() < 0 ? bigFromInt(1) : bigFromInt(0);
				pushObjRef(bip.res);
				break;	
			case LE:
				bip.op2 = popObjRef();
				bip.op1 = popObjRef();
				bigCmp() <= 0 ? bigFromInt(1) : bigFromInt(0);
				pushObjRef(bip.res);
				break;
			case GT:
				bip.op2 = popObjRef();
				bip.op1 = popObjRef();
				bigCmp() > 0 ? bigFromInt(1) : bigFromInt(0);
				pushObjRef(bip.res);
				break;
			case GE:
				bip.op2 = popObjRef();
				bip.op1 = popObjRef();
				bigCmp() >= 0 ? bigFromInt(1) : bigFromInt(0);
				pushObjRef(bip.res);
				break;
		}
		if(isDebug) {
			debug_pc = pc;
			break;		
		}
	}	
}

void printCode(unsigned int *instuctions, int pc){
	bool stop = false;
	while(!stop){		
		unsigned int ir = instuctions[pc] >> 24;
		int immediateVal = SIGN_EXTEND(IMMEDIATE(instuctions[pc]));
		pc++;	
		switch(ir){			
			case HALT:				
				printf("%04d:    HALT\n", pc-1);
				stop = true;
				break;
			case PUSHC:
				printf("%04d:    PUSHC\t%d\n", pc-1, immediateVal);
				break;
			case ADD:
				printf("%04d:    ADD\n", pc-1);
				break;
			case SUB:
				printf("%04d:    SUB\n", pc-1);
				break;
			case MUL:
				printf("%04d:    MUL\n", pc-1);
				break;
			case DIV:
				printf("%04d:    DIV\n", pc-1);
				break;
			case MOD:
				printf("%04d:    MOD\n", pc-1);
				break;
			case RDINT:
				printf("%04d:    RDINT\n", pc-1);
				break;
			case WRINT:
				printf("%04d:    WRINT\n", pc-1);
				break;
			case RDCHR:
				printf("%04d:    RDCHR\n", pc-1);
				break;
			case WRCHR:
				printf("%04d:    WRCHR\n", pc-1);
				break;
			case PUSHG:
				printf("%04d:    PUSHG\t%d\n", pc-1, immediateVal);
				break;
			case POPG:
				printf("%04d:    POPG\t%d\n", pc-1, immediateVal);
				break;
			case ASF:
				printf("%04d:    ASF\t%d\n", pc-1, immediateVal);
				break;
			case RSF:
				printf("%04d:    RSF\n", pc-1);
				break;
			case PUSHL:
				printf("%04d:    PUSHL\t%d\n", pc-1, immediateVal);
				break;
			case POPL:
				printf("%04d:    POPL\t%d\n", pc-1,immediateVal);
				break;
			case EQ:
				printf("%04d:    EQ\n", pc-1);
				break;
			case NE:
				printf("%04d:    NE\n", pc-1);
				break;
			case LT:
				printf("%04d:    LT\n", pc-1);
				break;	
			case LE:
				printf("%04d:    LE\n", pc-1);
				break;
			case GT:
				printf("%04d:    GT\n", pc-1);
				break;
			case GE:
				printf("%04d:    GE\n", pc-1);
				break;
			case JMP:
				printf("%04d:    JMP\t%d\n", pc-1,immediateVal);
				break;
			case BRF:
				printf("%04d:    BRF\t%d\n", pc-1,immediateVal);
				break;
			case BRT:
				printf("%04d:    BRT\t%d\n", pc-1,immediateVal);
				break;
			case CALL:
				printf("%04d:    CALL\t%d\n", pc-1,immediateVal);
				break;
			case RET:
				printf("%04d:    RET\n", pc-1);
				break;
			case DROP:
				printf("%04d:    DROP\t%d\n", pc-1,immediateVal);
				break;
			case PUSHR:
				printf("%04d:    PUSHR\n", pc-1);
				break;
			case POPR:
				printf("%04d:    POPR\n", pc-1);
				break;
			case DUP:
				printf("%04d:    DUP\n", pc-1);
				break;
		}
		if(isDebug) break;
	}	
	
}

void debug(unsigned int *instructions, char *fileName, int irSize, int sdaSize){	
	char befehl[10];
	int breakpoint = -1;
	while(isDebug){		
		printCode(instructions, debug_pc);
		printf("DEBUG: inspect, list, breakpoint, step, run, quit?\n");
		scanf("%s",befehl);		
		if( strcmp(befehl,"inspect") == 0){
			printf("%d\n", stackPointer);
			while(stackPointer != 0){
				stackPointer--;
				if(stack[stackPointer].isObjRef == TRUE){

				}
				
				printf("---->	%04d:    %d\n", stackPointer, *(int *) GET_REFS(stack[stackPointer].u.objRef));
			}

		}else if( strcmp(befehl,"list") == 0){
			isDebug = false;
			printCode(instructions, debug_pc);
			printf("	--- end of code ---\n");
			isDebug = true;
		}else if( strcmp(befehl,"breakpoint") == 0){
			printf("Breakpoint on?\n");
			scanf("%d",&breakpoint);
			printf("Breakpoint set on %d...Type 'run' to go there.\n", breakpoint);
		}else if( strcmp(befehl,"step") == 0){
			runCode(instructions);
		}else if( strcmp(befehl,"run") == 0){
			while(debug_pc != breakpoint && debug_pc != (irSize-1)){
				runCode(instructions);
			}
			if(breakpoint == -1) isDebug = false;
			breakpoint = -1;						
		}else if( strcmp(befehl,"quit") == 0){
			isDebug = false;
		}else{
			printf("	--- WRONG COMMAND: ---\n");
		}

	}
}

void readFile(char *fileName){
	FILE *fp = fopen(fileName, "r");
	if (fp != NULL) {
		int feld[4];
		if(fread(feld, sizeof(int), 4, fp) != 4){
			printf("Error: Header not readable\n");
			exit(99);	
		} else {
			int fileVersion;
			int irSize;
			int sdaSize;
			int n_stack = 64;		
			unsigned int *instructions;
			if(! (((feld[0] >> 0)&0xFF) == 'N' && ((feld[0] >> 8)&0xFF) == 'J' && 
			((feld[0] >> 16)&0xFF) == 'B' &&  ((feld[0] >> 24)&0xFF) == 'F') ){
				printf("Error: Format Identifier not correct\n");
				printf("Instead of 'N,J,B,F' it is '%c,%c,%c,%c' \n",
				(feld[0] >> 0),(feld[0] >> 8),(feld[0] >> 16),(feld[0] >> 24));
				exit(99);	
			}
			fileVersion = feld[1];
			if(!(fileVersion == version)){
				printf("Error: file '%s' has wrong version number\n",fileName);
				exit(99);	
			}
			irSize = feld[2];
			if(irSize == 0){
				printf("Error: No instructions available\n");
				exit(99);	
			}	
			instructions = malloc(sizeof(int)*irSize);
			if(fread(instructions, sizeof(int), irSize,fp) != irSize){
				printf("Error: Instructions not readable\n");
				exit(99);
			}
			sdaSize = feld[3];
			sda = malloc(sizeof(int)*sdaSize);
			if(isDebug){
				printf("DEBUG: file '%s' loaded (code size = %d, data size = %d)\n", fileName, irSize, sdaSize);
				printf("%s", STARTED_NJVM);
				debug(instructions, fileName, irSize, sdaSize);
			}else{
				printf("%s", STARTED_NJVM);
				stack = malloc(n_stack * 1024);
				runCode(instructions);				
			}
			printf("%s", STOPPED_NJVM);			

			free(stack);
			free(sda);
			free(instructions);
			fclose(fp);			
		}
	}else {
		printf("Error: cannot open code file '%s'\n",fileName);
		exit(99);
	}
}


int main(int argc, char *argv[]) {
	int arg = argc - 1;
	int isOption = false;

/*
	int *array = malloc(100);
	Hotzenplotz *p;


	*(array+0) = 13;
	*(array+1) = 14;
	*(array+2) = 15;

	array[0] = 13;
	array[1] = 14;
	array[2] = 15;

	
	p = malloc(100);
	*(p+0) = 2;
	*(p+1) = 3;
	*(p+2) = 4;

	p = &p[2];
	p = p - 2;

	printf("%d\n", array[1]);
	printf("%d\n", *array+1 );
	printf("%d\n", *array+2 );
	printf("%d\n", * &array[1] );

	printf("%d\n", *(p+0)  );
	printf("%d\n",  (int)* p );
	
*/


	if(argc < 2){
		printf("Error: no code file specified\n");
		exit(99);
	}
	if(argc > 3){
		printf("Error: more than one code file specified\n");	
		exit(99);
	}	
	while(!isOption && arg != 0){
		if( strcmp(argv[arg],"--help") == 0){
			printf("%s", STARTED_NJVM);
			printf("Usage: ./njvm [option] <code file>\n");
			printf("Options:\n");
			printf("  --debug        start virtual machine in debug mode\n");
			printf("  --version      show version and exit\n");
		  	printf("  --help         show this help and exit\n");
			printf("%s", STOPPED_NJVM);
			exit(99);
		}
		if( strcmp(argv[arg],"--version") == 0){
			printf("%s", STARTED_NJVM);
			printf("Ninja Virtual Machine version %d\n",version);
			printf("%s", STOPPED_NJVM);
			exit(99);
		}
		if( strcmp(argv[arg],"--debug") == 0){
			isDebug = true;
			(arg == 2) ? readFile(argv[1]) : readFile(argv[2]);
			exit(99);
		}
		arg--;
	}
	if(argc > 2){
		printf("Error: more than one code file specified\n");
	}else{		
		readFile(argv[1]);		
	}
    return 0;
}