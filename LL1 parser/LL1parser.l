%{  
#include <string.h>
#include<stdio.h>
#include<stdlib.h>
#define id(x) (x-'A' + 26)
#define review(x) (char)(x - 26 + 'A')
#define EPSILON "∈"
typedef struct {
	int number;
	char * c;
} Body;
typedef struct Prod{
	int num;
	char RHS[52][52];
}Prod;

typedef struct {
	int number;
	Body * list;
} Production;

int universla_flag = 0;
char **follow;
Prod grammar[52];
int terminals[256] = {0};
int variables[256] = {0};
char nt[50];
char prod[10][50]; 
int i = 0;
int total_prod = 0;

int getInput(int * variables, int * terminals, Production productions[256]);
int computeFirst(int * variables, int * terminals, Production * productions, char ** first);
int firstOf(int * variables, int * terminals, Production * productions, int symbol, char * first);
int computeFollow(int * variables, int * terminals, Production * productions, char ** firstArray);
int isNullable(Production production);
int expandFollow(int * variables, char ** follow);
int displayFollow(char ** follow);
void printtable(Production* production,char**first, char**follow,int* terminal, int* variable);
int precedence_list[52];

%}  

%% 
"%".*
[a-zA-Z\|\->∈\+\*]*   {strcpy(prod[i],yytext); i++;total_prod++;}
%%  


void print(char c){
	if(c < 'A' && c >= 'A'-26) printf("%c'",c+26);
	else printf("%c",c);
}
void Terminal_nonterminal(){
	for(int i = 0;i < 52;i++){
		if(grammar[precedence_list[i]].num){
            variables[review(precedence_list[i])] = 1;
			for(int j = 0;j < grammar[precedence_list[i]].num;j++){
				for(int k = 0;grammar[precedence_list[i]].RHS[j][k] != '\0';k++){
                    char vt = grammar[precedence_list[i]].RHS[j][k];
                    if(vt >='a' && vt <= 'z'){
                        terminals[vt] = 1;
					}
                    if(vt >= 'A' && vt <= 'Z'){
                        variables[vt] = 1;
					}
				}
				char t[2] = {'|','\n'};
			}
		}
	}

}

void printProd(){
	for(int i = 0;i < 52;i++){
		if(grammar[precedence_list[i]].num){
			print(review(precedence_list[i]));
			printf(" ->  ");
			for(int j = 0;j < grammar[precedence_list[i]].num;j++){
				for(int k = 0;grammar[precedence_list[i]].RHS[j][k] != '\0';k++){
					print(grammar[precedence_list[i]].RHS[j][k]);				}	
				char t[2] = {'|','\n'};
				printf("%c",t[j == grammar[precedence_list[i]].num - 1]);
			}
		}
	}
	printf("\n\n");

}
void createOrderOfVaribale(){
	int pointer = 0;
	precedence_list[pointer++] = id('S');
	precedence_list[pointer++] = id('S' - 26);
	grammar[id('S')].num = grammar[id('S'-26)].num = 0;

	for(int i = 'A';i <= 'Z';i++){
		if(i == 'S') continue;
		precedence_list[pointer++] = id(i);
		precedence_list[pointer++] = id(i-26);
		grammar[id(i)].num = grammar[id(i-26)].num = 0;
	}
}

void addToGrammar(int c,char * str){
	int idx = grammar[c].num;
	strcpy(grammar[c].RHS[idx],str);
	grammar[c].num++;
}

void remProd(int c,int itr){
	int num = grammar[c].num;
	for(int i = itr+1;i < num;i++){
		strcpy(grammar[c].RHS[i-1],grammar[c].RHS[i]);
	}
	grammar[c].num--;
}

void cleanImmediateLeftRecusrion(int nonT,int idx){
	char temp[20];
	for(int i = 1;;i++){
		if(grammar[nonT].RHS[idx][i] == '\0'){
			temp[i-1] = '\0';
			break;
		}
		temp[i-1] = grammar[nonT].RHS[idx][i];
	}
	char temp2[20] = {review(nonT-26)};
	strcat(temp,temp2);
	addToGrammar(nonT-26,temp);
}


void removeImmediateLeftRecursion(int nonT){
	int flag = 0;
	int num = grammar[nonT].num;
	for(int i = 0;i < num;i++){
		if(id(grammar[nonT].RHS[i][0]) == nonT){
			flag = 1;
			cleanImmediateLeftRecusrion(nonT,i);
		}
	}
	if(flag){
		addToGrammar(nonT - 26,EPSILON);
		char temp2[20] = {review(nonT-26)};
		for(int i = 0;i < num;i++){
			if(id(grammar[nonT].RHS[i][0]) != nonT){
				char temp[20];
				strcpy(temp,grammar[nonT].RHS[i]);
				strcat(temp,temp2);
				addToGrammar(nonT,temp);
			}
			remProd(nonT,i);
			i--;
			num--;
		}
	}
}
void cleanLeft(int c,int itr,int nonT){
	char temp[20];
	for(int i = 1;grammar[c].RHS[itr][i] != '\0';i++){
		temp[i-1] = grammar[c].RHS[itr][i];
	}
	temp[strlen(grammar[c].RHS[itr])-1] = '\0';
	for(int i = 0;i < grammar[nonT].num;i++){
		char temp2[50];
		strcpy(temp2,grammar[nonT].RHS[i]);
		strcat(temp2,temp);
		addToGrammar(c,temp2);
	}
	remProd(c,itr);
}

void removeIndirectleftRecursion(){
	for(int i = 0;i < 52;i+=2){
		for(int j = 0;j < i;j++){
			int num = grammar[precedence_list[i]].num;
			for(int itr = 0;itr < num;itr++){
				if(id(grammar[precedence_list[i]].RHS[itr][0]) == precedence_list[j]){
					cleanLeft(precedence_list[i],itr,precedence_list[j]);
					itr--;
					num--;
				}
			}
		}
		removeImmediateLeftRecursion(precedence_list[i]);
	}
}

void ssort(char arr[][52], int n){  
    int i, j, min_idx;  
    char minStr[52];  
    for (i = 0; i < n-1; i++){  
        int min_idx = i;  
        strcpy(minStr, arr[i]);  
        for (j = i + 1; j < n; j++){  
            if (strcmp(minStr, arr[j]) > 0){  
                strcpy(minStr, arr[j]);  
                min_idx = j;  
            }  
        }
        if (min_idx != i){  
            char temp[52];  
            strcpy(temp, arr[i]);
            strcpy(arr[i], arr[min_idx]);
            strcpy(arr[min_idx], temp);  
        }  
    }  
}

void sortallprod(){
	for(int i = 0;i < 52;i++){
		ssort(grammar[i].RHS,grammar[i].num);
	}
}

int checkNext(){
	int next;
	for(int i = 0;i < 52;i++){
		if(grammar[precedence_list[i]].num) next = i;
	}
	if(next & 1) next++;
	else next += 2;
	return next;
}

void createNewFor(int j,int start,int i,int nextVar,int flag[52][52]){
	for(int k = start;k < j;k++){
		char dest[20] = {'\0'};
		int itr;
		for(itr = 1;grammar[i].RHS[k][itr] != '\0';itr++){
			dest[itr-1] = grammar[i].RHS[k][itr];
		}
		dest[itr] = '\0';
		if(strcmp(dest,"\0") == 0) strcpy(dest,EPSILON);
		flag[i][k] = 1;
		addToGrammar(precedence_list[nextVar],dest);
	}
}

void removedReduction(int flag[52][52]){
	for(int i = 0;i < 52;i++){
		int removed = 0;
		int num = grammar[i].num;
		for(int j = 0;j < num;j++){
			if(flag[i][j] == 1){
				remProd(i,j-removed);
				removed++;
			}
		}
	}
}

void removeLeftFactoring(int nextVar){
	int flag [52][52];
	memset(flag,0,sizeof flag);
	for(int p = 0;p < 52;p++){
		int i = precedence_list[p];
		char currChar = grammar[i].RHS[0][0];
		int start = 0,num = grammar[i].num;
		for(int j = 1;j <= num;j++){
			if(j == num || grammar[i].RHS[j][0] != currChar){
				if(j - start >= 2){
					createNewFor(j,start,i,nextVar,flag);
					ssort(grammar[precedence_list[nextVar]].RHS,grammar[precedence_list[nextVar]].num);
					char newProd[20] = {grammar[i].RHS[start][0],review(precedence_list[nextVar])};
					addToGrammar(i,newProd);
					nextVar += 2;
				}
				if(j != num){
					currChar = grammar[i].RHS[j][0];
					start = j;
				}
			}
		}
	}
	removedReduction(flag);
}

void GrammarInput(){
	char a;
	char b[20];
    char *token1;
    char *token2;
    for(int k = 0; k < total_prod;k++)
    {
        a = prod[k][0];
       token1 = strtok(prod[k], "->");
       token1 = strtok(NULL, "->");

        token2 = strtok(token1, "|");
   
        while( token2 != NULL ) {
            strcpy(b,token2);
            if(strcmp(b,"∈") == 0){
			    strcpy(b,EPSILON);}
            addToGrammar(id(a),b);
            token2 = strtok(NULL, "|");
            
            }

    }
}
int main(int argc, char **argv)
{
    int nextVar;
	Production productions[256];
	int i;
	int j;
	char ** first = (char **) malloc(256 * sizeof(char *));
	for(i = 0; i < 256; i++) {
		first[i] = NULL;
	}

    FILE *file;

    file=fopen("grammar.txt","r");
    if(!file)
    {
        printf("could not open the file");
        exit(0);
    }
    yyin=file;
    yylex();
    createOrderOfVaribale();
	GrammarInput();
	printf("Given Grammar\n\n");
	 printProd();
    removeIndirectleftRecursion();
	printf("--------------------------------------------------------------");
    printf("\nGrammar after removal of left recursion-> \n\n");
    printProd();
	sortallprod();
	nextVar = checkNext();
	removeLeftFactoring(nextVar);
	printf("--------------------------------------------------------------");
    printf("\nGrammar after left factoring-> \n\n");
	printProd();
	Terminal_nonterminal();
	getInput(variables, terminals, productions);
	computeFirst(variables, terminals, productions, first);
	printf("\n\n");
	printf("--------------------------------------------------------------\n");
	computeFollow(variables, terminals, productions, first);
	printf("\n\n------------------------Parse Table---------------------------\n");
	printf("Here {..} is terminals and [...] is the production\n");

	printtable(productions,first,follow,terminals,variables);
	printf("\n--------------------------------------------------------------");
	if(universla_flag){
		printf("\n\nGiven Grammar is not LL(1) \n");
		printf(" -> Because one or more table have more than one entry\n and we can not completely avoid back tracking");
	}
	else{
		printf("\n\nGiven Grammar is LL(1)\n");
		printf(" -> Because each table has unique entry");}
		printf("\n\n--------------------------------------------------------------");
		printf("\n\n\n\n\n\n\n\n\n");
	return 0;
}

int yywrap()
{
return(1);
}
int getInput(int * variables, int * terminals, Production productions[256])
{
	int i;
	int j;
	char c,head;
	for (i = 0; i < 256; i++) {
		productions[i].number = 0;
		productions[i].list= NULL;
	}
    for(int i = 0;i < 52;i++){
		if(grammar[precedence_list[i]].num){
            head =review(precedence_list[i]);
			for(int j = 0;j < grammar[precedence_list[i]].num;j++)
            {
                if (variables[head]) {
                    productions[head].number++;
                    productions[head].list = (Body *) realloc(productions[head].list, productions[head].number * sizeof(Body));
                    productions[head].list[productions[head].number - 1].number = 0;
                    productions[head].list[productions[head].number - 1].c = NULL;
                
				for(int k = 0;grammar[precedence_list[i]].RHS[j][k] != '\0';k++){
                    
                    char vt = grammar[precedence_list[i]].RHS[j][k];
                    
                    if(vt < 0){
						vt='@';}
					
                    productions[head].list[productions[head].number - 1].number++; 
				    productions[head].list[productions[head].number - 1].c = (char *) realloc(productions[head].list[productions[head].number - 1].c, productions[head].list[productions[head].number - 1].number * sizeof(char));
				if (variables[vt] || terminals[vt]) {
					
					productions[head].list[productions[head].number - 1].c[productions[head].list[productions[head].number - 1].number - 1]  = vt;
				} else {
					productions[head].list[productions[head].number - 1].c[productions[head].list[productions[head].number - 1].number - 1]  = '\0';
					break;
				}
				}

		}
			}
		}
	}


	return 0;
} 

int computeFollow(int * variables, int * terminals, Production * productions, char ** firstArray)
{
	int head;
	int i;
	int j;
	int k;
	int l;
	int m;
	follow = (char **) malloc(256 * sizeof(char *));
	for (i = 0; i < 256; i++) {
		if (variables[i]) {
			follow[i] = (char *) malloc(256 * sizeof(char));
			for (j = 0; j < 256; j++) {
				follow[i][j] = 0;
			}
			if (i == 'S') {
				int dol  ='$';
				follow[i][dol] = 1;
			}
		} else {
			follow[i] = NULL;
		}
	}
	for (head = 0; head < 256; head++) {
		for (j = 0; j < productions[head].number; j++) {
			char symbol = '\0';
			char lookahead = '\0';
			for(k = 0; k < productions[head].list[j].number; k++) {
				symbol = productions[head].list[j].c[k];

				if (variables[symbol]) {
					if (k == productions[head].list[j].number - 1) {
						follow[symbol][head] = 1;
					}

					for (l = k + 1; l < productions[head].list[j].number; l++) {
						lookahead = productions[head].list[j].c[l];
						if (terminals[lookahead]) {
							if ((l == k + 1)) {
								follow[symbol][lookahead] = 1;
							}
							break;
						} else if (variables[lookahead]) {
							for (m = 0; m < 256; m++) {
								if (firstArray[lookahead][m]) {
									follow[symbol][m] = 1;
								}
							}
							if (isNullable(productions[lookahead])) {
								if (l == productions[head].list[j].number - 1) {
									follow[symbol][head] = 1;
								}
							} else {
								break;
							}
						} else {
							printf("Error: no clue\n");
							break;
						}
					}
				}
			}
		}
	}
	expandFollow(variables, follow);
	displayFollow(follow);
}

int expandFollow(int * variables, char ** follow)
{
	int i;
	int j;
	int k;
	for (i = 0; i < 256; i++) {
		if (variables[i]) {
			int boolean = 1;
			int z = 3;
			while (boolean) {
				boolean = 0;
				for (j = 0; j < 256; j++) {
					if ((follow[i][j] == 1) && variables[j]) {
						boolean = 1;
						for (k = 0; k < 256; k++) {
							if ((follow[j][k] == 1) && (follow[i][k] == 0)) {
								follow[i][k] = 1;
							}
						}
						follow[i][j] = -1;

					}
				}
			}
		}
	}
}

int displayFollow(char ** follow)
{
	int i;
	int j;
	printf("Follow\n\n");
	for (i = 0; i < 256; i++) {
		if (follow[i]) {
			if(i < 'A' && i >= 'A'-26)
				printf("follow( %c' ) = { ", i+26);
			else	
				printf("follow( %c ) = { ", i);
			for (j = 1; j < 256; j++) {
				if (follow[i][j] == 1) {
					if(j < 'A' && j >= 'A'-26)
						printf("%c' ",j+26);
					else
						printf("%c ", j);
				}
			}
			printf("}\n");
		}
	}	
}

int computeFirst(int * variables, int * terminals, Production * productions, char ** first)
{
	int i;
	int j;
	int k;
	char symbol;
	printf("--------------------------------------------------------------");
	printf("\nFirst\n\n");
	for (i = 0; i < 256; i++) {
		if (variables[i]) {
			first[i] = (char *) malloc(256 * sizeof(char));
			firstOf(variables, terminals, productions, i, first[i]);
			if(i < 'A' && i >= 'A'-26)
				printf("%c' -> { ", i+26);
			else
				printf("%c -> { ", i);
			if (first[i][0])  {
				printf("ϵ ");
			}
			for (j = 1; j < 256; j++) {
					if (first[i][j]) {
						if(j < 'A' && j >= 'A'-26)
					printf("%c' ", j+26);
					else
						
						printf("%c ", j);
					}
			}
			printf("}\n");
		}
	}
	return 0;
}

int firstOf(int * variables, int * terminals, Production * productions, int symbol, char * first)
{
	int lookup;
	int j;
	int k;
	for (j = 0; j < productions[symbol].number; j++) {
		for (k = 0; k < productions[symbol].list[j].number; k++) {
			lookup = productions[symbol].list[j].c[k];
			if (variables[lookup]) {
					firstOf(variables, terminals, productions, lookup, first);
					if (!isNullable(productions[lookup])) {
						break;
					}
			} else if (terminals[lookup]) {
				first[lookup] = 1;
				first[0] = 0;
                break;
			}
             else if (lookup == '\0') {
				first[lookup] = 1;
                }
			 else{
				printf("error -> %d symbol neither a terminal nor a variable", lookup);
			}
		}
		if (k == productions[symbol].list[j].number) {
			first[0] = 1;
		}
	}
	return 0;
}

int isNullable(Production production) {
	int i;
	for (i = 0; i < production.number; i++) {
		if (production.list[i].number && (production.list[i].c[0] == '\0')) {
			return 1;
		}
	}
	return 0;
}
int checkprodNull(Production prod,int j,char **first,char **follow,int *variable)
{
	int flag = -1;
	for(int k = 0; k < prod.list[j].number;k++)
	{
		int lk =prod.list[j].c[k];
		if(variable[lk] && first[lk][0])
		flag+=1;
		else{
		flag =-1;
		break;}
	}
	if (flag != -1)
		return 1;
	else
	{
		return 0;
	}
	
}

int checkinFirst(Production prod,int j,int t,int i,char **first,char**follow,int* terminal,int* variable)
{
	int flag = -1;
	 for(int k = 0; k < prod.list[j].number;k++)
		{
			int lookup = prod.list[j].c[k];
			
			if(variable[lookup]){
				if(first[lookup][t])
				{
					flag+=1;
						break;
				}
				else if(first[lookup][0])
					continue;
				}
				
			else if(terminal[lookup])
			{
				if(lookup == t){
					flag+=1;
					break;}
				else
				{
					flag == -1;
					break;
				}	
			}
		}
	if(flag != -1)
	return 1;
	else return 0;
}

int checknullinfirst(Production prod,int j,int t,int i,char **first,char**follow,int* terminal,int* variable)
{
	int flag = -1;
	 for(int k = 0; k < prod.list[j].number;k++)
		{
			
			int lookup = prod.list[j].c[k];
			if(prod.list[j].c[k] == '\0')
			{
				flag+=1;
			}
			if(variable[lookup]){
				if(first[lookup][0])
					flag+=1;
				else
				flag = -1;

		}
		if(terminal[lookup]){
			flag= -1;
			break;}

		}
		if(flag!= -1 && follow[i][t])
		return 1;
		else
		{
			return 0;
		}
		

}
void printtable(Production* production,char**first, char**follow, int* terminal, int* variable)
{	
    for(int i = 0; i <256;i++)
    {
      
        if(variable[i]){
			if(i < 'A' && i >= 'A'-26)
				printf("\n%c'  -> ",i+26);
			else
				printf("\n%c  -> ",i);
			for(int t = 0; t < 256;t++)
			{
				if(terminal[t] || t == 36)
				{ 
					printf("  {%c}",t);

				printf("[");
				int count = 0 ;
			     for(int j = 0; j < production[i].number;j++)
			     {	
					 
					 if(checkinFirst(production[i],j,t,i,first,follow,terminal,variable))
					 {
						 for(int k =0; k < production[i].list[j].number;k++)
						 {
							print(production[i].list[j].c[k]);
							 
						 }
						 count++;
						  printf(", ");
						 
					 }
					
					 else if(checknullinfirst(production[i],j,t,i,first,follow,terminal,variable))
					 {
						 for(int k =0; k < production[i].list[j].number;k++)
						 {
							 if(production[i].list[j].c[k] =='\0')
							 	printf("ϵ");
							 print(production[i].list[j].c[k]);
							 
						 }
						 count++;
						 
						  printf(", ");

					 }
					 if(count > 1)
				 	universla_flag = 1;
			      }printf("]");
				 
				
        }
		
    }
	printf("\n");
    }
	}}



