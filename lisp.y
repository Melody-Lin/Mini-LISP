%{
#include <stdio.h>
#include <string.h>
void yyerror(const char *message);
int ans;
int n1;
int n2;
struct Node{
    char type;
    int num;
    char* name;
    int func;
    struct Node *l,*r,*v;
};
struct Node *root=NULL;
struct Arr{
  char* name;
  int val;
  int func;
};
struct Arr var[1000];
int v_top=0;
int i;
struct Node *newNode(struct Node *left,struct Node *right,char data);
void add(struct Node *node);
void sub(struct Node *node);
void mul(struct Node *node);
void div(struct Node *node);
void mod(struct Node *node);
void grt(struct Node *node);
void sml(struct Node *node);
void eql(struct Node *node);
void and(struct Node *node);
void or(struct Node *node);
void preorder(struct Node *node);
%}
%union{
  int ival;
  int bval;
  char* sval;
  struct Node *nval;
}
%token ADD SUB MUL DIV MOD GRT SML EQL AND OR NOT PRTNUM PRTBOOL DEFINE FUNC IF
%token <ival> INUMBER
%token <sval> ID
%token <bval> BOOL
%type <nval> program stmts stmt exp exps def_stmt print_stmt variable num_op logical_op 
%type <nval> plus minus multiply divide modulus greater smaller equal and_op or_op not_op
%type <nval> if_exp test_exp then_exp else_exp
%%
program: stmts { root=$1; }
       ;
stmts: stmt stmts { $$=newNode($1,$2,'S'); }
     | stmt { $$=$1; }
     ; 
stmt: exp { $$=$1; }
    | def_stmt { $$=$1; }
    | print_stmt { $$=$1; }
    ;
exp: BOOL { $$=newNode(NULL,NULL,'B'); $$->num=$1; }
   | INUMBER { $$=newNode(NULL,NULL,'N'); $$->num=$1; }
   | variable { $$=$1; }
   | num_op { $$=$1; }
   | logical_op { $$=$1; }
   | if_exp { $$=$1; }
   ;
num_op: plus { $$=$1; }
      | minus { $$=$1; }
      | multiply { $$=$1; }
      | divide { $$=$1; }
      | modulus { $$=$1; }
      | greater { $$=$1; }
      | smaller { $$=$1; }
      | equal { $$=$1; }
      ;
plus: '(' ADD exp exps ')' { $$=newNode($3,$4,'+'); }
    ;
minus: '(' SUB exp exp ')' { $$=newNode($3,$4,'-'); }
     ;
multiply: '(' MUL exp exps ')' { $$=newNode($3,$4,'*'); }
        ;
divide: '(' DIV exp exp ')' { $$=newNode($3,$4,'/'); }
      ;
modulus: '(' MOD exp exp ')' { $$=newNode($3,$4,'%'); }
       ;
greater: '(' GRT exp exp ')' { $$=newNode($3,$4,'>'); }
       ;
smaller: '(' SML exp exp ')' { $$=newNode($3,$4,'<'); }
       ;
equal: '(' EQL exp exps ')' { $$=newNode($3,$4,'='); }
     ;
logical_op: and_op { $$=$1; }
          | or_op { $$=$1; }
          | not_op { $$=$1; }
          ;
and_op: '(' AND exp exps ')' { $$=newNode($3,$4,'&'); }
      ;
or_op: '(' OR exp exps ')' { $$=newNode($3,$4,'|'); }
     ;  
not_op: '(' NOT exp ')' { $$=newNode($3,NULL,'!'); }
      ;
exps: exp exps { $$=newNode($1,$2,'E'); }
    | exp { $$=$1; }
    ;
def_stmt: '(' DEFINE variable exp ')' { $$=newNode($3,$4,'D'); }
        ;
variable: ID { $$=newNode(NULL,NULL,'V'); $$->name=$1; }
        ;
if_exp: '(' IF test_exp then_exp else_exp ')' { $$=newNode($3,$5,'I'); $$->v=$4; }
      ;
test_exp: exp { $$=$1; }
        ;
then_exp: exp { $$=$1; }
        ;
else_exp: exp { $$=$1; }
        ;
print_stmt: '(' PRTNUM exp ')' { $$=newNode($3,NULL,'n'); }
          | '(' PRTBOOL exp ')' { $$=newNode($3,NULL,'b'); }
          ;
%%
void yyerror(const char *message){
  printf("syntax error\n");
}
struct Node *newNode(struct Node *left,struct Node *right,char data) {
  struct Node *node=(struct Node *)malloc(sizeof(struct Node));
  node->type=data;       
  node->num=0;
  node->name="";
  node->func=0;
  node->l=left;
  node->r=right;
  node->v=NULL;
  return node;
}
void add(struct Node *node){
  if(node->l!=NULL) {
    ans=ans+(node->l->num);
    if(node->l->type=='E') { add(node->l); }
  }
  if(node->r!=NULL) {
    ans=ans+(node->r->num);
    if(node->r->type=='E') { add(node->r); }
  }
}
void sub(struct Node *node){
  ans=(node->l->num)-(node->r->num);
}
void mul(struct Node *node){
  if(node->l!=NULL) {
    if(node->l->type!='E') { ans=ans*(node->l->num); }
    else { mul(node->l); }
  }
  if(node->r!=NULL) {
    if(node->r->type!='E') { ans=ans*(node->r->num); }
    else { mul(node->r); }
  }
}
void div(struct Node *node){
  if(node->l!=NULL && node->r!=NULL){
    ans=(node->l->num)/(node->r->num);
  }
}
void mod(struct Node *node){
  if(node->l!=NULL && node->r!=NULL){
    ans=(node->l->num)%(node->r->num);
  }
}
void grt(struct Node *node){
  if(node->l!=NULL && node->r!=NULL){
    if((node->l->num)>(node->r->num)) { ans=1; }
    else { ans=0; }      
  }
}
void sml(struct Node *node){
  if(node->l!=NULL && node->r!=NULL){
    if((node->l->num)<(node->r->num)) { ans=1; }
    else { ans=0; }      
  }
}
void eql(struct Node *node){
  if(node->l!=NULL){
    if(node->l->type!='E'){
      if(n1==0){
        n2=node->l->num;
        n1=1;
      }
      else{
        if((node->l->num)!=n2) { ans=0; }
      }
    }
    else { eql(node->l); }
  }
  if(node->r!=NULL){
    if(node->r->type!='E'){
      if(n1==0){
        n2=node->r->num;
        n1=1;
      }
      else{
        if((node->r->num)!=n2) { ans=0; }
      }  
    }
    else { eql(node->r); }
  }
}
void and(struct Node *node){
  if(node->l!=NULL){
    if(node->l->type!='E') { ans=ans&(node->l->num); }
    else { and(node->l); }
  }
  if(node->r!=NULL){
    if(node->r->type!='E') { ans=ans&(node->r->num); }
    else { and(node->r); }
  }
}
void or(struct Node *node){
  if(node->l!=NULL){
    if(node->l->type!='E') { ans=ans|(node->l->num); }
    else { or(node->l); }
  }
  if(node->r!=NULL){
    if(node->r->type!='E') { ans=ans|(node->r->num); }
    else { or(node->r); }
  }
}
void preorder(struct Node *node){
  if(node==NULL) { return; }
  if(node->type=='+'){
    preorder(node->l);
    preorder(node->r);
    ans=0;
    add(node);
    node->num=ans;
  }
  else if(node->type=='-'){
    preorder(node->l);
    preorder(node->r);
    sub(node);
    node->num=ans;
  }
  else if(node->type=='*'){
    preorder(node->l);
    preorder(node->r);
    ans=1;
    mul(node);
    node->num=ans;
  }
  else if(node->type=='/'){
    preorder(node->l);
    preorder(node->r);
    div(node);
    node->num=ans;
  }
  else if(node->type=='%'){
    preorder(node->l);
    preorder(node->r);
    mod(node);
    node->num=ans;
  }
  else if(node->type=='>'){
    preorder(node->l);
    preorder(node->r);
    grt(node);
    node->num=ans;
  }
  else if(node->type=='<'){
    preorder(node->l);
    preorder(node->r);
    sml(node);
    node->num=ans;
  }
  else if(node->type=='='){
    preorder(node->l);
    preorder(node->r);
    ans=1;
    n1=0;
    eql(node);
    node->num=ans;
  }
  else if(node->type=='&'){
    preorder(node->l);
    preorder(node->r);
    ans=1;
    and(node);
    node->num=ans;
  }
  else if(node->type=='|'){
    preorder(node->l);
    preorder(node->r);
    ans=0;
    or(node);
    node->num=ans;
  }
  else if(node->type=='!'){
    preorder(node->l);
    node->num=!node->l->num;
  }
  else if(node->type=='b'){
    preorder(node->l);
    if(node->l->num) { printf("#t\n"); }
    else { printf("#f\n"); }
  }
  else if(node->type=='n'){
    preorder(node->l);
    printf("%d\n",node->l->num);
  }
  else if(node->type=='D'){
    if(node->l->type=='V'){
      preorder(node->l);
      preorder(node->r);
      var[v_top].name=node->l->name;
      var[v_top++].val=node->r->num;
    }
  }
  else if(node->type=='V'){
    for(i=0;i<v_top;i++){
      if(var[i].func==node->func && strcmp(var[i].name,node->name)==0){
        node->num=var[i].val;
        break;
      }
    }
  }
  else if(node->type=='I'){
    preorder(node->l);
    preorder(node->v);
    preorder(node->r);
    if(node->l->num==1) { node->num=node->v->num; }   
    else { node->num=node->r->num; }   
  }
  else{
    preorder(node->l);
    preorder(node->r);
  }
}
int main(){
  yyparse();
  preorder(root);
  return (0);
}