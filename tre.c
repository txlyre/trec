//trec - Trec: tREe Constructor. by @txlyre,www:txlyre.website.
#include<stdio.h>
#include<stdlib.h>
typedef int I;typedef size_t Z;typedef double D;typedef char C;typedef C*S;typedef void V;typedef V*P;
typedef enum{var,num,tree,lam,cons,cond,app,prim}Y;typedef struct T{Y y;C v;D n;struct T*h,*t;}*T;//tree type&tree (y=type,v=var,n=number,h=head,t=tail).
typedef struct H{T*a;Z z,k;T r;I v;I p;}*H;H h;//GC (a=allocations,z=size of heap,k=count of allocations,r=root,v=verbose,p=pause).
#define R return
#define nil NULL
#define PR(s)fwrite(s,1,strlen(s),stdout)

V e(S s){fprintf(stderr,"%s error\n",s);exit(1);}//report fatal error.
P al(Z z){P r;if(!(r=malloc(z)))e("memory");R r;}//malloc&check.
P ca(Z k,Z z){P r;if(!(r=calloc(k,z)))e("memory");R r;}//calloc&check.
P re(P p,Z z){P r;if(!(r=realloc(p,z)))e("memory");R r;}//realloc&check.
V clr(T*p,Z s,Z z){for(;s<z;s++)p[s]=nil;}//clear memory piece.
V frT(T t,I d){if(!t)R;if(t->y==var||t->y==num)free(t);else if(d){frT(t->h,d>0?d-1:d);frT(t->t,d>0?d-1:d);free(t);}}//free tree recursively.
V ih(I v){h=al(sizeof(struct H));h->v=v;if(h->v)PR("gc: init.\n");h->z=16;h->p=0;h->a=ca(h->z,sizeof(T));clr(h->a,0,h->z);h->r=nil;}//init heap.
V dh(){if(h->v)PR("gc: deinit.\n");free(h->a);free(h);frT(h->r,-1);}//delete heap.
I in(T a,T b){if(a==b)R 1;if(!b||b->y==var||b->y==num)R 0;if(a==b->h)R 1;else if(a==b->t)R 1;else if(in(a,b->h))R 1;else if(in(a,b->t))R 1;else R 0;}//check that a is in b.
I il(T t){if(!t)R 1;if(!h->r)R 0;if(t==h->r)R 1;R in(t,h->r);}//check that t is live.
V ct(){if(h->p){if(h->v)PR("gc: paused.\n");R;}if(h->v)printf("gc: collect start (used:%zu / max:%zu).\n",h->k,h->z);for(Z i=0;i<h->z;i++)if(!il(h->a[i])){if(h->v)printf("gc: free %zu!\n",i);free(h->a[i]);h->a[i]=nil;h->k--;}if(h->v)printf("gc: collect finish (used:%zu / max:%zu).\n",h->k,h->z);}//collect trash.
V ex(){if(h->v)PR("gc: expand heap.\n");Z k=h->z;h->z*=2;h->a=re(h->a,h->z*sizeof(T));clr(h->a,k,h->z);}//expand heap.
T ha(){if(h->z==h->k)ct();if(h->z==h->k)ex();T t=nil;for(Z i=0;i<h->z;i++)if(!h->a[i]){if(h->v)printf("gc: heap allocation %zu!\n",i);h->a[i]=t=al(sizeof(struct T));h->k++;break;}if(!t)e("memory");R t;}//allocate tree in the heap.
T aT(Y y,C v,D n,T h,T t){T r=ha();r->y=y;r->v=v;r->n=n;r->h=h;r->t=t;R r;}//allocate tree.
#define DP {ct();I pst=h->p;h->p=1//do paused start.
#define DE h->p=pst;ct();}//do paused end.
#define var(v)(aT(var,v,0,nil,nil))
#define num(n)(aT(num,0,n,nil,nil))
#define tree(h,t)(aT(tree,0,0,h,t))
#define lam(h,t)(aT(lam,0,0,h,t))
#define cons(h,t)(aT(cons,0,0,h,t))
#define cond(h,t)(aT(cond,0,0,h,t))
#define app(h,t)(aT(app,0,0,h,t))
#define prim(h,t)(aT(prim,0,0,h,t))
V prT(T t,I i){for(I k=0;k<i;k++)putchar(' ');if(!t){PR("nil");R;}switch(t->y){
 case var:if(!t->v)PR("it");else putchar(t->v);R;
 case num:printf("%.15g",t->n);R;
 case tree:PR("Tree");break;case lam:PR("Lambda");break;
 case cons:PR("Cons");break;case cond:PR("Cond");break;
 case app:PR("Apply");break;case prim:PR("Prim");break;}
 putchar('\n');prT(t->h,i+1);putchar('\n');prT(t->t,i+1);}//print tree.
T r(T);T ra(T,T);T rd(T c,T b){while(b&&b->y==cons){T d=b->h;if(d&&d->y==cons){d->h=r(ra(d->h,c));if(d->h)R d->t;}b=b->t;}R b;} // reduce cond.
T cp(T t){if(!t)R t;R aT(t->y,t->v,t->n,cp(t->h),cp(t->t));}//copy tree.
T sb(T w,T x,T y){if(!w)R w;if(!x||x->y!=var)R w;if(w->y==var){if(w->v==x->v)R y;R w;}if(w->y==lam||w->y==prim){w->t=sb(w->t,x,y);R w;}
 if(w->h&&w->h->y==var&&w->h->v==x->v)w->h=y;else if(w->h)w->h=sb(w->h,x,y);
 if(w->t&&w->t->y==var&&w->t->v==x->v)w->t=y;else if(w->t)w->t=sb(w->t,x,y);R w;}//substitute var.
T ra(T f,T x){if(!f)R nil;if(f->y!=lam)R x;DP;T u=cp(f),v=var(0);if(f->h&&f->h->y==var)f->t=sb(f->t,f->h,x);f->t=sb(f->t,v,u);DE;R f->t;}//reduce apply.
I ln(T t){I z=0;while(t&&t->y==cons){z++;t=t->t;};R z;}//length.
T xn(T t,I n){if(n<0)n=ln(t)+n;if(n<0)R nil;for(I i=0;t&&t->y==cons;t=t->t,i++)if(i==n)R t->h;R nil;}//get nth element.
T sn(T t,I n,T x){if(n<0)n=ln(t)+n;if(n<0)R x;for(I i=0;t&&t->y==cons;t=t->t,i++)if(i==n)R t->h=x;R x;}//set nth element.
D t2i(T t){if(!t||t->y!=num)R 0;R t->n;}//try to convert to num.
I eq(T x,T y){if(!x&&y)R 0;else if(x&&!y)R 0;if(x==y)R 1;if(x->y==y->y){if(x->y==var)R x->v==y->v;else if(x->y==num)R x->n==y->n;R eq(x->h,y->h)&&eq(x->t,y->t);}R 0;}//equals.
T mp(T f,T x){T e=x;while(x&&x->y==cons){DP;T u=cp(f);x->h=ra(u,r(x->h));x->h=r(x->h);x=x->t;DE;}R e;}//map.
T rp(C p,T x){D a,b;T t,e;switch(p){
 case 'a':putchar('\n');x=nil;break;
 case 'b':prT(x,0);x=nil;break;
 case 'c':a=t2i(x);putchar((I)a);x=nil;break;
 case 'd':a=t2i(r(xn(x,0)));b=t2i(r(xn(x,1)));x=num(a+b);break;
 case 'e':a=t2i(r(xn(x,0)));b=t2i(r(xn(x,1)));x=num(a-b);break;
 case 'f':a=t2i(r(xn(x,0)));b=t2i(r(xn(x,1)));x=num(a*b);break;
 case 'g':a=t2i(r(xn(x,0)));b=t2i(r(xn(x,1)));x=num(a/b);break;
 case 'h':a=t2i(r(xn(x,0)));b=t2i(r(xn(x,1)));x=num(((Z)a)%((Z)b));break;
 case 'i':a=t2i(r(x));x=num(-a);break;
 case 'j':a=t2i(r(xn(x,0)));b=t2i(r(xn(x,1)));x=a>b?cons(nil,nil):nil;break;
 case 'k':a=t2i(r(xn(x,0)));b=t2i(r(xn(x,1)));x=a<b?cons(nil,nil):nil;break;
 case 'l':a=t2i(r(xn(x,0)));b=t2i(r(xn(x,1)));x=a>=b?cons(nil,nil):nil;break;
 case 'm':a=t2i(r(xn(x,0)));b=t2i(r(xn(x,1)));x=a<=b?cons(nil,nil):nil;break;
 case 'n':t=sn(x,0,r(xn(x,0)));e=sn(x,1,r(xn(x,1)));x=eq(t,e)?cons(nil,nil):nil;break;
 case 'o':t=sn(x,0,r(xn(x,0)));e=sn(x,1,r(xn(x,1)));x=!eq(t,e)?cons(nil,nil):nil;break;
 case 'p':t=sn(x,0,r(xn(x,0)));e=sn(x,1,r(xn(x,1)));x=mp(t,e);break;
 case 'q':t=sn(x,0,r(xn(x,0)));a=t2i(r(xn(x,1)));x=xn(t,a);break;
 case 'r':x=num(ln(x));break;}R x;}//reduce prim.
T r(T t){C v;if(!t)R t;switch(t->y){case var:case num:case lam:case cons:R t;
 case tree:r(t->h);t->h=nil;R t->t=r(t->t);case cond:t->h=r(t->h);t->t=r(t->t);R r(rd(t->h,t->t));
 case app:t->h=r(t->h);t->t=r(t->t);R r(ra(t->h,t->t));
 case prim:v=t->h?t->h->v:0;t->h=nil;t->t=r(t->t);R r(rp(v,t->t));}}//reduce tree.
#define PU(w,x){if(!w->h)w->h=x;else w=cons(x,w);}//push.
#define PO(w,x){if(w->h&&!w->t){x=w->h;w->h=nil;}else if(w->h&&w->t){x=w->h;w=w->t;}}//pop.
#define PT (h->r->h->h)
#define Q1 (h->r->h->t->h)
#define Q2 (h->r->h->t->t)
#define BT (h->r->t)
#define isd(c)((c)>='0'&&(c)<='9')
T c(S s){h->r=cons(cons(tree(nil,nil),cons(cons(nil,nil),cons(nil,nil))),nil);T p=PT,r=PT;I m=0,np=0,ns=0;Z i=0;while(s[i++]){C c=s[i-1];if(isd(c)){Z z=2,n=0;S b=al(z);while(isd(c)||c=='.'){b[n++]=c;if(n==z-1){z++;b=re(b,z);}c=s[i++];}b[n]=0;D d=strtod(b,nil);free(b);d=ns?-d:d;if(np)p->t=num(d);else p->h=num(d);i--;continue;}if(c=='{')m=1;if(m){if(c=='}')m=0;continue;}switch(c){
 case 'a'...'z':p->h=var(c);break;
 case 'A'...'Z':p->t=var('a'+(c-'A'));break;
 case '!':PU(Q1,p);p->h=tree(nil,nil);p=p->h;break;
 case '"':PU(Q1,p);p->t=tree(nil,nil);p=p->t;break;
 case '#':PU(Q1,p);p->h=app(nil,nil);p=p->h;break;
 case '$':PU(Q1,p);p->t=app(nil,nil);p=p->t;break;
 case '%':PU(Q1,p);p->h=prim(nil,nil);p=p->h;break;
 case '&':PU(Q1,p);p->t=prim(nil,nil);p=p->t;break;
 case '\'':PU(Q1,p);p->h=lam(nil,nil);p=p->h;break;
 case '(':PU(Q1,p);p->t=lam(nil,nil);p=p->t;break;
 case ')':PU(Q1,p);p->h=cons(nil,nil);p=p->h;break;
 case '*':PU(Q1,p);p->t=cons(nil,nil);p=p->t;break;
 case '+':PU(Q1,p);p->h=cond(nil,nil);p=p->h;break;
 case ',':PU(Q1,p);p->t=cond(nil,nil);p=p->t;break;
 case '-':p->h=nil;break;
 case '^':p->t=nil;break; 
 case '/':p->h=var(0);break;
 case ':':p->t=var(0);break;
 case ';':PO(Q1,p);break;
 case '<':p=r;Q1=cons(nil,nil);break;
 case '=':if(p->h&&p->h->y!=var&&p->h->y!=num)p=p->h;break;
 case '>':if(p->t&&p->t->y!=var&&p->t->y!=num)p=p->t;break;
 case '?':PU(Q2,p);break;
 case '@':PO(Q2,p);break;
 case '[':DP;BT=cp(p->h);DE;break;
 case '\\':np=!np;break;
 case ']':DP;BT=cp(p->t);DE;break;
 case '_':DP;p->h=cp(BT);DE;break;
 case '`':DP;p->t=cp(BT);DE;break;
 case '|':ns=!ns;break;}}R h->r=r;}//compile.
T ld(S s){Z z;FILE*f=fopen(s,"rb");if(!f)e("file");fseek(f,0,SEEK_END);z=ftell(f);rewind(f);
 S b=al(z+1);if(fread(b,1,z,f)!=z)e("file");fclose(f);b[z]=0;T t=c(b);free(b);R t;}//load file.
T ip(){C l[1024];Z z=1,k=0;S b=al(z);while(fgets(l,sizeof(l),stdin)){Z lz=strlen(l);k+=lz;if(k>=z){z+=lz;b=re(b,z);}strcat(b,l);}
 T t=c(b);free(b);R t;}//input.
V ph(I c){puts("usage: trec [-vtsh] [file]");exit(c);}//print help.
V bf(I c){fprintf(stderr,"?%c\n",c);ph(1);}//bad flag.
I main(I k,S*a){I fv,ft,fs;fv=ft=fs=0;S ap=nil;T t;
 for(I i=1;i<k;i++)if(*a[i]=='-'){for(I j=1,c=0;(c=a[i][j]);j++)if(c=='v')fv=!fv;else if(c=='t')ft=!ft;else if(c=='s')fs=!fs;else if(c=='h'||c=='?')ph(0);else bf(c);}else ap=a[i];ih(fv);
 if(ap)t=ld(ap);else t=ip();if(ft){PR("---------\n");prT(t,0);putchar('\n');PR("---------\n");}
 ct();t=r(t);if(fs)prT(t,0);ct();h->r=nil;ct();dh();R 0;}