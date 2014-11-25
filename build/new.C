
# 1 "./step1.c"
# 1 "<command-line>"
# 1 "./step1.c"

struct A{
int a;
};


int main()
{
int num1 = 8; ___CPFAssume(UNITS,"@unit(num1) = $m");

int num2 =10; ___CPFAssume(UNITS,"@unit(num2) = $km");
struct A aA; ___CPFAssume(UNITS,"@unit(aA.a) = $km");

if(num1 < num2){
num1 += aA.a;
}
else{
num2 += aA.a;
}
return 0;
}
