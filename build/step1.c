
struct A{
   int a;
};

int num1 = 8; ___CPFAssume(UNITS,"@unit(num1) = $m");

int main()
{
   int num2 =10; ___CPFAssume(UNITS,"@unit(num2) = $km");
   struct A aA;  ___CPFAssume(UNITS,"@unit(aA.a) = $km");

   if(num1 < num2){ //error1 here
      num1 += aA.a;  //error2 here
   }
   else{
      num2 += aA.a;
   }
   return 0;
}


