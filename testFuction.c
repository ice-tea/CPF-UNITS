
//@ precondition(UNITS): @unit(input1) = $km
//@ precondition(UNITS): @unit(input2) = @unit(input1)
//@ postcondition(UNITS): @unit(@result) = $m
int km2m(int input1, int input2){
   return (input1+input2)*1000;
}

int main()
{
   int num1 =1; //@ assume(UNITS): @unit(num1) = $m
   int num2 =2; //@ assume(UNITS): @unit(num2) = $km
   
   km2m(num2,num2); //right
   km2m(num1,num2); //wrong: input should be km

   num1 += km2m(num2,num2); //right
   num2 = num2 + km2m(num2,num2);  //wrong: result is m, can't add km
   return 0;
}

