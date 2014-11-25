CPF-UNITS
=========

check unit errors for C programs utilizing C Policy Framework

build:
------------------
build:
cd build/cil
./configure
make

Usage Example:
-------------------
### test File: test.C
```c
struct A{
   int a;
};

int num1 = 8; //@ assume(UNITS): @unit(num1) = $m

int main()
{
   int num2 =10; //@ assume(UNITS): @unit(num2) = $km
   struct A aA;  //@ assume(UNITS): @unit(aA.a) = $km

   if(num1 < num2){ //error1 here
      num1 += aA.a;  //error2 here
   }
   else{
      num2 += aA.a;
   }
   return 0;
}
```

### test command
```
./cpf-test test.C
```

### test result
```
result StringList: "Function main: ","ERROR on line 14(1): Unit violation detected in less than operation, incompatible units.","ERROR on line 15(1): Unit violation detected in addition operation, incompatible units.",
    "Environments created = 2"
```
