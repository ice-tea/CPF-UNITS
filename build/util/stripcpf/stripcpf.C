#include <iostream>
#include <string>
#include <cctype>

using std::cin;
using std::cout;
using std::endl;
using std::string;

enum Ann {Normal, At, AtC, AtCP, AtCPF, AtCPFParen, InCPF};

int main(int argc, char* argv[]) {
  
  char c;
  Ann state = Normal;
  string annotation;

  cin >> std::noskipws; // Don't skip whitespace

  while (cin >> c) {
    switch (state) {
    case Normal:
      if ('@' == c)
	state = At;
      else
	cout << c;
      break;

    case At:
      if ('c' == c)
	state = AtC;
      else {
	state = Normal;
	cout << "@" << c;
      }
      break;

    case AtC:
      if ('p' == c)
	state = AtCP;
      else {
	state = Normal;
	cout << "@c" << c;
      }
      break;

    case AtCP:
      if ('f' == c)
	state = AtCPF;
      else {
	state = Normal;
	cout << "@cp" << c;
      }
      break;

    case AtCPF:
      if ('(' == c)
	state = AtCPFParen;
      else {
	state = Normal;
	cout << "@cpf" << c;
      }
      break;
      
    case AtCPFParen:
      if ('"' == c) {
	state = InCPF;
	annotation = "";
      } else {
	state = Normal;
	cout << "@cpf(" << c;
      }
      break;
      
    case InCPF:
      if ('"' == c) {
	state = Normal;
	cout << "@cpf(" << annotation ; // ) should be next, don't put it in here
      } else {
	annotation += c;
      }
      break;

    }
  }

  return 0;
}
