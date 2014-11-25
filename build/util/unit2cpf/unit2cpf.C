// -*- mode:C++; tab-width:4; c-basic-offset:4; c-default-style:k&r -*-
#include <iostream>
#include <string>
#include <cctype>
#include <cstdlib>
#include <set>

using std::cin;
using std::cout;
using std::endl;
using std::string;
using std::set;

// Parser states
enum State {Normal, AnnMarker, Ann, AfterAnn, ExpMarker, NumSign, Num, AfterNum, 
			DenMarker, Den, AfterDen, InComment1, InComment2, InComment3, InComment4, 
			InStr1, InStr2, InChr1, InChr2};

// The allowable annotations in the program -- others will be dropped
string annotationArray[] = { "$noUnit","$nounit","$any",
			     "$meter","$m","$foot","$feet","$f","$mile","$kilometer","$km","$centimeter","$cm",
			     "$kilogram","$kg","$pound","$lb",
			     "$second","$s",
			     "$ampere","$A",
			     "$kelvin","$K","$Celsius","$C","$Fahrenheit","$F",
			     "$mole","$mol",
			     "$candela","$cd",
			     "$newton","$N" } ;
int annotationCount = 33;

int main(int argc, char* argv[]) {
  
    char c; // current character being processed
    string ann; // current annotation being built
    string annVar; // annotation variable being built
    string thisAnn; // current part of annotation, since could have $a $b
    State state = Normal; // current state of parser
    bool isAnnVar = false; // flag: is current ann a var?
    State priorState = Normal; // prior state: switch back from string, comment processing
	bool dropAnnotation = false; // flag: should we emit the current annotation?

    cin >> std::noskipws; // Don't skip whitespace

    // Pre-load possible annotation words into a set, so we can discard annotations
    // from other policies
    set<string> annotations;
    for (int i = 0; i < annotationCount; ++i)
		annotations.insert(annotationArray[i]);

    //
    // This loop acts as a big state machine. We read characters until we find a '$' character, which
    // starts an annotation. We then read until we finish with the annotation, which gets printed out in
    // a Maude-friendly fashion inside an @cpf attribute.
    //
    while (cin >> c) {
		switch (state) {
			case Normal: // State represents normal program code: see if we are entering an annotation
				if ('$' == c) { // marks start of annotation, set up to read annotation
					state = AnnMarker;
					isAnnVar = false;
					ann = "";
					thisAnn = "";
				} else if ('/' == c) { // marks potential start of comment
					priorState = state;
					state = InComment1;
					cout << c;
				} else if ('"' == c) { // marks start of string
					priorState = state;
					state = InStr1;
					cout << c;
				} else if ('\'' == c) { // marks start of character
					priorState = state;
					state = InChr1;
					cout << c;
				} else { // some other character -- just part of program
					cout << c;
				}
				break;
			
			case InComment1: // State marks potential start of comment: see if we are entering a comment
				if ('/' == c) { // marks start of line comment
					state = InComment2;
					cout << c;
				} else if ('*' == c) { // marks start of block comment
					state = InComment3;
					cout << c;
				} else if ('$' == c) { // marks start of annotation, set up to read annotation
					state = AnnMarker;
					isAnnVar = false;
					ann = "";
					thisAnn = "";
				} else if ('"' == c) { // marks start of string
					state = InStr1;
					cout << c;
				} else if ('\'' == c) { // marks start of character
					state = InChr1;
					cout << c;
				} else { // normal character, go back to prior state
					state = priorState;
					cout << c;
				}
				break;
			
			case InComment2: // Inside line comment, leave when we find the end of line
				if (isspace(c) and not isblank(c)) { 
					state = priorState; // found end of line
				}
				cout << c;
				break;
			
			case InComment3: // Inside block comment -- look for final *
				if ('*' == c) { 
					state = InComment4; // potential end of block comment, look for final /
				}
				cout << c;
				break;
			
			case InComment4: // Inside block comment -- found *, see if followed by /
				if ('/' == c) { // end of block comment
					state = priorState;
				} else if ('*' == c) { // not end, but another *, check next character for end of block comment
					// do nothing
				} else { 
					state = InComment3;
				}
				cout << c;
				break;
			
			case InStr1: // Inside a string -- look for the end of the string
				if ('\\' == c) { // start of escape character
					state = InStr2;
				} else if ('"' == c) { // end of string
					state = priorState;
				}
				cout << c;
				break;
			
			case InStr2: // Handle escaped character
				state = InStr1;
				cout << c;
				break;

			case InChr1: // Inside a character -- look for the closing '
				if ('\\' == c) { // start of excape character
					state = InChr2; 
				} else if ('\'' == c) { // end of character
					state = priorState;
				}
				cout << c;
				break;

			case InChr2: // Handle escaped character
				state = InChr1;
				cout << c;
				break;
		
			case AnnMarker: // Found $ at start of annotation, being processing the rest
				annVar = "";
				thisAnn = "";
				dropAnnotation = false;
				if (isalnum(c)) {
					state = Ann;
					if (isupper(c)) { // Ann vars are line $U, normal anns like $u
						isAnnVar = true;
						annVar += c;
					} else {
						ann += "$";
						ann += c;
						thisAnn += "$";
						thisAnn += c;
					}
				} else {
					// Error condition, expected an annotation but did not find one
					cout << ann << "$ <-- Expected annotation here!" << endl;
					exit(-1);
				}
				break;

			case Ann: // Processing annotation, need to check for end or next part
				if (isalnum(c)) { // Here, annotation continues
					if (isAnnVar) {
						annVar += c;
					} else {
						ann += c;
						thisAnn += c;
					}
				} else { // Found end of textual part of annotation
					if (isAnnVar) { // If processing annotation var, merge into annotation
						ann += "@" + annVar;
					} else if (not (annotations.count(thisAnn) > 0)) { // Check to see if textual annotation is in allowed set
						dropAnnotation = true;
					} 
				
					if (isblank(c)) { // blank -- move to after ann
						state = AfterAnn;
					} else if ('^' == c) { // ^ -- have an exponent
						ann += " ^ ";
						state = ExpMarker;
					} else if ('$' == c) { // $ -- starting another ann keyword
						ann += ' ';
						state = AnnMarker;
					} else { // end of annotation write to output stream
						if (dropAnnotation) {
							cout << c;
						} else {
							cout << "@cpf(" << ann << ") " << c;
						}
						state = Normal;
					}
				}
				break;
				
				
			case AfterAnn: // Finished an annotation keyword, see if there are any modifiers
				if ('$' == c) { // Starting another keyword
					ann += ' ';
					state = AnnMarker;
				} else if ('^' == c) { // Adding an exponent
					ann += " ^ ";
					state = ExpMarker;
				} else if (' ' == c) {
					// Do nothing, still looking for what's next
				} else { // End of annotation
					if (dropAnnotation) {
						cout << c;
					} else {
						cout << "@cpf(" << ann << ") " << c;
					}
					state = Normal;
				}	
				break;
			
			case ExpMarker: // We saw ^, now expecting an exponent, a rational number
				if (isblank(c)) { // Can have a space between the exponent and the start of the number
					// Do nothing
				} else if (isdigit(c)) { // Found the start of the number
					ann += c;
					state = Num;
				} else if (('+' == c) || ('-' == c)) { // Found a sign for a number
					ann += c; 
					state = NumSign;
				} else { // Error condition -- expected a number
					cout << ann << "<-- Expected number here!" << endl;
					exit(-1);
				}
				break;
			
			case NumSign: // Found the number sign, check to find the start of the number
				if (isblank(c)) { // Can have a space between the sign and the number, like + 3/4
					// Do nothing
				} else if (isdigit(c)) { // Found the number
					ann += c;
					state = Num;
				} else { // Error state
					cout << ann << "<-- Expected number here!" << endl;
				}
				break;
			
			case Num: // Read in the number in the exponent; this is the numerator
				if (isdigit(c)) { // Reading the number
					ann += c;
				} else if (isblank(c)) { // Found a blank -- start checking to see if we have a denominator
					state = AfterNum;
				} else if ('/' == c) { // Found the / for the denominator
					state = DenMarker;
					ann += " / ";
				} else if ('$' == c) { // Found the start of another annotation keyword
					ann += ' ';
					state = AnnMarker;
				} else { // End of annotation, emit
					if (dropAnnotation) {
						cout << c;
					} else {
						cout << "@cpf(" << ann << ") " << c;
					}
					state = Normal;
				}
				break;
			
			case AfterNum: // Exponent is rational -- check to see if the denominator follows
				if (isblank(c)) {
					// just keep scanning
				} else if ('/' == c) {
					state = DenMarker;
					ann += " / ";
				} else if ('$' == c) {
					ann += ' ';
					state = AnnMarker;
				} else {
					if (dropAnnotation) {
						cout << c;
					} else {
						cout << "@cpf(" << ann << ") " << c;
					}
					state = Normal;
				}
				break;
			
			case DenMarker: // Start of denominator
				if (isblank(c)) {
					// just keep going
				} else if (isdigit(c)) {
					ann += c;
					state = Den;
				} else {
					cout << ann << "<-- Expected number here!" << endl;
					exit(-1);
				}
				break;

			case Den: // Read denominator
				if (isdigit(c)) {
					ann += c;
				} else if (isblank(c)) {
					state = AfterDen;
				} else if ('$' == c) {
					ann += ' ';
					state = AnnMarker;
				} else {
					if (dropAnnotation) {
						cout << c;
					} else {
						cout << "@cpf(" << ann << ") " << c;
					}
					state = Normal;
				}
				break;

			case AfterDen: // After denominator -- see if another keyword follows
				if (isblank(c)) {
					// just keep scanning
				} else if ('$' == c) {
					ann += ' ';
					state = AnnMarker;
				} else {
					if (dropAnnotation) {
						cout << c;
					} else {
						cout << "@cpf(" << ann << ") " << c;
					}
					state = Normal;
				}
				break;
		}
    }
    
    return 0;
}
