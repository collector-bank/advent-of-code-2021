#define SEARCH_MIN

#include <iostream>
#include <vector>
#include <string>
#include <fstream>
#include <string>
#include <sstream>
#include <iterator>

using namespace std;

int main()
{
    vector<vector<string>> program;
    ifstream infile("input.txt");
    string line;
    while (getline(infile, line))
    {
        istringstream iss(line);
        vector<string> tokens;
        copy(istream_iterator<string>(iss), istream_iterator<string>(), back_inserter(tokens));
        program.push_back(tokens);
    }
    
    ofstream outfile("temp.cpp", std::ofstream::trunc);

    outfile << "#include <vector>" << endl;
    outfile << "#include <string>" << endl;
    outfile << "#include <iostream>" << endl;
    outfile << endl;
    outfile << "using namespace std;" << endl;
    outfile << endl;
    int inputPos = 0;
    outfile << "void executeBase(int &x, int& y, int& z, int& w) {" << endl;
    for(auto op : program)
    {
        const auto opName = op[0];
        if (opName == "inp")
        {
            outfile << "}" << endl;
            outfile << endl;
            outfile << "void execute" << inputPos++ <<"(int d, int &x, int& y, int& z, int& w) {" << endl;
            outfile << "  " << op[1] << " = d;" << endl;
        }
        else if (opName == "add")
        {
            outfile << "  " << op[1] << " += " << op[2] << ";" << endl;
        }
        else if (opName == "mul")
        {
            outfile << "  " << op[1] << " *= " << op[2] << ";" << endl;
        }
        else if (opName == "div")
        {
            outfile << "  " << op[1] << " /= " << op[2] << ";" << endl;
        }
        else if (opName == "mod")
        {
            outfile << "  " << op[1] << " %= " << op[2] << ";" << endl;
        }
        else if (opName == "eql")
        {
            outfile << "  " << op[1] << " = " << op[1] << " == " << op[2] << " ? 1 : 0;" << endl;
        }
        else
        {
            throw "unknown op";
        }
    }
    outfile << "}" << endl;
    outfile << endl;
    outfile << "int main() {" << endl;
    string indent = "  ";
    outfile << indent << "int x = 0; int y = 0; int z = 0; int w = 0;" << endl;
    for(int d=0; d < 14; d++) 
    {
#ifdef SEARCH_MIN
        outfile << indent << "for(int d" << d << " = 1; d" << d << " <= 9; ++d" << d << ") { " << endl;
#else        
        outfile << indent << "for(int d" << d << " = 9; d" << d << " > 0; --d" << d << ") { " << endl;
#endif
        indent += "  ";

        const string prev = d == 0 ? "" :  to_string(d-1);
        outfile << indent;
        outfile << "int x" << d << " = x" << prev << "; ";
        outfile << "int y" << d << " = y" << prev << "; ";
        outfile << "int z" << d << " = z" << prev << "; ";
        outfile << "int w" << d << " = w" << prev << ";" << endl;
        outfile << indent << "execute" << d << "(d" << d << ", x" << d << ", y" << d << ", z" << d << ", w" << d << ");" << endl;
        if (d == 4) {
            outfile << indent << "cout << \"digit=\" << " << "d0 << d1 << d2 << d3 << d4 << 0 << 0 << 0 << 0 << 0 << 0 << 0 << 0 << 0 << endl;" << endl;
        }
    }

    outfile << indent << "if (z13 == 0) { " << endl; 
    outfile << indent << "  cout << \"result = \";"  << endl;
    for(int d=0; d < 14; d++)
    {
        outfile << indent << "  cout << d" << d << "; " << endl;
    }
    outfile << indent << "  cout << endl;" << endl;
    outfile << indent << "  return 0;" << endl;
    outfile << indent << "}" << endl;

    indent = indent.substr(0, indent.size() - 2);
    for(int d=14; d>0; d--) 
    {
        outfile << indent << "}" << endl;
        indent = indent.substr(0, indent.size() - 2);
    }

    outfile << "  return 1;" << endl;
    outfile << "}" << endl;
    return 0;
}
