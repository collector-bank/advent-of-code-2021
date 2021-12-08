using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;


(string[] Signatures, string[] Code) ParseLine(string line)
{
    var s = line.Split("|");
    var left = s[0].Trim().Split(' ');
    var right = s[1].Trim().Split(' ');
    return (left, right);
}


bool IsOne(string s) => s.Length == 2;
bool IsFour(string s) => s.Length == 4;
bool IsSeven(string s) => s.Length == 3;
bool IsEight(string s) => s.Length == 7;

//var (signatures, codes) = ParseLine("abcefg cf acdeg acdfg bcdf abdfg abdefg acf abcdefg abcdfg | cdfeb fcadb cdfeb cdbaf");

string SymDiff(string a, string b)
{
    var a1 = new HashSet<char>(a);
    var b1 = new HashSet<char>(b);
    var r1 = a1.Except(b1);
    var r2 = b1.Except(a1);
    return new string(r1.Union(r2).ToArray());
}

string Diff(string a, string b)
{
    var a1 = new HashSet<char>(a);
    var b1 = new HashSet<char>(b);
    return new string(a1.Except(b1).ToArray());
}

string Intersect(string a, string b)
{
    var a1 = new HashSet<char>(a);
    var b1 = new HashSet<char>(b);
    return new string(a1.Intersect(b1).ToArray());
}


string Union(string a, string b)
{
    var a1 = new HashSet<char>(a);
    var b1 = new HashSet<char>(b);
    return new string(a1.Union(b1).ToArray());
}

bool Eq(string a, string b)
{
    var a1 = new HashSet<char>(a);
    var b1 = new HashSet<char>(b);
    return a1.SetEquals(b1);
}

int Decode(string[] signatures, string[] codes)
{
    var one = signatures.First(IsOne);
    var four = signatures.First(IsFour);
    var seven = signatures.First(IsSeven);
    var eight = signatures.First(IsEight);

    var a = SymDiff(seven, one);
    Debug.Assert(a.Length == 1);
    var bd = SymDiff(four, one);
    Debug.Assert(bd.Length == 2);
    var bdcf = Intersect(four, eight);
    Debug.Assert(bdcf.Length == 4);

    var zeroSixOrNine = signatures.Where(x => x.Length == 6).ToArray();
    //zeroSixOrNine = new string[] { "abcefg", "abdefg", "abcdfg" };

    Debug.Assert(zeroSixOrNine.Length == 3);

    var temp = SymDiff(zeroSixOrNine[0], zeroSixOrNine[1]);
    var temp2 = SymDiff(zeroSixOrNine[0], zeroSixOrNine[2]);
    var dce = Union(temp, temp2);
    Debug.Assert(dce.Length == 3);


    var cd = Intersect(bdcf, dce);
    Debug.Assert(cd.Length == 2);

    var cf = Intersect(one, four);
    Debug.Assert(cf.Length == 2);

    var c = Intersect(cd, cf);
    Debug.Assert(c.Length == 1);

    var d = Diff(cd, c);
    Debug.Assert(d.Length == 1);

    var f = Diff(cf, c);
    Debug.Assert(f.Length == 1);

    var b = Diff(bd, d);
    Debug.Assert(b.Length == 1);

    var e = Diff(Diff(dce, d), c);
    Debug.Assert(e.Length == 1);

    var aeg = SymDiff(four, eight);
    var g = Diff(Diff(aeg, a), e);
    Debug.Assert(g.Length == 1);

    var two = signatures.First(x => Eq(a + c + d + e + g, x));
    var three = signatures.First(x => Eq(a + c + d + f + g, x));
    var five = signatures.First(x => Eq(a + b + d + f + g, x));
    var six = signatures.First(x => Eq(a + b + d + e + f + g, x));
    var nine = signatures.First(x => Eq(a + b + c + d + f + g, x));
    var zero = signatures.First(x => Eq(a + b + c + e + f + g, x));


    //Debug.Assert("acedgfb" == eight);
    //Debug.Assert("cdfbe" == five);
    //Debug.Assert("gcdfa" == two);
    //Debug.Assert("fbcad" == three);
    //Debug.Assert("dab" == seven);
    //Debug.Assert("cefabd" == nine);
    //Debug.Assert("cdfgeb" == six);
    //Debug.Assert("eafb" == four);
    //Debug.Assert("cagedb" == zero);
    //Debug.Assert("ab" == one);

    var dict = new List<(string, int)>
    {
       (zero,  0 ),
       ( one, 1  ),
       ( two, 2  ),
       ( three,3 ),
       ( four, 4 ),
       ( five, 5 ),
       ( six, 6  ),
       ( seven, 7),
       ( eight, 8),
       ( nine, 9 ),
    };

    var nums =
        codes.Select(c => dict.First(x => Eq(x.Item1, c)).Item2).ToArray();

    var result = nums[0] * 1000 + nums[1] * 100 + nums[2] * 10 + nums[3];
    return result;
}


//var lines = File.ReadAllLines("input_sample.txt");
var lines = File.ReadAllLines("input.txt");
var data = lines.Select(ParseLine);

// part 1
var count = data.SelectMany(d => d.Code).Where(x => IsOne(x) || IsFour(x) || IsSeven(x) || IsEight(x)).ToList().Count();
Console.WriteLine("count="+count);

// part 2
//var (signatures, codes) = ParseLine("acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf");
var result = 0;
foreach(var d in data)
{
    var x = Decode(d.Signatures, d.Code);
    result += x;
}

Console.WriteLine("sum="+result);
