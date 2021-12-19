using System.Linq;
using System;
using System.Text;
using System.Collections;

namespace AdventOfCode
{
  public partial class Day18 : Day<int>
  {
    public override string Folder => "18";

    public override int Logic(string[] data)
    { 
      tries = 0;
      int i = 0;
      var input = data.Select(x => x.Split(" ")[0]).ToArray();
      SnailNumber number = SnailNumber.Parse(input[i]);
      // var test = SnailNumber.Parse("[[6,[5,[4,[3,2]]]],1]");
      // test.Reduce();
      

      for (i = 1; i < input.Length; i++)
      {
        var nr = SnailNumber.Parse(input[i]);
        var temp = number + nr;
        temp.Reduce();
        number = temp;
      }


      return number.Magnitude();
    }
    public override int Logic2(string[] data)
    {
      tries = 0;

      if (data.Length < 2)
      {
        return SnailNumber.Parse(data[0].Split(" ")[0]).Magnitude();
      }

      var input = data.Select(x => x.Split(" ")[0]).ToArray();

      var magnitudes = new List<int>();

      for (int i = 0; i < input.Length; i++)
      {
        for(int j=0; j < input.Length; j++)
        {
          if (i == j)
          {
            continue;
          }
          var left = SnailNumber.Parse(input[i]);
          var right = SnailNumber.Parse(input[j]);
          var temp = left + right;
          temp.Reduce();
          magnitudes.Add(temp.Magnitude());
        }
      }

      return magnitudes.Max();
    }
  }
}