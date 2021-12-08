using System.Linq;
using System;

namespace AdventOfCode
{
  public class Day8 : Day<int>
  {
    public override string Folder => "8";
    
    // cagedb: 0
    // ab: 1
    // gcdfa: 2
    // fbcad: 3
    // eafb: 4
    // cdfbe: 5
    // cdfgeb: 6
    // dab: 7
    // acedgfb: 8
    // cefabd: 9

    private string[] numbers;

    public Day8()
    {
      numbers = new string[] {
        "abcefg",
        "cf",
        "acdeg",
        "acdfg",
        "bcdf",
        "abdfg",
        "abdefg",
        "acf",
        "abcdefg",
        "abcdfg"
        }.Select(x => {
          var ret = x.ToList();
          ret.Sort();
          return string.Join("", ret);
        }).ToArray();
    }

    public override int Logic(string[] data)
    {
      tries = 0;

      int count = 0;

      for (int i = 0; i < data.Count(); i++)
      {
        var list = data[i].Split("|").Select(x => x.Split().Where(s => !string.IsNullOrWhiteSpace(s)).ToList()).ToList();

        var row = list[0];
        var display = list[1];

        var selectedNumbers = new string[]{
          // numbers[0],
          numbers[1],
          numbers[4],
          numbers[7],
          numbers[8]
        }.Select(x => x.Length).ToList();
        
        count += display.Where(x => selectedNumbers.Contains(x.Length)).Count();
      }

      return count;
    }

    public override int Logic2(string[] data)
    {
      tries = 0;

      var strs = data.Select(x => 
      {
        var list = x.Split("|").Select(x => x.Split().Where(s => !string.IsNullOrWhiteSpace(s)).ToList()).ToList();

        var row = list[0].Select(x => {
          var s = x.ToList();
          s.Sort();
          return string.Join("", s.ToArray());
        }).ToList();
        
        var display = list[1].Select(x => {
          var s = x.ToList();
          s.Sort();
          return string.Join("", s.ToArray());
        }).ToList();

        var mapping = new Dictionary<string, int>();

        mapping.Add(row.Single(x => x.Length == numbers[1].Length), 1);
        mapping.Add(row.Single(x => x.Length == numbers[4].Length), 4);
        mapping.Add(row.Single(x => x.Length == numbers[7].Length), 7);
        mapping.Add(row.Single(x => x.Length == numbers[8].Length), 8);

        foreach (var temp in row.Where(x => x.Length == 6))
        {
          if (mapping.Where(x => x.Value == 1).SelectMany(x => x.Key.Split("")).Count(x => temp.Contains(x)) == 1)
          {
            mapping.Add(temp, 6);
          }
          else if (mapping.Where(x => x.Value == 4).SelectMany(x => x.Key.Split("")).Count(x => temp.Contains(x)) == 4)
          {
            mapping.Add(temp, 9);
          }
          else 
          {
            mapping.Add(temp, 0);
          }
        }

        foreach (var temp in row.Where(x => x.Length == 5))
        {
          if (temp.Split("").Count(x => mapping.Single(x => x.Value == 6).Key.Contains(x)) == 5)
          {
            mapping.Add(temp, 5);
          }
          else if (mapping.Where(x => x.Value == 1).SelectMany(x => x.Key.Split("")).Count(x => temp.Contains(x)) == 2)
          {
            mapping.Add(temp, 3);
          }
          else
          {
            mapping.Add(temp, 2);
          }
        }

        var number = string.Join("", display.Select(x => {
          return mapping[x].ToString();
        }).ToArray());
        
        return number;
      }).ToList();


      return strs.Select(int.Parse).Sum();
    }
  }
}