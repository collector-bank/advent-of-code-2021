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

        var mapping = new string[10];

        mapping[1] = row.Single(x => x.Length == numbers[1].Length);
        mapping[4] = row.Single(x => x.Length == numbers[4].Length);
        mapping[7] = row.Single(x => x.Length == numbers[7].Length);
        mapping[8] = row.Single(x => x.Length == numbers[8].Length);

        var list2 = row.Where(x => x.Length == 6).ToList();
        foreach (var temp in list2)
        {
          bool isSix = mapping[1].Count(x => temp.Contains(x)) == 1;
          var isNine = mapping[4].Count(x => temp.Contains(x)) == 4;

          if (isSix)
          {
            mapping[6] = temp;
          }
          else if (isNine)
          {
            mapping[9] = temp;
          }
          else 
          {
            mapping[0] = temp;
          }
        }


        list2 = row.Where(x => x.Length == 5).ToList();
        foreach (var temp in list2)
        {
          if (temp.Count(x => mapping[6].Contains(x)) == 5)
          {
            mapping[5] = temp;
          }
          else if (mapping[1].Count(x => temp.Contains(x)) == 2)
          {
            mapping[3] = temp;
          }
          else
          {
            mapping[2] = temp;
          }
        }

        var mappingList = mapping.ToList();

        var number = string.Join("", display.Select(x => {
          return mappingList.IndexOf(x);
        }).ToArray());
        
        return number;
      }).ToList();


      return strs.Select(int.Parse).Sum();
    }
  }
}