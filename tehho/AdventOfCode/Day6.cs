using System.Linq;
using System;

namespace AdventOfCode
{
  public class Day6 : Day<Int64>
  {
    public override string Folder => "6";

    public override Int64 Logic(string[] data)
    {
      tries = 0;
      
      var dict = data[0]
        .Split(",")
        .Select(int.Parse)
        .GroupBy(x => x)
        .ToDictionary(
          x => x.Key % 6, 
          x => x.Count());

      var array = new int[9];
      var tempArray = new int[8] {0, 0, 0, 0, 0, 0, 0, 0};
      int i = 0;
      for (i = 0; i < 8; i++)
      {
        if (dict.ContainsKey(i))
        {
          array[i] = dict[i];
        }
        else
        {
          array[i] = 0;
        }
      }

      for (i = 0; i < 80; i++)
      {
        var tempHolder = array[0];
        var temp = new int[9];
        
        for (int j = 0; j < temp.Count() - 1; j++)
        {
          temp[j] = array[j + 1];
        }

        temp[6] += tempHolder;
        temp[temp.Count() - 1] = tempHolder;

        array = temp;
      }
      var sum = array.Sum();
      return array.Sum();
    }

    public override Int64 Logic2(string[] data)
    {
      tries = 0;
      
      var dict = data[0]
        .Split(",")
        .Select(int.Parse)
        .GroupBy(x => x)
        .ToDictionary(
          x => x.Key % 6,
          x => x.Count());

      var array = new Int64[9];
      int i = 0;
      for (i = 0; i < 9; i++)
      {
        if (dict.ContainsKey(i))
        {
          array[i] = dict[i];
        }
        else
        {
          array[i] = 0;
        }
      }

      for (i = 0; i < 256; i++)
      {
        var tempHolder = array[0];
        var temp = new Int64[9];
        
        for (int j = 0; j < temp.Count() - 1; j++)
        {
          temp[j] = array[j + 1];
        }

        temp[6] += tempHolder;
        temp[temp.Count() - 1] = tempHolder;

        array = temp;
      }
      var sum = array.Sum();
      return array.Sum();
    }
  }
}