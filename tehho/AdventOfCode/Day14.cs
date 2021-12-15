using System.Linq;
using System;
using System.Text;

namespace AdventOfCode
{
  public class Day14 : Day<long>
  {
    public override string Folder => "14";

    public override long Logic(string[] data)
    {
      tries = 0;

      string polymer = data[0];
      var rules = new Dictionary<string, char>();

      for (var i = 2; i < data.Length; i++)
      {
        var rule = data[i].Split(" -> ");
        rules.Add(rule[0], rule[1][0]);
      }

      for (int rounds = 0; rounds < 10; rounds++)
      {
        var sb = new StringBuilder();
        
        for (var i = 0; i < polymer.Length - 1; i++)
        {
          sb.Append(polymer[i]);
          var key = $"{polymer[i]}{polymer[i + 1]}";
          if (rules.ContainsKey(key))
          {
            sb.Append(rules[key]); 
          }
        }
        
        sb.Append(polymer[polymer.Length - 1]);

        polymer = sb.ToString();
      }

      var list = polymer.GroupBy(x => x).Select(x => x.Count()).ToList();

      var max = list.Max();
      var min = list.Min();

      return max - min;
    }

    public override long Logic2(string[] data)
    {
      tries = 0;

      string polymer = data[0];
      var rules = new Dictionary<string, string>();

      for (var i = 2; i < data.Length; i++)
      {
        var rule = data[i].Split(" -> ");
        var key = rule[0];
        var value = rule[1];
        rules.Add(key,value);
      }

      var list = new Dictionary<string, long>();

      for (int i = 0; i < polymer.Length - 1; i++)
      {
        var key = $"{polymer[i]}{polymer[i + 1]}";
        if (list.ContainsKey(key))
        {
          list[key]++;
        }
        else
        {
          list.Add(key, 1);
        }
      }

      for (int rounds = 0; rounds < 40; rounds++)
      {
        var newList = new Dictionary<string, long>();

        foreach (var item in list)
        {
          if (rules.ContainsKey(item.Key))
          {
            var rule = rules[item.Key];
            var left = $"{item.Key[0]}{rule}";
            var right = $"{rule}{item.Key[1]}";

            if (newList.ContainsKey(left))
            {
              newList[left] += item.Value;
            }
            else
            {
              newList.Add(left, item.Value);
            }

            if (newList.ContainsKey(right))
            {
              newList[right] += item.Value;
            }
            else
            {
              newList.Add(right, item.Value);
            }
          }
          else
          {
            newList.Add(item.Key, item.Value);
          }
        }

        list = newList;
      }

      var temp = new Dictionary<Char, long>();

      foreach (var item in list)
      {
          var key = item.Key[0];
          if (temp.ContainsKey(key))
          {
            temp[key] += item.Value;
          }
          else
          {
            temp.Add(key, item.Value);
          }
      }

      temp[polymer[polymer.Length -1]]++;

      var max = temp.Values.Max();
      var min = temp.Values.Min();

      return max - min;
    }
  }
}