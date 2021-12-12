using System.Linq;
using System;

namespace AdventOfCode
{
  public class Day10 : Day<long>
  {
    public override string Folder => "10";
    private string openingBrackets = "({<[";
    private string closingBrackets = ")}>]";


    public override long Logic(string[] data)
    {
      tries = 0;
      var stack = new Stack<Char>();
      var list = new List<Char>();
      var lookupTable = new Dictionary<char, int>() {
        {')', 3},
        {']', 57},
        {'}', 1197},
        {'>', 25137}
      };

      foreach (var row in data)
      {
        for (int i = 0; i < row.Length; i++)
        {
          if (openingBrackets.Contains(row[i]))
          {
            stack.Push(row[i]);
          }
          else if (closingBrackets.Contains(row[i]))
          {
            if (stack.Peek() == openingBrackets[closingBrackets.IndexOf(row[i])])
            {
              stack.Pop();
            }
            else
            {
              list.Add(row[i]);
              break;
            }
          }
        }
      }

      return list.Aggregate(0, (acc, c) => acc + lookupTable[c]);
    }

    public override long Logic2(string[] data)
    {
      tries = 0;
      var list = new List<long>();
      var lookupTable = new Dictionary<char, int>() {
        {'(', 1},
        {'[', 2},
        {'{', 3},
        {'<', 4}
      };

      foreach (var row in data)
      {
        var stack = new Stack<Char>();
        bool failed = false;

        for (int i = 0; i < row.Length; i++)
        {
          if (openingBrackets.Contains(row[i]))
          {
            stack.Push(row[i]);
          }
          else if (closingBrackets.Contains(row[i]))
          {
            if (stack.Peek() == openingBrackets[closingBrackets.IndexOf(row[i])])
            {
              stack.Pop();
            }
            else
            {
              failed = true;
              break;
            }
          }
        }

        if (!failed)
        {
          long count = 0;

          foreach (var c in stack)
          {
            count *= 5;
            count += lookupTable[c];
          }

          list.Add(count);
        }
      }

      list.Sort();

      return list[list.Count / 2];
    }
  }
}