using System.Linq;
using System;
using System.Text;
using System.Collections;
using GeometRi;

namespace AdventOfCode
{
  public partial class Day20 : Day<int>
  {
    public override string Folder => "20";

    public override int Logic(string[] data)
    {
      tries = 0;

      var test = Convert.ToInt16("...#...#.".Replace(".", "0").Replace("#", "1"), 2);

      var algorithm = data[0];

      if (algorithm.Length != 512)
        throw new Exception("Invalid algorithm length");

      var input = data[2..].ToList();
      input = BuildInput(input, 4).ToList();

      List<string> output;

      output = BuildOutput(input, algorithm).ToList();
      output = BuildOutput(output, algorithm).ToList();

      return output.Select(s => s.Count(c => c == '#')).Sum();
    }

    private IEnumerable<string> BuildInput(List<string> input, int count)
    {
      var padding = new string('.', count);
      var output = input.Select(s => $"{padding}{s}{padding}").ToList();

      for (int i = 0; i < count; i++)
      {
        output = output
          .Prepend(new string('.', output[0].Length))
          .Append(new string('.', output[0].Length))
          .ToList();
      }

      return output;
    }

    private IEnumerable<string> BuildOutput(List<string> input, string algorithm)
    {
      var output = new string[input.Count() - 2];

      for (int i = 0; i < input.Count() - 2; i++)
      {
        for (int j = 0; j < input[0].Length - 2; j++)
        {
          var sb = new StringBuilder();
          for (int k = 0; k < 3; k++)
          {
            sb.Append(input[i + k].Substring(j, 3));
          }
          var index = Convert.ToInt16(sb.ToString().Replace(".", "0").Replace("#", "1"), 2);
          output[i] += algorithm[index];
        }
      }

      return output;
    }

    public override int Logic2(string[] data)
    {
      tries = 0;

      var test = Convert.ToInt16("...#...#.".Replace(".", "0").Replace("#", "1"), 2);

      var algorithm = data[0];

      if (algorithm.Length != 512)
        throw new Exception("Invalid algorithm length");

      var input = data[2..].ToList();
      input = BuildInput(input, 100).ToList();

      List<string> output = input;

      for (int i = 0; i < 50; i++)
      {
        output = BuildOutput(output, algorithm).ToList();
      }

      return output.Select(s => s.Count(c => c == '#')).Sum();
    }
  }
}