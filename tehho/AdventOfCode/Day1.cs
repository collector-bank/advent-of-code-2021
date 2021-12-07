
namespace AdventOfCode
{
  public class Day1 : Day<int>
  {
    public override string Folder => "1";

    public override int Logic(string[] data)
    {
      tries = 0;
      var list = data
        .Select(x => int.Parse(x))
        .ToList();

      var count = 0;

      for (int i = 0; i < list.Count() - 1; i++)
      {
        tries++;
        if (list[i] < list[i + 1])
        {
          count++;
        }
      }

      return count;
    }

    public override int Logic2(string[] data)
    {
      tries = 0;
      var list = data
        .Select(x => int.Parse(x))
        .ToList();

      var count = 0;

      for (int i = 0; i < list.Count() - 3; i++)
      {
        tries++;
        if (list[i] < list[i + 3])
        {
          count++;
        }
      }

      return count;

      throw new ArgumentOutOfRangeException();
    }
  }
}