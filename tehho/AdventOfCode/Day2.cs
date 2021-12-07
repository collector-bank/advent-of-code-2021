
namespace AdventOfCode
{
  public class Day2 : Day<int>
  {
    public override string Folder => "2";

    private struct Node
    {
      public string key;
      public int value;
    }

    public override int Logic(string[] data)
    {
      tries = 0;
      var list = data
        .Select(x =>
        {
          var temp = x.Split(" ");
          var node = new Node();

          node.key = temp[0];
          node.value = int.Parse(temp[1]);

          return node;
        })
        .GroupBy(x => x.key)
        .Select(x => new Node() {key = x.Key, value = x.Sum(y => y.value)})
        .ToDictionary(x => x.key, x => x.value);

      return list["forward"] * (list["down"] - list["up"]);

      // var depth = 0;
      // var length = 0;

      // foreach (var node in list)
      // {
      //   switch (node.key)
      //   {
      //     case "forward":
      //       length += node.value;
      //       break;
      //     case "up":
      //       depth -= node.value;
      //       break;
      //     case "down":
      //       depth += node.value;
      //       break;
      //   }
      // }

      // return depth * length;
    }

    public override int Logic2(string[] data)
    {
      tries = 0;
      var list = data
        .Select(x =>
        {
          var temp = x.Split(" ");
          var node = new Node();

          node.key = temp[0];
          node.value = int.Parse(temp[1]);

          return node;
        })
        .ToList();

      var depth = 0;
      var length = 0;
      var aim = 0;

      foreach (var node in list)
      {
        switch (node.key)
        {
          case "forward":
            length += node.value;
            depth += node.value * aim;
            break;
          case "up":
            aim -= node.value;
            break;
          case "down":
            aim += node.value;
            break;
        }
      }

      return depth * length;
    }
  }
}