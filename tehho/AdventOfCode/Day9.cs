using System.Linq;
using System;

namespace AdventOfCode
{
  public class Day9 : Day<int>
  {
    public override string Folder => "9";

    struct Point
    {
      public int X;
      public int Y;
      public Point(int x, int y)
      {
        X = x;
        Y = y;
      }

      public static bool operator ==(Point a, Point b)
      {
        return a.X == b.X && a.Y == b.Y;
      }

      public static bool operator !=(Point a, Point b)
      {
        return !(a == b);
      }
    }

    public override int Logic(string[] data)
    {
      tries = 0;
      var list = data.Select(x => {
        return x.ToArray().Select(c => int.Parse(c.ToString())).ToArray();
      }).ToArray();

      var lowest = new List<int>();

      // check each number in list and see if it is lower then all othogonaly agesand
      for (int i = 0; i < list.Length; i++)
      {
        for (int j = 0; j < list[i].Length; j++)
        {
          var current = list[i][j];

          var neighbours = GetNeighbours(new Point(i,j), list).Where(p => list[p.X][p.Y] <= current);

          if (neighbours.Count() == 0)
          {
            lowest.Add(current);
          }
        }
      }


      return lowest.Count() + lowest.Sum();
    }

    public override int Logic2(string[] data)
    {
      tries = 0;
      var list = data.Select(x => {
        return x.ToArray().Select(c => int.Parse(c.ToString())).ToArray();
      }).ToArray();

      var basins = new List<List<Point>>();

      // check each number in list and see if it is lower then all othogonaly agesand
      for (int i = 0; i < list.Length; i++)
      {
        for (int j = 0; j < list[i].Length; j++)
        {
          var current = list[i][j];
          if (current == 9)
          {
            continue;
          }

          if (basins.Any(x => x.Any(y => y == new Point(i, j))))
          {
            continue;
          }

          var basin = new List<Point>();

          bool done = false;

          var temp = GetNeighbours(new Point(i, j), list).Where(point => list[point.X][point.Y] != 9);
          while (temp.Count() > 0)
          {
            temp = temp.Where(p => basin.Any(x => x == p) == false).ToList();
            basin.AddRange(temp);

            var newTemp = new List<Point>();

            foreach (var point in temp)
            {
              var neighbours = GetNeighbours(point, list).Where(point => list[point.X][point.Y] != 9);
              newTemp.AddRange(neighbours.Where(p => newTemp.Any(x => x == p) == false));
            }
            
            temp = newTemp;
          }
          basins.Add(basin);
        }
      }
      var result = basins.Select(b => b.Count()).ToList();
      result.Sort();

      return result.Take(^3..^0).Aggregate((x, y) => x * y);
    }

    private IEnumerable<Point> GetNeighbours(Point point, int[][] list)
    {
      var neighbours = new List<Point>();

      if (point.X + 1 < list.Length)
      {
        neighbours.Add(new Point(point.X + 1, point.Y));
      }
      if (point.X - 1 >= 0)
      {
        neighbours.Add(new Point(point.X - 1, point.Y));
      }
      if (point.Y + 1 < list[point.X].Length)
      {
        neighbours.Add(new Point(point.X, point.Y + 1));
      }
      if (point.Y - 1 >= 0)
      {
        neighbours.Add(new Point(point.X, point.Y - 1));
      }

      return neighbours;
    }
  }
}