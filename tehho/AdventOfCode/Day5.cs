using System.Linq;
using System;

namespace AdventOfCode
{
  public class Day5 : Day<int>
  {
    public override string Folder => "5";

    // struct of bingo board
    struct Point
    {
      public int x;
      public int y;
      
      public Point(int x, int y)
      {
        this.x = x;
        this.y = y;
      }

      public static bool operator ==(Point a, Point b) => a.x == b.x && a.y == b.y;

      public static bool operator !=(Point a, Point b) => !(a == b);

      public override bool Equals(object? obj) => obj is not null && obj is Point && this == (Point)obj;

      public override int GetHashCode() => x.GetHashCode() ^ y.GetHashCode();
    }

    struct Line
    {
      public Point start;
      public Point end;
      public Point Normal()
      {
        int x = end.x - start.x;
        int y = end.y - start.y;
        return new Point() { x = x == 0 ? 0 : x < 0 ? -1 : 1, y = y == 0 ? 0 : y < 0 ? -1 : 1 };
      }

      public double length()
      {
        return Math.Sqrt(Math.Pow(end.x - start.x, 2) + Math.Pow(end.y - start.y, 2));
      }
    }

    public override int Logic(string[] data)
    {
      tries = 0;

      var lines = data.Select(x => {
        var line = x.Split("->");
        return new Line {
          start = new Point {
            x = int.Parse(line[0].Split(",")[0]),
            y = int.Parse(line[0].Split(",")[1])
          },
          end = new Point {
            x = int.Parse(line[1].Split(",")[0]),
            y = int.Parse(line[1].Split(",")[1])
          }
        };
      }).ToList();

      var max = lines.Max(x => Math.Max(Math.Max(x.start.x, x.start.y), Math.Max(x.end.x, x.end.y))) + 1;

      int[][] board = new int[max][];
      for (int i = 0; i < max; i++)
      {
          board[i] = new int[max];
          for (int j = 0; j < max; j++)
          {
            board[i][j] = 0;
          }
      }

      lines = lines.Where(x => x.start.x == x.end.x || x.start.y == x.end.y).ToList();

      foreach (var l in lines)
      {
        var n = l.Normal();
        var line = new Line {
          start = new Point {
            x = l.end.x,
            y = l.end.y
          },
          end = new Point {
            x = l.start.x,
            y = l.start.y
          }
        };
        while (n != line.Normal())
        {
          board[line.end.x][line.end.y] += 1;
          line.end.x += n.x;
          line.end.y += n.y;
        }
      }

      return board.SelectMany(x => x).Count(x => x > 1);
    }

    public override int Logic2(string[] data)
    {
      tries = 0;

      var lines = data.Select(x => {
        var line = x.Split("->");
        return new Line {
          start = new Point {
            x = int.Parse(line[0].Split(",")[0]),
            y = int.Parse(line[0].Split(",")[1])
          },
          end = new Point {
            x = int.Parse(line[1].Split(",")[0]),
            y = int.Parse(line[1].Split(",")[1])
          }
        };
      }).ToList();

      var max = lines.Max(x => Math.Max(Math.Max(x.start.x, x.start.y), Math.Max(x.end.x, x.end.y))) + 1;

      int[][] board = new int[max][];
      for (int i = 0; i < max; i++)
      {
          board[i] = new int[max];
          for (int j = 0; j < max; j++)
          {
            board[i][j] = 0;
          }
      }

      foreach (var l in lines)
      {
        var n = l.Normal();
        var line = new Line {
          start = new Point {
            x = l.end.x,
            y = l.end.y
          },
          end = new Point {
            x = l.start.x,
            y = l.start.y
          }
        };
        while (n != line.Normal())
        {
          board[line.end.x][line.end.y] += 1;
          line.end.x += n.x;
          line.end.y += n.y;
        }
      }

      return board.SelectMany(x => x).Count(x => x > 1);
    }
  }
}