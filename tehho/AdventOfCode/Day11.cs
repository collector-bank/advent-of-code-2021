using System.Linq;
using System;

namespace AdventOfCode
{
  public class Day11 : Day<int>
  {
    public override string Folder => "11";


    struct Point
    {
      public int X;
      public int Y;
    };

    public override int Logic(string[] data)
    {
      tries = 0;

      // parse data as int
      var input = data.Select(x => x.ToArray().Select(x => int.Parse(x.ToString())).ToArray()).ToArray();
      var count = 0;

      for (int rounds = 1; rounds <= 100; rounds++)
      {
        var list = new List<Point>();

        for (int i = 0; i < input.Length; i++)
        {
          for (int j = 0; j < input[i].Length; j++)
          {
            if (input[i][j] == 9)
            {
              input[i][j] = 0;
              var temp = new List<Point>();
              temp.Add(new Point() { X = i, Y = j });

              while (temp.Count > 0)
              {
                var newTemp = new List<Point>();

                foreach (var p in temp)
                {
                  for (int x = -1; x < 2; x++)
                  {
                    if (p.X + x < 0 || p.X + x >= input.Length)
                      continue;

                    for (int y = -1; y < 2; y++)
                    {
                      if (x == 0 && y == 0)
                        continue;

                      if (p.Y + y < 0 || p.Y + y >= input[p.X + x].Length)
                        continue;

                      if (list.Any(p2 => p2.X == p.X + x && p2.Y == p.Y + y))
                        continue;

                      if (temp.Any(p2 => p2.X == p.X + x && p2.Y == p.Y + y))
                        continue;

                      if (newTemp.Any(p2 => p2.X == p.X + x && p2.Y == p.Y + y))
                        continue;

                      input[p.X + x][p.Y + y] += 1;
                    }
                  }

                  for (int x = -1; x < 2; x++)
                  {
                    if ( p.X + x < 0 || p.X + x >= input.Length )
                      continue;

                    for (int y = -1; y < 2; y++)
                    {
                      if (x == 0 && y == 0)
                        continue;

                      if (p.Y + y < 0 || p.Y + y >= input[p.X + x].Length)
                        continue;

                      if (input[p.X + x][p.Y + y] >= 9)
                      {
                        newTemp.Add(new Point() { X = p.X + x, Y = p.Y + y });
                        input[p.X + x][p.Y + y] = 0;
                      }
                    }
                  }
                }

                list.AddRange(temp);
                temp = newTemp;
              }
            }
          }

        }

        for (int i = 0; i < input.Length; i++)
        {
          for (int j = 0; j < input[i].Length; j++)
          {
            if (list.Any(p => p.X == i && p.Y == j))
              continue;

            input[i][j] += 1;
          }
        }

        count += list.Count();
      }

      return count;
    }

    public override int Logic2(string[] data)
    {
      tries = 0;

      // parse data as int
      var input = data.Select(x => x.ToArray().Select(x => int.Parse(x.ToString())).ToArray()).ToArray();

      var rounds = 0;
      while(input.Select(x => x.Count(x => x == 0)).Sum() != input.Length * input[0].Length)
      {
        var list = new List<Point>();

        for (int i = 0; i < input.Length; i++)
        {
          for (int j = 0; j < input[i].Length; j++)
          {
            if (input[i][j] == 9)
            {
              input[i][j] = 0;
              var temp = new List<Point>();
              temp.Add(new Point() { X = i, Y = j });

              while (temp.Count > 0)
              {
                var newTemp = new List<Point>();

                foreach (var p in temp)
                {
                  for (int x = -1; x < 2; x++)
                  {
                    if (p.X + x < 0 || p.X + x >= input.Length)
                      continue;

                    for (int y = -1; y < 2; y++)
                    {
                      if (x == 0 && y == 0)
                        continue;

                      if (p.Y + y < 0 || p.Y + y >= input[p.X + x].Length)
                        continue;

                      if (list.Any(p2 => p2.X == p.X + x && p2.Y == p.Y + y))
                        continue;

                      if (temp.Any(p2 => p2.X == p.X + x && p2.Y == p.Y + y))
                        continue;

                      if (newTemp.Any(p2 => p2.X == p.X + x && p2.Y == p.Y + y))
                        continue;

                      input[p.X + x][p.Y + y] += 1;
                    }
                  }

                  for (int x = -1; x < 2; x++)
                  {
                    if ( p.X + x < 0 || p.X + x >= input.Length )
                      continue;

                    for (int y = -1; y < 2; y++)
                    {
                      if (x == 0 && y == 0)
                        continue;

                      if (p.Y + y < 0 || p.Y + y >= input[p.X + x].Length)
                        continue;

                      if (input[p.X + x][p.Y + y] >= 9)
                      {
                        newTemp.Add(new Point() { X = p.X + x, Y = p.Y + y });
                        input[p.X + x][p.Y + y] = 0;
                      }
                    }
                  }
                }

                list.AddRange(temp);
                temp = newTemp;
              }
            }
          }

        }

        for (int i = 0; i < input.Length; i++)
        {
          for (int j = 0; j < input[i].Length; j++)
          {
            if (list.Any(p => p.X == i && p.Y == j))
              continue;

            input[i][j] += 1;
          }
        }
        
        rounds++;
      }

      return rounds;
    }
  }

}