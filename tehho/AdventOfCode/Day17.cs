using System.Linq;
using System;
using System.Text;
using System.Collections;

namespace AdventOfCode
{
  public class Day17 : Day<int>
  {
    public override string Folder => "17";

    private struct Point
    {
      public int X;
      public int Y;
      public Point(int x, int y)
      {
        X = x;
        Y = y;
      }
      public Point Copy()
      {
        return new Point(X, Y);
      }
    }

    public override int Logic(string[] data)
    {
      tries = 0;
      int highestY = int.MinValue;

      var position = new Point(0, 0);
      var direction = new Point(0, 1);

      var target1 = new Point(0, 0);
      var target2 = new Point(0, 0);

      var temp = data[0].Split(" ");
      var tempx = temp[2].Split("=")[1].Split(",")[0].Split("..").Select(int.Parse).ToArray();
      var tempy = temp[3].Split("=")[1].Split("..").Select(int.Parse).ToArray();

      target1.X = Math.Min(tempx[0], tempx[1]);
      target1.Y = Math.Min(tempy[0], tempy[1]);
      target2.X = Math.Max(tempx[0], tempx[1]);
      target2.Y = Math.Max(tempy[0], tempy[1]);

      

      for (var x = CountFromSumOfAllPrevious(target1.X); x < target2.X + 1; x++)
      {
        for (var y = 0; y < (Math.Abs(target1.Y) + 1); y++)
        {
          var height = SumOfAllPrevious(y);
          var dist = height - target2.Y;
          var count = CountFromSumOfAllPrevious(dist) + 1;
          direction.X = SumOfAllPrevious(x);
          direction.Y = height - SumOfAllPrevious(count);

          if (IsInTarget(direction, target1, target2))
          {
            if(height > highestY)
            {
              highestY = height;
            }
          }
          direction.X = x;
          direction.Y = y;

          height = Fire(position, direction, target1, target2);

          if(height > highestY)
          {
            highestY = height;
          }
        }
      }

      return highestY;
    }

    private int SumOfAllPrevious(int nr)
    {
      return (nr + 1) * nr / 2;
    }

    private int CountFromSumOfAllPrevious(int nr)
    {
      var temp = -0.5 + Math.Sqrt(0.25 + (nr * 2.0));
      return (int)temp;
    }

    private int Fire(Point position, Point direction, Point target1, Point target2)
    {
      var highestY = int.MinValue;

      while(
        !IsBellowTarget(position, target1, target2) && 
        !IsInTarget(position, target1, target2) && 
        !IsPassedTarget(position, target1, target2))
      {
        tries++;
        position.X += direction.X;
        position.Y += direction.Y;

        if (position.Y > highestY)
        {
          highestY = position.Y;
        }

        if (direction.X != 0)
          direction.X += direction.X > 0 ? -1 : 1;
        direction.Y--;
      }

      if (IsInTarget(position, target1, target2))
      {
        return highestY;
      }
      else {
        return int.MinValue;
      }
    }

    private bool IsInTarget(Point position, Point target1, Point target2)
    {
      return position.X >= target1.X && position.X <= target2.X && position.Y >= target1.Y && position.Y <= target2.Y;
    }

    private bool IsBellowTarget(Point position, Point target1, Point target2)
    {
      return position.Y < target1.Y && position.Y < target2.Y;
    }

    private bool IsPassedTarget(Point position, Point target1, Point target2)
    {
      return position.X > target1.X && position.X > target2.X;
    }

    public override int Logic2(string[] data)
    {
      tries = 0;
      int highestY = int.MinValue;

      var position = new Point(0, 0);
      var direction = new Point(0, 1);

      var target1 = new Point(0, 0);
      var target2 = new Point(0, 0);

      var temp = data[0].Split(" ");
      var tempx = temp[2].Split("=")[1].Split(",")[0].Split("..").Select(int.Parse).ToArray();
      var tempy = temp[3].Split("=")[1].Split("..").Select(int.Parse).ToArray();

      target1.X = Math.Min(tempx[0], tempx[1]);
      target1.Y = Math.Min(tempy[0], tempy[1]);
      target2.X = Math.Max(tempx[0], tempx[1]);
      target2.Y = Math.Max(tempy[0], tempy[1]);

      var result = 0;

      for (var x = CountFromSumOfAllPrevious(target1.X); x < target2.X + 1; x++)
      {
        for (var y = target1.Y; y < (Math.Abs(target1.Y) + 1); y++)
        {
          var height = SumOfAllPrevious(y);
          var dist = height - target2.Y;
          var count = CountFromSumOfAllPrevious(dist) + 1;
          direction.X = SumOfAllPrevious(x);
          direction.Y = height - SumOfAllPrevious(count);

          if (IsInTarget(direction, target1, target2))
          {
            if(height > highestY)
            {
              highestY = height;
            }
          }
          direction.X = x;
          direction.Y = y;

          height = Fire(position, direction, target1, target2);

          if(height > int.MinValue)
          {
            result++;
          }
        }
      }

      return result;
    }
  }
}