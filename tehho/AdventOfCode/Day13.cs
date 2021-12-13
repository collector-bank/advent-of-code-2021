using System.Linq;
using System;

namespace AdventOfCode
{
  public class Day13 : Day<string>
  {
    public override string Folder => "13";

    private struct Point
    {
      public int X;
      public int Y;
    }

    private struct Instruction
    {
      public string Direction;
      public int Coordinate;
    }


    public override string Logic(string[] data)
    {
      tries = 0;

      List<Point> points = new List<Point>();
      List<Instruction> instructions = new List<Instruction>();
      int i = 0;
      for (i = 0; !string.IsNullOrEmpty(data[i]); i++)
      {
        var temp = data[i].Split(',');
        points.Add(new Point() { X = int.Parse(temp[0]), Y = int.Parse(temp[1]) });
      }
      i++;

      for (; i < data.Length; i++)
      {
        var temp = data[i].Split(' ')[2].Split("=");
        var dir = temp[0];
        var coord = int.Parse(temp[1]);

        instructions.Add(new Instruction() { Direction = dir, Coordinate = coord });
      }

      int maxX = points.Max(p => p.X);
      int maxY = points.Max(p => p.Y);

      int[][] grid = new int[maxY + 1][];
      for (int y = 0; y < maxY + 1; y++)
      {
        grid[y] = new int[maxX + 1];
      }

      foreach (var point in points)
      {
        grid[point.Y][point.X] = 1;
      }

      grid = Fold(grid, instructions[0]);

      return grid.Sum(row => row.Sum(cell => cell)).ToString();
    }

    public override string Logic2(string[] data)
    {
      tries = 0; tries = 0;

      List<Point> points = new List<Point>();
      List<Instruction> instructions = new List<Instruction>();
      int i = 0;
      for (i = 0; !string.IsNullOrEmpty(data[i]); i++)
      {
        var temp = data[i].Split(',');
        points.Add(new Point() { X = int.Parse(temp[0]), Y = int.Parse(temp[1]) });
      }
      i++;

      for (; i < data.Length; i++)
      {
        var temp = data[i].Split(' ')[2].Split("=");
        var dir = temp[0];
        var coord = int.Parse(temp[1]);

        instructions.Add(new Instruction() { Direction = dir, Coordinate = coord });
      }

      int maxX = points.Max(p => p.X);
      int maxY = points.Max(p => p.Y);

      int[][] grid = new int[maxY + 1][];
      for (int y = 0; y < maxY + 1; y++)
      {
        grid[y] = new int[maxX + 1];
      }

      foreach (var point in points)
      {
        grid[point.Y][point.X] = 1;
      }

      foreach (var instruction in instructions)
      {
        grid = Fold(grid, instruction);
      }

      return "\n" + string.Join("\n" ,grid.Select(row => string.Join("", row.Select(cell => {
        if (cell == 0)
          return " ";
        else
          return "#";
      }).ToArray())));
    }

    private int[][] Fold(int[][] list, Instruction instruction)
    {
      int[][] newList = null;
      if (instruction.Direction == "x")
      {
        newList = new int[list.Length][];
        for (int i = 0; i < newList.Length; i++)
        {
          newList[i] = new int[instruction.Coordinate];
        }

        for (int y = 0; y < newList.Length; y++)
        {
          for (int x = 0; x < newList[y].Length; x++)
          {
            if (list[y][x] == 1)
            {
              newList[y][x] = 1;
            }
          }
        }

        for (int y = 0; y < list.Length; y++)
        {
          for (int x = instruction.Coordinate + 1; x < list[y].Length; x++)
          {
            if (list[y][x] == 1)
            {
              newList[y][instruction.Coordinate - (x - instruction.Coordinate)] = 1;
            }
          }
        }


      }
      else
      {
        newList = new int[instruction.Coordinate][];

        for (int i = 0; i < newList.Length; i++)
        {
          newList[i] = new int[list[0].Length];
        }
        
        for (int y = 0; y < newList.Length; y++)
        {
          for (int x = 0; x < newList[y].Length; x++)
          {
            if (list[y][x] == 1)
            {
              newList[y][x] = 1;
            }
          }
        }
        
        for (int y = instruction.Coordinate + 1; y < list.Length; y++)
        {
          for (int x = 0; x < list[y].Length; x++)
          {
            if (list[y][x] == 1)
            {
              newList[instruction.Coordinate - (y - instruction.Coordinate)][x] = 1;
            }
          }
        }

      }

      return newList;
    }
  }
}