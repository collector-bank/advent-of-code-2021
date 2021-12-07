using System.Linq;

namespace AdventOfCode
{
  public class Day4 : Day<int>
  {
    public override string Folder => "4";

    // struct of bingo board
    struct BingoBoard
    {
      public int[][] numbers;
    }

    public override int Logic(string[] data)
    {
      tries = 0;

      var list = data.AsSpan(2).ToArray().Select(x => x.Trim().Replace("  ", " ")).Where(x => !string.IsNullOrWhiteSpace(x)).ToArray();

      var numbers = data[0].Split(',').Select(int.Parse).ToArray();
      var bingoBoards = new List<BingoBoard>();

      for (int i = 0; i < list.Length; i += 5)
      {
        var bingoBoard = new BingoBoard { numbers = new int[5][] };
        
        for (int j = 0; j < 5; j++)
        {
          var temp = list[i + j].Split(' ').Select(int.Parse).ToArray();
          bingoBoard.numbers[j] = temp;
        }

        bingoBoards.Add(bingoBoard);
      }

      foreach (var number in numbers)
      {
          // for each bingo board
          // check if number is in any row or column or diagonal
          foreach (var bingoBoard in bingoBoards)
          {
            if (bingoBoard.numbers.Any(x => x.Contains(number)))
            {
              for (int i = 0; i < 5; i++)
              {
                if (bingoBoard.numbers[i].Contains(number))
                {
                  bingoBoard.numbers[i][bingoBoard.numbers[i].ToList().IndexOf(number)] *= -1;
                } 
              }
            }

            for (int i = 0; i < 5; i++)
            {
              var row = new List<int>();
              var column = new List<int>();
              var diagonal = new List<int>();

              for (int j = 0; j < 5; j++)
              {
                row.Add(bingoBoard.numbers[i][j]);
                column.Add(bingoBoard.numbers[j][i]);
                diagonal.Add(bingoBoard.numbers[j][j]);
              }

              if (row.Where(x => x < 0).Count() == 5 || column.Where(x => x < 0).Count() == 5 || diagonal.Where(x => x < 0).Count() == 5)
              {
                return number * bingoBoard.numbers.SelectMany(x => x).Where(x => x > 0).Sum();
              }
            }

          }
      }
      return -1;
    }

    public override int Logic2(string[] data)
    {
      tries = 0;

      var list = data.AsSpan(2).ToArray().Select(x => x.Trim().Replace("  ", " ")).Where(x => !string.IsNullOrWhiteSpace(x)).ToArray();

      var numbers = data[0].Split(',').Select(int.Parse).ToArray();
      var bingoBoards = new List<BingoBoard>();

      for (int i = 0; i < list.Length; i += 5)
      {
        var bingoBoard = new BingoBoard { numbers = new int[5][] };
        
        for (int j = 0; j < 5; j++)
        {
          var temp = list[i + j].Split(' ').Select(int.Parse).ToArray();
          bingoBoard.numbers[j] = temp;
        }

        bingoBoards.Add(bingoBoard);
      }

      foreach (var number in numbers)
      {
          // for each bingo board
          // check if number is in any row or column or diagonal
          foreach (var bingoBoard in bingoBoards)
          {
            if (bingoBoard.numbers.Any(x => x.Contains(number)))
            {
              for (int i = 0; i < 5; i++)
              {
                if (bingoBoard.numbers[i].Contains(number))
                {
                  bingoBoard.numbers[i][bingoBoard.numbers[i].ToList().IndexOf(number)] *= -1;
                } 
              }
            }
          }
          if (bingoBoards.Count() > 1)
          {
            bingoBoards = bingoBoards.Where(board => {
              for (int i = 0; i < 5; i++)
              {
                var row = new List<int>();
                var column = new List<int>();
                var diagonal = new List<int>();

                for (int j = 0; j < 5; j++)
                {
                  row.Add(board.numbers[i][j]);
                  column.Add(board.numbers[j][i]);
                  diagonal.Add(board.numbers[j][j]);
                }

                if (row.Count(x => x <= 0) == 5 || column.Count(x => x <= 0) == 5 || diagonal.Count(x => x <= 0) == 5)
                {
                  return false;
                }
              }
              
              return true;
            }).ToList();
          }
          else
          {
            for (int i = 0; i < 5; i++)
            {
              var row = new List<int>();
              var column = new List<int>();
              var diagonal = new List<int>();

              for (int j = 0; j < 5; j++)
              {
                row.Add(bingoBoards[0].numbers[i][j]);
                column.Add(bingoBoards[0].numbers[j][i]);
                diagonal.Add(bingoBoards[0].numbers[j][j]);
              }

              if (row.Count(x => x <= 0) == 5 || column.Count(x => x <= 0) == 5 || diagonal.Count(x => x <= 0) == 5)
              {
                return number * bingoBoards[0].numbers.SelectMany(x => x).Where(x => x > 0).Sum();
              }
            }

          }
      }
      return -1;
    }
  }
}