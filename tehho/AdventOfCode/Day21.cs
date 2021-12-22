using System.Linq;
using System;
using System.Text;
using System.Collections;
using GeometRi;

namespace AdventOfCode
{
  public partial class Day21 : Day<long>
  {
    public override string Folder => "21";

    private struct Player
    {
      public int Position { get; set; }
      public int Score { get; set; }

      public static bool operator ==(Player p, Player q)
      {
        return p.Position == q.Position && p.Score == q.Score;
      }

      public static bool operator !=(Player p, Player q)
      {
        return !(p == q);
      }

      public override bool Equals(object? obj)
      {
        if (obj is null)
        {
          return false;
        }

        if (obj is Player)
        {
          return this == (Player)obj;
        }

        return false;
      }

      public override int GetHashCode()
      {
        return Position.GetHashCode() ^ Score.GetHashCode();
      }
    }

    private class DeterministicDie
    {

      public int Value => _value;

      private int _value;

      public DeterministicDie(int value)
      {
          _value = value;
      }

      public int Roll()
      {
        var value = _value;
        _value++;
        return value;
      }
    }

    public override long Logic(string[] data)
    {
      tries = 0;

      var die = new DeterministicDie(1);

      var player1 = new Player();
      player1.Position = int.Parse(data[0].Split(" ")[^1]);
      var player2 = new Player();
      player2.Position = int.Parse(data[1].Split(" ")[^1]);
      var won = false;
      while (!won)
      {
        for (int i = 0; i < 3; i++)
        {
          player1.Position += die.Roll();
        }

        player1.Position = player1.Position % 10;

        player1.Score += player1.Position == 0 ? 10 : player1.Position;

        if (player1.Score >= 1000)
        {
          won = true;
          break;
        }
        for (int i = 0; i < 3; i++)
        {
          player2.Position += die.Roll();
        }

        player2.Position = player2.Position % 10;

        player2.Score += player2.Position == 0 ? 10 : player2.Position;

        if (player2.Score >= 1000)
        {
          won = true;
        }
      }

      return (player1.Score > player2.Score ? player2.Score : player1.Score) * (die.Value - 1);
    }

    public override long Logic2(string[] data)
    {
      tries = 0;

      var dieRolls = new Dictionary<int, int>
      {
        [3] = 1, // 1,1,1
        [4] = 3, // 1,1,2 1,2,1 2,1,1
        [5] = 6, // 1,1,3 1,3,1 3,1,1 2,2,1 2,1,2 1,2,2
        [6] = 7, // 1,2,3 2,1,3 2,3,1 3,2,1 3,1,2 1,3,2 2,2,2
        [7] = 6, // 2,2,3 2,3,2 3,2,2 3,3,1 3,1,3 1,3,3
        [8] = 3, // 2,3,3 3,2,3 3,3,2
        [9] = 1, // 3,3,3

      };

      var wins1 = new long[21];
      var wins2 = new long[21];
      var plays1 = new long[21];
      var plays2 = new long[21];



      var player = new Player();
      player.Position = int.Parse(data[0].Split(" ")[^1]);
      var player1 = new Dictionary<Player, long> { [player] = 1 };

      player = new Player();
      player.Position = int.Parse(data[1].Split(" ")[^1]);
      var player2 = new Dictionary<Player, long>() { [player] = 1 };

      for (int i = 0; i < 21; i++)
      {
        var pool1 = new Dictionary<Player, long>();
        var pool2 = new Dictionary<Player, long>();
        foreach (var die in dieRolls)
        {
          foreach (var p in player1)
          {
            var newPlayer = new Player();
            newPlayer.Position = (p.Key.Position + die.Key) % 10;
            newPlayer.Score = p.Key.Score + (newPlayer.Position == 0 ? 10 : newPlayer.Position);
            long value = p.Value * die.Value;
            if (newPlayer.Score >= 21)
            {
              wins1[i] += value;
            }
            else
            {
              plays1[i] += value;
              if (pool1.ContainsKey(newPlayer))
              {
                pool1[newPlayer] += value;
              }
              else
              {
                pool1[newPlayer] = value;
              } 
            }
          }

          foreach (var p in player2)
          {
            var newPlayer = new Player();
            newPlayer.Position = (p.Key.Position + die.Key) % 10;
            newPlayer.Score = p.Key.Score + (newPlayer.Position == 0 ? 10 : newPlayer.Position);
            long value = p.Value * die.Value;
            if (newPlayer.Score >= 21)
            {
              wins2[i] += value;
            }
            else
            {
              plays2[i] += value;
              if (pool2.ContainsKey(newPlayer))
              {
                pool2[newPlayer] += value;
              }
              else
              {
                pool2[newPlayer] = value;
              }
            }
          }
        }

        player1 = pool1;
        player2 = pool2;
      }

      long totalWins1 = 0;
      long totalWins2 = 0;

      for (int i = 1; i < wins1.Length - 1; i++)
      {
        totalWins1 += wins1[i] * plays2[i - 1];
      }

      for (int i = 0; i < wins2.Length - 1; i++)
      {
        totalWins2 += wins2[i] * plays1[i];
      }

      return Math.Max(totalWins1, totalWins2);
    }
  }
}