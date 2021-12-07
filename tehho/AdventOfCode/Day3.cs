
namespace AdventOfCode
{
  public class Day3 : Day<int>
  {
    public override string Folder => "3";

    public override int Logic(string[] data)
    {
      tries = 0;
      var gamma = 0;
      var epsilon = 0;

      // for each value in data check if position i is 0 or 1
      // if 0 then add 1 to gamma
      // if 1 then add 1 to epsilon

      for (var i = 0; i < data[0].Length; i++)
      {
        var one = 0;
        var zero = 0;

        foreach (var row in data)
        {
          if (row[i] == '0')
          {
            zero++;
          }
          else
          {
            one++;
          }
        }
        if (one > zero)
        {
          // bitwise add one to gamma at position i
          gamma = gamma | (1 << data[0].Length - (i + 1));
        }
        else
        {
          // bitwise add one to epsilon at position i
          epsilon = epsilon | (1 << data[0].Length - (i + 1));
        }
      }

      return gamma * epsilon;
    }

    public override int Logic2(string[] data)
    {
      tries = 0;
      var oxygen = 0;
      var co2 = 0;
      List<string> temp = data.ToList();

      for (var i = 0; i < data[0].Length; i++)
      {
        var one = 0;
        var zero = 0;

        foreach (var row in temp)
        {
          if (row[i] == '0')
          {
            zero++;
          }
          else
          {
            one++;
          }
        }
        temp = temp.Where(x => x[i] == (one >= zero ? '1' : '0')).ToList();

        if (temp.Count() == 1)
        {
          break;
        }
      }

      var str = temp.ElementAt(0);
      for (int i = 0; i < str.Length; i++)
      {
        if (str[i] == '1')
        {
          oxygen = oxygen | (1 << str.Length - (i + 1));
        }
      }

      temp = data.ToList();

      for (var i = 0; i < data[0].Length; i++)
      {
        var one = 0;
        var zero = 0;

        foreach (var row in temp)
        {
          if (row[i] == '0')
          {
            zero++;
          }
          else
          {
            one++;
          }
        }
        temp = temp.Where(x => x[i] == (one < zero ? '1' : '0')).ToList();

        if (temp.Count() == 1)
        {
          break;
        }
      }

      str = temp.ElementAt(0);
      for (int i = 0; i < str.Length; i++)
      {
        if (str[i] == '1')
        {
          co2 = co2 | (1 << str.Length - (i + 1));
        }
      }
      
      return oxygen * co2;
    }
  }
}