using System.Linq;
using System;
namespace AdventOfCode
{
  public class Day7 : Day<int>
  {
    public override string Folder => "7";

    public override int Logic(string[] data)
    {
      tries = 0;

      var list = data[0].Split(',').Select(int.Parse).ToList();
      list.Sort();

      var nodes = list.Distinct().ToList();
      
      var distance = list.Select(x => x - nodes[0]).Sum();


      for (int i = 1; i < nodes.Count; i++)
      {
        var distance2 = list.Select(x => Math.Abs(x - nodes[i])).Sum();
        if (distance2 < distance)
        {
          distance = distance2;
        }
        else
        {
          break;
        }
      }

      return distance;
    }

    public override int Logic2(string[] data)
    {
      tries = 0;

      var list = data[0].Split(',').Select(int.Parse).ToList();
      list.Sort();

      var nodes = list.Distinct().ToList();
      
      var distance = list.Select(x => 
        {
          var _distance = Math.Abs(x - nodes[0]);

          if (_distance == 0)
          {
            return 0;
          }
          else if (_distance % 2 == 0)
          {
            var mean = _distance / 2;

            return (_distance + 1) * mean;
          }
          else
          {
            var mean = (_distance / 2) + 1;
            return _distance * mean;
          }
        }).Sum();


      for (int i = 1; i < nodes.Count; i++)
      {
        var distance2 = list.Select(x => 
        {
          var _distance = Math.Abs(x - nodes[i]);

          if (_distance == 0)
          {
            return 0;
          }
          else if (_distance % 2 == 0)
          {
            var mean = _distance / 2;

            return (_distance + 1) * mean;
          }
          else
          {
            var mean = (_distance / 2) + 1;
            return _distance * mean;
          }
        }).Sum();

        if (distance2 < distance)
        {
          distance = distance2;
        }
        else
        {
          break;
        }
      }

      return distance;
    }
  }
}