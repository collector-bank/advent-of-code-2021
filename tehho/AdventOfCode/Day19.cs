using System.Linq;
using System;
using System.Text;
using System.Collections;
using GeometRi;

namespace AdventOfCode
{
  public partial class Day19 : Day<int>
  {
    public override string Folder => "19";

    private class Scanner
    {
      public List<Vector3d> Beacons { get; set; }
      public Vector3d Position { get; set; }

      public List<List<Vector3d>> AllRotatedBeacons { get; set; }

      public Scanner()
      {
        Beacons = new List<Vector3d>();
        Position = Vector3dExtensions.Zero;
        AllRotatedBeacons = new List<List<Vector3d>>();
      }
    }


    public override int Logic(string[] data)
    {
      tries = 0;

      var list = new List<Scanner>();

      for (int i = 0; i < data.Length; i++)
      {
        var s = new Scanner();

        for (i++; i < data.Length && !string.IsNullOrEmpty(data[i]); i++)
        {
          var temp = data[i].Split(',');
          var x = int.Parse(temp[0]);
          var y = int.Parse(temp[1]);
          var z = int.Parse(temp[2]);

          s.Beacons.Add(new Vector3d(x, y, z));
        }
        list.Add(s);
      }


      list = list.Select(s =>
      {
        var matrixes = Vector3dExtensions.AllDirections
          .SelectMany(direction => Vector3dExtensions
            .AllUpGivenForward(direction)
              .Select(up => new Matrix3d(up.Cross(direction), up, direction)))
              .ToList();
        s.AllRotatedBeacons = matrixes.Select(m => s.Beacons.Select(b => m * b).ToList()).ToList();
        return s;
      }).ToList();

      var foundIndex = new List<int>();

      foundIndex.Add(0);

      var newIndex = new List<int>();
      newIndex.AddRange(foundIndex);

      var stopWatch = new System.Diagnostics.Stopwatch();
      stopWatch.Start();

      while (foundIndex.Count() < list.Count())
      {
        var newFoundIndex = new List<int>();

        List<Task> tasks = new List<Task>();

        foreach (var sourceIndex in newIndex)
        {

          for (int targetIndex = 0; targetIndex < list.Count(); targetIndex++)
          {
            if (foundIndex.Contains(targetIndex))
              continue;


            var task = Task.Run(() =>
            {
              var source = list[sourceIndex];
              var target = list[targetIndex];
              var index = targetIndex;
              bool found = false;
              foreach (var sourceBeacon in source.Beacons)
              {
                foreach (var targetBeacons in target.AllRotatedBeacons)
                {
                  foreach (var targetBeacon in targetBeacons)
                  {
                    var position = sourceBeacon - targetBeacon;

                    var temp = targetBeacons.Select(b => b + position);

                    if (!temp.Contains(sourceBeacon))
                    {
                      continue;
                    }

                    var count = source.Beacons.Count(b => temp.Contains(b));
                    if (count == 12)
                    {
                      target.Beacons = temp.ToList();
                      target.Position = position;

                      found = true;
                      newFoundIndex.Add(index);
                      break;
                    }
                  }
                  if (found)
                    break;
                }
                if (found)
                  break;
              }
            });

            Task.Delay(100).GetAwaiter().GetResult();
            tasks.Add(task);
          }

        }

        Task.WhenAll(tasks.ToArray()).GetAwaiter().GetResult();

        newFoundIndex = newFoundIndex.Distinct().ToList();

        foundIndex.AddRange(newFoundIndex);
        newIndex = newFoundIndex;
      }

      stopWatch.Stop();

      Console.WriteLine(stopWatch.Elapsed);

      var result = list.SelectMany(s => s.Beacons).ToList();

      var grouping = result.GroupBy(p => p).ToList();
      var test = grouping.Select(g => g.Key).ToList();

      return test.Count();
    }

    public override int Logic2(string[] data)
    {
      tries = 0;

      var list = new List<Scanner>();

      for (int i = 0; i < data.Length; i++)
      {
        var s = new Scanner();

        for (i++; i < data.Length && !string.IsNullOrEmpty(data[i]); i++)
        {
          var temp = data[i].Split(',');
          var x = int.Parse(temp[0]);
          var y = int.Parse(temp[1]);
          var z = int.Parse(temp[2]);

          s.Beacons.Add(new Vector3d(x, y, z));
        }
        list.Add(s);
      }

      var foundIndex = new List<int>();

      foundIndex.Add(0);

      var newIndex = new List<int>();
      newIndex.AddRange(foundIndex);

      list = list.Select(s =>
      {
        var matrixes = Vector3dExtensions.AllDirections
          .SelectMany(direction => Vector3dExtensions
            .AllUpGivenForward(direction)
              .Select(up => new Matrix3d(up.Cross(direction), up, direction)))
              .ToList();
        s.AllRotatedBeacons = matrixes.Select(m => s.Beacons.Select(b => m * b).ToList()).ToList();
        return s;
      }).ToList();

      var stopWatch = new System.Diagnostics.Stopwatch();
      stopWatch.Start();

      while (foundIndex.Count() < list.Count())
      {
        var newFoundIndex = new List<int>();

        List<Task> tasks = new List<Task>();

        foreach (var sourceIndex in newIndex)
        {

          for (int targetIndex = 0; targetIndex < list.Count(); targetIndex++)
          {
            if (foundIndex.Contains(targetIndex))
              continue;


            var task = Task.Run(() =>
            {
              var source = list[sourceIndex];
              var target = list[targetIndex];
              var index = targetIndex;
              bool found = false;
              foreach (var sourceBeacon in source.Beacons)
              {
                foreach (var targetBeacons in target.AllRotatedBeacons)
                {
                  foreach (var targetBeacon in targetBeacons)
                  {
                    var position = sourceBeacon - targetBeacon;

                    var temp = targetBeacons.Select(b => b + position);

                    if (!temp.Contains(sourceBeacon))
                    {
                      continue;
                    }

                    var count = source.Beacons.Count(b => temp.Contains(b));
                    if (count == 12)
                    {
                      target.Beacons = temp.ToList();
                      target.Position = position;

                      found = true;
                      newFoundIndex.Add(index);
                      break;
                    }
                  }
                  if (found)
                    break;
                }
                if (found)
                  break;
              }
            });

            Task.Delay(100).GetAwaiter().GetResult();
            tasks.Add(task);
          }

        }

        Task.WhenAll(tasks.ToArray()).GetAwaiter().GetResult();

        newFoundIndex = newFoundIndex.Distinct().ToList();

        foundIndex.AddRange(newFoundIndex);
        newIndex = newFoundIndex;
      }

      stopWatch.Stop();
      Console.WriteLine(stopWatch.Elapsed);

      var distance = list.SelectMany(s => list.Select(t => s.Position - t.Position)).Max(p => (Math.Abs(p.X) + Math.Abs(p.Y) + Math.Abs(p.Z)));

      return (int)distance;
    }
  }
}