using System.IO;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using AdventOfCode;
using System;
using System.Collections.Generic;
using System.Linq;

namespace adventofcode2021.Test
{
  [TestClass]
  public class TestDay18
  {
    private IDay<int> day;
    private List<string[]> inputs;

    public TestDay18()
    {
      day = new Day18();
      inputs = new List<string[]>();

      var data = File.ReadAllLines(day.Folder + "/data.test");
      inputs.AddRange(data.Select(x => x.Split(' ')));
    }
  
    [TestMethod]
    public void Logic1()
    {
      var _input = inputs.ToList();
      var inData = File.ReadAllLines(day.Folder + "/data.1.1.test");
      _input.Add(inData);
      inData = File.ReadAllLines(day.Folder + "/data.1.2.test");
      _input.Add(inData);
      inData = File.ReadAllLines(day.Folder + "/data.1.3.test");
      _input.Add(inData);

      foreach (var data in _input)
      {
        var expected = int.Parse(data[^1]);
        var result = day.Logic(data[0..^1]);
        Assert.AreEqual(expected, result);
      }
    }

    [TestMethod]
    public void Logic2()
    {
      var _input = inputs.ToList();
      var inData = File.ReadAllLines(day.Folder + "/data.2.1.test");
      _input.Add(inData);

      foreach (var data in _input)
      {
        var expected = int.Parse(data[^1]);
        var result = day.Logic2(data[0..^1]);
        Assert.AreEqual(expected, result);
      }
    }
  }
}