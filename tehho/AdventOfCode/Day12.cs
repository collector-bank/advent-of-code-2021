using System.Linq;
using System;

namespace AdventOfCode
{
  public class Day12 : Day<int>
  {
    public override string Folder => "12";

    // create a node class
    private class Node
    {
      public string Name { get; set; }
      public List<Node> children;

      public Node(string name)
      {
        children = new List<Node>();
        Name = name;
      }
    }

    public override int Logic(string[] data)
    {
      tries = 0;

      var nodes = new Dictionary<string, Node>();

      nodes.Add("start", new Node("start"));
      nodes.Add("end", new Node("end"));

      foreach (var line in data)
      {
        var parts = line.Split('-');

        var parent = parts[0];
        var child = parts[1];

        if (!nodes.ContainsKey(parent))
        {
          nodes.Add(parent, new Node(parent));
        }

        if (!nodes.ContainsKey(child))
        {
          nodes.Add(child, new Node(child));
        }

        nodes[parent].children.Add(nodes[child]);
        nodes[child].children.Add(nodes[parent]);
      }

      // find all paths from start to end
      var paths = GetPath(nodes["start"], new List<Node>()).ToList();


      return paths.Count();
    }

    public override int Logic2(string[] data)
    {
      tries = 0;

      var nodes = new Dictionary<string, Node>();

      nodes.Add("start", new Node("start"));
      nodes.Add("end", new Node("end"));

      foreach (var line in data)
      {
        var parts = line.Split('-');

        var parent = parts[0];
        var child = parts[1];

        if (!nodes.ContainsKey(parent))
        {
          nodes.Add(parent, new Node(parent));
        }

        if (!nodes.ContainsKey(child))
        {
          nodes.Add(child, new Node(child));
        }

        nodes[parent].children.Add(nodes[child]);
        nodes[child].children.Add(nodes[parent]);
      }

      // find all paths from start to end
      var paths = GetPath2(nodes["start"], new List<Node>()).ToList();

      var path = paths.Select(p => string.Join(",", p.Select(x => x.Name).ToArray())).ToList();

      return paths.Count();
    }

    private IEnumerable<List<Node>> GetPath(Node node, List<Node> path)
    {
      path.Add(node);
      if (node.Name == "end")
      {
        yield return path;
      }
      else
      {
        foreach (var child in node.children)
        {
          if (child.Name.ToArray().All(x => Char.IsUpper(x)))
          {
            var newPath = GetPath(child, new List<Node>(path));

            if (newPath != null)
            {
              foreach (var p in newPath)
              {
                yield return p;
              }
            }
          }
          else if (!path.Select(n => n.Name).Contains(child.Name))
          {
            var newPath = GetPath(child, new List<Node>(path));

            if (newPath != null)
            {
              foreach (var p in newPath)
              {
                yield return p;
              }
            }
          }
        }
      }
    }

    private IEnumerable<List<Node>> GetPath2(Node node, List<Node> path)
    {
      path.Add(node);
      if (node.Name == "end")
      {
        yield return path;
      }
      else
      {
        foreach (var child in node.children)
        {
          if (child.Name == "start")
          {
            continue;
          }

          if (child.Name.ToArray().All(x => Char.IsUpper(x)))
          {
            var newPath = GetPath2(child, new List<Node>(path));

            if (newPath != null)
            {
              foreach (var p in newPath)
              {
                yield return p;
              }
            }
          }
          else if (!path.Select(n => n.Name).Contains(child.Name))
          {
            var newPath = GetPath2(child, new List<Node>(path));

            if (newPath != null)
            {
              foreach (var p in newPath)
              {
                yield return p;
              }
            }
          }
          else if (!path.Where(x => x.Name.All(c => Char.IsLower(c))).GroupBy(n => n.Name).Any(g => g.Count() > 1))
          {
            var newPath = GetPath2(child, new List<Node>(path));

            if (newPath != null)
            {
              foreach (var p in newPath)
              {
                yield return p;
              }
            }
          }
        }
      }
    }
  }

}