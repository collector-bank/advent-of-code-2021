using System.Linq;
using System;
using System.Text;

namespace AdventOfCode
{
  public class Day15 : Day<int>
  {
    public override string Folder => "15";

    private class Node
    {
      public int X;
      public int Y;
      public double DistanceToEnd;
      public List<Connection> Neighbours;
      public Node NearestToStart;
      public int MinCostToStart;
      public bool Visited;
      public int Cost;
    }

    private struct Connection
    {
      public Node From;
      public Node To;
    }

    public override int Logic(string[] data)
    {
      tries = 0;
      var grid = data.Select(x => x.ToCharArray().Select(c => int.Parse(c.ToString())).ToArray()).ToArray();

      var nodes = new List<Node>();

      for (int y = 0; y < grid.Length; y++)
      {
        for (int x = 0; x < grid[y].Length; x++)
        {
          nodes.Add(new Node()
          {
            X = x,
            Y = y,
            DistanceToEnd = Math.Sqrt(((grid[y].Length - (1 + x)) * (grid[y].Length - (1 + x))) + ((grid.Length - (1 + y)) * (grid.Length - (1 + y)))),
            Neighbours = new List<Connection>(),
            MinCostToStart = int.MaxValue,
            Visited = false,
            NearestToStart = null,
            Cost = grid[y][x]
          });
        }
      }

      for (int y = 0; y < grid.Length; y++)
      {
        for (int x = 0; x < grid[y].Length; x++)
        {
          var node = nodes.First(n => n.X == x && n.Y == y);
          Connection connection;
          if (x + 1 < grid[y].Length)
          {
            var node2 = nodes.First(n => n.X == x + 1 && n.Y == y);

            connection = new Connection() { From = node, To = node2};
            node.Neighbours.Add(connection);

            connection = new Connection() { From = node2, To = node};
            node2.Neighbours.Add(connection);
          }

          if (y + 1 < grid.Length)
          {
            var node3 = nodes.First(n => n.X == x && n.Y == y + 1);

            connection = new Connection() { From = node, To = node3};
            node.Neighbours.Add(connection);

            connection = new Connection() { From = node3, To = node};
            node3.Neighbours.Add(connection);
          }
        }
      }

      
      var start = nodes.First(n => n.X == 0 && n.Y == 0);
      start.Cost = 0;
      start.MinCostToStart = 0;
      var end = nodes.First(n => n.X == grid[0].Length - 1 && n.Y == grid.Length - 1);
      var path = new List<Node>();
      path.Add(start);

      do {
        path = path.OrderBy(x => x.MinCostToStart + x.DistanceToEnd).ToList();

        var node = path.First();
        path.Remove(node);

        foreach (var connection in node.Neighbours.Where(c => !c.To.Visited).OrderBy(x => x.To.Cost))
        {
          var childNode = connection.To;
          if (  childNode.MinCostToStart > node.MinCostToStart + childNode.Cost)
          {
            childNode.MinCostToStart = node.MinCostToStart + childNode.Cost;
            childNode.NearestToStart = node;
            if (!path.Contains(childNode))
              path.Add(childNode);
          }
        }
        tries++;
        node.Visited = true;
        if (node == end)
          break;
      }
      while(path.Any());

      var cost = 0;

      while (end != null)
      {
        cost += end.Cost;
        end = end.NearestToStart;
      }

      return cost;
    }

    public override int Logic2(string[] data)
    {
      tries = 0;
      var grid = data.Select(x => x.ToCharArray().Select(c => int.Parse(c.ToString())).ToArray()).ToArray();

      var temp = new int[grid.Length * 5][];

      for (int y = 0; y < grid.Length; y++)
      {
        for (int y1 = 0; y1 < 5; y1++)
        {
          var yPos = y + (grid.Length * y1);
          temp[yPos] = new int[grid[y].Length * 5];
          
          for (int x = 0; x < grid[y].Length; x++)
          {
            var old = grid[y][x];

            for (int x1 = 0; x1 < 5; x1++)
            {
              var value = (old + x1 + y1) % 9;

              if (value == 0)
                value = 9;

              var xPos = x + (grid[y].Length * x1);

              temp[yPos][xPos] = value;
            }
          }
        }
      }

      grid = temp;

      var nodes = new Node[grid.Length][];

      for (int y = 0; y < grid.Length; y++)
      {
        nodes[y] = new Node[grid[y].Length];
        for (int x = 0; x < grid[y].Length; x++)
        {
          nodes[y][x] = new Node()
          {
            X = x,
            Y = y,
            DistanceToEnd = Math.Sqrt(((grid[y].Length - (1 + x)) * (grid[y].Length - (1 + x))) + ((grid.Length - (1 + y)) * (grid.Length - (1 + y)))),
            Neighbours = new List<Connection>(),
            MinCostToStart = int.MaxValue,
            Visited = false,
            NearestToStart = null,
            Cost = grid[y][x]
          };
        }
      }

      for (int y = 0; y < nodes.Length; y++)
      {
        for (int x = 0; x < nodes[y].Length; x++)
        {
          var node = nodes[y][x];
          Connection connection;
          if (x + 1 < grid[y].Length)
          {
            var node2 = nodes[y][x + 1];

            connection = new Connection() { From = node, To = node2};
            node.Neighbours.Add(connection);

            connection = new Connection() { From = node2, To = node};
            node2.Neighbours.Add(connection);
          }

          if (y + 1 < grid.Length)
          {
            var node3 = nodes[y + 1][x];

            connection = new Connection() { From = node, To = node3};
            node.Neighbours.Add(connection);

            connection = new Connection() { From = node3, To = node};
            node3.Neighbours.Add(connection);
          }
        }
      }

      
      var start = nodes[0][0];
      start.Cost = 0;
      start.MinCostToStart = 0;
      var end = nodes[nodes.Length - 1][nodes[0].Length - 1];
      var path = new List<Node>();
      path.Add(start);

      do {
        path = path.OrderBy(x => x.MinCostToStart + x.DistanceToEnd).ToList();

        var node = path.First();
        path.Remove(node);

        foreach (var connection in node.Neighbours.Where(c => !c.To.Visited).OrderBy(x => x.To.Cost))
        {
          var childNode = connection.To;
          if (  childNode.MinCostToStart > node.MinCostToStart + childNode.Cost)
          {
            childNode.MinCostToStart = node.MinCostToStart + childNode.Cost;
            childNode.NearestToStart = node;
            if (!path.Contains(childNode))
              path.Add(childNode);
          }
        }
        tries++;
        node.Visited = true;
        if (node == end)
          break;
      }
      while(path.Any());

      var cost = 0;

      while (end != null)
      {
        cost += end.Cost;
        end = end.NearestToStart;
      }

      return cost;
    }
  }
}