using Dijkstra.NET.Graph;
using Dijkstra.NET.ShortestPath;

var lines = File.ReadAllLines("input.txt");

var nrow = lines.Length;
var ncol = lines[0].Length;

var graph = new Graph<(int, int), string>();
var coordToNode = new Dictionary<(int, int), NodeInfo>();
var nodeToValue = new Dictionary<uint, NodeInfo>();

int Solve(int factor)
{
    for (int j = 0; j < nrow * factor; j++)
    {
        for (int i = 0; i < ncol * factor; i++)
        {
            var jj = j % nrow;
            var ii = i % ncol;
            int vorig = lines[jj][ii] - '0';
            int disty = j / nrow;
            int distx = i / ncol;
            int v = vorig + distx + disty;
            if (v > 9) { v = v % 10 + 1; };
            var id = graph.AddNode((j, i));
            var ni = new NodeInfo(id, v, (j, i));
            coordToNode[(j, i)] = ni;
            nodeToValue[id] = ni;
        }
    }


    for (int j = 0; j < nrow * factor; j++)
    {
        for (int i = 0; i < ncol * factor; i++)
        {
            var n = coordToNode[(j, i)];
            if (i > 1)
            {
                var nn = coordToNode[(j, i - 1)];
                graph.Connect(n.id, nn.id, nn.value, "");
            }

            if (i < ncol * factor - 1)
            {
                var nn = coordToNode[(j, i + 1)];
                graph.Connect(n.id, nn.id, nn.value, "");
            }

            if (j > 1)
            {
                var nn = coordToNode[(j - 1, i)];
                graph.Connect(n.id, nn.id, nn.value, "");
            }

            if (j < nrow * factor - 1)
            {
                var nn = coordToNode[(j + 1, i)];
                graph.Connect(n.id, nn.id, nn.value, "");
            }

            graph.AddNode((j, i));
        }
    }

    var shortestPathResult = graph.Dijkstra(coordToNode[(0, 0)].id, coordToNode[(nrow * factor - 1, ncol * factor - 1)].id);
    var shortestPath = shortestPathResult.GetPath();
    var result = shortestPath.Skip(1).Select(id => nodeToValue[id].value).Sum();
    return result;
}


Console.WriteLine("Part 1: " + Solve(1)); // 527  
Console.WriteLine("Part 2: " + Solve(5)); // 2887

record NodeInfo(uint id, int value, (int, int) coord);

