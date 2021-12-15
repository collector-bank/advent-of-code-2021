using Dijkstra.NET.Graph;
using Dijkstra.NET.ShortestPath;


var lines = File.ReadAllLines("input.txt");

var nrow = lines.Length;
var ncol = lines[0].Length;


var graph = new Graph<(int,int), string>();
var coordToNode = new Dictionary<(int, int), NodeInfo>();
var nodeToValue = new Dictionary<uint, NodeInfo>();

for(int j=0; j<nrow; j++)
{
    for(int i=0; i<ncol; i++)
    {
        int v = lines[j][i] - '0';
        var id = graph.AddNode((j,i));
        var ni = new NodeInfo(id, v, (j, i));
        coordToNode[(j, i)] = ni;
        nodeToValue[id] = ni;
    }
}

for (int j = 0; j < nrow; j++)
{
    for (int i = 0; i < ncol; i++)
    {
        var n = coordToNode[(j,i)];
        if (i > 1) 
        {
            var nn = coordToNode[(j, i - 1)];
            graph.Connect(n.id, nn.id, nn.value, "");
        }

        if (i < ncol - 1)
        {
            var nn = coordToNode[(j, i + 1)];
            graph.Connect(n.id, nn.id, nn.value, "");
        }

        if (j > 1)
        {
            var nn = coordToNode[(j - 1, i)];
            graph.Connect(n.id, nn.id, nn.value, "");
        }

        if (j < nrow - 1)
        {
            var nn = coordToNode[(j + 1, i)];
            graph.Connect(n.id, nn.id, nn.value, "");
        }

        graph.AddNode((j, i));  
    }
}

var shortestPathResult = graph.Dijkstra(coordToNode[(0,0)].id, coordToNode[(nrow-1,ncol-1)].id);
var shortestPath = shortestPathResult.GetPath();
var result = shortestPath.Skip(1).Select(id => nodeToValue[id].value).Sum();
Console.WriteLine(result);

record NodeInfo(uint id, int value, (int,int) coord);

