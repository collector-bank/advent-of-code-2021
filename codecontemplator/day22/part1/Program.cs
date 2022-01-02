// See https://aka.ms/new-console-template for more information
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

var lines = File.ReadAllLines("input.txt");
var commands = lines.Select(Command.Parse).ToArray();

var cube = new Dictionary<(int,int,int),bool>();

for(int x=-50; x<=50; x++)
{
    for(int y=-50; y<=50; y++)
    {
        for(int z=-50; z<=50; z++)
        {
            cube[(x,y,z)] = false;
        }
    }
}

foreach(var cmd in commands)
{
    if (cmd.xMin >= -50 && cmd.xMax <= 50 &&
        cmd.yMin >= -50 && cmd.yMax <= 50 && 
        cmd.zMin >= -50 && cmd.zMax <= 50)
    {
        for(int x=cmd.xMin; x<=cmd.xMax; x++)
        {
            for(int y=cmd.yMin; y<=cmd.yMax; y++)
            {
                for(int z=cmd.zMin; z<=cmd.zMax; z++)
                {
                    cube[(x,y,z)] = cmd.on;
                }
            }
        }        
    }
}

var result = 0;
for(int x=-50; x<=50; x++)
{
    for(int y=-50; y<=50; y++)
    {
        for(int z=-50; z<=50; z++)
        {
            if (cube[(x,y,z)]) 
                result++;
        }
    }
}
Console.WriteLine("Num ones: {0}", result);

record Command(bool on, int xMin, int xMax, int yMin, int yMax, int zMin, int zMax)
{
    public static Command Parse(string line)
    {
        string[] split1 = line.Split(' ');
        var on = split1[0] == "on";
        var coordsRaw = split1[1].Split(',').Select(x => x.Substring(2)).ToArray();
        var coords = coordsRaw.Select(x => {
            var parts = x.Replace("..","$").Split("$");
            return (int.Parse(parts[0]), int.Parse(parts[1]));
        }).ToArray();
        return new Command(on, coords[0].Item1, coords[0].Item2, coords[1].Item1, coords[1].Item2, coords[2].Item1, coords[2].Item2);
    }
}