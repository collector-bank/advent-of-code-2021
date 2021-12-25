using System;
using System.IO;

var lines = File.ReadAllLines("input.txt");
var nRows = lines.Length;
var nCols = lines[0].Length;


var map = new char[nRows, nCols];

for(int j=0; j<nRows; ++j)
{
    for(int i=0; i<nCols; ++i)
    {
        map[j,i] = lines[j][i];
    }
}

void Print()
{
    for(int j=0; j<nRows; ++j)
    {
        for(int i=0; i<nCols; ++i)
        {
            Console.Write(map[j,i]);
        }
        Console.WriteLine();
    }
}

void Init(char[,] newMap)
{
    for(int j=0; j<nRows; ++j)
    {
        for(int i=0; i<nCols; ++i)
        {
            newMap[j,i] = '.';
        }
    }  
}

bool Step() 
{
    var newMap = new char[nRows, nCols];
    Init(newMap);
    bool moved = false;
    for(int j=0; j<nRows; ++j) 
    {
        for(int i=0; i<nCols; ++i) 
        {
            switch(map[j,i])
            {
                case '>': 
                    var i2 = (i + 1) % nCols;
                    if (map[j,i2] == '.') 
                    {
                        newMap[j,i2] = '>';
                        moved = true;
                    }
                    else
                    {
                        newMap[j, i] = '>';
                    }
                    break;
                case 'v':
                    newMap[j, i] = 'v';
                    break;
                case '.':
                    break;
                default:
                    throw new Exception();
            }
        }
    }

    map = newMap;
    newMap = new char[nRows, nCols];
    Init(newMap);
    for(int j=0; j<nRows; ++j) 
    {
        for(int i=0; i<nCols; ++i) 
        {
            switch(map[j,i])
            {
                case '>': 
                    newMap[j, i] = '>';
                    break;
                case 'v':
                    var j2 = (j + 1) % nRows;
                    if (map[j2,i] == '.') 
                    {
                        newMap[j2,i] = 'v';
                        moved = true;
                    }
                    else
                    {
                        newMap[j, i] = 'v';
                    }
                    break;
                case '.':
                    break;
                default:
                    throw new Exception();
            }
        }
    }

    map = newMap;
    return moved;
}

int step = 1;
Console.WriteLine("==== Initial ==== ", step);
Print();

while(Step())
{ 
    Console.WriteLine("==== After {0} ==== ", step);
    //Print();
    step++; 
};

Console.WriteLine(step);

