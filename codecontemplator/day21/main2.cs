var states = new Dictionary<(long,long,long,long), long>();
var wins = new long[] { 0, 0 };

(long Position,long Score) Get((long,long,long,long) state, int player) 
{
    if (player == 0)
        return (state.Item1, state.Item3);
    else
        return (state.Item2, state.Item4);
}


(long,long,long,long) Make((long,long,long,long) state, int player, (long,long) newValues) 
{
    if (player == 0)
        return (newValues.Item1, state.Item2, newValues.Item2, state.Item4);
    else
        return (state.Item1, newValues.Item1, state.Item3, newValues.Item2);
}

void Step(int player) 
{
    var newStates = new Dictionary<(long,long,long,long), long>();
    for(int d1=1; d1 <= 3; ++d1) 
    {
        for(int d2=1; d2 <= 3; ++d2) 
        {
            for(int d3=1; d3 <= 3; ++d3) 
            {
                var steps = d1 + d2 + d3;   
                foreach(var state in states.Keys)
                {
                     var count = states[state];
                     if (count > 0)
                     {
                         var (pos, score) = Get(state, player);
                         var newPos = pos + steps;
                         if (newPos > 10) 
                         {
                             newPos =  1 + (newPos - 1) % 10;
                         }
                         var newScore = score + newPos;
                         if (newScore >= 21) 
                         {
                             wins[player] += count;
                         } 
                         else
                         {
                             var newState = Make(state, player, (newPos, newScore));
                             if (newStates.ContainsKey(newState)) 
                             {
                                 newStates[newState] += count;
                             }
                             else
                             {
                                 newStates[newState] = count;
                             } 
                         }
                     }
                }   
            }
        }
    }
    states = newStates;
}

void Play(int initPosPlayer1, int initPosPlayer2)
{
    states[(initPosPlayer1, initPosPlayer2, 0, 0)] = 1;
    while(states.Keys.Count  > 0)
    {
        Console.WriteLine(states.Keys.Count);
        Step(0);
        Step(1);
    }
}

Play(5, 10);
Console.WriteLine("{0} {1}", wins[0], wins[1]);
