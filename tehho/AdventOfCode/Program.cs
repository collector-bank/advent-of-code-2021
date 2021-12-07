// See https://aka.ms/new-console-template for more information
using AdventOfCode;




var type = typeof(IDay<>);

var interfaces = type.Assembly.GetTypes()
  .SelectMany(t => t.GetInterfaces())
  .Where(t => t.GetGenericTypeDefinition() == type)
  .Distinct()
  .ToList();

// get all classes that impliment generic interface
var types = type.Assembly.GetTypes()
  .Where(p => interfaces.Any(i => i.IsAssignableFrom(p)) && !p.IsInterface && !p.IsAbstract)
  .ToList();

foreach (var genericType in types)
{
  var instance = Activator.CreateInstance(genericType);
  var member = genericType.GetProperty("Folder");
  var day = member!.GetValue(instance);
  string[] data2 = File.ReadAllLines("../data/" + day + "/data.raw");

  // Write to console "Day X" where X is the day number
  Console.WriteLine("Day " + day);

  var method = genericType.GetMethod("Logic");
  var result = method!.Invoke(instance, new object[] { data2 });
  Console.WriteLine($"Sum1: {result}");
  
  method = genericType.GetMethod("Logic2");
  result = method!.Invoke(instance, new object[] { data2 });
  Console.WriteLine($"Sum2: {result}");
}