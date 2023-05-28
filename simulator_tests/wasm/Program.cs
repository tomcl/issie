using System;
using System.Runtime.InteropServices.JavaScript;

return 0;

public partial class MyClass
{
    [JSExport]
    internal static void Benchmark(string path)
    {
        DotnetTest.benchmarkWithJSONN(ListJSONFilesSync(path), ReadJSONFilesSync(path), ListRAMFilesSync(path), ReadRAMFilesSync(path), 20);
    }

    [JSImport("fs.listJSONFilesSync", "main.mjs")]
    internal static partial string[] ListJSONFilesSync(string path);

    [JSImport("fs.listRAMFilesSync", "main.mjs")]
    internal static partial string[] ListRAMFilesSync(string path);

    [JSImport("fs.readJSONFilesSync", "main.mjs")]
    internal static partial string[] ReadJSONFilesSync(string path);

    [JSImport("fs.readRAMFilesSync", "main.mjs")]
    internal static partial string[] ReadRAMFilesSync(string path);
}

