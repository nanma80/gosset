#!/usr/bin/env wolframscript

phi = (Sqrt[5] + 1)/2;
edges3d = Get["data/leech_3d_edges.wl"];
vertices = Union[Flatten[edges3d, 1]];
getMap[list_] := Association[Table[{list[[k]] -> k}, {k, 1, Length[list]}]];
toGoldenField[value_] := Module[
    {sqrt5List, paddedList},
    sqrt5List = CoefficientList[value, Sqrt[5]];
    paddedList = If[Length[sqrt5List] == 0,
        {0, 0},
        If[Length[sqrt5List] == 1,
            Join[sqrt5List, {0}],
            sqrt5List
        ]
    ];
    {paddedList[[1]] - paddedList[[2]], 2 paddedList[[2]]}
];

verticesMap = getMap[vertices];
edgeIndices = Map[Sort[Map[verticesMap, #]]&, edges3d];
verticesGoldenField = Map[toGoldenField, Expand[vertices * phi^4], {2}];
exportData = <|
        "vertices" -> verticesGoldenField,
        "edges" -> edgeIndices - 1,
        "faces" -> {}
    |>;

outputFileName = "data/leech_3d_vertices_edges.json";
If[FileExistsQ[outputFileName], DeleteFile[outputFileName] ];
Export[outputFileName, exportData, "RawJSON", Compact -> True];
Print["saved json data to " <> outputFileName];
