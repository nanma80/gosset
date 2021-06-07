#!/usr/bin/env wolframscript

rows = Import["external_data/e8-projections.csv", "Data"];
projections = {};
For[i = 1, i <= Length[rows]/4 , i++,
    projection = <||>;
    name = rows[[(i - 1) * 4 + 1]][[1]];
    projection["name"] = name;
    matrixString = rows[[{(i - 1) * 4 + 2, (i - 1) * 4 + 3}]];
    matrix = Map[ToExpression, Map[StringSplit[#[[1]]]&, matrixString], 2];
    projection["matrix"] = matrix;
    AppendTo[projections, projection];
];

Print[Length[projections]];

dataFileName = "external_data/e8-projections.wl";
If[FileExistsQ[dataFileName], DeleteFile[dataFileName]];
Save[dataFileName, projections];
Print["Saved projections to " <> dataFileName];

rows = Import["external_data/leech-projections.csv", "Data"];
projections = {};
For[i = 1, i <= Length[rows]/4 , i++,
    projection = <||>;
    name = rows[[(i - 1) * 4 + 1]][[1]];
    projection["name"] = name;
    matrixString = rows[[{(i - 1) * 4 + 2, (i - 1) * 4 + 3}]];
    matrix = Map[ToExpression, Map[StringSplit[#[[1]]]&, matrixString], 2];
    projection["matrix"] = matrix;
    AppendTo[projections, projection];
];

Print[Length[projections]];

dataFileName = "external_data/leech-projections.wl";
If[FileExistsQ[dataFileName], DeleteFile[dataFileName]];
Save[dataFileName, projections];
Print["Saved projections to " <> dataFileName];
