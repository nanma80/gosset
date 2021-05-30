#!/usr/bin/env wolframscript

rows = Import["external_data/minvects.csv", "Data"];
mvs = Map[StringSplit[#[[1]]]&, rows];
minNormVectors = Map[ToExpression, mvs, 2];
Print[Tally[Map[Length, minNormVectors]]];
Print[Tally[Map[Norm, minNormVectors]]];

dataFileName = "leech_min_norm_vectors.wl";
If[FileExistsQ[dataFileName], DeleteFile[dataFileName]];
Save[dataFileName, minNormVectors];
Print["Saved minNormVectors to " <> dataFileName];
