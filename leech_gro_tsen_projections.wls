#!/usr/bin/env wolframscript

dataFolder = "data";
shape = "leech_vertices";
dataFileName = FileNameJoin[{dataFolder, shape <> ".wl"}];

exportToPov = True;
rangeFactor = 0.78;
thicknessRatio = 0.003;
imageSize = {4, 3} * 240;
roundPrecision = 0.000001;
limit = 1000;

start = AbsoluteTime[];
Needs["POVRayRender`"];
ConfigurePOVRayRender["POVRayPath" -> "C:\\Program Files\\POV-Ray\\v3.7\\bin\\pvengine64.exe"];

vertices = Get[dataFileName];
(* viewPoint = {0, 0, 1}; *)
viewPoint = {1, -1, 1}/Sqrt[3];
rotation = RotationMatrix[{{0, 0, 1}, viewPoint}];
lighting = {{"Point", White, viewPoint * 10}};

exportImage[outputFileName_, projectionMatrix_] := Module[
    {verticesOnEdges, verticesTally, cropCommand,
        graph},

    vertices3d = vertices.Transpose[projectionMatrix * 1.0].Transpose[rotation];
    matrixNorm = Norm[projectionMatrix[[1]]];

    maxNorm = Max[Map[Norm, vertices3d]];
    Print[maxNorm//N];
    (* Print[matrixNorm//N]; *)
    (* matrix norm is always 1 *)
    maxNorm = 4.3;

    projectTo3d[vertex_] := vertex/maxNorm;
    range = rangeFactor;

    thickness = thicknessRatio * range;
    
    roundedVertices3d = Round[vertices3d, roundPrecision];
    verticesTally = Tally[Expand[roundedVertices3d]];
    Print["Finished tallying " <> IntegerString[verticesTally//Length]];
    tallyTally = Sort[Tally[Map[#[[2]]&, verticesTally]]];
    Print["Distribution of tally counts:"];
    Print[tallyTally];
    colorMap = <||>;
    colorCount = Length[tallyTally];
    Print[colorCount];
    For[colorIndex = 1, colorIndex <= Length[tallyTally], colorIndex ++,
        multiplicity = tallyTally[[colorIndex]][[1]];
        colorMap[multiplicity] = Hue[Mod[colorIndex - 1 , colorCount]/colorCount, 1, 1 ];
    ];

    graph = Graphics3D[
        Map[{
            Opacity[1], 
            FaceForm[colorMap[#[[2]]]], 
            Sphere[projectTo3d[#[[1]]], thickness * (#[[2]])^(1/4) ]
            }&, verticesTally],
        ViewPoint -> viewPoint * 3000,
        PlotRange -> {{-range, range}, {-range, range}, {-range, range}},
        Lighting -> lighting
    ];

    cropCommand = "mogrify -crop " <> IntegerString[imageSize[[2]]] <> "x" <> IntegerString[imageSize[[2]]] <> "+" <> IntegerString[(imageSize[[1]]-imageSize[[2]])/2] <> "+0 " <> outputFileName;

    If[exportToPov,
        If[FileExistsQ[outputFileName], DeleteFile[outputFileName]];
        POVRayRender[graph, Method -> "Triangle", OutputPath -> outputFileName, ImageSize -> imageSize, RenderOptions -> "+A0.001 -J +UA"];
        Run[cropCommand];
        Print[cropCommand]
        ,
        Export[outputFileName, graph];
    ]
    Print["Saved the image to " <> outputFileName];
];

projections = Get["external_data/leech-projections.wl"];
For[i = 1, i <= limit && i <= Length[projections] , i++,
    projection = projections[[i]];

    name = projection["name"];
    matrix = projection["matrix"];
    projectionMatrix = Join[matrix, {Table[0, {k, 24}]}];

    outputFolder = "output/leech/gro_tsen_projections";
    outputFileName = name <> ".png";
    outputFileName = FileNameJoin[{outputFolder, outputFileName}];

    Print[{i, name}];
    exportImage[outputFileName, projectionMatrix];
];


end = AbsoluteTime[];
Print["Elapsed time in seconds: " <> ToString[end - start]];
