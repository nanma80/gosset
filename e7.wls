#!/usr/bin/env wolframscript

exportToPov = True;
lighting = {{"Point", White, {10, -10, 10}}};
rangeFactor = 0.9;
thickness = 0.15;

start = AbsoluteTime[];
Needs["POVRayRender`"];
ConfigurePOVRayRender["POVRayPath" -> "C:\\Program Files\\POV-Ray\\v3.7\\bin\\pvengine64.exe"];

cubeVertices[dimension_] := 2 * Map[IntegerDigits[#, 2][[2;;dimension+1]]&, Table[k, {k, 2^dimension, 2 * 2^dimension - 1}]] - 1;
orthoplexVertices[dimension_] := Module[
    {newVertex, i},
    vertices = {};
    For[i = 1, i <= dimension, i++,
        newVertex = ConstantArray[0, dimension];
        newVertex[[i]] = 1;
        AppendTo[vertices, newVertex];

        newVertex = ConstantArray[0, dimension];
        newVertex[[i]] = -1;
        AppendTo[vertices, newVertex];
    ];
    vertices
];
demicubeVertices[dimension_, altMode_] := Select[cubeVertices[dimension], Mod[Total[#] + 8, 4] == Mod[If[altMode, 0, 2] + dimension, 4] &];

get321Vertices[altMode_] := Module[
    {},
    vertices = {};
    vertices = Join[vertices, Map[Join[# * 2, {Sqrt[2]}]& , orthoplexVertices[6]]];
    vertices = Join[vertices, Map[Join[# * 2, {-Sqrt[2]}]& , orthoplexVertices[6]]];
    vertices = Join[vertices, Map[Join[#, {0}]& , demicubeVertices[6, altMode]]];
    vertices
];

projectToDirection[vertex_, projectionMatrix_] := projectionMatrix.vertex;
getEdges[vertices_] := Module[
    {edgeLength, i, j},
    edges = {};
    edgeLength = 10^10;
    For[i = 2, i < Length[vertices], i++, 
        If[ Norm[vertices[[i]] - vertices[[1]]] < edgeLength, edgeLength = Norm[vertices[[i]] - vertices[[1]]]];
    ];

    Print["edge length:"];
    Print[edgeLength];

    For[i = 1, i <= Length[vertices]-1, i++, 
        For[j = i + 1, j <= Length[vertices], j++, 
            If[ Norm[vertices[[i]] - vertices[[j]]] == edgeLength, AppendTo[edges, {i, j}] ];   
        ];
    ];
    edges
];

(* vertices = cubeVertices[3]; *)
(* vertices = demicubeVertices[6, True]; *)
(* vertices = orthoplexVertices[6]; *)
vertices = get321Vertices[True];

phi = GoldenRatio;

edges = getEdges[vertices];

Print["vertex count: " <> IntegerString[Length[vertices]] <> " ; edge count: " <> IntegerString[Length[edges]]] ;

theta = Pi/6;

projectionMatrix1 = {
    {phi, phi, 0, 0, -1, 1},
    {1, -1, phi, -phi, 0, 0},
    {0, 0, 1, 1, phi, phi }
};

projectionMatrix2 = {
    {-phi, phi, 0, 0, -1, 1},
    {-1, -1, phi, -phi, 0, 0},
    {0, 0, 1,  1, phi, phi }
};


projectionMatrixParameter[theta_] := {
    {phi Cos[theta] - Sin[theta], phi, 0, 0, -1, 1},
    {Cos[theta] + phi Sin[theta], -1, phi, -phi, 0, 0},
    {0, 0, 1,  1, phi, phi }
};

(* projectionMatrix = IdentityMatrix[3]; *)

(* projectionMatrix = projectionMatrix2; *)
projectionMatrix = projectionMatrixParameter[0];
(* projectionMatrix = projectionMatrix1; *)
Print[projectionMatrix];

If[Length[vertices[[1]]] > Length[projectionMatrix[[1]]],
    projectionMatrix = Map[Join[#, Table[0, {k, 1, Length[vertices[[1]]] - Length[projectionMatrix[[1]]]}]]&, projectionMatrix]];

displayVertices = Map[projectToDirection[#, projectionMatrix]&, vertices];
displayEdges = Map[displayVertices[[#]]&, edges, {2}];

range = Max[Map[N[Norm[#]]&, displayVertices]] * rangeFactor;

graph = Graphics3D[
    {
        {
            Opacity[1], FaceForm[LightBlue], Map[Cylinder[#, thickness]&, displayEdges]
        },
        {
            Opacity[1], FaceForm[Blue], Map[Sphere[#, thickness * 2]&, displayVertices]
        }
    },
    ViewPoint -> {2, -5, 1}*30,
    (* ViewPoint -> {phi, -1, 0} * 1000, *)
    PlotRange -> {{-range, range}, {-range, range}, {-range, range}},
    Lighting -> lighting
];

outputFolder = "output/";
outputFileName = outputFolder <> "test.png";

imageSize = {600, 450};
    cropCommand = "mogrify -crop " <> IntegerString[imageSize[[2]]] <> "x" <> IntegerString[imageSize[[2]]] <> "+" <> IntegerString[(imageSize[[1]]-imageSize[[2]])/2] <> "+0 " <> outputFileName;

If[exportToPov,
    If[FileExistsQ[outputFileName], DeleteFile[outputFileName]];
    POVRayRender[graph, Method -> "Triangle", OutputPath -> outputFileName, ImageSize -> imageSize, RenderOptions -> "+A0.001 -J +UA"];
    Run[cropCommand];
    ,
    Export[outputFileName, graph];
];
Print["Saved the image to " <> outputFileName];


end = AbsoluteTime[];
Print["Elapsed time in seconds"];
Print[end - start];
