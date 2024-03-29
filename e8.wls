#!/usr/bin/env wolframscript

dataFolder = "data";
refShape = "polytope_3_3_5";
refDataFileName = FileNameJoin[{dataFolder, refShape <> ".wl"}];


exportToPov = True;
lighting = {{"Point", White, {10, -10, 10}}};
rangeFactor = 0.85;
thickness = 0.04;
range = 5;
imageFolder = "output";
imageSize = {4, 3} * 240;

start = AbsoluteTime[];
Needs["POVRayRender`"];
ConfigurePOVRayRender["POVRayPath" -> "C:\\Program Files\\POV-Ray\\v3.7\\bin\\pvengine64.exe"];

getEdgesFromFace[face_] := Table[{face[[i+1]], face[[Mod[i+1, Length[face]] + 1]]}, {i, 0, Length[face] - 1}];
sameVertex[point1_, point2_] := point1 == point2;
sameCenter[edge1_, edge2_] := Total[edge1] == Total[edge2];
getLength[edge_] := Norm[edge[[2]] - edge[[1]]];
sameSet[array1_, array2_] := Length[ Intersection[array1, array2, SameTest -> sameVertex] ] == Length[array1];

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


get421Vertices[altMode_] := Module[
    {vertices},
    vertices = {};
    vertices = Join[vertices, Union[Flatten[Permutations/@Map[Join[#, {0, 0, 0, 0, 0, 0}]& , cubeVertices[2] * 2], 1]]];
    vertices = Join[vertices, demicubeVertices[8, altMode]];
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

    For[i = 1, i <= Length[vertices]-1, i++, 
        For[j = i + 1, j <= Length[vertices], j++, 
            If[ Norm[vertices[[i]] - vertices[[j]]] == edgeLength, AppendTo[edges, {i, j}] ];   
        ];
    ];
    edges
];

getTriangularFaces[vertices_] := Module[
    {edgeLength, i, j, k},
    faces = {};
    edgeLength = 10^10;
    For[i = 2, i < Length[vertices], i++, 
        If[ Norm[vertices[[i]] - vertices[[1]]] < edgeLength, edgeLength = Norm[vertices[[i]] - vertices[[1]]]];
    ];

    For[i = 1, i <= Length[vertices]-2, i++, 
        For[j = i + 1, j <= Length[vertices]-1, j++, 
            If[Norm[vertices[[i]] - vertices[[j]]] == edgeLength,
                For[k = j + 1, k <= Length[vertices], k++,
                    If[Norm[vertices[[i]] - vertices[[k]]] == edgeLength 
                        && Norm[vertices[[j]] - vertices[[k]]] == edgeLength,
                        AppendTo[faces, {i, j, k}];
                    ];
                ];   
            ];
        ];
    ];
    faces
];

phi = (Sqrt[5] + 1)/2;
 
projectionMatrix = {
    {phi, phi, 0, 0, 1, -1, 0, 0},
    {1, -1, phi, phi, 0, 0, 0, 0},
    {0, 0, 1, -1, phi, phi , 0, 0},
    {0, 0, 0, 0, 0, 0, phi+1, phi-1}
};

(* swap coordinates *)
(* projectionMatrix = projectionMatrix[[{2, 1, 3, 4}]]; *)

vertices = get421Vertices[True];
edgeIndices = getEdges[vertices];
(* faces = getTriangularFaces[vertices]; *)

vertices4d = FullSimplify[vertices.Transpose[projectionMatrix]];
edges4d = Map[vertices4d[[#]]&, edgeIndices];

Print["generated E8 data"];

viewPoint = {1, -1, 1}/Sqrt[3];
matrix3D = {viewPoint, {0, 1, 0}, {0, 0, 1}};
perpViewPoints = Orthogonalize[matrix3D][[2;;3]];

originalProjectionMatrixH4 = {
    {0.00, 0, 2 Sin[2 Pi/15], (1 + Sqrt[5]) Sin[Pi/15]},
    {1, -(1 + Sqrt[5]) Sin[Pi/30], 0, 0.00},
    {1, 3, 4, 1} * 0.0001,
    {0, 0, 0, 1} * 0.0001
} * 1.0;

originalProjectionMatrixF4 = {
    {1, 1, Sqrt[3] - 1, 0},
    {1, -1, 0, Sqrt[3] - 1},
    {1, 3, 4, 1} * 0.0001,
    {0, 0, 0, 1} * 0.0001
} * 0.6;


rotation3D = RotationMatrix[{{0, 0, 1}, viewPoint}];
(* projectionMatrix3d = rotation3D.Orthogonalize[originalProjectionMatrixH4][[1;;3]]; *)
(* projectionMatrix3d = rotation3D.originalProjectionMatrixH4[[1;;3]]; *)
projectionMatrix3d = rotation3D.originalProjectionMatrixF4[[1;;3]];


(* projectionMatrix3d = Orthogonalize[originalProjectionMatrixH4][[1;;3]]; *)
(* projectionMatrix3d = RotationMatrix[{{0, 0, 1}, {0, 1, phi}}].IdentityMatrix[4][[1;;3]]; *)
projectTo3d[vertex4d_] := projectionMatrix3d.vertex4d;

graph = Graphics3D[
    {
        {
            FaceForm[White], Map[Cylinder[#, thickness]&, Map[projectTo3d, edges4d, {2}]]
        },
        {
            FaceForm[Red], Map[Sphere[#, thickness * 2.5]&, Map[projectTo3d, vertices4d]]
        }
    },

    ViewPoint -> viewPoint * 3000,
    PlotRange -> {{-range, range}, {-range, range}, {-range, range}},
    Lighting -> lighting
];

outputFolder = "e8_421";
outputFolder = FileNameJoin[{imageFolder, outputFolder}];
If[!DirectoryQ[outputFolder], CreateDirectory[outputFolder]];

(* outputFileName = "e8_421_H4_projection.png"; *)
outputFileName = "e8_421_F4_projection.png";
If[exportToPov, outputFileName = "pov_" <> outputFileName ];
outputFileName = FileNameJoin[{outputFolder, outputFileName}];

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

end = AbsoluteTime[];
Print["Elapsed time in seconds: " <> ToString[end - start]];
