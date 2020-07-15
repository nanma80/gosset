#!/usr/bin/env wolframscript

dataFolder = "data";
refShape = "polytope_3_3_5";
refDataFileName = FileNameJoin[{dataFolder, refShape <> ".wl"}];


exportToPov = True;
lighting = {{"Point", White, {10, -10, 10}}};
rangeFactor = 0.85;
thickness = 0.13;
range = -1;
imageFolder = "output";
imageSize = {600, 450};
plotFaces = True;
rotationMode = 2;

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
projectionMatrix = projectionMatrix[[{2, 1, 3, 4}]];

vertices = get421Vertices[True];
edgeIndices = getEdges[vertices];
(* faces = getTriangularFaces[vertices]; *)

vertices4d = FullSimplify[vertices.Transpose[projectionMatrix]];
edges4d = Map[vertices4d[[#]]&, edgeIndices];

Print["generated E8 data"];

(*
In[67]:= FullSimplify[Norm/@vertices4d]//Tally
Out[67]= {{2 (1 + Sqrt[5]), 120}, {4, 120}}

In[66]:= FullSimplify[getLength/@edges4d]//Tally
Out[66]= {{2 (1 + Sqrt[5]), 3360}, {4, 3360}}

In[85]:= N[Norm[Total[#]]&/@edges4d]//Tally
Out[85]= {{11.2101, 1200}, {12.3107, 720}, {8.59584, 1440}, {9.98885, 1440}, {6.9282, 1200}, {4.70228, 720}}

*)

refShapes = {
    "polytope_3_3_5",
    "polytope_52_5_3",
    "polytope_3_3_52"
};

For[shapeIndex = 1, shapeIndex <= Length[refShapes], shapeIndex ++,
    refShape = refShapes[[shapeIndex]];

    Print[];
    Print[refShape];

    refDataFileName = FileNameJoin[{dataFolder, refShape <> ".wl"}];

    refCells = Get[refDataFileName];

    refFaces = Flatten[refCells, 1];
    refEdges = Union[ Flatten[Map[getEdgesFromFace, refFaces], 1], SameTest -> sameCenter];
    refVertices = Union[ Flatten[refFaces,1], SameTest -> sameVertex];
    Print["Numbers of vertices and edges are:"];
    Print[Length[refVertices]];
    Print[Length[refEdges]];

(*     FullSimplify[Norm/@refVertices]//Tally;

    FullSimplify[getLength/@refEdges]//Tally;

    N[Norm[Total[#]]&/@refEdges]//Tally;
 *)    (* {{3.80423, 720}} *)

    factors = {2, 2 * phi};

    For[factorIndex = 1, factorIndex <= 2, factorIndex++,
        factor = factors[[factorIndex]];
        Print["factor = " <> ToString[factor]];
        Print["Overlapping vertex count: " <> IntegerString[Intersection[vertices4d, refVertices * factor//Simplify]//Length]];
        Print["Overlapping edge count: " <> IntegerString[Intersection[refEdges * factor//Simplify, edges4d, SameTest -> sameSet]//Length]];
    ];
];




end = AbsoluteTime[];
Print["Elapsed time in seconds: " <> ToString[end - start]];

(* 
No swap

polytope_3_3_5
Numbers of vertices and edges are:
120
720
factor = 2
Overlapping vertex count: 120
Overlapping edge count: 0
factor = 1 + Sqrt[5]
Overlapping vertex count: 120
Overlapping edge count: 720

no swap

polytope_52_5_3
Numbers of vertices and edges are:
120
1200
factor = 2
Overlapping vertex count: 120
Overlapping edge count: 1200
factor = 1 + Sqrt[5]
Overlapping vertex count: 120
Overlapping edge count: 1200

swap

polytope_3_3_52
Numbers of vertices and edges are:
120
720
factor = 2
Overlapping vertex count: 120
Overlapping edge count: 720
factor = 1 + Sqrt[5]
Overlapping vertex count: 120
Overlapping edge count: 0

 *)