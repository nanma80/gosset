#!/usr/bin/env wolframscript

(* https://www.physast.uga.edu/~mgeller/JPA34p10243.pdf *)

imageFolder = "output";
imageSize = 240 * {4, 3};
exportToPov = True;
showVertices = True;
showEdges = True;
showFaces = False;
tryPerms = True;
roundPrecision = 0.000001;

Needs["Quaternions`"];
Needs["POVRayRender`"];
ConfigurePOVRayRender["POVRayPath" -> "C:\\Program Files\\POV-Ray\\v3.7\\bin\\pvengine64.exe"];

colors = {Red, Blue, Green, Yellow, Magenta, Cyan, Brown, Orange, Pink, Purple};
reflect[point_, mirror_] := point - 2 * (point.mirror)/(mirror.mirror)*mirror;
cubeVertices[dimension_] := 2 * Map[IntegerDigits[#, 2][[2;;dimension+1]]&, Table[k, {k, 2^dimension, 2 * 2^dimension - 1}]] - 1;

orthoplexVertices[dimension_] := Module[
    {newVertex, i, vertices},
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

get24CellVertices[] := Join[orthoplexVertices[4], cubeVertices[4]/2];
getComplexNumbers[v8_] := Table[v8[[2 i - 1]] + I v8[[2 i]], {i, 1, 4}];

getS4[c4_] := {
    Norm[c4[[1]]]^2 + Norm[c4[[2]]]^2 - Norm[c4[[3]]]^2 - Norm[c4[[4]]]^2,
    2 Re[Conjugate[c4[[1]]] c4[[3]] + Conjugate[c4[[2]]] c4[[4]] ],
    2 Im[Conjugate[c4[[1]]] c4[[3]] + Conjugate[c4[[2]]] c4[[4]] ],
    2 Re[c4[[1]] c4[[4]] - c4[[2]] c4[[3]] ],
    2 Im[c4[[1]] c4[[4]] - c4[[2]] c4[[3]] ]
};

getQ2[v8_] := {Apply[Quaternion, v8[[{1,2,3,4}]]], Apply[Quaternion, v8[[{5,6,7,8}]]]};
getS3[q2_] := Conjugate[q2[[1]] ** Conjugate[q2[[2]]]];

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

projectToDirection[vertex_, projectionMatrix_] := projectionMatrix.vertex;
getAngle[u_, v_] := Round[ArcCos[u.v/Norm[u]/Norm[v]]/Pi*180., roundPrecision];

vertices = get421Vertices[True];
vertices24Cell = get24CellVertices[];

complexes = Map[getComplexNumbers, vertices];

s4s = Map[getS4, complexes];

q2s = Map[getQ2, vertices];

s3s = Map[getS3, q2s];

(* Print[Tally[s4s]]; *)
Print[Map[#[[2]]&, Tally[s4s]]//Tally];
(* Print[Tally[s3s]]; *)
Print[Map[#[[2]]&, Tally[s3s]]//Tally];

s3GroupsMap = GroupBy[vertices, getS4[getComplexNumbers[#]]&];
uniqueS4s = Keys[s3GroupsMap];
vertexGroups = Values[s3GroupsMap];
edgeGroups = {};
faceGroups = {};

For[i = 1, i <= Length[vertexGroups], i++,
    g1 = vertexGroups[[i]];
    edgeIndices = getEdges[g1];
    edges = Map[g1[[#]]&, edgeIndices];
    AppendTo[edgeGroups, edges];

    faceIndices = getTriangularFaces[g1];
    faces = Map[g1[[#]]&, faceIndices];
    AppendTo[faceGroups, faces];

    (* verified: every group is indeed a 24-cell *)

(*     orthogonalIndices =
        Select[Subsets[Range[Length[g1]], {4}], 
            Simplify[g1[[#[[1]]]].g1[[#[[2]]]]] == 0
            && Simplify[g1[[#[[1]]]].g1[[#[[3]]]]] == 0
            && Simplify[g1[[#[[1]]]].g1[[#[[4]]]]] == 0
            && Simplify[g1[[#[[2]]]].g1[[#[[3]]]]] == 0
            && Simplify[g1[[#[[2]]]].g1[[#[[4]]]]] == 0
            && Simplify[g1[[#[[3]]]].g1[[#[[4]]]]] == 0 &
        ];
    bases = g1[[orthogonalIndices[[1]]]];
    generatedG1 = vertices24Cell.bases;
    Print[{Length[g1], Length[generatedG1], Length[Union[g1, generatedG1]], Length[orthogonalIndices]}]; *)
];

phi = (Sqrt[5] + 1)/2;

projection8to4 = {
    {phi, phi, 0, 0, 1, -1, 0, 0},
    {1, -1, phi, phi, 0, 0, 0, 0},
    {0, 0, 1, -1, phi, phi , 0, 0},
    {0, 0, 0, 0, 0, 0, phi+1, phi-1}
};

h4Matrices = Get["data/h4_symmetries.wl"];

permutations = Permutations[Range[8]];
mostSymmetry = 0;
mostSymmetryInfo = {};
symmetryMatrices = {};
For[permIndex = 97, permIndex <= Length[permutations] && permIndex <= 97, permIndex ++,
    If[Mod[permIndex, 100] == 1, Print[{permIndex, mostSymmetryInfo}]];
    perm = permutations[[permIndex]];
    newProjection = Transpose[Transpose[projection8to4][[perm]]];
    vertices4d = Map[newProjection.# &, vertices]//Simplify;
    
    (* test if all 240 vertices have H4 symmetry. By random sampling, it appears they always have *)
(*     hasH4Symmetry = True;
    For[i = 1, i <= 100, i++,
        
        If[MemberQ[vertices4d, h4Matrices[[ RandomInteger[{1, Length[h4Matrices]}] ]].vertices4d[[ RandomInteger[{1, Length[vertices4d]}] ]]//Simplify ] == False, 
            hasH4Symmetry = False;
            Break[]
        ];
    ];

    If[hasH4Symmetry == False,
        Print[{permIndex, hasH4Symmetry}]
    ];
 *)

    vertexGroups4d = Map[newProjection.# &, vertexGroups, {2}]//Simplify;
    vertexGroups4dMap = <||>;
    For[groupIndex = 1, groupIndex <= Length[vertexGroups4d], groupIndex++,
        For[inGroupIndex = 1, inGroupIndex <= Length[vertexGroups4d[[groupIndex]] ], inGroupIndex++,
            vertexGroups4dMap[vertexGroups4d[[groupIndex]][[inGroupIndex]]] = groupIndex;
        ]
    ];

    numSymmetry = 0;
    For[symIndex = 1, symIndex <= Length[h4Matrices], symIndex++,
        matrix = h4Matrices[[symIndex]];
        isSymmetry = True;
        For[groupIndex = 1, isSymmetry && groupIndex <= Length[vertexGroups4d], groupIndex++,
            trackedIndex = -1;
            For[inGroupIndex = 1, isSymmetry && inGroupIndex <= Length[vertexGroups4d[[groupIndex]] ] && inGroupIndex <= 5, inGroupIndex++,
                mappedIndex = vertexGroups4dMap[ Simplify[matrix.vertexGroups4d[[groupIndex]][[inGroupIndex]] ] ];
                If[trackedIndex == -1,
                    trackedIndex = mappedIndex,
                    If[trackedIndex != mappedIndex,
                        isSymmetry = False;
                    ];
                ];
            ];
        ];

        If[isSymmetry, numSymmetry++];
        If[isSymmetry,
            (* Print[Det[matrix]]; *)
            (* Print[Eigenvalues[matrix]//Tally]; *)
            (* Print[Eigenvectors[matrix]//N]; *)
            Print[matrix];
            AppendTo[symmetryMatrices, matrix];
        ];
    ];

    permInfo = {permIndex, perm, numSymmetry};
    If[numSymmetry > mostSymmetry,
        mostSymmetry = numSymmetry;
        mostSymmetryInfo = permInfo;
        Print[permInfo];
    ];
];

Print["Most symmetry achieved by:"];
Print[mostSymmetryInfo];
Print[symmetryMatrices];
Save["data/perm97_symmetries.wl", symmetryMatrices];

(* 
{4, {1, 2, 3, 4, 5, 7, 8, 6}, 24}
4: all Det == 1
{6, {1, 2, 3, 4, 5, 8, 7, 6}, 40}
6: 20 symmetries Det == 1 and 20 Det == -1
{97, {1, 2, 3, 8, 4, 5, 6, 7}, 72}
97: all Det == 1
*)