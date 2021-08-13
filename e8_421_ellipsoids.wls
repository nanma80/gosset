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
Use3d = False;

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
(* {{{8, 0, 0, 0, 0}, 24}, {{0, 8, 0, 0, 0}, 24}, {{0, 0, 8, 0, 0}, 24}, {{0, 0, 0, 8, 0}, 24}, {{0, 0, 0, 0, 8}, 24}, {{0, 0, 0, 0, -8}, 24}, {{0, 0, 0, -8, 0}, 24}, {{0, 0, -8, 0, 0}, 24}, {{0, -8, 0, 0, 0}, 24}, {{-8, 0, 0, 0, 0}, 24}} *)
Print[Map[#[[2]]&, Tally[s4s]]//Tally];
(* {{24, 10}} *)

(* Print[Tally[s3s]]; *)
Print[Map[#[[2]]&, Tally[s3s]]//Tally];
(* {{48, 1}, {24, 8}} *)

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
    Print[{Length[g1], Length[generatedG1], Length[Union[g1, generatedG1]], Length[orthogonalIndices]}];
    Print[{i, orthogonalIndices[[1]]}];
 *)    (* {1, {1, 6, 11, 12}}
    {2, {1, 2, 3, 4}}
    {3, {1, 2, 3, 4}}
    {4, {1, 2, 3, 4}}
    {5, {1, 2, 3, 4}}
    {6, {1, 2, 3, 4}}
    {7, {1, 2, 3, 4}}
    {8, {1, 2, 3, 4}}
    {9, {1, 2, 3, 4}}
    {10, {1, 6, 11, 12}}
 *)
];

phi = (Sqrt[5] + 1)/2;

projection8to4 = {
    {phi, phi, 0, 0, 1, -1, 0, 0},
    {1, -1, phi, phi, 0, 0, 0, 0},
    {0, 0, 1, -1, phi, phi , 0, 0},
    {0, 0, 0, 0, 0, 0, phi+1, phi-1}
};

rotation4d = {
    {1, 0, 0, 0},
    {0, phi, 1/phi, 0}/Sqrt[3],
    {0, -1/phi, phi, 0}/Sqrt[3],
    {0, 0, 0, 1}
};

projection8to4AndRotation = rotation4d.projection8to4;

h4Matrices = If[Use3d,
    Get["data/h3_symmetries.wl"],
    Get["data/h4_symmetries.wl"]
];

permutations = Permutations[Range[8]];
mostSymmetry = 0;
mostSymmetryInfo = {};
symmetryMatrices = {};
bestPerms = {};

(* For[permIndex = 1, permIndex <= Length[permutations] && permIndex <= 1, permIndex ++, *)

permIndex = 112;
    If[Mod[permIndex, 100] == 1, Print[{permIndex, mostSymmetryInfo}]];
    perm = permutations[[permIndex]];
    newProjection = Transpose[Transpose[projection8to4AndRotation][[perm]]];
    If[Use3d,
        newProjection = newProjection[[{1,2,3}]];
    ];
    vertices4d = Map[newProjection.# &, vertices]//Simplify;
    
    vertexGroups4d = Map[newProjection.# &, vertexGroups, {2}]//Simplify//Expand;

getRow[v4_] := {v4[[1]] v4[[1]], v4[[2]] v4[[2]], v4[[3]] v4[[3]], v4[[4]] v4[[4]], v4[[1]] v4[[2]], v4[[1]] v4[[3]], v4[[1]] v4[[4]], v4[[2]] v4[[3]], v4[[2]] v4[[4]], v4[[3]] v4[[4]], 1};

groupIndex = 3;
quadMatrix = Map[getRow, vertexGroups4d[[groupIndex]]];
(* Print[MatrixRank[quadMatrix]]; *)

(* matrixRanks = Table[
    MatrixRank[
        Map[getRow, vertexGroups4d[[k]]]
    ],
    {k, 1, Length[vertexGroups4d]}
];

Print[matrixRanks];
 *)

(* {7, 7, 10, 10, 7, 7, 10, 10, 7, 7} for permIndex=1
all 10 for 112
*)

ellipsoidEquation[v4_, coefs_] := coefs[[1]] v4[[1]] v4[[1]] + coefs[[2]] v4[[2]] v4[[2]] + coefs[[3]] v4[[3]] v4[[3]] + coefs[[4]] v4[[4]] v4[[4]] + coefs[[5]] v4[[1]] v4[[2]] + coefs[[6]] v4[[1]] v4[[3]] + coefs[[7]] v4[[1]] v4[[4]] + coefs[[8]] v4[[2]] v4[[3]] + coefs[[9]] v4[[2]] v4[[4]] + coefs[[10]] v4[[3]] v4[[4]] + coefs[[11]];

allCoefs = {};

(* LinearSolve[quadMatrix, Table[0, {k, 24}]] *)

groupIndexLimit = 10;
For[groupIndex = 1, groupIndex <= Length[vertexGroups4d] && groupIndex <= groupIndexLimit, groupIndex++,
    quadMatrix = Map[getRow, vertexGroups4d[[groupIndex]]]//Simplify;
    coefs = NullSpace[quadMatrix][[1]];
    coefs = (-coefs/coefs[[11]])//FullSimplify;
    Print[{groupIndex, coefs}];
    AppendTo[allCoefs, coefs];
(* ]; *)

(* For[groupIndex = 1, groupIndex <= Length[vertexGroups4d] && groupIndex <= groupIndexLimit, groupIndex++, *)
    coefs = allCoefs[[groupIndex]];

    symMatrix = {
        {coefs[[1]], coefs[[5]]/2, coefs[[6]]/2, coefs[[7]]/2},
        {coefs[[5]]/2, coefs[[2]], coefs[[8]]/2, coefs[[9]]/2},
        {coefs[[6]]/2, coefs[[8]]/2, coefs[[3]], coefs[[10]]/2},
        {coefs[[7]]/2, coefs[[9]]/2, coefs[[10]]/2, coefs[[4]]}
    };

    Print[{groupIndex, Eigenvalues[symMatrix]//N}];
    Print[{groupIndex, Eigenvectors[symMatrix]//N}];

    Print[
        Table[
            Round[{vertexGroups4d[[groupIndex]][[k]]}.symMatrix.Transpose[{vertexGroups4d[[groupIndex]][[k]]}], roundPrecision]
        , {k, 1, Length[vertexGroups4d[[groupIndex]]]}]        
    ];
];

Print[allCoefs];
(* 
Before 4D rotation
{1, {0.327254, 0.327254, 0.0238729, 0.0238729}}
{1, {{0., 0., 0., 1.}, {0., -0.381966, 1., 0.}, {0., 2.61803, 1., 0.}, {1., 0., 0., 0.}}}
{2, {0.153277, 0.153277, 0.0194687, 0.0194687}}
{2, {{-0.381966, -1.07047, 0., 1.}, {-1.07047, -0.381966, 1., 0.}, {-0.381966, 1.07047, 0., 1.}, {1.07047, -0.381966, 1., 0.}}}
{3, {0.0625, 0.0625, 0.0182373, 0.0182373}}
{3, {{0., 0., 0., 1.}, {0., -0.381966, 1., 0.}, {0., 2.61803, 1., 0.}, {1., 0., 0., 0.}}}
{4, {0.153277, 0.153277, 0.0194687, 0.0194687}}
{4, {{-0.937781, -0.402191, 0., 1.}, {-0.402191, 0.363738, 1., 0.}, {0.723332, 0.799801, 0., 1.}, {0.799801, -1.86488, 1., 0.}}}
{5, {0.153277, 0.153277, 0.0194687, 0.0194687}}
{5, {{-0.937781, 0.402191, 0., 1.}, {0.402191, 0.363738, 1., 0.}, {0.723332, -0.799801, 0., 1.}, {-0.799801, -1.86488, 1., 0.}}}
{6, {0.153277, 0.153277, 0.0194687, 0.0194687}}
{6, {{0.723332, -0.799801, 0., 1.}, {-0.799801, -1.86488, 1., 0.}, {-0.937781, 0.402191, 0., 1.}, {0.402191, 0.363738, 1., 0.}}}
{7, {0.153277, 0.153277, 0.0194687, 0.0194687}}
{7, {{0.723332, 0.799801, 0., 1.}, {0.799801, -1.86488, 1., 0.}, {-0.937781, -0.402191, 0., 1.}, {-0.402191, 0.363738, 1., 0.}}}
{8, {0.327254, 0.327254, 0.0238729, 0.0238729}}
{8, {{0., 2.61803, 1., 0.}, {1., 0., 0., 0.}, {0., 0., 0., 1.}, {0., -0.381966, 1., 0.}}}
{9, {0.153277, 0.153277, 0.0194687, 0.0194687}}
{9, {{-0.381966, 1.07047, 0., 1.}, {1.07047, -0.381966, 1., 0.}, {-0.381966, -1.07047, 0., 1.}, {-1.07047, -0.381966, 1., 0.}}}
{10, {0.0625, 0.0625, 0.0182373, 0.0182373}}
{10, {{0., 2.61803, 1., 0.}, {1., 0., 0., 0.}, {0., 0., 0., 1.}, {0., -0.381966, 1., 0.}}}

After 4D rotation:
{1, {0.32725424859373686, 0.32725424859373686, 0.023872875703131572, 0.023872875703131572}}
{1, {{0., 0., 0., 1.}, {0., 0., 1., 0.}, {0., 1., 0., 0.}, {1., 0., 0., 0.}}}

{8, {0.3272542485937369, 0.3272542485937369, 0.023872875703131572, 0.023872875703131572}}
{8, {{0., 1., 0., 0.}, {1., 0., 0., 0.}, {0., 0., 0., 1.}, {0., 0., 1., 0.}}}

{3, {0.0625, 0.0625, 0.01823725421878941, 0.01823725421878941}}
{3, {{0., 0., 0., 1.}, {0., 0., 1., 0.}, {0., 1., 0., 0.}, {1., 0., 0., 0.}}}

{10, {0.0625, 0.0625, 0.01823725421878941, 0.01823725421878941}}
{10, {{0., 1., 0., 0.}, {1., 0., 0., 0.}, {0., 0., 0., 1.}, {0., 0., 1., 0.}}}

{2, {0.15327701753558592, 0.15327701753558592, 0.019468733870677206, 0.019468733870677206}}
{2, {{0., -1., 0., 1.}, {-1., 0., 1., 0.}, {0., 1., 0., 1.}, {1., 0., 1., 0.}}}

{9, {0.15327701753558592, 0.15327701753558592, 0.019468733870677206, 0.019468733870677206}}
{9, {{0., 1., 0., 1.}, {1., 0., 1., 0.}, {0., -1., 0., 1.}, {-1., 0., 1., 0.}}}


{4, {0.15327701753558592, 0.15327701753558592, 0.01946873387067721, 0.01946873387067721}}
{4, {{-0.8660254037844386, -0.5, 0., 1.}, {-0.5, 0.8660254037844386, 1., 0.}, {0.8660254037844386, 0.5, 0., 1.}, {0.5, -0.8660254037844386, 1., 0.}}}

{5, {0.15327701753558592, 0.15327701753558592, 0.01946873387067721, 0.01946873387067721}}
{5, {{-0.8660254037844386, 0.5, 0., 1.}, {0.5, 0.8660254037844386, 1., 0.}, {0.8660254037844386, -0.5, 0., 1.}, {-0.5, -0.8660254037844386, 1., 0.}}}

{6, {0.15327701753558592, 0.15327701753558592, 0.019468733870677223, 0.019468733870677223}}
{6, {{0.8660254037844387, -0.5, 0., 1.}, {-0.5, -0.8660254037844386, 1., 0.}, {-0.8660254037844387, 0.5, 0., 1.}, {0.5, 0.8660254037844386, 1., 0.}}}

{7, {0.15327701753558592, 0.15327701753558592, 0.019468733870677223, 0.019468733870677223}}
{7, {{0.8660254037844387, 0.5, 0., 1.}, {0.5, -0.8660254037844386, 1., 0.}, {-0.8660254037844387, -0.5, 0., 1.}, {-0.5, 0.8660254037844386, 1., 0.}}}




 *)

(* groupIndex = 2;
quadMatrix = Map[getRow, vertexGroups4d[[groupIndex]]];
coefs = NullSpace[quadMatrix][[1]]//Simplify
quadMatrix.coefs//Simplify


checksum = Table[ellipsoidEquation[vertexGroups4d[[groupIndex]][[k]], coefs], {k, 1, 24}];
checksum//Simplify



groupIndex = 2;
sol = {x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11}/.Solve[
    Table[ellipsoidEquation[ vertexGroups4d[[groupIndex]][[k]], {x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11}] == 0, {k, 24}],
    {x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11}
]
coefs = sol[[1]];

Table[ellipsoidEquation[vertexGroups4d[[groupIndex]][[k]], coefs], {k, 1, 24}]//N


ellipsoidEquation[ vertexGroups4d[[1]][[1]], {x1, x2, x3, x4, x5, x6, x7, x8, x9, x10}]

ellipsoidEquation[v3_, coefs_] := coefs[[1]] v3[[1]] v3[[1]] + coefs[[2]] v3[[2]] v3[[2]] + coefs[[3]] v3[[3]] v3[[3]] + coefs[[4]] v3[[1]] v3[[2]] + coefs[[5]] v3[[1]] v3[[3]] + coefs[[6]] v3[[2]] v3[[3]] + coefs[[7]] v3[[1]] + coefs[[8]] v3[[2]] + coefs[[9]] v3[[3]];


sols = Table[
        Solve[
            Apply[And,
                Table[ellipsoidEquation[ vertexGroups4d[[i]][[k]], {x1, x2, x3, x4, x5, x6}] == 1, {k, 6}],
                    {x1, x2, x3, x4, x5, x6}
                ]
            ],
        {i, 1, Length[vertexGroups4d]}
    ];


sol = {x1, x2, x3, x4, x5, x6, x7, x8, x9}/.Solve[
    Table[ellipsoidEquation[ vertexGroups4d[[1]][[k]], {x1, x2, x3, x4, x5, x6, x7, x8, x9}] == 1, {k, 10}],
    {x1, x2, x3, x4, x5, x6, x7, x8, x9}
]
coefs = sol[[1]]/.{x1->0, x5->0};

ellipsoidEquation[v3_, coefs_] := coefs[[1]] v3[[1]] v3[[1]] + coefs[[2]] v3[[2]] v3[[2]] + coefs[[3]] v3[[3]] v3[[3]] + coefs[[4]] v3[[1]] v3[[2]] + coefs[[5]] v3[[1]] v3[[3]] + coefs[[6]] v3[[2]] v3[[3]];

sol = {x1, x2, x3, x4, x5, x6}/.Reduce[Apply[And, 
    Table[ellipsoidEquation[ vertexGroups4d[[2]][[k]], {x1, x2, x3, x4, x5, x6}] == 1//Simplify, {k, 10}]
    ],
    {x1, x2, x3, x4, x5, x6}
]

coefs = sol[[1]]

Table[ellipsoidEquation[vertexGroups4d[[2]][[k]], coefs], {k, 1, 24}]//N
 *)
(* Out[131]= {1., 1., 1., 1., 1., 1., 0.381966, 0.381966, 3.61803, 3.61803, 2.61803, 1.38197, 1.38197, 2.61803, 3.61803, 3.61803, 0.381966, 0.381966, 1., 1., 1., 1., 1., 1.} *)

(* ]; *)

(* Print["Most symmetry achieved by:"];
Print[mostSymmetryInfo];
Print[bestPerms];
Print[symmetryMatrices]; *)
(* Save["data/perm97_symmetries.wl", symmetryMatrices]; *)
(* Save["data/perm112_h3_symmetries.wl", symmetryMatrices]; *)

