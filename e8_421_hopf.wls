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

vertices = get421Vertices[True];
vertices24Cell = get24CellVertices[];

complexes = Map[getComplexNumbers, vertices];

s4s = Map[getS4, complexes];

q2s = Map[getQ2, vertices];

s3s = Map[getS3, q2s];

Print[Tally[s4s]];
Print[Map[#[[2]]&, Tally[s4s]]//Tally];
Print[Tally[s3s]];
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

permutations = Permutations[Range[8]];
(* permIndex = 12;
perm = permutations[[permIndex]];
newProjection = Transpose[Transpose[projection8to4][[perm]]];
vertices4d = Map[newProjection.# &, vertices]//Simplify;
mirror1 = vertices4d[[2]] - vertices4d[[1]];
mirror2 = vertices4d[[27]] - vertices4d[[1]];
Print[{mirror1, mirror2}//N];
vertices4dReflected = Map[reflect[#, mirror2]&, vertices4d]//Simplify;
 *)(* vertices4dReflected = Map[reflect[reflect[#, mirror1], mirror2]&, vertices4d]; *)

(* Print[Length[Intersection[Round[vertices4d, roundPrecision], Round[vertices4dReflected, roundPrecision]]]]; *)

(* permIndex = 12;

perm = permutations[[permIndex]];
newProjection = Transpose[Transpose[projection8to4][[perm]]];
vertexGroups4d = Map[newProjection.# &, vertexGroups, {2}];
edgeGroups4d = Map[newProjection.# &, edgeGroups, {3}];
faceGroups4d = Map[newProjection.# &, faceGroups, {3}];

vertices4dReflected = Map[reflect[#, mirror1]&, vertices4d];
vertexGroups4dReflected = Map[reflect[#, mirror1]&, vertexGroups4d, {2}];
intersectionMatrix = Table[Length[Intersection[Round[vertexGroups4d[[i]], roundPrecision], Round[vertexGroups4dReflected[[j]], roundPrecision ]]], {i, Length[vertexGroups4d]}, {j, Length[vertexGroups4dReflected]}];
nonZeroEntries = Total[Total[Map[If[#!=0, 1, 0] &, intersectionMatrix, {2}]]];
Print[nonZeroEntries];
 *)
If[tryPerms,
    minNonZeroEntries = 100000;
    For[permIndex = 1, permIndex <= Length[permutations], permIndex ++,
        If[Mod[permIndex, 1] == 0, Print[{permIndex, Length[permutations]}]];
        perm = permutations[[permIndex]];
        newProjection = Transpose[Transpose[projection8to4][[perm]]];
        vertexGroups4d = Map[newProjection.# &, vertexGroups, {2}];
        (* edgeGroups4d = Map[newProjection.# &, edgeGroups, {3}]; *)
        (* faceGroups4d = Map[newProjection.# &, faceGroups, {3}]; *)

        (* edgeCenterGroups4d = Table[Map[Total, edgeGroups4d[[k]]], {k, 1, Length[edgeGroups4d]}]; *)
        (* faceCenterGroups4d = Table[Map[Total, faceGroups4d[[k]]], {k, 1, Length[faceGroups4d]}]; *)

        vertexTally = Tally[Map[Sort[Tally[#]]&, Expand[FullSimplify[Map[Norm, vertexGroups4d, {2}]]]]];
        (* edgeTally = Tally[Map[Sort[Tally[#]]&, Expand[FullSimplify[Map[Norm, edgeCenterGroups4d, {2}]]]]]; *)
        (* faceTally = Tally[Map[Sort[Tally[#]]&, Expand[FullSimplify[Map[Norm, faceCenterGroups4d, {2}]]]]]; *)

        (* Print[{permIndex, perm, Length[vertexTally], Length[edgeTally], Length[faceTally]}]; *)

(*         If[Length[vertexTally] == 1 && Length[edgeTally] == 1 && Length[faceTally] == 1,
            Print[{permIndex, perm}];
        ];
 *)

        vertices4d = Map[newProjection.# &, vertices]//Simplify;
        outerShell = Select[vertices4d, Norm[#] > 6&];
        v1 = outerShell[[1]];
        If[Length[outerShell] != 120, Continue[]];
        firstCage = Select[outerShell, (#.v1 > 33 && #.v1 < 34)&];
        If[Length[firstCage] != 12, Continue[]];
        vTop = firstCage[[1]];
        firstCircle = Select[firstCage, (#.vTop > 33 && #.vTop < 34)&];
        If[Length[firstCircle] != 5, Continue[]];
        vCircleMark = firstCircle[[1]];
        vCircleLR = Select[firstCircle, (#.vCircleMark > 33 && #.vCircleMark < 34)&];
        If[Length[vCircleLR] != 2, Continue[]];

        mirror1 = vCircleMark - vCircleLR[[1]];
        mirror2 = vCircleLR[[1]] - vCircleLR[[2]];

        (* Print[{mirrorIndex, mirror1, mirror2}//N]; *)
        vertices4dReflected = Map[reflect[#, mirror1]&, vertices4d]//Simplify;
        (* vertices4dReflected = Map[reflect[reflect[#, mirror1], mirror2]&, vertices4d]; *)

        overallIntersection = Length[Intersection[Round[vertices4d, roundPrecision], Round[vertices4dReflected, roundPrecision]]];

        If[overallIntersection != Length[vertices4d],
            Continue[]
        ];

        If[Length[vertexTally] == 1,
            For[outerShellIndex = 1, outerShellIndex <= Length[outerShell], outerShellIndex++,
                v1 = outerShell[[outerShellIndex]];
                firstCage = Select[outerShell, (#.v1 > 33 && #.v1 < 34)&];
                If[Length[firstCage] != 12, Continue[]];

                For[firstCageIndex = 1, firstCageIndex <= Length[firstCage], firstCageIndex++,
                    vTop = firstCage[[firstCageIndex]];
                    firstCircle = Select[firstCage, (#.vTop > 33 && #.vTop < 34)&];
                    If[Length[firstCircle] != 5, Continue[]];
                    vCircleMark = firstCircle[[1]];
                    vCircleLR = Select[firstCircle, (#.vCircleMark > 33 && #.vCircleMark < 34)&];
                    If[Length[vCircleLR] != 2, Continue[]];

                    mirror1 = vCircleMark - vCircleLR[[1]];
                    mirror2 = vCircleLR[[1]] - vCircleLR[[2]];

                    vertexGroups4dReflected = Map[ reflect[#, mirror1]&, vertexGroups4d, {2}];
                    (* vertexGroups4dReflected = Map[ reflect[reflect[#, mirror1], mirror2]&, vertexGroups4d, {2}]; *)

                    intersectionMatrix = Table[Length[Intersection[Round[vertexGroups4d[[i]], roundPrecision], Round[vertexGroups4dReflected[[j]], roundPrecision ]]], {i, Length[vertexGroups4d]}, {j, Length[vertexGroups4dReflected]}];
                    nonZeroEntries = Total[Total[Map[If[#!=0, 1, 0] &, intersectionMatrix, {2}]]];
                    totalIntersection = Total[Total[intersectionMatrix]];
                    If[nonZeroEntries < minNonZeroEntries || nonZeroEntries == Length[vertexGroups4d], 
                        minNonZeroEntries = nonZeroEntries;
                        Print[{permIndex, outerShellIndex, firstCageIndex, perm, nonZeroEntries, totalIntersection}];
                        Print[intersectionMatrix];
                    ];
                ];
            ];
        ];
    ];
];

(* 
random mirrors:
2 mirrors: 182, 40 non zero, {6,6,6,6}*10 
1 mirror: 25, 34, 24, 24, 6666*8
mirrors around the first cage
false: 2887, {1, 6, 2, 3, 5, 4, 7, 8}, 10, 240
*)


permIndex = 182;
projection8to4 = Transpose[Transpose[projection8to4][[permutations[[permIndex]]]]];

(* vertexGroups4d = Map[projection8to4.# &, vertexGroups, {2}];
edgeGroups4d = Map[projection8to4.# &, edgeGroups, {3}];
faceGroups4d = Map[projection8to4.# &, faceGroups, {3}];

edgeCenterGroups4d = Table[Map[Total, edgeGroups4d[[k]]], {k, 1, Length[edgeGroups4d]}];
faceCenterGroups4d = Table[Map[Total, faceGroups4d[[k]]], {k, 1, Length[faceGroups4d]}];
 *)
projection4to3 = {
    {0, 0, 2 Sin[2 Pi/15], (1 + Sqrt[5]) Sin[Pi/15]},
    {1, -(1 + Sqrt[5]) Sin[Pi/30], 0, 0},
    Table[0, {k, 4}]
};

projection4to3 = IdentityMatrix[4][[{1,2,3}]];

projectionMatrix = projection4to3.projection8to4;

vertexGroups3d = Map[projectionMatrix.# &, vertexGroups, {2}];
edgeGroups3d = Map[projectionMatrix.# &, edgeGroups, {3}];
faceGroups3d = Map[projectionMatrix.# &, faceGroups, {3}];

(* viewPoint = {1, 0, phi}; *)
(* viewPoint = {-1, 0, phi}; *)
viewPoint = {0, -phi, -1};
(* viewPoint = {0, -phi, 1}; *)
(* viewPoint = {-phi, -1, 0}; *)
(* viewPoint = {phi, -1, 0}; *)
(* viewPoint = {0, 0, 10}; *)


range = 6;
thickness = 0.1;
lighting = {{"Point", White, {10, -10, 10}}};

graphElements = {};
For[groupIndex = 1, groupIndex <= Length[vertexGroups], groupIndex++,
    If[showVertices,
        AppendTo[graphElements, {
            Opacity[1],
            FaceForm[colors[[groupIndex]]],
            Sphere[vertexGroups3d[[groupIndex]], thickness]
        }]
    ];

    If[showEdges && groupIndex <= 10,
        AppendTo[graphElements, {
            Opacity[1],
            FaceForm[colors[[groupIndex]] ],
            Cylinder[edgeGroups3d[[groupIndex]], thickness * 0.8]
        }]
    ];

    If[showFaces,
        AppendTo[graphElements, {
            Opacity[0.5],
            FaceForm[colors[[groupIndex]] ],
            Map[Polygon, faceGroups3d[[groupIndex]]]
        }]
    ];
];

graph = Graphics3D[
        graphElements,
        ViewPoint -> viewPoint * 3000,
        PlotRange -> {{-range, range}, {-range, range}, {-range, range}},
        Lighting -> lighting
    ];

outputFileName = "e8_421/hopf" <> If[exportToPov, "_pov", "_native"] <> ".png";
outputFileName = FileNameJoin[{imageFolder, outputFileName}];
cropCommand = "mogrify -crop " <> IntegerString[imageSize[[2]]] <> "x" <> IntegerString[imageSize[[2]]] <> "+" <> IntegerString[(imageSize[[1]]-imageSize[[2]])/2] <> "+0 " <> outputFileName;

If[exportToPov,
    If[FileExistsQ[outputFileName], DeleteFile[outputFileName]];
    POVRayRender[graph, 
        Method -> "Triangle", 
        OutputPath -> outputFileName, 
        ImageSize -> imageSize
        , RenderOptions -> "+A0.001 -J +UA"
    ];
    Run[cropCommand];
    ,
    Export[outputFileName, graph];
];

