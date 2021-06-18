#!/usr/bin/env wolframscript

cubeVertices[dimension_] := 2 * Map[IntegerDigits[#, 2][[2;;dimension+1]]&, Table[k, {k, 2^dimension, 2 * 2^dimension - 1}]] - 1;

demicubeVertices[dimension_, altMode_] := Select[cubeVertices[dimension], Mod[Total[#] + 8, 4] == Mod[If[altMode, 0, 2] + dimension, 4] &];

get421Vertices[altMode_] := Module[
    {vertices},
    vertices = {};
    vertices = Join[vertices, Union[Flatten[Permutations/@Map[Join[#, {0, 0, 0, 0, 0, 0}]& , cubeVertices[2] * 2], 1]]];
    vertices = Join[vertices, demicubeVertices[8, altMode]];
    vertices
];

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

get241Vertices[] := Module[
    {vertices, templates, i, cube8, class3},
    vertices = {};
    vertices = Join[vertices, orthoplexVertices[8] * 4];
    vertices = Join[vertices, Union[Flatten[Permutations/@Map[Join[#, {0, 0, 0, 0}]& , cubeVertices[4] * 2], 1]]];
    templates = Permutations[{3, 1, 1, 1, 1, 1, 1, 1}];

    cube8 = cubeVertices[8];
    class3 = {};
    For[i = 1, i <= Length[templates], i++,
        For[j = 1, j <= Length[cube8], j++,
            If[Apply[Times, cube8[[j]]] == -1,
                newVertex = Table[templates[[i]][[k]] * cube8[[j]][[k]], {k, 8}];
                AppendTo[class3, newVertex]
            ];
        ];
    ];
    vertices = Join[vertices, class3];
    vertices
];


vertices = Get["data/leech_vertices.wl"];
Print[Length[vertices]];

vGroTsen = Get["external_data/leech-lattice-first-shell.wl"];
Print[Length[vGroTsen]];

int = Intersection[vertices, vGroTsen];
Print[Length[int]];

comp1 = Complement[vertices, vGroTsen];
Print[Length[comp1]];

comp2 = Complement[vGroTsen, vertices];
Print[Length[comp2]];

pm = ArrayFlatten[{{IdentityMatrix[8], IdentityMatrix[8] * 0, IdentityMatrix[8] * 0}}];
v8 = vGroTsen.Transpose[pm];
v8 = Union[v8];
(* 2641 points *)
Export["leech_8d_projection.csv", v8];

norms = Map[Norm, v8];

(* Tally of norms: {{4 Sqrt[2], 240}, {4, 2160}, {2 Sqrt[2], 240}, {0, 1}} *)

shell1Points = Select[v8, Norm[#]==4 Sqrt[2] &];
vertices421 = get421Vertices[True];
Tally[Map[Sort[#] &, Abs[shell1Points]]]
(* {{{0, 0, 0, 0, 0, 0, 4, 4}, 112}, {{2, 2, 2, 2, 2, 2, 2, 2}, 128}} *)

Map[Length, {shell1Points, vertices421 * 2, Intersection[shell1Points, vertices421 * 2]}]
(* all 240. these two sets are the same 
shell1 is twice 421.
By a python script, I can verify that these 240 vertices are boundary points of the convex hull of v8.
*)

shell2Points = Select[v8, Norm[#]==4 &];
(* 2160 *)
vertices241 = get241Vertices[];

Tally[Map[Sort[#] &, Abs[shell2Points]]]
(* {{{0, 0, 0, 0, 0, 0, 0, 4}, 16}, {{1, 1, 1, 1, 1, 1, 1, 3}, 1024}, {{0, 0, 0, 0, 2, 2, 2, 2}, 1120}} *)
Map[Length, {shell2Points, vertices241, Intersection[shell2Points, vertices241]}]
(* shell2Points are vertices of 241 *)

shell3Points = Select[v8, Norm[#]==2 Sqrt[2] &];
(* 240, another copy of 421 polytope *)

Tally[Map[Sort[#] &, Abs[shell3Points]]]
(* {{{0, 0, 0, 0, 0, 0, 2, 2}, 112}, {{1, 1, 1, 1, 1, 1, 1, 1}, 128}} *)

Map[Length, {shell3Points, vertices421, Intersection[shell3Points, vertices421]}]
(* all 240, shell3 is just 421 *)


edges8dMap = Get["data/leech_8d_edges.wl"];
unique8dEdges = Keys[edges8dMap];
(* 619081 *)

phi = (Sqrt[5] + 1)/2;
projection8to3 = {
    {phi, phi, 0, 0, 1, -1, 0, 0},
    {1, -1, phi, phi, 0, 0, 0, 0},
    {0, 0, 1, -1, phi, phi , 0, 0}
};

(* edges3d = Map[#.Transpose[projection8to3] &, unique8dEdges, {2}];

unique3dEdges = Union[Expand[edges3d]];

Length[unique3dEdges]

randomVec = {0.318317, 0.257591, 0.554799};

ordered3dEdges = Map[
    If[#[[1]].randomVec >#[[2]].randomVec, Reverse[#], #] &,
    unique3dEdges
];

rounded3dEdges = Round[ordered3dEdges, roundPrecision];
uniqueRounded3dEdges = Union[rounded3dEdges];
(* 135985 *)

unique3dEdges = Union[ordered3dEdges];
unique3dEdges = Select[unique3dEdges, Norm[# * 1.0] > 0.0001 &];
Save["data/leech_3d_edges.wl", unique3dEdges];

count: 135984
 *)

unique3dEdges = Get["data/leech_3d_edges.wl"];
(* 176310 *)
norms = Map[Norm, unique3dEdges * 1.0];
roundPrecision = 0.000001;

rounded3dEdges = Round[unique3dEdges, roundPrecision];
uniqueRounded3dEdges = Union[rounded3dEdges];
(* 176310 *)
rounded3dVertices = Union[Flatten[rounded3dEdges, 1]];
(* 1057 *)

randomVec = RandomReal[1, 3];
randomVec = {0.318317, 0.257591, 0.554799};

ordered3dEdges = Map[
    If[#[[1]].randomVec >#[[2]].randomVec, Reverse[#], #] &,
    unique3dEdges
];

rounded3dEdges = Round[ordered3dEdges, roundPrecision];
uniqueRounded3dEdges = Union[rounded3dEdges];
(* 135985 *)

unique3dEdges = Union[ordered3dEdges];

edges3dMap = Get["data/leech_3d_edges_tally.wl"];

segments3dMap = Get["data/leech_3d_segments_tally.wl"];

Print[Sort[ Tally[Values[edges3dMap]], #1[[1]]<#2[[1]]&]];
Print[Sort[ Tally[Values[segments3dMap]], #1[[1]]<#2[[1]]&]];
(* doesn't look right *)

zeroEdges = Select[Keys[edges3dMap], #[[1]]==#[[2]] &];
Map[edges3dMap, zeroEdges]//Total
(* 56495728 *)

zeroSegments = Select[Keys[segments3dMap], #[[1]]==#[[2]] &];
Map[segments3dMap, zeroSegments]//Total
(* 56495728 *)

(* getting 8-dimensions from front and end *)
pm2 = ArrayFlatten[{{IdentityMatrix[8] * 1, IdentityMatrix[8] * 0, IdentityMatrix[8] * 1}}];
v82 = vGroTsen.Transpose[pm2];
v82 = Union[v82];
Print[Length[v82]];
(* 2641 points *)

Tally[Map[Sort[#] &, Abs[v8]]]//Sort//InputForm
(* 
{{{0, 0, 0, 0, 0, 0, 4, 4}, 112}, {{0, 0, 0, 0, 0, 0, 0, 4}, 16}, {{1, 1, 1, 1, 1, 1, 1, 3}, 1024}, {{2, 2, 2, 2, 2, 2, 2, 2}, 128}, {{0, 0, 0, 0, 2, 2, 2, 2}, 1120}, {{0, 0, 0, 0, 0, 0, 2, 2}, 112}, {{1, 1, 1, 1, 1, 1, 1, 1}, 128}, {{0, 0, 0, 0, 0, 0, 0, 0}, 1}} 
*)

Tally[Map[Sort[#] &, Abs[v82]]]//Sort//InputForm
(* 
{{{0, 0, 0, 0, 0, 0, 0, 0}, 1}, {{0, 0, 0, 0, 0, 0, 0, 4}, 16}, {{0, 0, 0, 0, 0, 0, 0, 8}, 16}, {{0, 0, 0, 0, 0, 0, 4, 4}, 112}, {{0, 0, 0, 0, 2, 2, 2, 2}, 224}, {{0, 0, 0, 0, 4, 4, 4, 4}, 224}, {{0, 0, 0, 2, 2, 2, 2, 4}, 1792}, {{2, 2, 2, 2, 2, 2, 2, 2}, 256}}
 *)

v3 = Union[Expand[v8.Transpose[projection8to3]]];
v32 = Union[Expand[v82.Transpose[projection8to3]]];



phi = (Sqrt[5] + 1)/2;
projection8to3 = {
    {phi, phi, 0, 0, 1, -1, 0, 0},
    {1, -1, phi, phi, 0, 0, 0, 0},
    {0, 0, 1, -1, phi, phi, 0, 0}
};

projectionMatrix = ArrayFlatten[{{projection8to3 * 1, projection8to3 * 0, projection8to3 * 0 }}];

vertices3d = vertices.Transpose[projectionMatrix * 1.0];

roundPrecision = 0.000001;

roundedVertices3d = Round[vertices3d, roundPrecision];
verticesTally = Tally[Expand[roundedVertices3d]];

Print["Finished tallying " <> IntegerString[verticesTally//Length]];
tallyTally = Sort[Tally[Map[#[[2]]&, verticesTally]]];
Print["Distribution of tally counts:"];
Print[tallyTally];

uniqueV3 = Map[#[[1]]&, verticesTally];
Tally[ Round[Map[Norm, uniqueV3], roundPrecision ]]//Sort 

(* 
if projection matrix = 1, 0, 0 times 8to3, 32 different norms
icosahedral symmetry

if projection matrix = 1, 0, 1 times 8to3, 44 different norms
octahedron symmetry. Largest norms: vertices of octahedron, edge centers, vertices of cube etc.

*)

Select[uniqueV3, Norm[#]>12.4 && Norm[#]<12.99 &]

