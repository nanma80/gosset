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

pm = ArrayFlatten[{{IdentityMatrix[8], IdentityMatrix[8] * 0, IdentityMatrix[8] * 0}}];
tpm = Transpose[pm];
v8 = vertices.tpm;
v8unique = Union[v8];
Print[Length[v8]];
Print[Length[v8unique]];
(* 2641 points *)

v1 = vertices[[1]];
innerProducts = Tally[Map[v1.# &, vertices]]//Sort;
Print[innerProducts];
(* {{-32, 1}, {-16, 4600}, {-8, 47104}, {0, 93150}, {8, 47104}, {16, 4600}, {32, 1}} *)

targetInner = 16;
randomVec = RandomReal[1, 8];
limit = 10000;

edges8dMap = <||>;
For[i = 1, i<=limit && i <= Length[vertices]-1, i++, 
    If[Mod[i, 100]==0, Print[{i, limit, Length[edges8dMap]}]];
    For[j = i + 1, j <= limit && j <= Length[vertices], j++, 
        If[vertices[[i]].vertices[[j]] == targetInner, 
            edge8d = { vertices[[i]].tpm, vertices[[j]].tpm };
            If[edge8d[[1]].randomVec > edge8d[[2]].randomVec,
                edge8d = Reverse[edge8d]
            ];
            If[KeyExistsQ[edges8dMap, edge8d],
                edges8dMap[edge8d] = edges8dMap[edge8d] + 1,
                edges8dMap[edge8d] = 1
            ];
        ];   
    ];
];
Print[Length[edges8dMap]];
Print[{Total[Values[edges8dMap]], 452088000}];
(* The number of 24 edges should be: 452088000 *)

edges = Keys[edges8dMap];
appearedVertices = Union[Flatten[edges, 1]];
Print[{Length[appearedVertices], 2641}];

Save["data/leech_8d_edges.wl", edges8dMap];