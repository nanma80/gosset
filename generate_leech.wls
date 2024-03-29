#!/usr/bin/env wolframscript

(* from http://www.madore.org/~david/math/higman-sims-graph.html *)
elevenCycle = {
    {0}, {1, 0}, {1, 1}, {1, 8},
    {0}, {2, 0}, {2, 3}, {2, 10},
    {2, 8}, {1, 2}, {1, 9}, {1, 6}, 
    {1, 3}, {2, 9}, {2, 5}, {2, 2}, 
    {2, 7}, {1, 10}, {1, 5}, {1, 7}, 
    {1, 4}, {2, 1}, {2, 4}, {2, 6}
};

cubeVertices[dimension_] := 2 * Map[IntegerDigits[#, 2][[2;;dimension+1]]&, Table[k, {k, 2^dimension, 2 * 2^dimension - 1}]] - 1;

GetHSVertices[] := Module[
    {octads22, i, j, newVertex},

    vertices = {};
    newVertex = Table[If[i == 0 || i == 4, 4, 0], {i, 0, 23}];
    AppendTo[vertices, newVertex];
    For[j = 0, j < 24, j++,
        If[!(j == 0 || j == 4),
            t = {};
            newVertex = Table[If[i == j, -3, 1], {i, 0, 23}];
            AppendTo[vertices, newVertex];
        ];
    ];

    octads22 = {
        255, 13107, 21845, 26265, 39321, 43605, 52275,
        197525, 208985, 329017, 348307, 394835, 417845, 592211,
        626741, 657977, 696467, 789653, 835673, 1121809, 1171729,
        1198289, 1213469, 1320987, 1344177, 1582193, 1589783, 1970449,
        2181149, 2196689, 2239761, 2282001, 2365553, 2392343, 2626587,
        2638257, 2957841, 3146585, 3158165, 3342387, 4270257, 4293147,
        4331543, 4358513, 4475665, 4502545, 4723409, 4727069, 4932625,
        5244307, 5263417, 5570645, 6293045, 6316115, 6684825, 7899153,
        8463383, 8471153, 8524977, 8536347, 8655389, 8659409, 8882193,
        8943633, 8947473, 9439541, 9474131, 10027161, 10488467, 10526777,
        11141205, 11813905, 12586073, 12632213, 13369395, 13771281, 14749969
    };

    For[j=1, j<=Length[octads22], j++,
        n = octads22[[j]];
        newVertex = Reverse[IntegerDigits[n + 2^24, 2][[2;;24+1]] * 2];
        AppendTo[vertices, newVertex];
    ];

    vertices
];

GetAndCache759Vertices[] := Module[
    {vertices, dataFile, newVertex, hsVertices, v311, v3AllPerms},
    dataFile = "data/vertices_759.wl";
    vertices = {};
    If[FileExistsQ[dataFile], 
        Print["Reading from from " <> dataFile];
        vertices = Get[dataFile],
        Print["Creating 759 vertices"];
        hsVertices = GetHSVertices[];
        newVertex = {0, 0, 0, 0, 2, 0, 0, 2, 0, 0, 0, 0, 0, 2, 2, 0, 0, 0, 2, 2, 2, 0, 2, 0};
        AppendTo[hsVertices, newVertex];
        v311 = Select[ hsVertices, Total[#]==16 &];
        v3AllPerms = Permutations[v311[[1]]];
        vertices = Select[v3AllPerms, ContainsAll[{-32, -16, -8, 0, 8, 16, 32}, Union[Table[hsVertices[[k]].#, {k, 1, Length[hsVertices]}]]]&];
        Save[dataFile, vertices];
        Print["Saved 759 vertices to " <> dataFile];
    ];
    vertices
];

GetAndCacheV3Vertices[vertices759_] := Module[
    {verticesV3, cubeVertices8, i, j, dataFile},
    dataFile = "data/verticesV3.wl";
    verticesV3 = {};
    If[FileExistsQ[dataFile], 
        Print["Reading from from " <> dataFile];
        verticesV3 = Get[dataFile],
        Print["Creating V3 vertices"];
        
        verticesV3 = {};
        cubeVertices8 = cubeVertices[8];

        For[i = 1, i<= Length[vertices759], i++,
            If[Mod[i, 10] == 0, Print[i]];
            v759 = vertices759[[i]];
            nonZeroIndices = Select[Table[k, {k, 24}], v759[[#]] != 0 &];
            For[j = 1, j <= Length[cubeVertices8], j++,
                cubeV = cubeVertices8[[j]];
                If[ Apply[Times, cubeV] == 1,
                    newVertex = Table[0, {k, 24}];
                    newVertex[[nonZeroIndices]] = cubeV * 2;
                    AppendTo[verticesV3, newVertex];
                ];
            ];
        ];

        Save[dataFile, verticesV3];
        Print["Saved V3 vertices to " <> dataFile];
    ];
    verticesV3
];

GetAndCacheV2Vertices[testVertices_] := Module[
    {verticesV2, cubeVertices8, i, j, dataFile},
    dataFile = "data/verticesV2.wl";
    verticesV2 = {};
    If[FileExistsQ[dataFile], 
        Print["Reading from from " <> dataFile];
        verticesV2 = Get[dataFile],
        Print["Creating V2 vertices"];
        
        verticesV2 = {};
        Print["constructing cubeVertices24"];
        cubeVertices24 = cubeVertices[24];
        Print["constructed cubeVertices24: " <> IntegerString[Length[cubeVertices24]]];
        negCounts = {1, 7, 9, 11, 13, 15, 17, 23};
        (* {{1, 24}, {7, 6072}, {9, 12144}, {11, 30912}, {13, 30912}, {15, 12144}, {17, 6072}, {23, 24}} *)
        totals = 24 - 2 * negCounts;
        selectedCubeVertices24 = Select[cubeVertices24, MemberQ[totals, Total[#]] & ];
        Print["constructed selectedCubeVertices24: " <> IntegerString[Length[selectedCubeVertices24]]];
        (* 8.3 million entries *)
        For[i = 1, i <= 24, i++,
            Print["i = " <> IntegerString[i]];
            v2template = Table[1, {k, 24}];
            v2template[[i]] = 3;
            For[j = 1, j <= Length[selectedCubeVertices24], j++,
                If[Mod[j, 100000] == 0, Print[{j, Length[selectedCubeVertices24], Length[verticesV2]}]];
                cubeV = selectedCubeVertices24[[j]];
                newVertex = Table[v2template[[k]] * cubeV[[k]], {k, 24}];
                valid = True;
                For[l = 1, l <= Length[testVertices], l++,
                    inner = newVertex.testVertices[[l]];
                    (* target inner: -32, -16, -8, 0, 8, 16, 32 *)
                    If[inner < -32 || inner > 32 || Mod[inner, 8] != 0 || Abs[inner] == 24,
                        valid = False;
                        Break[];
                    ];
                ];
                If[valid, 
                    AppendTo[verticesV2, newVertex];
                ];
            ];
        ];

        Save[dataFile, verticesV2];
        Print["Saved V2 vertices to " <> dataFile];
    ];
    verticesV2
];

GetAndCacheV1Vertices[] := Module[
    {verticesV1, cubeVertices2, i, j, dataFile, template, templates, t, cubeV, newVertex},
    dataFile = "data/verticesV1.wl";
    verticesV1 = {};
    If[FileExistsQ[dataFile], 
        Print["Reading from from " <> dataFile];
        verticesV1 = Get[dataFile],

        Print["Creating V1 vertices"];
        verticesV1 = {};
        cubeVertices2 = cubeVertices[2];
        template = Join[Table[4, {k, 2}], Table[0, {k, 24 - 2}]];
        templates = Permutations[template];
        For[i = 1, i<= Length[templates], i++,
            t = templates[[i]];
            nonZeroIndices = Select[Table[k, {k, 24}], t[[#]] != 0 &];
            For[j = 1, j <= Length[cubeVertices2], j++,
                cubeV = cubeVertices2[[j]];
                newVertex = Table[0, {k, 24}];
                newVertex[[nonZeroIndices]] = cubeV * 4;
                AppendTo[verticesV1, newVertex];
            ];
        ];

        Save[dataFile, verticesV1];
        Print["Saved V1 vertices to " <> dataFile];
    ];
    verticesV1
];

GetAndCacheAllVertices[] := Module[
    {vertices759, verticesV1, verticesV2, verticesV3, dataFile, vertices},
    dataFile = "data/leech_vertices.wl";
    vertices = {};
    If[FileExistsQ[dataFile], 
        Print["Reading from from " <> dataFile];
        vertices = Get[dataFile],

        Print["Creating all Leech vertices"];
        
        hsVertices = GetHSVertices[];
        newVertex = {0, 0, 0, 0, 2, 0, 0, 2, 0, 0, 0, 0, 0, 2, 2, 0, 0, 0, 2, 2, 2, 0, 2, 0};
        AppendTo[hsVertices, newVertex];

        vertices759 = GetAndCache759Vertices[];
        Print[vertices759//Length];

        verticesV3 = GetAndCacheV3Vertices[vertices759];
        Print[verticesV3//Length];

        verticesV2 = GetAndCacheV2Vertices[hsVertices];
        Print[verticesV2//Length];

        verticesV1 = GetAndCacheV1Vertices[];
        Print[verticesV1//Length];

        vertices = Union[hsVertices, verticesV3, verticesV2, verticesV1];

        Save[dataFile, vertices];
        Print["Saved all Leech vertices to " <> dataFile];
    ];
    vertices
];

vertices = GetAndCacheAllVertices[];
Print[vertices//Length];


(* vertices = GetHSVertices[];
newVertex = {0, 0, 0, 0, 2, 0, 0, 2, 0, 0, 0, 0, 0, 2, 2, 0, 0, 0, 2, 2, 2, 0, 2, 0};
AppendTo[vertices, newVertex];

v311 = Select[ vertices, Total[#]==16 &];
Flatten[Table[v311[[i]].v311[[j]], {i, 1, Length[v311]}, {j, 1, Length[v311]}], 1]//Tally


v3AllPerms = Permutations[v311[[1]]];
v3Candidates = Select[v3AllPerms, ContainsAll[{-32, -16, -8, 0, 8, 16, 32}, Union[Table[vertices[[k]].#, {k, 1, Length[vertices]}]]]&];
Print[v3Candidates//Length];
 *)(* 759 *)



(* newVertices = Complement[v3Candidates, vertices];

AppendTo[vertices, newVertices[[1]]];
newVertices = Drop[newVertices, 1];
v3Candidates = Select[v3Candidates, ContainsAll[{-32, -16, -8, 0, 8, 16, 32}, Union[Table[vertices[[k]].#, {k, 1, Length[vertices]}]]]&];

Print[v3Candidates//Length];
 *)

(* 1111 *)

(* Print[v311]; *)
