#!/usr/bin/env wolframscript

edges8dMap = Get["data/leech_8d_edges.wl"];
unique8dEdges = Keys[edges8dMap];
Print[{Length[edges8dMap], Total[Values[edges8dMap]]}];
(* {2185801, 5081469120} *)

phi = (Sqrt[5] + 1)/2;
projection8to3 = {
    {phi, phi, 0, 0, 1, -1, 0, 0},
    {1, -1, phi, phi, 0, 0, 0, 0},
    {0, 0, 1, -1, phi, phi , 0, 0}
};

randomVec = {0.318317, 0.257591, 0.554799};
edges3dMap = <||>;

get3dEdge[edge8d_] := Module[
    {},
    edge3d = Map[Expand[#.Transpose[projection8to3]] &, edge8d];
    If[edge3d[[1]].randomVec > edge3d[[2]].randomVec, Reverse[edge3d], edge3d]
];

For[i = 1, i <= Length[unique8dEdges], i++,
    If[Mod[i, 10000]==0, Print[{i, Length[unique8dEdges]}]];
    edge8d = unique8dEdges[[i]];
    count8d = edges8dMap[edge8d];
    edge3d = get3dEdge[edge8d];
    If[KeyExistsQ[edges3dMap, edge3d],
        edges3dMap[edge3d] = edges3dMap[edge3d] + count8d,
        edges3dMap[edge3d] = count8d
    ];
];

Print[{Length[edges3dMap], Total[Values[edges3dMap]]}];
(* 373625, 5081469120 *)
edges = Keys[edges3dMap];
appearedVertices = Union[Flatten[edges, 1]];
Print[{Length[appearedVertices], 1057}];

Save["data/leech_3d_edges_tally.wl", edges3dMap];
