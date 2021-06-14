#!/usr/bin/env wolframscript

mergeMap[map_, key_, value_] := Module[
    { result },
    result = map;
    If[KeyExistsQ[result, key],
        result[key] = result[key] + value,
        result[key] = value
    ];
    result
];

edges3dMap = Get["data/leech_3d_edges_tally.wl"];
Print[{Length[edges3dMap], Total[Values[edges3dMap]]}];
(* 373625, 5081469120 *)
edges = Keys[edges3dMap];
appearedVertices = Union[Flatten[edges, 1]];
Print[{Length[appearedVertices], 1057}];
vertexCount = Length[appearedVertices];

randomVec = {0.318317, 0.257591, 0.554799};

segments3dMap = <||>;
SegmentIncrement = 0;

For[edgeIndex = 1, edgeIndex <= 30 && edgeIndex <= Length[edges], edgeIndex ++,
    If[Mod[edgeIndex, 20] == 0,
        Print[{edgeIndex, Length[edges], Length[segments3dMap], Total[Values[segments3dMap]]}]
    ];
    edge1 = edges[[edgeIndex]];
    tSolution = Solve[ t > 0 && t < 1 && Apply[Or, Table[t edge1[[1]] + (1 - t) edge1[[2]] == appearedVertices[[k]], {k, 1, vertexCount}]], {t}];
    If[Length[tSolution] > 0,
        Print[{ edgeIndex, Length[tSolution]}];
        breakPoints = Sort[ Expand[Simplify[t edge1[[1]] + (1 - t) edge1[[2]]/.tSolution]], #1.randomVec < #2.randomVec &];
        bpCount = Length[breakPoints];
        SegmentIncrement += bpCount * edges3dMap[edge1];
        segments3dMap = mergeMap[segments3dMap, {edge1[[1]], breakPoints[[1]]}, edges3dMap[edge1]];
        segments3dMap = mergeMap[segments3dMap, {breakPoints[[bpCount]], edge1[[2]]}, edges3dMap[edge1]];
        For[bpIndex = 1, bpIndex <= bpCount - 1, bpIndex++,
            segments3dMap = mergeMap[segments3dMap, {breakPoints[[bpIndex]], breakPoints[[bpIndex + 1]]}, edges3dMap[edge1]];    
        ];
        ,
        segments3dMap = mergeMap[segments3dMap, edge1, edges3dMap[edge1]];
        segments3dMap[edge1] = edges3dMap[edge1]
    ];
];

Print[{ Length[segments3dMap], Total[Values[segments3dMap]], Total[Values[edges3dMap]], Total[Values[segments3dMap]]- Total[Values[edges3dMap]], SegmentIncrement}];

segments = Keys[segments3dMap];
appearedVerticesInSegments = Union[Flatten[segments, 1]];
Print[{Length[appearedVerticesInSegments], 1057}];

Save["data/leech_3d_segments_tally.wl", segments3dMap];
