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
roundPrecision = 0.000001;
normalizeEdge[edge_] := Round[ N[
    If[Norm[edge[[1]]-edge[[2]]] == 0, 
        {0,0,0}, 
     (edge[[1]]-edge[[2]])/Norm[edge[[1]]-edge[[2]]] 
    ]], roundPrecision];


normalizedEdges = Map[normalizeEdge, edges];

normalizedTally = Tally[normalizedEdges];
Print[Sort[ normalizedTally, #1[[2]]>#2[[2]]&][[1;;10]]];
topNormEdge = Sort[ normalizedTally, #1[[2]]>#2[[2]]&][[1]][[1]];
selectedIndices = Select[Range[Length[edges]], normalizeEdge[edges[[#]]] == topNormEdge & ];
(* count: 2713 *)

selectedEdges = edges[[selectedIndices]];
selectedEdgesCount8d = Total[Map[edges3dMap[#]& , selectedEdges]];

segments3dMap = <||>;
SegmentIncrement = 0;
startTime = AbsoluteTime[];

For[edgeIndex = 1, edgeIndex <= 30 && edgeIndex <= Length[selectedIndices], edgeIndex ++,
    If[Mod[edgeIndex, 10] == 0,
        elapsed = Round[AbsoluteTime[] - startTime];
        Print[{edgeIndex, Length[selectedIndices], elapsed, TextString[Now], Length[segments3dMap], Total[Values[segments3dMap]],
            TextString[DateObject[startTime + elapsed/edgeIndex * Length[selectedIndices]]]
            }]
    ];

    edge1 = edges[[ selectedIndices[[edgeIndex]] ]];
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

Print[{ Length[segments3dMap], Total[Values[segments3dMap]], selectedEdgesCount8d, Total[Values[segments3dMap]]- selectedEdgesCount8d, SegmentIncrement}];

segments = Keys[segments3dMap];
appearedVerticesInSegments = Union[Flatten[segments, 1]];
Print[{Length[appearedVerticesInSegments], 1057}];

Save["data/leech_3d_segments_tally.wl", segments3dMap];
