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
roundPrecision = 0.000001;
randomVec = {0.318317, 0.257591, 0.554799};

edges3dMap = Get["data/leech_3d_edges_tally.wl"];
Print[{Length[edges3dMap], Total[Values[edges3dMap]]}];
(* 373625, 5081469120 *)
edges = Keys[edges3dMap];
appearedVertices = Union[Flatten[edges, 1]];
Print[{Length[appearedVertices], 1057}];
vertexCount = Length[appearedVertices];

normalizeEdge[edge_] := Round[ N[
    If[Norm[edge[[1]]-edge[[2]]] == 0, 
        {0,0,0}, 
     (edge[[1]]-edge[[2]])/Norm[edge[[1]]-edge[[2]]] 
    ]], roundPrecision];


(* normalizedEdges = Map[normalizeEdge, edges];

normalizedTally = Tally[normalizedEdges];
Print[Sort[ normalizedTally, #1[[2]]>#2[[2]]&][[1;;10]]];
topNormEdge = Sort[ normalizedTally, #1[[2]]>#2[[2]]&][[1]][[1]];
selectedIndices = Select[Range[Length[edges]], normalizeEdge[edges[[#]]] == topNormEdge & ];
Print[Length[selectedIndices]];

tallyTally = Tally[Map[#[[2]]&, normalizedTally]]//Sort//Reverse;
(* 
{{2713, 6}, {2026, 10}, {1450, 15}, {997, 1}, {732, 60}, {656, 30}, {552, 30}, {516, 60}, {460, 30}, {278, 30},
 {264, 60}, {260, 60}, {244, 30}, {210, 120}, {200, 60}, {182, 120}, {150, 120}, {136, 60}, {120, 60}, {80, 180},
 {70, 240}, {66, 120}, {60, 180}}
  *)
selectedEdges = edges[[selectedIndices]];
selectedEdgesCount8d = Total[Map[edges3dMap[#]& , selectedEdges]];
 *)

segments3dMap = <||>;
SegmentIncrement = 0;
startTime = AbsoluteTime[];
(* limit = Length[selectedIndices]; *)
limit = Length[edges];
(* limit = 300; *)
Print["starting the loop"];

(* For[edgeIndex = 1, edgeIndex <= limit && edgeIndex <= Length[selectedIndices], edgeIndex ++, *)
For[edgeIndex = 1, edgeIndex <= limit && edgeIndex <= Length[edges], edgeIndex ++,
    If[Mod[edgeIndex, 100] == 0,
        elapsed = Round[AbsoluteTime[] - startTime];
(*         Print[{edgeIndex, Length[selectedIndices], elapsed, TextString[Now], Length[segments3dMap], Total[Values[segments3dMap]],
            TextString[DateObject[startTime + elapsed/edgeIndex * Length[selectedIndices]]], elapsed/edgeIndex * Length[selectedIndices] * 1.0
            }]
 *)
        Print[{edgeIndex, Length[edges], elapsed, TextString[Now], Length[segments3dMap], Total[Values[segments3dMap]],
            TextString[DateObject[startTime + elapsed/edgeIndex * Length[edges]]], elapsed/edgeIndex * Length[edges] * 1.0
            }]
    ];

    edge1 = edges[[edgeIndex]];
    (* edge1 = edges[[ selectedIndices[[edgeIndex]] ]]; *)

    (* tSolution = Solve[ t > 0 && t < 1 && Apply[Or, Table[t edge1[[1]] + (1 - t) edge1[[2]] == appearedVertices[[k]], {k, 1, vertexCount}]], {t}]; *)
    (* candidates = Select[Range[vertexCount], Abs[Det[ Join[edge1 * 1.0, appearedVertices[[{#}]] * 1.0] ] ] < 0.0001 &]; *)
    (* candidates = Select[Range[vertexCount], Abs[Det[ Join[edge1 * 1.0, appearedVertices[[{#}]] * 1.0] ] ] < 0.0001 && (appearedVertices[[#]] - edge1[[1]]).(appearedVertices[[#]] - edge1[[2]]) < 0 &]; *)
    (* tSolution = Solve[ t > 0 && t < 1 && Apply[Or, Table[t edge1[[1]] + (1 - t) edge1[[2]] == appearedVertices[[candidates[[k]]]], {k, 1, Length[candidates] }]], {t}]; *)

    candidates = Select[Range[vertexCount], 
        Abs[Det[ Join[edge1 * 1.0, appearedVertices[[{#}]] * 1.0] ] ] < roundPrecision 
        && (appearedVertices[[#]] - edge1[[1]]).(appearedVertices[[#]] - edge1[[2]]) < 0 
        && Abs[Norm[edge1[[1]] - edge1[[2]] * 1.0 ] - Norm[ appearedVertices[[#]] - edge1[[1]]* 1.0 ] - Norm[ appearedVertices[[#]] - edge1[[2]]* 1.0 ]] < roundPrecision 
        &
        ];

    If[Length[candidates] > 0,
        (* Print[{ edgeIndex, Length[candidates]}]; *)
        (* breakPoints = Sort[ Expand[Simplify[t edge1[[1]] + (1 - t) edge1[[2]]/.tSolution]], #1.randomVec < #2.randomVec &]; *)
        breakPoints = Sort[ appearedVertices[[candidates]], #1.randomVec < #2.randomVec &];

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
Print[Round[AbsoluteTime[] - startTime]];
(* Print[{ Length[selectedIndices], Length[segments3dMap], Total[Values[segments3dMap]], selectedEdgesCount8d, Total[Values[segments3dMap]]- selectedEdgesCount8d, SegmentIncrement}]; *)
Print[{ edgeIndex-1, Length[segments3dMap], Total[Values[segments3dMap]], Total[Values[edges3dMap]], Total[Values[segments3dMap]]- Total[Values[edges3dMap]], SegmentIncrement}];


segments = Keys[segments3dMap];
appearedVerticesInSegments = Union[Flatten[segments, 1]];
Print[{Length[appearedVerticesInSegments], 1057}];

Save["data/leech_3d_segments_tally.wl", segments3dMap];

(* 
1:
2713 -> 826 
{826, 216267754, 138575552, 77692202, 236951040}

7:
2026 -> 756

17:
1450 -> 696

800:
150 -> 150

1682:
60 -> 60

*)

(* 
benchmark:
17, 1450, limit = 100
original: estimate at 100: 986
replace mergeMap inline: 971. no diff
get candidate indices by computing Det first: 188. definitely use
filter by Det and also inner prod: 58

remove solve: 43. best

*)