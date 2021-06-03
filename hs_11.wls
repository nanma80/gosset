#!/usr/bin/env wolframscript

dataFolder = "data";
refShape = "polytope_3_3_5";
refDataFileName = FileNameJoin[{dataFolder, refShape <> ".wl"}];

exportToPov = True;
lighting = {{"Point", White, {10, -10, 10}}};
rangeFactor = 0.78;
thickness = 0.005;
range = 3;
imageFolder = "output";
imageSize = {4, 3} * 240;
epsilon = 0.0000001;
truncateRatioDenominator = 120;
limit = -1; (* no sampling *)
(* limit = 10000; *)


start = AbsoluteTime[];
Needs["POVRayRender`"];
ConfigurePOVRayRender["POVRayPath" -> "C:\\Program Files\\POV-Ray\\v3.7\\bin\\pvengine64.exe"];

projectionMatrix = {
    Table[0, {k, 1, 24}],
    Table[0, {k, 1, 24}],
    Table[0, {k, 1, 24}]
};

(* from http://www.madore.org/~david/math/higman-sims-graph.html *)
elevenCycle = {
    {0}, {1, 0}, {1, 1}, {1, 8},
    {0}, {2, 0}, {2, 3}, {2, 10},
    {2, 8}, {1, 2}, {1, 9}, {1, 6}, 
    {1, 3}, {2, 9}, {2, 5}, {2, 2}, 
    {2, 7}, {1, 10}, {1, 5}, {1, 7}, 
    {1, 4}, {2, 1}, {2, 4}, {2, 6}
};

t = 0;
renormer = Sqrt[11];
For[i = 0, i < 24, i++,
    If[elevenCycle[[i + 1]][[1]] == 0,
        projectionMatrix[[1]][[i+1]] = 0;
        projectionMatrix[[2]][[i+1]] = 0
    ];

    If[elevenCycle[[i + 1]][[1]] == 1,
        projectionMatrix[[1]][[i+1]] = Cos[2 * Pi * (elevenCycle[[i + 1]][[2]] / 11 + t)]/renormer;
        projectionMatrix[[2]][[i+1]] = -Sin[2 * Pi * (elevenCycle[[i + 1]][[2]] / 11 + t)]/renormer
    ];

    If[elevenCycle[[i + 1]][[1]] == 2,
        projectionMatrix[[1]][[i+1]] = Cos[2 * Pi * (elevenCycle[[i + 1]][[2]] / 11 - t)]/renormer;
        projectionMatrix[[2]][[i+1]] = -Sin[2 * Pi * (elevenCycle[[i + 1]][[2]] / 11 - t)]/renormer
    ];
];

projectionMatrix = projectionMatrix * 1.0;


dataFileName = "leech_min_norm_vectors.wl";
minNormVectors = Get[dataFileName];

v2=Select[ minNormVectors, Min[#]==-3 && Total[#]==20 &];
(* Print[v2]; *)
v3=Select[ minNormVectors, Total[#]==16 &];


v1all=Select[ minNormVectors, Total[Abs[#]]==8 &];
v1all//Length
(* 1104 = 24*23/2 * 4 *)
Tally[Map[Sort[Tally[#]] &, Abs[v1all]]]
(* 0 ^ 22, 4 ^ 2, all perms and all +/- *)

v3all=Select[ minNormVectors, Total[Abs[#]]==16 &];
v3all//Length
(* 97152 = 759 * 128 = (23 * 11 * 3) * 128 *)
(* total: -16, -8, 0, 8, 16 *)

v2all = Select[minNormVectors, Total[Abs[#]] == 26&];
(* 98304 = 24 * 4096 *)
(* total: -20, -12, -4, 4, 12, 20 *)
Tally[Map[Sort[Tally[#]] &, Abs[v2all]]]
(* 1^23, 3^1 *)

v2negIndices=Table[ Select[Table[k, {k, 1, 24}], v2all[[i]][[#]]<0 & ], {i, Length[v2all]}];
Tally[v2negIndices]//Length
(* 98304. The signs are different across v2all *)

Abs[v3all]//Tally//Length
(* 759 *)
Abs[v2all]//Tally//Length
(* 24 *)

(* only inner products: -32, -16, -8, 0, 8, 16, 32 *)

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

hsVertices = GetHSVertices[];

vertices759 = GetAndCache759Vertices[];
Print[vertices759//Length];

verticesV3 = GetAndCacheV3Vertices[vertices759];
Print[verticesV3//Length];


vertices = Union[hsVertices, verticesV3];
Print[vertices//Length];

(* v311 = Select[ vertices, Total[#]==16 &]; *)
(* 77 *)
(* 
In[39]:= Total[v311]                                                                                                                  
Out[39]= {154, 42, 42, 42, 154, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42}
 *)

(* v3filt = Select[v3, #[[1]] == 2 && #[[5]] == 2 &];
(* let's make v3filt to match v311 *)
Intersection[v3filt, v311]//Length
Select[v311, #[[1]] == 2 && #[[5]] == 2 &]//Length

Flatten[Table[Select[v311, #[[i]] == 2 && #[[j]] == 2 && #[[k]] == 2 &]//Length, {i, 24}, {j, 24}, {k, 24}], 1]

Map[Sort[Tally[#]]&, Table[v311[[i]].v311[[j]], {i, 1, Length[v311]}, {j, 1, Length[v311]}]]//Tally
Map[Sort[Tally[#]]&, Table[v3filt[[i]].v3filt[[j]], {i, 1, Length[v3filt]}, {j, 1, Length[v3filt]}]]//Tally
(* the inner product structure is the same. all vectors in v3 play the same role *)

Flatten[Table[v311[[i]].v311[[j]], {i, 1, Length[v311]}, {j, 1, Length[v311]}], 1]//Tally
Flatten[Table[v3[[i]].v3[[j]], {i, 1, Length[v3]}, {j, 1, Length[v3]}], 1]//Tally
Intersection[v3, v311]//Length
Map[#[[{1, 2, 3, 4, 5, 12, 20, 22}]]&, Sort[v3]]

Intersection[Map[#[[{1, 2, 3, 4, 5, 12, 20, 22, 6, 7, 8, 9, 10, 11, 13, 14, 15, 16, 17, 18, 19, 21, 23, 24}]]&, Sort[v3]], v311]//Length


v3perm = Map[Permute[#, Cycles[{{6, 12}, {7, 20}, {8, 22}}]]&, v3];
v3perm = Map[Permute[#, Cycles[{{4, 5}, {15, 16}, {17, 19}, {18, 22}}]]&, v3perm ];

Intersection[v3perm , v311]//Length

(v311//Sort)[[76]]
(v3perm//Sort)[[758]]

Permute[ {2, 2, 2, 2, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 2, 0, 2, 2, 0, 0, 0, 0, 0, 0}, Cycles[{{4, 5}, {15, 16}, {17, 19}, {18, 22}}] ]
 *)

viewPoint = {0, 0, 1};

exportImage[transitionStep_, angle_, serialNumber_, frameFolder_] := Module[
    {verticesOnEdges, verticesTally, cropCommand,
        graph, outputFolder, outputFileName},

    theta = angle / 180 * Pi/2;
    coefficients = Table[0, {k, 1, 6}];
    coefficients[[transitionStep]] = Cos[theta];
    coefficients[[transitionStep + 1]] = Sin[theta];
    (* projectionMatrix = ArrayFlatten[{Table[projectionMatrixB4 * coefficients[[k]], {k, 1, 6}]}] * 1.0; *)

    vertices3d = FullSimplify[vertices.Transpose[projectionMatrix]];
    matrixNorm = Norm[projectionMatrix[[1]]];

    maxNorm = Max[Map[Norm, vertices3d]];
    Print[maxNorm//N];
    Print[matrixNorm//N];
    (* Print[matrixNorm//N]; *)
    (* matrix norm is always Sqrt[2] *)
    (* maxNorm = 7.4; *)

    projectTo3d[vertex_] := vertex/maxNorm;
    range = rangeFactor;

    thicknessRatio = 0.005;
    thickness = thicknessRatio * range;

    verticesTally = Tally[Expand[vertices3d]];
    Print["Finished tallying " <> IntegerString[verticesTally//Length]];
    tallyTally = Tally[Map[#[[2]]&, verticesTally]];
    Print["Distribution of tally counts:"];
    Print[Sort[tallyTally]];

    graph = Graphics3D[
        Map[{
            Opacity[1], 
            FaceForm[Hue[Mod[ #[[2]] - 1 , 5]/5, 1, 1 ]], 
            Sphere[projectTo3d[#[[1]]], thickness * (#[[2]])^(1/8) ]
            }&, verticesTally],
        ViewPoint -> viewPoint * 3000,
        PlotRange -> {{-range, range}, {-range, range}, {-range, range}},
        Lighting -> lighting
    ];

    outputFolder = FileNameJoin[{imageFolder, frameFolder}];
    If[!DirectoryQ[outputFolder], CreateDirectory[outputFolder]];

    outputFileName = "animation_frame_" <> IntegerString[serialNumber, 10, 4] <> ".png";
    outputFileName = FileNameJoin[{outputFolder, outputFileName}];

    cropCommand = "mogrify -crop " <> IntegerString[imageSize[[2]]] <> "x" <> IntegerString[imageSize[[2]]] <> "+" <> IntegerString[(imageSize[[1]]-imageSize[[2]])/2] <> "+0 " <> outputFileName;

    If[exportToPov,
        If[FileExistsQ[outputFileName], DeleteFile[outputFileName]];
        POVRayRender[graph, Method -> "Triangle", OutputPath -> outputFileName, ImageSize -> imageSize, RenderOptions -> "+A0.001 -J +UA"];
        Run[cropCommand];
        Print[cropCommand]
        ,
        Export[outputFileName, graph];
    ]
    Print["Saved the image to " <> outputFileName];
];

exportAnimation[] := Module[
    {
        shape, frameFolder, gifFileName, gifFolder, outputFolder, maxTheta,
        thetaStep, epsilon, thetas, theta, gifCommand
    },

    shape = "higman_sims";
    frameFolder = shape <> "_11_truncate_frames";
    gifFileName = shape <> "_11";
    gifFolder = FileNameJoin[{imageFolder, shape}];
    outputFolder = FileNameJoin[{imageFolder, frameFolder}];
    If[!DirectoryQ[outputFolder], CreateDirectory[outputFolder]];
    If[!DirectoryQ[gifFolder], CreateDirectory[gifFolder]];

    frameIndex = 1;
    For[stepIndex = 1, stepIndex <= 1, stepIndex ++,
        For[angle = 0, angle <= 180, angle += 1,
            Print["Frame #" <> IntegerString[frameIndex] <> " step: " <> IntegerString[stepIndex] <> " double angle: " <> IntegerString[angle]];
            If[Mod[angle, 1] == 0,
                exportImage[stepIndex, angle, frameIndex, frameFolder];
                frameIndex ++;
            ];
        ];
    ];

    gifCommand = "magick convert -quiet +repage -alpha remove -loop 0 -delay " <> IntegerString[4] <> " -dispose 2 " <> outputFolder <> "\\*animation*.png " <> gifFolder <> "\\" <> gifFileName <> ".gif";

    Print[gifCommand];
    Run[gifCommand];
    Print["Saved the animation to " <> gifFolder <> "\\" <> gifFileName <> ".gif"];

    mp4Command = "ffmpeg -i " <> gifFolder <> "\\" <> gifFileName <> ".gif" 
        <> " -movflags faststart -pix_fmt yuv420p -vf \"scale=trunc(iw/2)*2:trunc(ih/2)*2\" "
        <> gifFolder <> "\\" <> gifFileName <> ".mp4";

    mp4FileName = gifFolder <> "\\" <> gifFileName <> ".mp4";
    If[FileExistsQ[mp4FileName], DeleteFile[mp4FileName]];
    Print[mp4Command];
    Run[mp4Command];
    Print["Saved the video to " <> gifFolder <> "\\" <> gifFileName <> ".mp4"];
];

(* exportAnimation[]; *)
exportImage[1, 0, 3, "test"];


end = AbsoluteTime[];
Print["Elapsed time in seconds: " <> ToString[end - start]];
