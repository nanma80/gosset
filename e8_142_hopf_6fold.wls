#!/usr/bin/env wolframscript

(* https://www.physast.uga.edu/~mgeller/JPA34p10243.pdf *)

imageFolder = "output";
imageSize = 240 * {4, 3};
exportToPov = True;
showVertices = True;
showEdges = True;
showFaces = True;
transparentFaces = False;
topView = False;
roundPrecision = 0.000001;
onlyOneFiber = False;
fiberIndex = 6;
plotPoints = 30;
maxRecursion = 1;
showEllipsoids = False;

Needs["Quaternions`"];
Needs["POVRayRender`"];
ConfigurePOVRayRender["POVRayPath" -> "C:\\Program Files\\POV-Ray\\v3.7\\bin\\pvengine64.exe"];

colors = Join[{Red, Blue, Green, Yellow, Magenta, Cyan, Brown, Orange, Pink, Gray}, RandomColor[80]];
reflect[point_, mirror_] := point - 2 * (point.mirror)/(mirror.mirror)*mirror;
cubeVertices[dimension_] := 2 * Map[IntegerDigits[#, 2][[2;;dimension+1]]&, Table[k, {k, 2^dimension, 2 * 2^dimension - 1}]] - 1;
sameVertex[point1_, point2_] := point1 == point2;
sameCenter[edge1_, edge2_] := Total[edge1] == Total[edge2];
dedupEdges[edges_] := Union[edges, SameTest -> sameCenter];
dedupVertices[vs_] := Union[vs, SameTest -> sameVertex];
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

get142Vertices[] := Module[
    {vertices, templates, i, cube8, class3},
    demicube8 = demicubeVertices[8, False];
    cube5 = Map[Join[#, {0, 0, 0}]&, cubeVertices[5]];
    class1BeforePerm = {};
    
    template = {4, 2, 2, 2, 2, 0, 0, 0};
    For[j = 1, j <= Length[cube5], j++,            
        newVertex = Table[template[[k]] * cube5[[j]][[k]], {k, 8}];
        AppendTo[class1BeforePerm, newVertex];
    ];

    class1Permute = Map[Permutations, class1BeforePerm];
    class1 = Union[Flatten[class1Permute, 1]];

    class2 = demicube8 * 2;

    templates = Permutations[{5, 1, 1, 1, 1, 1, 1, 1}];

    class3 = {};
    For[i = 1, i <= Length[templates], i++,
        For[j = 1, j <= Length[demicube8], j++,
            newVertex = Table[templates[[i]][[k]] * demicube8[[j]][[k]], {k, 8}];
            AppendTo[class3, newVertex]
        ];
    ];

    templates = Permutations[{3, 3, 3, 1, 1, 1, 1, 1}];

    class4 = {};
    For[i = 1, i <= Length[templates], i++,
        For[j = 1, j <= Length[demicube8], j++,
            newVertex = Table[templates[[i]][[k]] * demicube8[[j]][[k]], {k, 8}];
            AppendTo[class4, newVertex]
        ];
    ];

    vertices = Join[class1, class2, class3, class4];
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

    (* Print[edgeLength]; *)

    For[i = 1, i <= Length[vertices]-1, i++, 
        For[j = i + 1, j <= Length[vertices], j++, 
            If[ Abs[ Norm[1.0 vertices[[i]] - vertices[[j]]] - edgeLength ] < roundPrecision, AppendTo[edges, {i, j}] ]   
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
getScaleFactor[groupIndex_] := (1 + 0.01 * groupIndex / 10);
doubleRotationMatrix[theta_] := ArrayFlatten[{{RotationMatrix[theta], 0}, {0, RotationMatrix[theta]}}];
projectEllipsoid[originalEllipsoid_, removeDims_] := Module[
    {},
    keepDims = Complement[Range[Length[originalEllipsoid]], removeDims];
    keepKeep = Map[#[[keepDims]]&, originalEllipsoid[[keepDims]]];
    keepRemove = Map[#[[removeDims]]&, originalEllipsoid[[keepDims]]];
    removeKeep = Map[#[[keepDims]]&, originalEllipsoid[[removeDims]]];
    removeRemove = Map[#[[removeDims]]&, originalEllipsoid[[removeDims]]];

    projectedEllipsoid = keepKeep - keepRemove.Inverse[removeRemove].removeKeep;
    projectedEllipsoid
];

vertices = get142Vertices[];
Print[Length[vertices]];
(* 17280 *)

edgeLength = 2 Sqrt[2];
For[i = 1, i < 30, i++,
    vIndex = RandomInteger[{1, Length[vertices]}];
    v1 = vertices[[vIndex]];
    neighbors = Select[vertices, Abs[ Norm[N[# - v1]] - edgeLength ] < roundPrecision & ];
    If[Length[neighbors] != 483840*2/17280 || True,
        Print[{Length[neighbors], vIndex, v1}];
    ];
];

vertices24Cell = get24CellVertices[];

complexes = Map[getComplexNumbers, vertices];

s4s = Map[getS4, complexes];

q2s = Map[getQ2, vertices];

s3s = Map[getS3, q2s];

Print[Map[#[[2]]&, Tally[s4s]]//Tally];
Print[Map[#[[2]]&, Tally[s3s]]//Tally];
(* demicube with True: *)
(* {{16, 152}, {24, 192}, {8, 1280}} *)
(* {{32, 64}, {48, 96}, {16, 664}} *)

(* demicube with False: *)
(* {{16, 128}, {24, 592}, {8, 128}} *)
(* {{32, 64}, {48, 288}, {24, 16}, {16, 64}} *)

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
];

phi = (Sqrt[5] + 1)/2;

projection8to4Original = {
    {phi, phi, 0, 0, 1, -1, 0, 0},
    {1, -1, phi, phi, 0, 0, 0, 0},
    {0, 0, 1, -1, phi, phi , 0, 0},
    {0, 0, 0, 0, 0, 0, phi+1, phi-1}
};

permutations = Permutations[Range[8]];
permIndex = 112;
projection8to4Permed = Transpose[Transpose[projection8to4Original][[permutations[[permIndex]]]]];
projection4to3 = IdentityMatrix[4][[{1,2,3}]];
(* projection4to3 = Join[IdentityMatrix[4][[{1,2}]], {{0, 0, 0, 0}}]; *)
ellipsoidMatrices = Get["data/perm112_ellipsoidMatrices.wl"];
removeDims = {4};
ellipsoidMatrices3d = Map[projectEllipsoid[#, removeDims]&, ellipsoidMatrices]//N;

exportImage[theta_, serialNumber_, frameFolder_] := Module[
    {
        cropCommand, graph, rotation4d, projection8to4, vertices4d, vertexGroups4d, projectionMatrix, 
        vertexGroups3d, edgeGroups3d, faceGroups3d, viewPoint, range, thickness, lighting, graphElements,
        groupIndex
    },

    rotation4d = doubleRotationMatrix[theta].{
        {1, 0, 0, 0},
        {0, phi, 1/phi, 0}/Sqrt[3],
        {0, -1/phi, phi, 0}/Sqrt[3],
        {0, 0, 0, 1}
    };

    projection8to4 = rotation4d.projection8to4Permed;

    vertices4d = Map[projection8to4.# &, vertices]//Simplify;
    vertexGroups4d = Map[projection8to4.# &, vertexGroups, {2}];

    projectionMatrix = projection4to3.projection8to4//N;

    vertexGroups3d = Round[ Map[projectionMatrix.# &, vertexGroups, {2}], roundPrecision];
    edgeGroups3d = Round[ Map[projectionMatrix.# &, edgeGroups, {3}], roundPrecision];
    faceGroups3d = Round[ Map[projectionMatrix.# &, faceGroups, {3}], roundPrecision];
    vertexGroups3d = Map[dedupVertices, vertexGroups3d];
    edgeGroups3d = Map[dedupEdges, edgeGroups3d];
    faceGroups3d = Map[dedupEdges, faceGroups3d];
(*     Print[Map[Length, vertexGroups3d]];
    Print[Map[Length, edgeGroups3d]];
    Print[Map[Length, faceGroups3d]];
 *)
    viewPoint = If[topView, {0, 0, 10}, {1, -1, 0.5}] * 10;
    viewPoint = viewPoint/Norm[viewPoint] * 20;

    range = If[showEllipsoids, 7.4, 8.5];
    thickness = If[showEllipsoids, 0.16, 0.1];
    lighting = {{"Point", White, {10, -10, 10}}};

    graphElements = {};

    For[groupIndex = 1, groupIndex <= Length[vertexGroups], groupIndex++,
        If[showVertices,
            AppendTo[graphElements, {
                Opacity[1],
                FaceForm[colors[[groupIndex]]],
                (* FaceForm[Darker[colors[[groupIndex]], 0.999]], *)
                Sphere[vertexGroups3d[[groupIndex]] * getScaleFactor[groupIndex], thickness]
            }]
        ];

        If[showEdges && If[onlyOneFiber, groupIndex == fiberIndex, True],
            AppendTo[graphElements, {
                Opacity[1],
                FaceForm[colors[[groupIndex]] ],
                (* FaceForm[Darker[colors[[groupIndex]], 0.999]], *)
                Cylinder[edgeGroups3d[[groupIndex]] * getScaleFactor[groupIndex], thickness * 0.8 * getScaleFactor[groupIndex]]
            }]
        ];

        If[showFaces && If[onlyOneFiber, groupIndex == fiberIndex, True],
            AppendTo[graphElements, {
                Opacity[If[transparentFaces, 0.5, 1]],
                FaceForm[colors[[groupIndex]] ],
                Map[Polygon, faceGroups3d[[groupIndex]] * getScaleFactor[groupIndex]]
            }]
        ];
    ];

    graph1 = Graphics3D[
            graphElements,
            ViewPoint -> viewPoint,
            PlotRange -> {{-range, range}, {-range, range}, {-range, range}},
            Lighting -> lighting
        ];


    If[showEllipsoids,
        contourPlots = Table[ContourPlot3D[
                {{x,y,z}}.ellipsoidMatrices3d[[groupIndex]].Transpose[{{x,y,z}}] == 1, 
                {x, -range, range}, {y, -range, range}, {z, -range, range},
                PlotPoints -> plotPoints, 
                MaxRecursion -> maxRecursion,
                MeshFunctions -> {0 &}, 
                MeshShading -> {{ colors[[groupIndex]] , Opacity[0.5] }}
            ], {groupIndex, 1, Length[ellipsoidMatrices3d]}];
        graph = Apply[Show, Join[contourPlots, {graph1}]],
        graph = graph1
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
        shape, frameFolder, gifFileName, gifFolder, outputFolder, angle,
        frameIndex, gifCommand, delay
    },

    shape = "e8_142";
    gifFileName = shape 
        <> If[showEllipsoids, "_with_ellipsoids", ""] 
        <> If[showFaces, "_with_faces", ""] 
        <> If[transparentFaces, "_transparent_faces", ""] 
        <> If[topView, "_topview", ""] 
        <> If[onlyOneFiber, "_fiber_" <> IntegerString[fiberIndex], ""] 
        <> "_hopf_6fold";
    frameFolder = gifFileName <> "_frames";
    gifFolder = FileNameJoin[{imageFolder, shape}];
    outputFolder = FileNameJoin[{imageFolder, frameFolder}];
    If[!DirectoryQ[outputFolder], CreateDirectory[outputFolder]];
    If[!DirectoryQ[gifFolder], CreateDirectory[gifFolder]];

    frameIndex = 1;
    angleStep = 0.5;
    (* angleStep = 10; *)
    For[angle = 0, angle <= 60 - angleStep, angle += angleStep,
        Print["Frame #" <> IntegerString[frameIndex] <> " angle: " <> TextString[angle]];
        (* If[Mod[angle, 1] == 0, *)
            exportImage[(angle + angleStep/2) / 180 * Pi, frameIndex, frameFolder];
        (* ]; *)
        frameIndex ++;
    ];

    delay = If[angleStep <= 2, 3, Floor[500/frameIndex]];
    gifCommand = "magick convert -quiet +repage -alpha remove -loop 0 -delay " <> IntegerString[delay] <> " -dispose 2 " <> outputFolder <> "\\*animation*.png " <> gifFolder <> "\\" <> gifFileName <> ".gif";

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



(* exportImage[(0.25 + 0.01)/180*Pi, 302, "e8_142"]; *)
(* exportImage[(0.5 + 0.25 + 0.01)/180*Pi, 202, "test"]; *)
(* exportImage[(0.5 + 0.01)/180*Pi, 2, "test"]; *)
(* exportImage[5 Pi/180, 2, "test"];
exportImage[10 Pi/180, 3, "test"];
exportImage[15 Pi/180, 4, "test"];
exportImage[20 Pi/180, 5, "test"];
 *)
(* 60 degree as same as 0 *)
(* exportAnimation[]; *)
