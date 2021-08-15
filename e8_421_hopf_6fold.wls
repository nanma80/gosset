#!/usr/bin/env wolframscript

(* https://www.physast.uga.edu/~mgeller/JPA34p10243.pdf *)

imageFolder = "output";
imageSize = 240 * {4, 3};
exportToPov = True;
showVertices = True;
showEdges = True;
showFaces = True;
topView = False;
roundPrecision = 0.000001;

Needs["Quaternions`"];
Needs["POVRayRender`"];
ConfigurePOVRayRender["POVRayPath" -> "C:\\Program Files\\POV-Ray\\v3.7\\bin\\pvengine64.exe"];

colors = {Red, Blue, Green, Yellow, Magenta, Cyan, Brown, Orange, Pink, Gray};
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
getAngle[u_, v_] := Round[ArcCos[u.v/Norm[u]/Norm[v]]/Pi*180., roundPrecision];
getScaleFactor[groupIndex_] := (1 + 0.01 * groupIndex / Length[vertexGroups]);
doubleRotationMatrix[theta_] := ArrayFlatten[{{RotationMatrix[theta], 0}, {0, RotationMatrix[theta]}}];

vertices = get421Vertices[True];
vertices24Cell = get24CellVertices[];

complexes = Map[getComplexNumbers, vertices];

s4s = Map[getS4, complexes];

q2s = Map[getQ2, vertices];

s3s = Map[getS3, q2s];

(* Print[Map[#[[2]]&, Tally[s4s]]//Tally]; *)
(* Print[Map[#[[2]]&, Tally[s3s]]//Tally]; *)

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

    vertexGroups3d = Map[projectionMatrix.# &, vertexGroups, {2}];
    edgeGroups3d = Map[projectionMatrix.# &, edgeGroups, {3}];
    faceGroups3d = Map[projectionMatrix.# &, faceGroups, {3}];

    viewPoint = If[topView, {0, 0, 10}, {1, -1, 0.5}] * 10;

    range = 5.5;
    thickness = 0.06;
    lighting = {{"Point", White, {10, -10, 10}}};

    graphElements = {};

    For[groupIndex = 1, groupIndex <= Length[vertexGroups], groupIndex++,
        If[showVertices,
            AppendTo[graphElements, {
                Opacity[1],
                FaceForm[colors[[groupIndex]]],
                Sphere[vertexGroups3d[[groupIndex]] * getScaleFactor[groupIndex], thickness]
            }]
        ];

        If[showEdges && groupIndex <= 10,
            AppendTo[graphElements, {
                Opacity[1],
                FaceForm[colors[[groupIndex]] ],
                Cylinder[edgeGroups3d[[groupIndex]] * getScaleFactor[groupIndex], thickness * 0.8 * getScaleFactor[groupIndex]]
            }]
        ];

        If[showFaces && groupIndex <= 10,
            AppendTo[graphElements, {
                Opacity[1],
                FaceForm[colors[[groupIndex]] ],
                Map[Polygon, faceGroups3d[[groupIndex]] * getScaleFactor[groupIndex]]
            }]
        ];
    ];

    graph = Graphics3D[
            graphElements,
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
        shape, frameFolder, gifFileName, gifFolder, outputFolder, angle,
        frameIndex, gifCommand, delay
    },

    shape = "e8_421";
    frameFolder = shape <> If[showFaces, "_with_faces", ""] <> If[topView, "_topview", ""] <> "_hopf_6fold_frames";
    gifFileName = shape <> If[showFaces, "_with_faces", ""] <> If[topView, "_topview", ""] <> "_hopf_6fold";
    gifFolder = FileNameJoin[{imageFolder, shape}];
    outputFolder = FileNameJoin[{imageFolder, frameFolder}];
    If[!DirectoryQ[outputFolder], CreateDirectory[outputFolder]];
    If[!DirectoryQ[gifFolder], CreateDirectory[gifFolder]];

    frameIndex = 1;
    angleStep = 0.5;
    For[angle = 0, angle <= 60 - angleStep, angle += angleStep,
        Print["Frame #" <> IntegerString[frameIndex] <> " angle: " <> TextString[angle]];
        (* If[Mod[angle, 1] == 0, *)
            exportImage[(angle + 0.01) / 180 * Pi, frameIndex, frameFolder];
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



(* exportImage[0, 1, "test"]; *)
(* exportImage[5 Pi/180, 2, "test"];
exportImage[10 Pi/180, 3, "test"];
exportImage[15 Pi/180, 4, "test"];
exportImage[20 Pi/180, 5, "test"];
 *)
(* 60 degree as same as 0 *)
exportAnimation[];
