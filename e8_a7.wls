#!/usr/bin/env wolframscript

dataFolder = "data";
refShape = "polytope_3_3_5";
refDataFileName = FileNameJoin[{dataFolder, refShape <> ".wl"}];


exportToPov = True;
lighting = {{"Point", White, {10, -10, 10}}};
rangeFactor = 0.9;
thickness = 0.005;
range = 3;
imageFolder = "output";
imageSize = {4, 3} * 240;
epsilon = 0.0000001;
truncateRatioDenominator = 120;

start = AbsoluteTime[];
Needs["POVRayRender`"];
ConfigurePOVRayRender["POVRayPath" -> "C:\\Program Files\\POV-Ray\\v3.7\\bin\\pvengine64.exe"];

getEdgesFromFace[face_] := Table[{face[[i+1]], face[[Mod[i+1, Length[face]] + 1]]}, {i, 0, Length[face] - 1}];
sameVertex[point1_, point2_] := point1 == point2;
sameCenter[edge1_, edge2_] := Total[edge1] == Total[edge2];
getLength[edge_] := Norm[edge[[2]] - edge[[1]]];
sameSet[array1_, array2_] := Length[ Intersection[array1, array2, SameTest -> sameVertex] ] == Length[array1];

cubeVertices[dimension_] := 2 * Map[IntegerDigits[#, 2][[2;;dimension+1]]&, Table[k, {k, 2^dimension, 2 * 2^dimension - 1}]] - 1;
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
demicubeVertices[dimension_, altMode_] := Select[cubeVertices[dimension], Mod[Total[#] + 8, 4] == Mod[If[altMode, 0, 2] + dimension, 4] &];


get421Vertices[altMode_] := Module[
    {vertices},
    vertices = {};
    vertices = Join[vertices, Union[Flatten[Permutations/@Map[Join[#, {0, 0, 0, 0, 0, 0}]& , cubeVertices[2] * 2], 1]]];
    vertices = Join[vertices, demicubeVertices[8, altMode]];
    vertices
];



projectToDirection[vertex_, projectionMatrix_] := projectionMatrix.vertex;
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

phi = (Sqrt[5] + 1)/2;
 


(* projectionMatrix = {
    Table[Sin[Pi k / 4 ], {k, 1, 8}],
    Table[Cos[Pi k / 4 ], {k, 1, 8}],
    Table[0, {k, 1, 8}]
} * truncateRatioDenominator;
 *)

projectionMatrix = {
    {Sqrt[2],   1, 0,       -1, Sqrt[2], -1, 0,         1},
    {0,         1, Sqrt[2],  1, 0,       -1, -Sqrt[2], -1},
    Table[0, {k, 1, 8}]
} * truncateRatioDenominator;


(* 
https://en.wikipedia.org/wiki/File:E8inA7.svg?fbclid=IwAR0Ay6TcQ9lDdXWnP-MRjWPGsbos4KM4a_wuD02GyTmVTUl8cjneXu_uKzg
u=(0.5, 0.353553390593, 0., -0.353553390593, -0.5, -0.353553390593, 0., 0.353553390593)
v=(0., 0.353553390593, 0.5, 0.353553390593, 0., -0.353553390593, -0.5, -0.353553390593)


https://en.wikipedia.org/wiki/User_talk:Jgmoxness?fbclid=IwAR1EQAd8KDLFF4qvFmwtHj48q58nwq88rEgPbLAdRi_AHD0ZpezuFrDzrIo#Directed_graphs
u={0.707, 0.5, 0., -0.5, 0., 0., 0., 0.}, v={0., 0.5, 0.707, 0.5, 0., 0., 0., 0.}
or alternatively, but producing the same image for E8,
u={0.5, 0.353553390593, 0., -0.353553390593, -0.5, -0.353553390593, 0., 0.353553390593}
v={0., 0.353553390593, 0.5, 0.353553390593, 0., -0.353553390593, -0.5, -0.353553390593}
*)

(* swap coordinates *)
(* projectionMatrix = projectionMatrix[[{2, 1, 3, 4}]]; *)

vertices = get421Vertices[True];
edgeIndices = getEdges[vertices];
(* faces = getTriangularFaces[vertices]; *)

vertices3d = FullSimplify[vertices.Transpose[projectionMatrix]];
edges3d = Map[vertices3d[[#]]&, edgeIndices];
maxNorm = Max[Map[Norm, vertices3d]];
Print[maxNorm//N];
(* hard code to avoid random scaling across frames *)
maxNorm = 780; 


(* viewPoint = {1, -1, 1}/Sqrt[3]; *)
viewPoint = {0, 0, 1};
rotation3D = RotationMatrix[{{0, 0, 1}, viewPoint}];
projectionMatrix3d = rotation3D/maxNorm;

projectTo3d[vertex_] := projectionMatrix3d.vertex;
range = rangeFactor;


exportImage[truncateRatioNumerator_, serialNumber_, frameFolder_] := Module[
    {verticesOnEdges, verticesTally, cropCommand,
        graph, outputFolder, outputFileName},

    truncateRatio = truncateRatioNumerator/truncateRatioDenominator;
    verticesOnEdges = Flatten[ Map[{ truncateRatio * #[[1]] + (1 - truncateRatio) * #[[2]], truncateRatio * #[[2]] + (1 - truncateRatio) * #[[1]] }&, edges3d], 1];

    verticesTally = Tally[Expand[verticesOnEdges]];
    Print["Finished tallying " <> IntegerString[verticesTally//Length]];

    graph = Graphics3D[
        Map[{
            Opacity[1], 
            FaceForm[Hue[Mod[ #[[2]] - 1 , 11]/11, 1, 1 ]], 
            Sphere[projectTo3d[#[[1]]], thickness * (#[[2]])^(1/3) ]
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

    shape = "e8_421";
    frameFolder = shape <> "_a7_truncate_frames";
    gifFileName = shape <> "_a7";
    gifFolder = FileNameJoin[{imageFolder, shape}];
    outputFolder = FileNameJoin[{imageFolder, frameFolder}];
    If[!DirectoryQ[outputFolder], CreateDirectory[outputFolder]];
    If[!DirectoryQ[gifFolder], CreateDirectory[gifFolder]];

    epsilon = 0.001;
    numerators = Join[
        Table[0, {k, Floor[truncateRatioDenominator / 12]}], 
        Table[k, {k, 1, Floor[truncateRatioDenominator * (1 - Sqrt[2]/2)]}],
        Table[truncateRatioDenominator * (1 - Sqrt[2]/2), {k, Floor[truncateRatioDenominator / 18]}],
        Table[k, {k, Floor[truncateRatioDenominator * (1 - Sqrt[2]/2)] + 1, Floor[truncateRatioDenominator * (Sqrt[2] - 1)]}],
        Table[truncateRatioDenominator * (Sqrt[2] - 1), {k, Floor[truncateRatioDenominator / 18]}],
        Table[k, {k, Floor[truncateRatioDenominator * (Sqrt[2] - 1)] + 1, truncateRatioDenominator / 2}],
        Table[truncateRatioDenominator/2, {k, 1, Floor[truncateRatioDenominator / 12]}]
    ];

    For[ frameIndex = 1, frameIndex <= Length[numerators] , frameIndex++,
        numerator = numerators[[frameIndex]];
        Print["Frame #" <> IntegerString[frameIndex] <> "/" <> IntegerString[Length[numerators]] 
            <> " ; numerator: " <> TextString[numerator]];
        If[Mod[frameIndex, 1] == 0,
            exportImage[numerator, frameIndex, frameFolder];
        ]
    ];

    gifCommand = "magick convert -duplicate 1,-2-1 -quiet +repage -alpha remove -loop 0 -delay " <> IntegerString[3] <> " -dispose 2 " <> outputFolder <> "\\*animation*.png " <> gifFolder <> "\\" <> gifFileName <> ".gif";

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

exportAnimation[];
(* exportImage[0, 1, "test"]; *)
(* exportImage[120 - 60 Sqrt[2], 351, "test"]; *)
(* exportImage[120 Sqrt[2] - 120, 497, "test"]; *)

end = AbsoluteTime[];
Print["Elapsed time in seconds: " <> ToString[end - start]];
