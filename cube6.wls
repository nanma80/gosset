#!/usr/bin/env wolframscript

exportToPov = True;
lighting = {{"Point", White, {10, -10, 10}}};
rangeFactor = 0.85;
thickness = 0.13;
range = -1;
imageFolder = "output";
imageSize = {600, 450};
plotFaces = False;
rotationMode = 3;

start = AbsoluteTime[];
Needs["POVRayRender`"];
ConfigurePOVRayRender["POVRayPath" -> "C:\\Program Files\\POV-Ray\\v3.7\\bin\\pvengine64.exe"];

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

get321Vertices[altMode_] := Module[
    {},
    vertices = {};
    vertices = Join[vertices, Map[Join[# * 2, {Sqrt[2]}]& , orthoplexVertices[6]]];
    vertices = Join[vertices, Map[Join[# * 2, {-Sqrt[2]}]& , orthoplexVertices[6]]];
    vertices = Join[vertices, Map[Join[#, {0}]& , demicubeVertices[6, altMode]]];
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

phi = GoldenRatio;

(* projectionMatrixParameter[theta_] := {
    {phi, phi,  0,  0,   1,  - Cos[theta],  - Sin[theta]},
    {1,   -1,  phi, phi, 0,   0,  0},
    {0,    0,   1,  -1,  phi, phi Cos[theta], phi Sin[theta]}
};
 *)

projectionMatrixParameter6D[theta_, rotationMode_] := 
If[rotationMode == 2,
    {
        {phi(Cos[theta] + Sin[theta]), phi (-Sin[theta] + Cos[theta]), 0, 0, 1, -1},
        {Cos[theta] - Sin[theta], -Cos[theta]-Sin[theta], phi, phi, 0, 0},
        {0, 0, 1, -1, phi, phi }
    },
    {
        {phi(Cos[theta] + Sin[theta]), phi (-Sin[theta] + Cos[theta]), 0, 0, Cos[theta] - Sin[theta], -Cos[theta]-Sin[theta]},
        {Cos[theta] - Sin[theta], -Cos[theta]-Sin[theta],    phi(Cos[theta] + Sin[theta]), phi (-Sin[theta] + Cos[theta]), 0, 0},
        {0, 0, Cos[theta] - Sin[theta], -Cos[theta]-Sin[theta], phi(Cos[theta] + Sin[theta]), phi (-Sin[theta] + Cos[theta]) }
    }];


(* 
projectionMatrix1 = {
    {phi, phi, 0, 0, 1, -1},
    {1, -1, phi, phi, 0, 0},
    {0, 0, 1, -1, phi, phi }
};
*)

(* projectionMatrix = IdentityMatrix[3]; *)

(* vertices = cubeVertices[3]; *)
(* vertices = demicubeVertices[6, True]; *)
(* vertices = orthoplexVertices[6]; *)

exportImage[theta_, phi3d_, serialNumber_, frameFolder_] := Module[
    {vertices, edges, displayVertices, displayEdges, 
        graph, outputFolder, outputFileName},

    vertices = cubeVertices[6];

    edges = getEdges[vertices];
    faces = getTriangularFaces[vertices];

    Print["vertex count: " <> IntegerString[Length[vertices]] 
        <> " ; edge count: " <> IntegerString[Length[edges]]
        <> " ; face count: " <> IntegerString[Length[faces]]];

    projectionMatrix = projectionMatrixParameter6D[theta, rotationMode];

    If[Length[vertices[[1]]] > Length[projectionMatrix[[1]]],
        projectionMatrix = Map[Join[#, Table[0, {k, 1, Length[vertices[[1]]] - Length[projectionMatrix[[1]]]}]]&, projectionMatrix]];

    displayVertices = Map[projectToDirection[#, RotationMatrix[phi3d, {-1, 0, phi}].projectionMatrix]&, vertices];

    displayEdges = Map[displayVertices[[#]]&, edges, {2}];
    displayFaces = Map[displayVertices[[#]]&, faces, {2}];

    range = If[range < 0, Max[Map[Norm[N[#]]&, displayVertices]] * rangeFactor, range];

    graphElements = {
            {
                Opacity[1], FaceForm[LightRed], Map[Cylinder[#, thickness]&, displayEdges]
            },
            {
                Opacity[1], FaceForm[Yellow], Map[Sphere[#, thickness * 2]&, displayVertices]
            }
        };


    If[plotFaces,
        AppendTo[graphElements,
            {
                Opacity[0.4], FaceForm[Blue], Map[Polygon, displayFaces]
            }
        ]
    ];

    graph = Graphics3D[
        graphElements,
        ViewPoint -> {2.6, -3.2, 2.8}*30,
        (* ViewPoint -> {phi, -1, 0} * 1000, *)
        ViewVertical -> {-1, 0, phi},
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
        ,
        Export[outputFileName, graph];
    ];
    Print["Saved the image to " <> outputFileName];
];

exportAnimation[frameCount_] := Module[
    {
        shape, frameFolder, gifFileName, gifFolder, outputFolder, maxTheta,
        thetaStep, epsilon, thetas, theta, gifCommand
    },

    shape = "cube6";
    frameFolder = shape <> "_rotation3d_frames" <> If[plotFaces, "_with_faces", ""] <> IntegerString[rotationMode];
    gifFileName = shape <> "_rotation3d" <> If[plotFaces, "_with_faces", ""] <> IntegerString[rotationMode];
    gifFolder = FileNameJoin[{imageFolder, shape}];
    outputFolder = FileNameJoin[{imageFolder, frameFolder}];
    If[!DirectoryQ[outputFolder], CreateDirectory[outputFolder]];
    If[!DirectoryQ[gifFolder], CreateDirectory[gifFolder]];

    maxTheta = Pi / 2;
    thetaStep = maxTheta/frameCount;

    epsilon = 0.001;
    thetas = Join[
        Table[epsilon, {k, Floor[frameCount / 6]}], 
        Table[k, {k, epsilon, maxTheta + epsilon, thetaStep}],
        Table[maxTheta + epsilon, {k, Floor[frameCount / 6]}],
        Table[k, {k, maxTheta + epsilon, 2 maxTheta + epsilon, thetaStep}]
    ];

    maxPhi3d = 2 Pi / 5;
    phi3dStep = maxPhi3d/Length[thetas];
    phi3ds = Table[k, {k, 0, maxPhi3d - phi3dStep, phi3dStep}];

    For[ frameIndex = 1, frameIndex <= Length[thetas] , frameIndex++,
        theta = thetas[[frameIndex]];
        phi3d = phi3ds[[frameIndex]];
        Print["Frame #" <> IntegerString[frameIndex] <> "/" <> IntegerString[Length[thetas]] 
            <> " ; theta: " <> ToString[theta, InputForm] 
            <> " ; phi3d: " <> ToString[phi3d, InputForm]];
        exportImage[theta, phi3d, frameIndex, frameFolder];
    ];

    (* gifCommand = "magick convert -duplicate 1,-2-1 -quiet +repage -alpha remove -loop 0 -delay "  (* loop back*) *)
    gifCommand = "magick convert -quiet +repage -alpha remove -loop 0 -delay " (* no looping back *)
        <> IntegerString[Floor[200/frameCount]] <> " -dispose 2 " 
        <> outputFolder <> "\\*animation_frame*.png " 
        <> gifFolder <> "\\" <> gifFileName <> ".gif";

    Print[gifCommand];
    Run[gifCommand];
    Print["Saved the animation to " <> gifFolder <> "\\" <> gifFileName <> ".gif"];
];

exportAnimation[48];
(* exportAnimation[12]; *)

(* exportImage[0, 0, 1, "test"]; *)
(* exportImage[Pi/6, 0, 2, "test"]; *)
(* exportImage[Pi/3, 0, 3, "test"]; *)
(* exportImage[Pi/2, 0, 4, "test"]; *)
(* exportImage[Pi/4*3, 0.2, 5, "test"]; *)

end = AbsoluteTime[];
Print["Elapsed time in seconds: " <> ToString[end - start]];
