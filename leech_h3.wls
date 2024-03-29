#!/usr/bin/env wolframscript

dataFolder = "data";
shape = "leech_vertices";
dataFileName = FileNameJoin[{dataFolder, shape <> ".wl"}];

exportToPov = True;
rangeFactor = 0.75;
thicknessRatio = 0.007;
imageSize = {4, 3} * 240;
roundPrecision = 0.000001;
showEdges = False;
showEdges = True;

start = AbsoluteTime[];
Needs["POVRayRender`"];
ConfigurePOVRayRender["POVRayPath" -> "C:\\Program Files\\POV-Ray\\v3.7\\bin\\pvengine64.exe"];

imageFolder = "output";
vertices = Get[dataFileName];
(* edges3dMap = Get["data/leech_3d_edges_tally.wl"]; *)
edges3dMap = Get["data/leech_3d_segments_tally.wl"];

(* viewPoint = {0, 0, 1}; *)
viewPoint = {1, -1, 1}/Sqrt[3];
rotation = RotationMatrix[{{0, 1.2, 1}, viewPoint}];
lighting = {{"Point", White, viewPoint * 10}};

exportImage[theta_, serialNumber_, frameFolder_] := Module[
    {verticesOnEdges, verticesTally, cropCommand,
        graph},

    phi = (Sqrt[5] + 1)/2;
    projection8to3 = {
        {phi, phi, 0, 0, 1, -1, 0, 0},
        {1, -1, phi, phi, 0, 0, 0, 0},
        {0, 0, 1, -1, phi, phi , 0, 0}
    };

    projectionMatrix = ArrayFlatten[{{projection8to3, projection8to3 * 0, projection8to3 * 0}}];

    rotationPhi = RotationMatrix[{{0, phi, 1}, {0, 0, 1}}];
    rotationTheta = RotationMatrix[theta, {0, 0, 1}];
    vertices3d = vertices.Transpose[projectionMatrix * 1.0].Transpose[rotationPhi].Transpose[rotationTheta];

    (* edges3dRotated = Map[#.Transpose[rotationPhi].Transpose[rotationTheta]&, edges3d * 1.0, {2}]; *)

    edges3dRotatedTally = Map[{Table[#[[1]][[k]].Transpose[rotationPhi].Transpose[rotationTheta], {k, 2}] ,  #[[2]] }&, Normal[edges3dMap]];
    Print["Finished tallying edges " <> IntegerString[edges3dRotatedTally//Length]];
    tallyTallyEdges = Sort[Tally[Map[#[[2]]&, edges3dRotatedTally]]];
    (* Print["Distribution of edge tally counts:"]; *)
    (* Print[tallyTallyEdges]; *)
    colorMapEdges = <||>;
    colorCountEdges = Length[tallyTallyEdges];

    For[colorIndex = 1, colorIndex <= Length[tallyTallyEdges], colorIndex ++,
        multiplicity = tallyTallyEdges[[colorIndex]][[1]];
        colorMapEdges[multiplicity] = Hue[Mod[0.2 + (colorIndex - 1) * 13./colorCountEdges, 1 ], 1, 1 ];
    ];
    Print["Edge color count: " <> IntegerString[colorCountEdges]];

    matrixNorm = Norm[projectionMatrix[[1]]];

    maxNorm = Max[Map[Norm, vertices3d]];
    Print[{maxNorm, matrixNorm}//N];
    (* maxNorm = 6; *)

    projectTo3d[vertex_] := vertex/maxNorm;
    range = rangeFactor;

    thickness = thicknessRatio * range;
    
    roundedVertices3d = Round[vertices3d, roundPrecision];
    verticesTally = Tally[Expand[roundedVertices3d]];

    Print["Finished tallying " <> IntegerString[verticesTally//Length]];
    tallyTally = Sort[Tally[Map[#[[2]]&, verticesTally]]];
    Print["Distribution of tally counts:"];
    Print[tallyTally];
    colorMap = <||>;
    colorCount = Length[tallyTally];

    For[colorIndex = 1, colorIndex <= Length[tallyTally], colorIndex ++,
        multiplicity = tallyTally[[colorIndex]][[1]];
        colorMap[multiplicity] = Hue[Mod[colorIndex - 1 , colorCount]/colorCount, 1, 1 ];
    ];
    Print["vertex color count: " <> IntegerString[colorCount]];

    graph = Graphics3D[
        {
            Map[{
            Opacity[1], 
            FaceForm[colorMap[#[[2]]]], 
            Sphere[projectTo3d[#[[1]]], thickness * (#[[2]])^(1/4) ]
            }&, verticesTally],
            If[showEdges,
                Map[{
                Opacity[1], 
                FaceForm[colorMapEdges[#[[2]]]],
                (* Cylinder[projectTo3d[#[[1]]], thickness/6 * (#[[2]])^(1/4) ] *) (* 110 *)
                (* Cylinder[projectTo3d[#[[1]]], thickness/6 * (1 + Log[2, #[[2]]]) ] (* 111 *) *)
                (* Cylinder[projectTo3d[#[[1]]], Max[thickness/7, thickness/7/3 * (#[[2]])^(1/6)] ] (* 112 *) *)
                Cylinder[projectTo3d[#[[1]]], thickness/7/2 * (#[[2]])^(1/4) ] (* 117 *)
                (* Cylinder[projectTo3d[#[[1]]], thickness/21 ] (* 113 *) *)
                }&, edges3dRotatedTally],
(* 
                {
                    FaceForm[White], Map[Cylinder[#, thickness]&, Map[projectTo3d, edges3dRotated, {2}]]
                },
 *)
                {}
            ]
        },
        (* ViewPoint -> viewPoint * 3000, *)
        ViewPoint -> {1, -1, Sqrt[7 - 3*Sqrt[5]]/2} * 5,
        (* ViewPoint -> {0, -0, 1} * 5, *)
        ViewVertical -> {0, 0, 1},
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

    shape = "leech";
    frameFolder = shape <> If[showEdges, "_with_edges", ""] <> "_h3_frames";
    gifFileName = shape <> If[showEdges, "_with_edges", ""] <> "_h3";
    gifFolder = FileNameJoin[{imageFolder, shape}];
    outputFolder = FileNameJoin[{imageFolder, frameFolder}];
    If[!DirectoryQ[outputFolder], CreateDirectory[outputFolder]];
    If[!DirectoryQ[gifFolder], CreateDirectory[gifFolder]];

    frameIndex = 1;
    angleStep = 1;
    For[angle = 0, angle <= 72 - angleStep, angle += angleStep,
        Print["Frame #" <> IntegerString[frameIndex] <> " angle: " <> TextString[angle]];
        If[Mod[angle, 1] == 0,
            exportImage[angle / 180 * Pi, frameIndex, frameFolder];
        ];
        frameIndex ++;
    ];

    gifCommand = "magick convert -quiet +repage -alpha remove -loop 0 -delay " <> IntegerString[3] <> " -dispose 2 " <> outputFolder <> "\\*animation*.png " <> gifFolder <> "\\" <> gifFileName <> ".gif";

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

(* exportImage[0, 101, "test"];
exportImage[Pi/180 * 1, 102, "test"];
exportImage[Pi/180 * 2, 103, "test"];
exportImage[Pi/180 * 3, 104, "test"];
exportImage[Pi/180 * 4, 105, "test"];
 *)
(* exportImage[Pi/180 * 45, 106, "test"]; *)
(* exportImage[Pi/180 * 15, 112, "test"]; *)
(* exportImage[Pi/180 * 15, 116, "test"]; *)
(* exportImage[Pi/180 * 15, 117, "test"]; *)
exportImage[Pi/180 * 15, 119, "test"];


end = AbsoluteTime[];
Print["Elapsed time in seconds: " <> ToString[end - start]];
