#!/usr/bin/env wolframscript

dataFolder = "data";
shape = "leech_vertices";
dataFileName = FileNameJoin[{dataFolder, shape <> ".wl"}];

exportToPov = True;
lighting = {{"Point", White, {10, -10, 10}}};
rangeFactor = 0.78;
thicknessRatio = 0.003;
imageFolder = "output";
imageSize = {4, 3} * 240;
roundPrecision = 0.000001;

start = AbsoluteTime[];
Needs["POVRayRender`"];
ConfigurePOVRayRender["POVRayPath" -> "C:\\Program Files\\POV-Ray\\v3.7\\bin\\pvengine64.exe"];

vertices = Get[dataFileName];
viewPoint = {0, 0, 1};

getProjectionMatrix[t_] := Module[
    {projectionMatrix, i, j, renormer, elevenCycle},
    projectionMatrix = Table[0, {i, 3}, {j, 24}];
    renormer = Sqrt[11];
    (* from http://www.madore.org/~david/math/higman-sims-graph.html *)
    elevenCycle = {
        {0}, {1, 0}, {1, 1}, {1, 8},
        {0}, {2, 0}, {2, 3}, {2, 10},
        {2, 8}, {1, 2}, {1, 9}, {1, 6}, 
        {1, 3}, {2, 9}, {2, 5}, {2, 2}, 
        {2, 7}, {1, 10}, {1, 5}, {1, 7}, 
        {1, 4}, {2, 1}, {2, 4}, {2, 6}
    };

    For[i = 0, i < 24, i++,
        If[elevenCycle[[i + 1]][[1]] == 0,
            projectionMatrix[[1]][[i+1]] = 0;
            projectionMatrix[[2]][[i+1]] = 0
        ];

        If[elevenCycle[[i + 1]][[1]] == 1,
            projectionMatrix[[1]][[i+1]] = Cos[2 * Pi * (elevenCycle[[i + 1]][[2]] / 11) + t]/renormer;
            projectionMatrix[[2]][[i+1]] = -Sin[2 * Pi * (elevenCycle[[i + 1]][[2]] / 11) + t]/renormer
        ];

        If[elevenCycle[[i + 1]][[1]] == 2,
            projectionMatrix[[1]][[i+1]] = Cos[2 * Pi * (elevenCycle[[i + 1]][[2]] / 11) - t]/renormer;
            projectionMatrix[[2]][[i+1]] = -Sin[2 * Pi * (elevenCycle[[i + 1]][[2]] / 11) - t]/renormer
        ];
    ];
    projectionMatrix
];

exportImage[angle_, serialNumber_, frameFolder_, zoomed_] := Module[
    {verticesOnEdges, verticesTally, cropCommand,
        graph, outputFolder, outputFileName},

    theta = angle / 90 * Pi/2;
    projectionMatrix = getProjectionMatrix[theta] * 1.0;

    vertices3d = vertices.Transpose[projectionMatrix];
    matrixNorm = Norm[projectionMatrix[[1]]];

    maxNorm = Max[Map[Norm, vertices3d]];
    Print[maxNorm//N];
    (* Print[matrixNorm//N]; *)
    (* matrix norm is always 1 *)
    maxNorm = 4.3;

    projectTo3d[vertex_] := vertex/maxNorm;
    range = If[zoomed, 1/3, 1] * rangeFactor;

    thickness = thicknessRatio * range;
    
    roundedVertices3d = Round[vertices3d, roundPrecision];
    verticesTally = Tally[Expand[roundedVertices3d]];
    (* verticesTally = Tally[Expand[vertices3d]]; *)
    Print["Finished tallying " <> IntegerString[verticesTally//Length]];
    tallyTally = Tally[Map[#[[2]]&, verticesTally]];
    Print["Distribution of tally counts:"];
    Print[Sort[tallyTally]];

    graph = Graphics3D[
        Map[{
            Opacity[1], 
            FaceForm[Hue[Mod[ #[[2]] - 1 , 13]/13, 1, 1 ]], 
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

exportAnimation[zoomed_] := Module[
    {
        shape, frameFolder, gifFileName, gifFolder, outputFolder, maxTheta,
        thetaStep, epsilon, thetas, theta, gifCommand
    },

    shape = "leech";
    zoomedString = If[zoomed, "_zoomed", ""];
    frameFolder = shape <> "_11" <> zoomedString <> "_frames";
    gifFileName = shape <> "_11" <> zoomedString;
    gifFolder = FileNameJoin[{imageFolder, shape}];
    outputFolder = FileNameJoin[{imageFolder, frameFolder}];
    If[!DirectoryQ[outputFolder], CreateDirectory[outputFolder]];
    If[!DirectoryQ[gifFolder], CreateDirectory[gifFolder]];

    frameIndex = 1;
    For[angle = 0, angle <= 360, angle += 1,
        Print["Frame #" <> IntegerString[frameIndex] <> "  angle: " <> IntegerString[angle]];
        If[Mod[angle, 1] == 0,
            exportImage[angle, frameIndex, frameFolder, zoomed];
            frameIndex ++;
        ];
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

exportAnimation[True];
exportAnimation[False];

(* exportImage[0, 7, "test", True];
exportImage[30, 8, "test", False];
exportImage[60, 9, "test", True];
exportImage[90, 10, "test", False];
 *)

end = AbsoluteTime[];
Print["Elapsed time in seconds: " <> ToString[end - start]];
