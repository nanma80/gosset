#!/usr/bin/env wolframscript

dataFolder = "data";
refShape = "polytope_3_3_5";
refDataFileName = FileNameJoin[{dataFolder, refShape <> ".wl"}];

exportToPov = True;
lighting = {{"Point", White, {10, -10, 10}}};
rangeFactor = 0.8;
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

projectionMatrixB4 = {
    Table[Sin[Pi k / 4 ], {k, 1, 4}],
    Table[Cos[Pi k / 4 ], {k, 1, 4}],
    Table[0, {k, 1, 4}]
};

dataFileName = "leech_min_norm_vectors.wl";
minNormVectors = Get[dataFileName];

If[limit > 0,
    sampleIndices = RandomSample[Range[Length[minNormVectors]], limit];
    vertices = minNormVectors[[sampleIndices]],
    vertices = minNormVectors
];

viewPoint = {0, 0, 1};

exportImage[transitionStep_, angle_, serialNumber_, frameFolder_] := Module[
    {verticesOnEdges, verticesTally, cropCommand,
        graph, outputFolder, outputFileName},

    theta = angle / 180 * Pi/2;
    coefficients = Table[0, {k, 1, 6}];
    coefficients[[transitionStep]] = Cos[theta];
    coefficients[[transitionStep + 1]] = Sin[theta];
    projectionMatrix = ArrayFlatten[{Table[projectionMatrixB4 * coefficients[[k]], {k, 1, 6}]}] * 1.0;

    vertices3d = FullSimplify[vertices.Transpose[projectionMatrix]];
    matrixNorm = Norm[projectionMatrix[[1]]];

    maxNorm = Max[Map[Norm, vertices3d]];
    Print[maxNorm//N];
    (* Print[matrixNorm//N]; *)
    (* matrix norm is always Sqrt[2] *)
    maxNorm = 7.4;

    projectTo3d[vertex_] := vertex/matrixNorm/maxNorm * Sqrt[2];
    range = rangeFactor;

    thicknessRatio = 0.005;
    thickness = thicknessRatio * range;

    verticesTally = Tally[Expand[vertices3d]];
    Print["Finished tallying " <> IntegerString[verticesTally//Length]];

    graph = Graphics3D[
        Map[{
            Opacity[1], 
            FaceForm[Hue[Mod[ #[[2]] - 1 , 13]/13, 1, 1 ]], 
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

    shape = "leech";
    frameFolder = shape <> "_b4_truncate_frames";
    gifFileName = shape <> "_b4";
    gifFolder = FileNameJoin[{imageFolder, shape}];
    outputFolder = FileNameJoin[{imageFolder, frameFolder}];
    If[!DirectoryQ[outputFolder], CreateDirectory[outputFolder]];
    If[!DirectoryQ[gifFolder], CreateDirectory[gifFolder]];

    frameIndex = 1;
    For[stepIndex = 1, stepIndex <= 1, stepIndex ++,
        For[angle = 0, angle <= 15, angle += 1,
            Print["Frame #" <> IntegerString[frameIndex] <> " step: " <> IntegerString[stepIndex] <> " double angle: " <> IntegerString[angle]];
            If[Mod[angle, 1] == 0,
                exportImage[stepIndex, angle, frameIndex, frameFolder];
                frameIndex ++;
            ];
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

exportAnimation[];
(* exportImage[1, 0, 1, "test"];
exportImage[1, 90, 2, "test"];
exportImage[2, 0, 3, "test"];
exportImage[2, 1, 4, "test"];
 *)

end = AbsoluteTime[];
Print["Elapsed time in seconds: " <> ToString[end - start]];
