#!/usr/bin/env wolframscript

ms = {{{-1, 0, 0}, {0, -1, 0}, {0, 0, -1}}, 
     {{-1, 0, 0}, {0, 1, 0}, {0, 0, 1}}, 
     {{-1/2, -1/4 - Sqrt[5]/4, 1/4 - Sqrt[5]/4}, {-1/4 - Sqrt[5]/4, 
       -1/4 + Sqrt[5]/4, 1/2}, {1/4 - Sqrt[5]/4, 1/2, -1/4 - Sqrt[5]/4}}, 
     {{-1/2, -1/4 - Sqrt[5]/4, 1/4 - Sqrt[5]/4}, {1/4 + Sqrt[5]/4, 
       1/4 - Sqrt[5]/4, -1/2}, {-1/4 + Sqrt[5]/4, -1/2, 1/4 + Sqrt[5]/4}}, 
     {{-1/2, 1/4 + Sqrt[5]/4, -1/4 + Sqrt[5]/4}, {-1/4 - Sqrt[5]/4, 
       1/4 - Sqrt[5]/4, -1/2}, {1/4 - Sqrt[5]/4, -1/2, 1/4 + Sqrt[5]/4}}, 
     {{-1/2, 1/4 + Sqrt[5]/4, -1/4 + Sqrt[5]/4}, {1/4 + Sqrt[5]/4, 
       -1/4 + Sqrt[5]/4, 1/2}, {-1/4 + Sqrt[5]/4, 1/2, -1/4 - Sqrt[5]/4}}, 
     {{1/2, -1/4 - Sqrt[5]/4, 1/4 - Sqrt[5]/4}, {-1/4 - Sqrt[5]/4, 
       1/4 - Sqrt[5]/4, -1/2}, {1/4 - Sqrt[5]/4, -1/2, 1/4 + Sqrt[5]/4}}, 
     {{1/2, -1/4 - Sqrt[5]/4, 1/4 - Sqrt[5]/4}, {1/4 + Sqrt[5]/4, 
       -1/4 + Sqrt[5]/4, 1/2}, {-1/4 + Sqrt[5]/4, 1/2, -1/4 - Sqrt[5]/4}}, 
     {{1/2, 1/4 + Sqrt[5]/4, -1/4 + Sqrt[5]/4}, {-1/4 - Sqrt[5]/4, 
       -1/4 + Sqrt[5]/4, 1/2}, {1/4 - Sqrt[5]/4, 1/2, -1/4 - Sqrt[5]/4}}, 
     {{1/2, 1/4 + Sqrt[5]/4, -1/4 + Sqrt[5]/4}, {1/4 + Sqrt[5]/4, 
       1/4 - Sqrt[5]/4, -1/2}, {-1/4 + Sqrt[5]/4, -1/2, 1/4 + Sqrt[5]/4}}, 
     {{1, 0, 0}, {0, -1, 0}, {0, 0, -1}}, {{1, 0, 0}, {0, 1, 0}, {0, 0, 1}}};

rotations = Select[ms, Det[#] > 0&];
reflections = Select[ms, Det[#] < 0&];

evalues = Map[Eigenvalues, rotations];
evectors = Map[Eigenvectors, rotations];

rvalues = Map[Eigenvalues, reflections];
rvectors = Map[Eigenvectors, reflections];


phi = (Sqrt[5] + 1)/2;

axisIndices = Select[ Flatten[Table[{i, j}, {i, Length[rotations]}, {j, 3}], 1], evalues[[#[[1]]]][[#[[2]]]] == 1 &];
(* {{1, 3}, {2, 1}, {3, 1}, {4, 3}, {5, 3}, {6, 1}, {6, 2}, {6, 3}} *)
axes = Map[evectors[[#[[1]]]][[#[[2]]]]&, axisIndices];

raxisIndices = Select[ Flatten[Table[{i, j}, {i, Length[reflections]}, {j, 3}], 1], rvalues[[#[[1]]]][[#[[2]]]] == -1 &];
(* {{1, 1}, {1, 2}, {1, 3}, {2, 1}, {3, 1}, {4, 1}, {5, 1}, {6, 1}} *)
raxes = Map[rvectors[[#[[1]]]][[#[[2]]]]&, raxisIndices];

Print[axes//N];
(* {{-1.61803, 2.61803, 1.}, {0., -0.381966, 1.}, {0., -0.381966, 1.}, {1.61803, 2.61803, 1.}, {1., 0., 0.}, {0., 0., 1.}, {0., 1., 0.}, {1., 0., 0.}} *)

Print[raxes//N];
(* {{0., 0., 1.}, {0., 1., 0.}, {1., 0., 0.}, {1., 0., 0.}, {1.61803, 2.61803, 1.}, {0., -0.381966, 1.}, {0., -0.381966, 1.}, {-1.61803, 2.61803, 1.}} *)


rotations[[1]] - RotationMatrix[Pi, axes[[1]]]//N
(* {-1.61803, 2.61803, 1.} *)

rotations[[2]] - RotationMatrix[2 Pi/3, axes[[2]]]//N 
(* {0., -0.381966, 1.} *)

rotations[[3]] - RotationMatrix[-2 Pi/3, axes[[3]]]//N
(* {0., -0.381966, 1.} *)

rotations[[4]] - RotationMatrix[ Pi , axes[[4]]]//N
(* {1.61803, 2.61803, 1.} *)

rotations[[5]] - RotationMatrix[ Pi , axes[[5]]]//N
(* {1., 0., 0.} *)

reflections[[2]] - (-1) IdentityMatrix[3].RotationMatrix[Pi, raxes[[4]]]//N
(* {1., 0., 0.} *)

reflections[[3]] - (-1) IdentityMatrix[3].RotationMatrix[Pi, raxes[[5]]]//N
(* {1.61803, 2.61803, 1.} *)

reflections[[4]] - (-1) IdentityMatrix[3].RotationMatrix[-2 Pi/3, raxes[[6]]]//N
(* {0., -0.381966, 1.} *)

reflections[[5]] - (-1) IdentityMatrix[3].RotationMatrix[2 Pi/3, raxes[[7]]]//N
(* {0., -0.381966, 1.} *)

reflections[[6]] - (-1) IdentityMatrix[3].RotationMatrix[Pi, raxes[[8]]]//N
(* {-1.61803, 2.61803, 1.} *)

