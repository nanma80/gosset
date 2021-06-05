#!/usr/bin/env wolframscript

vertices = Get["data/leech_vertices.wl"];
Print[Length[vertices]];

vGroTsen = Get["external_data/leech-lattice-first-shell.wl"];
Print[Length[vGroTsen]];

int = Intersection[vertices, vGroTsen];
Print[Length[int]];

comp1 = Complement[vertices, vGroTsen];
Print[Length[comp1]];

comp2 = Complement[vGroTsen, vertices];
Print[Length[comp2]];

