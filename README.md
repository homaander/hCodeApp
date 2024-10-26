# hCodeApp

```
0. A, B, C, D, E, F
1. F-E, E-D, D-C, C-B, B-A, A
2. A-(B-A), B-A-(C-B), C-B-(D-C), D-C-(E-D), E-D-(F-E), F-E
   2A-B,    -A+2B-C,   -B+2C-D,   -C+2D-E,   -D+2E-F, F-E

3. F-E-(-D+2E-F), -D+2E-F-(-C+2D-E), -C+2D-E-(-B+2C-D), -B+2C-D-(-A+2B-C), -A+2B-C-(2A-B),  2A-B
   D-3E+2F,       C-3D+3E-F,         B-3C+3D-E,         A-3B+3C-D,         -3A+3B-C,        2A-B

4. 2A-B-(-3A+3B-C), -3A+3B-C-(A-3B+3C-D), A-3B+3C-D-(B-3C+3D-E), B-3C+3D-E-(C-3D+3E-F), C-3D+3E-F-(D-3E+2F), D-3E+2F
   5A-4B+C,         -4A+6B-4C+D,          A-4B+6C-4D+E,          B-4C+6D-4E+F,          C-4D+6E-3F,          D-3E+2F

5.
```