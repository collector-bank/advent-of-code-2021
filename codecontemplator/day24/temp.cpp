#include <vector>
#include <string>
#include <iostream>

using namespace std;

void executeBase(int &x, int& y, int& z, int& w) {
}

void execute0(int d, int &x, int& y, int& z, int& w) {
  w = d;
  x *= 0;
  x += z;
  x %= 26;
  z /= 1;
  x += 12;
  x = x == w ? 1 : 0;
  x = x == 0 ? 1 : 0;
  y *= 0;
  y += 25;
  y *= x;
  y += 1;
  z *= y;
  y *= 0;
  y += w;
  y += 6;
  y *= x;
  z += y;
}

void execute1(int d, int &x, int& y, int& z, int& w) {
  w = d;
  x *= 0;
  x += z;
  x %= 26;
  z /= 1;
  x += 11;
  x = x == w ? 1 : 0;
  x = x == 0 ? 1 : 0;
  y *= 0;
  y += 25;
  y *= x;
  y += 1;
  z *= y;
  y *= 0;
  y += w;
  y += 12;
  y *= x;
  z += y;
}

void execute2(int d, int &x, int& y, int& z, int& w) {
  w = d;
  x *= 0;
  x += z;
  x %= 26;
  z /= 1;
  x += 10;
  x = x == w ? 1 : 0;
  x = x == 0 ? 1 : 0;
  y *= 0;
  y += 25;
  y *= x;
  y += 1;
  z *= y;
  y *= 0;
  y += w;
  y += 5;
  y *= x;
  z += y;
}

void execute3(int d, int &x, int& y, int& z, int& w) {
  w = d;
  x *= 0;
  x += z;
  x %= 26;
  z /= 1;
  x += 10;
  x = x == w ? 1 : 0;
  x = x == 0 ? 1 : 0;
  y *= 0;
  y += 25;
  y *= x;
  y += 1;
  z *= y;
  y *= 0;
  y += w;
  y += 10;
  y *= x;
  z += y;
}

void execute4(int d, int &x, int& y, int& z, int& w) {
  w = d;
  x *= 0;
  x += z;
  x %= 26;
  z /= 26;
  x += -16;
  x = x == w ? 1 : 0;
  x = x == 0 ? 1 : 0;
  y *= 0;
  y += 25;
  y *= x;
  y += 1;
  z *= y;
  y *= 0;
  y += w;
  y += 7;
  y *= x;
  z += y;
}

void execute5(int d, int &x, int& y, int& z, int& w) {
  w = d;
  x *= 0;
  x += z;
  x %= 26;
  z /= 1;
  x += 14;
  x = x == w ? 1 : 0;
  x = x == 0 ? 1 : 0;
  y *= 0;
  y += 25;
  y *= x;
  y += 1;
  z *= y;
  y *= 0;
  y += w;
  y += 0;
  y *= x;
  z += y;
}

void execute6(int d, int &x, int& y, int& z, int& w) {
  w = d;
  x *= 0;
  x += z;
  x %= 26;
  z /= 1;
  x += 12;
  x = x == w ? 1 : 0;
  x = x == 0 ? 1 : 0;
  y *= 0;
  y += 25;
  y *= x;
  y += 1;
  z *= y;
  y *= 0;
  y += w;
  y += 4;
  y *= x;
  z += y;
}

void execute7(int d, int &x, int& y, int& z, int& w) {
  w = d;
  x *= 0;
  x += z;
  x %= 26;
  z /= 26;
  x += -4;
  x = x == w ? 1 : 0;
  x = x == 0 ? 1 : 0;
  y *= 0;
  y += 25;
  y *= x;
  y += 1;
  z *= y;
  y *= 0;
  y += w;
  y += 12;
  y *= x;
  z += y;
}

void execute8(int d, int &x, int& y, int& z, int& w) {
  w = d;
  x *= 0;
  x += z;
  x %= 26;
  z /= 1;
  x += 15;
  x = x == w ? 1 : 0;
  x = x == 0 ? 1 : 0;
  y *= 0;
  y += 25;
  y *= x;
  y += 1;
  z *= y;
  y *= 0;
  y += w;
  y += 14;
  y *= x;
  z += y;
}

void execute9(int d, int &x, int& y, int& z, int& w) {
  w = d;
  x *= 0;
  x += z;
  x %= 26;
  z /= 26;
  x += -7;
  x = x == w ? 1 : 0;
  x = x == 0 ? 1 : 0;
  y *= 0;
  y += 25;
  y *= x;
  y += 1;
  z *= y;
  y *= 0;
  y += w;
  y += 13;
  y *= x;
  z += y;
}

void execute10(int d, int &x, int& y, int& z, int& w) {
  w = d;
  x *= 0;
  x += z;
  x %= 26;
  z /= 26;
  x += -8;
  x = x == w ? 1 : 0;
  x = x == 0 ? 1 : 0;
  y *= 0;
  y += 25;
  y *= x;
  y += 1;
  z *= y;
  y *= 0;
  y += w;
  y += 10;
  y *= x;
  z += y;
}

void execute11(int d, int &x, int& y, int& z, int& w) {
  w = d;
  x *= 0;
  x += z;
  x %= 26;
  z /= 26;
  x += -4;
  x = x == w ? 1 : 0;
  x = x == 0 ? 1 : 0;
  y *= 0;
  y += 25;
  y *= x;
  y += 1;
  z *= y;
  y *= 0;
  y += w;
  y += 11;
  y *= x;
  z += y;
}

void execute12(int d, int &x, int& y, int& z, int& w) {
  w = d;
  x *= 0;
  x += z;
  x %= 26;
  z /= 26;
  x += -15;
  x = x == w ? 1 : 0;
  x = x == 0 ? 1 : 0;
  y *= 0;
  y += 25;
  y *= x;
  y += 1;
  z *= y;
  y *= 0;
  y += w;
  y += 9;
  y *= x;
  z += y;
}

void execute13(int d, int &x, int& y, int& z, int& w) {
  w = d;
  x *= 0;
  x += z;
  x %= 26;
  z /= 26;
  x += -8;
  x = x == w ? 1 : 0;
  x = x == 0 ? 1 : 0;
  y *= 0;
  y += 25;
  y *= x;
  y += 1;
  z *= y;
  y *= 0;
  y += w;
  y += 9;
  y *= x;
  z += y;
}

int main() {
  int x = 0; int y = 0; int z = 0; int w = 0;
  for(int d0 = 1; d0 <= 9; ++d0) { 
    int x0 = x; int y0 = y; int z0 = z; int w0 = w;
    execute0(d0, x0, y0, z0, w0);
    for(int d1 = 1; d1 <= 9; ++d1) { 
      int x1 = x0; int y1 = y0; int z1 = z0; int w1 = w0;
      execute1(d1, x1, y1, z1, w1);
      for(int d2 = 1; d2 <= 9; ++d2) { 
        int x2 = x1; int y2 = y1; int z2 = z1; int w2 = w1;
        execute2(d2, x2, y2, z2, w2);
        for(int d3 = 1; d3 <= 9; ++d3) { 
          int x3 = x2; int y3 = y2; int z3 = z2; int w3 = w2;
          execute3(d3, x3, y3, z3, w3);
          for(int d4 = 1; d4 <= 9; ++d4) { 
            int x4 = x3; int y4 = y3; int z4 = z3; int w4 = w3;
            execute4(d4, x4, y4, z4, w4);
            cout << "digit=" << d0 << d1 << d2 << d3 << d4 << 0 << 0 << 0 << 0 << 0 << 0 << 0 << 0 << 0 << endl;
            for(int d5 = 1; d5 <= 9; ++d5) { 
              int x5 = x4; int y5 = y4; int z5 = z4; int w5 = w4;
              execute5(d5, x5, y5, z5, w5);
              for(int d6 = 1; d6 <= 9; ++d6) { 
                int x6 = x5; int y6 = y5; int z6 = z5; int w6 = w5;
                execute6(d6, x6, y6, z6, w6);
                for(int d7 = 1; d7 <= 9; ++d7) { 
                  int x7 = x6; int y7 = y6; int z7 = z6; int w7 = w6;
                  execute7(d7, x7, y7, z7, w7);
                  for(int d8 = 1; d8 <= 9; ++d8) { 
                    int x8 = x7; int y8 = y7; int z8 = z7; int w8 = w7;
                    execute8(d8, x8, y8, z8, w8);
                    for(int d9 = 1; d9 <= 9; ++d9) { 
                      int x9 = x8; int y9 = y8; int z9 = z8; int w9 = w8;
                      execute9(d9, x9, y9, z9, w9);
                      for(int d10 = 1; d10 <= 9; ++d10) { 
                        int x10 = x9; int y10 = y9; int z10 = z9; int w10 = w9;
                        execute10(d10, x10, y10, z10, w10);
                        for(int d11 = 1; d11 <= 9; ++d11) { 
                          int x11 = x10; int y11 = y10; int z11 = z10; int w11 = w10;
                          execute11(d11, x11, y11, z11, w11);
                          for(int d12 = 1; d12 <= 9; ++d12) { 
                            int x12 = x11; int y12 = y11; int z12 = z11; int w12 = w11;
                            execute12(d12, x12, y12, z12, w12);
                            for(int d13 = 1; d13 <= 9; ++d13) { 
                              int x13 = x12; int y13 = y12; int z13 = z12; int w13 = w12;
                              execute13(d13, x13, y13, z13, w13);
                              if (z13 == 0) { 
                                cout << "result = ";
                                cout << d0; 
                                cout << d1; 
                                cout << d2; 
                                cout << d3; 
                                cout << d4; 
                                cout << d5; 
                                cout << d6; 
                                cout << d7; 
                                cout << d8; 
                                cout << d9; 
                                cout << d10; 
                                cout << d11; 
                                cout << d12; 
                                cout << d13; 
                                cout << endl;
                                return 0;
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  return 1;
}
