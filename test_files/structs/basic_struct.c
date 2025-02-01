typedef struct x {
    int a;
    char b;
    int* c;
    short d[1];
}x;


int main() {
    int some = 10;
    x o;
    o.a = 1;
    o.c = &some;
    o.b = 5;
    return o.b;
}