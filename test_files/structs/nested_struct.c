typedef struct inner {
    int x;
    int y;
} inner;

typedef struct outer {
    long n;
    inner i;
} outer;

int main() {
    outer var;
    var.n = 10;
    var.i.x = 11;
    var.i.y = 12;
    return var.n + var.i.y;
}