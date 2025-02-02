typedef struct rec {
    rec* self;
    int num;
} rec;


int main() {
    rec x;
    rec y;
    y.num = 10;

    x.self = &y;
    x.num = 13;
    return x.self->num;
}