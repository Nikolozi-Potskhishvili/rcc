
int main() {
    int* x[2];
    int y = 10;
    x[1] = &y;
    return *x[1];
}