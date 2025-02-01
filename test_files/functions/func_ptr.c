void add(int* x, int y) {
    *x = *x + y;
}

int main() {
    int ptr = 10;
    int addition = 13;
    return add(&ptr, addition);
}