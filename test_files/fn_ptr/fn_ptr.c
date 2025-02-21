int add(int x, int y) {
    return x + y;
}

int main() {
    int (*fn_ptr)(int, int) = add;
    return fn_ptr(2, 2);
}