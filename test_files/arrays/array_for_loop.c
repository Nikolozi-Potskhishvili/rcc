int main() {
    int x[4];
    for(int y = 0; y < 4; y = y + 1) {
        x[y] = y;
    }

    return x[2] + x[3];
}