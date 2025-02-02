typedef struct point {
    int x;
    int y;
}point;


int main() {
    point points[10];
    for (int i = 0; i < 10; i += 1) {
        points[i].x = i * 2;
        points[i].y = i * 3;
    }

    return points[2].x + points[2].y;
}