int rec(int i) {
    if(i > 9) {
        return i;
    }
    return rec(i + 1);
}

int main() {
    return rec(0);
}