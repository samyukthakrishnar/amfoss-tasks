#include <stdio.h>

void pyramid(int n) {
    for (int i = 1; i < n; i++) {
        for (int j = 0; j < n - i; j++) printf(" ");
        if (i == 1) {
            printf("*\n");
        } else {
            printf("*");
            for (int j = 0; j < 2*i - 3; j++) printf(" ");
            printf("*\n");
        }
    }
    for (int j = 0; j < 2*n - 1; j++) printf("*");
    printf("\n");
}

int main() {
    int n;
    FILE *f = fopen("input.txt", "r");
    fscanf(f, "%d", &n);
    fclose(f);
    pyramid(n);
    return 0;
}
