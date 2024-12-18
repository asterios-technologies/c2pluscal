int a = 0;
int* arr;

int fact(int n) {
    if(n==0) {
        if(a==-1) {
            return 0;
        }
        return 1;
    }

    else {
        a = 1;
    }

    return n*fact(n-1);
}

int f(int x) {
    int y = 2;
    x += y;
    return x;
}

void g(int* x) {
    *x = 3;

    // z += 1
    x += 1*sizeof(int);
    *x += 1;
    return;
}

int main() {
    int x = 2;
    int z;
    int b;

    z = f(x);
    g(&x);
    b = fact(3);
    a = 2;

    // for(int i=0; i<10; i++) {
    //     a = 2;
    // }

    return 0;
}