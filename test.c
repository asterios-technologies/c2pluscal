int a = 0;

struct error{
   char* name;
   int id;
};

struct error error = {"test glob error", 1};

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
    // *(x+1*sizeof(int)) += 1;
    return;
}

void struct_fun(struct error* ptr) {
    ptr->id = 2;

    return;
}

int main() {
    int x = 2;
    int z;
    int b;
    int c;
    int i = 0;
    int d = 0;

    struct error error1 = {"test error", 3};
    struct error* error_ptr = &error1;

    z = f(x);
    g(&x);
    b = fact(3);
    a = 2;

    while(i < 6) {
        i += 1;
    }

    for(int j=0; j<10; j++) {
        d += i;
    }

    do {
        i += 1;
    } while(i < 10);

    struct_fun(error_ptr);
    c = error1.id;

    return 0;
}