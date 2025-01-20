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
    // int d = 0;

    struct error error1 = {"test error", 2};
    struct error* error_ptr = &error1;

    z = f(x);
    g(&x);
    b = fact(3);
    a = 2;

    struct_fun(error_ptr);
    c = error1.id;

    // for(int i=0; i<10; i++) {
    //     d += i;
    // }

    return 0;
}