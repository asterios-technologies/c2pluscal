int a = 0;
int glob_arr[10];

struct error{
   char* name;
   int id;
};

struct error error = {"test glob error", 1};

enum color { RED, GREEN, BLUE };

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

void arr_fun(int arr[]) {
    arr[0] = 12;
    return;
}

int main() {
    int x = 2;
    int z;
    int b;
    int c;
    int i = 0;
    int d = 0;
    int k = 0;
    int t = 0;

    int l = 2;
    int r = 41;
    int band = 13 & 11;
    int bor = 13 | 11;
    int bxor = 13 ^ 11;
    int bnot = ~13;

    int* ptr = &z;
    int** ptr_ptr = &ptr;

    *ptr_ptr = &x;
    k = **ptr_ptr;

    struct error error1 = {"test error", 3};
    struct error* error_ptr = &error1;

    int simple_arr[5] = {1, 2, 3, 4, 5};
    struct error* ptr_arr[1];
    ptr_arr[0] = error_ptr;

    z = f(x);
    g(&x);
    b = fact(3);
    a = 2;

    enum color my_color = RED;

    if (my_color == RED) {
        my_color = BLUE;
    }

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

    arr_fun(simple_arr);
    glob_arr[5] = 1;
    ptr_arr[0]->name = "test";
    t = simple_arr[2];

    l = l << 3;
    r = r >> 2;

    return 0;
}