// Global variables
int global_var = 0;
int global_array[10];

struct Error {
   char* name;
   int id;
};

struct Error global_error = {"test global error", 1};

enum Color { RED, GREEN, BLUE };

// Function to calculate factorial
int factorial(int n) {
    if (n == 0) {
        if (global_var == -1) {
            return 0;
        }
        return 1;
    } else {
        global_var = 1;
    }
    return n * factorial(n - 1);
}

// Function to add 2 to a given number
int add_two(int x) {
    int y = 2;
    x += y;
    return x;
}

// Function to modify a value through a pointer
void modify_value(int* x) {
    *x = 3;

    x += 1;
    *x += 1;
    return;
}

// Function to modify a struct through a pointer
void modify_struct(struct Error* ptr) {
    ptr->id = 2;
    return;
}

// Function to modify an array
void modify_array(int arr[]) {
    arr[0] = 12;
    return;
}

int main() {
    // Local variables
    int x = 2;
    int z;
    int b;
    int c;

    int i = 0;
    int d = 0;
    int k = 0;
    int t = 0;

    struct Error local_error = {"test error", 3};
    struct Error* error_ptr = &local_error;

    struct Error local_error2 = {"test error 2", 4};
    struct Error* error_ptr2 = &local_error2;

    int simple_array[5] = {1, 2, 3, 4, 5};
    struct Error* ptr_array[1];
    ptr_array[0] = error_ptr;

    // Test bitwise operations
    int left_shift = 2 << 3;
    int right_shift = 41 >> 2;
    int bit_and = 13 & 11;
    int bit_or = 13 | 11;
    int bit_xor = 13 ^ 11;
    int bit_not = ~13;

    // Test double dereferencing
    int* ptr_to_z = &z;
    int** ptr_to_ptr = &ptr_to_z;
    *ptr_to_ptr = &x;
    k = **ptr_to_ptr;

    // Test add_two function
    z = add_two(x);

    // Test modify_value function
    modify_value(&x);

    // Test factorial function
    b = factorial(3);

    // Test global variable modification
    global_var = 2;

    // Test enum and conditional statement
    enum Color my_color = RED;
    if (my_color == RED) {
        my_color = BLUE;
    }

    // Test while loop
    while (i < 6) {
        i += 1;
    }

    // Test for loop
    for (int j = 0; j < 10; j++) {
        d += i;
    }

    // Test do-while loop
    do {
        i += 1;
    } while (i < 10);

    // Test modify_struct function
    modify_struct(error_ptr);
    c = local_error.id;

    // Test modify_array function
    modify_array(simple_array);

    // Test global array modification
    global_array[5] = 1;

    // Test pointer array modification
    ptr_array[global_array[5]-1]->name = "test";
    t = simple_array[2];

    // Test pointer to struct field and array index
    char** ptr_field = &(error_ptr2->name);
    *ptr_field = "new error";

    int* ptr_index = &(global_array[2]);
    *ptr_index = 3;

    return 0;
}