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

    // Test add_two function
    z = add_two(x);
    
}