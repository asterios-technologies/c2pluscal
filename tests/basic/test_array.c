/* run.config
 * OPT: -pluscal 
 */

int global_array[10];

struct Error {
   char* name;
   int id;
};

struct Error global_error = {"test global error", 1};

// Function to modify an array
void modify_array(int arr[]) {
    arr[0] = 12;
    return;
}


int main() {
    int t;
    struct Error local_error = {"test error", 3};
    struct Error* error_ptr = &local_error;
    struct Error local_error2 = {"test error 2", 4};
    struct Error* error_ptr2 = &local_error2;
    int simple_array[5] = {1, 2, 3, 4, 5};
    struct Error* ptr_array[1];
    ptr_array[0] = error_ptr;

    // Modify array through a function call
    modify_array(simple_array);

    // Test global array modification
    global_array[5] = 1;

    // Test pointer array modification
    ptr_array[global_array[5]-1]->name = "test";
    t = simple_array[2];

    // Test pointer to an array
    int* ptr_index = &(global_array[2]);
    *ptr_index = 3;

    return 0;
}
