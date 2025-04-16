/* run.config
 * OPT: -pluscal 
 */

struct Error {
   char* name;
   int id;
};

struct Error global_error = {"test global error", 1};

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

int main() {
    // Local variables
    int x = 2;
    int z, k;

    struct Error local_error = {"test error", 3};
    struct Error* error_ptr = &local_error;

    struct Error local_error2 = {"test error 2", 4};
    struct Error* error_ptr2 = &local_error2;

    struct Error* ptr_array[1];
    ptr_array[0] = error_ptr;


    // Test double dereferencing
    int* ptr_to_z = &z;
    int** ptr_to_ptr = &ptr_to_z;
    *ptr_to_ptr = &x;
    k = **ptr_to_ptr;


    // Test modify_value function
    modify_value(&x);

    // Test modify_struct function
    modify_struct(error_ptr);

    // Test pointer to struct field and array index
    char** ptr_field = &(error_ptr2->name);
    *ptr_field = "new error";

    return 0;
}
