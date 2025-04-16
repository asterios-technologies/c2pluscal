/* run.config
 * OPT: -pluscal 
 */

 /* Test structs manipulation */ 

struct Error {
   char* name;
   int id;
};

struct Error global_error = {"test global error", 1};

// Function to modify a struct through a pointer
void modify_struct(struct Error* ptr) {
    ptr->id = 2;
    return;
}

int main() {
    int x = 1;
    int c;

    struct Error local_error = {"test error", 3};
    struct Error* error_ptr = &local_error;
    struct Error local_error2 = {"test error 2", 4};
    struct Error* error_ptr2 = &local_error2;
    struct Error* ptr_array[1];

    // Test modify_struct function
    modify_struct(error_ptr);
    c = local_error.id;

    // Test pointer array modification
    ptr_array[x-1]->name = "test";

    // Test pointer to struct field and array index
    char** ptr_field = &(error_ptr2->name);
    *ptr_field = "new error";
    return 0;
}
