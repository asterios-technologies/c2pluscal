/* run.config
 * OPT: -pluscal 
 */

// Global variable
int global_var = 0;

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

int main() {
    // Local variable
    int b;

    // Test factorial function
    b = factorial(3);
    
    return 0;
}
