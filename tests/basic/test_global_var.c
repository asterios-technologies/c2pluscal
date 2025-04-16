/* run.config
 * OPT: -pluscal 
 */


/* Test global variable modification */ 

int global_var = 0;
int global_array[10];

int main() {
    global_var = 2;
    global_var++;
    return 0;
}
