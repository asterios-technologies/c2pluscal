/* run.config
 * OPT: -pluscal 
 */

enum Color { RED, GREEN, BLUE };

int main() {
    enum Color color_tab[2];

    // Test enums
    enum Color my_color = RED;
    if (my_color == RED) {
        my_color = BLUE;
    }
    color_tab[0] = my_color;
    color_tab[1] = color_tab[0];
    return 0;
}
