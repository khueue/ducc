/* Test file for semantic errors. Contains exactly one error. */

int q(int a, int b, int c);

int main(void) {
  1 + q(1, 3);	// Too few arguments to function 'q'
}
