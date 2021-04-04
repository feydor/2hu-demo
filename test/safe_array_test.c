/* entry point for testrunner */
#include "munit/munit.h"
#include "../include/safe_array.h"

/* BEGIN tests */
static MunitResult init_works(const MunitParameter params[], void *safe_array) {
  SafeArray *sa = (SafeArray *) safe_array;
  (void) params; // silence compiler warnings for unused params
  munit_assert_int(sa->capacity, ==, SARRAY_INIT_CAPACITY);
  munit_assert_int(sa->size, ==, 0);
  return MUNIT_OK;
}

/* setup for tests */
static void* test_setup(const MunitParameter params[], void *user_data) {
  return strdup("Hello, world!");
}

/* teardown for tests */
static void test_tear_down(void *sa) {
  free(sa);
}

/* array of tests */
MunitTest tests[] = {
  {
    "/init_works", /* name */
    init_works, /* test */
    test_setup, /* setup */
    test_tear_down, /* tear_down */
    MUNIT_TEST_OPTION_NONE, /* options */
    NULL /* parameters */
  },
  /* Mark the end of the array with an entry where the test
   * function is NULL */
  { NULL, NULL, NULL, NULL, MUNIT_TEST_OPTION_NONE, NULL }
};

/* suit containing an array of tests */
static const MunitSuite suite = {
  "/safe_array_tests", /* name */
  tests, /* tests */
  NULL, /* suites */
  1, /* iterations */
  MUNIT_SUITE_OPTION_NONE /* options */
};

int main(int argc, char *argv[MUNIT_ARRAY_PARAM(argc + 1)]) {
  SafeArray *sa;
  sarray_init(sa);

  return munit_suite_main(&suite, (void *) sa, argc, argv);
}



