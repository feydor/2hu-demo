#include <gtest/gtest.h>
#include "../src/safe_array.c"

#define TESTING

SafeArray sa;

TEST(SafeArrayTest, InitWorks) {
  sarray_init(&sa);
  EXPECT_EQ(a.capacity, SARRAY_INIT_CAPACITY);
  EXPECT_EQ(a.size, 0);
  EXPECT_EQ(a.add_queue.capcity, SARRAY_INIT_CAPACITY);
  EXPECT_EQ(a.add_queue.size, 0);
  EXPECT_EQ(a.remove_queue.capcity, SARRAY_INIT_CAPACITY);
  EXPECT_EQ(a.remove_queue.size, 0);
}
