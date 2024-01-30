#include "override.h"
#include <iostream>
#include <thread>

int main() {
    record_try();
    std::this_thread::sleep_for(std::chrono::seconds(2));
    record_try();
    std::this_thread::sleep_for(std::chrono::seconds(2));
    record_try();
    std::this_thread::sleep_for(std::chrono::seconds(2));
    record_try();
}