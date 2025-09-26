# mgutility::thread::synchronized_value

A modern C++ thread-safe value wrapper with flexible locking strategies and convenient RAII guards.

## Features

- Encapsulates any value type `T` with internal mutex synchronization.
- Supports customizable lock policies to use `std::unique_lock`, `std::shared_lock`, or user-defined lock types.
- Provides read and write lock guards with transparent pointer-like and dereference semantics.
- Callable interface supporting thread-safe access with lambdas.
- Customizable operator support for wrapped values via specialization of `operators` template.
- Safe simultaneous locking of multiple `synchronized_value` instances without deadlock.

## C++ Version Compatibility

- Compatible with **C++11** as a minimum.
- If compiled with **C++17** or later, additional features such as `std::shared_mutex`, `std::shared_lock`, and `std::invoke_result_t` are automatically enabled.

## Usage

```c++
#include <mgutility/thread/synchronized_value.hpp>
#include <iostream>
#include <string>
#include <shared_mutex>

int main() {
    mgutility::thread::synchronized_value<int> sv(42);

    // Read access
    sv([](const int& val) {
        std::cout << "Value is " << val << "\n";
    });

    // Write access
    sv([](int& val) {
        val = 100;
    });

    // Classic lock guard style
    {
        auto guard = sv.synchronize(/* [optional] additional args for guard object like std::adopt_lock_t{} etc. */);
        std::cout << "Read with lock: " << *guard << "\n";
        guard = 50; // assign new value directly (same as *guard = 50)
    }
}
```

## Customization Points

### Specializing `lock_policy`

You can customize which lock types are used for reading and writing by specializing the `lock_policy` for your own mutex-like types. This allows transparent use of shared mutexes, custom spinlocks, or other synchronization primitives.

Example - specialize for a custom shared mutex:

```c++
struct MySharedMutex {
    void lock() { /* exclusive lock implementation */ }
    void unlock() { /* exclusive unlock */ }
    void lock_shared() { /* shared lock */ }
    void unlock_shared() { /* shared unlock */ }
};

template <>
struct mgutility::thread::lock_policy<MySharedMutex> {
    using read_lock = std::shared_lock<MySharedMutex>;
    using write_lock = std::unique_lock<MySharedMutex>;
};

int main() {
    mgutility::thread::synchronized_value<int, MySharedMutex> sv(42);

}

```

### Specializing `operators`

The `operators<T>` template provides default equality and inequality operators that operate on the wrapped reference of type `T`. Users can specialize or inherit from this template to add or override operators for custom or complex types.

Example specialization for `std::filesystem::path` demonstrating custom comparison and path concatenation:

```c++
#include <filesystem>

template <>
class mgutility::thread::operators<std::filesystem::path> : public ref_wrapper<std::filesystem::path> {
public:
    using ref_wrapper<std::filesystem::path>::value;

    explicit operators(std::filesystem::path& p) noexcept
        : ref_wrapper<std::filesystem::path>(p) {}

    auto operator==(const std::filesystem::path& rhs) const -> bool {
        return std::filesystem::equivalent(value(), rhs);
    }

    auto operator!=(const std::filesystem::path& rhs) const -> bool {
        return !(*this == rhs);
    }

    auto operator/(const std::filesystem::path& rhs) const -> std::filesystem::path& {
        return value() / rhs;
    }

    auto operator/(const std::string& rhs) const -> std::filesystem::path& {
        return value() / rhs;
    }
};

int main() {
    mgutility::thread::synchronized_value<std::filesystem::path> sv("/usr/local");

    {
        auto&& path = sv.synchronize();
        const auto binFolder = path / "bin";
    }

}
```

## License

MIT License, see the [LICENSE](LICENSE) file.

## Contribution

Contributions, bug reports, and feature requests are welcome! Feel free to check the issues page.

---
