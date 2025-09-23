/*
 * MIT License
 *
 * Copyright (c) 2025 mguludag
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */
#ifndef __MGUTILITY_THREAD_SYNCHRONIZED_VALUE_HPP__
#define __MGUTILITY_THREAD_SYNCHRONIZED_VALUE_HPP__
#include <functional>
#include <mutex>
#include <tuple>
#include <type_traits>
#include <utility>

namespace mgutility {
namespace thread {
namespace detail {

#ifdef _MSC_VER
#ifndef MGUTILITY_CPLUSPLUS
#define MGUTILITY_CPLUSPLUS _MSVC_LANG
#endif
#else
#ifndef MGUTILITY_CPLUSPLUS
#define MGUTILITY_CPLUSPLUS __cplusplus
#endif
#endif

/**
 * @brief Converts a list of types into void. Used for SFINAE.
 * @tparam Ts Parameter pack.
 *
 * @code
 * // Usage example:
 * template <typename T>
 * using void_t = typename void_type<T>::type;
 * @endcode
 */
template <typename... Ts>
struct void_type {
    using type = void;
};

/**
 * @brief Alias for void_t with parameter pack, usable for SFINAE detection.
 *
 * @tparam Ts Parameter pack.
 */
template <typename... Ts>
using void_t = typename void_type<Ts...>::type;

/**
 * @brief Checks if Callable is invocable with argument Arg.
 *
 * Evaluates to true if Callable can be called with Arg, otherwise false.
 * Useful for SFINAE enable_if constructs.
 *
 * @tparam Callable Callable type (lambda, function object).
 * @tparam Arg Argument type.
 *
 * @code
 * auto lambda = [](int x){ return x; };
 * static_assert(is_invocable<decltype(lambda), int>::value, "Callable with
 * int");
 * @endcode
 */
template <typename Callable, typename Arg, typename = void>
struct is_invocable : std::false_type {};

template <typename Callable, typename Arg>
struct is_invocable<
    Callable, Arg,
    void_t<decltype(std::declval<Callable>()(std::declval<Arg>()))>>
    : std::true_type {};

/**
 * @brief C++11 compatible enable_if alias template.
 * @tparam B Boolean condition.
 * @tparam T Type to enable on condition.
 */
template <bool B, typename T = void>
using enable_if_t = typename std::enable_if<B, T>::type;

/**
 * @brief Deduce result type of invoking F with Args (cross-version).
 *
 * Uses std::invoke_result_t on C++17+, otherwise std::result_of.
 *
 * @tparam F Callable type.
 * @tparam ArgTypes Argument pack.
 */
#if MGUTILITY_CPLUSPLUS > 201402L
#define MGUTILITY_NODISCARD [[nodiscard]]

template <typename F, typename... ArgTypes>
using invoke_result_t = std::invoke_result_t<F, ArgTypes...>;
#else
#define MGUTILITY_NODISCARD

template <typename F, typename... ArgTypes>
using invoke_result_t = typename std::result_of<F(ArgTypes...)>::type;
#endif

/**
 * @brief SFINAE alias: enabled only if F is callable with Args..., yields
 * result type.
 * @tparam F Callable type.
 * @tparam Args Argument types.
 */
template <typename F, typename... Args>
using enable_callable_with_t =
    enable_if_t<is_invocable<F, Args...>::value, invoke_result_t<F, Args...>>;

}  // namespace detail

/**
 * @brief Locks multiple synchronized_value objects and returns their write lock
 * guards.
 *
 * This function accepts any number of synchronized_value instances, atomically
 * locks their internal mutexes together using `std::lock`, ensuring no deadlock
 * occurs. It then returns a tuple of their respective write lock guards
 * constructed with `std::adopt_lock`.
 *
 * This guarantees that either all mutexes are locked together or none,
 * preventing partial locking and consequent deadlocks.
 *
 * @tparam SV Variadic template parameter pack of synchronized_value types.
 * @tparam Ret Tuple type of write_lock_guard_t of all passed synchronized_value
 * types.
 * @param sv Variadic references to synchronized_value instances to be locked.
 * @return A tuple holding locked write lock guards for concurrent safe access.
 *
 * @code
 * synchronized_value<int> sv1, sv2;
 *
 * // lock and synchronize multiple values
 * auto&& [first, second] = synchronize(sv1, sv2);
 * first = 10; // Safe write to sv1
 * second = 20; // Safe write to sv2
 * @endcode
 */
template <typename... SV,
          typename Ret = std::tuple<typename SV::write_lock_guard_t...>>
MGUTILITY_NODISCARD auto synchronize(SV&... sv) -> Ret;

/**
 * @brief Reference wrapper providing mutable and const access via value().
 *
 * Wraps a reference to an object and exposes explicit getters.
 *
 * @tparam T Type of referenced object.
 *
 * @code
 * int x = 42;
 * ref_wrapper<int> rw(x);
 * int& ref = rw.value();
 * @endcode
 */
template <typename T>
class ref_wrapper : std::reference_wrapper<T> {
   public:
    /**
     * @brief Constructor wrapping reference.
     * @param obj Reference to wrap.
     */
    inline explicit ref_wrapper(T& obj) noexcept
        : std::reference_wrapper<T>(obj) {}

    /**
     * @brief Get mutable reference.
     * @return Mutable reference to wrapped object.
     */
    auto value() noexcept -> T& { return std::reference_wrapper<T>::get(); }

    /**
     * @brief Get const reference.
     * @return Const reference to wrapped object.
     */
    auto value() const noexcept -> const T& {
        return std::reference_wrapper<T>::get();
    }
};

/**
 * @brief Base class providing default equality operators for wrapped reference
 * types.
 *
 * This template acts as a customization point: users may specialize or
 * inherit from this class to provide custom operator implementations suited
 * to their value types. By default, it offers `operator==` and `operator!=`
 * comparing the wrapped object to another value of type T.
 *
 * Users can specialize or derive from this class to add or override operators.
 *
 * Example usage specializing for `std::filesystem::path` demonstrating
 * equivalence comparison and path concatenation with operator/:
 * @code
 * #include <filesystem>
 * #include <iostream>
 *
 * // Specializing operators for std::filesystem::path
 * template<>
 * class operators<std::filesystem::path> : public
 * ref_wrapper<std::filesystem::path> { public: using
 * ref_wrapper<std::filesystem::path>::value;
 *
 *     explicit operators(std::filesystem::path& p) noexcept
 *         : ref_wrapper<std::filesystem::path>(p) {}
 *
 *     // Use filesystem's equivalent to compare paths semantically
 *     auto operator==(const std::filesystem::path& rhs) const -> bool {
 *         return std::filesystem::equivalent(value(), rhs);
 *     }
 *
 *     auto operator!=(const std::filesystem::path& rhs) const -> bool {
 *         return !(*this == rhs);
 *     }
 *
 *     // Concatenate paths using operator/
 *     auto operator/(const std::filesystem::path& rhs) const ->
 * std::filesystem::path { return value() / rhs;
 *     }
 * };
 *
 * void example_path_usage() {
 *     std::filesystem::path path1("/usr");
 *     std::filesystem::path path2("bin");
 *     operators<std::filesystem::path> opPath(path1);
 *
 *     auto fullPath = opPath / path2;
 *     std::cout << "Full path: " << fullPath << std::endl;
 *
 *     bool eq = (opPath == std::filesystem::path("/usr"));
 *     std::cout << "Paths equivalent? " << std::boolalpha << eq << std::endl;
 * }
 * @endcode
 *
 * @tparam T The referenced type that operator comparisons operate on.
 */
template <typename T>
class operators : public ref_wrapper<T> {
   public:
    using ref_wrapper<T>::value;

    explicit operators(T& obj) noexcept : ref_wrapper<T>{obj} {}

    /**
     * @brief Checks equality to another T.
     * @param rhs Other value for comparison.
     * @return true if equal.
     */
    auto operator==(const T& rhs) const -> bool { return value() == rhs; }

    /**
     * @brief Checks inequality to another T.
     * @param rhs Other value for comparison.
     * @return true if not equal.
     */
    auto operator!=(const T& rhs) const -> bool { return value() != rhs; }
};

/**
 * @brief Lock guard for scoped read/const access supporting pointer-like
 * semantics.
 *
 * Holds a lock guard (shared or unique) and enables transparent access via `->`
 * and `*`.
 *
 * @tparam T Value type.
 * @tparam Guard Lock guard type.
 * @tparam Trait Type transformation trait, defaults to adding const.
 *
 * @code
 * read_lock_guard<int, std::shared_lock<std::shared_mutex>> guard(value,
 * mutex); int x = *guard; // read access
 * @endcode
 */
template <typename T, typename Guard,
          template <typename> class Trait = std::add_const>
class read_lock_guard : public Guard, public operators<T> {
    using const_value_type = typename Trait<T>::type;

   public:
    using base_t = operators<T>;

    /**
     * @brief Constructs lock guard and proxy to const value.
     * @param value Value reference.
     * @param mtx Mutex or lockable object.
     * @param args Arguments for guard construction.
     */
    template <typename Lockable, typename... Args>
    inline read_lock_guard(const T& value, Lockable& mtx, Args&&... args)
        : Guard{mtx, std::forward<Args>(args)...},
          base_t{const_cast<T&>(value)} {}

    read_lock_guard(const read_lock_guard&) = delete;
    read_lock_guard& operator=(const read_lock_guard&) = delete;

    /**
     * @brief Move constructor.
     */
    read_lock_guard(read_lock_guard&& other) noexcept
        : Guard{std::move(other)}, base_t{other.value()} {}

    /**
     * @brief Const pointer-like access.
     */
    auto operator->() const -> const const_value_type* {
        return &base_t::value();
    }

    /**
     * @brief Const dereference.
     */
    MGUTILITY_NODISCARD auto operator*() const -> const const_value_type& {
        return base_t::value();
    }
};

/**
 * @brief Lock guard with mutable write access.
 *
 * Inherits from read_lock_guard but allows mutation.
 *
 * @tparam T Value type.
 * @tparam Guard Lock guard type.
 * @tparam Trait Trait for type transformation, defaults to removing const.
 *
 * @code
 * write_lock_guard<int, std::unique_lock<std::mutex>> guard(value, mutex);
 * *guard = 10; // modifies value
 * @endcode
 */
template <typename T, typename Guard,
          template <typename> class Trait = std::remove_const>
class write_lock_guard : public read_lock_guard<T, Guard, Trait> {
    using read_base = read_lock_guard<T, Guard, Trait>;
    using value_type = typename Trait<T>::type;

   public:
    /**
     * @brief Constructs lock guard with mutable access.
     * @param value Value reference.
     * @param mtx Mutex or lockable.
     * @param args Additional arguments for guard.
     */
    template <typename Lockable, typename... Args>
    inline write_lock_guard(T& value, Lockable& mtx, Args&&... args)
        : read_base{value, mtx, std::forward<Args>(args)...} {}

    write_lock_guard(const write_lock_guard&) = delete;
    write_lock_guard& operator=(const write_lock_guard&) = delete;

    /**
     * @brief Move constructor.
     */
    write_lock_guard(write_lock_guard&& other) noexcept
        : read_base{std::move(other)} {}

    /**
     * @brief Mutable pointer-like access.
     */
    auto operator->() -> value_type* { return &read_base::value(); }

    /**
     * @brief Mutable dereference.
     */
    auto operator*() -> value_type& { return read_base::value(); }

    /**
     * @brief Assignment operator for const value.
     * @param rhs New value.
     * @return Reference to *this.
     */
    auto operator=(const value_type& rhs) -> write_lock_guard& {
        read_base::value() = rhs;
        return *this;
    }

    /**
     * @brief Move assignment operator.
     * @param rhs New value (rvalue).
     * @return Reference to *this.
     */
    auto operator=(value_type&& rhs) -> write_lock_guard& {
        read_base::value() = std::move(rhs);
        return *this;
    }
};

/**
 * @brief Default locking policy using std::unique_lock for both read and write
 * operations.
 *
 * This template defines types `read_lock` and `write_lock` used to lock
 * a synchronization primitive of type `Lockable`. By default, both are aliases
 * for `std::unique_lock<Lockable>`.
 *
 * Users can specialize this template to customize locking behavior for custom
 * mutex-like types. For example, if `MySharedMutex` supports shared locking,
 * the user can specialize `lock_policy` to use `std::shared_lock` for reads.
 *
 * Example specialization for a custom lockable type:
 * @code
 * struct MySharedMutex {
 *     void lock() { // exclusive lock // }
 *     void unlock() { // exclusive unlock // }
 *     void lock_shared() { // shared lock // }
 *     void unlock_shared() { // shared unlock // }
 * };
 *
 * template <>
 * struct lock_policy<MySharedMutex> {
 *     using read_lock = std::shared_lock<MySharedMutex>;
 *     using write_lock = std::unique_lock<MySharedMutex>;
 * };
 *
 * void example() {
 *     MySharedMutex mtx;
 *     lock_policy<MySharedMutex>::read_lock rlock(mtx);
 *     lock_policy<MySharedMutex>::write_lock wlock(mtx);
 *     // Concurrent read allowed, exclusive write
 * }
 * @endcode
 *
 * @tparam Lockable The type of the mutex or lockable object.
 * @tparam Enable Reserved for SFINAE-based specializations.
 */
template <typename Lockable, typename = void>
struct lock_policy {
    using read_lock = std::unique_lock<Lockable>;
    using write_lock = std::unique_lock<Lockable>;
};

/**
 * @brief Thread-safe value wrapper providing RAII lock guards and callable
 * interface.
 *
 * Wraps an underlying value and synchronizes access using a lockable mutex.
 * Provides pointer and dereference operators to access scoped read/write
 * guards.
 *
 * @tparam T Value type.
 * @tparam Lockable Mutex type (default: std::mutex).
 * @tparam LockPolicy Locking policy (default: lock_policy).
 *
 * @code
 * synchronized_value<int> sync_int(42);
 * // Write access:
 * sync_int([](int& val) { val = 10; });
 * // Read access:
 * sync_int([](const int& val) { std::cout << val; });
 * @endcode
 */
template <typename T, typename Lockable = std::mutex,
          template <typename...> class LockPolicy = lock_policy>
class synchronized_value {
    using read_lock_t = typename lock_policy<Lockable>::read_lock;
    using write_lock_t = typename lock_policy<Lockable>::write_lock;

   public:
    using read_lock_guard_t = read_lock_guard<T, read_lock_t>;
    using write_lock_guard_t = write_lock_guard<T, write_lock_t>;

    /**
     * @brief Default constructs the underlying value.
     */
    synchronized_value() = default;

    /**
     * @brief Constructs with initial value.
     * @param value Initial value.
     */
    explicit synchronized_value(const T& value) : m_value{value} {}

    /**
     * @brief Constructs underlying value in place.
     * @param args Arguments forwarded to T constructor.
     *
     * @code
     * synchronized_value<std::vector<int>> sv(10, 42); // vector of 10 elements
     * 42
     * @endcode
     */
    template <typename... Args>
    explicit synchronized_value(Args&&... args)
        : m_value{std::forward<Args>(args)...} {}

    /**
     * @brief Copy constructs by copying the guarded value with thread safety.
     * @param other Other synchronized_value to copy from.
     *
     * @code
     * synchronized_value<int> sv1(42);
     * synchronized_value<int> sv2(sv1); // copies 42 under lock
     * @endcode
     */
    synchronized_value(const synchronized_value& other) : m_value{**other} {}

    /**
     * @brief Move constructs by moving the guarded value with thread safety.
     * @param other Other synchronized_value to move from.
     *
     * @code
     * synchronized_value<std::string> sv1("hello");
     * synchronized_value<std::string> sv2(std::move(sv1)); // moves "hello"
     * under lock
     * @endcode
     */
    synchronized_value(synchronized_value&& other) noexcept(
        std::is_nothrow_move_constructible<T>::value)
        : m_value{std::move(**other)} {}

    /**
     * @brief Copy assigns a new value to this object under mutual exclusion.
     * @param other Other synchronized_value to copy from.
     * @return Reference to *this.
     *
     * @code
     * synchronized_value<int> sv1(1), sv2(2);
     * sv1 = sv2; // sv1 becomes 2 safely
     * @endcode
     */
    auto operator=(const synchronized_value& other) -> synchronized_value& {
        if (this != &other) {
            auto&& guard = synchronize(*this, other);
            *std::get<0>(guard) = *std::get<1>(guard);
        }
        return *this;
    }

    /**
     * @brief Move assigns a new value to this object under mutual exclusion.
     * @param other Other synchronized_value to move from.
     * @return Reference to *this.
     *
     * @code
     * synchronized_value<std::string> sv1("old"), sv2("new");
     * sv1 = std::move(sv2); // sv1 becomes "new" safely
     * @endcode
     */
    auto operator=(synchronized_value&& other) noexcept(
        std::is_nothrow_move_assignable<T>::value) -> synchronized_value& {
        if (this != &other) {
            auto&& guard = synchronize(*this, other);
            *std::get<0>(guard) = std::move(*std::get<1>(guard));
        }
        return *this;
    }

    /**
     * @brief Conversion operator to type T, providing thread-safe copy.
     *
     * Copies the guarded value under lock and returns it.
     *
     * @code
     * synchronized_value<int> sv(42);
     * int x = static_cast<int>(sv); // x becomes 42 safely
     * @endcode
     */
    explicit operator T() const { return operator*(); }

    /**
     * @brief Obtains write lock guard by pointer access.
     * @return write_lock_guard_t providing mutable scoped access.
     */
    auto operator->() -> write_lock_guard_t {
        return write_lock_guard_t{m_value, m_lockable};
    }

    /**
     * @brief Obtains read lock guard by const pointer access.
     * @return read_lock_guard_t providing const access.
     */
    auto operator->() const -> read_lock_guard_t {
        return read_lock_guard_t{m_value, m_lockable};
    }

    /**
     * @brief Obtains read lock guard by const dereference.
     * @return read_lock_guard_t providing const access.
     */
    MGUTILITY_NODISCARD auto operator*() const -> read_lock_guard_t {
        return read_lock_guard_t{m_value, m_lockable};
    }

    /**
     * @brief Obtains write lock guard by dereference.
     * @return write_lock_guard_t providing mutable access.
     */
    MGUTILITY_NODISCARD auto operator*() -> write_lock_guard_t {
        return write_lock_guard_t{m_value, m_lockable};
    }

    /**
     * @brief Assigns a new value to the protected object.
     * @param value New value.
     * @return Reference to *this.
     */
    auto operator=(const T& value) -> synchronized_value& {
        operator*() = value;
        return *this;
    }

    /**
     * @brief Acquires read lock explicitly with arguments.
     * @param args Arguments forwarded to guard construction.
     * @return read_lock_guard_t for scoped const access.
     */
    template <typename... GuardArgs>
    MGUTILITY_NODISCARD auto synchronize(GuardArgs&&... args) const
        -> read_lock_guard_t {
        return read_lock_guard_t(m_value, m_lockable,
                                 std::forward<GuardArgs>(args)...);
    }

    /**
     * @brief Acquires write lock explicitly with arguments.
     * @param args Arguments forwarded to guard construction.
     * @return write_lock_guard_t for scoped mutable access.
     */
    template <typename... GuardArgs>
    MGUTILITY_NODISCARD auto synchronize(GuardArgs&&... args)
        -> write_lock_guard_t {
        return write_lock_guard_t(m_value, m_lockable,
                                  std::forward<GuardArgs>(args)...);
    }

    /**
     * @brief Invokes a callable with mutable reference under write lock.
     *
     * Enabled only if F is callable with T&.
     *
     * @param function Callable to invoke.
     * @return Result from callable.
     *
     * @code
     * synchronized_value<int> sv(100);
     * sv([](int& val) { val += 1; });
     * @endcode
     */
    template <typename F>
    auto operator()(F&& function) -> detail::enable_callable_with_t<F, T&> {
        auto guard = write_lock_guard_t{m_value, m_lockable};
        return function(*guard);
    }

    /**
     * @brief Invokes a callable with const reference under read lock.
     *
     * Enabled only if F is callable with const T&.
     *
     * @param function Callable to invoke.
     * @return Result from callable.
     *
     * @code
     * synchronized_value<int> sv(100);
     * sv([](const int& val) { std::cout << val; });
     * @endcode
     */
    template <typename F>
    auto operator()(F&& function) const
        -> detail::enable_callable_with_t<F, const T&> {
        const auto guard = read_lock_guard_t{m_value, m_lockable};
        return function(*guard);
    }

    /**
     * @brief Provides mutable access to the underlying lockable synchronization
     * primitive.
     *
     * Returns a reference to the internal mutex or shared_mutex used to protect
     * the stored value. This method is useful for advanced usage scenarios,
     * such as locking multiple synchronized_value instances simultaneously to
     * avoid deadlocks.
     *
     * The returned mutex can be passed to `std::lock` or combined with other
     * synchronization techniques.
     *
     * @return Reference to the internal lockable object.
     */
    MGUTILITY_NODISCARD auto lockable() -> Lockable& { return m_lockable; }

   private:
    T m_value;                    ///< Protected underlying value.
    mutable Lockable m_lockable;  ///< Mutex/shared_mutex guarding access.
};

/**
 * @brief Locks multiple synchronized_value objects and returns their write lock
 * guards.
 *
 * This function accepts any number of synchronized_value instances, atomically
 * locks their internal mutexes together using `std::lock`, ensuring no deadlock
 * occurs. It then returns a tuple of their respective write lock guards
 * constructed with `std::adopt_lock`.
 *
 * This guarantees that either all mutexes are locked together or none,
 * preventing partial locking and consequent deadlocks.
 *
 * @tparam SV Variadic template parameter pack of synchronized_value types.
 * @tparam Ret Tuple type of write_lock_guard_t of all passed synchronized_value
 * types.
 * @param sv Variadic references to synchronized_value instances to be locked.
 * @return A tuple holding locked write lock guards for concurrent safe access.
 *
 * @code
 * synchronized_value<int> sv1, sv2;
 *
 * // lock and synchronize multiple values
 * auto&& [first, second] = synchronize(sv1, sv2);
 * first = 10; // Safe write to sv1
 * second = 20; // Safe write to sv2
 * @endcode
 */
template <typename... SV, typename Ret>
MGUTILITY_NODISCARD auto synchronize(SV&... sv) -> Ret {
    std::lock(sv.lockable()...);
    return Ret{sv.synchronize(std::adopt_lock_t{})...};
}

}  // namespace thread
}  // namespace mgutility

#if MGUTILITY_CPLUSPLUS > 201402L
#include <shared_mutex>
/**
 * @brief Lock policy specialization for lockable supporting shared locking.
 *
 * Uses std::shared_lock for reading and std::unique_lock for writing.
 */
template <typename Lockable>
struct mgutility::thread::lock_policy<
    Lockable, mgutility::thread::detail::void_t<
                  decltype(std::declval<Lockable&>().lock_shared())>> {
    using read_lock = std::shared_lock<Lockable>;
    using write_lock = std::unique_lock<Lockable>;
};
#endif // MGUTILITY_CPLUSPLUS > 201402L

#endif // __MGUTILITY_THREAD_SYNCHRONIZED_VALUE_HPP__
