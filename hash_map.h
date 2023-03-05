#include <algorithm>
#include <cmath>
#include <initializer_list>
#include <iostream>
#include <memory>
#include <new>
#include <stdexcept>
#include <type_traits>
#include <utility>
#include <vector>

template <typename Key, typename Value>
class HashValue {
   public:
    using pair_type = std::pair<const Key, Value>;
    using ref_type = std::pair<Key&, Value&>;
    using rref_type = std::pair<Key&&, Value&&>;

   private:
    pair_type value_;

   public:
    // HashValue(const Key& key, const Value& value) : value_{key, value} {}
    // HashValue(Key&& key, Value&& value)
    //     : value_(std::move(key), std::move(value)) {}
    HashValue() = default;
    HashValue(const Key& key) : value_(key, Value()) {}
    HashValue(Key&& key) : value_(std::move(key), Value()) {}
    HashValue(const pair_type& value) : value_(value) {}
    HashValue(const HashValue& value) : value_(value.get()) {}
    HashValue(HashValue&& value) : value_(value.rref()) {}
    ~HashValue() = default;

    pair_type& get() { return *std::launder(std::addressof(value_)); }
    const pair_type& get() const {
        return *std::launder(std::addressof(value_));
    }
    ref_type ref() {
        return ref_type(const_cast<Key&>(value_.first), value_.second);
    }
    rref_type rref() {
        return rref_type(std::move(const_cast<Key&>(value_.first)),
                         std::move(value_.second));
    }

    HashValue& operator=(const pair_type& value) {
        ref() = value;
        return *this;
    }
    HashValue& operator=(const HashValue& other) { return *this = other.get(); }
    HashValue& operator=(HashValue&& other) {
        ref() = other.rref();
        return *this;
    }

    void swap(HashValue& other) { std::swap(*this, other); }
};

const size_t UNOCCUPIED = -1;

template <typename Key, typename Value, typename Hash = std::hash<Key>>
class HashBucket {
   public:
    using hash_value_type = HashValue<Key, Value>;
    using pair_type = typename hash_value_type::pair_type;

    HashBucket() = default;
    HashBucket(const HashBucket& bucket)
        : hash_(bucket.hash_), distance_(bucket.distance_) {
        if (!empty()) {
            new (value_) hash_value_type(bucket.get());
        }
    }
    ~HashBucket() {
        if (!empty()) {
            destroy_value();
        }
    }

    hash_value_type& value() {
        return *std::launder(
            reinterpret_cast<hash_value_type*>(std::addressof(value_)));
    }
    const hash_value_type& value() const {
        return *std::launder(
            reinterpret_cast<const hash_value_type*>(std::addressof(value_)));
    }
    size_t hash() const { return hash_; }
    size_t distance() const { return distance_; }
    pair_type& get() { return value().get(); }
    const pair_type& get() const { return value().get(); }
    bool empty() const { return distance_ == UNOCCUPIED; }

    void set_distance(size_t distance) { distance_ = distance; }
    void set(hash_value_type&& value, size_t hash, size_t distance) {
        clear();
        new (value_) hash_value_type(std::move(value));
        hash_ = hash;
        set_distance(distance);
    }
    void clear() {
        if (!empty()) {
            destroy_value();
        }
        set_distance(UNOCCUPIED);
    }

    // void swap(hash_value_type& hash_value) { std::swap(hash_value, value());
    // }
    void swap(hash_value_type& hash_value, size_t& hash, size_t& distance) {
        std::swap(hash_value, value());
        std::swap(hash_, hash);
        std::swap(distance_, distance);
    }

   private:
    void destroy_value() { value().~hash_value_type(); }

    alignas(hash_value_type) unsigned char value_[sizeof(hash_value_type)];
    size_t hash_;
    size_t distance_ = UNOCCUPIED;
};

const float MAX_LOAD_FACTOR = 0.8;
const float MIN_LOAD_FACTOR = 0.2;

template <typename Key, typename Value, typename Hash = std::hash<Key>>
class HashMap {
   public:
    using bucket_type = HashBucket<Key, Value, Hash>;
    using hash_value_type = HashValue<Key, Value>;
    using pair_type = typename hash_value_type::pair_type;
    using container_type = std::vector<bucket_type>;

    template <bool IsConst>
    class HashIterator {
       public:
        using iterator_type =
            typename std::conditional<IsConst,
                                      typename container_type::const_iterator,
                                      typename container_type::iterator>::type;
        using reference = decltype(std::declval<iterator_type>()->get());
        using pointer = decltype(&std::declval<iterator_type>()->get());

        reference operator*() const { return current_->get(); }
        pointer operator->() const { return &current_->get(); }

        HashIterator() = default;
        HashIterator(const iterator_type& current, const iterator_type& end)
            : current_(current), end_(end) {}

        void advance() {
            if (current_ == end_) {
                return;
            }
            do {
                ++current_;
            } while (current_ != end_ && current_->empty());
        }

        HashIterator& operator++() {
            advance();
            return *this;
        }
        HashIterator operator++(int) {
            HashIterator copy(*this);
            advance();
            return copy;
        }

        bool operator==(const HashIterator& other) const {
            if (other.current_ == other.end_) {
                return current_ == end_;
            }
            return current_ == other.current_;
        }
        bool operator!=(const HashIterator& other) const {
            return !(*this == other);
        };

       private:
        iterator_type current_;
        iterator_type end_;
    };

    using iterator = HashIterator<false>;
    using const_iterator = HashIterator<true>;

    HashMap(Hash hasher = Hash(), size_t count = 0)
        : hasher_(hasher),
          capacity_(get_capacity(count)),
          mask_(capacity_ ? capacity_ - 1 : 0),
          buckets_(capacity_) {
        update_sizes();
    }
    HashMap(std::initializer_list<pair_type> list, Hash hasher = Hash())
        : HashMap(hasher, list.size()) {
        for (const auto& value : list) {
            insert(value);
        }
    }
    template <typename It>
    HashMap(It begin, It end, Hash hasher = Hash()) : HashMap(hasher) {
        for (auto it = begin; it != end; ++it) {
            insert(*it);
        }
    }

    Hash hash_function() const { return hasher_; }

    bool empty() const { return size_ == 0; }
    std::size_t size() const { return size_; }

    iterator begin() {
        iterator it{buckets_.begin(), buckets_.end()};
        if (!buckets_.empty() && buckets_.begin()->empty()) {
            it.advance();
        }
        return it;
    }
    const_iterator begin() const {
        const_iterator it{buckets_.cbegin(), buckets_.cend()};
        if (!buckets_.empty() && buckets_.cbegin()->empty()) {
            it.advance();
        }
        return it;
    }
    iterator end() { return iterator{buckets_.end(), buckets_.end()}; }
    const_iterator end() const {
        return const_iterator{buckets_.end(), buckets_.end()};
    }
    iterator position_iterator(size_t position) {
        return iterator{buckets_.begin() + position, buckets_.end()};
    }
    const_iterator position_iterator(size_t position) const {
        return const_iterator{buckets_.begin() + position, buckets_.end()};
    }

    bool check(size_t position, const Key& key, size_t hash) const {
        return !empty() && !buckets_[position].empty() &&
               buckets_[position].hash() == hash &&
               buckets_[position].get().first == key;
    }
    iterator find(const Key& key) {
        if (empty()) {
            return end();
        }
        size_t hash = hasher_(key);
        size_t position = find_bucket(key, hash);
        if (!check(position, key, hash)) {
            return end();
        }
        return position_iterator(position);
    }
    const_iterator find(const Key& key) const {
        if (empty()) {
            return end();
        }
        size_t hash = hasher_(key);
        size_t position = find_bucket(key, hash);
        if (!check(position, key, hash)) {
            return end();
        }
        return position_iterator(position);
    };

    Value& at(const Key& key) {
        if (empty()) {
            throw std::out_of_range("Key not found");
        }
        size_t hash = hasher_(key);
        size_t position = find_bucket(key, hash);
        if (!check(position, key, hash)) {
            throw std::out_of_range("Key not found");
        }
        return buckets_[position].get().second;
    }
    const Value& at(const Key& key) const {
        if (empty()) {
            throw std::out_of_range("Key not found");
        }
        size_t hash = hasher_(key);
        size_t position = find_bucket(key, hash);
        if (!check(position, key, hash)) {
            throw std::out_of_range("Key not found");
        }
        return buckets_[position].get().second;
    }
    Value& operator[](const Key& key) {
        size_t hash = hasher_(key);
        size_t position = find_bucket(key, hash);
        if (!check(position, key, hash)) {
            position = insert(hash_value_type(key), hash);
        }
        return buckets_[position].get().second;
    }

    void insert(const pair_type& value) {
        insert(hash_value_type{value}, hasher_(value.first));
    }
    void insert(hash_value_type&& value) {
        insert(std::move(value), hasher_(value.get().first));
    }
    size_t insert(hash_value_type&& value, size_t hash) {
        size_t position = find_bucket(value.get().first, hash);
        if (check(position, value.get().first, hash)) {
            return position;
        }
        if (size_ == max_size_) {
            rehash(get_capacity(size_ + 1));
        }
        position = get_ideal(hash);
        size_t distance = 0;
        size_t answer = UNOCCUPIED;
        while (!buckets_[position].empty()) {
            if (buckets_[position].distance() < distance) {
                buckets_[position].swap(value, hash, distance);
                if (answer == UNOCCUPIED) {
                    answer = position;
                }
            }
            position = next_bucket(position);
            ++distance;
        }
        buckets_[position].set(std::move(value), hash, distance);
        ++size_;
        return answer == UNOCCUPIED ? position : answer;
    };

    void erase(const Key& key) { erase(key, hasher_(key)); }
    void erase(const Key& key, size_t hash) {
        size_t position = find_bucket(key, hash);
        if (!check(position, key, hash)) {
            return;
        }
        buckets_[position].clear();
        --size_;
        if (empty()) {
            return;
        } else if (size_ < min_size_) {
            rehash(get_capacity(size_));
            return;
        }
        for (size_t to_swap = next_bucket(position);
             buckets_[to_swap].distance() != 0;
             to_swap = next_bucket(to_swap)) {
            if (buckets_[to_swap].empty()) {
                continue;
            }
            auto& value = buckets_[to_swap].value();
            size_t hash = buckets_[to_swap].hash();
            size_t ideal = get_ideal(hash);
            buckets_[position].set(std::move(value), hash,
                                   get_distance(ideal, position));
            buckets_[to_swap].clear();
            position = next_bucket(position);
        }
    }

    size_t next_bucket(size_t bucket) const {
        return (bucket + 1) & (capacity_ - 1);
    }
    static size_t get_capacity(size_t count) {
        if (count == 0) {
            return 0;
        }
        size_t size = 1;
        while (size < count) {
            size <<= 1;
        }
        return size << 1;
    }
    size_t get_ideal(size_t hash) const { return hash & mask_; }
    size_t get_distance(size_t position_1, size_t position_2) {
        return (position_2 - position_1) & mask_;
    }

    size_t find_bucket(const Key& key, size_t hash) const {
        size_t bucket = get_ideal(hash);
        size_t distance = 0;
        if (empty()) {
            return bucket;
        }
        for (; !buckets_[bucket].empty() &&
               buckets_[bucket].distance() >= distance;
             bucket = next_bucket(bucket), ++distance) {
            if (buckets_[bucket].hash() == hash &&
                buckets_[bucket].get().first == key) {
                return bucket;
            }
        }
        return bucket;
    }
    void clear() {
        size_ = capacity_ = mask_ = max_size_ = min_size_ = 0;
        buckets_.clear();
    }
    void rehash(size_t size) {
        container_type old_buckets(size);
        buckets_.swap(old_buckets);
        capacity_ = size;
        mask_ = capacity_ - 1;
        size_ = 0;
        update_sizes();
        for (auto&& cur : old_buckets) {
            if (!cur.empty()) {
                insert(std::move(cur.value()));
            }
        }
    }
    void update_sizes() {
        max_size_ =
            std::min<size_t>(mask_, std::ceil(MAX_LOAD_FACTOR * capacity_));
        min_size_ = std::max<float>(0, std::floor(MIN_LOAD_FACTOR * capacity_));
    }

   private:
    Hash hasher_;
    size_t size_ = 0;
    size_t capacity_;
    size_t mask_;
    container_type buckets_;

    size_t max_size_ = 0;
    size_t min_size_ = 0;
};
