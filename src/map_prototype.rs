use std::collections::HashMap;
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};

struct HashOnlyMap<V> {
    inner: HashMap<u64, V>,
}

impl<V> HashOnlyMap<V> {
    fn new() -> Self {
        HashOnlyMap {
            inner: HashMap::new(),
        }
    }

    fn insert<K: Hash>(&mut self, key: &K, value: V) {
        let mut hasher = DefaultHasher::new();
        key.hash(&mut hasher);
        let h = hasher.finish();
        // Collision silently overwrites previous entry
        self.inner.insert(h, value);
    }

    fn get<K: Hash>(&self, key: &K) -> Option<&V> {
        let mut hasher = DefaultHasher::new();
        key.hash(&mut hasher);
        self.inner.get(&hasher.finish())
    }
}
