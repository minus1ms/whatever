use crate::{reverse::rev_compute_digest, sha256::compute_digest};

// turn digest back into h
#[test]
fn test_digest() {
    let example_digest: Vec<[u8; 32]> = vec![[
        110, 170, 144, 21, 178, 180, 216, 191, 41, 58, 74, 53, 243, 129, 51, 210, 232, 101, 199,
        41, 125, 108, 224, 153, 246, 128, 76, 65, 253, 249, 246, 15,
    ]];

    for digest in example_digest {
        let reverse = rev_compute_digest(digest);
        let original = compute_digest(reverse);
        assert_eq!(digest, original)
    }
}
