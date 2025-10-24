use serde::{Deserialize, Serialize};

/// Represents a user in the system
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct User {
    /// Unique user identifier
    pub id: u32,

    /// User's full name
    pub name: String,

    /// Primary email address
    pub email: String,

    /// User's age in years
    pub age: u32,

    /// Optional biographical information
    pub bio: Option<String>,

    /// Whether the user account is active
    pub is_active: bool,

    /// List of user's roles
    pub roles: Vec<String>,
}

/// A post type
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum PostType {
    Short,
    Long,
}

/// A blog post
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Post {
    /// Unique post identifier
    pub id: u32,

    /// Post title
    pub title: String,

    /// Post content in markdown
    pub content: String,

    /// Author user
    pub author: User,

    /// Number of likes
    pub likes: u32,

    /// Post tags for categorization
    pub tags: Vec<String>,

    /// Whether the post is published
    pub published: bool,

    // The type of post
    pub post_type: PostType,
}

/// A comment on a post
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Comment {
    /// Unique comment identifier
    pub id: u32,

    /// The post this comment belongs to
    pub post_id: u32,

    /// Comment author's user ID
    pub author_id: u32,

    /// Comment text content
    pub text: String,

    /// Optional parent comment ID for nested comments
    pub parent_id: Option<u32>,
}

/// Test struct for validating standard library generic types
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GenericTest {
    /// Good: Optional value
    pub good_option: Option<String>,

    /// Bad: Should be Option<String> but will receive a plain string
    pub bad_option: Option<String>,

    /// Good: Vector of integers
    pub good_vec: Vec<u32>,

    /// Bad: Should be Vec<u32> but will receive a single integer
    pub bad_vec: Vec<u32>,

    /// Good: HashMap with string keys and integer values
    pub good_hashmap: std::collections::HashMap<String, u32>,

    /// Bad: Should be HashMap but will receive wrong type
    pub bad_hashmap: std::collections::HashMap<String, u32>,

    /// Good: BTreeMap with string keys and boolean values
    pub good_btreemap: std::collections::BTreeMap<String, bool>,

    /// Bad: Should be BTreeMap but will receive wrong structure
    pub bad_btreemap: std::collections::BTreeMap<String, bool>,

    /// Good: HashSet of strings
    pub good_hashset: std::collections::HashSet<String>,

    /// Bad: Should be HashSet but will receive wrong type
    pub bad_hashset: std::collections::HashSet<String>,

    /// Good: Result type
    pub good_result: Result<String, String>,

    /// Bad: Should be Result but will receive wrong value
    pub bad_result: Result<String, String>,

    /// Good: Boxed value
    pub good_box: Box<u32>,

    /// Bad: Should be Box<u32> but will receive wrong type
    pub bad_box: Box<u32>,
}
