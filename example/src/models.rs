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
