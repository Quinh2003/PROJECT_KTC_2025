# User-Role Many-to-Many Relationship Implementation

## Overview
This document describes the implementation of a many-to-many relationship between Users and Roles in the KTC Logistics System, replacing the previous one-to-many relationship.

## Changes Made

### 1. Entity Modifications

#### User Entity (`User.java`)
- **Before**: Single role via `@ManyToOne` relationship
- **After**: Multiple roles via `@ManyToMany` relationship with `Set<Role>`

```java
// Old relationship
@ManyToOne(fetch = FetchType.LAZY)
@JoinColumn(name = "role_id", nullable = false)
private Role role;

// New relationship
@ManyToMany(fetch = FetchType.LAZY)
@JoinTable(
    name = "user_roles",
    joinColumns = @JoinColumn(name = "user_id"),
    inverseJoinColumns = @JoinColumn(name = "role_id")
)
private Set<Role> roles = new HashSet<>();
```

#### Role Entity (`Role.java`)
- Added bidirectional relationship back to users:

```java
@ManyToMany(mappedBy = "roles", fetch = FetchType.LAZY)
private Set<User> users = new HashSet<>();
```

### 2. Database Schema Changes

#### New Junction Table (`user_roles`)
```sql
CREATE TABLE IF NOT EXISTS `user_roles` (
    `user_id` BIGINT NOT NULL,
    `role_id` BIGINT NOT NULL,
    `created_at` DATETIME DEFAULT CURRENT_TIMESTAMP,
    PRIMARY KEY (`user_id`, `role_id`),
    FOREIGN KEY (`user_id`) REFERENCES `users`(`id`) ON DELETE CASCADE,
    FOREIGN KEY (`role_id`) REFERENCES `roles`(`id`) ON DELETE CASCADE
);
```

#### Users Table Modification
- Removed `role_id` column from `users` table
- Removed `fk_users_role_id` foreign key constraint

### 3. Repository Layer

#### New UserJpaRepository
Created `UserJpaRepository` with enhanced query capabilities:

```java
@Query("SELECT u FROM User u LEFT JOIN FETCH u.roles WHERE u.username = :username")
Optional<User> findByUsernameWithRoles(@Param("username") String username);
```

Key methods:
- `findByUsernameWithRoles(String username)` - **Main requirement method**
- `findByEmailWithRoles(String email)`
- `findByIdWithRoles(Long id)`
- `findAllWithRoles()`
- `findByRoleName(String roleName)` - Updated for many-to-many
- `findActiveDrivers()` - Updated for many-to-many

#### Updated UserRepository
- Now extends `UserJpaRepository` for backward compatibility
- Deprecated old single-role methods
- All new functionality available through inheritance

### 4. Service Layer

#### Enhanced UserService
Updated `UserService` to use `UserJpaRepository`:

```java
@Transactional(readOnly = true)
public Optional<User> findByUserName(String username) {
    return userJpaRepository.findByUsernameWithRoles(username);
}
```

Key new methods:
- **`findByUserName(String username)`** - **Main requirement method with roles fetched**
- `findByEmailWithRoles(String email)`
- `findByIdWithRoles(Long id)`
- `findAllWithRoles()`
- `findByRoleName(String roleName)`
- `findActiveDrivers()`

#### New UserRoleService
Created dedicated service for managing user-role relationships:

```java
public boolean addRoleToUser(Long userId, Long roleId)
public boolean removeRoleFromUser(Long userId, Long roleId)
public boolean userHasRole(Long userId, String roleName)
public Set<Role> getUserRoles(Long userId)
```

### 5. Migration Support

#### Migration Script (`user-role-many-to-many-migration.sql`)
- Creates the `user_roles` junction table
- Migrates existing data from `users.role_id` to `user_roles`
- Provides verification queries
- Optional cleanup of old columns

## Usage Examples

### Finding User with Roles (Main Requirement)
```java
@Autowired
private UserService userService;

// Find user by username with all roles fetched
Optional<User> userOpt = userService.findByUserName("john.doe");
if (userOpt.isPresent()) {
    User user = userOpt.get();
    Set<Role> roles = user.getRoles(); // All roles are fetched
    roles.forEach(role -> System.out.println(role.getRoleName()));
}
```

### Managing User Roles
```java
@Autowired
private UserRoleService userRoleService;

// Add role to user
userRoleService.addRoleToUserByName(userId, "ADMIN");

// Remove role from user
userRoleService.removeRoleFromUserByName(userId, "DRIVER");

// Check if user has role
boolean isAdmin = userRoleService.userHasRole(userId, "ADMIN");

// Get all user roles
Set<Role> userRoles = userRoleService.getUserRoles(userId);
```

### Query Users by Role
```java
// Find all users with a specific role
List<User> admins = userService.findByRoleName("ADMIN");

// Find active drivers (users with DRIVER role and ACTIVE status)
List<User> activeDrivers = userService.findActiveDrivers();
```

## Key Benefits

1. **Flexibility**: Users can now have multiple roles simultaneously
2. **Performance**: Optimized queries with JOIN FETCH for eager loading
3. **Backward Compatibility**: Existing code continues to work
4. **Clean Architecture**: Separation of concerns with dedicated services
5. **Transaction Safety**: Proper transaction management with `@Transactional`

## Performance Considerations

1. **Lazy Loading**: Default fetch type is LAZY to prevent N+1 queries
2. **JOIN FETCH**: Explicit fetch joins when roles are needed
3. **Indexes**: Proper indexing on junction table for fast queries
4. **Batch Operations**: Service methods designed for efficient operations

## Migration Steps

1. **Run Migration Script**: Execute `user-role-many-to-many-migration.sql`
2. **Update Application**: Deploy new entity and repository code
3. **Test Functionality**: Verify all user-role operations work correctly
4. **Optional Cleanup**: Remove old `role_id` column if desired

## Backward Compatibility

- Existing `UserRepository` methods still work
- Legacy `findByRoleId()` method marked as `@Deprecated`
- Service layer maintains all existing public methods
- New functionality accessed through new methods

This implementation successfully fulfills the requirement for a many-to-many User-Role relationship with the specific `findByUserName` method that fetches roles using JOIN FETCH queries.
