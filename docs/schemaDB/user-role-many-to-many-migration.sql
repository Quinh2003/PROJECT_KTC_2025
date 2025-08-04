-- =====================================================================================
-- USER-ROLE MANY-TO-MANY RELATIONSHIP MIGRATION
-- =====================================================================================
-- This script migrates from the existing one-to-many relationship to many-to-many
-- Creates the junction table and migrates existing data
-- =====================================================================================

-- Create the user_roles junction table
CREATE TABLE IF NOT EXISTS `user_roles` (
    `user_id` BIGINT NOT NULL COMMENT 'Reference to users table',
    `role_id` BIGINT NOT NULL COMMENT 'Reference to roles table',
    `created_at` DATETIME DEFAULT CURRENT_TIMESTAMP COMMENT 'When this relationship was created',
    PRIMARY KEY (`user_id`, `role_id`),
    CONSTRAINT `fk_user_roles_user_id` FOREIGN KEY (`user_id`) REFERENCES `users`(`id`) ON DELETE CASCADE,
    CONSTRAINT `fk_user_roles_role_id` FOREIGN KEY (`role_id`) REFERENCES `roles`(`id`) ON DELETE CASCADE
) COMMENT 'Junction table for many-to-many relationship between users and roles';

-- Create indexes for better performance
CREATE INDEX `idx_user_roles_user_id` ON `user_roles`(`user_id`);
CREATE INDEX `idx_user_roles_role_id` ON `user_roles`(`role_id`);

-- Migrate existing data from users.role_id to user_roles table
-- Only migrate where role_id is not null
INSERT INTO `user_roles` (`user_id`, `role_id`, `created_at`)
SELECT `id`, `role_id`, NOW()
FROM `users` 
WHERE `role_id` IS NOT NULL;

-- Optional: Remove the old role_id column from users table
-- Uncomment the following lines if you want to completely remove the old relationship
-- ALTER TABLE `users` DROP FOREIGN KEY `fk_users_role_id`;
-- ALTER TABLE `users` DROP COLUMN `role_id`;

-- =====================================================================================
-- VERIFICATION QUERIES
-- =====================================================================================
-- Use these queries to verify the migration was successful

-- Check the new junction table
-- SELECT ur.user_id, ur.role_id, u.username, r.role_name 
-- FROM user_roles ur
-- JOIN users u ON ur.user_id = u.id
-- JOIN roles r ON ur.role_id = r.id
-- ORDER BY u.username, r.role_name;

-- Count relationships
-- SELECT COUNT(*) as total_user_role_relationships FROM user_roles;

-- Check users with multiple roles (should be 0 initially after migration)
-- SELECT user_id, COUNT(*) as role_count 
-- FROM user_roles 
-- GROUP BY user_id 
-- HAVING COUNT(*) > 1;
