-- 补齐消息模板表的邮箱渠道字段（email_status/email_content），与 NoticeMessage 实体及 docker 全量建表保持一致。
-- 2026-11-03-notice-message-channels.sql 仅补了受众类型和短信渠道字段，遗漏邮箱渠道，导致后台消息模板查询报 Unknown column 'email_status'。
-- @author Bulbasaur
-- @since 2026-11-06
-- 在 admin-api 实际配置的数据库中执行；仅新增带默认值的列，不影响存量模板数据；可重复执行。

SET @need_alter := (
  EXISTS (SELECT 1 FROM information_schema.TABLES
          WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = 'li_notice_message')
  AND NOT EXISTS (SELECT 1 FROM information_schema.COLUMNS
                  WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = 'li_notice_message' AND COLUMN_NAME = 'email_status'));
SET @sql := IF(@need_alter,
  'ALTER TABLE `li_notice_message` ADD COLUMN `email_status` varchar(16) NOT NULL DEFAULT ''CLOSE'' COMMENT ''邮箱渠道 OPEN/CLOSE''',
  'SELECT ''skip li_notice_message.email_status''');
PREPARE stmt FROM @sql;
EXECUTE stmt;
DEALLOCATE PREPARE stmt;

SET @need_alter := (
  EXISTS (SELECT 1 FROM information_schema.TABLES
          WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = 'li_notice_message')
  AND NOT EXISTS (SELECT 1 FROM information_schema.COLUMNS
                  WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = 'li_notice_message' AND COLUMN_NAME = 'email_content'));
SET @sql := IF(@need_alter,
  'ALTER TABLE `li_notice_message` ADD COLUMN `email_content` varchar(2000) NULL COMMENT ''邮箱正文，空则与站内信同文''',
  'SELECT ''skip li_notice_message.email_content''');
PREPARE stmt FROM @sql;
EXECUTE stmt;
DEALLOCATE PREPARE stmt;
