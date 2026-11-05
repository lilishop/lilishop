-- 补齐旧消息模板表的受众类型和短信渠道字段，保留现有模板内容与站内信开关。
-- @author Bulbasaur
-- @since 2026-11-03
-- 在 admin-api 实际配置的数据库中执行；可重复执行。

SET @need_alter := (
  EXISTS (SELECT 1 FROM information_schema.TABLES
          WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = 'li_notice_message')
  AND NOT EXISTS (SELECT 1 FROM information_schema.COLUMNS
                  WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = 'li_notice_message' AND COLUMN_NAME = 'type'));
SET @sql := IF(@need_alter,
  'ALTER TABLE `li_notice_message` ADD COLUMN `type` varchar(32) NOT NULL DEFAULT ''MEMBER'' COMMENT ''受众类型 MEMBER/STORE/PLATFORM''',
  'SELECT ''skip li_notice_message.type''');
PREPARE stmt FROM @sql;
EXECUTE stmt;
DEALLOCATE PREPARE stmt;

SET @need_alter := (
  EXISTS (SELECT 1 FROM information_schema.TABLES
          WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = 'li_notice_message')
  AND NOT EXISTS (SELECT 1 FROM information_schema.COLUMNS
                  WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = 'li_notice_message' AND COLUMN_NAME = 'sms_status'));
SET @sql := IF(@need_alter,
  'ALTER TABLE `li_notice_message` ADD COLUMN `sms_status` varchar(16) NOT NULL DEFAULT ''CLOSE'' COMMENT ''短信渠道 OPEN/CLOSE''',
  'SELECT ''skip li_notice_message.sms_status''');
PREPARE stmt FROM @sql;
EXECUTE stmt;
DEALLOCATE PREPARE stmt;

SET @need_alter := (
  EXISTS (SELECT 1 FROM information_schema.TABLES
          WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = 'li_notice_message')
  AND NOT EXISTS (SELECT 1 FROM information_schema.COLUMNS
                  WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = 'li_notice_message' AND COLUMN_NAME = 'sms_template_code'));
SET @sql := IF(@need_alter,
  'ALTER TABLE `li_notice_message` ADD COLUMN `sms_template_code` varchar(64) NULL COMMENT ''短信服务商模板CODE''',
  'SELECT ''skip li_notice_message.sms_template_code''');
PREPARE stmt FROM @sql;
EXECUTE stmt;
DEALLOCATE PREPARE stmt;

SET @need_alter := (
  EXISTS (SELECT 1 FROM information_schema.TABLES
          WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = 'li_notice_message')
  AND NOT EXISTS (SELECT 1 FROM information_schema.COLUMNS
                  WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = 'li_notice_message' AND COLUMN_NAME = 'sms_content'));
SET @sql := IF(@need_alter,
  'ALTER TABLE `li_notice_message` ADD COLUMN `sms_content` varchar(500) NULL COMMENT ''短信模板正文预览''',
  'SELECT ''skip li_notice_message.sms_content''');
PREPARE stmt FROM @sql;
EXECUTE stmt;
DEALLOCATE PREPARE stmt;
