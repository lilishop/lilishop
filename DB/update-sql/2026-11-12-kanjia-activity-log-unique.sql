-- 砍价记录表增加活动ID与帮砍会员ID联合唯一索引，防止并发帮砍导致数据错乱。
-- @author Bulbasaur
-- @since 2026-11-12
-- 先统计 GROUP BY kanjia_activity_id, kanjia_member_id HAVING COUNT(*) > 1：有重复则 skip，不预写删除规则，留给人工处理。
-- 无重复且尚无 uk_kanjia_activity_member 时 ADD UNIQUE KEY。
-- DDL 可能隐式提交；不承诺整份脚本可用事务整体回滚。

SET NAMES utf8mb4;

SET @has_table := (
  SELECT COUNT(1) FROM information_schema.TABLES
  WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = 'li_kanjia_activity_log');

SET @dup_count := 0;
SET @sql := IF(@has_table = 1,
  'SELECT COUNT(*) INTO @dup_count FROM (SELECT kanjia_activity_id, kanjia_member_id FROM li_kanjia_activity_log GROUP BY kanjia_activity_id, kanjia_member_id HAVING COUNT(*) > 1) dup_kanjia',
  'SELECT 0 INTO @dup_count');
PREPARE stmt FROM @sql; EXECUTE stmt; DEALLOCATE PREPARE stmt;

SET @need_unique := (
  @has_table = 1
  AND @dup_count = 0
  AND EXISTS (
    SELECT 1 FROM information_schema.COLUMNS
    WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = 'li_kanjia_activity_log' AND COLUMN_NAME = 'kanjia_activity_id')
  AND EXISTS (
    SELECT 1 FROM information_schema.COLUMNS
    WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = 'li_kanjia_activity_log' AND COLUMN_NAME = 'kanjia_member_id')
  AND NOT EXISTS (
    SELECT 1 FROM information_schema.STATISTICS
    WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = 'li_kanjia_activity_log' AND INDEX_NAME = 'uk_kanjia_activity_member'));

SET @sql := IF(@need_unique = 1,
  'ALTER TABLE `li_kanjia_activity_log` ADD UNIQUE KEY `uk_kanjia_activity_member` (`kanjia_activity_id`, `kanjia_member_id`)',
  IF(@dup_count > 0,
    'SELECT ''skip li_kanjia_activity_log.uk_kanjia_activity_member: duplicate records exist''',
    'SELECT ''skip li_kanjia_activity_log.uk_kanjia_activity_member'''));
PREPARE stmt FROM @sql; EXECUTE stmt; DEALLOCATE PREPARE stmt;
