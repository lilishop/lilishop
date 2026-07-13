/**
 * 文件表增加媒体分类字段
 */
ALTER TABLE `li_file`
    ADD COLUMN `file_category` varchar(32) DEFAULT NULL COMMENT '文件分类：IMAGE/VIDEO/FILE' AFTER `file_type`;

UPDATE `li_file`
SET `file_category` = 'IMAGE'
WHERE (`file_category` IS NULL OR `file_category` = '')
  AND `file_type` LIKE '%image%';

UPDATE `li_file`
SET `file_category` = 'VIDEO'
WHERE (`file_category` IS NULL OR `file_category` = '')
  AND `file_type` LIKE '%video%';

UPDATE `li_file`
SET `file_category` = 'FILE'
WHERE `file_category` IS NULL OR `file_category` = '';

/**
 * 主题色配置
 */
INSERT INTO `li_setting` (`id`, `create_by`, `create_time`, `delete_flag`, `update_by`, `update_time`, `setting_value`)
VALUES ('THEME_SETTING', 'admin', NOW(), b'0', 'admin', NOW(),
        '{"themeColor":"#ff3c2a","lightColor":"#ff6b35","aiderLightColor":"#ff9f28"}')
ON DUPLICATE KEY UPDATE `setting_value` = VALUES(`setting_value`);
