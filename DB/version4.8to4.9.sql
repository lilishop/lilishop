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
