
/**
    文件表增加拥有者名称
 */
ALTER TABLE li_file ADD `owner_name` varchar(255) DEFAULT NULL COMMENT '拥有者名称';

/**
  初始化文件拥有者名称
 */
UPDATE li_file f JOIN li_store s ON f.owner_id = s.id
    SET f.owner_name = s.store_name
WHERE user_enums = 'STORE';

UPDATE li_file f JOIN li_admin_user a ON f.owner_id = a.id
    SET f.owner_name = a.nick_name
WHERE user_enums = 'MANAGER';

UPDATE li_file f JOIN li_member m ON f.owner_id = m.id
    SET f.owner_name = m.nick_name
WHERE user_enums = 'MEMBER';