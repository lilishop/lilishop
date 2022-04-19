/** 增加签到日期 **/
ALTER TABLE li_member_sign ADD day int DEFAULT NULL COMMENT '签到日 ';
ALTER TABLE li_member_sign DROP INDEX uk_member_day;
ALTER TABLE li_member_sign add unique uk_member_day (member_id, day) COMMENT 'uk_member_day';


/** 添加历史热词表 **/
CREATE TABLE `li_hot_words_history` (
                             `id` bigint NOT NULL COMMENT 'ID',
                             `create_by` varchar(255) CHARACTER SET utf8 COLLATE utf8_bin DEFAULT NULL COMMENT '创建者',
                             `create_time` datetime(6) DEFAULT NULL COMMENT '创建时间',
                             `delete_flag` bit(1) DEFAULT NULL COMMENT '是否删除',
                             `update_by` varchar(255) CHARACTER SET utf8 COLLATE utf8_bin DEFAULT NULL COMMENT '修改者',
                             `update_time` datetime(6) DEFAULT NULL COMMENT '修改时间',
                             PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin;