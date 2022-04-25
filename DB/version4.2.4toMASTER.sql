/** 增加签到日期 **/
ALTER TABLE li_member_sign ADD day int DEFAULT NULL COMMENT '签到日 ';
ALTER TABLE li_member_sign DROP INDEX uk_member_day;
ALTER TABLE li_member_sign add unique uk_member_day (member_id, day) COMMENT 'uk_member_day';



-- ----------------------------
-- Table structure for li_hot_words_history
-- ----------------------------
DROP TABLE IF EXISTS `li_hot_words_history`;
CREATE TABLE `li_hot_words_history` (
                                        `id` bigint NOT NULL COMMENT 'ID',
                                        `create_time` datetime(6) DEFAULT NULL COMMENT '创建时间',
                                        `keywords` varchar(255) COLLATE utf8_bin DEFAULT NULL COMMENT '热词',
                                        `score` int DEFAULT NULL COMMENT '热词分数',
                                        PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb3 COLLATE=utf8_bin;
