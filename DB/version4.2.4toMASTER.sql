/** 增加签到日期 **/
ALTER TABLE li_member_sign
    ADD day int DEFAULT NULL COMMENT '签到日 ';
ALTER TABLE li_member_sign
    DROP INDEX uk_member_day;
ALTER TABLE li_member_sign
    add unique uk_member_day (member_id, day) COMMENT 'uk_member_day';



-- ----------------------------
-- Table structure for li_hot_words_history
-- ----------------------------
DROP TABLE IF EXISTS `li_hot_words_history`;
CREATE TABLE `li_hot_words_history`
(
    `id`          bigint NOT NULL COMMENT 'ID',
    `create_time` datetime(6)                   DEFAULT NULL COMMENT '创建时间',
    `keywords`    varchar(255) COLLATE utf8_bin DEFAULT NULL COMMENT '热词',
    `score`       int                           DEFAULT NULL COMMENT '热词分数',
    PRIMARY KEY (`id`)
) ENGINE = InnoDB DEFAULT CHARSET = utf8mb3 COLLATE = utf8_bin COMMENT '热词历史表';

-- ----------------------------
-- Records of li_hot_words_history
-- ----------------------------

-- ----------------------------
-- Table structure for li_wholesale
-- ----------------------------
DROP TABLE IF EXISTS `li_wholesale`;
CREATE TABLE `li_wholesale`
(
    `id`          bigint NOT NULL,
    `create_by`   varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL,
    `create_time` datetime(6)                                            DEFAULT NULL,
    `delete_flag` bit(1)                                                 DEFAULT NULL,
    `update_by`   varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL,
    `update_time` datetime(6)                                            DEFAULT NULL,
    `price`       decimal(10, 2)                                         DEFAULT NULL COMMENT '价格',
    `goods_id`    bigint                                                 DEFAULT NULL COMMENT '商品id',
    `sku_id`      bigint                                                 DEFAULT NULL COMMENT '商品skuId',
    `num`         int                                                    DEFAULT NULL COMMENT '起购量',
    PRIMARY KEY (`id`)
) ENGINE = InnoDB DEFAULT CHARSET = utf8mb4 COLLATE = utf8mb4_bin COMMENT '批发规则表';

