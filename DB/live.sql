SET NAMES utf8mb4;
SET FOREIGN_KEY_CHECKS = 0;

-- ----------------------------
-- Table structure for li_live_room
-- ----------------------------
DROP TABLE IF EXISTS `li_live_room`;
CREATE TABLE `li_live_room` (
  `id` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NOT NULL COMMENT 'ID',
  `create_by` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '创建者',
  `create_time` datetime(6) DEFAULT NULL COMMENT '创建时间',
  `delete_flag` bit(1) DEFAULT b'0' COMMENT '删除标志 true/false 删除/未删除',
  `update_by` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '更新者',
  `update_time` datetime(6) DEFAULT NULL COMMENT '更新时间',
  `title` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '直播标题',
  `description` text CHARACTER SET utf8mb4 COLLATE utf8mb4_bin COMMENT '直播描述',
  `cover_img` varchar(500) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '直播封面',
  `display_mode` varchar(32) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '展示模式: 0 横屏 1 竖屏 2 三分屏',
  `viewer_count` int DEFAULT NULL COMMENT '直播观看人数',
  `actual_view_number` bigint DEFAULT 0 COMMENT '真实直播观看人数',
  `live_introduce` text CHARACTER SET utf8mb4 COLLATE utf8mb4_bin COMMENT '直播介绍',
  `total_likes` int DEFAULT 0 COMMENT '直播点赞数',
  `total_comments` int DEFAULT 0 COMMENT '直播评论数',
  `comment_people_number` int DEFAULT 0 COMMENT '评论人数',
  `push_stream_server` varchar(1024) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '推流服务器',
  `push_stream_code` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '推流码',
  `pull_stream_url` varchar(1024) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '拉流地址',
  `start_time` datetime(6) DEFAULT NULL COMMENT '直播开始时间',
  `end_time` datetime(6) DEFAULT NULL COMMENT '直播结束时间',
  `im_group_id` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT 'IM群Id',
  `order_count` int DEFAULT 0 COMMENT '订单数',
  `goods_count` int DEFAULT 0 COMMENT '商品数',
  `store_id` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '店铺Id',
  `store_name` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '店铺名称',
  `auth_flag` bit(1) DEFAULT NULL COMMENT '审核标识 是否审核评论',
  `live_status` varchar(32) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '直播状态',
  `sn` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '直播编号',
  PRIMARY KEY (`id`) USING BTREE,
  KEY `idx_li_live_room_store_id` (`store_id`) USING BTREE,
  KEY `idx_li_live_room_live_status` (`live_status`) USING BTREE,
  KEY `idx_li_live_room_sn` (`sn`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin ROW_FORMAT=DYNAMIC COMMENT='直播间';

-- ----------------------------
-- Table structure for li_live_user
-- ----------------------------
DROP TABLE IF EXISTS `li_live_user`;
CREATE TABLE `li_live_user` (
  `id` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NOT NULL COMMENT 'ID',
  `create_by` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '创建者',
  `create_time` datetime(6) DEFAULT NULL COMMENT '创建时间',
  `delete_flag` bit(1) DEFAULT b'0' COMMENT '删除标志 true/false 删除/未删除',
  `update_by` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '更新者',
  `update_time` datetime(6) DEFAULT NULL COMMENT '更新时间',
  `live_room_id` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '直播流ID',
  `user_id` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '用户ID',
  `user_name` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '用户名',
  `user_face` varchar(500) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '用户头像',
  `watch_time` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '观看时间',
  `mute_flag` bit(1) DEFAULT NULL COMMENT '禁言标识',
  `user_sig` varchar(1024) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '用户腾讯云Sig',
  `amount` decimal(12,2) DEFAULT NULL COMMENT '消费金额',
  PRIMARY KEY (`id`) USING BTREE,
  UNIQUE KEY `uk_li_live_user_room_user` (`live_room_id`, `user_id`) USING BTREE,
  KEY `idx_li_live_user_live_room_id` (`live_room_id`) USING BTREE,
  KEY `idx_li_live_user_user_id` (`user_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin ROW_FORMAT=DYNAMIC COMMENT='直播间用户';

-- ----------------------------
-- Table structure for li_live_goods
-- ----------------------------
DROP TABLE IF EXISTS `li_live_goods`;
CREATE TABLE `li_live_goods` (
  `id` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NOT NULL COMMENT 'ID',
  `create_by` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '创建者',
  `create_time` datetime(6) DEFAULT NULL COMMENT '创建时间',
  `delete_flag` bit(1) DEFAULT b'0' COMMENT '删除标志 true/false 删除/未删除',
  `update_by` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '更新者',
  `update_time` datetime(6) DEFAULT NULL COMMENT '更新时间',
  `goods_id` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '商品ID',
  `sku_id` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT 'SKU ID',
  `goods_name` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '商品名称',
  `stock` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '库存数量',
  `price` decimal(12,2) DEFAULT NULL COMMENT '商品价格',
  `thumbnail` varchar(500) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '商品图片',
  `sales_count` int DEFAULT NULL COMMENT '销售量',
  `store_id` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '店铺ID',
  `store_name` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '店铺名称',
  `origin_price` decimal(12,2) DEFAULT NULL COMMENT '原价',
  `live_room_id` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '直播流ID',
  `sell_point` varchar(500) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '卖点',
  `popularity` int DEFAULT NULL COMMENT '热度',
  `hide_flag` bit(1) DEFAULT NULL COMMENT '是否隐藏',
  `can_buy_flag` bit(1) DEFAULT NULL COMMENT '是否可购买',
  `sold_out_flag` bit(1) DEFAULT NULL COMMENT '是否售罄',
  `recommend` bit(1) DEFAULT NULL COMMENT '是否推荐',
  PRIMARY KEY (`id`) USING BTREE,
  KEY `idx_li_live_goods_live_room_id` (`live_room_id`) USING BTREE,
  KEY `idx_li_live_goods_goods_id` (`goods_id`) USING BTREE,
  KEY `idx_li_live_goods_store_id` (`store_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin ROW_FORMAT=DYNAMIC COMMENT='直播间商品';

-- ----------------------------
-- Table structure for li_live_coupon
-- ----------------------------
DROP TABLE IF EXISTS `li_live_coupon`;
CREATE TABLE `li_live_coupon` (
  `id` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NOT NULL COMMENT 'ID',
  `create_by` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '创建者',
  `create_time` datetime(6) DEFAULT NULL COMMENT '创建时间',
  `delete_flag` bit(1) DEFAULT b'0' COMMENT '删除标志 true/false 删除/未删除',
  `update_by` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '更新者',
  `update_time` datetime(6) DEFAULT NULL COMMENT '更新时间',
  `coupon_id` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '优惠券ID',
  `coupon_price` decimal(12,2) DEFAULT NULL COMMENT '优惠券金额',
  `coupon_name` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '优惠券名称',
  `live_room_id` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '直播间ID',
  `live_room_name` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '直播间名称',
  `hide_flag` bit(1) DEFAULT NULL COMMENT '是否隐藏',
  `recommend` bit(1) DEFAULT NULL COMMENT '是否推荐',
  PRIMARY KEY (`id`) USING BTREE,
  KEY `idx_li_live_coupon_live_room_id` (`live_room_id`) USING BTREE,
  KEY `idx_li_live_coupon_coupon_id` (`coupon_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin ROW_FORMAT=DYNAMIC COMMENT='直播优惠券';

-- ----------------------------
-- Table structure for li_live_message
-- ----------------------------
DROP TABLE IF EXISTS `li_live_message`;
CREATE TABLE `li_live_message` (
  `id` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NOT NULL COMMENT 'ID',
  `create_by` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '创建者',
  `create_time` datetime(6) DEFAULT NULL COMMENT '创建时间',
  `delete_flag` bit(1) DEFAULT b'0' COMMENT '删除标志 true/false 删除/未删除',
  `update_by` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '更新者',
  `update_time` datetime(6) DEFAULT NULL COMMENT '更新时间',
  `live_room_id` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '直播间ID',
  `live_name` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '直播名称',
  `live_user_id` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '直播用户ID',
  `user_id` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '用户ID',
  `user_name` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '用户名',
  `user_face` varchar(500) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '用户头像',
  `message` text CHARACTER SET utf8mb4 COLLATE utf8mb4_bin COMMENT '消息内容',
  `im_group_id` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT 'IM群ID',
  `auth_status` varchar(32) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '审核状态',
  PRIMARY KEY (`id`) USING BTREE,
  KEY `idx_li_live_message_live_room_id` (`live_room_id`) USING BTREE,
  KEY `idx_li_live_message_user_id` (`user_id`) USING BTREE,
  KEY `idx_li_live_message_auth_status` (`auth_status`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin ROW_FORMAT=DYNAMIC COMMENT='直播间消息';

-- ----------------------------
-- Table structure for li_live_block
-- ----------------------------
DROP TABLE IF EXISTS `li_live_block`;
CREATE TABLE `li_live_block` (
  `id` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NOT NULL COMMENT 'ID',
  `create_by` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '创建者',
  `create_time` datetime(6) DEFAULT NULL COMMENT '创建时间',
  `delete_flag` bit(1) DEFAULT b'0' COMMENT '删除标志 true/false 删除/未删除',
  `update_by` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '更新者',
  `update_time` datetime(6) DEFAULT NULL COMMENT '更新时间',
  `user_id` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '用户ID',
  `user_name` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '用户名',
  `user_avatar` varchar(500) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '用户头像',
  `live_room_id` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '直播间ID',
  `store_id` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '店铺ID',
  `store_name` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '店铺名称',
  `reason` varchar(500) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '拉黑原因',
  PRIMARY KEY (`id`) USING BTREE,
  KEY `idx_li_live_block_live_room_id` (`live_room_id`) USING BTREE,
  KEY `idx_li_live_block_user_id` (`user_id`) USING BTREE,
  KEY `idx_li_live_block_store_id` (`store_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin ROW_FORMAT=DYNAMIC COMMENT='直播黑名单';

SET FOREIGN_KEY_CHECKS = 1;


ALTER TABLE li_order ADD live_room_id varchar (200);

-- 客户优惠券记录直播领取来源
ALTER TABLE `li_member_coupon`
    ADD COLUMN `live_room_id` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '领取来源直播间ID' AFTER `member_coupon_status`,
    ADD COLUMN `live_room_name` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '领取来源直播间名称' AFTER `live_room_id`,
    ADD KEY `idx_li_member_coupon_live_room_id` (`live_room_id`) USING BTREE;

-- 清理直播间用户重复数据并添加唯一约束（同一用户在同一直播间仅保留一条记录）
DELETE u1 FROM `li_live_user` u1
    INNER JOIN `li_live_user` u2
        ON u1.`live_room_id` = u2.`live_room_id`
        AND u1.`user_id` = u2.`user_id`
        AND u1.`id` > u2.`id`;
ALTER TABLE `li_live_user`
    ADD UNIQUE KEY `uk_li_live_user_room_user` (`live_room_id`, `user_id`) USING BTREE;

-- version4.9to5.0: remove elasticsearch, add system log table

CREATE TABLE IF NOT EXISTS `li_system_log` (
  `id` varchar(64) NOT NULL COMMENT '主键',
  `create_time` bigint DEFAULT NULL COMMENT '日志记录时间',
  `username` varchar(255) DEFAULT NULL COMMENT '请求用户',
  `request_url` varchar(500) DEFAULT NULL COMMENT '请求路径',
  `request_param` text COMMENT '请求参数',
  `response_body` text COMMENT '响应参数',
  `ip` varchar(64) DEFAULT NULL COMMENT 'ip',
  `name` varchar(255) DEFAULT NULL COMMENT '方法操作名称',
  `request_type` varchar(32) DEFAULT NULL COMMENT '请求类型',
  `customer_log` text COMMENT '自定义日志内�?,
  `ip_info` varchar(500) DEFAULT NULL COMMENT 'ip信息',
  `cost_time` int DEFAULT NULL COMMENT '花费时间',
  `store_id` bigint DEFAULT NULL COMMENT '商家',
  PRIMARY KEY (`id`),
  KEY `idx_li_system_log_store_id` (`store_id`),
  KEY `idx_li_system_log_create_time` (`create_time`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin COMMENT='系统操作日志';
