/** 新增已退货数量 **/
ALTER TABLE li_order_item ADD return_goods_number int DEFAULT 0 COMMENT '退货数量 ';


-- 促销重构sql
ALTER TABLE li_coupon DROP COLUMN promotion_status;

ALTER TABLE li_coupon_activity DROP COLUMN promotion_status;
ALTER TABLE li_coupon_activity ADD `scope_id` mediumtext CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '范围关联的ID';
ALTER TABLE li_coupon_activity ADD `scope_type` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT 'PORTION_GOODS' COMMENT '关联范围类型';

ALTER TABLE li_full_discount DROP COLUMN promotion_status;
ALTER TABLE li_full_discount ADD `scope_id` mediumtext CHARACTER SET utf8mb4 COLLATE utf8mb4_bin COMMENT '范围关联的ID';
ALTER TABLE li_full_discount ADD `scope_type` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT 'PORTION_GOODS' COMMENT '关联范围类型';

ALTER TABLE li_kanjia_activity_goods DROP COLUMN promotion_status;
ALTER TABLE li_kanjia_activity_goods ADD `scope_id` mediumtext CHARACTER SET utf8mb4 COLLATE utf8mb4_bin COMMENT '范围关联的ID';
ALTER TABLE li_kanjia_activity_goods ADD `scope_type` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT 'PORTION_GOODS' COMMENT '关联范围类型';
ALTER TABLE li_kanjia_activity_goods ADD `goods_id` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL;

ALTER TABLE li_pintuan DROP COLUMN promotion_status;
ALTER TABLE li_pintuan ADD `scope_id` mediumtext CHARACTER SET utf8mb4 COLLATE utf8mb4_bin COMMENT '范围关联的ID';
ALTER TABLE li_pintuan ADD `scope_type` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT 'PORTION_GOODS' COMMENT '关联范围类型';

ALTER TABLE li_points_goods DROP COLUMN promotion_status;
ALTER TABLE li_points_goods ADD `scope_id` mediumtext CHARACTER SET utf8mb4 COLLATE utf8mb4_bin COMMENT '范围关联的ID';
ALTER TABLE li_points_goods ADD `scope_type` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT 'PORTION_GOODS' COMMENT '关联范围类型';
ALTER TABLE li_points_goods ADD `original_price` double(10,2) DEFAULT NULL COMMENT '原价';
ALTER TABLE li_points_goods ADD `thumbnail` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '缩略图';
ALTER TABLE li_points_goods ADD `goods_id` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '商品编号';
ALTER TABLE li_points_goods ADD `goods_name` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '货品名称';

ALTER TABLE li_promotion_goods DROP COLUMN promotion_status;
ALTER TABLE li_promotion_goods ADD `scope_id` mediumtext CHARACTER SET utf8mb4 COLLATE utf8mb4_bin COMMENT '范围关联的ID';
ALTER TABLE li_promotion_goods ADD `scope_type` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT 'PORTION_GOODS' COMMENT '关联范围类型';
ALTER TABLE li_promotion_goods ADD `original_price` double(10,2) DEFAULT NULL COMMENT '原价';
ALTER TABLE li_promotion_goods ADD `points` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '兑换积分';
ALTER TABLE li_promotion_goods ADD `goods_id` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '商品编号';

ALTER TABLE li_seckill DROP COLUMN promotion_status;
ALTER TABLE li_seckill ADD `scope_id` mediumtext CHARACTER SET utf8mb4 COLLATE utf8mb4_bin COMMENT '范围关联的ID';
ALTER TABLE li_seckill ADD `scope_type` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT 'PORTION_GOODS' COMMENT '关联范围类型';

ALTER TABLE li_full_discount RENAME COLUMN is_coupon TO coupon_flag;
ALTER TABLE li_full_discount RENAME COLUMN is_free_freight TO free_freight_flag;
ALTER TABLE li_full_discount RENAME COLUMN is_full_minus TO full_minus_flag;
ALTER TABLE li_full_discount RENAME COLUMN is_full_rate TO full_rate_flag;
ALTER TABLE li_full_discount RENAME COLUMN is_gift TO gift_flag;
ALTER TABLE li_full_discount RENAME COLUMN is_point TO point_flag;

ALTER TABLE li_member_coupon RENAME COLUMN is_platform TO platform_flag;

ALTER TABLE li_goods RENAME COLUMN is_auth TO auth_flag;

ALTER TABLE li_goods_sku RENAME COLUMN is_promotion TO promotion_flag;
ALTER TABLE li_goods_sku RENAME COLUMN is_auth TO auth_flag;


-- 增加会员表索引
ALTER TABLE li_member ADD INDEX query_mobile (`mobile`) COMMENT 'query_member';
-- 会员签到唯一索引 惠券查询索引
ALTER TABLE li_member_sign ADD INDEX query_create_time (`create_time`) COMMENT 'query_create_time';
ALTER TABLE li_member_sign ADD INDEX query_member_id (`member_id`) COMMENT 'query_member_id';
ALTER TABLE li_member_sign add unique uk_member_day (member_id, create_time) COMMENT 'uk_member_day';