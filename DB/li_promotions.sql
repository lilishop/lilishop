
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
