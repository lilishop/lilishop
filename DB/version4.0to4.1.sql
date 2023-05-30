/** 修改文章表**/
ALTER TABLE li_article DROP COLUMN open_status;
ALTER TABLE li_article ADD open_status bit ( 1 );
UPDATE li_article SET open_status = 1;
/** 增加会员等级会员经验值**/
ALTER TABLE li_member ADD grade_id VARCHAR ( 32 );
ALTER TABLE li_member ADD experience BIGINT;
/**去除商品运费承担**/
ALTER TABLE li_goods DROP COLUMN freight_payer;
/**添加商品类型**/
ALTER TABLE li_goods ADD goods_type VARCHAR ( 32 );
ALTER TABLE li_goods_sku ADD goods_type VARCHAR ( 32 );
ALTER TABLE li_specification ADD spec_value text;
UPDATE li_goods SET goods_type = "PHYSICAL_GOODS";
UPDATE li_goods_sku SET goods_type = "PHYSICAL_GOODS";
/**增加优惠券有效期类型**/
ALTER TABLE li_coupon ADD range_day_type VARCHAR ( 32 );
ALTER TABLE li_coupon ADD effective_days INT;
/**秒杀增加显示商品数量**/
ALTER TABLE li_seckill ADD goods_num INT;
/**商品参数增加字段**/
ALTER TABLE li_goods_params ADD is_index INT;
/**添加优惠券活动、直播、积分商品菜单**/
DELETE FROM li_menu WHERE id = '1372391359398281216';
DELETE FROM li_menu WHERE id = '1372394573938032640';
INSERT INTO `li_menu`(`id`, `create_by`, `create_time`, `delete_flag`, `update_by`, `update_time`, `description`, `front_route`, `icon`, `level`, `name`, `parent_id`, `path`, `sort_order`, `title`, `front_component`) VALUES (1403988156444962818, 'admin', '2021-06-13 16:10:36', b'0', 'admin', '2021-06-13 16:44:47', 'null', 'promotion/couponActivity/coupon', '', 2, 'coupon-activity', '1367049214198022144', 'coupon-activity', 0.00, '券活动', 'null');
INSERT INTO `li_menu`(`id`, `create_by`, `create_time`, `delete_flag`, `update_by`, `update_time`, `description`, `front_route`, `icon`, `level`, `name`, `parent_id`, `path`, `sort_order`, `title`, `front_component`) VALUES (1407601962899230721, 'admin', '2021-06-23 15:30:35', b'0', NULL, NULL, NULL, 'Main', '', 1, 'liveManage', '1367040067201138688', '/', 2.00, '直播管理', NULL);
INSERT INTO `li_menu`(`id`, `create_by`, `create_time`, `delete_flag`, `update_by`, `update_time`, `description`, `front_route`, `icon`, `level`, `name`, `parent_id`, `path`, `sort_order`, `title`, `front_component`) VALUES (1407602049759072258, 'admin', '2021-06-23 15:30:55', b'0', NULL, NULL, NULL, 'promotion/live/live', '', 2, 'live', '1407601962899230721', 'live', 1.00, '直播管理', NULL);
INSERT INTO `li_menu`(`id`, `create_by`, `create_time`, `delete_flag`, `update_by`, `update_time`, `description`, `front_route`, `icon`, `level`, `name`, `parent_id`, `path`, `sort_order`, `title`, `front_component`) VALUES (1407602441964244994, 'admin', '2021-06-23 15:32:29', b'0', NULL, NULL, NULL, 'Main', '', 1, 'pointManage', '1367040067201138688', '/', 3.00, '积分活动', NULL);
INSERT INTO `li_menu`(`id`, `create_by`, `create_time`, `delete_flag`, `update_by`, `update_time`, `description`, `front_route`, `icon`, `level`, `name`, `parent_id`, `path`, `sort_order`, `title`, `front_component`) VALUES (1407602516912263170, 'admin', '2021-06-23 15:32:47', b'0', NULL, NULL, NULL, 'promotion/pointsGoods/pointsGoods', '', 2, 'pointsGoods', '1407602441964244994', 'pointsGoods', 1.00, '积分商品', NULL);
INSERT INTO `li_menu`(`id`, `create_by`, `create_time`, `delete_flag`, `update_by`, `update_time`, `description`, `front_route`, `icon`, `level`, `name`, `parent_id`, `path`, `sort_order`, `title`, `front_component`) VALUES (1407602673334636546, 'admin', '2021-06-23 15:33:24', b'0', NULL, NULL, NULL, 'promotion/pointsGoodsCategory/pointsGoodsCategory', '', 2, 'pointsGoodsCategory', '1407602441964244994', 'pointsGoodsCategory', 2.00, '积分分类', NULL);
/** 添加直播间**/
CREATE TABLE `li_studio` (
     `id` bigint NOT NULL COMMENT 'ID',
     `create_by` varchar(255) CHARACTER SET utf8 COLLATE utf8_bin DEFAULT NULL COMMENT '创建者',
     `create_time` datetime(6) DEFAULT NULL COMMENT '创建时间',
     `delete_flag` bit(1) DEFAULT NULL COMMENT '是否删除',
     `update_by` varchar(255) CHARACTER SET utf8 COLLATE utf8_bin DEFAULT NULL COMMENT '修改者',
     `update_time` datetime(6) DEFAULT NULL COMMENT '修改时间',
     `anchor_name` varchar(255) CHARACTER SET utf8 COLLATE utf8_bin DEFAULT NULL COMMENT '主播昵称',
     `anchor_wechat` varchar(255) CHARACTER SET utf8 COLLATE utf8_bin DEFAULT NULL COMMENT '主播微信号',
     `cover_img` varchar(255) CHARACTER SET utf8 COLLATE utf8_bin DEFAULT NULL COMMENT '背景图',
     `end_time` varchar(255) CHARACTER SET utf8 COLLATE utf8_bin DEFAULT NULL COMMENT '结束时间',
     `feeds_img` varchar(255) CHARACTER SET utf8 COLLATE utf8_bin DEFAULT NULL COMMENT '封面图',
     `media_url` varchar(255) CHARACTER SET utf8 COLLATE utf8_bin DEFAULT NULL COMMENT '回放视频链接',
     `name` varchar(255) CHARACTER SET utf8 COLLATE utf8_bin DEFAULT NULL COMMENT '直播间名字',
     `qr_code_url` varchar(255) CHARACTER SET utf8 COLLATE utf8_bin DEFAULT NULL COMMENT '小程序直播码',
     `recommend` bit(1) DEFAULT NULL COMMENT '推荐直播间',
     `room_goods_list` text CHARACTER SET utf8 COLLATE utf8_bin COMMENT '直播间商品',
     `room_goods_num` int DEFAULT NULL COMMENT '直播间商品数量',
     `room_id` int DEFAULT NULL COMMENT '房间ID',
     `share_img` varchar(255) CHARACTER SET utf8 COLLATE utf8_bin DEFAULT NULL COMMENT '分享图',
     `start_time` varchar(255) CHARACTER SET utf8 COLLATE utf8_bin DEFAULT NULL COMMENT '开始时间',
     `status` varchar(255) CHARACTER SET utf8 COLLATE utf8_bin DEFAULT NULL COMMENT '直播间状态',
     `store_id` varchar(255) CHARACTER SET utf8 COLLATE utf8_bin DEFAULT NULL COMMENT '店铺ID',
     PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin;
/**添加直播间商品**/
CREATE TABLE `li_commodity` (
    `id` bigint NOT NULL COMMENT 'ID',
    `create_by` varchar(255) CHARACTER SET utf8 COLLATE utf8_bin DEFAULT NULL COMMENT '创建者',
    `create_time` datetime(6) DEFAULT NULL COMMENT '创建时间',
    `delete_flag` bit(1) DEFAULT NULL COMMENT '是否删除',
    `update_by` varchar(255) CHARACTER SET utf8 COLLATE utf8_bin DEFAULT NULL COMMENT '修改者',
    `update_time` datetime(6) DEFAULT NULL COMMENT '修改时间',
    `audit_id` varchar(255) CHARACTER SET utf8 COLLATE utf8_bin DEFAULT NULL COMMENT '审核单ID',
    `audit_status` varchar(255) CHARACTER SET utf8 COLLATE utf8_bin DEFAULT NULL COMMENT '审核状态',
    `goods_id` varchar(255) CHARACTER SET utf8 COLLATE utf8_bin DEFAULT NULL COMMENT '商品ID',
    `goods_image` varchar(255) CHARACTER SET utf8 COLLATE utf8_bin DEFAULT NULL COMMENT '图片',
    `live_goods_id` int DEFAULT NULL COMMENT '微信程序直播商品ID',
    `name` varchar(255) CHARACTER SET utf8 COLLATE utf8_bin DEFAULT NULL COMMENT '商品名称',
    `price` double DEFAULT NULL COMMENT '价格',
    `price2` double DEFAULT NULL COMMENT '价格2',
    `price_type` int DEFAULT NULL COMMENT '价格类型',
    `sku_id` varchar(255) CHARACTER SET utf8 COLLATE utf8_bin DEFAULT NULL COMMENT '规格ID',
    `store_id` varchar(255) CHARACTER SET utf8 COLLATE utf8_bin DEFAULT NULL COMMENT '店铺ID',
    `url` varchar(255) CHARACTER SET utf8 COLLATE utf8_bin DEFAULT NULL COMMENT '商品详情页的小程序路径',
    PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin;
/**添加小程序直播-商品关联 **/
CREATE TABLE `li_studio_commodity` (
    `id` bigint NOT NULL COMMENT 'ID',
    `goods_id` int DEFAULT NULL COMMENT '商品ID',
    `room_id` int DEFAULT NULL COMMENT '房间ID',
    PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin;
/**添加优惠券活动 **/
CREATE TABLE `li_coupon_activity` (
  `id` bigint NOT NULL COMMENT 'ID',
  `create_by` varchar(255) CHARACTER SET utf8 COLLATE utf8_bin DEFAULT NULL COMMENT '创建者',
  `create_time` datetime(6) DEFAULT NULL COMMENT '创建时间',
  `delete_flag` bit(1) DEFAULT NULL COMMENT '是否删除',
  `update_by` varchar(255) CHARACTER SET utf8 COLLATE utf8_bin DEFAULT NULL COMMENT '修改者',
  `update_time` datetime(6) DEFAULT NULL COMMENT '修改时间',
  `end_time` datetime(6) DEFAULT NULL COMMENT '结束时间',
  `promotion_name` varchar(255) CHARACTER SET utf8 COLLATE utf8_bin DEFAULT NULL COMMENT '活动名称',
  `promotion_status` varchar(255) CHARACTER SET utf8 COLLATE utf8_bin NOT NULL COMMENT '活动状态',
  `start_time` datetime(6) DEFAULT NULL COMMENT '开始时间',
  `store_id` varchar(255) CHARACTER SET utf8 COLLATE utf8_bin DEFAULT NULL COMMENT '店铺ID',
  `store_name` varchar(255) CHARACTER SET utf8 COLLATE utf8_bin DEFAULT NULL COMMENT '店铺名称',
  `activity_scope` varchar(255) CHARACTER SET utf8 COLLATE utf8_bin DEFAULT NULL COMMENT '活动范围',
  `activity_scope_info` text CHARACTER SET utf8 COLLATE utf8_bin COMMENT '活动范围详细',
  `coupon_activity_type` varchar(255) CHARACTER SET utf8 COLLATE utf8_bin NOT NULL COMMENT '活动类型',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin;
/**添加优惠券活动-优惠券关联 **/
CREATE TABLE `li_coupon_activity_item` (
    `id` bigint NOT NULL COMMENT 'ID',
    `create_by` varchar(255) CHARACTER SET utf8 COLLATE utf8_bin DEFAULT NULL COMMENT '创建者',
    `create_time` datetime(6) DEFAULT NULL COMMENT '创建时间',
    `delete_flag` bit(1) DEFAULT NULL COMMENT '是否删除',
    `update_by` varchar(255) CHARACTER SET utf8 COLLATE utf8_bin DEFAULT NULL COMMENT '修改者',
    `update_time` datetime(6) DEFAULT NULL COMMENT '修改时间',
    `activity_id` varchar(255) CHARACTER SET utf8 COLLATE utf8_bin DEFAULT NULL COMMENT '活动ID',
    `coupon_id` varchar(255) CHARACTER SET utf8 COLLATE utf8_bin DEFAULT NULL COMMENT '优惠券ID',
    `num` int DEFAULT NULL COMMENT '优惠券数量',
    PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin;
/** 修改商品模板详情字段类型**/
ALTER TABLE `li_draft_goods` MODIFY COLUMN `intro` TEXT CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL;
/** 添加秒杀设置**/
INSERT INTO `li_setting` ( `id`, `create_by`, `create_time`, `delete_flag`, `update_by`, `update_time`, `setting_value` ) VALUES ( 'SECKILL_SETTING', NULL, NULL, NULL, 'admin', '2021-06-04 09:58:39.384000', '{\"seckillRule\":\"秒杀规则\",\"hours\":\"9,10,11,12,13,14,15,21,22,23\"}' );
/** 添加直播间商品默认数量**/
alter table li_studio alter column room_goods_num set default 0;