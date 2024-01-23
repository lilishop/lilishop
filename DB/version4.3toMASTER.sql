CREATE TABLE `li_file_directory`
(
    `id`             bigint                                                        NOT NULL AUTO_INCREMENT COMMENT '主键ID',
    `create_time`    datetime                                                      NULL DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
    `create_by`      varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin         NULL DEFAULT NULL COMMENT '创建者',
    `update_time`    datetime                                                      NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '更新时间',
    `update_by`      varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin         NULL DEFAULT NULL COMMENT '更新者',
    `delete_flag`    tinyint(1)                                                    NULL DEFAULT 0 COMMENT '删除标志 true/false 删除/未删除',
    `directory_type` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci NULL DEFAULT NULL COMMENT '文件目录类型',
    `directory_name` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci NULL DEFAULT NULL COMMENT '拥有者名称',
    `owner_id`       varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci NULL DEFAULT NULL COMMENT '拥有者id',
    `parent_id`      bigint                                                        NULL DEFAULT NULL COMMENT '父分类ID',
    `level`          int                                                           NULL DEFAULT NULL COMMENT '层级',
    PRIMARY KEY (`id`) USING BTREE
) ENGINE = InnoDB
  AUTO_INCREMENT = 1698937596963311619
  CHARACTER SET = utf8mb4
  COLLATE = utf8mb4_0900_ai_ci COMMENT = '文件夹'
  ROW_FORMAT = DYNAMIC;

SET FOREIGN_KEY_CHECKS = 1;

ALTER TABLE li_file
    ADD file_directory_id varchar(255) COMMENT '文件夹ID';

/**
  订单包裹
 */
CREATE TABLE `li_order_package` (
                                    `id` bigint NOT NULL COMMENT 'ID',
                                    `create_by` varchar(255) CHARACTER SET utf8mb3 COLLATE utf8mb3_bin DEFAULT NULL COMMENT '创建者',
                                    `create_time` datetime(6) DEFAULT NULL COMMENT '创建时间',
                                    `delete_flag` bit(1) DEFAULT NULL COMMENT '是否删除',
                                    `update_by` varchar(255) CHARACTER SET utf8mb3 COLLATE utf8mb3_bin DEFAULT NULL COMMENT '修改者',
                                    `update_time` datetime(6) DEFAULT NULL COMMENT '修改时间',
                                    `package_no` varchar(255) CHARACTER SET utf8mb3 COLLATE utf8mb3_bin DEFAULT NULL COMMENT '包裹单号',
                                    `order_sn` varchar(255) CHARACTER SET utf8mb3 COLLATE utf8mb3_bin DEFAULT NULL COMMENT '订单编号',
                                    `logistics_no` varchar(255) CHARACTER SET utf8mb3 COLLATE utf8mb3_bin DEFAULT NULL COMMENT '发货单号',
                                    `logistics_code` varchar(255) CHARACTER SET utf8mb3 COLLATE utf8mb3_bin DEFAULT NULL COMMENT '物流公司CODE',
                                    `logistics_name` varchar(255) CHARACTER SET utf8mb3 COLLATE utf8mb3_bin DEFAULT NULL COMMENT '物流公司名称',
                                    `consignee_mobile` varchar(255) CHARACTER SET utf8mb3 COLLATE utf8mb3_bin DEFAULT NULL COMMENT '收件人手机',
                                    `status` varchar(255) CHARACTER SET utf8mb3 COLLATE utf8mb3_bin DEFAULT NULL COMMENT '状态',
                                    PRIMARY KEY (`id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb3 COLLATE=utf8mb3_bin ROW_FORMAT=DYNAMIC;

/**
  分包货物
 */
CREATE TABLE `li_order_package_item` (
                                         `id` bigint NOT NULL COMMENT 'ID',
                                         `create_by` varchar(255) CHARACTER SET utf8mb3 COLLATE utf8mb3_bin DEFAULT NULL COMMENT '创建者',
                                         `create_time` datetime(6) DEFAULT NULL COMMENT '创建时间',
                                         `delete_flag` bit(1) DEFAULT NULL COMMENT '是否删除',
                                         `update_by` varchar(255) CHARACTER SET utf8mb3 COLLATE utf8mb3_bin DEFAULT NULL COMMENT '修改者',
                                         `update_time` datetime(6) DEFAULT NULL COMMENT '修改时间',
                                         `package_no` varchar(255) CHARACTER SET utf8mb3 COLLATE utf8mb3_bin DEFAULT NULL COMMENT '包裹单号',
                                         `order_sn` varchar(255) CHARACTER SET utf8mb3 COLLATE utf8mb3_bin DEFAULT NULL COMMENT '订单编号',
                                         `order_item_sn` varchar(255) COLLATE utf8mb3_bin DEFAULT NULL COMMENT '子订单编号',
                                         `deliver_number` int DEFAULT NULL COMMENT '发货数量',
                                         `logistics_time` datetime(6) DEFAULT NULL COMMENT '发货时间',
                                         `goods_name` varchar(255) COLLATE utf8mb3_bin DEFAULT NULL,
                                         `thumbnail` varchar(255) COLLATE utf8mb3_bin DEFAULT NULL,
                                         PRIMARY KEY (`id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb3 COLLATE=utf8mb3_bin ROW_FORMAT=DYNAMIC;

/*
    订单货物增加发货数量
*/
ALTER TABLE li_order_item ADD  `deliver_number` int DEFAULT NULL COMMENT '发货数量';

ALTER TABLE li_goods_sku ADD  `alert_quantity` int DEFAULT NULL COMMENT '预警库存';

/*
    sku增加预警库存
*/
ALTER TABLE li_goods_sku ADD  `alert_quantity` int DEFAULT NULL COMMENT '预警库存';
/*
    增加库存预警菜单
*/
INSERT INTO `lilishop`.`li_store_menu`(`id`, `create_by`, `create_time`, `delete_flag`, `update_by`, `update_time`, `description`, `front_route`, `icon`, `level`, `name`, `parent_id`, `path`, `sort_order`, `title`, `permission`) VALUES (1349237928434098177, NULL, '2022-01-11 22:35:45.000000', b'0', NULL, '2022-01-11 22:37:05', NULL, 'goods/goods-seller/alertQuantity', 'ios-american-football', 2, 'alert-goods-quantity', '1348810864748945408', 'alert-goods-quantity', '1.14', '库存预警', NULL);

/**
  交易唤醒表，增加交易流水详情
 */
ALTER TABLE li_order_item ADD `is_refund` varchar(255) DEFAULT NULL COMMENT '是否退款';

/**
  交易表增加订单状态字段
 */
ALTER TABLE li_order_item ADD `refund_price` decimal(10,2) DEFAULT NULL COMMENT '退款金额';

/**
  结算单表，增加砍价/积分退款金额字段
 */
ALTER TABLE li_bill  ADD `point_refund_settlement_price` decimal(10,2) DEFAULT NULL COMMENT '退货积分补贴返还';
ALTER TABLE li_bill  ADD `kanjia_refund_settlement_price` decimal(10,2) DEFAULT NULL COMMENT '退货砍价补贴返还';

UPDATE li_bill b
SET b.point_refund_settlement_price =IFNULL((
SELECT
   SUM( point_settlement_price )
FROM
   li_store_flow sf
WHERE
   sf.flow_type = 'REFUND'
   AND sf.store_id=b.store_id
   AND sf.create_time BETWEEN b.start_time
   AND b.end_time),0)

UPDATE li_bill b
SET b.kanjia_refund_settlement_price =IFNULL((
 SELECT
   SUM( kanjia_settlement_price )
 FROM
   li_store_flow sf
 WHERE
   sf.flow_type = 'REFUND'
   AND sf.store_id=b.store_id
   AND sf.create_time BETWEEN b.start_time
   AND b.end_time),0);

