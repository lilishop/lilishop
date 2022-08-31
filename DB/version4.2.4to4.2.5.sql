/**增加电子面单店铺信息**/
ALTER TABLE `li_store_logistics` ADD `customer_name` varchar(255) DEFAULT NULL COMMENT '客户代码';
ALTER TABLE `li_store_logistics` ADD `customer_pwd` varchar(255) DEFAULT NULL COMMENT '客户密码';
ALTER TABLE `li_store_logistics` ADD `month_code` varchar(255) DEFAULT NULL COMMENT '月结号/密钥';
ALTER TABLE `li_store_logistics` ADD `send_site` varchar(255) DEFAULT NULL COMMENT '归属网点';
ALTER TABLE `li_store_logistics` ADD `send_staff` varchar(255) DEFAULT NULL COMMENT '收件快递员';
ALTER TABLE `li_store_logistics` ADD `face_sheet_flag` bit(1) DEFAULT NULL COMMENT '是否使用电子面单';
ALTER TABLE `li_store_logistics` ADD `pay_type` varchar(255) COLLATE utf8mb4_bin DEFAULT NULL COMMENT '支付方式';
ALTER TABLE `li_store_logistics` ADD `exp_type` varchar(255) COLLATE utf8mb4_bin DEFAULT NULL COMMENT '快递类型';

