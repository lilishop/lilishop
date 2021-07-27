<<<<<<< HEAD
/** 添加结算单积分、砍价结算价信息**/
ALTER TABLE li_bill ADD point_settlement_price double DEFAULT 0.00 COMMENT '积分商品结算金额 ';
ALTER TABLE li_bill ADD kanjia_settlement_price double DEFAULT 0.00 COMMENT '砍价商品结算金额';
ALTER TABLE li_store_flow ADD point_settlement_price double DEFAULT 0.00 COMMENT '积分商品结算金额';
ALTER TABLE li_store_flow ADD kanjia_settlement_price double DEFAULT 0.00 COMMENT '砍价商品结算金额';
=======
/** 热词管理*/
UPDATE `lilishop`.`li_menu` SET `create_by` = 'admin', `create_time` = '2021-03-03 09:54:51', `delete_flag` = b'0', `update_by` = 'admin', `update_time` = '2021-07-23 19:07:45', `description` = 'null', `front_route` = 'page/article-manage/hotWords', `icon` = 'md-aperture', `level` = 2, `name` = 'hotKeyWord', `parent_id` = '1367050250249830400', `path` = 'hotKeyWord', `sort_order` = 0.00, `title` = '搜索热词', `front_component` = 'null' WHERE `id` = 1367050829697122304;
>>>>>>> master
