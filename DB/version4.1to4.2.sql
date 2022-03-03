/** 添加分销员结算信息**/
ALTER TABLE li_distribution ADD settlement_bank_account_name varchar ( 200 );
ALTER TABLE li_distribution ADD settlement_bank_account_num varchar ( 200 );
ALTER TABLE li_distribution ADD settlement_bank_branch_name varchar ( 200 );

/** 文章分类添加默认值**/
ALTER TABLE li_article_category alter column sort set default 0;
/** 添加分销提现菜单**/
INSERT INTO `rxshop`.`li_menu`(`id`, `create_by`, `create_time`, `delete_flag`, `update_by`, `update_time`, `description`, `front_route`, `icon`, `level`, `name`, `parent_id`, `path`, `sort_order`, `title`, `front_component`) VALUES (1410862675914764290, 'admin', '2021-07-02 15:27:29', b'0', 'admin', '2021-07-02 15:27:45', 'null', 'distribution/distributionCash', '', 2, 'distributionCash', '1374173575405109248', 'distributionCash', 5.00, '分销提现', 'null');

/** 促销商品添加商品类型**/
ALTER TABLE li_promotion_goods ADD goods_type varchar (200);
/** 修改历史积分积分类型的字段类型 **/
alter table li_member_points_history modify column point_type varchar(50);
update li_member_points_history set point_type = 'INCREASE' where point_type=1;
update li_member_points_history set point_type = 'REDUCE' where point_type=0;

/** 添加分词管理菜单*/
INSERT INTO `rxshop`.`li_menu` (`id`, `create_by`, `create_time`, `delete_flag`, `update_by`, `update_time`, `description`, `front_route`, `icon`, `level`, `name`, `parent_id`, `path`, `sort_order`, `title`, `front_component`) VALUES (1349247640584085511, 'admin', '2021-01-13 14:51:20', b'0', 'admin', '2021-02-25 09:02:59', 'null', 'customWords/index', 'ios-american-football', 2, 'customWords', '1349237129847005184', 'customWords', 8.00, '分词管理', 'null');

/** 修改会员评价店铺图片字段类型 **/
alter table li_member_evaluation modify column reply_image text;

/** 修改售后图片字段类型 **/
alter table li_after_sale modify column after_sale_image text;

/** 提现申请审核sql **/
INSERT INTO `rxshop`.`li_menu`(`id`, `create_by`, `create_time`, `delete_flag`, `update_by`, `update_time`, `description`, `front_route`, `icon`, `level`, `name`, `parent_id`, `path`, `sort_order`, `title`, `front_component`) VALUES (1367042804944994305, 'admin', '2021-03-03 09:22:58', b'0', NULL, NULL, NULL, 'member/advance/withdrawApply', 'ios-alert', 2, 'withdrawApply', '1367042490443497472', 'withdrawApply', 1.00, '提现申请', NULL);
