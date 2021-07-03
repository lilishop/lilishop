/** 添加分销员结算信息**/
ALTER TABLE li_distribution ADD settlement_bank_account_name varchar ( 200 );
ALTER TABLE li_distribution ADD settlement_bank_account_num varchar ( 200 );
ALTER TABLE li_distribution ADD settlement_bank_branch_name varchar ( 200 );

/** 文章分类添加默认值**/
ALTER TABLE li_article_category alter column sort set default 0;
/** 添加分销提现菜单**/
INSERT INTO `lilishop`.`li_menu`(`id`, `create_by`, `create_time`, `delete_flag`, `update_by`, `update_time`, `description`, `front_route`, `icon`, `level`, `name`, `parent_id`, `path`, `sort_order`, `title`, `front_component`) VALUES (1410862675914764290, 'admin', '2021-07-02 15:27:29', b'0', 'admin', '2021-07-02 15:27:45', 'null', 'distribution/distributionCash', '', 2, 'distributionCash', '1374173575405109248', 'distributionCash', 5.00, '分销提现', 'null');