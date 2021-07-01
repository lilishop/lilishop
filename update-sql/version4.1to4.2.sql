/** 添加分销员结算信息**/
ALTER TABLE li_distribution ADD settlement_bank_account_name varchar ( 200 );
ALTER TABLE li_distribution ADD settlement_bank_account_num varchar ( 200 );
ALTER TABLE li_distribution ADD settlement_bank_branch_name varchar ( 200 );

/** 文章分类添加默认值**/
ALTER TABLE li_article_category alter column sort set default 0;