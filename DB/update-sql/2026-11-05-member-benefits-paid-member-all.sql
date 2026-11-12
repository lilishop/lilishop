-- 客户等级、客户权益与付费会员合并升级脚本（MySQL 8）
-- @author Bulbasaur
-- @since 2026-11-05
-- 在已安装 Lilishop 基础表的目标业务库内执行；不指定库名、不包含 USE。
-- 前置：li_member_grade（含 grade_background 等等级字段）、li_order、li_after_sale、li_store_flow、
-- li_menu、li_timed_task、li_setting 已存在；本脚本不是完整建库脚本。
-- 原六份独立脚本已合并移除，统一执行本文件。
-- 结构和菜单按原脚本支持重复执行；默认数据遇已有配置可能整套跳过，
-- 请检查 defaults_allowed，跳过不代表执行失败，也不会覆盖既有配置。
-- DDL 会隐式提交，不能将整份脚本视为一个可整体回滚的事务。
-- 数据修改保留各原脚本的事务边界；执行中遇错应停止并检查。
-- 不新增“权益发放明细”菜单；付费页面名称为“付费会员”“付费记录”。

-- ============================================================
-- 1. 客户权益表结构及周期任务
-- ============================================================
-- 客户权益结构升级；默认数据在第4节初始化。MySQL 8。
CREATE TABLE IF NOT EXISTS li_member_benefit (
 id varchar(64) PRIMARY KEY, create_by varchar(64), create_time datetime DEFAULT CURRENT_TIMESTAMP,
 update_by varchar(64), update_time datetime DEFAULT CURRENT_TIMESTAMP, delete_flag bit(1) DEFAULT b'0',
 benefit_name varchar(50) NOT NULL, benefit_logo varchar(500), benefit_type varchar(64),
 benefit_desc text, benefit_sort int NOT NULL DEFAULT 1, benefit_state varchar(16) NOT NULL DEFAULT 'OPEN', benefit_config text
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- 兼容旧版本短文本字段，富文本说明及券礼包配置不再受500字限制。
ALTER TABLE li_member_benefit MODIFY benefit_desc MEDIUMTEXT, MODIFY benefit_config MEDIUMTEXT;

CREATE TABLE IF NOT EXISTS li_member_grade_benefit_grant (
 id varchar(64) PRIMARY KEY, create_by varchar(64), create_time datetime DEFAULT CURRENT_TIMESTAMP,
 update_by varchar(64), update_time datetime DEFAULT CURRENT_TIMESTAMP, delete_flag bit(1) DEFAULT b'0',
 member_id varchar(64) NOT NULL, grade_id varchar(64) NOT NULL,
 UNIQUE KEY uk_member_grade(member_id,grade_id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

CREATE TABLE IF NOT EXISTS li_member_benefit_delivery (
 id varchar(64) PRIMARY KEY, create_by varchar(64), create_time datetime DEFAULT CURRENT_TIMESTAMP,
 update_by varchar(64), update_time datetime DEFAULT CURRENT_TIMESTAMP, delete_flag bit(1) DEFAULT b'0',
 member_id varchar(64) NOT NULL, source_type varchar(32) NOT NULL, source_ref varchar(64) NOT NULL,
 benefit_id varchar(64) NOT NULL, benefit_name varchar(50), benefit_type varchar(64), benefit_config text,
 cycle_key varchar(64) NOT NULL, status varchar(16) NOT NULL DEFAULT 'PENDING', attempts int NOT NULL DEFAULT 0,
 error_message varchar(500),
 UNIQUE KEY uk_delivery(member_id,source_type,source_ref,benefit_id,cycle_key),
 KEY idx_delivery_status(status,create_time)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='逐项权益发放账本';

-- 购物积分由交易完结处理在交易完结后发放，复用li_member_points_history防重。
-- 不修改订单表结构，不新增积分辅助表；旧版已执行的字段和表保留供核对，新代码不使用。

INSERT INTO li_timed_task (id,task_name,group_name,bean_name,method_name,cron,status,description)
SELECT 'MemberBenefitExecute','客户权益周期发放','客户任务','memberBenefitExecute','execute','0 10 * * * ?',1,
 '每小时发放当前周期权益，失败自动重试最多10次；不补发历史未生成的周期'
WHERE NOT EXISTS (SELECT 1 FROM li_timed_task WHERE id='MemberBenefitExecute');

-- ============================================================
-- 2. 付费会员表结构及结算字段
-- ============================================================
-- 单一付费会员，无等级/套餐；平台承担商品会员优惠。MySQL 8。
-- 付费会员配置统一存储于 li_setting，key 为 PAID_MEMBER_SETTING。
-- 兼容已执行旧版脚本的环境：仅在新 key 不存在时迁入旧配置，不覆盖现有设置、不删除旧表。
SET @paid_member_ddl = (SELECT IF(COUNT(*)>0,
 'INSERT INTO li_setting (id,create_by,create_time,delete_flag,update_by,update_time,setting_value) SELECT ''PAID_MEMBER_SETTING'',create_by,create_time,delete_flag,update_by,update_time,config_json FROM li_paid_member_setting WHERE id=''DEFAULT'' AND delete_flag=0 AND NOT EXISTS (SELECT 1 FROM li_setting WHERE id=''PAID_MEMBER_SETTING'')',
 'SELECT 1') FROM information_schema.TABLES WHERE TABLE_SCHEMA=DATABASE() AND TABLE_NAME='li_paid_member_setting');
PREPARE paid_member_stmt FROM @paid_member_ddl; EXECUTE paid_member_stmt; DEALLOCATE PREPARE paid_member_stmt;

CREATE TABLE IF NOT EXISTS li_paid_member_order (
 id varchar(64) PRIMARY KEY, create_by varchar(64), create_time datetime DEFAULT CURRENT_TIMESTAMP,
 update_by varchar(64), update_time datetime DEFAULT CURRENT_TIMESTAMP, delete_flag bit(1) DEFAULT b'0',
 sn varchar(64) NOT NULL, member_id varchar(64) NOT NULL, member_name varchar(255), request_id varchar(64) NOT NULL,
 amount decimal(18,2) NOT NULL, months int NOT NULL, status varchar(16) NOT NULL,
 config_snapshot MEDIUMTEXT NOT NULL, config_version varchar(64) NOT NULL, pay_deadline datetime NOT NULL,
 pay_time datetime, start_time datetime, end_time datetime, payment_method varchar(32), receivable_no varchar(128),
 UNIQUE KEY uk_paid_sn(sn), UNIQUE KEY uk_paid_request(member_id,request_id),
 KEY idx_paid_active(member_id,status,start_time,end_time)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='付费会员订单表';

SET @paid_member_ddl = (SELECT IF(COUNT(*)=0, 'ALTER TABLE li_store_flow ADD COLUMN paid_member_subsidy_price decimal(18,2) NOT NULL DEFAULT 0', 'SELECT 1') FROM information_schema.COLUMNS WHERE TABLE_SCHEMA=DATABASE() AND TABLE_NAME='li_store_flow' AND COLUMN_NAME='paid_member_subsidy_price');
PREPARE paid_member_stmt FROM @paid_member_ddl; EXECUTE paid_member_stmt; DEALLOCATE PREPARE paid_member_stmt;




-- ============================================================
-- 已执行旧版脚本的 paid_member_order_id 列及其索引保留，不自动删除；新代码不再读写。
-- 3. 兼容旧版：停用普通客户等级折扣权益
-- ============================================================
-- 仅用于已执行过旧版客户权益SQL的环境；新环境不需要新增等级折扣字段。
-- 不删除历史订单、权益发放记录和已有数据库列。
START TRANSACTION;
UPDATE li_member_benefit
SET benefit_name='会员活动说明', benefit_type='CUSTOM',
    benefit_desc='<p>会员活动的参与条件、时间和内容以商城活动公告为准。</p>',
    benefit_config='{}', update_time=NOW()
WHERE id='MB_DEFAULT_03' AND benefit_type='MEMBER_DISCOUNT';

UPDATE li_member_benefit
SET benefit_state='CLOSE', update_time=NOW()
WHERE benefit_type='MEMBER_DISCOUNT';
COMMIT;

-- 剩余旧折扣定义均应关闭；接口已过滤这些历史类型。
SELECT id, benefit_name, benefit_state FROM li_member_benefit WHERE benefit_type='MEMBER_DISCOUNT';

-- ============================================================
-- 4. 初始化10条权益和4个等级（已有同ID数据不覆盖）
-- ============================================================
-- 10条非优惠券权益（ID 1～10，使用已配置的 OSS 图标）、4个客户等级。
-- @author Bulbasaur
-- @since 2026-11-05
-- 仅空配置库或仅包含本套默认数据时插入；不覆盖、不删除、不绑定已有会员。
START TRANSACTION;
SET @allow_member_defaults = (SELECT COUNT(*) FROM li_member_benefit WHERE id NOT IN ('1','2','3','4','5','6','7','8','9','10'))=0 AND (SELECT COUNT(*) FROM li_member_grade WHERE id NOT IN ('1','2','3','4'))=0;
INSERT INTO `li_member_benefit` (`id`, `create_by`, `create_time`, `delete_flag`, `update_by`, `update_time`, `benefit_name`, `benefit_logo`, `benefit_type`, `benefit_desc`, `benefit_sort`, `benefit_state`, `benefit_config`) SELECT '1', NULL, '2026-11-04 16:29:18.000000', b'0', 'admin', '2026-11-04 21:38:22.761000', '等级专属徽章', 'https://lilishop-oss.oss-cn-beijing.aliyuncs.com/MANAGER/2117947015370043393/e629982542f64f3db75020e5e842788c.png', 'GRADE_BADGE', '<p>获得当前等级的专属徽章&#xff0c;在会员中心展示。</p>', 1, 'OPEN', '{}' WHERE @allow_member_defaults AND NOT EXISTS (SELECT 1 FROM li_member_benefit WHERE id='1');
INSERT INTO `li_member_benefit` (`id`, `create_by`, `create_time`, `delete_flag`, `update_by`, `update_time`, `benefit_name`, `benefit_logo`, `benefit_type`, `benefit_desc`, `benefit_sort`, `benefit_state`, `benefit_config`) SELECT '2', NULL, '2026-11-04 16:29:19.000000', b'0', 'admin', '2026-11-04 21:38:30.957000', '会员专属客服', 'https://lilishop-oss.oss-cn-beijing.aliyuncs.com/MANAGER/2117947015370043393/88d48600589c41d7b97a5a35097cf4e0.png', 'EXCLUSIVE_CUSTOMER_SERVICE', '<p>享有会员客服咨询服务&#xff0c;请通过商城在线客服入口联系。</p>', 2, 'OPEN', '{}' WHERE @allow_member_defaults AND NOT EXISTS (SELECT 1 FROM li_member_benefit WHERE id='2');
INSERT INTO `li_member_benefit` (`id`, `create_by`, `create_time`, `delete_flag`, `update_by`, `update_time`, `benefit_name`, `benefit_logo`, `benefit_type`, `benefit_desc`, `benefit_sort`, `benefit_state`, `benefit_config`) SELECT '3', NULL, '2026-11-04 16:29:19.000000', b'0', 'admin', '2026-11-04 21:38:39.899000', '会员活动说明', 'https://lilishop-oss.oss-cn-beijing.aliyuncs.com/MANAGER/2117947015370043393/cf34dbbda09946489bf5b0231860d012.png', 'CUSTOM', '<p>会员活动的参与条件、时间和内容以商城活动公告为准。</p>', 3, 'OPEN', '{}' WHERE @allow_member_defaults AND NOT EXISTS (SELECT 1 FROM li_member_benefit WHERE id='3');
INSERT INTO `li_member_benefit` (`id`, `create_by`, `create_time`, `delete_flag`, `update_by`, `update_time`, `benefit_name`, `benefit_logo`, `benefit_type`, `benefit_desc`, `benefit_sort`, `benefit_state`, `benefit_config`) SELECT '4', NULL, '2026-11-04 16:29:19.000000', b'0', 'admin', '2026-11-04 21:38:51.189000', '购物1.5倍积分', 'https://lilishop-oss.oss-cn-beijing.aliyuncs.com/MANAGER/2117947015370043393/58e629e0915e481181ca574371688759.png', 'POINT_MULTIPLIER', '<p>订单完成后按购物积分规则获得1.5倍积分&#xff1b;同类倍率取最高值。</p>', 4, 'OPEN', '{"multiplier":1.5}' WHERE @allow_member_defaults AND NOT EXISTS (SELECT 1 FROM li_member_benefit WHERE id='4');
INSERT INTO `li_member_benefit` (`id`, `create_by`, `create_time`, `delete_flag`, `update_by`, `update_time`, `benefit_name`, `benefit_logo`, `benefit_type`, `benefit_desc`, `benefit_sort`, `benefit_state`, `benefit_config`) SELECT '5', NULL, '2026-11-04 16:29:19.000000', b'0', 'admin', '2026-11-04 21:39:02.141000', '购物双倍积分', 'https://lilishop-oss.oss-cn-beijing.aliyuncs.com/MANAGER/2117947015370043393/35b640facdc543db9b57a2b47643816a.png', 'POINT_MULTIPLIER', '<p>订单完成后按购物积分规则获得2倍积分&#xff1b;退款按下单规则回退。</p>', 5, 'OPEN', '{"multiplier":2}' WHERE @allow_member_defaults AND NOT EXISTS (SELECT 1 FROM li_member_benefit WHERE id='5');
INSERT INTO `li_member_benefit` (`id`, `create_by`, `create_time`, `delete_flag`, `update_by`, `update_time`, `benefit_name`, `benefit_logo`, `benefit_type`, `benefit_desc`, `benefit_sort`, `benefit_state`, `benefit_config`) SELECT '6', NULL, '2026-11-04 16:29:19.000000', b'0', 'admin', '2026-11-04 21:39:13.175000', '入会50积分', 'https://lilishop-oss.oss-cn-beijing.aliyuncs.com/MANAGER/2117947015370043393/54cf95fa0ed6400eba268181d4d43401.png', 'GIFT_POINT', '<p>首次达到普通会员等级赠送50积分&#xff0c;同一等级仅赠送一次。</p>', 6, 'OPEN', '{"giftPoint":50}' WHERE @allow_member_defaults AND NOT EXISTS (SELECT 1 FROM li_member_benefit WHERE id='6');
INSERT INTO `li_member_benefit` (`id`, `create_by`, `create_time`, `delete_flag`, `update_by`, `update_time`, `benefit_name`, `benefit_logo`, `benefit_type`, `benefit_desc`, `benefit_sort`, `benefit_state`, `benefit_config`) SELECT '7', NULL, '2026-11-04 16:29:19.000000', b'0', 'admin', '2026-11-04 21:39:39.230000', '升级100积分', 'https://lilishop-oss.oss-cn-beijing.aliyuncs.com/MANAGER/2117947015370043393/c639048c3fb5415a9e0f8b32c6a69f4b.png', 'GIFT_POINT', '<p>首次达到关联等级赠送100积分&#xff0c;同一等级仅赠送一次。</p>', 7, 'OPEN', '{"giftPoint":100}' WHERE @allow_member_defaults AND NOT EXISTS (SELECT 1 FROM li_member_benefit WHERE id='7');
INSERT INTO `li_member_benefit` (`id`, `create_by`, `create_time`, `delete_flag`, `update_by`, `update_time`, `benefit_name`, `benefit_logo`, `benefit_type`, `benefit_desc`, `benefit_sort`, `benefit_state`, `benefit_config`) SELECT '8', NULL, '2026-11-04 16:29:19.000000', b'0', 'admin', '2026-11-04 21:39:54.781000', '升级300积分', 'https://lilishop-oss.oss-cn-beijing.aliyuncs.com/MANAGER/2117947015370043393/a5243dbf74d94cc982ee28641c66c3b4.png', 'GIFT_POINT', '<p>首次达到关联等级赠送300积分&#xff0c;同一等级仅赠送一次。</p>', 8, 'OPEN', '{"giftPoint":300}' WHERE @allow_member_defaults AND NOT EXISTS (SELECT 1 FROM li_member_benefit WHERE id='8');
INSERT INTO `li_member_benefit` (`id`, `create_by`, `create_time`, `delete_flag`, `update_by`, `update_time`, `benefit_name`, `benefit_logo`, `benefit_type`, `benefit_desc`, `benefit_sort`, `benefit_state`, `benefit_config`) SELECT '9', NULL, '2026-11-04 16:29:19.000000', b'0', 'admin', '2026-11-04 21:40:08.295000', '生日关怀说明', 'https://lilishop-oss.oss-cn-beijing.aliyuncs.com/MANAGER/2117947015370043393/26898929300f40efb9fb3edd05937d74.png', 'CUSTOM', '<p>生日关怀服务以商城实际活动公告为准。本权益为说明展示&#xff0c;不自动发放积分、礼品或优惠券。</p>', 9, 'OPEN', '{}' WHERE @allow_member_defaults AND NOT EXISTS (SELECT 1 FROM li_member_benefit WHERE id='9');
INSERT INTO `li_member_benefit` (`id`, `create_by`, `create_time`, `delete_flag`, `update_by`, `update_time`, `benefit_name`, `benefit_logo`, `benefit_type`, `benefit_desc`, `benefit_sort`, `benefit_state`, `benefit_config`) SELECT '10', NULL, '2026-11-04 16:29:19.000000', b'0', 'admin', '2026-11-04 21:40:16.730000', '尊享服务说明', 'https://lilishop-oss.oss-cn-beijing.aliyuncs.com/MANAGER/2117947015370043393/c94b9dac3d2a41dc9070b23c9237e3b3.png', 'CUSTOM', '<p>展示钻石会员的尊享服务说明&#xff0c;具体服务请咨询商城客服&#xff0c;不自动生成履约订单。</p>', 10, 'OPEN', '{}' WHERE @allow_member_defaults AND NOT EXISTS (SELECT 1 FROM li_member_benefit WHERE id='10');
INSERT INTO `li_member_grade` (`id`, `create_by`, `create_time`, `delete_flag`, `update_by`, `update_time`, `grade_name`, `is_default`, `grade_image`, `grade_background`, `grade_font_color`, `required_experience`, `grade_sort`, `grade_state`, `benefit_ids`) SELECT '1', NULL, '2026-11-04 16:29:19.000000', b'0', 'admin', '2026-11-04 21:12:04.539000', '普通会员', b'1', 'https://lilishop-oss.oss-cn-beijing.aliyuncs.com/MANAGER/2117947015370043393/4c9dee6a9f3a49e29149e2a73f915122.png', 'https://lilishop-oss.oss-cn-beijing.aliyuncs.com/MANAGER/2117947015370043393/d6d4dcda15de41a7a3d32d0285395c04.jpg', '#333333', 1, 1, 'OPEN', '1,6' WHERE @allow_member_defaults AND NOT EXISTS (SELECT 1 FROM li_member_grade WHERE id='1');
INSERT INTO `li_member_grade` (`id`, `create_by`, `create_time`, `delete_flag`, `update_by`, `update_time`, `grade_name`, `is_default`, `grade_image`, `grade_background`, `grade_font_color`, `required_experience`, `grade_sort`, `grade_state`, `benefit_ids`) SELECT '2', NULL, '2026-11-04 16:29:19.000000', b'0', 'admin', '2026-11-04 21:12:11.848000', '白银会员', b'0', 'https://lilishop-oss.oss-cn-beijing.aliyuncs.com/MANAGER/2117947015370043393/7bbe054695754b9fbf18fa17fefcd542.png', 'https://lilishop-oss.oss-cn-beijing.aliyuncs.com/MANAGER/2117947015370043393/9ac25f921c6e44a58473f3b85bede739.jpg', '#333333', 1000, 2, 'OPEN', '1,2,3,7,9' WHERE @allow_member_defaults AND NOT EXISTS (SELECT 1 FROM li_member_grade WHERE id='2');
INSERT INTO `li_member_grade` (`id`, `create_by`, `create_time`, `delete_flag`, `update_by`, `update_time`, `grade_name`, `is_default`, `grade_image`, `grade_background`, `grade_font_color`, `required_experience`, `grade_sort`, `grade_state`, `benefit_ids`) SELECT '3', NULL, '2026-11-04 16:29:19.000000', b'0', 'admin', '2026-11-04 21:12:19.479000', '黄金会员', b'0', 'https://lilishop-oss.oss-cn-beijing.aliyuncs.com/MANAGER/2117947015370043393/6be4279660174865a6e066986bb6e35e.png', 'https://lilishop-oss.oss-cn-beijing.aliyuncs.com/MANAGER/2117947015370043393/fc96739b017e4f2fbef26b1358778362.jpg', '#333333', 5000, 3, 'OPEN', '1,2,3,4,8,9' WHERE @allow_member_defaults AND NOT EXISTS (SELECT 1 FROM li_member_grade WHERE id='3');
INSERT INTO `li_member_grade` (`id`, `create_by`, `create_time`, `delete_flag`, `update_by`, `update_time`, `grade_name`, `is_default`, `grade_image`, `grade_background`, `grade_font_color`, `required_experience`, `grade_sort`, `grade_state`, `benefit_ids`) SELECT '4', NULL, '2026-11-04 16:29:19.000000', b'0', 'admin', '2026-11-04 21:12:26.773000', '钻石会员', b'0', 'https://lilishop-oss.oss-cn-beijing.aliyuncs.com/MANAGER/2117947015370043393/14e1d00ea53245f78ed213cf334f41f4.png', 'https://lilishop-oss.oss-cn-beijing.aliyuncs.com/MANAGER/2117947015370043393/a6b486be1852414c95bca905e251b1ad.jpg', '#333333', 15000, 4, 'OPEN', '1,2,3,5,8,9,10' WHERE @allow_member_defaults AND NOT EXISTS (SELECT 1 FROM li_member_grade WHERE id='4');
COMMIT;
SELECT @allow_member_defaults AS defaults_allowed;
SELECT id,benefit_name,benefit_type FROM li_member_benefit WHERE id IN ('1','2','3','4','5','6','7','8','9','10') ORDER BY benefit_sort;
SELECT id,grade_name,required_experience FROM li_member_grade WHERE id IN ('1','2','3','4') ORDER BY grade_sort;

-- ============================================================
-- 5. 权益管理二级菜单
-- ============================================================
-- 客户模块 -> 权益管理（二级）-> 客户权益管理。
-- 可重复执行；保留已有页面 ID，不修改业务数据。
-- @author Bulbasaur
-- @since 2026-11-05
START TRANSACTION;

-- 优先使用现有客户权益页面作为模板；首次安装则使用客户等级页面。
SET @benefit_menu_source = (
 SELECT id FROM li_menu
 WHERE delete_flag=b'0' AND level=2
  AND (name IN ('member-benefit','member-grade','memberGrade') OR title IN ('客户权益管理','等级列表','客户等级'))
 ORDER BY (name='member-benefit') DESC,(title='客户权益管理') DESC,(name='member-grade') DESC,id
 LIMIT 1
);

INSERT INTO li_menu
 (id,create_by,create_time,delete_flag,update_by,update_time,description,front_route,icon,level,name,parent_id,path,sort_order,title,front_component,permission)
SELECT 2121558191760000004,'Bulbasaur',NOW(),b'0','Bulbasaur',NOW(),
 '客户权益管理','Main','ios-gift',1,'memberBenefitManage',source_group.parent_id,'/',
 COALESCE(source_group.sort_order,0)+0.5,'权益管理',NULL,NULL
FROM li_menu source
JOIN li_menu source_group ON source.parent_id=CAST(source_group.id AS CHAR)
WHERE source.id=@benefit_menu_source AND source_group.delete_flag=b'0' AND source_group.level=1
 AND NOT EXISTS(SELECT 1 FROM li_menu WHERE name='memberBenefitManage');

INSERT INTO li_menu
 (id,create_by,create_time,delete_flag,update_by,update_time,description,front_route,icon,level,name,parent_id,path,sort_order,title,front_component,permission)
SELECT 2121558191760000005,'Bulbasaur',NOW(),b'0','Bulbasaur',NOW(),
 '客户权益配置','member/benefit/index','ios-gift',2,'member-benefit',CAST(group_menu.id AS CHAR),
 'member-benefit',0,'客户权益管理',source.front_component,source.permission
FROM li_menu source
JOIN li_menu group_menu ON group_menu.name='memberBenefitManage' AND group_menu.delete_flag=b'0'
WHERE source.id=@benefit_menu_source
 AND NOT EXISTS(SELECT 1 FROM li_menu WHERE name='member-benefit');

-- 已有菜单只调整分组，保留页面 ID 和接口权限。
UPDATE li_menu page
JOIN li_menu group_menu ON group_menu.name='memberBenefitManage' AND group_menu.delete_flag=b'0'
SET page.parent_id=CAST(group_menu.id AS CHAR),page.level=2,
 page.sort_order=0,
 page.update_by='Bulbasaur',page.update_time=NOW()
WHERE page.name='member-benefit' AND page.delete_flag=b'0';

COMMIT;
-- 预期两行，客户权益管理页面的 parent_id 指向「权益管理」分组。
SELECT id,name,title,level,parent_id,front_route FROM li_menu
WHERE name IN ('memberBenefitManage','member-benefit') AND delete_flag=b'0'
ORDER BY level,sort_order;

-- ============================================================
-- 6. 付费会员二级菜单及付费记录
-- ============================================================
-- 客户模块下新增二级「付费会员」，下设设置、记录页面。
-- 兼容旧版脚本：保留页面 ID，迁移到新分组；可重复执行。
-- @author Bulbasaur
-- @since 2026-11-05
START TRANSACTION;
INSERT INTO li_menu
 (id,create_by,create_time,delete_flag,update_by,update_time,description,front_route,icon,level,name,parent_id,path,sort_order,title,front_component,permission)
SELECT 2121558191760000003,'Bulbasaur',NOW(),b'0','Bulbasaur',NOW(),'付费会员管理','Main','ios-card',1,
 'paidMemberManage',source_group.parent_id,'/',COALESCE(source_group.sort_order,0)+1,'付费会员',NULL,NULL
FROM li_menu source_group
JOIN (SELECT * FROM li_menu WHERE delete_flag=b'0' AND level=2 AND (name IN ('member-grade','memberGrade') OR title IN ('等级列表','客户等级')) ORDER BY (title='等级列表') DESC LIMIT 1) source
 ON source.parent_id=CAST(source_group.id AS CHAR)
WHERE source_group.delete_flag=b'0' AND source_group.level=1
 AND NOT EXISTS(SELECT 1 FROM li_menu WHERE name='paidMemberManage');

INSERT INTO li_menu
 (id,create_by,create_time,delete_flag,update_by,update_time,description,front_route,icon,level,name,parent_id,path,sort_order,title,front_component,permission)
SELECT 2121558191760000001,'Bulbasaur',NOW(),b'0','Bulbasaur',NOW(),'单一付费会员配置','member/paid/settings','ios-card',source.level,
 'paid-member-settings',source.parent_id,'paid-member-settings',COALESCE(source.sort_order,0)+10,'付费会员',source.front_component,source.permission
FROM (SELECT * FROM li_menu WHERE delete_flag=b'0' AND (name IN ('member-grade','memberGrade') OR title IN ('等级列表','客户等级')) ORDER BY (title='等级列表') DESC LIMIT 1) source
WHERE NOT EXISTS(SELECT 1 FROM li_menu WHERE name='paid-member-settings');
INSERT INTO li_menu
 (id,create_by,create_time,delete_flag,update_by,update_time,description,front_route,icon,level,name,parent_id,path,sort_order,title,front_component,permission)
SELECT 2121558191760000002,'Bulbasaur',NOW(),b'0','Bulbasaur',NOW(),'付费会员订单记录','member/paid/records','ios-list',source.level,
 'paid-member-records',source.parent_id,'paid-member-records',COALESCE(source.sort_order,0)+11,'付费记录',source.front_component,source.permission
FROM (SELECT * FROM li_menu WHERE delete_flag=b'0' AND (name IN ('member-grade','memberGrade') OR title IN ('等级列表','客户等级')) ORDER BY (title='等级列表') DESC LIMIT 1) source
WHERE NOT EXISTS(SELECT 1 FROM li_menu WHERE name='paid-member-records');

-- 旧环境已经存在页面时，只调整归属，不重建页面。
UPDATE li_menu page
JOIN li_menu group_menu ON group_menu.name='paidMemberManage' AND group_menu.delete_flag=b'0'
SET page.parent_id=CAST(group_menu.id AS CHAR),page.level=2,
 page.title=CASE page.name WHEN 'paid-member-settings' THEN '付费会员' ELSE '付费记录' END,
 page.sort_order=CASE page.name WHEN 'paid-member-settings' THEN 0 ELSE 1 END,
 page.update_by='Bulbasaur',page.update_time=NOW()
WHERE page.name IN ('paid-member-settings','paid-member-records') AND page.delete_flag=b'0';

COMMIT;
-- 应返回三行，设置/记录的 parent_id 均为付费会员分组 ID。
SELECT id,name,title,level,parent_id,front_route FROM li_menu WHERE name IN ('paidMemberManage','paid-member-settings','paid-member-records') AND delete_flag=b'0' ORDER BY level,sort_order;
