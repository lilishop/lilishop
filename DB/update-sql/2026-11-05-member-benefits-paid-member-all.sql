-- 客户等级、客户权益升级脚本（MySQL 8）
-- @author Bulbasaur
-- @since 2026-11-05
-- 在已安装基础表的目标业务库内执行，不指定库名，不包含 USE。
-- 前置：li_member_grade（含等级背景字段）、li_order、li_after_sale、li_store_flow、li_menu、li_timed_task 已存在。
-- 可重复执行；默认数据不覆盖已有配置，请检查 defaults_allowed。
-- DDL 隐式提交，整份脚本不能作为单一事务整体回滚。
-- 不新增权益发放明细菜单，不处理 li_role_menu。
-- 同一脚本后续版本将补充付费会员升级内容。

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
