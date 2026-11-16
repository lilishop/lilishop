-- 平台每日免费抽奖。作者 Bulbasaur，创建日期 2026-11-14。
-- 仅创建新业务表，不修改已有余额、券、配置、菜单或角色授权。可重复执行，旧记录保留。
-- DDL隐式提交；上线前备份，不能以整份事务回滚。重复执行不会修复已有同名异构表，应先核对SHOW CREATE TABLE。
-- 先执行SQL，再重启buyer-api和manager-api，重新构建管理端和移动端。
-- 管理入口：营销/抽奖活动独立菜单，另执行2026-11-15-lottery-menu.sql创建菜单及独立接口权限范围。
-- 北京时间自然日重置；有效区间左闭右开；发布后冻结活动和奖池，库存不补充，重试不重复发奖。
CREATE TABLE IF NOT EXISTS li_lottery_activity (
 id varchar(64) NOT NULL, create_by varchar(64) DEFAULT NULL, create_time datetime(3) DEFAULT NULL,
 update_by varchar(64) DEFAULT NULL, update_time datetime(3) DEFAULT NULL, delete_flag bit(1) NOT NULL DEFAULT b'0',
 name varchar(100) NOT NULL COMMENT '活动名称', description varchar(2000) NOT NULL COMMENT '公开活动规则',
 daily_limit int NOT NULL COMMENT '每人每北京时间自然日免费次数1至100',
 enabled bit(1) NOT NULL DEFAULT b'0' COMMENT '0下架1上架', published bit(1) NOT NULL DEFAULT b'0' COMMENT '首次发布后冻结规则',
 start_time datetime(3) NOT NULL COMMENT '北京时间含开始', end_time datetime(3) NOT NULL COMMENT '北京时间不含结束',
 PRIMARY KEY(id), KEY idx_lottery_active(enabled,start_time,end_time)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='平台免费抽奖活动';
CREATE TABLE IF NOT EXISTS li_lottery_prize (
 id varchar(64) NOT NULL, create_by varchar(64) DEFAULT NULL, create_time datetime(3) DEFAULT NULL,
 update_by varchar(64) DEFAULT NULL, update_time datetime(3) DEFAULT NULL, delete_flag bit(1) NOT NULL DEFAULT b'0',
 activity_id varchar(64) NOT NULL COMMENT 'li_lottery_activity.id', name varchar(100) NOT NULL COMMENT '奖项名称',
 type varchar(16) NOT NULL COMMENT 'POINTS积分COUPON券THANKS未中奖', weight int NOT NULL COMMENT '概率万分点，合计10000',
 total int NOT NULL COMMENT '累计库存，THANKS为0', issued int NOT NULL DEFAULT 0 COMMENT '成功发放份数',
 points bigint NOT NULL DEFAULT 0 COMMENT '每份整数积分，非积分奖为0', coupon_id varchar(64) DEFAULT NULL COMMENT 'li_coupon.id，仅平台活动券',
 sort_order int NOT NULL COMMENT '展示顺序0起', PRIMARY KEY(id), KEY idx_lottery_pool(activity_id,delete_flag,sort_order)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='发布后固定的抽奖奖池，耗尽概率落未中奖';
CREATE TABLE IF NOT EXISTS li_lottery_record (
 id varchar(64) NOT NULL, create_by varchar(64) DEFAULT NULL, create_time datetime(3) DEFAULT NULL,
 update_by varchar(64) DEFAULT NULL, update_time datetime(3) DEFAULT NULL, delete_flag bit(1) NOT NULL DEFAULT b'0',
 activity_id varchar(64) NOT NULL COMMENT 'li_lottery_activity.id', activity_name varchar(100) NOT NULL COMMENT '活动名快照',
 member_id varchar(64) NOT NULL COMMENT 'li_member.id', request_id varchar(64) NOT NULL COMMENT '会员内唯一幂等请求号',
 draw_day date NOT NULL COMMENT '北京时间次数所属日期', prize_id varchar(64) NOT NULL COMMENT '最终li_lottery_prize.id',
 prize_name varchar(100) NOT NULL COMMENT '奖品名快照', prize_type varchar(16) NOT NULL COMMENT 'POINTS COUPON THANKS',
 points bigint NOT NULL DEFAULT 0 COMMENT '实际发放整数积分', member_coupon_id varchar(64) DEFAULT NULL COMMENT 'li_member_coupon.id保存券规则有效期快照',
 PRIMARY KEY(id), UNIQUE KEY uk_lottery_request(member_id,request_id), KEY idx_lottery_daily(member_id,activity_id,draw_day),
 KEY idx_lottery_history(activity_id,create_time), KEY idx_lottery_member(member_id,create_time)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='已完成抽奖及发奖流水，含未中奖';
-- 验证：核对三表结构及索引；真实数据库验收重复请求、并发最后一份库存、日界线、事务失败回滚。
-- 首次配置：创建平台活动券（如需券奖），保存抽奖草稿，检查概率合计100%，上架后规则不可更改。
