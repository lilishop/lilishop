-- ============================================================
-- 分销系统 v2 数据库升级脚本（唯一入口，勿再拆分其他 patch 文件）
-- 说明：
-- 1. 以运行库现有 li_distribution* 表为基础增量升级，不包含历史数据回填与回滚脚本
-- 2. 金额字段统一 DECIMAL(20,2)，比例字段 DECIMAL(10,4)，Java 侧使用 BigDecimal
-- 3. JSON 快照字段使用 TEXT 存储 JSON 文本，便于兼容低版本 MySQL
-- 4. id 为雪花 ID 字符串，与现有 BaseEntity 保持一致
-- 5. 执行前请备份数据库；ALTER 语句仅首次执行有效，重复执行需自行跳过已存在列
-- 6. 新环境：按顺序执行全文；存量环境：已存在列/表可跳过，重点关注文末「存量迁移」注释块
--
-- @author Bulbasaur
-- @since 2026/07/23
-- ============================================================


-- ============================================================
-- 第 0 步：诊断（可选，执行前确认当前表结构）
-- ============================================================

-- SHOW CREATE TABLE li_distribution;
-- SHOW CREATE TABLE li_distribution_order;
-- SHOW CREATE TABLE li_distribution_cash;

SELECT COLUMN_NAME, COLUMN_TYPE, COLUMN_COMMENT
FROM information_schema.COLUMNS
WHERE TABLE_SCHEMA = DATABASE()
  AND TABLE_NAME = 'li_distribution'
  AND COLUMN_NAME IN (
      'grade_id', 'invite_code', 'join_time', 'valid_sales_amount',
      'valid_commission_amount', 'valid_customer_count', 'invite_count',
      'withdrawal_frozen', 'withdrawn_total', 'version'
  );


-- ============================================================
-- 第 1 步：扩展 li_distribution（分销员主表）
-- ============================================================

ALTER TABLE li_distribution
    ADD COLUMN grade_id VARCHAR(64) DEFAULT NULL COMMENT '当前分销等级值（1-5）' AFTER distribution_status,
    ADD COLUMN parent_distribution_id VARCHAR(64) DEFAULT NULL COMMENT '上级分销员ID' AFTER grade_id,
    ADD COLUMN invite_code VARCHAR(32) DEFAULT NULL COMMENT '分销员邀请码（全局唯一）' AFTER parent_distribution_id,
    ADD COLUMN join_time DATETIME DEFAULT NULL COMMENT '正式加入分销时间（审核通过时间）' AFTER invite_code,
    ADD COLUMN valid_sales_amount DECIMAL(20, 2) NOT NULL DEFAULT 0.00 COMMENT '累计有效销售额（等级升级口径，退款冲正）' AFTER join_time,
    ADD COLUMN valid_commission_amount DECIMAL(20, 2) NOT NULL DEFAULT 0.00 COMMENT '累计有效佣金收益（等级升级口径，退款冲正）' AFTER valid_sales_amount,
    ADD COLUMN valid_customer_count INT NOT NULL DEFAULT 0 COMMENT '累计有效客户数（首次成交的直属客户）' AFTER valid_commission_amount,
    ADD COLUMN invite_count INT NOT NULL DEFAULT 0 COMMENT '累计邀请下级分销员数' AFTER valid_customer_count,
    ADD COLUMN withdrawal_frozen DECIMAL(20, 2) NOT NULL DEFAULT 0.00 COMMENT '提现冻结金额（申请中，与 commission_frozen 分离）' AFTER invite_count,
    ADD COLUMN withdrawn_total DECIMAL(20, 2) NOT NULL DEFAULT 0.00 COMMENT '累计已提现金额（审核通过且线下打款）' AFTER withdrawal_frozen,
    ADD COLUMN version INT NOT NULL DEFAULT 0 COMMENT '乐观锁版本号' AFTER withdrawn_total;

ALTER TABLE li_distribution
    ADD UNIQUE KEY uk_distribution_invite_code (invite_code),
    ADD KEY idx_distribution_grade_id (grade_id),
    ADD KEY idx_distribution_parent (parent_distribution_id),
    ADD KEY idx_distribution_join_time (join_time),
    ADD KEY idx_distribution_member_status (member_id, distribution_status);


-- ============================================================
-- 第 2 步：扩展 li_distribution_order（分销销售归因事实表）
-- ============================================================

ALTER TABLE li_distribution_order
    ADD COLUMN invite_distribution_id VARCHAR(64) DEFAULT NULL COMMENT '二级分销员ID（邀请奖励受益人）' AFTER distribution_name,
    ADD COLUMN invite_distribution_name VARCHAR(255) DEFAULT NULL COMMENT '二级分销员名称' AFTER invite_distribution_id,
    ADD COLUMN performance_distributor_id VARCHAR(64) DEFAULT NULL COMMENT '业绩归属分销员ID（统计口径唯一归属）' AFTER invite_distribution_name,
    ADD COLUMN self_purchase TINYINT(1) NOT NULL DEFAULT 0 COMMENT '是否分销员自购：0否 1是' AFTER performance_distributor_id,
    ADD COLUMN payment_time DATETIME DEFAULT NULL COMMENT '支付成功时间' AFTER self_purchase,
    ADD COLUMN commission_pool DECIMAL(20, 2) DEFAULT NULL COMMENT '本明细佣金池金额（固定佣金×数量）' AFTER payment_time,
    ADD COLUMN relation_snapshot TEXT COMMENT '支付时客户关系快照（JSON）' AFTER commission_pool,
    ADD COLUMN pay_snapshot TEXT COMMENT '支付时商品佣金/等级/结算配置快照（JSON）' AFTER relation_snapshot,
    ADD COLUMN invite_commission DOUBLE DEFAULT NULL COMMENT '二级邀请佣金' AFTER rebate,
    ADD COLUMN invite_sell_back_rebate DOUBLE DEFAULT 0 COMMENT '二级邀请佣金退款冲回金额' AFTER sell_back_rebate,
    ADD COLUMN settle_time DATETIME DEFAULT NULL COMMENT '佣金结算时间' AFTER settle_cycle;

ALTER TABLE li_distribution_order
    ADD KEY idx_do_performance_distributor (performance_distributor_id),
    ADD KEY idx_do_payment_time (payment_time),
    ADD KEY idx_do_perf_payment (performance_distributor_id, payment_time),
    ADD KEY idx_do_member_payment (member_id, payment_time),
    ADD KEY idx_do_order_sn (order_sn),
    -- 一单一明细唯一：afterCommit / MQ 补偿 / 人工补触发并发写入时靠 DuplicateKey 幂等
    ADD UNIQUE KEY uk_do_order_item_sn (order_item_sn);


-- ============================================================
-- 第 3 步：扩展 li_distribution_cash（分销提现申请表）
-- ============================================================

ALTER TABLE li_distribution_cash
    ADD COLUMN audit_user_id VARCHAR(64) DEFAULT NULL COMMENT '审核人ID' AFTER distribution_cash_status,
    ADD COLUMN audit_remark VARCHAR(500) DEFAULT NULL COMMENT '审核备注' AFTER audit_user_id,
    ADD COLUMN reject_reason VARCHAR(500) DEFAULT NULL COMMENT '拒绝原因' AFTER audit_remark,
    ADD COLUMN before_can_rebate DECIMAL(20, 2) DEFAULT NULL COMMENT '申请时可提现余额快照' AFTER reject_reason,
    ADD COLUMN version INT NOT NULL DEFAULT 0 COMMENT '乐观锁版本号' AFTER before_can_rebate;

ALTER TABLE li_distribution_cash
    ADD KEY idx_dc_distribution_status (distribution_id, distribution_cash_status),
    ADD KEY idx_dc_audit_user (audit_user_id),
    ADD KEY idx_dc_create_time (create_time);


-- ============================================================
-- 第 4 步：新建 li_distribution_grade（分销等级配置）
-- ============================================================

CREATE TABLE IF NOT EXISTS li_distribution_grade (
    id                  VARCHAR(64)     NOT NULL COMMENT '主键',
    grade_name          VARCHAR(64)     NOT NULL COMMENT '等级名称',
    grade_sort          INT             NOT NULL DEFAULT 0 COMMENT '等级排序（越小越低）',
    sales_threshold     DECIMAL(20, 2)  NOT NULL DEFAULT 0.00 COMMENT '升级销售额阈值',
    commission_threshold DECIMAL(20, 2) NOT NULL DEFAULT 0.00 COMMENT '升级收益额阈值',
    invite_threshold    INT             NOT NULL DEFAULT 0 COMMENT '升级邀请人数阈值',
    sales_enabled       TINYINT(1)      NOT NULL DEFAULT 1 COMMENT '是否启用销售额升级条件',
    commission_enabled  TINYINT(1)      NOT NULL DEFAULT 1 COMMENT '是否启用收益额升级条件',
    invite_enabled      TINYINT(1)      NOT NULL DEFAULT 1 COMMENT '是否启用邀请人数升级条件',
    product_commission_rate DECIMAL(10, 4) NOT NULL DEFAULT 0.0000 COMMENT '直属商品佣金比例（占佣金池，0.1 表示 10%）',
    invite_commission_rate  DECIMAL(10, 4) NOT NULL DEFAULT 0.0000 COMMENT '邀请奖励比例（占佣金池，0.1 表示 10%）',
    is_default          TINYINT(1)      NOT NULL DEFAULT 0 COMMENT '是否默认等级',
    create_by           VARCHAR(64)     DEFAULT NULL COMMENT '创建者',
    create_time         DATETIME        DEFAULT NULL COMMENT '创建时间',
    update_by           VARCHAR(64)     DEFAULT NULL COMMENT '更新者',
    update_time         DATETIME        DEFAULT NULL COMMENT '更新时间',
    delete_flag         TINYINT(1)      NOT NULL DEFAULT 0 COMMENT '删除标志 0未删除 1已删除',
    PRIMARY KEY (id),
    UNIQUE KEY uk_grade_name (grade_name, delete_flag),
    KEY idx_grade_sort (grade_sort)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='分销等级配置';


-- ============================================================
-- 第 5 步：新建 li_distribution_grade_history（等级变更历史）
-- ============================================================

CREATE TABLE IF NOT EXISTS li_distribution_grade_history (
    id                      VARCHAR(64)     NOT NULL COMMENT '主键',
    distribution_id         VARCHAR(64)     NOT NULL COMMENT '分销员ID',
    previous_grade_id       VARCHAR(64)     DEFAULT NULL COMMENT '变更前等级值（1-5）',
    grade_id                VARCHAR(64)     NOT NULL COMMENT '变更后等级值（1-5）',
    grade_name              VARCHAR(64)     DEFAULT NULL COMMENT '等级名称快照',
    goods_commission_rate   DECIMAL(10, 4)  DEFAULT NULL COMMENT '商品佣金比例快照',
    invite_commission_rate  DECIMAL(10, 4)  DEFAULT NULL COMMENT '邀请佣金比例快照',
    upgrade_time            DATETIME        NOT NULL COMMENT '升级时间（自动升级写入，非通用变更时间）',
    create_by               VARCHAR(64)     DEFAULT NULL COMMENT '创建者',
    create_time             DATETIME        DEFAULT NULL COMMENT '创建时间',
    update_by               VARCHAR(64)     DEFAULT NULL COMMENT '更新者',
    update_time             DATETIME        DEFAULT NULL COMMENT '更新时间',
    delete_flag             TINYINT(1)      NOT NULL DEFAULT 0 COMMENT '删除标志',
    PRIMARY KEY (id),
    KEY idx_dgh_distribution_time (distribution_id, upgrade_time),
    KEY idx_dgh_grade (grade_id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='分销员等级变更历史';


-- ============================================================
-- 第 6 步：新建 li_distribution_customer_relation（客户当前关系）
-- ============================================================

CREATE TABLE IF NOT EXISTS li_distribution_customer_relation (
    id                          VARCHAR(64)     NOT NULL COMMENT '主键',
    member_id                   VARCHAR(64)     NOT NULL COMMENT '客户会员ID',
    distribution_id             VARCHAR(64)     DEFAULT NULL COMMENT '当前直属分销员ID',
    bind_source                 VARCHAR(32)     DEFAULT NULL COMMENT '绑定来源：SCAN扫码 ADMIN后台 SELF_BIND自绑定 RECRUIT招募',
    invite_code                 VARCHAR(32)     DEFAULT NULL COMMENT '绑定使用的邀请码',
    bind_mode                   VARCHAR(32)     DEFAULT NULL COMMENT '锁客模式快照',
    permanent_bind_mode         VARCHAR(32)     DEFAULT NULL COMMENT '永久锁客方式快照',
    relation_status             VARCHAR(32)     NOT NULL DEFAULT 'UNBOUND' COMMENT '关系状态：PENDING待转永久 TEMPORARY短期 PERMANENT永久 SELF_BIND自绑定 EXPIRED已到期 UNBOUND已解绑',
    bind_time                   DATETIME        DEFAULT NULL COMMENT '绑定建立时间',
    last_scan_time              DATETIME        DEFAULT NULL COMMENT '最近一次扫码时间',
    pending_permanent_deadline  DATETIME        DEFAULT NULL COMMENT '待转永久截止时间',
    expire_at                   DATETIME        DEFAULT NULL COMMENT '关系到期时间（短期锁客）',
    permanent_at                DATETIME        DEFAULT NULL COMMENT '永久生效时间',
    trigger_order_sn            VARCHAR(64)     DEFAULT NULL COMMENT '触发转永久的订单编号',
    starred                     TINYINT(1)      NOT NULL DEFAULT 0 COMMENT '是否星标客户：0否 1是',
    version                     INT             NOT NULL DEFAULT 0 COMMENT '乐观锁版本号',
    create_by                   VARCHAR(64)     DEFAULT NULL COMMENT '创建者',
    create_time                 DATETIME        DEFAULT NULL COMMENT '创建时间',
    update_by                   VARCHAR(64)     DEFAULT NULL COMMENT '更新者',
    update_time                 DATETIME        DEFAULT NULL COMMENT '更新时间',
    delete_flag                 TINYINT(1)      NOT NULL DEFAULT 0 COMMENT '删除标志',
    PRIMARY KEY (id),
    UNIQUE KEY uk_dcr_member (member_id),
    KEY idx_dcr_distribution_status (distribution_id, relation_status),
    KEY idx_dcr_status_expire (relation_status, expire_at),
    KEY idx_dcr_bind_time (bind_time),
    KEY idx_dcr_pending_deadline (pending_permanent_deadline)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='客户与分销员当前关系';


-- ============================================================
-- 第 7 步：新建 li_distribution_customer_relation_log（客户关系变更日志）
-- ============================================================

CREATE TABLE IF NOT EXISTS li_distribution_customer_relation_log (
    id                          VARCHAR(64)     NOT NULL COMMENT '主键',
    relation_id                 VARCHAR(64)     DEFAULT NULL COMMENT '关系记录ID',
    member_id                   VARCHAR(64)     NOT NULL COMMENT '客户会员ID',
    change_type                 VARCHAR(32)     NOT NULL COMMENT '变更类型：CREATE RENEW TO_PERMANENT SELF_BIND_REPLACE EXPIRE ADMIN_REBIND ADMIN_UNBIND RETREAT',
    before_distribution_id      VARCHAR(64)     DEFAULT NULL COMMENT '变更前分销员ID',
    after_distribution_id       VARCHAR(64)     DEFAULT NULL COMMENT '变更后分销员ID',
    before_relation_status      VARCHAR(32)     DEFAULT NULL COMMENT '变更前关系状态',
    after_relation_status       VARCHAR(32)     DEFAULT NULL COMMENT '变更后关系状态',
    before_expire_at            DATETIME        DEFAULT NULL COMMENT '变更前到期时间',
    after_expire_at             DATETIME        DEFAULT NULL COMMENT '变更后到期时间',
    operator_id                 VARCHAR(64)     DEFAULT NULL COMMENT '操作者ID',
    operator_type               VARCHAR(32)     DEFAULT NULL COMMENT '操作者类型：SYSTEM ADMIN',
    reason                      VARCHAR(500)    DEFAULT NULL COMMENT '变更原因',
    create_by                   VARCHAR(64)     DEFAULT NULL COMMENT '创建者',
    create_time                 DATETIME        DEFAULT NULL COMMENT '创建时间',
    update_by                   VARCHAR(64)     DEFAULT NULL COMMENT '更新者',
    update_time                 DATETIME        DEFAULT NULL COMMENT '更新时间',
    delete_flag                 TINYINT(1)      NOT NULL DEFAULT 0 COMMENT '删除标志',
    PRIMARY KEY (id),
    KEY idx_dcrl_member_time (member_id, create_time),
    KEY idx_dcrl_relation_time (relation_id, create_time),
    KEY idx_dcrl_change_type (change_type, create_time)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='客户关系变更日志';


-- ============================================================
-- 第 8 步：（已废弃）li_distribution_invite_relation
-- 邀请上下级关系已合并至 li_distribution.parent_distribution_id
-- 存量迁移见文末「第 29 步」注释块
-- ============================================================

-- ============================================================
-- 第 9 步：（已废弃）li_distribution_reward_order
-- 佣金已合并至 li_distribution_order（rebate / invite_commission）
-- 存量迁移见文末「第 29 步」注释块
-- ============================================================


-- ============================================================
-- 第 10 步：新建 li_distribution_wallet_log（分销钱包流水）
-- ============================================================

CREATE TABLE IF NOT EXISTS li_distribution_wallet_log (
    id                          VARCHAR(64)     NOT NULL COMMENT '主键',
    distribution_id             VARCHAR(64)     NOT NULL COMMENT '分销员ID',
    biz_type                    VARCHAR(32)     NOT NULL COMMENT '业务类型：COMMISSION_FREEZE COMMISSION_SETTLE COMMISSION_REFUND WITHDRAW_APPLY WITHDRAW_APPROVE WITHDRAW_REJECT MANUAL_ADJUST',
    biz_sn                      VARCHAR(64)     DEFAULT NULL COMMENT '业务单号（订单号/提现单号等）',
    change_amount               DECIMAL(20, 2)  NOT NULL DEFAULT 0.00 COMMENT '本次变动金额（可正可负）',
    before_can_rebate           DECIMAL(20, 2)  NOT NULL DEFAULT 0.00 COMMENT '变动前可提现余额',
    after_can_rebate            DECIMAL(20, 2)  NOT NULL DEFAULT 0.00 COMMENT '变动后可提现余额',
    before_commission_frozen    DECIMAL(20, 2)  NOT NULL DEFAULT 0.00 COMMENT '变动前待结算佣金冻结',
    after_commission_frozen     DECIMAL(20, 2)  NOT NULL DEFAULT 0.00 COMMENT '变动后待结算佣金冻结',
    before_withdrawal_frozen    DECIMAL(20, 2)  NOT NULL DEFAULT 0.00 COMMENT '变动前提现冻结',
    after_withdrawal_frozen     DECIMAL(20, 2)  NOT NULL DEFAULT 0.00 COMMENT '变动后提现冻结',
    before_withdrawn_total      DECIMAL(20, 2)  NOT NULL DEFAULT 0.00 COMMENT '变动前累计已提现',
    after_withdrawn_total       DECIMAL(20, 2)  NOT NULL DEFAULT 0.00 COMMENT '变动后累计已提现',
    operator_id                 VARCHAR(64)     DEFAULT NULL COMMENT '操作者ID',
    remark                      VARCHAR(500)    DEFAULT NULL COMMENT '备注',
    idempotent_key              VARCHAR(128)    NOT NULL COMMENT '幂等键（全局唯一）',
    create_by                   VARCHAR(64)     DEFAULT NULL COMMENT '创建者',
    create_time                 DATETIME        DEFAULT NULL COMMENT '创建时间',
    update_by                   VARCHAR(64)     DEFAULT NULL COMMENT '更新者',
    update_time                 DATETIME        DEFAULT NULL COMMENT '更新时间',
    delete_flag                 TINYINT(1)      NOT NULL DEFAULT 0 COMMENT '删除标志',
    PRIMARY KEY (id),
    UNIQUE KEY uk_dwl_idempotent (idempotent_key),
    KEY idx_dwl_distribution_time (distribution_id, create_time),
    KEY idx_dwl_biz (biz_type, biz_sn),
    KEY idx_dwl_create_time (create_time)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='分销钱包流水';


-- ============================================================
-- 第 11 步：新建 li_distribution_share_event（商品分享行为记录）
-- ============================================================

CREATE TABLE IF NOT EXISTS li_distribution_share_event (
    id                  VARCHAR(64)     NOT NULL COMMENT '主键',
    event_id            VARCHAR(64)     NOT NULL COMMENT '客户端事件ID（幂等）',
    share_id            VARCHAR(64)     NOT NULL COMMENT '分享标识（用于访问归因）',
    distribution_id     VARCHAR(64)     NOT NULL COMMENT '分享分销员ID',
    goods_id            VARCHAR(64)     DEFAULT NULL COMMENT '商品ID',
    sku_id              VARCHAR(64)     DEFAULT NULL COMMENT 'SKU ID',
    channel             VARCHAR(32)     DEFAULT NULL COMMENT '分享渠道：WECHAT_MINI_PROGRAM H5 等',
    trigger_page        VARCHAR(64)     DEFAULT NULL COMMENT '触发页面：distribution_home distribution_share 等',
    share_button_type   VARCHAR(64)     DEFAULT NULL COMMENT '分享按钮类型：IMMEDIATE_SHARE SAVE_TEXT_IMAGE SAVE_POSTER 等',
    share_time          DATETIME        NOT NULL COMMENT '分享点击时间',
    create_by           VARCHAR(64)     DEFAULT NULL COMMENT '创建者',
    create_time         DATETIME        DEFAULT NULL COMMENT '创建时间',
    update_by           VARCHAR(64)     DEFAULT NULL COMMENT '更新者',
    update_time         DATETIME        DEFAULT NULL COMMENT '更新时间',
    delete_flag         TINYINT(1)      NOT NULL DEFAULT 0 COMMENT '删除标志',
    PRIMARY KEY (id),
    UNIQUE KEY uk_dse_event_id (event_id),
    KEY idx_dse_share_id (share_id),
    KEY idx_dse_distribution_time (distribution_id, share_time),
    KEY idx_dse_goods_time (goods_id, share_time),
    KEY idx_dse_share_time (share_time)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='分销商品分享行为记录';


-- ============================================================
-- 第 12 步：新建 li_distribution_visit_event（分享链接访问记录）
-- ============================================================

CREATE TABLE IF NOT EXISTS li_distribution_visit_event (
    id                  VARCHAR(64)     NOT NULL COMMENT '主键',
    event_id            VARCHAR(255)    NOT NULL COMMENT '服务端幂等键：visitorId:distributionId:goodsId:yyyy-MM-dd',
    share_id            VARCHAR(64)     NOT NULL COMMENT '分享标识',
    distribution_id     VARCHAR(64)     NOT NULL COMMENT '分享分销员ID',
    goods_id            VARCHAR(64)     DEFAULT NULL COMMENT '商品ID',
    visitor_id          VARCHAR(64)     NOT NULL COMMENT '访客标识（未登录）',
    member_id           VARCHAR(64)     DEFAULT NULL COMMENT '会员ID（登录后关联）',
    visit_time          DATETIME        NOT NULL COMMENT '访问时间',
    create_by           VARCHAR(64)     DEFAULT NULL COMMENT '创建者',
    create_time         DATETIME        DEFAULT NULL COMMENT '创建时间',
    update_by           VARCHAR(64)     DEFAULT NULL COMMENT '更新者',
    update_time         DATETIME        DEFAULT NULL COMMENT '更新时间',
    delete_flag         TINYINT(1)      NOT NULL DEFAULT 0 COMMENT '删除标志',
    PRIMARY KEY (id),
    UNIQUE KEY uk_dve_event_id (event_id),
    KEY idx_dve_share_time (share_id, visit_time),
    KEY idx_dve_distribution_time (distribution_id, visit_time),
    KEY idx_dve_goods_time (goods_id, visit_time),
    KEY idx_dve_visitor_time (visitor_id, visit_time),
    KEY idx_dve_member_time (member_id, visit_time)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='分销分享链接访问记录';


-- ============================================================
-- 第 13 步：新建 li_distribution_recruit_application（招募申请记录）
-- 说明：招募计划/动态字段已迁至 li_setting（DISTRIBUTION_RECRUIT_SETTING），不再建 plan 子表
-- ============================================================

CREATE TABLE IF NOT EXISTS li_distribution_recruit_application (
    id                      VARCHAR(64)     NOT NULL COMMENT '主键',
    member_id               VARCHAR(64)     NOT NULL COMMENT '申请人会员ID',
    plan_id                 VARCHAR(64)     NOT NULL COMMENT '分销招募设置ID（li_setting）',
    plan_version_no         INT             NOT NULL COMMENT '申请时设置版本号',
    apply_source            VARCHAR(32)     NOT NULL COMMENT '申请来源：MANUAL_ENTRY AUTO_ENTRY ADMIN',
    condition_snapshot      TEXT            COMMENT '条件快照（JSON）',
    apply_form_snapshot     TEXT            COMMENT '动态填写内容快照（JSON）',
    recruit_token           VARCHAR(64)     DEFAULT NULL COMMENT '招募邀请令牌',
    recruit_inviter_id      VARCHAR(64)     DEFAULT NULL COMMENT '招募邀请方分销员ID',
    bound_distributor_id    VARCHAR(64)     DEFAULT NULL COMMENT '申请时客户绑定分销员ID',
    agreement_content       TEXT            COMMENT '申请时服务协议内容快照（富文本）',
    agreement_accept_time   DATETIME        DEFAULT NULL COMMENT '协议接受时间',
    audit_mode_snapshot     VARCHAR(16)     DEFAULT NULL COMMENT '审核方式快照',
    application_status      VARCHAR(32)     NOT NULL DEFAULT 'PENDING' COMMENT '状态：PENDING待审核 APPROVED已通过 REJECTED已拒绝 CANCELLED已取消',
    audit_user_id           VARCHAR(64)     DEFAULT NULL COMMENT '审核人ID',
    audit_time              DATETIME        DEFAULT NULL COMMENT '审核时间',
    reject_reason           VARCHAR(500)    DEFAULT NULL COMMENT '拒绝原因',
    distribution_id         VARCHAR(64)     DEFAULT NULL COMMENT '审核通过后关联的分销员ID',
    create_by               VARCHAR(64)     DEFAULT NULL COMMENT '创建者',
    create_time             DATETIME        DEFAULT NULL COMMENT '创建时间',
    update_by               VARCHAR(64)     DEFAULT NULL COMMENT '更新者',
    update_time             DATETIME        DEFAULT NULL COMMENT '更新时间',
    delete_flag             TINYINT(1)      NOT NULL DEFAULT 0 COMMENT '删除标志',
    PRIMARY KEY (id),
    KEY idx_dra_member_status (member_id, application_status),
    KEY idx_dra_plan_time (plan_id, create_time),
    KEY idx_dra_status_time (application_status, create_time),
    KEY idx_dra_recruit_inviter (recruit_inviter_id),
    KEY idx_dra_distribution (distribution_id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='分销员招募申请记录';


-- ============================================================
-- 第 14 步：新建 li_distribution_stats_daily_distributor（分销员日聚合，可选）
-- 用于行为数据/概览的离线快照，由日结任务幂等写入
-- ============================================================

CREATE TABLE IF NOT EXISTS li_distribution_stats_daily_distributor (
    id                      VARCHAR(64)     NOT NULL COMMENT '主键',
    stat_date               DATE            NOT NULL COMMENT '统计日期（业务时区 Asia/Shanghai）',
    distributor_id          VARCHAR(64)     NOT NULL COMMENT '分销员ID',
    member_id               VARCHAR(64)     DEFAULT NULL COMMENT '会员ID（冗余）',
    grade_id                VARCHAR(64)     DEFAULT NULL COMMENT '当日结束时等级ID',
    new_sales_amount        DECIMAL(20, 2)  NOT NULL DEFAULT 0.00 COMMENT '新增销售额（原始实付口径）',
    new_customer_count      INT             NOT NULL DEFAULT 0 COMMENT '新增客户数',
    pay_customer_count      INT             NOT NULL DEFAULT 0 COMMENT '支付客户数',
    new_invite_count        INT             NOT NULL DEFAULT 0 COMMENT '新增邀请下级数',
    order_invite_count      INT             NOT NULL DEFAULT 0 COMMENT '开单邀请数（产生业绩的下级去重）',
    share_count             INT             NOT NULL DEFAULT 0 COMMENT '商品分享次数',
    share_goods_count       INT             NOT NULL DEFAULT 0 COMMENT '分享商品数（goods_id去重）',
    share_uv                INT             NOT NULL DEFAULT 0 COMMENT '分享引流UV',
    share_pv                INT             NOT NULL DEFAULT 0 COMMENT '分享浏览量PV',
    settled_commission      DECIMAL(20, 2)  NOT NULL DEFAULT 0.00 COMMENT '当日结算佣金（DIRECT+INVITE）',
    create_by               VARCHAR(64)     DEFAULT NULL COMMENT '创建者',
    create_time             DATETIME        DEFAULT NULL COMMENT '创建时间',
    update_by               VARCHAR(64)     DEFAULT NULL COMMENT '更新者',
    update_time             DATETIME        DEFAULT NULL COMMENT '更新时间',
    delete_flag             TINYINT(1)      NOT NULL DEFAULT 0 COMMENT '删除标志',
    PRIMARY KEY (id),
    UNIQUE KEY uk_dsdd_date_distributor (stat_date, distributor_id),
    KEY idx_dsdd_distributor_date (distributor_id, stat_date),
    KEY idx_dsdd_stat_date (stat_date)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='分销员按日聚合快照（可选）';


-- ============================================================
-- 第 15 步：新建 li_distribution_import_batch / li_distribution_import_item（分销员批量导入）
-- ============================================================

CREATE TABLE IF NOT EXISTS li_distribution_import_batch (
    id              VARCHAR(64)     NOT NULL COMMENT '主键',
    file_name       VARCHAR(255)    DEFAULT NULL COMMENT '导入文件名',
    total_count     INT             DEFAULT NULL COMMENT '总行数',
    success_count   INT             DEFAULT NULL COMMENT '成功行数',
    fail_count      INT             DEFAULT NULL COMMENT '失败行数',
    status          VARCHAR(32)     DEFAULT NULL COMMENT '状态：PROCESSING/DONE/FAILED',
    operator_id     VARCHAR(64)     DEFAULT NULL COMMENT '操作人ID',
    operator_name   VARCHAR(128)    DEFAULT NULL COMMENT '操作人名称',
    remark          VARCHAR(500)    DEFAULT NULL COMMENT '备注',
    create_by       VARCHAR(64)     DEFAULT NULL COMMENT '创建者',
    create_time     DATETIME        DEFAULT NULL COMMENT '创建时间',
    update_by       VARCHAR(64)     DEFAULT NULL COMMENT '更新者',
    update_time     DATETIME        DEFAULT NULL COMMENT '更新时间',
    delete_flag     TINYINT(1)      NOT NULL DEFAULT 0 COMMENT '删除标志',
    PRIMARY KEY (id),
    KEY idx_dib_status_time (status, create_time),
    KEY idx_dib_operator_time (operator_id, create_time)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='分销员批量导入批次';


CREATE TABLE IF NOT EXISTS li_distribution_import_item (
    id                  VARCHAR(64)     NOT NULL COMMENT '主键',
    batch_id            VARCHAR(64)     NOT NULL COMMENT '批次ID',
    row_num             INT             DEFAULT NULL COMMENT 'Excel行号',
    mobile              VARCHAR(32)     DEFAULT NULL COMMENT '手机号',
    open_id             VARCHAR(128)    DEFAULT NULL COMMENT '分销员openid',
    nickname            VARCHAR(128)    DEFAULT NULL COMMENT '昵称',
    inviter_mobile      VARCHAR(32)     DEFAULT NULL COMMENT '邀请人手机号',
    inviter_open_id     VARCHAR(128)    DEFAULT NULL COMMENT '邀请人openid',
    grade_sort          INT             DEFAULT NULL COMMENT '等级序号',
    distribution_id     VARCHAR(64)     DEFAULT NULL COMMENT '成功后的分销员ID',
    status              VARCHAR(32)     DEFAULT NULL COMMENT '状态：SUCCESS/FAIL',
    fail_reason         VARCHAR(500)    DEFAULT NULL COMMENT '失败原因',
    create_by           VARCHAR(64)     DEFAULT NULL COMMENT '创建者',
    create_time         DATETIME        DEFAULT NULL COMMENT '创建时间',
    update_by           VARCHAR(64)     DEFAULT NULL COMMENT '更新者',
    update_time         DATETIME        DEFAULT NULL COMMENT '更新时间',
    delete_flag         TINYINT(1)      NOT NULL DEFAULT 0 COMMENT '删除标志',
    PRIMARY KEY (id),
    KEY idx_dii_batch_row (batch_id, row_num),
    KEY idx_dii_batch_status (batch_id, status),
    KEY idx_dii_distribution (distribution_id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='分销员批量导入明细';


-- ============================================================
-- 第 16 步：新建 li_distribution_bank_card（分销员银行卡）
-- ============================================================

CREATE TABLE IF NOT EXISTS li_distribution_bank_card (
    id                  VARCHAR(64)     NOT NULL COMMENT '主键',
    distribution_id     VARCHAR(64)     NOT NULL COMMENT '分销员ID',
    holder_name         VARCHAR(64)     NOT NULL COMMENT '收款人姓名',
    bank_name           VARCHAR(128)    NOT NULL COMMENT '银行名称',
    card_no             VARCHAR(32)     NOT NULL COMMENT '银行卡号',
    is_default          TINYINT(1)      NOT NULL DEFAULT 0 COMMENT '是否默认卡：0否 1是',
    create_by           VARCHAR(64)     DEFAULT NULL COMMENT '创建者',
    create_time         DATETIME        DEFAULT NULL COMMENT '创建时间',
    update_by           VARCHAR(64)     DEFAULT NULL COMMENT '更新者',
    update_time         DATETIME        DEFAULT NULL COMMENT '更新时间',
    delete_flag         TINYINT(1)      NOT NULL DEFAULT 0 COMMENT '删除标志',
    PRIMARY KEY (id),
    KEY idx_dbc_distribution (distribution_id),
    KEY idx_dbc_distribution_default (distribution_id, is_default)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='分销员银行卡';


-- ============================================================
-- 第 20 步：初始化默认分销等级（可按业务调整）
-- ============================================================

INSERT INTO li_distribution_grade (
    id, grade_name, grade_sort, sales_threshold, commission_threshold, invite_threshold,
    sales_enabled, commission_enabled, invite_enabled,
    product_commission_rate, invite_commission_rate,
    is_default, create_by, create_time, update_by, update_time, delete_flag
)
SELECT
    '1000000000000000001',
    '普通分销员',
    1,
    0.00,
    0.00,
    0,
    0,
    0,
    0,
    0.1000,
    0.0000,
    1,
    'system',
    NOW(),
    'system',
    NOW(),
    0
FROM DUAL
WHERE NOT EXISTS (
    SELECT 1 FROM li_distribution_grade WHERE grade_name = '普通分销员' AND delete_flag = 0
);


-- ============================================================
-- 第 21 步：平台后台菜单 INSERT 模板（默认注释，按需取消注释执行）
-- 显示位置建议：运营 -> 分销管理 下新增子菜单
--
-- 注意：
-- 1. li_menu.id 为数值型雪花 ID，不可用 UUID 字符串
-- 2. parent_id 请替换为你库中「分销管理」菜单的实际 ID
-- 3. front_route 需与 manager 前端 views 路径一致
-- 4. 执行后请清 Redis 权限缓存并重新登录平台后台
-- ============================================================

/*
-- ---------- 诊断：查找「分销管理」父菜单 ----------
SELECT id, title, name, level, parent_id, front_route, delete_flag
FROM li_menu
WHERE delete_flag = 0
  AND (title LIKE '%分销%' OR name LIKE '%distribution%')
ORDER BY level, sort_order;


-- ---------- 分销等级管理 ----------
INSERT INTO li_menu (
    id, title, name, path, level, front_route, parent_id, sort_order, permission,
    create_by, create_time, update_by, update_time, delete_flag
)
SELECT
    FLOOR(UNIX_TIMESTAMP(NOW(3)) * 1000000) + FLOOR(RAND() * 1000000),
    '分销等级',
    'distributionGrade',
    'distributionGrade',
    2,
    'distribution/distributionGrade',
    parent.id,
    IFNULL((
        SELECT MAX(child.sort_order) + 1
        FROM li_menu child
        WHERE child.parent_id = parent.id AND child.delete_flag = 0
    ), 1),
    '/manager/distribution/grade*,/manager/distribution/grade/**',
    'system', NOW(), 'system', NOW(), 0
FROM li_menu parent
WHERE parent.delete_flag = 0
  AND parent.title = '分销管理'
  AND parent.level = 1
  AND NOT EXISTS (
      SELECT 1 FROM li_menu m
      WHERE m.name = 'distributionGrade' AND m.delete_flag = 0
  )
LIMIT 1;


-- ---------- 分销招募设置 ----------
INSERT INTO li_menu (
    id, title, name, path, level, front_route, parent_id, sort_order, permission,
    create_by, create_time, update_by, update_time, delete_flag
)
SELECT
    FLOOR(UNIX_TIMESTAMP(NOW(3)) * 1000000) + FLOOR(RAND() * 1000000),
    '分销招募',
    'distributionRecruit',
    'distributionRecruit',
    2,
    'distribution/distributionRecruit',
    parent.id,
    IFNULL((
        SELECT MAX(child.sort_order) + 1
        FROM li_menu child
        WHERE child.parent_id = parent.id AND child.delete_flag = 0
    ), 1),
    '/manager/distribution/recruit*,/manager/distribution/recruit/**,/manager/distribution/setting/v2/recruit*',
    'system', NOW(), 'system', NOW(), 0
FROM li_menu parent
WHERE parent.delete_flag = 0
  AND parent.title = '分销管理'
  AND parent.level = 1
  AND NOT EXISTS (
      SELECT 1 FROM li_menu m
      WHERE m.name = 'distributionRecruit' AND m.delete_flag = 0
  )
LIMIT 1;


-- ---------- 客户关系管理 ----------
INSERT INTO li_menu (
    id, title, name, path, level, front_route, parent_id, sort_order, permission,
    create_by, create_time, update_by, update_time, delete_flag
)
SELECT
    FLOOR(UNIX_TIMESTAMP(NOW(3)) * 1000000) + FLOOR(RAND() * 1000000),
    '客户关系',
    'distributionCustomerRelation',
    'distributionCustomerRelation',
    2,
    'distribution/distributionCustomerRelation',
    parent.id,
    IFNULL((
        SELECT MAX(child.sort_order) + 1
        FROM li_menu child
        WHERE child.parent_id = parent.id AND child.delete_flag = 0
    ), 1),
    '/manager/distribution/customerRelation*,/manager/distribution/customerRelation/**',
    'system', NOW(), 'system', NOW(), 0
FROM li_menu parent
WHERE parent.delete_flag = 0
  AND parent.title = '分销管理'
  AND parent.level = 1
  AND NOT EXISTS (
      SELECT 1 FROM li_menu m
      WHERE m.name = 'distributionCustomerRelation' AND m.delete_flag = 0
  )
LIMIT 1;


-- 说明：行为数据/商品数据/概览 V2 已合并进现有「分销统计」菜单（statistics/distribution），无需单独建页。


-- ---------- 为超级管理员角色绑定上述菜单（role_id 请按实际替换） ----------
INSERT INTO li_role_menu (
    id, role_id, menu_id, is_super, create_by, create_time, update_by, update_time, delete_flag
)
SELECT
    FLOOR(UNIX_TIMESTAMP(NOW(3)) * 1000000) + FLOOR(RAND() * 1000000) + ABS(CRC32(CONCAT(r.id, m.id))),
    r.id,
    m.id,
    1,
    'system', NOW(), 'system', NOW(), 0
FROM li_role r
INNER JOIN li_menu m
    ON m.name IN (
        'distributionGrade',
        'distributionRecruit',
        'distributionCustomerRelation'
    )
   AND m.delete_flag = 0
WHERE r.delete_flag = 0
  AND r.name = '超级管理员'
  AND NOT EXISTS (
      SELECT 1
      FROM li_role_menu rm
      WHERE rm.role_id = r.id AND rm.menu_id = m.id AND rm.delete_flag = 0
  );


-- ---------- 确认插入结果 ----------
SELECT id, title, name, level, parent_id, front_route
FROM li_menu
WHERE delete_flag = 0
  AND name IN (
      'distributionGrade',
      'distributionRecruit',
      'distributionCustomerRelation'
  )
ORDER BY sort_order;


-- ---------- 刷新缓存 ----------
-- Redis CLI：
--   redis-cli KEYS "{PERMISSION_LIST_MANAGER}_*"
--   redis-cli KEYS "{MANAGER_USER_MENU}_*"
-- 删除上述 key 后重新登录平台后台
*/


-- ============================================================
-- 第 22 步：等级升级条件启用字段（已有库增量）
-- ============================================================

ALTER TABLE li_distribution_grade
    ADD COLUMN IF NOT EXISTS sales_enabled TINYINT(1) NOT NULL DEFAULT 1 COMMENT '是否启用销售额升级条件' AFTER invite_threshold,
    ADD COLUMN IF NOT EXISTS commission_enabled TINYINT(1) NOT NULL DEFAULT 1 COMMENT '是否启用收益额升级条件' AFTER sales_enabled,
    ADD COLUMN IF NOT EXISTS invite_enabled TINYINT(1) NOT NULL DEFAULT 1 COMMENT '是否启用邀请人数升级条件' AFTER commission_enabled;

UPDATE li_distribution_grade
SET sales_enabled = 0,
    commission_enabled = 0,
    invite_enabled = 0
WHERE grade_sort = 1
  AND delete_flag = 0;


-- ============================================================
-- 第 23 步：分销商品审核字段（已有库增量）
-- ============================================================

ALTER TABLE li_distribution_goods
    ADD COLUMN IF NOT EXISTS auth_status VARCHAR(32) NOT NULL DEFAULT 'PASS' COMMENT '审核状态：TOBEAUDITED待审核 PASS已审核 REFUSE审核未通过' AFTER quantity,
    ADD COLUMN IF NOT EXISTS auth_remark VARCHAR(255) DEFAULT NULL COMMENT '审核备注' AFTER auth_status;


-- ============================================================
-- 第 24 步：（已废弃）邀请关系表字段对齐
-- 存量迁移见文末「第 29 步」注释块
-- ============================================================


-- ============================================================
-- 第 25 步：分销员等级字段统一为等级值（1-5）
-- ============================================================

UPDATE li_distribution d
    JOIN li_distribution_grade g ON d.grade_id = g.id
SET d.grade_id = CAST(g.grade_sort AS CHAR)
WHERE d.grade_id IS NOT NULL
  AND d.grade_id <> '';


-- ============================================================
-- 第 26 步：移除旧「分销申请」菜单（招募审核已迁至「分销招募」）
-- ============================================================

UPDATE li_role_menu rm
INNER JOIN li_menu m ON rm.menu_id = m.id
SET rm.delete_flag = 1,
    rm.update_time = NOW()
WHERE rm.delete_flag = 0
  AND m.delete_flag = 0
  AND (m.name = 'distributionApply' OR m.path = 'distributionApply' OR m.title = '分销申请');

UPDATE li_menu
SET delete_flag = 1,
    update_time = NOW()
WHERE delete_flag = 0
  AND (name = 'distributionApply' OR path = 'distributionApply' OR title = '分销申请');


-- ============================================================
-- 第 27 步：分销员乐观锁 version 字段补全（已有库增量）
-- ============================================================

UPDATE li_distribution
SET version = 0
WHERE version IS NULL;


-- ============================================================
-- 第 29 步：V2 精简 — 存量库迁移与删除废弃字段/表
-- 新环境若已按上文最终结构建表，本步多数语句可跳过或仅执行 DROP 清理
-- ============================================================

-- ---------- 29.0 存量库：邀请关系回填至 parent_distribution_id ----------
-- UPDATE li_distribution d
--     INNER JOIN li_distribution_invite_relation ir
--         ON ir.child_distribution_id = d.id AND IFNULL(ir.delete_flag, 0) = 0
-- SET d.parent_distribution_id = ir.parent_distribution_id
-- WHERE d.parent_distribution_id IS NULL
--   AND ir.parent_distribution_id IS NOT NULL;

-- ---------- 29.1 存量库：li_distribution_order 邀请佣金字段（若第 2 步未执行）----------
ALTER TABLE li_distribution_order
    ADD COLUMN IF NOT EXISTS invite_distribution_id VARCHAR(64) DEFAULT NULL COMMENT '二级分销员ID（邀请奖励受益人）' AFTER distribution_name,
    ADD COLUMN IF NOT EXISTS invite_distribution_name VARCHAR(255) DEFAULT NULL COMMENT '二级分销员名称' AFTER invite_distribution_id,
    ADD COLUMN IF NOT EXISTS invite_commission DOUBLE DEFAULT NULL COMMENT '二级邀请佣金' AFTER rebate,
    ADD COLUMN IF NOT EXISTS invite_sell_back_rebate DOUBLE DEFAULT 0 COMMENT '二级邀请佣金退款冲回金额' AFTER sell_back_rebate,
    ADD COLUMN IF NOT EXISTS settle_time DATETIME DEFAULT NULL COMMENT '佣金结算时间' AFTER settle_cycle;

-- ---------- 29.2 存量库：reward_order 回填邀请佣金 ----------
-- UPDATE li_distribution_order do
--     INNER JOIN li_distribution_reward_order r
--         ON r.order_item_sn = do.order_item_sn
--         AND r.reward_type = 'INVITE'
--         AND IFNULL(r.delete_flag, 0) = 0
-- SET do.invite_distribution_id = COALESCE(do.invite_distribution_id, r.beneficiary_distribution_id),
--     do.invite_commission = COALESCE(do.invite_commission, r.reward_amount),
--     do.settle_time = COALESCE(do.settle_time, r.settle_time)
-- WHERE IFNULL(do.invite_commission, 0) = 0 OR do.invite_distribution_id IS NULL;

-- ---------- 29.3 删除 li_distribution 已废弃字段 ----------
ALTER TABLE li_distribution
    DROP COLUMN IF EXISTS name,
    DROP COLUMN IF EXISTS id_number,
    DROP COLUMN IF EXISTS settlement_bank_account_name,
    DROP COLUMN IF EXISTS settlement_bank_account_num,
    DROP COLUMN IF EXISTS settlement_bank_branch_name;

-- ---------- 29.4 删除 li_distribution_grade 已废弃字段 ----------
ALTER TABLE li_distribution_grade
    DROP COLUMN IF EXISTS condition_match_mode,
    DROP COLUMN IF EXISTS grade_state;

-- ---------- 29.5 删除已废弃表 ----------
DROP TABLE IF EXISTS li_distribution_invite_relation;
DROP TABLE IF EXISTS li_distribution_reward_order;
DROP TABLE IF EXISTS li_distribution_selected_goods;

-- ---------- 29.6 删除 li_distribution_goods 冗余库存（实时读 SKU 库存）----------
ALTER TABLE li_distribution_goods
    DROP COLUMN IF EXISTS quantity;

-- ---------- 29.7 招募设置迁至 li_setting，删除计划/字段子表（存量库）----------
-- 【存量库】若 li_distribution_recruit_plan 有数据，请先在管理端重新保存一次招募设置，或自行将配置写入 li_setting（id=DISTRIBUTION_RECRUIT_SETTING）。
DROP TABLE IF EXISTS li_distribution_recruit_field;
DROP TABLE IF EXISTS li_distribution_recruit_plan_goods;
DROP TABLE IF EXISTS li_distribution_recruit_plan;

-- ---------- 29.8 招募申请动态填写内容合并至申请主表（存量库）----------
ALTER TABLE li_distribution_recruit_application
    ADD COLUMN IF NOT EXISTS apply_form_snapshot TEXT COMMENT '动态填写内容快照（JSON）' AFTER condition_snapshot;

DROP TABLE IF EXISTS li_distribution_recruit_application_answer;

-- ---------- 29.9 分销模式设置已合并至 DISTRIBUTION_SETTING，删除旧 li_setting 记录 ----------
DELETE FROM li_setting WHERE id = 'DISTRIBUTION_MODE_SETTING';


-- ============================================================
-- 第 30 步：执行后验证（可选）
-- ============================================================

SELECT TABLE_NAME, TABLE_COMMENT
FROM information_schema.TABLES
WHERE TABLE_SCHEMA = DATABASE()
  AND TABLE_NAME LIKE 'li_distribution%'
ORDER BY TABLE_NAME;


-- ============================================================
-- 第 31 步：分销等级比例统一为 0～1 小数（0.1 = 10%）
-- 先查再迁。1.5 这类值按「>1 则 ÷100」会变成 0.015，若本意是 1.5% 需人工改。
-- ============================================================

SELECT id, grade_name, product_commission_rate, invite_commission_rate
FROM li_distribution_grade WHERE delete_flag = 0;

-- 历史表先看一眼：运行时 Java 实体列为 goods_commission_rate / invite_commission_rate。
-- 仓库建表已是扁平列；运行库若仍无这两列（尚未打 008），下面 UPDATE 会跳过，勿对 JSON 快照做 ÷100。
SELECT COLUMN_NAME, COLUMN_TYPE
FROM information_schema.COLUMNS
WHERE TABLE_SCHEMA = DATABASE()
  AND TABLE_NAME = 'li_distribution_grade_history'
  AND COLUMN_NAME IN ('goods_commission_rate', 'invite_commission_rate');

-- 明显按百分数存的值（>1）转为小数
UPDATE li_distribution_grade
SET product_commission_rate = product_commission_rate / 100
WHERE product_commission_rate > 1;

UPDATE li_distribution_grade
SET invite_commission_rate = invite_commission_rate / 100
WHERE invite_commission_rate > 1;

-- 旧种子 1.0000 是「看起来像 1%、结算当整池」，改为默认 10%
UPDATE li_distribution_grade
SET product_commission_rate = 0.1000
WHERE id = '1000000000000000001'
  AND product_commission_rate = 1.0000;

-- 历史表：有比例列才迁（与等级表同样 >1 ÷100）
SET @db = DATABASE();

SET @exist = (
    SELECT COUNT(*) FROM information_schema.COLUMNS
    WHERE TABLE_SCHEMA = @db AND TABLE_NAME = 'li_distribution_grade_history' AND COLUMN_NAME = 'goods_commission_rate'
);
SET @sql = IF(@exist > 0,
    'UPDATE li_distribution_grade_history SET goods_commission_rate = goods_commission_rate / 100 WHERE goods_commission_rate > 1',
    'SELECT 1');
PREPARE stmt FROM @sql; EXECUTE stmt; DEALLOCATE PREPARE stmt;

SET @exist = (
    SELECT COUNT(*) FROM information_schema.COLUMNS
    WHERE TABLE_SCHEMA = @db AND TABLE_NAME = 'li_distribution_grade_history' AND COLUMN_NAME = 'invite_commission_rate'
);
SET @sql = IF(@exist > 0,
    'UPDATE li_distribution_grade_history SET invite_commission_rate = invite_commission_rate / 100 WHERE invite_commission_rate > 1',
    'SELECT 1');
PREPARE stmt FROM @sql; EXECUTE stmt; DEALLOCATE PREPARE stmt;

ALTER TABLE li_distribution_grade
    MODIFY COLUMN product_commission_rate DECIMAL(10, 4) NOT NULL DEFAULT 0.0000 COMMENT '直属商品佣金比例（占佣金池，0.1 表示 10%）',
    MODIFY COLUMN invite_commission_rate DECIMAL(10, 4) NOT NULL DEFAULT 0.0000 COMMENT '邀请奖励比例（占佣金池，0.1 表示 10%）';
