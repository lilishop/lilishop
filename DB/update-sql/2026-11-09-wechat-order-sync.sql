-- 小程序订单实用版：支付流水级同步状态及脱敏审计日志。
-- @author Bulbasaur
-- @since 2026-11-09
-- 无业务数据回填：历史无记录订单由接口显示 UNCHECKED（待核查），不推断成功。
-- 先执行此脚本，再部署后端和管理端；不修改配置、订单及角色授权。
-- CREATE TABLE 为 DDL，MySQL 可能隐式提交；不承诺整份脚本事务回滚。

CREATE TABLE IF NOT EXISTS li_wechat_order_sync (
    id varchar(32) NOT NULL COMMENT '同步记录 ID',
    transaction_id varchar(128) CHARACTER SET ascii COLLATE ascii_bin NOT NULL COMMENT '微信流水唯一键；缺流水异常使用 ORDER:商城订单号占位',
    app_id varchar(64) DEFAULT NULL COMMENT '首次发出请求时的小程序 AppID 快照',
    mch_id varchar(64) DEFAULT NULL COMMENT '首次发出请求时的微信商户号快照',
    status varchar(20) NOT NULL DEFAULT 'PENDING' COMMENT 'PENDING/PROCESSING/SUCCESS/FAILED；无记录显示 UNCHECKED',
    request_hash varchar(64) DEFAULT NULL COMMENT '请求 SHA256 摘要，不存明文物流及身份凭据',
    success_hash varchar(64) DEFAULT NULL COMMENT '最近成功请求摘要；与最近失败尝试版本分开，用于防重',
    upload_time varchar(64) DEFAULT NULL COMMENT 'ISO8601 带时区上传时间；同版本失败重试复用',
    error_code varchar(64) DEFAULT NULL COMMENT '最近发货上报错误码',
    error_message varchar(500) DEFAULT NULL COMMENT '最近发货上报脱敏错误说明',
    last_attempt_time datetime(3) DEFAULT NULL COMMENT '最近一次发货上报尝试时间，Asia/Shanghai',
    success_time datetime(3) DEFAULT NULL COMMENT '最近一次发货上报成功时间，非收货或结算时间',
    receive_status varchar(20) DEFAULT NULL COMMENT '收货通知状态；SUCCESS 表示接口已受理',
    receive_time datetime(3) DEFAULT NULL COMMENT '收货通知成功时间，Asia/Shanghai',
    create_time datetime(3) NOT NULL COMMENT '创建时间，Asia/Shanghai',
    update_time datetime(3) NOT NULL COMMENT '更新时间，Asia/Shanghai',
    PRIMARY KEY (id),
    UNIQUE KEY uk_wechat_order_sync_transaction (transaction_id),
    KEY idx_wechat_order_sync_status_time (status, update_time)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='微信支付流水级订单发货同步状态';

CREATE TABLE IF NOT EXISTS li_wechat_order_sync_log (
    id varchar(32) NOT NULL COMMENT '日志 ID',
    transaction_id varchar(128) CHARACTER SET ascii COLLATE ascii_bin NOT NULL COMMENT '关联同步状态流水键',
    order_sn varchar(64) NOT NULL COMMENT '触发操作的商城订单号',
    operation varchar(20) NOT NULL COMMENT 'SHIPPING 发货上报 / RECEIVE 收货通知',
    trigger_type varchar(20) NOT NULL COMMENT 'AUTO / MANUAL',
    operator_id varchar(64) NOT NULL COMMENT '管理员 ID 或 SYSTEM',
    operator_name varchar(255) DEFAULT NULL COMMENT '操作人姓名快照',
    status varchar(20) NOT NULL COMMENT 'PROCESSING/SUCCESS/FAILED/PENDING/SKIPPED',
    request_hash varchar(64) DEFAULT NULL COMMENT '请求内容摘要',
    summary varchar(2000) DEFAULT NULL COMMENT '仅含商城订单号及配送数量，不保存原始请求',
    error_code varchar(64) DEFAULT NULL COMMENT '本次业务错误码或微信错误码',
    error_message varchar(500) DEFAULT NULL COMMENT '本次脱敏错误说明',
    create_time datetime(3) NOT NULL COMMENT '开始时间，Asia/Shanghai',
    finish_time datetime(3) DEFAULT NULL COMMENT '结束时间，Asia/Shanghai',
    duration_ms bigint DEFAULT NULL COMMENT '耗时，毫秒',
    PRIMARY KEY (id),
    KEY idx_wechat_order_sync_log_transaction_time (transaction_id, create_time, id),
    KEY idx_wechat_order_sync_log_order_time (order_sn, create_time)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='微信订单上报脱敏操作日志';

-- 验证建议：SHOW CREATE TABLE 两张新表；确认 transaction_id 唯一索引存在。
-- 发布后用合并支付、未发货、缺物流及重复事件分别验证，不批量补报历史订单。
