/**
 * 移除 Elasticsearch，系统日志改存 MySQL
 */
CREATE TABLE IF NOT EXISTS `li_system_log` (
  `id` varchar(64) NOT NULL COMMENT '主键',
  `create_time` bigint DEFAULT NULL COMMENT '日志记录时间',
  `username` varchar(255) DEFAULT NULL COMMENT '请求用户',
  `request_url` varchar(500) DEFAULT NULL COMMENT '请求路径',
  `request_param` text COMMENT '请求参数',
  `response_body` text COMMENT '响应参数',
  `ip` varchar(64) DEFAULT NULL COMMENT 'ip',
  `name` varchar(255) DEFAULT NULL COMMENT '方法操作名称',
  `request_type` varchar(32) DEFAULT NULL COMMENT '请求类型',
  `customer_log` text COMMENT '自定义日志内容',
  `ip_info` varchar(500) DEFAULT NULL COMMENT 'ip信息',
  `cost_time` int DEFAULT NULL COMMENT '花费时间',
  `store_id` bigint DEFAULT NULL COMMENT '商家',
  PRIMARY KEY (`id`),
  KEY `idx_li_system_log_store_id` (`store_id`),
  KEY `idx_li_system_log_create_time` (`create_time`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin COMMENT='系统操作日志';
