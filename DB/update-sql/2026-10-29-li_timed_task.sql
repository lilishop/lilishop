-- -----------------------------------------------------
-- 1. 创建定时任务配置表 li_timed_task
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `li_timed_task` (
  `id` varchar(64) NOT NULL COMMENT '任务唯一标识',
  `task_name` varchar(100) NOT NULL COMMENT '任务名称',
  `group_name` varchar(50) NOT NULL DEFAULT '默认任务' COMMENT '任务分组',
  `bean_name` varchar(100) NOT NULL COMMENT 'Spring Bean类名或名称',
  `method_name` varchar(64) NOT NULL DEFAULT 'execute' COMMENT '调用的方法名',
  `cron` varchar(64) NOT NULL COMMENT 'Cron 表达式',
  `status` tinyint(1) NOT NULL DEFAULT 1 COMMENT '状态：1 启用，0 停用',
  `description` varchar(255) DEFAULT NULL COMMENT '任务描述',
  `last_exec_time` datetime DEFAULT NULL COMMENT '上次执行时间',
  `last_exec_status` tinyint(1) DEFAULT NULL COMMENT '上次执行结果：1 成功，0 失败',
  `last_exec_cost` bigint DEFAULT NULL COMMENT '上次执行耗时(毫秒)',
  `last_exec_error` text DEFAULT NULL COMMENT '上次执行异常信息',
  `create_time` datetime DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
  `update_time` datetime DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '更新时间',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='定时任务配置表';

-- -----------------------------------------------------
-- 2. 初始化预置定时任务数据
-- -----------------------------------------------------
INSERT INTO `li_timed_task` (`id`, `task_name`, `group_name`, `bean_name`, `method_name`, `cron`, `status`, `description`) VALUES
-- 订单拆分子任务
('ORDER_AUTO_COMPLETE', '订单自动确认收货', '订单任务', 'orderEveryDayTaskExecute', 'executeAutoComplete', '0 0 3 * * ?', 1, '订单发货后达到自动收货时限自动确认收货'),
('ORDER_AUTO_EVALUATE', '订单自动好评', '订单任务', 'orderEveryDayTaskExecute', 'executeAutoEvaluate', '0 0 3 * * ?', 1, '订单完成达到评价时限未评价自动五星好评'),
('ORDER_CLOSE_AFTER_SALE', '自动关闭超时售后', '订单任务', 'orderEveryDayTaskExecute', 'executeCloseAfterSale', '0 0 4 * * ?', 1, '买家申请售后超时未处理或未退货自动关闭'),
('ORDER_CLOSE_COMPLAINT', '自动关闭超时投诉', '订单任务', 'orderEveryDayTaskExecute', 'executeCloseComplaint', '0 0 4 * * ?', 1, '交易投诉超时未响应自动关闭'),
('ORDER_PROFIT_SHARING', '订单分账与解冻', '订单任务', 'orderEveryDayTaskExecute', 'executeProfitSharing', '0 0 5 * * ?', 1, '订单结算周期完成后解冻佣金与分账'),
('ORDER_CREATE_BILL', '生成店铺结算单', '订单任务', 'orderEveryDayTaskExecute', 'executeCreateBill', '0 0 6 * * ?', 1, '按店铺结算周期生成每日或每月对账结算单'),

-- 订单与预约独立任务
('CancelOrderTaskExecute', '订单超时自动取消', '订单任务', 'cancelOrderTaskExecute', 'execute', '0 0/5 * * * ?', 1, '未付款订单超时后自动取消并释放库存'),
('RechargeOrderTaskExecute', '充值订单超时取消', '订单任务', 'rechargeOrderTaskExecute', 'execute', '0 0/10 * * * ?', 1, '充值未付款订单超时后关闭'),
('AppointmentMinuteTaskExecute', '预约超时核销与提醒', '预约任务', 'appointmentMinuteTaskExecute', 'execute', '0 0/5 * * * ?', 1, '预约服务未到店核销自动处理及临期服务微信提醒'),

-- 营销活动任务
('CouponExecute', '优惠券状态更新', '营销任务', 'couponExecute', 'execute', '0 0/30 * * * ?', 1, '失效或过期优惠券自动标记为已过期'),
('GiftCardCashExpireExecute', '礼品卡现金过期处理', '营销任务', 'giftCardCashExpireExecute', 'execute', '0 0 0 * * ?', 1, '礼品卡绑定的余额过期处理'),
('PromotionEverydayExecute', '促销活动每日更新', '营销任务', 'promotionEverydayExecute', 'execute', '0 0 0 * * ?', 1, '每日秒杀、拼团、满减等活动的开始与结束状态流转'),
('MemberCouponSignEverydayExecute', '会员优惠券每日签到', '营销任务', 'memberCouponSignEverydayExecute', 'execute', '0 0 0 * * ?', 1, '会员每日签到领券等定时触发'),

-- 商品与店铺任务
('GoodsExecute', '商品定时上下架', '商品任务', 'goodsExecute', 'execute', '0 0/10 * * * ?', 1, '定时上架或达到下架时间的商品状态流转'),
('HotWordsEveryDayTaskExecute', '搜索热词每日更新', '商品任务', 'hotWordsEveryDayTaskExecute', 'execute', '0 0 1 * * ?', 1, '统计搜索频次更新热门搜索关键词'),
('StoreExecute', '店铺状态处理', '店铺任务', 'storeExecute', 'execute', '0 0/30 * * * ?', 1, '店铺过期或年费到期状态处理'),
('StoreRatingExecute', '店铺评分统计更新', '店铺任务', 'storeRatingExecute', 'execute', '0 0 2 * * ?', 1, '根据近期用户评价重新计算店铺综合评分'),

-- 系统与分销任务
('BroadcastExecute', '小程序直播状态同步', '系统任务', 'broadcastExecute', 'execute', '0 0/30 * * * ?', 1, '同步微信小程序直播间数据和回放状态'),
('DistributionRelationExpireExecute', '分销绑定关系过期', '分销任务', 'distributionRelationExpireExecute', 'execute', '0 0 2 * * ?', 1, '超过有效期的分销关系自动解绑'),

-- 统计报表任务
('OnlineMemberStatistics', '在线会员统计', '统计任务', 'onlineMemberStatistics', 'execute', '0 0/10 * * * ?', 1, '统计当前在线用户数与会话情况'),
('PageViewStatisticsExecute', '页面访问PV/UV统计', '统计任务', 'pageViewStatisticsExecute', 'execute', '0 0 * * * ?', 1, '按小时归档店铺与平台页面访问量'),
('MemberStatisticsExecute', '会员活跃与增长统计', '统计任务', 'memberStatisticsExecute', 'execute', '0 0 1 * * ?', 1, '每日归档会员总数、新增会员与活跃情况')
ON DUPLICATE KEY UPDATE 
  `task_name` = VALUES(`task_name`),
  `group_name` = VALUES(`group_name`),
  `bean_name` = VALUES(`bean_name`),
  `method_name` = VALUES(`method_name`),
  `description` = VALUES(`description`);
