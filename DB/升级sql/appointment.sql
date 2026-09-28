-- =============================================================================
-- 预约服务升级脚本
--
-- 做两件事，均可重复执行：
--   1. 创建预约相关表（商品设置、时段库存、订单、工单、核销码、服务人员、表单）
--   2. 写入商家后台菜单
--
-- 菜单写入后需要重新登录商家后台才会看到新菜单。
-- =============================================================================

-- -----------------------------------------------------------------------------
-- 1. 预约服务表
--    商品配置在 SPU 扩展表，可约时段按 SKU 模板生成，下单后扣日历库存，
--    支付成功生成工单和核销码。
-- -----------------------------------------------------------------------------

-- 预约商品扩展：一个商品一条。记录服务方式、可约日期、提前预约、取消规则、
-- 关联表单、服务时长和时段划分方式。
CREATE TABLE IF NOT EXISTS `li_appointment_goods_ext` (
  `id`                      varchar(32)   NOT NULL COMMENT '主键',
  `create_by`               varchar(64)   DEFAULT NULL COMMENT '创建者',
  `create_time`             datetime      DEFAULT NULL COMMENT '创建时间',
  `update_by`               varchar(64)   DEFAULT NULL COMMENT '更新者',
  `update_time`             datetime      DEFAULT NULL COMMENT '更新时间',
  `delete_flag`             bit(1)        DEFAULT b'0' COMMENT '删除标志',
  `goods_id`                varchar(32)   NOT NULL COMMENT '商品 SPU ID',
  `store_id`                varchar(32)   NOT NULL COMMENT '店铺 ID',
  `service_modes`           varchar(64)   NOT NULL COMMENT '服务模式 IN_STORE,ON_SITE',
  `show_remaining_qty`      bit(1)        NOT NULL DEFAULT b'1' COMMENT '是否展示剩余可约数量',
  `bookable_date_mode`      varchar(32)   NOT NULL DEFAULT 'EVERY_DAY' COMMENT '可预约日期模式 EVERY_DAY/CUSTOM',
  `custom_date_start`       date          DEFAULT NULL COMMENT '自定义可约起始日',
  `custom_date_end`         date          DEFAULT NULL COMMENT '自定义可约结束日',
  `custom_weekdays`         varchar(32)   DEFAULT NULL COMMENT '自定义周几 1-7 逗号分隔',
  `booking_date_range_days` int           NOT NULL DEFAULT 1 COMMENT '预约日期范围天数',
  `advance_booking_mode`    varchar(32)   NOT NULL DEFAULT 'NONE' COMMENT '提前预约 NONE/HOURS',
  `advance_booking_hours`   int           DEFAULT NULL COMMENT '须提前小时数',
  `cancel_mode`             varchar(64)   NOT NULL COMMENT '取消规则快照模式',
  `cancel_hours`            int           DEFAULT NULL COMMENT '取消规则小时数',
  `form_submit_mode`        varchar(32)   NOT NULL DEFAULT 'PER_APPOINTMENT' COMMENT '表单提交粒度',
  `system_form_id`          varchar(32)   DEFAULT NULL COMMENT '关联系统表单 ID',
  `system_form_snapshot`    text          COMMENT '关联表单快照 JSON',
  `service_duration_minutes` int          NOT NULL COMMENT '服务时长分钟（商品级）',
  `slot_division_mode`      varchar(32)   NOT NULL DEFAULT 'AUTO' COMMENT '时段划分 AUTO/CUSTOM',
  `auto_start_time`         varchar(8)    DEFAULT NULL COMMENT '自动划分开始 HH:mm',
  `auto_end_time`           varchar(8)    DEFAULT NULL COMMENT '自动划分结束 HH:mm',
  PRIMARY KEY (`id`),
  UNIQUE KEY `uk_goods_id` (`goods_id`),
  KEY `idx_store_id` (`store_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='预约商品扩展设置';

-- 预约时段模板：按 SKU 保存一天里可约的时间段、容量和排序。
-- AUTO 划分时由服务时长自动切段后写入；CUSTOM 时由商家手工配置。
CREATE TABLE IF NOT EXISTS `li_appointment_slot_template` (
  `id`                varchar(32)  NOT NULL COMMENT '主键',
  `create_by`         varchar(64)  DEFAULT NULL COMMENT '创建者',
  `create_time`       datetime     DEFAULT NULL COMMENT '创建时间',
  `update_by`         varchar(64)  DEFAULT NULL COMMENT '更新者',
  `update_time`       datetime     DEFAULT NULL COMMENT '更新时间',
  `delete_flag`       bit(1)       DEFAULT b'0' COMMENT '删除标志',
  `goods_id`          varchar(32)  NOT NULL COMMENT '商品 SPU ID',
  `sku_id`            varchar(32)  NOT NULL COMMENT 'SKU ID',
  `store_id`          varchar(32)  NOT NULL COMMENT '店铺 ID',
  `slot_start_time`   varchar(8)   NOT NULL COMMENT '时段开始 HH:mm',
  `slot_end_time`     varchar(8)   NOT NULL COMMENT '时段结束 HH:mm',
  `enabled`           bit(1)       NOT NULL DEFAULT b'1' COMMENT '是否启用',
  `capacity`          int          NOT NULL DEFAULT 0 COMMENT '可约数量 0=不限',
  `sort_order`        int          NOT NULL DEFAULT 0 COMMENT '排序',
  PRIMARY KEY (`id`),
  KEY `idx_goods_sku` (`goods_id`, `sku_id`),
  KEY `idx_store_goods` (`store_id`, `goods_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='预约时段模板';

-- 预约日历库存：某个 SKU 在某一天、某一时段的剩余可约数量。
-- available_qty 是扣减和回补的权威字段，version 用于乐观锁。
CREATE TABLE IF NOT EXISTS `li_appointment_slot_stock` (
  `id`                varchar(32)  NOT NULL COMMENT '主键',
  `create_by`         varchar(64)  DEFAULT NULL COMMENT '创建者',
  `create_time`       datetime     DEFAULT NULL COMMENT '创建时间',
  `update_by`         varchar(64)  DEFAULT NULL COMMENT '更新者',
  `update_time`       datetime     DEFAULT NULL COMMENT '更新时间',
  `delete_flag`       bit(1)       DEFAULT b'0' COMMENT '删除标志',
  `goods_id`          varchar(32)  NOT NULL COMMENT '商品 SPU ID',
  `sku_id`            varchar(32)  NOT NULL COMMENT 'SKU ID',
  `store_id`          varchar(32)  NOT NULL COMMENT '店铺 ID',
  `slot_date`         date         NOT NULL COMMENT '预约日期',
  `slot_start_time`   varchar(8)   NOT NULL COMMENT '时段开始 HH:mm',
  `slot_end_time`     varchar(8)   NOT NULL COMMENT '时段结束 HH:mm',
  `capacity`          int          NOT NULL DEFAULT 0 COMMENT '可约数量 0=不限',
  `available_qty`     int          NOT NULL DEFAULT 0 COMMENT '剩余可约（权威）',
  `version`           int          NOT NULL DEFAULT 0 COMMENT '乐观锁',
  PRIMARY KEY (`id`),
  UNIQUE KEY `uk_slot` (`sku_id`, `slot_date`, `slot_start_time`, `slot_end_time`),
  KEY `idx_store_date` (`store_id`, `slot_date`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='预约时段日历库存';

-- 预约订单扩展：一单一条。保存所选时段、联系人、上门地址、表单答案、
-- 下单时的取消规则快照，以及累计已退实付金额。
CREATE TABLE IF NOT EXISTS `li_appointment_order_ext` (
  `id`                        varchar(32)  NOT NULL COMMENT '主键',
  `create_by`                 varchar(64)  DEFAULT NULL COMMENT '创建者',
  `create_time`               datetime     DEFAULT NULL COMMENT '创建时间',
  `update_by`                 varchar(64)  DEFAULT NULL COMMENT '更新者',
  `update_time`               datetime     DEFAULT NULL COMMENT '更新时间',
  `delete_flag`               bit(1)       DEFAULT b'0' COMMENT '删除标志',
  `order_sn`                  varchar(64)  NOT NULL COMMENT '订单号',
  `goods_id`                  varchar(32)  NOT NULL COMMENT '商品 SPU ID',
  `sku_id`                    varchar(32)  NOT NULL COMMENT 'SKU ID',
  `store_id`                  varchar(32)  NOT NULL COMMENT '店铺 ID',
  `member_id`                 varchar(32)  NOT NULL COMMENT '会员 ID',
  `service_mode`              varchar(32)  NOT NULL COMMENT 'IN_STORE/ON_SITE',
  `slot_stock_id`             varchar(32)  NOT NULL COMMENT '时段库存 ID',
  `slot_date`                 date         NOT NULL COMMENT '预约日期',
  `slot_start_time`           varchar(8)   NOT NULL COMMENT '时段开始 HH:mm',
  `slot_end_time`             varchar(8)   NOT NULL COMMENT '时段结束 HH:mm',
  `quantity`                  int          NOT NULL DEFAULT 1 COMMENT '购买份数 N',
  `contact_name`              varchar(64)  DEFAULT NULL COMMENT '联系人',
  `contact_phone`             varchar(32)  DEFAULT NULL COMMENT '联系电话',
  `service_address_snapshot`  text         COMMENT '上门服务地址快照 JSON',
  `form_answer_json`          text         COMMENT '表单答案 JSON',
  `cancel_policy_snapshot`    varchar(512) DEFAULT NULL COMMENT '取消规则快照',
  `work_order_visible_snapshot` bit(1)     DEFAULT b'1' COMMENT '工单展示快照',
  `refunded_amount`           decimal(10,2) NOT NULL DEFAULT 0 COMMENT '累计已退实付金额',
  PRIMARY KEY (`id`),
  UNIQUE KEY `uk_order_sn` (`order_sn`),
  KEY `idx_store_slot_date` (`store_id`, `slot_date`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='预约订单扩展';

-- 预约服务人员：店铺可派工的人员，绑定店员账号。上门接单、到店核销时选用。
CREATE TABLE IF NOT EXISTS `li_appointment_service_staff` (
  `id`              varchar(32)  NOT NULL COMMENT '主键',
  `create_by`       varchar(64)  DEFAULT NULL COMMENT '创建者',
  `create_time`     datetime     DEFAULT NULL COMMENT '创建时间',
  `update_by`       varchar(64)  DEFAULT NULL COMMENT '更新者',
  `update_time`     datetime     DEFAULT NULL COMMENT '更新时间',
  `delete_flag`     bit(1)       DEFAULT b'0' COMMENT '删除标志',
  `store_id`        varchar(32)  NOT NULL COMMENT '店铺 ID',
  `clerk_user_id`   varchar(32)  NOT NULL COMMENT '店员用户 ID',
  `staff_name`      varchar(64)  NOT NULL COMMENT '人员姓名',
  `id_photo_url`    varchar(512) DEFAULT NULL COMMENT '证件照',
  `contact_phone`   varchar(32)  DEFAULT NULL COMMENT '联系电话',
  `sort_order`      int          NOT NULL DEFAULT 0 COMMENT '排序',
  `status`          varchar(32)  NOT NULL DEFAULT 'ENABLED' COMMENT 'ENABLED/DISABLED',
  PRIMARY KEY (`id`),
  UNIQUE KEY `uk_store_clerk` (`store_id`, `clerk_user_id`),
  KEY `idx_store_status` (`store_id`, `status`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='预约服务人员';

-- 预约工单：与订单 1:1。记录服务状态、预约时间、预约人、派工人员。
-- 状态：待领取 / 已领取 / 待核销 / 服务中 / 服务结束 / 已退款。
CREATE TABLE IF NOT EXISTS `li_appointment_work_order` (
  `id`                  varchar(32)  NOT NULL COMMENT '主键',
  `create_by`           varchar(64)  DEFAULT NULL COMMENT '创建者',
  `create_time`         datetime     DEFAULT NULL COMMENT '创建时间',
  `update_by`           varchar(64)  DEFAULT NULL COMMENT '更新者',
  `update_time`         datetime     DEFAULT NULL COMMENT '更新时间',
  `delete_flag`         bit(1)       DEFAULT b'0' COMMENT '删除标志',
  `work_order_sn`       varchar(64)  NOT NULL COMMENT '工单号',
  `order_sn`            varchar(64)  NOT NULL COMMENT '订单号 1:1',
  `member_id`           varchar(32)  NOT NULL COMMENT '会员 ID',
  `store_id`            varchar(32)  NOT NULL COMMENT '店铺 ID',
  `service_mode`        varchar(32)  NOT NULL COMMENT 'IN_STORE/ON_SITE',
  `service_status`      varchar(32)  NOT NULL COMMENT 'PENDING_CLAIM/CLAIMED/WAIT_VERIFY/IN_SERVICE/SERVICE_ENDED/REFUNDED',
  `appointment_time`    datetime     NOT NULL COMMENT '预约时间快照',
  `booker_name`         varchar(64)  DEFAULT NULL COMMENT '预约人姓名',
  `booker_phone`        varchar(32)  DEFAULT NULL COMMENT '预约人电话',
  `appointment_address` varchar(512) DEFAULT NULL COMMENT '预约地址',
  `staff_id`            varchar(32)  DEFAULT NULL COMMENT '服务人员 ID',
  `remark`              varchar(512) DEFAULT NULL COMMENT '备注',
  PRIMARY KEY (`id`),
  UNIQUE KEY `uk_order_sn` (`order_sn`),
  UNIQUE KEY `uk_work_order_sn` (`work_order_sn`),
  KEY `idx_store_status` (`store_id`, `service_status`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='预约服务工单';

-- 核销码：一份购买数量对应一条码（序号 1..N）。同一店铺内码唯一。
-- 状态：未使用 / 已使用 / 已作废。
CREATE TABLE IF NOT EXISTS `li_appointment_verification_code` (
  `id`                  varchar(32)  NOT NULL COMMENT '主键',
  `create_by`           varchar(64)  DEFAULT NULL COMMENT '创建者',
  `create_time`         datetime     DEFAULT NULL COMMENT '创建时间',
  `update_by`           varchar(64)  DEFAULT NULL COMMENT '更新者',
  `update_time`         datetime     DEFAULT NULL COMMENT '更新时间',
  `delete_flag`         bit(1)       DEFAULT b'0' COMMENT '删除标志',
  `order_sn`            varchar(64)  NOT NULL COMMENT '订单号',
  `order_item_sn`       varchar(64)  DEFAULT NULL COMMENT '订单项编号',
  `store_id`            varchar(32)  NOT NULL COMMENT '店铺 ID',
  `member_id`           varchar(32)  NOT NULL COMMENT '会员 ID',
  `code`                varchar(16)  NOT NULL COMMENT '核销码',
  `seq_no`              int          NOT NULL DEFAULT 1 COMMENT '序号 1..N',
  `status`              varchar(32)  NOT NULL DEFAULT 'UNUSED' COMMENT 'UNUSED/USED/VOIDED',
  `verified_time`       datetime     DEFAULT NULL COMMENT '核销时间',
  `verified_by_user_id` varchar(32)  DEFAULT NULL COMMENT '核销人用户 ID',
  `staff_id`            varchar(32)  DEFAULT NULL COMMENT '核销服务人员 ID',
  PRIMARY KEY (`id`),
  UNIQUE KEY `uk_store_code` (`store_id`, `code`),
  KEY `idx_order_sn` (`order_sn`),
  KEY `idx_order_status` (`order_sn`, `status`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='预约核销码（每单 N 条）';

-- 店铺预约配置：到店、上门各一条。控制工单是否对买家展示、上门是否接单。
CREATE TABLE IF NOT EXISTS `li_appointment_store_config` (
  `id`                              varchar(32)  NOT NULL COMMENT '主键',
  `create_by`                       varchar(64)  DEFAULT NULL COMMENT '创建者',
  `create_time`                     datetime     DEFAULT NULL COMMENT '创建时间',
  `update_by`                       varchar(64)  DEFAULT NULL COMMENT '更新者',
  `update_time`                     datetime     DEFAULT NULL COMMENT '更新时间',
  `delete_flag`                     bit(1)       DEFAULT b'0' COMMENT '删除标志',
  `store_id`                        varchar(32)  NOT NULL COMMENT '店铺 ID',
  `service_mode`                    varchar(32)  NOT NULL COMMENT 'IN_STORE/ON_SITE',
  `work_order_visible`              bit(1)       NOT NULL DEFAULT b'1' COMMENT '到店工单展示',
  `accept_order_enabled`            bit(1)       NOT NULL DEFAULT b'1' COMMENT '上门接单开关',
  `config_json`                     text         COMMENT '扩展配置 JSON',
  PRIMARY KEY (`id`),
  UNIQUE KEY `uk_store_mode` (`store_id`, `service_mode`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='预约店铺配置';

-- 系统表单模板：店铺自定义预约时要填的字段，商品设置里引用后快照到订单。
CREATE TABLE IF NOT EXISTS `li_appointment_system_form` (
  `id`          varchar(32)  NOT NULL COMMENT '主键',
  `create_by`   varchar(64)  DEFAULT NULL COMMENT '创建者',
  `create_time` datetime     DEFAULT NULL COMMENT '创建时间',
  `update_by`   varchar(64)  DEFAULT NULL COMMENT '更新者',
  `update_time` datetime     DEFAULT NULL COMMENT '更新时间',
  `delete_flag` bit(1)       DEFAULT b'0' COMMENT '删除标志',
  `store_id`    varchar(32)  NOT NULL COMMENT '店铺 ID',
  `form_name`   varchar(128) NOT NULL COMMENT '表单名称',
  `form_schema` text         NOT NULL COMMENT '表单字段定义 JSON',
  `status`      varchar(32)  NOT NULL DEFAULT 'ENABLED' COMMENT 'ENABLED/DISABLED',
  PRIMARY KEY (`id`),
  KEY `idx_store_status` (`store_id`, `status`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='预约系统表单模板';

-- -----------------------------------------------------------------------------
-- 2. 商家菜单
--    商品 → 商品设置 → 到店服务、上门服务
--    订单 → 订单管理 → 订单核销、工单管理
--    设置 → 店员设置 → 服务人员（紧挨店员管理）
-- -----------------------------------------------------------------------------

-- 旧菜单 appointment-settings 改名为「商品设置」，挂到一级「商品」下，排序 7.0。
UPDATE li_store_menu moved
INNER JOIN li_store_menu goods
    ON goods.delete_flag = 0
   AND goods.level = 0
   AND goods.name = 'goods'
SET moved.title = '商品设置',
    moved.name = 'goods-settings',
    moved.path = 'goods-settings',
    moved.parent_id = goods.id,
    moved.sort_order = '7.0',
    moved.update_by = 'system',
    moved.update_time = NOW()
WHERE moved.delete_flag = 0
  AND moved.name = 'appointment-settings';

-- 没有旧菜单可改名时，直接插入「商品设置」目录（front_route=Main，仅作分组）。
INSERT INTO li_store_menu (
    id, title, name, path, level, front_route, parent_id, sort_order, permission,
    create_by, create_time, update_by, update_time, delete_flag
)
SELECT
    CAST(UNIX_TIMESTAMP(NOW(3)) * 1000 AS UNSIGNED) * 1000 + 41,
    '商品设置',
    'goods-settings',
    'goods-settings',
    1,
    'Main',
    goods.id,
    '7.0',
    '',
    'system', NOW(), 'system', NOW(), 0
FROM li_store_menu goods
WHERE goods.delete_flag = 0
  AND goods.level = 0
  AND goods.name = 'goods'
  AND NOT EXISTS (
      SELECT 1 FROM li_store_menu m
      WHERE m.name = 'goods-settings' AND m.delete_flag = 0
  )
LIMIT 1;

-- 「商品设置」下新增「到店服务」页面。
INSERT INTO li_store_menu (
    id, title, name, path, level, front_route, parent_id, sort_order, permission,
    create_by, create_time, update_by, update_time, delete_flag
)
SELECT
    CAST(UNIX_TIMESTAMP(NOW(3)) * 1000 AS UNSIGNED) * 1000 + 42,
    '到店服务',
    'appointment-in-store-settings',
    'appointment-in-store-settings',
    2,
    'appointment/inStoreSettings',
    parent.id,
    '7.1',
    '/store/appointment/settings/in-store*',
    'system', NOW(), 'system', NOW(), 0
FROM li_store_menu parent
WHERE parent.delete_flag = 0
  AND parent.name = 'goods-settings'
  AND NOT EXISTS (
      SELECT 1 FROM li_store_menu m
      WHERE m.name = 'appointment-in-store-settings' AND m.delete_flag = 0
  )
LIMIT 1;

-- 「商品设置」下新增「上门服务」页面。
INSERT INTO li_store_menu (
    id, title, name, path, level, front_route, parent_id, sort_order, permission,
    create_by, create_time, update_by, update_time, delete_flag
)
SELECT
    CAST(UNIX_TIMESTAMP(NOW(3)) * 1000 AS UNSIGNED) * 1000 + 43,
    '上门服务',
    'appointment-on-site-settings',
    'appointment-on-site-settings',
    2,
    'appointment/onSiteSettings',
    parent.id,
    '7.2',
    '/store/appointment/settings/on-site*',
    'system', NOW(), 'system', NOW(), 0
FROM li_store_menu parent
WHERE parent.delete_flag = 0
  AND parent.name = 'goods-settings'
  AND NOT EXISTS (
      SELECT 1 FROM li_store_menu m
      WHERE m.name = 'appointment-on-site-settings' AND m.delete_flag = 0
  )
LIMIT 1;

-- 到店、上门两个菜单若已存在（例如上次挂错父级），纠正父级和排序。
UPDATE li_store_menu child
INNER JOIN li_store_menu parent
    ON parent.delete_flag = 0
   AND parent.name = 'goods-settings'
SET child.parent_id = parent.id,
    child.sort_order = IF(child.name = 'appointment-in-store-settings', '7.1', '7.2'),
    child.update_by = 'system',
    child.update_time = NOW()
WHERE child.delete_flag = 0
  AND child.name IN ('appointment-in-store-settings', 'appointment-on-site-settings');

-- 「订单管理」下、与订单列表同级，新增「订单核销」。
INSERT INTO li_store_menu (
    id, title, name, path, level, front_route, parent_id, sort_order, permission,
    create_by, create_time, update_by, update_time, delete_flag
)
SELECT
    CAST(UNIX_TIMESTAMP(NOW(3)) * 1000 AS UNSIGNED) * 1000 + 23,
    '订单核销',
    'orderVerification',
    'orderVerification',
    2,
    'order/order/orderVerification',
    order_list.parent_id,
    '3',
    '/store/order/order/verification/**',
    'system', NOW(), 'system', NOW(), 0
FROM li_store_menu order_list
WHERE order_list.delete_flag = 0
  AND order_list.name = 'orderList'
  AND NOT EXISTS (
      SELECT 1 FROM li_store_menu m
      WHERE m.name = 'orderVerification' AND m.delete_flag = 0
  )
LIMIT 1;

-- 「订单管理」下、与订单列表同级，新增「工单管理」。
INSERT INTO li_store_menu (
    id, title, name, path, level, front_route, parent_id, sort_order, permission,
    create_by, create_time, update_by, update_time, delete_flag
)
SELECT
    CAST(UNIX_TIMESTAMP(NOW(3)) * 1000 AS UNSIGNED) * 1000 + 31,
    '工单管理',
    'appointment-work-orders',
    'appointment-work-orders',
    2,
    'appointment/workOrderList',
    order_list.parent_id,
    '2.12',
    '/store/appointment/work-orders*',
    'system', NOW(), 'system', NOW(), 0
FROM li_store_menu order_list
WHERE order_list.delete_flag = 0
  AND order_list.name = 'orderList'
  AND NOT EXISTS (
      SELECT 1 FROM li_store_menu m
      WHERE m.name = 'appointment-work-orders' AND m.delete_flag = 0
  )
LIMIT 1;

-- 「店员管理」同级新增「服务人员」，排序紧挨店员管理（6.32）。
INSERT INTO li_store_menu (
    id, title, name, path, level, front_route, parent_id, sort_order, permission,
    create_by, create_time, update_by, update_time, delete_flag
)
SELECT
    CAST(UNIX_TIMESTAMP(NOW(3)) * 1000 AS UNSIGNED) * 1000 + 51,
    '服务人员',
    'appointment-service-staff',
    'appointment-service-staff',
    2,
    'appointment/serviceStaffList',
    clerk.parent_id,
    '6.32',
    '/store/appointment/service-staff*',
    'system', NOW(), 'system', NOW(), 0
FROM li_store_menu clerk
WHERE clerk.delete_flag = 0
  AND clerk.front_route = 'shop/system/clerk/clerkManage'
  AND NOT EXISTS (
      SELECT 1 FROM li_store_menu m
      WHERE m.name = 'appointment-service-staff' AND m.delete_flag = 0
  )
LIMIT 1;
