-- =============================================================================
-- 商品参数倒排表 li_goods_search_param
-- 作者：Mike
-- 创建：2026-09-03
-- =============================================================================
--
-- 搜索侧参数筛选与参数 selector 的唯一数据来源。纯派生数据，物理删除，不带审计列。
--
-- 【类型绑定】goods_id 的类型与排序规则必须与 li_goods_sku.goods_id 保持完全一致
-- （当前为 varchar(255) utf8mb3_general_ci）。查询条件恒为 p.goods_id = gs.goods_id：
--   * 改成任何 utf8mb4 排序规则 -> 两侧同为列引用且排序规则冲突，MySQL 报 1267，查询无法执行；
--   * 改成 bigint -> 与 varchar 比较转 DOUBLE，idx_..._lookup 的 goods_id 段失效。
-- 因此若将来迁移 li_goods_sku.goods_id 的类型，必须在同一次变更里迁移本列。
--
-- 新环境 / 已有库：执行本文件即可（CREATE TABLE IF NOT EXISTS，可重复执行）。
-- =============================================================================

-- -----------------------------------------------------------------------------
-- 一、建表
-- -----------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS `li_goods_search_param` (
  `id` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NOT NULL COMMENT 'ID',
  `goods_id` varchar(255) CHARACTER SET utf8mb3 COLLATE utf8mb3_general_ci NOT NULL COMMENT '商品ID，与 li_goods_sku.goods_id 同型同排序规则',
  `param_name` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NOT NULL COMMENT '参数名',
  `param_value` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NOT NULL COMMENT '参数值',
  `param_sort` int NULL COMMENT '参数维度的排序，同一 (goods_id, param_name) 的各行取值相同（冗余列）',
  PRIMARY KEY (`id`) USING BTREE,
  UNIQUE KEY `uk_li_goods_search_param_goods_name_value` (`goods_id`,`param_name`,`param_value`) USING BTREE,
  KEY `idx_li_goods_search_param_lookup` (`param_name`,`param_value`,`goods_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin ROW_FORMAT=DYNAMIC COMMENT='商品参数倒排（搜索用）';
