-- 平台营销下新增独立“抽奖活动”菜单，与积分商品解耦。
-- @author Bulbasaur
-- @since 2026-11-15
-- 仅新增缺失菜单，保留已有同名/同组件菜单（含已删除记录），不写入li_role_menu。
-- 无DDL；不修改抽奖业务表和积分商品菜单。首次安装及已执行抽奖业务表SQL的环境均需执行。
-- 执行后超级管理员重新登录；普通角色需在角色管理明确分配抽奖活动查询/管理权限。

START TRANSACTION;

INSERT INTO li_menu
    (id, create_by, create_time, delete_flag, update_by, update_time,
     description, front_route, icon, level, name, parent_id, path, sort_order,
     title, front_component, permission)
SELECT UUID_SHORT(), 'Bulbasaur', NOW(), b'0', 'Bulbasaur', NOW(),
       '平台抽奖活动、奖池配置及抽奖记录', 'promotions/lottery/LotteryManage', source.icon, source.level,
       'manager-lottery', source.parent_id, 'lottery',
       CAST(COALESCE(source.sort_order, 0) AS DECIMAL(10, 3)) + 0.1,
       '抽奖活动', source.front_component, '/manager/promotion/lottery*'
FROM li_menu source
WHERE (source.front_route = 'promotions/full-discount/full-discount' OR source.name IN ('full-cut-activity', 'full-discount'))
  AND source.delete_flag = b'0'
  AND NOT EXISTS (
      SELECT 1 FROM li_menu existing
      WHERE existing.name = 'manager-lottery'
         OR existing.front_route = 'promotions/lottery/LotteryManage'
  )
ORDER BY source.id
LIMIT 1;

COMMIT;

-- 验证菜单父级、组件路径与独立权限；无结果时核对现有营销分组，不自动创建额外分组或授权。
SELECT page.id, page.title, page.name, page.front_route, page.permission, page.delete_flag,
       parent.title AS parent_title
FROM li_menu page
LEFT JOIN li_menu parent ON page.parent_id = CAST(parent.id AS CHAR)
WHERE page.name = 'manager-lottery' OR page.front_route = 'promotions/lottery/LotteryManage';
