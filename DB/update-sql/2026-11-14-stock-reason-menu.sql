-- 平台后台：在「商品 -> 商品管理」下补充出入库原因维护入口。
-- @author Bulbasaur
-- @since 2026-11-14
-- 仅新增缺失菜单，不修改已有菜单、原因数据或 li_role_menu 角色授权。
-- 同名或同组件菜单（含已删除记录）存在时跳过，保留历史配置，由管理员核实。
-- 执行后超级管理员重新登录查看；普通角色需在角色管理中明确授权。

START TRANSACTION;

INSERT INTO li_menu
    (id, create_by, create_time, delete_flag, update_by, update_time,
     description, front_route, level, name, parent_id, path, sort_order,
     title, permission)
SELECT UUID_SHORT(), 'Bulbasaur', NOW(), b'0', 'Bulbasaur', NOW(),
       '统一维护入库、出库及报损原因', 'procurement/reason/reasonManage',
       2, 'stockReasonManage', CAST(parent.id AS CHAR), 'stockReasonManage',
       10, '出入库原因', '/manager/procurement/reason*'
FROM li_menu parent
WHERE parent.name = 'goodsManager' AND parent.level = 1 AND parent.delete_flag = b'0'
  AND NOT EXISTS (
      SELECT 1 FROM li_menu existing
      WHERE existing.name = 'stockReasonManage'
         OR existing.front_route = 'procurement/reason/reasonManage'
  )
ORDER BY parent.id
LIMIT 1;

COMMIT;

-- 验证菜单及父级；无结果时先确认 goodsManager 分组存在，勿直接扩大角色授权。
SELECT page.id, page.title, page.name, page.front_route, page.delete_flag,
       parent.title AS parent_title, page.permission
FROM li_menu page
LEFT JOIN li_menu parent ON page.parent_id = CAST(parent.id AS CHAR)
WHERE page.name = 'stockReasonManage'
   OR page.front_route = 'procurement/reason/reasonManage';
