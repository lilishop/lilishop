-- 平台后台独立的定时上架菜单；重复执行不会重复创建。
-- @author Bulbasaur
-- @since 2026-11-03
-- 仅继承已有平台商品菜单的角色范围，不授予其他角色访问权限。

START TRANSACTION;

INSERT INTO li_menu
    (id, create_by, create_time, delete_flag, update_by, update_time,
     description, front_route, icon, level, name, parent_id, path, sort_order,
     title, front_component, permission)
SELECT 2121548191760000000, 'Bulbasaur', NOW(), b'0', 'Bulbasaur', NOW(),
       '商品计划上架列表', 'goods/scheduled-upper/index', 'md-time', source.level,
       'goods-scheduled-upper', source.parent_id, 'goods-scheduled-upper',
       COALESCE(source.sort_order, 0) + 0.5, '定时上架', source.front_component, source.permission
FROM li_menu source
WHERE source.name = 'managerGoods' AND source.delete_flag = b'0'
  AND NOT EXISTS (SELECT 1 FROM li_menu target WHERE target.name = 'goods-scheduled-upper')
LIMIT 1;

INSERT INTO li_role_menu
    (id, create_by, create_time, delete_flag, update_by, update_time, is_super, menu_id, role_id)
SELECT UUID_SHORT(), 'Bulbasaur', NOW(), b'0', 'Bulbasaur', NOW(), bindings.is_super,
       CAST(scheduled.id AS CHAR), bindings.role_id
FROM li_role_menu bindings
INNER JOIN li_menu source ON CAST(source.id AS CHAR) = bindings.menu_id
INNER JOIN li_menu scheduled ON scheduled.name = 'goods-scheduled-upper' AND scheduled.delete_flag = b'0'
WHERE source.name = 'managerGoods' AND source.delete_flag = b'0' AND bindings.delete_flag = b'0'
  AND NOT EXISTS (
      SELECT 1 FROM li_role_menu existing
      WHERE existing.menu_id = CAST(scheduled.id AS CHAR)
        AND existing.role_id = bindings.role_id AND existing.delete_flag = b'0'
  );

COMMIT;
