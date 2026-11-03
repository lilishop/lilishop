-- 商家后台独立的定时上架菜单；可重复执行。
-- 仅继承商品列表已有的店铺与角色权限范围。
-- @author Bulbasaur
-- @since 2026-11-03
START TRANSACTION;

INSERT INTO li_store_menu
    (id, create_by, create_time, delete_flag, update_by, update_time,
     description, front_route, icon, level, name, parent_id, path, sort_order, title, permission)
SELECT 2121548191760000001, 'Bulbasaur', NOW(), b'0', 'Bulbasaur', NOW(),
       '商品计划上架列表', 'goods/scheduled-upper/index', 'md-time', source.level,
       'goods-scheduled-upper', source.parent_id, 'goods-scheduled-upper',
       CAST(COALESCE(source.sort_order, '0') AS DECIMAL(10, 3)) + 0.005, '定时上架', source.permission
FROM li_store_menu source
WHERE source.name = 'goods' AND source.front_route = 'goods/goods-seller/goods' AND source.delete_flag = b'0'
  AND NOT EXISTS (SELECT 1 FROM li_store_menu target WHERE target.name = 'goods-scheduled-upper')
LIMIT 1;

INSERT INTO li_store_menu_role
    (id, create_by, create_time, delete_flag, update_by, update_time, role_id, menu_id, store_id, is_super)
SELECT UUID_SHORT(), 'Bulbasaur', NOW(), b'0', 'Bulbasaur', NOW(), bindings.role_id,
       CAST(scheduled.id AS CHAR), bindings.store_id, bindings.is_super
FROM li_store_menu_role bindings
INNER JOIN li_store_menu source ON CAST(source.id AS CHAR) = bindings.menu_id
INNER JOIN li_store_menu scheduled ON scheduled.name = 'goods-scheduled-upper' AND scheduled.delete_flag = b'0'
WHERE source.name = 'goods' AND source.front_route = 'goods/goods-seller/goods'
  AND source.delete_flag = b'0' AND bindings.delete_flag = b'0'
  AND NOT EXISTS (
      SELECT 1 FROM li_store_menu_role existing
      WHERE existing.menu_id = CAST(scheduled.id AS CHAR) AND existing.role_id = bindings.role_id
        AND existing.store_id <=> bindings.store_id AND existing.delete_flag = b'0'
  );

COMMIT;
