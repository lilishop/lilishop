-- 移除已下线的限时直降、第 N 件优惠菜单及角色关联。
-- 保留历史活动、订单和财务数据；可重复执行。
-- @author Bulbasaur
-- @since 2026-11-03
START TRANSACTION;

DELETE rm FROM li_role_menu rm
JOIN li_menu m ON m.id = rm.menu_id
WHERE m.name IN ('flash-discount', 'nth-item-discount')
  AND m.front_route IN ('promotions/flash-discount', 'promotions/nth-item-discount');

DELETE mr FROM li_store_menu_role mr
JOIN li_store_menu m ON m.id = mr.menu_id
WHERE m.name IN ('flash-discount', 'nth-item-discount')
  AND m.front_route IN ('promotion/flash-discount/flash-discount', 'promotion/nth-item-discount/nth-item-discount');

DELETE FROM li_menu
WHERE name IN ('flash-discount', 'nth-item-discount')
  AND front_route IN ('promotions/flash-discount', 'promotions/nth-item-discount');

DELETE FROM li_store_menu
WHERE name IN ('flash-discount', 'nth-item-discount')
  AND front_route IN ('promotion/flash-discount/flash-discount', 'promotion/nth-item-discount/nth-item-discount');

COMMIT;
