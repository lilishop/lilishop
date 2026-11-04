-- 平台后台：运营模块下新增「小程序订单」（简单版）。
-- 不建业务表；继承现有商品订单菜单的角色范围和接口权限，可重复执行。
START TRANSACTION;

-- 现有菜单使用「模块 -> Main 分组 -> 页面」三级结构。
INSERT INTO li_menu
    (id, create_by, create_time, delete_flag, update_by, update_time,
     description, front_route, icon, level, name, parent_id, path, sort_order,
     title, front_component, permission)
SELECT 2121900000000000002, 'system', NOW(), b'0', 'system', NOW(),
       '小程序订单入口', 'Main', 'ios-aperture', 1, 'wechatOrderManage',
       CAST(operate.id AS CHAR), '/', 4.5, '小程序订单', NULL, NULL
FROM li_menu operate
WHERE operate.name = 'operate' AND operate.delete_flag = b'0'
  AND NOT EXISTS (SELECT 1 FROM li_menu target WHERE target.name = 'wechatOrderManage')
LIMIT 1;

INSERT INTO li_menu
    (id, create_by, create_time, delete_flag, update_by, update_time,
     description, front_route, icon, level, name, parent_id, path, sort_order,
     title, front_component, permission)
SELECT 2121900000000000001, 'system', NOW(), b'0', 'system', NOW(),
       '微信小程序来源订单查询，复用现有订单详情', 'order/wechat/orderList', source.icon, source.level,
       'wechatOrderList', CAST(group_menu.id AS CHAR), 'wechatOrderList',
       0, '小程序订单', source.front_component, source.permission
FROM li_menu source
INNER JOIN li_menu group_menu ON group_menu.name = 'wechatOrderManage' AND group_menu.delete_flag = b'0'
WHERE source.name = 'orderList' AND source.delete_flag = b'0'
  AND NOT EXISTS (SELECT 1 FROM li_menu target WHERE target.name = 'wechatOrderList')
LIMIT 1;

-- 兼容已执行旧版脚本的环境：更新已有页面，保留 ID 和角色绑定。
UPDATE li_menu page
INNER JOIN li_menu group_menu ON group_menu.name = 'wechatOrderManage' AND group_menu.delete_flag = b'0'
SET page.title = '小程序订单', page.parent_id = CAST(group_menu.id AS CHAR),
    page.level = 2, page.sort_order = 0, page.update_by = 'system', page.update_time = NOW()
WHERE page.name = 'wechatOrderList' AND page.delete_flag = b'0';

INSERT INTO li_role_menu
    (id, create_by, create_time, delete_flag, update_by, update_time, is_super, menu_id, role_id)
SELECT UUID_SHORT(), 'system', NOW(), b'0', 'system', NOW(), bindings.is_super,
       CAST(wechat.id AS CHAR), bindings.role_id
FROM li_role_menu bindings
INNER JOIN li_menu source ON CAST(source.id AS CHAR) = bindings.menu_id
INNER JOIN li_menu wechat ON wechat.name = 'wechatOrderList' AND wechat.delete_flag = b'0'
WHERE source.name = 'orderList' AND source.delete_flag = b'0' AND bindings.delete_flag = b'0'
  AND NOT EXISTS (
      SELECT 1 FROM li_role_menu existing
      WHERE existing.menu_id = CAST(wechat.id AS CHAR)
        AND existing.role_id = bindings.role_id AND existing.delete_flag = b'0'
  );

-- 补齐页面角色的运营模块和分组权限，避免只有页面权限时菜单树不可见。
INSERT INTO li_role_menu
    (id, create_by, create_time, delete_flag, update_by, update_time, is_super, menu_id, role_id)
SELECT UUID_SHORT(), 'system', NOW(), b'0', 'system', NOW(), bindings.is_super,
       CAST(ancestor.id AS CHAR), bindings.role_id
FROM li_role_menu bindings
INNER JOIN li_menu page ON CAST(page.id AS CHAR) = bindings.menu_id
INNER JOIN li_menu ancestor ON ancestor.name IN ('operate', 'wechatOrderManage') AND ancestor.delete_flag = b'0'
WHERE page.name = 'wechatOrderList' AND page.delete_flag = b'0' AND bindings.delete_flag = b'0'
  AND NOT EXISTS (
      SELECT 1 FROM li_role_menu existing
      WHERE existing.menu_id = CAST(ancestor.id AS CHAR)
        AND existing.role_id = bindings.role_id AND existing.delete_flag = b'0'
  );

COMMIT;
