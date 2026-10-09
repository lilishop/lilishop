-- 楼层装修 > 主题色（三级菜单）
-- 页面：lilishop-ui/manager/src/views/page-decoration/theme-setting.vue
-- 权限：主题色保存走系统设置接口 /manager/setting/setting/*
INSERT INTO li_menu
  (id, create_by, create_time, delete_flag, update_by, update_time, description, front_route, icon, level, name, parent_id, path, sort_order, title, front_component, permission)
VALUES
  (2057668382760361987, 'admin', NOW(), 0, 'admin', NOW(), NULL, 'page-decoration/theme-setting', 'md-color-palette', 2, 'theme-setting', '1367050320584114176', 'themeSetting', 2.0, '主题色', NULL, '/manager/setting/setting*');
