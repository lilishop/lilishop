-- 楼层装修 > 分类设置（三级菜单）
-- 页面：lilishop-ui/manager/src/views/page-decoration/category-setting.vue
-- 权限：分类设置保存走系统设置接口 /manager/setting/setting/*
INSERT INTO li_menu
  (id, create_by, create_time, delete_flag, update_by, update_time, description, front_route, icon, level, name, parent_id, path, sort_order, title, front_component, permission)
VALUES
  (2057668382760361988, 'admin', NOW(), 0, 'admin', NOW(), NULL, 'page-decoration/category-setting', 'md-apps', 2, 'category-setting', '1367050320584114176', 'categorySetting', 3.0, '分类设置', NULL, '/manager/setting/setting*');
