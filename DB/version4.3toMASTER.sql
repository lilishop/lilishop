CREATE TABLE `li_file_directory`
(
    `id`             bigint                                                        NOT NULL AUTO_INCREMENT COMMENT '主键ID',
    `create_time`    datetime                                                      NULL DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
    `create_by`      varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin         NULL DEFAULT NULL COMMENT '创建者',
    `update_time`    datetime                                                      NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '更新时间',
    `update_by`      varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin         NULL DEFAULT NULL COMMENT '更新者',
    `delete_flag`    tinyint(1)                                                    NULL DEFAULT 0 COMMENT '删除标志 true/false 删除/未删除',
    `directory_type` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci NULL DEFAULT NULL COMMENT '文件目录类型',
    `directory_name` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci NULL DEFAULT NULL COMMENT '拥有者名称',
    `owner_id`       varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci NULL DEFAULT NULL COMMENT '拥有者id',
    `parent_id`      bigint                                                        NULL DEFAULT NULL COMMENT '父分类ID',
    `level`          int                                                           NULL DEFAULT NULL COMMENT '层级',
    PRIMARY KEY (`id`) USING BTREE
) ENGINE = InnoDB
  AUTO_INCREMENT = 1698937596963311619
  CHARACTER SET = utf8mb4
  COLLATE = utf8mb4_0900_ai_ci COMMENT = '文件夹'
  ROW_FORMAT = DYNAMIC;

SET FOREIGN_KEY_CHECKS = 1;

ALTER TABLE li_file
    ADD file_directory_id varchar(255) COMMENT '文件夹ID';