-- 客服图片独立存储凭据：保存店铺、会话归属及 OSS 位置快照，供发送时验证。
-- 作者：Bulbasaur
-- 创建日期：2026-11-09（Asia/Shanghai）
-- 可重复执行；不迁移历史图片、不修改已有配置及授权。DDL 可能隐式提交，不支持整份事务回滚。
-- 执行后检查 SHOW CREATE TABLE li_im_image，并在客服设置中配置独立公共读桶后验收双方上传。
CREATE TABLE IF NOT EXISTS li_im_image (
    id varchar(64) NOT NULL COMMENT '图片凭据ID及随机文件名',
    store_id varchar(64) NOT NULL COMMENT '所属li_store.id，由会话推导',
    talk_id varchar(64) NOT NULL COMMENT '所属li_im_talk.id',
    uploader_id varchar(64) NOT NULL COMMENT '实际上传账号ID',
    uploader_role varchar(16) NOT NULL COMMENT '上传账号类型：MEMBER客户、STORE商家',
    bucket varchar(63) NOT NULL COMMENT '上传时OSS桶快照',
    endpoint varchar(255) NOT NULL COMMENT '上传时OSS节点快照',
    object_key varchar(512) NOT NULL COMMENT '含店铺与会话ID的对象路径',
    url varchar(1024) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NOT NULL COMMENT '公共读HTTPS永久地址，精确匹配',
    content_type varchar(32) NOT NULL COMMENT '服务端识别的图片MIME类型',
    file_size bigint NOT NULL COMMENT '图片大小，单位字节，最大10MB',
    create_time datetime(3) NOT NULL COMMENT '上传凭据创建时间',
    PRIMARY KEY (id),
    KEY idx_im_image_talk_uploader (talk_id, uploader_id, uploader_role)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='客服图片上传凭据';
