package cn.lili.modules.im.entity.dos;

import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;
import java.util.Date;

/**
 * 客服会话图片上传凭据，保存存储快照及会话归属，供发送消息时校验，独立于公共素材库。
 * @author Bulbasaur
 * @since 2026-11-09
 */
@Data
@TableName("li_im_image")
public class ImImage {
    /** 图片记录 ID，同时作为 OSS 随机文件名，禁止客户端指定。 */
    @TableId
    private String id;
    /** 所属店铺 ID，对应 li_store.id，由 li_im_talk 的店铺参与方确定。 */
    private String storeId;
    /** 所属客服会话 ID，对应 li_im_talk.id。 */
    private String talkId;
    /** 实际上传账号 ID，来自登录凭据，商家账号与店铺 ID 分开记录。 */
    private String uploaderId;
    /** 上传账号类型：MEMBER 客户、STORE 商家；与账号 ID 一起限制发送权限。 */
    private String uploaderRole;
    /** 上传时的 OSS Bucket 快照，后续修改配置不改变已有图片位置。 */
    private String bucket;
    /** 上传时 OSS Endpoint 快照，不包含协议和路径。 */
    private String endpoint;
    /** OSS 对象路径，包含店铺 ID、会话 ID 和北京时间日期目录。 */
    private String objectKey;
    /** 公共读 HTTPS 永久地址，作为 PICTURE 消息正文，不保存临时签名。 */
    private String url;
    /** 根据文件内容识别的图片 MIME 类型：image/png、image/jpeg、image/gif、image/webp。 */
    private String contentType;
    /** 上传图片大小，单位字节，允许范围 1 至 10 × 1024 × 1024。 */
    private Long fileSize;
    /** 上传凭据创建时间，按绝对时间保存，路径日期采用 Asia/Shanghai。 */
    private Date createTime;
}
