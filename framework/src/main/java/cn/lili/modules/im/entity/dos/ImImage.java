package cn.lili.modules.im.entity.dos;

import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Date;

/**
 * 客服会话图片上传凭据
 *
 * @author Bulbasaur
 * @since 2026-11-09
 */
@Data
@TableName("li_im_image")
@Schema(description = "客服会话图片上传凭据")
@NoArgsConstructor
@AllArgsConstructor
public class ImImage {

    @TableId
    @Schema(description = "图片凭据ID及随机文件名")
    private String id;

    @Schema(description = "所属店铺ID")
    private String storeId;

    @Schema(description = "所属客服会话ID")
    private String talkId;

    @Schema(description = "实际上传账号ID")
    private String uploaderId;

    @Schema(description = "上传账号类型：MEMBER客户、STORE商家")
    private String uploaderRole;

    @Schema(description = "上传时OSS桶快照")
    private String bucket;

    @Schema(description = "上传时OSS节点快照")
    private String endpoint;

    @Schema(description = "OSS对象路径")
    private String objectKey;

    @Schema(description = "公共读HTTPS永久地址")
    private String url;

    @Schema(description = "图片MIME类型")
    private String contentType;

    @Schema(description = "图片大小(字节)")
    private Long fileSize;

    @Schema(description = "上传凭据创建时间")
    private Date createTime;
}
