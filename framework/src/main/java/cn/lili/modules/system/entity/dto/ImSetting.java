package cn.lili.modules.system.entity.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serializable;

/**
 * IM设置
 *
 * @author Bulbasaur
 * @since 2021/5/16 11:10 下午
 */
@Data
public class ImSetting implements Serializable {


    /** 客服平台访问地址，用于买家和商家跳转客服页面。 */
    @Schema(description = "平台地址")
    private String httpUrl;

    /** 客服图片专用阿里云 OSS Bucket，必须与通用文件桶不同；留空表示未配置。 */
    @Schema(description = "客服图片专用 OSS Bucket")
    private String imageOssBucket;

    /** 客服图片 OSS 公网节点域名，不含协议与路径，例如 oss-cn-beijing.aliyuncs.com。 */
    @Schema(description = "客服图片 OSS Endpoint")
    private String imageOssEndpoint;

    /** 客服图片专用 OSS 服务端访问密钥 ID，不下发给聊天客户端。 */
    @Schema(description = "客服图片 OSS AccessKey ID")
    private String imageOssAccessKeyId;

    /** 客服图片专用 OSS 服务端访问密钥 Secret，不下发给聊天客户端。 */
    @Schema(description = "客服图片 OSS AccessKey Secret")
    private String imageOssAccessKeySecret;

}
