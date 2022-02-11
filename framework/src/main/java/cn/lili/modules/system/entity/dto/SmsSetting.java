package cn.lili.modules.system.entity.dto;

import lombok.Data;

import java.io.Serializable;

/**
 * 短信配置
 * 这里在前台不做调整，方便客户直接把服务商的内容配置在我们平台
 * @author Chopper
 * @since 2020/11/30 15:23
 */
@Data
public class SmsSetting implements Serializable {
    /**
     * 从上到下yi依次是
     * 节点地址
     * key
     * 密钥
     * 签名
     */
    private String regionId;

    private String accessKeyId;

    private String accessSecret;

    private String signName;
}
