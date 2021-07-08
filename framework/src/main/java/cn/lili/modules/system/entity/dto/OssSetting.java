package cn.lili.modules.system.entity.dto;

import lombok.Data;

import java.io.Serializable;

/**
 * OSS设置
 *
 * @author Chopper
 * @date 2020/11/26 15:50
 */

@Data
public class OssSetting implements Serializable {

    private static final long serialVersionUID = 2975271656230801861L;
    /**
     * 域名
     */
    private String endPoint = "oss-cn-beijing.aliyuncs.com";
    /**
     * 储存空间
     */
    private String bucketName = "lilishop-oss";
    /**
     * 存放路径路径
     */
    private String picLocation = "/template";
    /**
     * 密钥id
     */
    private String accessKeyId = "LTAI4G4deX59EyjpEULaJdsU";
    /**
     * 密钥
     */
    private String accessKeySecret = "BlRBpl7WBman6GYYwLKMiKqMTXFhWf";
}
