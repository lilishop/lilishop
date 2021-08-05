package cn.lili.modules.system.entity.dto;

import lombok.Data;

import java.io.Serializable;

/**
 * OSS设置
 *
 * @author Chopper
 * @since 2020/11/26 15:50
 */

@Data
public class OssSetting implements Serializable {

    private static final long serialVersionUID = 2975271656230801861L;
    /**
     * 域名
     */
    private String endPoint = "";
    /**
     * 储存空间
     */
    private String bucketName = "";
    /**
     * 存放路径路径
     */
    private String picLocation = "";
    /**
     * 密钥id
     */
    private String accessKeyId = "";
    /**
     * 密钥
     */
    private String accessKeySecret = "";
}
