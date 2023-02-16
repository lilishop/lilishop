package cn.lili.modules.system.entity.dto;

import cn.lili.common.utils.StringUtils;
import cn.lili.modules.file.entity.enums.OssEnum;
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
     * oss类型
     */
    private String type;

    /**
     * 阿里云-域名
     */
    private String aliyunOSSEndPoint = "";
    /**
     * 阿里云-储存空间
     */
    private String aliyunOSSBucketName = "";
    /**
     * 阿里云-存放路径路径
     */
    private String aliyunOSSPicLocation = "";
    /**
     * 阿里云-密钥id
     */
    private String aliyunOSSAccessKeyId = "";
    /**
     * 阿里云-密钥
     */
    private String aliyunOSSAccessKeySecret = "";


    /**
     * minio服务地址
     */
    private String m_endpoint;

    /**
     * minio 前端请求地址
     */
    private String m_frontUrl;

    /**
     * minio用户名
     */
    private String m_accessKey;

    /**
     * minio密码
     */
    private String m_secretKey;

    /**
     * minio bucket名称
     */
    private String m_bucketName;


    /**
     * 华为云-发起者的Access Key
     *
     * @return
     */

    String huaweicloudOBSAccessKey;
    /**
     * 华为云-密钥
     */
    String huaweicloudOBSSecretKey;
    /**
     * 华为云OBS-节点
     */
    String huaweicloudOBSEndPoint;

    /**
     * 华为云OBS-桶
     */
    private String huaweicloudOBSBucketName = "";

    /**
     * 腾讯云 用户的 SecretId
     */
    String tencentCOSSecretId;
    /**
     * 腾讯云 用户的 SecretKey
     */
    String tencentCOSSecretKey;
    /**
     * 腾讯云 bucket 的地域
     */
    String tencentCOSRegion;
    /**
     * 腾讯云 bucket
     */
    String tencentCOSBucket;
    /**
     * 腾讯云-域名
     */
    private String tencentCOSEndPoint = "";

    public String getType() {
        //默认给阿里云oss存储类型
        if (StringUtils.isEmpty(type)) {
            return OssEnum.ALI_OSS.name();
        }
        return type;
    }
}
