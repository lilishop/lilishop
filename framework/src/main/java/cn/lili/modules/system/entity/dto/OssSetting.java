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


    public String getType() {
        //默认给阿里云oss存储类型
        if (StringUtils.isEmpty(type)) {
            return OssEnum.ALI_OSS.name();
        }
        return type;
    }
}
