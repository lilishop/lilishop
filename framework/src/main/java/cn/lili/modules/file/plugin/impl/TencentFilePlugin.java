package cn.lili.modules.file.plugin.impl;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.modules.file.entity.enums.OssEnum;
import cn.lili.modules.file.plugin.FilePlugin;
import cn.lili.modules.system.entity.dto.OssSetting;
import com.qcloud.cos.COSClient;
import com.qcloud.cos.ClientConfig;
import com.qcloud.cos.auth.BasicCOSCredentials;
import com.qcloud.cos.auth.COSCredentials;
import com.qcloud.cos.exception.CosClientException;
import com.qcloud.cos.exception.CosServiceException;
import com.qcloud.cos.http.HttpProtocol;
import com.qcloud.cos.model.DeleteObjectsRequest;
import com.qcloud.cos.model.ObjectMetadata;
import com.qcloud.cos.model.PutObjectRequest;
import com.qcloud.cos.region.Region;
import lombok.extern.slf4j.Slf4j;

import java.io.File;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;

/**
 * 腾讯cos 文件操作
 *
 * @author Bulbasaur
 */

@Slf4j
public class TencentFilePlugin implements FilePlugin {

    private OssSetting ossSetting;

    public TencentFilePlugin(OssSetting ossSetting) {
        this.ossSetting = ossSetting;
    }

    @Override
    public OssEnum pluginName() {
        return OssEnum.TENCENT_COS;
    }

    /**
     * 获取oss client
     *
     * @return
     */
    private COSClient getCOSClient() {
        // 1 初始化用户身份信息（secretId, secretKey）。
        COSCredentials cred = new BasicCOSCredentials(ossSetting.getTencentCOSSecretId(), ossSetting.getTencentCOSSecretKey());
        // 2 设置 bucket 的地域, COS 地域的简称请参见 https://cloud.tencent.com/document/product/436/6224
        ClientConfig clientConfig = new ClientConfig(new Region("COS_REGION"));
        // 这里建议设置使用 https 协议
        clientConfig.setHttpProtocol(HttpProtocol.https);
        // 3 生成 cos 客户端。
        return new COSClient(cred, clientConfig);
    }


    /**
     * 获取配置前缀
     *
     * @return
     */
    private String getUrlPrefix() {
        return "https://" + ossSetting.getTencentCOSBucket() + "." + ossSetting.getTencentCOSEndPoint() + "/";
    }

    @Override
    public String pathUpload(String filePath, String key) {
        COSClient cosClient = getCOSClient();
        try {
            cosClient.putObject(ossSetting.getTencentCOSBucket(), key, new File(filePath));
        } catch (CosServiceException oe) {
            throw new ServiceException(ResultCode.OSS_EXCEPTION_ERROR);
        } catch (CosClientException ce) {
            throw new ServiceException(ResultCode.OSS_EXCEPTION_ERROR);
        } finally {
            cosClient.shutdown();
        }
        return getUrlPrefix() + key;
    }

    @Override
    public String inputStreamUpload(InputStream inputStream, String key) {
        COSClient cosClient = getCOSClient();
        try {
            ObjectMetadata meta = new ObjectMetadata();
            meta.setContentType("image/jpg");
            cosClient.putObject(ossSetting.getTencentCOSBucket(), key, inputStream, meta);
        } catch (CosServiceException oe) {
            throw new ServiceException(ResultCode.OSS_EXCEPTION_ERROR);
        } catch (CosClientException ce) {
            throw new ServiceException(ResultCode.OSS_EXCEPTION_ERROR);
        } finally {
            cosClient.shutdown();
        }
        return getUrlPrefix() + key;
    }

    @Override
    public void deleteFile(List<String> keys) {
        COSClient cosClient = getCOSClient();

        try {
            List<DeleteObjectsRequest.KeyVersion> delObjects = new ArrayList<>();
            for (String key:keys) {
                delObjects.add(new DeleteObjectsRequest.KeyVersion(key));
            }
            cosClient.deleteObjects(new DeleteObjectsRequest(ossSetting.getTencentCOSBucket()).withKeys(delObjects));
        } catch (CosServiceException oe) {
            throw new ServiceException(ResultCode.OSS_EXCEPTION_ERROR);
        } catch (CosClientException ce) {
            throw new ServiceException(ResultCode.OSS_EXCEPTION_ERROR);
        } finally {
            cosClient.shutdown();
        }
    }
}
