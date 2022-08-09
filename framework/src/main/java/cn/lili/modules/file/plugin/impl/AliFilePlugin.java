package cn.lili.modules.file.plugin.impl;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.modules.file.entity.enums.OssEnum;
import cn.lili.modules.file.plugin.FilePlugin;
import cn.lili.modules.system.entity.dto.OssSetting;
import com.aliyun.oss.ClientException;
import com.aliyun.oss.OSS;
import com.aliyun.oss.OSSClientBuilder;
import com.aliyun.oss.OSSException;
import com.aliyun.oss.model.DeleteObjectsRequest;
import com.aliyun.oss.model.ObjectMetadata;
import lombok.extern.slf4j.Slf4j;

import java.io.File;
import java.io.InputStream;
import java.util.List;

/**
 * 阿里oss 文件操作
 *
 * @author Chopper
 */

@Slf4j
public class AliFilePlugin implements FilePlugin {

    private OssSetting ossSetting;

    public AliFilePlugin(OssSetting ossSetting) {
        this.ossSetting = ossSetting;
    }

    @Override
    public OssEnum pluginName() {
        return OssEnum.ALI_OSS;
    }

    /**
     * 获取oss client
     *
     * @return
     */
    private OSS getOssClient() {
        return new OSSClientBuilder().build(
                ossSetting.getEndPoint(),
                ossSetting.getAccessKeyId(),
                ossSetting.getAccessKeySecret());
    }


    /**
     * 获取配置前缀
     *
     * @return
     */
    private String getUrlPrefix() {
        return "https://" + ossSetting.getBucketName() + "." + ossSetting.getEndPoint() + "/";
    }

    @Override
    public String pathUpload(String filePath, String key) {
        OSS ossClient = getOssClient();
        try {
            ossClient.putObject(ossSetting.getBucketName(), key, new File(filePath));
        } catch (OSSException oe) {
            log.error("Caught an OSSException, which means your request made it to OSS, "
                    + "but was rejected with an error response for some reason.");
            log.error("Error Message: " + oe.getErrorMessage());
            log.error("Error Code:       " + oe.getErrorCode());
            log.error("Request ID:      " + oe.getRequestId());
            log.error("Host ID:           " + oe.getHostId());
            throw new ServiceException(ResultCode.OSS_EXCEPTION_ERROR);
        } catch (ClientException ce) {
            log.error("Caught an ClientException, which means the client encountered "
                    + "a serious internal problem while trying to communicate with OSS, "
                    + "such as not being able to access the network.");
            log.error("Error Message: " + ce.getMessage());
            throw new ServiceException(ResultCode.OSS_EXCEPTION_ERROR);
        } finally {
            /*
             * Do not forget to shut down the client finally to release all allocated resources.
             */
            ossClient.shutdown();
        }
        ossClient.shutdown();
        return getUrlPrefix() + key;
    }

    @Override
    public String inputStreamUpload(InputStream inputStream, String key) {
        OSS ossClient = getOssClient();
        try {
            ObjectMetadata meta = new ObjectMetadata();
            meta.setContentType("image/jpg");
            ossClient.putObject(ossSetting.getBucketName(), key, inputStream, meta);
        } catch (OSSException oe) {
            log.error("Caught an OSSException, which means your request made it to OSS, "
                    + "but was rejected with an error response for some reason.");
            log.error("Error Message: " + oe.getErrorMessage());
            log.error("Error Code:       " + oe.getErrorCode());
            log.error("Request ID:      " + oe.getRequestId());
            log.error("Host ID:           " + oe.getHostId());
            throw new ServiceException(ResultCode.OSS_EXCEPTION_ERROR);
        } catch (ClientException ce) {
            log.error("Caught an ClientException, which means the client encountered "
                    + "a serious internal problem while trying to communicate with OSS, "
                    + "such as not being able to access the network.");
            log.error("Error Message: " + ce.getMessage());
            throw new ServiceException(ResultCode.OSS_EXCEPTION_ERROR);
        } finally {
            /*
             * Do not forget to shut down the client finally to release all allocated resources.
             */
            ossClient.shutdown();
        }
        ossClient.shutdown();
        return getUrlPrefix() + key;
    }

    @Override
    public void deleteFile(List<String> key) {
        OSS ossClient = getOssClient();

        try {
            ossClient.deleteObjects(
                    new DeleteObjectsRequest(ossSetting.getBucketName()).withKeys(key));
        } catch (OSSException oe) {
            log.error("Caught an OSSException, which means your request made it to OSS, "
                    + "but was rejected with an error response for some reason.");
            log.error("Error Message: " + oe.getErrorMessage());
            log.error("Error Code:       " + oe.getErrorCode());
            log.error("Request ID:      " + oe.getRequestId());
            log.error("Host ID:           " + oe.getHostId());
            throw new ServiceException(ResultCode.OSS_DELETE_ERROR);
        } catch (ClientException ce) {
            log.error("Caught an ClientException, which means the client encountered "
                    + "a serious internal problem while trying to communicate with OSS, "
                    + "such as not being able to access the network.");
            log.error("Error Message: " + ce.getMessage());
            throw new ServiceException(ResultCode.OSS_DELETE_ERROR);
        } finally {
            /*
             * Do not forget to shut down the client finally to release all allocated resources.
             */
            ossClient.shutdown();
        }
    }
}
