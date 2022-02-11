package cn.lili.modules.file.plugin.impl;

import cn.hutool.core.util.StrUtil;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.modules.file.plugin.FileManagerPlugin;
import cn.lili.modules.system.entity.dos.Setting;
import cn.lili.modules.system.entity.dto.OssSetting;
import cn.lili.modules.system.entity.enums.SettingEnum;
import cn.lili.modules.system.service.SettingService;
import com.aliyun.oss.ClientException;
import com.aliyun.oss.OSS;
import com.aliyun.oss.OSSClientBuilder;
import com.aliyun.oss.OSSException;
import com.aliyun.oss.model.DeleteObjectsRequest;
import com.aliyun.oss.model.ObjectMetadata;
import com.google.gson.Gson;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.io.File;
import java.io.InputStream;
import java.util.List;

/**
 * 阿里oss 文件操作
 *
 * @author Chopper
 */

@Component
@Slf4j
public class AliFileManagerPlugin implements FileManagerPlugin {

    @Autowired
    private SettingService settingService;

    /**
     * 下一个初始化配置参数的时间
     * 这里为了防止多次调用redis，减少与redis的交互时间
     */
    private static Long nextInitSetting;

    /**
     * 暂时设定3分账请求一次设置
     */
    private static final Long INTERVAL = 60 * 3 * 1000L;

    /**
     * 静态设置，最快三分钟更新一次
     */
    private static OssSetting ossSetting;

    /**
     * 获取oss client
     *
     * @return
     */
    private OSS getOssClient() {
        OssSetting ossSetting = getSetting();

        return new OSSClientBuilder().build(
                ossSetting.getEndPoint(),
                ossSetting.getAccessKeyId(),
                ossSetting.getAccessKeySecret());
    }

    /**
     * 获取配置
     *
     * @return
     */
    private OssSetting getSetting() {
        //如果没有配置，或者没有下次刷新时间，或者下次刷新时间小于当前时间，则从redis 更新一次
        if (ossSetting == null || nextInitSetting == null || nextInitSetting < System.currentTimeMillis()) {
            Setting setting = settingService.get(SettingEnum.OSS_SETTING.name());
            if (setting == null || StrUtil.isBlank(setting.getSettingValue())) {
                throw new ServiceException(ResultCode.OSS_NOT_EXIST);
            }
            nextInitSetting = System.currentTimeMillis() + INTERVAL;
            ossSetting = new Gson().fromJson(setting.getSettingValue(), OssSetting.class);
            return ossSetting;
        }
        return ossSetting;
    }

    /**
     * 获取配置前缀
     *
     * @return
     */
    private String getUrlPrefix() {
        OssSetting ossSetting = getSetting();
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
            ossClient.putObject(getSetting().getBucketName(), key, inputStream, meta);
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
                    new DeleteObjectsRequest(getSetting().getBucketName()).withKeys(key));
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
