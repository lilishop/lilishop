package cn.lili.modules.file.plugin.impl;

import cn.hutool.core.util.StrUtil;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.modules.file.plugin.QiNiuManagerPlugin;
import cn.lili.modules.system.entity.dos.Setting;
import cn.lili.modules.system.entity.dto.OssSetting;
import cn.lili.modules.system.entity.enums.SettingEnum;
import cn.lili.modules.system.service.SettingService;
import com.aliyun.oss.ClientException;
import com.aliyun.oss.OSS;
import com.aliyun.oss.OSSClientBuilder;
import com.aliyun.oss.OSSException;
import com.aliyun.oss.model.DeleteObjectsRequest;
import com.google.gson.Gson;
import com.qiniu.common.QiniuException;
import com.qiniu.http.Response;
import com.qiniu.storage.BucketManager;
import com.qiniu.storage.Configuration;
import com.qiniu.storage.UploadManager;
import com.qiniu.util.Auth;
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
public class QiNiuFileManagerPlugin implements QiNiuManagerPlugin {

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

    private Auth auth;
    private UploadManager uploadManager;
    private BucketManager bucketManager;


    /**
     * 获取oss client
     *
     * @return
     */
    private OSS getQiNiuOssClient() {
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


    @Override
    public String inputStreamUpload(InputStream inputStream, String key) {
        OssSetting ossSetting = getSetting();
        auth = Auth.create(ossSetting.getAccessKeyId(), ossSetting.getAccessKeySecret());
        uploadManager = new UploadManager(new Configuration());
        String upToken = auth.uploadToken(ossSetting.getBucketName());

        try {
            Response response = uploadManager.put(inputStream, ossSetting.getPicLocation()  +"/"+ key, upToken, null, "image/jpg");
            if (response.statusCode == 200) {
                return ossSetting.getEndPoint() + "/"+ ossSetting.getPicLocation() + "/" + key;
            }
        } catch (QiniuException e) {
            e.printStackTrace();
        }
        return "";
    }

    @Override
    public void deleteFile(List<String> key) {
        OssSetting ossSetting = getSetting();
        if (bucketManager == null) {
            if (auth == null) {
                auth = Auth.create(ossSetting.getAccessKeyId(), ossSetting.getAccessKeySecret());
            }
            bucketManager = new BucketManager(auth, new Configuration());
        }
//
//        for (int i = 0; 0 < key.size(); i++) {
//            try {
//                bucketManager.delete(ossSetting.getBucketName()+ "/"+ ossSetting.getPicLocation(), key.get(i));
//            } catch (Exception e) {
//                e.printStackTrace();
//            }
//        }
    }

    @Override
    public String getUrl(String url, Integer width, Integer height) {
        //缩略图全路径
        //返回缩略图全路径
        return url + "?x-oss-process=style/" + width + "X" + height;
    }
}
