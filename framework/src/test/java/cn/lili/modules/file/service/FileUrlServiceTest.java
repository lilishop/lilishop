package cn.lili.modules.file.service;

import cn.lili.modules.file.entity.enums.OssEnum;
import cn.lili.modules.system.entity.dto.OssSetting;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class FileUrlServiceTest {

    private final FileUrlService fileUrlService = new FileUrlService();

    @Test
    void toPublicUrlShouldKeepOriginalWhenCdnDisabled() {
        OssSetting setting = new OssSetting();
        setting.setType(OssEnum.LOCAL.name());
        setting.setLocalFileUrlPrefix("/files");
        setting.setCdnEnabled(false);
        setting.setCdnUrlPrefix("https://cdn.example.com");

        assertEquals("/files/MANAGER/a.png", fileUrlService.toPublicUrl("/files/MANAGER/a.png", setting));
    }

    @Test
    void toPublicUrlShouldRewriteLocalStoragePathWhenCdnEnabled() {
        OssSetting setting = new OssSetting();
        setting.setType(OssEnum.LOCAL.name());
        setting.setLocalFileUrlPrefix("/files");
        setting.setCdnEnabled(true);
        setting.setCdnUrlPrefix("https://cdn.example.com/assets/");

        assertEquals("https://cdn.example.com/assets/files/MANAGER/a.png?x=1",
                fileUrlService.toPublicUrl("/files/MANAGER/a.png?x=1", setting));
    }

    @Test
    void toPublicUrlShouldRewriteCloudOriginWhenCdnEnabled() {
        OssSetting setting = new OssSetting();
        setting.setType(OssEnum.ALI_OSS.name());
        setting.setAliyunOSSBucketName("demo-bucket");
        setting.setAliyunOSSEndPoint("oss-cn-hangzhou.aliyuncs.com");
        setting.setCdnEnabled(true);
        setting.setCdnUrlPrefix("https://img.example.com");

        assertEquals("https://img.example.com/goods/a.png",
                fileUrlService.toPublicUrl("https://demo-bucket.oss-cn-hangzhou.aliyuncs.com/goods/a.png", setting));
    }

    @Test
    void toImageUrlShouldAppendVendorStyleBeforeCdnRewrite() {
        OssSetting setting = new OssSetting();
        setting.setType(OssEnum.TENCENT_COS.name());
        setting.setTencentCOSBucket("demo-123");
        setting.setTencentCOSRegion("ap-shanghai");
        setting.setCdnEnabled(true);
        setting.setCdnUrlPrefix("https://cdn.example.com");

        assertEquals("https://cdn.example.com/goods/a.png?imageMogr2/thumbnail/300x300",
                fileUrlService.toImageUrl("https://demo-123.cos.ap-shanghai.myqcloud.com/goods/a.png", 300, 300, setting));
    }

    @Test
    void rewriteTextShouldHandleHtmlAndJsonContent() {
        OssSetting setting = new OssSetting();
        setting.setType(OssEnum.LOCAL.name());
        setting.setLocalFileUrlPrefix("/files");
        setting.setCdnEnabled(true);
        setting.setCdnUrlPrefix("https://cdn.example.com");

        assertEquals("<p><img src=\"https://cdn.example.com/files/article/a.png\"></p>",
                fileUrlService.rewriteText("<p><img src=\"/files/article/a.png\"></p>", setting));
        assertEquals("{\"image\":\"https://cdn.example.com/files/page/a.png\"}",
                fileUrlService.rewriteText("{\"image\":\"/files/page/a.png\"}", setting));
    }
}
