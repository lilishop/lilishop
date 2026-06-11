package cn.lili.modules.file.service;

import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.file.entity.enums.OssEnum;
import cn.lili.modules.system.entity.dto.OssSetting;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;

class FileUrlResponseProcessorTest {

    private final FileUrlService fileUrlService = new FileUrlService();

    @Test
    void rewriteResponseShouldProcessNestedGoodsArticleAndPageData() {
        OssSetting setting = new OssSetting();
        setting.setType(OssEnum.LOCAL.name());
        setting.setLocalFileUrlPrefix("/files");
        setting.setCdnEnabled(true);
        setting.setCdnUrlPrefix("https://cdn.example.com");

        DemoGoods goods = new DemoGoods();
        goods.thumbnail = "/files/goods/thumb.png";
        goods.content = "<p><img src='/files/article/content.png'></p>";
        goods.pageData = "{\"list\":[{\"url\":\"/files/page/banner.png\"}]}";
        goods.gallery.add("/files/goods/gallery.png");

        ResultMessage<DemoGoods> resultMessage = new ResultMessage<>();
        resultMessage.setResult(goods);

        fileUrlService.rewriteResponse(resultMessage, setting);

        assertEquals("https://cdn.example.com/files/goods/thumb.png", resultMessage.getResult().thumbnail);
        assertEquals("<p><img src='https://cdn.example.com/files/article/content.png'></p>", resultMessage.getResult().content);
        assertEquals("{\"list\":[{\"url\":\"https://cdn.example.com/files/page/banner.png\"}]}",
                resultMessage.getResult().pageData);
        assertEquals("https://cdn.example.com/files/goods/gallery.png", resultMessage.getResult().gallery.get(0));
    }

    static class DemoGoods {
        String thumbnail;
        String content;
        String pageData;
        List<String> gallery = new ArrayList<>();
    }
}
