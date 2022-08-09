package cn.lili.buyer.test.cart;


import cn.lili.modules.file.plugin.FilePlugin;
import cn.lili.modules.goods.entity.dos.Brand;
import cn.lili.modules.goods.service.BrandService;
import com.xkcoding.http.util.StringUtil;
import lombok.extern.slf4j.Slf4j;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import java.io.*;
import java.net.URL;
import java.util.List;

/**
 * @author paulG
 * @since 2020/11/14
 **/
@Slf4j
@ExtendWith(SpringExtension.class)
@SpringBootTest
class FileTest {


    @Autowired
    private FilePlugin fileManagerPlugin;

    @Autowired
    private BrandService brandService;

    @Test
    void test() throws Exception {
        List<Brand> categoryList = brandService.list();
        for (Brand brand : categoryList) {
            try {
                if (StringUtil.isEmpty(brand.getLogo()) || brand.getLogo().indexOf("lilishop") > 1) {
                    continue;
                }
                URL url = new URL(brand.getLogo());
                InputStream inputStream = url.openStream();
                //上传至第三方云服务或服务器
                brand.setLogo(fileManagerPlugin.inputStreamUpload(inputStream, brand.getId() + ".png"));
            } catch (IOException e) {
                log.error("上传你文件出错",e);
            }
        }
        brandService.updateBatchById(categoryList);
    }

}
