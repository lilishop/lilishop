package cn.lili.modules.goods.serviceimpl;


import cn.lili.modules.file.service.FileUrlService;
import cn.lili.modules.goods.entity.dos.GoodsGallery;
import cn.lili.modules.goods.mapper.GoodsGalleryMapper;
import cn.lili.modules.goods.service.GoodsGalleryService;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

/**
 * 商品相册接口实现
 *
 * @author pikachu
 * @version v1.0
 * @since v1.0
 * 2020-02-23 15:18:56
 */
@Service
public class GoodsGalleryServiceImpl extends ServiceImpl<GoodsGalleryMapper, GoodsGallery> implements GoodsGalleryService {
    @Autowired
    private FileUrlService fileUrlService;


    @Override
    @Transactional(rollbackFor = Exception.class)
    public void add(List<String> goodsGalleryList, String goodsId) {
        //删除原来商品相册信息
        this.baseMapper.delete(new QueryWrapper<GoodsGallery>().eq("goods_id", goodsId));
        //确定好图片选择器后进行处理
        int i = 0;
        for (String origin : goodsGalleryList) {
            //获取商品原图相册
            GoodsGallery galley = this.getGoodsGallery(origin);
            galley.setGoodsId(goodsId);
            //默认第一个为默认图片
            galley.setIsDefault(i == 0 ? 1 : 0);
            i++;
            this.baseMapper.insert(galley);
        }
    }

    @Override
    public GoodsGallery getGoodsGallery(String origin) {
        GoodsGallery goodsGallery = new GoodsGallery();
        // 各种展示尺寸统一使用原图，只转换公开访问地址。
        String imageUrl = fileUrlService.toPublicUrl(origin);
        goodsGallery.setSmall(imageUrl);
        goodsGallery.setThumbnail(imageUrl);
        goodsGallery.setOriginal(imageUrl);
        return goodsGallery;
    }

    @Override
    public List<GoodsGallery> goodsGalleryList(String goodsId) {
        //根据商品id查询商品相册
        return this.baseMapper.selectList(new QueryWrapper<GoodsGallery>().eq("goods_id", goodsId));
    }

    /**
     * 根据商品 id删除商品相册缩略图
     *
     * @param goodsId 商品ID
     */
    @Override
    public void removeByGoodsId(String goodsId) {
        this.baseMapper.delete(new QueryWrapper<GoodsGallery>().eq("goods_id", goodsId));
    }



}
