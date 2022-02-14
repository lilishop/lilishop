package cn.lili.modules.goods.serviceimpl;

import cn.hutool.json.JSONUtil;
import cn.lili.modules.file.util.FileUtil;
import cn.lili.modules.goods.entity.dos.GoodsGallery;
import cn.lili.modules.goods.mapper.GoodsGalleryMapper;
import cn.lili.modules.goods.service.GoodsGalleryService;
import cn.lili.modules.system.entity.dos.Setting;
import cn.lili.modules.system.entity.dto.GoodsSetting;
import cn.lili.modules.system.entity.enums.SettingEnum;
import cn.lili.modules.system.service.SettingService;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.UpdateWrapper;
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
    /**
     * 设置
     */
    @Autowired
    private SettingService settingService;


    @Override
    @Transactional(rollbackFor = Exception.class)
    public void add(List<String> goodsGalleryList, String goodsId) {
        //删除原来商品相册信息
        this.baseMapper.delete(new UpdateWrapper<GoodsGallery>().eq("goods_id", goodsId));
        //确定好图片选择器后进行处理
        int i = 0;
        for (String origin : goodsGalleryList) {
            //获取带所有缩略的相册
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
        //获取商品系统配置决定是否审核
        Setting setting = settingService.get(SettingEnum.GOODS_SETTING.name());
        GoodsSetting goodsSetting = JSONUtil.toBean(setting.getSettingValue(), GoodsSetting.class);
        //缩略图
        String thumbnail = FileUtil.getUrl(origin, goodsSetting.getAbbreviationPictureWidth(), goodsSetting.getAbbreviationPictureHeight());
        //小图
        String small = FileUtil.getUrl(origin, goodsSetting.getSmallPictureWidth(), goodsSetting.getSmallPictureHeight());
        //赋值
        goodsGallery.setSmall(small);
        goodsGallery.setThumbnail(thumbnail);
        goodsGallery.setOriginal(origin);
        return goodsGallery;
    }

    @Override
    public List<GoodsGallery> goodsGalleryList(String goodsId) {
        //根据商品id查询商品相册
        return this.baseMapper.selectList(new QueryWrapper<GoodsGallery>().eq("goods_id", goodsId));
    }
}