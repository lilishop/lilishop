package cn.lili.modules.store.service;

import cn.lili.modules.store.entity.dos.StoreGoodsLabel;
import cn.lili.modules.store.entity.vos.StoreGoodsLabelVO;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;

/**
 * 店铺商品分类业务层
 *
 * @author Bulbasaur
 * @date 2020-03-07 09:24:33
 */
public interface StoreGoodsLabelService extends IService<StoreGoodsLabel> {

    /**
     * 根据商家ID获取店铺分类列表
     *
     * @param storeId 商家ID
     * @return 店铺分类列表
     */
    List<StoreGoodsLabelVO> listByStoreId(String storeId);

    /**
     * 添加商品分类
     *
     * @param storeGoodsLabel 店铺商品分类
     * @return 店铺商品分类
     */
    StoreGoodsLabel addStoreGoodsLabel(StoreGoodsLabel storeGoodsLabel);

    /**
     * 修改商品分类
     *
     * @param storeGoodsLabel 店铺商品分类
     * @return 店铺商品分类
     */
    StoreGoodsLabel editStoreGoodsLabel(StoreGoodsLabel storeGoodsLabel);

    /**
     * 删除商品分类
     *
     * @param storeLabelId 店铺 分类 ID
     */
    void removeStoreGoodsLabel(String storeLabelId);

}
