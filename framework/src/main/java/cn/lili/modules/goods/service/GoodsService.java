package cn.lili.modules.goods.service;

import cn.lili.modules.goods.entity.dos.Goods;
import cn.lili.modules.goods.entity.dto.GoodsOperationDTO;
import cn.lili.modules.goods.entity.dto.GoodsOperationFuLuDTO;
import cn.lili.modules.goods.entity.dto.GoodsSearchParams;
import cn.lili.modules.goods.entity.enums.GoodsAuthEnum;
import cn.lili.modules.goods.entity.enums.GoodsStatusEnum;
import cn.lili.modules.goods.entity.vos.GoodsVO;
import cn.lili.modules.store.entity.dos.Store;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;

/**
 * 商品业务层
 *
 * @author pikachu
 * @since 2020-02-23 16:18:56
 */
public interface GoodsService extends IService<Goods> {


    /**
     * 根据品牌获取商品
     *
     * @param brandIds 品牌ids
     */
    List<Goods> getByBrandIds(List<String> brandIds);

    /**
     * 下架所有商家商品
     *
     * @param storeId 店铺ID
     */
    void underStoreGoods(String storeId);

    /**
     * 更新商品参数
     *
     * @param goodsId 商品id
     * @param params 商品参数
     */
    void updateGoodsParams(String goodsId, String params);

    /**
     * 获取某分类下的商品数量
     *
     * @param categoryId 分类ID
     * @return 商品数量
     */
    long getGoodsCountByCategory(String categoryId);

    /**
     * 添加商品
     *
     * @param goodsOperationDTO 商品查询条件
     */
    void addGoods(GoodsOperationDTO goodsOperationDTO);

    /**
     * 修改商品
     *
     * @param goodsOperationDTO 商品查询条件
     * @param goodsId           商品ID
     */
    void editGoods(GoodsOperationDTO goodsOperationDTO, String goodsId);

    /**
     * 查询商品VO
     *
     * @param goodsId 商品id
     * @return 商品VO
     */
    GoodsVO getGoodsVO(String goodsId);


    /**
     * 商品查询
     *
     * @param goodsSearchParams 查询参数
     * @return 商品分页
     */
    IPage<Goods> queryByParams(GoodsSearchParams goodsSearchParams);


    /**
     * 商品查询
     *
     * @param goodsSearchParams 查询参数
     * @return 商品信息
     */
    List<Goods> queryListByParams(GoodsSearchParams goodsSearchParams);

    /**
     * 批量审核商品
     *
     * @param goodsIds      商品id列表
     * @param goodsAuthEnum 审核操作
     * @return 审核结果
     */
    boolean auditGoods(List<String> goodsIds, GoodsAuthEnum goodsAuthEnum);


    /**
     * 更新商品上架状态状态
     *
     * @param goodsIds        商品ID集合
     * @param goodsStatusEnum 更新的商品状态
     * @param underReason     下架原因
     * @return 更新结果
     */
    Boolean updateGoodsMarketAble(List<String> goodsIds, GoodsStatusEnum goodsStatusEnum, String underReason);


    /**
     * 更新商品上架状态状态
     *
     * @param storeId        店铺ID
     * @param goodsStatusEnum 更新的商品状态
     * @param underReason     下架原因
     * @return 更新结果
     */
    Boolean updateGoodsMarketAbleByStoreId(String storeId, GoodsStatusEnum goodsStatusEnum, String underReason);

    /**
     * 更新商品上架状态状态
     *
     * @param goodsIds        商品ID集合
     * @param goodsStatusEnum 更新的商品状态
     * @param underReason     下架原因
     * @return 更新结果
     */
    Boolean managerUpdateGoodsMarketAble(List<String> goodsIds, GoodsStatusEnum goodsStatusEnum, String underReason);
    /**
     * 删除商品
     *
     * @param goodsIds 商品ID
     * @return 操作结果
     */
    Boolean deleteGoods(List<String> goodsIds);

    /**
     * 设置商品运费模板
     *
     * @param goodsIds   商品列表
     * @param templateId 运费模板ID
     * @return 操作结果
     */
    Boolean freight(List<String> goodsIds, String templateId);

    /**
     * 修改商品库存数量
     *
     * @param goodsId  商品ID
     * @param quantity 库存数量
     */
    void updateStock(String goodsId, Integer quantity);

    /**
     * 更新商品评价数量
     *
     * @param goodsId 商品ID
     */
    void updateGoodsCommentNum(String goodsId);

    /**
     * 更新商品的购买数量
     *
     * @param goodsId  商品ID
     * @param buyCount 购买数量
     */
    void updateGoodsBuyCount(String goodsId, int buyCount);

    /**
     * 批量更新商品的店铺信息
     * @param store
     */
    void updateStoreDetail(Store store);
    /**
     * 统计店铺的商品数量
     * @param storeId 店铺id
     * @return
     */
    long countStoreGoodsNum(String storeId);

}
