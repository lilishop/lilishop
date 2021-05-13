package cn.lili.modules.promotion.service;

import cn.lili.common.vo.PageVO;
import cn.lili.modules.promotion.entity.dos.PointsGoods;
import cn.lili.modules.promotion.entity.vos.PointsGoodsSearchParams;
import cn.lili.modules.promotion.entity.vos.PointsGoodsVO;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;

/**
 * 积分商品业务层
 *
 * @author paulG
 * @date 2020/11/18 9:45 上午
 **/
public interface PointsGoodsService extends IService<PointsGoods> {

    /**
     * 批量添加积分商品
     *
     * @param pointsGoodsList 积分商品列表
     * @return 是否添加成功
     */
    boolean addPointsGoods(List<PointsGoodsVO> pointsGoodsList);

    /**
     * 更新一个积分商品
     *
     * @param pointsGoodsDTO 编辑的积分商品信息
     * @return 是否更新成功
     */
    boolean updatePointsGoods(PointsGoodsVO pointsGoodsDTO);

    /**
     * 批量更新积分商品状态
     *
     * @param ids             积分商品id集合
     * @param promotionStatus 更新的状态
     * @return 是否更新成功
     */
    boolean updatePointsGoodsPromotionStatus(List<String> ids, String promotionStatus);

    /**
     * 批量删除积分商品
     *
     * @param ids 积分商品id集合
     * @return 是否删除成功
     */
    boolean deletePointsGoods(List<String> ids);

    /**
     * 根据ID获取积分详情
     *
     * @param id 积分商品id
     * @return 积分详情
     */
    PointsGoodsVO getPointsGoodsDetail(String id);

    /**
     * 根据SkuID获取积分商品信息
     *
     * @param skuId 商品skuId
     * @return 积分详情
     */
    PointsGoods getPointsGoodsDetailBySkuId(String skuId);

    /**
     * 根据条件查询积分商品
     *
     * @param searchParams 积分商品查询参数
     * @param page         分页参数
     * @return 积分商品查询结果
     */
    IPage<PointsGoodsVO> getPointsGoodsByPage(PointsGoodsSearchParams searchParams, PageVO page);

}
