package cn.lili.modules.promotion.service;

import cn.lili.common.enums.PromotionTypeEnum;
import cn.lili.common.vo.PageVO;
import cn.lili.modules.promotion.entity.dos.BasePromotions;
import cn.lili.modules.promotion.entity.dto.search.BasePromotionsSearchParams;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;

/**
 * 抽象通用促销服务
 * 如需拓展原促销实体字段，新拓展类继承自促销实体即可
 *
 * @param <T> 促销类型，继承自促销基础类
 * @author paulG
 * @since 2021/11/18
 **/
public interface AbstractPromotionsService<T extends BasePromotions> extends IService<T> {

    /**
     * 通用促销保存
     * 调用顺序:
     * 1. initPromotion 初始化促销信息
     * 2. checkPromotions 检查促销参数
     * 3. save 保存促销信息
     * 4. updatePromotionGoods 更新促销商品信息
     * 5。 updateEsGoodsIndex 更新商品索引促销信息
     *
     * @param promotions 促销信息
     * @return 是否保存成功
     */
    boolean savePromotions(T promotions);

    /**
     * 通用促销更新
     * 调用顺序:
     * 1. checkStatus 检查促销状态
     * 2. checkPromotions 检查促销参数
     * 3. saveOrUpdate 保存促销信息
     * 4. updatePromotionGoods 更新促销商品信息
     * 5. updateEsGoodsIndex 更新商品索引促销信息
     *
     * @param promotions 促销信息
     * @return 是否更新成功
     */
    boolean updatePromotions(T promotions);

    /**
     * 更新促销状态
     * 如果要更新促销状态为关闭，startTime和endTime置为空即可
     *
     * @param ids       促销id集合
     * @param startTime 开始时间
     * @param endTime   结束时间
     * @return 是否更新成功
     */
    boolean updateStatus(List<String> ids, Long startTime, Long endTime);

    /**
     * 移除促销活动
     *
     * @param ids 促销活动id集合
     * @return 是否移除成功
     */
    boolean removePromotions(List<String> ids);

    /**
     * 分页查询促销信息
     *
     * @param searchParams 查询参数，继承自继承促销查询参数
     * @param page         分页参数
     * @param <S>          继承自基础促销查询参数的促销查询参数
     * @return 分页促销信息
     */
    <S extends BasePromotionsSearchParams> IPage<T> pageFindAll(S searchParams, PageVO page);

    /**
     * 列表查询促销信息
     *
     * @param searchParams 查询参数，继承自继承促销查询参数
     * @param <S> 继承自基础促销查询参数的促销查询参数
     * @return 列表促销信息
     */
    <S extends BasePromotionsSearchParams> List<T> listFindAll(S searchParams);

    /**
     * 初始化促销字段
     *
     * @param promotions 促销实体
     */
    void initPromotion(T promotions);

    /**
     * 检查促销参数
     *
     * @param promotions 促销实体
     */
    void checkPromotions(T promotions);

    /**
     * 检查促销状态
     *
     * @param promotions 促销实体
     */
    void checkStatus(T promotions);

    /**
     * 更新促销商品信息
     *
     * @param promotions 促销实体
     * @return
     */
    boolean updatePromotionsGoods(T promotions);

    /**
     * 更新促销信息到商品索引
     *
     * @param promotions 促销实体
     */
    void updateEsGoodsIndex(T promotions);

    /**
     * 当前促销类型
     *
     * @return 当前促销类型
     */
    PromotionTypeEnum getPromotionType();

}
