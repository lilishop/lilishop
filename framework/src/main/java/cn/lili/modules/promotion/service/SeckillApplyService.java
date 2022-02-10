package cn.lili.modules.promotion.service;

import cn.lili.common.vo.PageVO;
import cn.lili.modules.promotion.entity.dos.SeckillApply;
import cn.lili.modules.promotion.entity.dto.search.SeckillSearchParams;
import cn.lili.modules.promotion.entity.vos.SeckillApplyVO;
import cn.lili.modules.promotion.entity.vos.SeckillGoodsVO;
import cn.lili.modules.promotion.entity.vos.SeckillTimelineVO;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;

/**
 * 秒杀申请业务层
 *
 * @author Chopper
 * @since 2020/11/18 9:45 上午
 */
public interface SeckillApplyService extends IService<SeckillApply> {


    /**
     * 获取当天秒杀活动信息列表（时刻及对应时刻下的商品）
     *
     * @return 秒杀活动信息列表
     */
    List<SeckillTimelineVO> getSeckillTimeline();

    /**
     * 获取当天某个时刻的秒杀活动商品列表
     *
     * @param timeline 指定时刻
     * @return 秒杀活动商品列表
     */
    List<SeckillGoodsVO> getSeckillGoods(Integer timeline);

    /**
     * 分页查询限时请购申请列表
     *
     * @param queryParam 秒杀活动申请查询参数
     * @param pageVo     分页参数
     * @return 限时请购申请列表
     */
    IPage<SeckillApply> getSeckillApplyPage(SeckillSearchParams queryParam, PageVO pageVo);

    /**
     * 查询限时请购申请列表
     *
     * @param queryParam 秒杀活动申请查询参数
     * @return 限时请购申请列表
     */
    List<SeckillApply> getSeckillApplyList(SeckillSearchParams queryParam);

    /**
     * 查询限时请购申请列表总数
     *
     * @param queryParam 查询条件
     * @return 限时请购申请列表总数
     */
    long getSeckillApplyCount(SeckillSearchParams queryParam);

    /**
     * 查询限时请购申请
     *
     * @param queryParam 秒杀活动申请查询参数
     * @return 限时请购申请
     */
    SeckillApply getSeckillApply(SeckillSearchParams queryParam);

    /**
     * 添加秒杀活动申请
     * 检测是否商品是否同时参加多个活动
     * 将秒杀商品信息存入秒杀活动中
     * 保存秒杀活动商品，促销商品信息
     *
     * @param seckillId        秒杀活动编号
     * @param storeId          商家id
     * @param seckillApplyList 秒杀活动申请列表
     */
    void addSeckillApply(String seckillId, String storeId, List<SeckillApplyVO> seckillApplyList);

    /**
     * 批量删除秒杀活动商品
     *
     * @param seckillId 秒杀活动活动id
     * @param id        秒杀活动商品
     */
    void removeSeckillApply(String seckillId, String id);

    /**
     * 更新秒杀商品库存
     *
     * @param seckillId 秒杀活动id
     * @param skuId 商品skuId
     * @param quantity 库存
     */
    void updateSeckillApplyQuantity(String seckillId, String skuId, Integer quantity);

}