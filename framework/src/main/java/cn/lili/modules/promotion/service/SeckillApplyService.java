package cn.lili.modules.promotion.service;

import cn.lili.common.vo.PageVO;
import cn.lili.modules.promotion.entity.dos.SeckillApply;
import cn.lili.modules.promotion.entity.vos.SeckillApplyVO;
import cn.lili.modules.promotion.entity.vos.SeckillGoodsVO;
import cn.lili.modules.promotion.entity.vos.SeckillSearchParams;
import cn.lili.modules.promotion.entity.vos.SeckillTimelineVO;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;
import java.util.Map;

/**
 * 秒杀申请业务层
 *
 * @author Chopper
 * @date 2020/11/18 9:45 上午
 */
public interface SeckillApplyService extends IService<SeckillApply> {


    /**
     * 获取当天限时抢购信息列表（时刻及对应时刻下的商品）
     *
     * @return 限时抢购信息列表
     */
    List<SeckillTimelineVO> getSeckillTimeline();


    /**
     * 获取当天某个时刻的限时抢购商品列表
     *
     * @param timeline 指定时刻
     * @return 限时抢购商品列表
     */
    List<SeckillGoodsVO> getSeckillGoods(Integer timeline);

    /**
     * 审核一批申请
     *
     * @param ids         限时抢购申请编号
     * @param seckillId   限时抢购编号
     * @param applyStatus 审批状态
     * @param failReason  驳回原因
     */
    void auditBatchApply(String[] ids, String seckillId, String applyStatus, String failReason);

    /**
     * 分页查询限时请购申请列表
     *
     * @param queryParam 限时抢购申请查询参数
     * @param pageVo     分页参数
     * @return 限时请购申请列表
     */
    IPage<SeckillApply> getSeckillApplyFromMysql(SeckillSearchParams queryParam, PageVO pageVo);

    /**
     * 从mongo中分页查询限时请购申请列表
     *
     * @param queryParam 限时抢购申请查询参数
     * @param pageVo     分页参数
     * @return 限时请购申请列表
     */
    IPage<SeckillApply> getSeckillApplyFromMongo(SeckillSearchParams queryParam, PageVO pageVo);

    /**
     * 添加限时抢购申请
     *
     * @param seckillId        限时抢购编号
     * @param storeId          商家id
     * @param seckillApplyList 限时抢购申请列表
     */
    void addSeckillApply(String seckillId, String storeId, List<SeckillApplyVO> seckillApplyList);

    /**
     * 批量删除限时抢购申请
     *
     * @param seckillId 限时抢购活动id
     * @param ids       限时抢购申请id集合
     */
    void removeSeckillApplyByIds(String seckillId, List<String> ids);

    /**
     * 更新限时抢购库存数量
     *
     * @param id  限时抢购申请（限时抢购商品）id
     * @param num 数量
     * @return 是否成功
     */
    boolean updateSeckillStock(String id, Integer num);


    /**
     * 更新限时抢购库存数量
     *
     * @param map key 为 限时抢购申请（限时抢购商品）id， value 为数量
     * @return 是否成功
     */
    boolean updateSeckillStock(Map<String, Integer> map);

}