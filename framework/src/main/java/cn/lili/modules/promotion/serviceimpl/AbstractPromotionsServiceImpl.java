package cn.lili.modules.promotion.serviceimpl;

import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.json.JSONUtil;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.properties.RocketmqCustomProperties;
import cn.lili.common.vo.PageVO;
import cn.lili.modules.promotion.entity.dos.BasePromotions;
import cn.lili.modules.promotion.entity.dos.PromotionGoods;
import cn.lili.modules.promotion.entity.dto.search.BasePromotionsSearchParams;
import cn.lili.modules.promotion.entity.enums.PromotionsScopeTypeEnum;
import cn.lili.modules.promotion.event.UpdateEsGoodsIndexPromotionsEvent;
import cn.lili.modules.promotion.service.AbstractPromotionsService;
import cn.lili.modules.promotion.service.PromotionGoodsService;
import cn.lili.modules.promotion.tools.PromotionTools;
import cn.lili.mybatis.util.PageUtil;
import cn.lili.rocketmq.RocketmqSendCallbackBuilder;
import cn.lili.rocketmq.tags.GoodsTagsEnum;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.UpdateWrapper;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.apache.rocketmq.spring.core.RocketMQTemplate;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;

/**
 * @author paulG
 * @since 2021/11/30
 **/
public abstract class AbstractPromotionsServiceImpl<M extends BaseMapper<T>, T extends BasePromotions> extends ServiceImpl<M, T> implements AbstractPromotionsService<T> {

    /**
     * 促销商品
     */
    @Autowired
    private PromotionGoodsService promotionGoodsService;

    /**
     * rocketMq配置
     */
    @Autowired
    private RocketmqCustomProperties rocketmqCustomProperties;

    /**
     * rocketMq
     */
    @Autowired
    private RocketMQTemplate rocketMQTemplate;

    @Autowired
    private ApplicationEventPublisher applicationEventPublisher;

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
    @Override
    @Transactional(rollbackFor = {Exception.class})
    public boolean savePromotions(T promotions) {
        this.initPromotion(promotions);
        this.checkPromotions(promotions);
        boolean save = this.save(promotions);
        if (this.updatePromotionsGoods(promotions)) {
            this.updateEsGoodsIndex(promotions);
        }
        return save;
    }

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
    @Override
    @Transactional(rollbackFor = {Exception.class})
    public boolean updatePromotions(T promotions) {
        this.checkStatus(promotions);
        this.checkPromotions(promotions);
        boolean save = this.saveOrUpdate(promotions);
        if (this.updatePromotionsGoods(promotions)) {
            this.updateEsGoodsIndex(promotions);
        }
        return save;
    }

    /**
     * 更新促销状态
     * 如果要更新促销状态为关闭，startTime和endTime置为空即可
     *
     * @param ids       促销id集合
     * @param startTime 开始时间
     * @param endTime   结束时间
     * @return 是否更新成功
     */
    @Override
    @Transactional(rollbackFor = {Exception.class})
    public boolean updateStatus(List<String> ids, Long startTime, Long endTime) {
        List<T> promotionsList = this.list(new QueryWrapper<T>().in("id", ids));
        for (T t : promotionsList) {
            if (startTime != null && endTime != null) {
                t.setStartTime(new Date(startTime));
                t.setEndTime(new Date(endTime));
            } else {
                t.setStartTime(null);
                t.setEndTime(null);
            }
            this.checkStatus(t);
            this.updatePromotionsGoods(t);
            this.updateEsGoodsIndex(t);
        }
        if (startTime != null && endTime != null) {
            return this.update(new UpdateWrapper<T>().in("id", ids).set("start_time", new Date(startTime)).set("end_time", new Date(endTime)));
        } else {
            return this.update(new UpdateWrapper<T>().in("id", ids).set("start_time", null).set("end_time", null));
        }
    }

    /**
     * 移除促销活动
     *
     * @param ids 促销活动id集合
     * @return 是否移除成功
     */
    @Override
    @Transactional(rollbackFor = {Exception.class})
    public boolean removePromotions(List<String> ids) {
        for (String id : ids) {
            T promotions = this.getById(id);
            this.checkStatus(promotions);
            promotions.setStartTime(null);
            promotions.setEndTime(null);
            this.updateEsGoodsIndex(promotions);
        }
        this.promotionGoodsService.deletePromotionGoods(ids);
        return this.removeByIds(ids);
    }

    /**
     * 分页查询促销信息
     *
     * @param searchParams 查询参数，继承自继承促销查询参数
     * @param page         分页参数
     * @return 分页促销信息
     */
    @Override
    public <S extends BasePromotionsSearchParams> IPage<T> pageFindAll(S searchParams, PageVO page) {
        page.setNotConvert(false);
        return this.page(PageUtil.initPage(page), searchParams.queryWrapper());
    }

    /**
     * 列表查询促销信息
     *
     * @param searchParams 查询参数，继承自继承促销查询参数
     * @return 列表促销信息
     */
    @Override
    public <S extends BasePromotionsSearchParams> List<T> listFindAll(S searchParams) {
        return this.list(searchParams.queryWrapper());
    }

    /**
     * 初始化促销字段
     *
     * @param promotions 促销实体
     */
    @Override
    public void initPromotion(T promotions) {
        if (CharSequenceUtil.isEmpty(promotions.getScopeType())) {
            promotions.setScopeType(PromotionsScopeTypeEnum.PORTION_GOODS.name());
        }
    }

    /**
     * 检查促销参数
     *
     * @param promotions 促销实体
     */
    @Override
    public void checkPromotions(T promotions) {
        PromotionTools.checkPromotionTime(promotions.getStartTime(), promotions.getEndTime());
    }

    /**
     * 检查促销状态
     *
     * @param promotions 促销实体
     */
    @Override
    public void checkStatus(T promotions) {
        T byId = this.getById(promotions.getId());
        if (byId == null) {
            throw new ServiceException(ResultCode.PROMOTION_ACTIVITY_ERROR);
        }
    }

    /**
     * 更新促销商品信息
     *
     * @param promotions 促销实体
     * @return
     */
    @Override
    @Transactional(rollbackFor = {Exception.class})
    public boolean updatePromotionsGoods(T promotions) {
        if (promotions.getStartTime() == null && promotions.getEndTime() == null) {
            this.promotionGoodsService.deletePromotionGoods(Collections.singletonList(promotions.getId()));
            return true;
        }
        boolean result = true;
        if (CharSequenceUtil.equalsAny(promotions.getScopeType(), PromotionsScopeTypeEnum.ALL.name(), PromotionsScopeTypeEnum.PORTION_GOODS_CATEGORY.name())) {
            PromotionGoods promotionGoods = new PromotionGoods();
            promotionGoods.setScopeId(promotions.getScopeId());
            promotionGoods.setScopeType(promotions.getScopeType());
            promotionGoods.setPromotionId(promotions.getId());
            promotionGoods.setStoreId(promotions.getStoreId());
            promotionGoods.setStoreName(promotions.getStoreName());
            promotionGoods.setStartTime(promotions.getStartTime());
            promotionGoods.setEndTime(promotions.getEndTime());
            promotionGoods.setPromotionType(this.getPromotionType().name());
            promotionGoods.setTitle(promotions.getPromotionName());
            this.promotionGoodsService.deletePromotionGoods(Collections.singletonList(promotions.getId()));
            result = this.promotionGoodsService.save(promotionGoods);
        }
        return result;
    }

    /**
     * 更新促销信息到商品索引
     *
     * @param promotions 促销实体
     */
    @Override
    public void updateEsGoodsIndex(T promotions) {
        if (promotions.getStartTime() == null && promotions.getEndTime() == null) {
            //删除商品促销消息
            String destination = rocketmqCustomProperties.getGoodsTopic() + ":" + GoodsTagsEnum.DELETE_GOODS_INDEX_PROMOTIONS.name();
            //发送mq消息
            rocketMQTemplate.asyncSend(destination, JSONUtil.toJsonStr(promotions), RocketmqSendCallbackBuilder.commonCallback());
        } else {

            String esPromotionKey = this.getPromotionType().name() + "-" + promotions.getId();
            Map<String, Object> map = new HashMap<>();
            // es促销key
            map.put("esPromotionKey", esPromotionKey);
            // 促销类型全路径名
            map.put("promotionsType", promotions.getClass().getName());
            // 促销实体
            map.put("promotions", promotions);
            applicationEventPublisher.publishEvent(new UpdateEsGoodsIndexPromotionsEvent("更新商品索引促销事件", JSONUtil.toJsonStr(map)));
        }
    }

}
