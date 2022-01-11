package cn.lili.modules.promotion.serviceimpl;


import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.common.enums.PromotionTypeEnum;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.vo.PageVO;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.entity.enums.GoodsStatusEnum;
import cn.lili.modules.goods.service.GoodsSkuService;
import cn.lili.modules.promotion.entity.dos.KanjiaActivityGoods;
import cn.lili.modules.promotion.entity.dos.PromotionGoods;
import cn.lili.modules.promotion.entity.dto.KanjiaActivityGoodsDTO;
import cn.lili.modules.promotion.entity.dto.KanjiaActivityGoodsOperationDTO;
import cn.lili.modules.promotion.entity.dto.search.KanjiaActivityGoodsParams;
import cn.lili.modules.promotion.entity.enums.PromotionsScopeTypeEnum;
import cn.lili.modules.promotion.entity.enums.PromotionsStatusEnum;
import cn.lili.modules.promotion.entity.vos.kanjia.KanjiaActivityGoodsListVO;
import cn.lili.modules.promotion.entity.vos.kanjia.KanjiaActivityGoodsVO;
import cn.lili.modules.promotion.mapper.KanJiaActivityGoodsMapper;
import cn.lili.modules.promotion.service.KanjiaActivityGoodsService;
import cn.lili.modules.promotion.service.PromotionGoodsService;
import cn.lili.modules.promotion.tools.PromotionTools;
import cn.lili.mybatis.util.PageUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;

/**
 * 砍价业务层实现
 *
 * @author qiuqiu
 * @since 2021/7/1
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class KanjiaActivityGoodsServiceImpl extends AbstractPromotionsServiceImpl<KanJiaActivityGoodsMapper, KanjiaActivityGoods> implements KanjiaActivityGoodsService {

    /**
     * 规格商品
     */
    @Autowired
    private GoodsSkuService goodsSkuService;

    @Autowired
    private PromotionGoodsService promotionGoodsService;

    @Override
    public Boolean add(KanjiaActivityGoodsOperationDTO kanJiaActivityGoodsOperationDTO) {
        List<KanjiaActivityGoods> kanjiaActivityGoodsList = new ArrayList<>();
        List<PromotionGoods> promotionGoodsList = new ArrayList<>();
        for (KanjiaActivityGoodsDTO kanJiaActivityGoodsDTO : kanJiaActivityGoodsOperationDTO.getPromotionGoodsList()) {
            //根据skuId查询商品信息
            GoodsSku goodsSku = this.checkSkuExist(kanJiaActivityGoodsDTO.getSkuId());
            //参数检测
            this.checkParam(kanJiaActivityGoodsDTO, goodsSku);
            //检测同一时间段是否存在相同的商品
            PromotionTools.checkPromotionTime(kanJiaActivityGoodsOperationDTO.getStartTime(), kanJiaActivityGoodsOperationDTO.getEndTime());
            kanJiaActivityGoodsDTO.setStartTime(kanJiaActivityGoodsOperationDTO.getStartTime());
            kanJiaActivityGoodsDTO.setEndTime(kanJiaActivityGoodsOperationDTO.getEndTime());
            //检测同一时间段不能允许添加相同的商品
            if (this.checkSkuDuplicate(kanJiaActivityGoodsDTO) != null) {
                throw new ServiceException("商品id为" + goodsSku.getId() + "的商品已参加砍价商品活动！");
            }
            kanJiaActivityGoodsDTO.setGoodsSku(goodsSku);
            kanJiaActivityGoodsDTO.setGoodsId(goodsSku.getGoodsId());
            kanJiaActivityGoodsDTO.setSkuId(kanJiaActivityGoodsDTO.getSkuId());
            kanJiaActivityGoodsDTO.setThumbnail(goodsSku.getThumbnail());
            kanJiaActivityGoodsDTO.setGoodsName(goodsSku.getGoodsName());
            kanJiaActivityGoodsDTO.setOriginalPrice(goodsSku.getPrice());
            kanJiaActivityGoodsDTO.setScopeId(goodsSku.getId());
            kanjiaActivityGoodsList.add(kanJiaActivityGoodsDTO);
        }
        boolean result = this.saveBatch(kanjiaActivityGoodsList);
        if (result) {
            for (KanjiaActivityGoods kanjiaActivityGoods : kanjiaActivityGoodsList) {
                PromotionGoods promotionGoods = new PromotionGoods();
                BeanUtils.copyProperties(kanjiaActivityGoods, promotionGoods);
                promotionGoods.setQuantity(kanjiaActivityGoods.getStock());
                promotionGoods.setPromotionId(kanjiaActivityGoods.getId());
                promotionGoods.setPromotionType(PromotionTypeEnum.KANJIA.name());
                promotionGoods.setTitle(PromotionTypeEnum.KANJIA.name() + "-" + kanjiaActivityGoods.getGoodsName());
                promotionGoods.setScopeType(PromotionsScopeTypeEnum.PORTION_GOODS.name());
                promotionGoods.setScopeId(kanjiaActivityGoods.getSkuId());
                promotionGoods.setPromotionType(PromotionTypeEnum.KANJIA.name());
                promotionGoodsList.add(promotionGoods);
            }
            boolean saveBatch = this.promotionGoodsService.saveBatch(promotionGoodsList);
            if (saveBatch) {
                for (KanjiaActivityGoods kanjiaActivityGoods : kanjiaActivityGoodsList) {
                    this.updateEsGoodsIndex(kanjiaActivityGoods);
                }
            }
        }

        return result;
    }


    @Override
    public IPage<KanjiaActivityGoodsListVO> kanjiaGoodsVOPage(KanjiaActivityGoodsParams kanjiaActivityGoodsParams, PageVO pageVO) {
        return this.baseMapper.kanjiaActivityGoodsVOPage(PageUtil.initPage(pageVO), kanjiaActivityGoodsParams.queryWrapper());
    }


    /**
     * 检查商品Sku是否存
     *
     * @param skuId skuId
     * @return 商品sku
     */
    private GoodsSku checkSkuExist(String skuId) {
        GoodsSku goodsSku = this.goodsSkuService.getGoodsSkuByIdFromCache(skuId);
        if (goodsSku == null) {
            log.error("商品ID为" + skuId + "的商品不存在！");
            throw new ServiceException();
        }
        return goodsSku;
    }

    /**
     * 检查参与砍价商品参数
     *
     * @param kanJiaActivityGoodsDTO 砍价商品信息
     * @param goodsSku               商品sku信息
     */
    private void checkParam(KanjiaActivityGoodsDTO kanJiaActivityGoodsDTO, GoodsSku goodsSku) {
        //校验商品是否存在
        if (goodsSku == null) {
            throw new ServiceException(ResultCode.PROMOTION_GOODS_NOT_EXIT);
        }
        //校验商品状态
        if (goodsSku.getMarketEnable().equals(GoodsStatusEnum.DOWN.name())) {
            throw new ServiceException(ResultCode.GOODS_NOT_EXIST);
        }
        //校验活动库存是否超出此sku的库存
        if (goodsSku.getQuantity() < kanJiaActivityGoodsDTO.getStock()) {
            throw new ServiceException(ResultCode.KANJIA_GOODS_ACTIVE_STOCK_ERROR);
        }
        //校验最低购买金额不能高于商品金额
        if (goodsSku.getPrice() < kanJiaActivityGoodsDTO.getPurchasePrice()) {
            throw new ServiceException(ResultCode.KANJIA_GOODS_ACTIVE_PRICE_ERROR);
        }
        //校验结算价格不能超过商品金额
        if (goodsSku.getPrice() < kanJiaActivityGoodsDTO.getSettlementPrice()) {
            throw new ServiceException(ResultCode.KANJIA_GOODS_ACTIVE_SETTLEMENT_PRICE_ERROR);
        }
        //校验最高砍价金额
        if (kanJiaActivityGoodsDTO.getHighestPrice() > goodsSku.getPrice() || kanJiaActivityGoodsDTO.getHighestPrice() <= 0) {
            throw new ServiceException(ResultCode.KANJIA_GOODS_ACTIVE_HIGHEST_PRICE_ERROR);
        }
        //校验最低砍价金额
        if (kanJiaActivityGoodsDTO.getLowestPrice() > goodsSku.getPrice() || kanJiaActivityGoodsDTO.getLowestPrice() <= 0) {
            throw new ServiceException(ResultCode.KANJIA_GOODS_ACTIVE_LOWEST_PRICE_ERROR);
        }
        //校验最低砍价金额不能高与最低砍价金额
        if (kanJiaActivityGoodsDTO.getLowestPrice() > kanJiaActivityGoodsDTO.getHighestPrice()) {
            throw new ServiceException(ResultCode.KANJIA_GOODS_ACTIVE_LOWEST_PRICE_ERROR);
        }
    }

    /**
     * 检查砍价商品是否重复存在
     *
     * @param kanJiaActivityGoodsDTO 砍价商品
     * @return 积分商品信息
     */
    private KanjiaActivityGoods checkSkuDuplicate(KanjiaActivityGoodsDTO kanJiaActivityGoodsDTO) {
        QueryWrapper<KanjiaActivityGoods> queryWrapper = new QueryWrapper<>();
        queryWrapper.eq("sku_id", kanJiaActivityGoodsDTO.getSkuId());
        if (kanJiaActivityGoodsDTO != null && CharSequenceUtil.isNotEmpty(kanJiaActivityGoodsDTO.getId())) {
            queryWrapper.ne("id", kanJiaActivityGoodsDTO.getId());
        }
        queryWrapper.and(i -> i
                .or(PromotionTools.queryPromotionStatus(PromotionsStatusEnum.START))
                .or(PromotionTools.queryPromotionStatus(PromotionsStatusEnum.NEW)));

        if (kanJiaActivityGoodsDTO != null && kanJiaActivityGoodsDTO.getStartTime() != null) {
            queryWrapper.ge("start_time", kanJiaActivityGoodsDTO.getStartTime());
        }

        if (kanJiaActivityGoodsDTO != null && kanJiaActivityGoodsDTO.getEndTime() != null) {
            queryWrapper.le("end_time", kanJiaActivityGoodsDTO.getEndTime());
        }

        return this.getOne(queryWrapper);

    }

    @Override
    public KanjiaActivityGoodsDTO getKanjiaGoodsDetail(String goodsId) {
        KanjiaActivityGoods kanjiaActivityGoods = this.getById(goodsId);
        if (kanjiaActivityGoods == null) {
            log.error("id为" + goodsId + "的砍价商品不存在！");
            throw new ServiceException();
        }
        KanjiaActivityGoodsDTO kanjiaActivityGoodsDTO = new KanjiaActivityGoodsDTO();
        BeanUtils.copyProperties(kanjiaActivityGoods, kanjiaActivityGoodsDTO);
        GoodsSku goodsSku = this.goodsSkuService.getGoodsSkuByIdFromCache(kanjiaActivityGoods.getSkuId());
        if (goodsSku != null) {
            kanjiaActivityGoodsDTO.setGoodsSku(goodsSku);
        }
        return kanjiaActivityGoodsDTO;
    }

    @Override
    public KanjiaActivityGoods getKanjiaGoodsBySkuId(String skuId) {
        KanjiaActivityGoods kanjiaActivityGoods = this.getOne(
                new LambdaQueryWrapper<KanjiaActivityGoods>()
                        .eq(KanjiaActivityGoods::getSkuId, skuId)
                        .ge(KanjiaActivityGoods::getEndTime, new Date())
                        .le(KanjiaActivityGoods::getStartTime, new Date()));
        if (kanjiaActivityGoods != null && PromotionsStatusEnum.START.name().equals(kanjiaActivityGoods.getPromotionStatus())) {
            return kanjiaActivityGoods;
        }
        return null;
    }

    @Override
    public KanjiaActivityGoodsVO getKanJiaGoodsVO(String id) {

        KanjiaActivityGoodsVO kanJiaActivityGoodsVO = new KanjiaActivityGoodsVO();
        //获取砍价商品
        KanjiaActivityGoods kanJiaActivityGoods = this.getById(id);
        //获取商品SKU
        GoodsSku goodsSku = this.goodsSkuService.getGoodsSkuByIdFromCache(kanJiaActivityGoods.getSkuId());
        //填写活动商品价格、剩余数量
        kanJiaActivityGoodsVO.setGoodsSku(goodsSku);
        kanJiaActivityGoodsVO.setStock(kanJiaActivityGoods.getStock());
        kanJiaActivityGoodsVO.setPurchasePrice(kanJiaActivityGoods.getPurchasePrice());
        //返回商品数据
        return kanJiaActivityGoodsVO;

    }

    @Override
    public boolean updateKanjiaActivityGoods(KanjiaActivityGoodsDTO kanJiaActivityGoodsDTO) {
        //校验砍价商品是否存在
        KanjiaActivityGoods dbKanJiaActivityGoods = this.getKanjiaGoodsDetail(kanJiaActivityGoodsDTO.getId());
        //校验当前活动是否已经开始,只有新建的未开始的活动可以编辑
        if (!dbKanJiaActivityGoods.getPromotionStatus().equals(PromotionsStatusEnum.NEW.name())) {
            throw new ServiceException(ResultCode.PROMOTION_UPDATE_ERROR);
        }
        //获取当前sku信息
        GoodsSku goodsSku = this.checkSkuExist(kanJiaActivityGoodsDTO.getSkuId());
        //校验商品状态
        if (goodsSku.getMarketEnable().equals(GoodsStatusEnum.DOWN.name())) {
            throw new ServiceException(ResultCode.GOODS_NOT_EXIST);
        }
        //常规校验砍价商品参数
        this.checkParam(kanJiaActivityGoodsDTO, goodsSku);
        //检测开始结束时间是否正确
        PromotionTools.checkPromotionTime(kanJiaActivityGoodsDTO.getStartTime(), kanJiaActivityGoodsDTO.getEndTime());
        //检测同一时间段不能允许添加相同的商品
        if (this.checkSkuDuplicate(kanJiaActivityGoodsDTO) != null) {
            throw new ServiceException("商品id为" + goodsSku.getId() + "的商品已参加砍价商品活动！");
        }
        this.promotionGoodsService.deletePromotionGoods(Collections.singletonList(kanJiaActivityGoodsDTO.getId()));
        PromotionGoods promotionGoods = new PromotionGoods(kanJiaActivityGoodsDTO);
        this.promotionGoodsService.save(promotionGoods);
        this.updateEsGoodsIndex(kanJiaActivityGoodsDTO);
        //修改数据库
        return this.updateById(kanJiaActivityGoodsDTO);
    }

    /**
     * 当前促销类型
     *
     * @return 当前促销类型
     */
    @Override
    public PromotionTypeEnum getPromotionType() {
        return PromotionTypeEnum.KANJIA;
    }
}