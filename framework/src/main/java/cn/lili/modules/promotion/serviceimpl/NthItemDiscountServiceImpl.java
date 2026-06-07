package cn.lili.modules.promotion.serviceimpl;

import cn.lili.common.enums.PromotionTypeEnum;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.modules.promotion.entity.dos.NthItemDiscount;
import cn.lili.modules.promotion.entity.dto.search.PromotionGoodsSearchParams;
import cn.lili.modules.promotion.entity.enums.PromotionsScopeTypeEnum;
import cn.lili.modules.promotion.entity.enums.PromotionsStatusEnum;
import cn.lili.modules.promotion.entity.vos.NthItemDiscountVO;
import cn.lili.modules.promotion.mapper.NthItemDiscountMapper;
import cn.lili.modules.promotion.service.NthItemDiscountService;
import cn.lili.modules.promotion.service.PromotionGoodsService;
import cn.lili.modules.promotion.tools.PromotionTools;
import cn.lili.modules.promotion.tools.SkuPromotionGoodsSupport;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class NthItemDiscountServiceImpl extends AbstractPromotionsServiceImpl<NthItemDiscountMapper, NthItemDiscount>
        implements NthItemDiscountService {

    @Autowired
    private PromotionGoodsService promotionGoodsService;
    @Autowired
    private SkuPromotionGoodsSupport skuPromotionGoodsSupport;

    @Override
    public NthItemDiscountVO getNthItemDiscountVO(String id) {
        NthItemDiscount discount = this.getById(id);
        if (discount == null) {
            throw new ServiceException(ResultCode.NTH_ITEM_DISCOUNT_NOT_EXIST_ERROR);
        }
        NthItemDiscountVO vo = new NthItemDiscountVO(discount);
        PromotionGoodsSearchParams searchParams = new PromotionGoodsSearchParams();
        searchParams.setPromotionId(discount.getId());
        vo.setPromotionGoodsList(promotionGoodsService.listFindAll(searchParams));
        return vo;
    }

    @Override
    public void checkPromotions(NthItemDiscount promotions) {
        super.checkPromotions(promotions);
        if (promotions.getNthNum() == null || promotions.getNthNum() < 2) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "第N件须大于等于2");
        }
        if (promotions instanceof NthItemDiscountVO) {
            PromotionTools.checkPromotionTime(promotions.getStartTime(), promotions.getEndTime());
        }
    }

    @Override
    public boolean updatePromotionsGoods(NthItemDiscount promotions) {
        boolean result = super.updatePromotionsGoods(promotions);
        if (!PromotionsStatusEnum.CLOSE.name().equals(promotions.getPromotionStatus())
                && PromotionsScopeTypeEnum.PORTION_GOODS.name().equals(promotions.getScopeType())
                && promotions instanceof NthItemDiscountVO vo) {
            skuPromotionGoodsSupport.replacePromotionGoods(vo, vo.getPromotionGoodsList(), getPromotionType());
        }
        return result;
    }

    @Override
    public PromotionTypeEnum getPromotionType() {
        return PromotionTypeEnum.NTH_ITEM_DISCOUNT;
    }
}
