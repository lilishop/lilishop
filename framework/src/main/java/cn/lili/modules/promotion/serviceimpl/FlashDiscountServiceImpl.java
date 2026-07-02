package cn.lili.modules.promotion.serviceimpl;

import cn.lili.common.enums.PromotionTypeEnum;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.modules.promotion.entity.dos.FlashDiscount;
import cn.lili.modules.promotion.entity.dto.search.PromotionGoodsSearchParams;
import cn.lili.modules.promotion.entity.enums.PromotionsScopeTypeEnum;
import cn.lili.modules.promotion.entity.enums.PromotionsStatusEnum;
import cn.lili.modules.promotion.entity.vos.FlashDiscountVO;
import cn.lili.modules.promotion.mapper.FlashDiscountMapper;
import cn.lili.modules.promotion.service.FlashDiscountService;
import cn.lili.modules.promotion.service.PromotionGoodsService;
import cn.lili.modules.promotion.tools.PromotionTools;
import cn.lili.modules.promotion.tools.SkuPromotionGoodsSupport;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class FlashDiscountServiceImpl extends AbstractPromotionsServiceImpl<FlashDiscountMapper, FlashDiscount>
        implements FlashDiscountService {

    @Autowired
    private PromotionGoodsService promotionGoodsService;
    @Autowired
    private SkuPromotionGoodsSupport skuPromotionGoodsSupport;

    @Override
    public FlashDiscountVO getFlashDiscountVO(String id) {
        FlashDiscount flashDiscount = this.getById(id);
        if (flashDiscount == null) {
            throw new ServiceException(ResultCode.FLASH_DISCOUNT_NOT_EXIST_ERROR);
        }
        FlashDiscountVO vo = new FlashDiscountVO(flashDiscount);
        PromotionGoodsSearchParams searchParams = new PromotionGoodsSearchParams();
        searchParams.setPromotionId(flashDiscount.getId());
        vo.setPromotionGoodsList(promotionGoodsService.listFindAll(searchParams));
        return vo;
    }

    @Override
    public void checkPromotions(FlashDiscount promotions) {
        super.checkPromotions(promotions);
        if (promotions instanceof FlashDiscountVO) {
            PromotionTools.checkPromotionTime(promotions.getStartTime(), promotions.getEndTime());
        }
    }

    @Override
    public boolean updatePromotionsGoods(FlashDiscount promotions) {
        boolean result = super.updatePromotionsGoods(promotions);
        if (!PromotionsStatusEnum.CLOSE.name().equals(promotions.getPromotionStatus())
                && PromotionsScopeTypeEnum.PORTION_GOODS.name().equals(promotions.getScopeType())
                && promotions instanceof FlashDiscountVO vo) {
            skuPromotionGoodsSupport.replacePromotionGoods(vo, vo.getPromotionGoodsList(), getPromotionType());
        }
        return result;
    }

    @Override
    public PromotionTypeEnum getPromotionType() {
        return PromotionTypeEnum.FLASH_DISCOUNT;
    }
}
