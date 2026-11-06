package cn.lili.modules.promotion.serviceimpl;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.entity.enums.GoodsTypeEnum;
import cn.lili.modules.goods.service.GoodsSkuService;
import cn.lili.modules.promotion.entity.dto.GiftCardCashActivityDTO;
import cn.lili.modules.promotion.entity.vos.PickupCardSkuVO;
import cn.lili.modules.promotion.service.PickupCardRuleService;
import com.alibaba.fastjson2.JSON;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * 提货卡规则校验及商品展示实现，供活动管理与兑换共用，避免活动管理依赖订单和交易创建链。
 *
 * @author Bulbasaur
 * @since 2026-11-06
 */
@Service
public class PickupCardRuleServiceImpl implements PickupCardRuleService {

    @Autowired
    private GoodsSkuService goodsSkuService;

    /**
     * 校验 1～50 个不同的有效实物 SKU，并将 ONE、ALL 的种数及零面额归一化。
     */
    @Override
    public void validateActivity(GiftCardCashActivityDTO dto) {
        List<String> ids = dto.getPickupSkuIds();
        validateSelection(dto.getPickupMode(), dto.getPickupCount(), ids, ids, false);
        for (String id : ids) {
            GoodsSku sku = goodsSkuService.getById(id);
            if (!isValidSku(sku)) throw error("请选择审核通过且已上架的普通实物商品");
        }
        if ("ONE".equals(dto.getPickupMode())) dto.setPickupCount(1);
        if ("ALL".equals(dto.getPickupMode())) dto.setPickupCount(ids.size());
        dto.setFaceValue(BigDecimal.ZERO.setScale(2));
    }

    /**
     * 校验兑换种数及集合归属，既用于活动保存，也用于后端兑换提交。
     */
    @Override
    public void validateSelection(String mode, Integer count, List<String> allowed, List<String> selected, boolean redeem) {
        if (!Set.of("ONE", "ALL", "ANY").contains(mode == null ? "" : mode)
                || allowed == null || allowed.isEmpty() || allowed.size() > 50
                || allowed.stream().anyMatch(id -> id == null || id.isBlank())
                || new HashSet<>(allowed).size() != allowed.size())
            throw error("兑换规则无效，商品须为 1～50 个不同 SKU");
        int required = "ONE".equals(mode) ? 1 : "ALL".equals(mode) ? allowed.size() : count == null ? 0 : count;
        if (required < 1 || required > allowed.size()) throw error("兑换种数必须在 1 和候选商品数之间");
        if (redeem && (selected == null || selected.size() != required
                || new HashSet<>(selected).size() != selected.size() || !allowed.containsAll(selected))) {
            throw error("请按卡规则选择指定数量的不同商品，每种一件");
        }
    }

    /**
     * 返回允许公开的商品字段，失效商品保留占位，不替换成其他 SKU。
     */
    @Override
    public List<PickupCardSkuVO> goods(String skuIds) {
        if (skuIds == null || skuIds.isBlank()) return List.of();
        List<PickupCardSkuVO> result = new ArrayList<>();
        for (String id : JSON.parseArray(skuIds, String.class)) {
            GoodsSku sku = goodsSkuService.getById(id);
            PickupCardSkuVO vo = new PickupCardSkuVO();
            vo.setId(id);
            vo.setGoodsName(sku == null ? "商品已失效（" + id + "）" : sku.getGoodsName());
            Integer stock = isValidSku(sku) ? goodsSkuService.getStock(id) : 0;
            vo.setAvailable(stock != null && stock > 0);
            if (sku != null) {
                vo.setThumbnail(sku.getThumbnail());
                vo.setSpecs(sku.getSpecs());
                vo.setStoreName(sku.getStoreName());
                vo.setPrice(sku.getPrice());
            }
            result.add(vo);
        }
        return result;
    }

    /**
     * 仅审核通过且上架的普通实物可提货，销售价必须为有限非负数。
     */
    @Override
    public boolean isValidSku(GoodsSku sku) {
        return sku != null && !Boolean.TRUE.equals(sku.getDeleteFlag()) && "PASS".equals(sku.getAuthFlag())
                && "UPPER".equals(sku.getMarketEnable()) && GoodsTypeEnum.PHYSICAL_GOODS.name().equals(sku.getGoodsType())
                && sku.getPrice() != null && Double.isFinite(sku.getPrice()) && sku.getPrice() >= 0;
    }

    /**
     * 将规则校验错误转换为统一的业务异常。
     */
    private static ServiceException error(String message) {
        return new ServiceException(ResultCode.PARAMS_ERROR, message);
    }
}
