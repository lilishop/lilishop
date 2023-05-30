package cn.lili.modules.order.cart.render.impl;

import cn.lili.common.utils.SnowFlake;
import cn.lili.modules.order.cart.entity.dto.StoreRemarkDTO;
import cn.lili.modules.order.cart.entity.dto.TradeDTO;
import cn.lili.modules.order.cart.entity.enums.RenderStepEnums;
import cn.lili.modules.order.cart.render.CartRenderStep;
import org.springframework.core.annotation.Order;
import org.springframework.stereotype.Service;

/**
 * sn 生成
 *
 * @author Chopper
 * @since 2020-07-02 14:47
 */
@Service
public class CartSnRender implements CartRenderStep {

    @Override
    public RenderStepEnums step() {
        return RenderStepEnums.CART_SN;
    }

    @Override
    public void render(TradeDTO tradeDTO) {

        //生成各个sn
        tradeDTO.setSn(SnowFlake.createStr("T"));
        tradeDTO.getCartList().forEach(item -> {
            //写入备注
            if (tradeDTO.getStoreRemark() != null) {
                for (StoreRemarkDTO remark : tradeDTO.getStoreRemark()) {
                    if (item.getStoreId().equals(remark.getStoreId())) {
                        item.setRemark(remark.getRemark());
                    }
                }
            }
            item.setSn(SnowFlake.createStr("O"));
        });

    }
}
