package cn.lili.modules.member.util;
import cn.hutool.json.JSONUtil;
import cn.lili.modules.order.order.entity.dto.PriceDetailDTO;
import org.junit.jupiter.api.Test;
import java.util.List;
import static org.assertj.core.api.Assertions.*;
/**
 * 平台补贴不减少商家结算基数，兼容旧订单并验证汇总。
 * @author Bulbasaur
 * @since 2026-11-05
 */
class PaidMemberSettlementTest {
    /**
     * 构造原价 100 元、会员补贴 12 元、佣金比例 10% 的样本。
     *
     * @return 用于实付及商家结算断言的价格明细
     */
    private PriceDetailDTO price(){var p=new PriceDetailDTO();p.setOriginalPrice(100D);p.setGoodsPrice(100D);p.setPaidMemberSubsidy(12D);p.setDiscountPrice(12D);p.setPlatFormCommissionPoint(10D);return p;}
    /**
     * 验证买家支付 88 元、平台补贴 12 元，商家以 100 元计佣后结算 90 元。
     */
    /**
     * 验证商家券减少结算基数，购物卡仅继续减少现金实付，不再次减少商家结算。
     */
    /**
     * 验证多项报价累加会员补贴，旧订单 JSON 缺失补贴字段时按 0 兼容。
     */
    @Test void buyerPays88PlatformPays12MerchantSettlesOn100(){var p=price();assertThat(p.getFlowPrice()).isEqualTo(88D);assertThat(p.getPlatFormCommission()).isEqualTo(10D);assertThat(p.getBillPrice()).isEqualTo(90D);}
    @Test void merchantCouponAndGiftCardKeepCorrectSettlement(){var p=price();p.setCouponPrice(10D);assertThat(p.getFlowPrice()).isEqualTo(78D);assertThat(p.getBillPrice()).isEqualTo(81D);p.setGiftCardPrice(20D);assertThat(p.getFlowPrice()).isEqualTo(58D);assertThat(p.getBillPrice()).isEqualTo(81D);}
    @Test void aggregatesSubsidyAndLegacyJsonDefaultsToZero(){var total=new PriceDetailDTO();total.accumulationPriceDTO(List.of(price(),price()));assertThat(total.getPaidMemberSubsidy()).isEqualTo(24D);assertThat(total.getFlowPrice()).isEqualTo(176D);assertThat(total.getBillPrice()).isEqualTo(180D);var legacy=JSONUtil.toBean("{\"goodsPrice\":100,\"discountPrice\":0}",PriceDetailDTO.class);assertThat(legacy.getPaidMemberSubsidy()).isZero();assertThat(legacy.getFlowPrice()).isEqualTo(100D);}
}
