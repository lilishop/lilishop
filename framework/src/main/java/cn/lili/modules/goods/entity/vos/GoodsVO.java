package cn.lili.modules.goods.entity.vos;

import cn.lili.modules.goods.entity.dos.Goods;
import cn.lili.modules.goods.entity.dos.Wholesale;
import cn.lili.modules.goods.entity.dto.GoodsParamsDTO;
import cn.lili.modules.goods.entity.dto.GoodsParamsItemDTO;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.util.List;
import java.util.Map;

/**
 * 商品VO
 *
 * @author pikachu
 * @since 2020-02-26 23:24:13
 */
@Data
@EqualsAndHashCode(callSuper = true)
public class GoodsVO extends Goods {

    private static final long serialVersionUID = 6377623919990713567L;

    @Schema(description = "分类名称")
    private List<String> categoryName;

    @Schema(description = "商品参数")
    private List<GoodsParamsItemDTO> goodsParamsDTOList;

    @Schema(description = "商品图片")
    private List<String> goodsGalleryList;

    @Schema(description = "sku列表")
    private List<GoodsSkuVO> skuList;

    @Schema(description = "批发商品消费规则列表")
    private List<Wholesale> wholesaleList;

    @Schema(description = "预约商品设置（编辑回显）")
    private Map<String, Object> appointmentSettings;

    @Schema(description = "预约时段模板（编辑回显）")
    private Map<String, Object> appointmentSlotTemplate;
}
