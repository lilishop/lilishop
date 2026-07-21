package cn.lili.modules.goods.entity.dto;

import cn.lili.modules.goods.entity.dos.GoodsSku;
import com.baomidou.mybatisplus.annotation.TableField;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import org.springframework.beans.BeanUtils;

import java.util.List;

/**
 * @author paulG
 * @since 2022/6/13
 **/
@Data
@EqualsAndHashCode(callSuper = true)
@NoArgsConstructor
@AllArgsConstructor
public class GoodsSkuDTO extends GoodsSku {

    private static final long serialVersionUID = 6600436187015048097L;

    private List<GoodsParamsDTO> goodsParamDTOS;

    @Schema(description = "商品参数json")
    private String params;

    @Schema(description = "品牌名称")
    @TableField(exist = false)
    private String brandName;

    @Schema(description = "品牌logo")
    @TableField(exist = false)
    private String brandLogo;


    public GoodsSkuDTO(GoodsSku goodsSku, List<GoodsParamsDTO> goodsParamDTOS) {
        BeanUtils.copyProperties(goodsSku, this);
        this.goodsParamDTOS = goodsParamDTOS;
    }
}
