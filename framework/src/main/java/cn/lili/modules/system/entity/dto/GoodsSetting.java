package cn.lili.modules.system.entity.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serializable;

/**
 * 商品设置
 *
 * @author Chopper
 * @since 2020/11/17 7:58 下午
 */
@Data
public class GoodsSetting implements Serializable {

    private static final long serialVersionUID = -4132785717179910025L;
    @Schema(description = "是否开启商品审核")
    private Boolean goodsCheck;

    @Schema(description = "自营店商品是否需要审核；未配置时沿用商品审核设置")
    private Boolean selfOperatedGoodsCheck;

}
