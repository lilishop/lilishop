package cn.lili.modules.distribution.entity.vos;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.util.ArrayList;
import java.util.List;

/**
 * 分销商品分享上下文
 *
 * @author Bulbasaur
 * @since 2026-07-28
 */
@Data
public class DistributionGoodsShareVO {

    @Schema(description = "默认分享文案")
    private String defaultShareText;

    @Schema(description = "商品名称")
    private String goodsName;

    @Schema(description = "商品卖点")
    private String goodsSellingPoint;

    @Schema(description = "商品价格")
    private Double price;

    @Schema(description = "商品缩略图")
    private String thumbnail;

    @Schema(description = "商品图片列表")
    private List<String> images = new ArrayList<>();

    @Schema(description = "分享链接（H5 完整地址或小程序页面路径）")
    private String miniProgramLink;

    @Schema(description = "小程序页面路径（不含域名）")
    private String sharePath;

    @Schema(description = "链接来源：WXMP_URL=小程序短链，H5=H5回退链接")
    private String linkSource;
}
