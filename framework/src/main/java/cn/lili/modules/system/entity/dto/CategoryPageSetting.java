package cn.lili.modules.system.entity.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serializable;

/**
 * 分类页设置
 */
@Data
public class CategoryPageSetting implements Serializable {

    private static final long serialVersionUID = 1L;

    @Schema(description = "分类页模板:1左树右宫格 2顶部一级图标+左侧二级+三级标签 3顶部一级图标+左侧二级 4左侧一级+右上二级横滑")
    private Integer template = 1;

    @Schema(description = "商品列表展示字段开关")
    private ShowFields showFields = new ShowFields();

    /**
     * 商品列表展示字段开关
     */
    @Data
    @Schema(description = "商品列表展示字段开关")
    public static class ShowFields implements Serializable {

        private static final long serialVersionUID = 1L;

        @Schema(description = "商品副标题")
        private Boolean subTitle = true;

        @Schema(description = "规格值")
        private Boolean spec = true;

        @Schema(description = "标签")
        private Boolean tags = true;

        @Schema(description = "自营标签")
        private Boolean selfTag = true;

        @Schema(description = "价格(市场价/会员价)")
        private Boolean price = true;

        @Schema(description = "划线价")
        private Boolean linePrice = true;

        @Schema(description = "销量/好评/好评率")
        private Boolean sales = true;

        @Schema(description = "购物车")
        private Boolean cart = true;
    }
}
