package cn.lili.modules.goods.entity.dos;

import cn.lili.mybatis.BaseIdEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * 商品参数倒排（搜索用）。纯派生数据，物理删除，不带审计列。
 *
 * @author Mike
 * @date 2026-09-03
 */
@Data
@EqualsAndHashCode(callSuper = true)
@TableName("li_goods_search_param")
@Schema(description = "商品参数倒排（搜索用）")
public class GoodsSearchParam extends BaseIdEntity {

    @Schema(description = "商品ID，与 li_goods_sku.goods_id 同型同排序规则")
    private String goodsId;

    @Schema(description = "参数名")
    private String paramName;

    @Schema(description = "参数值")
    private String paramValue;

    @Schema(description = "参数维度的排序，同一 (goods_id, param_name) 的各行取值相同")
    private Integer paramSort;
}
