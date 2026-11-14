package cn.lili.modules.stock.entity.dos;

import cn.lili.mybatis.BaseIdEntity;
import com.baomidou.mybatisplus.annotation.FieldFill;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableName;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;
import org.springframework.data.annotation.CreatedDate;
import org.springframework.format.annotation.DateTimeFormat;

import java.util.Date;

/**
 * 库存变动流水实体类
 * 用于追踪记录商品SKU在全部业务场景下的库存增减台账及变动前后快照
 *
 * @author Bulbasaur
 * @since 2026-11-12
 */
@Data
@EqualsAndHashCode(callSuper = false)
@TableName("li_stock_flow")
@Schema(description = "库存流水")
public class StockFlow extends BaseIdEntity {

    private static final long serialVersionUID = 1L;

    @CreatedDate
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @TableField(fill = FieldFill.INSERT)
    @Schema(description = "创建时间")
    private Date createTime;

    @Schema(description = "店铺ID")
    private String storeId;

    @Schema(description = "商品ID")
    private String goodsId;

    @Schema(description = "SKU ID")
    private String skuId;

    @Schema(description = "商品名称")
    private String goodsName;

    @TableField(exist = false)
    @Schema(description = "当前 li_goods_sku 对应 SKU 的缩略图 URL，仅查询时补充，不作为历史图片快照存储；SKU 不存在时为空")
    private String thumbnail;

    @Schema(description = "规格快照")
    private String specs;

    @Schema(description = "变动类型")
    private String flowType;

    @Schema(description = "变动方向")
    private String direction;

    @Schema(description = "变动数量")
    private Integer quantity;

    @Schema(description = "变动前库存")
    private Integer beforeStock;

    @Schema(description = "变动后库存")
    private Integer afterStock;

    @Schema(description = "业务单号")
    private String bizSn;

    @Schema(description = "操作人")
    private String operator;

    @Schema(description = "备注")
    private String remark;
}
