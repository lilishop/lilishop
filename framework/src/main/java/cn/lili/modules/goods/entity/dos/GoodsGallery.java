package cn.lili.modules.goods.entity.dos;

import cn.lili.mybatis.BaseIdEntity;
import com.baomidou.mybatisplus.annotation.FieldFill;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;
import org.springframework.data.annotation.CreatedBy;

/**
 * 商品相册
 *
 * @author pikachu
 * @since 2020-02-23 9:14:33
 */
@Data
@TableName("li_goods_gallery")
@Schema(description = "商品相册")
@EqualsAndHashCode(callSuper = false)
public class GoodsGallery extends BaseIdEntity {


    @CreatedBy
    @TableField(fill = FieldFill.INSERT)
    @Schema(description = "创建者", hidden = true)
    private String createBy;

    /**
     * 商品主键
     */
    @Schema(description = "商品id")
    private String goodsId;

    /**
     * 缩略图路径
     */
    @Schema(description = "缩略图路径")
    private String thumbnail;

    /**
     * 小图路径
     */
    @Schema(description = "小图路径")
    private String small;

    /**
     * 原图路径
     */
    @Schema(description = "原图路径", requiredMode = Schema.RequiredMode.REQUIRED)
    private String original;

    /**
     * 是否是默认图片1   0没有默认
     */
    @Schema(description = "是否是默认图片1   0没有默认")
    private Integer isDefault;

    /**
     * 排序
     */
    @Schema(description = "排序", requiredMode = Schema.RequiredMode.REQUIRED)
    private Integer sort;

}